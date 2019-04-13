use bytes::BytesMut;
use fnv::FnvBuildHasher;
use futures::stream::Fuse;
use futures::sync::mpsc::{Sender, UnboundedReceiver};
use futures::sync::oneshot;
use futures::{Async, AsyncSink, Future, Poll, Sink, Stream};
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::mem;
use std::time::{Duration, Instant};
use tokio_timer::Delay;

use crate::header::map::HeaderMapExtension;
use crate::header::types::CSeq;
use crate::protocol::codec::decoder::request::DecodeError as RequestDecodeError;
use crate::protocol::codec::decoder::response::DecodeError as ResponseDecodeError;
use crate::protocol::codec::{CodecEvent, DecodeError, Message, ProtocolError};
use crate::protocol::connection::pending::{PendingRequestResponse, PendingRequestUpdate};
use crate::protocol::connection::sender::SenderHandle;
use crate::request::Request;
use crate::response::{
    Response, BAD_REQUEST_RESPONSE, NOT_ENOUGH_BANDWIDTH_RESPONSE, REQUEST_URI_TOO_LONG_RESPONSE,
    VERSION_NOT_SUPPORTED_RESPONSE,
};
use crate::status::StatusCode;

/// Receiver responsible for processing incoming messages, including forwarding requests to the
/// request handler and matching responses to pending requests.
#[must_use = "futures do nothing unless polled"]
pub struct Receiver<TStream>
where
    TStream: Stream<Item = Message, Error = ProtocolError> + Send + 'static,
{
    /// How long should we wait before decoding is timed out and the connection is dropped.
    decode_timeout_duration: Duration,

    /// The current decoding timer tracking the time between the start of decoding and the end.
    decoding_timer: Option<Delay>,

    /// The forwarding receiver responsible for forwarding requests to the request handler in the
    /// order of their `"CSeq"`s.
    forwarding_receiver: Option<ForwardingReceiver>,

    /// Are requests allowed to be accepted.
    requests_allowed: bool,

    /// The response receiver responsible for matching incoming responses to pending requests.
    response_receiver: Option<ResponseReceiver>,

    /// A stream of codec events used to reset the decoding timer.
    rx_codec_event: Fuse<UnboundedReceiver<CodecEvent>>,

    /// A handle to the sender through which responses are sent.
    sender_handle: Option<SenderHandle>,

    /// The underlying connection stream from which messages are read and decoded from. This stream
    /// must represent an ordered, reliable protocol (e.g. TCP).
    stream: Option<TStream>,
}

impl<TStream> Receiver<TStream>
where
    TStream: Stream<Item = Message, Error = ProtocolError> + Send + 'static,
{
    /// Processes the given codec event.
    ///
    /// Encoding events are ignored, but decoding events are used to create decoding timers such
    /// that unlively connections are not kept open.
    fn handle_codec_event(&mut self, event: CodecEvent) {
        use self::CodecEvent::*;

        match event {
            DecodingStarted => {
                let expire_time = Instant::now() + self.decode_timeout_duration;
                self.decoding_timer = Some(Delay::new(expire_time));
            }
            DecodingEnded => {
                self.decoding_timer = None;
            }
            _ => {}
        }
    }

    /// Processes the given message.
    ///
    /// If it is a request, it will be buffered internally until it is ready to be forwarded to the
    /// request handler.
    ///
    /// If it is a response, it will be matched against a pending request with the same `"CSeq"` (if
    /// it exists).
    fn handle_message(&mut self, message: Message) -> Result<(), RequestReceiverError> {
        match message {
            Message::Request(request) => {
                if self.requests_allowed {
                    self.forwarding_receiver
                        .as_mut()
                        .expect("presence of request receiver should imply forwarding")
                        .handle_request(request)?;
                }
            }
            Message::Response(response) => {
                if let Some(response_receiver) = self.response_receiver.as_mut() {
                    response_receiver.handle_response(response);
                }
            }
        }

        Ok(())
    }

    /// Handles the given protocol error that occurred while trying to poll the internal stream by
    /// sending a client error response (i.e. status code between 400-500).
    ///
    /// This mainly is to provide better error messages to the agent if we can pinpoint what the
    /// exact problem is (e.g. an unsupported RTPS version vs invalid syntax).
    fn handle_protocol_error(&self, error: &ProtocolError) {
        if let Some(sender_handle) = self.sender_handle.as_ref() {
            match error {
                ProtocolError::DecodeError(_) => {
                    let message = Message::Response(BAD_REQUEST_RESPONSE.clone());
                    send_message(message, sender_handle);
                }
                ProtocolError::DecodeError(DecodeError::Request(
                    RequestDecodeError::UnsupportedVersion,
                ))
                | ProtocolError::DecodeError(DecodeError::Response(
                    ResponseDecodeError::UnsupportedVersion,
                )) => {
                    let message = Message::Response(VERSION_NOT_SUPPORTED_RESPONSE.clone());
                    send_message(message, sender_handle);
                }
                ProtocolError::DecodeError(DecodeError::Request(
                    RequestDecodeError::URITooLong,
                )) => {
                    let message = Message::Response(REQUEST_URI_TOO_LONG_RESPONSE.clone());
                    send_message(message, sender_handle);
                }
                _ => {}
            }
        }
    }

    /// Handles an error while trying to process a request.
    ///
    /// This should not be called if the request receiver is shutdown.
    fn handle_request_receiver_error(&self, error: RequestReceiverError) {
        let sender_handle = self
            .sender_handle
            .as_ref()
            .expect("request receiver error should imply message sending is active");

        match error {
            RequestReceiverError::BadRequest => {
                let message = Message::Response(BAD_REQUEST_RESPONSE.clone());
                send_message(message, sender_handle);
            }
            RequestReceiverError::NotEnoughBandwidth => {
                let message = Message::Response(NOT_ENOUGH_BANDWIDTH_RESPONSE.clone());
                send_message(message, sender_handle);
            }
        }
    }

    /// Returns whether request forwarding to the handler is shutdown.
    ///
    /// If true, this implies that request receiving is shutdown. There may still be requests that
    /// are currently queued in the handler, so responses may still be written.
    pub fn is_forwarding_shutdown(&self) -> bool {
        self.forwarding_receiver.is_none()
    }

    /// Returns whether all receiving is shutdown (i.e. no requests or responses can be received).
    pub fn is_receiving_shutdown(&self) -> bool {
        self.is_request_receiver_shutdown() && self.is_response_receiver_shutdown()
    }

    /// Returns whether request receiving is shutdown.
    ///
    /// The forwarding receiver may still be running due to the backpressure of the handler queue.
    pub fn is_request_receiver_shutdown(&self) -> bool {
        !self.requests_allowed
    }

    /// Returns whether response receiving is shutdown.
    pub fn is_response_receiver_shutdown(&self) -> bool {
        self.response_receiver.is_none()
    }

    /// Returns whether all receiving and forwarding is shutdown.
    fn is_shutdown(&self) -> bool {
        self.is_receiving_shutdown() && self.is_forwarding_shutdown()
    }

    /// Constructs a new receiver.
    pub fn new(
        stream: TStream,
        rx_pending_request: UnboundedReceiver<PendingRequestUpdate>,
        rx_codec_event: UnboundedReceiver<CodecEvent>,
        tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
        sender_handle: SenderHandle,
        decode_timeout_duration: Duration,
        request_buffer_size: usize,
    ) -> Self {
        Receiver {
            decode_timeout_duration,
            decoding_timer: None,
            forwarding_receiver: Some(ForwardingReceiver::new(
                tx_incoming_request,
                request_buffer_size,
            )),
            requests_allowed: true,
            response_receiver: Some(ResponseReceiver::new(rx_pending_request)),
            rx_codec_event: rx_codec_event.fuse(),
            sender_handle: Some(sender_handle),
            stream: Some(stream),
        }
    }

    /// Checks for new codec events.
    ///
    /// If `Ok(Async::Ready(()))` is never returned.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there are no more codec events to be processed.
    ///
    /// If `Err(())` is never returned.
    pub fn poll_codec_events(&mut self) -> Poll<(), ()> {
        loop {
            match self
                .rx_codec_event
                .poll()
                .expect("`Receiver.rx_codec_event` should not error")
            {
                Async::Ready(Some(event)) => self.handle_codec_event(event),
                Async::NotReady => return Ok(Async::NotReady),
                Async::Ready(None) => panic!("`Receiver.rx_codec_event` should not end"),
            }
        }
    }

    /// Checks to see if the decoding timer has expired.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the timer has expired.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there is either no timer or it has not expired.
    ///
    /// If `Err(())` is returned, then there was a timer error due to there being too many timers.
    fn poll_decoding_timer(&mut self) -> Poll<(), ()> {
        if let Some(decoding_timer) = self.decoding_timer.as_mut() {
            match decoding_timer.poll() {
                Ok(Async::Ready(_)) => return Ok(Async::Ready(())),
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Err(ref error) if error.is_at_capacity() => return Err(()),
                _ => panic!("decoding timer should not be shutdown"),
            }
        }

        Ok(Async::NotReady)
    }

    /// Drives the request and response receivers.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then all of receiving is shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then receiving is still running, at least partially.
    /// Specifically, either the request or response receivers are still running.
    ///
    /// If `Err(())` is returned, then there was an error driving either the request or response
    /// receivers. This could arise due to some underlying IO error or due to too many timers being
    /// created.
    fn poll_receiving(&mut self) -> Poll<(), ()> {
        if let Some(response_receiver) = self.response_receiver.as_mut() {
            match response_receiver.poll() {
                Ok(Async::Ready(_)) | Err(_) => {
                    // From our side of the connection, this implies that we are no longer sending
                    // anymore requests and so we do not care to process responses any longer.
                    self.shutdown_response_receiver();
                }
                _ => (),
            }
        }

        match self.poll_stream() {
            Ok(Async::Ready(_)) | Err(_) => {
                // There are no more messages to be received.
                return Ok(Async::Ready(()));
            }
            _ => (),
        }

        // The response here is effectively constant, so ignore it.
        let _ = self.poll_codec_events();

        match self.poll_decoding_timer() {
            Ok(Async::Ready(_)) | Err(_) => {
                // Either the decoding timer expired or there were too many timers.
                //
                // In the former, the other agent took too long to send anymore data, so we close
                // receiving in order to avoid locking up resources for no reason.
                //
                // In the latter, we no longer have a decoder timer. Shutting down receiving serves
                // two purposes here. One, it helps to prevent DoS attacks and two, it helps to shed
                // load.
                return Ok(Async::Ready(()));
            }
            _ => (),
        }

        Ok(Async::NotReady)
    }

    /// Checks if there are any messages to be processed from the internal connection stream.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the stream has been closed and no more messages
    /// will be received.
    ///
    /// If `Ok(Async::NotReady)` is returned, then either there are no more messages to be processed
    /// from the stream currently, or no messages can currently be accepted.
    ///
    /// If `Err(`[`ProtocolError`]`)` is returned, then there was a protocol error while trying to
    /// poll the stream.
    pub fn poll_stream(&mut self) -> Poll<(), ProtocolError> {
        match self.stream.take() {
            Some(mut stream) => loop {
                if let Some(forwarding_receiver) = self.forwarding_receiver.as_ref() {
                    // If the forwarding receiver is full, then any incoming requests cannot be
                    // handled. This also blocks any incoming responses, since we have to process
                    // messages as they come.
                    if forwarding_receiver.is_full() {
                        self.stream = Some(stream);
                        return Ok(Async::NotReady);
                    }
                }

                match stream.poll() {
                    Ok(Async::Ready(Some(message))) => {
                        if let Err(error) = self.handle_message(message) {
                            self.handle_request_receiver_error(error);
                        }
                    }
                    Ok(Async::NotReady) => {
                        self.stream = Some(stream);
                        return Ok(Async::NotReady);
                    }
                    Ok(Async::Ready(None)) => return Ok(Async::Ready(())),
                    Err(error) => {
                        self.handle_protocol_error(&error);
                        return Err(error);
                    }
                }
            },
            None => Ok(Async::Ready(())),
        }
    }

    /// Shuts down the forwarding receiver.
    ///
    /// Since the request receiver cannot be running without a forwarding receiver, this also shuts
    /// down the request receiver.
    ///
    /// Returns whether all receiving and forwarding is shutdown.
    pub fn shutdown_forwarding_receiver(&mut self) -> bool {
        self.forwarding_receiver = None;
        self.shutdown_request_receiver()
    }

    /// Shuts down all of receiving.
    ///
    /// The forwarding receiver will be shutdown only if there are no buffered requests awaiting
    /// forwarding.
    ///
    /// Returns whether all receiving and forwarding is shutdown.
    pub fn shutdown_receiving(&mut self) -> bool {
        self.shutdown_request_receiver();
        self.shutdown_response_receiver()
    }

    /// Shuts down the request receiver.
    ///
    /// The forwarding receiver will be shutdown only if there are no buffered requests awaiting
    /// forwarding.
    ///
    /// Returns whether all receiving and forwarding is shutdown.
    pub fn shutdown_request_receiver(&mut self) -> bool {
        self.requests_allowed = false;

        if self.is_response_receiver_shutdown() {
            self.sender_handle = None;
            self.stream = None;
        }

        if let Some(forwarding_receiver) = self.forwarding_receiver.as_mut() {
            if !forwarding_receiver.has_requests_ready() {
                self.forwarding_receiver = None;
            }
        }

        self.is_shutdown()
    }

    /// Shuts down the response receiver.
    ///
    /// Returns whether all receiving and forwarding is shutdown.
    pub fn shutdown_response_receiver(&mut self) -> bool {
        self.response_receiver = None;

        if self.is_request_receiver_shutdown() {
            self.sender_handle = None;
            self.stream = None;
        }

        self.is_shutdown()
    }
}

impl<TStream> Future for Receiver<TStream>
where
    TStream: Stream<Item = Message, Error = ProtocolError> + Send + 'static,
{
    type Item = ();
    type Error = ();

    /// Processes any incoming messages, forwards ready requests to the request handler, and matches
    /// incoming requests to pending requests.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the receiver (including forwarding) is shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there is no more progress that can be made
    /// currently.
    ///
    /// If `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self.poll_receiving() {
            Ok(Async::Ready(_)) | Err(_) => {
                self.shutdown_receiving();
            }
            _ => (),
        }

        let is_shutdown = match self.forwarding_receiver.as_mut() {
            Some(forwarding_receiver) => match forwarding_receiver.poll() {
                Err(_) => self.shutdown_forwarding_receiver(),
                Ok(Async::Ready(_)) if self.is_request_receiver_shutdown() => {
                    self.shutdown_forwarding_receiver()
                }
                _ => false,
            },
            None => self.is_shutdown(),
        };

        if is_shutdown {
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

/// Receiver responsible for forwarding incoming requests to the request handler in the order of
/// their `"CSeq"`s.
///
/// Even with a reliable transport protocol like TCP, there is no guarantee on the order requests
/// arrive in such as when multiplexing is involved (e.g. proxies). As a result, the we need to
/// temporarily buffer requests internally until it is their turn to be forwarded.
#[must_use = "futures do nothing unless polled"]
struct ForwardingReceiver {
    /// A map from request `"CSeq"`s to the corresponding request.
    buffered_requests: HashMap<CSeq, Request<BytesMut>, FnvBuildHasher>,

    /// The expected sequence number for the next incoming request. This will be [`Option::None`] in
    /// the case where we have yet to receive a request, since it is the client that determines the
    /// initial `"CSeq"`.
    incoming_sequence_number: Option<CSeq>,

    /// The capacity of the buffer map.
    request_buffer_size: usize,

    /// The channel that connects to the request handler. Requests sent through this channel should
    /// be ordered by their `"CSeq"`s.
    tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
}

impl ForwardingReceiver {
    /// Handles the given request by buffering it internally until it is time for it to be
    /// forwarded.
    ///
    /// There are some error conditions handled here:
    ///  - If the `"CSeq"` of the request is not valid, a Bad Request (400) is returned.
    ///  - If the difference between the current incoming sequence number and the request's `"CSeq"`
    ///    is larger than the internal buffer, a Not Enough Bandwidth (453) is returned.
    ///  - If the `"CSeq"` of the request is associated to an already buffered request, a Bad
    ///    Request (400) is returned.
    pub fn handle_request(
        &mut self,
        request: Request<BytesMut>,
    ) -> Result<(), RequestReceiverError> {
        match request.headers().typed_get::<CSeq>() {
            Some(cseq) => {
                let incoming_sequence_number = self.incoming_sequence_number_or_default(cseq);

                if *(cseq - incoming_sequence_number) > self.request_buffer_size as u32 {
                    Err(RequestReceiverError::NotEnoughBandwidth)
                } else {
                    debug_assert!(self.buffered_requests.len() < self.request_buffer_size);

                    match self.buffered_requests.entry(cseq) {
                        Entry::Occupied(_) => Err(RequestReceiverError::BadRequest),
                        Entry::Vacant(entry) => {
                            entry.insert(request);
                            Ok(())
                        }
                    }
                }
            }
            None => Err(RequestReceiverError::BadRequest),
        }
    }

    /// Returns whether the forwarding receiver has any requests that are ready to be forwarded.
    /// Even if there are requests ready, it may not be possible to forward any at the current time
    /// due to the handler queue being full.
    pub fn has_requests_ready(&self) -> bool {
        match self.incoming_sequence_number {
            Some(incoming_sequence_number) => self
                .buffered_requests
                .contains_key(&incoming_sequence_number),
            None => false,
        }
    }

    /// Returns the current `"CSeq"` used for incoming requests or defaults to the given one.
    ///
    /// Before the first request has been received on a connection, we do not know what `"CSeq"` we
    /// should be looking at, so we default to whatever the first request has if we do not yet know.
    /// In this case, the internal `"CSeq"` will be set to this default.
    pub fn incoming_sequence_number_or_default(&mut self, cseq: CSeq) -> CSeq {
        match self.incoming_sequence_number {
            Some(cseq) => cseq,
            None => {
                self.incoming_sequence_number = Some(cseq);
                cseq
            }
        }
    }

    /// Returns whether the internal request buffer is full.
    pub fn is_full(&self) -> bool {
        self.buffered_requests.len() >= self.request_buffer_size
    }

    /// Constructs a new forwarding receivers that forwards requests, in order, through the given
    /// bounded channel of the given size.
    ///
    /// The size is also used internally in the forwarding receiver as the size of the buffer used
    /// to reorder the requests based on their `"CSeq"`.
    pub fn new(
        tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
        request_buffer_size: usize,
    ) -> Self {
        ForwardingReceiver {
            buffered_requests: HashMap::with_capacity_and_hasher(
                request_buffer_size,
                FnvBuildHasher::default(),
            ),
            incoming_sequence_number: None,
            request_buffer_size,
            tx_incoming_request,
        }
    }
}

impl Future for ForwardingReceiver {
    type Item = ();
    type Error = ();

    /// Tries to forward any ready requests to the request handler.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then all requests that could have been forwarded have
    /// been forwarded.
    ///
    /// If `Ok(Async::NotReady)` is returned, then channel between the forwarding receiver and the
    /// request handler is full, and forwarding will have to be tried again later.
    ///
    /// If `Err(())` is returned, then the request handler's receiver has been dropped meaning the
    /// forwarding receiver can be shutdown.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(mut incoming_sequence_number) = self.incoming_sequence_number {
            while let Some(request) = self.buffered_requests.remove(&incoming_sequence_number) {
                match self
                    .tx_incoming_request
                    .start_send((incoming_sequence_number, request))
                    .map_err(|_| ())?
                {
                    AsyncSink::Ready => {
                        incoming_sequence_number = incoming_sequence_number.wrapping_increment()
                    }
                    AsyncSink::NotReady((_, request)) => {
                        self.buffered_requests
                            .insert(incoming_sequence_number, request);
                        self.incoming_sequence_number = Some(incoming_sequence_number);
                        return Ok(Async::NotReady);
                    }
                }
            }

            self.incoming_sequence_number = Some(incoming_sequence_number);
        }

        Ok(Async::Ready(()))
    }
}

/// The type responsible for keeping track of pending requests, matching incoming responses to those
/// requests, and notifying request owners of the match.
#[must_use = "futures do nothing unless polled"]
struct ResponseReceiver {
    /// A map of pending requests sent by this agent that are awaiting responses from the end-agent.
    pending_requests: HashMap<CSeq, oneshot::Sender<PendingRequestResponse>, FnvBuildHasher>,

    /// The stream of pending request updates that is used to add or remove pending requests.
    rx_pending_request: Fuse<UnboundedReceiver<PendingRequestUpdate>>,
}

impl ResponseReceiver {
    /// Handles a pending request updates.
    ///
    /// If the update is the addition of a pending request, the request, along with its `"CSeq"`
    /// value, will be stored awaiting the corresponding response.
    ///
    /// If the update is the removal of a pending request, the request with the given `"CSeq"` is
    /// removed and no response will be matched even if it does come at a later time.
    fn handle_pending_request_update(&mut self, update: PendingRequestUpdate) {
        use self::PendingRequestUpdate::*;

        match update {
            AddPendingRequest((cseq, tx_pending_request)) => {
                debug_assert!(!self.pending_requests.contains_key(&cseq));
                self.pending_requests.insert(cseq, tx_pending_request);
            }
            RemovePendingRequest(cseq) => {
                debug_assert!(self.pending_requests.contains_key(&cseq));
                self.pending_requests.remove(&cseq);
            }
        }
    }

    /// Handles the given response by finding the matching pending request and sending it back to
    /// the future owning the pending request.
    ///
    /// Continue (100) responses are treated differently in that the pending request is not removed,
    /// but instead updated to let the owner know that the request can still be considered alive.
    pub fn handle_response(&mut self, response: Response<BytesMut>) {
        debug_assert!(!self.should_shutdown());

        // Ignore any responses without a `"CSeq"` header or without a corresponding pending
        // request.
        //
        // It is possible the oneshot receiver has been dropped before we can send a
        // response. If the pending request future was being polled on a separate thread
        // then the response receiver, the pending request future may have closed the
        // receiver and sent a new pending request update to cancel the request.

        if let Some(cseq) = response.headers().typed_get::<CSeq>() {
            if response.status_code() == StatusCode::Continue {
                if let Some(pending_request) = self.pending_requests.get_mut(&cseq) {
                    let (tx_pending_request, rx_pending_request) = oneshot::channel();

                    if mem::replace(pending_request, tx_pending_request)
                        .send(PendingRequestResponse::Continue(rx_pending_request))
                        .is_err()
                    {
                        self.pending_requests.remove(&cseq);
                    }
                }
            } else if let Some(pending_request) = self.pending_requests.remove(&cseq) {
                let _ = pending_request.send(PendingRequestResponse::Response(response));
            }
        }
    }

    /// Constructs a new response receiver using the given pending request update stream.
    pub fn new(rx_pending_request: UnboundedReceiver<PendingRequestUpdate>) -> Self {
        ResponseReceiver {
            pending_requests: HashMap::with_hasher(FnvBuildHasher::default()),
            rx_pending_request: rx_pending_request.fuse(),
        }
    }

    /// Removes all pending requests from the response receiver.
    ///
    /// All pending request receivers will receive a message indicating that no request will be
    /// matched, effectively resulting in a cancelled request.
    fn remove_pending_requests(&mut self) {
        self.pending_requests
            .drain()
            .for_each(|(_, tx_pending_request)| {
                let _ = tx_pending_request.send(PendingRequestResponse::None);
            });
    }

    /// Whether whether the response receiver is capable of receiving anymore responses.
    pub fn should_shutdown(&self) -> bool {
        self.rx_pending_request.is_done()
    }
}

impl Drop for ResponseReceiver {
    fn drop(&mut self) {
        // Try to handle any remaining pending request updates before cancelling all pending
        // requests.
        self.poll().unwrap();
        self.remove_pending_requests();
    }
}

impl Future for ResponseReceiver {
    type Item = ();
    type Error = ();

    /// Handles incoming pending request updates.
    ///
    /// A pending request update is either the addition of a pending request or the removal of a
    /// pending request (probably due a timeout).
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the pending request update stream has ended and
    /// the response receiver is shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there are no pending request updates to be
    /// processed currently.
    ///
    /// The error `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            match self
                .rx_pending_request
                .poll()
                .expect("`ResponseReceiver.rx_pending_request` should not error")
            {
                Async::Ready(Some(update)) => self.handle_pending_request_update(update),
                Async::NotReady => return Ok(Async::NotReady),
                Async::Ready(None) => {
                    // If the pending request stream has ended, this means there should be no
                    // pending requests. If there were pending requests, they could never expire
                    // because the stream used to remove them has ended. So, we assume that it
                    // cannot happen.
                    debug_assert!(self.pending_requests.is_empty());
                    return Ok(Async::Ready(()));
                }
            }
        }
    }
}

/// Error that may be returned when processing incoming requests.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
enum RequestReceiverError {
    /// Some part of the request is invalid and cannot be processed.
    BadRequest,

    /// The difference in the next expected `"CSeq"` and the request's `"CSeq"` was too large to
    /// internally buffer.
    NotEnoughBandwidth,
}

impl Display for RequestReceiverError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::RequestReceiverError::*;

        match self {
            BadRequest => write!(formatter, "bad request"),
            NotEnoughBandwidth => write!(formatter, "not enough bandwidth"),
        }
    }
}

impl Error for RequestReceiverError {}

/// Sends the given message through the given sender handler.
///
/// It is assumed the sender handle is always alive, sending a message must not fail.
fn send_message(message: Message, sender_handle: &SenderHandle) {
    sender_handle
        .try_send_message(message)
        .expect("`Receiver.sender_handle` should not have been dropped");
}
