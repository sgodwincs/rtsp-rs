use bytes::BytesMut;
use futures::future::{self, Either};
use futures::stream::Fuse;
use futures::sync::mpsc::{Sender, UnboundedReceiver};
use futures::sync::oneshot;
use futures::{Async, AsyncSink, Future, Poll, Sink, Stream};
use std::collections::HashMap;
use std::mem;
use std::time::{Duration, Instant};
use tokio_timer::{Delay, Error as TimerError};

use super::{PendingRequestResponse, PendingRequestUpdate, SenderHandle};
use crate::header::types::CSeq;
use crate::header::{HeaderName, HeaderValue, TypedHeader};
use crate::protocol::{
    CodecEvent, DecodeError, InvalidMessage, IrrecoverableInvalidRequest,
    IrrecoverableInvalidResponse, Message, MessageResult, ProtocolError,
};
use crate::request::Request;
use crate::response::{Response, BAD_REQUEST_RESPONSE};
use crate::status::StatusCode;

lazy_static! {
    static ref NOT_ENOUGH_BANDWIDTH_RESPONSE: Response<BytesMut> = Response::builder()
        .status_code(StatusCode::NotEnoughBandwidth)
        .build(BytesMut::new())
        .expect("not enough bandwidth response should not be invalid");

    // TODO: As per specification, the "response SHOULD contain a message body describing why
    // that version is not supported and what other protocols are supported by that agent".

    static ref VERSION_NOT_SUPPORTED_RESPONSE: Response<BytesMut> = Response::builder()
        .status_code(StatusCode::RTSPVersionNotSupported)
        .build(BytesMut::new())
        .expect("RTSP version not supported response should not be invalid");
}

pub struct Receiver {
    forwarding_receiver: Option<ForwardingReceiver>,
    inner: Option<ReceiverInner>,
}

impl Receiver {
    pub fn new(
        stream: Box<Stream<Item = MessageResult, Error = ProtocolError> + Send + 'static>,
        rx_pending_request: UnboundedReceiver<PendingRequestUpdate>,
        rx_codec_event: UnboundedReceiver<CodecEvent>,
        tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
        decode_timeout_duration: Duration,
        request_buffer_size: usize,
    ) -> Self {
        Receiver {
            forwarding_receiver: Some(ForwardingReceiver::new(
                tx_incoming_request,
                request_buffer_size,
            )),
            inner: Some(ReceiverInner::new(
                stream,
                rx_codec_event,
                rx_pending_request,
                decode_timeout_duration,
            )),
        }
    }

    pub fn is_forwarding_shutdown(&self) -> bool {
        self.forwarding_receiver.is_none()
    }

    pub fn is_receiving_shutdown(&self) -> bool {
        self.inner.is_none()
    }

    pub fn is_request_receiver_shutdown(&self) -> bool {
        if let Some(ref inner) = self.inner {
            inner.is_request_receiver_shutdown()
        } else {
            true
        }
    }

    pub fn is_response_receiver_shutdown(&self) -> bool {
        if let Some(ref inner) = self.inner {
            inner.is_response_receiver_shutdown()
        } else {
            true
        }
    }

    pub fn number_pending(&self) -> usize {
        if let Some(ref inner) = self.inner {
            inner.number_pending()
        } else {
            0
        }
    }

    pub fn poll(&mut self, sender_handle: &mut Option<SenderHandle>) -> Poll<(), ()> {
        if let Some(inner) = self.inner.take() {
            self.inner = self.poll_inner(inner, sender_handle);
        }

        if let Some(forwarding_receiver) = self.forwarding_receiver.take() {
            self.forwarding_receiver = self.poll_forwarding_receiver(forwarding_receiver);
        }

        if self.should_shutdown() {
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }

    fn poll_forwarding_receiver(
        &mut self,
        mut forwarding_receiver: ForwardingReceiver,
    ) -> Option<ForwardingReceiver> {
        match forwarding_receiver.poll_forward() {
            Err(_) => None,
            Ok(Async::Ready(_)) if self.is_request_receiver_shutdown() => None,
            _ => Some(forwarding_receiver),
        }
    }

    fn poll_inner(
        &mut self,
        mut inner: ReceiverInner,
        sender_handle: &mut Option<SenderHandle>,
    ) -> Option<ReceiverInner> {
        match inner.poll_pending_request_update() {
            Ok(Async::Ready(_)) | Err(_) => {
                inner.shutdown_response_receiver();
            }
            _ => (),
        }

        match inner.poll_receive(&mut self.forwarding_receiver, sender_handle) {
            Ok(Async::Ready(_)) | Err(_) => return None,
            _ => (),
        }

        inner.poll_codec_events();

        match inner.poll_decoding_timer() {
            Ok(Async::Ready(_)) | Err(_) => return None,
            _ => (),
        }

        Some(inner)
    }

    fn should_shutdown(&self) -> bool {
        self.is_receiving_shutdown() && self.is_forwarding_shutdown()
    }

    pub fn shutdown_request_receiver(&mut self) -> bool {
        if let Some(mut inner) = self.inner.take() {
            if inner.shutdown_request_receiver() {
                self.inner = None;
            } else {
                self.inner = Some(inner);
            }

            if let Some(forwarding_receiver) = self.forwarding_receiver.take() {
                if forwarding_receiver.number_buffered() > 0 {
                    self.forwarding_receiver = Some(forwarding_receiver);
                }
            }
        }

        self.should_shutdown()
    }

    pub fn shutdown_response_receiver(&mut self) -> bool {
        if let Some(mut inner) = self.inner.take() {
            if inner.shutdown_response_receiver() {
                self.inner = None;
            } else {
                self.inner = Some(inner);
            }
        }

        self.should_shutdown()
    }
}

struct ReceiverInner {
    decode_timeout_duration: Duration,
    decoding_timer: Either<Delay, future::Empty<(), TimerError>>,
    request_receiver: Option<RequestReceiver>,
    response_receiver: Option<ResponseReceiver>,
    rx_codec_event: UnboundedReceiver<CodecEvent>,
    stream: Box<Stream<Item = MessageResult, Error = ProtocolError> + Send + 'static>,
}

impl ReceiverInner {
    fn handle_request_receiver_error(
        error: RequestReceiverError,
        sender_handle: &mut SenderHandle,
    ) {
        match error {
            RequestReceiverError::BadRequest => {
                let message = Message::Response(BAD_REQUEST_RESPONSE.clone());
                ReceiverInner::send_message(message, sender_handle);
            }
            RequestReceiverError::NotEnoughBandwidth => {
                let message = Message::Response(NOT_ENOUGH_BANDWIDTH_RESPONSE.clone());
                ReceiverInner::send_message(message, sender_handle);
            }
        }
    }

    fn handle_protocol_error(error: &ProtocolError, sender_handle: &mut Option<SenderHandle>) {
        if let Some(sender_handle) = sender_handle {
            match error {
                ProtocolError::DecodeError(DecodeError::InvalidRequest(
                    IrrecoverableInvalidRequest::UnsupportedVersion,
                ))
                | ProtocolError::DecodeError(DecodeError::InvalidResponse(
                    IrrecoverableInvalidResponse::UnsupportedVersion,
                )) => {
                    let message = Message::Response(VERSION_NOT_SUPPORTED_RESPONSE.clone());
                    ReceiverInner::send_message(message, sender_handle);
                }
                ProtocolError::DecodeError(_) => {
                    let message = Message::Response(BAD_REQUEST_RESPONSE.clone());
                    ReceiverInner::send_message(message, sender_handle);
                }
                _ => {}
            }
        }
    }

    pub fn new(
        stream: Box<Stream<Item = MessageResult, Error = ProtocolError> + Send + 'static>,
        rx_codec_event: UnboundedReceiver<CodecEvent>,
        rx_pending_request: UnboundedReceiver<PendingRequestUpdate>,
        decode_timeout_duration: Duration,
    ) -> Self {
        ReceiverInner {
            decode_timeout_duration,
            decoding_timer: Either::B(future::empty()),
            request_receiver: Some(RequestReceiver),
            response_receiver: Some(ResponseReceiver::new(rx_pending_request)),
            rx_codec_event,
            stream,
        }
    }

    fn send_message(message: Message, sender_handle: &mut SenderHandle) {
        sender_handle
            .try_send_message(message)
            .expect("message sending receiver should not have been dropped");
    }

    fn handle_codec_event(&mut self, event: CodecEvent) {
        match event {
            CodecEvent::DecodingStarted => {
                let expire_time = Instant::now() + self.decode_timeout_duration;
                self.decoding_timer = Either::A(Delay::new(expire_time));
            }
            CodecEvent::DecodingEnded => {
                self.decoding_timer = Either::B(future::empty());
            }
            _ => {}
        }
    }

    fn handle_message(
        &mut self,
        forwarding_receiver: &mut Option<ForwardingReceiver>,
        message: MessageResult,
    ) -> Result<(), RequestReceiverError> {
        match message {
            Ok(Message::Request(request)) => {
                if let Some(ref request_receiver) = self.request_receiver {
                    let forwarding_receiver = forwarding_receiver
                        .as_mut()
                        .expect("presence of request receiver should imply forwarding");
                    request_receiver.handle_request(forwarding_receiver, request)?;
                }
            }
            Ok(Message::Response(response)) => {
                if let Some(mut response_receiver) = self.response_receiver.take() {
                    response_receiver.handle_response(response);

                    if !response_receiver.should_shutdown() {
                        self.response_receiver = Some(response_receiver);
                    }
                }
            }
            Err(InvalidMessage::InvalidRequest(_)) => return Err(RequestReceiverError::BadRequest),
            Err(InvalidMessage::InvalidResponse(_)) => {}
        }

        Ok(())
    }

    pub fn is_request_receiver_shutdown(&self) -> bool {
        self.request_receiver.is_none()
    }

    pub fn is_response_receiver_shutdown(&self) -> bool {
        self.response_receiver.is_none()
    }

    pub fn number_pending(&self) -> usize {
        if let Some(ref response_receiver) = self.response_receiver {
            response_receiver.number_pending()
        } else {
            0
        }
    }

    pub fn poll_pending_request_update(&mut self) -> Poll<(), ()> {
        if let Some(ref mut response_receiver) = self.response_receiver {
            response_receiver.poll_pending_request_update()
        } else {
            Ok(Async::Ready(()))
        }
    }

    pub fn poll_codec_events(&mut self) {
        while let Async::Ready(item) = self
            .rx_codec_event
            .poll()
            .expect("codec event receiver should not error")
        {
            let event = item.expect("unbounded receiver `rx_codec_event` should not end");
            self.handle_codec_event(event);
        }
    }

    pub fn poll_decoding_timer(&mut self) -> Poll<(), ()> {
        if let Async::Ready(_) = self
            .decoding_timer
            .poll()
            .expect("decoding timer should not error")
        {
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }

    pub fn poll_receive(
        &mut self,
        forwarding_receiver: &mut Option<ForwardingReceiver>,
        sender_handle: &mut Option<SenderHandle>,
    ) -> Poll<(), ProtocolError> {
        while !self.should_shutdown() {
            if let Some(ref forwarding_receiver) = forwarding_receiver {
                if forwarding_receiver.is_full() {
                    return Ok(Async::NotReady);
                }
            }

            match self.stream.poll() {
                Ok(Async::Ready(Some(message))) => {
                    if let Err(error) = self.handle_message(forwarding_receiver, message) {
                        let sender_handle = sender_handle.as_mut().expect(
                            "request receiver error should imply message sending is active",
                        );

                        ReceiverInner::handle_request_receiver_error(error, sender_handle);
                    }
                }
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Ok(Async::Ready(None)) => return Ok(Async::Ready(())),
                Err(error) => {
                    ReceiverInner::handle_protocol_error(&error, sender_handle);
                    return Err(error);
                }
            }
        }

        Ok(Async::Ready(()))
    }

    fn should_shutdown(&self) -> bool {
        self.is_request_receiver_shutdown() && self.is_response_receiver_shutdown()
    }

    pub fn shutdown_request_receiver(&mut self) -> bool {
        self.request_receiver = None;
        self.should_shutdown()
    }

    pub fn shutdown_response_receiver(&mut self) -> bool {
        self.response_receiver = None;
        self.should_shutdown()
    }
}

struct RequestReceiver;

impl RequestReceiver {
    pub fn handle_request(
        &self,
        forwarding_receiver: &mut ForwardingReceiver,
        request: Request<BytesMut>,
    ) -> Result<(), RequestReceiverError> {
        let header_values = request
            .headers()
            .get_all(&HeaderName::CSeq)
            .cloned()
            .collect::<Vec<HeaderValue>>();

        match CSeq::try_from_header_raw(&header_values) {
            Ok(cseq) => {
                let incoming_sequence_number =
                    forwarding_receiver.incoming_sequence_number_or_default(cseq);

                if *(cseq - incoming_sequence_number)
                    > forwarding_receiver.request_buffer_size() as u32
                {
                    Err(RequestReceiverError::NotEnoughBandwidth)
                } else {
                    forwarding_receiver.buffer_request(cseq, request);
                    Ok(())
                }
            }
            Err(_) => Err(RequestReceiverError::BadRequest),
        }
    }
}

struct ForwardingReceiver {
    buffered_requests: HashMap<CSeq, Request<BytesMut>>,
    incoming_sequence_number: Option<CSeq>,
    request_buffer_size: usize,
    tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
}

impl ForwardingReceiver {
    pub fn new(
        tx_incoming_request: Sender<(CSeq, Request<BytesMut>)>,
        request_buffer_size: usize,
    ) -> Self {
        ForwardingReceiver {
            buffered_requests: HashMap::with_capacity(request_buffer_size),
            incoming_sequence_number: None,
            request_buffer_size,
            tx_incoming_request,
        }
    }

    pub fn buffer_request(&mut self, cseq: CSeq, request: Request<BytesMut>) {
        debug_assert!(!self.buffered_requests.contains_key(&cseq));
        debug_assert!(self.buffered_requests.len() < self.request_buffer_size);

        self.buffered_requests.insert(cseq, request);
    }

    pub fn incoming_sequence_number_or_default(&mut self, cseq: CSeq) -> CSeq {
        let incoming_sequence_number = match self.incoming_sequence_number {
            Some(incoming_sequence_number) => incoming_sequence_number,
            None => {
                self.incoming_sequence_number = Some(cseq);
                cseq
            }
        };

        incoming_sequence_number
    }

    pub fn is_full(&self) -> bool {
        self.buffered_requests.len() >= self.request_buffer_size
    }

    pub fn number_buffered(&self) -> usize {
        self.buffered_requests.len()
    }

    pub fn poll_forward(&mut self) -> Poll<(), ()> {
        if let Some(mut incoming_sequence_number) = self.incoming_sequence_number {
            while let Some(request) = self.buffered_requests.remove(&incoming_sequence_number) {
                match self
                    .tx_incoming_request
                    .start_send((incoming_sequence_number, request))
                    .map_err(|_| ())?
                {
                    AsyncSink::Ready => {
                        incoming_sequence_number = incoming_sequence_number.increment()
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

    pub fn request_buffer_size(&self) -> usize {
        self.request_buffer_size
    }
}

struct ResponseReceiver {
    pending_requests: HashMap<CSeq, oneshot::Sender<PendingRequestResponse>>,
    rx_pending_request: Fuse<UnboundedReceiver<PendingRequestUpdate>>,
}

impl ResponseReceiver {
    pub fn new(rx_pending_request: UnboundedReceiver<PendingRequestUpdate>) -> Self {
        ResponseReceiver {
            pending_requests: HashMap::new(),
            rx_pending_request: rx_pending_request.fuse(),
        }
    }

    fn handle_pending_request_update(&mut self, update: PendingRequestUpdate) {
        match update {
            PendingRequestUpdate::AddPendingRequest((cseq, tx_pending_request)) => {
                debug_assert!(!self.pending_requests.contains_key(&cseq));
                self.pending_requests.insert(cseq, tx_pending_request);
            }
            PendingRequestUpdate::RemovePendingRequest(cseq) => {
                debug_assert!(self.pending_requests.contains_key(&cseq));
                self.pending_requests.remove(&cseq);
            }
        }
    }

    pub fn handle_response(&mut self, response: Response<BytesMut>) {
        debug_assert!(!self.should_shutdown());

        let header_values = response
            .headers()
            .get_all(&HeaderName::CSeq)
            .cloned()
            .collect::<Vec<HeaderValue>>();
        let cseq = CSeq::try_from_header_raw(&header_values);

        if let Ok(cseq) = cseq {
            if response.status_code() == StatusCode::Continue {
                let mut remove_pending_request = false;

                {
                    if let Some(mut pending_request) = self.pending_requests.get_mut(&cseq) {
                        let (tx_pending_request, rx_pending_request) = oneshot::channel();

                        if let Err(_) = mem::replace(pending_request, tx_pending_request)
                            .send(PendingRequestResponse::Continue(rx_pending_request))
                        {
                            remove_pending_request = true;
                        }
                    }
                }

                if remove_pending_request {
                    self.pending_requests.remove(&cseq);
                }
            } else if let Some(mut pending_request) = self.pending_requests.remove(&cseq) {
                pending_request
                    .send(PendingRequestResponse::Response(response))
                    .ok();
            }
        }
    }

    pub fn number_pending(&self) -> usize {
        self.pending_requests.len()
    }

    pub fn poll_pending_request_update(&mut self) -> Poll<(), ()> {
        while let Async::Ready(item) = self
            .rx_pending_request
            .poll()
            .expect("unbounded receiver `rx_pending_request` should not error")
        {
            match item {
                Some(update) => self.handle_pending_request_update(update),
                None => {
                    if self.pending_requests.is_empty() {
                        return Ok(Async::Ready(()));
                    }
                }
            }
        }

        Ok(Async::NotReady)
    }

    fn remove_pending_requests(&mut self) {
        self.pending_requests
            .drain()
            .for_each(|(_, tx_pending_request)| {
                tx_pending_request.send(PendingRequestResponse::None).ok();
            });
    }

    pub fn should_shutdown(&self) -> bool {
        self.rx_pending_request.is_done() && self.pending_requests.is_empty()
    }
}

impl Drop for ResponseReceiver {
    fn drop(&mut self) {
        self.poll_pending_request_update().ok();
        self.remove_pending_requests();
    }
}

#[derive(Debug)]
enum RequestReceiverError {
    BadRequest,
    NotEnoughBandwidth,
}
