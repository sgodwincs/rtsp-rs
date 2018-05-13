use bytes::BytesMut;
use futures::future::{empty, loop_fn, Either, Loop};
use futures::prelude::*;
use futures::sync::mpsc::{unbounded, Receiver, Sender, UnboundedReceiver, UnboundedSender};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};
use tokio_executor::{DefaultExecutor, Executor, SpawnError};
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_timer::Delay;

use header::types::CSeq;
use header::{HeaderName, HeaderValue, TypedHeader};
use protocol::{Codec, CodecEvent, Error, InvalidMessage, Message, MessageResult};
use request::Request;
use response::Response;
use status::StatusCode;

pub const DEFAULT_DECODE_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUESTS_BUFFER_SIZE: usize = 10;

#[derive(Debug)]
pub struct Protocol {
    state: Arc<Mutex<ProtocolState>>,
}

impl Protocol {
    pub fn new<IO>(io: IO) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite,
    {
        Protocol::with_config(io, Config::new())
    }

    pub fn with_config<IO>(io: IO, config: Config) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite,
    {
        let mut executor = DefaultExecutor::current();
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_codec_event, rx_codec_event) = unbounded();
        let state = Arc::new(Mutex::new(ProtocolState::new(tx_state_change)));
        let (sink, stream) = io.framed(Codec::with_events(tx_codec_event)).split();

        executor.spawn(Box::new(create_decoding_timer_task(
            state.clone(),
            rx_codec_event,
        )))?;

        Ok(Protocol { state })
    }
}

#[derive(Default)]
pub struct Config;

impl Config {
    pub fn new() -> Self {
        Config
    }
}

#[derive(Clone, Debug)]
pub enum ReadWriteState {
    All,
    Error(Error),
    Request,
    Response,
    None,
}

pub type ReadWriteStatePair = (ReadWriteState, ReadWriteState);

impl ReadWriteState {
    pub fn is_all(&self) -> bool {
        match *self {
            ReadWriteState::All => true,
            _ => false,
        }
    }

    pub fn is_request(&self) -> bool {
        match *self {
            ReadWriteState::Request => true,
            _ => false,
        }
    }

    pub fn is_response(&self) -> bool {
        match *self {
            ReadWriteState::Response => true,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        match *self {
            ReadWriteState::None => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match *self {
            ReadWriteState::Error(_) => true,
            _ => false,
        }
    }

    pub fn requests_allowed(&self) -> bool {
        self.is_all() || self.is_request()
    }

    pub fn responses_allowed(&self) -> bool {
        self.is_all() || self.is_response()
    }
}

impl Eq for ReadWriteState {}

impl PartialEq for ReadWriteState {
    fn eq(&self, other: &ReadWriteState) -> bool {
        use self::ReadWriteState::*;

        match *self {
            All => other.is_all(),
            Request => other.is_request(),
            Response => other.is_response(),
            None => other.is_none(),
            Error(_) => other.is_error(),
        }
    }
}

fn try_update_state(current: &mut ReadWriteState, new: ReadWriteState) -> bool {
    use self::ReadWriteState::*;

    match new {
        All => false,
        Request => if current.is_all() {
            *current = ReadWriteState::Request;
            true
        } else if current.is_response() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        Response => if current.is_all() {
            *current = ReadWriteState::Response;
            true
        } else if current.is_request() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        None => if !current.is_none() && !current.is_error() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        Error(error) => if !current.is_error() {
            *current = ReadWriteState::Error(error);
            true
        } else {
            false
        },
    }
}

#[derive(Debug)]
pub struct ProtocolState {
    read_state: ReadWriteState,
    tx_state_change: UnboundedSender<ReadWriteStatePair>,
    write_state: ReadWriteState,
}

impl ProtocolState {
    pub fn new(tx_state_change: UnboundedSender<ReadWriteStatePair>) -> Self {
        ProtocolState {
            read_state: ReadWriteState::All,
            tx_state_change,
            write_state: ReadWriteState::All,
        }
    }

    pub fn read_state(&self) -> ReadWriteState {
        self.read_state.clone()
    }

    pub fn state(&self) -> ReadWriteStatePair {
        (self.read_state(), self.write_state())
    }

    pub fn update_read_state(&mut self, read_state: ReadWriteState) {
        if try_update_state(&mut self.read_state, read_state) {
            self.tx_state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn update_state(&mut self, read_state: ReadWriteState, write_state: ReadWriteState) {
        self.update_read_state(read_state);
        self.update_write_state(write_state);
    }

    pub fn update_write_state(&mut self, write_state: ReadWriteState) {
        if try_update_state(&mut self.write_state, write_state) {
            self.tx_state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn write_state(&self) -> ReadWriteState {
        self.write_state.clone()
    }
}

fn lock_state(state: &Arc<Mutex<ProtocolState>>) -> MutexGuard<ProtocolState> {
    state.lock().expect("acquiring state lock should not fail")
}

/// Constructs a task that manages the timer for decoding messages.
///
/// If the timer expires, then messages can no longer be read from the connection. But writing
/// responses is still allowed as long as the protocol state had not already passed that point.
///
/// Since the timer is managed via events from the codec, if the stream of the events end, then it
/// is implied that the connection is completely closed.
///
/// This task will only end if the timer expires or if the codec is dropped.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_codec_event` - A stream of [`CodecEvent`]s.
fn create_decoding_timer_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_codec_event: UnboundedReceiver<CodecEvent>,
) -> impl Future<Item = (), Error = ()> {
    loop_fn(
        (state, rx_codec_event, Either::A(empty())),
        |(state, rx_codec_event, timer)| {
            timer
                .select2(rx_codec_event.into_future())
                .map_err(|_| panic!("decode timer and codec event receivers should not error"))
                .and_then(|item| {
                    Ok(match item {
                        Either::A(_) => {
                            // Decoding timer has expired. At this point, we consider any reads from
                            // the connection to be dead. The only writing allowed now are
                            // responses, since we would not be able to get any of the responses
                            // back for any requests we were to send.

                            lock_state(&state).update_state(
                                ReadWriteState::Error(Error::DecodingTimedOut),
                                ReadWriteState::Response,
                            );
                            Loop::Break(())
                        }
                        Either::B(((None, _), _)) => {
                            // The codec that handles decoding and encoding has been dropped. This
                            // implies that the connection is completely closed. It is likely that
                            // the protocol state has already been updated by this point, but we do
                            // it just in case, since this is the final protocol state.

                            lock_state(&state)
                                .update_state(ReadWriteState::None, ReadWriteState::None);
                            Loop::Break(())
                        }
                        Either::B(((Some(event), rx_codec_event), timer)) => match event {
                            CodecEvent::DecodingStarted => {
                                let expire_time = Instant::now() + DEFAULT_DECODE_TIMEOUT_DURATION;
                                let timer = Either::B(Delay::new(expire_time));
                                Loop::Continue((state, rx_codec_event, timer))
                            }
                            CodecEvent::DecodingEnded => {
                                Loop::Continue((state, rx_codec_event, Either::A(empty())))
                            }
                            _ => Loop::Continue((state, rx_codec_event, timer)),
                        },
                    })
                })
        },
    )
}

fn create_request_handler_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_incoming_request: Receiver<Request<BytesMut>>,
    tx_ordered_incoming_request: UnboundedSender<Request<BytesMut>>,
    tx_outgoing_message: UnboundedSender<Message>,
) -> impl Future<Item = (), Error = ()> {
    rx_incoming_request
        .fold(
            (
                state,
                tx_ordered_incoming_request,
                Some(tx_outgoing_message),
                None,
                HashMap::with_capacity(DEFAULT_REQUESTS_BUFFER_SIZE),
            ),
            |(
                state,
                tx_ordered_incoming_request,
                mut tx_outgoing_message,
                mut incoming_sequence_number,
                mut pending_requests,
            ),
             request| {
                let header_values = request
                    .headers()
                    .get_all(HeaderName::CSeq)
                    .iter()
                    .cloned()
                    .collect::<Vec<HeaderValue>>();

                match CSeq::try_from_header_raw(&header_values) {
                    Ok(cseq) => {
                        let mut request_sequence_number = incoming_sequence_number.unwrap_or(cseq);

                        if *(cseq - request_sequence_number) > DEFAULT_REQUESTS_BUFFER_SIZE as u32 {
                            // We received a request with a valid `CSeq` header, but the sequence
                            // number referenced is too far out from the current sequence number.
                            //
                            // This is actually a bit of a break from the specification. The
                            // specification states that requests must be handled in the order of
                            // their `CSeq`, but this is not practical here and opens up possible
                            // resource exhaustion attacks. For example, the protocol will only
                            // process the next expected `CSeq` to abide by the specification. All
                            // other requests will simply be buffered until the next one with the
                            // next `CSeq` has arrived. But the range of possible `CSeq`s is way too
                            // large to buffer per agent, so we only buffer a small amount.

                            if lock_state(&state).write_state().responses_allowed() {
                                let response = Response::builder()
                                    .status_code(StatusCode::ServiceUnavailable)
                                    .build(BytesMut::new())
                                    .expect("service unavailable response should not be invalid");
                                tx_outgoing_message = try_send_message(
                                    tx_outgoing_message,
                                    Message::Response(response),
                                );
                            }
                        } else {
                            pending_requests.insert(cseq, request);

                            while let Some(request) =
                                pending_requests.remove(&request_sequence_number)
                            {
                                if tx_ordered_incoming_request.unbounded_send(request).is_err() {
                                    return Err(());
                                }

                                request_sequence_number = request_sequence_number.increment();
                            }
                        }

                        incoming_sequence_number = Some(request_sequence_number);
                    }
                    Err(_) => {
                        // Either no `CSeq` header was found, or parsing of the header failed.
                        //
                        // To handle this, we send a `400 Bad Request`, but we do not specify a
                        // `CSeq` in the response. Unfortunately, it is not likely the client will
                        // even do anything with this response, since it too does not have a `CSeq`.
                        // For example, if a client using this implementation managed to send a
                        // request with no `CSeq` and received a response with no `CSeq`, it would
                        // just ignore it.
                        //
                        // Also, this message is immediately queued and may not necessarily be the
                        // order in which a client would expect the response, but there is no
                        // avoiding this. `CSeq` must be given in order to demultiplex.

                        if lock_state(&state).write_state().responses_allowed() {
                            let response = Response::builder()
                                .status_code(StatusCode::BadRequest)
                                .build(BytesMut::new())
                                .expect("bad request response should not be invalid");
                            tx_outgoing_message =
                                try_send_message(tx_outgoing_message, Message::Response(response));
                        }
                    }
                }

                // If the receiver for outgoing messages has been dropped, then we can no longer
                // send any messages. So forwarding ordered requests from this task is no longer
                // meaningful, since they cannot be responded to.

                if tx_outgoing_message.is_none() {
                    return Err(());
                }

                Ok((
                    state,
                    tx_ordered_incoming_request,
                    tx_outgoing_message,
                    incoming_sequence_number,
                    pending_requests,
                ))
            },
        )
        .map(|_| ())
        .map_err(|_| ())
}

/// Constructs a task that splits incoming messages into either request or response streams.
///
/// Currently, the given request and response sinks are bounded channels, and this task will block
/// on sending any requests or responses to those sinks. Because of this, it is necessary to ensure
/// that the corresponding receivers are being read frequently enough to avoid blocking when the
/// channel is full.
///
/// This task will end in one of three situations:
///
/// * The incoming message stream has ended.
/// * There was an irrecoverable error during message decoding that produced an error in the message
///   stream.
/// * A protocol state change was detected in which reading is no longer necessary or possible. This
///   can happen from external causes or from within the function. Specifically, if the request and
///   response channel receivers are dropped, this will implicitly cause the corresponding state
///   change thus forcing this task to end.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_state_change` - A stream of [`ReadWriteStatePair`] protocol state changes.
/// * `stream` - The stream of incoming messages.
/// * `tx_incoming_request` - The sink where requests extracted from the message stream will be
///   sent.
/// * `tx_incoming_response` - The sink where responses extracted from the message stream will be
///   sent.
/// * `tx_outgoing_message` - A sink where messages to be sent to the connected host are sent. This
///   is needed in order to send `400 Bad Request` responses in response to invalid messages.
fn create_split_messages_task<S>(
    state: Arc<Mutex<ProtocolState>>,
    rx_state_change: UnboundedReceiver<ReadWriteStatePair>,
    stream: S,
    tx_incoming_request: Sender<Request<BytesMut>>,
    tx_incoming_response: Sender<Response<BytesMut>>,
    tx_outgoing_message: UnboundedSender<Message>,
) -> impl Future<Item = (), Error = ()>
where
    S: Stream<Item = MessageResult, Error = Error>,
{
    fn forward_message<M>(
        tx_incoming_message: Option<Sender<M>>,
        message: M,
        state: &Arc<Mutex<ProtocolState>>,
        error_state: (Option<ReadWriteState>, Option<ReadWriteState>),
    ) -> Option<Sender<M>> {
        tx_incoming_message.and_then(|tx_incoming_message| {
            match tx_incoming_message.send(message).wait() {
                Ok(tx_incoming_message) => Some(tx_incoming_message),
                Err(_) => {
                    let mut locked_state = lock_state(state);

                    if let Some(new_read_state) = error_state.0 {
                        locked_state.update_read_state(new_read_state);
                    }

                    if let Some(new_write_state) = error_state.1 {
                        locked_state.update_write_state(new_write_state);
                    }

                    None
                }
            }
        })
    }

    loop_fn(
        (
            state,
            rx_state_change.into_future(),
            stream.into_future(),
            Some(tx_incoming_request),
            Some(tx_incoming_response),
            Some(tx_outgoing_message),
        ),
        |(
            state,
            rx_state_change,
            stream,
            mut tx_incoming_request,
            mut tx_incoming_response,
            mut tx_outgoing_message,
        )| {
            stream.select2(rx_state_change).then(move |result| {
                Ok(match result {
                    Ok(Either::A(((item, stream), rx_state_change))) => {
                        match item {
                            Some(result) => {
                                match result {
                                    Ok(Message::Request(request)) => {
                                        // Forward the request to the request stream. If an error
                                        // occurs (implying that the request stream has been
                                        // dropped), then we can only read responses from now on. We
                                        // do not change the write state, since even though no new
                                        // responses will be made, there still may be responses
                                        // pending from previous requests.

                                        tx_incoming_request = forward_message(
                                            tx_incoming_request,
                                            request,
                                            &state,
                                            (Some(ReadWriteState::Response), None),
                                        );
                                    }
                                    Ok(Message::Response(response)) => {
                                        // Forward the response to the response stream. If an error
                                        // occurs (implying that the response stream has been
                                        // dropped), then we can only read requests and send
                                        // responses.

                                        tx_incoming_response = forward_message(
                                            tx_incoming_response,
                                            response,
                                            &state,
                                            (
                                                Some(ReadWriteState::Request),
                                                Some(ReadWriteState::Response),
                                            ),
                                        );
                                    }
                                    Err(InvalidMessage::InvalidRequest(_)) => {
                                        // We received a request that was invalid, but it still was
                                        // able to be decoded. We send a `400 Bad Request` to deal
                                        // with this. This is one situation, however, in which the
                                        // order that requests are handled is not based on `CSeq`.
                                        // Even if the message had a valid `CSeq` header, it will
                                        // not be inspected, since at least one part of the message
                                        // was syntactically incorrect. As a result, the receiving
                                        // agent has to manage mapping a response with no `CSeq` to
                                        // its respective request. This is unlikely, and in general
                                        // not possible if proxies are involved, since responses can
                                        // be received out of order.

                                        if lock_state(&state).write_state().responses_allowed() {
                                            let response = Response::builder()
                                                .status_code(StatusCode::BadRequest)
                                                .build(BytesMut::new())
                                                .expect(
                                                    "bad request response should not be invalid",
                                                );
                                            tx_outgoing_message = try_send_message(
                                                tx_outgoing_message,
                                                Message::Response(response),
                                            );
                                        }
                                    }
                                    Err(InvalidMessage::InvalidResponse(_)) => {
                                        // Received an invalid response, but the only appropriate
                                        // action here is to just ignore it.
                                    }
                                }
                            }
                            None => {
                                // The incoming message stream has ended. We can only write
                                // responses from this point forward.

                                lock_state(&state)
                                    .update_state(ReadWriteState::None, ReadWriteState::Response);
                                return Ok(Loop::Break(()));
                            }
                        }

                        Loop::Continue((
                            state,
                            rx_state_change,
                            stream.into_future(),
                            tx_incoming_request,
                            tx_incoming_response,
                            tx_outgoing_message,
                        ))
                    }
                    Ok(Either::B(((new_state, rx_state_change), stream))) => {
                        match new_state.expect("state change receiver should not end") {
                            // We can no longer read anything
                            (ReadWriteState::None, _) | (ReadWriteState::Error(_), _) => {
                                return Ok(Loop::Break(()));
                            }
                            // We can no longer read responses
                            (ReadWriteState::Request, _) => tx_incoming_response = None,
                            // We can no longer read requests
                            (ReadWriteState::Response, _)
                            | (_, ReadWriteState::None)
                            | (_, ReadWriteState::Error(_))
                            | (_, ReadWriteState::Response) => tx_incoming_request = None,
                            _ => (),
                        }

                        Loop::Continue((
                            state,
                            rx_state_change.into_future(),
                            stream,
                            tx_incoming_request,
                            tx_incoming_response,
                            tx_outgoing_message,
                        ))
                    }
                    Err(Either::A(((error, _), _))) => {
                        // There was an error reading from the message stream that was
                        // irrecoverable. We can no longer read anything, but we may be able to
                        // still write responses. Requests cannot be written, since we would not be
                        // able to read their responses.

                        lock_state(&state)
                            .update_state(ReadWriteState::Error(error), ReadWriteState::Response);
                        Loop::Break(())
                    }
                    Err(Either::B(_)) => panic!("state change receiver should not error"),
                })
            })
        },
    )
}

fn try_send_message(
    tx_outgoing_message: Option<UnboundedSender<Message>>,
    message: Message,
) -> Option<UnboundedSender<Message>> {
    tx_outgoing_message.and_then(|tx_outgoing_message| {
        tx_outgoing_message
            .unbounded_send(message)
            .ok()
            .map(|_| tx_outgoing_message)
    })
}
