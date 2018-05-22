use bytes::BytesMut;
use futures::future::{self, lazy, loop_fn, Either, Loop};
use futures::sync::mpsc::{
    channel, unbounded, Receiver, Sender, UnboundedReceiver, UnboundedSender,
};
use futures::sync::oneshot;
use futures::{stream, Future, Sink, Stream};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::io;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};
use tokio_executor::{DefaultExecutor, Executor, SpawnError};
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_timer::Delay;

use header::types::CSeq;
use header::{HeaderName, HeaderValue, TypedHeader};
use protocol::{Codec, CodecEvent, InvalidMessage, Message, MessageResult, ProtocolError};
use request::Request;
use response::Response;
use status::StatusCode;

pub const DEFAULT_DECODE_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUEST_BUFFER_SIZE: usize = 10;
pub const DEFAULT_REQUEST_TIMEOUT_DURATION: Duration = Duration::from_secs(5);
pub const DEFAULT_RESPONSE_BUFFER_SIZE: usize = 10;
pub const DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION: Duration = Duration::from_secs(10);

#[derive(Debug)]
pub struct Protocol {
    request_default_timeout_duration: Option<Duration>,
    rx_ordered_incoming_request: UnboundedReceiver<Request<BytesMut>>,
    sequence_number: CSeq,
    state: Arc<Mutex<ProtocolState>>,
    tx_pending_request: Arc<Mutex<UnboundedSender<PendingRequestUpdate>>>,
    tx_outgoing_message: UnboundedSender<Message>,
    tx_shutdown: Option<oneshot::Sender<ShutdownType>>,
}

impl Protocol {
    pub fn new<IO>(io: IO) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite + Send + 'static,
    {
        Protocol::with_config(io, Config::default())
    }

    pub fn with_config<IO>(io: IO, mut config: Config) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite + Send + 'static,
    {
        // Create all channels needed for communication from task to task and from protocol to task.
        // This is pretty terrible looking, so it would be nice to come up with a better structure.

        let (tx_codec_event, rx_codec_event) = unbounded();
        let (tx_pending_request, rx_pending_request) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(config.request_buffer_size);
        let (tx_incoming_response, rx_incoming_response) = channel(config.response_buffer_size);
        let (tx_state_change_1, rx_state_change_1) = unbounded();
        let (tx_state_change_2, rx_state_change_2) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();
        let (tx_ordered_incoming_request, rx_ordered_incoming_request) = unbounded();
        let (tx_shutdown, rx_shutdown) = oneshot::channel();

        let mut executor = DefaultExecutor::current();
        let state = Arc::new(Mutex::new(ProtocolState::new(vec![
            tx_state_change_1,
            tx_state_change_2,
        ])));
        let (sink, stream) = io.framed(Codec::with_events(tx_codec_event)).split();

        executor.spawn(Box::new(
            create_decoding_timer_task(
                state.clone(),
                rx_codec_event,
                config.decode_timeout_duration,
            ).then(|_| {
                println!("Decoding task is done!");
                Ok(())
            }),
        ))?;
        executor.spawn(Box::new(
            create_request_handler_task(
                state.clone(),
                rx_incoming_request,
                tx_ordered_incoming_request,
                tx_outgoing_message.clone(),
                config.request_buffer_size,
            ).then(|_| {
                println!("Request handler task is done!");
                Ok(())
            }),
        ))?;
        executor.spawn(create_response_handler_task(
            state.clone(),
            rx_incoming_response,
            rx_pending_request,
        ))?;
        executor.spawn(Box::new(
            create_send_messages_task(state.clone(), rx_state_change_1, sink, rx_outgoing_message)
                .then(|_| {
                    println!("Message sending task is done!");
                    Ok(())
                }),
        ))?;
        executor.spawn(Box::new(
            create_split_messages_task(
                state.clone(),
                rx_state_change_2,
                stream,
                tx_incoming_request,
                tx_incoming_response,
                tx_outgoing_message.clone(),
            ).then(|_| {
                println!("Message splitting task is done!");
                Ok(())
            }),
        ))?;

        let state_clone = state.clone();
        let shutdown_future = config
            .shutdown_future
            .take()
            .expect("config shutdown future should not be `None`");

        executor.spawn(Box::new(
            rx_shutdown
                .map_err(|_| ())
                .select(shutdown_future)
                .then(move |result| {
                    let shutdown_type = match result {
                        Ok((shutdown_type, _)) => shutdown_type,
                        Err(_) => ShutdownType::Hard,
                    };

                    println!("SHUTDOWN OCCURRING! {:?}", shutdown_type);
                    match shutdown_type {
                        ShutdownType::Hard => {
                            lock_state(&state_clone)
                                .update_state(ReadState::None, WriteState::None);
                        }
                        ShutdownType::Soft(_) => {
                            lock_state(&state_clone)
                                .update_state(ReadState::Response, WriteState::Response);
                        }
                    }

                    Ok::<_, ()>(())
                }),
        ))?;

        Ok(Protocol {
            request_default_timeout_duration: config.request_default_timeout_duration,
            rx_ordered_incoming_request,
            sequence_number: CSeq::try_from(0).expect("sequence number `0` should not be invalid"),
            state,
            tx_outgoing_message: tx_outgoing_message,
            tx_pending_request: Arc::new(Mutex::new(tx_pending_request)),
            tx_shutdown: Some(tx_shutdown),
        })
    }

    pub fn is_shutdown(&self) -> bool {
        let state = lock_state(&self.state).state();
        state.0.is_none() || state.0.is_error() && state.1.is_none() || state.1.is_error()
    }

    pub fn read_state(&self) -> ReadState {
        self.state().0
    }

    pub fn send_request<B>(
        &mut self,
        request: Request<B>,
        // options: RequestOptions,
    ) -> impl Future<Item = Response<BytesMut>, Error = ProtocolError>
    where
        B: AsRef<[u8]>,
    {
        let error_closure = || Err(ProtocolError::Closed);

        // Check if the write state allows sending requests. We should also probably check whether
        // the read state allows reading responses, but if everything is working correctly, it
        // should agree with the write state.

        if !self.write_state().requests_allowed() {
            return Either::A(lazy(error_closure));
        }

        let sequence_number = self.sequence_number;
        self.sequence_number = self.sequence_number.increment();

        let mut request = request.map(|body| BytesMut::from(body.as_ref()));
        let cseq_header = CSeq::to_header_raw(&sequence_number)
            .into_iter()
            .nth(0)
            .expect("`CSeq` header should always have one header");
        request.headers_mut().insert(HeaderName::CSeq, cseq_header);

        let (tx_response, rx_response) = oneshot::channel();
        let update = PendingRequestUpdate::AddPendingRequest((sequence_number, tx_response));

        if let Err(_) = self
            .tx_pending_request
            .lock()
            .expect("acquiring pending request sender should not fail")
            .unbounded_send(update)
        {
            // This really should not happen, since we checked the write state above. But if it
            // does, make sure that the state is updated correctly.

            lock_state(&self.state).update_state(ReadState::Request, WriteState::Response);
            return Either::A(lazy(error_closure));
        }

        if let Err(_) = self
            .tx_outgoing_message
            .unbounded_send(Message::Request(request))
        {
            // This also should not happen. And if it does, we need to remove the pending request we
            // just sent. Also update the state to be what it should be.

            self.tx_pending_request
                .lock()
                .expect("acquiring pending request sender should not fail")
                .unbounded_send(PendingRequestUpdate::RemovePendingRequest(sequence_number))
                .ok();
            lock_state(&self.state).update_state(ReadState::Response, WriteState::None);
            return Either::A(lazy(error_closure));
        }

        Either::B(loop_fn(
            (
                rx_response,
                self.tx_pending_request.clone(),
                self.request_default_timeout_duration,
                sequence_number,
            ),
            |(rx_response, tx_pending_request, request_timeout_duration, sequence_number)| {
                let timer = if let Some(duration) = request_timeout_duration {
                    let expire_time = Instant::now() + duration;
                    Either::A(Delay::new(expire_time))
                } else {
                    Either::B(future::empty())
                };

                rx_response.select2(timer).then(move |result| {
                    let response = match result {
                        Ok(Either::A((response, _))) => Some(response),
                        Err(Either::B(_)) => Some(PendingRequestResponse::None),
                        _ => None,
                    };

                    match response {
                        Some(PendingRequestResponse::Continue(rx_response)) => {
                            Ok(Loop::Continue((
                                rx_response,
                                tx_pending_request,
                                request_timeout_duration,
                                sequence_number,
                            )))
                        }
                        Some(PendingRequestResponse::None) => Err(ProtocolError::Cancelled),
                        Some(PendingRequestResponse::Response(response)) => {
                            Ok(Loop::Break(response))
                        }
                        None => {
                            tx_pending_request
                                .lock()
                                .expect("acquiring pending request sender should not fail")
                                .unbounded_send(PendingRequestUpdate::RemovePendingRequest(
                                    sequence_number,
                                ))
                                .ok();
                            Err(ProtocolError::RequestTimedOut)
                        }
                    }
                })
            },
        ))
    }

    fn send_response<B>(&self, response: Response<B>) -> Result<(), ProtocolError>
    where
        B: AsRef<[u8]>,
    {
        let response = response.map(|body| BytesMut::from(body.as_ref()));

        self.tx_outgoing_message
            .unbounded_send(Message::Response(response))
            .map_err(|_| ProtocolError::Closed)
    }

    pub fn state(&self) -> ReadWriteStatePair {
        lock_state(&self.state).state()
    }

    pub fn write_state(&self) -> WriteState {
        self.state().1
    }
}

impl Drop for Protocol {
    fn drop(&mut self) {
        // Protocol object is being dropped, start a soft shutdown.

        if let Some(tx_shutdown) = self.tx_shutdown.take() {
            tx_shutdown
                .send(ShutdownType::Soft(DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION))
                .ok();
        }
    }
}

pub struct Config {
    pub(crate) decode_timeout_duration: Duration,
    pub(crate) request_buffer_size: usize,
    pub(crate) request_default_timeout_duration: Option<Duration>,
    pub(crate) response_buffer_size: usize,
    pub(crate) shutdown_future: Option<Box<Future<Item = ShutdownType, Error = ()> + Send>>,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    pub fn decode_timeout_duration(&self) -> Duration {
        self.decode_timeout_duration
    }

    pub fn request_buffer_size(&self) -> usize {
        self.request_buffer_size
    }

    pub fn request_default_timeout_duration(&self) -> Option<Duration> {
        self.request_default_timeout_duration
    }

    pub fn response_buffer_size(&self) -> usize {
        self.response_buffer_size
    }
}

impl Default for Config {
    fn default() -> Self {
        Config::builder()
            .build()
            .expect("default config builder options should be valid")
    }
}

pub struct ConfigBuilder {
    error: Option<BuilderError>,
    decode_timeout_duration: Duration,
    request_buffer_size: usize,
    request_default_timeout_duration: Option<Duration>,
    response_buffer_size: usize,
    shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send>,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        ConfigBuilder::default()
    }

    pub fn build(self) -> Result<Config, BuilderError> {
        if let Some(error) = self.error {
            return Err(error);
        }

        Ok(Config {
            decode_timeout_duration: self.decode_timeout_duration,
            request_buffer_size: self.request_buffer_size,
            request_default_timeout_duration: self.request_default_timeout_duration,
            response_buffer_size: self.response_buffer_size,
            shutdown_future: Some(self.shutdown_future),
        })
    }

    pub fn decode_timeout_duration(&mut self, duration: Duration) -> &mut Self {
        if duration.as_secs() == 0 {
            self.error = Some(BuilderError::InvalidDecodeTimeoutDuration);
        } else {
            self.decode_timeout_duration = duration;
        }

        self
    }

    pub fn request_buffer_size(&mut self, size: usize) -> &mut Self {
        if size == 0 {
            self.error = Some(BuilderError::InvalidRequestBufferSize)
        } else {
            self.request_buffer_size = size;
        }

        self
    }

    pub fn request_default_timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        match duration {
            Some(ref duration) if duration.as_secs() == 0 => {
                self.error = Some(BuilderError::InvalidRequestDefaultTimeoutDuration);
            }
            _ => self.request_default_timeout_duration = duration,
        }

        self
    }

    pub fn response_buffer_size(&mut self, size: usize) -> &mut Self {
        if size == 0 {
            self.error = Some(BuilderError::InvalidResponseBufferSize);
        } else {
            self.response_buffer_size = size;
        }

        self
    }

    pub fn shutdown_future<F>(&mut self, future: F) -> &mut Self
    where
        F: Future<Item = ShutdownType, Error = ()> + Send + 'static,
    {
        self.shutdown_future = Box::new(future);
        self
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        ConfigBuilder {
            error: None,
            decode_timeout_duration: DEFAULT_DECODE_TIMEOUT_DURATION,
            request_buffer_size: DEFAULT_REQUEST_BUFFER_SIZE,
            request_default_timeout_duration: Some(DEFAULT_REQUEST_TIMEOUT_DURATION),
            response_buffer_size: DEFAULT_RESPONSE_BUFFER_SIZE,
            shutdown_future: Box::new(future::empty()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuilderError {
    InvalidDecodeTimeoutDuration,
    InvalidRequestBufferSize,
    InvalidRequestDefaultTimeoutDuration,
    InvalidResponseBufferSize,
}

impl fmt::Display for BuilderError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for BuilderError {
    fn description(&self) -> &str {
        use self::BuilderError::*;

        match self {
            InvalidDecodeTimeoutDuration => "invalid decode timeout duration",
            InvalidRequestBufferSize => "invalid request buffer size",
            InvalidRequestDefaultTimeoutDuration => "invalid request default timeout duration",
            InvalidResponseBufferSize => "invalid response buffer size",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownType {
    Hard,
    Soft(Duration),
}

macro_rules! state_type {
    ($(#[$docs:meta])* $type_name:ident) => {
        $(#[$docs])*
        #[derive(Clone, Debug)]
        pub enum $type_name {
            All,
            Error(ProtocolError),
            Request,
            Response,
            None,
        }

        impl $type_name {
            pub fn try_update_state(&mut self, new: $type_name) -> bool {
                use self::$type_name::*;

                match new {
                    All => false,
                    Request => if self.is_all() {
                        *self = Request;
                        true
                    } else if self.is_response() {
                        *self = None;
                        true
                    } else {
                        false
                    },
                    Response => if self.is_all() {
                        *self = Response;
                        true
                    } else if self.is_request() {
                        *self = None;
                        true
                    } else {
                        false
                    },
                    None => if !self.is_none() && !self.is_error() {
                        *self = None;
                        true
                    } else {
                        false
                    },
                    Error(error) => if !self.is_error() {
                        *self = Error(error);
                        true
                    } else {
                        false
                    },
                }
            }

            pub fn is_all(&self) -> bool {
                match *self {
                    $type_name::All => true,
                    _ => false,
                }
            }

            pub fn is_request(&self) -> bool {
                match *self {
                    $type_name::Request => true,
                    _ => false,
                }
            }

            pub fn is_response(&self) -> bool {
                match *self {
                    $type_name::Response => true,
                    _ => false,
                }
            }

            pub fn is_none(&self) -> bool {
                match *self {
                    $type_name::None => true,
                    _ => false,
                }
            }

            pub fn is_error(&self) -> bool {
                match *self {
                    $type_name::Error(_) => true,
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

        impl Eq for $type_name {}

        impl PartialEq for $type_name {
            fn eq(&self, other: &$type_name) -> bool {
                use self::$type_name::*;

                match *self {
                    All => other.is_all(),
                    Request => other.is_request(),
                    Response => other.is_response(),
                    None => other.is_none(),
                    Error(_) => other.is_error(),
                }
            }
        }
    };
}

state_type!(ReadState);
state_type!(WriteState);

pub type ReadWriteStatePair = (ReadState, WriteState);

#[derive(Debug)]
pub struct ProtocolState {
    read_state: ReadState,
    state_changes: Vec<UnboundedSender<ReadWriteStatePair>>,
    write_state: WriteState,
}

impl ProtocolState {
    pub fn new(state_changes: Vec<UnboundedSender<ReadWriteStatePair>>) -> Self {
        ProtocolState {
            read_state: ReadState::All,
            state_changes,
            write_state: WriteState::All,
        }
    }

    fn send_state_change(&self) {
        for state_change in self.state_changes.iter() {
            state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn read_state(&self) -> ReadState {
        self.read_state.clone()
    }

    pub fn state(&self) -> ReadWriteStatePair {
        (self.read_state(), self.write_state())
    }

    pub fn update_read_state(&mut self, read_state: ReadState) {
        if self.read_state.try_update_state(read_state) {
            self.send_state_change();
        }
    }

    pub fn update_state(&mut self, read_state: ReadState, write_state: WriteState) {
        let mut state_changed = false;

        if self.read_state.try_update_state(read_state) {
            state_changed = true;
        }

        if self.write_state.try_update_state(write_state) {
            state_changed = true;
        }

        if state_changed {
            self.send_state_change();
        }
    }

    pub fn update_write_state(&mut self, write_state: WriteState) {
        if self.write_state.try_update_state(write_state) {
            self.send_state_change();
        }
    }

    pub fn write_state(&self) -> WriteState {
        self.write_state.clone()
    }
}

#[derive(Debug)]
enum PendingRequestResponse {
    Continue(oneshot::Receiver<PendingRequestResponse>),
    None,
    Response(Response<BytesMut>),
}

impl PendingRequestResponse {
    pub fn is_continue(&self) -> bool {
        match self {
            PendingRequestResponse::Continue(_) => true,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        match self {
            PendingRequestResponse::None => true,
            _ => false,
        }
    }

    pub fn is_response(&self) -> bool {
        match self {
            PendingRequestResponse::Response(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum PendingRequestUpdate {
    AddPendingRequest((CSeq, oneshot::Sender<PendingRequestResponse>)),
    RemovePendingRequest(CSeq),
}

fn lock_state(state: &Arc<Mutex<ProtocolState>>) -> MutexGuard<ProtocolState> {
    state.lock().expect("acquiring state lock should not fail")
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

/// Constructs a task that manages the timer for decoding messages.
///
/// If the timer expires, then messages can no longer be read from the connection. But writing
/// responses is still allowed as long as the protocol state had not already passed that point.
///
/// Since the timer is managed via events from the codec, if the stream of the events end, then it
/// is implied that the connection is completely closed.
///
/// This task will only end if the timer expires or if the codec is dropped. The codec will be
/// dropped when both the message splitting and sending tasks have ended.
///
/// TODO: We really do not have to wait for the codec to have been dropped to end this task. The
/// weaker condition of having the message splitting task end would work as well. This can be done
/// by watching for state changes. I am currently not adding this as it is not strictly necessary
/// and also complicates the code more.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_codec_event` - A stream of [`CodecEvent`]s.
/// * `decode_timeout_duration` - The duration of time that must pass for decoding to timeout.
fn create_decoding_timer_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_codec_event: UnboundedReceiver<CodecEvent>,
    decode_timeout_duration: Duration,
) -> impl Future<Item = (), Error = ()> {
    loop_fn(
        (state, rx_codec_event, Either::A(future::empty())),
        move |(state, rx_codec_event, timer)| {
            timer
                .select2(rx_codec_event.into_future())
                .map_err(|_| panic!("decode timer and codec event receivers should not error"))
                .and_then(move |item| {
                    Ok(match item {
                        Either::A(_) => {
                            // Decoding timer has expired. At this point, we consider any reads from
                            // the connection to be dead. The only writing allowed now are
                            // responses, since we would not be able to get any of the responses
                            // back for any requests we were to send.

                            lock_state(&state).update_state(
                                ReadState::Error(ProtocolError::DecodingTimedOut),
                                WriteState::Response,
                            );
                            Loop::Break(())
                        }
                        Either::B(((None, _), _)) => {
                            // The codec that handles decoding and encoding has been dropped. This
                            // implies that the connection is completely closed. It is likely that
                            // the protocol state has already been updated by this point, but we do
                            // it just in case, since this is the final protocol state.

                            lock_state(&state).update_state(ReadState::None, WriteState::None);
                            Loop::Break(())
                        }
                        Either::B(((Some(event), rx_codec_event), timer)) => match event {
                            CodecEvent::DecodingStarted => {
                                let expire_time = Instant::now() + decode_timeout_duration;
                                let timer = Either::B(Delay::new(expire_time));
                                Loop::Continue((state, rx_codec_event, timer))
                            }
                            CodecEvent::DecodingEnded => {
                                Loop::Continue((state, rx_codec_event, Either::A(future::empty())))
                            }
                            _ => Loop::Continue((state, rx_codec_event, timer)),
                        },
                    })
                })
        },
    )
}

/// Constructs a task that orders incoming requests based on their `CSeq` header.
///
/// This task is not strictly necessary in its entirety. While we do need to parse the `CSeq` header
/// to make sure it is valid, ordering incoming requests should not be necessary when a reliable,
/// in-order transport like TCP is used. However, the specification states that we must process
/// requests in order of their sequence numbers in case support for out-of-order transports were to
/// be added (e.g. UDP).
///
/// Also, as it is configured in the protocol's [`Config`], the size of the buffer of incoming
/// requests needs to be set. If a request comes with a sequence number that is too far from the
/// current sequence number, it will not be processed and will be responded to with a
/// `503 Service Unavailable` response.
///
/// As for the starting sequence number, the specification allows the sender to start with any
/// number, so the first request's sequence number will be the starting sequence number. This does
/// have some problems if unreliable transports are to be used, but the specification will specify
/// better error handling in the case that such support is added.
///
/// This task will end in one of three situations:
///
/// * The incoming request stream ends.
/// * The ordered incoming request channel receiver is dropped.
/// * The outgoing message channel receiver is dropped. This implies that requests and responses can
///   no longer be written, so there is no point in ordering requests anymore since we cannot
///   response to them.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_incoming_request` - A stream of [`Request`]s that are to be ordered.
/// * `tx_incoming_response` - The sink where [`Request`]s will be sent in order based on their
///   `CSeq` header.
/// * `tx_outgoing_message` - A sink where messages to be sent to the connected host are sent. This
///   is needed in order to send `400 Bad Request` and `503 Service Unavailable` responses.
fn create_request_handler_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_incoming_request: Receiver<Request<BytesMut>>,
    tx_ordered_incoming_request: UnboundedSender<Request<BytesMut>>,
    tx_outgoing_message: UnboundedSender<Message>,
    request_buffer_size: usize,
) -> impl Future<Item = (), Error = ()> {
    rx_incoming_request
        .fold(
            (
                state,
                tx_ordered_incoming_request,
                Some(tx_outgoing_message),
                None,
                HashMap::with_capacity(request_buffer_size),
            ),
            move |(
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
                        // The initial sequence number is defined by the client's first request. So,
                        // if we do not currently have a sequence number yet, the one in the request
                        // will be used.

                        let mut request_sequence_number = incoming_sequence_number.unwrap_or(cseq);

                        if *(cseq - request_sequence_number) > request_buffer_size as u32 {
                            // We received a request with a valid `CSeq` header, but the sequence
                            // number referenced is too far out from the current sequence number.
                            //
                            // This is actually a bit of a break from the specification. The
                            // specification states that requests must be handled in the order of
                            // their `CSeq`, but this is not practical here and opens up possible
                            // resource exhaustion attacks. The protocol will only process the next
                            // expected `CSeq` to abide by the specification. All other requests
                            // will simply be buffered until the next one with the next `CSeq` has
                            // arrived. But the range of possible `CSeq`s is way too large to buffer
                            // per agent, so we only buffer a small amount.

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
                            // Make sure we are processing requests in the order of their `CSeq`
                            // headers. This is actually redundant if an reliable transport is used
                            // (e.g. TCP), but the specification states that implementations must do
                            // so in case support for unreliable or out-of-order transports are to
                            // be added.

                            pending_requests.insert(cseq, request);

                            while let Some(request) =
                                pending_requests.remove(&request_sequence_number)
                            {
                                // An error here means that the receiver for the channel has been
                                // dropped, so we no longer will handle anymore requests.

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
        .then(|_| Ok(()))
}

/// Constructs a task that maps incoming responses to pending requests based on the `CSeq` header.
///
/// Note that the order in which responses are returned to pending requests is completely dependent
/// on the order in which they arrive. Although support for buffering responses so that they are
/// returned in the correct order could be added, there are some complications with this.
/// Specifically, determining how to deal with request timeouts in situations where head-of-line
/// blocking is occurring is non-trivial (at least from what I can tell).
///
/// TODO: The reason why this is the only task that returns a boxed [`Future`] is due to an
/// [ICE](https://github.com/rust-lang/rust/issues/48751). Once the ICE has been fixed, it can
/// return `impl Future`.
///
/// # Arguments
///
/// * `rx_incoming_response` - A stream of [`Response`]s that are to be mapped to pending requests.
/// * `rx_pending_request` - A stream of [`PendingRequestUpdate`]s that add and remove pending
///   requests that are waiting to be mapped responses.
fn create_response_handler_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_incoming_response: Receiver<Response<BytesMut>>,
    rx_pending_request: UnboundedReceiver<PendingRequestUpdate>,
) -> Box<Future<Item = (), Error = ()> + Send + 'static> {
    fn handle_response(
        response: Option<Response<BytesMut>>,
        pending_requests: &mut HashMap<CSeq, Option<oneshot::Sender<PendingRequestResponse>>>,
    ) -> bool {
        match response {
            Some(response) => {
                let header_values = response
                    .headers()
                    .get_all(HeaderName::CSeq)
                    .iter()
                    .cloned()
                    .collect::<Vec<HeaderValue>>();
                let cseq = CSeq::try_from_header_raw(&header_values);

                // Make sure the `CSeq` was valid (and that there was only one) and that a request
                // is pending with the given value.

                if let Ok(cseq) = cseq {
                    if response.status_code() == StatusCode::Continue {
                        // Received a `100 Continue` response meaning we should expect the actual
                        // response a bit later. But we want to make sure that any timers for the
                        // request do not timeout, so we notify them of the continue.
                        //
                        // TODO: When NLL is stable, restructure this.

                        let mut remove_pending_request = false;

                        {
                            let pending_request = pending_requests.get_mut(&cseq);

                            if let Some(pending_request) = pending_request {
                                // Since we are using oneshots here, we need to create a new channel
                                // to be used and send it as well.

                                let (tx_pending_request, rx_pending_request) = oneshot::channel();
                                let result = pending_request
                                    .take()
                                    .unwrap()
                                    .send(PendingRequestResponse::Continue(rx_pending_request));

                                // We do care if it fails here, since if it fails, we need to remove
                                // the pending request from the hashmap.

                                match result {
                                    Ok(_) => *pending_request = Some(tx_pending_request),
                                    Err(_) => remove_pending_request = true,
                                }
                            }
                        }

                        if remove_pending_request {
                            pending_requests.remove(&cseq);
                        }
                    } else {
                        // Received a final response.

                        let pending_request = pending_requests.remove(&cseq);

                        if let Some(mut pending_request) = pending_request {
                            // We do not really care if this fails. It would only fail in the case
                            // where the receiver has been dropped, but either way, we can get rid
                            // of the pending request.

                            pending_request
                                .take()
                                .unwrap()
                                .send(PendingRequestResponse::Response(response))
                                .ok();
                        }
                    }
                }

                true
            }
            None => {
                // The incoming response stream has ended, so all currently pending requests will
                // not be responded to. Instead of sending a response to them, we will just send
                // `PendingRequestResponse::None` indicating that no response will be coming.

                for (_, tx_pending_request) in pending_requests.iter_mut() {
                    // Even though the value type for the linked hash map is an `Option<T>`, it will
                    // never be `None` at this point. It is only done this way because the oneshot
                    // channel `send` requires ownership and so we are unable to deal without the
                    // option. If `LinkedHashMap` had a `drain` function, we could, but it does not.
                    //
                    // We also do not really care if this fails. If would only fail in the case
                    // where the receiver has been dropped, but either way, we can get rid of the
                    // pending request.

                    tx_pending_request
                        .take()
                        .unwrap()
                        .send(PendingRequestResponse::None)
                        .ok();
                }

                false
            }
        }
    }

    Box::new(
        loop_fn(
            (
                state,
                rx_incoming_response.into_future(),
                Some(rx_pending_request.into_future()),
                HashMap::new(),
            ),
            |(state, rx_incoming_response, rx_pending_request, mut pending_requests)| {
                // TODO: Once async/await are usable, avoid this branch by using `empty`. It should be
                // possible to do it without async/await, but I was not able to get it to work.

                if let Some(rx_pending_request) = rx_pending_request {
                    Either::A(
                        rx_incoming_response
                            .select2(rx_pending_request)
                            .and_then(|item| match item {
                                Either::A((
                                    (response, rx_incoming_response),
                                    rx_pending_request,
                                )) => {
                                    if handle_response(response, &mut pending_requests) {
                                        Ok(Loop::Continue((
                                            state,
                                            rx_incoming_response.into_future(),
                                            Some(rx_pending_request),
                                            pending_requests,
                                        )))
                                    } else {
                                        lock_state(&state)
                                            .update_state(ReadState::Request, WriteState::Response);
                                        Ok(Loop::Break(()))
                                    }
                                }
                                Either::B(((update, rx_pending_request), rx_incoming_response)) => {
                                    if let Some(update) = update {
                                        match update {
                                            // A new request has been made that is waiting for a
                                            // response. When the response has been received, the given
                                            // oneshot channel will be used to forward the response.
                                            PendingRequestUpdate::AddPendingRequest((
                                                cseq,
                                                tx_pending_request,
                                            )) => {
                                                pending_requests
                                                    .insert(cseq, Some(tx_pending_request));
                                            }

                                            // A previous pending request has determined that it will no
                                            // longer wait for a response. This will typically happen if
                                            // there is a timeout for the request.
                                            PendingRequestUpdate::RemovePendingRequest(cseq) => {
                                                pending_requests.remove(&cseq);
                                            }
                                        }

                                        Ok(Loop::Continue((
                                            state,
                                            rx_incoming_response,
                                            Some(rx_pending_request.into_future()),
                                            pending_requests,
                                        )))
                                    } else if pending_requests.is_empty() {
                                        // The update stream has ended, and there are no pending
                                        // requests, so this task can end.

                                        lock_state(&state)
                                            .update_state(ReadState::Request, WriteState::Response);
                                        Ok(Loop::Break(()))
                                    } else {
                                        Ok(Loop::Continue((
                                            state,
                                            rx_incoming_response,
                                            None,
                                            pending_requests,
                                        )))
                                    }
                                }
                            })
                            .map_err(|_| ()),
                    )
                } else {
                    Either::B(
                        rx_incoming_response
                            .and_then(|(response, rx_incoming_response)| {
                                if !handle_response(response, &mut pending_requests)
                                    || pending_requests.is_empty()
                                {
                                    lock_state(&state)
                                        .update_state(ReadState::Request, WriteState::Response);
                                    Ok(Loop::Break(()))
                                } else {
                                    Ok(Loop::Continue((
                                        state,
                                        rx_incoming_response.into_future(),
                                        None,
                                        pending_requests,
                                    )))
                                }
                            })
                            .map_err(|_| ()),
                    )
                }
            },
        ).then(|_| {
            println!("Response handler task is done!");
            Ok(())
        }),
    )
}

/// Constructs a task that forwards all outgoing messages into a sink.
///
/// While the write state should be obeyed, this task will not allow sending of requests or
/// responses that are not allowed by the current write state.
///
/// This task will end in one of three situations:
///
/// * The vector of receivers all end, so no more messages will be sent through the sink.
/// * While trying to send a message through the sink, an error occurs.
/// * A protocol state change was detected in which writing is no longer necessary or possible.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_state_change` - A stream of [`ReadWriteStatePair`] protocol state changes.
/// * `sink` - The sink which all messages are forwarded to.
/// * `outgoing_messages` - A list of message channel receivers from different sources. All of the
///   messages sent to these receivers will be forwarded to the given sink as they arrive.
fn create_send_messages_task<S>(
    state: Arc<Mutex<ProtocolState>>,
    rx_state_change: UnboundedReceiver<ReadWriteStatePair>,
    sink: S,
    rx_outgoing_message: UnboundedReceiver<Message>,
) -> impl Future<Item = (), Error = ()>
where
    S: Sink<SinkItem = Message, SinkError = ProtocolError>,
{
    let state_clone = state.clone();
    let stream = rx_outgoing_message
        .filter(move |message| {
            // Only allow writing of messages that are allowed by the current write state. This is
            // here just in case something else in the protocol does not obey the write state. But
            // if everything does, then this is a waste of time. Until everything has been tested
            // extensively, this will remain.
            //
            // TODO: Remove this after testing all tasks extensively.

            match message {
                Message::Request(_) => lock_state(&state).write_state().requests_allowed(),
                Message::Response(_) => lock_state(&state).write_state().responses_allowed(),
            }
        })
        .map_err(|_| {
            // We have to match the error type that the sink generates, and since we do not want to
            // change the sink's error type in order to catch any errors that occur, we must change
            // the error here. This is a bit weird, since this is not even possible due to channel
            // receivers not being able to error.
            //
            // TODO: I think this may be able to be fixed once `futures` starts using the `!` type.

            ProtocolError::IO(Arc::new(io::Error::new(
                io::ErrorKind::Other,
                "outgoing message stream error",
            )))
        });

    loop_fn(
        (
            state_clone,
            rx_state_change.into_future(),
            sink.send_all(stream),
        ),
        |(state, rx_state_change, send_messages)| {
            rx_state_change.select2(send_messages).then(|result| {
                Ok(match result {
                    Ok(Either::A(((new_state, rx_state_change), send_messages))) => {
                        println!(
                            "Message sending task received a state change: {:?}",
                            new_state
                        );
                        match new_state.expect("state change receiver should not end") {
                            // We can no longer write any messages.
                            (_, WriteState::None) | (_, WriteState::Error(_)) => {
                                // Make sure read state is set correctly. This should have already
                                // happened, but just in case.

                                lock_state(&state).update_read_state(ReadState::Response);
                                Loop::Break(())
                            }
                            _ => Loop::Continue((
                                state,
                                rx_state_change.into_future(),
                                send_messages,
                            )),
                        }
                    }
                    Ok(Either::B(_)) => {
                        // All of the messages of the stream defined above have been finished
                        // meaning no more messages will be sent, so the task can end.

                        lock_state(&state).update_state(ReadState::Response, WriteState::None);
                        Loop::Break(())
                    }
                    Err(Either::A(_)) => panic!("state change receiver should not error"),
                    Err(Either::B((error, _))) => {
                        // There was an error while trying to send a message through the sink. Save
                        // the error to the write state and end the task.

                        lock_state(&state)
                            .update_state(ReadState::Response, WriteState::Error(error));
                        Loop::Break(())
                    }
                })
            })
        },
    )
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
/// In order for this task to end properly in all cases, we have to make sure that the dropping of
/// either of the incoming channel receivers implies a state change. Otherwise, it could be that
/// both of the incoming channels are no longer functional and thus this task cannot perform any
/// work. But it will not be able to detect that, so a state change must occur so the task can
/// handle any changes.
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
    S: Stream<Item = MessageResult, Error = ProtocolError>,
{
    fn forward_message<M>(
        tx_incoming_message: Option<Sender<M>>,
        message: M,
        state: &Arc<Mutex<ProtocolState>>,
        error_state: (Option<ReadState>, Option<WriteState>),
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
                    Ok(Either::A(((Some(result), stream), rx_state_change))) => {
                        // Received a new message, but we need to check whether or not it is valid.
                        println!("RECEIVED A MESSAGE: {:?}", result);
                        match result {
                            Ok(Message::Request(request)) => {
                                // Forward the request to the request stream. If an error occurs
                                // (implying that the request stream has been dropped), then we can
                                // only read responses from now on. We do not change the write
                                // state, since even though no new responses will be made, there
                                // still may be responses pending from previous requests.

                                tx_incoming_request = forward_message(
                                    tx_incoming_request,
                                    request,
                                    &state,
                                    (Some(ReadState::Response), None),
                                );
                            }
                            Ok(Message::Response(response)) => {
                                // Forward the response to the response stream. If an error occurs
                                // (implying that the response stream has been dropped), then we can
                                // only read requests and send responses.

                                tx_incoming_response = forward_message(
                                    tx_incoming_response,
                                    response,
                                    &state,
                                    (Some(ReadState::Request), Some(WriteState::Response)),
                                );
                            }
                            Err(InvalidMessage::InvalidRequest(_)) => {
                                // We received a request that was invalid, but it still was able to
                                // be decoded. We send a `400 Bad Request` to deal with this. This
                                // is one situation, however, in which the order that requests are
                                // handled is not based on `CSeq`. Even if the message had a valid
                                // `CSeq` header, it will not be inspected, since at least one part
                                // of the message was syntactically incorrect. As a result, the
                                // receiving agent has to manage mapping a response with no `CSeq`
                                // to its respective request. This is unlikely, and in general not
                                // possible if proxies are involved, since responses can be received
                                // out of order.

                                if lock_state(&state).write_state().responses_allowed() {
                                    let response = Response::builder()
                                        .status_code(StatusCode::BadRequest)
                                        .build(BytesMut::new())
                                        .expect("bad request response should not be invalid");
                                    tx_outgoing_message = try_send_message(
                                        tx_outgoing_message,
                                        Message::Response(response),
                                    );
                                }
                            }
                            Err(InvalidMessage::InvalidResponse(_)) => {
                                // Received an invalid response, but the only appropriate action
                                // here is to just ignore it.
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
                    Ok(Either::A(((None, _), _))) => {
                        // The incoming message stream has ended. We can only write responses from
                        // this point forward.

                        lock_state(&state).update_state(ReadState::None, WriteState::Response);
                        Loop::Break(())
                    }
                    Ok(Either::B(((new_state, rx_state_change), stream))) => {
                        println!(
                            "Message splitting task received a state change: {:?}",
                            new_state
                        );

                        match new_state.expect("state change receiver should not end") {
                            // We can no longer read anything.
                            (ReadState::None, _) | (ReadState::Error(_), _) => {
                                // Make sure write state is set correctly. This should have already
                                // happened, but just in case.

                                lock_state(&state).update_write_state(WriteState::Response);
                                return Ok(Loop::Break(()));
                            }

                            // We can no longer read responses.
                            (ReadState::Request, _) => {
                                // Make sure write state is set correctly. This should have already
                                // happened, but just in case.

                                lock_state(&state).update_write_state(WriteState::Response);
                                tx_incoming_response = None;
                            }

                            // We can no longer read requests.
                            (ReadState::Response, _)
                            | (_, WriteState::None)
                            | (_, WriteState::Error(_))
                            | (_, WriteState::Request) => {
                                // Make sure read state is set correctly. This should have already
                                // happened, but just in case. Also drop message sending stream.

                                lock_state(&state).update_read_state(ReadState::Response);
                                tx_outgoing_message = None;
                                tx_incoming_request = None;
                            }
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
                        println!("MESSAGE SPLITTING ERROR: {}", error);
                        // There was an error reading from the message stream that was
                        // irrecoverable. We can no longer read anything, but we may be able to
                        // still write responses. Requests cannot be written, since we would not be
                        // able to read their responses.

                        lock_state(&state)
                            .update_state(ReadState::Error(error), WriteState::Response);
                        Loop::Break(())
                    }
                    Err(Either::B(_)) => panic!("state change receiver should not error"),
                })
            })
        },
    )
}

#[cfg(test)]
mod test {
    // TODO: Maybe try to cleanup this testing so that the setup is not so repetitive?

    use std::convert::TryFrom;
    use std::mem;
    use tokio::runtime::current_thread;
    use tokio::runtime::Runtime;

    use super::*;
    use method::Method;
    use protocol::RecoverableInvalidRequest;
    use uri::URI;

    #[test]
    fn test_decoding_timer_task_event_handling() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_codec_event, rx_codec_event) = unbounded();
        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_decoding_timer_task(
            protocol_state.clone(),
            rx_codec_event,
            Duration::from_millis(50),
        );

        // Simulate codec encoding and decoding.

        tx_codec_event
            .unbounded_send(CodecEvent::DecodingStarted)
            .unwrap();
        tx_codec_event
            .unbounded_send(CodecEvent::EncodingStarted)
            .unwrap();
        tx_codec_event
            .unbounded_send(CodecEvent::EncodingEnded)
            .unwrap();
        tx_codec_event
            .unbounded_send(CodecEvent::DecodingEnded)
            .unwrap();

        let mut runtime = Runtime::new().unwrap();

        // Simulate dropping codec event. We wait to do it here, so the task will handle the above
        // events. Also make sure the delay here is longer than the timeout duration to test whether
        // the `CodecEvent::DecodingEnded` successfully stops the timer.

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100)).then(|_| {
                mem::drop(tx_codec_event);
                Ok(())
            }),
        );

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::None)
        );
    }

    #[test]
    fn test_decoding_timer_task_codec_dropped() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_codec_event, rx_codec_event) = unbounded();
        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_decoding_timer_task(
            protocol_state.clone(),
            rx_codec_event,
            DEFAULT_DECODE_TIMEOUT_DURATION,
        );

        // Simulate the dropping of the codec (meaning the sender has been dropped as well).

        mem::drop(tx_codec_event);

        // Wait for task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::None)
        );
    }

    #[test]
    fn test_decoding_timer_task_expired() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_codec_event, rx_codec_event) = unbounded();
        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_decoding_timer_task(
            protocol_state.clone(),
            rx_codec_event,
            Duration::from_millis(1),
        );

        // Simulate the decoding of a message that timeouts.

        tx_codec_event
            .unbounded_send(CodecEvent::DecodingStarted)
            .unwrap();

        // Wait for task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (
                ReadState::Error(ProtocolError::DecodingTimedOut),
                WriteState::Response
            )
        );
    }

    #[test]
    fn test_request_handler_task_cseq_ordering() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_ordered_incoming_request, rx_ordered_incoming_request) = unbounded();
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_request_handler_task(
            protocol_state.clone(),
            rx_incoming_request,
            tx_ordered_incoming_request,
            tx_outgoing_message,
            DEFAULT_REQUEST_BUFFER_SIZE,
        );

        let mut runtime = current_thread::Runtime::new().unwrap();

        // Send request with `CSeq` header which sets the starting sequence number.

        let request_1 = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "0")
            .build("".into())
            .unwrap();
        let tx_incoming_request = runtime
            .block_on(tx_incoming_request.send(request_1.clone()))
            .unwrap();

        // Send next requests with out of order `CSeq`s.

        let request_2 = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "2")
            .build("".into())
            .unwrap();
        let tx_incoming_request = runtime
            .block_on(tx_incoming_request.send(request_2.clone()))
            .unwrap();

        let request_3 = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "1")
            .build("".into())
            .unwrap();
        let tx_incoming_request = runtime
            .block_on(tx_incoming_request.send(request_3.clone()))
            .unwrap();

        // Simulate ending incoming requests.

        mem::drop(tx_incoming_request);

        // Wait for the task to end.

        runtime.block_on(task).unwrap();

        let ordered_incoming_requests = runtime
            .block_on(rx_ordered_incoming_request.collect())
            .unwrap();

        assert_eq!(
            ordered_incoming_requests,
            vec![request_1, request_3, request_2]
        );
    }

    #[test]
    fn test_request_handler_task_cseq_out_of_range() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_ordered_incoming_request, rx_ordered_incoming_request) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_request_handler_task(
            protocol_state.clone(),
            rx_incoming_request,
            tx_ordered_incoming_request,
            tx_outgoing_message,
            DEFAULT_REQUEST_BUFFER_SIZE,
        );

        let mut runtime = current_thread::Runtime::new().unwrap();

        // Send request with `CSeq` header which sets the starting sequence number.

        let request_1 = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "0")
            .build("".into())
            .unwrap();
        let tx_incoming_request = runtime
            .block_on(tx_incoming_request.send(request_1.clone()))
            .unwrap();

        // Send next request with very different `CSeq`.

        let request_2 = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "99999")
            .build("".into())
            .unwrap();
        let tx_incoming_request = runtime
            .block_on(tx_incoming_request.send(request_2))
            .unwrap();

        // Simulate ending incoming requests.

        mem::drop(tx_incoming_request);

        // Wait for the task to end.

        runtime.block_on(task).unwrap();

        let ordered_incoming_requests = runtime
            .block_on(rx_ordered_incoming_request.collect())
            .unwrap();

        assert_eq!(ordered_incoming_requests, vec![request_1]);

        let outgoing_messages = runtime.block_on(rx_outgoing_message.collect()).unwrap();
        let expected_message = Message::Response(
            Response::builder()
                .status_code(StatusCode::ServiceUnavailable)
                .build("".into())
                .unwrap(),
        );

        assert_eq!(outgoing_messages, vec![expected_message]);
    }

    #[test]
    fn test_request_handler_task_invalid_cseq_responses_allowed() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_ordered_incoming_request, _rx_ordered_incoming_request) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_request_handler_task(
            protocol_state.clone(),
            rx_incoming_request,
            tx_ordered_incoming_request,
            tx_outgoing_message,
            DEFAULT_REQUEST_BUFFER_SIZE,
        );

        // Send request with invalid `CSeq` header.

        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "invalid cseq")
            .build("".into())
            .unwrap();

        let mut runtime = current_thread::Runtime::new().unwrap();
        let tx_incoming_request = runtime.block_on(tx_incoming_request.send(request)).unwrap();

        // Simulate ending incoming requests.

        mem::drop(tx_incoming_request);

        // Wait for the task to end.

        runtime.block_on(task).unwrap();

        let outgoing_messages = runtime.block_on(rx_outgoing_message.collect()).unwrap();
        let expected_message = Message::Response(
            Response::builder()
                .status_code(StatusCode::BadRequest)
                .build("".into())
                .unwrap(),
        );

        assert_eq!(outgoing_messages, vec![expected_message]);
    }

    #[test]
    fn test_request_handler_task_invalid_cseq_responses_not_allowed() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_ordered_incoming_request, _rx_ordered_incoming_request) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));

        {
            lock_state(&protocol_state).update_write_state(WriteState::Request);
        }

        let task = create_request_handler_task(
            protocol_state.clone(),
            rx_incoming_request,
            tx_ordered_incoming_request,
            tx_outgoing_message,
            DEFAULT_REQUEST_BUFFER_SIZE,
        );

        // Send request with invalid `CSeq` header.

        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .header(HeaderName::CSeq, "invalid cseq")
            .build("".into())
            .unwrap();

        let mut runtime = current_thread::Runtime::new().unwrap();
        let tx_incoming_request = runtime.block_on(tx_incoming_request.send(request)).unwrap();

        // Simulate ending incoming requests.

        mem::drop(tx_incoming_request);

        // Wait for the task to end.

        runtime.block_on(task).unwrap();

        let outgoing_messages = runtime.block_on(rx_outgoing_message.collect()).unwrap();
        assert_eq!(outgoing_messages, vec![]);
    }

    #[test]
    fn test_request_handler_task_stream_ends() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_ordered_incoming_request, _rx_ordered_incoming_request) = unbounded();
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_request_handler_task(
            protocol_state.clone(),
            rx_incoming_request,
            tx_ordered_incoming_request,
            tx_outgoing_message,
            DEFAULT_REQUEST_BUFFER_SIZE,
        );

        // Simulate ending incoming requests.

        mem::drop(tx_incoming_request);

        // Wait for the task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();
    }

    #[test]
    fn test_response_handler_task_continue() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_pending_request, rx_pending_request) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_response_handler_task(
            protocol_state.clone(),
            rx_incoming_response,
            rx_pending_request,
        );

        // Add a pending request.

        let (tx_pending_request_1, rx_pending_request_1) = oneshot::channel();
        tx_pending_request
            .unbounded_send(PendingRequestUpdate::AddPendingRequest((
                CSeq::try_from(0).unwrap(),
                tx_pending_request_1,
            )))
            .unwrap();

        // Send a `100 Continue` response after some delay and then the final response.

        let final_response = Response::builder()
            .header(HeaderName::CSeq, "0")
            .build("".into())
            .unwrap();
        let final_response_clone = final_response.clone();

        let mut runtime = Runtime::new().unwrap();

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100))
                .then(move |_| {
                    let continue_response = Response::builder()
                        .status_code(StatusCode::Continue)
                        .header(HeaderName::CSeq, "0")
                        .build("".into())
                        .unwrap();

                    tx_incoming_response
                        .send(continue_response)
                        .and_then(|tx_incoming_response| tx_incoming_response.send(final_response))
                })
                .then(|_| Ok(())),
        );
        runtime.spawn(rx_pending_request_1.then(move |pending_request| {
            let pending_request = pending_request.unwrap();
            let receiver = match pending_request {
                PendingRequestResponse::Continue(receiver) => receiver,
                _ => panic!("expected a continue"),
            };

            receiver.then(move |pending_request| {
                let pending_request = pending_request.unwrap();

                match pending_request {
                    PendingRequestResponse::Response(response) => {
                        assert_eq!(response, final_response_clone)
                    }
                    _ => panic!("expected a response"),
                }

                Ok(())
            })
        }));

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::Request, WriteState::Response)
        );
    }

    #[test]
    fn test_response_handler_task_pending_requests_stream_ends() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (_tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_pending_request, rx_pending_request) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_response_handler_task(
            protocol_state.clone(),
            rx_incoming_response,
            rx_pending_request,
        );

        // Simulate ending the pending request stream.

        mem::drop(tx_pending_request);

        // Wait for task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::Request, WriteState::Response)
        );
    }

    #[test]
    fn test_response_handler_task_response_matching() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_pending_request, rx_pending_request) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_response_handler_task(
            protocol_state.clone(),
            rx_incoming_response,
            rx_pending_request,
        );

        // Add a pending request.

        let (tx_pending_request_1, rx_pending_request_1) = oneshot::channel();
        tx_pending_request
            .unbounded_send(PendingRequestUpdate::AddPendingRequest((
                CSeq::try_from(0).unwrap(),
                tx_pending_request_1,
            )))
            .unwrap();

        // Send a response after some delay.

        let response = Response::builder()
            .header(HeaderName::CSeq, "0")
            .build("".into())
            .unwrap();
        let response_clone = response.clone();

        let mut runtime = Runtime::new().unwrap();

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100))
                .then(move |_| tx_incoming_response.send(response))
                .then(|_| Ok(())),
        );

        // Wait for task to end.

        runtime.spawn(rx_pending_request_1.then(move |pending_response| {
            match pending_response.unwrap() {
                PendingRequestResponse::Response(received_response) => {
                    assert_eq!(received_response, response_clone);
                }
                _ => panic!("expected a response"),
            }
            Ok(())
        }));

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::Request, WriteState::Response)
        );
    }

    #[test]
    fn test_response_handler_task_response_stream_ends() {
        let (tx_state_change, _rx_state_change) = unbounded();
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_pending_request, rx_pending_request) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_response_handler_task(
            protocol_state.clone(),
            rx_incoming_response,
            rx_pending_request,
        );

        // Add a pending request that will not be fulfilled.

        let (tx_pending_request_1, rx_pending_request_1) = oneshot::channel();
        tx_pending_request
            .unbounded_send(PendingRequestUpdate::AddPendingRequest((
                CSeq::try_from(0).unwrap(),
                tx_pending_request_1,
            )))
            .unwrap();

        let mut runtime = Runtime::new().unwrap();

        // Simulate ending the incoming response stream, but wait to do it so that the update above
        // can be handled.

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                mem::drop(tx_incoming_response);
                Ok(())
            }),
        );

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        // Wait for task to end.

        let pending_request_response = current_thread::Runtime::new()
            .unwrap()
            .block_on(rx_pending_request_1)
            .unwrap();
        assert!(pending_request_response.is_none());

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::Request, WriteState::Response)
        );
    }

    #[test]
    fn test_send_messages_task_sink_error() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_send_messages_task(
            protocol_state.clone(),
            rx_state_change,
            tx_message.sink_map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            rx_outgoing_message,
        );

        // Simulate sending a response.

        let response = Response::builder().build("".into()).unwrap();
        tx_outgoing_message
            .unbounded_send(Message::Response(response))
            .unwrap();

        // Simulate dropping the stream.

        mem::drop(rx_message);

        // Wait for the task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (
                ReadState::Response,
                WriteState::Error(ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                ))))
            )
        );
    }

    #[test]
    fn test_send_messages_task_state_change_no_writes() {
        let dummy_error = ProtocolError::IO(Arc::new(io::Error::new(
            io::ErrorKind::Other,
            "dummy error",
        )));

        for new_write_state in [WriteState::None, WriteState::Error(dummy_error)].iter() {
            let (tx_state_change, rx_state_change) = unbounded();
            let (tx_message, _rx_message) = unbounded();
            let (_tx_outgoing_message, rx_outgoing_message) = unbounded();

            let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));

            {
                lock_state(&protocol_state).update_write_state(new_write_state.clone());
            }

            let task = create_send_messages_task(
                protocol_state.clone(),
                rx_state_change,
                tx_message.sink_map_err(|_| {
                    ProtocolError::IO(Arc::new(io::Error::new(
                        io::ErrorKind::Other,
                        "dummy error",
                    )))
                }),
                rx_outgoing_message,
            );

            // Wait for the task to end.

            current_thread::Runtime::new()
                .unwrap()
                .block_on(task)
                .unwrap();

            assert_eq!(
                lock_state(&protocol_state).state(),
                (ReadState::Response, new_write_state.clone())
            );
        }
    }

    #[test]
    fn test_send_messages_task_stream_ends() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, _rx_message) = unbounded();
        let (_tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_send_messages_task(
            protocol_state.clone(),
            rx_state_change,
            tx_message.sink_map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            rx_outgoing_message,
        );

        // Wait for the task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::Response, WriteState::None)
        );
    }

    #[test]
    fn test_split_messages_task_forward_request_error() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, _rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending a request.

        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("".into())
            .unwrap();

        tx_message
            .unbounded_send(Ok(Message::Request(request)))
            .unwrap();

        // Simulate dropping the forwarded request stream.

        mem::drop(rx_incoming_request);

        let mut runtime = Runtime::new().unwrap();

        // Simulate ending the stream, but wait to do it so that the protocol state can be checked
        // after the task has handled a forwarding request error.

        let protocol_state_cloned = protocol_state.clone();

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                assert_eq!(
                    lock_state(&protocol_state_cloned).state(),
                    (ReadState::Response, WriteState::All)
                );

                mem::drop(tx_message);
                Ok(())
            }),
        );

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );
    }

    #[test]
    fn test_split_messages_task_forward_response_error() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending a response.

        let response = Response::builder().build("".into()).unwrap();
        tx_message
            .unbounded_send(Ok(Message::Response(response)))
            .unwrap();

        // Simulate dropping the forwarded responses stream.

        mem::drop(rx_incoming_response);

        let mut runtime = Runtime::new().unwrap();

        // Simulate ending the stream, but wait to do it so that the protocol state can be checked
        // after the task has handled a forwarding request error.

        let protocol_state_cloned = protocol_state.clone();

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                assert_eq!(
                    lock_state(&protocol_state_cloned).state(),
                    (ReadState::Request, WriteState::Response)
                );

                mem::drop(tx_message);
                Ok(())
            }),
        );

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );
    }

    #[test]
    fn test_split_messages_task_forward_success() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending a request and response.

        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("".into())
            .unwrap();

        tx_message
            .unbounded_send(Ok(Message::Request(request.clone())))
            .unwrap();

        let response = Response::builder().build("".into()).unwrap();
        tx_message
            .unbounded_send(Ok(Message::Response(response.clone())))
            .unwrap();

        // Simulate ending the stream.

        mem::drop(tx_message);

        // Wait for task to end.

        let mut runtime = current_thread::Runtime::new().unwrap();

        runtime.block_on(task).unwrap();
        let incoming_requests = runtime.block_on(rx_incoming_request.collect()).unwrap();
        let incoming_responses = runtime.block_on(rx_incoming_response.collect()).unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );
        assert_eq!(incoming_requests, vec![request]);
        assert_eq!(incoming_responses, vec![response]);
    }

    #[test]
    fn test_split_messages_task_invalid_request_responses_not_allowed() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, _rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));

        {
            lock_state(&protocol_state).update_write_state(WriteState::None);
        }

        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending an invalid request.

        tx_message
            .unbounded_send(Err(InvalidMessage::InvalidRequest(
                RecoverableInvalidRequest::InvalidURI,
            )))
            .unwrap();

        // Simulate ending the stream.

        mem::drop(tx_message);

        // Wait for task to end.

        let mut runtime = current_thread::Runtime::new().unwrap();

        runtime.block_on(task).unwrap();
        let outgoing_messages = runtime.block_on(rx_outgoing_message.collect()).unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::None)
        );
        assert_eq!(outgoing_messages, vec![]);
    }

    #[test]
    fn test_split_messages_task_invalid_request_responses_allowed() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, _rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending an invalid request.

        tx_message
            .unbounded_send(Err(InvalidMessage::InvalidRequest(
                RecoverableInvalidRequest::InvalidURI,
            )))
            .unwrap();

        // Simulate ending the stream.

        mem::drop(tx_message);

        // Wait for task to end.

        let mut runtime = current_thread::Runtime::new().unwrap();

        runtime.block_on(task).unwrap();
        let outgoing_messages = runtime.block_on(rx_outgoing_message.collect()).unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );

        let expected_message = Message::Response(
            Response::builder()
                .status_code(StatusCode::BadRequest)
                .build("".into())
                .unwrap(),
        );

        assert_eq!(outgoing_messages, vec![expected_message]);
    }

    #[test]
    fn test_split_messages_task_state_change_no_reads() {
        let dummy_error = ProtocolError::IO(Arc::new(io::Error::new(
            io::ErrorKind::Other,
            "dummy error",
        )));

        for new_read_state in [ReadState::None, ReadState::Error(dummy_error)].iter() {
            let (tx_state_change, rx_state_change) = unbounded();
            let (_tx_message, rx_message) = unbounded();
            let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
            let (tx_incoming_response, _rx_incoming_response) =
                channel(DEFAULT_RESPONSE_BUFFER_SIZE);
            let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

            let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));

            {
                lock_state(&protocol_state).update_read_state(new_read_state.clone());
            }

            let task = create_split_messages_task(
                protocol_state.clone(),
                rx_state_change,
                rx_message.map_err(|_| {
                    ProtocolError::IO(Arc::new(io::Error::new(
                        io::ErrorKind::Other,
                        "dummy error",
                    )))
                }),
                tx_incoming_request,
                tx_incoming_response,
                tx_outgoing_message,
            );

            // Wait for task to end.

            current_thread::Runtime::new()
                .unwrap()
                .block_on(task)
                .unwrap();

            assert_eq!(
                lock_state(&protocol_state).state(),
                (new_read_state.clone(), WriteState::Response)
            );
        }
    }

    #[test]
    fn test_split_messages_task_state_change_no_requests() {
        let dummy_error = ProtocolError::IO(Arc::new(io::Error::new(
            io::ErrorKind::Other,
            "dummy error",
        )));

        let mut new_states = [
            (Some(ReadState::Response), None),
            (None, Some(WriteState::None)),
            (None, Some(WriteState::Error(dummy_error))),
            (None, Some(WriteState::Request)),
        ];

        for new_state in new_states.iter_mut() {
            let (tx_state_change, rx_state_change) = unbounded();
            let (tx_message, rx_message) = unbounded();
            let (tx_incoming_request, rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
            let (tx_incoming_response, _) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
            let (tx_outgoing_message, _) = unbounded();

            let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
            let mut write_state = new_state.1.clone();

            {
                if let Some(new_read_state) = new_state.0.take() {
                    lock_state(&protocol_state).update_read_state(new_read_state);
                }

                if let Some(new_write_state) = new_state.1.take() {
                    lock_state(&protocol_state).update_write_state(new_write_state);
                }
            }

            let task = create_split_messages_task(
                protocol_state.clone(),
                rx_state_change,
                rx_message.map_err(|_| {
                    ProtocolError::IO(Arc::new(io::Error::new(
                        io::ErrorKind::Other,
                        "dummy error",
                    )))
                }),
                tx_incoming_request,
                tx_incoming_response,
                tx_outgoing_message,
            );

            let mut runtime = Runtime::new().unwrap();

            // Simulate sending a request but wait to do it so that the protocol state change can be
            // handled first.

            runtime.spawn(
                Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                    let request = Request::builder()
                        .method(Method::Options)
                        .uri(URI::Any)
                        .build("".into())
                        .unwrap();

                    tx_message
                        .unbounded_send(Ok(Message::Request(request)))
                        .unwrap();
                    Ok(())
                }),
            );

            // Wait for task to end.

            runtime.spawn(task);
            runtime.shutdown_on_idle().wait().unwrap();

            let incoming_requests = current_thread::Runtime::new()
                .unwrap()
                .block_on(rx_incoming_request.collect())
                .unwrap();

            let expected_write_state = if let Some(new_write_state) = write_state.take() {
                if new_write_state.is_request() {
                    WriteState::None
                } else {
                    new_write_state
                }
            } else {
                WriteState::Response
            };

            assert_eq!(
                lock_state(&protocol_state).state(),
                (ReadState::None, expected_write_state)
            );
            assert!(incoming_requests.is_empty());
        }
    }

    #[test]
    fn test_split_messages_task_state_change_no_responses() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));

        {
            lock_state(&protocol_state).update_read_state(ReadState::Request);
        }

        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        let mut runtime = Runtime::new().unwrap();

        // Simulate sending a response but wait to do it so that the protocol state change can be
        // handled first.

        let protocol_state_cloned = protocol_state.clone();

        runtime.spawn(
            Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                assert_eq!(
                    lock_state(&protocol_state_cloned).state(),
                    (ReadState::Request, WriteState::Response)
                );

                let response = Response::builder().build("".into()).unwrap();
                tx_message
                    .unbounded_send(Ok(Message::Response(response)))
                    .unwrap();
                Ok(())
            }),
        );

        // Wait for task to end.

        runtime.spawn(task);
        runtime.shutdown_on_idle().wait().unwrap();

        let incoming_responses = current_thread::Runtime::new()
            .unwrap()
            .block_on(rx_incoming_response.collect())
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );
        assert!(incoming_responses.is_empty());
    }

    #[test]
    fn test_split_messages_task_stream_ends() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, _rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.map_err(|_| {
                ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate ending the stream.

        mem::drop(tx_message);

        // Wait for task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (ReadState::None, WriteState::Response)
        );
    }

    #[test]
    fn test_split_messages_task_stream_error() {
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_message, rx_message) = unbounded();
        let (tx_incoming_request, _rx_incoming_request) = channel(DEFAULT_REQUEST_BUFFER_SIZE);
        let (tx_incoming_response, _rx_incoming_response) = channel(DEFAULT_RESPONSE_BUFFER_SIZE);
        let (tx_outgoing_message, _rx_outgoing_message) = unbounded();

        let protocol_state = Arc::new(Mutex::new(ProtocolState::new(vec![tx_state_change])));
        let task = create_split_messages_task(
            protocol_state.clone(),
            rx_state_change,
            rx_message.then(|_: Result<Result<Message, InvalidMessage>, ()>| {
                Err(ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                ))))
            }),
            tx_incoming_request,
            tx_incoming_response,
            tx_outgoing_message,
        );

        // Simulate sending an error. Since we cannot actually send an error, we send whatever here
        // and use the `then` combinator to force an error.

        let message = Message::Response(Response::builder().build("".into()).unwrap());
        tx_message.unbounded_send(Ok(message)).unwrap();

        // Wait for the task to end.

        current_thread::Runtime::new()
            .unwrap()
            .block_on(task)
            .unwrap();

        assert_eq!(
            lock_state(&protocol_state).state(),
            (
                ReadState::Error(ProtocolError::IO(Arc::new(io::Error::new(
                    io::ErrorKind::Other,
                    "dummy error",
                )))),
                WriteState::Response
            )
        );
    }
}
