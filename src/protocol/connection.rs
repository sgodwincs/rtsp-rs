use bytes::BytesMut;
use futures::future::{self, Either};
use futures::stream::Fuse;
use futures::sync::mpsc::{unbounded, UnboundedReceiver, UnboundedSender};
use futures::sync::oneshot;
use futures::{Async, AsyncSink, Future, Poll, Sink, Stream};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};
use std::{fmt, mem};
use tokio_executor::{DefaultExecutor, Executor, SpawnError};
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_timer::{Delay, Error as TimerError};

use header::types::CSeq;
use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader};
use protocol::{
    Codec, CodecEvent, DecodeError, InvalidMessage, IrrecoverableInvalidRequest,
    IrrecoverableInvalidResponse, Message, MessageResult, OperationError, ProtocolError,
    RequestTimeoutType, Service,
};
use request::Request;
use response::Response;
use status::StatusCode;
use version::Version;

pub const DEFAULT_DECODE_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUEST_BUFFER_SIZE: usize = 10;
pub const DEFAULT_REQUEST_MAX_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUEST_TIMEOUT_DURATION: Duration = Duration::from_secs(5);
pub const DEFAULT_RESPONSE_BUFFER_SIZE: usize = 10;
pub const DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION: Duration = Duration::from_secs(10);

#[derive(Debug)]
pub struct Connection {
    request_default_max_timeout_duration: Option<Duration>,
    request_default_timeout_duration: Option<Duration>,
    sequence_number: CSeq,
    state: Arc<Mutex<ConnectionState>>,
    tx_pending_request: Option<UnboundedSender<PendingRequestUpdate>>,
    tx_outgoing_message: Option<UnboundedSender<Message>>,
    tx_shutdown: Option<oneshot::Sender<ShutdownType>>,
}

impl Connection {
    pub fn new<IO, S>(io: IO, service: Option<S>) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite + Send + 'static,
        S: Service<Request = Request<BytesMut>> + Send + 'static,
        S::Future: Send + 'static,
        S::Response: Into<Response<BytesMut, HeaderMap>>,
    {
        Connection::with_config(io, service, Config::default())
    }

    pub fn with_config<IO, S>(
        io: IO,
        service: Option<S>,
        mut config: Config,
    ) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite + Send + 'static,
        S: Service<Request = Request<BytesMut>> + Send + 'static,
        S::Future: Send + 'static,
        S::Response: Into<Response<BytesMut, HeaderMap>>,
    {
        // Create all channels needed for communication from task to task and from connection to
        // task.

        let (tx_codec_event, rx_codec_event) = unbounded();
        let (tx_pending_request, rx_pending_request) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = unbounded();
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();
        let (tx_shutdown, rx_shutdown) = oneshot::channel();

        let mut executor = DefaultExecutor::current();
        let state = Arc::new(Mutex::new(ConnectionState::new()));
        let (sink, stream) = io.framed(Codec::with_events(tx_codec_event)).split();

        executor.spawn(Box::new(DecodingTimerTask::new(
            state.clone(),
            rx_codec_event,
            config.decode_timeout_duration,
        )))?;

        executor.spawn(Box::new(MessageReceiverTask::new(
            state.clone(),
            stream,
            rx_pending_request,
            tx_incoming_request,
            tx_outgoing_message.clone(),
            config.request_buffer_size,
        )))?;

        executor.spawn(Box::new(MessageSenderTask::new(
            state.clone(),
            sink,
            rx_outgoing_message,
        )))?;

        let shutdown_future = config
            .shutdown_future
            .take()
            .expect("config shutdown future should not be `None`");

        executor.spawn(Box::new(ShutdownTask::new(
            state.clone(),
            shutdown_future,
            rx_shutdown,
        )))?;

        if let Some(service) = service {
            executor.spawn(Box::new(RequestHandlerTask::new(
                state.clone(),
                service,
                rx_incoming_request,
                tx_outgoing_message.clone(),
            )))?;
        } else {
            lock_state(&state).update_state(ReadState::Response, WriteState::Request);
        }

        Ok(Connection {
            request_default_max_timeout_duration: config.request_default_max_timeout_duration,
            request_default_timeout_duration: config.request_default_timeout_duration,
            sequence_number: CSeq::try_from(0).expect("sequence number `0` should not be invalid"),
            state: state,
            tx_pending_request: Some(tx_pending_request),
            tx_outgoing_message: Some(tx_outgoing_message),
            tx_shutdown: Some(tx_shutdown),
        })
    }

    pub fn is_shutdown(&self) -> bool {
        let state = lock_state(&self.state).state();
        state.0.is_none() || state.0.is_error() && state.1.is_none() || state.1.is_error()
    }

    fn pending_request(&self) -> &UnboundedSender<PendingRequestUpdate> {
        self.tx_pending_request
            .as_ref()
            .take()
            .expect("pending request sender should not be `None`")
    }

    fn outgoing_message(&self) -> &UnboundedSender<Message> {
        self.tx_outgoing_message
            .as_ref()
            .take()
            .expect("outgoing message sender should not be `None`")
    }

    pub fn read_state(&self) -> ReadState {
        self.state().0
    }

    pub fn send_request<R, B>(
        &mut self,
        request: R,
    ) -> impl Future<Item = Response<BytesMut>, Error = OperationError>
    where
        R: Into<Request<B>>,
        B: AsRef<[u8]>,
    {
        let request_default_max_timeout_duration = self.request_default_max_timeout_duration;
        let request_default_timeout_duration = self.request_default_timeout_duration;

        self.send_request_with_options(
            request,
            RequestOptions::builder()
                .max_timeout_duration(request_default_max_timeout_duration)
                .timeout_duration(request_default_timeout_duration)
                .build()
                .expect("request options should not be invalid internally"),
        )
    }

    pub fn send_request_with_options<R, B>(
        &mut self,
        request: R,
        options: RequestOptions,
    ) -> impl Future<Item = Response<BytesMut>, Error = OperationError>
    where
        R: Into<Request<B>>,
        B: AsRef<[u8]>,
    {
        // Check if the write state allows sending requests. We should also probably check whether
        // the read state allows reading responses, but if everything is working correctly, it
        // should agree with the write state.

        if !self.write_state().requests_allowed() {
            return Either::A(future::err(OperationError::Closed));
        }

        let sequence_number = self.sequence_number;
        self.sequence_number = self.sequence_number.increment();

        let mut request = request.into().map(|body| BytesMut::from(body.as_ref()));
        let cseq_header = CSeq::to_header_raw(&sequence_number)
            .into_iter()
            .nth(0)
            .expect("`CSeq` header should always have one header");
        request.headers_mut().insert(HeaderName::CSeq, cseq_header);

        let (tx_response, rx_response) = oneshot::channel();
        let update = PendingRequestUpdate::AddPendingRequest((sequence_number, tx_response));

        if let Err(_) = self.pending_request().unbounded_send(update) {
            // This really should not happen, since we checked the write state above. But if it
            // does, make sure that the state is updated correctly.

            lock_state(&self.state).update_state(ReadState::Request, WriteState::Response);
            return Either::A(future::err(OperationError::Closed));
        }

        if let Err(_) = self
            .outgoing_message()
            .unbounded_send(Message::Request(request))
        {
            // This also should not happen. And if it does, we need to remove the pending request we
            // just sent. Also update the state to be what it should be.

            self.pending_request()
                .unbounded_send(PendingRequestUpdate::RemovePendingRequest(sequence_number))
                .ok();
            lock_state(&self.state).update_state(ReadState::Response, WriteState::None);
            return Either::A(future::err(OperationError::Closed));
        }

        Either::B(SendRequestFuture::new(
            rx_response,
            self.pending_request().clone(),
            sequence_number,
            options.timeout_duration(),
            options.max_timeout_duration(),
        ))
    }

    pub fn shutdown(&mut self, shutdown_type: ShutdownType) {
        if let Some(tx_shutdown) = self.tx_shutdown.take() {
            self.tx_outgoing_message = None;
            self.tx_pending_request = None;
            tx_shutdown.send(shutdown_type).ok();
        }
    }

    pub fn state(&self) -> ReadWriteStatePair {
        lock_state(&self.state).state()
    }

    pub fn write_state(&self) -> WriteState {
        self.state().1
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        // Connection is being dropped, start a soft shutdown.

        self.shutdown(ShutdownType::Soft(DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION));
    }
}

pub struct Config {
    pub(crate) decode_timeout_duration: Duration,
    pub(crate) request_buffer_size: usize,
    pub(crate) request_default_max_timeout_duration: Option<Duration>,
    pub(crate) request_default_timeout_duration: Option<Duration>,
    pub(crate) response_buffer_size: usize,
    pub(crate) shutdown_future:
        Option<Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>>,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    pub fn new() -> Self {
        Config::default()
    }

    pub fn decode_timeout_duration(&self) -> Duration {
        self.decode_timeout_duration
    }

    pub fn request_buffer_size(&self) -> usize {
        self.request_buffer_size
    }

    pub fn request_default_max_timeout_duration(&self) -> Option<Duration> {
        self.request_default_max_timeout_duration
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
            .expect("default config builder should be valid")
    }
}

pub struct ConfigBuilder {
    decode_timeout_duration: Duration,
    request_buffer_size: usize,
    request_default_max_timeout_duration: Option<Duration>,
    request_default_timeout_duration: Option<Duration>,
    response_buffer_size: usize,
    shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        ConfigBuilder::default()
    }

    pub fn build(self) -> Result<Config, ConfigBuilderError> {
        if self.decode_timeout_duration.as_secs() == 0 {
            return Err(ConfigBuilderError::InvalidDecodeTimeoutDuration);
        }

        if self.request_buffer_size == 0 {
            return Err(ConfigBuilderError::InvalidRequestBufferSize);
        }

        if let Some(duration) = self.request_default_max_timeout_duration {
            if duration.as_secs() == 0 {
                return Err(ConfigBuilderError::InvalidRequestDefaultMaxTimeoutDuration);
            }
        }

        if let Some(duration) = self.request_default_timeout_duration {
            if duration.as_secs() == 0 {
                return Err(ConfigBuilderError::InvalidRequestDefaultTimeoutDuration);
            }
        }

        if self.response_buffer_size == 0 {
            return Err(ConfigBuilderError::InvalidResponseBufferSize);
        }

        Ok(Config {
            decode_timeout_duration: self.decode_timeout_duration,
            request_buffer_size: self.request_buffer_size,
            request_default_max_timeout_duration: self.request_default_max_timeout_duration,
            request_default_timeout_duration: self.request_default_timeout_duration,
            response_buffer_size: self.response_buffer_size,
            shutdown_future: Some(self.shutdown_future),
        })
    }

    pub fn decode_timeout_duration(&mut self, duration: Duration) -> &mut Self {
        self.decode_timeout_duration = duration;
        self
    }

    pub fn request_buffer_size(&mut self, size: usize) -> &mut Self {
        self.request_buffer_size = size;
        self
    }

    pub fn request_default_max_timeout_duration(
        &mut self,
        duration: Option<Duration>,
    ) -> &mut Self {
        self.request_default_max_timeout_duration = duration;
        self
    }

    pub fn request_default_timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.request_default_timeout_duration = duration;
        self
    }

    pub fn response_buffer_size(&mut self, size: usize) -> &mut Self {
        self.response_buffer_size = size;
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
            decode_timeout_duration: DEFAULT_DECODE_TIMEOUT_DURATION,
            request_buffer_size: DEFAULT_REQUEST_BUFFER_SIZE,
            request_default_max_timeout_duration: Some(DEFAULT_REQUEST_MAX_TIMEOUT_DURATION),
            request_default_timeout_duration: Some(DEFAULT_REQUEST_TIMEOUT_DURATION),
            response_buffer_size: DEFAULT_RESPONSE_BUFFER_SIZE,
            shutdown_future: Box::new(future::empty()),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ConfigBuilderError {
    InvalidDecodeTimeoutDuration,
    InvalidRequestBufferSize,
    InvalidRequestDefaultMaxTimeoutDuration,
    InvalidRequestDefaultTimeoutDuration,
    InvalidResponseBufferSize,
}

impl fmt::Display for ConfigBuilderError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for ConfigBuilderError {
    fn description(&self) -> &str {
        use self::ConfigBuilderError::*;

        match self {
            InvalidDecodeTimeoutDuration => "invalid decode timeout duration",
            InvalidRequestBufferSize => "invalid request buffer size",
            InvalidRequestDefaultMaxTimeoutDuration => {
                "invalid request default max timeout duration"
            }
            InvalidRequestDefaultTimeoutDuration => "invalid request default timeout duration",
            InvalidResponseBufferSize => "invalid response buffer size",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RequestOptions {
    max_timeout_duration: Option<Duration>,
    timeout_duration: Option<Duration>,
}

impl RequestOptions {
    pub fn builder() -> RequestOptionsBuilder {
        RequestOptionsBuilder::new()
    }

    pub fn new() -> Self {
        RequestOptions::default()
    }

    pub fn max_timeout_duration(&self) -> Option<Duration> {
        self.max_timeout_duration
    }

    pub fn timeout_duration(&self) -> Option<Duration> {
        self.timeout_duration
    }
}

impl Default for RequestOptions {
    fn default() -> Self {
        RequestOptions::builder()
            .build()
            .expect("default request options builder should be valid")
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RequestOptionsBuilder {
    max_timeout_duration: Option<Duration>,
    timeout_duration: Option<Duration>,
}

impl RequestOptionsBuilder {
    pub fn new() -> Self {
        RequestOptionsBuilder::default()
    }

    pub fn build(self) -> Result<RequestOptions, RequestOptionsBuilderError> {
        if let Some(timeout_duration) = self.timeout_duration {
            if timeout_duration.as_secs() == 0 {
                return Err(RequestOptionsBuilderError::InvalidTimeoutDuration);
            }
        }

        if let Some(max_timeout_duration) = self.max_timeout_duration {
            if max_timeout_duration.as_secs() == 0 {
                return Err(RequestOptionsBuilderError::InvalidMaxTimeoutDuration);
            }

            if let Some(timeout_duration) = self.timeout_duration {
                if timeout_duration > max_timeout_duration {
                    return Err(RequestOptionsBuilderError::TimeoutDurationGreaterThanMax);
                }
            }
        }

        Ok(RequestOptions {
            max_timeout_duration: self.max_timeout_duration,
            timeout_duration: self.timeout_duration,
        })
    }

    pub fn max_timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.max_timeout_duration = duration;
        self
    }

    pub fn timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.timeout_duration = duration;
        self
    }
}

impl Default for RequestOptionsBuilder {
    fn default() -> Self {
        RequestOptionsBuilder {
            max_timeout_duration: None,
            timeout_duration: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RequestOptionsBuilderError {
    InvalidTimeoutDuration,
    InvalidMaxTimeoutDuration,
    TimeoutDurationGreaterThanMax,
}

impl fmt::Display for RequestOptionsBuilderError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for RequestOptionsBuilderError {
    fn description(&self) -> &str {
        use self::RequestOptionsBuilderError::*;

        match self {
            InvalidTimeoutDuration => "invalid timeout duration",
            InvalidMaxTimeoutDuration => "invalid max timeout duration",
            TimeoutDurationGreaterThanMax => {
                "timeout duration is greater than the max timeout duration"
            }
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
pub struct ConnectionState {
    read_state: ReadState,
    state_changes: Vec<UnboundedSender<ReadWriteStatePair>>,
    write_state: WriteState,
}

impl ConnectionState {
    pub fn new() -> Self {
        ConnectionState {
            read_state: ReadState::All,
            state_changes: vec![],
            write_state: WriteState::All,
        }
    }

    fn send_state_change(&self) {
        for state_change in self.state_changes.iter() {
            state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn add_listener(&mut self) -> UnboundedReceiver<ReadWriteStatePair> {
        let (tx_state_change, rx_state_change) = unbounded();
        self.state_changes.push(tx_state_change);
        rx_state_change
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

struct DecodingTimerTask {
    decode_timeout_duration: Duration,
    decoding_timer: Either<Delay, future::Empty<(), TimerError>>,
    rx_codec_event: Fuse<UnboundedReceiver<CodecEvent>>,
    rx_state_change: Fuse<UnboundedReceiver<ReadWriteStatePair>>,
    state: Arc<Mutex<ConnectionState>>,
}

impl DecodingTimerTask {
    pub fn new(
        state: Arc<Mutex<ConnectionState>>,
        rx_codec_event: UnboundedReceiver<CodecEvent>,
        decode_timeout_duration: Duration,
    ) -> Self {
        let rx_state_change = lock_state(&state).add_listener().fuse();

        DecodingTimerTask {
            decode_timeout_duration,
            decoding_timer: Either::B(future::empty()),
            rx_codec_event: rx_codec_event.fuse(),
            rx_state_change,
            state,
        }
    }

    fn cleanup(&mut self) {
        if !self.rx_codec_event.is_done() {
            self.rx_codec_event.get_mut().close();
        }

        if !self.rx_state_change.is_done() {
            self.rx_state_change.get_mut().close();
        }
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

    fn handle_state_change(&mut self, new_state: ReadWriteStatePair) -> bool {
        if new_state.0.is_none() || new_state.0.is_error() {
            true
        } else {
            false
        }
    }
}

impl Future for DecodingTimerTask {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        // Handle state changes.

        while let Async::Ready(Some(new_state)) = self
            .rx_state_change
            .poll()
            .expect("state change receiver should not error")
        {
            if self.handle_state_change(new_state) {
                self.cleanup();
                return Ok(Async::Ready(()));
            }
        }

        // Handle codec events.

        while let Async::Ready(item) = self
            .rx_codec_event
            .poll()
            .expect("codec event receiver should not error")
        {
            match item {
                Some(event) => self.handle_codec_event(event),
                None => {
                    // The codec that handles decoding and encoding has been dropped. This implies
                    // that the connection is completely closed.

                    lock_state(&self.state).update_state(ReadState::None, WriteState::None);
                    self.cleanup();
                    return Ok(Async::Ready(()));
                }
            }
        }

        // Handle decoding timer.

        if let Async::Ready(()) = self
            .decoding_timer
            .poll()
            .expect("decoding timer should not error")
        {
            lock_state(&self.state).update_state(
                ReadState::Error(ProtocolError::DecodingTimedOut),
                WriteState::Response,
            );
            return Ok(Async::Ready(()));
        }

        Ok(Async::NotReady)
    }
}

struct MessageReceiverTask<S> {
    buffered_requests: HashMap<CSeq, Request<BytesMut>>,
    incoming_sequence_number: Option<CSeq>,
    pending_requests: HashMap<CSeq, oneshot::Sender<PendingRequestResponse>>,
    request_buffer_size: usize,
    rx_pending_request: Fuse<UnboundedReceiver<PendingRequestUpdate>>,
    rx_state_change: Fuse<UnboundedReceiver<ReadWriteStatePair>>,
    tx_incoming_request: Option<UnboundedSender<Request<BytesMut>>>,
    tx_outgoing_message: Option<UnboundedSender<Message>>,
    stream: S,
    state: Arc<Mutex<ConnectionState>>,
}

impl<S> MessageReceiverTask<S>
where
    S: Stream<Item = MessageResult, Error = ProtocolError>,
{
    pub fn new(
        state: Arc<Mutex<ConnectionState>>,
        stream: S,
        rx_pending_request: UnboundedReceiver<PendingRequestUpdate>,
        tx_incoming_request: UnboundedSender<Request<BytesMut>>,
        tx_outgoing_message: UnboundedSender<Message>,
        request_buffer_size: usize,
    ) -> Self {
        let rx_state_change = lock_state(&state).add_listener().fuse();

        MessageReceiverTask {
            buffered_requests: HashMap::with_capacity(request_buffer_size),
            incoming_sequence_number: None,
            pending_requests: HashMap::new(),
            request_buffer_size,
            rx_pending_request: rx_pending_request.fuse(),
            rx_state_change,
            tx_incoming_request: Some(tx_incoming_request),
            tx_outgoing_message: Some(tx_outgoing_message),
            stream,
            state,
        }
    }

    fn cleanup(&mut self) {
        self.remove_pending_requests();
        self.tx_incoming_request = None;
        self.tx_outgoing_message = None;

        if !self.rx_pending_request.is_done() {
            self.rx_pending_request.get_mut().close();
        }

        if !self.rx_state_change.is_done() {
            self.rx_state_change.get_mut().close();
        }
    }

    fn handle_message(&mut self, message: MessageResult) {
        match message {
            Ok(Message::Request(request)) => {
                if lock_state(&self.state).read_state().requests_allowed() {
                    self.handle_request(request);
                }
            }
            Ok(Message::Response(response)) => {
                if lock_state(&self.state).read_state().responses_allowed() {
                    self.handle_response(response);
                }
            }
            Err(InvalidMessage::InvalidRequest(_)) => {
                // We received a request that was invalid, but it still was able to be decoded. We
                // send a `400 Bad Request` to deal with this. This is one situation, however, in
                // which the order that requests are handled is not based on `CSeq`. Even if the
                // message had a valid `CSeq` header, it will not be inspected, since at least one
                // part of the message was syntactically incorrect. As a result, the receiving agent
                // has to manage mapping a response with no `CSeq` to its respective request. This
                // is unlikely, and in general not possible if proxies are involved, since responses
                // can be received out of order.

                self.send_bad_request();
            }
            Err(InvalidMessage::InvalidResponse(_)) => {
                // Received an invalid response, but the only appropriate action here is to just
                // ignore it.
            }
        }
    }

    fn handle_pending_request_update(&mut self, update: PendingRequestUpdate) {
        match update {
            PendingRequestUpdate::AddPendingRequest((cseq, tx_pending_request)) => {
                // A new request has been made that is waiting for a response. When the response
                // has been received, the given oneshot channel will be used to forward the
                // response.

                self.pending_requests.insert(cseq, tx_pending_request);
            }
            PendingRequestUpdate::RemovePendingRequest(cseq) => {
                // A previously pending request has determined that it will no longer wait for a
                // response. This will happen if there is a timeout for the request.

                self.pending_requests.remove(&cseq);
            }
        }
    }

    fn handle_request(&mut self, request: Request<BytesMut>) {
        let header_values = request
            .headers()
            .get_all(HeaderName::CSeq)
            .iter()
            .cloned()
            .collect::<Vec<HeaderValue>>();

        match CSeq::try_from_header_raw(&header_values) {
            Ok(cseq) => {
                // The initial sequence number is defined by the client's first request. So, if we
                // do not currently have a sequence number yet, the one in the request will be used.

                let mut sequence_number = self.incoming_sequence_number.unwrap_or(cseq);

                if *(cseq - sequence_number) > self.request_buffer_size as u32 {
                    // We received a request with a valid `CSeq` header, but the sequence number
                    // referenced is too far out from the current sequence number.
                    //
                    // This is actually a bit of a break from the specification. The specification
                    // states that requests must be handled in the order of their `CSeq`, but this
                    // is not practical here and opens up possible resource exhaustion attacks. The
                    // connection will only process the next expected `CSeq` to abide by the
                    // specification. All other requests will simply be buffered until the next one
                    // with the next `CSeq` has arrived. But the range of possible `CSeq`s is way
                    // too large to buffer per agent, so we only buffer a small amount.

                    self.send_not_enough_bandwidth();
                } else {
                    self.buffered_requests.insert(cseq, request);

                    while let Some(request) = self.buffered_requests.remove(&sequence_number) {
                        if self
                            .tx_incoming_request
                            .as_ref()
                            .expect("incoming request sender should not be `None`")
                            .unbounded_send(request)
                            .is_err()
                        {
                            lock_state(&self.state).update_read_state(ReadState::None);
                            self.tx_incoming_request = None;
                        }

                        sequence_number = sequence_number.increment();
                    }

                    self.incoming_sequence_number = Some(sequence_number);
                }
            }
            Err(_) => {
                // Either no `CSeq` header was found, or parsing of the header failed.
                //
                // To handle this, we send a `400 Bad Request`, but we do not specify a  `CSeq` in
                // the response. Unfortunately, it is not likely the client will even do anything
                // with this response, since it too does not have a `CSeq`. For example, if a client
                // using this implementation managed to send a request with no `CSeq` and received a
                // response with no `CSeq`, it would just ignore it.
                //
                // Also, this message is immediately queued and may not necessarily be the order in
                // which a client would expect the response, but there is no avoiding this. `CSeq`
                // must be given in order to demultiplex.

                self.send_bad_request();
            }
        }
    }

    fn handle_response(&mut self, response: Response<BytesMut>) {
        if response.version() != Version::RTSP20 {
            return;
        }

        let header_values = response
            .headers()
            .get_all(HeaderName::CSeq)
            .iter()
            .cloned()
            .collect::<Vec<HeaderValue>>();
        let cseq = CSeq::try_from_header_raw(&header_values);

        // Make sure the `CSeq` was valid (and that there was only one) and that a request is
        // pending with the given value.

        if let Ok(cseq) = cseq {
            if response.status_code() == StatusCode::Continue {
                // Received a `100 Continue` response meaning we should expect the actual response a
                // bit later. But we want to make sure that any timers for the request do not
                // timeout, so we notify them of the continue.
                //
                // TODO: When NLL is stable, restructure this.

                let mut remove_pending_request = false;

                {
                    if let Some(mut pending_request) = self.pending_requests.get_mut(&cseq) {
                        // Since we are using oneshots here, we need to create a new channel to be
                        // used and send it as well.

                        let (tx_pending_request, rx_pending_request) = oneshot::channel();

                        // We do care if it fails here, since if it fails, we need to remove the
                        // pending request from the hashmap.

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
                // Received a final response for a pending request.
                //
                // We do not really care if this fails. It would only fail in the case where the
                // receiver has been dropped, but either way, we can get rid of the pending request.

                pending_request
                    .send(PendingRequestResponse::Response(response))
                    .ok();
            }

            if self.rx_pending_request.is_done() && self.pending_requests.is_empty() {
                // There are no more pending requests, and we can be sure that no more will be
                // created. So we can change the state to disallow reading responses.

                lock_state(&self.state).update_state(ReadState::Request, WriteState::Response);
            }
        }
    }

    fn handle_state_change(&mut self, new_state: ReadWriteStatePair) -> bool {
        if new_state.0.is_none() || new_state.0.is_error() {
            // We can no longer read anything.

            lock_state(&self.state).update_write_state(WriteState::Response);
            return true;
        }

        if new_state.0.is_request() {
            // We can no longer read responses.

            lock_state(&self.state).update_write_state(WriteState::Response);
        }

        if new_state.0.is_response()
            || new_state.1.is_none()
            || new_state.1.is_error()
            || new_state.1.is_request()
        {
            // We can no longer read requests. We do not change the write state, since there may
            // still be incoming requests that are being handled.

            lock_state(&self.state).update_read_state(ReadState::Response);
            self.tx_incoming_request = None;
        }

        if self.should_exit() {
            return true;
        }

        false
    }

    fn remove_pending_requests(&mut self) {
        self.pending_requests
            .drain()
            .for_each(|(_, tx_pending_request)| {
                // We do not really care if this fails. If would only fail in the case where the
                // receiver has been dropped, but either way, we can get rid of the pending request.

                tx_pending_request.send(PendingRequestResponse::None).ok();
            });
    }

    fn send_bad_request(&mut self) {
        if lock_state(&self.state).write_state().responses_allowed() {
            let response = Response::builder()
                .status_code(StatusCode::BadRequest)
                .build(BytesMut::new())
                .expect("bad request response should not be invalid");
            self.try_send_message(Message::Response(response));
        }
    }

    fn send_not_enough_bandwidth(&mut self) {
        if lock_state(&self.state).write_state().responses_allowed() {
            let response = Response::builder()
                .status_code(StatusCode::NotEnoughBandwidth)
                .build(BytesMut::new())
                .expect("not enough bandwidth response should not be invalid");
            self.try_send_message(Message::Response(response));
        }
    }

    fn send_version_not_supported(&mut self) {
        // TODO: As per specification, the "response SHOULD contain a message body describing why
        // that version is not supported and what other protocols are supported by that agent".

        if lock_state(&self.state).write_state().responses_allowed() {
            let response = Response::builder()
                .status_code(StatusCode::RTSPVersionNotSupported)
                .build(BytesMut::new())
                .expect("RTSP version not supported response should not be invalid");
            self.try_send_message(Message::Response(response));
        }
    }

    fn try_send_message(&mut self, message: Message) {
        self.tx_outgoing_message = self
            .tx_outgoing_message
            .take()
            .and_then(|tx_outgoing_message| {
                tx_outgoing_message
                    .unbounded_send(message)
                    .ok()
                    .map(|_| tx_outgoing_message)
                    .or_else(|| {
                        lock_state(&self.state).update_read_state(ReadState::Response);
                        self.tx_incoming_request = None;
                        None
                    })
            })
    }

    fn should_exit(&self) -> bool {
        let state = lock_state(&self.state).read_state();
        state.is_none() || state.is_error()
    }
}

impl<S> Future for MessageReceiverTask<S>
where
    S: Stream<Item = MessageResult, Error = ProtocolError>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            // Handle state changes.

            while let Async::Ready(Some(new_state)) = self
                .rx_state_change
                .poll()
                .expect("state change receiver should not error")
            {
                if self.handle_state_change(new_state) {
                    self.cleanup();
                    return Ok(Async::Ready(()));
                }
            }

            // Handle pending request updates.

            while let Async::Ready(item) = self
                .rx_pending_request
                .poll()
                .expect("pending request update receiver should not error")
            {
                match item {
                    Some(update) => self.handle_pending_request_update(update),
                    None => {
                        if self.pending_requests.is_empty() {
                            // No more are pending, so we can ignore all incoming responses.
                            // Although this state change will eventually force a change in the
                            // above loop, we need to make sure that we do not allow anymore
                            // responses for the stream reading after this loop.
                            //
                            // If it turns out that we cannot read requests or responses due to this
                            // state change, the task will end on the next poll.

                            lock_state(&self.state).update_read_state(ReadState::Request);
                        }

                        break;
                    }
                }
            }

            // Handle one new message.

            match self.stream.poll() {
                Ok(Async::Ready(Some(message))) => self.handle_message(message),
                Ok(Async::Ready(None)) => {
                    // The incoming message stream has ended. We can only write responses from this
                    // point forward.

                    lock_state(&self.state).update_state(ReadState::None, WriteState::Response);
                    self.cleanup();
                    return Ok(Async::Ready(()));
                }
                Ok(Async::NotReady) => break,
                Err(error) => {
                    // There was an error reading from the message stream that was irrecoverable. We
                    // can no longer read anything, but we may be able to still write responses.
                    // Requests cannot be written, since we would not be able to read their
                    // responses.

                    match error {
                        ProtocolError::DecodeError(DecodeError::InvalidRequest(
                            IrrecoverableInvalidRequest::UnsupportedVersion,
                        ))
                        | ProtocolError::DecodeError(DecodeError::InvalidResponse(
                            IrrecoverableInvalidResponse::UnsupportedVersion,
                        )) => {
                            self.send_version_not_supported();
                        }
                        ProtocolError::DecodeError(_) => self.send_bad_request(),
                        _ => {}
                    }

                    lock_state(&self.state)
                        .update_state(ReadState::Error(error), WriteState::Response);
                    self.cleanup();
                    return Ok(Async::Ready(()));
                }
            }
        }

        Ok(Async::NotReady)
    }
}

struct MessageSenderTask<S> {
    buffered_message: Option<Message>,
    rx_outgoing_message: UnboundedReceiver<Message>,
    state: Arc<Mutex<ConnectionState>>,
    sink: S,
}

impl<S> MessageSenderTask<S>
where
    S: Sink<SinkItem = Message, SinkError = ProtocolError>,
{
    pub fn new(
        state: Arc<Mutex<ConnectionState>>,
        sink: S,
        rx_outgoing_message: UnboundedReceiver<Message>,
    ) -> Self {
        MessageSenderTask {
            buffered_message: None,
            rx_outgoing_message,
            state,
            sink,
        }
    }

    fn try_send_message(&mut self, message: Message) -> Poll<(), ()> {
        match self.sink.start_send(message) {
            Ok(AsyncSink::Ready) => Ok(Async::Ready(())),
            Ok(AsyncSink::NotReady(message)) => {
                self.buffered_message = Some(message);
                Ok(Async::NotReady)
            }
            Err(error) => {
                lock_state(&self.state).update_state(ReadState::Response, WriteState::Error(error));
                Err(())
            }
        }
    }
}

impl<S> Future for MessageSenderTask<S>
where
    S: Sink<SinkItem = Message, SinkError = ProtocolError>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        // Check if we have a buffered message waiting to be sent.

        if let Some(message) = self.buffered_message.take() {
            match self.try_send_message(message) {
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Err(_) => return Ok(Async::Ready(())),
                _ => {}
            }
        }

        loop {
            match self
                .rx_outgoing_message
                .poll()
                .expect("outgoing message receiver should not error")
            {
                Async::Ready(Some(message)) => match self.try_send_message(message) {
                    Ok(Async::NotReady) => break,
                    Err(_) => return Ok(Async::Ready(())),
                    _ => {}
                },
                Async::Ready(None) => {
                    if let Err(error) = self.sink.close() {
                        lock_state(&self.state)
                            .update_state(ReadState::Response, WriteState::Error(error));
                    } else {
                        lock_state(&self.state).update_state(ReadState::Response, WriteState::None);
                    }

                    return Ok(Async::Ready(()));
                }
                Async::NotReady => {
                    if let Err(error) = self.sink.poll_complete() {
                        lock_state(&self.state)
                            .update_state(ReadState::Response, WriteState::Error(error));
                    } else {
                        break;
                    }
                }
            }
        }

        Ok(Async::NotReady)
    }
}

pub struct RequestHandlerTask<S>
where
    S: Service,
{
    rx_incoming_request: UnboundedReceiver<Request<BytesMut>>,
    service: S,
    serviced_request: Option<S::Future>,
    state: Arc<Mutex<ConnectionState>>,
    tx_outgoing_message: UnboundedSender<Message>,
}

impl<S> RequestHandlerTask<S>
where
    S: Service<Request = Request<BytesMut>> + Send + 'static,
    S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, HeaderMap>>,
{
    pub fn new(
        state: Arc<Mutex<ConnectionState>>,
        service: S,
        rx_incoming_request: UnboundedReceiver<Request<BytesMut>>,
        tx_outgoing_message: UnboundedSender<Message>,
    ) -> Self {
        RequestHandlerTask {
            rx_incoming_request,
            service,
            serviced_request: None,
            state,
            tx_outgoing_message,
        }
    }
}

impl<S> Future for RequestHandlerTask<S>
where
    S: Service<Request = Request<BytesMut>> + Send + 'static,
    S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, HeaderMap>>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            if let Some(ref mut serviced_request) = self.serviced_request {
                let response = match serviced_request.poll() {
                    Ok(Async::Ready(response)) => response.into(),
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                    Err(_) => Response::builder()
                        .status_code(StatusCode::InternalServerError)
                        .build(BytesMut::new())
                        .expect("internal server error response should not be invalid"),
                };

                if let Err(_) = self
                    .tx_outgoing_message
                    .unbounded_send(Message::Response(response))
                {
                    lock_state(&self.state).update_state(ReadState::Response, WriteState::Request);
                    return Ok(Async::Ready(()));
                }
            }

            match self
                .rx_incoming_request
                .poll()
                .expect("incoming request receiver should not error")
            {
                Async::Ready(Some(request)) => {
                    self.serviced_request = Some(self.service.call(request));
                }
                Async::Ready(None) => {
                    lock_state(&self.state).update_state(ReadState::Response, WriteState::Request);
                    return Ok(Async::Ready(()));
                }
                Async::NotReady => return Ok(Async::NotReady),
            }
        }
    }
}

pub struct SendRequestFuture {
    max_timer: Option<Delay>,
    rx_response: oneshot::Receiver<PendingRequestResponse>,
    sequence_number: CSeq,
    timeout_duration: Option<Duration>,
    timer: Option<Delay>,
    tx_pending_request: UnboundedSender<PendingRequestUpdate>,
}

impl SendRequestFuture {
    pub(self) fn new(
        rx_response: oneshot::Receiver<PendingRequestResponse>,
        tx_pending_request: UnboundedSender<PendingRequestUpdate>,
        sequence_number: CSeq,
        timeout_duration: Option<Duration>,
        max_timeout_duration: Option<Duration>,
    ) -> Self {
        let max_timer = max_timeout_duration.map(|duration| Delay::new(Instant::now() + duration));
        let timer = timeout_duration.map(|duration| Delay::new(Instant::now() + duration));

        SendRequestFuture {
            max_timer,
            rx_response,
            sequence_number,
            timer,
            timeout_duration,
            tx_pending_request,
        }
    }
}

impl Future for SendRequestFuture {
    type Item = Response<BytesMut>;
    type Error = OperationError;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Async::Ready(response) = self
            .rx_response
            .poll()
            .expect("pending request response receiver should not error")
        {
            match response {
                PendingRequestResponse::Continue(rx_response) => {
                    self.rx_response = rx_response;
                    self.timer = self
                        .timeout_duration
                        .map(|duration| Delay::new(Instant::now() + duration));;
                }
                PendingRequestResponse::None => {
                    self.rx_response.close();
                    return Err(OperationError::RequestCancelled);
                }
                PendingRequestResponse::Response(response) => {
                    self.rx_response.close();
                    return Ok(Async::Ready(response));
                }
            }
        }

        if let Some(timer) = self.max_timer.as_mut() {
            if let Async::Ready(_) = timer.poll().expect("max timer should not error") {
                self.tx_pending_request
                    .unbounded_send(PendingRequestUpdate::RemovePendingRequest(
                        self.sequence_number,
                    ))
                    .ok();
                self.rx_response.close();
                return Err(OperationError::RequestTimedOut(RequestTimeoutType::Long));
            }
        }

        if let Some(timer) = self.timer.as_mut() {
            if let Async::Ready(_) = timer.poll().expect("timer should not error") {
                self.tx_pending_request
                    .unbounded_send(PendingRequestUpdate::RemovePendingRequest(
                        self.sequence_number,
                    ))
                    .ok();
                self.rx_response.close();
                return Err(OperationError::RequestTimedOut(RequestTimeoutType::Short));
            }
        }

        Ok(Async::NotReady)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum ShutdownStatus {
    Running,
    Shutdown,
    ShuttingDown,
}

struct ShutdownTask {
    rx_shutdown: oneshot::Receiver<ShutdownType>,
    rx_state_change: Fuse<UnboundedReceiver<ReadWriteStatePair>>,
    shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
    shutdown_timer: Option<Delay>,
    state: Arc<Mutex<ConnectionState>>,
    status: ShutdownStatus,
}

impl ShutdownTask {
    pub fn new(
        state: Arc<Mutex<ConnectionState>>,
        shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
        rx_shutdown: oneshot::Receiver<ShutdownType>,
    ) -> Self {
        let rx_state_change = lock_state(&state).add_listener().fuse();

        ShutdownTask {
            rx_shutdown,
            rx_state_change,
            shutdown_future,
            shutdown_timer: None,
            state,
            status: ShutdownStatus::Running,
        }
    }

    fn handle_state_change(&mut self, new_state: ReadWriteStatePair) -> bool {
        (new_state.0.is_none() || new_state.0.is_error())
            && (new_state.1.is_none() || new_state.1.is_error())
    }

    fn shutdown(&mut self, shutdown_type: ShutdownType) {
        match shutdown_type {
            ShutdownType::Hard => {
                self.status = ShutdownStatus::Shutdown;
                self.rx_shutdown.close();
                lock_state(&self.state).update_state(ReadState::None, WriteState::None);
            }
            ShutdownType::Soft(duration) => {
                let expire_time = Instant::now() + duration;
                self.shutdown_timer = Some(Delay::new(expire_time));
                self.status = ShutdownStatus::ShuttingDown;
                self.rx_shutdown.close();
                lock_state(&self.state).update_state(ReadState::Response, WriteState::Response);
            }
        }
    }
}

impl Future for ShutdownTask {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            match self.status {
                ShutdownStatus::Running => {
                    // Check if the shutdown future has fired.

                    match self.shutdown_future.poll() {
                        Ok(Async::Ready(shutdown_type)) => self.shutdown(shutdown_type),
                        Err(_) => self.shutdown(ShutdownType::Hard),
                        _ => {}
                    }

                    // Check if the shutdown receiver has received a signal.

                    if let Async::Ready(shutdown_type) = self
                        .rx_shutdown
                        .poll()
                        .expect("shutdown receiver should not error")
                    {
                        self.shutdown(shutdown_type);
                    }

                    if self.status == ShutdownStatus::Running {
                        break;
                    }
                }
                ShutdownStatus::ShuttingDown => {
                    // Handle state changes.

                    while let Async::Ready(Some(new_state)) = self
                        .rx_state_change
                        .poll()
                        .expect("state change receiver should not error")
                    {
                        if self.handle_state_change(new_state) {
                            self.status = ShutdownStatus::Shutdown;
                        }
                    }

                    if let Async::Ready(_) = self
                        .shutdown_timer
                        .as_mut()
                        .expect("timer should exist during shutdown")
                        .poll()
                        .expect("shutdown timer should not error")
                    {
                        lock_state(&self.state).update_state(ReadState::None, WriteState::None);
                        self.status = ShutdownStatus::Shutdown;
                    }

                    if self.status == ShutdownStatus::ShuttingDown {
                        break;
                    }
                }
                ShutdownStatus::Shutdown => {
                    if !self.rx_state_change.is_done() {
                        self.rx_state_change.get_mut().close();
                    }

                    return Ok(Async::Ready(()));
                }
            }
        }

        Ok(Async::NotReady)
    }
}

fn lock_state(state: &Arc<Mutex<ConnectionState>>) -> MutexGuard<ConnectionState> {
    state.lock().expect("acquiring state lock should not fail")
}
