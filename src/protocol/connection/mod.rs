mod handler;
mod pending;
mod receiver;
mod sender;
mod shutdown;

use self::pending::{PendingRequestResponse, PendingRequestUpdate};
use self::receiver::Receiver;
use self::sender::{Sender, SenderHandle};
use self::shutdown::{Shutdown, ShutdownState};

pub use self::handler::RequestHandler;
pub use self::pending::{
    RequestOptions, RequestOptionsBuilder, RequestOptionsBuilderError, SendRequestFuture,
};
pub use self::shutdown::ShutdownType;

use bytes::BytesMut;
use futures::future::Either;
use futures::sync::mpsc::{channel, unbounded, UnboundedSender};
use futures::sync::oneshot;
use futures::{future, Async, Future, Poll, Stream};
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio_io::{AsyncRead, AsyncWrite};

use crate::header::types::CSeq;
use crate::header::{HeaderMap, HeaderMapExtension, HeaderName, TypedHeader};
use crate::protocol::{Codec, Message, OperationError, Service};
use crate::request::Request;
use crate::response::Response;

pub const DEFAULT_CONTINUE_WAIT_DURATION: Duration = Duration::from_secs(5);
pub const DEFAULT_DECODE_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_GRACEFUL_SHUTDOWN_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUEST_BUFFER_SIZE: usize = 10;
pub const DEFAULT_REQUEST_MAX_TIMEOUT_DURATION: Duration = Duration::from_secs(20);
pub const DEFAULT_REQUEST_TIMEOUT_DURATION: Duration = Duration::from_secs(10);

#[must_use = "futures do nothing unless polled"]
pub struct Connection {
    allow_requests: Arc<AtomicBool>,
    receiver: Option<Receiver>,
    sender: Option<Sender>,
    sender_handle: Option<SenderHandle>,
    shutdown: Shutdown,
}

impl Connection {
    pub fn new<Transport, S>(
        transport: Transport,
        service: Option<S>,
    ) -> (Self, Option<RequestHandler<S>>, ConnectionHandle)
    where
        Transport: AsyncRead + AsyncWrite + Send + 'static,
        S: Service<Request = Request<BytesMut>> + Send + 'static,
        S::Future: Send + 'static,
        S::Response: Into<Response<BytesMut>>,
    {
        Connection::with_config(transport, service, Config::default())
    }

    pub fn with_config<Transport, S>(
        transport: Transport,
        service: Option<S>,
        config: Config,
    ) -> (Self, Option<RequestHandler<S>>, ConnectionHandle)
    where
        Transport: AsyncRead + AsyncWrite + Send + 'static,
        S: Service<Request = Request<BytesMut>> + Send + 'static,
        S::Future: Send + 'static,
        S::Response: Into<Response<BytesMut>>,
    {
        let (tx_codec_event, rx_codec_event) = unbounded();
        let (tx_incoming_request, rx_incoming_request) = channel(config.request_buffer_size());
        let (tx_pending_request, rx_pending_request) = unbounded();
        let (tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_connection_shutdown_event, rx_connection_shutdown_event) = oneshot::channel();
        let (tx_handler_shutdown_event, rx_handler_shutdown_event) = oneshot::channel();
        let (sink, stream) = transport.framed(Codec::with_events(tx_codec_event)).split();

        let receiver = Receiver::new(
            Box::new(stream),
            rx_pending_request,
            rx_codec_event,
            tx_incoming_request,
            config.decode_timeout_duration(),
            config.request_buffer_size(),
        );
        let (sender, sender_handle) = Sender::new(Box::new(sink));
        let handler = if let Some(service) = service {
            Some(RequestHandler::new(
                service,
                rx_incoming_request,
                sender_handle.clone(),
                tx_handler_shutdown_event,
                config.continue_wait_duration(),
            ))
        } else {
            None
        };
        let rx_handler_shutdown_event = if handler.is_some() {
            Some(rx_handler_shutdown_event)
        } else {
            None
        };

        let connection = Connection {
            allow_requests: Arc::new(AtomicBool::new(true)),
            receiver: Some(receiver),
            sender: Some(sender),
            sender_handle: Some(sender_handle.clone()),
            shutdown: Shutdown::new(rx_initiate_shutdown, tx_connection_shutdown_event),
        };
        let connection_handle = ConnectionHandle::new(
            connection.allow_requests.clone(),
            config.graceful_shutdown_default_timeout_duration(),
            config.request_default_max_timeout_duration(),
            config.request_default_timeout_duration(),
            rx_connection_shutdown_event,
            rx_handler_shutdown_event,
            sender_handle,
            tx_pending_request,
            tx_initiate_shutdown,
        );

        (connection, handler, connection_handle)
    }

    fn is_receiver_shutdown(&self) -> bool {
        self.receiver.is_none()
    }

    fn is_sender_shutdown(&self) -> bool {
        self.sender.is_none()
    }

    fn poll_receiver(&mut self) -> Poll<(), ()> {
        if let Some(ref mut receiver) = self.receiver {
            receiver.poll(&mut self.sender_handle)
        } else {
            Ok(Async::Ready(()))
        }
    }

    fn poll_sender(&mut self) -> Poll<(), ()> {
        if let Some(ref mut sender) = self.sender {
            sender.poll().map_err(|_| ())
        } else {
            Ok(Async::Ready(()))
        }
    }

    fn should_shutdown(&self) -> bool {
        self.is_receiver_shutdown() && self.is_sender_shutdown()
    }

    fn shutdown_receiver(&mut self) {
        self.receiver = None;
    }

    fn shutdown_request_receiver(&mut self) {
        if let Some(mut receiver) = self.receiver.take() {
            if !receiver.shutdown_request_receiver() {
                self.receiver = Some(receiver);
            } else {
                self.shutdown_sender();
            }
        }
    }

    fn shutdown_response_receiver(&mut self) {
        if let Some(mut receiver) = self.receiver.take() {
            if !receiver.shutdown_response_receiver() {
                self.receiver = Some(receiver);
            }
        }
    }

    fn shutdown_sender(&mut self) {
        self.sender = None;
        self.sender_handle = None;
    }
}

impl Future for Connection {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self.poll_receiver() {
            Ok(Async::Ready(_)) | Err(_) => {
                self.shutdown_receiver();
                self.shutdown_sender();
            }
            _ => {
                let shutdown_response_receiver = {
                    let receiver = self.receiver.as_mut().unwrap();
                    receiver.is_request_receiver_shutdown()
                        && !self.allow_requests.load(Ordering::SeqCst)
                        && receiver.number_pending() == 0
                };

                if shutdown_response_receiver {
                    self.shutdown_response_receiver();
                }
            }
        }

        match self.poll_sender() {
            Ok(Async::Ready(_)) | Err(_) => {
                self.shutdown_request_receiver();
                self.shutdown_sender();
            }
            _ => (),
        }

        self.shutdown
            .poll()
            .expect("polling `shutdown` should not error");

        match self.shutdown.state() {
            ShutdownState::Running => {
                let allow_responses = if let Some(ref receiver) = self.receiver {
                    !receiver.is_response_receiver_shutdown()
                } else {
                    false
                };

                if !allow_responses || self.is_sender_shutdown() {
                    self.allow_requests.store(false, Ordering::SeqCst);
                }
            }
            ShutdownState::ShuttingDown => {
                self.shutdown_request_receiver();
                self.allow_requests.store(false, Ordering::SeqCst);
            }
            ShutdownState::Shutdown => {
                self.shutdown_receiver();
                self.shutdown_sender();
                self.allow_requests.store(false, Ordering::SeqCst);
                return Ok(Async::Ready(()));
            }
        }

        if self.should_shutdown() {
            self.shutdown.ensure_shutdown();
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

#[derive(Clone)]
pub struct ConnectionHandle {
    allow_requests: Arc<AtomicBool>,
    request_default_max_timeout_duration: Option<Duration>,
    request_default_timeout_duration: Option<Duration>,
    sender_handle: SenderHandle,
    sequence_number: Arc<Mutex<CSeq>>,
    shutdown_receiver: Arc<Mutex<Option<ConnectionShutdownReceiver>>>,
    shutdown_sender: Arc<Mutex<ConnectionShutdownSender>>,
    tx_pending_request: UnboundedSender<PendingRequestUpdate>,
}

impl ConnectionHandle {
    pub(self) fn new(
        allow_requests: Arc<AtomicBool>,
        graceful_shutdown_default_timeout_duration: Duration,
        request_default_max_timeout_duration: Option<Duration>,
        request_default_timeout_duration: Option<Duration>,
        rx_connection_shutdown_event: oneshot::Receiver<()>,
        rx_handler_shutdown_event: Option<oneshot::Receiver<()>>,
        sender_handle: SenderHandle,
        tx_pending_request: UnboundedSender<PendingRequestUpdate>,
        tx_initiate_shutdown: oneshot::Sender<ShutdownType>,
    ) -> Self {
        let shutdown_receiver = ConnectionShutdownReceiver::new(
            rx_connection_shutdown_event,
            rx_handler_shutdown_event,
        );
        let shutdown_sender = ConnectionShutdownSender::new(
            graceful_shutdown_default_timeout_duration,
            tx_initiate_shutdown,
        );

        ConnectionHandle {
            allow_requests,
            request_default_max_timeout_duration,
            request_default_timeout_duration,
            sender_handle,
            sequence_number: Arc::new(Mutex::new(
                CSeq::try_from(0).expect("sequence number `0` should not be invalid"),
            )),
            shutdown_receiver: Arc::new(Mutex::new(Some(shutdown_receiver))),
            shutdown_sender: Arc::new(Mutex::new(shutdown_sender)),
            tx_pending_request,
        }
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
        if !self.allow_requests.load(Ordering::SeqCst) {
            return Either::A(future::err(OperationError::Closed));
        }

        let mut lock = self
            .sequence_number
            .lock()
            .expect("locking `sequence_number` should not error");
        let sequence_number = *lock;
        let mut request = request.into().map(|body| BytesMut::from(body.as_ref()));
        request.headers_mut().typed_insert(sequence_number);

        let (tx_response, rx_response) = oneshot::channel();
        let update = PendingRequestUpdate::AddPendingRequest((sequence_number, tx_response));

        if let Err(_) = self.tx_pending_request.unbounded_send(update) {
            return Either::A(future::err(OperationError::Closed));
        }

        if let Err(_) = self
            .sender_handle
            .try_send_message(Message::Request(request))
        {
            self.tx_pending_request
                .unbounded_send(PendingRequestUpdate::RemovePendingRequest(sequence_number))
                .ok();
            return Either::A(future::err(OperationError::Closed));
        }

        *lock = sequence_number.increment();

        Either::B(SendRequestFuture::new(
            rx_response,
            self.tx_pending_request.clone(),
            sequence_number,
            options.timeout_duration(),
            options.max_timeout_duration(),
        ))
    }

    pub fn shutdown(&mut self, shutdown_type: ShutdownType) {
        self.shutdown_sender
            .lock()
            .expect("locking `shutdown_sender` should not error")
            .shutdown(shutdown_type)
    }

    pub fn take_shutdown(&mut self) -> Option<ConnectionShutdownReceiver> {
        self.shutdown_receiver
            .lock()
            .expect("locking `shutdown_receiver` should not error")
            .take()
    }
}

pub struct ConnectionShutdownReceiver {
    rx_connection_shutdown_event: Option<oneshot::Receiver<()>>,
    rx_handler_shutdown_event: Option<oneshot::Receiver<()>>,
}

impl ConnectionShutdownReceiver {
    pub(self) fn new(
        rx_connection_shutdown_event: oneshot::Receiver<()>,
        rx_handler_shutdown_event: Option<oneshot::Receiver<()>>,
    ) -> Self {
        ConnectionShutdownReceiver {
            rx_connection_shutdown_event: Some(rx_connection_shutdown_event),
            rx_handler_shutdown_event,
        }
    }
}

impl Future for ConnectionShutdownReceiver {
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(mut receiver) = self.rx_connection_shutdown_event.take() {
            match receiver.poll() {
                Ok(Async::NotReady) => self.rx_connection_shutdown_event = Some(receiver),
                Err(_) => panic!("connection shutdown event receiver should not error"),
                _ => (),
            }
        }

        if let Some(mut receiver) = self.rx_handler_shutdown_event.take() {
            match receiver.poll() {
                Ok(Async::NotReady) => self.rx_handler_shutdown_event = Some(receiver),
                Err(_) => panic!("handler shutdown event receiver should not error"),
                _ => (),
            }
        }

        if self.rx_connection_shutdown_event.is_none() && self.rx_handler_shutdown_event.is_none() {
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

struct ConnectionShutdownSender {
    graceful_shutdown_default_timeout_duration: Duration,
    tx_initiate_shutdown: Option<oneshot::Sender<ShutdownType>>,
}

impl ConnectionShutdownSender {
    pub fn new(
        graceful_shutdown_default_timeout_duration: Duration,
        tx_initiate_shutdown: oneshot::Sender<ShutdownType>,
    ) -> Self {
        ConnectionShutdownSender {
            graceful_shutdown_default_timeout_duration,
            tx_initiate_shutdown: Some(tx_initiate_shutdown),
        }
    }

    pub fn shutdown(&mut self, shutdown_type: ShutdownType) {
        if let Some(tx_initiate_shutdown) = self.tx_initiate_shutdown.take() {
            tx_initiate_shutdown.send(shutdown_type).ok();
        }
    }
}

impl Drop for ConnectionShutdownSender {
    fn drop(&mut self) {
        let duration = self.graceful_shutdown_default_timeout_duration;
        self.shutdown(ShutdownType::Graceful(duration));
    }
}

pub struct Config {
    continue_wait_duration: Option<Duration>,
    decode_timeout_duration: Duration,
    graceful_shutdown_default_timeout_duration: Duration,
    request_buffer_size: usize,
    request_default_max_timeout_duration: Option<Duration>,
    request_default_timeout_duration: Option<Duration>,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    pub fn new() -> Self {
        Config::default()
    }

    pub fn continue_wait_duration(&self) -> Option<Duration> {
        self.continue_wait_duration
    }

    pub fn decode_timeout_duration(&self) -> Duration {
        self.decode_timeout_duration
    }

    pub fn graceful_shutdown_default_timeout_duration(&self) -> Duration {
        self.graceful_shutdown_default_timeout_duration
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
}

impl Default for Config {
    fn default() -> Self {
        Config::builder()
            .build()
            .expect("default config builder should be valid")
    }
}

pub struct ConfigBuilder {
    continue_wait_duration: Option<Duration>,
    decode_timeout_duration: Duration,
    graceful_shutdown_default_timeout_duration: Duration,
    request_buffer_size: usize,
    request_default_max_timeout_duration: Option<Duration>,
    request_default_timeout_duration: Option<Duration>,
}

impl ConfigBuilder {
    pub fn new() -> Self {
        ConfigBuilder::default()
    }

    pub fn build(self) -> Result<Config, ConfigBuilderError> {
        if let Some(continue_wait_duration) = self.continue_wait_duration {
            if continue_wait_duration.as_secs() == 0 {
                return Err(ConfigBuilderError::InvalidContinueWaitDuration);
            }
        }

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

        Ok(Config {
            continue_wait_duration: self.continue_wait_duration,
            decode_timeout_duration: self.decode_timeout_duration,
            graceful_shutdown_default_timeout_duration: self
                .graceful_shutdown_default_timeout_duration,
            request_buffer_size: self.request_buffer_size,
            request_default_max_timeout_duration: self.request_default_max_timeout_duration,
            request_default_timeout_duration: self.request_default_timeout_duration,
        })
    }

    pub fn continue_wait_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.continue_wait_duration = duration;
        self
    }

    pub fn decode_timeout_duration(&mut self, duration: Duration) -> &mut Self {
        self.decode_timeout_duration = duration;
        self
    }

    pub fn graceful_shutdown_default_timeout_duration(&mut self, duration: Duration) -> &mut Self {
        self.graceful_shutdown_default_timeout_duration = duration;
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
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        ConfigBuilder {
            continue_wait_duration: Some(DEFAULT_CONTINUE_WAIT_DURATION),
            decode_timeout_duration: DEFAULT_DECODE_TIMEOUT_DURATION,
            graceful_shutdown_default_timeout_duration: DEFAULT_GRACEFUL_SHUTDOWN_TIMEOUT_DURATION,
            request_buffer_size: DEFAULT_REQUEST_BUFFER_SIZE,
            request_default_max_timeout_duration: Some(DEFAULT_REQUEST_MAX_TIMEOUT_DURATION),
            request_default_timeout_duration: Some(DEFAULT_REQUEST_TIMEOUT_DURATION),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ConfigBuilderError {
    InvalidContinueWaitDuration,
    InvalidDecodeTimeoutDuration,
    InvalidRequestBufferSize,
    InvalidRequestDefaultMaxTimeoutDuration,
    InvalidRequestDefaultTimeoutDuration,
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
            InvalidContinueWaitDuration => "invalid continue wait duration",
            InvalidDecodeTimeoutDuration => "invalid decode timeout duration",
            InvalidRequestBufferSize => "invalid request buffer size",
            InvalidRequestDefaultMaxTimeoutDuration => {
                "invalid request default max timeout duration"
            }
            InvalidRequestDefaultTimeoutDuration => "invalid request default timeout duration",
        }
    }
}
