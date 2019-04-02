mod handler;
mod pending;
mod receiver;
mod sender;
mod shutdown;

pub use self::handler::RequestHandler;
pub use self::pending::{
    RequestOptions, RequestOptionsBuilder, SendRequest, REQUEST_MAX_TIMEOUT_DEFAULT_DURATION,
    REQUEST_TIMEOUT_DEFAULT_DURATION,
};
pub use self::sender::SenderHandle;
pub use self::shutdown::ShutdownType;

use bytes::BytesMut;
use futures::future::{Either, Shared};
use futures::stream::{SplitSink, SplitStream};
use futures::sync::mpsc::{self, UnboundedSender};
use futures::sync::oneshot;
use futures::{future, Async, Future, Poll, Stream};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::mem;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio_codec::{Decoder, Framed};
use tokio_io::{AsyncRead, AsyncWrite};

use crate::header::map::HeaderMapExtension;
use crate::header::types::CSeq;
use crate::protocol::codec::{Codec, Message};
use crate::protocol::connection::pending::PendingRequestUpdate;
use crate::protocol::connection::receiver::Receiver;
use crate::protocol::connection::sender::Sender;
use crate::protocol::connection::shutdown::{ShutdownHandler, ShutdownState};
use crate::protocol::service::Service;
use crate::request::Request;
use crate::response::Response;

pub const DEFAULT_CONTINUE_WAIT_DURATION: Duration = Duration::from_secs(5);
pub const DEFAULT_DECODE_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_GRACEFUL_SHUTDOWN_TIMEOUT_DURATION: Duration = Duration::from_secs(10);
pub const DEFAULT_REQUEST_BUFFER_SIZE: usize = 10;

/// Represents an RTSP connection between two RTSP agents.
///
/// RTSP servers and clients are both capable of sending and receiving requests and responses. As a
/// result, they share the same underlying connection logic.
#[must_use = "futures do nothing unless polled"]
pub struct Connection<TTransport>
where
    TTransport: AsyncRead + AsyncWrite + Send + 'static,
{
    /// A shared atomic that determines whether we are allowed to send requests through this
    /// connection.
    allow_requests: Arc<AtomicBool>,

    /// The internal receiver responsible for processing all incoming messages.
    receiver: Option<Receiver<SplitStream<Framed<TTransport, Codec>>>>,

    /// A shutdown event receiver for when the request handler has finished processing all requests.
    rx_handler_shutdown_event: Option<Shared<oneshot::Receiver<()>>>,

    /// The internal sender responsible for sending all outgoing messages through the connection.
    sender: Option<Sender<SplitSink<Framed<TTransport, Codec>>>>,

    /// The shutdown handler that keeps watch for a shutdown signal.
    shutdown: ShutdownHandler,
}

impl<TTransport> Connection<TTransport>
where
    TTransport: AsyncRead + AsyncWrite + Send + 'static,
{
    /// Returns whether the receiver is shutdown.
    fn is_receiver_shutdown(&self) -> bool {
        self.receiver.is_none()
    }

    /// Returns whether the sender is shutdown.
    fn is_sender_shutdown(&self) -> bool {
        self.sender.is_none()
    }

    /// Returns whether both the receiver and sender are shutdown.
    fn is_shutdown(&self) -> bool {
        self.is_receiver_shutdown() && self.is_sender_shutdown()
    }

    /// Constructs a new connection using the default configuration.
    ///
    /// See [`Connection::with_config`] for more information.
    pub fn new<TService>(
        transport: TTransport,
        service: Option<TService>,
    ) -> (Self, Option<RequestHandler<TService>>, ConnectionHandle)
    where
        TService: Service<Request = Request<BytesMut>> + Send + 'static,
        TService::Future: Send + 'static,
        TService::Response: Into<Response<BytesMut>>,
    {
        Connection::with_config(transport, service, Config::default())
    }

    /// Polls the receiver if it is still running.
    fn poll_receiver(&mut self) {
        if let Some(receiver) = self.receiver.as_mut() {
            match receiver.poll() {
                Ok(Async::Ready(_)) | Err(_) => {
                    self.shutdown_receiver();
                }
                _ => (),
            }
        }
    }

    /// Polls the request handler shutdown event receiver to see if it has been shutdown.
    ///
    /// This is a no-op if the receiver is not shutdown. Otherwise, if the request handler is also
    /// shutdown, this means the sender needs to be shutdown as well, so the connection can be
    /// closed.
    fn poll_request_handler_shutdown(&mut self) {
        if self.is_receiver_shutdown() {
            if let Some(rx_handler_shutdown_event) = self.rx_handler_shutdown_event.as_mut() {
                match rx_handler_shutdown_event.poll() {
                    Ok(Async::Ready(_)) | Err(_) => {
                        self.shutdown_sender();
                    }
                    Ok(Async::NotReady) => (),
                }
            }
        }
    }

    /// Polls the sender if it is still running.
    ///
    /// If the sender finishes, then no more messages can be sent. Since no more messages can be
    /// sent, we shutdown request receiving since we would not be able to send responses.
    fn poll_sender(&mut self) {
        if let Some(sender) = self.sender.as_mut() {
            match sender.poll() {
                Ok(Async::Ready(_)) | Err(_) => {
                    self.shutdown_request_receiver();
                    self.shutdown_sender();
                }
                _ => (),
            }
        }
    }

    /// Shuts down the receiver.
    fn shutdown_receiver(&mut self) {
        self.receiver = None;
    }

    /// Shuts down the request receiver.
    ///
    /// If the request receiver was the only remaining active component of the receiver, then the
    /// entire receiver is shutdown.
    fn shutdown_request_receiver(&mut self) {
        if let Some(receiver) = self.receiver.as_mut() {
            if receiver.shutdown_request_receiver() {
                self.receiver = None;
            }
        }
    }

    /// Shuts down the sender.
    fn shutdown_sender(&mut self) {
        self.sender = None;
    }

    /// Constructs a new connection with the given configuration.
    ///
    /// Three different parts are returned: the connection itself, the request handler, and a handle
    /// to the connection.
    ///
    /// The connection should be run as a task until completion. It is responsible for all reading,
    /// writing, and shutdown management.
    ///
    /// The request handler should also be run as a task until completion. It is responsible for
    /// the processing and mapping of incoming requests into responses. The given service will be
    /// used as the mapping function.
    ///
    /// The connection handle is used to send requests and to force a shutdown of the connection if
    /// desired.
    pub fn with_config<TService>(
        transport: TTransport,
        service: Option<TService>,
        config: Config,
    ) -> (Self, Option<RequestHandler<TService>>, ConnectionHandle)
    where
        TService: Service<Request = Request<BytesMut>> + Send + 'static,
        TService::Future: Send + 'static,
        TService::Response: Into<Response<BytesMut>>,
    {
        // Create all channels that the connection components will use to communicate with each
        // other.

        let (tx_codec_event, rx_codec_event) = mpsc::unbounded();
        let (tx_incoming_request, rx_incoming_request) =
            mpsc::channel(config.request_buffer_size());
        let (tx_pending_request, rx_pending_request) = mpsc::unbounded();
        let (tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_connection_shutdown_event, rx_connection_shutdown_event) = oneshot::channel();
        let (tx_handler_shutdown_event, rx_handler_shutdown_event) = oneshot::channel();
        let codec = Codec::with_events(tx_codec_event);
        let (sink, stream) = codec.framed(transport).split();

        // Create individual components. A request handler is only created if a service was given.

        let (sender, sender_handle) = Sender::new(sink);
        let receiver = Receiver::new(
            stream,
            rx_pending_request,
            rx_codec_event,
            tx_incoming_request,
            sender_handle.clone(),
            config.decode_timeout_duration(),
            config.request_buffer_size(),
        );
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
            Some(rx_handler_shutdown_event.shared())
        } else {
            None
        };

        // Create the connection and the connection handle.

        let connection = Connection {
            allow_requests: Arc::new(AtomicBool::new(true)),
            receiver: Some(receiver),
            rx_handler_shutdown_event: rx_handler_shutdown_event.clone(),
            sender: Some(sender),
            shutdown: ShutdownHandler::new(rx_initiate_shutdown, tx_connection_shutdown_event),
        };
        let connection_handle = ConnectionHandle::new(
            connection.allow_requests.clone(),
            rx_connection_shutdown_event.shared(),
            rx_handler_shutdown_event,
            sender_handle,
            tx_pending_request,
            tx_initiate_shutdown,
            config.graceful_shutdown_timeout_default_duration(),
            config.request_max_timeout_default_duration(),
            config.request_timeout_default_duration(),
        );

        (connection, handler, connection_handle)
    }
}

impl<TTransport> Future for Connection<TTransport>
where
    TTransport: AsyncRead + AsyncWrite + Send + 'static,
{
    type Item = ();
    type Error = ();

    /// Polls all aspects of the connection including reading, writing, and shutdown management.
    ///
    /// The only aspect that is no polled here is the request receiver which runs on a separate
    /// task.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the connection is shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there is nothing else to process for the
    /// connection at the moment.
    ///
    /// The error `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        self.poll_receiver();
        self.poll_sender();

        // This cannot error, so ignore it.
        let _ = self.shutdown.poll();

        match self.shutdown.state() {
            ShutdownState::Running => {
                let allow_responses = match self.receiver.as_ref() {
                    Some(receiver) => !receiver.is_response_receiver_shutdown(),
                    None => false,
                };

                // If we cannot accept responses or cannot send messages, disallow sending requests
                // from our side.
                if !allow_responses || self.is_sender_shutdown() {
                    self.allow_requests.store(false, Ordering::SeqCst);
                }

                // Handle the case where the receiver is shutdown, but the request handler may still
                // be processing requests (which keeps the sender open). Once the request handler is
                // done, there is no longer any reason to keep the sender or the connection alive.
                self.poll_request_handler_shutdown();
            }
            ShutdownState::ShuttingDown => {
                // We are entering a graceful shutdown. Do not allow request to be sent or read. We
                // keep the response receiver open because we may still have pending requests, and
                // we keep the sender open because the request handler may still be processing
                // requests.
                self.shutdown_request_receiver();
                self.allow_requests.store(false, Ordering::SeqCst);
            }
            ShutdownState::Shutdown => {
                // The connection has been completely shutdown, no more messages can be sent or
                // read. While the connection is no longer running, the request handler may still be
                // as it will process whatever requests it has in its queue even if it cannot send
                // responses.

                self.shutdown_receiver();
                self.shutdown_sender();
                self.allow_requests.store(false, Ordering::SeqCst);
                return Ok(Async::Ready(()));
            }
        }

        // We may have went from a running state to a shutdown state above, so check again.
        if self.is_shutdown() {
            self.shutdown.force_shutdown();
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

/// A handle to an RTSP connection.
///
/// This can be used to send requests or shutdown the connection.
#[derive(Clone, Debug)]
pub struct ConnectionHandle {
    /// Whether the connection allows us to send requests.
    allow_requests: Arc<AtomicBool>,

    /// The default duration for how long we should wait until a request is considered timed out.
    /// This is not refreshed on each Continue (100) response.
    request_max_timeout_default_duration: Option<Duration>,

    /// The default duration for how long we should wait until a request is considered timed out.
    /// This is refreshed on each Continue (100) response.
    request_timeout_default_duration: Option<Duration>,

    /// A handle to the sender, so that we can send requests.
    sender_handle: SenderHandle,

    /// The next `"CSeq"` that will be used when sending a request.
    sequence_number: Arc<Mutex<CSeq>>,

    /// A receiver which can be used to check when shutdown of the connection and request handler
    /// has finished.
    shutdown_receiver: ConnectionShutdownReceiver,

    /// A shared sender which allows us to shutdown the connection.
    shutdown_sender: Arc<Mutex<ConnectionShutdownSender>>,

    /// A sender used to notify the response receiver that we want to add a new pending request.
    tx_pending_request: UnboundedSender<PendingRequestUpdate>,
}

impl ConnectionHandle {
    /// Constructs a new connection handle.
    #[allow(clippy::too_many_arguments)]
    pub(self) fn new(
        allow_requests: Arc<AtomicBool>,
        rx_connection_shutdown_event: Shared<oneshot::Receiver<()>>,
        rx_handler_shutdown_event: Option<Shared<oneshot::Receiver<()>>>,
        sender_handle: SenderHandle,
        tx_pending_request: UnboundedSender<PendingRequestUpdate>,
        tx_initiate_shutdown: oneshot::Sender<ShutdownType>,
        graceful_shutdown_timeout_default_duration: Duration,
        request_max_timeout_default_duration: Option<Duration>,
        request_timeout_default_duration: Option<Duration>,
    ) -> Self {
        let shutdown_receiver = ConnectionShutdownReceiver::new(
            rx_connection_shutdown_event,
            rx_handler_shutdown_event,
        );
        let shutdown_sender = ConnectionShutdownSender::new(
            tx_initiate_shutdown,
            graceful_shutdown_timeout_default_duration,
        );

        ConnectionHandle {
            allow_requests,
            request_max_timeout_default_duration,
            request_timeout_default_duration,
            sender_handle,
            sequence_number: Arc::new(Mutex::new(CSeq::random())),
            shutdown_receiver,
            shutdown_sender: Arc::new(Mutex::new(shutdown_sender)),
            tx_pending_request,
        }
    }

    /// Sends the given request with default options.
    ///
    /// See [`ConnectionHandle::send_request_with_options`] for more information.
    pub fn send_request<TRequest, TBody>(
        &mut self,
        request: TRequest,
    ) -> impl Future<Item = Response<BytesMut>, Error = OperationError>
    where
        TRequest: Into<Request<TBody>>,
        TBody: AsRef<[u8]>,
    {
        let options = RequestOptions::builder()
            .max_timeout_duration(self.request_max_timeout_default_duration)
            .timeout_duration(self.request_timeout_default_duration)
            .build();

        self.send_request_with_options(request, options)
    }

    /// Sends the given request with the given options.
    ///
    /// A future is returned which will evaluate to the response for the request assuming no errors
    /// occurred. Possible errors include the connection being closed, the request timing out, or
    /// the request being cancelled due to state changes.
    pub fn send_request_with_options<TRequest, TBody>(
        &mut self,
        request: TRequest,
        options: RequestOptions,
    ) -> impl Future<Item = Response<BytesMut>, Error = OperationError>
    where
        TRequest: Into<Request<TBody>>,
        TBody: AsRef<[u8]>,
    {
        if !self.allow_requests.load(Ordering::SeqCst) {
            return Either::A(future::err(OperationError::Closed));
        }

        let mut lock = self
            .sequence_number
            .lock()
            .expect("`ConnectionHandler.sequence_number` should not be poisoned");
        let sequence_number = *lock;

        // Convert the request into the expect transport format, including setting the `"CSeq"`.
        let mut request = request.into().map(|body| BytesMut::from(body.as_ref()));
        request.headers_mut().typed_insert(sequence_number);

        // Notify the response receiver that we are going to be expecting a response for this
        // request.
        let (tx_response, rx_response) = oneshot::channel();
        let update = PendingRequestUpdate::AddPendingRequest((sequence_number, tx_response));

        if self.tx_pending_request.unbounded_send(update).is_err() {
            return Either::A(future::err(OperationError::Closed));
        }

        if self
            .sender_handle
            .try_send_message(Message::Request(request))
            .is_err()
        {
            // The sender is shutdown, so we need to renotify the response receiver and remove the
            // pending request we just added. If this fails as well, then the response receiver has
            // been shutdown, but it does not matter.
            let _ = self
                .tx_pending_request
                .unbounded_send(PendingRequestUpdate::RemovePendingRequest(sequence_number));
            return Either::A(future::err(OperationError::Closed));
        }

        *lock = sequence_number.wrapping_increment();
        mem::drop(lock);

        Either::B(SendRequest::new(
            rx_response,
            self.tx_pending_request.clone(),
            sequence_number,
            options.timeout_duration(),
            options.max_timeout_duration(),
        ))
    }

    /// Shuts down the connection if it is not already shutdown.
    ///
    /// See [`ShutdownType`] for the different ways in which the connection can be shutdown.
    ///
    /// Even if the connection is shutdown, the request handler may still be processing remaining
    /// requests.
    pub fn shutdown(&mut self, shutdown_type: ShutdownType) {
        self.shutdown_sender
            .lock()
            .expect("`ConnectionHandler.shutdown_sender` should not be poisoned")
            .shutdown(shutdown_type)
    }

    /// Returns a future which will finish when both the connection and request handler have been
    /// shutdown.
    pub fn shutdown_receiver(&mut self) -> ConnectionShutdownReceiver {
        self.shutdown_receiver.clone()
    }
}

/// A future that will finish when both the connection and request handler have been shutdown.
#[derive(Clone, Debug)]
pub struct ConnectionShutdownReceiver {
    /// A future which will finish when the connection has been shutdown.
    rx_connection_shutdown_event: Option<Shared<oneshot::Receiver<()>>>,

    /// A future which will finish when the request handler has been shutdown.
    rx_handler_shutdown_event: Option<Shared<oneshot::Receiver<()>>>,
}

impl ConnectionShutdownReceiver {
    /// Constructs a new connection shutdown receiver.
    pub(self) fn new(
        rx_connection_shutdown_event: Shared<oneshot::Receiver<()>>,
        rx_handler_shutdown_event: Option<Shared<oneshot::Receiver<()>>>,
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

    /// Polls to see if both the connection and request handler are shutdown.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then both are shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then both are not shutdown.
    ///
    /// The error `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(mut receiver) = self.rx_connection_shutdown_event.take() {
            if receiver
                .poll()
                .expect(
                    "`ConnectionShutdownReceiver.rx_connection_shutdown_event` should not error",
                )
                .is_not_ready()
            {
                self.rx_connection_shutdown_event = Some(receiver);
            }
        }

        if let Some(mut receiver) = self.rx_handler_shutdown_event.take() {
            if receiver
                .poll()
                .expect("`ConnectionShutdownReceiver.rx_handler_shutdown_event` should not error")
                .is_not_ready()
            {
                self.rx_handler_shutdown_event = Some(receiver);
            }
        }

        if self.rx_connection_shutdown_event.is_none() && self.rx_handler_shutdown_event.is_none() {
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }
}

/// A wrapper type used to initiate connection shutdown from a connection handle.
#[derive(Debug)]
struct ConnectionShutdownSender {
    /// The default duration for a graceful shutdown.
    graceful_shutdown_timeout_default_duration: Duration,

    /// A sender used to initiate a connection shutdown.
    tx_initiate_shutdown: Option<oneshot::Sender<ShutdownType>>,
}

impl ConnectionShutdownSender {
    /// Constructs a new connection shutdown sender.
    pub fn new(
        tx_initiate_shutdown: oneshot::Sender<ShutdownType>,
        graceful_shutdown_timeout_default_duration: Duration,
    ) -> Self {
        ConnectionShutdownSender {
            graceful_shutdown_timeout_default_duration,
            tx_initiate_shutdown: Some(tx_initiate_shutdown),
        }
    }

    /// Attemps to shutdown the connection if it has not already been shutdown.
    pub fn shutdown(&mut self, shutdown_type: ShutdownType) {
        if let Some(tx_initiate_shutdown) = self.tx_initiate_shutdown.take() {
            let _ = tx_initiate_shutdown.send(shutdown_type);
        }
    }
}

impl Drop for ConnectionShutdownSender {
    fn drop(&mut self) {
        let duration = self.graceful_shutdown_timeout_default_duration;
        self.shutdown(ShutdownType::Graceful(duration));
    }
}

/// A configuration option for controlling the behavior of an RTSP connection.
pub struct Config {
    continue_wait_duration: Option<Duration>,
    decode_timeout_duration: Duration,
    graceful_shutdown_timeout_default_duration: Duration,
    request_buffer_size: usize,
    request_max_timeout_default_duration: Option<Duration>,
    request_timeout_default_duration: Option<Duration>,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    pub fn new() -> Self {
        Config::default()
    }

    /// Returns how long the server should wait to send Continue (100) responses, while a request is
    /// being processed.
    pub fn continue_wait_duration(&self) -> Option<Duration> {
        self.continue_wait_duration
    }

    /// Returns how long the server will wait on a decoding step before considering the connection
    /// dead.
    pub fn decode_timeout_duration(&self) -> Duration {
        self.decode_timeout_duration
    }

    /// Returns the default timeout duration for how long a graceful shutdown should take.
    pub fn graceful_shutdown_timeout_default_duration(&self) -> Duration {
        self.graceful_shutdown_timeout_default_duration
    }

    /// Returns how many requests are allow to be buffered on the connection.
    pub fn request_buffer_size(&self) -> usize {
        self.request_buffer_size
    }

    /// Returns the default timeout duration for how long we should wait for a request until it is
    /// considered timed out. This is not refreshed by Continue (100) responses.
    pub fn request_max_timeout_default_duration(&self) -> Option<Duration> {
        self.request_max_timeout_default_duration
    }

    /// Returns the default timeout duration for how long we should wait for a request until it is
    /// considered timed out. This is refreshed by Continue (100) responses.
    pub fn request_timeout_default_duration(&self) -> Option<Duration> {
        self.request_timeout_default_duration
    }
}

impl Default for Config {
    fn default() -> Self {
        Config::builder().build()
    }
}

/// A builder type for constructing a connection configuration instance.
pub struct ConfigBuilder {
    continue_wait_duration: Option<Duration>,
    decode_timeout_duration: Duration,
    graceful_shutdown_timeout_default_duration: Duration,
    request_buffer_size: usize,
    request_max_timeout_default_duration: Option<Duration>,
    request_timeout_default_duration: Option<Duration>,
}

impl ConfigBuilder {
    /// Consumes the builder and constructs the [`Config`].
    pub fn build(self) -> Config {
        Config {
            continue_wait_duration: self.continue_wait_duration,
            decode_timeout_duration: self.decode_timeout_duration,
            graceful_shutdown_timeout_default_duration: self
                .graceful_shutdown_timeout_default_duration,
            request_buffer_size: self.request_buffer_size,
            request_max_timeout_default_duration: self.request_max_timeout_default_duration,
            request_timeout_default_duration: self.request_timeout_default_duration,
        }
    }

    /// Sets how long the server should wait to send Continue (100) responses, while a request is
    /// being processed.
    pub fn continue_wait_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.continue_wait_duration = duration;
        self
    }

    /// Sets how long the server will wait on a decoding step before considering the connection
    /// dead.
    pub fn decode_timeout_duration(&mut self, duration: Duration) -> &mut Self {
        self.decode_timeout_duration = duration;
        self
    }

    /// Sets the default timeout duration for how long a graceful shutdown should take.
    pub fn graceful_shutdown_timeout_default_duration(&mut self, duration: Duration) -> &mut Self {
        self.graceful_shutdown_timeout_default_duration = duration;
        self
    }

    /// Constructs a new config builder.
    pub fn new() -> Self {
        ConfigBuilder {
            continue_wait_duration: Some(DEFAULT_CONTINUE_WAIT_DURATION),
            decode_timeout_duration: DEFAULT_DECODE_TIMEOUT_DURATION,
            graceful_shutdown_timeout_default_duration: DEFAULT_GRACEFUL_SHUTDOWN_TIMEOUT_DURATION,
            request_buffer_size: DEFAULT_REQUEST_BUFFER_SIZE,
            request_max_timeout_default_duration: Some(REQUEST_MAX_TIMEOUT_DEFAULT_DURATION),
            request_timeout_default_duration: Some(REQUEST_TIMEOUT_DEFAULT_DURATION),
        }
    }

    /// Sets how many requests are allow to be buffered on the connection.
    pub fn request_buffer_size(&mut self, size: usize) -> &mut Self {
        self.request_buffer_size = size;
        self
    }

    /// Sets the default timeout duration for how long we should wait for a request until it is
    /// considered timed out. This is not refreshed by Continue (100) responses.
    pub fn request_max_timeout_default_duration(
        &mut self,
        duration: Option<Duration>,
    ) -> &mut Self {
        self.request_max_timeout_default_duration = duration;
        self
    }

    /// Sets the default timeout duration for how long we should wait for a request until it is
    /// considered timed out. This is refreshed by Continue (100) responses.
    pub fn request_timeout_default_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.request_timeout_default_duration = duration;
        self
    }

    /// Consumes the builder and sets how long the server should wait to send Continue (100)
    /// responses, while a request is being processed.
    pub fn with_continue_wait_duration(mut self, duration: Option<Duration>) -> Self {
        self.continue_wait_duration(duration);
        self
    }

    /// Consumes the builder and sets how long the server will wait on a decoding step before
    /// considering the connection dead.
    pub fn with_decode_timeout_duration(mut self, duration: Duration) -> Self {
        self.decode_timeout_duration(duration);
        self
    }

    /// Consumes the builder and sets the default timeout duration for how long a graceful shutdown
    /// should take.
    pub fn with_graceful_shutdown_timeout_default_duration(mut self, duration: Duration) -> Self {
        self.graceful_shutdown_timeout_default_duration(duration);
        self
    }

    /// Consumes the builder and sets how many requests are allow to be buffered on the connection.
    pub fn with_request_buffer_size(mut self, size: usize) -> Self {
        self.request_buffer_size(size);
        self
    }

    /// Consumes the builder and sets the default timeout duration for how long we should wait for a
    /// request until it is  considered timed out. This is not refreshed by Continue (100)
    /// responses.
    pub fn with_request_max_timeout_default_duration(mut self, duration: Option<Duration>) -> Self {
        self.request_max_timeout_default_duration(duration);
        self
    }

    /// Consumes the builder and sets the default timeout duration for how long we should wait for a
    /// request until it is considered timed out. This is refreshed by Continue (100) responses.
    pub fn with_request_timeout_default_duration(mut self, duration: Option<Duration>) -> Self {
        self.request_timeout_default_duration(duration);
        self
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        ConfigBuilder::new()
    }
}

/// An error type related to a specific operation being performed by a caller.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum OperationError {
    /// An attempt was made to send a request when the write state no longer allows sending
    /// requests. This situation can occur if, for example, a graceful shutdown is happening or an
    /// error occurred while trying to send a message to the receiving agent.
    Closed,

    /// A pending request that neither timed out nor received a corresponding response was
    /// cancelled. This will only occur when the read state has been changed such that responses are
    /// no longer able to be read, thus any requests currently pending will be cancelled.
    RequestCancelled,

    /// A pending request timed out while waiting for its corresponding response. For any given
    /// request, there are two timeouts to be considered. The first is a timeout for a duration of
    /// time for which no response is heard. However, if a Continue (100) response is received for
    /// this request, the timer will be reset. The second timer is one for the entire duration for
    /// which we are waiting for the final response regardless of any received Continue (100)
    /// responses.
    RequestTimedOut(RequestTimeoutType),
}

impl Display for OperationError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::OperationError::*;

        match self {
            Closed => write!(formatter, "closed"),
            RequestCancelled => write!(formatter, "request cancelled"),
            RequestTimedOut(RequestTimeoutType::Long) => {
                write!(formatter, "request timed out (long)")
            }
            RequestTimedOut(RequestTimeoutType::Short) => {
                write!(formatter, "request timed out (short)")
            }
        }
    }
}

impl Error for OperationError {}

/// Specifies what kind of request timeout occurred.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RequestTimeoutType {
    /// The timeout was for the entire duration of waiting for the final response. Specifically,
    /// Continue (100) responses are not final responses.
    Long,

    /// The timeout was for a duration for which no response was heard.
    Short,
}

#[cfg(test)]
mod test {
    use futures::stream::{SplitSink, SplitStream};
    use tokio_codec::Framed;
    use tokio_tcp::TcpStream;

    use crate::protocol::codec::Codec;
    use crate::protocol::connection::handler::RequestHandler;
    use crate::protocol::connection::pending::SendRequest;
    use crate::protocol::connection::receiver::Receiver;
    use crate::protocol::connection::sender::{Sender, SenderHandle};
    use crate::protocol::connection::{Connection, ConnectionHandle, ConnectionShutdownReceiver};
    use crate::protocol::service::EmptyService;

    #[test]
    fn test_bounds() {
        fn check_send<Type: Send>() {}
        fn check_send_and_sync<Type: Send + Sync>() {}

        check_send::<Connection<TcpStream>>();
        check_send::<Receiver<SplitStream<Framed<TcpStream, Codec>>>>();
        check_send::<RequestHandler<EmptyService>>();
        check_send::<SendRequest>();
        check_send::<Sender<SplitSink<Framed<TcpStream, Codec>>>>();

        check_send_and_sync::<ConnectionHandle>();
        check_send_and_sync::<ConnectionShutdownReceiver>();
        check_send_and_sync::<SenderHandle>();
    }
}
