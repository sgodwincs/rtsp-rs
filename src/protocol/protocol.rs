//! RTSP Protocol
//!
//! This module specifies a protocol object for managing requests and responses of an underyling
//! IO object (presumably a TCP stream in the insecure case).

use bytes::BytesMut;
use futures::{future, Async, AsyncSink, Future, Poll, Sink, Stream};
use futures::future::{empty, Either, Executor, IntoFuture};
use futures::stream::Fuse;
use futures::sync::mpsc::{channel, unbounded, Receiver, UnboundedSender};
use futures::sync::oneshot;
use std::cell::RefCell;
use std::io;
use std::rc::Rc;
use tokio::executor::current_thread;
use tokio_io::{AsyncRead, AsyncWrite};

use protocol::codec::{Codec, InvalidMessage, Message, MessageResult, RequestResult, ResponseResult};
use request::Request;
use response::Response;

/// The default buffer size for incoming requests. This can be changed by specifying a different
/// buffer size via `Config`.
const DEFAULT_REQUESTS_BUFFER_SIZE: usize = 10;

/// The default buffer size for incoming responses. This can be changed by specifying a different
/// buffer size via `Config`.
const DEFAULT_RESPONSES_BUFFER_SIZE: usize = 10;

/// This is similar to the `try_ready` macro, but it will ignore errors by instead returning
/// `Ok(Async::Ready(()))`. This is used to control what is considered an error by the tasks.
macro_rules! try_ready_ignore_error {
    ($e:expr) => (match $e {
        Ok(Async::Ready(value)) => value,
        Ok(Async::NotReady) => return Ok(Async::NotReady),
        Err(_) => return Ok(Async::Ready(()))
    })
}

/// The underyling protocol for managing requests and responses for both servers and clients. The
/// protocol takes some IO object that implements `AsyncRead` and `AsyncWrite` and provides an
/// interface suitable for RTSP by using the RTSP codec.
///
/// # Details
///
/// Since RTSP agents can both send requests and receive responses, this struct is agnostic to
/// whether a client or server is built on top of it. All incoming requests will be split into
/// request and response streams. The request stream can be iterated via the `for_each_request`
/// function, whereas the responses are not directly available. Because RTSP is a pipelined protocol
/// each time a request is sent, the next response in the response stream is just taken to be its
/// response.
///
/// # Thread Safety
///
/// Currently, this struct is not thread-safe (implements neither `Send` nor `Sync`).
///
/// # Keep Alive
///
/// Because this object can take any IO object implementing `AsyncRead` and `AsyncWrite`, there is
/// no configuration option to set keep alive if the underlying IO is a TCP stream. This must be
/// done prior to constructing the protocol.
#[derive(Debug)]
pub struct Protocol {
    /// A stored IO error that occurred during either reading from the message stream or writing to
    /// the message sink. The presence of this error is always fatal to the protocol and immediately
    /// causes all tasks to stop.
    ///
    /// The IO error is stored rather weirdly here due to the constraint that `io::Error` cannot be
    /// cloned. And since the protocol allows retrieving this error, using
    /// `Rc<RefCell<Option<io::Error>>>` would give users write access to the underlying error. As
    /// a result, the error is again wrapped in another `Rc` to avoid this.
    error: Rc<RefCell<Option<Rc<io::Error>>>>,

    /// A message sink to an unbounded channel where all messages to be sent to the server are sent.
    message_sink: UnboundedSender<Message>,

    /// A stream of all incoming requests from the server. The stream is accessible via either
    /// `for_each_request` or `request_stream`, but only can be taken once.
    request_stream: RefCell<Option<Receiver<RequestResult>>>,

    /// When a request is sent via `send_request`, it needs to be matched to the corresponding
    /// response. Since RTSP is a pipelined protocol, this will always just be the next response
    /// that is not matched to a different request. One of the tasks spawned during `new` will match
    /// each incoming response with the next channel in the stream. It then uses the channel to send
    /// the response back to the caller who sent the request.
    response_channel_sink: UnboundedSender<oneshot::Sender<ResponseResult>>,

    /// An internal shutdown sender used to shutdown all running tasks. This is different from the
    /// shutdown future provided with `Config` in that their users are different, but they have the
    /// same result.
    shutdown_oneshot: Rc<RefCell<Option<oneshot::Sender<()>>>>,
}

impl Protocol {
    /// Constructs a new `Protocol` with the default configuration. All reading and writing will
    /// be done using the given IO object.
    ///
    /// See `with_config` for more information.
    pub fn new<IO>(io: IO) -> Result<Self, ()>
    where
        IO: AsyncRead + AsyncWrite + 'static,
    {
        Protocol::with_config(io, Config::default())
    }

    /// Constructs a new `Protocol` with the given configuration. All reading and writing will
    /// be done using the given IO object.
    ///
    /// Currently, the protocol spawns three tasks using the `current_thread` executor. As a result,
    /// the protocol must be created in the context of `current_thread::run`.
    ///
    /// # Errors
    ///
    /// An error will be returned if one of the protocol tasks were not able to be executed on the
    /// executor.
    pub fn with_config<IO>(io: IO, config: Config) -> Result<Self, ()>
    where
        IO: AsyncRead + AsyncWrite + 'static,
    {
        let (executor, requests_buffer_size, responses_buffer_size, shutdown_future) =
            config.into_parts();
        let (sink, stream) = io.framed(Codec::new()).split();
        let (tx_incoming_request, rx_incoming_request) = channel(requests_buffer_size);
        let (tx_incoming_response, rx_incoming_response) = channel(responses_buffer_size);
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();
        let (tx_matching_response, rx_matching_response) =
            unbounded::<oneshot::Sender<ResponseResult>>();
        let error = Rc::new(RefCell::new(None));
        let (tx_shutdown, rx_shutdown) = oneshot::channel();
        let tx_shutdown = Rc::new(RefCell::new(Some(tx_shutdown)));
        let rx_shutdown = rx_shutdown.map_err(|_| ()).select(shutdown_future).shared();

        executor
            .execute(wrap_task(
                SplitMessagesTask::new(
                    stream,
                    tx_incoming_request.sink_map_err(|_| ()),
                    tx_incoming_response.sink_map_err(|_| ()),
                    rx_shutdown.clone().map(|_| ()).map_err(|_| ()),
                ),
                tx_shutdown.clone(),
                error.clone(),
            ))
            .map_err(|_| ())?;

        executor
            .execute(wrap_task(
                SendMessagesTask::new(sink, rx_outgoing_message),
                tx_shutdown.clone(),
                error.clone(),
            ))
            .map_err(|_| ())?;

        executor
            .execute(wrap_task(
                rx_incoming_response
                    .zip(rx_matching_response)
                    .for_each(|(response, channel)| {
                        channel
                            .send(response)
                            .expect("oneshot for matching responses should not be cancelled");
                        Ok(())
                    })
                    .map_err(|_| panic!("response matching task should never return an error")),
                tx_shutdown.clone(),
                error.clone(),
            ))
            .map_err(|_| ())?;

        Ok(Protocol {
            error,
            message_sink: tx_outgoing_message,
            request_stream: RefCell::new(Some(rx_incoming_request)),
            response_channel_sink: tx_matching_response,
            shutdown_oneshot: tx_shutdown,
        })
    }

    /// Returns any protocol error that has occured.
    pub fn error(&self) -> Option<Rc<io::Error>> {
        self.error.borrow().clone()
    }

    /// Returns a `Future` that calls the given handler for each incoming request. This should
    /// generally be spawned as a task. If it is spawned as a task, note that the stream will end
    /// as soon as the protocol shutsdown, including when the protocol object is dropped.
    ///
    /// # Panics
    ///
    /// A call to this function will panic if either it or `request_stream` have already been
    /// called. This is because calling either of these functions take ownership of the request
    /// stream and thus cannot be used again.
    pub fn for_each_request<F, R>(&self, handler: F) -> impl Future<Item = (), Error = ()>
    where
        F: FnMut(RequestResult) -> R,
        R: IntoFuture<Item = (), Error = ()>,
    {
        self.request_stream
            .borrow_mut()
            .take()
            .expect("request stream can only be consumed once")
            .for_each(handler)
    }

    /// Returns whether or not a protocol error has occurred. If it has, this implies that the
    /// protocol is shutdown.
    pub fn has_error(&self) -> bool {
        self.error().is_some()
    }

    /// Returns whether or not the protocol has been shutdown.
    pub fn is_shutdown(&self) -> bool {
        self.shutdown_oneshot.borrow().is_none()
    }

    /// Returns the underlying request stream. Due to the way it is implemented, it is guaranteed
    /// that this stream will never emit an error. If the protocol is shutdown in any fashion, this
    /// stream will simply end.
    ///
    /// # Panics
    ///
    /// A call to this function will panic if either it or `for_each_request` have already been
    /// called. This is because calling either of these functions take ownership of the request
    /// stream and thus cannot be used again.
    pub fn request_stream(&self) -> impl Stream<Item = RequestResult, Error = ()> {
        self.request_stream
            .borrow_mut()
            .take()
            .expect("request stream can only be consumed once")
            .map_err(|_| ())
    }

    /// Sends the given request to the connected recipient. The returned value is a future resolving
    /// to the response for the given request. It is possible that the response is invalid but was
    /// still able to be parsed. If the response was not able to be parsed, then the underlying IO
    /// is considered to be corrupt and can no longer be used.
    ///
    /// # Errors
    ///
    /// An error will be returned if the agent is no longer connected, an error occurs on the
    /// underyling IO object, or the returned response from the agent is invalid in a way that is
    /// irrecoverable.
    pub fn send_request<B>(
        &self,
        request: Request<B>,
    ) -> impl Future<Item = ResponseResult, Error = Rc<io::Error>>
    where
        B: AsRef<[u8]>,
    {
        if self.is_shutdown() {
            let error = io::Error::from(io::ErrorKind::NotConnected);
            return Either::A(future::err(Rc::new(error)));
        }

        let error = self.error.clone();
        let request = request.map(|body| BytesMut::from(body.as_ref()));
        let (tx_incoming_response, rx_incoming_response) = oneshot::channel();

        // While not explicitly handled here, if an error occurs while trying to send the message
        // through the sink, it will be caught below. This is because any error related to the sink
        // will immediately cause a shutdown.

        self.message_sink
            .unbounded_send(Message::Request(request))
            .expect("`SendMessagesTask` should be running during request sending");
        self.response_channel_sink
            .unbounded_send(tx_incoming_response)
            .expect("response matching task should be running during request sending");

        let future = rx_incoming_response.then(move |result| {
            // An error here means that `tx_incoming_response` has been dropped implying that the
            // protocol itself has been shutdown. Depending on why it was shutdown will determine
            // what error we return.

            result.map_err(|_| {
                if let Some(error) = error.borrow().as_ref() {
                    error.clone()
                } else {
                    let error = io::Error::from(io::ErrorKind::NotConnected);
                    Rc::new(error)
                }
            })
        });

        Either::B(future)
    }

    /// Send the given response to the connected recipient. Calling this function will immediately
    /// add the response to the message queue and requires no waiting.
    ///
    /// # Errors
    ///
    /// An error will occur if the the agent is no longer connected.
    pub fn send_response<B>(&self, response: Response<B>) -> io::Result<()>
    where
        B: AsRef<[u8]>,
    {
        if self.is_shutdown() {
            return Err(io::Error::from(io::ErrorKind::NotConnected));
        }

        let response = response.map(|body| BytesMut::from(body.as_ref()));
        self.message_sink
            .unbounded_send(Message::Response(response))
            .expect("`SendMessagesTask` be running during response sending");

        Ok(())
    }
}

impl Drop for Protocol {
    fn drop(&mut self) {
        // If the protocol has not already been shutdown, do it now.

        if let Some(sender) = self.shutdown_oneshot.replace(None) {
            sender.send(()).ok();
        }
    }
}

pub struct Config {
    executor: Box<Executor<background::Background>>,
    requests_buffer_size: usize,
    responses_buffer_size: usize,
    shutdown_future: Box<Future<Item = (), Error = ()>>,
}

impl Config {
    pub fn new() -> Self {
        Config::default()
    }

    pub fn into_parts(
        self,
    ) -> (
        Box<Executor<background::Background>>,
        usize,
        usize,
        Box<Future<Item = (), Error = ()>>,
    ) {
        let Config {
            executor,
            requests_buffer_size,
            responses_buffer_size,
            shutdown_future,
        } = self;

        (
            executor,
            requests_buffer_size,
            responses_buffer_size,
            shutdown_future,
        )
    }

    pub fn set_executor<E>(&mut self, executor: E)
    where
        E: Executor<background::Background> + 'static,
    {
        self.executor = Box::new(executor);
    }

    pub fn set_request_buffer_size(&mut self, requests_buffer_size: usize) -> Result<(), ()> {
        if requests_buffer_size == 0 {
            return Err(());
        }

        self.requests_buffer_size = requests_buffer_size;
        Ok(())
    }

    pub fn set_response_buffer_size(&mut self, responses_buffer_size: usize) -> Result<(), ()> {
        if responses_buffer_size == 0 {
            return Err(());
        }

        self.responses_buffer_size = responses_buffer_size;
        Ok(())
    }

    pub fn set_shutdown_future<F>(&mut self, shutdown_future: F)
    where
        F: Future<Item = (), Error = ()> + 'static,
    {
        self.shutdown_future = Box::new(shutdown_future);
    }
}

impl Default for Config {
    fn default() -> Self {
        Config {
            executor: Box::new(current_thread::task_executor()),
            requests_buffer_size: DEFAULT_REQUESTS_BUFFER_SIZE,
            responses_buffer_size: DEFAULT_RESPONSES_BUFFER_SIZE,
            shutdown_future: Box::new(empty()),
        }
    }
}

/// This is copied from Hyper.
mod background {
    use futures::{Future, Poll};

    pub struct Background {
        inner: Box<Future<Item = (), Error = ()>>,
    }

    pub fn bg(fut: Box<Future<Item = (), Error = ()>>) -> Background {
        Background { inner: fut }
    }

    impl Future for Background {
        type Item = ();
        type Error = ();

        fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
            self.inner.poll()
        }
    }
}

#[must_use = "futures do nothing unless polled"]
struct SplitMessagesTask<MessageStream, RequestSink, ResponseSink, ShutdownFuture>
where
    MessageStream: Stream,
{
    buffered_request: Option<RequestResult>,
    buffered_response: Option<ResponseResult>,
    request_sink: Option<RequestSink>,
    response_sink: Option<ResponseSink>,
    shutdown_future: ShutdownFuture,
    stream: Option<Fuse<MessageStream>>,
}

impl<MessageStream, RequestSink, ResponseSink, ShutdownFuture>
    SplitMessagesTask<MessageStream, RequestSink, ResponseSink, ShutdownFuture>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
    ShutdownFuture: Future<Item = (), Error = ()>,
{
    pub fn new(
        stream: MessageStream,
        request_sink: RequestSink,
        response_sink: ResponseSink,
        shutdown_future: ShutdownFuture,
    ) -> Self {
        SplitMessagesTask {
            buffered_request: None,
            buffered_response: None,
            request_sink: Some(request_sink),
            response_sink: Some(response_sink),
            shutdown_future,
            stream: Some(stream.fuse()),
        }
    }

    fn close_sinks(&mut self) -> Poll<<Self as Future>::Item, ()> {
        if self.request_sink.is_some() {
            try_ready!(self.request_sink_mut().close());
            self.request_sink = None;
        }

        if self.response_sink.is_some() {
            try_ready!(self.response_sink_mut().close());
            self.response_sink = None;
        }

        Ok(Async::Ready(()))
    }

    fn request_sink_mut(&mut self) -> &mut RequestSink {
        self.request_sink
            .as_mut()
            .take()
            .expect("attempted to poll `SplitMessagesTask` after completion")
    }

    fn response_sink_mut(&mut self) -> &mut ResponseSink {
        self.response_sink
            .as_mut()
            .take()
            .expect("attempted to poll `SplitMessagesTask` after completion")
    }

    fn stream_mut(&mut self) -> &mut Fuse<MessageStream> {
        self.stream
            .as_mut()
            .take()
            .expect("attempt to poll `SplitMessagesTask` after completion")
    }

    fn try_start_send_request(
        &mut self,
        request: RequestResult,
    ) -> Poll<<Self as Future>::Item, ()> {
        debug_assert!(self.buffered_request.is_none());

        if let AsyncSink::NotReady(request) = self.request_sink_mut().start_send(request)? {
            self.buffered_request = Some(request);
            Ok(Async::NotReady)
        } else {
            Ok(Async::Ready(()))
        }
    }

    fn try_start_send_response(
        &mut self,
        response: ResponseResult,
    ) -> Poll<<Self as Future>::Item, ()> {
        debug_assert!(self.buffered_response.is_none());

        if let AsyncSink::NotReady(response) = self.response_sink_mut().start_send(response)? {
            self.buffered_response = Some(response);
            Ok(Async::NotReady)
        } else {
            Ok(Async::Ready(()))
        }
    }
}

impl<MessageStream, RequestSink, ResponseSink, ShutdownFuture> Future
    for SplitMessagesTask<MessageStream, RequestSink, ResponseSink, ShutdownFuture>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
    ShutdownFuture: Future<Item = (), Error = ()>,
{
    type Item = ();
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(request) = self.buffered_request.take() {
            try_ready_ignore_error!(self.try_start_send_request(request));
        }

        if let Some(response) = self.buffered_response.take() {
            try_ready_ignore_error!(self.try_start_send_response(response));
        }

        if self.buffered_request.is_some() || self.buffered_response.is_some() {
            return Ok(Async::NotReady);
        }

        loop {
            match self.shutdown_future.poll() {
                Ok(Async::Ready(_)) | Err(_) => return Ok(Async::Ready(())),
                _ => (),
            }

            match self.stream_mut().poll()? {
                Async::Ready(Some(message)) => match message {
                    Ok(Message::Request(request)) => {
                        try_ready_ignore_error!(self.try_start_send_request(Ok(request)));
                    }
                    Ok(Message::Response(response)) => {
                        try_ready_ignore_error!(self.try_start_send_response(Ok(response)));
                    }
                    Err(InvalidMessage::InvalidRequest(invalid_request)) => {
                        try_ready_ignore_error!(self.try_start_send_request(Err(invalid_request)));
                    }
                    Err(InvalidMessage::InvalidResponse(invalid_response)) => {
                        try_ready_ignore_error!(self.try_start_send_response(Err(
                            invalid_response
                        )));
                    }
                },
                Async::Ready(None) => {
                    try_ready_ignore_error!(self.close_sinks());
                    return Ok(Async::Ready(()));
                }
                Async::NotReady => {
                    try_ready_ignore_error!(self.request_sink_mut().poll_complete());
                    try_ready_ignore_error!(self.response_sink_mut().poll_complete());
                    return Ok(Async::NotReady);
                }
            }
        }
    }
}

#[must_use = "futures do nothing unless polled"]
pub struct SendMessagesTask<MessageSink, MessageStream>
where
    MessageStream: Stream,
{
    buffered_message: Option<Message>,
    message_sink: Option<MessageSink>,
    message_stream: Option<Fuse<MessageStream>>,
}

impl<MessageSink, MessageStream> SendMessagesTask<MessageSink, MessageStream>
where
    MessageSink: Sink<SinkItem = Message, SinkError = io::Error>,
    MessageStream: Stream<Item = Message, Error = ()>,
{
    pub fn new(message_sink: MessageSink, message_stream: MessageStream) -> Self {
        SendMessagesTask {
            buffered_message: None,
            message_sink: Some(message_sink),
            message_stream: Some(message_stream.fuse()),
        }
    }

    fn sink_mut(&mut self) -> &mut MessageSink {
        self.message_sink
            .as_mut()
            .take()
            .expect("Attempted to poll `SendMessagesTask` after completion")
    }

    fn stream_mut(&mut self) -> &mut Fuse<MessageStream> {
        self.message_stream
            .as_mut()
            .take()
            .expect("attempted to poll `SendMessagesTask` after completion")
    }

    fn try_start_send(&mut self, message: Message) -> Poll<<Self as Future>::Item, io::Error> {
        debug_assert!(self.buffered_message.is_none());

        if let AsyncSink::NotReady(item) = self.sink_mut().start_send(message)? {
            self.buffered_message = Some(item);
            Ok(Async::NotReady)
        } else {
            Ok(Async::Ready(()))
        }
    }
}

impl<MessageSink, MessageStream> Future for SendMessagesTask<MessageSink, MessageStream>
where
    MessageSink: Sink<SinkItem = Message, SinkError = io::Error>,
    MessageStream: Stream<Item = Message, Error = ()>,
{
    type Item = ();
    type Error = io::Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(message) = self.buffered_message.take() {
            try_ready!(self.try_start_send(message));
        }

        loop {
            match self.stream_mut().poll() {
                Ok(Async::Ready(Some(message))) => try_ready!(self.try_start_send(message)),
                Ok(Async::Ready(None)) | Err(_) => {
                    try_ready!(self.sink_mut().close());
                    return Ok(Async::Ready(()));
                }
                Ok(Async::NotReady) => {
                    try_ready!(self.sink_mut().poll_complete());
                    return Ok(Async::NotReady);
                }
            }
        }
    }
}

fn wrap_task<TaskFuture>(
    task: TaskFuture,
    tx_shutdown: Rc<RefCell<Option<oneshot::Sender<()>>>>,
    protocol_error: Rc<RefCell<Option<Rc<io::Error>>>>,
) -> background::Background
where
    TaskFuture: Future<Item = (), Error = io::Error> + 'static,
{
    background::bg(Box::new(task.then(move |result| {
        if let Err(error) = result {
            let mut protocol_error = protocol_error.borrow_mut();

            if protocol_error.is_none() {
                *protocol_error = Some(Rc::new(error));
            }
        }

        if let Some(sender) = tx_shutdown.replace(None) {
            sender.send(()).ok();
        }

        Ok(())
    })))
}
