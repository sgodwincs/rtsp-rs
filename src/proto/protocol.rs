use bytes::BytesMut;
use futures::{future, Async, AsyncSink, Future, Poll, Sink, Stream};
use futures::future::{Either, Shared};
use futures::stream::Fuse;
use futures::sync::mpsc::{channel, unbounded, Receiver, UnboundedSender};
use futures::sync::oneshot;
use std::cell::RefCell;
use std::io;
use std::rc::Rc;
use tokio::executor::current_thread;
use tokio::net::TcpStream;
use tokio_io::AsyncRead;

use proto::codec::{Codec, InvalidMessage, Message, MessageResult, RequestResult, ResponseResult};
use request::Request;
use response::Response;

const REQUESTS_BUFFER_SIZE: usize = 10;
const RESPONSES_BUFFER_SIZE: usize = 10;

macro_rules! try_ready_ignore_error {
    ($e:expr) => (match $e {
        Ok(Async::Ready(value)) => value,
        Ok(Async::NotReady) => return Ok(Async::NotReady),
        Err(_) => return Ok(Async::Ready(()))
    })
}

fn wrap_task<T, E>(
    task: impl Future<Item = T, Error = E>,
    tx_shutdown: Rc<RefCell<Option<oneshot::Sender<()>>>>,
    rx_shutdown: Shared<oneshot::Receiver<()>>,
) -> impl Future<Item = (), Error = ()> {
    rx_shutdown
        .map(|_| ())
        .map_err(|_| ())
        .select(task.then(move |_| {
            if let Some(sender) = tx_shutdown.replace(None) {
                sender.send(()).ok();
            }

            Ok::<_, ()>(())
        }))
        .map(|_| ())
        .map_err(|_| ())
}

#[derive(Debug)]
pub struct Protocol {
    error: Rc<RefCell<Option<Rc<io::Error>>>>,
    message_sink: UnboundedSender<Message>,
    request_stream: Receiver<RequestResult>,
    response_channel_sink: UnboundedSender<oneshot::Sender<ResponseResult>>,
    shutdown_oneshot: Rc<RefCell<Option<oneshot::Sender<()>>>>,
}

impl Protocol {
    pub fn new(tcp_stream: TcpStream) -> io::Result<Protocol> {
        let (sink, stream) = tcp_stream.framed(Codec::new()).split();
        let (tx_incoming_request, rx_incoming_request) = channel(REQUESTS_BUFFER_SIZE);
        let (tx_incoming_response, rx_incoming_response) = channel(RESPONSES_BUFFER_SIZE);
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();
        let (tx_matching_response, rx_matching_response) =
            unbounded::<oneshot::Sender<ResponseResult>>();
        let error = Rc::new(RefCell::new(None));
        let (tx_shutdown, rx_shutdown) = oneshot::channel();
        let tx_shutdown = Rc::new(RefCell::new(Some(tx_shutdown)));
        let rx_shutdown = rx_shutdown.shared();

        current_thread::spawn(wrap_task(
            SplitMessagesTask::new(
                stream,
                tx_incoming_request.sink_map_err(|_| ()),
                tx_incoming_response.sink_map_err(|_| ()),
                error.clone(),
            ),
            tx_shutdown.clone(),
            rx_shutdown.clone(),
        ));

        current_thread::spawn(wrap_task(
            SendMessagesTask::new(sink, rx_outgoing_message, error.clone()),
            tx_shutdown.clone(),
            rx_shutdown.clone(),
        ));

        current_thread::spawn(wrap_task(
            rx_incoming_response
                .zip(rx_matching_response)
                .for_each(|(response, channel)| {
                    channel
                        .send(response)
                        .expect("oneshot for matching responses should not be cancelled");
                    Ok(())
                }),
            tx_shutdown.clone(),
            rx_shutdown.clone(),
        ));

        Ok(Protocol {
            error,
            message_sink: tx_outgoing_message,
            request_stream: rx_incoming_request,
            response_channel_sink: tx_matching_response,
            shutdown_oneshot: tx_shutdown,
        })
    }

    pub fn error(&self) -> Option<Rc<io::Error>> {
        self.error.borrow().clone()
    }

    pub fn has_error(&self) -> bool {
        self.error().is_some()
    }

    pub fn is_shutdown(&self) -> bool {
        self.shutdown_oneshot.borrow().is_none()
    }

    pub fn send_request<B>(
        &mut self,
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

        self.message_sink
            .unbounded_send(Message::Request(request))
            .expect("`SendMessagesTask` be running if `is_closed` is false during request sending");
        self.response_channel_sink
            .unbounded_send(tx_incoming_response)
            .expect("matching should be running if `is_closed` is false during request sending");

        let future = rx_incoming_response.then(move |result| {
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

    pub fn send_response<B>(&mut self, response: Response<B>) -> io::Result<()>
    where
        B: AsRef<[u8]>,
    {
        if self.is_shutdown() {
            return Err(io::Error::from(io::ErrorKind::NotConnected));
        }

        let response = response.map(|body| BytesMut::from(body.as_ref()));
        self.message_sink
            .unbounded_send(Message::Response(response))
            .expect(
                "`SendMessagesTask` be running if `is_closed` is false during response sending",
            );

        Ok(())
    }
}

impl Drop for Protocol {
    fn drop(&mut self) {
        if let Some(sender) = self.shutdown_oneshot.replace(None) {
            sender.send(()).ok();
        }
    }
}

#[must_use = "futures do nothing unless polled"]
struct SplitMessagesTask<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream,
{
    buffered_request: Option<RequestResult>,
    buffered_response: Option<ResponseResult>,
    error: Rc<RefCell<Option<Rc<io::Error>>>>,
    request_sink: Option<RequestSink>,
    response_sink: Option<ResponseSink>,
    stream: Option<Fuse<MessageStream>>,
}

impl<MessageStream, RequestSink, ResponseSink>
    SplitMessagesTask<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
{
    pub fn new(
        stream: MessageStream,
        request_sink: RequestSink,
        response_sink: ResponseSink,
        error: Rc<RefCell<Option<Rc<io::Error>>>>,
    ) -> Self {
        SplitMessagesTask {
            buffered_request: None,
            buffered_response: None,
            error,
            request_sink: Some(request_sink),
            response_sink: Some(response_sink),
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

    fn poll_task(&mut self) -> Poll<<Self as Future>::Item, io::Error> {
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
    ) -> Poll<<Self as Future>::Item, <Self as Future>::Error> {
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
    ) -> Poll<<Self as Future>::Item, <Self as Future>::Error> {
        debug_assert!(self.buffered_response.is_none());

        if let AsyncSink::NotReady(response) = self.response_sink_mut().start_send(response)? {
            self.buffered_response = Some(response);
            Ok(Async::NotReady)
        } else {
            Ok(Async::Ready(()))
        }
    }
}

impl<MessageStream, RequestSink, ResponseSink> Future
    for SplitMessagesTask<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self.poll_task() {
            Ok(Async::Ready(_)) => Ok(Async::Ready(())),
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(error) => {
                *self.error.borrow_mut() = Some(Rc::new(error));
                Err(())
            }
        }
    }
}

pub struct SendMessagesTask<MessageSink, MessageStream>
where
    MessageStream: Stream,
{
    buffered_message: Option<Message>,
    error: Rc<RefCell<Option<Rc<io::Error>>>>,
    message_sink: Option<MessageSink>,
    message_stream: Option<Fuse<MessageStream>>,
}

impl<MessageSink, MessageStream> SendMessagesTask<MessageSink, MessageStream>
where
    MessageSink: Sink<SinkItem = Message, SinkError = io::Error>,
    MessageStream: Stream<Item = Message, Error = ()>,
{
    pub fn new(
        message_sink: MessageSink,
        message_stream: MessageStream,
        error: Rc<RefCell<Option<Rc<io::Error>>>>,
    ) -> Self {
        SendMessagesTask {
            buffered_message: None,
            error,
            message_sink: Some(message_sink),
            message_stream: Some(message_stream.fuse()),
        }
    }

    fn poll_task(&mut self) -> Poll<<Self as Future>::Item, io::Error> {
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
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        match self.poll_task() {
            Ok(Async::Ready(_)) => Ok(Async::Ready(())),
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(error) => {
                *self.error.borrow_mut() = Some(Rc::new(error));
                Err(())
            }
        }
    }
}
