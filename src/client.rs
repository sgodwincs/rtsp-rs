use futures::{Async, AsyncSink, Future, Poll, Sink, Stream};
use futures::stream::Fuse;
use futures::sync::mpsc::channel;
use std::io;
use std::net::SocketAddr;
use tokio_io::AsyncRead;
use tokio_core::net::TcpStream;
use tokio_core::reactor::Handle;

use proto::codec::{Codec, InvalidMessage, Message, MessageResult, RequestResult, ResponseResult};

const REQUESTS_BUFFER_SIZE: usize = 10;
const RESPONSES_BUFFER_SIZE: usize = 10;

pub struct ClientTransport {
    handle: Handle,
    sink: Box<Sink<SinkItem = Message, SinkError = ()>>,
    requests_stream: Box<Stream<Item = RequestResult, Error = ()>>,
    responses_stream: Box<Stream<Item = ResponseResult, Error = ()>>,
}

impl ClientTransport {
    pub fn connect(
        address: SocketAddr,
        handle: Handle,
    ) -> impl Future<Item = ClientTransport, Error = io::Error> {
        TcpStream::connect(&address, &handle).and_then(|tcp_stream| {
            let (sink, stream) = tcp_stream.framed(Codec::new()).split();
            let (requests_sink, requests_stream) = channel(REQUESTS_BUFFER_SIZE);
            let (responses_sink, responses_stream) = channel(RESPONSES_BUFFER_SIZE);

            handle.spawn(SplitMessages::new(
                stream,
                requests_sink.sink_map_err(|_| ()),
                responses_sink.sink_map_err(|_| ()),
            ));

            let transport = ClientTransport {
                handle,
                sink: Box::new(sink.sink_map_err(|_| ())),
                requests_stream: Box::new(requests_stream),
                responses_stream: Box::new(responses_stream),
            };

            Ok(transport)
        })
    }
}

#[must_use = "futures do nothing unless polled"]
struct SplitMessages<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream,
{
    buffered_request: Option<RequestResult>,
    buffered_response: Option<ResponseResult>,
    stream: Option<Fuse<MessageStream>>,
    request_sink: Option<RequestSink>,
    response_sink: Option<ResponseSink>,
}

impl<MessageStream, RequestSink, ResponseSink>
    SplitMessages<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
{
    pub fn new(
        stream: MessageStream,
        request_sink: RequestSink,
        response_sink: ResponseSink,
    ) -> SplitMessages<MessageStream, RequestSink, ResponseSink> {
        SplitMessages {
            buffered_request: None,
            buffered_response: None,
            stream: Some(stream.fuse()),
            request_sink: Some(request_sink),
            response_sink: Some(response_sink),
        }
    }

    fn request_sink_mut(&mut self) -> &mut RequestSink {
        self.request_sink
            .as_mut()
            .take()
            .expect("attempted to poll `SplitMessages` after completion")
    }

    fn response_sink_mut(&mut self) -> &mut ResponseSink {
        self.response_sink
            .as_mut()
            .take()
            .expect("attempted to poll `SplitMessages` after completion")
    }

    fn stream_mut(&mut self) -> &mut Fuse<MessageStream> {
        self.stream
            .as_mut()
            .take()
            .expect("attempt to poll `SplitMessages` after completion")
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
    for SplitMessages<MessageStream, RequestSink, ResponseSink>
where
    MessageStream: Stream<Item = MessageResult, Error = io::Error>,
    RequestSink: Sink<SinkItem = RequestResult, SinkError = ()>,
    ResponseSink: Sink<SinkItem = ResponseResult, SinkError = ()>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Some(request) = self.buffered_request.take() {
            try_ready!(self.try_start_send_request(request));
        }

        if let Some(response) = self.buffered_response.take() {
            try_ready!(self.try_start_send_response(response));
        }

        loop {
            match self.stream_mut().poll().map_err(|_| ())? {
                Async::Ready(Some(message)) => match message {
                    Ok(Message::Request(request)) => {
                        try_ready!(self.try_start_send_request(Ok(request)))
                    }
                    Ok(Message::Response(response)) => {
                        try_ready!(self.try_start_send_response(Ok(response)))
                    }
                    Err(InvalidMessage::InvalidRequest(invalid_request)) => {
                        try_ready!(self.try_start_send_request(Err(invalid_request)))
                    }
                    Err(InvalidMessage::InvalidResponse(invalid_response)) => {
                        try_ready!(self.try_start_send_response(Err(invalid_response)))
                    }
                },
                Async::Ready(None) => {
                    self.request_sink_mut().close()?;
                    self.response_sink_mut().close()?;
                }
                Async::NotReady => {
                    self.request_sink_mut().poll_complete()?;
                    self.response_sink_mut().poll_complete()?;
                    return Ok(Async::NotReady);
                }
            }
        }
    }
}
