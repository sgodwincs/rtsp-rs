use futures::sync::mpsc::{unbounded, UnboundedReceiver, UnboundedSender};
use futures::{Async, AsyncSink, Poll, Sink, Stream};

use header::types::Date;
use header::{HeaderName, TypedHeader};
use protocol::{Message, ProtocolError};

pub struct Sender {
    buffered_message: Option<Message>,
    rx_outgoing_message: Option<UnboundedReceiver<Message>>,
    sink: Box<Sink<SinkItem = Message, SinkError = ProtocolError> + Send + 'static>,
}

impl Sender {
    pub fn new(
        sink: Box<Sink<SinkItem = Message, SinkError = ProtocolError> + Send + 'static>,
    ) -> (Self, SenderHandle) {
        let (tx_outgoing_message, rx_outgoing_message) = unbounded();
        let sender = Sender {
            buffered_message: None,
            rx_outgoing_message: Some(rx_outgoing_message),
            sink,
        };

        (sender, SenderHandle(tx_outgoing_message))
    }

    pub fn poll(&mut self) -> Poll<(), ProtocolError> {
        if let Some(buffered_message) = self.buffered_message.take() {
            if self.try_send_message(buffered_message)?.is_not_ready() {
                return Ok(Async::NotReady);
            }
        }

        try_ready!(self.poll_write());
        self.sink.close()
    }

    fn poll_write(&mut self) -> Poll<(), ProtocolError> {
        if let Some(mut rx_outgoing_message) = self.rx_outgoing_message.take() {
            loop {
                match rx_outgoing_message
                    .poll()
                    .expect("unbounded receiver `rx_outgoing_message` should not error")
                {
                    Async::Ready(Some(mut message)) => {
                        let date = Date::new()
                            .to_header_raw()
                            .into_iter()
                            .nth(0)
                            .expect("`Date` header should always have one header");

                        match message {
                            Message::Request(ref mut request) => {
                                request.headers_mut().insert(HeaderName::Date, date);
                            }
                            Message::Response(ref mut response) => {
                                response.headers_mut().insert(HeaderName::Date, date);
                            }
                        }

                        if let Async::NotReady = self.try_send_message(message)? {
                            self.rx_outgoing_message = Some(rx_outgoing_message);
                            return Ok(Async::NotReady);
                        }
                    }
                    Async::NotReady => {
                        self.sink.poll_complete()?;
                        self.rx_outgoing_message = Some(rx_outgoing_message);
                        return Ok(Async::NotReady);
                    }
                    Async::Ready(None) => break,
                }
            }
        }

        Ok(Async::Ready(()))
    }

    fn try_send_message(&mut self, message: Message) -> Poll<(), ProtocolError> {
        debug_assert!(self.buffered_message.is_none());

        if let AsyncSink::NotReady(message) = self.sink.start_send(message)? {
            self.buffered_message = Some(message);
            return Ok(Async::NotReady);
        }

        Ok(Async::Ready(()))
    }
}

impl Drop for Sender {
    fn drop(&mut self) {
        self.poll_write().ok();
    }
}

#[derive(Clone)]
pub struct SenderHandle(UnboundedSender<Message>);

impl SenderHandle {
    pub fn try_send_message(&mut self, message: Message) -> Result<(), ()> {
        self.0.unbounded_send(message).map_err(|_| ())
    }
}
