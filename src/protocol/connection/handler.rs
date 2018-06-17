use bytes::BytesMut;
use futures::sync::mpsc::Receiver;
use futures::{Async, Future, Poll, Stream};

use super::SenderHandle;
use header::HeaderMap;
use protocol::{Message, Service};
use request::Request;
use response::Response;
use status::StatusCode;

#[must_use = "futures do nothing unless polled"]
pub struct RequestHandler<S>
where
    S: Service,
{
    rx_incoming_request: Receiver<Request<BytesMut>>,
    sender_handle: SenderHandle,
    service: S,
    serviced_request: Option<S::Future>,
}

impl<S> RequestHandler<S>
where
    S: Service<Request = Request<BytesMut>>,
    // S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, HeaderMap>>,
{
    pub(crate) fn new(
        service: S,
        rx_incoming_request: Receiver<Request<BytesMut>>,
        sender_handle: SenderHandle,
    ) -> Self {
        RequestHandler {
            rx_incoming_request,
            sender_handle,
            service,
            serviced_request: None,
        }
    }

    fn poll_serviced_request(
        &mut self,
        mut serviced_request: S::Future,
    ) -> Poll<Response<BytesMut>, ()> {
        match serviced_request.poll() {
            Ok(Async::Ready(response)) => Ok(Async::Ready(response.into())),
            Ok(Async::NotReady) => {
                self.serviced_request = Some(serviced_request);
                Ok(Async::NotReady)
            }
            Err(_) => Ok(Async::Ready(
                Response::builder()
                    .status_code(StatusCode::InternalServerError)
                    .build(BytesMut::new())
                    .expect("internal server error response should not be invalid"),
            )),
        }
    }
}

impl<S> Future for RequestHandler<S>
where
    S: Service<Request = Request<BytesMut>>,
    // S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, HeaderMap>>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            if let Some(serviced_request) = self.serviced_request.take() {
                let response = try_ready!(self.poll_serviced_request(serviced_request));
                self.sender_handle
                    .try_send_message(Message::Response(response))?;
            }

            match self
                .rx_incoming_request
                .poll()
                .expect("receiver `rx_incoming_request` should not error")
            {
                Async::Ready(Some(request)) => {
                    self.serviced_request = Some(self.service.call(request))
                }
                Async::Ready(None) => return Ok(Async::Ready(())),
                Async::NotReady => return Ok(Async::NotReady),
            }
        }
    }
}
