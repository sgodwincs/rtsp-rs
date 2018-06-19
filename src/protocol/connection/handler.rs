use bytes::BytesMut;
use futures::sync::mpsc::Receiver;
use futures::{Async, Future, Poll, Stream};

use super::SenderHandle;
use header::{HeaderMap, HeaderName, HeaderValue};
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
    serviced_request: Option<(HeaderValue, S::Future)>,
}

impl<S> RequestHandler<S>
where
    S: Service<Request = Request<BytesMut>>,
    S::Future: Send + 'static,
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
        serviced_request: &mut S::Future,
    ) -> Poll<Response<BytesMut>, ()> {
        match serviced_request.poll() {
            Ok(Async::Ready(response)) => Ok(Async::Ready(response.into())),
            Ok(Async::NotReady) => Ok(Async::NotReady),
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
    S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, HeaderMap>>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            if let Some(serviced_request) = self.serviced_request.take() {
                let (cseq, mut serviced_request) = serviced_request;

                match self.poll_serviced_request(&mut serviced_request) {
                    Ok(Async::Ready(mut response)) => {
                        response.headers_mut().insert(HeaderName::CSeq, cseq);
                        self.sender_handle
                            .try_send_message(Message::Response(response))?;
                    }
                    Ok(Async::NotReady) => {
                        self.serviced_request = Some((cseq, serviced_request));
                        return Ok(Async::NotReady);
                    }
                    Err(_) => panic!("calling `poll_serviced_request` should not error"),
                }
            }

            match self
                .rx_incoming_request
                .poll()
                .expect("receiver `rx_incoming_request` should not error")
            {
                Async::Ready(Some(request)) => {
                    let cseq = request.headers().get(HeaderName::CSeq).unwrap().clone();
                    self.serviced_request = Some((cseq, self.service.call(request)))
                }
                Async::Ready(None) => return Ok(Async::Ready(())),
                Async::NotReady => return Ok(Async::NotReady),
            }
        }
    }
}
