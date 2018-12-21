use bytes::BytesMut;
use futures::sync::mpsc::Receiver;
use futures::sync::oneshot;
use futures::{Async, Future, Poll, Stream};
use std::time::{Duration, Instant};
use tokio_timer::Delay;

use super::SenderHandle;
use header::types::CSeq;
use header::{HeaderName, RawHeaderMap, TypedHeader};
use protocol::{Message, Service};
use request::Request;
use response::{Response, NOT_IMPLEMENTED_RESPONSE};
use status::StatusCode;
use uri::Scheme;

#[must_use = "futures do nothing unless polled"]
pub struct RequestHandler<S>
where
    S: Service,
{
    continue_timer: Option<Delay>,
    continue_wait_duration: Option<Duration>,
    rx_incoming_request: Receiver<(CSeq, Request<BytesMut>)>,
    sender_handle: SenderHandle,
    service: S,
    serviced_request: Option<(CSeq, S::Future)>,
    tx_shutdown_event: Option<oneshot::Sender<()>>,
}

impl<S> RequestHandler<S>
where
    S: Service<Request = Request<BytesMut>>,
    S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, RawHeaderMap>>,
{
    pub(crate) fn new(
        service: S,
        rx_incoming_request: Receiver<(CSeq, Request<BytesMut>)>,
        sender_handle: SenderHandle,
        tx_shutdown_event: oneshot::Sender<()>,
        continue_wait_duration: Option<Duration>,
    ) -> Self {
        RequestHandler {
            continue_timer: None,
            continue_wait_duration,
            rx_incoming_request,
            sender_handle,
            service,
            serviced_request: None,
            tx_shutdown_event: Some(tx_shutdown_event),
        }
    }

    fn poll_continue_timer(&mut self, cseq: CSeq) {
        while let Some(mut continue_timer) = self.continue_timer.take() {
            match continue_timer
                .poll()
                .expect("polling `continue_timer` should not error")
            {
                Async::Ready(_) => {
                    let response = Response::builder()
                        .status_code(StatusCode::Continue)
                        .build(BytesMut::new())
                        .expect("continue response should not be invalid");
                    self.send_response(cseq, response);
                    self.reset_continue_timer();
                }
                Async::NotReady => {
                    self.continue_timer = Some(continue_timer);
                    break;
                }
            }
        }
    }

    fn poll_serviced_request(
        &mut self,
        cseq: CSeq,
        mut serviced_request: S::Future,
    ) -> Poll<(), ()> {
        match serviced_request.poll() {
            Ok(Async::Ready(response)) => {
                self.send_response(cseq, response.into());
                self.continue_timer = None;
                Ok(Async::Ready(()))
            }
            Ok(Async::NotReady) => {
                self.poll_continue_timer(cseq);
                self.serviced_request = Some((cseq, serviced_request));
                Ok(Async::NotReady)
            }
            Err(_) => {
                let response = Response::builder()
                    .status_code(StatusCode::InternalServerError)
                    .build(BytesMut::new())
                    .expect("internal server error response should not be invalid");
                self.send_response(cseq, response);
                self.continue_timer = None;
                Ok(Async::Ready(()))
            }
        }
    }

    fn process_request(&mut self, cseq: CSeq, request: Request<BytesMut>) {
        if request.uri().scheme() == Some(Scheme::RTSPU) {
            self.send_response(cseq, NOT_IMPLEMENTED_RESPONSE.clone());
        }

        self.reset_continue_timer();
        self.serviced_request = Some((cseq, self.service.call(request)))
    }

    fn reset_continue_timer(&mut self) {
        if let Some(duration) = self.continue_wait_duration {
            let expire_time = Instant::now() + duration;
            self.continue_timer = Some(Delay::new(expire_time));
        }
    }

    fn send_response(&mut self, cseq: CSeq, mut response: Response<BytesMut>) {
        let cseq = cseq.to_header_raw().remove(0);
        response.headers_mut().insert(HeaderName::CSeq, cseq);

        // We do not care if this fails. All requests that reach this handler will be processed
        // regardless of whether or not a response can actually be sent.

        self.sender_handle
            .try_send_message(Message::Response(response))
            .ok();
    }
}

impl<S> Drop for RequestHandler<S>
where
    S: Service,
{
    fn drop(&mut self) {
        self.tx_shutdown_event
            .take()
            .expect("request handler should only send shutdown event on drop")
            .send(())
            .ok();
    }
}

impl<S> Future for RequestHandler<S>
where
    S: Service<Request = Request<BytesMut>>,
    S::Future: Send + 'static,
    S::Response: Into<Response<BytesMut, RawHeaderMap>>,
{
    type Item = ();
    type Error = ();

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            if let Some((cseq, serviced_request)) = self.serviced_request.take() {
                match self.poll_serviced_request(cseq, serviced_request) {
                    Ok(Async::Ready(_)) => (),
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                    Err(_) => panic!("calling `poll_serviced_request` should not error"),
                }
            }

            match self
                .rx_incoming_request
                .poll()
                .expect("receiver `rx_incoming_request` should not error")
            {
                Async::Ready(Some((cseq, request))) => self.process_request(cseq, request),
                Async::Ready(None) => return Ok(Async::Ready(())),
                Async::NotReady => return Ok(Async::NotReady),
            }
        }
    }
}
