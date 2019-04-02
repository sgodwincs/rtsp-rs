//! Connection Request Handler
//!
//! This module contains the logic for servicing incoming requests and mapping them to responses.

use bytes::BytesMut;
use futures::stream::Fuse;
use futures::sync::mpsc::Receiver;
use futures::sync::oneshot;
use futures::{try_ready, Async, Future, Poll, Stream};
use std::time::{Duration, Instant};
use tokio_timer::Delay;
use tower_service::Service;

use crate::header::map::HeaderMapExtension;
use crate::header::name::HeaderName;
use crate::header::types::{CSeq, ContentLength};
use crate::protocol::codec::Message;
use crate::protocol::connection::sender::SenderHandle;
use crate::request::Request;
use crate::response::{
    Response, BAD_REQUEST_RESPONSE, CONTINUE_RESPONSE, INTERNAL_SERVER_ERROR_RESPONSE,
    NOT_IMPLEMENTED_RESPONSE,
};
use crate::uri::Scheme;

/// The type responsible for servicing incoming requests and sending responses back.
#[derive(Debug)]
#[must_use = "futures do nothing unless polled"]
pub struct RequestHandler<TService>
where
    TService: Service<Request<BytesMut>>,
    TService::Future: Send + 'static,
    TService::Response: Into<Response<BytesMut>>,
{
    /// A timer indicating when a 100 (Continue) response should be sent. This is only sent if the
    /// request currently being serviced does not finish before the timer expires.
    continue_timer: Option<Delay>,

    /// The duration for how long the handler should wait between consecutive 100 (Continue)
    /// responses.
    continue_wait_duration: Option<Duration>,

    /// The stream of incoming requests to be serviced. It is assumed that the requests are coming
    /// in order by their `"CSeq"` headers.
    rx_incoming_request: Fuse<Receiver<(CSeq, Request<BytesMut>)>>,

    /// A handle to the sender instance used for sending responses returned by the service.
    sender_handle: Option<SenderHandle>,

    /// The service that acts as a function for mapping requests to responses.
    service: TService,

    /// The [`Future`] that will finish with the response for the current request being serviced.
    serviced_request: Option<(CSeq, TService::Future)>,

    /// A sender that notifies the receiver when the request handler is shutdown. Shutdown occurs
    /// when all incoming requests have finished being serviced.
    tx_shutdown_event: Option<oneshot::Sender<()>>,
}

impl<TService> RequestHandler<TService>
where
    TService: Service<Request<BytesMut>>,
    TService::Future: Send + 'static,
    TService::Response: Into<Response<BytesMut>>,
{
    /// Constructs a new request handler.
    pub fn new(
        service: TService,
        rx_incoming_request: Receiver<(CSeq, Request<BytesMut>)>,
        sender_handle: SenderHandle,
        tx_shutdown_event: oneshot::Sender<()>,
        continue_wait_duration: Option<Duration>,
    ) -> Self {
        RequestHandler {
            continue_timer: None,
            continue_wait_duration,
            rx_incoming_request: rx_incoming_request.fuse(),
            sender_handle: Some(sender_handle),
            service,
            serviced_request: None,
            tx_shutdown_event: Some(tx_shutdown_event),
        }
    }

    /// Polls the continue timer if it is currently set.
    ///
    /// If the timer expires, a 100 (Continue) response will be sent and the timer will be reset.
    /// There is a possibility of a timer error occurring where there are too many timers. If this
    /// happens, then the timer will be removed and no 100 (Continue) responses will be sent for
    /// this request anymore. This is done intentionally to shed load.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then either a 100 (Continue) response was sent (and
    /// the timer reset) or there was a timer error that will not allow anymore 100 (Continue)
    /// responses to be sent for this request.
    ///
    /// If `Ok(Async::NotReady)` is returned, then the timer is not ready yet.
    ///
    /// The error `Err(())` will never be returned.
    fn poll_continue_timer(&mut self, cseq: CSeq) -> Poll<(), ()> {
        while let Some(continue_timer) = self.continue_timer.as_mut() {
            match continue_timer.poll() {
                Ok(Async::Ready(_)) => {
                    self.send_response(cseq, CONTINUE_RESPONSE.clone());
                    self.reset_continue_timer();
                }
                Ok(Async::NotReady) => return Ok(Async::NotReady),
                Err(ref error) if error.is_at_capacity() => {
                    // There are too many timers currently. In order to shed load, stop sending
                    // 100 (Continue) responses for this request. The client may stop waiting for
                    // the request, but it will still be handled even if the corresponding response
                    // does not make it to the client before the request expires.
                    self.continue_timer = None;
                }
                _ => panic!("continue timer should not be shutdown"),
            }
        }

        Ok(Async::Ready(()))
    }

    /// Polls the current request being serviced.
    ///
    /// If the service returns an error while processing the request, then the handler will send
    /// a 500 (Internal Server Error) response back to the client.
    ///
    /// If `Ok(Async::Ready(()))` is returned, then the request is finished being serviced. This
    /// means the response has been constructed to send back to the client.
    ///
    /// If `Ok(Async::NotReady)` is returned, then the request is still being serviced.
    ///
    /// The error `Err(())` will never be returned.
    fn poll_serviced_request(&mut self) -> Poll<(), ()> {
        match self.serviced_request.as_mut() {
            Some((cseq, serviced_request)) => {
                let cseq = *cseq;

                match serviced_request.poll() {
                    Ok(Async::Ready(response)) => {
                        self.send_response(cseq, response.into());
                        self.continue_timer = None;
                        self.serviced_request = None;
                        Ok(Async::Ready(()))
                    }
                    Ok(Async::NotReady) => {
                        try_ready!(self.poll_continue_timer(cseq));
                        Ok(Async::NotReady)
                    }
                    Err(_) => {
                        self.send_response(cseq, INTERNAL_SERVER_ERROR_RESPONSE.clone());
                        self.continue_timer = None;
                        self.serviced_request = None;
                        Ok(Async::Ready(()))
                    }
                }
            }
            None => Ok(Async::Ready(())),
        }
    }

    /// Starts processing the given request.
    ///
    /// The processing performs some high-level request validation (e.g. [`Scheme::RTSPU`] is not
    /// allowed as a URI scheme). If any of the validations fail, a 400 (Bad Request) response is
    /// sent back with the request never being forwarded to the service. Otherwise, the request is
    /// forwarded to the service and the continue timer is set.
    fn process_request(&mut self, cseq: CSeq, request: Request<BytesMut>) {
        if request.uri().scheme() == Some(Scheme::RTSPU) {
            self.send_response(cseq, NOT_IMPLEMENTED_RESPONSE.clone());
            return;
        }

        match request.headers().typed_get::<ContentLength>() {
            Some(content_length)
                if *content_length > 0
                    && !request.headers().contains_key(&HeaderName::ContentType) =>
            {
                self.send_response(cseq, BAD_REQUEST_RESPONSE.clone());
            }
            _ => {
                self.reset_continue_timer();
                self.serviced_request = Some((cseq, self.service.call(request)));
            }
        }
    }

    /// Resets the continue timer assuming a wait duration was given in the constructor.
    fn reset_continue_timer(&mut self) {
        if let Some(duration) = self.continue_wait_duration {
            let expire_time = Instant::now() + duration;
            self.continue_timer = Some(Delay::new(expire_time));
        }
    }

    /// Attempts to send a response through the internal sender handle.
    ///
    /// It is possible that the sender has already been dropped, but from the perspective of the
    /// request handler, this does not matter. All requests that reach the request handler will be
    /// serviced even if a response can never reach the client.
    fn send_response(&mut self, cseq: CSeq, mut response: Response<BytesMut>) {
        response.headers_mut().typed_insert(cseq);

        if let Some(sender_handle) = self.sender_handle.as_mut() {
            if sender_handle
                .try_send_message(Message::Response(response))
                .is_err()
            {
                // The receive has been dropped implying no more responses can be sent. We'll still
                // process all incoming requests, but since no more responses can be sent, no more
                // requests should be handled other than the one already queued.
                self.sender_handle = None;
            }
        }
    }

    /// Signals that the request handler is shutdown through the internal oneshot.
    ///
    /// In general, shutdown should preferably not occur while the request stream is still active
    /// and definitely should not happen while we are in the middle of servicing a request (as this
    /// can lead to undefined application state).
    fn shutdown(&mut self) {
        self.continue_timer = None;
        self.sender_handle = None;
        self.serviced_request = None;

        if let Some(tx_shutdown_event) = self.tx_shutdown_event.take() {
            let _ = tx_shutdown_event.send(());
        }
    }
}

impl<TService> Drop for RequestHandler<TService>
where
    TService: Service<Request<BytesMut>>,
    TService::Future: Send + 'static,
    TService::Response: Into<Response<BytesMut>>,
{
    fn drop(&mut self) {
        self.shutdown();
    }
}

impl<TService> Future for RequestHandler<TService>
where
    TService: Service<Request<BytesMut>>,
    TService::Future: Send + 'static,
    TService::Response: Into<Response<BytesMut>>,
{
    type Item = ();
    type Error = ();

    /// Polls the request handler to make progress on the current serviced request and to accept
    /// requests from the queue.
    ///
    /// RTSP requires that all requests on a single connection are handled in the order of their
    /// `"CSeq"` header. As a result, new requests are not accepted from the queue until the current
    /// request being serviced is finished.
    ///
    /// If `Ok(Async::Ready(()))` is returned, this implies that the stream of incoming requests has
    /// ended, so there are no more requests to be handled.
    ///
    /// If `Ok(Async::NotReady)` is returned, then there is no request currently be serviced and
    /// there are no requests in the incoming queue.
    ///
    /// The error `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            try_ready!(self.poll_serviced_request());

            match self
                .rx_incoming_request
                .poll()
                .expect("`RequestHandler.rx_incoming_request` should not error")
            {
                Async::Ready(Some((cseq, request))) => self.process_request(cseq, request),
                Async::NotReady => return Ok(Async::NotReady),
                Async::Ready(None) => {
                    self.shutdown();
                    return Ok(Async::Ready(()));
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use bytes::BytesMut;
    use futures::future;
    use futures::sync::{mpsc, oneshot};
    use futures::{Future, Stream};
    use std::convert::TryFrom;
    use std::time::{Duration, Instant};
    use std::{io, mem};
    use tokio::runtime::current_thread;
    use tokio_timer::Delay;
    use tower_service::Service;

    use crate::header::types::{CSeq, ContentLength};
    use crate::method::Method;
    use crate::protocol::codec::Message;
    use crate::protocol::connection::handler::RequestHandler;
    use crate::protocol::connection::sender::SenderHandle;
    use crate::request::Request;
    use crate::response::{
        Response, BAD_REQUEST_RESPONSE, CONTINUE_RESPONSE, NOT_IMPLEMENTED_RESPONSE,
    };
    use crate::uri::request::URI;

    struct DelayedTestService;

    impl Service<Request<BytesMut>> for DelayedTestService {
        type Response = Response<BytesMut>;
        type Error = io::Error;
        type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

        fn call(&mut self, _: Request<BytesMut>) -> Self::Future {
            Box::new(
                Delay::new(Instant::now() + Duration::from_millis(250)).then(|_| {
                    Ok(Response::<()>::builder()
                        .with_body(BytesMut::new())
                        .build()
                        .unwrap())
                }),
            )
        }
    }

    struct TestService;

    impl Service<Request<BytesMut>> for TestService {
        type Response = Response<BytesMut>;
        type Error = io::Error;
        type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

        fn call(&mut self, _: Request<BytesMut>) -> Self::Future {
            Box::new(future::ok(
                Response::<()>::builder()
                    .with_body(BytesMut::new())
                    .build()
                    .unwrap(),
            ))
        }
    }

    #[test]
    fn test_request_handler_continue_response() {
        let (mut tx_incoming_request, rx_incoming_request) = mpsc::channel(10);
        let (tx_outgoing_message, rx_outgoing_message) = mpsc::unbounded();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let sender_handle = SenderHandle(tx_outgoing_message);
        let request_handler = RequestHandler::new(
            DelayedTestService,
            rx_incoming_request,
            sender_handle,
            tx_shutdown_event,
            Some(Duration::from_millis(100)),
        );

        tx_incoming_request
            .try_send((
                CSeq::try_from(0).unwrap(),
                Request::<()>::builder()
                    .with_method(Method::Setup)
                    .with_uri(URI::asterisk())
                    .with_body(BytesMut::new())
                    .build()
                    .unwrap(),
            ))
            .unwrap();
        mem::drop(tx_incoming_request);

        assert!(current_thread::block_on_all(request_handler).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());

        let expected_responses = vec![
            Message::Response(
                CONTINUE_RESPONSE
                    .clone()
                    .into_builder()
                    .with_typed_header(CSeq::try_from(0).unwrap())
                    .build()
                    .unwrap(),
            ),
            Message::Response(
                CONTINUE_RESPONSE
                    .clone()
                    .into_builder()
                    .with_typed_header(CSeq::try_from(0).unwrap())
                    .build()
                    .unwrap(),
            ),
            Message::Response(
                Response::<()>::builder()
                    .with_typed_header(CSeq::try_from(0).unwrap())
                    .with_body(BytesMut::new())
                    .build()
                    .unwrap(),
            ),
        ];
        assert_eq!(
            current_thread::block_on_all(rx_outgoing_message.collect()),
            Ok(expected_responses)
        );
    }

    #[test]
    fn test_request_handler_process_request() {
        let (mut tx_incoming_request, rx_incoming_request) = mpsc::channel(10);
        let (tx_outgoing_message, rx_outgoing_message) = mpsc::unbounded();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let sender_handle = SenderHandle(tx_outgoing_message);
        let request_handler = RequestHandler::new(
            TestService,
            rx_incoming_request,
            sender_handle,
            tx_shutdown_event,
            None,
        );

        tx_incoming_request
            .try_send((
                CSeq::try_from(0).unwrap(),
                Request::<()>::builder()
                    .with_method(Method::Setup)
                    .with_uri(URI::asterisk())
                    .with_body(BytesMut::new())
                    .build()
                    .unwrap(),
            ))
            .unwrap();
        mem::drop(tx_incoming_request);

        assert!(current_thread::block_on_all(request_handler).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());

        let expected_responses = vec![Message::Response(
            Response::<()>::builder()
                .with_typed_header(CSeq::try_from(0).unwrap())
                .with_body(BytesMut::new())
                .build()
                .unwrap(),
        )];
        assert_eq!(
            current_thread::block_on_all(rx_outgoing_message.collect()),
            Ok(expected_responses)
        );
    }

    #[test]
    fn test_request_handler_process_request_missing_content_type() {
        let (mut tx_incoming_request, rx_incoming_request) = mpsc::channel(10);
        let (tx_outgoing_message, rx_outgoing_message) = mpsc::unbounded();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let sender_handle = SenderHandle(tx_outgoing_message);
        let request_handler = RequestHandler::new(
            TestService,
            rx_incoming_request,
            sender_handle,
            tx_shutdown_event,
            None,
        );

        tx_incoming_request
            .try_send((
                CSeq::try_from(0).unwrap(),
                Request::<()>::builder()
                    .with_method(Method::Setup)
                    .with_uri(URI::asterisk())
                    .with_typed_header(ContentLength::try_from(4).unwrap())
                    .with_body(BytesMut::from("body".as_bytes()))
                    .build()
                    .unwrap(),
            ))
            .unwrap();
        mem::drop(tx_incoming_request);

        assert!(current_thread::block_on_all(request_handler).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());

        let expected_responses = vec![Message::Response(
            BAD_REQUEST_RESPONSE
                .clone()
                .into_builder()
                .with_typed_header(CSeq::try_from(0).unwrap())
                .build()
                .unwrap(),
        )];
        assert_eq!(
            current_thread::block_on_all(rx_outgoing_message.collect()),
            Ok(expected_responses)
        );
    }

    #[test]
    fn test_request_handler_process_request_rtspu_scheme() {
        let (mut tx_incoming_request, rx_incoming_request) = mpsc::channel(10);
        let (tx_outgoing_message, rx_outgoing_message) = mpsc::unbounded();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let sender_handle = SenderHandle(tx_outgoing_message);
        let request_handler = RequestHandler::new(
            TestService,
            rx_incoming_request,
            sender_handle,
            tx_shutdown_event,
            None,
        );

        tx_incoming_request
            .try_send((
                CSeq::try_from(0).unwrap(),
                Request::<()>::builder()
                    .with_method(Method::Setup)
                    .with_uri(URI::try_from("rtspu://example.com/").unwrap())
                    .with_body(BytesMut::new())
                    .build()
                    .unwrap(),
            ))
            .unwrap();
        mem::drop(tx_incoming_request);

        assert!(current_thread::block_on_all(request_handler).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());

        let expected_responses = vec![Message::Response(
            NOT_IMPLEMENTED_RESPONSE
                .clone()
                .into_builder()
                .with_typed_header(CSeq::try_from(0).unwrap())
                .build()
                .unwrap(),
        )];
        assert_eq!(
            current_thread::block_on_all(rx_outgoing_message.collect()),
            Ok(expected_responses)
        );
    }

    #[test]
    fn test_request_handler_shutdown() {
        let (tx_incoming_request, rx_incoming_request) = mpsc::channel(10);
        let (tx_outgoing_message, rx_outgoing_message) = mpsc::unbounded();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let sender_handle = SenderHandle(tx_outgoing_message);
        let request_handler = RequestHandler::new(
            TestService,
            rx_incoming_request,
            sender_handle,
            tx_shutdown_event,
            None,
        );

        mem::drop(tx_incoming_request);

        assert!(current_thread::block_on_all(request_handler).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());
        assert_eq!(
            current_thread::block_on_all(rx_outgoing_message.collect()),
            Ok(vec![])
        );
    }
}
