use bytes::BytesMut;
use futures::{future, Async, Future, Poll};
use std::io;
use tower_service::Service;

use crate::request::Request;
use crate::response::Response;

pub struct EmptyService;

impl Service<Request<BytesMut>> for EmptyService {
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<dyn Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _: Request<BytesMut>) -> Self::Future {
        Box::new(future::empty())
    }

    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        Ok(Async::Ready(()))
    }
}
