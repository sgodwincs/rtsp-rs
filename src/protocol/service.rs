use bytes::BytesMut;
use futures::{future, Future};
use std::error::Error;
use std::io;

use request::Request;
use response::Response;

pub trait Service {
    type Request;
    type Response;
    type Error: Into<Box<Error + Send + Sync + 'static>>;
    type Future: Future<Item = Self::Response, Error = Self::Error>;

    fn call(&mut self, request: Self::Request) -> Self::Future;
}

pub struct EmptyService;

impl Service for EmptyService {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _: Self::Request) -> Self::Future {
        Box::new(future::empty())
    }
}
