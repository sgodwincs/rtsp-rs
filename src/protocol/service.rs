use bytes::BytesMut;
use futures::{future, Future};
use std::error::Error;
use std::io;

use request::Request;
use response::Response;

pub trait Service {
    type RequestBody;
    type ResponseBody;
    type Error: Into<Box<Error + Send + Sync>>;
    type Future: Future<Item = Response<Self::ResponseBody>, Error = Self::Error>;

    fn call(&mut self, request: Request<Self::RequestBody>) -> Self::Future;
}

pub struct EmptyService;

impl Service for EmptyService {
    type RequestBody = BytesMut;
    type ResponseBody = BytesMut;
    type Error = io::Error;
    type Future =
        Box<Future<Item = Response<Self::ResponseBody>, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _: Request<Self::RequestBody>) -> Self::Future {
        Box::new(future::empty())
    }
}
