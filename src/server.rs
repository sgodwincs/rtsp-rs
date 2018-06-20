use bytes::BytesMut;
use futures::Future;
use std::io;

use request::Request;
use response::Response;

pub struct Server;

pub struct RequestHandler;

impl Service for RequestHandler {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _request: Self::Request) -> Self::Future {
        let response = Response::builder().build("".into()).unwrap();
        Box::new(future::ok(response))
    }
}
