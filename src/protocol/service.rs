use futures::Future;
use std::error::Error;

use request::Request;
use response::Response;

pub trait Service {
    type RequestBody;
    type ResponseBody;
    type Error: Into<Box<Error + Send + Sync>>;
    type Future: Future<Item = Response<Self::ResponseBody>, Error = Self::Error>;

    fn call(&mut self, request: Request<Self::RequestBody>) -> Self::Future;
}
