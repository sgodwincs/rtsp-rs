#![feature(slice_patterns)]
#![feature(try_from)]
#![recursion_limit = "128"]

extern crate ascii;
extern crate bytes;
extern crate fnv;
extern crate futures;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate tokio;
extern crate tokio_io;
extern crate tokio_proto;
extern crate tokio_service;
extern crate url;

pub mod proto;

mod syntax;

pub mod header;
pub mod method;
pub mod reason;
pub mod request;
pub mod response;
pub mod status;
pub mod uri;
pub mod version;

pub use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader, TypedHeaderMap};
pub use method::Method;
pub use reason::ReasonPhrase;
pub use request::Request;
pub use response::Response;
pub use status::{StatusCode, StatusCodeClass};
pub use uri::URI;
pub use version::Version;
