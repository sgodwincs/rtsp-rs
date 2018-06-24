#![feature(non_exhaustive)]
#![feature(try_from)]
#![recursion_limit = "256"]

extern crate ascii;
extern crate bytes;
extern crate chrono;
extern crate itertools;
#[macro_use]
extern crate delegate;
extern crate fnv;
#[macro_use]
extern crate futures;
#[macro_use]
extern crate lazy_static;
extern crate rand;
extern crate regex;
#[cfg(test)]
extern crate tokio;
extern crate tokio_executor;
extern crate tokio_io;
extern crate tokio_tcp;
extern crate tokio_timer;
extern crate url;

mod syntax;

pub mod client;
pub mod header;
pub mod method;
pub mod protocol;
pub mod reason;
pub mod request;
pub mod response;
pub mod server;
pub mod session;
pub mod status;
pub mod uri;
pub mod version;

pub use client::Client;
pub use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader, TypedHeaderMap};
pub use method::Method;
pub use protocol::Service;
pub use reason::ReasonPhrase;
pub use request::Request;
pub use response::Response;
pub use server::Server;
pub use session::SessionID;
pub use status::{StatusCode, StatusCodeClass};
pub use uri::{RequestURI, RequestURIField};
pub use version::Version;
