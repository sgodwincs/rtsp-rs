#![feature(never_type)]
#![feature(non_exhaustive)]
#![feature(try_from)]
#![recursion_limit = "256"]

extern crate ascii;
extern crate bytes;
extern crate chrono;
extern crate itertools;
#[macro_use]
extern crate futures;
#[macro_use]
extern crate lazy_static;
extern crate ordered_multimap;
extern crate rand;
extern crate regex;
#[cfg(test)]
extern crate tokio;
extern crate tokio_executor;
extern crate tokio_io;
extern crate tokio_tcp;
extern crate tokio_timer;
extern crate uriparse;

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

pub use self::client::Client;
pub use self::header::{HeaderName, HeaderValue, HeaderMap, TypedHeader, TypedHeaderMap};
pub use self::method::Method;
pub use self::protocol::Service;
pub use self::reason::ReasonPhrase;
pub use self::request::Request;
pub use self::response::Response;
pub use self::server::Server;
pub use self::session::SessionID;
pub use self::status::{StatusCode, StatusCodeClass};
pub use self::uri::RequestURI;
pub use self::version::Version;
