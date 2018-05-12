#![feature(non_exhaustive)]
#![feature(try_from)]

extern crate ascii;
extern crate bytes;
extern crate chrono;
extern crate fnv;
extern crate futures;
#[macro_use]
extern crate lazy_static;
extern crate linked_hash_map;
extern crate regex;
extern crate tokio_executor;
extern crate tokio_io;
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
pub mod session;
pub mod status;
pub mod uri;
pub mod version;

pub use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader, TypedHeaderMap};
pub use method::Method;
pub use reason::ReasonPhrase;
pub use request::Request;
pub use response::Response;
pub use session::SessionID;
pub use status::{StatusCode, StatusCodeClass};
pub use uri::URI;
pub use version::Version;
