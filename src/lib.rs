#![feature(try_from)]

extern crate ascii;
extern crate fnv;
extern crate url;

#[macro_use]
macro_rules! byte_map {
    ($($flag:expr,)*) => ([
        $($flag != 0,)*
    ])
}

pub mod header;
pub mod method;
pub mod request;
pub mod status;
pub mod version;

pub use header::{HeaderMap, HeaderName, HeaderValue};
pub use method::Method;
pub use request::Request;
pub use status::{StatusCode, StatusCodeClass};
pub use version::Version;
