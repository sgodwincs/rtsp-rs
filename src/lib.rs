#![feature(try_from)]

extern crate ascii;

#[macro_use]
macro_rules! byte_map {
    ($($flag:expr,)*) => ([
        $($flag != 0,)*
    ])
}

pub mod method;
pub mod status;
pub mod version;

pub use method::Method;
pub use status::{StatusCode, StatusCodeClass};
pub use version::Version;
