#![feature(int_error_matching)]
#![feature(non_exhaustive)]
#![feature(type_alias_enum_variants)]

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
