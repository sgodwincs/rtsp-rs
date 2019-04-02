pub use uriparse::{
    Authority, AuthorityError, Fragment, Host, HostError, Password, PasswordError, Path, PathError,
    PortError, Query, QueryError, RegisteredName, RegisteredNameError, Scheme, SchemeError,
    SchemeStatus, Segment, UnregisteredScheme, Username, UsernameError,
};

pub mod request;

/// The default port used for URIs with the RTSP scheme.
pub const RTSP_DEFAULT_PORT: u16 = 554;

/// The default port used for URIs with the RTSPS scheme.
pub const RTSPS_DEFAULT_PORT: u16 = 332;

/// The default port used for URIs with the RTSPU scheme.
pub const RTSPU_DEFAULT_PORT: u16 = 554;
