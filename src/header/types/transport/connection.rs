use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};

/// The connection parameter in a transport specification indicating client's preference for
/// connection usage.
///
/// Clients use the connection parameter in a transport specification part of the `"Transport"`
/// header in a `"SETUP"` request to indicate the client's preference for either reusing an existing
/// connection between client and server (in which case the client sets the `"connection"` parameter
/// to `"existing"`) or requesting the creation of a new connection between client and server (in
/// which case the client sets the `"connection"` parameter to `"new"`).  Typically, clients use the
/// `"new"` value for the first `"SETUP"` request for a URL, and `"existing"` for subsequent
/// `"SETUP"` requests for a URL.
///
/// If a client SETUP request assigns the `"new"` value to `"connection"`, the server response must
/// also assign the `"new"` value to "connection" on the `"Transport"` line.
///
/// If a client SETUP request assigns the `"existing"` value to `"connection"`, the server response
/// must assign a value of `"existing"` or `"new"` to `"connection"` on the `"Transport"` line, at
/// its discretion.
///
/// The default value of `"connection"` is `"existing"`, for all `"SETUP"` requests (initial and
/// subsequent).
///
/// This parameter may only be used if the media-transport protocol of the lower-level transport is
/// connection oriented (such as TCP). However, it cannot be used when interleaving data over an
/// RTSP connection.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Connection {
    /// Indicates that an existing connection between the client and server is to be reused.
    Existing,

    /// Indicates that a new connection is to be created between the client and server.
    New,
}

impl Connection {
    /// Returns a `&str` representation of the connection parameter.
    ///
    /// The returned string is always lowercase.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::Connection;
    ///
    /// assert_eq!(Connection::Existing.as_str(), "existing");
    /// assert_eq!(Connection::New.as_str(), "new");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Connection::*;

        match self {
            Existing => "existing",
            New => "new",
        }
    }
}

impl AsRef<[u8]> for Connection {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for Connection {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Default for Connection {
    fn default() -> Self {
        Connection::Existing
    }
}

impl Display for Connection {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<Connection> for String {
    fn from(value: Connection) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for Connection {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Connection> for [u8] {
    fn eq(&self, other: &Connection) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'connection> PartialEq<&'connection [u8]> for Connection {
    fn eq(&self, other: &&'connection [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'connection> PartialEq<Connection> for &'connection [u8] {
    fn eq(&self, other: &Connection) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Connection {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Connection> for str {
    fn eq(&self, other: &Connection) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'connection> PartialEq<&'connection str> for Connection {
    fn eq(&self, other: &&'connection str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'connection> PartialEq<Connection> for &'connection str {
    fn eq(&self, other: &Connection) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'connection> TryFrom<&'connection [u8]> for Connection {
    type Error = ConnectionError;

    fn try_from(value: &'connection [u8]) -> Result<Self, Self::Error> {
        use self::Connection::*;

        if value.eq_ignore_ascii_case(b"existing") {
            Ok(Existing)
        } else if value.eq_ignore_ascii_case(b"new") {
            Ok(New)
        } else {
            Err(ConnectionError)
        }
    }
}

impl<'connection> TryFrom<&'connection str> for Connection {
    type Error = ConnectionError;

    fn try_from(value: &'connection str) -> Result<Self, Self::Error> {
        Connection::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a [`Connection`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct ConnectionError;

impl Display for ConnectionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "unknown connection parameter")
    }
}

impl Error for ConnectionError {}

impl From<Infallible> for ConnectionError {
    fn from(_: Infallible) -> Self {
        ConnectionError
    }
}
