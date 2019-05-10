use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};

/// The setup parameter in a transport specification indicating the roles a client wishes to play in
/// a TCP connection.
///
/// Clients use the setup parameter on the `"Transport"` line in a `"SETUP"` request to indicate the
/// roles it wishes to play in a TCP connection.  This parameter is adapted from
/// [[RFC4145]](https://tools.ietf.org/html/rfc4145). The use of this parameter in RTP/AVP/TCP
/// non-interleaved transport is discussed in
/// [[RFC7826, Appendix C.2.2]](https://tools.ietf.org/html/rfc7826#appendix-C.2.2); the discussion
/// below is limited to syntactic issues.
///
/// If a client does not specify a setup value, the `"active"` value is assumed.
///
/// In response to a client `"SETUP"` request where the setup parameter is set to `"active"`, a
/// server's 2xx reply must assign the setup parameter to `"passive"` on the `"Transport"` header
/// line.
///
/// In response to a client `"SETUP"` request where the setup parameter is set to `"passive"`, a
/// server's 2xx reply must assign the setup parameter to `"active"` on the `"Transport"` header
/// line.
///
/// In response to a client `"SETUP"` request where the setup parameter is set to `"actpass"`, a
/// server's 2xx reply must assign the setup parameter to `"active"` or `"passive"` on the
/// `"Transport"` header line.
///
/// Note that the `"holdconn"` value for setup is not defined for RTSP use, and must not appear on a
/// `"Transport"` line.
///
/// This parameter may only be used if the media-transport protocol of the lower-level transport is
/// connection oriented (such as TCP). However, it cannot be used when interleaving data over an
/// RTSP connection.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Setup {
    /// The client will initiate an outgoing connection.
    Active,

    /// The client will accept an incoming connection.
    ActPass,

    /// The client is willing to accept an incoming connection or to initiate an outgoing
    /// connection.
    Passive,
}

impl Setup {
    /// Returns a `&str` representation of the setup parameter.
    ///
    /// The returned string is always lowercase.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::Setup;
    ///
    /// assert_eq!(Setup::Active.as_str(), "active");
    /// assert_eq!(Setup::ActPass.as_str(), "actpass");
    /// assert_eq!(Setup::Passive.as_str(), "passive");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Setup::*;

        match self {
            Active => "active",
            ActPass => "actpass",
            Passive => "passive",
        }
    }
}

impl AsRef<[u8]> for Setup {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for Setup {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Default for Setup {
    fn default() -> Self {
        Setup::Active
    }
}

impl Display for Setup {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<Setup> for String {
    fn from(value: Setup) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for Setup {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Setup> for [u8] {
    fn eq(&self, other: &Setup) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'setup> PartialEq<&'setup [u8]> for Setup {
    fn eq(&self, other: &&'setup [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'setup> PartialEq<Setup> for &'setup [u8] {
    fn eq(&self, other: &Setup) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Setup {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Setup> for str {
    fn eq(&self, other: &Setup) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'setup> PartialEq<&'setup str> for Setup {
    fn eq(&self, other: &&'setup str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'setup> PartialEq<Setup> for &'setup str {
    fn eq(&self, other: &Setup) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'setup> TryFrom<&'setup [u8]> for Setup {
    type Error = SetupError;

    fn try_from(value: &'setup [u8]) -> Result<Self, Self::Error> {
        use self::Setup::*;

        if value.eq_ignore_ascii_case(b"active") {
            Ok(Active)
        } else if value.eq_ignore_ascii_case(b"actpass") {
            Ok(ActPass)
        } else if value.eq_ignore_ascii_case(b"passive") {
            Ok(Passive)
        } else {
            Err(SetupError)
        }
    }
}

impl<'setup> TryFrom<&'setup str> for Setup {
    type Error = SetupError;

    fn try_from(value: &'setup str) -> Result<Self, Self::Error> {
        Setup::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a [`Setup`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct SetupError;

impl Display for SetupError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "unknown setup parameter")
    }
}

impl Error for SetupError {}

impl From<Infallible> for SetupError {
    fn from(_: Infallible) -> Self {
        SetupError
    }
}
