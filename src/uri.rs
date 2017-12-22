//! RTSP URI
//!
//! This is a wrapper around the `Url` type from the `url` crate. This is done because request URIs
//! do not have to actually be URIs (can be `'*'`) and providing `TryFrom` implementations for this
//! type make it easier to deal with the `Request` type. Also, the `Url` type is a strict superset
//! of valid URIs that are allowed in the request line, specifically, the URIs must contain an
//! authority part.

use std::convert::TryFrom;
use std::{error, fmt, str};
use url::Url;

/// This is a wrapper type to prevent users from creating instances of `URI` with unchecked `Url`s.
/// This unfortunately makes using `URI` slightly inconvenient as another level of redirection is
/// required to access the underlying `Url`.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct URIInner(Url);

impl URIInner {
    /// Retrieves the underlying `Url` type immutably.
    pub fn uri(&self) -> &Url {
        &self.0
    }
}

/// The main type of this module. Represents either a valid URI or a URI of the form `'*'`.
#[derive(Clone, Eq, Hash, PartialEq)]
pub enum URI {
    Any,
    URI(URIInner),
}

impl URI {
    /// Returns a `&str` representation of the URI. If the URI is `Any`, then the string returned
    /// will be `*`, otherwise it will be the internal URI as a string.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com").unwrap();
    /// assert_eq!(uri.as_str(), "rtsp://example.com/");
    ///
    /// let uri = URI::try_from("*").unwrap();
    /// assert_eq!(uri.as_str(), "*");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::URI::*;

        match *self {
            Any => "*",
            URI(ref uri) => uri.0.as_str(),
        }
    }
}

impl fmt::Debug for URIInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl fmt::Display for URIInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for URI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl fmt::Display for URI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Provides a fallible conversion from a byte slice to a `URI`. Note that you cannot do the
/// following:
///
/// ```compile_fail
/// let play = URI::try_from(b"*").unwrap();
/// ```
///
/// This is because `b"*"` is of type `&[u8; 1]` and so it must be converted to `&[u8]` in order to
/// perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be provided
/// once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for URI {
    type Error = InvalidURI;

    /// Converts a `&[u8]` to an RTSP URI. The URI must be either a valid URI or must be `'*'`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::URI;
    ///
    /// let any = URI::try_from(&b"*"[..]).unwrap();
    /// assert_eq!(any, URI::Any);
    ///
    /// let uri = URI::try_from(&b"rtsp://example.com"[..]).unwrap();
    ///
    /// match uri {
    ///     URI::Any => panic!("expected valid URI"),
    ///     URI::URI(uri) => println!("got uri: {}", uri)
    /// }
    ///
    /// let error = URI::try_from(&b"this is an invalid URI"[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        use self::URI::*;

        if value.len() == 1 && value[0] == b'*' {
            Ok(Any)
        } else {
            let uri = str::from_utf8(value).map_err(|_| InvalidURI)?;
            let uri = Url::parse(uri).map_err(|_| InvalidURI)?;

            if uri.has_authority() {
                Ok(URI(URIInner(uri)))
            } else {
                Err(InvalidURI)
            }
        }
    }
}

impl<'a> TryFrom<&'a str> for URI {
    type Error = InvalidURI;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        URI::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a `URI` from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidURI;

impl fmt::Display for InvalidURI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for InvalidURI {
    fn description(&self) -> &str {
        "invalid RTSP URI"
    }
}
