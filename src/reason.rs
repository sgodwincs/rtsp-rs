//! RTSP Reason Types
//!
//! This module contains structs related to RTSP reason phrases, notably the `ReasonPhrase` type
//! itself. Typically, you will import the `rtsp::ReasonPhrase` type rather than reaching into this
//! module itself.

use std::convert::TryFrom;
use std::error::Error;
use std::{fmt, str};

/// A wrapper type used to avoid users creating invalid reason phrases for the `Response` type.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct ReasonPhrase(String);

impl ReasonPhrase {
    /// Returns a `&str` representation of the reason phrase.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::ReasonPhrase;
    ///
    /// assert_eq!(ReasonPhrase::try_from("OK").unwrap().as_str(), "OK");
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Converts a string representation into a `ReasonPhrase` with no validation.
    pub(crate) unsafe fn from_str_unchecked<S>(value: S) -> Self
    where
        S: Into<String>,
    {
        ReasonPhrase(value.into())
    }
}

impl AsRef<str> for ReasonPhrase {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for ReasonPhrase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for ReasonPhrase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Performs equality checking of a `ReasonPhrase` with a `str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::ReasonPhrase;
///
/// assert_eq!(ReasonPhrase::try_from("OK").unwrap(), "OK");
/// ```
impl PartialEq<str> for ReasonPhrase {
    fn eq(&self, other: &str) -> bool {
        self.0.to_lowercase() == other.to_lowercase()
    }
}

/// Performs equality checking of a `ReasonPhrase` with a `&str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::ReasonPhrase;
///
/// assert_eq!(ReasonPhrase::try_from("OK").unwrap(), "OK");
/// ```
impl<'a> PartialEq<&'a str> for ReasonPhrase {
    fn eq(&self, other: &&'a str) -> bool {
        self.0.to_lowercase() == (*other).to_lowercase()
    }
}

/// Provides a fallible conversion from a byte slice to a `ReasonPhrase`. Note that you cannot do
/// the following:
///
/// ```compile_fail
/// let ok = ReasonPhrase::try_from(b"OK").unwrap();
/// ```
///
/// This is because `b"OK"` is of type `&[u8; 2]` and so it must be converted to `&[u8]` in order
/// to perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be
/// provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for ReasonPhrase {
    type Error = InvalidReasonPhrase;

    /// Converts a `&[u8]` to an RTSP reason phrase. The reason phrase must be non-empty and be
    /// valid UTF-8 with only printable characters allowed from ASCII-US. Case sensitivity is
    /// preserved.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::ReasonPhrase;
    ///
    /// let ok = ReasonPhrase::try_from(&b"OK"[..]).unwrap();
    /// assert_eq!(ok.as_str(), "OK");
    ///
    /// let error = ReasonPhrase::try_from(&b""[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        ReasonPhrase::try_from(str::from_utf8(value).map_err(|_| InvalidReasonPhrase)?)
    }
}

impl<'a> TryFrom<&'a str> for ReasonPhrase {
    type Error = InvalidReasonPhrase;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(InvalidReasonPhrase);
        }

        for c in value.chars() {
            if c.is_ascii() && (c < ' ' || c > '~') && c != '\t' {
                return Err(InvalidReasonPhrase);
            }
        }

        Ok(ReasonPhrase(value.to_string()))
    }
}

/// A possible error value when converting to a `ReasonPhrase` from a `&[u8]` or `&str`.
///
/// This error indicates that the reason phrase was empty, contained non-printable ASCII-US
/// characters, or was invalid UTF-8.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidReasonPhrase;

impl fmt::Display for InvalidReasonPhrase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for InvalidReasonPhrase {
    fn description(&self) -> &str {
        "invalid RTSP reason phrase"
    }
}
