//! RTSP Version
//!
//! This module contains a definition of the `Version` type. The `Version` type is intended to be
//! accessed through the root of the crate (`rtsp::Version`) rather than this module.
//!
//! The `Version` enum type contains variants that represent the various versions of the the RTSP
//! protocol. Note that although RTSP 1.0 and RTSP 2.0 are listed here, RTSP 2.0 obseletes RTSP 1.0
//! and thus RTSP 1.0 is not actually supported by this library.
//!
//! # Examples
//!
//! ```
//! use rtsp::Version;
//!
//! let rtsp10 = Version::RTSP10;
//! let rtsp20 = Version::RTSP20;
//!
//! assert!(rtsp10 != rtsp20);
//! assert_eq!(format!("{}", rtsp20), "RTSP/2.0")
//! ```
//!

use std::convert::TryFrom;
use std::error;
use std::fmt;

/// Represents a version of the RTSP spec.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Version {
    /// `RTSP/1.0`
    /// [[RFC2326](https://tools.ietf.org/html/rfc2326)]
    ///
    /// This is only listed here for completeness. RTSP 2.0 obseletes RTSP 1.0.
    RTSP10,

    /// `RTSP/2.0`
    /// [[RFC7826](https://tools.ietf.org/html/rfc782)]
    RTSP20,
}

impl Version {
    /// Returns a `&str` representation of the version.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::Version;
    ///
    /// assert_eq!(Version::RTSP20.as_str(), "RTSP/2.0");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Version::*;

        match *self {
            RTSP10 => "RTSP/1.0",
            RTSP20 => "RTSP/2.0",
        }
    }
}

impl Default for Version {
    fn default() -> Self {
        Version::RTSP20
    }
}

impl fmt::Debug for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.as_str())
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

/// Provides a fallible conversion from a byte slice to a `Version`. Note that you cannot do the
/// following:
///
/// ```compile_fail
/// let play = Version::try_from(b"RTSP/2.0").unwrap();
/// ```
///
/// This is because `b"RTSP/2.0"` is of type `&[u8; 8]` and so it must be converted to `&[u8]` in
/// order to perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be
/// provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for Version {
    type Error = Error;

    /// Converts a `&[u8]` to an RTSP version. The version must be of the form `RTSP/*.*` where the
    /// `*` are 1 digit numbers.
    ///
    /// The conversion is case insensitive.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::Version;
    ///
    /// let rtsp20 = Version::try_from(&b"RTSP/2.0"[..]).unwrap();
    /// assert_eq!(rtsp20, Version::RTSP20);
    ///
    /// let rtsp10 = Version::try_from(&b"RTSP/1.0"[..]).unwrap();
    /// assert_eq!(rtsp10, Version::RTSP10);
    ///
    /// let error = Version::try_from(&b"RTSP/3.0"[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        use self::Version::*;

        let value = value.to_ascii_uppercase();

        if value.len() != 8 || !value.starts_with(b"RTSP/") || value[6] != b'.' {
            return Err(Error::InvalidVersion);
        }

        let major = value[5].checked_sub(b'0').ok_or(Error::InvalidVersion)?;
        let minor = value[7].checked_sub(b'0').ok_or(Error::InvalidVersion)?;

        if major == 1 && minor == 0 {
            Ok(RTSP10)
        } else if major == 2 && minor == 0 {
            Ok(RTSP20)
        } else {
            Err(Error::UnknownVersion)
        }
    }
}

impl<'a> TryFrom<&'a str> for Version {
    type Error = Error;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Version::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a `Version` from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Error {
    /// This error indicates that the version was not of the form `RTSP/*.*` where * are 1 digit
    /// numbers.
    InvalidVersion,

    /// This error indicates that the version was of the correct form but the version is not
    /// recognized.
    UnknownVersion,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        use self::Error::*;

        match self {
            &InvalidVersion => "invalid RTSP version",
            &UnknownVersion => "unknown RTSP version",
        }
    }
}
