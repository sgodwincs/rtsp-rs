//! Version
//!
//! This module contains a definition of the [`Version`] type.
//!
//! The [`Version`] enum type contains variants that represent the various versions of the the RTSP
//! protocol. Note that although RTSP 1.0 and RTSP 2.0 are listed here, RTSP 2.0 obseletes RTSP 1.0
//! and thus RTSP 1.0 is not actually supported by this library.
//!
//! # Examples
//!
//! ```
//! use rtsp::version::Version;
//!
//! let rtsp10 = Version::RTSP10;
//! let rtsp20 = Version::RTSP20;
//!
//! assert!(rtsp10 != rtsp20);
//! assert_eq!(rtsp20.as_str(), "RTSP/2.0")
//! ```

use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};

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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::version::Version;
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

    /// Returns whether the version is [`Version::RTSP10`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::version::Version;
    ///
    /// assert!(Version::RTSP10.is_rtsp10());
    /// assert!(!Version::RTSP20.is_rtsp10());
    /// ```
    pub fn is_rtsp10(self) -> bool {
        use self::Version::*;

        match self {
            RTSP10 => true,
            _ => false,
        }
    }

    /// Returns whether the version is [`Version::RTSP20`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::version::Version;
    ///
    /// assert!(!Version::RTSP10.is_rtsp20());
    /// assert!(Version::RTSP20.is_rtsp20());
    /// ```
    pub fn is_rtsp20(self) -> bool {
        use self::Version::*;

        match self {
            RTSP20 => true,
            _ => false,
        }
    }
}

impl AsRef<[u8]> for Version {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for Version {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Debug for Version {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl Default for Version {
    fn default() -> Self {
        Version::RTSP20
    }
}

impl Display for Version {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<Version> for String {
    fn from(value: Version) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for Version {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Version> for [u8] {
    fn eq(&self, other: &Version) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'version> PartialEq<&'version [u8]> for Version {
    fn eq(&self, other: &&'version [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'version> PartialEq<Version> for &'version [u8] {
    fn eq(&self, other: &Version) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Version {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Version> for str {
    fn eq(&self, other: &Version) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'version> PartialEq<&'version str> for Version {
    fn eq(&self, other: &&'version str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'version> PartialEq<Version> for &'version str {
    fn eq(&self, other: &Version) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'version> TryFrom<&'version [u8]> for Version {
    type Error = VersionError;

    fn try_from(value: &'version [u8]) -> Result<Self, Self::Error> {
        use self::Version::*;

        if value.len() != 8
            || value
                .iter()
                .take(5)
                .map(u8::to_ascii_uppercase)
                .ne(b"RTSP/".iter().cloned())
            || value[6] != b'.'
        {
            return Err(VersionError::Invalid);
        }

        let major = value[5].checked_sub(b'0').ok_or(VersionError::Invalid)?;
        let minor = value[7].checked_sub(b'0').ok_or(VersionError::Invalid)?;

        if major == 1 && minor == 0 {
            Ok(RTSP10)
        } else if major == 2 && minor == 0 {
            Ok(RTSP20)
        } else if major > 9 || minor > 9 {
            Err(VersionError::Invalid)
        } else {
            Err(VersionError::Unknown)
        }
    }
}

impl<'version> TryFrom<&'version str> for Version {
    type Error = VersionError;

    fn try_from(value: &'version str) -> Result<Self, Self::Error> {
        Version::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a [`Version`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum VersionError {
    /// The version was not of the form `"RTSP/*.*"` where each `'*'` is a one digit number.
    Invalid,

    /// The version was of the correct form, but the version was not recognized.
    Unknown,
}

impl Display for VersionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::VersionError::*;

        match self {
            Invalid => write!(formatter, "invalid version"),
            Unknown => write!(formatter, "unknown version"),
        }
    }
}

impl Error for VersionError {}

impl From<Infallible> for VersionError {
    fn from(_: Infallible) -> Self {
        VersionError::Invalid
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryFrom;

    use crate::version::{Version, VersionError};

    #[test]
    fn test_version_try_from() {
        assert_eq!(Version::try_from("rtsp/1.0"), Ok(Version::RTSP10));
        assert_eq!(Version::try_from("RTSP/1.0"), Ok(Version::RTSP10));
        assert_eq!(Version::try_from("rtsp/2.0"), Ok(Version::RTSP20));
        assert_eq!(Version::try_from("RTSP/2.0"), Ok(Version::RTSP20));

        assert_eq!(Version::try_from(""), Err(VersionError::Invalid));
        assert_eq!(Version::try_from("rtsp/"), Err(VersionError::Invalid));
        assert_eq!(Version::try_from("rtsp/0.0"), Err(VersionError::Unknown));
        assert_eq!(Version::try_from("rtsp/9.9"), Err(VersionError::Unknown));
        assert_eq!(Version::try_from("rtsp/A.A"), Err(VersionError::Invalid));
    }
}
