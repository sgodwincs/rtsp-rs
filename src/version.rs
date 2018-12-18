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
//! use rtsp::Version;
//!
//! let rtsp10 = Version::RTSP10;
//! let rtsp20 = Version::RTSP20;
//!
//! assert!(rtsp10 != rtsp20);
//! assert_eq!(rtsp20.as_str(), "RTSP/2.0")
//! ```

use std::convert::TryFrom;
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
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
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

    /// Returns whether or not the version is [`Version::RTSP10`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::Version;
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

    /// Returns whether or not the version is [`Version::RTSP20`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::Version;
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
        formatter.write_str(self.as_str())
    }
}

impl Default for Version {
    fn default() -> Self {
        Version::RTSP20
    }
}

impl Display for Version {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl<'query> From<Version> for String {
    fn from(value: Version) -> Self {
        value.to_string()
    }
}

impl<'version> TryFrom<&'version [u8]> for Version {
    type Error = InvalidVersion;

    fn try_from(value: &'version [u8]) -> Result<Self, Self::Error> {
        use self::Version::*;

        if value.len() != 8
            || value
                .iter()
                .take(5)
                .map(|byte| byte.to_ascii_uppercase())
                .ne(b"RTSP/".iter().cloned())
            || value[6] != b'.'
        {
            return Err(InvalidVersion::Invalid);
        }

        let major = value[5].checked_sub(b'0').ok_or(InvalidVersion::Invalid)?;
        let minor = value[7].checked_sub(b'0').ok_or(InvalidVersion::Invalid)?;

        if major == 1 && minor == 0 {
            Ok(RTSP10)
        } else if major == 2 && minor == 0 {
            Ok(RTSP20)
        } else if major > 9 || minor > 9 {
            Err(InvalidVersion::Invalid)
        } else {
            Err(InvalidVersion::Unknown)
        }
    }
}

impl<'version> TryFrom<&'version str> for Version {
    type Error = InvalidVersion;

    fn try_from(value: &'version str) -> Result<Self, Self::Error> {
        Version::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a [`Version`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidVersion {
    /// This error indicates that the version was not of the form `"RTSP/*.*"` where `'*'` are 1
    /// digit numbers.
    Invalid,

    /// This error indicates that the version was of the correct form, but the version is not
    /// recognized.
    Unknown,
}

impl Display for InvalidVersion {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidVersion {
    fn description(&self) -> &str {
        use self::InvalidVersion::*;

        match *self {
            Invalid => "invalid RTSP version",
            Unknown => "unknown RTSP version",
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(Version::try_from("rtsp/1.0"), Ok(Version::RTSP10));
        assert_eq!(Version::try_from("RTSP/1.0"), Ok(Version::RTSP10));
        assert_eq!(Version::try_from("rtsp/2.0"), Ok(Version::RTSP20));
        assert_eq!(Version::try_from("RTSP/2.0"), Ok(Version::RTSP20));

        assert_eq!(Version::try_from(""), Err(InvalidVersion::Invalid));
        assert_eq!(Version::try_from("rtsp/"), Err(InvalidVersion::Invalid));
        assert_eq!(Version::try_from("rtsp/0.0"), Err(InvalidVersion::Unknown));
        assert_eq!(Version::try_from("rtsp/9.9"), Err(InvalidVersion::Unknown));
        assert_eq!(Version::try_from("rtsp/A.A"), Err(InvalidVersion::Invalid));
    }
}
