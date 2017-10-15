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

use std::fmt;

/// Represents a version of the RTSP spec.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

impl Default for Version {
    fn default() -> Version {
        Version::RTSP20
    }
}

impl fmt::Debug for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Version::RTSP10 => "RTSP/1.0",
                Version::RTSP20 => "RTSP/2.0",
            }
        )
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
