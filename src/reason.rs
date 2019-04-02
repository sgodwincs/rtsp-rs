//! Reason Phrase
//!
//! This module contains structs related to RTSP reason phrases, notably the [`ReasonPhrase`] type
//! itself.

use std::borrow::Cow;
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::str;

/// A wrapper type used to avoid users creating invalid reason phrases for the [`Response`] type.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ReasonPhrase(Cow<'static, str>);

impl ReasonPhrase {
    /// Returns a `&str` representation of the reason phrase.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::reason::ReasonPhrase;
    ///
    /// assert_eq!(ReasonPhrase::try_from("OK").unwrap().as_str(), "OK");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Converts a `&'static str` into a [`ReasonPhrase`] with no validation.
    ///
    /// This is primarily used to have zero cost conversions from status code canonical reason
    /// phrases.
    ///
    /// # Unsafe
    ///
    /// This function is to only be used when the input is UTF-8 encoded (which is already
    /// guaranteed by the signature) but only contains the subset of ASCII-US characters which are
    /// printable.
    pub(crate) unsafe fn from_static_str_unchecked(value: &'static str) -> Self {
        debug_assert!(ReasonPhrase::try_from(value).is_ok());
        ReasonPhrase(Cow::from(value))
    }
}

impl AsRef<[u8]> for ReasonPhrase {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for ReasonPhrase {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Display for ReasonPhrase {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<ReasonPhrase> for String {
    fn from(value: ReasonPhrase) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for ReasonPhrase {
    fn eq(&self, other: &[u8]) -> bool {
        self.0.as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ReasonPhrase> for [u8] {
    fn eq(&self, other: &ReasonPhrase) -> bool {
        self.eq_ignore_ascii_case(other.0.as_bytes())
    }
}

impl<'reason_phrase> PartialEq<&'reason_phrase [u8]> for ReasonPhrase {
    fn eq(&self, other: &&'reason_phrase [u8]) -> bool {
        self.0.as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'reason_phrase> PartialEq<ReasonPhrase> for &'reason_phrase [u8] {
    fn eq(&self, other: &ReasonPhrase) -> bool {
        self.eq_ignore_ascii_case(other.0.as_bytes())
    }
}

impl PartialEq<str> for ReasonPhrase {
    fn eq(&self, other: &str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ReasonPhrase> for str {
    fn eq(&self, other: &ReasonPhrase) -> bool {
        self.eq_ignore_ascii_case(&other.0)
    }
}

impl<'reason_phrase> PartialEq<&'reason_phrase str> for ReasonPhrase {
    fn eq(&self, other: &&'reason_phrase str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

impl<'reason_phrase> PartialEq<ReasonPhrase> for &'reason_phrase str {
    fn eq(&self, other: &ReasonPhrase) -> bool {
        self.eq_ignore_ascii_case(&other.0)
    }
}

impl<'a> TryFrom<&'a [u8]> for ReasonPhrase {
    type Error = ReasonPhraseError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        ReasonPhrase::try_from(str::from_utf8(value).map_err(|_| ReasonPhraseError::InvalidUTF8)?)
    }
}

impl<'a> TryFrom<&'a str> for ReasonPhrase {
    type Error = ReasonPhraseError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(ReasonPhraseError::Empty);
        }

        for c in value.chars() {
            if c.is_ascii() && (c < ' ' || c > '~') && c != '\t' {
                return Err(ReasonPhraseError::InvalidASCII);
            }
        }

        Ok(ReasonPhrase(Cow::from(value.to_string())))
    }
}

/// A possible error value when converting to a [`ReasonPhrase`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ReasonPhraseError {
    /// The reason phrase was empty.
    Empty,

    /// The reason phrase contained non-printable ASCII-US characters.
    InvalidASCII,

    /// The reason phrase was invalid UTF-8.
    InvalidUTF8,
}

impl Display for ReasonPhraseError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ReasonPhraseError::*;

        match self {
            Empty => write!(formatter, "empty reason phrase"),
            InvalidASCII => write!(formatter, "invalid reason phrase ASCII character"),
            InvalidUTF8 => write!(formatter, "invalid reason phrase UTF-8"),
        }
    }
}

impl Error for ReasonPhraseError {}

impl From<Infallible> for ReasonPhraseError {
    fn from(_: Infallible) -> Self {
        ReasonPhraseError::Empty
    }
}
