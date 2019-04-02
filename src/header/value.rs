//! Header Value

use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::{cmp, str};

/// An RTSP header value that is UTF-8 encoded.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct HeaderValue(String);

impl HeaderValue {
    /// Returns a `&str` representation of the header value.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.as_str(), "value")
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Converts a string representation into a [`HeaderValue`] with no validation.
    ///
    /// # Unsafe
    ///
    /// This function is to only be used when the input is UTF-8 encoded (which is already
    /// guaranteed by the signature) but only contains the subset of ASCII-US characters which are
    /// printable. Also, all linebreaks of the form `"\r\n"` must be followed by a space or tab.
    pub(crate) unsafe fn from_string_unchecked(value: String) -> Self {
        debug_assert!(HeaderValue::try_from(value.as_str()).is_ok());
        HeaderValue(value)
    }

    /// Returns whether the length of the header value is zero.
    ///
    /// The length is in bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.is_empty(), false);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the length of the header value.
    ///
    /// The length is in bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.len(), 5);
    /// ```
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl AsRef<[u8]> for HeaderValue {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for HeaderValue {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Display for HeaderValue {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<HeaderValue> for String {
    fn from(value: HeaderValue) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for HeaderValue {
    fn eq(&self, other: &[u8]) -> bool {
        self.0.as_bytes() == other
    }
}

impl PartialEq<HeaderValue> for [u8] {
    fn eq(&self, other: &HeaderValue) -> bool {
        self == other.0.as_bytes()
    }
}

impl<'header> PartialEq<&'header [u8]> for HeaderValue {
    fn eq(&self, other: &&'header [u8]) -> bool {
        self.0.as_bytes() == *other
    }
}

impl<'header> PartialEq<HeaderValue> for &'header [u8] {
    fn eq(&self, other: &HeaderValue) -> bool {
        *self == other.0.as_bytes()
    }
}

impl PartialEq<str> for HeaderValue {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<HeaderValue> for str {
    fn eq(&self, other: &HeaderValue) -> bool {
        self == other.0
    }
}

impl<'header> PartialEq<&'header str> for HeaderValue {
    fn eq(&self, other: &&'header str) -> bool {
        self.0 == *other
    }
}

impl<'header> PartialEq<HeaderValue> for &'header str {
    fn eq(&self, other: &HeaderValue) -> bool {
        *self == other.0
    }
}

impl PartialOrd<str> for HeaderValue {
    fn partial_cmp(&self, other: &str) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(other)
    }
}

impl PartialOrd<HeaderValue> for str {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        self.partial_cmp(&*other.0)
    }
}

impl<'header> PartialOrd<&'header str> for HeaderValue {
    fn partial_cmp(&self, other: &&'header str) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(*other)
    }
}

impl<'header> PartialOrd<HeaderValue> for &'header str {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        self.partial_cmp(&&*other.0)
    }
}

impl<'a> TryFrom<&'a str> for HeaderValue {
    type Error = HeaderValueError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        enum ExpectState {
            Any,
            LF,
            SpaceOrTab,
        }

        let mut expect_state = ExpectState::Any;

        for c in value.chars() {
            match expect_state {
                ExpectState::Any => {
                    if c.is_ascii() {
                        if c == '\r' {
                            expect_state = ExpectState::LF;
                        } else if (c < ' ' || c > '~') && c != '\t' {
                            return Err(HeaderValueError::InvalidASCII);
                        }
                    }
                }
                ExpectState::LF => match c {
                    '\n' => expect_state = ExpectState::SpaceOrTab,
                    _ => return Err(HeaderValueError::InvalidLineBreak),
                },
                ExpectState::SpaceOrTab => match c {
                    ' ' | '\t' => expect_state = ExpectState::Any,
                    _ => return Err(HeaderValueError::InvalidLineBreak),
                },
            }
        }

        if expect_state != ExpectState::Any {
            Err(HeaderValueError::InvalidLineBreak)
        } else {
            Ok(HeaderValue(value.to_string()))
        }
    }
}

impl<'a> TryFrom<&'a [u8]> for HeaderValue {
    type Error = HeaderValueError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        str::from_utf8(value)
            .map_err(|_| HeaderValueError::InvalidUTF8)
            .and_then(HeaderValue::try_from)
    }
}

/// A possible error value when converting to a [`HeaderValue`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum HeaderValueError {
    /// The header value contained unallowed ASCII-US characters.
    InvalidASCII,

    /// The header value contained an invalid line break.
    InvalidLineBreak,

    /// The header value was invalid UTF-8.
    InvalidUTF8,
}

impl Display for HeaderValueError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::HeaderValueError::*;

        match self {
            InvalidASCII => write!(formatter, "invalid header value ASCII character"),
            InvalidLineBreak => write!(formatter, "invalid header value line break"),
            InvalidUTF8 => write!(formatter, "invalid header value UTF-8"),
        }
    }
}

impl Error for HeaderValueError {}

impl From<Infallible> for HeaderValueError {
    fn from(_: Infallible) -> Self {
        HeaderValueError::InvalidASCII
    }
}

#[cfg(test)]
mod test {
    use std::convert::TryFrom;

    use crate::header::value::HeaderValue;

    #[test]
    fn test_header_value_try_from_str() {
        assert_eq!(
            HeaderValue::try_from("test"),
            Ok(HeaderValue("test".to_string()))
        );
        assert_eq!(
            HeaderValue::try_from("test 1 2\t\t3"),
            Ok(HeaderValue("test 1 2\t\t3".to_string()))
        );
        assert_eq!(
            HeaderValue::try_from("test\r\n 1 2\t\t3"),
            Ok(HeaderValue("test\r\n 1 2\t\t3".to_string()))
        );
        assert!(HeaderValue::try_from("test\n1 2\t\t3").is_err());
        assert!(HeaderValue::try_from("test\r\n1 2\t\t3").is_err());
        assert!(HeaderValue::try_from("test\n\r 1 2\t\t3").is_err());
    }
}
