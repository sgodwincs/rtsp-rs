//! RTSP Header Value

use std::convert::TryFrom;
use std::error::Error;
use std::{cmp, fmt, str};

/// An RTSP header value that is UTF-8 encoded.
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct HeaderValue(String);

impl HeaderValue {
    /// Converts a `HeaderValue` to a byte slice.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.as_bytes(), b"value")
    /// ```
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    /// Returns a `&str` representation of the header value.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.as_str(), "value")
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Converts a string representation into a `HeaderValue` with no validation.
    ///
    /// # Unsafe Use
    ///
    /// This function is to only be used when the input is UTF-8 encoded (which is already
    /// guaranteed by the signature) but only contains the subset of ASCII-US characters which are
    /// printable. Also, all linebreaks of the form `"\r\n"` must be followed by a space or tab.
    pub(crate) unsafe fn from_str_unchecked<S>(value: S) -> Self
    where
        S: Into<String>,
    {
        debug_assert!(HeaderValue::try_from(value).is_ok());
        HeaderValue(value.into())
    }

    /// Returns whether or not the length of the header value is 0.
    ///
    /// This length is in bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
    ///
    /// let header_value = HeaderValue::try_from("value").unwrap();
    /// assert_eq!(header_value.is_empty(), false);
    /// ```
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the length of `self`.
    ///
    /// This length is in bytes.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
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
        self.as_bytes()
    }
}

impl AsRef<str> for HeaderValue {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for HeaderValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for HeaderValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<[u8]> for HeaderValue {
    fn eq(&self, other: &[u8]) -> bool {
        self.0.as_bytes() == other
    }
}

impl PartialEq<HeaderValue> for [u8] {
    fn eq(&self, other: &HeaderValue) -> bool {
        *other == *self
    }
}

impl PartialEq<str> for HeaderValue {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

impl PartialEq<HeaderValue> for str {
    fn eq(&self, other: &HeaderValue) -> bool {
        *other == *self
    }
}

impl<'a> PartialEq<HeaderValue> for &'a str {
    fn eq(&self, other: &HeaderValue) -> bool {
        *other == *self
    }
}

impl PartialEq<String> for HeaderValue {
    fn eq(&self, other: &String) -> bool {
        self.0 == *other
    }
}

impl PartialEq<HeaderValue> for String {
    fn eq(&self, other: &HeaderValue) -> bool {
        *other == *self
    }
}

impl<'a> PartialEq<HeaderValue> for &'a HeaderValue {
    fn eq(&self, other: &HeaderValue) -> bool {
        **self == *other
    }
}

impl<'a, T: ?Sized> PartialEq<&'a T> for HeaderValue
where
    HeaderValue: PartialEq<T>,
{
    fn eq(&self, other: &&'a T) -> bool {
        *self == **other
    }
}

impl PartialOrd<[u8]> for HeaderValue {
    fn partial_cmp(&self, other: &[u8]) -> Option<cmp::Ordering> {
        self.as_bytes().partial_cmp(other)
    }
}

impl PartialOrd<HeaderValue> for [u8] {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        self.partial_cmp(other.as_bytes())
    }
}

impl PartialOrd<str> for HeaderValue {
    fn partial_cmp(&self, other: &str) -> Option<cmp::Ordering> {
        self.as_str().partial_cmp(other)
    }
}

impl PartialOrd<HeaderValue> for str {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        self.partial_cmp(other.as_str())
    }
}

impl<'a> PartialOrd<HeaderValue> for &'a str {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        (*self).partial_cmp(other.as_str())
    }
}

impl PartialOrd<String> for HeaderValue {
    fn partial_cmp(&self, other: &String) -> Option<cmp::Ordering> {
        self.partial_cmp(other.as_bytes())
    }
}

impl PartialOrd<HeaderValue> for String {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        self.as_bytes().partial_cmp(other.as_bytes())
    }
}

impl<'a> PartialOrd<HeaderValue> for &'a HeaderValue {
    fn partial_cmp(&self, other: &HeaderValue) -> Option<cmp::Ordering> {
        (**self).partial_cmp(other)
    }
}

impl<'a, T: ?Sized> PartialOrd<&'a T> for HeaderValue
where
    HeaderValue: PartialOrd<T>,
{
    fn partial_cmp(&self, other: &&'a T) -> Option<cmp::Ordering> {
        self.partial_cmp(*other)
    }
}

impl<'a> TryFrom<&'a str> for HeaderValue {
    type Error = InvalidHeaderValue;

    /// Converts a `&str` to an RTSP header value. The header value is encoded using UTF-8 with some
    /// slight restrictions. Only visible characters from the ASCII character set are allowed and
    /// line breaks `"\r\n"` must be followed by either a space or tab.
    ///
    /// All case sensitivity is perserved.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
    ///
    /// assert_eq!(HeaderValue::try_from("test").unwrap().as_str(), "test");
    /// assert_eq!(HeaderValue::try_from("test\r\n ").unwrap().as_str(), "test\r\n ");
    ///
    /// assert!(HeaderValue::try_from("test \u{000}").is_err());
    /// assert!(HeaderValue::try_from("test\r\n").is_err());
    /// ```
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
                ExpectState::Any => if c.is_ascii() {
                    if c == '\r' {
                        expect_state = ExpectState::LF;
                    } else if (c < ' ' || c > '~') && c != '\t' {
                        return Err(InvalidHeaderValue);
                    }
                },
                ExpectState::LF => match c {
                    '\n' => expect_state = ExpectState::SpaceOrTab,
                    _ => return Err(InvalidHeaderValue),
                },
                ExpectState::SpaceOrTab => match c {
                    ' ' | '\t' => expect_state = ExpectState::Any,
                    _ => return Err(InvalidHeaderValue),
                },
            }
        }

        if expect_state != ExpectState::Any {
            Err(InvalidHeaderValue)
        } else {
            Ok(HeaderValue(value.to_string()))
        }
    }
}

/// Provides a fallible conversion from a byte slice to a `HeaderValue`. Note that you cannot do the
/// following:
///
/// ```compile_fail
/// let header_value = HeaderValue::try_from(b"value").unwrap();
/// ```
///
/// This is because `b"value"` is of type `&[u8; 5]` and so it must be converted to `&[u8]` in order
/// to perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be
/// provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for HeaderValue {
    type Error = InvalidHeaderValue;

    /// Converts a `&[u8]` to an RTSP header value. The header value is encoded using UTF-8 with
    /// some slight restrictions. Only visible characters from the ASCII character set are allowed
    /// and line breaks `"\r\n"` must be followed by either a space or tab.
    ///
    /// All case sensitivity is perserved.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::HeaderValue;
    ///
    /// assert_eq!(HeaderValue::try_from(&b"test"[..]).unwrap().as_str(), "test");
    /// assert_eq!(HeaderValue::try_from(&b"test\r\n "[..]).unwrap().as_str(), "test\r\n ");
    ///
    /// assert!(HeaderValue::try_from(&"test \u{000}"[..]).is_err());
    /// assert!(HeaderValue::try_from(&b"test\r\n"[..]).is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        str::from_utf8(value)
            .map_err(|_| InvalidHeaderValue)
            .and_then(HeaderValue::try_from)
    }
}

/// A possible error value when converting to a `HeaderValue` from a `&[u8]` or `&str`.
///
/// This error indicates the header value was not valid UTF-8, had invalid line breaks, or contained
/// unallowed ASCII characters (only characters from 0x21 to 0x7E including spaces and tabs).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidHeaderValue;

impl fmt::Display for InvalidHeaderValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for InvalidHeaderValue {
    fn description(&self) -> &str {
        "invalid RTSP header value"
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_try_from_str() {
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
