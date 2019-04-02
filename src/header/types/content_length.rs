use core::num::IntErrorKind;
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::iter::once;
use std::ops::Deref;

use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;

/// The maximum size the content length can be.
pub const MAX_CONTENT_LENGTH: u64 = 9_999_999_999_999_999_999;

/// The `"Content-Length"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.17).
///
/// The RFC states that the content length of a request/response can be up to 19 digits long which
/// actually would require use of u64, but since the length of the buffer during decoding is of
/// type [`usize`], this cannot be guaranteed on all platforms.
///
/// The default value for this header is zero.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ContentLength(usize);

impl Deref for ContentLength {
    type Target = usize;

    fn deref(&self) -> &usize {
        &self.0
    }
}

impl TryFrom<usize> for ContentLength {
    type Error = ContentLengthError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        if value as u64 > MAX_CONTENT_LENGTH {
            Err(ContentLengthError::ExceedsMaximumLength)
        } else {
            Ok(ContentLength(value))
        }
    }
}

impl TypedHeader for ContentLength {
    type DecodeError = ContentLengthError;

    /// Converts the raw header values to the [`ContentLength`] header type. Based on the syntax
    /// provided by [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the
    /// following syntax:
    ///
    /// ```text
    /// DIGIT = %x30-39 ; any US-ASCII digit "0".."9"
    /// CR = %x0D ; US-ASCII CR, carriage return (13)
    /// LF = %x0A  ; US-ASCII LF, linefeed (10)
    /// SP = %x20  ; US-ASCII SP, space (32)
    /// HT = %x09  ; US-ASCII HT, horizontal-tab (9)
    /// CRLF = CR LF
    /// LWS = [CRLF] 1*( SP / HT ) ; Line-breaking whitespace
    /// SWS = [LWS] ; Separating whitespace
    /// HCOLON = *( SP / HT ) ":" SWS
    /// Content-Length = "Content-Length" HCOLON 1*19DIGIT
    /// ```
    ///
    /// However, in order to allow 19 digits to be used, 64 bit integers need to be used
    /// (i.e. [`u64`]), but the length of the buffer during decoding is of type [`usize`], so the
    /// actual allowed length may be significantly smaller depending on the architecture.
    ///
    /// The absence of a header value defaults the content length to a value of zero.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(ContentLength::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = ContentLength::try_from(10).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("10").unwrap()];
    /// assert_eq!(ContentLength::decode(&mut raw_header.iter()).unwrap(), Some(typed_header));
    ///
    /// let raw_header = vec![HeaderValue::try_from("invalid content length").unwrap()];
    /// assert!(ContentLength::decode(&mut raw_header.iter()).is_err());
    /// ```
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let value = match values.next() {
            Some(value) => value,
            None => return Ok(None),
        };

        if values.next().is_some() {
            return Err(ContentLengthError::MoreThanOneHeader);
        }

        let content_length = value
            .as_str()
            .parse::<usize>()
            .map_err(|error| ContentLengthError::try_from(error.kind().clone()).unwrap())?;
        ContentLength::try_from(content_length).map(Some)
    }

    /// Converts the [`ContentLength`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let typed_header = ContentLength::try_from(10).unwrap();
    /// let expected_raw_header = vec![HeaderValue::try_from("10").unwrap()];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        // Unsafe; In order for this to be safe, we must ensure that `value` contains no unprintable
        // ASCII-US characters and that all linebreaks of the form `"\r\n"` are followed by a space
        // or tab. Since [`ContentLength`] serializes into a number, it satisfies the constraints.

        values.extend(once(unsafe {
            HeaderValue::from_string_unchecked(self.0.to_string())
        }))
    }

    /// Returns the statically assigned [`HeaderName`] for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::ContentLength
    }
}

/// A possible error value when converting to a [`ContentLength`] from [`HeaderName`]s.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ContentLengthError {
    /// The content length header was empty.
    Empty,

    /// The content length header was parsed, but the length exceeds the maximum length a content
    /// length can be.
    ExceedsMaximumLength,

    /// The content length header contained an invalid digit.
    InvalidDigit,

    /// There was more than one content length header.
    MoreThanOneHeader,

    /// The content length value could not be parsed as it overflowed.
    Overflow,
}

impl Display for ContentLengthError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ContentLengthError::*;

        match self {
            Empty => write!(formatter, "empty content length"),
            ExceedsMaximumLength => write!(formatter, "content length exceeds maximum length"),
            InvalidDigit => write!(formatter, "invalid content length digit"),
            MoreThanOneHeader => write!(formatter, "more than one content length header"),
            Overflow => write!(formatter, "content length overflow"),
        }
    }
}

impl Error for ContentLengthError {}

impl From<Infallible> for ContentLengthError {
    fn from(_: Infallible) -> Self {
        ContentLengthError::Empty
    }
}

impl TryFrom<IntErrorKind> for ContentLengthError {
    type Error = ();

    fn try_from(value: IntErrorKind) -> Result<Self, Self::Error> {
        use self::ContentLengthError::*;

        match value {
            IntErrorKind::Empty => Ok(Empty),
            IntErrorKind::InvalidDigit => Ok(InvalidDigit),
            IntErrorKind::Overflow => Ok(Overflow),
            _ => Err(()),
        }
    }
}
