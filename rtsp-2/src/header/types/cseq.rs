use core::num::IntErrorKind;
use rand::{self, Rng};
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::iter::once;
use std::ops::{Add, Deref, Sub};

use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;

/// The maximum size the CSeq can be.
pub const MAX_CSEQ: u32 = 999_999_999;

/// The `"CSeq"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.20).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CSeq(u32);

impl CSeq {
    pub fn random() -> Self {
        let cseq = rand::thread_rng().gen_range(0, MAX_CSEQ + 1);
        CSeq(cseq)
    }

    /// Increments the [`CSeq`] value, wrapping it back to zero in case of overflow.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::CSeq;
    ///
    /// let cseq = CSeq::try_from(18).unwrap();
    /// assert_eq!(cseq.wrapping_increment(), CSeq::try_from(19).unwrap());
    ///
    /// let cseq = CSeq::try_from(999_999_999).unwrap();
    /// assert_eq!(cseq.wrapping_increment(), CSeq::default());
    /// ```
    pub fn wrapping_increment(self) -> Self {
        CSeq((self.0 + 1) % (MAX_CSEQ + 1))
    }
}

impl Add for CSeq {
    type Output = CSeq;

    fn add(self, other: CSeq) -> Self::Output {
        CSeq((self.0 + other.0) % (MAX_CSEQ + 1))
    }
}

impl Deref for CSeq {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Sub for CSeq {
    type Output = CSeq;

    fn sub(self, other: CSeq) -> Self::Output {
        CSeq(if self >= other {
            self.0 - other.0
        } else {
            MAX_CSEQ - (other.0 - self.0)
        })
    }
}

impl TryFrom<u32> for CSeq {
    type Error = CSeqError;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value > MAX_CSEQ {
            Err(CSeqError::ExceedsMaximumLength)
        } else {
            Ok(CSeq(value))
        }
    }
}

impl TypedHeader for CSeq {
    type DecodeError = CSeqError;

    /// Converts the raw header values to the [`CSeq`] header type. Based on the syntax provided by
    /// [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the
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
    /// CSeq = "CSeq" HCOLON cseq-nr
    /// cseq-nr = 1*9DIGIT
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::CSeq;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(CSeq::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = CSeq::try_from(10).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("10").unwrap()];
    /// assert_eq!(CSeq::decode(&mut raw_header.iter()).unwrap(), Some(typed_header));
    ///
    /// let raw_header = vec![HeaderValue::try_from("invalid cseq").unwrap()];
    /// assert!(CSeq::decode(&mut raw_header.iter()).is_err());
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
            return Err(CSeqError::MoreThanOneHeader);
        }

        let cseq = value
            .as_str()
            .parse::<u32>()
            .map_err(|error| CSeqError::try_from(error.kind().clone()).unwrap())?;
        CSeq::try_from(cseq).map(Some)
    }

    /// Converts the [`CSeq`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::CSeq;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let typed_header = CSeq::try_from(0).unwrap();
    /// let expected_raw_header = vec![HeaderValue::try_from("0").unwrap()];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        // Unsafe: In order for this to be safe, we must ensure that `value` contains no unprintable
        // ASCII-US characters and that all linebreaks of the form `"\r\n"` are followed by a space
        // or tab. Since [`CSeq`] serializes into a number, it satisfies the constraints.

        values.extend(once(unsafe {
            HeaderValue::from_string_unchecked(self.0.to_string())
        }))
    }

    /// Returns the statically assigned [`HeaderName`] for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::CSeq
    }
}

/// A possible error value when converting to a [`CSeq`] from [`HeaderName`]s.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum CSeqError {
    /// The `"CSeq"` header was empty.
    Empty,

    /// The `"CSeq"` header was parsed, but the length exceeds the maximum length a CSeq can be.
    ExceedsMaximumLength,

    /// The `"CSeq"` header contained an invalid digit.
    InvalidDigit,

    /// There was more than one `"CSeq"` header.
    MoreThanOneHeader,

    /// The `"CSeq"` value could not be parsed as it overflowed.
    Overflow,
}

impl Display for CSeqError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::CSeqError::*;

        match self {
            Empty => write!(formatter, "empty content length"),
            ExceedsMaximumLength => write!(formatter, "content length exceeds maximum length"),
            InvalidDigit => write!(formatter, "invalid content length digit"),
            MoreThanOneHeader => write!(formatter, "more than one content length header"),
            Overflow => write!(formatter, "content length overflow"),
        }
    }
}

impl Error for CSeqError {}

impl From<Infallible> for CSeqError {
    fn from(_: Infallible) -> Self {
        CSeqError::Empty
    }
}

impl TryFrom<IntErrorKind> for CSeqError {
    type Error = ();

    fn try_from(value: IntErrorKind) -> Result<Self, Self::Error> {
        use self::CSeqError::*;

        match value {
            IntErrorKind::Empty => Ok(Empty),
            IntErrorKind::InvalidDigit => Ok(InvalidDigit),
            IntErrorKind::PosOverflow => Ok(Overflow),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_sub() {
        let cseq_1 = CSeq::try_from(50).unwrap();
        let cseq_2 = CSeq::try_from(100).unwrap();

        assert_eq!(*(cseq_1 - cseq_1), 0);
        assert_eq!(*(cseq_2 - cseq_1), 50);
        assert_eq!(*(cseq_1 - cseq_2), MAX_CSEQ - 50);
    }
}
