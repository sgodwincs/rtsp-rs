use std::convert::TryFrom;
use std::iter::once;
use std::ops::{Add, Deref, Sub};

use crate::header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use crate::syntax::trim_whitespace_left;

pub const MAX_CSEQ: u32 = 999_999_999;

/// The `CSeq` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.20).
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CSeq(u32);

impl CSeq {
    pub fn increment(self) -> Self {
        CSeq((self.0 + 1) % (MAX_CSEQ + 1))
    }
}

impl Add for CSeq {
    type Output = CSeq;

    fn add(self, other: CSeq) -> Self::Output {
        (&self).add(&other)
    }
}

impl<'a> Add<&'a CSeq> for CSeq {
    type Output = CSeq;

    fn add(self, other: &'a CSeq) -> Self::Output {
        (&self).add(other)
    }
}

impl<'a> Add<CSeq> for &'a CSeq {
    type Output = CSeq;

    fn add(self, other: CSeq) -> Self::Output {
        self.add(&other)
    }
}

impl<'a, 'b> Add<&'a CSeq> for &'b CSeq {
    type Output = CSeq;

    fn add(self, other: &'a CSeq) -> Self::Output {
        CSeq((self.0 + other.0) % (MAX_CSEQ + 1))
    }
}

impl Deref for CSeq {
    type Target = u32;

    fn deref(&self) -> &u32 {
        &self.0
    }
}

impl Sub for CSeq {
    type Output = CSeq;

    fn sub(self, other: CSeq) -> Self::Output {
        (&self).sub(&other)
    }
}

impl<'a> Sub<&'a CSeq> for CSeq {
    type Output = CSeq;

    fn sub(self, other: &'a CSeq) -> Self::Output {
        (&self).sub(other)
    }
}

impl<'a> Sub<CSeq> for &'a CSeq {
    type Output = CSeq;

    fn sub(self, other: CSeq) -> Self::Output {
        self.sub(&other)
    }
}

impl<'a, 'b> Sub<&'a CSeq> for &'b CSeq {
    type Output = CSeq;

    fn sub(self, other: &'a CSeq) -> Self::Output {
        CSeq(if self >= other {
            self.0 - other.0
        } else {
            MAX_CSEQ - (other.0 - self.0)
        })
    }
}

impl TryFrom<u32> for CSeq {
    type Error = InvalidTypedHeader;

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if value > MAX_CSEQ {
            Err(InvalidTypedHeader)
        } else {
            Ok(CSeq(value))
        }
    }
}

impl TypedHeader for CSeq {
    type DecodeError = InvalidTypedHeader;

    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::CSeq
    }

    /// Converts the raw header values to the `CSeq` header type. Based on the syntax provided by
    /// [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the
    /// following syntax:
    ///
    /// ```text
    /// DIGIT = %x30-39 ; any US-ASCII digit "0".."9"
    /// CSeq = "CSeq" HCOLON cseq-nr
    /// cseq-nr = 1*9DIGIT
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::CSeq;
    /// use rtsp::header::TypedHeader;
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
            return Err(InvalidTypedHeader);
        }

        trim_whitespace_left(value.as_str())
            .parse::<u32>()
            .map_err(|_| InvalidTypedHeader)
            .and_then(|cseq| CSeq::try_from(cseq).map(Some))
    }

    /// Converts the `CSeq` type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::CSeq;
    /// use rtsp::header::TypedHeader;
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
        // Unsafe Justification
        //
        // In order for this to be safe, we must ensure that `value` contains no unprintable
        // ASCII-US characters and that all linebreaks of the form `"\r\n"` are followed by a space
        // or tab. Since [`CSeq`] serializes into a number, it satisfies the constraints.

        values.extend(once(unsafe {
            HeaderValue::from_str_unchecked(self.0.to_string().as_str())
        }))
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
