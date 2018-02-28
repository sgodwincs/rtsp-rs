use std::convert::TryFrom;
use std::ops::Deref;

use header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use syntax::trim_whitespace_left;

pub const MAX_CSEQ: u32 = 999_999_999;

/// The `CSeq` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.20).
#[derive(Clone, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct CSeq(u32);

impl TypedHeader for CSeq {
    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::CSeq
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
    ///
    /// let typed_header = CSeq::try_from(0).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("0").unwrap()];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    /// ```
    fn to_header_raw(&self) -> Vec<HeaderValue> {
        vec![
            unsafe { HeaderValue::from_str_unchecked(self.0.to_string().as_str()) },
        ]
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
    ///
    /// let typed_header = CSeq::try_from(10).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("10").unwrap()];
    ///
    /// assert_eq!(
    ///     CSeq::try_from_header_raw(&raw_header).unwrap(),
    ///     typed_header
    /// );
    ///
    /// let raw_header = vec![HeaderValue::try_from("invalid cseq").unwrap()];
    ///
    /// assert!(CSeq::try_from_header_raw(&raw_header).is_err());
    /// ```
    fn try_from_header_raw(header: &[HeaderValue]) -> Result<Self, InvalidTypedHeader> {
        if header.len() == 0 || header.len() > 1 {
            Err(InvalidTypedHeader)
        } else {
            trim_whitespace_left(header[0].as_str())
                .parse::<u32>()
                .map_err(|_| InvalidTypedHeader)
                .and_then(|cseq| CSeq::try_from(cseq))
        }
    }
}

impl Deref for CSeq {
    type Target = u32;

    fn deref(&self) -> &u32 {
        &self.0
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
