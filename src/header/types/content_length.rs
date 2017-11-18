
use std::convert::TryFrom;
use std::ops::Deref;

use header::{Header, HeaderName, HeaderValue, InvalidHeader};
use syntax::trim_whitespace_left;

/// The RFC states that the content length of a request/response can be up to 19 digits long which
/// actually would require use of u64, but since the length of the buffer during decoding is of
/// type `usize`, this cannot be guaranteed on all platforms.
#[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ContentLength(usize);

impl Header for ContentLength {
    fn header_name() -> HeaderName {
        HeaderName::ContentLength
    }

    fn from_header_raw(header: &[&HeaderValue]) -> Result<Self, InvalidHeader> {
        assert!(!header.is_empty());

        if header.len() > 1 {
            Err(InvalidHeader)
        } else {
            trim_whitespace_left(header[0].as_str())
                .parse::<usize>()
                .map(|x| ContentLength(x))
                .map_err(|_| InvalidHeader)
        }
    }

    fn into_header_raw(self) -> Vec<HeaderValue> {
        vec![HeaderValue::try_from(self.0.to_string().as_str()).unwrap()]
    }
}

impl Deref for ContentLength {
    type Target = usize;

    fn deref(&self) -> &usize {
        &self.0
    }
}

impl From<usize> for ContentLength {
    fn from(value: usize) -> ContentLength {
        ContentLength(value)
    }
}
