use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Write};
use std::ops::{Deref, RangeInclusive};

use crate::syntax;

/// The interleaved parameter which implies mixing the media stream with the control stream in
/// whatever protocol is being used by the control stream, using the mechanism defined in
/// [[RFC7826, Section 14]](https://tools.ietf.org/html/rfc7826#section-14).
///
/// The argument provides the channel number to be used in the `$` block and musat be present. This
/// parameter may be specified as an interval, e.g., `"interleaved=4-5"` in cases where the
/// transport choice for the media stream requires it, e.g., for RTP with RTCP. The channel number
/// given in the request is only a guidance from the client to the server on what channel number(s)
/// to use. The server may set any valid channel number in the response. The declared channels are
/// bidirectional, so both end parties may send data on the given channel. One example of such usage
/// is the second channel used for RTCP, where both server and client send RTCP packets on the same
/// channel.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Interleaved(RangeInclusive<u8>);

impl Deref for Interleaved {
    type Target = RangeInclusive<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Interleaved {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        self.0.start().fmt(formatter)?;

        if self.0.start() != self.0.end() {
            formatter.write_char('-')?;
            self.0.end().fmt(formatter)?;
        }

        Ok(())
    }
}

impl From<Interleaved> for String {
    fn from(value: Interleaved) -> Self {
        value.to_string()
    }
}

impl<'interleaved> TryFrom<&'interleaved [u8]> for Interleaved {
    type Error = InterleavedError;

    fn try_from(value: &'interleaved [u8]) -> Result<Self, Self::Error> {
        let (channel_start, value) = parse_channel(value)?;

        if value.is_empty() {
            return Ok(Interleaved(RangeInclusive::new(
                channel_start,
                channel_start,
            )));
        }

        let value = syntax::trim_bytes_whitespace_left(value);

        if !value.starts_with(b"-") {
            return Err(InterleavedError::InvalidCharacter);
        }

        let value = syntax::trim_bytes_whitespace_left(&value[1..]);
        let (channel_end, value) = parse_channel(value)?;

        if !value.is_empty() {
            return Err(InterleavedError::InvalidCharacter);
        }

        Ok(Interleaved(RangeInclusive::new(channel_start, channel_end)))
    }
}

impl<'interleaved> TryFrom<&'interleaved str> for Interleaved {
    type Error = InterleavedError;

    fn try_from(value: &'interleaved str) -> Result<Self, Self::Error> {
        Interleaved::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to [`Interleaved`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InterleavedError {
    /// The parameter was empty.
    Empty,

    /// An invalid character was used in the parameter. Only decimal digits are allowed.
    InvalidCharacter,

    /// One of the channels was a valid number, but it was too large to fit in a `u8`.
    Overflow,
}

impl Display for InterleavedError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::InterleavedError::*;

        match self {
            Empty => write!(formatter, "empty interleaved parameter"),
            InvalidCharacter => write!(formatter, "invalid interleaved parameter character"),
            Overflow => write!(formatter, "interleaved channel overflow"),
        }
    }
}

impl Error for InterleavedError {}

impl From<Infallible> for InterleavedError {
    fn from(_: Infallible) -> Self {
        InterleavedError::Empty
    }
}

fn parse_channel(value: &[u8]) -> Result<(u8, &[u8]), InterleavedError> {
    let mut channel = 0u8;
    let mut digits_found = 0;

    for byte in value {
        if !byte.is_ascii_digit() {
            return if digits_found > 0 {
                Ok((channel, &value[digits_found..]))
            } else {
                Err(InterleavedError::Empty)
            };
        }

        channel = channel.checked_mul(10).ok_or(InterleavedError::Overflow)?;
        channel = channel
            .checked_add((byte - b'0').into())
            .ok_or(InterleavedError::Overflow)?;
        digits_found += 1;
    }

    if digits_found == 0 {
        Err(InterleavedError::Empty)
    } else {
        Ok((channel, &value[digits_found..]))
    }
}
