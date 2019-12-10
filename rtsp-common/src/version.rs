use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{Display, Formatter, Result as FormatterResult};

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[non_exhaustive]
pub enum Version {
    Rtsp1_0,
    Rtsp2_0,
}

impl Version {
    pub fn as_encoded(&self) -> &'static [u8] {
        self.as_str().as_bytes()
    }

    pub fn as_str(&self) -> &'static str {
        use self::Version::*;

        match self {
            Rtsp1_0 => "RTSP/1.0",
            Rtsp2_0 => "RTSP/2.0",
        }
    }

    pub fn try_decode(value: &[u8]) -> Result<Self, DecodeError> {
        use self::Version::*;

        if value.len() != 8
            || value
                .iter()
                .take(5)
                .map(u8::to_ascii_uppercase)
                .ne(b"RTSP/".iter().cloned())
            || value[6] != b'.'
        {
            return Err(DecodeError::Invalid);
        }

        let major = value[5].checked_sub(b'0').ok_or(DecodeError::Invalid)?;
        let minor = value[7].checked_sub(b'0').ok_or(DecodeError::Invalid)?;

        if major == 1 && minor == 0 {
            Ok(Rtsp1_0)
        } else if major == 2 && minor == 0 {
            Ok(Rtsp2_0)
        } else if major > 9 || minor > 9 {
            Err(DecodeError::Invalid)
        } else {
            Err(DecodeError::Unknown(major, minor))
        }
    }
}

impl Default for Version {
    fn default() -> Self {
        Version::Rtsp2_0
    }
}

impl From<Version> for &'static [u8] {
    fn from(value: Version) -> Self {
        value.as_encoded()
    }
}

impl From<Version> for &'static str {
    fn from(value: Version) -> Self {
        value.as_str()
    }
}

impl<'version> TryFrom<&'version [u8]> for Version {
    type Error = DecodeError;

    fn try_from(value: &'version [u8]) -> Result<Self, Self::Error> {
        Self::try_decode(value)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum DecodeError {
    Invalid,
    Unknown(u8, u8),
}

impl Display for DecodeError {
    fn fmt(&self, formatter: &mut Formatter) -> FormatterResult {
        use self::DecodeError::*;

        match self {
            Invalid => formatter.write_str("invalid version"),
            Unknown(major, minor) => write!(formatter, "unknown version '{}.{}'", major, minor),
        }
    }
}

impl Error for DecodeError {}

impl From<Infallible> for DecodeError {
    fn from(_: Infallible) -> Self {
        unreachable!()
    }
}

#[cfg(test)]
pub mod tests {
    use super::{DecodeError, Version};

    #[test]
    fn test_as_encoded() {
        assert_eq!(Version::Rtsp1_0.as_encoded(), b"RTSP/1.0");
        assert_eq!(Version::Rtsp2_0.as_encoded(), b"RTSP/2.0");

        assert_eq!(
            Version::try_decode(b"rtsp/1.0").unwrap().as_encoded(),
            b"RTSP/1.0"
        );
        assert_eq!(
            Version::try_decode(b"rtsp/2.0").unwrap().as_encoded(),
            b"RTSP/2.0"
        );
    }

    #[test]
    fn test_try_decode() {
        assert_eq!(Version::try_decode(b"RTSP/1.0"), Ok(Version::Rtsp1_0));
        assert_eq!(Version::try_decode(b"rtsp/1.0"), Ok(Version::Rtsp1_0));
        assert_eq!(Version::try_decode(b"RtSp/1.0"), Ok(Version::Rtsp1_0));
        assert_eq!(Version::try_decode(b"RTSP/2.0"), Ok(Version::Rtsp2_0));
        assert_eq!(Version::try_decode(b"RtSp/2.0"), Ok(Version::Rtsp2_0));
        assert_eq!(Version::try_decode(b"rtsp/2.0"), Ok(Version::Rtsp2_0));

        assert_eq!(Version::try_decode(b""), Err(DecodeError::Invalid));
        assert_eq!(Version::try_decode(b"RTSP/a.b"), Err(DecodeError::Invalid));
        assert_eq!(Version::try_decode(b"RTSP/2"), Err(DecodeError::Invalid));
        assert_eq!(
            Version::try_decode(b"RTSP/9.9"),
            Err(DecodeError::Unknown(9, 9))
        );
        assert_eq!(
            Version::try_decode(b"rtsp/0.0"),
            Err(DecodeError::Unknown(0, 0))
        );
    }
}
