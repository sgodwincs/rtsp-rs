use ascii::{AsciiChar, AsciiString};
use chrono::{offset, DateTime, Utc};
use rand::{thread_rng, Rng};
use std::convert::TryFrom;
use std::error::Error;
use std::time::Duration;
use std::{fmt, str};

pub const DEFAULT_SESSION_TIMEOUT: Duration = Duration::from_secs(60);
pub const MAX_SESSION_TIMEOUT: u64 = 9_999_999_999_999_999_999;
pub const SESSION_ID_ALPHABET: [u8; 67] = [
    b'$', b'-', b'_', b'.', b'+', b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a',
    b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', b'p', b'q',
    b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z', b'A', b'B', b'C', b'D', b'E', b'F', b'G',
    b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O', b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W',
    b'X', b'Y', b'Z',
];
pub const SESSION_ID_GENERATED_LENGTH: usize = 22;

/// A wrapper type used to avoid users creating invalid session identifiers.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct SessionID(AsciiString);

impl SessionID {
    pub fn random() -> Self {
        let mut rng = thread_rng();
        let mut session_id = AsciiString::with_capacity(SESSION_ID_GENERATED_LENGTH);

        for _ in 0..=SESSION_ID_GENERATED_LENGTH {
            let c = *rng
                .choose(&SESSION_ID_ALPHABET)
                .expect("`SESSION_ID_ALPHABET` should not be empty");
            let c = AsciiChar::from(c)
                .expect("None of the characters in the alphabet should be non ASCII");
            session_id.push(c);
        }

        SessionID(session_id)
    }

    /// Returns a `&str` representation of the session identifier.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::SessionID;
    ///
    /// assert_eq!(
    ///     SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap().as_str(),
    ///     "QKyjN8nt2WqbWw4tIYof52"
    /// );
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<str> for SessionID {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for SessionID {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl fmt::Display for SessionID {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

/// Performs equality checking of a `SessionID` with a `str`. This check is case sensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// use std::convert::TryFrom;
///
/// use rtsp::SessionID;
///
/// assert_eq!(SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap(), "QKyjN8nt2WqbWw4tIYof52");
/// ```
impl PartialEq<str> for SessionID {
    fn eq(&self, other: &str) -> bool {
        self.0 == other
    }
}

/// Performs equality checking of a `SessionID` with a `&str`. This check is case sensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// use std::convert::TryFrom;
///
/// use rtsp::SessionID;
///
/// assert_eq!(SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap(), "QKyjN8nt2WqbWw4tIYof52");
/// ```
impl<'a> PartialEq<&'a str> for SessionID {
    fn eq(&self, other: &&'a str) -> bool {
        self.0 == *other
    }
}

/// Provides a fallible conversion from a byte slice to a `SessionID`. Note that you cannot do
/// the following:
///
/// ```compile_fail
/// let session_id = SessionID::try_from(b"QKyjN8nt2WqbWw4tIYof52").unwrap();
/// ```
///
/// This is because `b"QKyjN8nt2WqbWw4tIYof52"` is of type `&[u8; 22]` and so it must be converted
/// to `&[u8]` in order to perform the conversion. Another `TryFrom` implementation from
/// `&[u8, N: usize]` will be provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for SessionID {
    type Error = InvalidSessionID;

    /// Converts a `&[u8]` to an RTSP session ID. The session ID must follow the following rules:
    ///
    /// ```text
    /// UPALPHA = %x41-5A ; any US-ASCII uppercase letter "A".."Z"
    /// LOALPHA = %x61-7A ; any US-ASCII lowercase letter "a".."z"
    /// ALPHA = UPALPHA / LOALPHA
    /// DIGIT = %x30-39 ; any US-ASCII digit "0".."9"
    /// safe = "$" / "-" / "_" / "." / "+"
    /// session-id =  1*256( ALPHA / DIGIT / safe )
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::SessionID;
    ///
    /// let session_id = SessionID::try_from(&b"QKyjN8nt2WqbWw4tIYof52"[..]).unwrap();
    /// assert_eq!(session_id.as_str(), "QKyjN8nt2WqbWw4tIYof52");
    ///
    /// let error = SessionID::try_from(&b""[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        // Although the syntax in the specification allows session IDs of length 1, it also states
        // earlier in the specfication that "Session identifiers are strings of a length between
        // 8-128 characters." Obviously, there is some disconnect here, so I will go with the more
        // secure approach.

        if value.len() < 8 || value.len() > 256 {
            return Err(InvalidSessionID);
        }

        for &b in value.iter() {
            if !b.is_ascii()
                || (!(b as char).is_alphanumeric()
                    && b != b'$'
                    && b != b'-'
                    && b != b'_'
                    && b != b'.'
                    && b != b'+')
            {
                return Err(InvalidSessionID);
            }
        }

        let value = unsafe { AsciiString::from_ascii_unchecked(value) };
        Ok(SessionID(value))
    }
}

impl<'a> TryFrom<&'a str> for SessionID {
    type Error = InvalidSessionID;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        SessionID::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a `SessionID` from a `&[u8]` or `&str`.
///
/// This error indicates that the session ID was empty or contained invalid characters.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidSessionID;

impl fmt::Display for InvalidSessionID {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidSessionID {
    fn description(&self) -> &str {
        "invalid RTSP session identifier"
    }
}

pub trait Session {
    fn expire_time(&self) -> DateTime<Utc>;

    fn id(&self) -> &SessionID;

    fn is_expired(&self) -> bool {
        offset::Utc::now() > self.expire_time()
    }

    fn set_expire_time(&mut self, expire_time: DateTime<Utc>);

    fn set_timeout(&mut self, timeout: Duration) -> Result<(), ()>;

    fn timeout(&self) -> Option<Duration> {
        self.expire_time()
            .signed_duration_since(offset::Utc::now())
            .to_std()
            .ok()
    }
}
