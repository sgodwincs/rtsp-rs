//!

use chrono::{offset, DateTime, Duration, Utc};
use std::convert::TryFrom;
use std::error::Error;
use std::{fmt, str};

pub const DEFAULT_TIMEOUT: u64 = 60;
pub const MAX_TIMEOUT: u64 = 9_999_999_999_999_999_999;

/// A wrapper type used to avoid users creating invalid session identifiers.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct SessionID(String);

impl SessionID {
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
        write!(formatter, "{}", self.0)
    }
}

impl fmt::Display for SessionID {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
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
        self.0.to_lowercase() == (*other).to_lowercase()
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
        SessionID::try_from(str::from_utf8(value).map_err(|_| InvalidSessionID)?)
    }
}

impl<'a> TryFrom<&'a str> for SessionID {
    type Error = InvalidSessionID;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        if value.len() < 8 || value.len() > 256 {
            return Err(InvalidSessionID);
        }

        for c in value.chars() {
            if !c.is_ascii()
                || (!c.is_alphanumeric() && c != '$' && c != '-' && c != '_' && c != '.'
                    && c != '+')
            {
                return Err(InvalidSessionID);
            }
        }

        Ok(SessionID(value.to_string()))
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
    fn id(&self) -> &SessionID;
    fn timeout(&self) -> DateTime<Utc>;
    fn touch(&mut self);
}

pub struct DefaultSession {
    id: SessionID,
    timeout: DateTime<Utc>,
}

impl DefaultSession {
    pub fn new<T>(id: T) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        let timeout = offset::Utc::now()
            .checked_add_signed(Duration::seconds(DEFAULT_TIMEOUT as i64))
            .unwrap();

        DefaultSession::with_timeout(id, timeout)
    }

    pub fn with_timeout<T>(id: T, timeout: DateTime<Utc>) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        Ok(DefaultSession {
            id: SessionID::try_from(id)?,
            timeout: timeout,
        })
    }
}

impl Session for DefaultSession {
    fn id(&self) -> &SessionID {
        &self.id
    }

    fn timeout(&self) -> DateTime<Utc> {
        self.timeout
    }

    fn touch(&mut self) {
        self.timeout = offset::Utc::now();
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ExpiredSession;

impl fmt::Display for ExpiredSession {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for ExpiredSession {
    fn description(&self) -> &str {
        "expired RTSP session"
    }
}
