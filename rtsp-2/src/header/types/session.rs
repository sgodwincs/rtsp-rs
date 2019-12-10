use core::num::IntErrorKind;
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::iter::once;
use std::time::Duration;

use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;
use crate::session::Session as SessionData;
use crate::session::{SessionID, SessionIDError};
use crate::syntax;

pub use crate::session::{DEFAULT_SESSION_TIMEOUT, MAX_SESSION_TIMEOUT};

/// The `"Session"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.49).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Session {
    id: SessionID,
    timeout: Option<Duration>,
}

impl Session {
    pub fn try_from_session_with_timeout<S>(value: S) -> Self
    where
        S: SessionData,
    {
        Session {
            id: value.id().clone(),
            timeout: value.timeout(),
        }
    }

    pub fn try_from_session_without_timeout<S>(value: S) -> Self
    where
        S: SessionData,
    {
        Session {
            id: value.id().clone(),
            timeout: None,
        }
    }

    /// Constructs a new `Session` instance with the specified session ID. No timeout is set for
    /// this session.
    ///
    /// # Example
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::header::types::Session;
    /// use rtsp::session::SessionID;
    ///
    /// let session = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), None);
    /// ```
    pub fn without_timeout<T>(id: T) -> Result<Self, SessionIDError>
    where
        SessionID: TryFrom<T, Error = SessionIDError>,
    {
        Ok(Session {
            id: SessionID::try_from(id)?,
            timeout: None,
        })
    }

    /// Constructs a new `Session` instance with the specified session ID and timeout.
    ///
    /// # Example
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::header::types::Session;
    /// use rtsp::session::SessionID;
    ///
    /// let session =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", Duration::from_secs(180)).unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), Some(Duration::new(180, 0)));
    /// ```
    pub fn with_timeout<T>(id: T, timeout: Duration) -> Result<Self, SessionIDError>
    where
        SessionID: TryFrom<T, Error = SessionIDError>,
    {
        Ok(Session {
            id: SessionID::try_from(id)?,
            timeout: Some(timeout),
        })
    }

    pub fn has_timeout(&self) -> bool {
        self.timeout.is_some()
    }

    pub fn id(&self) -> &SessionID {
        &self.id
    }

    pub fn id_mut(&mut self) -> &mut SessionID {
        &mut self.id
    }

    pub fn timeout(&self) -> Option<Duration> {
        self.timeout
    }

    pub fn timeout_mut(&mut self) -> &mut Option<Duration> {
        &mut self.timeout
    }
}

impl TypedHeader for Session {
    type DecodeError = SessionError;

    /// Converts the raw header values to the [`Session`] header type. Based on the syntax provided
    /// by [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the following
    /// syntax:
    ///
    /// ```text
    /// UPALPHA = %x41-5A ; any US-ASCII uppercase letter "A".."Z"
    /// LOALPHA = %x61-7A ; any US-ASCII lowercase letter "a".."z"
    /// ALPHA = UPALPHA / LOALPHA
    /// DIGIT = %x30-39 ; any US-ASCII digit "0".."9"
    /// CR = %x0D ; US-ASCII CR, carriage return (13)
    /// LF = %x0A  ; US-ASCII LF, linefeed (10)
    /// SP = %x20  ; US-ASCII SP, space (32)
    /// HT = %x09  ; US-ASCII HT, horizontal-tab (9)
    /// CRLF = CR LF
    /// LWS = [CRLF] 1*( SP / HT ) ; Line-breaking whitespace
    /// SWS = [LWS] ; Separating whitespace
    /// HCOLON = *( SP / HT ) ":" SWS
    /// safe = "$" / "-" / "_" / "." / "+"
    /// EQUAL = SWS "=" SWS ; equal
    /// SEMI = SWS ";" SWS ; semicolon
    /// session-id = 1*256( ALPHA / DIGIT / safe )
    /// Session = "Session" HCOLON session-id
    ///           [ SEMI "timeout" EQUAL delta-seconds ]
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::Session;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(Session::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    /// assert_eq!(Session::decode(&mut raw_header.iter()).unwrap(), Some(typed_header));
    ///
    /// let typed_header =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", Duration::from_secs(180)).unwrap();
    /// let raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()
    /// ];
    /// assert_eq!(Session::decode(&mut raw_header.iter()).unwrap(), Some(typed_header));
    ///
    /// let raw_header = vec![HeaderValue::try_from("invalid session").unwrap()];
    /// assert!(Session::decode(&mut raw_header.iter()).is_err());
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
            return Err(SessionError::MoreThanOneHeader);
        }

        let parts = value.as_str().splitn(2, ';').collect::<Vec<&str>>();
        let id = SessionID::try_from(syntax::trim_whitespace(parts[0]))?;

        if parts.len() == 1 {
            Ok(Some(Session { id, timeout: None }))
        } else {
            let parts = parts[1]
                .splitn(2, '=')
                .map(|part| syntax::trim_whitespace(part))
                .collect::<Vec<&str>>();

            if parts.len() != 2 || parts[0].to_lowercase() != "timeout" {
                Err(SessionError::InvalidParameterSyntax)
            } else {
                let delta = parts[1]
                    .parse::<u64>()
                    .map_err(|error| TimeoutError::try_from(error.kind().clone()).unwrap())?;

                if delta > MAX_SESSION_TIMEOUT {
                    Err(SessionError::Timeout(TimeoutError::ExceedsMaximumLength))
                } else {
                    Ok(Some(Session {
                        id,
                        timeout: Some(Duration::new(delta, 0)),
                    }))
                }
            }
        }
    }

    /// Converts the `Session` type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::Session;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::session::DEFAULT_SESSION_TIMEOUT;
    ///
    /// let typed_header = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let expected_raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    ///
    /// let typed_header =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", Duration::from_secs(180)).unwrap();
    /// let expected_raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    ///
    /// let typed_header =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", DEFAULT_SESSION_TIMEOUT).unwrap();
    /// let expected_raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        let id = self.id.as_str();

        let value = if let Some(timeout) = self.timeout {
            if timeout.as_secs() == DEFAULT_SESSION_TIMEOUT.as_secs() {
                id.to_string()
            } else {
                format!("{}; timeout = {}", id, timeout.as_secs())
            }
        } else {
            id.to_string()
        };

        // Unsafe Justification
        //
        // In order for this to be safe, we must ensure that `value` contains no unprintable
        // ASCII-US characters and that all linebreaks of the form `"\r\n"` are followed by a space
        // or tab. In the above construction, the only thing that could violate this constraint
        // would be the serialization of the session ID. However, a session ID can only have a
        // subset of printable ASCII-US characters and cannot have newlines or carriage returns.
        values.extend(once(unsafe { HeaderValue::from_string_unchecked(value) }));
    }

    /// Returns the statically assigned [`HeaderName`] for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Session
    }
}

/// A possible error value when converting to a [`Session`] from [`HeaderName`]s.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum SessionError {
    /// The parameter syntax was invalid (e.g. `"; timeout = ..."`).
    InvalidParameterSyntax,

    /// There was more than one `"Session"` header.
    MoreThanOneHeader,

    /// The session ID was invalid.
    SessionID(SessionIDError),

    Timeout(TimeoutError),
}

impl Display for SessionError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::SessionError::*;

        match self {
            InvalidParameterSyntax => write!(formatter, "invalid session header parameter syntax"),
            MoreThanOneHeader => write!(formatter, "more than one session header"),
            SessionID(error) => error.fmt(formatter),
            Timeout(error) => error.fmt(formatter),
        }
    }
}

impl Error for SessionError {}

impl From<Infallible> for SessionError {
    fn from(_: Infallible) -> Self {
        SessionError::InvalidParameterSyntax
    }
}

impl From<SessionIDError> for SessionError {
    fn from(value: SessionIDError) -> Self {
        SessionError::SessionID(value)
    }
}

impl From<TimeoutError> for SessionError {
    fn from(value: TimeoutError) -> Self {
        SessionError::Timeout(value)
    }
}

/// A possible error value when parsing the timeout parameter of a `"Session"` header.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum TimeoutError {
    /// The `"Session"` header timeout was empty.
    Empty,

    /// The `"Session"` header timeout was parsed, but the length exceeds the maximum length a
    /// timeout can be.
    ExceedsMaximumLength,

    /// The `"Session"` header timeout contained an invalid digit.
    InvalidDigit,

    /// The `"Session"` value timeout could not be parsed as it overflowed.
    Overflow,
}

impl Display for TimeoutError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::TimeoutError::*;

        match self {
            Empty => write!(formatter, "empty session header timeout"),
            ExceedsMaximumLength => {
                write!(formatter, "session header timeout exceeds maximum length")
            }
            InvalidDigit => write!(formatter, "invalid session header timeout digit"),
            Overflow => write!(formatter, "session header timeout overflow"),
        }
    }
}

impl Error for TimeoutError {}

impl From<Infallible> for TimeoutError {
    fn from(_: Infallible) -> Self {
        TimeoutError::Empty
    }
}

impl TryFrom<IntErrorKind> for TimeoutError {
    type Error = ();

    fn try_from(value: IntErrorKind) -> Result<Self, Self::Error> {
        use self::TimeoutError::*;

        match value {
            IntErrorKind::Empty => Ok(Empty),
            IntErrorKind::InvalidDigit => Ok(InvalidDigit),
            IntErrorKind::Overflow => Ok(Overflow),
            _ => Err(()),
        }
    }
}
