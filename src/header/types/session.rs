use std::convert::TryFrom;
use std::iter::once;
use std::time::Duration;

use crate::header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use crate::session::Session as SessionData;
use crate::session::{InvalidSessionID, SessionID, DEFAULT_SESSION_TIMEOUT, MAX_SESSION_TIMEOUT};
use crate::syntax::trim_whitespace;

/// The `Session` typed header as described by
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
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::SessionID;
    /// use rtsp::header::types::Session;
    ///
    /// let session = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), None);
    /// ```
    pub fn without_timeout<T>(id: T) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
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
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::SessionID;
    /// use rtsp::header::types::Session;
    ///
    /// let session =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", Duration::from_secs(180)).unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), Some(Duration::new(180, 0)));
    /// ```
    pub fn with_timeout<T>(id: T, timeout: Duration) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
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
    type DecodeError = InvalidTypedHeader;

    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Session
    }

    /// Converts the raw header values to the `Session` header type. Based on the syntax provided by
    /// [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the following
    /// syntax:
    ///
    /// ```text
    /// UPALPHA = %x41-5A ; any US-ASCII uppercase letter "A".."Z"
    /// LOALPHA = %x61-7A ; any US-ASCII lowercase letter "a".."z"
    /// ALPHA = UPALPHA / LOALPHA
    /// DIGIT = %x30-39 ; any US-ASCII digit "0".."9"
    /// safe = "$" / "-" / "_" / "." / "+"
    /// session-id = 1*256( ALPHA / DIGIT / safe )
    /// Session = "Session" HCOLON session-id
    ///           [ SEMI "timeout" EQUAL delta-seconds ]
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    /// use rtsp::header::TypedHeader;
    ///
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
            return Err(InvalidTypedHeader);
        }

        let parts = value.as_str().splitn(2, ';').collect::<Vec<&str>>();
        let id = SessionID::try_from(trim_whitespace(parts[0])).map_err(|_| InvalidTypedHeader)?;

        if parts.len() == 1 {
            Ok(Some(Session { id, timeout: None }))
        } else {
            let parts = parts[1]
                .splitn(2, '=')
                .map(|part| trim_whitespace(part))
                .collect::<Vec<&str>>();

            if parts.len() != 2 || parts[0].to_lowercase() != "timeout" {
                Err(InvalidTypedHeader)
            } else {
                parts[1]
                    .parse::<u64>()
                    .map_err(|_| InvalidTypedHeader)
                    .and_then(|delta| {
                        if delta > MAX_SESSION_TIMEOUT {
                            Err(InvalidTypedHeader)
                        } else {
                            Ok(Some(Session {
                                id,
                                timeout: Some(Duration::new(delta, 0)),
                            }))
                        }
                    })
            }
        }
    }

    /// Converts the `Session` type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    /// use rtsp::header::TypedHeader;
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
        values.extend(once(unsafe { HeaderValue::from_str_unchecked(value) }));
    }
}
