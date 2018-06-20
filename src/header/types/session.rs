use chrono::offset::Utc;
use std::convert::TryFrom;
use std::time::Duration;

use header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use session::Session as SessionData;
use session::{
    ExpiredSession, InvalidSessionID, SessionID, DEFAULT_SESSION_TIMEOUT, MAX_SESSION_TIMEOUT,
};
use syntax::trim_whitespace;

/// The `Session` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.49).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Session {
    id: SessionID,
    timeout: Option<Duration>,
}

impl Session {
    pub fn try_from_session_with_timeout<S>(value: S) -> Result<Self, ExpiredSession>
    where
        S: SessionData,
    {
        let timeout = value
            .timeout()
            .signed_duration_since(Utc::now())
            .to_std()
            .map_err(|_| ExpiredSession)?;

        Ok(Session {
            id: value.id().clone(),
            timeout: Some(timeout),
        })
    }

    pub fn try_from_session_without_timeout<S>(value: S) -> Result<Self, ExpiredSession>
    where
        S: SessionData,
    {
        let mut session = Session::try_from_session_with_timeout(value)?;
        session.timeout = None;
        Ok(session)
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
    /// let session = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), Some(Duration::new(180, 0)));
    /// ```
    pub fn with_timeout<T>(id: T, timeout: u64) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        Ok(Session {
            id: SessionID::try_from(id)?,
            timeout: Some(Duration::new(timeout, 0)),
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
    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Session
    }

    /// Converts the `Session` type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    /// use rtsp::session::DEFAULT_SESSION_TIMEOUT;
    ///
    /// let typed_header = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    ///
    /// let typed_header = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// let raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()
    /// ];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    ///
    /// let typed_header =
    ///     Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", DEFAULT_SESSION_TIMEOUT).unwrap();
    /// let raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()
    /// ];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    /// ```
    fn to_header_raw(&self) -> Vec<HeaderValue> {
        let id = self.id.as_str();

        let value = if let Some(timeout) = self.timeout {
            if timeout.as_secs() == DEFAULT_SESSION_TIMEOUT {
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

        vec![unsafe { HeaderValue::from_str_unchecked(value) }]
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
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    ///
    /// let typed_header = Session::without_timeout("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    ///
    /// assert_eq!(
    ///     Session::try_from_header_raw(&raw_header).unwrap(),
    ///     typed_header
    /// );
    ///
    /// let typed_header = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// let raw_header = vec![
    ///     HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()
    /// ];
    ///
    /// assert_eq!(
    ///     Session::try_from_header_raw(&raw_header).unwrap(),
    ///     typed_header
    /// );
    ///
    /// let raw_header = vec![HeaderValue::try_from("invalid session").unwrap()];
    ///
    /// assert!(Session::try_from_header_raw(&raw_header).is_err());
    /// ```
    fn try_from_header_raw(header: &[HeaderValue]) -> Result<Self, InvalidTypedHeader> {
        if header.len() == 0 || header.len() > 1 {
            return Err(InvalidTypedHeader);
        }

        let parts = header[0].as_str().splitn(2, ';').collect::<Vec<&str>>();
        let id = SessionID::try_from(trim_whitespace(parts[0])).map_err(|_| InvalidTypedHeader)?;

        if parts.len() == 1 {
            Ok(Session { id, timeout: None })
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
                            Ok(Session {
                                id,
                                timeout: Some(Duration::new(delta, 0)),
                            })
                        }
                    })
            }
        }
    }
}
