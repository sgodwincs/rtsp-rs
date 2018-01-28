use chrono::offset::Utc;
use std::convert::TryFrom;
use std::time::Duration;

use header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use session::{ExpiredSession, InvalidSessionID, SessionID, DEFAULT_TIMEOUT, MAX_TIMEOUT};
use session::Session as SessionData;
use syntax::trim_whitespace;

/// The `Session` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.49).
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Session {
    id: SessionID,
    timeout: Duration,
}

impl Session {
    /// Constructs a new `Session` instance with the specified session ID.
    ///
    /// # Example
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::SessionID;
    /// use rtsp::header::types::Session;
    ///
    /// let session = Session::new("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), Duration::new(60, 0));
    /// ```
    pub fn new<T>(id: T) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        Session::with_timeout(id, DEFAULT_TIMEOUT)
    }

    /// Constructs a new `Session` instance with the specified session ID.
    ///
    /// # Example
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// use std::time::Duration;
    ///
    /// use rtsp::SessionID;
    /// use rtsp::header::types::Session;
    ///
    /// let session = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// assert_eq!(session.id(), &SessionID::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap());
    /// assert_eq!(session.timeout(), Duration::new(180, 0));
    /// ```
    pub fn with_timeout<T>(id: T, timeout: u64) -> Result<Self, InvalidSessionID>
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        Ok(Session {
            id: SessionID::try_from(id)?,
            timeout: Duration::new(timeout, 0),
        })
    }

    pub fn id(&self) -> &SessionID {
        &self.id
    }

    pub fn id_mut(&mut self) -> &mut SessionID {
        &mut self.id
    }

    pub fn timeout(&self) -> Duration {
        self.timeout
    }

    pub fn timeout_mut(&mut self) -> &mut Duration {
        &mut self.timeout
    }

    pub fn try_from_session<S>(value: S) -> Result<Self, ExpiredSession>
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
            timeout: timeout,
        })
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
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    ///
    /// let typed_header = Session::new("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    ///
    /// let typed_header = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()];
    /// assert_eq!(typed_header.to_header_raw(), raw_header);
    /// ```
    fn to_header_raw(&self) -> Vec<HeaderValue> {
        let value = if self.timeout.as_secs() == DEFAULT_TIMEOUT {
            self.id.as_str().to_string()
        } else {
            format!("{}; timeout = {}", self.id.as_str(), self.timeout.as_secs())
        };

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
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::Session;
    ///
    /// let typed_header = Session::new("QKyjN8nt2WqbWw4tIYof52").unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52").unwrap()];
    ///
    /// assert_eq!(
    ///     Session::try_from_header_raw(&raw_header).unwrap(),
    ///     typed_header
    /// );
    ///
    /// let typed_header = Session::with_timeout("QKyjN8nt2WqbWw4tIYof52", 180).unwrap();
    /// let raw_header = vec![HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 180").unwrap()];
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

        if parts.len() > 2 {
            return Err(InvalidTypedHeader);
        }

        let id = SessionID::try_from(trim_whitespace(parts[0])).map_err(|_| InvalidTypedHeader)?;

        if parts.len() == 1 {
            Ok(Session {
                id,
                timeout: Duration::new(DEFAULT_TIMEOUT, 0),
            })
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
                        if delta > MAX_TIMEOUT {
                            Err(InvalidTypedHeader)
                        } else {
                            Ok(Session {
                                id,
                                timeout: Duration::new(delta, 0),
                            })
                        }
                    })
            }
        }
    }
}
