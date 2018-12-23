use itertools::Itertools;
use std::collections::HashSet;
use std::convert::TryFrom;
use std::iter::{once, FromIterator};
use std::ops::{Deref, DerefMut};

use crate::header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};
use crate::method::Method;
use crate::syntax::trim_whitespace;

/// The `"Public"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.39).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Public(pub HashSet<Method>);

impl Public {
    /// Constructs a new header with no methods by default.
    pub fn new() -> Self {
        Public::default()
    }
}

impl Deref for Public {
    type Target = HashSet<Method>;

    fn deref(&self) -> &HashSet<Method> {
        &self.0
    }
}

impl DerefMut for Public {
    fn deref_mut(&mut self) -> &mut HashSet<Method> {
        &mut self.0
    }
}

impl FromIterator<Method> for Public {
    fn from_iter<I>(iterator: I) -> Self
    where
        I: IntoIterator<Item = Method>,
    {
        Public(HashSet::from_iter(iterator))
    }
}

impl TypedHeader for Public {
    type DecodeError = InvalidTypedHeader;

    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Public
    }

    /// Converts the raw header values to the [`Public`] header type. Based on the syntax
    /// provided by [RFC7826](https://tools.ietf.org/html/rfc7826#section-20), this header has the
    /// following syntax:
    ///
    /// ```text
    /// CR = %x0D ; US-ASCII CR, carriage return (13)
    /// LF = %x0A ; US-ASCII LF, linefeed (10)
    /// SP = %x20 ; US-ASCII SP, space (32)
    /// HT = %x09 ; US-ASCII HT, horizontal-tab (9)
    /// LWS = [CRLF] 1*( SP / HT ) ; Line-breaking whitespace
    /// SWS = [LWS] ; Separating whitespace
    /// HCOLON = *( SP / HT ) ":" SWS
    /// token = 1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
    ///       / %x41-5A / %x5E-7A / %x7C / %x7E)
    ///       ; 1*<any CHAR except CTLs or tspecials>
    /// COMMA = SWS "," SWS ; comma
    /// Method  = "DESCRIBE"
    ///         / "GET_PARAMETER"
    ///         / "OPTIONS"
    ///         / "PAUSE"
    ///         / "PLAY"
    ///         / "PLAY_NOTIFY"
    ///         / "REDIRECT"
    ///         / "SETUP"
    ///         / "SET_PARAMETER"
    ///         / "TEARDOWN"
    ///         / extension-method
    /// extension-method = token
    /// Public = "Public" HCOLON Method *(COMMA Method)
    /// ```
    ///
    /// This conversion will never fail. All values separated with commas will be converted to the
    /// [`Method`] type. If this fails, then it will be ignored.
    ///
    /// The absence of a header value defaults the header to an empty list of methods.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Public;
    /// use rtsp::header::TypedHeader;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(Public::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = vec![Method::Play, Method::Setup].into_iter().collect::<Public>();
    /// let raw_header = vec![HeaderValue::try_from("SETUP, \"INVALID\", PLAY, PLAY").unwrap()];
    /// assert_eq!(
    ///     Public::decode(&mut raw_header.iter()).unwrap(),
    ///     Some(typed_header)
    /// );
    /// ```
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let mut methods = HashSet::new();
        let mut present = false;

        for value in values {
            let parts = value.as_str().split(',');

            for part in parts {
                if let Ok(method) = Method::try_from(trim_whitespace(part)) {
                    methods.insert(method);
                }
            }

            present = true;
        }

        if present {
            Ok(Some(Public(methods)))
        } else {
            Ok(None)
        }
    }

    /// Converts the [`Public`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Public;
    /// use rtsp::header::TypedHeader;
    ///
    /// let typed_header = vec![Method::Play, Method::Setup].into_iter().collect::<Public>();
    /// let expected_raw_headers = vec![
    ///     vec![HeaderValue::try_from("PLAY, SETUP").unwrap()],
    ///     vec![HeaderValue::try_from("SETUP, PLAY").unwrap()],
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert!(raw_header == expected_raw_headers[0] ||
    ///         raw_header == expected_raw_headers[1]);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        // Unsafe Justification
        //
        // Header values must be valid UTF-8, and since we know that the [`Method`] type
        // guarantees valid ASCII-US (with no newlines), it satisfies the constraints.

        let value = self.iter().map(|method| method.as_str()).join(", ");
        values.extend(once(unsafe { HeaderValue::from_str_unchecked(value) }));
    }
}
