//! Header Name

use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::str;

use crate::syntax;

macro_rules! standard_headers {
    (
        $(
            $(#[$docs:meta])*
            ($variant:ident, $name:expr, $canonical_name:expr);
        )+
    ) => {
        /// An RTSP header name.
        ///
        /// All standardized header names are supported with an ASCII encoded extension.
        #[derive(Clone, Eq, Hash, PartialEq)]
        #[non_exhaustive]
        pub enum HeaderName {
        $(
            $(#[$docs])*
            $variant,
        )+

            /// A header name that is not one of the standardized header names. This is encoded
            /// using ASCII-US.
            Extension(ExtensionHeaderName)
        }

        impl HeaderName {
            /// Returns a `&str` representation of the header name.
            ///
            /// # Examples
            ///
            /// ```
            /// use std::convert::TryFrom;
            ///
            /// use rtsp::header::name::HeaderName;
            ///
            /// assert_eq!(HeaderName::ContentLength.as_str(), "content-length");
            /// ```
            pub fn as_str(&self) -> &str {
                use self::HeaderName::*;

                match *self {
                $(
                    $variant => $name,
                )+
                    Extension(ref extension) => extension.as_str()
                }
            }
            /// Returns a `&str` canonical representation of the header name.
            ///
            /// # Examples
            ///
            /// ```
            /// use std::convert::TryFrom;
            ///
            /// use rtsp::header::name::HeaderName;
            ///
            /// assert_eq!(HeaderName::ContentLength.canonical_name(), "Content-Length");
            /// ```
            pub fn canonical_name(&self) -> &str {
                use self::HeaderName::*;

                match *self {
                $(
                    $variant => $canonical_name,
                )+
                    Extension(ref extension) => extension.canonical_name()
                }
            }
        }

        #[cfg(test)]
        mod test {
            use crate::header::name::HeaderName;

            #[test]
            fn test_standard_header_as_str() {
            $(
                let header_name = HeaderName::$variant;
                assert_eq!(header_name.as_str(), $name);
            )+
            }

            #[test]
            fn test_standard_header_canonical_name() {
            $(
                let header_name = HeaderName::$variant;
                assert_eq!(header_name.canonical_name(), $canonical_name);
            )+
            }

            #[test]
            fn test_standard_header_name_equality() {
            $(
                let header_name = HeaderName::$variant;
                assert_eq!(
                    header_name.as_str(),
                    header_name.canonical_name().to_lowercase().as_str()
                );
            )+
            }
        }
    }
}

impl HeaderName {
    /// A helper function that creates a new [`HeaderName`] instance with the given header name
    /// extension.
    ///
    /// It first checks to see if the header name is valid, and if not, it will return an error.
    fn extension(name: &[u8]) -> Result<HeaderName, HeaderNameError> {
        if name.is_empty() {
            return Err(HeaderNameError::Empty);
        }

        if !syntax::is_token(name) {
            return Err(HeaderNameError::InvalidCharacter);
        }

        // Unsafe: The function above [`syntax::is_token`] ensures that the value is valid ASCII-US.
        let name = unsafe { str::from_utf8_unchecked(name) }.to_string();
        let name_lowercase = name.to_ascii_lowercase();
        Ok(HeaderName::Extension(ExtensionHeaderName(
            name,
            name_lowercase,
        )))
    }
}

impl AsRef<[u8]> for HeaderName {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for HeaderName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Debug for HeaderName {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.canonical_name())
    }
}

impl Display for HeaderName {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.canonical_name())
    }
}

impl From<HeaderName> for String {
    fn from(value: HeaderName) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for HeaderName {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<HeaderName> for [u8] {
    fn eq(&self, other: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'header> PartialEq<&'header [u8]> for HeaderName {
    fn eq(&self, other: &&'header [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'header> PartialEq<HeaderName> for &'header [u8] {
    fn eq(&self, other: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for HeaderName {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<HeaderName> for str {
    fn eq(&self, other: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'header> PartialEq<&'header str> for HeaderName {
    fn eq(&self, other: &&'header str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'header> PartialEq<HeaderName> for &'header str {
    fn eq(&self, other: &HeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'a> TryFrom<&'a [u8]> for HeaderName {
    type Error = HeaderNameError;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        use self::HeaderName::*;

        macro_rules! check_header_name {
            ($header_name:ident) => {
                let bytes = $header_name.as_str().as_bytes();
                let starts_with = value
                    .iter()
                    .take(bytes.len())
                    .map(u8::to_ascii_lowercase)
                    .eq(bytes.iter().cloned());

                if starts_with && value.len() == bytes.len() {
                    return Ok($header_name);
                }
            };
        }

        match value.len() {
            3 => {
                check_header_name!(Via);
                HeaderName::extension(value)
            }
            4 => {
                check_header_name!(CSeq);
                check_header_name!(Date);
                check_header_name!(From);
                check_header_name!(MTag);
                HeaderName::extension(value)
            }
            5 => {
                check_header_name!(Allow);
                check_header_name!(Range);
                check_header_name!(Scale);
                check_header_name!(Speed);
                HeaderName::extension(value)
            }
            6 => {
                check_header_name!(Accept);
                check_header_name!(Public);
                check_header_name!(Server);
                HeaderName::extension(value)
            }
            7 => {
                check_header_name!(Expires);
                check_header_name!(Require);
                check_header_name!(Session);
                HeaderName::extension(value)
            }
            8 => {
                check_header_name!(IfMatch);
                check_header_name!(Location);
                check_header_name!(Referrer);
                check_header_name!(RTPInfo);
                HeaderName::extension(value)
            }
            9 => {
                check_header_name!(Bandwidth);
                check_header_name!(Blocksize);
                check_header_name!(Supported);
                check_header_name!(Timestamp);
                check_header_name!(Transport);
                HeaderName::extension(value)
            }
            10 => {
                check_header_name!(Connection);
                check_header_name!(SeekStyle);
                check_header_name!(UserAgent);
                HeaderName::extension(value)
            }
            11 => {
                check_header_name!(MediaRange);
                check_header_name!(RetryAfter);
                check_header_name!(Unsupported);
                HeaderName::extension(value)
            }
            12 => {
                check_header_name!(ContentBase);
                check_header_name!(ContentType);
                HeaderName::extension(value)
            }
            13 => {
                check_header_name!(AcceptRanges);
                check_header_name!(Authorization);
                check_header_name!(CacheControl);
                check_header_name!(IfNoneMatch);
                check_header_name!(LastModified);
                check_header_name!(NotifyReason);
                check_header_name!(ProxyRequire);
                HeaderName::extension(value)
            }
            14 => {
                check_header_name!(ContentLength);
                check_header_name!(RequestStatus);
                HeaderName::extension(value)
            }
            15 => {
                check_header_name!(AcceptEncoding);
                check_header_name!(AcceptLanguage);
                check_header_name!(ProxySupported);
                HeaderName::extension(value)
            }
            16 => {
                check_header_name!(ContentEncoding);
                check_header_name!(ContentLanguage);
                check_header_name!(ContentLocation);
                check_header_name!(MediaProperties);
                check_header_name!(TerminateReason);
                check_header_name!(WWWAuthenticate);
                HeaderName::extension(value)
            }
            17 => {
                check_header_name!(IfModifiedSince);
                HeaderName::extension(value)
            }
            18 => {
                check_header_name!(AcceptCredentials);
                check_header_name!(PipelinedRequests);
                check_header_name!(ProxyAuthenticate);
                HeaderName::extension(value)
            }
            19 => {
                check_header_name!(AuthenticationInfo);
                check_header_name!(ProxyAuthorization);
                HeaderName::extension(value)
            }
            22 => {
                check_header_name!(ConnectionCredentials);
                HeaderName::extension(value)
            }
            25 => {
                check_header_name!(ProxyAuthenticationInfo);
                HeaderName::extension(value)
            }
            _ => HeaderName::extension(value),
        }
    }
}

impl<'a> TryFrom<&'a str> for HeaderName {
    type Error = HeaderNameError;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        HeaderName::try_from(value.as_bytes())
    }
}

/// A wrapper type used to avoid users creating extension header names that are actually
/// standardized header names.
#[derive(Clone)]
pub struct ExtensionHeaderName(String, String);

impl ExtensionHeaderName {
    /// Returns a `&str` representation of the extension header name. The returned string is
    /// lowercase even if the extension header name originally was a non-lowercase header name.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    ///
    /// match HeaderName::try_from("ExTeNsIoN").unwrap() {
    ///     HeaderName::Extension(extension) => assert_eq!(extension.as_str(), "extension"),
    ///     _ => panic!("expected extension header name")
    /// }
    /// ```
    pub fn as_str(&self) -> &str {
        self.1.as_str()
    }

    /// Returns a `&str` representation of the extension header name. The returned string is
    /// is of the same case as when it was created.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    ///
    /// match HeaderName::try_from("ExTeNsIoN").unwrap() {
    ///     HeaderName::Extension(extension) => assert_eq!(extension.canonical_name(), "ExTeNsIoN"),
    ///     _ => panic!("expected extension header name")
    /// }
    /// ```
    pub fn canonical_name(&self) -> &str {
        self.0.as_str()
    }
}

impl Debug for ExtensionHeaderName {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.1)
    }
}

impl Display for ExtensionHeaderName {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.1)
    }
}

impl Eq for ExtensionHeaderName {}

impl From<ExtensionHeaderName> for String {
    fn from(value: ExtensionHeaderName) -> Self {
        value.to_string()
    }
}

impl Hash for ExtensionHeaderName {
    fn hash<TState>(&self, state: &mut TState)
    where
        TState: Hasher,
    {
        self.1.hash(state);
    }
}

impl PartialEq for ExtensionHeaderName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq_ignore_ascii_case(other.as_str())
    }
}

impl PartialEq<[u8]> for ExtensionHeaderName {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionHeaderName> for [u8] {
    fn eq(&self, other: &ExtensionHeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'header> PartialEq<&'header [u8]> for ExtensionHeaderName {
    fn eq(&self, other: &&'header [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'header> PartialEq<ExtensionHeaderName> for &'header [u8] {
    fn eq(&self, other: &ExtensionHeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for ExtensionHeaderName {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionHeaderName> for str {
    fn eq(&self, other: &ExtensionHeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'header> PartialEq<&'header str> for ExtensionHeaderName {
    fn eq(&self, other: &&'header str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'header> PartialEq<ExtensionHeaderName> for &'header str {
    fn eq(&self, other: &ExtensionHeaderName) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

/// A possible error value when converting to a [`HeaderName`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum HeaderNameError {
    /// The header name was empty.
    Empty,

    /// The header name contained an invalid token character.
    InvalidCharacter,
}

impl Display for HeaderNameError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::HeaderNameError::*;

        match self {
            Empty => write!(formatter, "empty header name"),
            InvalidCharacter => write!(formatter, "invalid header name character"),
        }
    }
}

impl Error for HeaderNameError {}

impl From<Infallible> for HeaderNameError {
    fn from(_: Infallible) -> Self {
        HeaderNameError::Empty
    }
}

standard_headers! {
    /// Accept
    /// [[RFC7826, Section 18.1](https://tools.ietf.org/html/rfc7826#section-18.1)]
    (Accept, "accept", "Accept");

    /// Accept-Credentials
    /// [[RFC7826, Section 18.2](https://tools.ietf.org/html/rfc7826#section-18.2)]
    (AcceptCredentials, "accept-credentials", "Accept-Credentials");

    /// Accept-Encoding
    /// [[RFC7826, Section 18.3](https://tools.ietf.org/html/rfc7826#section-18.3)]
    (AcceptEncoding, "accept-encoding", "Accept-Encoding");

    /// Accept-Language
    /// [[RFC7826, Section 18.4](https://tools.ietf.org/html/rfc7826#section-18.4)]
    (AcceptLanguage, "accept-language", "Accept-Language");

    /// Accept-Ranges
    /// [[RFC7826, Section 18.5](https://tools.ietf.org/html/rfc7826#section-18.5)]
    (AcceptRanges, "accept-ranges", "Accept-Ranges");

    /// Allow
    /// [[RFC7826, Section 18.6](https://tools.ietf.org/html/rfc7826#section-18.6)]
    (Allow, "allow", "Allow");

    /// Authentication-Info
    /// [[RFC7826, Section 18.7](https://tools.ietf.org/html/rfc7826#section-18.7)]
    (AuthenticationInfo, "authentication-info", "Authentication-Info");

    /// Authorization
    /// [[RFC7826, Section 18.8](https://tools.ietf.org/html/rfc7826#section-18.8)]
    (Authorization, "authorization", "Authorization");

    /// Bandwidth
    /// [[RFC7826, Section 18.9](https://tools.ietf.org/html/rfc7826#section-18.9)]
    (Bandwidth, "bandwidth", "Bandwidth");

    /// Blocksize
    /// [[RFC7826, Section 18.10](https://tools.ietf.org/html/rfc7826#section-18.10)]
    (Blocksize, "blocksize", "Blocksize");

    /// Cache-Control
    /// [[RFC7826, Section 18.11](https://tools.ietf.org/html/rfc7826#section-18.11)]
    (CacheControl, "cache-control", "Cache-Control");

    /// Connection
    /// [[RFC7826, Section 18.12](https://tools.ietf.org/html/rfc7826#section-18.12)]
    (Connection, "connection", "Connection");

    /// Connection-Credentials
    /// [[RFC7826, Section 18.13](https://tools.ietf.org/html/rfc7826#section-18.13)]
    (ConnectionCredentials, "connection-credentials", "Connection-Credentials");

    /// Content-Base
    /// [[RFC7826, Section 18.14](https://tools.ietf.org/html/rfc7826#section-18.14)]
    (ContentBase, "content-base", "Content-Base");

    /// Content-Encoding
    /// [[RFC7826, Section 18.15](https://tools.ietf.org/html/rfc7826#section-18.15)]
    (ContentEncoding, "content-encoding", "Content-Encoding");

    /// Content-Language
    /// [[RFC7826, Section 18.16](https://tools.ietf.org/html/rfc7826#section-18.16)]
    (ContentLanguage, "content-language", "Content-Language");

    /// Content-Length
    /// [[RFC7826, Section 18.17](https://tools.ietf.org/html/rfc7826#section-18.17)]
    (ContentLength, "content-length", "Content-Length");

    /// Content-Location
    /// [[RFC7826, Section 18.18](https://tools.ietf.org/html/rfc7826#section-18.18)]
    (ContentLocation, "content-location", "Content-Location");

    /// Content-Type
    /// [[RFC7826, Section 18.19](https://tools.ietf.org/html/rfc7826#section-18.19)]
    (ContentType, "content-type", "Content-Type");

    /// CSeq
    /// [[RFC7826, Section 18.20](https://tools.ietf.org/html/rfc7826#section-18.20)]
    (CSeq, "cseq", "CSeq");

    /// Date
    /// [[RFC7826, Section 18.21](https://tools.ietf.org/html/rfc7826#section-18.21)]
    (Date, "date", "Date");

    /// Expires
    /// [[RFC7826, Section 18.22](https://tools.ietf.org/html/rfc7826#section-18.22)]
    (Expires, "expires", "Expires");

    /// Date
    /// [[RFC7826, Section 18.23](https://tools.ietf.org/html/rfc7826#section-18.23)]
    (From, "from", "From");

    /// If-Match
    /// [[RFC7826, Section 18.24](https://tools.ietf.org/html/rfc7826#section-18.24)]
    (IfMatch, "if-match", "If-Match");

    /// If-Modified-Since
    /// [[RFC7826, Section 18.25](https://tools.ietf.org/html/rfc7826#section-18.25)]
    (IfModifiedSince, "if-modified-since", "If-Modified-Since");

    /// If-None-Match
    /// [[RFC7826, Section 18.26](https://tools.ietf.org/html/rfc7826#section-18.26)]
    (IfNoneMatch, "if-none-match", "If-None-Match");

    /// Last-Modified
    /// [[RFC7826, Section 18.27](https://tools.ietf.org/html/rfc7826#section-18.27)]
    (LastModified, "last-modified", "Last-Modified");

    /// Location
    /// [[RFC7826, Section 18.28](https://tools.ietf.org/html/rfc7826#section-18.28)]
    (Location, "location", "Location");

    /// Media-Properties
    /// [[RFC7826, Section 18.29](https://tools.ietf.org/html/rfc7826#section-18.29)]
    (MediaProperties, "media-properties", "Media-Properties");

    /// Media-Range
    /// [[RFC7826, Section 18.30](https://tools.ietf.org/html/rfc7826#section-18.30)]
    (MediaRange, "media-range", "Media-Range");

    /// MTag
    /// [[RFC7826, Section 18.31](https://tools.ietf.org/html/rfc7826#section-18.31)]
    (MTag, "mtag", "MTag");

    /// Notify-Reason
    /// [[RFC7826, Section 18.32](https://tools.ietf.org/html/rfc7826#section-18.32)]
    (NotifyReason, "notify-reason", "Notify-Reason");

    /// Pipelined-Requests
    /// [[RFC7826, Section 18.33](https://tools.ietf.org/html/rfc7826#section-18.33)]
    (PipelinedRequests, "pipelined-requests", "Pipelined-Requests");

    /// Proxy-Authenticate
    /// [[RFC7826, Section 18.34](https://tools.ietf.org/html/rfc7826#section-18.34)]
    (ProxyAuthenticate, "proxy-authenticate", "Proxy-Authenticate");

    /// Proxy-Authentication-Info
    /// [[RFC7826, Section 18.35](https://tools.ietf.org/html/rfc7826#section-18.35)]
    (ProxyAuthenticationInfo, "proxy-authentication-info", "Proxy-Authentication-Info");

    /// Proxy-Authorization
    /// [[RFC7826, Section 18.36](https://tools.ietf.org/html/rfc7826#section-18.36)]
    (ProxyAuthorization, "proxy-authorization", "Proxy-Authorization");

    /// Proxy-Require
    /// [[RFC7826, Section 18.37](https://tools.ietf.org/html/rfc7826#section-18.37)]
    (ProxyRequire, "proxy-require", "Proxy-Require");

    /// Proxy-Supported
    /// [[RFC7826, Section 18.38](https://tools.ietf.org/html/rfc7826#section-18.38)]
    (ProxySupported, "proxy-supported", "Proxy-Supported");

    /// Public
    /// [[RFC7826, Section 18.39](https://tools.ietf.org/html/rfc7826#section-18.39)]
    (Public, "public", "Public");

    /// Range
    /// [[RFC7826, Section 18.40](https://tools.ietf.org/html/rfc7826#section-18.40)]
    (Range, "range", "Range");

    /// Referrer
    /// [[RFC7826, Section 18.41](https://tools.ietf.org/html/rfc7826#section-18.41)]
    (Referrer, "referrer", "Referrer");

    /// Request-Status
    /// [[RFC7826, Section 18.42](https://tools.ietf.org/html/rfc7826#section-18.42)]
    (RequestStatus, "request-status", "Request-Status");

    /// Require
    /// [[RFC7826, Section 18.43](https://tools.ietf.org/html/rfc7826#section-18.43)]
    (Require, "require", "Require");

    /// Retry-After
    /// [[RFC7826, Section 18.44](https://tools.ietf.org/html/rfc7826#section-18.44)]
    (RetryAfter, "retry-after", "Retry-After");

    /// RTP-Info
    /// [[RFC7826, Section 18.45](https://tools.ietf.org/html/rfc7826#section-18.45)]
    (RTPInfo, "rtp-info", "RTP-Info");

    /// Scale
    /// [[RFC7826, Section 18.46](https://tools.ietf.org/html/rfc7826#section-18.46)]
    (Scale, "scale", "Scale");

    /// Seek-Style
    /// [[RFC7826, Section 18.47](https://tools.ietf.org/html/rfc7826#section-18.47)]
    (SeekStyle, "seek-style", "Seek-Style");

    /// Server
    /// [[RFC7826, Section 18.48](https://tools.ietf.org/html/rfc7826#section-18.48)]
    (Server, "server", "Server");

    /// Session
    /// [[RFC7826, Section 18.49](https://tools.ietf.org/html/rfc7826#section-18.49)]
    (Session, "session", "Session");

    /// Speed
    /// [[RFC7826, Section 18.50](https://tools.ietf.org/html/rfc7826#section-18.50)]
    (Speed, "speed", "Speed");

    /// Supported
    /// [[RFC7826, Section 18.51](https://tools.ietf.org/html/rfc7826#section-18.51)]
    (Supported, "supported", "Supported");

    /// Terminate-Reason
    /// [[RFC7826, Section 18.52](https://tools.ietf.org/html/rfc7826#section-18.52)]
    (TerminateReason, "terminate-reason", "Terminate-Reason");

    /// Timestamp
    /// [[RFC7826, Section 18.53](https://tools.ietf.org/html/rfc7826#section-18.53)]
    (Timestamp, "timestamp", "Timestamp");

    /// Transport
    /// [[RFC7826, Section 18.54](https://tools.ietf.org/html/rfc7826#section-18.54)]
    (Transport, "transport", "Transport");

    /// Unsupported
    /// [[RFC7826, Section 18.55](https://tools.ietf.org/html/rfc7826#section-18.55)]
    (Unsupported, "unsupported", "Unsupported");

    /// User-Agent
    /// [[RFC7826, Section 18.56](https://tools.ietf.org/html/rfc7826#section-18.56)]
    (UserAgent, "user-agent", "User-Agent");

    /// Via
    /// [[RFC7826, Section 18.57](https://tools.ietf.org/html/rfc7826#section-18.57)]
    (Via, "via", "Via");

    /// WWW-Authenticate
    /// [[RFC7826, Section 18.58](https://tools.ietf.org/html/rfc7826#section-18.58)]
    (WWWAuthenticate, "www-authenticate", "WWW-Authenticate");
}
