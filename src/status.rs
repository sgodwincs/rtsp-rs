//! RTSP Status Code
//!
//! This module contains RTSP-status code related structs and errors. The main type in this module
//! is `StatusCode` which is not intended to be used through this module but rather the
//! `rtsp::StatusCode` enum type. Each variant on the `StatusCode` type represents either a specific
//! standardized status code or a custom status code.
//!
//! # Example
//!
//! ```
//! # #![feature(try_from)]
//! #
//! # use std::convert::TryFrom;
//! #
//! use rtsp::{StatusCode, StatusCodeClass};
//!
//! assert_eq!(StatusCode::try_from(200), Ok(StatusCode::OK));
//! assert_eq!(StatusCode::try_from("404"), Ok(StatusCode::NotFound));
//! assert_eq!(u16::from(StatusCode::NotFound), 404);
//! assert_eq!(StatusCode::OK.class(), StatusCodeClass::Success);
//! ```

use std::convert::TryFrom;
use std::error::Error;
use std::fmt;

use reason::ReasonPhrase;

macro_rules! status_codes {
    (
        $(
            $(#[$docs:meta])*
            ($code:expr, $variant:ident, $phrase:expr);
        )+
    ) => {
        /// An RTSP status code (as defined in RFC 7826).
        ///
        /// Although the status code is represented as a `u16`, only values between [100, 599]
        /// should be used since only these are defined as valid status codes with a status class by
        /// RTSP.
        ///
        /// # Examples
        ///
        /// ```
        /// # #![feature(try_from)]
        /// #
        /// # use std::convert::TryFrom;
        /// #
        /// use rtsp::{StatusCode, StatusCodeClass};
        ///
        /// assert_eq!(StatusCode::try_from(200).unwrap(), StatusCode::OK);
        /// assert_eq!(StatusCode::try_from("404"), Ok(StatusCode::NotFound));
        /// assert_eq!(u16::from(StatusCode::NotFound), 404);
        /// assert_eq!(StatusCode::OK.class(), StatusCodeClass::Success);
        /// ```
        ///
        #[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
        #[non_exhaustive]
        pub enum StatusCode {
        $(
            $(#[$docs])*
            $variant,
        )+

            /// A status code that is between [100, 599] but is not one of the standardized status
            /// codes.
            Extension(ExtensionStatusCode)
        }

        impl StatusCode {
            /// Get the standardised "reason-phrase" for this status code.
            ///
            /// This is mostly here for servers writing responses but could potentially have an
            /// application at other times.
            ///
            /// The reason phrase is defined as being exclusively for human readers. You should
            /// avoid deriving any meaning from it at all costs.
            ///
            /// # Examples
            ///
            /// ```
            /// use rtsp::StatusCode;
            ///
            /// assert_eq!(StatusCode::OK.canonical_reason().unwrap().as_str(), "OK");
            /// ```
            pub fn canonical_reason(&self) -> Option<ReasonPhrase> {
                use self::StatusCode::*;

                match *self {
                $(
                    $variant => Some(unsafe { ReasonPhrase::from_str_unchecked($phrase) }),
                )+
                    Extension(_) => None
                }
            }

            /// Constructs a new `StatusCode` from a given `u16`. It is assumed that the `u16` is
            /// in the allowed range.
            fn from_u16(status_code: u16) -> Self {
                use self::StatusCode::*;

                match status_code {
                $(
                    $code => $variant,
                )+
                    _ => Extension(ExtensionStatusCode(status_code))
                }
            }
        }

        impl From<StatusCode> for u16 {
            fn from(value: StatusCode) -> u16 {
                use self::StatusCode::*;

                match value {
                $(
                    $variant => $code,
                )+
                    Extension(code) => code.0
                }
            }
        }

        #[test]
        fn test_status_code_canonical_reason() {
        $(
            let status_code = StatusCode::$variant;
            assert_eq!(status_code.canonical_reason().unwrap().as_str(), $phrase);
        )+
        }

        #[test]
        fn test_from_status_code_to_u16() {
        $(
            let status_code = StatusCode::$variant;
            assert_eq!(u16::from(status_code), $code);
        )+

            let status_code = StatusCode::Extension(ExtensionStatusCode(599));
            assert_eq!(u16::from(status_code), 599);
        }
    }
}

/// A wrapper type used to avoid users creating extension status codes that are actually
/// standardized status codes.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ExtensionStatusCode(u16);

impl From<ExtensionStatusCode> for u16 {
    fn from(value: ExtensionStatusCode) -> u16 {
        value.0
    }
}

impl StatusCode {
    /// Returns the class that this status code falls under.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::{StatusCode, StatusCodeClass};
    ///
    /// assert_eq!(StatusCode::BadRequest.class(), StatusCodeClass::ClientError);
    /// ```
    pub fn class(&self) -> StatusCodeClass {
        use self::StatusCodeClass::*;

        match u16::from(*self) {
            100...199 => Informational,
            200...299 => Success,
            300...399 => Redirection,
            400...499 => ClientError,
            500...599 => ServerError,
            _ => panic!("status code with invalid class"),
        }
    }

    /// Returns whether the status code is of the informational class (between [100, 199]).
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::StatusCode;
    ///
    /// assert!(StatusCode::Continue.is_informational());
    /// assert!(!StatusCode::OK.is_informational());
    /// ```
    pub fn is_informational(&self) -> bool {
        self.class() == StatusCodeClass::Informational
    }

    /// Returns whether the status code is of the success class (between [200, 299]).
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::StatusCode;
    ///
    /// assert!(StatusCode::OK.is_success());
    /// assert!(!StatusCode::NotFound.is_success());
    /// ```
    pub fn is_success(&self) -> bool {
        self.class() == StatusCodeClass::Success
    }

    /// Returns whether the status code is of the redirection class (between [300, 399]).
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::StatusCode;
    ///
    /// assert!(StatusCode::Found.is_redirection());
    /// assert!(!StatusCode::BadRequest.is_redirection());
    /// ```
    pub fn is_redirection(&self) -> bool {
        self.class() == StatusCodeClass::Redirection
    }

    /// Returns whether the status code is of the client error class (between [400, 499]).
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::StatusCode;
    ///
    /// assert!(StatusCode::NotFound.is_client_error());
    /// assert!(!StatusCode::InternalServerError.is_client_error());
    /// ```
    pub fn is_client_error(&self) -> bool {
        self.class() == StatusCodeClass::ClientError
    }

    /// Returns whether the status code is of the server error class (between [100, 199]).
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::StatusCode;
    ///
    /// assert!(StatusCode::InternalServerError.is_server_error());
    /// assert!(!StatusCode::OK.is_server_error());
    /// ```
    pub fn is_server_error(&self) -> bool {
        self.class() == StatusCodeClass::ServerError
    }
}

impl fmt::Debug for StatusCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", u16::from(*self))
    }
}

impl fmt::Display for StatusCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", u16::from(*self))
    }
}

impl Default for StatusCode {
    fn default() -> Self {
        StatusCode::OK
    }
}

/// Provides a fallible conversion from a byte slice to a `StatusCode`. Note that you cannot do the
/// following:
///
/// ```compile_fail
/// let ok = StatusCode::try_from(b"200").unwrap();
/// ```
///
/// This is because `b"200"` is of type `&[u8; 3]` and so it must be converted to `&[u8]` in order
/// to perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be
/// provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for StatusCode {
    type Error = InvalidStatusCode;

    /// Converts a `&[u8]` to a status code. The size of the byte slice must be exactly 3, and the
    /// resulting `u16` must be greater than or equal to 100 but less than 600.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::StatusCode;
    ///
    /// let ok = StatusCode::try_from(&b"200"[..]).unwrap();
    /// assert_eq!(ok, StatusCode::OK);
    ///
    /// let error = StatusCode::try_from(&b"099"[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        if value.len() != 3 {
            return Err(InvalidStatusCode);
        }

        let a = value[0].wrapping_sub(b'0') as u16;
        let b = value[1].wrapping_sub(b'0') as u16;
        let c = value[2].wrapping_sub(b'0') as u16;

        if a == 0 || a > 5 || b > 9 || c > 9 {
            return Err(InvalidStatusCode);
        }

        let status_code = (a * 100) + (b * 10) + c;
        Ok(StatusCode::from_u16(status_code))
    }
}

impl<'a> TryFrom<&'a str> for StatusCode {
    type Error = InvalidStatusCode;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        StatusCode::try_from(value.as_bytes())
    }
}

impl TryFrom<u16> for StatusCode {
    type Error = InvalidStatusCode;

    /// Converts a u16 to a status code.
    ///
    /// The function validates the correctness of the supplied `u16`. It must be greater than or
    /// equal to 100 but less than 600.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::StatusCode;
    ///
    /// let ok = StatusCode::try_from(200).unwrap();
    /// assert_eq!(ok, StatusCode::OK);
    ///
    /// let error = StatusCode::try_from(99);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: u16) -> Result<Self, Self::Error> {
        if value < 100 || value >= 600 {
            return Err(InvalidStatusCode);
        }

        Ok(StatusCode::from_u16(value))
    }
}

/// Represents the class that a status code falls under.
/// [[RFC7826, Section 17]](https://tools.ietf.org/html/rfc7826#section-17)
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum StatusCodeClass {
    /// A status code between [100, 199].
    /// [[RFC7826, Section 17.1]](https://tools.ietf.org/html/rfc7826#section-17.1)
    Informational,

    /// A status code between [200, 299].
    /// [[RFC7826, Section 17.2]](https://tools.ietf.org/html/rfc7826#section-17.2)
    Success,

    /// A status code between [300, 399].
    /// [[RFC7826, Section 17.3]](https://tools.ietf.org/html/rfc7826#section-17.3)
    Redirection,

    /// A status code between [400, 499].
    /// [[RFC7826, Section 17.4]](https://tools.ietf.org/html/rfc7826#section-17.4)
    ClientError,

    /// A status code between [500, 599].
    /// [[RFC7826, Section 17.5]](https://tools.ietf.org/html/rfc7826#section-17.5)
    ServerError,
}

/// A generic error indicating that the status code was invalid.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidStatusCode;

impl fmt::Display for InvalidStatusCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Error::description(self))
    }
}

impl Error for InvalidStatusCode {
    fn description(&self) -> &str {
        "invalid RTSP status code"
    }
}

status_codes! {
    /// 100 Continue
    /// [[RFC7826, Section 17.1.1](https://tools.ietf.org/html/rfc7826#section-17.1.1)]
    (100, Continue, "Continue");

    /// 200 OK
    /// [[RFC7826, Section 17.2.1](https://tools.ietf.org/html/rfc7826#section-17.2.1)]
    (200, OK, "OK");

    /// 301 Moved Permanently
    /// [[RFC7826, Section 17.3.2](https://tools.ietf.org/html/rfc7826#section-17.3.2)]
    (301, MovedPermanently, "Moved Permanently");

    /// 302 Found
    /// [[RFC7826, Section 17.3.3](https://tools.ietf.org/html/rfc7826#section-17.3.3)]
    (302, Found, "Found");

    /// 303 See Other
    /// [[RFC7826, Section 17.3.4](https://tools.ietf.org/html/rfc7826#section-17.3.4)]
    (303, SeeOther, "See Other");

    /// 304 Not Modified
    /// [[RFC7826, Section 17.3.5](https://tools.ietf.org/html/rfc7826#section-17.3.5)]
    (304, NotModified, "Not Modified");

    /// 305 Use Proxy
    /// [[RFC7826, Section 17.3.6](https://tools.ietf.org/html/rfc7826#section-17.3.6)]
    (305, UseProxy, "Use Proxy");

    /// 400 Bad Request
    /// [[RFC7826, Section 17.4.1](https://tools.ietf.org/html/rfc7826#section-17.4.1)]
    (400, BadRequest, "Bad Request");

    /// 401 Unauthorized
    /// [[RFC7826, Section 17.4.2](https://tools.ietf.org/html/rfc7826#section-17.4.2)]
    (401, Unauthorized, "Unauthorized");

    /// 402 Payment Required
    /// [[RFC7826, Section 17.4.3](https://tools.ietf.org/html/rfc7826#section-17.4.3)]
    (402, PaymentRequired, "Payment Required");

    /// 403 Forbidden
    /// [[RFC7826, Section 17.4.4](https://tools.ietf.org/html/rfc7826#section-17.4.4)]
    (403, Forbidden, "Forbidden");

    /// 404 Not Fund
    /// [[RFC7826, Section 17.4.5](https://tools.ietf.org/html/rfc7826#section-17.4.5)]
    (404, NotFound, "Not Found");

    /// 405 Method Not Allowed
    /// [[RFC7826, Section 17.4.6](https://tools.ietf.org/html/rfc7826#section-17.4.6)]
    (405, MethodNotAllowed, "Method Not Allowed");

    /// 406 Not Acceptable
    /// [[RFC7826, Section 17.4.7](https://tools.ietf.org/html/rfc7826#section-17.4.7)]
    (406, NotAcceptable, "Not Acceptable");

    /// 407 Proxy Authentication Required
    /// [[RFC7826, Section 17.4.8](https://tools.ietf.org/html/rfc7826#section-17.4.8)]
    (407, ProxyAuthenticationRequired, "Proxy Authentication Required");

    /// 408 Request Timeout
    /// [[RFC7826, Section 17.4.9](https://tools.ietf.org/html/rfc7826#section-17.4.9)]
    (408, RequestTimeout, "Request Timeout");

    /// 410 Gone
    /// [[RFC7826, Section 17.4.10](https://tools.ietf.org/html/rfc7826#section-17.4.10)]
    (410, Gone, "Gone");

    /// 412 Precondition Failed
    /// [[RFC7826, Section 17.4.11](https://tools.ietf.org/html/rfc7826#section-17.4.11)]
    (412, PreconditionFailed, "Precondition Failed");

    /// 413 Request Message Body Too Large
    /// [[RFC7826, Section 17.4.12](https://tools.ietf.org/html/rfc7826#section-17.4.12)]
    (413, RequestMessageBodyTooLarge, "Request Message Body Too Large");

    /// 414 Request-URI Too Long
    /// [[RFC7826, Section 17.4.13](https://tools.ietf.org/html/rfc7826#section-17.4.13)]
    (414, RequestURITooLong, "Request-URI Too Long");

    /// 415 Unsupported Media Type
    /// [[RFC7826, Section 17.4.14](https://tools.ietf.org/html/rfc7826#section-17.4.14)]
    (415, UnsupportedMediaType, "Unsupported Media Type");

    /// 451 Parameter Not Understood
    /// [[RFC7826, Section 17.4.15](https://tools.ietf.org/html/rfc7826#section-17.4.15)]
    (451, ParameterNotUnderstood, "Parameter Not Understood");

    // RFC 7826 mentions status code "452 Illegal Conference Identifier" which was only allowed in
    // RTSP 1.0. Should it be added here just for completeness? Though, it is listed as "reserved"
    // in the status code list.

    // /// 452 Illegal Conference Identifier
    // (452, IllegalConferenceIdentifier, "Illegal Conference Identifier");
    // /// [[RFC7826, Section 17.4.16](https://tools.ietf.org/html/rfc7826#section-17.4.16)]

    /// 453 Not Enough Bandwidth
    /// [[RFC7826, Section 17.4.17](https://tools.ietf.org/html/rfc7826#section-17.4.17)]
    (453, NotEnoughBandwidth, "Not Enough Bandwidth");

    /// 454 Session Not Found
    /// [[RFC7826, Section 17.4.18](https://tools.ietf.org/html/rfc7826#section-17.4.18)]
    (454, SessionNotFound, "Session Not Found");

    /// 455 Method Not Valid in This State
    /// [[RFC7826, Section 17.4.19](https://tools.ietf.org/html/rfc7826#section-17.4.19)]
    (455, MethodNotValidInThisState, "Method Not Valid in This State");

    /// 456 Header Field Not Valid for Resource
    /// [[RFC7826, Section 17.4.20](https://tools.ietf.org/html/rfc7826#section-17.4.20)]
    (456, HeaderFieldNotValidForResource, "Header Field Not Valid for Resource");

    /// 457 Invalid Range
    /// [[RFC7826, Section 17.4.21](https://tools.ietf.org/html/rfc7826#section-17.4.21)]
    (457, InvalidRange, "Invalid Range");

    /// 458 Parameter Is Read-Only
    /// [[RFC7826, Section 17.4.22](https://tools.ietf.org/html/rfc7826#section-17.4.22)]
    (458, ParameterIsReadOnly, "Parameter Is Read-Only");

    /// 459 Aggregate Operation Not Allowed
    /// [[RFC7826, Section 17.4.23](https://tools.ietf.org/html/rfc7826#section-17.4.23)]
    (459, AggregateOperationNotAllowed, "Aggregate Operation Not Allowed");

    /// 460 Only Aggregate Operation Allowed
    /// [[RFC7826, Section 17.4.24](https://tools.ietf.org/html/rfc7826#section-17.4.24)]
    (460, OnlyAggregateOperationAllowed, "Only Aggregate Operation Allowed");

    /// 461 Unsupported Transport
    /// [[RFC7826, Section 17.4.25](https://tools.ietf.org/html/rfc7826#section-17.4.25)]
    (461, UnsupportedTransport, "Unsupported Transport");

    /// 462 Destination Unreachable
    /// [[RFC7826, Section 17.4.26](https://tools.ietf.org/html/rfc7826#section-17.4.26)]
    (462, DestinationUnreachable, "Destination Unreachable");

    /// 463 Destination Prohibited
    /// [[RFC7826, Section 17.4.27](https://tools.ietf.org/html/rfc7826#section-17.4.27)]
    (463, DestinationProhibited, "Destination Prohibited");

    /// 464 Data Transport Not Ready Yet
    /// [[RFC7826, Section 17.4.28](https://tools.ietf.org/html/rfc7826#section-17.4.28)]
    (464, DataTransportNotReadyYet, "Data Transport Not Ready Yet");

    /// 465 Notification Reason Unknown
    /// [[RFC7826, Section 17.4.29](https://tools.ietf.org/html/rfc7826#section-17.4.29)]
    (465, NotificationReasonUnknown, "Notification Reason Unknown");

    /// 466 Key Management Error
    /// [[RFC7826, Section 17.4.30](https://tools.ietf.org/html/rfc7826#section-17.4.30)]
    (466, KeyManagementError, "Key Management Error");

    /// 470 Connection Authorization Required
    /// [[RFC7826, Section 17.4.31](https://tools.ietf.org/html/rfc7826#section-17.4.31)]
    (470, ConnectionAuthorizationRequired, "Connection Authorization Required");

    /// 471 Connection Credentials Not Accepted
    /// [[RFC7826, Section 17.4.32](https://tools.ietf.org/html/rfc7826#section-17.4.32)]
    (471, ConnectionCredentialsNotAccepted, "Connection Credentials Not Accepted");

    /// 472 Failure to Establish Secure Connection
    /// [[RFC7826, Section 17.4.33](https://tools.ietf.org/html/rfc7826#section-17.4.33)]
    (472, FailureToEstablishSecureConnection, "Failure to Establish Secure Connection");

    /// 500 Internal Server Error
    /// [[RFC7826, Section 17.5.1](https://tools.ietf.org/html/rfc7826#section-17.5.1)]
    (500, InternalServerError, "Internal Server Error");

    /// 501 Not Implemented
    /// [[RFC7826, Section 17.5.2](https://tools.ietf.org/html/rfc7826#section-17.5.2)]
    (501, NotImplemented, "Not Implemented");

    /// 502 Bad Gateway
    /// [[RFC7826, Section 17.5.3](https://tools.ietf.org/html/rfc7826#section-17.5.3)]
    (502, BadGateway, "Bad Gateway");

    /// 503 Service Unavailable
    /// [[RFC7826, Section 17.5.4](https://tools.ietf.org/html/rfc7826#section-17.5.4)]
    (503, ServiceUnavailable, "Service Unavailable");

    /// 504 Gateway Timeout
    /// [[RFC7826, Section 17.5.5](https://tools.ietf.org/html/rfc7826#section-17.5.5)]
    (504, GatewayTimeout, "Gateway Timeout");

    /// 505 RTSP Version Not Supported
    /// [[RFC7826, Section 17.5.6](https://tools.ietf.org/html/rfc7826#section-17.5.6)]
    (505, RTSPVersionNotSupported, "RTSP Version Not Supported");

    /// 551 Option Not Supported
    /// [[RFC7826, Section 17.5.7](https://tools.ietf.org/html/rfc7826#section-17.5.7)]
    (551, OptionNotSupported, "Option Not Supported");

    /// 553 Proxy Unavailable
    /// [[RFC7826, Section 17.5.8](https://tools.ietf.org/html/rfc7826#section-17.5.8)]
    (553, ProxyUnavailable, "Proxy Unavailable");
}
