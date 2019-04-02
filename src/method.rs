//! Method
//!
//! This module contains RTSP-method related structs, errors, and such. Each variant on the
//! [`Method`] type represents either a specific standardized method or a custom method.
//!
//! # Examples
//!
//! ```
//! use std::convert::TryFrom;
//!
//! use rtsp::method::Method;
//!
//! assert_eq!(Method::Play, Method::try_from("PLAY").unwrap());
//! assert_eq!(Method::Describe.as_str(), "DESCRIBE");
//! ```

use std::convert::{AsRef, Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::Deref;
use std::str;

use crate::syntax;

/// An RTSP request method (as defined in
/// [[RFC7826, Section 13]](https://tools.ietf.org/html/rfc7826#section-13)).
///
/// Each variant (excluding [`Method::Extension`]) represents a standardized RTSP method.
///
/// # Examples
///
/// ```
/// use std::convert::TryFrom;
///
/// use rtsp::method::Method;
///
/// assert_eq!(Method::Play, Method::try_from("PLAY").unwrap());
/// assert_eq!(Method::Describe.as_str(), "DESCRIBE");
/// ```
#[derive(Clone, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Method {
    /// DESCRIBE
    /// [[RFC7826, Section 13.2](https://tools.ietf.org/html/rfc7826#section-13.2)]
    Describe,

    /// GET_PARAMETER
    /// [[RFC7826, Section 13.8](https://tools.ietf.org/html/rfc7826#section-13.8)]
    GetParameter,

    /// OPTIONS
    /// [[RFC7826, Section 13.1](https://tools.ietf.org/html/rfc7826#section-13.1)]
    Options,

    /// PAUSE
    /// [[RFC7826, Section 13.6](https://tools.ietf.org/html/rfc7826#section-13.6)]
    Pause,

    /// PLAY
    /// [[RFC7826, Section 13.4](https://tools.ietf.org/html/rfc7826#section-13.4)]
    Play,

    /// PLAY_NOTIFY
    /// [[RFC7826, Section 13.5](https://tools.ietf.org/html/rfc7826#section-13.5)]
    PlayNotify,

    /// REDIRECT
    /// [[RFC7826, Section 13.10](https://tools.ietf.org/html/rfc7826#section-13.10)]
    Redirect,

    /// SET_PARAMETER
    /// [[RFC7826, Section 13.9](https://tools.ietf.org/html/rfc7826#section-13.9)]
    SetParameter,

    /// SETUP
    /// [[RFC7826, Section 13.3](https://tools.ietf.org/html/rfc7826#section-13.3)]
    Setup,

    /// TEARDOWN
    /// [[RFC7826, Section 13.7](https://tools.ietf.org/html/rfc7826#section-13.7)]
    Teardown,

    /// An extension method that is not one of the standardized methods. This is encoded using
    /// ASCII-US and is always uppercase.
    Extension(ExtensionMethod),
}

impl Method {
    /// Returns a `&str` representation of the RTSP method.
    ///
    /// The returned string is uppercase even if the method originally was a non-uppercase extension
    /// method.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    ///
    /// assert_eq!(Method::Play.as_str(), "PLAY");
    /// assert_eq!(Method::try_from("extension").unwrap().as_str(), "EXTENSION");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Method::*;

        match self {
            Describe => "DESCRIBE",
            GetParameter => "GET_PARAMETER",
            Options => "OPTIONS",
            Pause => "PAUSE",
            Play => "PLAY",
            PlayNotify => "PLAY_NOTIFY",
            Redirect => "REDIRECT",
            SetParameter => "SET_PARAMETER",
            Setup => "SETUP",
            Teardown => "TEARDOWN",
            Extension(name) => name.as_str(),
        }
    }

    /// A helper function that creates a new [`Method`] instance with the given method name
    /// extension.
    ///
    /// It first checks to see if the method name is valid, and if not, it will return an error.
    ///
    /// Based on, [[RFC7826, Section 20.1](https://tools.ietf.org/html/rfc7826#section-20.1)], a
    /// method name follows the following rules:
    ///
    /// ```text
    /// token = 1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
    ///       /  %x41-5A / %x5E-7A / %x7C / %x7E)
    ///          ; 1*<any CHAR except CTLs or tspecials>
    /// Method = "DESCRIBE"
    ///        / "GET_PARAMETER"
    ///        / "OPTIONS"
    ///        / "PAUSE"
    ///        / "PLAY"
    ///        / "PLAY_NOTIFY"
    ///        / "REDIRECT"
    ///        / "SETUP"
    ///        / "SET_PARAMETER"
    ///        / "TEARDOWN"
    ///        / extension-method
    /// extension-method = token
    /// ```
    ///
    /// There is an exception not covered from the above ABNF, specifically, method names cannot
    /// start with `'$'` as this is an indicator of RTP packet interleaving.
    fn extension(value: &[u8]) -> Result<Method, MethodError> {
        if value.is_empty() {
            return Err(MethodError::Empty);
        }

        if value[0] == b'$' {
            return Err(MethodError::StartsWithDollarSign);
        }

        if !syntax::is_token(value) {
            return Err(MethodError::InvalidCharacter);
        }

        // Unsafe: The function above [`syntax::is_token`] ensures that the value is valid ASCII-US.
        let value = unsafe { str::from_utf8_unchecked(value) }.to_ascii_uppercase();
        Ok(Method::Extension(ExtensionMethod(value)))
    }
}

impl AsRef<[u8]> for Method {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for Method {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Debug for Method {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl Display for Method {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<Method> for String {
    fn from(value: Method) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for Method {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Method> for [u8] {
    fn eq(&self, other: &Method) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'method> PartialEq<&'method [u8]> for Method {
    fn eq(&self, other: &&'method [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'method> PartialEq<Method> for &'method [u8] {
    fn eq(&self, other: &Method) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Method {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Method> for str {
    fn eq(&self, other: &Method) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'method> PartialEq<&'method str> for Method {
    fn eq(&self, other: &&'method str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'method> PartialEq<Method> for &'method str {
    fn eq(&self, other: &Method) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'method> TryFrom<&'method [u8]> for Method {
    type Error = MethodError;

    fn try_from(value: &'method [u8]) -> Result<Self, Self::Error> {
        use self::Method::*;

        macro_rules! check_method {
            ($method:ident) => {
                let bytes = $method.as_str().as_bytes();
                let starts_with = value
                    .iter()
                    .take(bytes.len())
                    .map(u8::to_ascii_uppercase)
                    .eq(bytes.iter().cloned());

                if starts_with && value.len() == bytes.len() {
                    return Ok($method);
                }
            };
        }

        match value.len() {
            4 => {
                check_method!(Play);
                Method::extension(value)
            }
            5 => {
                check_method!(Pause);
                check_method!(Setup);
                Method::extension(value)
            }
            7 => {
                check_method!(Options);
                Method::extension(value)
            }
            8 => {
                check_method!(Describe);
                check_method!(Redirect);
                check_method!(Teardown);
                Method::extension(value)
            }
            11 => {
                check_method!(PlayNotify);
                Method::extension(value)
            }
            13 => {
                check_method!(GetParameter);
                check_method!(SetParameter);
                Method::extension(value)
            }
            _ => Method::extension(value),
        }
    }
}

impl<'method> TryFrom<&'method str> for Method {
    type Error = MethodError;

    fn try_from(value: &'method str) -> Result<Self, Self::Error> {
        Method::try_from(value.as_bytes())
    }
}

/// A wrapper type used to avoid users creating extension methods that are actually standardized
/// methods.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExtensionMethod(String);

impl AsRef<[u8]> for ExtensionMethod {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl AsRef<str> for ExtensionMethod {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for ExtensionMethod {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ExtensionMethod {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<ExtensionMethod> for String {
    fn from(value: ExtensionMethod) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for ExtensionMethod {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionMethod> for [u8] {
    fn eq(&self, other: &ExtensionMethod) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'method> PartialEq<&'method [u8]> for ExtensionMethod {
    fn eq(&self, other: &&'method [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'method> PartialEq<ExtensionMethod> for &'method [u8] {
    fn eq(&self, other: &ExtensionMethod) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for ExtensionMethod {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionMethod> for str {
    fn eq(&self, other: &ExtensionMethod) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'method> PartialEq<&'method str> for ExtensionMethod {
    fn eq(&self, other: &&'method str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'method> PartialEq<ExtensionMethod> for &'method str {
    fn eq(&self, other: &ExtensionMethod) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl ExtensionMethod {
    /// Returns a `&str` representation of the extension method.
    ///
    /// The returned string is uppercase even if the extension method originally was a non-uppercase
    /// method.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    ///
    /// match Method::try_from("extension").unwrap() {
    ///     Method::Extension(extension) => assert_eq!(extension.as_str(), "EXTENSION"),
    ///     _ => panic!("expected extension method")
    /// }
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// A possible error value when converting to a [`Method`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum MethodError {
    // The method was empty.
    Empty,

    // The method contained an invalid character.
    InvalidCharacter,

    // The method started with `'$'`. This is not allowed because interleaved RTP messages through
    // RTSP are indicated by this.
    StartsWithDollarSign,
}

impl Display for MethodError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::MethodError::*;

        match self {
            Empty => write!(formatter, "empty method"),
            InvalidCharacter => write!(formatter, "invalid method character"),
            StartsWithDollarSign => write!(formatter, "method starts with '$'"),
        }
    }
}

impl Error for MethodError {}

impl From<Infallible> for MethodError {
    fn from(_: Infallible) -> Self {
        MethodError::Empty
    }
}
