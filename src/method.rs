//! RTSP Method
//!
//! This module contains RTSP-method related structs, errors, and such. The main type of this
//! module, `Method`, is also re-exported at the root of the crate as `rtsp::Method` and is
//! primarily intended to be imported through that location.
//!
//! # Examples
//!
//! ```
//! # #![feature(try_from)]
//! #
//! # use std::convert::TryFrom;
//! #
//! use rtsp::Method;
//!
//! assert_eq!(Method::Play, Method::try_from("PLAY").unwrap());
//! assert_eq!(Method::Describe.as_str(), "DESCRIBE");
//! ```

use ascii::AsciiString;
use std::convert::{AsRef, TryFrom};
use std::error::Error;
use std::fmt;

use syntax::is_token;

/// An RTSP request method.
///
/// Each variant (excluding `Extension`) represents a standardized RTSP method.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::Method;
///
/// assert_eq!(Method::Play, Method::try_from("PLAY").unwrap());
/// assert_eq!(Method::Describe.as_str(), "DESCRIBE");
/// ```
#[derive(Clone, Eq, Hash, PartialEq)]
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

/// A wrapper type used to avoid users creating extension methods that are actually standardized
/// methods.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct ExtensionMethod(AsciiString);

impl fmt::Debug for ExtensionMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for ExtensionMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Performs equality checking of a `ExtensionMethod` with a `str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::Method;
///
/// match Method::try_from("extension").unwrap() {
///     Method::Extension(extension) => assert_eq!(extension, *"eXtEnSiOn"),
///     _ => panic!("expected extension method")
/// }
/// ```
impl PartialEq<str> for ExtensionMethod {
    fn eq(&self, other: &str) -> bool {
        self.0 == other.to_ascii_uppercase()
    }
}

/// Performs equality checking of a `ExtensionMethod` with a `&str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::Method;
///
/// match Method::try_from("extension").unwrap() {
///     Method::Extension(extension) => assert_eq!(extension, "eXtEnSiOn"),
///     _ => panic!("expected extension method")
/// }
/// ```
impl<'a> PartialEq<&'a str> for ExtensionMethod {
    fn eq(&self, other: &&'a str) -> bool {
        self.0 == (*other).to_ascii_uppercase()
    }
}

impl ExtensionMethod {
    /// Returns a `&str` representation of the extension method. The returned string is uppercase
    /// even if the extension method originally was a non-uppercase method.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::Method;
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

impl Method {
    /// Returns a `&str` representation of the RTSP method. The returned string is uppercase even
    /// if the method originally was a non-uppercase extension method.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::Method;
    ///
    /// assert_eq!(Method::Play.as_str(), "PLAY");
    /// assert_eq!(Method::try_from("extension").unwrap().as_str(), "EXTENSION");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Method::*;

        match *self {
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
            Extension(ref name) => name.as_str(),
        }
    }

    /// A helper function that creates a new `Method` instance with the given method name extension.
    /// It first checks to see if the method name is valid, and if not, it will return an error.
    fn extension(name: &[u8]) -> Result<Method, InvalidMethod> {
        if !Method::is_valid_method_name(name) {
            return Err(InvalidMethod);
        }

        let name = unsafe { AsciiString::from_ascii_unchecked(name) };
        Ok(Method::Extension(ExtensionMethod(name)))
    }

    /// Returns whether the given method name is valid. Based on
    /// [[RFC7826, Section 20.1](https://tools.ietf.org/html/rfc7826#section-20.1)], a method name
    /// follows the following rules:
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
    /// start with `$`.
    fn is_valid_method_name(name: &[u8]) -> bool {
        if name.is_empty() || name[0] == b'$' {
            return false;
        }

        is_token(name)
    }
}

impl AsRef<str> for Method {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

/// Performs equality checking of a `Method` with a `str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::Method;
///
/// assert_eq!(Method::try_from("eXtEnSiOn").unwrap(), *"exTENSION");
/// ```
impl PartialEq<str> for Method {
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other.to_ascii_uppercase()
    }
}

/// Performs equality checking of a `Method` with a `&str`. This check is case insensitive.
///
/// # Examples
///
/// ```
/// # #![feature(try_from)]
/// #
/// # use std::convert::TryFrom;
/// #
/// use rtsp::Method;
///
/// assert_eq!(Method::try_from("extension").unwrap(), "extension");
/// ```
impl<'a> PartialEq<&'a str> for Method {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_ref() == (*other).to_ascii_uppercase()
    }
}

impl fmt::Debug for Method {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl fmt::Display for Method {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

/// Provides a fallible conversion from a byte slice to a `Method`. Note that you cannot do the
/// following:
///
/// ```compile_fail
/// let play = Method::try_from(b"PLAY").unwrap();
/// ```
///
/// This is because `b"PLAY"` is of type `&[u8; 4]` and so it must be converted to `&[u8]` in order
/// to perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be
/// provided once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for Method {
    type Error = InvalidMethod;

    /// Converts a `&[u8]` to an RTSP method. The method name must not start with `$` and must
    /// contain only valid token characters. Since valid token characters includes only a subset of
    /// the ASCII-US character set, care should be taken when converting a UTF-8 encoded string to a
    /// method name.
    ///
    /// The conversion is case insensitive, but the method name is converted to uppercase.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::Method;
    ///
    /// let play = Method::try_from(&b"PLAY"[..]).unwrap();
    /// assert_eq!(play, Method::Play);
    ///
    /// let describe = Method::try_from(&b"describe"[..]).unwrap();
    /// assert_eq!(describe, Method::Describe);
    ///
    /// let extension = Method::try_from(&b"Ext"[..]).unwrap();
    /// assert_eq!(extension.as_str(), "EXT");
    ///
    /// let error = Method::try_from(&b"$Ext"[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        use self::Method::*;

        let value = value.to_ascii_uppercase();

        match value.len() {
            4 => match value.as_slice() {
                b"PLAY" => Ok(Play),
                _ => Method::extension(value.as_slice()),
            },
            5 => match value.as_slice() {
                b"PAUSE" => Ok(Pause),
                b"SETUP" => Ok(Setup),
                _ => Method::extension(value.as_slice()),
            },
            7 => match value.as_slice() {
                b"OPTIONS" => Ok(Options),
                _ => Method::extension(value.as_slice()),
            },
            8 => match value.as_slice() {
                b"DESCRIBE" => Ok(Describe),
                b"REDIRECT" => Ok(Redirect),
                b"TEARDOWN" => Ok(Teardown),
                _ => Method::extension(value.as_slice()),
            },
            11 => match value.as_slice() {
                b"PLAY_NOTIFY" => Ok(PlayNotify),
                _ => Method::extension(value.as_slice()),
            },
            13 => match value.as_slice() {
                b"GET_PARAMETER" => Ok(GetParameter),
                b"SET_PARAMETER" => Ok(SetParameter),
                _ => Method::extension(value.as_slice()),
            },
            _ => Method::extension(value.as_slice()),
        }
    }
}

impl<'a> TryFrom<&'a str> for Method {
    type Error = InvalidMethod;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Method::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a `Method` from a `&[u8]` or `&str`.
///
/// This error indicates that the method name was of size 0, started with `$`, or contained invalid
/// token characters.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidMethod;

impl fmt::Display for InvalidMethod {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for InvalidMethod {
    fn description(&self) -> &str {
        "invalid RTSP method"
    }
}
