use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ops::Deref;
use std::str;

use crate::syntax;

/// The mode parameter indicates the methods to be supported for this session.
///
/// The currently defined valid value is `"PLAY". If not provided, the default is `"PLAY"`.  The
/// `"RECORD"` value was defined in [[RFC2326](https://tools.ietf.org/html/rfc2326)]; in this
/// specification, it is unspecified but reserved. `"RECORD"` and other values may be specified in
/// the future.
pub enum Mode {
    /// An extension mode that is not one of the standardized modes. This is encoded using ASCII-US
    /// and is always uppercase.
    Extension(ExtensionMode),

    /// PLAY
    /// [[RFC7826, Section 13.4](https://tools.ietf.org/html/rfc7826#section-13.4)]
    Play,
}

impl Mode {
    /// Returns a `&str` representation of the mode parameter.
    ///
    /// The returned string is uppercase even if the extension mode originally was a non-uppercase
    /// mode.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::Mode;
    ///
    /// assert_eq!(Mode::Play.as_str(), "PLAY");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::Mode::*;

        match self {
            Play => "PLAY",
            Extension(extension) => extension.as_str(),
        }
    }

    /// A helper function that creates a new [`Mode`] instance with the given mode name extension.
    ///
    /// It first checks to see if the mode name is valid, and if not, it will return an error.
    ///
    /// Based on, [[RFC7826, Section 20.1](https://tools.ietf.org/html/rfc7826#section-20.1)], a
    /// mode name follows the following rules:
    ///
    /// ```text
    /// token = 1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
    ///       / %x41-5A / %x5E-7A / %x7C / %x7E)
    ///         ; 1*<any CHAR except CTLs or tspecials>
    /// mode = "PLAY" / token
    /// ```
    fn extension(value: &[u8]) -> Result<Self, ModeError> {
        if value.is_empty() {
            return Err(ModeError::Empty);
        }

        if value.eq_ignore_ascii_case(b"RECORD") {
            return Err(ModeError::Reserved);
        }

        if !syntax::is_token(value) {
            return Err(ModeError::InvalidCharacter);
        }

        // Unsafe: The function above [`syntax::is_token`] ensures that the value is valid ASCII-US.
        let value = unsafe { str::from_utf8_unchecked(value) }.to_ascii_uppercase();
        Ok(Mode::Extension(ExtensionMode(value)))
    }
}

impl AsRef<[u8]> for Mode {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for Mode {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Default for Mode {
    fn default() -> Self {
        Mode::Play
    }
}

impl Display for Mode {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<Mode> for String {
    fn from(value: Mode) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for Mode {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Mode> for [u8] {
    fn eq(&self, other: &Mode) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'mode> PartialEq<&'mode [u8]> for Mode {
    fn eq(&self, other: &&'mode [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'mode> PartialEq<Mode> for &'mode [u8] {
    fn eq(&self, other: &Mode) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Mode {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<Mode> for str {
    fn eq(&self, other: &Mode) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'mode> PartialEq<&'mode str> for Mode {
    fn eq(&self, other: &&'mode str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'mode> PartialEq<Mode> for &'mode str {
    fn eq(&self, other: &Mode) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'mode> TryFrom<&'mode [u8]> for Mode {
    type Error = ModeError;

    fn try_from(value: &'mode [u8]) -> Result<Self, Self::Error> {
        if value.eq_ignore_ascii_case(b"PLAY") {
            Ok(Mode::Play)
        } else {
            Mode::extension(value)
        }
    }
}

impl<'mode> TryFrom<&'mode str> for Mode {
    type Error = ModeError;

    fn try_from(value: &'mode str) -> Result<Self, Self::Error> {
        Mode::try_from(value.as_bytes())
    }
}

/// A wrapper type used to avoid users creating extension modes that are actually standardized
/// modes.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExtensionMode(String);

impl ExtensionMode {
    /// Returns a `&str` representation of the extension mode.
    ///
    /// The returned string is uppercase even if the extension mode originally was a non-uppercase
    /// mode.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::Mode;
    ///
    /// match Mode::try_from("extension").unwrap() {
    ///     Mode::Extension(extension) => assert_eq!(extension.as_str(), "EXTENSION"),
    ///     _ => panic!("expected extension mode")
    /// }
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<[u8]> for ExtensionMode {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl AsRef<str> for ExtensionMode {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for ExtensionMode {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ExtensionMode {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<ExtensionMode> for String {
    fn from(value: ExtensionMode) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for ExtensionMode {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionMode> for [u8] {
    fn eq(&self, other: &ExtensionMode) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'mode> PartialEq<&'mode [u8]> for ExtensionMode {
    fn eq(&self, other: &&'mode [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'mode> PartialEq<ExtensionMode> for &'mode [u8] {
    fn eq(&self, other: &ExtensionMode) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for ExtensionMode {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionMode> for str {
    fn eq(&self, other: &ExtensionMode) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'mode> PartialEq<&'mode str> for ExtensionMode {
    fn eq(&self, other: &&'mode str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'mode> PartialEq<ExtensionMode> for &'mode str {
    fn eq(&self, other: &ExtensionMode) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

/// A possible error value when converting to a [`Mode`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ModeError {
    /// The mode was empty.
    Empty,

    /// The mode contained an invalid character.
    InvalidCharacter,

    /// The mode was reserved.
    Reserved,
}

impl Display for ModeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ModeError::*;

        match self {
            Empty => write!(formatter, "empty mode"),
            InvalidCharacter => write!(formatter, "invalid mode character"),
            Reserved => write!(formatter, "reserved mode"),
        }
    }
}

impl Error for ModeError {}

impl From<Infallible> for ModeError {
    fn from(_: Infallible) -> Self {
        ModeError::Empty
    }
}
