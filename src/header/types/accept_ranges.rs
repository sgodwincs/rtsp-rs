use itertools::Itertools;
use linked_hash_set::LinkedHashSet;
use std::convert::TryFrom;
use std::fmt::{self, Display, Formatter};
use std::iter::{once, FromIterator};
use std::ops::{Deref, DerefMut};
use std::str;

use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;
use crate::syntax;

/// The `"Accept-Ranges"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.5).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct AcceptRanges(LinkedHashSet<RangeFormat>);

impl AcceptRanges {
    /// Constructs a new header with no range formats by default.
    pub fn new() -> Self {
        AcceptRanges::default()
    }
}

impl Deref for AcceptRanges {
    type Target = LinkedHashSet<RangeFormat>;

    fn deref(&self) -> &LinkedHashSet<RangeFormat> {
        &self.0
    }
}

impl DerefMut for AcceptRanges {
    fn deref_mut(&mut self) -> &mut LinkedHashSet<RangeFormat> {
        &mut self.0
    }
}

impl FromIterator<RangeFormat> for AcceptRanges {
    fn from_iter<TIterator>(iterator: TIterator) -> Self
    where
        TIterator: IntoIterator<Item = RangeFormat>,
    {
        AcceptRanges(LinkedHashSet::from_iter(iterator))
    }
}

impl TypedHeader for AcceptRanges {
    type DecodeError = AcceptRangesError;

    /// Converts the raw header values to the [`AcceptRanges`] header type. Based on the syntax
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
    /// Accept-Ranges = "Accept-Ranges" HCOLON acceptable-ranges
    /// acceptable-ranges = (range-unit *(COMMA range-unit))
    /// range-unit = "npt" / "smpte" / "smpte-30-drop" / "smpte-25"
    ///            / "clock" / extension-format
    /// extension-format = token
    /// ```
    ///
    /// All values separated with commas will be converted to the [`RangeFormat`] type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::accept_ranges::RangeFormat;
    /// use rtsp::header::types::AcceptRanges;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(AcceptRanges::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = vec![RangeFormat::Clock, RangeFormat::NPT]
    ///     .into_iter()
    ///     .collect::<AcceptRanges>();
    /// let raw_header = vec![HeaderValue::try_from("clock, npt").unwrap()];
    /// assert_eq!(
    ///     AcceptRanges::decode(&mut raw_header.iter()).unwrap(),
    ///     Some(typed_header)
    /// );
    /// ```
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let mut range_formats = LinkedHashSet::new();
        let mut present = false;

        for value in values {
            let parts = value.as_str().split(',');

            for part in parts {
                range_formats.insert(RangeFormat::try_from(syntax::trim_whitespace(part))?);
            }

            present = true;
        }

        if present {
            Ok(Some(AcceptRanges(range_formats)))
        } else {
            Ok(None)
        }
    }

    /// Converts the [`AcceptRanges`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::accept_ranges::RangeFormat;
    /// use rtsp::header::types::AcceptRanges;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let typed_header = vec![RangeFormat::Clock, RangeFormat::NPT]
    ///     .into_iter()
    ///     .collect::<AcceptRanges>();
    /// let expected_raw_headers = vec![
    ///     vec![HeaderValue::try_from("clock, npt").unwrap()],
    ///     vec![HeaderValue::try_from("npt, clock").unwrap()],
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
        // Header values must be valid UTF-8, and since we know that the [`RangeFormat`] type
        // guarantees valid ASCII-US (with no newlines), it satisfies the constraints.
        let value = self.iter().map(RangeFormat::as_str).join(", ");
        values.extend(once(unsafe { HeaderValue::from_string_unchecked(value) }));
    }

    /// Returns the statically assigned [`HeaderName`] for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::AcceptRanges
    }
}

pub type AcceptRangesError = RangeFormatError;

/// Possible range formats that can be used in the `"Accept-Ranges"` header.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum RangeFormat {
    /// UTC absolute time format expressed using a timestamp based on ISO 8601. The date is a
    /// complete representation of the calendar date in basic format (`YYYYMMDD`) without
    /// separators. The time of day is provided in the complete representation basic format
    /// (`hhmmss`), allowing decimal fractions of seconds requiring `"."` (full stop) as decimal
    /// separator and limiting thee number of digits to no more than nine. The time expressed must
    /// use UTC (GMT), i.e., no time zone offsets are allowed. The full date and time specification
    /// is the eight-digit date followed by a `"T"` followed by the six-digit time value, optionally
    /// followed by a full stop followed by one to nine fractions of a second and ended by `"Z"`,
    /// e.g., `YYYYMMDDThhmmss.ssZ`.
    Clock,

    /// An unregistered range format.
    Extension(ExtensionRangeFormat),

    /// Normal Play Time (NPT) indicates the stream-absolute position relative to the beginning of
    /// the presentation. The timestamp consists of two parts: The mandatory first part may be
    /// expressed in either seconds only or in hours, minutes, and seconds. The optional second part
    /// consists of a decimal point and decimal figures and indicates fractions of a seconds.
    NPT,

    /// A format derived from a Society of Motion Picture and Television Engineers (SMPTE)
    /// specification and epxresses time offsets anchored at the start of the media clip. Relative
    /// timestamps are expressed as SMPTE time codes for frame-level access accuracy.
    SMPTE,

    /// See [`RangeFormat::SMPTE`].
    ///
    /// A SMPTE format with a frame rate of 25 frames per second.
    SMPTE25,

    /// See [`RangeFormat::SMPTE`].
    ///
    /// A SMPTE format with a frame rate of 29.97 frames per second. Since SMPTE 30 can only use
    /// frames with values 0 through 29, the first two frame indices (values 00 and 01) of every
    /// minute, except every tenth minute, are dropped.
    SMPTE30Drop,
}

impl RangeFormat {
    /// Returns a `&str` representation of the range format.
    ///
    /// The returned string is lowercase even if the range format originally was a non-lowercase
    /// extension range format.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::accept_ranges::RangeFormat;
    ///
    /// assert_eq!(RangeFormat::Clock.as_str(), "clock");
    /// assert_eq!(RangeFormat::try_from("EXTENSION").unwrap().as_str(), "extension");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::RangeFormat::*;

        match self {
            Clock => "clock",
            NPT => "npt",
            SMPTE => "smpte",
            SMPTE25 => "smpte-25",
            SMPTE30Drop => "smpte-30-drop",
            Extension(extension) => extension.as_str(),
        }
    }

    /// A helper function that creates a new [`RangeFormat`] instance with the given range format
    /// extension.
    ///
    /// It first checks to see if the range format is valid, and if not, it will return an error.
    ///
    /// Based on, [[RFC7826, Section 20.2.3](https://tools.ietf.org/html/rfc7826#section-20.2.3)], a
    /// range format follows the following rules:
    ///
    /// ```text
    /// token = 1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
    ///       /  %x41-5A / %x5E-7A / %x7C / %x7E)
    ///          ; 1*<any CHAR except CTLs or tspecials>
    /// range-unit = "npt" / "smpte" / "smpte-30-drop" / "smpte-25"
    ///            / "clock" / extension-format
    /// extension-format = token
    /// ```
    fn extension(value: &[u8]) -> Result<RangeFormat, RangeFormatError> {
        if value.is_empty() {
            return Err(RangeFormatError::Empty);
        }

        if !syntax::is_token(value) {
            return Err(RangeFormatError::InvalidCharacter);
        }

        // Unsafe: The function above [`syntax::is_token`] ensures that the value is valid ASCII-US.
        let value = unsafe { str::from_utf8_unchecked(value) }.to_ascii_lowercase();
        Ok(RangeFormat::Extension(ExtensionRangeFormat(value)))
    }
}

impl AsRef<[u8]> for RangeFormat {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for RangeFormat {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Display for RangeFormat {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<RangeFormat> for String {
    fn from(value: RangeFormat) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for RangeFormat {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<RangeFormat> for [u8] {
    fn eq(&self, other: &RangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'range_format> PartialEq<&'range_format [u8]> for RangeFormat {
    fn eq(&self, other: &&'range_format [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'range_format> PartialEq<RangeFormat> for &'range_format [u8] {
    fn eq(&self, other: &RangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for RangeFormat {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<RangeFormat> for str {
    fn eq(&self, other: &RangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'range_format> PartialEq<&'range_format str> for RangeFormat {
    fn eq(&self, other: &&'range_format str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'range_format> PartialEq<RangeFormat> for &'range_format str {
    fn eq(&self, other: &RangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'range_format> TryFrom<&'range_format [u8]> for RangeFormat {
    type Error = RangeFormatError;

    fn try_from(value: &'range_format [u8]) -> Result<Self, Self::Error> {
        use self::RangeFormat::*;

        macro_rules! check_range_format {
            ($range_format:ident) => {
                let bytes = $range_format.as_str().as_bytes();
                let starts_with = value
                    .iter()
                    .take(bytes.len())
                    .map(u8::to_ascii_lowercase)
                    .eq(bytes.iter().cloned());

                if starts_with && value.len() == bytes.len() {
                    return Ok($range_format);
                }
            };
        }

        match value.len() {
            3 => {
                check_range_format!(NPT);
                RangeFormat::extension(value)
            }
            5 => {
                check_range_format!(Clock);
                check_range_format!(SMPTE);
                RangeFormat::extension(value)
            }
            8 => {
                check_range_format!(SMPTE25);
                RangeFormat::extension(value)
            }
            13 => {
                check_range_format!(SMPTE30Drop);
                RangeFormat::extension(value)
            }
            _ => RangeFormat::extension(value),
        }
    }
}

impl<'range_format> TryFrom<&'range_format str> for RangeFormat {
    type Error = RangeFormatError;

    fn try_from(value: &'range_format str) -> Result<Self, Self::Error> {
        RangeFormat::try_from(value.as_bytes())
    }
}

/// A wrapper type used to avoid users creating extension range formats that are actually
/// standardized range formats.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExtensionRangeFormat(String);

impl AsRef<[u8]> for ExtensionRangeFormat {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl AsRef<str> for ExtensionRangeFormat {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for ExtensionRangeFormat {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ExtensionRangeFormat {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<ExtensionRangeFormat> for String {
    fn from(value: ExtensionRangeFormat) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for ExtensionRangeFormat {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionRangeFormat> for [u8] {
    fn eq(&self, other: &ExtensionRangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'range_format> PartialEq<&'range_format [u8]> for ExtensionRangeFormat {
    fn eq(&self, other: &&'range_format [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'range_format> PartialEq<ExtensionRangeFormat> for &'range_format [u8] {
    fn eq(&self, other: &ExtensionRangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for ExtensionRangeFormat {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<ExtensionRangeFormat> for str {
    fn eq(&self, other: &ExtensionRangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'range_format> PartialEq<&'range_format str> for ExtensionRangeFormat {
    fn eq(&self, other: &&'range_format str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'range_format> PartialEq<ExtensionRangeFormat> for &'range_format str {
    fn eq(&self, other: &ExtensionRangeFormat) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl ExtensionRangeFormat {
    /// Returns a `&str` representation of the extension range format.
    ///
    /// The returned string is lowercase even if the extension range format originally was a
    /// non-lowercase range format.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::accept_ranges::RangeFormat;
    ///
    /// match RangeFormat::try_from("EXTENSION").unwrap() {
    ///     RangeFormat::Extension(extension) => assert_eq!(extension.as_str(), "extension"),
    ///     _ => panic!("expected extension range format")
    /// }
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// A possible error value when converting to a [`RangeFormat`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum RangeFormatError {
    // The range format was empty.
    Empty,

    // The range format contained an invalid character.
    InvalidCharacter,
}
