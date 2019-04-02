//! Response Decoder
//!
//! This module contains a response decoder that can decode a response from a given buffer. The
//! decoder is stateful and allows for data to be given incrementally through a series of calls to
//! the [`Decoder::decode`] function.
//!
//! # Examples
//!
//! ```
//! # extern crate bytes;
//! # extern crate rtsp;
//! #
//! use bytes::BytesMut;
//! use std::convert::TryFrom;
//!
//! use rtsp::header::name::HeaderName;
//! use rtsp::header::value::HeaderValue;
//! use rtsp::protocol::codec::decoder::response::Decoder;
//! use rtsp::response::Response;
//!
//! # fn main() {
//! let buffer =
//!     "RTSP/2.0 200 OK\r\n\
//!      Content-Length: 4\r\n\
//!      \r\n\
//!      Body";
//! let mut decoder = Decoder::new();
//! let (result, bytes_decoded) = decoder.decode(buffer);
//! let expected_response = Response::<()>::builder()
//!     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
//!     .with_body(BytesMut::from("Body".as_bytes()))
//!     .build()
//!     .unwrap();
//!
//! assert_eq!(result.unwrap(), expected_response);
//! assert_eq!(bytes_decoded, buffer.len());
//! # }
//! ```

use bytes::BytesMut;
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::mem;

use crate::header::map::HeaderMapExtension;
use crate::header::name::{HeaderName, HeaderNameError};
use crate::header::types::ContentLength;
use crate::header::value::{HeaderValue, HeaderValueError};
use crate::reason::{ReasonPhrase, ReasonPhraseError};
use crate::response::{Builder as ResponseBuilder, Response};
use crate::status::{StatusCode, StatusCodeError};
use crate::version::{Version, VersionError};

use crate::protocol::codec::decoder::{
    self, DecodeResult as GenericDecodeResult, BODY_DEFAULT_MAX_LENGTH, HEADER_DEFAULT_MAX_COUNT,
    HEADER_NAME_DEFAULT_MAX_LENGTH, HEADER_VALUE_DEFAULT_MAX_LENGTH,
    REASON_PHRASE_DEFAULT_MAX_LENGTH,
};

/// The current state of the response parsing.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum DecodeState<TError> {
    /// The decoder is currently at the stage of parsing the response body.
    Body,

    /// The decoder just finished parsing a response. With regards to the [`Decoder::state`]
    /// function, the returned decode state will never be this variant. This is used internally in
    /// the implementation but a decoding step never finishes on this state.
    End,

    /// The decoder has encountered an error that it cannot recover from. Decoding cannot continue
    /// from this state, and the decoder must be reset before continuing to decode something else.
    Error(TError),

    /// The decoder is currently at the stage of parsing a response header. The state will stay here
    /// until all headers have been decoded.
    Header,

    /// The decoder is currently at the stage of parsing the reason phrase.
    ReasonPhrase,

    /// The decoder is currently at the stage of parsing the status code.
    StatusCode,

    /// The decoder is currently at the stage of parsing the version.
    Version,
}

/// A set of configuration options controlling how the response decoder functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Config {
    /// The maximum length a response body can be.
    body_max_length: usize,

    /// The maximum number of headers a response can contain.
    header_max_count: usize,

    /// The maximum length a header name can be.
    header_name_max_length: usize,

    /// The maximum length a header value can be.
    header_value_max_length: usize,

    /// The maximum length a reason phrase can be.
    reason_phrase_max_length: usize,
}

impl Config {
    /// Returns the maximum length a response body can be.
    pub fn body_max_length(&self) -> usize {
        self.body_max_length
    }

    /// Constructs a builder for specifying possible options.
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    /// Returns the maximum number of headers a response can contain.
    pub fn header_max_count(&self) -> usize {
        self.header_max_count
    }

    /// Returns the maximum length a header name can be.
    pub fn header_name_max_length(&self) -> usize {
        self.header_name_max_length
    }

    /// Returns the maximum length a header value can be.
    pub fn header_value_max_length(&self) -> usize {
        self.header_value_max_length
    }

    /// Returns the the maximum length a method can be.
    pub fn reason_phrase_max_length(&self) -> usize {
        self.reason_phrase_max_length
    }
}

impl Default for Config {
    fn default() -> Self {
        Config::builder().build()
    }
}

/// A builder type for constructing a [`Config`].
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct ConfigBuilder {
    /// The maximum length a response body can be.
    body_max_length: usize,

    /// The maximum number of headers a response can contain.
    header_max_count: usize,

    /// The maximum length a header name can be.
    header_name_max_length: usize,

    /// The maximum length a header value can be.
    header_value_max_length: usize,

    /// The maximum length a reason phrase can be.
    reason_phrase_max_length: usize,
}

impl ConfigBuilder {
    /// Converts the builder into a [`Config`].
    pub fn build(self) -> Config {
        Config {
            body_max_length: self.body_max_length,
            header_max_count: self.header_max_count,
            header_name_max_length: self.header_name_max_length,
            header_value_max_length: self.header_value_max_length,
            reason_phrase_max_length: self.reason_phrase_max_length,
        }
    }

    /// Sets the maximum possible body length.
    pub fn body_max_length(&mut self, length: usize) -> &mut Self {
        self.body_max_length = length;
        self
    }

    /// Sets the maximum possible number of headers.
    pub fn header_max_count(&mut self, length: usize) -> &mut Self {
        self.header_max_count = length;
        self
    }

    /// Sets the maximum possible header name length.
    pub fn header_name_max_length(&mut self, length: usize) -> &mut Self {
        self.header_name_max_length = length;
        self
    }

    /// Sets the maximum header value length.
    pub fn header_value_max_length(&mut self, length: usize) -> &mut Self {
        self.header_value_max_length = length;
        self
    }

    /// Constructs a new builder with a default configuration.
    pub fn new() -> Self {
        ConfigBuilder {
            body_max_length: BODY_DEFAULT_MAX_LENGTH,
            header_max_count: HEADER_DEFAULT_MAX_COUNT,
            header_name_max_length: HEADER_NAME_DEFAULT_MAX_LENGTH,
            header_value_max_length: HEADER_VALUE_DEFAULT_MAX_LENGTH,
            reason_phrase_max_length: REASON_PHRASE_DEFAULT_MAX_LENGTH,
        }
    }

    /// Sets the maximum possible URI length.
    pub fn reason_phrase_max_length(&mut self, length: usize) -> &mut Self {
        self.reason_phrase_max_length = length;
        self
    }

    /// Sets the maximum possible body length.
    pub fn with_body_max_length(mut self, length: usize) -> Self {
        self.body_max_length(length);
        self
    }

    /// Sets the maximum possible number of headers.
    pub fn with_header_max_count(mut self, length: usize) -> Self {
        self.header_max_count(length);
        self
    }

    /// Sets the maximum header name length.
    pub fn with_header_name_max_length(mut self, length: usize) -> Self {
        self.header_name_max_length(length);
        self
    }

    /// Sets the maximum header value length.
    pub fn with_header_value_max_length(mut self, length: usize) -> Self {
        self.header_value_max_length(length);
        self
    }

    /// Sets the maximum possible reason phrase length.
    pub fn with_reason_phrase_max_length(mut self, length: usize) -> Self {
        self.reason_phrase_max_length(length);
        self
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        ConfigBuilder::new()
    }
}

/// An alias for the decode result that is returned from the response decoder.
pub type DecodeResult<TResult> = GenericDecodeResult<TResult, DecodeError>;

/// The decoder for decoding RTSP responses from a buffer.
///
/// This decoder is stateful in that it can partially decode a response if not enough data is
/// present. In this case, the [`Decoder::decode`] function needs to be called again with the rest
/// of the data.
///
/// # Examples
///
/// ```
/// # extern crate bytes;
/// # extern crate rtsp;
/// #
/// use bytes::BytesMut;
/// use std::convert::TryFrom;
///
/// use rtsp::header::name::HeaderName;
/// use rtsp::header::value::HeaderValue;
/// use rtsp::protocol::codec::decoder::response::Decoder;
/// use rtsp::response::Response;
///
/// # fn main() {
/// let buffer =
///     "RTSP/2.0 200 OK\r\n\
///      Content-Length: 4\r\n\
///      \r\n\
///      Body";
/// let mut decoder = Decoder::new();
/// let (result, bytes_decoded) = decoder.decode(buffer);
/// let expected_response = Response::<()>::builder()
///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
///     .with_body(BytesMut::from("Body".as_bytes()))
///     .build()
///     .unwrap();
///
/// assert_eq!(result.unwrap(), expected_response);
/// assert_eq!(bytes_decoded, buffer.len());
/// # }
/// ```
#[derive(Debug)]
pub struct Decoder {
    /// Stateful builder for constructing a response across potentially multiple decode calls.
    builder: ResponseBuilder<BytesMut>,

    /// Configuration for how the decoding should operate.
    config: Config,

    /// The content length of the current response being decoded. This value is only useful if the
    /// current state is [`DecodeState::Body`].
    content_length: ContentLength,

    /// The current decode state of the response decoding.
    state: DecodeState<DecodeError>,
}

impl Decoder {
    /// Attempts to decode a response from `buffer`.
    ///
    /// There are three different cases that can occur as the result of this function:
    ///
    ///  1. A response is successfully decoded and is returned along with the number of bytes
    ///     decoded. The function can be called again with the same buffer or some other data to
    ///     continue decoding.
    ///  2. A response has been partially decoded but more data is needed to finish decoding. The
    ///     function must be called again with the rest of the data in order to finish the decoding.
    ///
    ///     In this case, it could be possible that it has already been detected that the response
    ///     is invalid, but the "stream" is not corrupt and parsing can continue.
    ///  3. An error occured during parsing. The decoder is in a state in which decoding can no
    ///     longer occur. In this case, the decoder must be reset before any further decoding.
    ///
    /// As long as the decoder is in a valid parsing state, this function can be called repeatedly
    /// with more and more data. The state can be checked using the [`Decoder::state`] function. If
    /// the state is invalid, the decoder can be reset to its default state using the
    /// [`Decoder::reset`] function.
    ///
    /// The number of bytes decoded is the number of bytes successfully decoded, but is likely
    /// incorrect in the case of an error.
    ///
    /// # Examples
    ///
    /// Decoding a full response:
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::protocol::codec::decoder::response::Decoder;
    /// use rtsp::response::Response;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "RTSP/2.0 200 OK\r\n\
    ///      Content-Length: 5\r\n\
    ///      \r\n\
    ///      Test!";
    /// let mut decoder = Decoder::new();
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    /// let expected_response = Response::<()>::builder()
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("5").unwrap())
    ///     .with_body(BytesMut::from("Test!".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// # }
    /// ```
    ///
    /// Decoding a partial response:
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::protocol::codec::decoder::response::Decoder;
    /// use rtsp::response::Response;
    ///
    /// # fn main() {
    /// let buffer = "RTSP/2.0 200 OK\r\n";
    /// let mut decoder = Decoder::new();
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    ///
    /// assert!(result.is_incomplete());
    /// assert_eq!(bytes_decoded, buffer.len());
    ///
    /// let buffer =
    ///     "Content-Length: 5\r\n\
    ///      \r\n\
    ///      Test!";
    ///
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    ///
    /// let expected_response = Response::<()>::builder()
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("5").unwrap())
    ///     .with_body(BytesMut::from("Test!".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// # }
    /// ```
    pub fn decode<TBuffer>(&mut self, buffer: TBuffer) -> (DecodeResult<Response<BytesMut>>, usize)
    where
        TBuffer: AsRef<[u8]>,
    {
        use self::DecodeState::*;
        use self::GenericDecodeResult::*;

        let mut buffer = buffer.as_ref();
        let buffer_size = buffer.len();

        loop {
            let decode_result = match self.state {
                Header => self.decode_header(&mut buffer),
                StatusCode => self.decode_status_code(&mut buffer),
                ReasonPhrase => self.decode_reason_phrase(&mut buffer),
                Version => self.decode_version(&mut buffer),
                Body => self.decode_body(&mut buffer),
                End => {
                    let difference = buffer_size - buffer.len();
                    let builder = mem::replace(&mut self.builder, Response::builder());
                    let response = builder
                        .build()
                        .expect("no response parts should be missing");
                    self.state = Version;

                    break (Complete(response), difference);
                }
                DecodeState::Error(error) => break (DecodeResult::Error(error), 0),
            };

            match decode_result {
                Complete(_) => continue,
                DecodeResult::Error(error) => {
                    self.state = DecodeState::Error(error);
                    break (DecodeResult::Error(error), buffer_size - buffer.len());
                }
                Incomplete => break (Incomplete, buffer_size - buffer.len()),
            }
        }
    }

    /// This function more so just extracts the body with a length determined by the content length
    /// than it does decode it. This decoder does not try to decode the body based on the content
    /// type, this should be done at a higher level.
    fn decode_body<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        if *self.content_length > buffer.len() {
            Incomplete
        } else {
            let (body, undecoded_buffer) = buffer.split_at(*self.content_length);
            *buffer = undecoded_buffer;
            self.state = DecodeState::End;
            self.builder.body(BytesMut::from(body));
            Complete(())
        }
    }

    /// Decodes a full header from the buffer, including the header name and header value that may
    /// span multiple lines.
    fn decode_header<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        match try_complete!(self.decode_header_name(buffer)) {
            Some(header_name) => {
                if self.builder.headers.values_len() >= self.config.header_max_count() {
                    return Error(DecodeError::TooManyHeaders);
                }

                let rest = &buffer[header_name.len() + 1..];
                let header_value = try_complete!(self.decode_header_value(&rest));
                let header_end_index = header_value.len() + 2;

                let header_name = decoder::trim_header_name(header_name);
                let header_value = decoder::trim_header_value(header_value);

                match HeaderName::try_from(header_name) {
                    Ok(header_name) => match HeaderValue::try_from(header_value) {
                        Ok(header_value) => {
                            self.builder.header(header_name, header_value);
                            *buffer = &rest[header_end_index..];
                            Complete(())
                        }
                        Err(error) => Error(error.into()),
                    },
                    Err(error) => Error(error.into()),
                }
            }
            None => {
                *buffer = &buffer[2..];
                self.state = DecodeState::Body;
                self.content_length = match self.builder.headers.typed_try_get::<ContentLength>() {
                    Ok(Some(content_length)) if *content_length > self.config.body_max_length() => {
                        return Error(DecodeError::BodyTooLong);
                    }
                    Ok(Some(content_length)) => content_length,
                    Ok(None) => ContentLength::default(),
                    Err(_) => {
                        return Error(DecodeError::InvalidContentLength);
                    }
                };
                Complete(())
            }
        }
    }

    /// Decodes a header name from the buffer.
    fn decode_header_name<'buffer>(
        &mut self,
        buffer: &'buffer [u8],
    ) -> DecodeResult<Option<&'buffer [u8]>> {
        use self::GenericDecodeResult::*;

        if buffer.starts_with(b"\r\n") {
            return Complete(None);
        }

        let max_length = self.config.header_name_max_length() + 1;
        let mut iter = buffer.iter().take(max_length);

        match iter.position(|&byte| byte == b':') {
            Some(index) => Complete(Some(&buffer[0..index])),
            None if buffer.len() >= max_length => Error(DecodeError::HeaderNameTooLong),
            None => Incomplete,
        }
    }

    /// Decodes a header value from the buffer.
    fn decode_header_value<'buffer>(
        &mut self,
        buffer: &'buffer [u8],
    ) -> DecodeResult<&'buffer [u8]> {
        use self::GenericDecodeResult::*;

        let max_length = self.config.header_value_max_length() + 1;
        let iter = buffer
            .windows(2)
            .take(max_length)
            .enumerate()
            .filter(|&(_, b)| b == b"\r\n")
            .map(|(i, _)| i);

        for index in iter {
            match buffer.get(index + 2) {
                Some(&byte) if byte == b' ' || byte == b'\t' => continue,
                Some(_) => return Complete(&buffer[0..index]),
                None => return Incomplete,
            }
        }

        if buffer.len() >= max_length {
            Error(DecodeError::HeaderValueTooLong)
        } else {
            Incomplete
        }
    }

    /// Decodes a reason phrase from the buffer.
    fn decode_reason_phrase<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        let max_length = self.config.reason_phrase_max_length() + 2;

        match buffer
            .windows(2)
            .take(max_length)
            .position(|bytes| bytes == b"\r\n")
        {
            Some(index) => match ReasonPhrase::try_from(&buffer[0..index]) {
                Ok(reason_phrase) => {
                    self.builder.reason_phrase(Some(reason_phrase));
                    self.state = DecodeState::Header;
                    *buffer = &buffer[index + 2..];
                    Complete(())
                }
                Err(error) => Error(error.into()),
            },
            None if buffer.len() >= max_length => Error(DecodeError::ReasonPhraseTooLong),
            None => Incomplete,
        }
    }

    /// Decodes a status code from the buffer.
    fn decode_status_code<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        if buffer.len() < 4 {
            return Incomplete;
        }

        if buffer[3] != b' ' {
            return Error(DecodeError::StatusCode(StatusCodeError::Invalid));
        }

        match StatusCode::try_from(&buffer[0..3]) {
            Ok(status_code) => {
                self.builder.status_code(status_code);
                self.state = DecodeState::ReasonPhrase;
                *buffer = &buffer[4..];
                Complete(())
            }
            Err(error) => Error(error.into()),
        }
    }

    /// Decodes a version from the buffer.
    fn decode_version<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        if buffer.len() < 9 {
            return Incomplete;
        }

        if buffer[8] != b' ' {
            return Error(DecodeError::Version(VersionError::Invalid));
        }

        match Version::try_from(&buffer[0..8]) {
            Ok(version) if version == Version::RTSP20 => {
                self.builder.version(version);
                self.state = DecodeState::StatusCode;
                *buffer = &buffer[9..];
                Complete(())
            }
            Ok(_) => Error(DecodeError::UnsupportedVersion),
            Err(error) => Error(error.into()),
        }
    }

    /// Constructs a new response decoder.
    pub fn new() -> Self {
        Decoder::with_config(Config::default())
    }

    /// Resets the decoder state back to its default state.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::protocol::codec::decoder::response::{DecodeError, Decoder, DecodeState};
    /// use rtsp::version::VersionError;
    ///
    /// # fn main() {
    /// let buffer = "Bad Response!\r\nExtra garbage data";
    /// let mut decoder = Decoder::new();
    ///
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    /// assert_eq!(
    ///     decoder.state(),
    ///     DecodeState::Error(DecodeError::Version(VersionError::Invalid))
    /// );
    /// assert!(result.is_error());
    ///
    /// decoder.reset();
    ///
    /// assert_eq!(decoder.state(), DecodeState::Version);
    /// # }
    /// ```
    pub fn reset(&mut self) {
        self.builder = ResponseBuilder::new();
        self.content_length = ContentLength::default();
        self.state = DecodeState::Version;
    }

    /// Returns the current parsing state. If the decode state is the [`DecodeState::Error`]
    /// variant, it will include the error.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::protocol::codec::decoder::response::{Decoder, DecodeState};
    /// use rtsp::response::Response;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "RTSP/2.0 200 OK\r\n\
    ///      Content-Length: 4\r\n\
    ///      \r\n\
    ///      Body";
    /// let mut decoder = Decoder::new();
    /// assert_eq!(decoder.state(), DecodeState::Version);
    ///
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    /// let expected_response = Response::<()>::builder()
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
    ///     .with_body(BytesMut::from("Body".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// assert_eq!(decoder.state(), DecodeState::Version);
    /// # }
    /// ```
    pub fn state(&self) -> DecodeState<DecodeError> {
        self.state.clone()
    }

    /// Constructs a new response decoder using the given configuration.
    pub fn with_config(config: Config) -> Self {
        Decoder {
            builder: ResponseBuilder::new(),
            config,
            content_length: ContentLength::default(),
            state: DecodeState::Version,
        }
    }
}

impl Default for Decoder {
    fn default() -> Self {
        Decoder::new()
    }
}

/// An error type for when the response was invalid.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum DecodeError {
    /// The body, as determined by the content length header, was too long.
    BodyTooLong,

    /// There was an error decoding a header name.
    HeaderName(HeaderNameError),

    /// A header name was too long.
    HeaderNameTooLong,

    /// There was an error decoding a header value.
    HeaderValue(HeaderValueError),

    /// A header value was too long.`
    HeaderValueTooLong,

    /// The content length header was invalid.
    InvalidContentLength,

    /// There was an error decoding the reason phrase.
    ReasonPhrase(ReasonPhraseError),

    /// The reason phrase was too long.
    ReasonPhraseTooLong,

    /// There was an error decoding the status code.
    StatusCode(StatusCodeError),

    /// The response contained too many headers.
    TooManyHeaders,

    /// The version was unsupported.
    UnsupportedVersion,

    /// There was an error decoding the version.
    Version(VersionError),
}

impl Error for DecodeError {}

impl Display for DecodeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::DecodeError::*;

        match self {
            BodyTooLong => write!(formatter, "body too long"),
            HeaderName(error) => error.fmt(formatter),
            HeaderNameTooLong => write!(formatter, "header name too long"),
            HeaderValue(error) => error.fmt(formatter),
            HeaderValueTooLong => write!(formatter, "header value too long"),
            InvalidContentLength => write!(formatter, "invalid content length header"),
            ReasonPhrase(error) => error.fmt(formatter),
            ReasonPhraseTooLong => write!(formatter, "reason phrase too long"),
            StatusCode(error) => error.fmt(formatter),
            TooManyHeaders => write!(formatter, "too many headers"),
            UnsupportedVersion => write!(formatter, "unsupported version"),
            Version(error) => error.fmt(formatter),
        }
    }
}

impl From<HeaderNameError> for DecodeError {
    fn from(value: HeaderNameError) -> Self {
        DecodeError::HeaderName(value)
    }
}

impl From<HeaderValueError> for DecodeError {
    fn from(value: HeaderValueError) -> Self {
        DecodeError::HeaderValue(value)
    }
}

impl From<Infallible> for DecodeError {
    fn from(_: Infallible) -> Self {
        DecodeError::InvalidContentLength
    }
}

impl From<ReasonPhraseError> for DecodeError {
    fn from(value: ReasonPhraseError) -> Self {
        DecodeError::ReasonPhrase(value)
    }
}

impl From<StatusCodeError> for DecodeError {
    fn from(value: StatusCodeError) -> Self {
        DecodeError::StatusCode(value)
    }
}

impl From<VersionError> for DecodeError {
    fn from(value: VersionError) -> Self {
        DecodeError::Version(value)
    }
}

#[cfg(test)]
mod test {
    use crate::header::name::HeaderNameError;
    use crate::protocol::codec::decoder::response::{
        ConfigBuilder, DecodeError, DecodeResult, Decoder,
    };
    use crate::reason::ReasonPhraseError;
    use crate::status::StatusCodeError;
    use crate::version::VersionError;

    #[test]
    fn test_decoder_decode_body_invalid_content_length() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      Content-Length: content length\r\n\
                      \r\n\
                      body";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::InvalidContentLength)
        );
    }

    #[test]
    fn test_decoder_decode_body_too_long() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      Content-Length: 5\r\n\
                      \r\n\
                      12345";
        let config = ConfigBuilder::new().with_body_max_length(2).build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::BodyTooLong));
    }

    #[test]
    fn test_decoder_decode_header_name_empty() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      : 1\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::HeaderName(HeaderNameError::Empty))
        );
    }

    #[test]
    fn test_decoder_decode_header_name_invalid_character() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      Content Length: 1\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::HeaderName(HeaderNameError::InvalidCharacter))
        );
    }

    #[test]
    fn test_decoder_decode_header_name_too_long() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      My-Very-Long-Header-Name: 5\r\n\
                      \r\n";
        let config = ConfigBuilder::new().with_header_name_max_length(10).build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::HeaderNameTooLong));
    }

    #[test]
    fn test_decoder_decode_header_too_many() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      A: 1\r\n\
                      B: 2\r\n\
                      \r\n";
        let config = ConfigBuilder::new().with_header_max_count(1).build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::TooManyHeaders));
    }

    #[test]
    fn test_decoder_decode_header_value_too_long() {
        let buffer = "RTSP/2.0 200 OK\r\n\
                      Header: my very long header value\r\n\
                      \r\n";
        let config = ConfigBuilder::new()
            .with_header_value_max_length(10)
            .build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::HeaderValueTooLong));
    }

    #[test]
    fn test_decoder_decode_reason_phrase_empty() {
        let buffer = "RTSP/2.0 200 \r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::ReasonPhrase(ReasonPhraseError::Empty))
        );
    }

    #[test]
    fn test_decoder_decode_reason_phrase_too_long() {
        let buffer = "RTSP/2.0 200 OKOKOKOKOKOK\r\n\
                      \r\n";
        let config = ConfigBuilder::new()
            .with_reason_phrase_max_length(8)
            .build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::ReasonPhraseTooLong)
        );
    }

    #[test]
    fn test_decoder_decode_status_code_invalid() {
        let buffer = "RTSP/2.0  OK\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::StatusCode(StatusCodeError::Invalid))
        );
    }

    #[test]
    fn test_decoder_decode_version_invalid() {
        let buffer = "RTSP/2.0! 200 OK\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::Version(VersionError::Invalid))
        );
    }

    #[test]
    fn test_decoder_decode_version_unknown() {
        let buffer = "RTSP/2.1 200 OK\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::Version(VersionError::Unknown))
        );
    }

    #[test]
    fn test_decoder_decode_version_unsupported() {
        let buffer = "RTSP/1.0 200 OK\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::UnsupportedVersion));
    }
}
