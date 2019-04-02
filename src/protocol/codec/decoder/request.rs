//! Request Decoder
//!
//! This module contains a request decoder that can decode a request from a given buffer. The
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
//! use rtsp::method::Method;
//! use rtsp::protocol::codec::decoder::request::Decoder;
//! use rtsp::request::Request;
//! use rtsp::uri::request::URI;
//!
//! # fn main() {
//! let buffer =
//!     "SETUP * RTSP/2.0\r\n\
//!      Content-Length: 4\r\n\
//!      \r\n\
//!      Body";
//! let mut decoder = Decoder::new();
//! let (result, bytes_decoded) = decoder.decode(buffer);
//! let expected_request = Request::<()>::builder()
//!     .with_method(Method::Setup)
//!     .with_uri(URI::asterisk())
//!     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
//!     .with_body(BytesMut::from("Body".as_bytes()))
//!     .build()
//!     .unwrap();
//!
//! assert_eq!(result.unwrap(), expected_request);
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
use crate::method::{Method, MethodError};
use crate::protocol::codec::decoder::{
    self, DecodeResult as GenericDecodeResult, BODY_DEFAULT_MAX_LENGTH, HEADER_DEFAULT_MAX_COUNT,
    HEADER_NAME_DEFAULT_MAX_LENGTH, HEADER_VALUE_DEFAULT_MAX_LENGTH, METHOD_DEFAULT_MAX_LENGTH,
    URI_DEFAULT_MAX_LENGTH,
};
use crate::request::{Builder as RequestBuilder, Request};
use crate::uri::request::{URIError, URI};
use crate::version::{Version, VersionError};

/// The current state of the request parsing.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum DecodeState<TError> {
    /// The decoder is currently at the stage of parsing the request body.
    Body,

    /// The decoder just finished parsing a request. With regards to the [`Decoder::state`]
    /// function, the returned decode state will never be this variant. This is used internally in
    /// the implementation but a decoding step never finishes on this state.
    End,

    /// The decoder has encountered an error that it cannot recover from. Decoding cannot continue
    /// from this state, and the decoder must be reset before continuing to decode something else.
    Error(TError),

    /// The decoder is currently at the stage of parsing a request header. The state will stay here
    /// until all headers have been decoded.
    Header,

    /// The decoder is currently at the stage of parsing the method.
    Method,

    /// The decoder is currently at the stage of parsing the URI.
    URI,

    /// The decoder is currently at the stage of parsing the version.
    Version,
}

/// A set of configuration options controlling how the request decoder functions.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Config {
    /// The maximum length a request body can be.
    body_max_length: usize,

    /// The maximum number of headers a request can contain.
    header_max_count: usize,

    /// The maximum length a header name can be.
    header_name_max_length: usize,

    /// The maximum length a header value can be.
    header_value_max_length: usize,

    /// The maximum length a method can be.
    method_max_length: usize,

    /// The maximum length a URI can be.
    uri_max_length: usize,
}

impl Config {
    /// Returns the maximum length a request body can be.
    pub fn body_max_length(&self) -> usize {
        self.body_max_length
    }

    /// Constructs a builder for specifying possible options.
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::new()
    }

    /// Returns the maximum number of headers a request can contain.
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
    pub fn method_max_length(&self) -> usize {
        self.method_max_length
    }

    /// Returns the maximum length a URI can be.
    pub fn uri_max_length(&self) -> usize {
        self.uri_max_length
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
    /// The maximum length a request body can be.
    body_max_length: usize,

    /// The maximum number of headers a request can contain.
    header_max_count: usize,

    /// The maximum length a header name can be.
    header_name_max_length: usize,

    /// The maximum length a header value can be.
    header_value_max_length: usize,

    /// The maximum length a method can be.
    method_max_length: usize,

    /// The maximum length a request URI can be.
    uri_max_length: usize,
}

impl ConfigBuilder {
    /// Converts the builder into a [`Config`].
    pub fn build(self) -> Config {
        Config {
            body_max_length: self.body_max_length,
            header_max_count: self.header_max_count,
            header_name_max_length: self.header_name_max_length,
            header_value_max_length: self.header_value_max_length,
            method_max_length: self.method_max_length,
            uri_max_length: self.uri_max_length,
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

    /// Sets the maximum possible method length.
    pub fn method_max_length(&mut self, length: usize) -> &mut Self {
        self.method_max_length = length;
        self
    }

    /// Constructs a new builder with a default configuration.
    pub fn new() -> Self {
        ConfigBuilder {
            body_max_length: BODY_DEFAULT_MAX_LENGTH,
            header_max_count: HEADER_DEFAULT_MAX_COUNT,
            header_name_max_length: HEADER_NAME_DEFAULT_MAX_LENGTH,
            header_value_max_length: HEADER_VALUE_DEFAULT_MAX_LENGTH,
            method_max_length: METHOD_DEFAULT_MAX_LENGTH,
            uri_max_length: URI_DEFAULT_MAX_LENGTH,
        }
    }

    /// Sets the maximum possible URI length.
    pub fn uri_max_length(&mut self, length: usize) -> &mut Self {
        self.uri_max_length = length;
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

    /// Sets the maximum possible method length.
    pub fn with_method_max_length(mut self, length: usize) -> Self {
        self.method_max_length(length);
        self
    }

    /// Sets the maximum possible URI length.
    pub fn with_uri_max_length(mut self, length: usize) -> Self {
        self.uri_max_length(length);
        self
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        ConfigBuilder::new()
    }
}

/// An alias for the decode result that is returned from the request decoder.
pub type DecodeResult<TResult> = GenericDecodeResult<TResult, DecodeError>;

/// The decoder for decoding RTSP requests from a buffer.
///
/// This decoder is stateful in that it can partially decode a request if not enough data is
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
/// use rtsp::method::Method;
/// use rtsp::protocol::codec::decoder::request::Decoder;
/// use rtsp::request::Request;
/// use rtsp::uri::request::URI;
///
/// # fn main() {
/// let buffer =
///     "SETUP * RTSP/2.0\r\n\
///      Content-Length: 4\r\n\
///      \r\n\
///      Body";
/// let mut decoder = Decoder::new();
/// let (result, bytes_decoded) = decoder.decode(buffer);
/// let expected_request = Request::<()>::builder()
///     .with_method(Method::Setup)
///     .with_uri(URI::asterisk())
///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
///     .with_body(BytesMut::from("Body".as_bytes()))
///     .build()
///     .unwrap();
///
/// assert_eq!(result.unwrap(), expected_request);
/// assert_eq!(bytes_decoded, buffer.len());
/// # }
/// ```
#[derive(Debug)]
pub struct Decoder {
    /// Stateful builder for constructing a request across potentially multiple decode calls.
    builder: RequestBuilder<BytesMut>,

    /// Configuration for how the decoding should operate.
    config: Config,

    /// The content length of the current request being decoded. This value is only useful if the
    /// current state is [`DecodeState::Body`].
    content_length: ContentLength,

    /// The current decode state of the request decoding.
    state: DecodeState<DecodeError>,
}

impl Decoder {
    /// Attempts to decode a request from `buffer`.
    ///
    /// There are three different cases that can occur as the result of this function:
    ///
    ///  1. A request is successfully decoded and is returned along with the number of bytes
    ///     decoded. The function can be called again with the same buffer or some other data to
    ///     continue decoding.
    ///  2. A request has been partially decoded but more data is needed to finish decoding. The
    ///     function must be called again with the rest of the data in order to finish the decoding.
    ///
    ///     In this case, it could be possible that it has already been detected that the request is
    ///     invalid, but the "stream" is not corrupt and parsing can continue.
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
    /// Decoding a full request:
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
    /// use rtsp::method::Method;
    /// use rtsp::protocol::codec::decoder::request::Decoder;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "GET_PARAMETER rtsp://example.com RTSP/2.0\r\n\
    ///      Content-Length: 4\r\n\
    ///      Session: QKyjN8nt2WqbWw4tIYof52; timeout = 60\r\n\
    ///      \r\n\
    ///      Body";
    /// let mut decoder = Decoder::new();
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    /// let expected_request = Request::<()>::builder()
    ///     .with_method(Method::GetParameter)
    ///     .with_uri(URI::try_from("rtsp://example.com").unwrap())
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
    ///     .with_header(
    ///         HeaderName::Session,
    ///         HeaderValue::try_from("QKyjN8nt2WqbWw4tIYof52; timeout = 60").unwrap()
    ///     )
    ///     .with_body(BytesMut::from("Body".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// # }
    /// ```
    ///
    /// Decoding a partial request:
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
    /// use rtsp::method::Method;
    /// use rtsp::protocol::codec::decoder::request::Decoder;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// # fn main() {
    /// let buffer = "SETUP * RTSP/2.0\r\n";
    /// let mut decoder = Decoder::new();
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    ///
    /// assert!(result.is_incomplete());
    /// assert_eq!(bytes_decoded, buffer.len());
    ///
    /// let buffer =
    ///     "Content-Length: 5\r\n\
    ///     \r\n\
    ///     Test!";
    ///
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    ///
    /// let expected_request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("5").unwrap())
    ///     .with_body(BytesMut::from("Test!".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// # }
    /// ```
    pub fn decode<TBuffer>(&mut self, buffer: TBuffer) -> (DecodeResult<Request<BytesMut>>, usize)
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
                Method => self.decode_method(&mut buffer),
                URI => self.decode_uri(&mut buffer),
                Version => self.decode_version(&mut buffer),
                Body => self.decode_body(&mut buffer),
                End => {
                    let difference = buffer_size - buffer.len();
                    let builder = mem::replace(&mut self.builder, Request::builder());
                    let request = builder.build().expect("no request parts should be missing");
                    self.state = Method;

                    break (Complete(request), difference);
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
            .filter(|&(_, bytes)| bytes == b"\r\n")
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

    /// Decodes a method from the buffer.
    fn decode_method<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        let max_length = self.config.method_max_length() + 1;
        let mut iter = buffer.iter().take(max_length);

        match iter.position(|&byte| byte == b' ') {
            Some(index) => match Method::try_from(&buffer[0..index]) {
                Ok(method) => {
                    self.builder.method(method);
                    self.state = DecodeState::URI;
                    *buffer = &buffer[index + 1..];
                    Complete(())
                }
                Err(error) => Error(error.into()),
            },
            None if buffer.len() >= max_length => Error(DecodeError::MethodTooLong),
            None => Incomplete,
        }
    }

    /// Decodes a URI from the buffer.
    fn decode_uri<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        let max_length = self.config.uri_max_length() + 1;
        let mut iter = buffer.iter().take(max_length);

        match iter.position(|&byte| byte == b' ') {
            Some(index) => match URI::try_from(&buffer[0..index]) {
                Ok(uri) => {
                    self.builder.uri(uri);
                    self.state = DecodeState::Version;
                    *buffer = &buffer[index + 1..];
                    Complete(())
                }
                Err(error) => Error(error.into()),
            },
            None if buffer.len() >= max_length => Error(DecodeError::URITooLong),
            None => Incomplete,
        }
    }

    /// Decodes a version from the buffer.
    fn decode_version<'buffer>(&mut self, buffer: &mut &'buffer [u8]) -> DecodeResult<()> {
        use self::GenericDecodeResult::*;

        if buffer.len() < 10 {
            return Incomplete;
        }

        if &buffer[8..10] != b"\r\n" {
            return Error(DecodeError::Version(VersionError::Invalid));
        }

        match Version::try_from(&buffer[0..8]) {
            Ok(version) if version == Version::RTSP20 => {
                self.builder.version(version);
                self.state = DecodeState::Header;
                *buffer = &buffer[10..];
                Complete(())
            }
            Ok(_) => Error(DecodeError::UnsupportedVersion),
            Err(error) => Error(error.into()),
        }
    }

    /// Constructs a new request decoder.
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
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::method::Method;
    /// use rtsp::protocol::codec::decoder::request::{
    ///     DecodeError, DecodeState, Decoder,
    /// };
    /// use rtsp::request::Request;
    /// use rtsp::uri::PathError;
    /// use rtsp::uri::request::{URI, URIError};
    ///
    /// # fn main() {
    /// let buffer = "Bad Request!\r\nExtra garbage data";
    /// let mut decoder = Decoder::new();
    ///
    /// let (result, _) = decoder.decode(buffer);
    /// assert_eq!(
    ///     decoder.state(),
    ///     DecodeState::Error(DecodeError::URI(URIError::Path(PathError::InvalidCharacter)))
    /// );
    /// assert!(result.is_error());
    ///
    /// decoder.reset();
    ///
    /// assert_eq!(decoder.state(), DecodeState::Method);
    /// # }
    /// ```
    pub fn reset(&mut self) {
        self.builder = RequestBuilder::new();
        self.content_length = ContentLength::default();
        self.state = DecodeState::Method;
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
    /// use rtsp::method::Method;
    /// use rtsp::protocol::codec::decoder::request::{DecodeState, Decoder};
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "SETUP * RTSP/2.0\r\n\
    ///      Content-Length: 4\r\n\
    ///      \r\n\
    ///      Body";
    /// let mut decoder = Decoder::new();
    /// assert_eq!(decoder.state(), DecodeState::Method);
    ///
    /// let (result, bytes_decoded) = decoder.decode(buffer);
    /// let expected_request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("4").unwrap())
    ///     .with_body(BytesMut::from("Body".as_bytes()))
    ///     .build()
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_decoded, buffer.len());
    /// assert_eq!(decoder.state(), DecodeState::Method);
    /// # }
    /// ```
    pub fn state(&self) -> DecodeState<DecodeError> {
        self.state.clone()
    }

    /// Constructs a new request decoder with the given configuration.
    pub fn with_config(config: Config) -> Self {
        Decoder {
            builder: RequestBuilder::new(),
            config,
            content_length: ContentLength::default(),
            state: DecodeState::Method,
        }
    }
}

impl Default for Decoder {
    fn default() -> Self {
        Decoder::new()
    }
}

/// An error type for when the request was invalid.
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

    /// There was an error decoding the method.
    Method(MethodError),

    /// The method was too long.
    MethodTooLong,

    /// The request contained too many headers.
    TooManyHeaders,

    /// There was an error decoding the URI.
    URI(URIError),

    /// The URI was too long.
    URITooLong,

    /// The version was unsupported.
    UnsupportedVersion,

    /// There was an error decoding the version.
    Version(VersionError),
}

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
            Method(error) => error.fmt(formatter),
            MethodTooLong => write!(formatter, "method too long"),
            TooManyHeaders => write!(formatter, "too many headers"),
            URI(error) => error.fmt(formatter),
            URITooLong => write!(formatter, "URI too long"),
            UnsupportedVersion => write!(formatter, "unsupported version"),
            Version(error) => error.fmt(formatter),
        }
    }
}

impl Error for DecodeError {}

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

impl From<MethodError> for DecodeError {
    fn from(value: MethodError) -> Self {
        DecodeError::Method(value)
    }
}

impl From<URIError> for DecodeError {
    fn from(value: URIError) -> Self {
        DecodeError::URI(value)
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
    use crate::method::MethodError;
    use crate::protocol::codec::decoder::request::{
        ConfigBuilder, DecodeError, DecodeResult, Decoder,
    };
    use crate::uri::request::URIError;
    use crate::version::VersionError;

    #[test]
    fn test_decoder_decode_body_invalid_content_length() {
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
        let buffer = "SETUP * RTSP/2.0\r\n\
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
    fn test_decoder_decode_method_empty() {
        let buffer = " * RTSP/2.0\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::Method(MethodError::Empty))
        );
    }

    #[test]
    fn test_decoder_decode_method_invalid_character() {
        let buffer = "GET/ * RTSP/2.0\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::Method(MethodError::InvalidCharacter))
        );
    }

    #[test]
    fn test_decoder_decode_method_starts_with_dollar_sign() {
        let buffer = "$GET * RTSP/2.0\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::Method(MethodError::StartsWithDollarSign))
        );
    }

    #[test]
    fn test_decoder_decode_method_too_long() {
        let buffer = "MY_LONG_METHOD_NAME * RTSP/2.0\r\n\
                      \r\n";
        let config = ConfigBuilder::new().with_method_max_length(8).build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::MethodTooLong));
    }

    #[test]
    fn test_decoder_decode_uri_host_empty() {
        let buffer = "GET rtsp:///my/long/uri/path RTSP/2.0\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(
            result,
            DecodeResult::Error(DecodeError::URI(URIError::EmptyHost))
        );
    }

    #[test]
    fn test_decoder_decode_uri_too_long() {
        let buffer = "GET rtsp://example.com/my/long/uri/path RTSP/2.0\r\n\
                      \r\n";
        let config = ConfigBuilder::new().with_uri_max_length(20).build();
        let mut decoder = Decoder::with_config(config);
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::URITooLong));
    }

    #[test]
    fn test_decoder_decode_version_invalid() {
        let buffer = "SETUP * RTSP/2.0!\r\n\
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
        let buffer = "SETUP * RTSP/2.1\r\n\
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
        let buffer = "SETUP * RTSP/1.0\r\n\
                      \r\n";
        let mut decoder = Decoder::new();
        let (result, bytes_decoded) = decoder.decode(buffer);
        assert_ne!(bytes_decoded, buffer.len());
        assert_eq!(result, DecodeResult::Error(DecodeError::UnsupportedVersion));
    }
}
