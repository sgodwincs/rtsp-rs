//! RTSP Request/Response Decoders
//!
//! This module contains both request and response decoders that can decode a request/response from
//! a given buffer. These decoders are stateful and allow for data to be given incrementally through
//! a series of calls to the `decode` functions.
//!
//! # Examples
//!
//! Decoding a request:
//!
//! ```
//! # extern crate bytes;
//! # extern crate rtsp;
//! #
//! use bytes::BytesMut;
//!
//! use rtsp::{HeaderName, Request};
//! use rtsp::protocol::RequestDecoder;
//!
//! # fn main() {
//! let buffer =
//!     "SETUP * RTSP/2.0\r\n\
//!      Content-Length: 4\r\n\
//!      \r\n\
//!      Body";
//! let mut decoder = RequestDecoder::new();
//! let (result, bytes_parsed) = decoder.decode(buffer);
//! let expected_request = Request::builder()
//!     .method("SETUP")
//!     .uri("*")
//!     .header(HeaderName::ContentLength, " 4")
//!     .build(BytesMut::from("Body".as_bytes()))
//!     .unwrap();
//!
//! assert_eq!(result.unwrap(), expected_request);
//! assert_eq!(bytes_parsed, buffer.len());
//! # }
//! ```
//!
//! Decoding a response:
//!
//! ```
//! # extern crate bytes;
//! # extern crate rtsp;
//! #
//! use bytes::BytesMut;
//!
//! use rtsp::{HeaderName, Response};
//! use rtsp::protocol::ResponseDecoder;
//!
//! # fn main() {
//! let buffer =
//!     "RTSP/2.0 200 OK\r\n\
//!      Content-Length: 4\r\n\
//!      \r\n\
//!      Body";
//! let mut decoder = ResponseDecoder::new();
//! let (result, bytes_parsed) = decoder.decode(buffer);
//! let expected_response = Response::builder()
//!     .header(HeaderName::ContentLength, " 4")
//!     .build(BytesMut::from("Body".as_bytes()))
//!     .unwrap();
//!
//! assert_eq!(result.unwrap(), expected_response);
//! assert_eq!(bytes_parsed, buffer.len());
//! # }
//! ```

use bytes::BytesMut;
use std::convert::TryFrom;
use std::mem::replace;
use std::{error, fmt};

use header::types::ContentLength;
use header::{Entry, HeaderName, HeaderValue, TypedHeader};
use request::{Builder as RequestBuilder, BuilderError as RequestBuilderError, Request};
use response::{Builder as ResponseBuilder, BuilderError as ResponseBuilderError, Response};

/// The current state of the request/response parsing.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum ParseState<E> {
    /// The decoder is currently at the stage of parsing the request/response body.
    Body,

    /// The decoder just finished parsing a request/response. With regards to the `parse_state`
    /// functions, the returned parse state will never be this variant. This is used internally
    /// in the implementation but a decoding step never finishes on this state.
    End,

    /// The decoder is currently at the stage of parsing a request/response header. The state will
    /// stay here until all headers have been parsed.
    Header,

    /// The decoder is currently at the stage of parsing the request line or response line. Any
    /// empty newlines before the actual line are ignored.
    InfoLine,

    /// The decoder has encountered an error that it cannot recover from. Decoding cannot continue
    /// from this state, and the decoder must be reset before continuing to decode something else.
    IrrecoverableError(E),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
/// The result of request/response parsing.
pub enum ParseResult<T, E> {
    /// The decoder has succesfully parsed the request/response from the given buffer, and it is
    /// returned within this variant.
    ///
    /// Note that the given buffer for the `decode` function may still contain more request/response
    /// data that can be decoded even after this is returned. This is because the `decode` function
    /// will only decode up to the end of a request/response. In this case, you need to keep
    /// decoding the buffer until either an error occurs or more data is needed (which may just be
    /// "EOF").
    Complete(T),

    /// The decoder has encountered an error during parsing. However, this error may be recoverable
    /// and the error itself needs to be checked. If it is recoverable, the decoder will still be
    /// in a valid parsing state and can continue decoding the same buffer it had been before. But
    /// if the error is irrecoverable, the decoder is in a state where it cannot continue decoding
    /// without being reset. Also, although the number of bytes parsed is returned even when an
    /// irrecoverable error occurs, this should not be used to indicate the exact position where
    /// the error occured, since this is dependent on the implementation.
    ///
    /// Note that if the error is recoverable, this means that the decoder did successfully parse
    /// the given request/response completely and that the "stream" is not corrupt.
    Error(E),

    /// The decoder has not encountered an irrecoverable (though it may have encountered a
    /// recoverable error) but cannot continue decoding without more data. In this case, everything
    /// that has been parsed is stored within the decoder and another call to the decoder with more
    /// data can finish the decoding.
    ///
    /// Note that the given buffer for the `decode` function may not have been completely parsed and
    /// should be extended with the rest of the request/response data. It is also not completely
    /// clear that more data is actually needed. If you were to give the `decode `function a buffer
    /// that contained just one request and called it again afterwards on the rest of the same
    /// buffer, you would get an `Incomplete`. But this would just be synonymous with "EOF" since a
    /// partial request/response was not actually decoded. In this case, it is up to the caller to
    /// figure out whether or not more data is needed or if it was just "EOF".
    Incomplete,
}

impl<T, E> ParseResult<T, E> {
    /// Returns `true` if the parse result is `Complete`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Complete(());
    /// assert_eq!(parse_result.is_complete(), true);
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Error(());
    /// assert_eq!(parse_result.is_complete(), false);
    /// ```
    pub fn is_complete(&self) -> bool {
        match *self {
            ParseResult::Complete(_) => true,
            _ => false,
        }
    }

    /// Returns `true` if the parse result is `Error`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Error(());
    /// assert_eq!(parse_result.is_error(), true);
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Incomplete;
    /// assert_eq!(parse_result.is_error(), false);
    /// ```
    pub fn is_error(&self) -> bool {
        match *self {
            ParseResult::Error(_) => true,
            _ => false,
        }
    }

    /// Returns `true` if the parse result is `Incomplete`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Incomplete;
    /// assert_eq!(parse_result.is_incomplete(), true);
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Error(());
    /// assert_eq!(parse_result.is_incomplete(), false);
    /// ```
    pub fn is_incomplete(&self) -> bool {
        match *self {
            ParseResult::Incomplete => true,
            _ => false,
        }
    }

    /// Maps the value within the `Complete` variant into the value returned from `f`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Complete(());
    /// assert_eq!(parse_result.map(|_| 5).unwrap(), 5);
    /// ```
    pub fn map<U, F>(self, f: F) -> ParseResult<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            ParseResult::Complete(value) => ParseResult::Complete(f(value)),
            ParseResult::Error(error) => ParseResult::Error(error),
            ParseResult::Incomplete => ParseResult::Incomplete,
        }
    }

    /// Maps the value within the `Complete` variant into the value returned from `f`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Error(());
    /// assert_eq!(parse_result.map_error(|_| 5).unwrap_error(), 5);
    /// ```
    pub fn map_error<U, F>(self, f: F) -> ParseResult<T, U>
    where
        F: FnOnce(E) -> U,
    {
        match self {
            ParseResult::Complete(value) => ParseResult::Complete(value),
            ParseResult::Error(error) => ParseResult::Error(f(error)),
            ParseResult::Incomplete => ParseResult::Incomplete,
        }
    }

    /// Unwraps the parse result, yielding the content of the `Complete`.
    ///
    /// # Panics
    ///
    /// Panics if the value is not `Complete`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Complete(());
    /// assert_eq!(parse_result.unwrap(), ());
    /// ```
    ///
    /// ```{.should_panic}
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Incomplete;
    /// parse_result.unwrap();
    /// ```
    pub fn unwrap(self) -> T {
        match self {
            ParseResult::Complete(value) => value,
            _ => panic!("unwrapped `ParseResult` that was not `Complete`"),
        }
    }

    /// Unwraps the parse result, yielding the content of the `Error`.
    ///
    /// # Panics
    ///
    /// Panics if the value is not `Error`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Error(());
    /// assert_eq!(parse_result.unwrap_error(), ());
    /// ```
    ///
    /// ```{.should_panic}
    /// use rtsp::protocol::ParseResult;
    ///
    /// let parse_result: ParseResult<(), ()> = ParseResult::Incomplete;
    /// parse_result.unwrap_error();
    /// ```
    pub fn unwrap_error(self) -> E {
        match self {
            ParseResult::Error(error) => error,
            _ => panic!("unwrapped `ParseResult` that was not `Error`"),
        }
    }
}

impl<T, E> From<Result<T, E>> for ParseResult<T, E> {
    fn from(value: Result<T, E>) -> ParseResult<T, E> {
        match value {
            Ok(value) => ParseResult::Complete(value),
            Err(error) => ParseResult::Error(error),
        }
    }
}

/// An alias for the parse result that is returned from the request decoder.
pub type RequestParseResult<T> = ParseResult<T, InvalidRequest>;

/// The decoder for decoding RTSP requests from a buffer. This decoder is stateful in that it can
/// partially parse a request if not enough data is present. In this case, the `decode` function
/// needs to be called again with the rest of the data.
///
/// # Examples
///
/// ```
/// # extern crate bytes;
/// # extern crate rtsp;
/// #
/// use bytes::BytesMut;
///
/// use rtsp::{HeaderName, Request};
/// use rtsp::protocol::RequestDecoder;
///
/// # fn main() {
/// let buffer =
///     "SETUP * RTSP/2.0\r\n\
///      Content-Length: 4\r\n\
///      \r\n\
///      Body";
/// let mut decoder = RequestDecoder::new();
/// let (result, bytes_parsed) = decoder.decode(buffer);
/// let expected_request = Request::builder()
///     .method("SETUP")
///     .uri("*")
///     .header(HeaderName::ContentLength, " 4")
///     .build(BytesMut::from("Body".as_bytes()))
///     .unwrap();
///
/// assert_eq!(result.unwrap(), expected_request);
/// assert_eq!(bytes_parsed, buffer.len());
/// # }
/// ```
#[derive(Debug)]
pub struct RequestDecoder {
    body: Option<BytesMut>,
    builder: RequestBuilder,
    content_length: ContentLength,
    state: ParseState<InvalidRequest>,
}

impl RequestDecoder {
    /// Constructs a new request decoder.
    pub fn new() -> Self {
        RequestDecoder {
            body: None,
            builder: RequestBuilder::new(),
            content_length: ContentLength::default(),
            state: ParseState::InfoLine,
        }
    }

    /// Attempts to decode a request from `buffer`. There are three different cases that can occur
    /// as the result of this function:
    ///
    ///  1. A request is successfully parsed and is returned along with the number of bytes parsed.
    ///     The function can be called again with the same buffer or some other data to continue
    ///     decoding.
    ///  2. A request has been partially parsed but more data is needed to finish decoding. The
    ///     function must be called again with the rest of the data in order to finish the decoding.
    ///
    ///     Note that in this case, it could be possible that it has already been detected that the
    ///     request is invalid, but the "stream" is not corrupt and parsing can continue.
    ///  3. An error occured during parsing. This can be further divided into two categories:
    ///     recoverable and irrecoverable errors.
    ///
    ///     If the error was irrecoverable, then the decoder is in a state in which decoding can no
    ///     longer occur. In this case, the decoder must be reset before any further decoding. An
    ///     irrecoverable is simply when no further assumptions can be made as to the structure of
    ///     the data. This can occur for instance if the RTSP version was invalid or unknown or if
    ///     there was a header line without a `":"`.
    ///
    ///     If the error was recoverable, then the entire request was parsed but was found to be
    ///     invalid. In this case, decoding can still continue from the same buffer (if there is
    ///     more data). An example of a recoverable error would be if the method contained invalid
    ///     characters.
    ///
    /// Note that as long as the decoder is in a valid parsing state, this function can be called
    /// repeatedly with more and more data. The state can be checked using the `parse_state`
    /// function. If the state is invalid, the decoder can be reset to its default state using the
    /// `reset` function.
    ///
    /// The number of bytes parsed is the number of bytes successfully parsed even if an error had
    /// occured. However, in the case of an irrecoverable error, more bytes could have been parsed
    /// than returned but because of the implementation, less than that was returned.
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
    ///
    /// use rtsp::{HeaderName, Request};
    /// use rtsp::protocol::RequestDecoder;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "GET_PARAMETER rtsp://example.com RTSP/2.0\r\n\
    ///      Content-Length: 4\r\n\
    ///      Session: QKyjN8nt2WqbWw4tIYof52; timeout = 60\r\n\
    ///      \r\n\
    ///      Body";
    /// let mut decoder = RequestDecoder::new();
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// let expected_request = Request::builder()
    ///     .method("GET_PARAMETER")
    ///     .uri("rtsp://example.com")
    ///     .header(HeaderName::ContentLength, " 4")
    ///     .header(HeaderName::Session, " QKyjN8nt2WqbWw4tIYof52; timeout = 60")
    ///     .build(BytesMut::from("Body".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_parsed, buffer.len());
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
    ///
    /// use rtsp::{HeaderName, Request};
    /// use rtsp::protocol::RequestDecoder;
    ///
    /// # fn main() {
    /// let buffer = "SETUP * RTSP/2.0\r\n";
    /// let mut decoder = RequestDecoder::new();
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    ///
    /// assert!(result.is_incomplete());
    /// assert_eq!(bytes_parsed, buffer.len());
    ///
    /// let buffer =
    ///     "Content-Length: 5\r\n\
    ///     \r\n\
    ///     Test!";
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    ///
    /// let expected_request = Request::builder()
    ///     .method("SETUP")
    ///     .uri("*")
    ///     .header(HeaderName::ContentLength, " 5")
    ///     .build(BytesMut::from("Test!".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_parsed, buffer.len());
    /// # }
    /// ```
    pub fn decode<B>(&mut self, buffer: B) -> (RequestParseResult<Request<BytesMut>>, usize)
    where
        B: AsRef<[u8]>,
    {
        use self::ParseResult::*;
        use self::ParseState::*;

        let mut buffer = buffer.as_ref();
        let buffer_size = buffer.len();

        loop {
            let parse_result = match self.state {
                IrrecoverableError(error) => break (Error(error), 0),
                InfoLine => self.parse_request_line(&mut buffer),
                Header => self.parse_header(&mut buffer),
                Body => self.parse_body(&mut buffer),
                End => {
                    let request = self.builder
                        .build(replace(&mut self.body, None).unwrap())
                        .map_err(|error| {
                            InvalidRequest::try_from(error).expect("unexpected `BuilderError`")
                        });
                    self.builder = RequestBuilder::new();
                    self.state = InfoLine;
                    break (request.into(), buffer_size - buffer.len());
                }
            };

            match parse_result {
                Complete(_) => continue,
                Error(error) => {
                    self.state = ParseState::IrrecoverableError(error);
                    break (Error(error), buffer_size - buffer.len());
                }
                Incomplete => break (Incomplete, buffer_size - buffer.len()),
            }
        }
    }

    /// This function more so just extracts the body with a length determined by the content length
    /// than it does parse it. This decoder does not try to parse the body based on the content
    /// type, this should be done at a higher level.
    fn parse_body<'a>(&mut self, buffer: &mut &'a [u8]) -> RequestParseResult<()> {
        use self::ParseResult::*;

        if *self.content_length > buffer.len() {
            Incomplete
        } else {
            let (body, unparsed_buffer) = buffer.split_at(*self.content_length);
            *buffer = unparsed_buffer;

            self.state = ParseState::End;
            self.body = Some(BytesMut::from(body));
            Complete(())
        }
    }

    /// Parses a header of the request. If no more headers are present, it will parse the content
    /// length header and proceed to the next parse state.
    fn parse_header<'a>(&mut self, buffer: &mut &'a [u8]) -> RequestParseResult<()> {
        use self::ParseResult::*;

        match parse_header(buffer) {
            Error(_) => Error(InvalidRequest::InvalidHeaderLine),
            Incomplete => Incomplete,
            Complete(None) => {
                self.state = ParseState::Body;
                let entry = self.builder
                    .headers
                    .entry(HeaderName::ContentLength)
                    .expect("`ContentLength` should be a valid `HeaderName`");

                match get_content_length(entry) {
                    Ok(content_length) => self.content_length = content_length,
                    Err(_) => return Error(InvalidRequest::InvalidContentLength),
                }

                Complete(())
            }
            Complete(Some((name, value))) => {
                let name = trim_header(name);
                self.builder.header(name, value);
                Complete(())
            }
        }
    }

    /// Parses the request line of the request. Any empty newlines prior to the request line are
    /// ignored. As long as the request line is of the form `METHOD URI VERSION\r\n` where `METHOD`,
    /// `URI`, and `VERSION` are not necessarily valid values, the request will continue to be
    /// parsed. This allows for handling bad requests due to invalid method characters for example.
    /// If the request line is not of that form, then there is no way to recover, so parsing cannot
    /// continue any further.
    fn parse_request_line<'a>(&mut self, buffer: &mut &'a [u8]) -> RequestParseResult<()> {
        use self::ParseResult::*;
        use self::RequestBuilderError as BuilderError;

        loop {
            break match get_line(buffer) {
                Some((_, 0)) => continue,
                Some((line, _)) => match split_info_line(line) {
                    Some((method, uri, version)) => {
                        self.state = ParseState::Header;
                        self.builder.version(version);

                        if self.builder.error == Some(BuilderError::InvalidVersion) {
                            Error(InvalidRequest::InvalidVersion)
                        } else if self.builder.error == Some(BuilderError::UnsupportedVersion) {
                            Error(InvalidRequest::UnsupportedVersion)
                        } else {
                            self.builder.method(method).uri(uri);
                            Complete(())
                        }
                    }
                    None => Error(InvalidRequest::InvalidRequestLine),
                },
                None => Incomplete,
            };
        }
    }

    /// Returns the current parsing state. If the parse state is the `IrrecoverableError` variant,
    /// it will include the error.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    ///
    /// use rtsp::{HeaderName, Request};
    /// use rtsp::protocol::{ParseState, RequestDecoder};
    ///
    /// # fn main() {
    /// let buffer =
    ///     "SETUP * RTSP/2.0\r\n\
    ///      Content-Length: 4\r\n\
    ///      \r\n\
    ///      Body";
    /// let mut decoder = RequestDecoder::new();
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// let expected_request = Request::builder()
    ///     .method("SETUP")
    ///     .uri("*")
    ///     .header(HeaderName::ContentLength, " 4")
    ///     .build(BytesMut::from("Body".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_request);
    /// assert_eq!(bytes_parsed, buffer.len());
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    /// # }
    /// ```
    pub fn parse_state(&self) -> ParseState<InvalidRequest> {
        self.state.clone()
    }

    /// Resets the decoder state back to its default state. This is useful if the decoder goes into
    /// an invalid state if an irrecoverable error had occurred.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    ///
    /// use rtsp::HeaderName;
    /// use rtsp::protocol::{InvalidRequest, ParseState, RequestDecoder};
    ///
    /// # fn main() {
    /// let buffer = "Bad Request!\r\nExtra garbage data";
    /// let mut decoder = RequestDecoder::new();
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// assert_eq!(
    ///     decoder.parse_state(),
    ///     ParseState::IrrecoverableError(InvalidRequest::InvalidRequestLine)
    /// );
    /// assert!(result.is_error());
    /// assert!(!result.unwrap_error().is_recoverable());
    ///
    /// decoder.reset();
    ///
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    /// # }
    /// ```
    pub fn reset(&mut self) {
        self.body = None;
        self.builder = RequestBuilder::new();
        self.content_length = ContentLength::default();
        self.state = ParseState::InfoLine;
    }
}

/// An error type for when the request was invalid. Within the set of errors exist two subsets:
/// recoverable and irrecoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidRequest {
    InvalidContentLength,
    InvalidHeaderLine,
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidRequestLine,
    InvalidURI,
    InvalidVersion,
    UnsupportedVersion,
}

impl InvalidRequest {
    /// Returns whether or not the error is recoverable.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::InvalidRequest;
    ///
    /// assert!(InvalidRequest::InvalidHeaderName.is_recoverable());
    /// assert!(!InvalidRequest::InvalidContentLength.is_recoverable());
    /// ```
    pub fn is_recoverable(&self) -> bool {
        use self::InvalidRequest::*;

        match *self {
            InvalidContentLength | InvalidHeaderLine | InvalidRequestLine | InvalidVersion
            | UnsupportedVersion => false,
            _ => true,
        }
    }
}

impl fmt::Display for InvalidRequest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for InvalidRequest {
    fn description(&self) -> &str {
        use self::InvalidRequest::*;

        match *self {
            InvalidContentLength => "invalid RTSP request - invalid content length",
            InvalidHeaderLine => "invalid RTSP request - invalid header line",
            InvalidHeaderName => "invalid RTSP request - invalid header name",
            InvalidHeaderValue => "invalid RTSP request - invalid header value",
            InvalidMethod => "invalid RTSP request - invalid reason phrase",
            InvalidRequestLine => "invalid RTSP request - invalid request line",
            InvalidURI => "invalid RTSP request - invalid status code",
            InvalidVersion => "invalid RTSP request - invalid version",
            UnsupportedVersion => "invalid RTSP request - unsupported version",
        }
    }
}

impl TryFrom<RequestBuilderError> for InvalidRequest {
    type Error = ();

    fn try_from(value: RequestBuilderError) -> Result<InvalidRequest, Self::Error> {
        use self::InvalidRequest::*;

        match value {
            RequestBuilderError::InvalidHeaderName => Ok(InvalidHeaderName),
            RequestBuilderError::InvalidHeaderValue => Ok(InvalidHeaderValue),
            RequestBuilderError::InvalidMethod => Ok(InvalidMethod),
            RequestBuilderError::InvalidURI => Ok(InvalidURI),
            _ => Err(()),
        }
    }
}

/// An alias for the parse result that is returned from the response decoder.
pub type ResponseParseResult<T> = ParseResult<T, InvalidResponse>;

/// The decoder for decoding RTSP responses from a buffer. This decoder is stateful in that it can
/// partially parse a response if not enough data is present. In this case, the `decode` function
/// needs to be called again with the rest of the data.
///
/// # Examples
///
/// ```
/// # extern crate bytes;
/// # extern crate rtsp;
/// #
/// use bytes::BytesMut;
///
/// use rtsp::{HeaderName, Response};
/// use rtsp::protocol::ResponseDecoder;
///
/// # fn main() {
/// let buffer =
///     "RTSP/2.0 200 OK\r\n\
///      Content-Length: 4\r\n\
///      \r\n\
///      Body";
/// let mut decoder = ResponseDecoder::new();
/// let (result, bytes_parsed) = decoder.decode(buffer);
/// let expected_response = Response::builder()
///     .header(HeaderName::ContentLength, " 4")
///     .build(BytesMut::from("Body".as_bytes()))
///     .unwrap();
///
/// assert_eq!(result.unwrap(), expected_response);
/// assert_eq!(bytes_parsed, buffer.len());
/// # }
/// ```
#[derive(Debug)]
pub struct ResponseDecoder {
    body: Option<BytesMut>,
    builder: ResponseBuilder,
    content_length: ContentLength,
    state: ParseState<InvalidResponse>,
}

impl ResponseDecoder {
    pub fn new() -> Self {
        ResponseDecoder {
            body: None,
            builder: ResponseBuilder::new(),
            content_length: ContentLength::default(),
            state: ParseState::InfoLine,
        }
    }

    /// Attempts to decode a response from `buffer`. There are three different cases that can occur
    /// as the result of this function:
    ///
    ///  1. A response is successfully parsed and is returned along with the number of bytes parsed.
    ///     The function can be called again with the rest of the same buffer or some other data to
    ///     continue decoding.
    ///  2. A response has been partially parsed but more data is needed to finish decoding. The
    ///     function must be called again with the rest of the data in order to finish the decoding.
    ///
    ///     Note that in this case, it could be possible that it has already been detected that the
    ///     response is invalid, but the "stream" is not corrupt and parsing can continue.
    ///  3. An error occured during parsing. This can be further divided into two categories:
    ///     recoverable and irrecoverable errors.
    ///
    ///     If the error was irrecoverable, then the decoder is in a state in which decoding can no
    ///     longer occur. In this case, the decoder must be reset before any further decoding. An
    ///     irrecoverable is simply when no further assumptions can be made as to the structure of
    ///     the data. This can occur for instance if the RTSP version was invalid or unknown or if
    ///     there was a header line without a `":"`.
    ///
    ///     If the error was recoverable, then the entire response was parsed but was found to be
    ///     invalid. In this case, decoding can still continue from the same buffer (if there is
    ///     more data). An example of a recoverable error would be if the method contained invalid
    ///     characters.
    ///
    /// Note that as long as the decoder is in a valid parsing state, this function can be called
    /// repeatedly with more and more data. The state can be checked using the `parse_state`
    /// function. If the state is invalid, the decoder can be reset to its default state using the
    /// `reset` function.
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
    ///
    /// use rtsp::{HeaderName, Response};
    /// use rtsp::protocol::ResponseDecoder;
    ///
    /// # fn main() {
    /// let buffer =
    ///     "RTSP/2.0 200 OK\r\n\
    ///      Content-Length: 5\r\n\
    ///      \r\n\
    ///      Test!";
    /// let mut decoder = ResponseDecoder::new();
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// let expected_response = Response::builder()
    ///     .header(HeaderName::ContentLength, " 5")
    ///     .build(BytesMut::from("Test!".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_parsed, buffer.len());
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
    ///
    /// use rtsp::{HeaderName, Response};
    /// use rtsp::protocol::ResponseDecoder;
    ///
    /// # fn main() {
    /// let buffer = "RTSP/2.0 200 OK\r\n";
    /// let mut decoder = ResponseDecoder::new();
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    ///
    /// assert!(result.is_incomplete());
    /// assert_eq!(bytes_parsed, buffer.len());
    ///
    /// let buffer =
    ///     "Content-Length: 5\r\n\
    ///      \r\n\
    ///      Test!";
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    ///
    /// let expected_response = Response::builder()
    ///     .header(HeaderName::ContentLength, " 5")
    ///     .build(BytesMut::from("Test!".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_parsed, buffer.len());
    /// # }
    /// ```
    pub fn decode<B>(&mut self, buffer: B) -> (ResponseParseResult<Response<BytesMut>>, usize)
    where
        B: AsRef<[u8]>,
    {
        use self::ParseResult::*;
        use self::ParseState::*;

        let mut buffer = buffer.as_ref();
        let buffer_size = buffer.len();

        loop {
            let parse_result = match self.state {
                IrrecoverableError(error) => break (Error(error), 0),
                InfoLine => self.parse_response_line(&mut buffer),
                Header => self.parse_header(&mut buffer),
                Body => self.parse_body(&mut buffer),
                End => {
                    let response = self.builder
                        .build(replace(&mut self.body, None).unwrap())
                        .map_err(|error| {
                            InvalidResponse::try_from(error).expect("unexpected `BuilderError`")
                        });
                    self.builder = ResponseBuilder::new();
                    self.state = InfoLine;
                    break (response.into(), buffer_size - buffer.len());
                }
            };

            match parse_result {
                Complete(_) => continue,
                Error(error) => {
                    self.state = ParseState::IrrecoverableError(error);
                    break (Error(error), buffer_size - buffer.len());
                }
                Incomplete => break (Incomplete, buffer_size - buffer.len()),
            }
        }
    }

    /// This function more so just extracts the body with a length determined by the content length
    /// than it does parse it. This decoder does not try to parse the body based on the content
    /// type, this should be done at a higher level.
    fn parse_body<'a>(&mut self, buffer: &mut &'a [u8]) -> ResponseParseResult<()> {
        use self::ParseResult::*;

        if *self.content_length > buffer.len() {
            Incomplete
        } else {
            let (body, unparsed_buffer) = buffer.split_at(*self.content_length);
            *buffer = unparsed_buffer;

            self.state = ParseState::End;
            self.body = Some(BytesMut::from(body));
            Complete(())
        }
    }

    /// Parses a header of the response. If no more headers are present, it will parse the content
    /// length header and proceed to the next parse state.
    fn parse_header<'a>(&mut self, buffer: &mut &'a [u8]) -> ResponseParseResult<()> {
        use self::ParseResult::*;

        match parse_header(buffer) {
            Error(_) => Error(InvalidResponse::InvalidHeaderLine),
            Incomplete => Incomplete,
            Complete(None) => {
                self.state = ParseState::Body;
                let entry = self.builder
                    .headers
                    .entry(HeaderName::ContentLength)
                    .expect("`ContentLength` should be a valid `HeaderName`");

                match get_content_length(entry) {
                    Ok(content_length) => self.content_length = content_length,
                    Err(_) => return Error(InvalidResponse::InvalidContentLength),
                }

                Complete(())
            }
            Complete(Some((name, value))) => {
                let name = trim_header(name);
                self.builder.header(name, value);
                Complete(())
            }
        }
    }

    /// Parses the response line of the response. Any empty newlines prior to the response line are
    /// ignored. As long as the response line is of the form `VERSION STATUS_CODE REASON_PHRASE\r\n`
    /// where `VERSION`, `STATUS_CODE`, and `REASON_PHRASE` are not necessarily valid values, the
    /// reseponse will continue to be parsed. This allows for handling bad responses due to invalid
    /// reason phrase characters for example. If the response line is not of that form, then there
    /// is no way to recover, so decoding cannot continue any further.
    fn parse_response_line<'a>(&mut self, buffer: &mut &'a [u8]) -> ResponseParseResult<()> {
        use self::ParseResult::*;
        use self::ResponseBuilderError as BuilderError;

        loop {
            break match get_line(buffer) {
                Some((_, 0)) => continue,
                Some((line, _)) => match split_info_line(line) {
                    Some((version, status_code, reason_phrase)) => {
                        self.state = ParseState::Header;
                        self.builder.version(version);

                        if self.builder.error == Some(BuilderError::InvalidVersion) {
                            Error(InvalidResponse::InvalidVersion)
                        } else if self.builder.error == Some(BuilderError::UnsupportedVersion) {
                            Error(InvalidResponse::UnsupportedVersion)
                        } else {
                            self.builder
                                .status_code(status_code)
                                .reason(Some(reason_phrase));
                            Complete(())
                        }
                    }
                    None => Error(InvalidResponse::InvalidResponseLine),
                },
                None => Incomplete,
            };
        }
    }

    /// Returns the current parsing state. If the parse state is the `IrrecoverableError` variant,
    /// it will include the error.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    ///
    /// use rtsp::{HeaderName, Response};
    /// use rtsp::protocol::{ParseState, ResponseDecoder};
    ///
    /// # fn main() {
    /// let buffer =
    ///    "RTSP/2.0 200 OK\r\n\
    ///     Content-Length: 4\r\n\
    ///     \r\n\
    ///     Body";
    /// let mut decoder = ResponseDecoder::new();
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// let expected_response = Response::builder()
    ///     .header(HeaderName::ContentLength, " 4")
    ///     .build(BytesMut::from("Body".as_bytes()))
    ///     .unwrap();
    ///
    /// assert_eq!(result.unwrap(), expected_response);
    /// assert_eq!(bytes_parsed, buffer.len());
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    /// # }
    /// ```
    pub fn parse_state(&self) -> ParseState<InvalidResponse> {
        self.state.clone()
    }

    /// Resets the decoder state back to its default state. This is useful if the decoder goes into
    /// an invalid state if an irrecoverable error had occurred.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate bytes;
    /// # extern crate rtsp;
    /// #
    /// use bytes::BytesMut;
    ///
    /// use rtsp::HeaderName;
    /// use rtsp::protocol::{InvalidResponse, ParseState, ResponseDecoder};
    ///
    /// # fn main() {
    /// let buffer = "Bad Response!\r\nExtra garbage data";
    /// let mut decoder = ResponseDecoder::new();
    ///
    /// let (result, bytes_parsed) = decoder.decode(buffer);
    /// assert_eq!(
    ///     decoder.parse_state(),
    ///     ParseState::IrrecoverableError(InvalidResponse::InvalidResponseLine)
    /// );
    /// assert!(result.is_error());
    /// assert!(!result.unwrap_error().is_recoverable());
    ///
    /// decoder.reset();
    ///
    /// assert_eq!(decoder.parse_state(), ParseState::InfoLine);
    /// # }
    /// ```
    pub fn reset(&mut self) {
        self.body = None;
        self.builder = ResponseBuilder::new();
        self.content_length = ContentLength::default();
        self.state = ParseState::InfoLine;
    }
}

/// An error type for when the response was invalid. Within the set of errors exist two subsets:
/// recoverable and irrecoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidResponse {
    InvalidContentLength,
    InvalidHeaderLine,
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidResponseLine,
    InvalidStatusCode,
    InvalidVersion,
    UnsupportedVersion,
}

impl InvalidResponse {
    /// Returns whether or not the error is recoverable.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::InvalidRequest;
    ///
    /// assert!(InvalidRequest::InvalidHeaderName.is_recoverable());
    /// assert!(!InvalidRequest::InvalidContentLength.is_recoverable());
    /// ```
    pub fn is_recoverable(&self) -> bool {
        use self::InvalidResponse::*;

        match *self {
            InvalidContentLength | InvalidHeaderLine | InvalidResponseLine | InvalidVersion
            | UnsupportedVersion => false,
            _ => true,
        }
    }
}

impl fmt::Display for InvalidResponse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for InvalidResponse {
    fn description(&self) -> &str {
        use self::InvalidResponse::*;

        match *self {
            InvalidContentLength => "invalid RTSP response - invalid content length",
            InvalidHeaderLine => "invalid RTSP response - invalid header line",
            InvalidHeaderName => "invalid RTSP response - invalid header name",
            InvalidHeaderValue => "invalid RTSP response - invalid header value",
            InvalidReasonPhrase => "invalid RTSP response - invalid reason phrase",
            InvalidResponseLine => "invalid RTSP response - invalid response line",
            InvalidStatusCode => "invalid RTSP response - invalid status code",
            InvalidVersion => "invalid RTSP response - invalid version",
            UnsupportedVersion => "invalid RTSP response - unsupported version",
        }
    }
}

impl TryFrom<ResponseBuilderError> for InvalidResponse {
    type Error = ();

    fn try_from(value: ResponseBuilderError) -> Result<InvalidResponse, Self::Error> {
        use self::InvalidResponse::*;

        match value {
            ResponseBuilderError::InvalidHeaderName => Ok(InvalidHeaderName),
            ResponseBuilderError::InvalidHeaderValue => Ok(InvalidHeaderValue),
            ResponseBuilderError::InvalidReasonPhrase => Ok(InvalidReasonPhrase),
            ResponseBuilderError::InvalidStatusCode => Ok(InvalidStatusCode),
            _ => Err(()),
        }
    }
}

/// Consumes a line from the buffer, returning the line found and the index at which the `\r\n`
/// started.
fn get_line<'a>(buffer: &mut &'a [u8]) -> Option<(&'a [u8], usize)> {
    if let Some(i) = buffer.windows(2).position(|b| b == b"\r\n") {
        let (line, unparsed_buffer) = buffer.split_at(i);
        *buffer = &unparsed_buffer[2..];
        Some((line, i))
    } else {
        None
    }
}

/// Given the `Entry` for the `Content-Length` header (assuming use of `HeaderMap`), this function
/// attempts to parse the given header and return the content length, defaulting to 0 if the header
/// does not exist.
fn get_content_length(header_entry: Entry<HeaderValue>) -> Result<ContentLength, ()> {
    match header_entry {
        Entry::Occupied(entry) => if entry.iter().count() > 1 {
            Err(())
        } else {
            let header_values = &entry.iter().cloned().collect::<Vec<HeaderValue>>();
            ContentLength::try_from_header_raw(header_values).map_err(|_| ())
        },
        Entry::Vacant(_) => Ok(ContentLength::default()),
    }
}

/// A helper function to parse a header. This function will return two indices where one is
/// optional. The first index will be the end of the header, while the second index is an
/// `Option<usize>` of where the the header name and value are separated (the index of the `:`).
/// RTSP allows for having multiline headers as long as newlines contained within header values
/// start with a space or tab.
fn parse_header_multiline(buffer: &[u8]) -> ParseResult<Option<(usize, usize)>, ()> {
    use self::ParseResult::*;

    let mut iter = buffer
        .windows(2)
        .enumerate()
        .filter(|&(_, b)| b == b"\r\n")
        .map(|(i, _)| i);

    match iter.next() {
        Some(0) => Complete(None),
        Some(mut i) => if let Some(j) = buffer.iter().take(i).position(|&b| b == b':') {
            loop {
                match buffer.get(i + 2) {
                    Some(&b) if b == b' ' || b == b'\t' => match iter.next() {
                        Some(k) => i = k,
                        None => break Incomplete,
                    },
                    Some(_) => break Complete(Some((i, j))),
                    None => break Incomplete,
                }
            }
        } else {
            Error(())
        },
        None => Incomplete,
    }
}

/// Parses a header of the request/response.
fn parse_header<'a>(buffer: &mut &'a [u8]) -> ParseResult<Option<(&'a [u8], &'a [u8])>, ()> {
    use self::ParseResult::*;

    match parse_header_multiline(*buffer) {
        Error(_) => Error(()),
        Incomplete => Incomplete,
        Complete(None) => {
            *buffer = &(*buffer)[2..];
            Complete(None)
        }
        Complete(Some((i, j))) => {
            let (header, unparsed_buffer) = buffer.split_at(i);
            *buffer = &unparsed_buffer[2..];
            let (name, mut value) = header.split_at(j);
            value = &value[1..];
            Complete(Some((name, value)))
        }
    }
}

/// Splits the information line (either the request or response line) into three parts.
fn split_info_line(line: &[u8]) -> Option<(&[u8], &[u8], &[u8])> {
    if let Some(i) = line.iter().position(|&b| b == b' ') {
        let (part_1, mut line) = line.split_at(i);
        line = &line[1..];

        if let Some(i) = line.iter().position(|&b| b == b' ') {
            let (part_2, mut line) = line.split_at(i);
            line = &line[1..];
            return Some((part_1, part_2, line));
        }
    }

    None
}

/// Trims the `&[u8]` of any trailing spaces or tabs. It is possible that the header is actually
/// UTF-8 encoded and this function will create an invalid value, but since header names have to be
/// ASCII-US encoded, it should not cause any problems.
fn trim_header(mut header: &[u8]) -> &[u8] {
    while header.ends_with(&[b' ']) || header.ends_with(&[b'\t']) {
        header = &header[0..header.len() - 1];
    }

    header
}
