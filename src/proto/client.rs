use bytes::BytesMut;
use std::mem::replace;
use std::{error, fmt, io};
use tokio_io::codec::{Decoder, Encoder};

use super::{consume_line, get_content_length, parse_header, trim_header};
use header::{HeaderName, HeaderValue};
use header::types::ContentLength;
use request::Request;
use response::{Builder, BuilderError, Response};
use version::Version;

pub struct ClientCodec {
    body: Option<BytesMut>,
    builder: Builder,
    content_length: ContentLength,
    state: ParseState,
}

#[derive(Eq, PartialEq)]
pub enum ParseState {
    Body,
    End,
    Header,
    ResponseLine,
}

impl ClientCodec {
    /// This function more so just extracts the body with a length determined by the content length
    /// than it does parse it. This decoder does not try to parse the body based on the content
    /// type, this should be done at a higher level.
    fn parse_body(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        if *self.content_length > buffer.len() {
            None
        } else {
            self.state = ParseState::End;
            self.body = Some(buffer.split_to(*self.content_length));
            Some(Ok(()))
        }
    }

    /// Parses a header of the response. If no more headers are present, it will parse the content
    /// length header and proceed to the next parse state.
    fn parse_header(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match parse_header(buffer) {
            Some(Ok(Some((name, value)))) => {
                self.builder
                    .header(trim_header(name.as_ref()), value.as_ref());
                Some(Ok(()))
            }
            Some(Ok(None)) => {
                self.state = ParseState::Body;
                let entry = self.builder
                    .headers
                    .entry(HeaderName::ContentLength)
                    .unwrap();

                match get_content_length(entry) {
                    Ok(content_length) => self.content_length = content_length,
                    Err(error) => return Some(Err(error)),
                }

                Some(Ok(()))
            }
            Some(Err(error)) => Some(Err(error)),
            None => None,
        }
    }

    /// Parses the response line of the response. Any empty newlines prior to the response line are
    /// ignored. As long as the response line is of the form `VERSION STATUS_CODE REASON_PHRASE\r\n`
    /// where `VERSION`, `STATUS_CODE`, and `REASON_PHRASE` are not necessarily valid values, the
    /// reseponse will continue to be parsed. This allows for handling bad responses due to invalid
    /// reason phrase characters for example. If the response line is not of that form, then there
    /// is no way to recover, so the connection will just be closed on return of the IO error. An IO
    /// error will also be returned if the RTSP version given is not RTSP/2.0 since that is the only
    /// version this implementation supports.
    fn parse_response_line(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match consume_line(buffer) {
            Some((_, 0)) => {
                buffer.split_to(2);
                None
            }
            Some((mut line, _)) => {
                if let Some(i) = line.iter().position(|&b| b == b' ') {
                    let version = line.split_to(i);
                    line.split_to(1);

                    if let Some(i) = line.iter().position(|&b| b == b' ') {
                        let status_code = line.split_to(i);
                        line.split_to(1);

                        self.state = ParseState::Header;
                        self.builder
                            .version(version.as_ref())
                            .status_code(status_code.as_ref())
                            .reason(Some(line.as_ref()));

                        if self.builder.version != Version::RTSP20 {
                            return Some(Err(io::Error::new(
                                io::ErrorKind::Other,
                                "unsupported RTSP version".to_string(),
                            )));
                        }

                        return Some(Ok(()));
                    }
                }

                // Request line was not of the form `VERSION STATUS_CODE REASON_PHRASE\r\n`.

                Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "failed to parse request line".to_string(),
                )))
            }
            None => None,
        }
    }
}

impl Decoder for ClientCodec {
    type Item = Result<Response<BytesMut>, InvalidResponse>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        use self::ParseState::*;

        loop {
            let parse_result = match self.state {
                Body => self.parse_body(buffer),
                Header => self.parse_header(buffer),
                ResponseLine => self.parse_response_line(buffer),
                End => {
                    let request = self.builder
                        .build(replace(&mut self.body, None).unwrap())
                        .map_err(|error| InvalidResponse::from(error));
                    self.builder = Builder::new();
                    self.state = ResponseLine;

                    break Ok(Some(request));
                }
            };

            match parse_result {
                None => break Ok(None),
                Some(Err(error)) => break Err(error),
                _ => continue,
            }
        }
    }
}

impl Encoder for ClientCodec {
    type Item = Request<BytesMut>;
    type Error = io::Error;

    fn encode(&mut self, mut message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        // Force `Content-Length` to be correctly set.

        let body_size =
            unsafe { HeaderValue::from_str_unchecked(message.body().len().to_string()) };
        message
            .headers_mut()
            .insert(HeaderName::ContentLength, body_size);

        buffer.extend(message.method().as_str().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.uri().as_str().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.version().as_str().as_bytes());
        buffer.extend(b"\r\n");

        for (name, value) in message.headers().iter() {
            buffer.extend(name.canonical_name().as_bytes());
            buffer.extend(b": ");
            buffer.extend(value.as_bytes());
            buffer.extend(b"\r\n");
        }

        buffer.extend(b"\r\n");
        buffer.extend(message.body());

        Ok(())
    }
}

impl Default for ClientCodec {
    fn default() -> Self {
        ClientCodec {
            body: None,
            builder: Builder::new(),
            content_length: ContentLength::from(0),
            state: ParseState::ResponseLine,
        }
    }
}

/// An error type for when the request was successfully parsed but contained invalid values.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidResponse {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidStatusCode,
    InvalidVersion,
    MissingReasonPhrase,
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

        match self {
            &InvalidHeaderName => "invalid RTSP request - invalid header name",
            &InvalidHeaderValue => "invalid RTSP request-  invalid header value",
            &InvalidReasonPhrase => "invalid RTSP request-  invalid reason phrase",
            &InvalidStatusCode => "invalid RTSP request - invalid status code",
            &InvalidVersion => "invalid RTSP request - invalid version",
            _ => panic!("rest of `BuilderError`s should not be accessible"),
        }
    }
}

impl From<BuilderError> for InvalidResponse {
    fn from(value: BuilderError) -> InvalidResponse {
        use self::InvalidResponse::*;

        match value {
            BuilderError::InvalidHeaderName => InvalidHeaderName,
            BuilderError::InvalidHeaderValue => InvalidHeaderValue,
            BuilderError::InvalidReasonPhrase => InvalidReasonPhrase,
            BuilderError::InvalidStatusCode => InvalidStatusCode,
            BuilderError::InvalidVersion => InvalidVersion,
            _ => panic!("rest of `BuilderError`s should not be accessible"),
        }
    }
}
