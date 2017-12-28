use bytes::BytesMut;
use std::{error, fmt, io};
use std::mem::replace;
use tokio_io::codec::{Decoder, Encoder};

use super::{consume_line, get_content_length, parse_header, trim_header};
use header::{HeaderName, HeaderValue};
use header::types::ContentLength;
use request::{Builder, BuilderError, Request};
use response::Response;
use version::Version;

#[derive(Debug)]
pub struct ServerCodec {
    body: Option<BytesMut>,
    builder: Builder,
    content_length: ContentLength,
    state: ParseState,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub enum ParseState {
    Body,
    End,
    Header,
    RequestLine,
}

impl ServerCodec {
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

    /// Parses a header of the request. If no more headers are present, it will parse the content
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

    /// Parses the request line of the request. Any empty newlines prior to the request line are
    /// ignored. As long as the request line is of the form `METHOD URI VERSION\r\n` where `METHOD`,
    /// `URI`, and `VERSION` are not necessarily valid values, the request will continue to be
    /// parsed. This allows for handling bad requests due to invalid method characters for example.
    /// If the request line is not of that form, then there is no way to recover, so the connection
    /// will just be closed on return of the IO error. An IO error will also be returned if the
    /// RTSP version given is not RTSP/2.0 since that is the only version this implementation
    /// supports.
    fn parse_request_line(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match consume_line(buffer) {
            Some((_, 0)) => {
                buffer.split_to(2);
                None
            }
            Some((mut line, _)) => {
                if let Some(i) = line.iter().position(|&b| b == b' ') {
                    let method = line.split_to(i);
                    line.split_to(1);

                    if let Some(i) = line.iter().position(|&b| b == b' ') {
                        let uri = line.split_to(i);
                        line.split_to(1);

                        self.state = ParseState::Header;
                        self.builder
                            .method(method.as_ref())
                            .uri(uri.as_ref())
                            .version(line.as_ref());

                        if self.builder.version != Version::RTSP20 {
                            return Some(Err(io::Error::new(
                                io::ErrorKind::Other,
                                "unsupported RTSP version".to_string(),
                            )));
                        }

                        return Some(Ok(()));
                    }
                }

                // Request line was not of the form `METHOD URI VERSION\r\n`.

                Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "failed to parse request line".to_string(),
                )))
            }
            None => None,
        }
    }
}

impl Decoder for ServerCodec {
    type Item = Result<Request<BytesMut>, InvalidRequest>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        use self::ParseState::*;

        loop {
            let parse_result = match self.state {
                Body => self.parse_body(buffer),
                Header => self.parse_header(buffer),
                RequestLine => self.parse_request_line(buffer),
                End => {
                    let request = self.builder
                        .build(replace(&mut self.body, None).unwrap())
                        .map_err(|error| InvalidRequest::from(error));
                    self.builder = Builder::new();
                    self.state = RequestLine;

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

impl Encoder for ServerCodec {
    type Item = Response<BytesMut>;
    type Error = io::Error;

    fn encode(&mut self, mut message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        // Force `Content-Length` to be correctly set.

        let body_size =
            unsafe { HeaderValue::from_str_unchecked(message.body().len().to_string()) };
        message
            .headers_mut()
            .insert(HeaderName::ContentLength, body_size);

        buffer.extend(message.version().as_str().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.status_code().to_string().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.reason().as_str().as_bytes());
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

impl Default for ServerCodec {
    fn default() -> Self {
        ServerCodec {
            body: None,
            builder: Builder::new(),
            content_length: ContentLength::from(0),
            state: ParseState::RequestLine,
        }
    }
}

/// An error type for when the request was successfully parsed but contained invalid values.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidRequest {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidURI,
    InvalidVersion,
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

        match self {
            &InvalidHeaderName => "invalid RTSP request - invalid header name",
            &InvalidHeaderValue => "invalid RTSP request-  invalid header value",
            &InvalidMethod => "invalid RTSP request-  invalid method",
            &InvalidURI => "invalid RTSP request - invalid URI",
            &InvalidVersion => "invalid RTSP request - invalid version",
        }
    }
}

impl From<BuilderError> for InvalidRequest {
    fn from(value: BuilderError) -> InvalidRequest {
        use self::InvalidRequest::*;

        match value {
            BuilderError::InvalidHeaderName => InvalidHeaderName,
            BuilderError::InvalidHeaderValue => InvalidHeaderValue,
            BuilderError::InvalidMethod => InvalidMethod,
            BuilderError::InvalidURI => InvalidURI,
            BuilderError::InvalidVersion => InvalidVersion,
            _ => panic!("rest of `BuilderError`s should not be accessible"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use header::types::ContentLength;

    #[test]
    fn test_decoder() {
        // This test is sensitive to the padding in the `Content-Length` header. If a space is added
        // after the `:`, it will fail.

        let mut buffer = BytesMut::from(
            "SETUP * RTSP/2.0\r\n\
             Content-Length:4\r\n\
             \r\n\
             Body"
                .as_bytes(),
        );
        let mut decoder = ServerCodec::default();
        let decoded_request = decoder
            .decode(&mut buffer)
            .unwrap()
            .unwrap()
            .unwrap()
            .into_typed();
        let expected_request = Request::typed_builder()
            .method("SETUP")
            .uri("*")
            .header(ContentLength::from(4))
            .build(BytesMut::from("Body".as_bytes()))
            .unwrap();

        assert_eq!(decoded_request, expected_request);
    }
}
