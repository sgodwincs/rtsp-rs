use bytes::BytesMut;
use std::{error, fmt, io};
use std::mem::replace;
use tokio_io::codec::{Decoder, Encoder};

use super::{consume_line, trim_header};
use header::{Entry, HeaderName, HeaderValue, TypedHeader};
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

    /// A helper function to parse a header. This function will return two indices where one is
    /// optional. The first index will be the end of the header, while the second index is an
    /// `Option<usize>` of where the the header name and value are separated (the index of the `:`).
    /// RTSP allows for having multiline headers as long as newlines contained within header values
    /// start with a space or tab.
    ///
    /// If the newline found is empty (meaning the header section is over), then the content length
    /// of the request will be calculated. An absence of the `Content-Length` header implicitly
    /// implies that the content length is 0. If there is more than one content length header or
    /// the value of the header cannot be parsed correctly, then the connection will be closed on
    /// return of the IO error.
    fn parse_header_multiline(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Option<io::Result<(usize, Option<usize>)>> {
        let mut iter = buffer
            .windows(2)
            .enumerate()
            .filter(|&(_, b)| b == b"\r\n")
            .map(|(i, _)| i);

        match iter.next() {
            Some(0) => {
                self.state = ParseState::Body;

                self.content_length = match self.builder
                    .headers
                    .entry(HeaderName::ContentLength)
                    .unwrap()
                {
                    Entry::Occupied(entry) => if entry.iter().count() > 1 {
                        return Some(Err(io::Error::new(
                            io::ErrorKind::Other,
                            "found multiple \"Content-Length\" headers".to_string(),
                        )));
                    } else {
                        match ContentLength::try_from_header_raw(&entry
                            .iter()
                            .cloned()
                            .collect::<Vec<HeaderValue>>())
                        {
                            Ok(content_length) => content_length,
                            Err(_) => {
                                return Some(Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "invalid \"Content-Length\" header".to_string(),
                                )))
                            }
                        }
                    },
                    Entry::Vacant(_) => ContentLength::from(0),
                };

                Some(Ok((0, None)))
            }
            Some(mut i) => if let Some(j) = buffer.iter().take(i).position(|&b| b == b':') {
                loop {
                    match buffer.get(i + 2) {
                        Some(&b) if b == b' ' || b == b'\t' => match iter.next() {
                            Some(k) => i = k,
                            None => break None,
                        },
                        Some(_) => break Some(Ok((i, Some(j)))),
                        None => break None,
                    }
                }
            } else {
                Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "failed to parse request header".to_string(),
                )))
            },
            None => None,
        }
    }

    /// Parses a header of the request. The parsed header name will be trimmed on the right for any
    /// spaces and tabs, but the header value will remained completely untouched. This is because
    /// the syntax of an RTSP header is ambiguous as to whether whitespace is simply whitespace
    /// allowed after the colon or if the whitespace is significant to the header value (since
    /// header values are allowed to have whitespace as part of their value).
    fn parse_header(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match self.parse_header_multiline(buffer) {
            Some(Ok((i, j))) => {
                let mut result = buffer.split_to(i);
                buffer.split_to(2);

                if let Some(j) = j {
                    let header_name = result.split_to(j);
                    let header_name = trim_header(header_name.as_ref());
                    result.split_to(1);

                    self.builder.header(header_name, result.as_ref());
                }

                Some(Ok(()))
            }
            Some(Err(error)) => Some(Err(error)),
            None => None,
        }
    }

    /// Parses the request line of the request. Any empty newlines prior to the request line are
    /// ignored. As long as the request line if of the form `METHOD URI VERSION\r\n` where `METHOD`,
    /// `URI`, and `VERSION` are not necessarily valid values, the request will continue to be
    /// parsed. This allows for handling bad requests for invalid method characters for example. If
    /// the request line is not of that form, then there is no way to recover, so the connection
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

    fn encode(&mut self, message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        buffer.extend(message.version().as_str().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.status_code().to_string().as_bytes());
        buffer.extend(b" ");
        buffer.extend(message.reason().as_bytes());
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
            .header(ContentLength(4))
            .build(BytesMut::from("Body".as_bytes()))
            .unwrap();

        assert_eq!(decoded_request, expected_request);
    }
}
