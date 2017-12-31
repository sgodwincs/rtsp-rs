use bytes::BytesMut;
use std::{error, fmt, io};
use std::convert::TryFrom;
use tokio_io::codec::{Decoder, Encoder};

use proto::{encode_request, encode_response, InvalidRequest, InvalidResponse, ParseResult,
            RequestDecoder, ResponseDecoder};
use request::Request;
use response::Response;

/// The client codec that handles encoding requests and decoding responses.
#[derive(Debug)]
pub struct ClientCodec {
    decoder: ResponseDecoder,
}

impl Decoder for ClientCodec {
    type Item = Result<Response<BytesMut>, InvalidParsedResponse>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        use self::ParseResult::*;

        let (result, bytes_parsed) = self.decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            Complete(response) => Ok(Some(Ok(response))),
            Error(error) => {
                if error.is_recoverable() {
                    Ok(Some(Err(InvalidParsedResponse::try_from(error)
                        .expect("unexpected irrecoverable response parse error"))))
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, error.to_string()))
                }
            }
            Incomplete => Ok(None),
        }
    }
}

impl Default for ClientCodec {
    fn default() -> Self {
        ClientCodec {
            decoder: ResponseDecoder::new(),
        }
    }
}

impl Encoder for ClientCodec {
    type Item = Request<BytesMut>;
    type Error = io::Error;

    fn encode(&mut self, mut message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        encode_request(&mut message, buffer);
        Ok(())
    }
}

/// An error type for when the response was invalid. These are all recoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidParsedResponse {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidStatusCode,
}

impl fmt::Display for InvalidParsedResponse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for InvalidParsedResponse {
    fn description(&self) -> &str {
        use self::InvalidParsedResponse::*;

        match *self {
            InvalidHeaderName => "invalid RTSP response - invalid header name",
            InvalidHeaderValue => "invalid RTSP response - invalid header value",
            InvalidReasonPhrase => "invalid RTSP response - invalid reason phrase",
            InvalidStatusCode => "invalid RTSP response - invalid status code",
        }
    }
}

impl TryFrom<InvalidResponse> for InvalidParsedResponse {
    type Error = ();

    fn try_from(value: InvalidResponse) -> Result<InvalidParsedResponse, Self::Error> {
        use self::InvalidParsedResponse::*;

        match value {
            InvalidResponse::InvalidHeaderName => Ok(InvalidHeaderName),
            InvalidResponse::InvalidHeaderValue => Ok(InvalidHeaderValue),
            InvalidResponse::InvalidReasonPhrase => Ok(InvalidReasonPhrase),
            InvalidResponse::InvalidStatusCode => Ok(InvalidStatusCode),
            _ => Err(()),
        }
    }
}

/// The server codec that handles encoding responses and decoding requests.
#[derive(Debug)]
pub struct ServerCodec {
    decoder: RequestDecoder,
}

impl Decoder for ServerCodec {
    type Item = Result<Request<BytesMut>, InvalidParsedRequest>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        use self::ParseResult::*;

        let (result, bytes_parsed) = self.decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            Complete(request) => Ok(Some(Ok(request))),
            Error(error) => {
                if error.is_recoverable() {
                    Ok(Some(Err(InvalidParsedRequest::try_from(error)
                        .expect("unexpected irrecoverable request parse error"))))
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, error.to_string()))
                }
            }
            Incomplete => Ok(None),
        }
    }
}

impl Default for ServerCodec {
    fn default() -> Self {
        ServerCodec {
            decoder: RequestDecoder::new(),
        }
    }
}

impl Encoder for ServerCodec {
    type Item = Response<BytesMut>;
    type Error = io::Error;

    fn encode(&mut self, mut message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        encode_response(&mut message, buffer);
        Ok(())
    }
}

/// An error type for when the request was invalid. These are all recoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidParsedRequest {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidURI,
}

impl fmt::Display for InvalidParsedRequest {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for InvalidParsedRequest {
    fn description(&self) -> &str {
        use self::InvalidParsedRequest::*;

        match *self {
            InvalidHeaderName => "invalid RTSP request - invalid header name",
            InvalidHeaderValue => "invalid RTSP request - invalid header value",
            InvalidMethod => "invalid RTSP request - invalid method",
            InvalidURI => "invalid RTSP request - invalid uri",
        }
    }
}

impl TryFrom<InvalidRequest> for InvalidParsedRequest {
    type Error = ();

    fn try_from(value: InvalidRequest) -> Result<InvalidParsedRequest, Self::Error> {
        use self::InvalidParsedRequest::*;

        match value {
            InvalidRequest::InvalidHeaderName => Ok(InvalidHeaderName),
            InvalidRequest::InvalidHeaderValue => Ok(InvalidHeaderValue),
            InvalidRequest::InvalidMethod => Ok(InvalidMethod),
            InvalidRequest::InvalidURI => Ok(InvalidURI),
            _ => Err(()),
        }
    }
}
