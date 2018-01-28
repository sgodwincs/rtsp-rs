use bytes::BytesMut;
use std::{fmt, io};
use std::convert::TryFrom;
use std::error::Error;
use tokio_io::codec::{Decoder, Encoder};

use proto::{encode_request, encode_response, InvalidRequest, InvalidResponse, ParseResult,
            ParseState, RequestDecoder, ResponseDecoder};
use request::Request;
use response::Response;

const MINIMUM_INFO_LINE_SIZE: usize = 14;

/// The codec that handles encoding requests/responses and decoding requests/responses. Because
/// servers and clients can both send requests and receive responses, this codec is shared by the
/// two.
#[derive(Debug)]
pub struct Codec {
    request_decoder: RequestDecoder,
    response_decoder: ResponseDecoder,
}

impl Codec {
    pub fn new() -> Self {
        Codec::default()
    }

    fn decode_request(
        &mut self,
        buffer: &mut BytesMut,
    ) -> io::Result<Option<<Self as Decoder>::Item>> {
        use self::ParseResult::*;

        let (result, bytes_parsed) = self.request_decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            Complete(request) => Ok(Some(Ok(Message::Request(request)))),
            Error(error) => {
                if error.is_recoverable() {
                    Ok(Some(Err(InvalidMessage::InvalidRequest(
                        InvalidParsedRequest::try_from(error)
                            .expect("unexpected irrecoverable request parse error"),
                    ))))
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, error.to_string()))
                }
            }
            Incomplete => Ok(None),
        }
    }

    fn decode_response(
        &mut self,
        buffer: &mut BytesMut,
    ) -> io::Result<Option<<Self as Decoder>::Item>> {
        use self::ParseResult::*;

        let (result, bytes_parsed) = self.response_decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            Complete(response) => Ok(Some(Ok(Message::Response(response)))),
            Error(error) => {
                if error.is_recoverable() {
                    Ok(Some(Err(InvalidMessage::InvalidResponse(
                        InvalidParsedResponse::try_from(error)
                            .expect("unexpected irrecoverable response parse error"),
                    ))))
                } else {
                    Err(io::Error::new(io::ErrorKind::Other, error.to_string()))
                }
            }
            Incomplete => Ok(None),
        }
    }
}

impl Decoder for Codec {
    type Item = Result<Message, InvalidMessage>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        if self.request_decoder.parse_state() != ParseState::InfoLine {
            self.decode_request(buffer)
        } else if self.response_decoder.parse_state() != ParseState::InfoLine {
            self.decode_response(buffer)
        } else {
            while buffer.starts_with(b"\r\n") {
                buffer.split_to(2);
            }

            if buffer.len() < MINIMUM_INFO_LINE_SIZE {
                Ok(None)
            } else if buffer.starts_with(b"RTSP/") {
                self.decode_response(buffer)
            } else {
                self.decode_request(buffer)
            }
        }
    }
}

impl Default for Codec {
    fn default() -> Self {
        Codec {
            request_decoder: RequestDecoder::new(),
            response_decoder: ResponseDecoder::new(),
        }
    }
}

impl Encoder for Codec {
    type Item = Message;
    type Error = io::Error;

    fn encode(&mut self, message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        match message {
            Message::Request(request) => encode_request(&request, buffer),
            Message::Response(response) => encode_response(&response, buffer),
        }

        Ok(())
    }
}

pub enum Message {
    Request(Request<BytesMut>),
    Response(Response<BytesMut>),
}

pub enum InvalidMessage {
    InvalidRequest(InvalidParsedRequest),
    InvalidResponse(InvalidParsedResponse),
}

/// An error type for when the response was invalid. These are all recoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidParsedResponse {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidStatusCode,
}

impl fmt::Display for InvalidParsedResponse {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidParsedResponse {
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

/// An error type for when the request was invalid. These are all recoverable errors.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidParsedRequest {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidURI,
}

impl fmt::Display for InvalidParsedRequest {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidParsedRequest {
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
