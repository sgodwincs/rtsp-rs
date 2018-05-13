use bytes::BytesMut;
use futures::sync::mpsc::UnboundedSender;
use std::convert::TryFrom;
use std::error::Error as ErrorTrait;
use std::sync::Arc;
use std::{fmt, io};
use tokio_io::codec::{Decoder, Encoder};

use protocol::{
    encode_request, encode_response, InvalidRequest, InvalidResponse, ParseResult, ParseState,
    RequestDecoder, ResponseDecoder,
};
use request::Request;
use response::Response;

/// The minimum amount of bytes needed in the information line in order to differentiate between
/// responses and requests.
const MINIMUM_INFO_LINE_SIZE: usize = 5;

/// An abstract message result that is produced by message decoding. The error variant only includes
/// errors that are recoverable.
pub type MessageResult = Result<Message, InvalidMessage>;

/// A request result that is produced by request decoding. The error variant only includes errors
/// that are recoverable.
pub type RequestResult = Result<Request<BytesMut>, RecoverableInvalidRequest>;

/// A response result that is produced by response decoding. The error variant only includes errors
/// that are recoverable.
pub type ResponseResult = Result<Response<BytesMut>, RecoverableInvalidResponse>;

/// The codec that handles encoding requests/responses and decoding requests/responses. Because
/// servers and clients can both send requests and receive responses, this codec is shared by the
/// two (as well as proxies).
#[derive(Debug)]
pub struct Codec {
    /// The request decoder that maintains partial parsing state.
    request_decoder: RequestDecoder,

    /// The response decoder that maintains partial parsing state.
    response_decoder: ResponseDecoder,

    /// An event sink that is sent [CodecEvent]s. For example, whenever decoding starts, an event
    /// will be sent for that.
    tx_event: Option<UnboundedSender<CodecEvent>>,
}

impl Codec {
    /// Constructs a new codec without an event sink.
    pub fn new() -> Self {
        Codec::default()
    }

    /// Constructs a new codec with an event sink.
    ///
    /// # Arguments
    ///
    /// * `tx_event` - The sink that will be sent any [`CodecEvent`] that occur.
    pub fn with_events(tx_event: UnboundedSender<CodecEvent>) -> Self {
        Codec {
            request_decoder: RequestDecoder::new(),
            response_decoder: ResponseDecoder::new(),
            tx_event: Some(tx_event),
        }
    }

    /// Sends a [`CodecEvent`] through the internal event sink.
    ///
    /// If an error is encountered while sending the codec event, then no more events will be sent
    /// for the duration of this codec's lifetime.
    ///
    /// # Arguments
    ///
    /// * `event` - The event to send through the sink.
    fn send_codec_event(&mut self, event: CodecEvent) {
        self.tx_event = self.tx_event
            .take()
            .and_then(|tx_event| tx_event.unbounded_send(event).ok().map(|_| tx_event));
    }

    /// Decodes a request.
    ///
    /// Using the internal request decoder, this function will attempt to make progress on decoding
    /// a request using the buffer. If a request is successfully decoded or an error occurs, this
    /// function will send a [`CodecEvent::DecodingEnded`] event.
    ///
    /// # Arguments
    ///
    /// * `buffer` - The byte buffer containing the request to decode.
    ///
    /// # Return Value
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a request, then
    ///   `Ok(Some(Ok(`[`Message`]`)))` will be returned.
    /// * If there was not enough data but no error
    ///   occurred, then `Ok(None)` will be returned indicating that more data is needed.
    /// * If the decoder encountered an error while decoding that was irrecoverable, then
    ///   `Err(`[`Error`]`)` will be returned.
    /// * If the decoder encountered an error while decoding that was recoverable, then
    ///   `Ok(Some(Err(`[`InvalidMessage`]`)))` will be returned.
    fn decode_request(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Result<Option<<Self as Decoder>::Item>, <Self as Decoder>::Error> {
        let (result, bytes_parsed) = self.request_decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            ParseResult::Complete(request) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Ok(Some(Ok(Message::Request(request))))
            }
            ParseResult::Error(error) => {
                self.send_codec_event(CodecEvent::DecodingEnded);

                if error.is_recoverable() {
                    Ok(Some(Err(InvalidMessage::InvalidRequest(
                        RecoverableInvalidRequest::try_from(error)
                            .expect("unexpected irrecoverable request parse error"),
                    ))))
                } else {
                    Err(Error::InvalidRequest(
                        IrrecoverableInvalidRequest::try_from(error)
                            .expect("unexpected recoverable request parse error"),
                    ))
                }
            }
            ParseResult::Incomplete => Ok(None),
        }
    }

    /// Decodes a response.
    ///
    /// Using the internal response decoder, this function will attempt to make progress on decoding
    /// a response using the buffer. If a response is successfully decoded or an error occurs, this
    /// function will send a [`CodecEvent::DecodingEnded`] event.
    ///
    /// # Arguments
    ///
    /// * `buffer` - The byte buffer containing the response to decode.
    ///
    /// # Return Value
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a response, then
    ///   `Ok(Some(Ok(`[`Message`]`)))` will be returned.
    /// * If there was not enough data but no error
    ///   occurred, then `Ok(None)` will be returned indicating that more data is needed.
    /// * If the decoder encountered an error while decoding that was irrecoverable, then
    ///   `Err(`[`Error`]`)` will be returned.
    /// * If the decoder encountered an error while decoding that was recoverable, then
    ///   `Ok(Some(Err(`[`InvalidMessage`]`)))` will be returned.
    fn decode_response(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Result<Option<<Self as Decoder>::Item>, <Self as Decoder>::Error> {
        let (result, bytes_parsed) = self.response_decoder.decode(&buffer);
        buffer.split_to(bytes_parsed);

        match result {
            ParseResult::Complete(response) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Ok(Some(Ok(Message::Response(response))))
            }
            ParseResult::Error(error) => {
                self.send_codec_event(CodecEvent::DecodingEnded);

                if error.is_recoverable() {
                    Ok(Some(Err(InvalidMessage::InvalidResponse(
                        RecoverableInvalidResponse::try_from(error)
                            .expect("unexpected irrecoverable response parse error"),
                    ))))
                } else {
                    Err(Error::InvalidResponse(
                        IrrecoverableInvalidResponse::try_from(error)
                            .expect("unexpected recoverable response parse error"),
                    ))
                }
            }
            ParseResult::Incomplete => Ok(None),
        }
    }
}

impl Decoder for Codec {
    type Item = MessageResult;
    type Error = Error;

    /// Decodes a message.
    ///
    /// Using the internal decoders, this function will attempt to make progress on decoding either
    /// a request or response using the buffer. If neither of the decoders were active, this
    /// function will send a [`CodecEvent::DecodingStarted`] event if the buffer is non-empty after
    /// removing all preceding newlines.
    ///
    /// # Arguments
    ///
    /// * `buffer` - The byte buffer containing the message to decode.
    ///
    /// # Return Value
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a message, then
    ///   `Ok(Some(Ok(`[`Message`]`)))` will be returned.
    /// * If there was not enough data but no error
    ///   occurred, then `Ok(None)` will be returned indicating that more data is needed.
    /// * If the decoder encountered an error that was irrecoverable, then
    ///   `Err(`[`Error`]`)` will be returned.
    /// * If the decoder encountered an error that was recoverable, then
    ///   `Ok(Some(Err(`[`InvalidMessage`]`)))` will be returned.
    fn decode(&mut self, buffer: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        // Need to determine whether we are trying to decode a request or response. If either of the
        // internal decoder states are past their starting states, then we continue off of that.
        // Otherwise, we check if the message starts with `"RTSP/"` which indicates that it is a
        // response. If not, it is a request.

        if self.request_decoder.parse_state() != ParseState::InfoLine {
            self.decode_request(buffer)
        } else if self.response_decoder.parse_state() != ParseState::InfoLine {
            self.decode_response(buffer)
        } else {
            // Ignore any preceding newlines.

            while buffer.starts_with(b"\r\n") {
                buffer.split_to(2);
            }

            if !buffer.is_empty() {
                self.send_codec_event(CodecEvent::DecodingStarted);
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

    /// Called when there are no more bytes available to be read from the underlying I/O.
    ///
    /// # Arguments
    ///
    /// * `buffer` - The byte buffer containing the message to decode.
    ///
    /// # Return Value
    ///
    /// This function will attempt to decode a message as described in `Codec::decode()`. If there
    /// is not enough data to do so, then `Err(`[`Error::UnexpectedEOF`]`)` will be returned.
    fn decode_eof(&mut self, buffer: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        match self.decode(buffer)? {
            Some(message) => Ok(Some(message)),
            None => {
                if buffer.is_empty() {
                    Ok(None)
                } else {
                    Err(Error::UnexpectedEOF)
                }
            }
        }
    }
}

impl Default for Codec {
    fn default() -> Self {
        Codec {
            request_decoder: RequestDecoder::new(),
            response_decoder: ResponseDecoder::new(),
            tx_event: None,
        }
    }
}

impl Encoder for Codec {
    type Item = Message;
    type Error = Error;

    /// Encodes a message.
    ///
    /// This function will encode the given message into the given buffer. Before encoding the
    /// message, a [`CodecEvent::EncodingStarted`] event will be sent. And after encoding has
    /// finished, an [`CodecEvent::EncodingEnded`] event will be sent.
    ///
    /// # Arguments
    ///
    /// * `message` - The message to be encoded.
    /// * `buffer` - The buffer to encode the message into.
    ///
    /// # Return Value
    ///
    /// Although a `Result` is returned, this function will never return an error as the actual
    /// message encoding cannot fail. As a result, `Ok(())` will always be returned.
    fn encode(&mut self, message: Self::Item, buffer: &mut BytesMut) -> Result<(), Self::Error> {
        self.send_codec_event(CodecEvent::EncodingStarted);

        match message {
            Message::Request(request) => encode_request(&request, buffer),
            Message::Response(response) => encode_response(&response, buffer),
        }

        self.send_codec_event(CodecEvent::EncodingEnded);
        Ok(())
    }
}

/// An event type that is corresponds to either encoding or decoding events that happen internally
/// in the codec.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum CodecEvent {
    /// The decoding of a message has started.
    DecodingStarted,

    /// The decoding of a message has ended. This will be sent when either a message is successfully
    /// decoded or decoding fails.
    DecodingEnded,

    /// The encoding of a message has started.
    EncodingStarted,

    /// The encoding of a message has ended.
    EncodingEnded,
}

/// An abstract message type that is either a request or response.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Message {
    Request(Request<BytesMut>),
    Response(Response<BytesMut>),
}

/// A generic error type for any RTSP networking related errors.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum Error {
    /// An error that occurs when too much time has passed from the start of message decoding. The
    /// timer starts whenever the information line of a request or response is encountered.
    /// Preceding newlines do not start the timer.
    DecodingTimedOut,

    /// An underlying I/O error occurred either in the stream or sink.
    IO(Arc<io::Error>),

    /// An irrecoverable error was encountered while decoding a request.
    InvalidRequest(IrrecoverableInvalidRequest),

    /// An irrecoverable error was encountered while decoding a response.
    InvalidResponse(IrrecoverableInvalidResponse),

    /// A networking operation was attempted when there was no connection.
    NotConnected,

    /// The underlying stream has ended, but there is still leftover data that is not enough for a
    /// full request or response to be decoded from.
    UnexpectedEOF,
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for Error {
    fn description(&self) -> &str {
        use self::Error::*;

        match *self {
            DecodingTimedOut => "decoding timed out",
            IO(ref io) => io.description(),
            InvalidRequest(ref request) => request.description(),
            InvalidResponse(ref response) => response.description(),
            NotConnected => "not connected",
            UnexpectedEOF => "unexpected EOF",
        }
    }
}

impl From<io::Error> for Error {
    fn from(value: io::Error) -> Error {
        Error::IO(Arc::new(value))
    }
}

/// An error representing an irrecoverable decoding error of a request.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum IrrecoverableInvalidRequest {
    InvalidContentLength,
    InvalidHeaderLine,
    InvalidRequestLine,
    InvalidVersion,
    UnsupportedVersion,
}

impl fmt::Display for IrrecoverableInvalidRequest {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for IrrecoverableInvalidRequest {
    fn description(&self) -> &str {
        use self::IrrecoverableInvalidRequest::*;

        match *self {
            InvalidContentLength => "invalid RTSP request - invalid content length",
            InvalidHeaderLine => "invalid RTSP request - invalid header line",
            InvalidRequestLine => "invalid RTSP request - invalid request line",
            InvalidVersion => "invalid RTSP request - invalid version",
            UnsupportedVersion => "invalid RTSP request - unsupported version",
        }
    }
}

impl TryFrom<InvalidRequest> for IrrecoverableInvalidRequest {
    type Error = ();

    fn try_from(value: InvalidRequest) -> Result<Self, Self::Error> {
        use self::IrrecoverableInvalidRequest::*;

        match value {
            InvalidRequest::InvalidContentLength => Ok(InvalidContentLength),
            InvalidRequest::InvalidHeaderLine => Ok(InvalidHeaderLine),
            InvalidRequest::InvalidRequestLine => Ok(InvalidRequestLine),
            InvalidRequest::InvalidVersion => Ok(InvalidVersion),
            InvalidRequest::UnsupportedVersion => Ok(UnsupportedVersion),
            _ => Err(()),
        }
    }
}

/// An error representing an irrecoverable decoding error of a response.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum IrrecoverableInvalidResponse {
    InvalidContentLength,
    InvalidHeaderLine,
    InvalidResponseLine,
    InvalidVersion,
    UnsupportedVersion,
}

impl fmt::Display for IrrecoverableInvalidResponse {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for IrrecoverableInvalidResponse {
    fn description(&self) -> &str {
        use self::IrrecoverableInvalidResponse::*;

        match *self {
            InvalidContentLength => "invalid RTSP response - invalid content length",
            InvalidHeaderLine => "invalid RTSP response - invalid header line",
            InvalidResponseLine => "invalid RTSP response - invalid response line",
            InvalidVersion => "invalid RTSP response - invalid version",
            UnsupportedVersion => "invalid RTSP response - unsupported version",
        }
    }
}

impl TryFrom<InvalidResponse> for IrrecoverableInvalidResponse {
    type Error = ();

    fn try_from(value: InvalidResponse) -> Result<Self, Self::Error> {
        use self::IrrecoverableInvalidResponse::*;

        match value {
            InvalidResponse::InvalidContentLength => Ok(InvalidContentLength),
            InvalidResponse::InvalidHeaderLine => Ok(InvalidHeaderLine),
            InvalidResponse::InvalidResponseLine => Ok(InvalidResponseLine),
            InvalidResponse::InvalidVersion => Ok(InvalidVersion),
            InvalidResponse::UnsupportedVersion => Ok(UnsupportedVersion),
            _ => Err(()),
        }
    }
}

// An abstract message error type that is either a request or response decoding error. The error is
// always recoverable.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum InvalidMessage {
    InvalidRequest(RecoverableInvalidRequest),
    InvalidResponse(RecoverableInvalidResponse),
}

impl fmt::Display for InvalidMessage {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for InvalidMessage {
    fn description(&self) -> &str {
        use self::InvalidMessage::*;

        match *self {
            InvalidRequest(ref request) => request.description(),
            InvalidResponse(ref response) => response.description(),
        }
    }
}

/// An error representing an recoverable decoding error of a request.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum RecoverableInvalidRequest {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidURI,
}

impl fmt::Display for RecoverableInvalidRequest {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for RecoverableInvalidRequest {
    fn description(&self) -> &str {
        use self::RecoverableInvalidRequest::*;

        match *self {
            InvalidHeaderName => "invalid RTSP request - invalid header name",
            InvalidHeaderValue => "invalid RTSP request - invalid header value",
            InvalidMethod => "invalid RTSP request - invalid method",
            InvalidURI => "invalid RTSP request - invalid uri",
        }
    }
}

impl TryFrom<InvalidRequest> for RecoverableInvalidRequest {
    type Error = ();

    fn try_from(value: InvalidRequest) -> Result<Self, Self::Error> {
        use self::RecoverableInvalidRequest::*;

        match value {
            InvalidRequest::InvalidHeaderName => Ok(InvalidHeaderName),
            InvalidRequest::InvalidHeaderValue => Ok(InvalidHeaderValue),
            InvalidRequest::InvalidMethod => Ok(InvalidMethod),
            InvalidRequest::InvalidURI => Ok(InvalidURI),
            _ => Err(()),
        }
    }
}

/// An error representing an recoverable decoding error of a response.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum RecoverableInvalidResponse {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidStatusCode,
}

impl fmt::Display for RecoverableInvalidResponse {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl ErrorTrait for RecoverableInvalidResponse {
    fn description(&self) -> &str {
        use self::RecoverableInvalidResponse::*;

        match *self {
            InvalidHeaderName => "invalid RTSP response - invalid header name",
            InvalidHeaderValue => "invalid RTSP response - invalid header value",
            InvalidReasonPhrase => "invalid RTSP response - invalid reason phrase",
            InvalidStatusCode => "invalid RTSP response - invalid status code",
        }
    }
}

impl TryFrom<InvalidResponse> for RecoverableInvalidResponse {
    type Error = ();

    fn try_from(value: InvalidResponse) -> Result<Self, Self::Error> {
        use self::RecoverableInvalidResponse::*;

        match value {
            InvalidResponse::InvalidHeaderName => Ok(InvalidHeaderName),
            InvalidResponse::InvalidHeaderValue => Ok(InvalidHeaderValue),
            InvalidResponse::InvalidReasonPhrase => Ok(InvalidReasonPhrase),
            InvalidResponse::InvalidStatusCode => Ok(InvalidStatusCode),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod test {
    use futures::sync::mpsc::unbounded;
    use futures::{Future, Stream};
    use tokio::runtime::current_thread::Runtime;

    use super::*;
    use header::HeaderName;

    #[test]
    fn test_codec_decoding() {
        let mut codec = Codec::new();
        let mut buffer = BytesMut::from(
            "SETUP * RTSP/2.0\r\n\
             Content-Length: 4\r\n\
             \r\n\
             Body",
        );
        let expected_request = Request::builder()
            .method("SETUP")
            .uri("*")
            .header(HeaderName::ContentLength, " 4")
            .build(BytesMut::from("Body".as_bytes()))
            .unwrap();

        assert_eq!(
            codec.decode(&mut buffer).unwrap().unwrap().unwrap(),
            Message::Request(expected_request)
        );

        let mut buffer = BytesMut::from("RTSP/2.0 200 OK\r\n");
        assert_eq!(codec.decode(&mut buffer).unwrap(), None);

        let mut buffer = BytesMut::from("\r\n\r\n");
        assert_eq!(
            codec.decode(&mut buffer).unwrap().unwrap().unwrap(),
            Message::Response(Response::builder().build("".into()).unwrap())
        );
    }

    #[test]
    fn test_codec_encoding() {
        let mut codec = Codec::new();
        let mut buffer = BytesMut::new();
        let request = Request::builder()
            .method("SETUP")
            .uri("*")
            .header(HeaderName::ContentLength, " 4")
            .build(BytesMut::from("Body".as_bytes()))
            .unwrap();
        let expected_buffer = BytesMut::from(
            "SETUP * RTSP/2.0\r\n\
             Content-Length: 4\r\n\
             \r\n\
             Body",
        );

        codec
            .encode(Message::Request(request), &mut buffer)
            .unwrap();
        assert_eq!(buffer, expected_buffer);
    }

    #[test]
    fn test_codec_events() {
        let (tx_event, rx_event) = unbounded();

        {
            let mut codec = Codec::with_events(tx_event);
            let mut buffer = BytesMut::new();
            let response = Response::builder().build("".into()).unwrap();
            codec
                .encode(Message::Response(response), &mut buffer)
                .unwrap();

            let mut buffer = BytesMut::from("SETUP * RTSP/2.0\r\n");
            assert!(codec.decode(&mut buffer).is_ok());

            let mut buffer = BytesMut::new();
            let response = Response::builder().build("".into()).unwrap();
            codec
                .encode(Message::Response(response), &mut buffer)
                .unwrap();

            let mut buffer = BytesMut::from("\r\n\r\n");
            assert!(codec.decode(&mut buffer).is_ok());
        }

        let mut runtime = Runtime::new().unwrap();
        let events = runtime.block_on(rx_event.collect()).unwrap();

        assert_eq!(
            events,
            vec![
                CodecEvent::EncodingStarted,
                CodecEvent::EncodingEnded,
                CodecEvent::DecodingStarted,
                CodecEvent::EncodingStarted,
                CodecEvent::EncodingEnded,
                CodecEvent::DecodingEnded,
            ]
        );
    }
}
