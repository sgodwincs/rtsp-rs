#[macro_use]
pub mod decoder;
pub mod encoder;

use bytes::BytesMut;
use futures::sync::mpsc::UnboundedSender;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io;
use std::sync::Arc;
use tokio_codec::{Decoder, Encoder};

use crate::protocol::codec::decoder::request::{
    DecodeError as RequestDecodeError, DecodeState as RequestDecodeState, Decoder as RequestDecoder,
};
use crate::protocol::codec::decoder::response::{
    DecodeError as ResponseDecodeError, DecodeState as ResponseDecodeState,
    Decoder as ResponseDecoder,
};
use crate::protocol::codec::decoder::DecodeResult;
use crate::protocol::codec::encoder::request;
use crate::protocol::codec::encoder::response;
use crate::request::Request;
use crate::response::Response;

/// The minimum amount of bytes needed in the information line in order to differentiate between
/// requests and responses.
const MINIMUM_INFO_LINE_SIZE: usize = 5;

/// The codec that handles encoding requests/responses and decoding requests/responses. Because
/// servers and clients can both send requests and receive responses, this codec is shared by the
/// two (as well as proxies).
#[derive(Debug)]
pub struct Codec {
    /// The request decoder that maintains partial parsing state.
    request_decoder: RequestDecoder,

    /// The response decoder that maintains partial parsing state.
    response_decoder: ResponseDecoder,

    /// An event sink that is sent [`CodecEvent`]s. For example, whenever decoding starts, an event
    /// will be sent for that.
    tx_event: Option<UnboundedSender<CodecEvent>>,
}

impl Codec {
    /// Decodes a request.
    ///
    /// Using the internal request decoder, this function will attempt to make progress on decoding
    /// a request using the buffer. If a request is successfully decoded or an error occurs, this
    /// function will send a [`CodecEvent::DecodingEnded`] event.
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a request, then
    ///   `Ok(Some(`[`Message`]`))` will be returned.
    /// * If there was not enough data but no error occurred, then `Ok(None)` will be returned
    ///   indicating that more data is needed.
    /// * If the decoder encountered an error while decoding, then `Err(`[`ProtocolError`]`)` will
    ///   be returned.
    fn decode_request(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Result<Option<<Self as Decoder>::Item>, <Self as Decoder>::Error> {
        let (result, bytes_decoded) = self.request_decoder.decode(&buffer);
        buffer.split_to(bytes_decoded);

        match result {
            DecodeResult::Complete(request) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Ok(Some(Message::Request(request)))
            }
            DecodeResult::Incomplete => Ok(None),
            DecodeResult::Error(error) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Err(ProtocolError::DecodeError(error.into()))
            }
        }
    }

    /// Decodes a response.
    ///
    /// Using the internal response decoder, this function will attempt to make progress on decoding
    /// a response using the buffer. If a response is successfully decoded or an error occurs, this
    /// function will send a [`CodecEvent::DecodingEnded`] event.
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a response, then
    ///   `Ok(Some(`[`Message`]`))` will be returned.
    /// * If there was not enough data but no error occurred, then `Ok(None)` will be returned
    ///   indicating that more data is needed.
    /// * If the decoder encountered an error while decoding, then `Err(`[`ProtocolError`]`)` will
    ///   be returned.
    fn decode_response(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Result<Option<<Self as Decoder>::Item>, <Self as Decoder>::Error> {
        let (result, bytes_decoded) = self.response_decoder.decode(&buffer);
        buffer.split_to(bytes_decoded);

        match result {
            DecodeResult::Complete(response) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Ok(Some(Message::Response(response)))
            }
            DecodeResult::Incomplete => Ok(None),
            DecodeResult::Error(error) => {
                self.send_codec_event(CodecEvent::DecodingEnded);
                Err(ProtocolError::DecodeError(error.into()))
            }
        }
    }

    /// Constructs a new codec without an event sink.
    pub fn new() -> Self {
        Codec {
            request_decoder: RequestDecoder::new(),
            response_decoder: ResponseDecoder::new(),
            tx_event: None,
        }
    }

    /// Sends a [`CodecEvent`] through the internal event sink.
    ///
    /// If an error is encountered while sending the codec event, then no more events will be sent
    /// for the duration of this codec's lifetime.
    fn send_codec_event(&mut self, event: CodecEvent) {
        if let Some(tx_event) = self.tx_event.as_ref() {
            if tx_event.unbounded_send(event).is_err() {
                self.tx_event = None;
            }
        }
    }

    /// Constructs a new codec with an event sink.
    pub fn with_events(tx_event: UnboundedSender<CodecEvent>) -> Self {
        Codec {
            request_decoder: RequestDecoder::new(),
            response_decoder: ResponseDecoder::new(),
            tx_event: Some(tx_event),
        }
    }
}

impl Decoder for Codec {
    type Item = Message;
    type Error = ProtocolError;

    /// Decodes a message.
    ///
    /// Using the internal decoders, this function will attempt to make progress on decoding either
    /// a request or response using the buffer. If neither of the decoders are active, this
    /// function will send a [`CodecEvent::DecodingStarted`] event if the buffer is non-empty after
    /// removing all preceding newlines.
    ///
    /// The return value of this function can be divided into four parts:
    ///
    /// * If there was enough data provided to successfully decode a message, then
    ///   `Ok(Some(`[`Message`]`))` will be returned.
    /// * If there was not enough data but no error occurred, then `Ok(None)` will be returned
    ///   indicating that more data is needed.
    /// * If the decoder encountered an error, then `Err(`[`ProtocolError`]`)` will be returned.
    fn decode(&mut self, buffer: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        // Need to determine whether we are trying to decode a request or response. If either of the
        // internal decoder states are past their starting states, then we continue off of that.
        // Otherwise, we check if the message starts with `"RTSP/"` which indicates that it is a
        // response. If not, it is a request.

        if self.request_decoder.state() != RequestDecodeState::Method {
            self.decode_request(buffer)
        } else if self.response_decoder.state() != ResponseDecodeState::Version {
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
    /// This function will attempt to decode a message as described in [`Codec::decode`]. If there
    /// is not enough data to do so, then `Err(`[`ProtocolError::UnexpectedEOF`]`)` will be
    /// returned.
    fn decode_eof(&mut self, buffer: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        match self.decode(buffer)? {
            Some(message) => Ok(Some(message)),
            None => {
                if buffer.is_empty() {
                    Ok(None)
                } else {
                    Err(ProtocolError::UnexpectedEOF)
                }
            }
        }
    }
}

impl Default for Codec {
    fn default() -> Self {
        Codec::new()
    }
}

impl Encoder for Codec {
    type Item = Message;
    type Error = ProtocolError;

    /// Encodes a message.
    ///
    /// This function will encode the given message into the given buffer. Before encoding the
    /// message, a [`CodecEvent::EncodingStarted`] event will be sent. And after encoding has
    /// finished, an [`CodecEvent::EncodingEnded`] event will be sent.
    ///
    /// Although a [`Result`] is returned, this function will never return an error as the actual
    /// message encoding cannot fail. As a result, `Ok(())` will always be returned.
    fn encode(&mut self, message: Self::Item, buffer: &mut BytesMut) -> Result<(), Self::Error> {
        self.send_codec_event(CodecEvent::EncodingStarted);

        match message {
            Message::Request(request) => request::encode(&request, buffer),
            Message::Response(response) => response::encode(&response, buffer),
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
    /// The decoding of a message has ended. This will be sent when either a message is successfully
    /// decoded or decoding fails.
    DecodingEnded,

    /// The decoding of a message has started.
    DecodingStarted,

    /// The encoding of a message has ended.
    EncodingEnded,

    /// The encoding of a message has started.
    EncodingStarted,
}

/// An abstract message type that is either a request or response.
#[derive(Clone, Debug, Eq, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Message {
    /// This message is a request.
    Request(Request<BytesMut>),

    /// This message is a response.
    Response(Response<BytesMut>),
}

/// A generic error type for any protocol errors that occur.
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ProtocolError {
    /// An error was encountered while decoding a request or response.
    DecodeError(DecodeError),

    /// An error that occurs when too much time has passed from the start of message decoding. The
    /// timer starts whenever the information line of a request or response is encountered.
    /// Preceding newlines do not start the timer.
    DecodingTimedOut,

    /// An underlying I/O error occurred either in the stream or sink.
    IO(Arc<io::Error>),

    /// The underlying stream has ended, but there is still leftover data that is not enough for a
    /// full request or response to be decoded from.
    UnexpectedEOF,
}

impl Display for ProtocolError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ProtocolError::*;

        match self {
            DecodeError(error) => error.fmt(formatter),
            DecodingTimedOut => write!(formatter, "decoding timed out"),
            IO(error) => error.fmt(formatter),
            UnexpectedEOF => write!(formatter, "unexpected EOF"),
        }
    }
}

impl Error for ProtocolError {}

impl From<Infallible> for ProtocolError {
    fn from(_: Infallible) -> Self {
        ProtocolError::DecodingTimedOut
    }
}

impl From<io::Error> for ProtocolError {
    fn from(value: io::Error) -> ProtocolError {
        ProtocolError::IO(Arc::new(value))
    }
}

/// An error was encountered while decoding a request or response.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum DecodeError {
    /// An error was encountered while decoding a request.
    Request(RequestDecodeError),

    /// An error was encountered while decoding a response.
    Response(ResponseDecodeError),
}

impl Display for DecodeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::DecodeError::*;

        match self {
            Request(error) => error.fmt(formatter),
            Response(error) => error.fmt(formatter),
        }
    }
}

impl Error for DecodeError {}

impl From<Infallible> for DecodeError {
    fn from(_: Infallible) -> Self {
        DecodeError::Request(RequestDecodeError::InvalidContentLength)
    }
}

impl From<RequestDecodeError> for DecodeError {
    fn from(value: RequestDecodeError) -> Self {
        DecodeError::Request(value)
    }
}

impl From<ResponseDecodeError> for DecodeError {
    fn from(value: ResponseDecodeError) -> Self {
        DecodeError::Response(value)
    }
}

#[cfg(test)]
mod test {
    use bytes::BytesMut;
    use futures::stream::Stream;
    use futures::sync::mpsc::unbounded;
    use std::convert::TryFrom;
    use tokio::runtime::current_thread::Runtime;
    use tokio_codec::{Decoder, Encoder};

    use crate::header::name::HeaderName;
    use crate::header::types::ContentLength;
    use crate::header::value::HeaderValue;
    use crate::method::Method;
    use crate::protocol::codec::{Codec, CodecEvent, Message};
    use crate::request::Request;
    use crate::response::Response;
    use crate::uri::request::URI;

    #[test]
    fn test_codec_decoding() {
        let mut codec = Codec::new();
        let mut buffer = BytesMut::from(
            "SETUP * RTSP/2.0\r\n\
             Content-Length: 4\r\n\
             \r\n\
             Body",
        );
        let mut builder = Request::builder();
        builder
            .method(Method::Setup)
            .uri(URI::asterisk())
            .header(
                HeaderName::ContentLength,
                HeaderValue::try_from("4").unwrap(),
            )
            .body(BytesMut::from("Body"));
        let expected_request = builder.build().unwrap();
        assert_eq!(
            codec.decode(&mut buffer).unwrap().unwrap(),
            Message::Request(expected_request)
        );

        let mut buffer = BytesMut::from("RTSP/2.0 200 OK\r\n");
        assert_eq!(codec.decode(&mut buffer).unwrap(), None);

        let mut buffer = BytesMut::from("\r\n\r\n");
        let mut builder = Response::builder();
        builder.body(BytesMut::new());
        let response = builder.build().unwrap();
        assert_eq!(
            codec.decode(&mut buffer).unwrap().unwrap(),
            Message::Response(response)
        );
    }

    #[test]
    fn test_codec_encoding() {
        let mut codec = Codec::new();
        let mut buffer = BytesMut::new();
        let mut builder = Request::builder();
        builder
            .method(Method::Setup)
            .uri(URI::asterisk())
            .typed_header(ContentLength::try_from(4).unwrap())
            .body(BytesMut::from("Body"));
        let request = builder.build().unwrap();
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
            let mut builder = Response::builder();
            builder.body(BytesMut::new());
            let response = builder.build().unwrap();
            codec
                .encode(Message::Response(response), &mut buffer)
                .unwrap();

            let mut buffer = BytesMut::from("SETUP * RTSP/2.0\r\n");
            assert!(codec.decode(&mut buffer).is_ok());

            let mut buffer = BytesMut::new();
            let mut builder = Response::builder();
            builder.body(BytesMut::new());
            let response = builder.build().unwrap();
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
