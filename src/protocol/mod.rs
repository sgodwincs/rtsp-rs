pub mod codec;
pub mod connection;
pub mod decoder;
pub mod encoder;
pub mod service;

pub use self::codec::{
    Codec, CodecEvent, DecodeError, InvalidMessage, IrrecoverableInvalidRequest,
    IrrecoverableInvalidResponse, Message, MessageResult, OperationError, ProtocolError,
    RecoverableInvalidRequest, RecoverableInvalidResponse, RequestResult, RequestTimeoutType,
    ResponseResult,
};
pub use self::connection::{
    Config, ConfigBuilder, ConfigBuilderError, Connection, ReadState, ReadWriteStatePair,
    ShutdownType, WriteState, DEFAULT_DECODE_TIMEOUT_DURATION, DEFAULT_REQUEST_BUFFER_SIZE,
    DEFAULT_REQUEST_MAX_TIMEOUT_DURATION, DEFAULT_REQUEST_TIMEOUT_DURATION,
    DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION,
};
pub use self::decoder::{
    InvalidRequest, InvalidResponse, ParseResult, ParseState, RequestDecoder, RequestParseResult,
    ResponseDecoder, ResponseParseResult,
};
pub use self::encoder::{encode_request, encode_response};
pub use self::service::{EmptyService, Service};
