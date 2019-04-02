macro_rules! try_complete {
    ($result:expr) => {
        match $result {
            crate::protocol::codec::decoder::DecodeResult::Complete(value) => value,
            crate::protocol::codec::decoder::DecodeResult::Error(error) => {
                return crate::protocol::codec::decoder::DecodeResult::Error(error.into());
            }
            crate::protocol::codec::decoder::DecodeResult::Incomplete => {
                return crate::protocol::codec::decoder::DecodeResult::Incomplete;
            }
        }
    };
}

pub mod request;
pub mod response;

/// The default maximum length a request or response body can be.
const BODY_DEFAULT_MAX_LENGTH: usize = 65536;

/// The default maximum count of headers allowed in a request or response.
const HEADER_DEFAULT_MAX_COUNT: usize = 64;

/// The default maximum length a header name can be in a request or response.
const HEADER_NAME_DEFAULT_MAX_LENGTH: usize = 100;

/// The default maximum length a header value can be in a request or response.
const HEADER_VALUE_DEFAULT_MAX_LENGTH: usize = 2000;

/// The default maximum length a method can be in a request.
const METHOD_DEFAULT_MAX_LENGTH: usize = 50;

/// The default maximum length a reason phrase can be in a response.
const REASON_PHRASE_DEFAULT_MAX_LENGTH: usize = 50;

/// The default maximum length a URI can be in a request.
const URI_DEFAULT_MAX_LENGTH: usize = 2000;

/// The result of request/response parsing.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum DecodeResult<TResult, TError> {
    /// The decoder has succesfully decoded the request/response from the given buffer, and it is
    /// returned within this variant.
    ///
    /// The given buffer for the decode function may still contain more request/response data that
    /// can be decoded even after this is returned. This is because the decode function will only
    /// decode up to the end of a request/response. In this case, you need to keep decoding the
    /// buffer until either an error occurs or more data is needed (which may just be EOF).
    Complete(TResult),

    /// The decoder has encountered an error during parsing. The number of bytes decoded returned
    /// is not reliable in this case.
    Error(TError),

    /// The decoder cannot continue decoding without more data. In this case, everything that has
    /// been decoded is stored within the decoder and another call to the decoder with more data can
    /// finish the decoding.
    ///
    /// The given buffer for the decode function may not have been completely decoded and should be
    /// extended with the rest of the request/response data. It is also not completely clear that
    /// more data is actually needed. If you were to give the decoder a buffer that contained just
    /// one request and called it again afterwards on the rest of the same buffer, you would get an
    /// [`DecodeResult::Incomplete`]. But this would just be synonymous with EOF since a partial
    /// request/response was not actually decoded. In this case, it is up to the caller to figure
    /// out whether more data is needed or if it was just EOF.
    Incomplete,
}

impl<TResult, TError> DecodeResult<TResult, TError> {
    /// Returns [`true`] if the decode result is [`DecodeResult::Complete`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Complete(());
    /// assert_eq!(result.is_complete(), true);
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Error(());
    /// assert_eq!(result.is_complete(), false);
    /// ```
    pub fn is_complete(&self) -> bool {
        match *self {
            DecodeResult::Complete(_) => true,
            _ => false,
        }
    }

    /// Returns [`true`] if the decode result is [`DecodeResult::Error`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Error(());
    /// assert_eq!(result.is_error(), true);
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Incomplete;
    /// assert_eq!(result.is_error(), false);
    /// ```
    pub fn is_error(&self) -> bool {
        match *self {
            DecodeResult::Error(_) => true,
            _ => false,
        }
    }

    /// Returns [`true`] if the decode result is [`DecodeResult::Incomplete`].
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Incomplete;
    /// assert_eq!(result.is_incomplete(), true);
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Error(());
    /// assert_eq!(result.is_incomplete(), false);
    /// ```
    pub fn is_incomplete(&self) -> bool {
        match *self {
            DecodeResult::Incomplete => true,
            _ => false,
        }
    }

    /// Maps the value within the [`DecodeResult::Complete`] variant into the value returned from
    /// the mapper function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Complete(());
    /// assert_eq!(result.map(|_| 5).unwrap(), 5);
    /// ```
    pub fn map<TNewResult, TMapper>(self, mapper: TMapper) -> DecodeResult<TNewResult, TError>
    where
        TMapper: FnOnce(TResult) -> TNewResult,
    {
        match self {
            DecodeResult::Complete(value) => DecodeResult::Complete(mapper(value)),
            DecodeResult::Error(error) => DecodeResult::Error(error),
            DecodeResult::Incomplete => DecodeResult::Incomplete,
        }
    }

    /// Maps the value within the [`DecodeResult::Complete`] variant into the value returned from
    /// the mapper function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Error(());
    /// assert_eq!(result.map_error(|_| 5).unwrap_error(), 5);
    /// ```
    pub fn map_error<TNewError, TMapper>(self, mapper: TMapper) -> DecodeResult<TResult, TNewError>
    where
        TMapper: FnOnce(TError) -> TNewError,
    {
        match self {
            DecodeResult::Complete(value) => DecodeResult::Complete(value),
            DecodeResult::Error(error) => DecodeResult::Error(mapper(error)),
            DecodeResult::Incomplete => DecodeResult::Incomplete,
        }
    }

    /// Unwraps the decode result, yielding the content of the [`DecodeResult::Complete`].
    ///
    /// # Panics
    ///
    /// Panics if the value is not `Complete`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Complete(());
    /// assert_eq!(result.unwrap(), ());
    /// ```
    ///
    /// ```{.should_panic}
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Incomplete;
    /// result.unwrap();
    /// ```
    pub fn unwrap(self) -> TResult {
        match self {
            DecodeResult::Complete(value) => value,
            _ => panic!("unwrapped decode result that was not complete"),
        }
    }

    /// Unwraps the decode result, yielding the content of the [`DecodeResult::Error`].
    ///
    /// # Panics
    ///
    /// Panics if the value is not `Error`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Error(());
    /// assert_eq!(result.unwrap_error(), ());
    /// ```
    ///
    /// ```should_panic
    /// use rtsp::protocol::codec::decoder::DecodeResult;
    ///
    /// let result: DecodeResult<(), ()> = DecodeResult::Incomplete;
    /// result.unwrap_error();
    /// ```
    pub fn unwrap_error(self) -> TError {
        match self {
            DecodeResult::Error(error) => error,
            _ => panic!("unwrapped decode result that was not error"),
        }
    }
}

impl<TResult, TError> From<Result<TResult, TError>> for DecodeResult<TResult, TError> {
    fn from(value: Result<TResult, TError>) -> DecodeResult<TResult, TError> {
        match value {
            Ok(value) => DecodeResult::Complete(value),
            Err(error) => DecodeResult::Error(error),
        }
    }
}

/// Trims the given header name of any trailing spaces or tabs.
fn trim_header_name(name: &[u8]) -> &[u8] {
    let mut index = name.len();

    while index > 0 {
        let byte = name[index - 1];

        if byte == b' ' || byte == b'\t' {
            index -= 1
        } else {
            break;
        }
    }

    &name[0..index]
}

/// Trims the given header value of any leading spaces or tabs, including an initial linebreak.
fn trim_header_value(value: &[u8]) -> &[u8] {
    let has_initial_linebreak = value.starts_with(b"\r\n");
    let mut index = if has_initial_linebreak { 2 } else { 0 };

    while index < value.len() {
        let byte = value[index];

        if byte == b' ' || byte == b'\t' {
            index += 1;
        } else {
            break;
        }
    }

    if has_initial_linebreak && index == 2 {
        value
    } else {
        &value[index..]
    }
}

#[cfg(test)]
mod test {
    use crate::protocol::codec::decoder;

    #[test]
    fn test_trim_header_name() {
        assert_eq!(decoder::trim_header_name(b""), b"");
        assert_eq!(decoder::trim_header_name(b"                     "), b"");
        assert_eq!(decoder::trim_header_name(b"test"), b"test");
        assert_eq!(decoder::trim_header_name(b"test \t\t\t\t \t \t"), b"test");
    }

    #[test]
    fn test_trim_header_value() {
        assert_eq!(decoder::trim_header_value(b""), b"");
        assert_eq!(decoder::trim_header_value(b"test"), b"test");
        assert_eq!(decoder::trim_header_value(b" \t\t       "), b"");
        assert_eq!(decoder::trim_header_value(b"\r\ntest"), b"\r\ntest");
        assert_eq!(decoder::trim_header_value(b"\r\n test"), b"test");
        assert_eq!(decoder::trim_header_value(b"\r\n \t\t\t\t  test"), b"test");
        assert_eq!(decoder::trim_header_value(b" \t\t\t\t  test"), b"test");
        assert_eq!(decoder::trim_header_value(b" \t\t\r\n test"), b"\r\n test");
    }
}
