//! RTSP Response Types
//!
//! This module contains structs related to RTSP responses, notably the `Response` type itself as
//! well as a builder to create responses. Typically, you will import the `rtsp::Response` type
//! rather than reaching into this module itself.

use bytes::BytesMut;
use std::convert::TryFrom;
use std::error::Error;
use std::fmt;
use std::mem::replace;

use crate::header::{HeaderMap, HeaderMapExtension, HeaderName, HeaderValue, TypedHeader};
use crate::reason::ReasonPhrase;
use crate::status::StatusCode;
use crate::version::Version;

lazy_static! {
    pub(crate) static ref BAD_REQUEST_RESPONSE: Response<BytesMut> = Response::builder()
        .status_code(StatusCode::BadRequest)
        .build(BytesMut::new())
        .expect("bad request response should not be invalid");
    pub(crate) static ref NOT_IMPLEMENTED_RESPONSE: Response<BytesMut> = Response::builder()
        .status_code(StatusCode::NotImplemented)
        .build(BytesMut::new())
        .expect("not implemented response should not be invalid");
}

/// Represents an RTSP response.
///
/// An RTSP response consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
///
/// This struct implements `PartialEq` but care should be taken when using it. Two responses can
/// be semantically equivalent but not be byte by byte. This will mainly occur due to extra spaces
/// in headers. Even when using a typed response, the same problem will occur.
///
/// Note that it is not necessary to ever set the `Content-Length` header as it will be forcibly
/// set during encoding even if it is already present.
#[derive(Clone, Eq, PartialEq)]
pub struct Response<B> {
    /// The body component of the response. This is generic to support arbitrary content types.
    body: B,

    /// Specifies a reason phrase for the given status code. RTSP allows agents to give custom
    /// reason phrases and even recommends it in specific cases.
    reason_phrase: ReasonPhrase,

    /// A header map that will either be `HeaderMap` or `TypedHeaderMap`.
    headers: HeaderMap,

    /// The status code of the response.
    status_code: StatusCode,

    /// The protocol version that is being used.
    version: Version,
}

impl Response<()> {
    /// Constructs a new builder that uses untyped headers.
    pub fn builder() -> Builder {
        Builder::new()
    }
}

impl<B> Response<B> {
    /// Returns an immutable reference to the response body.
    pub fn body(&self) -> &B {
        &self.body
    }

    /// Returns a mutable reference to the response body. To change the type of the body, use the
    /// `map` function.
    pub fn body_mut(&mut self) -> &mut B {
        &mut self.body
    }

    /// Returns an immutable reference to the response header map.
    pub fn headers(&self) -> &HeaderMap {
        &self.headers
    }

    /// Returns a mutable reference to the response header map.
    pub fn headers_mut(&mut self) -> &mut HeaderMap {
        &mut self.headers
    }

    /// Maps the body of this response to a new type `T` using the provided function.
    pub fn map<T, F>(self, mut mapper: F) -> Response<T>
    where
        F: FnMut(B) -> T,
    {
        Response {
            body: mapper(self.body),
            headers: self.headers,
            reason_phrase: self.reason_phrase,
            status_code: self.status_code,
            version: self.version,
        }
    }

    /// Returns an immutable reference to the response reason.
    pub fn reason(&self) -> &ReasonPhrase {
        &self.reason_phrase
    }

    /// Returns a mutable reference to the response reason.
    pub fn reason_mut(&mut self) -> &mut ReasonPhrase {
        &mut self.reason_phrase
    }

    /// Returns a copy of the response status code.
    pub fn status_code(&self) -> StatusCode {
        self.status_code
    }

    /// Returns a mutable reference to the response status code.
    pub fn status_code_mut(&mut self) -> &mut StatusCode {
        &mut self.status_code
    }

    /// Returns a copy of the response version.
    pub fn version(&self) -> Version {
        self.version
    }
}

impl<B> fmt::Debug for Response<B>
where
    B: fmt::Debug,
{
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter
            .debug_struct("Response")
            .field("version", &self.version())
            .field("status", &self.status_code())
            .field("reason", self.reason())
            .field("headers", self.headers())
            .field("body", self.body())
            .finish()
    }
}

/// An RTSP response builder
///
/// This type can be used to construct a `Response` through a builder-like pattern.
#[derive(Clone, Debug)]
pub struct Builder {
    /// Specifies a custom reason phrase for the given status code. RTSP allows agents to give
    /// custom reason phrases and even recommends it in specific cases. If it is detected that the
    /// status code is an extension or that the reason phrase is not the canonical reason phrase for
    /// the given status code, then this will be the custom reason phrase.
    pub(crate) custom_reason_phrase: Option<ReasonPhrase>,

    /// A stored error used when making a `Response`.
    pub(crate) error: Option<BuilderError>,

    /// A header map that will either be `HeaderMap` or `TypedHeaderMap`.
    pub(crate) headers: HeaderMap,

    /// The status code of the response.
    pub(crate) status_code: StatusCode,

    /// The protocol version that is being used.
    pub(crate) version: Version,
}

impl Builder {
    /// Creates a new default instance of a `Builder` to construct a `Response`.
    pub fn new() -> Builder {
        Builder::default()
    }

    /// Constructs a `Response` by using the given body. Note that this function does not consume
    /// the builder, allowing you to construct responses with different bodies with the same
    /// builder, but all of the fields will be reset.
    ///
    /// # Errors
    ///
    /// An error will be returned if part of the response is invalid.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn build<B>(&mut self, body: B) -> Result<Response<B>, BuilderError> {
        if let Some(error) = self.error {
            return Err(error);
        }

        let reason_phrase = if let StatusCode::Extension(_) = self.status_code {
            if self.custom_reason_phrase.is_none() {
                return Err(BuilderError::MissingReasonPhrase);
            }

            self.custom_reason_phrase.take().unwrap()
        } else {
            self.status_code
                .canonical_reason()
                .expect("status code should be standard")
                .clone()
        };

        Ok(Response {
            body,
            headers: replace(&mut self.headers, HeaderMap::new()),
            reason_phrase,
            status_code: self.status_code,
            version: self.version,
        })
    }

    /// Set the reason phrase for this response.
    ///
    /// # Errors
    ///
    /// An error will be stored if the given reason phrase is `Some(T)` where `T` is an invalid
    /// `ReasonPhrase`. Note that if a extension status code is specified, you *must* specify a
    /// reason phrase or an error will be returned during the `build` function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .status_code(StatusCode::OK)
    ///     .reason(Some("Good Response"))
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn reason<T>(&mut self, reason_phrase: Option<T>) -> &mut Self
    where
        ReasonPhrase: TryFrom<T>,
    {
        if let Some(custom_reason_phrase) = reason_phrase {
            match ReasonPhrase::try_from(custom_reason_phrase) {
                Ok(custom_reason_phrase) => self.custom_reason_phrase = Some(custom_reason_phrase),
                Err(_) => self.error = Some(BuilderError::InvalidReasonPhrase),
            }
        } else {
            self.custom_reason_phrase = None;
        }

        self
    }

    /// Set the status code for this response.
    ///
    /// The default value for the status code is 200.
    ///
    /// # Errors
    ///
    /// An error will be stored if the given status code is an invalid `StatusCode`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .status_code(StatusCode::MovedPermanently)
    ///     .header("Location", "rtsp://example.com/resource")
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn status_code<T>(&mut self, status_code: T) -> &mut Self
    where
        StatusCode: TryFrom<T>,
    {
        match StatusCode::try_from(status_code) {
            Ok(status_code) => self.status_code = status_code,
            Err(_) => self.error = Some(BuilderError::InvalidStatusCode),
        }

        self
    }

    /// Set the RTSP version for this response.
    ///
    /// The default value for the version is RTSP/2.0.
    ///
    /// # Errors
    ///
    /// An error will be stored if the given version is an invalid or unsupported `Version`.
    /// Currently the only supported version is RTSP/2.0.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .version(Version::RTSP20)
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn version<T>(&mut self, version: T) -> &mut Self
    where
        Version: TryFrom<T>,
    {
        match Version::try_from(version) {
            Ok(version) if version == Version::RTSP20 => self.version = version,
            Ok(_) => self.error = Some(BuilderError::UnsupportedVersion),
            Err(_) => self.error = Some(BuilderError::InvalidVersion),
        }

        self
    }
}

impl Builder {
    /// Appends a header to this response.
    ///
    /// This function will append the provided key/value as a header to the internal `HeaderMap`
    /// being constructed. Essentially, this is equivalent to calling `HeaderMap::append`.
    /// Because of this, you are able to add a given header multiple times.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Errors
    ///
    /// An error will be stored if the given name is an invalid `HeaderName`, or the given value is
    /// an invalid `HeaderValue`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .status_code(StatusCode::MovedPermanently)
    ///     .header("Location", "rtsp://example.com/resource")
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn header<K, V>(&mut self, name: K, value: V) -> &mut Self
    where
        HeaderName: TryFrom<K>,
        HeaderValue: TryFrom<V>,
    {
        match HeaderName::try_from(name) {
            Ok(name) => match HeaderValue::try_from(value) {
                Ok(value) => {
                    self.headers.append(name, value);
                }
                Err(_) => self.error = Some(BuilderError::InvalidHeaderValue),
            },
            Err(_) => self.error = Some(BuilderError::InvalidHeaderName),
        }

        self
    }

    /// Sets a typed header for this response.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Errors
    ///
    /// Since typed headers are used here, this function cannot produce an error for the builder.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::*;
    ///
    /// let response = Response::builder()
    ///     .status_code(StatusCode::OK)
    ///     .typed_header(ContentLength::try_from(5).unwrap())
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn typed_header<H: TypedHeader>(&mut self, header: H) -> &mut Self {
        self.headers.typed_insert(header);
        self
    }
}

impl Default for Builder {
    #[inline]
    fn default() -> Self {
        Builder {
            custom_reason_phrase: None,
            error: None,
            headers: HeaderMap::default(),
            status_code: StatusCode::default(),
            version: Version::default(),
        }
    }
}

/// An error type for when the response builder encounters an error.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum BuilderError {
    /// This error indicates that a given header name was invalid.
    InvalidHeaderName,

    /// This error indicates that a given header value was invalid.
    InvalidHeaderValue,

    /// This error indicates that the reason phrase was invalid.
    InvalidReasonPhrase,

    /// This error indicates that the status code was invalid.
    InvalidStatusCode,

    /// This error indicates that the version was invalid.
    InvalidVersion,

    /// This error indicates that an extension status code was given, but a corresponding reason
    /// phrase was not given. There is no default reason phrase to use for extensions, so one must
    /// be specified.
    MissingReasonPhrase,

    /// This error indicates that the version was unsupported. The only supported version is
    /// RTSP 2.0.
    UnsupportedVersion,
}

impl fmt::Display for BuilderError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for BuilderError {
    fn description(&self) -> &str {
        use self::BuilderError::*;

        match self {
            &InvalidHeaderName => "invalid RTSP header name",
            &InvalidHeaderValue => "invalid RTSP header value",
            &InvalidReasonPhrase => "invalid RTSP reason phrase",
            &InvalidStatusCode => "invalid RTSP status code",
            &InvalidVersion => "invalid RTSP version",
            &MissingReasonPhrase => "missing RTSP reason phrase",
            &UnsupportedVersion => "unsupported RTSP version",
        }
    }
}
