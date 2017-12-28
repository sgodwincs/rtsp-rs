//! RTSP Response Types
//!
//! This module contains structs related to RTSP responses, notably the `Response` type itself as
//! well as a builder to create responses. Typically, you will import the `rtsp::Response` type
//! rather than reaching into this module itself.

use std::convert::TryFrom;
use std::{error, fmt};
use std::mem::replace;

use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader, TypedHeaderMap};
use reason::ReasonPhrase;
use status::StatusCode;
use version::Version;

/// Represents an RTSP response.
///
/// An RTSP response consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
///
/// This struct implements `PartialEq` but care should be taken when using it. Two requests can
/// be semantically equivalent but not be byte by byte. This will mainly occur due to extra spaces
/// in headers. Even when using a typed request, the same problem will occur.
///
/// Note that it is not necessary to ever set the `Content-Length` header as it will be forcibly
/// set during encoding even if it is already present.
#[derive(Clone, Eq, PartialEq)]
pub struct Response<B, H = HeaderMap<HeaderValue>>
where
    H: Default,
{
    /// The body component of the response. This is generic to support arbitrary content types.
    body: B,

    /// Specifies a reason phrase for the given status code. RTSP allows agents to give custom
    /// reason phrases and even recommends it in specific cases.
    reason_phrase: ReasonPhrase,

    /// A header map that will either be `HeaderMap<HeaderValue>` or `TypedHeaderMap`.
    headers: H,

    /// The status code of the response.
    status_code: StatusCode,

    /// The protocol version that is being used.
    version: Version,
}

impl Response<()> {
    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn typed_builder() -> Builder<TypedHeaderMap> {
        Builder::new()
    }
}

impl<B, H> Response<B, H>
where
    H: Default,
{
    pub fn body(&self) -> &B {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut B {
        &mut self.body
    }

    pub fn headers(&self) -> &H {
        &self.headers
    }

    pub fn headers_mut(&mut self) -> &mut H {
        &mut self.headers
    }

    pub fn map<T, F>(self, mut f: F) -> Response<T, H>
    where
        F: FnMut(B) -> T,
    {
        let body = f(self.body);

        Response {
            body: body,
            headers: self.headers,
            reason_phrase: self.reason_phrase,
            status_code: self.status_code,
            version: self.version,
        }
    }

    pub fn reason(&self) -> &ReasonPhrase {
        &self.reason_phrase
    }

    pub fn reason_mut(&mut self) -> &mut ReasonPhrase {
        &mut self.reason_phrase
    }

    pub fn status_code(&self) -> &StatusCode {
        &self.status_code
    }

    pub fn status_code_mut(&mut self) -> &mut StatusCode {
        &mut self.status_code
    }

    pub fn version(&self) -> Version {
        self.version
    }

    pub fn version_mut(&mut self) -> &mut Version {
        &mut self.version
    }
}

impl<B> Response<B, HeaderMap<HeaderValue>> {
    pub fn into_typed(self) -> Response<B, TypedHeaderMap> {
        Response {
            body: self.body,
            headers: self.headers.into(),
            reason_phrase: self.reason_phrase,
            status_code: self.status_code,
            version: self.version,
        }
    }
}

impl<B> Response<B, TypedHeaderMap> {
    pub fn into_untyped(self) -> Response<B, HeaderMap<HeaderValue>> {
        Response {
            body: self.body,
            headers: self.headers.into(),
            reason_phrase: self.reason_phrase,
            status_code: self.status_code,
            version: self.version,
        }
    }
}

impl<B, H> fmt::Debug for Response<B, H>
where
    B: fmt::Debug,
    H: fmt::Debug + Default,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Response")
            .field("version", &self.version())
            .field("status", self.status_code())
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
pub struct Builder<H = HeaderMap<HeaderValue>>
where
    H: Default,
{
    /// Specifies a custom reason phrase for the given status code. RTSP allows agents to give
    /// custom reason phrases and even recommends it in specific cases. If it is detected that the
    /// status code is an extension or that the reason phrase is not the canonical reason phrase for
    /// the given status code, then this will be the custom reason phrase.
    pub(crate) custom_reason_phrase: Option<ReasonPhrase>,

    /// A stored error used when making a `Response`.
    pub(crate) error: Option<BuilderError>,

    /// A header map that will either be `HeaderMap<HeaderValue>` or `TypedHeaderMap`.
    pub(crate) headers: H,

    /// The status code of the response.
    pub(crate) status_code: StatusCode,

    /// The protocol version that is being used.
    pub(crate) version: Version,
}

impl<H> Builder<H>
where
    H: Default,
{
    /// Creates a new default instance of a `Builder` to construct a `Response`.
    pub fn new() -> Builder<H> {
        Builder::default()
    }

    /// Consumes this builder, using the provided `body` to return a constructed `Response`.
    ///
    /// # Errors
    ///
    /// This function may return an error if part of the response is invalid (such as a header).
    pub fn build<B>(&mut self, body: B) -> Result<Response<B, H>, BuilderError> {
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
            headers: replace(&mut self.headers, H::default()),
            reason_phrase,
            status_code: self.status_code,
            version: self.version,
        })
    }

    /// Set the reason phrase for this response.
    ///
    /// This function will configure the RTSP reason phrase of the `Response` that will be returned
    /// from `build`. If a reason phrase is not given or `None` is supplied here, the canonical
    /// reason phrase for the given `StatusCode` will be used. If the status code does not have a
    /// canonical reason phrase (it is an extension), the `build` function will return an error.
    ///
    /// The given reason phrase must be non-empty and be valid UTF-8 with a subset of characters
    /// allowed from ASCII-US. If these conditions are not met, the `build` function will return an
    /// error.
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
    /// This function will configure the RTSP version of the `Response` that will be returned from
    /// `build`.
    ///
    /// The default value for the status code is 200.
    ///
    /// # Errors
    ///
    /// If the given status code is not a valid status code, an error will be saved and retured when
    /// the `build` function is invoked.
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
    /// This function will configure the RTSP version of the `Response` that will be returned from
    /// `build`.
    ///
    /// The default value for the version is RTSP/2.0.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let response = Response::builder()
    ///     .version(Version::RTSP10)
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn version<T>(&mut self, version: T) -> &mut Self
    where
        Version: TryFrom<T>,
    {
        match Version::try_from(version) {
            Ok(version) => self.version = version,
            Err(_) => self.error = Some(BuilderError::InvalidVersion),
        }

        self
    }
}

impl Builder<HeaderMap<HeaderValue>> {
    /// Appends a header to this response.
    ///
    /// This function will append the provided key/value as a header to the internal `HeaderMap`
    /// being constructued. Essentially, this is equivalent to calling `HeaderMap::append`. Because
    /// of this, you are able to add a given header multiple times.
    ///
    /// By default, the response contains no headers.
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
    pub fn header<K, V>(&mut self, key: K, value: V) -> &mut Self
    where
        HeaderName: TryFrom<K>,
        HeaderValue: TryFrom<V>,
    {
        match HeaderName::try_from(key) {
            Ok(key) => match HeaderValue::try_from(value) {
                Ok(value) => {
                    self.headers.append(key, value);
                }
                Err(_) => self.error = Some(BuilderError::InvalidHeaderValue),
            },
            Err(_) => self.error = Some(BuilderError::InvalidHeaderName),
        }

        self
    }

    /// Converts this builder into a builder that contains typed headers.
    pub fn into_typed(self) -> Builder<TypedHeaderMap> {
        Builder {
            custom_reason_phrase: self.custom_reason_phrase,
            error: self.error,
            headers: self.headers.into(),
            status_code: self.status_code,
            version: self.version,
        }
    }
}

impl Builder<TypedHeaderMap> {
    /// Sets a typed header for this request. Since typed headers are used here, this function
    /// cannot produce an error for the builder.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::*;
    ///
    /// let response = Response::typed_builder()
    ///     .status_code(StatusCode::OK)
    ///     .header(ContentLength::from(5))
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn header<H: TypedHeader>(&mut self, header: H) -> &mut Self {
        self.headers.set(header);
        self
    }

    /// Sets a raw header for this request. Since typed headers are used here, this function cannot
    /// produce an error for the builder.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    ///
    /// let response = Response::typed_builder()
    ///     .status_code(StatusCode::OK)
    ///     .header_raw(HeaderName::ContentLength, vec![HeaderValue::try_from("5").unwrap()])
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn header_raw(&mut self, name: HeaderName, value: Vec<HeaderValue>) -> &mut Self {
        self.headers.set_raw(name, value);
        self
    }

    /// Converts this builder into a builder that contains untyped headers.
    pub fn into_untyped(self) -> Builder<HeaderMap<HeaderValue>> {
        Builder {
            custom_reason_phrase: self.custom_reason_phrase,
            error: self.error,
            headers: self.headers.into(),
            status_code: self.status_code,
            version: self.version,
        }
    }
}

impl<H> Default for Builder<H>
where
    H: Default,
{
    #[inline]
    fn default() -> Self {
        Builder {
            custom_reason_phrase: None,
            error: None,
            headers: H::default(),
            status_code: StatusCode::default(),
            version: Version::default(),
        }
    }
}

/// An error type for when the response builder encounters an error.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuilderError {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidReasonPhrase,
    InvalidStatusCode,
    InvalidVersion,
    MissingReasonPhrase,
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::error::Error;

        write!(f, "{}", self.description())
    }
}

impl error::Error for BuilderError {
    fn description(&self) -> &str {
        use self::BuilderError::*;

        match self {
            &InvalidHeaderName => "invalid RTSP header name",
            &InvalidHeaderValue => "invalid RTSP header value",
            &InvalidReasonPhrase => "invalid RTSP reason phrase",
            &InvalidStatusCode => "invalid RTSP status code",
            &InvalidVersion => "invalid RTSP version",
            &MissingReasonPhrase => "missing RTSP reason phrase",
        }
    }
}
