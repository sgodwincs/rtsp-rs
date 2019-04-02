//! Response
//!
//! This module contains structs related to RTSP responses, notably the [`Response`] type itself as
//! well as a builder to create responses.

use bytes::BytesMut;
use lazy_static::lazy_static;
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use crate::header::map::{HeaderMap, HeaderMapExtension, TypedHeader};
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;
use crate::reason::ReasonPhrase;
use crate::status::StatusCode;
use crate::version::Version;

lazy_static! {
    pub(crate) static ref BAD_REQUEST_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::BadRequest)
            .with_body(BytesMut::new())
            .build()
            .expect("bad request response should not be invalid");
    pub(crate) static ref CONTINUE_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::Continue)
            .with_body(BytesMut::new())
            .build()
            .expect("continue response should not be invalid");
    pub(crate) static ref INTERNAL_SERVER_ERROR_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::InternalServerError)
            .with_body(BytesMut::new())
            .build()
            .expect("internal server error response should not be invalid");
    pub(crate) static ref NOT_ENOUGH_BANDWIDTH_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::NotEnoughBandwidth)
            .with_body(BytesMut::new())
            .build()
            .expect("not enough bandwidth response should not be invalid");
    pub(crate) static ref NOT_IMPLEMENTED_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::NotImplemented)
            .with_body(BytesMut::new())
            .build()
            .expect("not implemented response should not be invalid");

    // TODO: As per specification, the "response SHOULD contain a message body describing why
    // that version is not supported and what other protocols are supported by that agent".
    pub(crate) static ref VERSION_NOT_SUPPORTED_RESPONSE: Response<BytesMut> =
        Response::<()>::builder()
            .with_status_code(StatusCode::RTSPVersionNotSupported)
            .with_body(BytesMut::new())
            .build()
            .expect("RTSP version not supported response should not be invalid");
}

/// Represents an RTSP response.
///
/// An RTSP response consists of a status code, reason phrase, version, headers and a, potentially
/// empty, body. The body component is generic, enabling arbitrary types to represent the RTSP body.
///
/// This struct implements [`PartialEq`] but care should be taken when using it. Two responses can
/// be semantically equivalent but not be byte by byte. This will mainly occur due to extra spaces
/// in headers. Even when using a typed response, the same problem will occur.
///
/// It is not necessary to ever set the `"Content-Length"` header as it will be forcibly set during
/// encoding even if it is already present.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Response<TBody> {
    /// The body component of the response. This is generic to support arbitrary content types.
    body: TBody,

    /// Specifies a reason phrase for the given status code. RTSP allows agents to give custom
    /// reason phrases and even recommends it in specific cases.
    reason_phrase: ReasonPhrase,

    /// A mapping of all header names to their values. Headers can be repeated here.
    headers: HeaderMap,

    /// The status code of the response.
    status_code: StatusCode,

    /// The protocol version that is being used.
    version: Version,
}

impl<TBody> Response<TBody> {
    /// Returns a shared reference to the response body.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.body(), &"body");
    /// ```
    pub fn body(&self) -> &TBody {
        &self.body
    }

    /// Returns a mutable reference to the response body.
    ///
    /// To change the type of the body, use the [`Response::map`] function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let mut response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.body_mut(), &mut "body");
    /// ```
    pub fn body_mut(&mut self) -> &mut TBody {
        &mut self.body
    }

    /// Constructs a new builder.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.body(), &"body");
    /// ```
    pub fn builder() -> Builder<TBody> {
        Builder::new()
    }

    /// Constructs a response from its individual parts.
    ///
    /// # Errors
    ///
    /// An error will be returned if the combination of parts creates an invalid response.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::iter::FromIterator;
    ///
    /// use rtsp::header::map::HeaderMap;
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    /// use rtsp::version::Version;
    ///
    /// let response = Response::from_parts(
    ///     Version::RTSP20,
    ///     StatusCode::OK,
    ///     StatusCode::OK.canonical_reason().unwrap(),
    ///     HeaderMap::from_iter(vec![
    ///         (HeaderName::ContentLength, HeaderValue::try_from("0").unwrap())
    ///     ]),
    ///     "",
    /// ).unwrap();
    /// assert_eq!(response.version(), Version::RTSP20);
    /// assert_eq!(response.status_code(), StatusCode::OK);
    /// assert_eq!(response.reason_phrase(), &StatusCode::OK.canonical_reason().unwrap());
    /// assert_eq!(
    ///     response.headers().get(&HeaderName::ContentLength),
    ///     Some(&HeaderValue::try_from("0").unwrap())
    /// );
    /// assert_eq!(response.body(), &"");
    /// ```
    pub fn from_parts(
        version: Version,
        status_code: StatusCode,
        reason_phrase: ReasonPhrase,
        headers: HeaderMap,
        body: TBody,
    ) -> Result<Self, ResponseError> {
        let mut builder = Response::<()>::builder()
            .with_version(version)
            .with_status_code(status_code)
            .with_reason_phrase(Some(reason_phrase))
            .with_body(body);
        builder.headers = headers;
        builder.build()
    }

    /// Returns an shared reference to the response header map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::header::map::HeaderMapExtension;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_typed_header::<ContentLength>(ContentLength::default())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(
    ///     response.headers().typed_get::<ContentLength>(),
    ///     Some(ContentLength::default())
    /// );
    /// ```
    pub fn headers(&self) -> &HeaderMap {
        &self.headers
    }

    /// Returns a mutable reference to the response header map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::header::map::HeaderMapExtension;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::response::Response;
    ///
    /// let mut response = Response::<()>::builder()
    ///     .with_typed_header::<ContentLength>(ContentLength::default())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(
    ///     response.headers_mut().typed_get::<ContentLength>(),
    ///     Some(ContentLength::default())
    /// );
    /// ```
    pub fn headers_mut(&mut self) -> &mut HeaderMap {
        &mut self.headers
    }

    /// Converts the response into its corresponding builder.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let builder = response.clone().into_builder();
    /// let new_response = builder.build().unwrap();
    /// assert_eq!(response, new_response);
    /// ```
    pub fn into_builder(self) -> Builder<TBody> {
        Builder {
            body: Some(self.body),
            custom_reason_phrase: Some(self.reason_phrase),
            headers: self.headers,
            status_code: self.status_code,
            version: self.version,
        }
    }

    /// Converts the response into its individual components.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let (version, status_code, reason_phrase, headers, body) = response.clone().into_parts();
    /// let new_response = Response::from_parts(
    ///     version,
    ///     status_code,
    ///     reason_phrase,
    ///     headers,
    ///     body
    /// ).unwrap();
    /// assert_eq!(response, new_response);
    /// ```
    pub fn into_parts(self) -> (Version, StatusCode, ReasonPhrase, HeaderMap, TBody) {
        (
            self.version,
            self.status_code,
            self.reason_phrase,
            self.headers,
            self.body,
        )
    }

    /// Maps the body of this response to a new type `T` using the provided function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let mut response = Response::<()>::builder()
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let response = response.map(|_| 0);
    /// assert_eq!(response.body(), &0);
    /// ```
    pub fn map<TNewBody, TMapper>(self, mut mapper: TMapper) -> Response<TNewBody>
    where
        TMapper: FnMut(TBody) -> TNewBody,
    {
        Response {
            body: mapper(self.body),
            headers: self.headers,
            reason_phrase: self.reason_phrase,
            status_code: self.status_code,
            version: self.version,
        }
    }

    /// Returns an shared reference to the response reason.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.reason_phrase(), &StatusCode::OK.canonical_reason().unwrap());
    /// ```
    pub fn reason_phrase(&self) -> &ReasonPhrase {
        &self.reason_phrase
    }

    /// Returns a mutable reference to the response reason.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let mut response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.reason_phrase_mut(), &mut StatusCode::OK.canonical_reason().unwrap());
    /// ```
    pub fn reason_phrase_mut(&mut self) -> &mut ReasonPhrase {
        &mut self.reason_phrase
    }

    /// Returns a copy of the response status code.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_status_code(StatusCode::OK)
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.status_code(), StatusCode::OK);
    /// ```
    pub fn status_code(&self) -> StatusCode {
        self.status_code
    }

    /// Returns a mutable reference to the response status code.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let mut response = Response::<()>::builder()
    ///     .with_status_code(StatusCode::OK)
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.status_code_mut(), &mut StatusCode::OK);
    /// ```
    pub fn status_code_mut(&mut self) -> &mut StatusCode {
        &mut self.status_code
    }

    /// Returns a copy of the response version.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::version::Version;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(response.version(), Version::RTSP20);
    /// ```
    pub fn version(&self) -> Version {
        self.version
    }
}

/// An RTSP response builder.
///
/// This type can be used to construct a [`Response`] through a builder pattern.
#[derive(Clone, Debug)]
pub struct Builder<TBody> {
    /// The body component of the response. This is generic to support arbitrary content types.
    pub(crate) body: Option<TBody>,

    /// Specifies a custom reason phrase for the given status code. RTSP allows agents to give
    /// custom reason phrases and even recommends it in specific cases. If it is detected that the
    /// status code is an extension or that the reason phrase is not the canonical reason phrase for
    /// the given status code, then this will be the custom reason phrase.
    pub(crate) custom_reason_phrase: Option<ReasonPhrase>,

    /// A mapping of all header names to their values. Headers can be repeated here.
    pub(crate) headers: HeaderMap,

    /// The status code of the response.
    pub(crate) status_code: StatusCode,

    /// The protocol version that is being used.
    pub(crate) version: Version,
}

impl<TBody> Builder<TBody> {
    /// Set the body for this response.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let mut builder = Response::builder();
    /// builder.body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn body(&mut self, body: TBody) -> &mut Self {
        self.body = Some(body);
        self
    }

    /// Constructs a [`Response`] by consuming all fields set on this builder.
    ///
    /// # Errors
    ///
    /// An error will be returned if part of the response is missing.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let mut builder = Response::builder();
    /// builder.body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn build(mut self) -> Result<Response<TBody>, ResponseError> {
        let reason_phrase = if let StatusCode::Extension(_) = self.status_code {
            match self.custom_reason_phrase.take() {
                Some(reason_phrase) => reason_phrase,
                None => return Err(ResponseError::MissingReasonPhrase),
            }
        } else {
            self.status_code
                .canonical_reason()
                .expect("status code should be standard")
                .clone()
        };
        let body = self.body.ok_or(ResponseError::MissingBody)?;

        if self.version != Version::RTSP20 {
            return Err(ResponseError::UnsupportedVersion);
        }

        Ok(Response {
            body,
            headers: self.headers,
            reason_phrase,
            status_code: self.status_code,
            version: self.version,
        })
    }

    /// Appends a header to this response.
    ///
    /// This function will append the provided name/value as a header to the internal [`HeaderMap`]
    /// being constructed. Essentially, this is equivalent to calling [`HeaderMap::append`].
    /// Because of this, you are able to add a given header multiple times.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::response::Response;
    ///
    /// let mut builder = Response::builder();
    /// builder
    ///     .header(HeaderName::CSeq, HeaderValue::try_from("835").unwrap())
    ///     .body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn header(&mut self, name: HeaderName, value: HeaderValue) -> &mut Self {
        self.headers.append(name, value);
        self
    }

    /// Creates a new default instance of a [`Builder`] to construct a [`Response`].
    pub fn new() -> Self {
        Builder {
            body: None,
            custom_reason_phrase: None,
            headers: HeaderMap::default(),
            status_code: StatusCode::default(),
            version: Version::default(),
        }
    }

    /// Set the reason phrase for this response.
    ///
    /// # Errors
    ///
    /// If a extension status code is specified, you *must* specify a reason phrase or an error will
    /// be returned during the [`Builder::build`] function.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::reason::ReasonPhrase;
    /// use rtsp::response::Response;
    ///
    /// let mut builder = Response::builder();
    /// builder
    ///     .reason_phrase(Some(ReasonPhrase::try_from("Good Response").unwrap()))
    ///     .body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn reason_phrase(&mut self, reason_phrase: Option<ReasonPhrase>) -> &mut Self {
        self.custom_reason_phrase = reason_phrase;
        self
    }

    /// Set the status code for this response.
    ///
    /// The default value for the status code is 200.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let mut builder = Response::builder();
    /// builder
    ///     .status_code(StatusCode::MovedPermanently)
    ///     .header(
    ///         HeaderName::Location,
    ///         HeaderValue::try_from("rtsp://example.com/resource").unwrap()
    ///     )
    ///     .body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn status_code(&mut self, status_code: StatusCode) -> &mut Self {
        self.status_code = status_code;
        self
    }

    /// Sets a typed header for this response.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::response::Response;
    ///
    /// let mut builder = Response::builder();
    /// builder
    ///     .typed_header(ContentLength::try_from(5).unwrap())
    ///     .body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn typed_header<TTypedHeader>(&mut self, header: TTypedHeader) -> &mut Self
    where
        TTypedHeader: TypedHeader,
    {
        self.headers.typed_insert(header);
        self
    }

    /// Attempts to set the version for this response.
    ///
    /// # Errors
    ///
    /// An error will be returned if the given version is an invalid [`Version`] or if it is
    /// unsupported.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::version::Version;
    ///
    /// let mut builder = Response::builder();
    /// builder
    ///     .version(Version::RTSP20)
    ///     .body(());
    /// let response = builder.build().unwrap();
    /// ```
    pub fn version(&mut self, version: Version) -> &mut Self {
        self.version = version;
        self
    }

    /// Consumes the builder and sets the body part of the response.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_body<TNewBody>(self, body: TNewBody) -> Builder<TNewBody> {
        Builder {
            body: Some(body),
            custom_reason_phrase: self.custom_reason_phrase,
            headers: self.headers,
            status_code: self.status_code,
            version: self.version,
        }
    }

    /// Consumes the builder and appends a header as part of the response.
    ///
    /// This function will append the provided name/value as a header to the internal [`HeaderMap`]
    /// being constructed. Essentially, this is equivalent to calling [`HeaderMap::append`].
    /// Because of this, you are able to add a given header multiple times.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("0").unwrap())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_header(mut self, name: HeaderName, value: HeaderValue) -> Self {
        self.header(name, value);
        self
    }

    /// Consumes the builder and sets the reason phrase part of the response.
    ///
    /// # Errors
    ///
    /// If a extension status code is specified, you *must* specify a reason phrase or an error will
    /// be returned during the [`Builder::build`] function.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::reason::ReasonPhrase;
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_reason_phrase(Some(ReasonPhrase::try_from("Good Response").unwrap()))
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_reason_phrase(mut self, reason_phrase: Option<ReasonPhrase>) -> Self {
        self.reason_phrase(reason_phrase);
        self
    }

    /// Consumes the builder and sets the status code part of the response.
    ///
    /// The default value for the status code is 200.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::status::StatusCode;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_status_code(StatusCode::OK)
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_status_code(mut self, status_code: StatusCode) -> Self {
        self.status_code(status_code);
        self
    }

    /// Consumes the builder and sets a typed header as part of the response.
    ///
    /// By default, the response contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::response::Response;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_typed_header::<ContentLength>(ContentLength::default())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_typed_header<TTypedHeader>(mut self, header: TTypedHeader) -> Self
    where
        TTypedHeader: TypedHeader,
    {
        self.typed_header(header);
        self
    }

    /// Consumes the builder and sets the version part of the response.
    ///
    /// # Errors
    ///
    /// An error will be returned by [`Builder::build`] if the given version is unsupported.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Response;
    /// use rtsp::version::Version;
    ///
    /// let response = Response::<()>::builder()
    ///     .with_version(Version::RTSP20)
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_version(mut self, version: Version) -> Self {
        self.version(version);
        self
    }
}

impl<TBody> Default for Builder<TBody> {
    fn default() -> Self {
        Builder::new()
    }
}

/// An error type for when the response builder encounters an error.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ResponseError {
    /// The body was not specified.
    MissingBody,

    /// The reason phrase was not specified for an extension status code.
    MissingReasonPhrase,

    /// The version was unsupported. The only supported version is RTSP 2.0.
    UnsupportedVersion,
}

impl Display for ResponseError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ResponseError::*;

        match self {
            MissingBody => write!(formatter, "missing response body"),
            MissingReasonPhrase => write!(formatter, "missing response reason phrase"),
            UnsupportedVersion => write!(formatter, "unsupported response version"),
        }
    }
}

impl Error for ResponseError {}

impl From<Infallible> for ResponseError {
    fn from(_: Infallible) -> Self {
        ResponseError::MissingBody
    }
}
