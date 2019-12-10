//! Request
//!
//! This module contains structs related to RTSP requests, notably the [`Request`] type itself as
//! well as a builder to create requests.

use std::convert::Infallible;
use std::error::Error;
use std::fmt::{self, Display, Formatter};

use crate::header::map::{HeaderMap, HeaderMapExtension, TypedHeader};
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;
use crate::method::Method;
use crate::uri::request::URI;
use crate::version::Version;

/// Represents an RTSP request.
///
/// An RTSP request consists of a method, URI, version, headers and a, potentially empty, body. The
/// body component is generic, enabling arbitrary types to represent the RTSP body.
///
/// This struct implements [`PartialEq`] but care should be taken when using it. Two requests can
/// be semantically equivalent but not be byte by byte. This will mainly occur due to extra spaces
/// in headers. Even when using typed headers, the same problem will occur.
///
/// It is not necessary to ever set the `"Content-Length"` header as it will be forcibly set during
/// encoding even if it is already present.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Request<TBody> {
    /// The body component of the request. This is generic to support arbitrary content types.
    body: TBody,

    /// A mapping of all header names to their values. Headers can be repeated here.
    headers: HeaderMap,

    /// The RTSP method to be applied to the resource. This can be any standardized RTSP method or
    /// an extension method.
    method: Method,

    /// The absolute RTSP request URI (including scheme, host, and port) for the target resource.
    /// IPv6 literals are supported.
    ///
    /// RTSP also supports specifying just `'*'` for the URI in the request line indicating that the
    /// request does not apply to a particular resource but to the server or proxy itself. This is
    /// only allowed when the request method does not necessarily apply to a resource.
    uri: URI,

    /// The protocol version that is being used.
    version: Version,
}

impl<TBody> Request<TBody> {
    /// Returns a shared reference to the request body.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.body(), &"body");
    /// ```
    pub fn body(&self) -> &TBody {
        &self.body
    }

    /// Returns a mutable reference to the request body.
    ///
    /// To change the type of the body, use the [`Request::map`] function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.body_mut(), &mut "body");
    /// ```
    pub fn body_mut(&mut self) -> &mut TBody {
        &mut self.body
    }

    /// Constructs a new builder.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.body(), &"body");
    /// ```
    pub fn builder() -> Builder<TBody> {
        Builder::new()
    }

    /// Constructs a request from its individual parts.
    ///
    /// # Errors
    ///
    /// An error will be returned if the combination of parts creates an invalid request.
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
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    /// use rtsp::version::Version;
    ///
    /// let request = Request::from_parts(
    ///     Method::Setup,
    ///     URI::try_from("rtsp://example.com").unwrap(),
    ///     Version::Rtsp2_0,
    ///     HeaderMap::from_iter(vec![
    ///         (HeaderName::ContentLength, HeaderValue::try_from("0").unwrap())
    ///     ]),
    ///     "",
    /// ).unwrap();
    /// assert_eq!(request.method(), &Method::Setup);
    /// assert_eq!(request.uri(), &URI::try_from("rtsp://example.com").unwrap());
    /// assert_eq!(request.version(), Version::Rtsp2_0);
    /// assert_eq!(
    ///     request.headers().get(&HeaderName::ContentLength),
    ///     Some(&HeaderValue::try_from("0").unwrap())
    /// );
    /// assert_eq!(request.body(), &"");
    /// ```
    pub fn from_parts(
        method: Method,
        uri: URI,
        version: Version,
        headers: HeaderMap,
        body: TBody,
    ) -> Result<Self, RequestError> {
        let mut builder = Request::<()>::builder()
            .with_method(method)
            .with_uri(uri)
            .with_body(body)
            .with_version(version);
        builder.headers = headers;
        builder.build()
    }

    /// Returns a shared reference to the request header map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::header::map::HeaderMapExtension;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_typed_header::<ContentLength>(ContentLength::default())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(
    ///     request.headers().typed_get::<ContentLength>(),
    ///     Some(ContentLength::default())
    /// );
    /// ```
    pub fn headers(&self) -> &HeaderMap {
        &self.headers
    }

    /// Returns a mutable reference to the request header map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::header::map::HeaderMapExtension;
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_typed_header::<ContentLength>(ContentLength::default())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(
    ///     request.headers_mut().typed_get::<ContentLength>(),
    ///     Some(ContentLength::default())
    /// );
    /// ```
    pub fn headers_mut(&mut self) -> &mut HeaderMap {
        &mut self.headers
    }

    /// Converts the request into its corresponding builder.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let builder = request.clone().into_builder();
    /// let new_request = builder.build().unwrap();
    /// assert_eq!(request, new_request);
    /// ```
    pub fn into_builder(self) -> Builder<TBody> {
        Builder {
            body: Some(self.body),
            headers: self.headers,
            method: Some(self.method),
            uri: Some(self.uri),
            version: self.version,
        }
    }

    /// Converts the request into its individual components.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let (method, uri, version, headers, body) = request.clone().into_parts();
    /// let new_request = Request::from_parts(method, uri, version, headers, body).unwrap();
    /// assert_eq!(request, new_request);
    /// ```
    pub fn into_parts(self) -> (Method, URI, Version, HeaderMap, TBody) {
        (self.method, self.uri, self.version, self.headers, self.body)
    }

    /// Maps the body of this request to a new type using the provided function.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// let request = request.map(|_| 0);
    /// assert_eq!(request.body(), &0);
    /// ```
    pub fn map<TNewBody, TMapper>(self, mut mapper: TMapper) -> Request<TNewBody>
    where
        TMapper: FnMut(TBody) -> TNewBody,
    {
        Request {
            body: mapper(self.body),
            headers: self.headers,
            method: self.method,
            uri: self.uri,
            version: self.version,
        }
    }

    /// Returns a shared reference to the request method.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.method(), &Method::Setup);
    /// ```
    pub fn method(&self) -> &Method {
        &self.method
    }

    /// Returns a mutable reference to the request method.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.method(), &mut Method::Setup);
    /// ```
    pub fn method_mut(&mut self) -> &mut Method {
        &mut self.method
    }

    /// Returns a shared reference to the request URI.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.uri(), &URI::asterisk());
    /// ```
    pub fn uri(&self) -> &URI {
        &self.uri
    }

    /// Returns a mutable reference to the request URI.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.uri_mut(), &mut URI::asterisk());
    /// ```
    pub fn uri_mut(&mut self) -> &mut URI {
        &mut self.uri
    }

    /// Returns a copy of the request version.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    /// use rtsp::version::Version;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("body")
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(request.version(), Version::Rtsp2_0);
    /// ```
    pub fn version(&self) -> Version {
        self.version
    }
}

/// An RTSP request builder.
///
/// This type can be used to construct a [`Request`] through a builder pattern.
#[derive(Clone, Debug)]
pub struct Builder<TBody> {
    /// The body component of the request. This is generic to support arbitrary content types.
    pub(crate) body: Option<TBody>,

    /// A mapping of all header names to their values. Headers can be repeated here.
    pub(crate) headers: HeaderMap,

    /// The RTSP method to be applied to the resource. This can be any standardized RTSP method or
    /// an extension method.
    pub(crate) method: Option<Method>,

    /// The absolute RTSP URI (including scheme, host, and port) for the target resource. IPv6
    /// literals are supported.
    ///
    /// RTSP also supports specifying just `'*'` for the URI in the request line indicating that the
    /// request does not apply to a particular resource but to the server or proxy itself. This is
    /// only allowed when the request method does not necessarily apply to a resource.
    pub(crate) uri: Option<URI>,

    /// The protocol version that is being used.
    pub(crate) version: Version,
}

impl<TBody> Builder<TBody> {
    /// Set the body for this request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Setup)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn body(&mut self, body: TBody) -> &mut Self {
        self.body = Some(body);
        self
    }

    /// Constructs a [`Request`] by consuming all fields set on this builder.
    ///
    /// # Errors
    ///
    /// An error will be returned if part of the request is missing.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Setup)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn build(self) -> Result<Request<TBody>, RequestError> {
        let method = self.method.ok_or(RequestError::MissingMethod)?;
        let uri = self.uri.ok_or(RequestError::MissingURI)?;
        let body = self.body.ok_or(RequestError::MissingBody)?;

        if self.version != Version::Rtsp2_0 {
            return Err(RequestError::UnsupportedVersion);
        }

        Ok(Request {
            body,
            headers: self.headers,
            method,
            uri,
            version: self.version,
        })
    }

    /// Appends a header to this request.
    ///
    /// This function will append the provided name/value as a header to the internal [`HeaderMap`]
    /// being constructed. Essentially, this is equivalent to calling [`HeaderMap::append`].
    /// Because of this, you are able to add a given header multiple times.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Play)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .header(HeaderName::CSeq, HeaderValue::try_from("835").unwrap())
    ///     .header(HeaderName::Session, HeaderValue::try_from("ULExwZCXh2pd0xuFgkgZJW").unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn header(&mut self, name: HeaderName, value: HeaderValue) -> &mut Self {
        self.headers.append(name, value);
        self
    }

    /// Set the method for this request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Play)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn method(&mut self, method: Method) -> &mut Self {
        self.method = Some(method);
        self
    }

    /// Creates a new default instance of a [`Builder`] to construct a [`Request`].
    pub fn new() -> Self {
        Builder {
            body: None,
            headers: HeaderMap::default(),
            method: None,
            uri: None,
            version: Version::default(),
        }
    }

    /// Sets a typed header for this request.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Setup)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .typed_header(ContentLength::try_from(5).unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn typed_header<TTypedHeader>(&mut self, header: TTypedHeader) -> &mut Self
    where
        TTypedHeader: TypedHeader,
    {
        self.headers.typed_insert(header);
        self
    }

    /// Set the URI for this request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Setup)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn uri(&mut self, uri: URI) -> &mut Self {
        self.uri = Some(uri);
        self
    }

    /// Attempts to set the version for this request.
    ///
    /// # Errors
    ///
    /// An error will be returned by [`Builder::build`] if the given version is unsupported.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    /// use rtsp::version::Version;
    ///
    /// let mut builder = Request::builder();
    /// builder
    ///     .method(Method::Setup)
    ///     .uri(URI::try_from("rtsp://server.com").unwrap())
    ///     .version(Version::Rtsp2_0)
    ///     .body(());
    /// let request = builder.build().unwrap();
    /// ```
    pub fn version(&mut self, version: Version) -> &mut Self {
        self.version = version;
        self
    }

    /// Consumes the builder and sets the body part of the request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_body<TNewBody>(self, body: TNewBody) -> Builder<TNewBody> {
        Builder {
            body: Some(body),
            headers: self.headers,
            method: self.method,
            uri: self.uri,
            version: self.version,
        }
    }

    /// Consumes the builder and appends a header as part of the request.
    ///
    /// This function will append the provided name/value as a header to the internal [`HeaderMap`]
    /// being constructed. Essentially, this is equivalent to calling [`HeaderMap::append`].
    /// Because of this, you are able to add a given header multiple times.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::name::HeaderName;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_header(HeaderName::ContentLength, HeaderValue::try_from("0").unwrap())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_header(mut self, name: HeaderName, value: HeaderValue) -> Self {
        self.header(name, value);
        self
    }

    /// Consumes the builder and sets the method part of the request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_method(mut self, method: Method) -> Self {
        self.method(method);
        self
    }

    /// Consumes the builder and sets a typed header as part of the request.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::ContentLength;
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
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

    /// Consumes the builder and sets the URI part of the request.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_body("")
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_uri(mut self, uri: URI) -> Self {
        self.uri(uri);
        self
    }

    /// Consumes the builder and sets the version part of the request.
    ///
    /// # Errors
    ///
    /// An error will be returned by [`Builder::build`] if the given version is unsupported.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::method::Method;
    /// use rtsp::request::Request;
    /// use rtsp::uri::request::URI;
    /// use rtsp::version::Version;
    ///
    /// let request = Request::<()>::builder()
    ///     .with_method(Method::Setup)
    ///     .with_uri(URI::asterisk())
    ///     .with_version(Version::Rtsp2_0)
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

/// An error type for when there is an error constructing a request.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum RequestError {
    /// The body was not specified.
    MissingBody,

    /// The method was not specified.
    MissingMethod,

    /// The URI was not specified.
    MissingURI,

    /// The version was unsupported. The only supported version is RTSP 2.0.
    UnsupportedVersion,
}

impl Display for RequestError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::RequestError::*;

        match self {
            MissingBody => write!(formatter, "missing request body"),
            MissingMethod => write!(formatter, "missing request method"),
            MissingURI => write!(formatter, "missing request URI"),
            UnsupportedVersion => write!(formatter, "unsupported request version"),
        }
    }
}

impl Error for RequestError {}

impl From<Infallible> for RequestError {
    fn from(_: Infallible) -> Self {
        RequestError::MissingBody
    }
}
