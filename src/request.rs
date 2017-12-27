//! RTSP Request Types
//!
//! This module contains structs related to RTSP requests, notably the `Request` type itself as well
//! as a builder to create requests. Typically, you will import the `rtsp::Request` type rather than
//! reaching into this module itself.

use std::convert::TryFrom;
use std::{error, fmt};
use std::mem::replace;

use header::{HeaderMap, HeaderName, HeaderValue, TypedHeader, TypedHeaderMap};
use method::Method;
use uri::URI;
use version::Version;

/// Represents an RTSP request.
///
/// An RTSP request consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
#[derive(Clone, Eq, PartialEq)]
pub struct Request<B, H = HeaderMap<HeaderValue>>
where
    H: Default,
{
    /// The body component of the request. This is generic to support arbitrary content types.
    body: B,

    /// A header map that will either be `HeaderMap<HeaderValue>` or `TypedHeaderMap`.
    headers: H,

    /// The RTSP method to be applied to the resource. This can be any standardized RTSP method or
    /// an extension method.
    method: Method,

    /// The absolute RTSP URI (including scheme, host, and port) for the target resource. IPv6
    /// literals are supported.
    ///
    /// RTSP also supports specifying just `*` for the URI in the request line indicating that the
    /// request does not apply to a particular resource but to the server or proxy itself. This is
    /// only allowed when the request method does not necessarily apply to a resource.
    uri: URI,

    /// The protocol version that is being used.
    version: Version,
}

impl Request<()> {
    pub fn builder() -> Builder {
        Builder::new()
    }

    pub fn typed_builder() -> Builder<TypedHeaderMap> {
        Builder::new()
    }

    pub fn describe<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Describe).uri(uri);
        b
    }

    pub fn get_parameter<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::GetParameter).uri(uri);
        b
    }

    pub fn options<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Options).uri(uri);
        b
    }

    pub fn pause<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Pause).uri(uri);
        b
    }

    pub fn play<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Play).uri(uri);
        b
    }

    pub fn play_notify<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::PlayNotify).uri(uri);
        b
    }

    pub fn redirect<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Redirect).uri(uri);
        b
    }

    pub fn set_parameter<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::SetParameter).uri(uri);
        b
    }

    pub fn setup<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Setup).uri(uri);
        b
    }

    pub fn teardown<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        let mut b = Builder::new();
        b.method(Method::Teardown).uri(uri);
        b
    }
}

impl<B, H> Request<B, H>
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

    pub fn map<T, F>(self, mut f: F) -> Request<T, H>
    where
        F: FnMut(B) -> T,
    {
        let body = f(self.body);

        Request {
            body: body,
            headers: self.headers,
            method: self.method,
            uri: self.uri,
            version: self.version,
        }
    }

    pub fn method(&self) -> &Method {
        &self.method
    }

    pub fn method_mut(&mut self) -> &mut Method {
        &mut self.method
    }

    pub fn uri(&self) -> &URI {
        &self.uri
    }

    pub fn uri_mut(&mut self) -> &mut URI {
        &mut self.uri
    }

    pub fn version(&self) -> Version {
        self.version
    }

    pub fn version_mut(&mut self) -> &mut Version {
        &mut self.version
    }
}

impl<B> Request<B, HeaderMap<HeaderValue>> {
    pub fn into_typed(self) -> Request<B, TypedHeaderMap> {
        Request {
            body: self.body,
            headers: self.headers.into(),
            method: self.method,
            uri: self.uri,
            version: self.version,
        }
    }
}

impl<B> Request<B, TypedHeaderMap> {
    pub fn into_untyped(self) -> Request<B, HeaderMap<HeaderValue>> {
        Request {
            body: self.body,
            headers: self.headers.into(),
            method: self.method,
            uri: self.uri,
            version: self.version,
        }
    }
}

impl<B: fmt::Debug> fmt::Debug for Request<B> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Request")
            .field("method", self.method())
            .field("uri", self.uri())
            .field("version", &self.version())
            .field("headers", self.headers())
            .field("body", self.body())
            .finish()
    }
}

/// An RTSP request builder
///
/// This type can be used to construct a `Request` through a builder-like pattern.
#[derive(Clone, Debug)]
pub struct Builder<H = HeaderMap<HeaderValue>>
where
    H: Default,
{
    /// A stored error used when making a `Request`.
    pub(crate) error: Option<BuilderError>,

    /// A header map that will either be `HeaderMap<HeaderValue>` or `TypedHeaderMap`.
    pub(crate) headers: H,

    /// The RTSP method to be applied to the resource. This can be any standardized RTSP method or
    /// an extension method.
    pub(crate) method: Option<Method>,

    /// The absolute RTSP URI (including scheme, host, and port) for the target resource. IPv6
    /// literals are supported.
    ///
    /// RTSP also supports specifying just `*` for the URI in the request line indicating that the
    /// request does not apply to a particular resource but to the server or proxy itself. This is
    /// only allowed when the request method does not necessarily apply to a resource.
    pub(crate) uri: Option<URI>,

    /// The protocol version that is being used.
    pub(crate) version: Version,
}


impl<H> Builder<H>
where
    H: Default,
{
    /// Creates a new default instance of a `Builder` to construct a `Request`.
    pub fn new() -> Builder<H> {
        Builder::default()
    }

    /// Consumes this builder, using the provided `body` to return a constructed `Request`.
    ///
    /// # Errors
    ///
    /// This function may return an error if part of the request has not been configured (such as
    /// the method or URI) or a configured part is invalid (such as a header).
    pub fn build<B>(&mut self, body: B) -> Result<Request<B, H>, BuilderError> {
        if let Some(error) = self.error {
            return Err(error);
        }

        if let Some(method) = replace(&mut self.method, None) {
            if let Some(uri) = replace(&mut self.uri, None) {
                Ok(Request {
                    body,
                    headers: replace(&mut self.headers, H::default()),
                    method,
                    uri,
                    version: self.version,
                })
            } else {
                Err(BuilderError::MissingURI)
            }
        } else {
            Err(BuilderError::MissingMethod)
        }
    }

    /// Set the RTSP method for this request.
    ///
    /// This function will configure the RTSP method of the `Request` that will be returned from
    /// `build`.
    ///
    /// This does not have a default value and, as a result, it must be specified before `build` is
    /// called.
    ///
    /// # Errors
    ///
    /// If the given method is not a valid method, an error will be saved and retured when the
    /// `build` function is invoked.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let request = Request::builder()
    ///     .method(Method::Setup)
    ///     .uri("rtsp://server.com")
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn method<T>(&mut self, method: T) -> &mut Self
    where
        Method: TryFrom<T>,
    {
        match Method::try_from(method) {
            Ok(method) => self.method = Some(method),
            Err(_) => self.error = Some(BuilderError::InvalidMethod),
        }

        self
    }

    /// Set the RTSP URI for this request.
    ///
    /// This function will configure the RTSP URI of the `Request` that will be returned from
    /// `build`.
    ///
    /// This does not have a default value and, as a result, it must be specified before `build` is
    /// called.
    ///
    /// # Errors
    ///
    /// A given URI must have an authority part specified or an error will be saved and returned
    /// when the `build` function is invoked.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let request = Request::builder()
    ///     .method(Method::Setup)
    ///     .uri("rtsp://server.com")
    ///     .build(())
    ///     .unwrap();
    /// ```
    pub fn uri<T>(&mut self, uri: T) -> &mut Self
    where
        URI: TryFrom<T>,
    {
        match URI::try_from(uri) {
            Ok(uri) => self.uri = Some(uri),
            Err(_) => self.error = Some(BuilderError::InvalidURI),
        }

        self
    }

    /// Set the RTSP version for this request.
    ///
    /// This function will configure the RTSP version of the `Request` that will be returned from
    /// `build`.
    ///
    /// The default value for the version is RTSP/2.0.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let request = Request::builder()
    ///     .method(Method::Setup)
    ///     .uri("rtsp://server.com")
    ///     .version(Version::RTSP20)
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
    /// Appends a header to this request.
    ///
    /// This function will append the provided key/value as a header to the internal `HeaderMap`
    /// being constructued. Essentially, this is equivalent to calling `HeaderMap::append`. Because
    /// of this, you are able to add a given header multiple times.
    ///
    /// By default, the request contains no headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    ///
    /// let request = Request::builder()
    ///     .method(Method::Play)
    ///     .uri("rtsp://server.com")
    ///     .header("CSeq", "835")
    ///     .header("Session", "ULExwZCXh2pd0xuFgkgZJW")
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
            error: self.error,
            headers: self.headers.into(),
            method: self.method,
            uri: self.uri,
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
    /// let request = Request::typed_builder()
    ///     .method(Method::Play)
    ///     .uri("rtsp://server.com")
    ///     .header(ContentLength(5))
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
    /// let request = Request::typed_builder()
    ///     .method(Method::Play)
    ///     .uri("rtsp://server.com")
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
            error: self.error,
            headers: self.headers.into(),
            method: self.method,
            uri: self.uri,
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
            error: None,
            headers: H::default(),
            method: None,
            uri: None,
            version: Version::default(),
        }
    }
}

/// An error type for when the request builder encounters an error.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum BuilderError {
    InvalidHeaderName,
    InvalidHeaderValue,
    InvalidMethod,
    InvalidURI,
    InvalidVersion,
    MissingMethod,
    MissingURI,
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
            &InvalidMethod => "invalid RTSP method",
            &InvalidURI => "invalid RTSP URI",
            &InvalidVersion => "invalid RTSP version",
            &MissingMethod => "missing RTSP method",
            &MissingURI => "missing RTSP URI",
        }
    }
}
