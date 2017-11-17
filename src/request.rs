//! RTSP Request Types
//!
//! This module contains structs related to RTSP requests, notably the `Request` type itself as well
//! as a builder to create requests. Typically, you will import the `rtsp::Request` type rather than
//! reaching into this module itself.

use std::convert::TryFrom;
use std::fmt;

use header::{HeaderMap, HeaderName, HeaderValue};
use method::Method;
use uri::URI;
use version::Version;

/// Represents an RTSP request.
///
/// An RTSP request consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
#[derive(Clone, Eq, PartialEq)]
pub struct Request<T> {
    /// The body component of the request. This is generic to support arbitrary content types.
    body: T,

    /// A multimap of header names to values that maintains insertion order.
    headers: HeaderMap<HeaderValue>,

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

    pub fn describe<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Describe).uri(uri)
    }

    pub fn get_parameter<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::GetParameter).uri(uri)
    }

    pub fn options<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Options).uri(uri)
    }

    pub fn pause<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Pause).uri(uri)
    }

    pub fn play<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Play).uri(uri)
    }

    pub fn play_notify<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::PlayNotify).uri(uri)
    }

    pub fn redirect<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Redirect).uri(uri)
    }

    pub fn set_parameter<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::SetParameter).uri(uri)
    }

    pub fn setup<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Setup).uri(uri)
    }

    pub fn teardown<T>(uri: T) -> Builder
    where
        URI: TryFrom<T>,
    {
        Builder::new().method(Method::Teardown).uri(uri)
    }
}

impl<T> Request<T> {
    pub fn body(&self) -> &T {
        &self.body
    }

    pub fn headers(&self) -> &HeaderMap<HeaderValue> {
        &self.headers
    }

    pub fn method(&self) -> &Method {
        &self.method
    }

    pub fn uri(&self) -> &URI {
        &self.uri
    }

    pub fn version(&self) -> Version {
        self.version
    }
}

impl<T: fmt::Debug> fmt::Debug for Request<T> {
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
pub struct Builder {
    /// A stored error used when making a `Request`.
    pub(crate) error: Option<BuilderError>,

    /// A multimap of header names to values that maintains insertion order.
    pub(crate) headers: HeaderMap<HeaderValue>,

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

impl Builder {
    /// Creates a new default instance of a `Builder` to construct a `Request`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::request::Builder;
    ///
    /// let request = Builder::new();
    /// ```
    pub fn new() -> Self {
        Builder::default()
    }

    /// Consumes this builder, using the provided `body` to return a constructed `Request`.
    ///
    /// # Errors
    ///
    /// This function may return an error if part of the request has not been configured (such as
    /// the method or URI) or a configured part is invalid (such as a header).
    pub fn build<T>(self, body: T) -> Result<Request<T>, BuilderError> {
        if let Some(error) = self.error {
            return Err(error);
        }

        if let Some(method) = self.method {
            if let Some(uri) = self.uri {
                Ok(Request {
                    body,
                    headers: self.headers,
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
    pub fn header<K, V>(mut self, key: K, value: V) -> Self
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
    pub fn method<T>(mut self, method: T) -> Self
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
    pub fn uri<T>(mut self, uri: T) -> Self
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
    pub fn version<T>(mut self, version: T) -> Self
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


impl Default for Builder {
    #[inline]
    fn default() -> Self {
        Builder {
            error: None,
            headers: HeaderMap::new(),
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
