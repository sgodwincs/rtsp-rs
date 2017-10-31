//! RTSP Response Types
//!
//! This module contains structs related to RTSP responses, notably the `Response` type itself as
//! well as a builder to create responses. Typically, you will import the `rtsp::Response` type
//! rather than reaching into this module itself.

use std::convert::TryFrom;

use header::{HeaderMap, HeaderName, HeaderValue};
use status::StatusCode;
use version::Version;

/// Represents an RTSP response.
///
/// An RTSP response consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
#[derive(Clone, Eq, PartialEq)]
pub struct Response<T> {
    /// Specifies a custom reason phrase for the given status code. RTSP allows agents to give
    /// custom reason phrases and even recommends it in specific cases. If it is detected that the
    /// status code is an extension or that the reason phrase is not the canonical reason phrase for
    /// the given status code, then this will be the custom reason phrase.
    custom_reason_phrase: Option<String>,

    /// The body component of the response. This is generic to support arbitrary content types.
    body: T,

    /// A multimap of header names to values that maintains insertion order.
    headers: HeaderMap<HeaderValue>,

    /// The status code of the response.
    status_code: StatusCode,

    /// The protocol version that is being used.
    version: Version,
}

impl Response<()> {
    pub fn builder() -> Builder {
        Builder::new()
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
    custom_reason_phrase: Option<String>,

    /// A stored error used when making a `Response`.
    error: Option<BuilderError>,

    /// A multimap of header names to values that maintains insertion order.
    headers: HeaderMap<HeaderValue>,

    /// The status code of the response.
    status_code: StatusCode,

    /// The protocol version that is being used.
    version: Version,
}

impl Builder {
    /// Creates a new default instance of a `Builder` to construct a `Response`.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::response::Builder;
    ///
    /// let response = Builder::new();
    /// ```
    pub fn new() -> Builder {
        Builder::default()
    }

    /// Consumes this builder, using the provided `body` to return a constructed `Response`.
    ///
    /// # Errors
    ///
    /// This function may return an error if part of the response is invalid (such as a header).
    pub fn build<T>(self, body: T) -> Result<Response<T>, BuilderError> {
        if let Some(error) = self.error {
            return Err(error);
        }

        if let StatusCode::Extension(_) = self.status_code {
            if self.custom_reason_phrase.is_none() {
                return Err(BuilderError::MissingReasonPhrase);
            }
        }

        Ok(Response {
            body,
            custom_reason_phrase: self.custom_reason_phrase,
            headers: self.headers,
            status_code: self.status_code,
            version: self.version,
        })
    }

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
    pub fn header<K, V>(mut self, key: K, value: V) -> Builder
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
    pub fn status_code<T>(mut self, status_code: T) -> Builder
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
    pub fn version(mut self, version: Version) -> Builder {
        self.version = version;
        self
    }
}

impl Default for Builder {
    #[inline]
    fn default() -> Builder {
        Builder {
            custom_reason_phrase: None,
            error: None,
            headers: HeaderMap::new(),
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
    InvalidStatusCode,
    MissingReasonPhrase,
}
