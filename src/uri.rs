//! Request URI
//!
//! This module contains a definition of the [`RequestURI`] type.
//!
//! See [[RFC7826, Section 4.2]](https://tools.ietf.org/html/rfc7826#section-4.2) for a description
//! of how RTSP handles URIs (and request URIs) in general.
//!
//! For a brief overview, request URIs must be absolute with the exception that the URI reference
//! `"*"` is allowed. An asterisk `"*"` indicates that the request does not apply to a particular
//! resource but to the server or proxy itself and is only alowed when the request method does not
//! necessarily apply to a resource.
//!
//! An absolute URI in this case has the constraints that both a scheme and a host are
//! specified, e.g., `"rtsp://example.com"`. See [[RFC3986](https://tools.ietf.org/html/rfc3986)]
//! for a complete description of URIs.
//!
//! The specification states that three schemes have been registered in relation to RTSP:
//!  - RTSP - indicates transport via a reliable protocol (i.e. TCP)
//!  - RTSPS - indicates transport using a secure transport (i.e. TLS)
//!  - RTSPU - indicates unspecified transport of RTSP messages over unreliable transport (not
//!            supported by RTSP 2.0 currently)
//!
//! # Examples
//!
//! ```
//! # #![feature(try_from)]
//! #
//! use std::convert::TryFrom;
//!
//! use rtsp::RequestURI;
//!
//! let uri = RequestURI::try_from("rtsp://127.0.0.1").unwrap();
//! assert_eq!(uri.to_string(), "rtsp://127.0.0.1/");
//! ```

use std::convert::TryFrom;
use std::error::Error;
use std::fmt::{self, Display, Formatter, Write};
use std::mem;
use uriparse::{
    Fragment, InvalidAuthority as InvalidURIAuthority, InvalidHost as InvalidURIHost, InvalidQuery,
    InvalidScheme as InvalidURIScheme, InvalidURIReference, Scheme as URIScheme, URIReference, URI,
};

// TODO(https://github.com/rust-lang/rust/issues/49683): Need to wait for type alias enums before we
// export all `uriparse` types with `'static` lifetimes. If we do it now, the user will no longer be
// able to use variants of enum types (e.g. `Host<'static>::RegisteredName` is an error currently).
pub use uriparse::{
    Authority, Host, InvalidPath, InvalidPort, InvalidUserInfo, Password, Path, Query,
    RegisteredName, SchemeStatus, Segment, UnregisteredScheme, Username,
};

lazy_static! {
    static ref ASTERISK_PATH: Path<'static> = Path::try_from("*").unwrap();
    static ref SENTINEL_AUTHORITY: Authority<'static> = Authority::try_from("").unwrap();
    static ref SENTINEL_PATH: Path<'static> = Path::try_from("").unwrap();
}

/// The URI used in the RTSP request line.
///
/// See [[RFC7826, Section 4.2]](https://tools.ietf.org/html/rfc7826#section-4.2) for a description
/// of how RTSP handles URIs (and request URIs) in general.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RequestURI {
    /// A wrapper around the URI components. This will be `None` if the request URI is an asterisk.
    components: Option<RequestURIComponents>,
}

impl RequestURI {
    /// Creates a new request URI with string representation `"*"`.
    ///
    /// An asterisk `"*"` indicates that the request does not apply to a particular resource but to
    /// the server or proxy itself and is only alowed when the request method does not necessarily
    /// apply to a resource.
    ///
    /// An asterisk request URI is essentially immutable, all functions on this type that would
    /// mutate request URIs are no-ops (e.g. [`RequestURI::set_scheme`]).
    pub fn asterisk() -> RequestURI {
        RequestURI { components: None }
    }

    /// Returns the authority of the request URI.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com/my/path").unwrap();
    /// assert_eq!(uri.authority().unwrap().to_string(), "example.com");
    /// ```
    pub fn authority(&self) -> Option<&Authority<'static>> {
        self.components
            .as_ref()
            .map(|components| &components.authority)
    }

    /// Constructs a default builder for a request URI.
    ///
    /// This provides an alternative means of constructing a URI besides parsing and
    /// [`RequestURI::from_parts`].
    ///
    /// This function cannot be used to create an asterisk request URI, use [`RequestURI::asterisk`]
    /// for that.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::RequestURI;
    ///
    /// let mut builder = RequestURI::builder();
    /// builder.scheme("rtsp").authority("example.com").path("/my/path");
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/my/path");
    /// ```
    pub fn builder<'uri>() -> RequestURIBuilder<'uri> {
        RequestURIBuilder::new()
    }

    /// Constructs a new [`RequestURI`] from the individual parts: scheme, authority, path, and
    /// query.
    ///
    /// This function cannot be used to create an asterisk request URI, use [`RequestURI::asterisk`]
    /// for that.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    /// use rtsp::uri::Query;
    ///
    /// let uri = RequestURI::from_parts(
    ///     "rtsp",
    ///     "example.com",
    ///     "",
    ///     None::<Query>
    /// ).unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/");
    /// ```
    pub fn from_parts<
        'new_uri,
        SchemeType,
        AuthorityType,
        PathType,
        QueryType,
        SchemeError,
        AuthorityError,
        PathError,
        QueryError,
    >(
        scheme: SchemeType,
        authority: AuthorityType,
        path: PathType,
        query: Option<QueryType>,
    ) -> Result<RequestURI, InvalidRequestURI>
    where
        Scheme: TryFrom<SchemeType, Error = SchemeError>,
        Authority<'new_uri>: TryFrom<AuthorityType, Error = AuthorityError>,
        Path<'new_uri>: TryFrom<PathType, Error = PathError>,
        Query<'new_uri>: TryFrom<QueryType, Error = QueryError>,
        InvalidAuthority: From<AuthorityError>,
        InvalidPath: From<PathError>,
        InvalidQuery: From<QueryError>,
        InvalidScheme: From<SchemeError>,
    {
        let mut builder = RequestURIBuilder::new();
        builder
            .scheme(scheme)
            .authority(authority)
            .path(path)
            .query(query);
        builder.build()
    }

    /// Returns whether the request URI has an authority component.
    ///
    /// This will be false only for asterisk request URIs.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.has_authority());
    ///
    /// let uri = RequestURI::try_from("*").unwrap();
    /// assert!(!uri.has_authority());
    /// ```
    pub fn has_authority(&self) -> bool {
        self.components.is_some()
    }

    /// Returns whether the request URI has a host component.
    ///
    /// This will be false only for asterisk request URIs.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.has_host());
    ///
    /// let uri = RequestURI::try_from("*").unwrap();
    /// assert!(!uri.has_host());
    /// ```
    pub fn has_host(&self) -> bool {
        self.components.is_some()
    }

    /// Returns whether the request URI has a password component.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://user:pass@127.0.0.1").unwrap();
    /// assert!(uri.has_password());
    ///
    /// let uri = RequestURI::try_from("rtsp://user@127.0.0.1").unwrap();
    /// assert!(!uri.has_password());
    /// ```
    pub fn has_password(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| components.authority.has_password())
            .unwrap_or(false)
    }

    /// Returns whether the URI has a port.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://127.0.0.1:554").unwrap();
    /// assert!(uri.has_port());
    ///
    /// let uri = RequestURI::try_from("rtsp://127.0.0.1").unwrap();
    /// assert!(!uri.has_port());
    /// ```
    pub fn has_port(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| components.authority.has_port())
            .unwrap_or(false)
    }

    /// Returns whether the request URI has a query component.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com/my/path?my=query").unwrap();
    /// assert!(uri.has_query());
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com/my/path").unwrap();
    /// assert!(!uri.has_query());
    /// ```
    pub fn has_query(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| components.query.is_some())
            .unwrap_or(false)
    }

    /// Returns whether the request URI has a username component.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://username@example.com").unwrap();
    /// assert!(uri.has_username());
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// assert!(!uri.has_username());
    /// ```
    pub fn has_username(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| components.authority.has_username())
            .unwrap_or(false)
    }

    /// Returns the host of the URI.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://username@example.com").unwrap();
    /// assert_eq!(uri.host().unwrap().to_string(), "example.com");
    /// ```
    pub fn host(&self) -> Option<&Host> {
        self.components
            .as_ref()
            .map(|components| components.authority.host())
    }

    /// Consumes the request URI and converts it into a builder with the same values.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    /// use rtsp::uri::Query;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com/path?query").unwrap();
    /// let mut builder = uri.into_builder().unwrap();
    /// builder.query(None::<Query>);
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path");
    /// ```
    pub fn into_builder(self) -> Option<RequestURIBuilder<'static>> {
        if self.is_asterisk() {
            return None;
        }

        let (scheme, authority, path, query) = self.into_parts().unwrap();
        let mut builder = RequestURIBuilder::new();
        builder
            .scheme(scheme)
            .authority(authority)
            .path(path)
            .query(query);
        Some(builder)
    }

    /// Consumes the [`RequestURI`] and returns its parts: scheme, authority, path, and query.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from(
    ///     "rtsp://username:password@example.com:554/my/path?my=query",
    /// ).unwrap();
    /// let (scheme, authority, path, query) = uri.into_parts().unwrap();
    ///
    /// assert_eq!(scheme, "rtsp");
    /// assert_eq!(authority.to_string(), "username:password@example.com:554");
    /// assert_eq!(path, "/my/path");
    /// assert_eq!(query.unwrap(), "my=query");
    /// ```
    pub fn into_parts(
        self,
    ) -> Option<(
        Scheme,
        Authority<'static>,
        Path<'static>,
        Option<Query<'static>>,
    )> {
        self.components.map(|components| {
            (
                components.scheme,
                components.authority,
                components.path,
                components.query,
            )
        })
    }

    /// Returns whether the request URI is an asterisk URI (i.e. its string representation is
    /// `"*"`).
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("*").unwrap();
    /// assert!(uri.is_asterisk());
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// assert!(!uri.is_asterisk());
    /// ```
    pub fn is_asterisk(&self) -> bool {
        self.components.is_none()
    }

    /// Returns whether the URI is normalized.
    ///
    /// A normalized URI will have all of its components normalized.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com/?a=b").unwrap();
    /// assert!(uri.is_normalized());
    ///
    /// let mut uri = RequestURI::try_from("rtsp://EXAMPLE.com/?a=b").unwrap();
    /// assert!(!uri.is_normalized());
    /// uri.normalize();
    /// assert!(uri.is_normalized());
    /// ```
    pub fn is_normalized(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| {
                if let Some(port) = components.authority.port() {
                    if port == components.scheme.default_port() {
                        return false;
                    }
                }

                if let Some(query) = components.query.as_ref() {
                    if !query.is_normalized() {
                        return false;
                    }
                }

                components.authority.is_normalized() && components.path.is_normalized(false)
            })
            .unwrap_or(true)
    }

    /// Maps the authority using the given map function.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    /// use rtsp::uri::Authority;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.map_authority(|_| Authority::try_from("127.0.0.1").unwrap());
    /// assert_eq!(uri.to_string(), "rtsp://127.0.0.1/");
    /// ```
    pub fn map_authority<Mapper>(&mut self, mapper: Mapper) -> Option<&Authority<'static>>
    where
        Mapper: FnOnce(Authority<'static>) -> Authority<'static>,
    {
        match self.components.as_mut() {
            Some(components) => {
                let authority = mapper(mem::replace(
                    &mut components.authority,
                    SENTINEL_AUTHORITY.clone(),
                ));
                self.set_authority(authority)
                    .expect("mapped authority resulted in invalid state")
            }
            None => self.authority(),
        }
    }

    /// Maps the path using the given map function.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.map_path(|mut path| {
    ///     path.push("test").unwrap();
    ///     path.push("path").unwrap();
    ///     path
    /// });
    /// assert_eq!(uri.to_string(), "rtsp://example.com/test/path");
    /// ```
    pub fn map_path<Mapper>(&mut self, mapper: Mapper) -> &Path<'static>
    where
        Mapper: FnOnce(Path<'static>) -> Path<'static>,
    {
        match self.components.as_mut() {
            Some(components) => {
                let path = mapper(mem::replace(&mut components.path, SENTINEL_PATH.clone()));
                self.set_path(path)
                    .expect("mapped path resulted in invalid state")
            }
            None => self.path(),
        }
    }

    /// Maps the query using the given map function.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    /// use rtsp::uri::Query;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.map_query(|_| Some(Query::try_from("query").unwrap()));
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?query");
    /// ```
    pub fn map_query<Mapper>(&mut self, mapper: Mapper) -> Option<&Query<'static>>
    where
        Mapper: FnOnce(Option<Query<'static>>) -> Option<Query<'static>>,
    {
        match self.components.as_mut() {
            Some(components) => {
                let query = mapper(components.query.take());
                self.set_query(query)
                    .expect("mapped query resulted in invalid state")
            }
            None => self.query(),
        }
    }

    /// Maps the scheme using the given map function.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    /// use rtsp::uri::Scheme;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.map_scheme(|_| Scheme::try_from("rtsps").unwrap());
    /// assert_eq!(uri.to_string(), "rtsps://example.com/");
    /// ```
    pub fn map_scheme<Mapper>(&mut self, mapper: Mapper) -> Option<Scheme>
    where
        Mapper: FnOnce(Scheme) -> Scheme,
    {
        match self.components.as_ref() {
            Some(components) => {
                let scheme = mapper(components.scheme);
                self.set_scheme(scheme)
                    .expect("mapped scheme resulted in invalid state")
            }
            None => self.scheme(),
        }
    }

    /// Normalizes the request URI.
    ///
    /// A normalized request URI will have all of its components normalized.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com/?a=b").unwrap();
    /// uri.normalize();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?a=b");
    ///
    /// let mut uri = RequestURI::try_from("rtsp://EXAMPLE.com/?a=b").unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://EXAMPLE.com/?a=b");
    /// uri.normalize();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?a=b");
    /// ```
    pub fn normalize(&mut self) {
        if let Some(components) = self.components.as_mut() {
            components.authority.normalize();
            components.path.normalize(false);

            if let Some(query) = components.query.as_mut() {
                query.normalize();
            }

            if let Some(port) = components.authority.port() {
                if port == components.scheme.default_port() {
                    components.authority.set_port(None);
                }
            }
        }
    }

    /// Returns the path of the request URI.
    ///
    /// If the request URI is an asterisk, this will return the path `"*"`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://127.0.0.1/my/path").unwrap();
    /// assert_eq!(uri.path(), "/my/path");
    /// ```
    pub fn path(&self) -> &Path<'static> {
        match self.components.as_ref() {
            Some(components) => &components.path,
            None => &ASTERISK_PATH,
        }
    }

    /// Returns the password of the request URI.
    ///
    /// Usage of a password in URIs is deprecated.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://user:pass@example.com").unwrap();
    /// assert_eq!(uri.password().unwrap(), "pass");
    /// ```
    pub fn password(&self) -> Option<&Password<'static>> {
        self.components
            .as_ref()
            .and_then(|components| components.authority.password())
    }

    /// Returns the port of the request URI.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com:8080/").unwrap();
    /// assert_eq!(uri.port().unwrap(), 8080);
    /// ```
    pub fn port(&self) -> Option<u16> {
        self.components
            .as_ref()
            .and_then(|components| components.authority.port())
    }

    /// Returns the query of the request URI.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://127.0.0.1?my=query").unwrap();
    /// assert_eq!(uri.query().unwrap(), "my=query");
    /// ```
    pub fn query(&self) -> Option<&Query<'static>> {
        self.components
            .as_ref()
            .and_then(|components| components.query.as_ref())
    }

    /// Returns the scheme of the request URI.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://127.0.0.1/").unwrap();
    /// assert_eq!(uri.scheme().unwrap(), "rtsp");
    /// ```
    pub fn scheme(&self) -> Option<Scheme> {
        self.components.as_ref().map(|components| components.scheme)
    }

    /// Sets the authority of the request URI.
    ///
    /// An error will be returned if the conversion to an [`Authority`] fails.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.set_authority("user@example.com:80");
    /// assert_eq!(uri.to_string(), "rtsp://user@example.com:80/");
    /// ```
    pub fn set_authority<AuthorityType, AuthorityError>(
        &mut self,
        authority: AuthorityType,
    ) -> Result<Option<&Authority<'static>>, InvalidRequestURI>
    where
        Authority<'static>: TryFrom<AuthorityType, Error = AuthorityError>,
        InvalidRequestURI: From<AuthorityError>,
    {
        if let Some(components) = self.components.as_mut() {
            components.authority = Authority::try_from(authority)?;
        }

        Ok(self.authority())
    }

    /// Sets the path of the request URI.
    ///
    /// An error will be returned if the conversion to a [`Path`] fails.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.set_path("my/path");
    /// assert_eq!(uri.to_string(), "rtsp://example.com/my/path");
    /// ```
    pub fn set_path<PathType, PathError>(
        &mut self,
        path: PathType,
    ) -> Result<&Path<'static>, InvalidRequestURI>
    where
        Path<'static>: TryFrom<PathType, Error = PathError>,
        InvalidRequestURI: From<PathError>,
    {
        if let Some(components) = self.components.as_mut() {
            let mut path = Path::try_from(path)?;
            path.set_absolute(true);
            components.path = path;
        }

        Ok(self.path())
    }

    /// Sets the query of the request URI.
    ///
    /// An error will be returned if the conversion to a [`Query`] fails.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.set_query(Some("myquery"));
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?myquery");
    /// ```
    pub fn set_query<QueryType, QueryError>(
        &mut self,
        query: Option<QueryType>,
    ) -> Result<Option<&Query<'static>>, InvalidRequestURI>
    where
        Query<'static>: TryFrom<QueryType, Error = QueryError>,
        InvalidRequestURI: From<QueryError>,
    {
        if let Some(components) = self.components.as_mut() {
            components.query = match query {
                Some(query) => Some(Query::try_from(query)?),
                None => None,
            };
        }

        Ok(self.query())
    }

    /// Sets the scheme of the request URI.
    ///
    /// An error will be returned if the conversion to a [`Scheme`] fails.
    ///
    /// If the request URI is an asterisk, this is a no-op.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let mut uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// uri.set_scheme("rtsps");
    /// assert_eq!(uri.to_string(), "rtsps://example.com/");
    /// ```
    pub fn set_scheme<SchemeType, SchemeError>(
        &mut self,
        scheme: SchemeType,
    ) -> Result<Option<Scheme>, InvalidRequestURI>
    where
        Scheme: TryFrom<SchemeType, Error = SchemeError>,
        InvalidRequestURI: From<SchemeError>,
    {
        if let Some(components) = self.components.as_mut() {
            components.scheme = Scheme::try_from(scheme)?;
        }

        Ok(self.scheme())
    }

    /// Returns the username of the request URI.
    ///
    /// If the request URI is an asterisk, this will return `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURI;
    ///
    /// let uri = RequestURI::try_from("rtsp://username@example.com").unwrap();
    /// assert_eq!(uri.username().unwrap(), "username");
    /// ```
    pub fn username(&self) -> Option<&Username<'static>> {
        self.components
            .as_ref()
            .and_then(|components| components.authority.username())
    }
}

impl Display for RequestURI {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        match self.components.as_ref() {
            Some(components) => {
                formatter.write_str(components.scheme.as_str())?;
                formatter.write_str("://")?;
                formatter.write_str(&components.authority.to_string())?;
                formatter.write_str(&components.path.to_string())?;

                if let Some(ref query) = components.query {
                    formatter.write_char('?')?;
                    formatter.write_str(query.as_str())?;
                }

                Ok(())
            }
            None => formatter.write_char('*'),
        }
    }
}

impl From<RequestURI> for String {
    fn from(value: RequestURI) -> Self {
        value.to_string()
    }
}

impl<'uri> From<RequestURI> for URIReference<'uri> {
    fn from(value: RequestURI) -> Self {
        match value.components {
            Some(components) => URIReference::from_parts(
                Some(components.scheme),
                Some(components.authority),
                components.path,
                components.query,
                None::<Fragment>,
            ),
            None => URIReference::from_parts(
                None::<URIScheme>,
                None::<Authority>,
                ASTERISK_PATH.clone(),
                None::<Query>,
                None::<Fragment>,
            ),
        }
        .unwrap()
    }
}

impl<'uri> TryFrom<&'uri [u8]> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: &'uri [u8]) -> Result<Self, Self::Error> {
        let uri_reference = URIReference::try_from(value)
            .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        RequestURI::try_from(uri_reference)
    }
}

impl<'uri> TryFrom<&'uri str> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: &'uri str) -> Result<Self, Self::Error> {
        RequestURI::try_from(value.as_bytes())
    }
}

impl<'uri> TryFrom<URI<'uri>> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: URI<'uri>) -> Result<Self, Self::Error> {
        RequestURI::try_from(URIReference::from(value))
    }
}

impl<'uri> TryFrom<URIReference<'uri>> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: URIReference<'uri>) -> Result<Self, Self::Error> {
        if value.has_scheme() && !value.has_authority() {
            return Err(InvalidRequestURI::MissingAuthority);
        }

        if !value.has_scheme() && value.has_authority() {
            return Err(InvalidRequestURI::MissingScheme);
        }

        if value.is_relative_reference() {
            let segments = value.path().segments();

            if segments.len() != 1
                || segments[0].as_str() != "*"
                || value.has_query()
                || value.has_fragment()
            {
                return Err(InvalidRequestURI::InvalidRelativeReference);
            } else {
                return Ok(RequestURI { components: None });
            }
        } else {
            if Scheme::try_from(value.scheme().unwrap()).is_err() {
                return Err(InvalidRequestURI::InvalidScheme(
                    InvalidScheme::NonRTSPScheme,
                ));
            }

            match value.host().unwrap() {
                Host::RegisteredName(ref name) if name.as_str().is_empty() => {
                    return Err(InvalidRequestURI::InvalidAuthority(
                        InvalidAuthority::InvalidHost(InvalidHost::EmptyHost),
                    ));
                }
                _ => (),
            }
        }

        let (scheme, authority, path, query, _) = value.into_owned().into_parts();
        let scheme = Scheme::try_from(scheme.unwrap())?;

        Ok(RequestURI {
            components: Some(RequestURIComponents {
                scheme,
                authority: authority.unwrap(),
                path,
                query,
            }),
        })
    }
}

/// An internal wrapper around all request URI components. This is done so that the entire thing can
/// be wrapped in an `Option` to avoid having to either use an enum as an interface or having all
/// components as `Options`s.
///
/// Request URIs cannot have fragments, so it is not included here.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct RequestURIComponents {
    /// The scheme of the request URI.
    scheme: Scheme,

    /// The authority of the request URI.
    authority: Authority<'static>,

    /// The path of the request URI.
    path: Path<'static>,

    /// The query of the request URI.
    query: Option<Query<'static>>,
}

/// A builder type for [`RequestURI]`.
///
/// You must use the [`RequestURI::scheme`], [`RequestURI:authority] and [`RequestURI::path`]
/// functions before building as URIs always have a scheme and path.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RequestURIBuilder<'uri> {
    /// The authority component of the URI reference as defined in
    /// [[RFC3986, Section 3.2]](https://tools.ietf.org/html/rfc3986#section-3.2).
    authority: Option<Result<Authority<'uri>, InvalidAuthority>>,

    /// The path component of the URI reference as defined in
    /// [[RFC3986, Section 3.3]](https://tools.ietf.org/html/rfc3986#section-3.3).
    path: Option<Result<Path<'uri>, InvalidPath>>,

    /// The query component of the URI reference as defined in
    /// [[RFC3986, Section 3.4]](https://tools.ietf.org/html/rfc3986#section-3.4).
    query: Option<Result<Query<'uri>, InvalidQuery>>,

    /// The scheme component of the URI reference as defined in
    /// [[RFC3986, Section 3.1]](https://tools.ietf.org/html/rfc3986#section-3.1).
    scheme: Option<Result<Scheme, InvalidScheme>>,
}

impl<'uri> RequestURIBuilder<'uri> {
    /// Sets the authority part of the request URI.
    ///
    /// If the given authority is not a valid authority (i.e. the conversion fails), an error is
    /// stored internally and checked during the [`RequestURIBuilder::build`] function. The error
    /// state will be rewritten for any following calls to this function.
    ///
    /// It is required to specify an authority.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").authority("example.com").path("/path");
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path");
    /// ```
    pub fn authority<AuthorityType, AuthorityError>(
        &mut self,
        authority: AuthorityType,
    ) -> &mut Self
    where
        Authority<'uri>: TryFrom<AuthorityType, Error = AuthorityError>,
        InvalidAuthority: From<AuthorityError>,
    {
        self.authority = Some(Authority::try_from(authority).map_err(|error| error.into()));
        self
    }

    /// Consumes the builder and tries to build a [`RequestURI`].
    ///
    /// This function will error in one of two situations:
    ///  - One of the components specified in the builder is invalid.
    ///  - A scheme, path, or authority was not specified in the builder.
    ///
    /// # Examples
    ///
    /// First error type (invalid path):
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").authority("example.com").path("this is an invalid path %%%");
    /// assert!(builder.build().is_err());
    /// ```
    ///
    /// Second error type (scheme, authority, or path was not specified):
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").path("/my/path");
    /// assert!(builder.build().is_err());
    /// ```
    pub fn build(self) -> Result<RequestURI, InvalidRequestURI> {
        let scheme = match self.scheme {
            Some(scheme) => scheme?,
            None => return Err(InvalidRequestURI::MissingScheme),
        };
        let authority = match self.authority {
            Some(authority) => authority?.into_owned(),
            None => return Err(InvalidRequestURI::MissingAuthority),
        };
        let path = match self.path {
            Some(path) => path?.into_owned(),
            None => return Err(InvalidRequestURI::MissingPath),
        };
        let query = match self.query {
            Some(query) => Some(query?.into_owned()),
            None => None,
        };

        let uri_reference =
            URIReference::from_parts(Some(scheme), Some(authority), path, query, None::<Fragment>)?;
        RequestURI::try_from(uri_reference)
    }

    /// Constructs a new builder with nothing set.
    pub fn new() -> Self {
        RequestURIBuilder::default()
    }

    /// Sets the path part of the request URI.
    ///
    /// If the given path is not a valid path (i.e. the conversion fails), an error is stored
    /// internally and checked during the [`RequestURIBuilder::build`] function. The error state
    /// will be rewritten for any following calls to this function.
    ///
    /// It is required to specify a path.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").authority("example.com").path("path");
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path");
    /// ```
    pub fn path<PathType, PathError>(&mut self, path: PathType) -> &mut Self
    where
        Path<'uri>: TryFrom<PathType, Error = PathError>,
        InvalidPath: From<PathError>,
    {
        self.path = Some(Path::try_from(path).map_err(|error| error.into()));
        self
    }

    /// Sets the query part of the request URI.
    ///
    /// If the given query is not a valid query (i.e. the conversion fails), an error is stored
    /// internally and checked during the [`RequestURIBuilder::build`] function. The error state
    /// will be rewritten for any following calls to this function.
    ///
    /// It is optional to specify a query.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").authority("example.com").path("path").query(Some("query"));
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path?query");
    /// ```
    pub fn query<QueryType, QueryError>(&mut self, query: Option<QueryType>) -> &mut Self
    where
        Query<'uri>: TryFrom<QueryType, Error = QueryError>,
        InvalidQuery: From<QueryError>,
    {
        self.query = query.map(|query| Query::try_from(query).map_err(|error| error.into()));
        self
    }

    /// Sets the scheme part of the request URI.
    ///
    /// If the given scheme is not a valid scheme (i.e. the conversion fails), an error is stored
    /// internally and checked during the [`RequestURIBuilder::build`] function. The error state
    /// will be rewritten for any following calls to this function.
    ///
    /// It is required to specify a scheme.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::uri::RequestURIBuilder;
    ///
    /// let mut builder = RequestURIBuilder::new();
    /// builder.scheme("rtsp").authority("example.com").path("path");
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path");
    /// ```
    pub fn scheme<SchemeType, SchemeError>(&mut self, scheme: SchemeType) -> &mut Self
    where
        Scheme: TryFrom<SchemeType, Error = SchemeError>,
        InvalidScheme: From<SchemeError>,
    {
        self.scheme = Some(Scheme::try_from(scheme).map_err(|error| error.into()));
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Scheme {
    RTSP,
    RTSPS,
    RTSPU,
}

impl Scheme {
    /// Returns the scheme in its string representation.
    pub fn as_str(&self) -> &str {
        use self::Scheme::*;

        match *self {
            RTSP => "rtsp",
            RTSPS => "rtsps",
            RTSPU => "rtspu",
        }
    }

    /// Returns the default port that will be used for the scheme is not specified.
    pub fn default_port(&self) -> u16 {
        use self::Scheme::*;

        match *self {
            RTSP | RTSPU => 554,
            RTSPS => 332,
        }
    }
}

impl From<Scheme> for URIScheme<'static> {
    fn from(value: Scheme) -> Self {
        use self::Scheme::*;

        match value {
            RTSP => URIScheme::RTSP,
            RTSPS => URIScheme::RTSPS,
            RTSPU => URIScheme::RTSPU,
        }
    }
}

impl PartialEq<[u8]> for Scheme {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'query> PartialEq<Scheme> for [u8] {
    fn eq(&self, other: &Scheme) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'a> PartialEq<&'a [u8]> for Scheme {
    fn eq(&self, other: &&'a [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'a, 'query> PartialEq<Scheme> for &'a [u8] {
    fn eq(&self, other: &Scheme) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for Scheme {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'query> PartialEq<Scheme> for str {
    fn eq(&self, other: &Scheme) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'a> PartialEq<&'a str> for Scheme {
    fn eq(&self, other: &&'a str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'a, 'query> PartialEq<Scheme> for &'a str {
    fn eq(&self, other: &Scheme) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'scheme> TryFrom<&'scheme [u8]> for Scheme {
    type Error = InvalidScheme;

    fn try_from(value: &'scheme [u8]) -> Result<Self, Self::Error> {
        let scheme = URIScheme::try_from(value)?;
        Ok(Scheme::try_from(scheme).unwrap())
    }
}

impl<'scheme> TryFrom<&'scheme str> for Scheme {
    type Error = InvalidScheme;

    fn try_from(value: &'scheme str) -> Result<Self, Self::Error> {
        Scheme::try_from(value.as_bytes())
    }
}

impl<'scheme> TryFrom<URIScheme<'scheme>> for Scheme {
    type Error = InvalidScheme;

    fn try_from(value: URIScheme<'scheme>) -> Result<Self, Self::Error> {
        Scheme::try_from(&value)
    }
}

impl<'a, 'scheme> TryFrom<&'a URIScheme<'scheme>> for Scheme {
    type Error = InvalidScheme;

    fn try_from(value: &'a URIScheme<'scheme>) -> Result<Self, Self::Error> {
        use self::URIScheme::*;

        match value {
            RTSP => Ok(Scheme::RTSP),
            RTSPS => Ok(Scheme::RTSPS),
            RTSPU => Ok(Scheme::RTSPU),
            _ => Err(InvalidScheme::NonRTSPScheme),
        }
    }
}

/// An error representing an invalid authority.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidAuthority {
    /// The host component of the authority was invalid.
    InvalidHost(InvalidHost),

    /// The port component of the authority was invalid.
    InvalidPort(InvalidPort),

    /// The user information component of the authority was invalid.
    InvalidUserInfo(InvalidUserInfo),
}

impl Display for InvalidAuthority {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::InvalidAuthority::*;

        match self {
            InvalidHost(invalid_host) => write!(formatter, "{}", invalid_host),
            InvalidPort(invalid_port) => write!(formatter, "{}", invalid_port),
            InvalidUserInfo(invalid_user_info) => write!(formatter, "{}", invalid_user_info),
        }
    }
}

impl Error for InvalidAuthority {}

impl From<!> for InvalidAuthority {
    fn from(value: !) -> Self {
        value
    }
}

impl From<InvalidURIAuthority> for InvalidAuthority {
    fn from(value: InvalidURIAuthority) -> Self {
        use self::InvalidAuthority::*;

        match value {
            InvalidURIAuthority::InvalidHost(invalid_host) => InvalidHost(invalid_host.into()),
            InvalidURIAuthority::InvalidPort(invalid_port) => InvalidPort(invalid_port),
            InvalidURIAuthority::InvalidUserInfo(invalid_user_info) => {
                InvalidUserInfo(invalid_user_info)
            }
            _ => panic!("unhandled invalid authority error"),
        }
    }
}

impl From<InvalidHost> for InvalidAuthority {
    fn from(value: InvalidHost) -> Self {
        InvalidAuthority::InvalidHost(value)
    }
}

impl From<InvalidPort> for InvalidAuthority {
    fn from(value: InvalidPort) -> Self {
        InvalidAuthority::InvalidPort(value)
    }
}

impl From<InvalidUserInfo> for InvalidAuthority {
    fn from(value: InvalidUserInfo) -> Self {
        InvalidAuthority::InvalidUserInfo(value)
    }
}

/// An error representing an invalid host.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidHost {
    /// The syntax for a future IP literal was used and is not currently supported.
    AddressMechanismNotSupported,

    /// A host was specified, but it was an empty string.
    EmptyHost,

    /// An invalid character for an IPv4 address or registered name was used. Due to the ambiguity
    /// of the grammar, it is not possible to say which. It is also possible that all the characters
    /// were valid, but there was an invalid percent encoding (e.g. `"%ZZ"`).
    InvalidIPv4OrRegisteredNameCharacter,

    /// The syntax for an IPv6 literal was used (i.e. `"[...]"`), but it contained an invalid IPv6
    /// character.
    InvalidIPv6Character,

    /// The syntax for an IPv6 literal was used (i.e. `"[...]"`) and all of the characters were
    /// valid IPv6 characters. However, the format of the literal was invalid.
    InvalidIPv6Format,

    /// The syntax for a future IP literal was used (i.e. `"[v*...]"` where `"*"` is a hexadecimal
    /// digit), but it contained an invalid character.
    InvalidIPvFutureCharacter,
}

impl Display for InvalidHost {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::InvalidHost::*;

        match self {
            AddressMechanismNotSupported => write!(formatter, "address mechanism not supported"),
            EmptyHost => write!(formatter, "empty host"),
            InvalidIPv4OrRegisteredNameCharacter => {
                write!(formatter, "invalid IPv4 or registered name character")
            }
            InvalidIPv6Character => write!(formatter, "invalid IPv6 character"),
            InvalidIPv6Format => write!(formatter, "invalid IPv6 format"),
            InvalidIPvFutureCharacter => write!(formatter, "invalid IPvFuture character"),
        }
    }
}

impl Error for InvalidHost {}

impl From<!> for InvalidHost {
    fn from(value: !) -> Self {
        value
    }
}

impl From<InvalidURIHost> for InvalidHost {
    fn from(value: InvalidURIHost) -> Self {
        use self::InvalidHost::*;

        match value {
            InvalidURIHost::AddressMechanismNotSupported => AddressMechanismNotSupported,
            InvalidURIHost::InvalidIPv4OrRegisteredNameCharacter => {
                InvalidIPv4OrRegisteredNameCharacter
            }
            InvalidURIHost::InvalidIPv6Character => InvalidIPv6Character,
            InvalidURIHost::InvalidIPv6Format => InvalidIPv6Format,
            InvalidURIHost::InvalidIPvFutureCharacter => InvalidIPvFutureCharacter,
            _ => panic!("unhandled invalid host error"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidRequestURI {
    /// The authority component of the relative reference was invalid.
    InvalidAuthority(InvalidAuthority),

    /// The path component of the relative reference was invalid.
    InvalidPath(InvalidPath),

    /// The query component of the relative reference was invalid.
    InvalidQuery(InvalidQuery),

    /// The parsed URI was actually a relative reference but was not an asterisk which is the only
    /// allowed relative reference.
    InvalidRelativeReference,

    /// The scheme component of the relative reference was invalid.
    InvalidScheme(InvalidScheme),

    /// This error occurs when you do not specify an authority component on the builder.
    ///
    /// This can only occur when using [`RequestURIBuilder`].
    MissingAuthority,

    /// This error occurs when you do not specify a path component on the builder.
    ///
    /// This can only occur when using [`RequestURIBuilder`].
    MissingPath,

    /// This error occurs when you do not specify a scheme component on the builder.
    ///
    /// This can only occur when using [`RequestURIBuilder`].
    MissingScheme,
}

impl Display for InvalidRequestURI {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::InvalidRequestURI::*;

        match self {
            InvalidAuthority(invalid_authority) => write!(formatter, "{}", invalid_authority),
            InvalidPath(invalid_path) => write!(formatter, "{}", invalid_path),
            InvalidQuery(invalid_query) => write!(formatter, "{}", invalid_query),
            InvalidRelativeReference => write!(formatter, "invalid relative reference"),
            InvalidScheme(invalid_scheme) => write!(formatter, "{}", invalid_scheme),
            MissingAuthority => write!(formatter, "missing authority"),
            MissingPath => write!(formatter, "missing path"),
            MissingScheme => write!(formatter, "missing scheme"),
        }
    }
}

impl Error for InvalidRequestURI {}

impl From<!> for InvalidRequestURI {
    fn from(value: !) -> Self {
        value
    }
}

impl From<InvalidURIReference> for InvalidRequestURI {
    fn from(value: InvalidURIReference) -> Self {
        use self::InvalidRequestURI::*;

        match value {
            InvalidURIReference::InvalidAuthority(invalid_authority) => {
                InvalidAuthority(invalid_authority.into())
            }
            InvalidURIReference::InvalidPath(invalid_path) => InvalidPath(invalid_path),
            InvalidURIReference::InvalidQuery(invalid_query) => InvalidQuery(invalid_query),
            InvalidURIReference::InvalidScheme(invalid_scheme) => {
                InvalidScheme(invalid_scheme.into())
            }
            InvalidURIReference::MissingPath => MissingPath,
            _ => panic!("unhandled invalid URI reference error"),
        }
    }
}

impl From<InvalidAuthority> for InvalidRequestURI {
    fn from(value: InvalidAuthority) -> Self {
        InvalidRequestURI::InvalidAuthority(value)
    }
}

impl From<InvalidURIAuthority> for InvalidRequestURI {
    fn from(value: InvalidURIAuthority) -> Self {
        InvalidRequestURI::InvalidAuthority(value.into())
    }
}

impl From<InvalidPath> for InvalidRequestURI {
    fn from(value: InvalidPath) -> Self {
        InvalidRequestURI::InvalidPath(value)
    }
}

impl From<InvalidQuery> for InvalidRequestURI {
    fn from(value: InvalidQuery) -> Self {
        InvalidRequestURI::InvalidQuery(value)
    }
}

impl From<InvalidScheme> for InvalidRequestURI {
    fn from(value: InvalidScheme) -> Self {
        InvalidRequestURI::InvalidScheme(value)
    }
}

impl From<InvalidURIScheme> for InvalidRequestURI {
    fn from(value: InvalidURIScheme) -> Self {
        InvalidRequestURI::InvalidScheme(value.into())
    }
}

/// An error representing an invalid scheme.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum InvalidScheme {
    /// The scheme component was empty.
    CannotBeEmpty,

    /// The scheme contained an invalid scheme character.
    InvalidCharacter,

    /// The scheme did not start with an alphabetic character.
    MustStartWithAlphabetic,

    /// The scheme was a non-RTSP scheme.
    NonRTSPScheme,
}

impl Display for InvalidScheme {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::InvalidScheme::*;

        match self {
            CannotBeEmpty => write!(formatter, "scheme cannot be empty"),
            InvalidCharacter => write!(formatter, "invalid scheme character"),
            MustStartWithAlphabetic => {
                write!(formatter, "scheme must start with alphabetic character")
            }
            NonRTSPScheme => write!(formatter, "non-RTSP scheme"),
        }
    }
}

impl Error for InvalidScheme {}

impl From<!> for InvalidScheme {
    fn from(value: !) -> Self {
        value
    }
}

impl From<InvalidURIScheme> for InvalidScheme {
    fn from(value: InvalidURIScheme) -> Self {
        use self::InvalidScheme::*;

        match value {
            InvalidURIScheme::CannotBeEmpty => CannotBeEmpty,
            InvalidURIScheme::InvalidCharacter => InvalidCharacter,
            InvalidURIScheme::MustStartWithAlphabetic => MustStartWithAlphabetic,
            _ => panic!("unhandled invalid URI scheme error"),
        }
    }
}
