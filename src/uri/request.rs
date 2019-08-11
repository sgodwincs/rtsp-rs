//! Request URI
//!
//! This module contains a definition of the [`URI`] type to be used as a request URI.
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
//! use std::convert::TryFrom;
//!
//! use rtsp::uri::request::URI;
//!
//! let uri = URI::try_from("rtsp://127.0.0.1").unwrap();
//! assert_eq!(uri.to_string(), "rtsp://127.0.0.1/");
//! ```

use lazy_static::lazy_static;
use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Write};
use std::mem;
use uriparse::{
    Host as GenericHost, Scheme as GenericScheme, URIReference, URIReferenceError,
    URI as GenericURI,
};

use crate::uri::{
    Authority, AuthorityError, Fragment, Host, Password, Path, PathError, Query, QueryError,
    Scheme, SchemeError, Username, RTSPS_DEFAULT_PORT, RTSPU_DEFAULT_PORT, RTSP_DEFAULT_PORT,
};

lazy_static! {
    static ref ASTERISK_PATH: Path<'static> =
        Path::try_from("*").expect("expected asterisk path to be valid");
    static ref SENTINEL_AUTHORITY: Authority<'static> =
        Authority::try_from("").expect("expected sentinel authority to be valid");
    static ref SENTINEL_PATH: Path<'static> =
        Path::try_from("").expect("expected sentinel path to be valid");
}

/// The URI used in the RTSP request line.
///
/// See [[RFC7826, Section 4.2]](https://tools.ietf.org/html/rfc7826#section-4.2) for a description
/// of how RTSP handles URIs (and request URIs) in general.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct URI {
    /// A wrapper around the URI components. This will be [`Option::None`] if the request URI is an asterisk.
    components: Option<Components>,
}

impl URI {
    /// Creates a new request URI with string representation `"*"`.
    ///
    /// An asterisk `"*"` indicates that the request does not apply to a particular resource but to
    /// the server or proxy itself and is only alowed when the request method does not necessarily
    /// apply to a resource.
    ///
    /// An asterisk request URI is essentially immutable, all functions on this type that would
    /// mutate request URIs are no-ops (e.g. [`URI::set_scheme`]).
    pub fn asterisk() -> Self {
        URI { components: None }
    }

    /// Returns the authority of the request URI.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com/my/path").unwrap();
    /// assert_eq!(uri.authority().unwrap().to_string(), "example.com");
    /// ```
    pub fn authority(&self) -> Option<&Authority> {
        self.components
            .as_ref()
            .map(|components| &components.authority)
    }

    /// Constructs a default builder for a request URI.
    ///
    /// This provides an alternative means of constructing a URI besides parsing and
    /// [`URI::from_parts`].
    ///
    /// This function cannot be used to create an asterisk request URI, use [`URI::asterisk`] for
    /// that.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("example.com").unwrap())
    ///     .with_path(Path::try_from("/my/path").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/my/path");
    /// ```
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Returns the default port for this URI based on the scheme.
    ///
    /// This will be [`Option::None`] only for asterisk request URIs.
    ///
    /// The default port for RTSP and RTSPU is 554, while the default port for RTSPS is 332.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("example.com").unwrap())
    ///     .with_path(Path::try_from("/my/path").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.default_port().unwrap(), 554);
    /// ```
    pub fn default_port(&self) -> Option<u16> {
        match self.scheme()? {
            Scheme::RTSP => Some(RTSP_DEFAULT_PORT),
            Scheme::RTSPS => Some(RTSPS_DEFAULT_PORT),
            Scheme::RTSPU => Some(RTSPU_DEFAULT_PORT),
            _ => panic!("unexpected request URI scheme"),
        }
    }

    /// Constructs a new [`URI`] from the individual parts: scheme, authority, path, and
    /// query.
    ///
    /// This function cannot be used to create an asterisk request URI, use [`URI::asterisk`] for
    /// that.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Query, Scheme};
    ///
    /// let uri = URI::from_parts(
    ///     Scheme::RTSP,
    ///     Authority::try_from("example.com").unwrap(),
    ///     Path::try_from("").unwrap(),
    ///     None::<Query>
    /// ).unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/");
    /// ```
    pub fn from_parts(
        scheme: Scheme,
        authority: Authority,
        path: Path,
        query: Option<Query>,
    ) -> Result<Self, URIError> {
        Builder::new()
            .with_scheme(scheme)
            .with_authority(authority)
            .with_path(path)
            .with_query(query)
            .build()
    }

    /// Returns whether the request URI has an authority component.
    ///
    /// This will be false only for asterisk request URIs.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.has_authority());
    ///
    /// let uri = URI::try_from("*").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.has_host());
    ///
    /// let uri = URI::try_from("*").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://user:pass@127.0.0.1").unwrap();
    /// assert!(uri.has_password());
    ///
    /// let uri = URI::try_from("rtsp://user@127.0.0.1").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://127.0.0.1:554").unwrap();
    /// assert!(uri.has_port());
    ///
    /// let uri = URI::try_from("rtsp://127.0.0.1").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com/my/path?my=query").unwrap();
    /// assert!(uri.has_query());
    ///
    /// let uri = URI::try_from("rtsp://example.com/my/path").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://username@example.com").unwrap();
    /// assert!(uri.has_username());
    ///
    /// let uri = URI::try_from("rtsp://example.com").unwrap();
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
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://username@example.com").unwrap();
    /// assert_eq!(uri.host().unwrap().to_string(), "example.com");
    /// ```
    pub fn host(&self) -> Option<&Host> {
        self.components
            .as_ref()
            .map(|components| components.authority.host())
    }

    /// Consumes the request URI and converts it into a builder with the same values.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Query;
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com/path?query").unwrap();
    /// let uri = uri.into_builder()
    ///     .unwrap()
    ///     .with_query(None)
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/path");
    /// ```
    pub fn into_builder(self) -> Option<Builder> {
        if self.is_asterisk() {
            return None;
        }

        let (scheme, authority, path, query) = self
            .into_parts()
            .expect("expected URI, not relative reference");
        let builder = Builder::new()
            .with_scheme(scheme)
            .with_authority(authority)
            .with_path(path)
            .with_query(query);
        Some(builder)
    }

    /// Consumes the [`URI`] and returns its parts: scheme, authority, path, and query.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from(
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
        Scheme<'static>,
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("*").unwrap();
    /// assert!(uri.is_asterisk());
    ///
    /// let uri = URI::try_from("rtsp://example.com").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com/?a=b").unwrap();
    /// assert!(uri.is_normalized());
    ///
    /// let mut uri = URI::try_from("rtsp://EXAMPLE.com/?a=b").unwrap();
    /// assert!(!uri.is_normalized());
    /// uri.normalize();
    /// assert!(uri.is_normalized());
    /// ```
    pub fn is_normalized(&self) -> bool {
        self.components
            .as_ref()
            .map(|components| {
                if let Some(port) = components.authority.port() {
                    if port == self.default_port().expect("expected default port for URI") {
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Authority;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.map_authority(|_| Authority::try_from("127.0.0.1").unwrap());
    /// assert_eq!(uri.to_string(), "rtsp://127.0.0.1/");
    /// ```
    pub fn map_authority<TMapper>(&mut self, mapper: TMapper) -> Option<&Authority>
    where
        TMapper: FnOnce(Authority) -> Authority,
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.map_path(|mut path| {
    ///     path.push("test").unwrap();
    ///     path.push("path").unwrap();
    ///     path
    /// });
    /// assert_eq!(uri.to_string(), "rtsp://example.com/test/path");
    /// ```
    pub fn map_path<TMapper>(&mut self, mapper: TMapper) -> &Path
    where
        TMapper: FnOnce(Path) -> Path,
    {
        match self.components.as_mut() {
            Some(components) => {
                let path = mapper(mem::replace(&mut components.path, SENTINEL_PATH.clone()));
                self.set_path(path)
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Query;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.map_query(|_| Some(Query::try_from("query").unwrap()));
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?query");
    /// ```
    pub fn map_query<TMapper>(&mut self, mapper: TMapper) -> Option<&Query>
    where
        TMapper: FnOnce(Option<Query>) -> Option<Query>,
    {
        match self.components.as_mut() {
            Some(components) => {
                let query = mapper(components.query.take());
                self.set_query(query)
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
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Scheme;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.map_scheme(|_| Scheme::RTSPS);
    /// assert_eq!(uri.to_string(), "rtsps://example.com/");
    /// ```
    pub fn map_scheme<TMapper>(&mut self, mapper: TMapper) -> Option<Scheme>
    where
        TMapper: FnOnce(Scheme) -> Scheme,
    {
        match self.components.as_ref() {
            Some(components) => {
                let scheme = mapper(components.scheme.clone());
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com/?a=b").unwrap();
    /// uri.normalize();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?a=b");
    ///
    /// let mut uri = URI::try_from("rtsp://EXAMPLE.com/?a=b").unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://EXAMPLE.com/?a=b");
    /// uri.normalize();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?a=b");
    /// ```
    pub fn normalize(&mut self) {
        let default_port = self.default_port();

        if let Some(components) = self.components.as_mut() {
            components.authority.normalize();
            components.path.normalize(false);

            if let Some(query) = components.query.as_mut() {
                query.normalize();
            }

            if let Some(port) = components.authority.port() {
                if port == default_port.expect("expected default port for URI") {
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://127.0.0.1/my/path").unwrap();
    /// assert_eq!(uri.path(), "/my/path");
    /// ```
    pub fn path(&self) -> &Path {
        match self.components.as_ref() {
            Some(components) => &components.path,
            None => &ASTERISK_PATH,
        }
    }

    /// Returns the password of the request URI.
    ///
    /// Usage of a password in URIs is deprecated.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://user:pass@example.com").unwrap();
    /// assert_eq!(uri.password().unwrap(), "pass");
    /// ```
    pub fn password(&self) -> Option<&Password> {
        self.components
            .as_ref()
            .and_then(|components| components.authority.password())
    }

    /// Returns the port of the request URI.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://example.com:8080/").unwrap();
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://127.0.0.1?my=query").unwrap();
    /// assert_eq!(uri.query().unwrap(), "my=query");
    /// ```
    pub fn query(&self) -> Option<&Query> {
        self.components
            .as_ref()
            .and_then(|components| components.query.as_ref())
    }

    /// Returns the scheme of the request URI.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://127.0.0.1/").unwrap();
    /// assert_eq!(uri.scheme().unwrap(), "rtsp");
    /// ```
    pub fn scheme(&self) -> Option<Scheme> {
        self.components
            .as_ref()
            .map(|components| components.scheme.clone())
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Authority;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.set_authority(Authority::try_from("user@example.com:80").unwrap());
    /// assert_eq!(uri.to_string(), "rtsp://user@example.com:80/");
    /// ```
    pub fn set_authority(
        &mut self,
        authority: Authority<'static>,
    ) -> Result<Option<&Authority>, URIError> {
        match authority.host() {
            Host::RegisteredName(name) if name.as_str().is_empty() => {
                return Err(URIError::EmptyHost);
            }
            _ => (),
        }

        if let Some(components) = self.components.as_mut() {
            components.authority = authority;
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Path;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.set_path(Path::try_from("my/path").unwrap());
    /// assert_eq!(uri.to_string(), "rtsp://example.com/my/path");
    /// ```
    pub fn set_path(&mut self, mut path: Path<'static>) -> &Path {
        if let Some(components) = self.components.as_mut() {
            path.set_absolute(true);
            components.path = path;
        }

        self.path()
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
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Query;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.set_query(Some(Query::try_from("myquery").unwrap()));
    /// assert_eq!(uri.to_string(), "rtsp://example.com/?myquery");
    /// ```
    pub fn set_query(&mut self, query: Option<Query<'static>>) -> Option<&Query> {
        if let Some(components) = self.components.as_mut() {
            components.query = match query {
                Some(query) => Some(query),
                None => None,
            };
        }

        self.query()
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
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::Scheme;
    /// use rtsp::uri::request::URI;
    ///
    /// let mut uri = URI::try_from("rtsp://example.com").unwrap();
    /// uri.set_scheme(Scheme::RTSPS);
    /// assert_eq!(uri.to_string(), "rtsps://example.com/");
    /// ```
    pub fn set_scheme(&mut self, scheme: Scheme<'static>) -> Result<Option<Scheme>, URIError> {
        if scheme != Scheme::RTSP && scheme != Scheme::RTSPS && scheme != Scheme::RTSPU {
            return Err(URIError::NonRTSPScheme);
        }

        if let Some(components) = self.components.as_mut() {
            components.scheme = scheme;
        }

        Ok(self.scheme())
    }

    /// Returns the username of the request URI.
    ///
    /// If the request URI is an asterisk, this will return [`Option::None`].
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    ///
    /// let uri = URI::try_from("rtsp://username@example.com").unwrap();
    /// assert_eq!(uri.username().unwrap(), "username");
    /// ```
    pub fn username(&self) -> Option<&Username> {
        self.components
            .as_ref()
            .and_then(|components| components.authority.username())
    }
}

impl Display for URI {
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

impl From<URI> for String {
    fn from(value: URI) -> Self {
        value.to_string()
    }
}

impl<'uri> From<URI> for URIReference<'uri> {
    fn from(value: URI) -> Self {
        match value.components {
            Some(components) => URIReference::from_parts(
                Some(components.scheme),
                Some(components.authority),
                components.path,
                components.query,
                None::<Fragment>,
            ),
            None => URIReference::from_parts(
                None::<GenericScheme>,
                None::<Authority>,
                ASTERISK_PATH.clone(),
                None::<Query>,
                None::<Fragment>,
            ),
        }
        .expect("request URI should be a valid URI reference")
    }
}

impl<'uri> TryFrom<&'uri [u8]> for URI {
    type Error = URIError;

    fn try_from(value: &'uri [u8]) -> Result<Self, Self::Error> {
        let uri_reference = URIReference::try_from(value)
            .map_err(|error| URIError::try_from(error).expect("unexpected URI reference error"))?;
        URI::try_from(uri_reference)
    }
}

impl<'uri> TryFrom<&'uri str> for URI {
    type Error = URIError;

    fn try_from(value: &'uri str) -> Result<Self, Self::Error> {
        URI::try_from(value.as_bytes())
    }
}

impl<'uri> TryFrom<GenericURI<'uri>> for URI {
    type Error = URIError;

    fn try_from(value: GenericURI<'uri>) -> Result<Self, Self::Error> {
        URI::try_from(URIReference::from(value))
    }
}

impl<'uri> TryFrom<URIReference<'uri>> for URI {
    type Error = URIError;

    fn try_from(value: URIReference<'uri>) -> Result<Self, Self::Error> {
        if value.has_fragment() {
            return Err(URIError::FragmentNotAllowed);
        }

        if value.has_scheme() && !value.has_authority() {
            return Err(URIError::MissingAuthority);
        }

        if !value.has_scheme() && value.has_authority() {
            return Err(URIError::MissingScheme);
        }

        /*
        seems to be the case that we need to add ; parsing here for path parameters
        ; is a reserved key word
        */
        if value.is_relative_reference() {
            let segments = value.path().segments();

            if segments.len() != 1 || segments[0].as_str() != "*" || value.has_query() {
                return Err(URIError::InvalidRelativeReference);
            } else {
                return Ok(URI { components: None });
            }
        } else {
            let scheme = value.scheme().expect("expected a scheme");

            if scheme != &Scheme::RTSP && scheme != &Scheme::RTSPS && scheme != &Scheme::RTSPU {
                return Err(URIError::NonRTSPScheme);
            }

            match value.host().expect("expected a host") {
                GenericHost::RegisteredName(name) if name.as_str().is_empty() => {
                    return Err(URIError::EmptyHost);
                }
                _ => (),
            }
        }

        let (scheme, authority, path, query, _) = value.into_owned().into_parts();

        Ok(URI {
            components: Some(Components {
                scheme: scheme.expect("expected a scheme"),
                authority: authority.expect("expected an authority"),
                path,
                query,
            }),
        })
    }
}

/// An internal wrapper around all request URI components. This is done so that the entire thing can
/// be wrapped in an [`Option`] to avoid having to either use an enum as an interface or having all
/// components as [`Options`]s.
///
/// Request URIs cannot have fragments, so it is not included here.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct Components {
    /// The scheme of the request URI.
    scheme: Scheme<'static>,

    /// The authority of the request URI.
    authority: Authority<'static>,

    /// The path of the request URI.
    path: Path<'static>,

    /// The query of the request URI.
    query: Option<Query<'static>>,
}

/// A builder type for [`URI]`.
///
/// You must use the [`URI::scheme`], [`URI:authority] and [`URI::path`] functions before building
/// as request URIs always have a scheme, authority, and path.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Builder {
    /// The authority component of the URI reference as defined in
    /// [[RFC3986, Section 3.2]](https://tools.ietf.org/html/rfc3986#section-3.2).
    authority: Option<Authority<'static>>,

    /// The path component of the URI reference as defined in
    /// [[RFC3986, Section 3.3]](https://tools.ietf.org/html/rfc3986#section-3.3).
    path: Option<Path<'static>>,

    /// The query component of the URI reference as defined in
    /// [[RFC3986, Section 3.4]](https://tools.ietf.org/html/rfc3986#section-3.4).
    query: Option<Query<'static>>,

    /// The scheme component of the URI reference as defined in
    /// [[RFC3986, Section 3.1]](https://tools.ietf.org/html/rfc3986#section-3.1).
    scheme: Option<Scheme<'static>>,
}

impl Builder {
    /// Set the authority for this URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let mut builder = URI::builder();
    /// builder
    ///     .scheme(Scheme::RTSP)
    ///     .authority(Authority::try_from("server.com").unwrap())
    ///     .path(Path::try_from("").unwrap());
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn authority(&mut self, authority: Authority) -> &mut Self {
        self.authority = Some(authority.into_owned());
        self
    }

    /// Consumes the builder and tries to build a [`URI`].
    ///
    /// This function will error if a scheme, path, or authority was not specified in the builder.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("example.com").unwrap())
    ///     .with_path(Path::try_from("/my/path").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://example.com/my/path");
    /// ```
    pub fn build(self) -> Result<URI, URIError> {
        let scheme = match self.scheme {
            Some(scheme) => scheme,
            None => return Err(URIError::MissingScheme),
        };
        let authority = match self.authority {
            Some(authority) => authority,
            None => return Err(URIError::MissingAuthority),
        };
        let path = match self.path {
            Some(path) => path,
            None => return Err(URIError::MissingPath),
        };
        let query = match self.query {
            Some(query) => Some(query),
            None => None,
        };

        let uri_reference =
            URIReference::from_parts(Some(scheme), Some(authority), path, query, None::<Fragment>)
                .map_err(|error| {
                    URIError::try_from(error).expect("unexpected URI reference error")
                })?;
        URI::try_from(uri_reference)
    }

    /// Constructs a new builder with nothing set.
    pub fn new() -> Self {
        Builder::default()
    }

    /// Set the path for this URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let mut builder = URI::builder();
    /// builder
    ///     .scheme(Scheme::RTSP)
    ///     .authority(Authority::try_from("server.com").unwrap())
    ///     .path(Path::try_from("").unwrap());
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn path(&mut self, path: Path) -> &mut Self {
        self.path = Some(path.into_owned());
        self
    }

    /// Set the path for this URI.
    ///
    /// The default value is having no query.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Query, Scheme};
    ///
    /// let mut builder = URI::builder();
    /// builder
    ///     .scheme(Scheme::RTSP)
    ///     .authority(Authority::try_from("server.com").unwrap())
    ///     .path(Path::try_from("").unwrap())
    ///     .query(Some(Query::try_from("query").unwrap()));
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/?query");
    /// ```
    pub fn query(&mut self, query: Option<Query>) -> &mut Self {
        self.query = query.map(Query::into_owned);
        self
    }

    /// Set the scheme for this URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let mut builder = URI::builder();
    /// builder
    ///     .scheme(Scheme::RTSP)
    ///     .authority(Authority::try_from("server.com").unwrap())
    ///     .path(Path::try_from("").unwrap());
    /// let uri = builder.build().unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn scheme(&mut self, scheme: Scheme) -> &mut Self {
        self.scheme = Some(scheme.into_owned());
        self
    }

    /// Consumes the builder and sets the authority part of the URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("server.com").unwrap())
    ///     .with_path(Path::try_from("").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn with_authority(mut self, authority: Authority) -> Self {
        self.authority(authority);
        self
    }

    /// Consumes the builder and sets the path part of the URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("server.com").unwrap())
    ///     .with_path(Path::try_from("").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn with_path(mut self, path: Path) -> Self {
        self.path(path);
        self
    }

    /// Consumes the builder and sets the query part of the URI.
    ///
    /// The default value is having no query.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Query, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("server.com").unwrap())
    ///     .with_path(Path::try_from("").unwrap())
    ///     .with_query(Some(Query::try_from("query").unwrap()))
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/?query");
    /// ```
    pub fn with_query(mut self, query: Option<Query>) -> Self {
        self.query(query);
        self
    }

    /// Consumes the builder and sets the scheme part of the URI.
    ///
    /// # Errors
    ///
    /// This does not have a default value and, as a result, it must be specified before
    /// [`Builder::build`] is called.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(type_alias_enum_variants)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::uri::request::URI;
    /// use rtsp::uri::{Authority, Path, Scheme};
    ///
    /// let uri = URI::builder()
    ///     .with_scheme(Scheme::RTSP)
    ///     .with_authority(Authority::try_from("server.com").unwrap())
    ///     .with_path(Path::try_from("").unwrap())
    ///     .build()
    ///     .unwrap();
    /// assert_eq!(uri.to_string(), "rtsp://server.com/");
    /// ```
    pub fn with_scheme(mut self, scheme: Scheme) -> Self {
        self.scheme(scheme);
        self
    }
}

/// A generic error indicating that the URI was invalid.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum URIError {
    /// The authority was invalid.
    Authority(AuthorityError),

    /// The host was an empty.
    EmptyHost,

    /// A fragment was specified, but a fragment is not allowed for request URIs.
    FragmentNotAllowed,

    /// The parsed URI was actually a relative reference but was not an asterisk which is the only
    /// allowed relative reference.
    InvalidRelativeReference,

    /// The authority was not specified.
    MissingAuthority,

    /// The path was not specified.
    MissingPath,

    /// The path was not specified.
    MissingScheme,

    /// The scheme was a non-RTSP scheme.
    NonRTSPScheme,

    /// The path was invalid.
    Path(PathError),

    /// The query was invalid.
    Query(QueryError),

    /// The scheme was invalid.
    Scheme(SchemeError),
}

impl Display for URIError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::URIError::*;

        match self {
            Authority(error) => error.fmt(formatter),
            EmptyHost => write!(formatter, "empty URI host"),
            FragmentNotAllowed => write!(formatter, "fragment not allowed in request URI"),
            InvalidRelativeReference => write!(formatter, "invalid URI relative reference"),
            MissingAuthority => write!(formatter, "missing URI authority"),
            MissingPath => write!(formatter, "missing URI path"),
            MissingScheme => write!(formatter, "missing URI scheme"),
            NonRTSPScheme => write!(formatter, "non-RTSP URI scheme"),
            Path(error) => error.fmt(formatter),
            Query(error) => error.fmt(formatter),
            Scheme(error) => error.fmt(formatter),
        }
    }
}

impl Error for URIError {}

impl From<Infallible> for URIError {
    fn from(_: Infallible) -> Self {
        URIError::InvalidRelativeReference
    }
}

impl TryFrom<URIReferenceError> for URIError {
    type Error = ();

    fn try_from(value: URIReferenceError) -> Result<Self, Self::Error> {
        use self::URIError::*;

        match value {
            URIReferenceError::Authority(error) => Ok(Authority(error)),
            URIReferenceError::Fragment(_) => Ok(FragmentNotAllowed),
            URIReferenceError::MissingPath => Ok(MissingPath),
            URIReferenceError::Path(error) => Ok(Path(error)),
            URIReferenceError::Query(error) => Ok(Query(error)),
            URIReferenceError::Scheme(error) => Ok(Scheme(error)),
            _ => Err(()),
        }
    }
}
