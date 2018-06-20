//! Request URI
//!
//! This module contains a definition of the [`RequestURIField`] and [`RequestURI`] types. These
//! types are intended to be accessed through the root of the crate rather than this module.
//!
//! These types are wrappers around the [`Url`] type from the `url` crate. This is done because
//! request URIs do not have to actually be URIs (can be `"*"`) and providing [`TryFrom`]
//! implementations for this type makes it easier to deal with the `Request` type. Also, the
//! [`Url`] type is a strict superset of valid URIs that are allowed in the request line,
//! specifically, the URIs must contain an authority part.

use std::convert::TryFrom;
use std::error::Error;
use std::io;
use std::net::IpAddr;
use std::{fmt, str};
use url::{Origin, Url};

pub use url::{form_urlencoded, Host, HostAndPort, PathSegmentsMut, UrlQuery as RequestURIQuery};

/// The main type of this module. Represents either a valid URI or a URI of the form `"*"`.
#[derive(Clone, Eq, Hash, PartialEq)]
pub enum RequestURIField {
    /// Indicates that the request does not apply to a particular resource but to the server or
    /// proxy itself, and is only allowed when the request method does not necessarily apply to a
    /// resource.
    Any,

    /// An absolute request URI (including scheme, host, and port).
    URI(RequestURI),
}

impl RequestURIField {
    /// Returns a [`&str`] representation of the request URI field. If the field is
    /// [`RequestURIField::Any`], then the string returned will be `"*"`, otherwise it will be the
    /// [`RequestURI`] as a string.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURIField;
    ///
    /// let uri = RequestURIField::try_from("rtsp://example.com").unwrap();
    /// assert_eq!(uri.as_str(), "rtsp://example.com/");
    ///
    /// let uri = RequestURIField::try_from("*").unwrap();
    /// assert_eq!(uri.as_str(), "*");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::RequestURIField::*;

        match *self {
            Any => "*",
            URI(ref uri) => uri.as_str(),
        }
    }

    /// Returns whether or not the request URI field is [`RequestURIField::Any`].
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURIField;
    ///
    /// assert!(RequestURIField::Any.is_any());
    ///
    /// let uri = RequestURIField::try_from("rtsp://example.com").unwrap();
    /// assert!(!uri.is_any());
    /// ```
    pub fn is_any(&self) -> bool {
        use self::RequestURIField::*;

        match *self {
            Any => true,
            _ => false,
        }
    }

    /// Returns whether or not the request URI field is [`RequestURIField::URI`].
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURIField;
    ///
    /// assert!(!RequestURIField::Any.is_uri());
    ///
    /// let uri = RequestURIField::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.is_uri());
    /// ```
    pub fn is_uri(&self) -> bool {
        use self::RequestURIField::*;

        match *self {
            URI(_) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for RequestURIField {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

impl fmt::Display for RequestURIField {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.as_str())
    }
}

/// Provides a fallible conversion from a byte slice to a [`RequestURIField`].
///
/// Note that you cannot do the following:
///
/// ```compile_fail
/// let play = RequestURIField::try_from(b"*").unwrap();
/// ```
///
/// This is because `b"*"` is of type `&[u8; 1]` and so it must be converted to `&[u8]` in order to
/// perform the conversion. Another `TryFrom` implementation from `&[u8, N: usize]` will be provided
/// once constant generics land on nightly.
impl<'a> TryFrom<&'a [u8]> for RequestURIField {
    type Error = InvalidRequestURI;

    /// Converts a `&[u8]` to a request URI field.
    ///
    /// # Return Value
    ///
    /// An error will be returned if the field is not `b"*"`, or it is not a valid URI with an
    /// authority part.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURIField;
    ///
    /// let any = RequestURIField::try_from(&b"*"[..]).unwrap();
    /// assert_eq!(any, RequestURIField::Any);
    ///
    /// let uri = RequestURIField::try_from(&b"rtsp://example.com"[..]).unwrap();
    /// assert!(uri.is_uri());
    ///
    /// let error = RequestURIField::try_from(&b"this is an invalid URI"[..]);
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let value = str::from_utf8(value).map_err(|_| InvalidRequestURI)?;
        RequestURIField::try_from(value)
    }
}

impl<'a> TryFrom<&'a str> for RequestURIField {
    type Error = InvalidRequestURI;

    /// Converts a `&str` to a request URI field.
    ///
    /// # Return Value
    ///
    /// An error will be returned if the field is not `"*"`, and it is not a valid URI with an
    /// authority part.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::RequestURIField;
    ///
    /// let any = RequestURIField::try_from("*").unwrap();
    /// assert_eq!(any, RequestURIField::Any);
    ///
    /// let uri = RequestURIField::try_from("rtsp://example.com").unwrap();
    /// assert!(uri.is_uri());
    ///
    /// let error = RequestURIField::try_from("this is an invalid URI");
    /// assert!(error.is_err());
    /// ```
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        use self::RequestURIField::*;

        if value.len() == 1 && value == "*" {
            Ok(Any)
        } else {
            Ok(URI(RequestURI::try_from(value)?))
        }
    }
}

/// A wrapper type around [`Url`] to prevent users from creating instances of
/// [`RequestURIField`] with unchecked [`Url`]s.
#[derive(Clone, Eq, Hash, PartialEq)]
pub struct RequestURI {
    inner: Url,
}

impl RequestURI {
    pub fn host(&self) -> Host<&str> {
        self.inner
            .host()
            .expect("request URI should always have a host")
    }

    pub fn host_str(&self) -> &str {
        self.inner
            .host_str()
            .expect("request URI should always have a host")
    }

    pub fn path_segments(&self) -> str::Split<char> {
        self.inner
            .path_segments()
            .expect("request URI should have a base")
    }

    pub fn path_segments_mut(&mut self) -> PathSegmentsMut {
        self.inner
            .path_segments_mut()
            .expect("request URI should have a base")
    }

    pub fn origin(&self) -> (String, Host<String>, u16) {
        match self.inner.origin() {
            Origin::Tuple(scheme, host, port) => (scheme, host, port),
            Origin::Opaque(_) => panic!("request URI should not have opaque origin"),
        }
    }

    pub fn set_host(&mut self, host: &str) {
        self.inner
            .set_host(Some(host))
            .expect("request URI should have a base");
    }

    pub fn set_ip_host(&mut self, address: IpAddr) {
        self.inner
            .set_ip_host(address)
            .expect("request URI should have a base");
    }

    pub fn set_password(&mut self, password: Option<&str>) {
        self.inner
            .set_password(password)
            .expect("request URI should have a base and host");
    }

    pub fn set_username(&mut self, username: &str) {
        self.inner
            .set_username(username)
            .expect("request URI should have a base and host");
    }

    delegate! {
        target self.inner {
            pub fn as_str(&self) -> &str;
            pub fn domain(&self) -> Option<&str>;
            pub fn into_string(self) -> String;
            pub fn password(&self) -> Option<&str>;
            pub fn path(&self) -> &str;
            pub fn port(&self) -> Option<u16>;
            pub fn port_or_known_default(&self) -> Option<u16>;
            pub fn query(&self) -> Option<&str>;
            pub fn query_pairs(&self) -> form_urlencoded::Parse;
            pub fn query_pairs_mut(&mut self) -> form_urlencoded::Serializer<RequestURIQuery>;
            pub fn scheme(&self) -> &str;
            pub fn set_path(&mut self, path: &str);
            pub fn set_port(&mut self, port: Option<u16>) -> Result<(), ()>;
            pub fn set_query(&mut self, query: Option<&str>);
            pub fn set_scheme(&mut self, scheme: &str) -> Result<(), ()>;
            pub fn username(&self) -> &str;
            pub fn with_default_port<F>(&self, f: F) -> io::Result<HostAndPort<&str>>
                where F: FnOnce(&Url) -> Result<u16, ()>;
        }
    }
}

impl fmt::Debug for RequestURI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.inner)
    }
}

impl fmt::Display for RequestURI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl<'a> TryFrom<&'a str> for RequestURI {
    type Error = InvalidRequestURI;

    /// Converts a `&str` to a request URI.
    ///
    /// Note that the fragment is removed from the URI if it is valid.
    ///
    /// # Return Value
    ///
    /// An error will be returned if it is not a valid URI with an authority part.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::{RequestURI};
    /// use rtsp::uri::Host;
    ///
    /// let uri = RequestURI::try_from("rtsp://example.com").unwrap();
    /// assert_eq!(uri.host(), Host::Domain("example.com"));
    /// assert_eq!(uri.scheme(), "rtsp");
    /// ```
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        let mut uri = Url::parse(value).map_err(|_| InvalidRequestURI)?;
        uri.set_fragment(None);

        if uri.has_authority() {
            Ok(RequestURI { inner: uri })
        } else {
            Err(InvalidRequestURI)
        }
    }
}

/// A possible error value when converting to a [`RequestURI`] or `[RequestURIField]` from a `&[u8]`
/// or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidRequestURI;

impl fmt::Display for InvalidRequestURI {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidRequestURI {
    fn description(&self) -> &str {
        "invalid RTSP request URI"
    }
}
