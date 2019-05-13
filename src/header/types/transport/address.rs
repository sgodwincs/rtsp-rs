use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter, Write};
use std::ops::Deref;
use std::str;
use uriparse::{authority, Host};

use crate::syntax;

/// A configuration parameter used to represent addresses for either the `"dest_addr"` or
/// `"src_addr"` transport header parameters.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Address {
    /// The address format does not follow the format
    /// specified by [[RFC3986](https://tools.ietf.org/html/rfc3986)].
    Extension(ExtensionAddress),

    /// The address follows a `"host:port"` format where the syntax is specified by
    /// [[RFC3986]](https://tools.ietf.org/html/rfc3986).
    HostPort(HostPort),
}

impl Address {
    /// A helper function that creates a new [`Address`] instance with the address extension.
    ///
    /// It first checks to see if the address is valid, and if not, it will return an error.
    ///
    /// Based on, [[RFC7826, Section 20.1](https://tools.ietf.org/html/rfc7826#section-20.1)], an
    /// extension address follows the following rules:
    ///
    /// ```text
    /// UTF8-2 = <As defined in RFC 3629>
    /// UTF8-3 = <As defined in RFC 3629>
    /// UTF8-4 = <As defined in RFC 3629>
    /// UTF8-NONASCII = UTF8-2 / UTF8-3 / UTF8-4
    /// quoted-pair = "\\" / ( "\" DQUOTE )
    /// qdtext = %x20-21 / %x23-5B / %x5D-7E / quoted-pair
    ///        / UTF8-NONASCII
    ///        ; No DQUOTE and no "\"
    /// extension-addr = 1*qdtext
    /// ``
    fn extension(value: &[u8]) -> Result<Self, AddressError> {
        if !syntax::is_qdtext(value) {
            return Err(AddressError);
        }

        // Unsafe: The function above [`syntax::is_token`] ensures that the value is valid
        // ASCII-US.
        let value = unsafe { str::from_utf8_unchecked(value) }.to_lowercase();
        Ok(Address::Extension(ExtensionAddress(value)))
    }
}

impl Display for Address {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::Address::*;

        match self {
            Extension(extension) => extension.fmt(formatter),
            HostPort(host_port) => host_port.fmt(formatter),
        }
    }
}

impl From<Address> for String {
    fn from(value: Address) -> Self {
        value.to_string()
    }
}

impl<'address> TryFrom<&'address [u8]> for Address {
    type Error = AddressError;

    fn try_from(value: &'address [u8]) -> Result<Self, Self::Error> {
        let (host, port) = match value.iter().position(|&byte| byte == b':') {
            Some(index) => {
                let host = &value[0..index];
                let port = &value[index + 1..];
                (host, Some(port))
            }
            None => (value, None),
        };

        let host = match Host::try_from(host) {
            Ok(host) => match host {
                Host::RegisteredName(ref host) if host.as_str().is_empty() => None,
                _ => Some(host.into_owned()),
            },
            Err(_) => return Address::extension(value),
        };
        let port = match port {
            Some(port) => match authority::parse_port(port) {
                Ok(port) => port,
                Err(_) => return Address::extension(value),
            },
            None => None,
        };

        Ok(Address::HostPort(HostPort { host, port }))
    }
}

impl<'address> TryFrom<&'address str> for Address {
    type Error = AddressError;

    fn try_from(value: &'address str) -> Result<Self, Self::Error> {
        Address::try_from(value.as_bytes())
    }
}

/// A subset of an authority component for a URI as defined by
/// [[RFC3986](https://tools.ietf.org/html/rfc3986)].
///
/// Only the host and port components of the authority can be specified.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct HostPort {
    /// The host as defined in
    /// [[RFC3986, Section 3.2.2](https://tools.ietf.org/html/rfc3986#section-3.2.2)].
    host: Option<Host<'static>>,

    /// The port componentas defined in
    /// [[RFC3986, Section 3.2.3](https://tools.ietf.org/html/rfc3986#section-3.2.3)].
    port: Option<u16>,
}

impl HostPort {
    /// The host component as defined in
    /// [[RFC3986, Section 3.2.2](https://tools.ietf.org/html/rfc3986#section-3.2.2)].
    ///
    /// Although the URI specification allows for empty hosts, when used as part of an address
    /// parameter in the transport header, an empty host is synonymous with not specifying one. This
    /// is because an address such as `":8080"` actually does not refer to an empty host, but rather
    /// refers to the host of the agent whom sent the message.
    pub fn host(&self) -> Option<&Host> {
        self.host.as_ref()
    }
    /// The port component as defined in
    /// [[RFC3986, Section 3.2.3]](https://tools.ietf.org/html/rfc3986#section-3.2.3).
    ///
    /// The port will be `None` if a port was not specified.
    pub fn port(&self) -> Option<u16> {
        self.port
    }
}

impl From<HostPort> for String {
    fn from(value: HostPort) -> String {
        value.to_string()
    }
}

impl Display for HostPort {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        if let Some(host) = self.host.as_ref() {
            host.fmt(formatter)?;
        }

        if let Some(port) = self.port {
            formatter.write_char(':')?;
            port.fmt(formatter)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ExtensionAddress(String);

impl ExtensionAddress {
    /// Returns a `&str` representation of the extension address.
    ///
    /// The returned string is lowercase even if the extension address originally was a
    /// non-lowercase address.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::Address;
    ///
    /// match Address::try_from(r#"\"EXTENSION\""#).unwrap() {
    ///     Address::Extension(extension) => assert_eq!(extension.as_str(), r#"\"extension\""#),
    ///     _ => panic!("expected extension address")
    /// }
    /// ```
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<[u8]> for ExtensionAddress {
    fn as_ref(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl AsRef<str> for ExtensionAddress {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Deref for ExtensionAddress {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for ExtensionAddress {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.0)
    }
}

impl From<ExtensionAddress> for String {
    fn from(value: ExtensionAddress) -> Self {
        value.to_string()
    }
}

/// A possible error value when converting to a [`Address`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct AddressError;

impl Display for AddressError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "invalid address")
    }
}

impl Error for AddressError {}

impl From<Infallible> for AddressError {
    fn from(_: Infallible) -> Self {
        AddressError
    }
}
