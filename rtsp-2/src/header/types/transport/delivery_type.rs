use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};

/// The delivery type parameter in a transport specification indicating whether unicast or multicast
/// delivery will be attempted.
///
/// One of the two values must be specified. Clients that are capable of handling both
/// unicast and multicast transmission need to indicate such capability by including two full
/// transport-specs with separate parameters for each.
#[derive(Clone, Copy)]
pub enum DeliveryType {
    /// Multicast delivery is to be used.
    ///
    /// If used, the multicast-specific parameter `"ttl"` can be specified as well.
    Multicast,

    /// Unicast delivery is to be used.
    Unicast,
}

impl DeliveryType {
    /// Returns a `&str` representation of the connection parameter.
    ///
    /// The returned string is always lowercase.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::types::transport::DeliveryType;
    ///
    /// assert_eq!(DeliveryType::Multicast.as_str(), "multicast");
    /// assert_eq!(DeliveryType::Unicast.as_str(), "unicast");
    /// ```
    pub fn as_str(&self) -> &str {
        use self::DeliveryType::*;

        match self {
            Multicast => "multicast",
            Unicast => "unicast",
        }
    }
}

impl AsRef<[u8]> for DeliveryType {
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl AsRef<str> for DeliveryType {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Display for DeliveryType {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "{}", self.as_str())
    }
}

impl From<DeliveryType> for String {
    fn from(value: DeliveryType) -> Self {
        value.to_string()
    }
}

impl PartialEq<[u8]> for DeliveryType {
    fn eq(&self, other: &[u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<DeliveryType> for [u8] {
    fn eq(&self, other: &DeliveryType) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl<'delivery_type> PartialEq<&'delivery_type [u8]> for DeliveryType {
    fn eq(&self, other: &&'delivery_type [u8]) -> bool {
        self.as_str().as_bytes().eq_ignore_ascii_case(other)
    }
}

impl<'delivery_type> PartialEq<DeliveryType> for &'delivery_type [u8] {
    fn eq(&self, other: &DeliveryType) -> bool {
        self.eq_ignore_ascii_case(other.as_str().as_bytes())
    }
}

impl PartialEq<str> for DeliveryType {
    fn eq(&self, other: &str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl PartialEq<DeliveryType> for str {
    fn eq(&self, other: &DeliveryType) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'delivery_type> PartialEq<&'delivery_type str> for DeliveryType {
    fn eq(&self, other: &&'delivery_type str) -> bool {
        self.as_str().eq_ignore_ascii_case(other)
    }
}

impl<'delivery_type> PartialEq<DeliveryType> for &'delivery_type str {
    fn eq(&self, other: &DeliveryType) -> bool {
        self.eq_ignore_ascii_case(other.as_str())
    }
}

impl<'delivery_type> TryFrom<&'delivery_type [u8]> for DeliveryType {
    type Error = DeliveryTypeError;

    fn try_from(value: &'delivery_type [u8]) -> Result<Self, Self::Error> {
        use self::DeliveryType::*;

        if value.eq_ignore_ascii_case(b"unicast") {
            Ok(Unicast)
        } else if value.eq_ignore_ascii_case(b"multicast") {
            Ok(Multicast)
        } else {
            Err(DeliveryTypeError)
        }
    }
}

impl<'delivery_type> TryFrom<&'delivery_type str> for DeliveryType {
    type Error = DeliveryTypeError;

    fn try_from(value: &'delivery_type str) -> Result<Self, Self::Error> {
        DeliveryType::try_from(value.as_bytes())
    }
}

/// A possible error value when converting to a [`DeliveryType`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct DeliveryTypeError;

impl Display for DeliveryTypeError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "unknown delivery type parameter")
    }
}

impl Error for DeliveryTypeError {}

impl From<Infallible> for DeliveryTypeError {
    fn from(_: Infallible) -> Self {
        DeliveryTypeError
    }
}
