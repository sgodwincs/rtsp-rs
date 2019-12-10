use std::convert::{Infallible, TryFrom};
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ops::Deref;

/// The layers parameter in a transport specification that determines the number of multicast layers
/// to be used for a media stream.
///
/// The layers are sent to consecutive addresses starting at the `"dest_addr"` address.
///  If the parameter is not included, it defaults to a single layer.
///
/// This parameter may only be used if the delivery type of the transport specification is
/// multicast.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Layers(u8);

impl Default for Layers {
    fn default() -> Self {
        Layers(1)
    }
}

impl Deref for Layers {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'status> TryFrom<u8> for Layers {
    type Error = LayersError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value == 0 || value > 9 {
            Err(LayersError)
        } else {
            Ok(Layers(value))
        }
    }
}

/// A possible error value when converting to [`Layers`] from a `&[u8]` or `&str`.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct LayersError;

impl Display for LayersError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        write!(formatter, "invalid layer parameter")
    }
}

impl Error for LayersError {}

impl From<Infallible> for LayersError {
    fn from(_: Infallible) -> Self {
        LayersError
    }
}
