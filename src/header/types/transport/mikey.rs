use base64::{self, DecodeError as Base64DecodeError};
use std::convert::TryFrom;
use std::ops::Deref;

/// The MIKEY parameter used in conjunction with transport specifications that can utilize MIKEY
/// [[RFC3830]](https://tools.ietf.org/html/rfc3830) for security context establishment.
///
/// So far, only the SRTP-based RTP profiles SAVP and SAVPF can utilize MIKEY, and this is defined
/// in [[RFC7826, Appendix C.1.4.1]](https://tools.ietf.org/html/rfc7826#appendix-C.1.4.1). This
/// parameter can be included both in request and response messages. The binary MIKEY message shall
/// be Base64-encoded [[RFC4648]](https://tools.ietf.org/html/rfc4648) before being included in the
/// value part of the parameter, where the encoding adheres to the definition in
/// [[RFC4648, Section 4]](https://tools.ietf.org/html/rfc4648#section-4) and where the padding bits
/// are set to zero.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct MIKEY(Vec<u8>);

impl Deref for MIKEY {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'mikey> TryFrom<&'mikey [u8]> for MIKEY {
    type Error = MIKEYError;

    fn try_from(value: &'mikey [u8]) -> Result<Self, Self::Error> {
        Ok(MIKEY(base64::decode(value)?))
    }
}

pub type MIKEYError = Base64DecodeError;
