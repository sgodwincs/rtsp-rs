use core::fmt;
use std::convert::TryFrom;
use std::f32;
use std::fmt::{Error, Formatter};
use std::hash::Hash;
use std::hash::Hasher;
use std::iter::{once, FromIterator};
use std::ops::{Deref, DerefMut};
use std::str;

extern crate mime;
use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;
use crate::syntax;
use itertools::Itertools;
use linked_hash_set::LinkedHashSet;
use mime::Mime;
use std::string::ToString;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Accept(LinkedHashSet<MediaType>);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum AcceptError {
    InvalidUtf8,
    InvalidSyntax,
}

impl Deref for Accept {
    type Target = LinkedHashSet<MediaType>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Accept {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromIterator<MediaType> for Accept {
    fn from_iter<TIterator>(iterator: TIterator) -> Self
    where
        TIterator: IntoIterator<Item = MediaType>,
    {
        Accept(LinkedHashSet::from_iter(iterator))
    }
}

impl TypedHeader for Accept {
    type DecodeError = AcceptError;

    /// The Accept request-header field can be used to specify certain
    /// presentation description and parameter media types [RFC6838] that are
    /// acceptable for the response to the DESCRIBE request.
    ///
    /// ```text
    /// Accept            =  "Accept" HCOLON
    ///                        [ accept-range *(COMMA accept-range) ]
    ///    accept-range      =  media-type-range [SEMI accept-params]
    ///    accept-params     =  "q" EQUAL qvalue *(SEMI generic-param )
    ///    qvalue            =  ( "0" [ "." *3DIGIT ] )
    ///                      /  ( "1" [ "." *3("0") ] )
    ///    generic-param   =  token [ EQUAL gen-value ] //no example usage so not included
    ///    gen-value       =  token / host / quoted-string
    ///    host              = < As defined in RFC 3986>
    ///
    ///    media-type-range  =  ( "*//*"
    ///                            / ( m-type SLASH "*" )
    ///                            / ( m-type SLASH m-subtype )
    ///                                ) *( SEMI m-parameter )
    ///    m-type             =  discrete-type / composite-type
    ///    discrete-type      =  "text" / "image" / "audio" / "video"
    ///                      /  "application" / extension-token
    ///    composite-type   =  "message" / "multipart" / extension-token
    ///    extension-token  =  ietf-token / x-token
    ///    ietf-token       =  token
    ///    x-token          =  "x-" token
    ///    m-subtype        =  extension-token / iana-token
    ///    iana-token       =  token
    ///    m-parameter      =  m-attribute EQUAL m-value
    ///    m-attribute      =  token
    ///    m-value          =  token / quoted-string
    ///    token           =  1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
    ///                        /  %x41-5A / %x5E-7A / %x7C / %x7E) //any CHAR except CTLs or tspecials
    /// ```
    ///
    /// All values seperated with commas will be converted into [`MediaType`] type.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::accept_ranges::RangeFormat;
    /// use rtsp::header::types::Accept;
    /// use rtsp::header::types::accept::*;
    /// use rtsp::header::value::HeaderValue;
    /// use mime::*;
    /// use std::str::FromStr;
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(Accept::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = vec![
    ///     MediaType::new(mime::STAR_STAR, None),
    ///     MediaType::new(Mime::from_str("video/mp4").unwrap(),
    ///     Some(QualityParam::new(0.5)))
    /// ].into_iter().collect::<Accept>();
    /// let raw_header = vec![HeaderValue::try_from("*/*, video/mp4;q=0.5").unwrap()];
    /// assert_eq!(
    ///     Accept::decode(&mut raw_header.iter()).unwrap(),
    ///     Some(typed_header)
    /// );
    /// ```
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let mut accept = LinkedHashSet::new();
        let mut present = false;
        for value in values {
            let media = value.as_str().split(',');
            for mtype in media {
                let media_type = MediaType::try_from(syntax::trim_whitespace(mtype))?;
                accept.insert(media_type);
            }
            present = true;
        }
        if present {
            Ok(Some(Accept(accept)))
        } else {
            Ok(None)
        }
    }

    /// Converts the [`Accept`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::accept::MediaType;
    /// use rtsp::header::types::Accept;
    /// use rtsp::header::value::HeaderValue;
    /// use rtsp::header::types::accept::QualityParam;
    /// use mime::*;
    /// use std::str::FromStr;
    ///
    /// let typed_header = vec![
    ///     MediaType::new(mime::STAR_STAR, None),
    ///     MediaType::new(Mime::from_str("video/*").unwrap(),
    ///     Some(QualityParam::new(0.5)))
    /// ].into_iter().collect::<Accept>();
    /// let expected_raw_headers = vec![
    ///     vec![HeaderValue::try_from("*/*, video/* ;q=0.5").unwrap()],
    ///     vec![HeaderValue::try_from("video/* ;q=0.5, */*").unwrap()],
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert!(raw_header == expected_raw_headers[0] ||
    ///         raw_header == expected_raw_headers[1]);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        // Unsafe Justification
        //
        // Mime types only allow for US-ASCII which is also valid UTF-8
        let value = self.iter().map(MediaType::to_string).join(", ");
        values.extend(once(unsafe { HeaderValue::from_string_unchecked(value) }));
    }

    fn header_name() -> &'static HeaderName {
        &HeaderName::Accept
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MediaType {
    m_type: Mime,
    quality: Option<QualityParam>,
}

impl MediaType {
    pub fn new(m_type: Mime, quality: Option<QualityParam>) -> Self {
        MediaType { m_type, quality }
    }
}

impl fmt::Display for MediaType {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let m_type = self.m_type.to_string();
        match &self.quality {
            Some(quality) => write!(f, "{} ;{}", m_type, quality),
            None => write!(f, "{}", m_type),
        }
    }
}

impl<'media_type> TryFrom<&'media_type [u8]> for MediaType {
    type Error = AcceptError;

    fn try_from(value: &'media_type [u8]) -> Result<Self, Self::Error> {
        let mut split = value.splitn(2, |&element| element == b';');
        let mediatype = split.next().ok_or(AcceptError::InvalidSyntax)?;
        let raw_utf8 = str::from_utf8(mediatype).map_err(|_| AcceptError::InvalidUtf8)?;
        let mime = raw_utf8.parse().map_err(|_| AcceptError::InvalidSyntax)?;
        let m_type = mime;

        match split.next() {
            Some(quality) => {
                let quality_param =
                    QualityParam::try_from(quality).map_err(|_| AcceptError::InvalidSyntax)?;
                let quality = Some(quality_param);
                return Ok(MediaType { m_type, quality });
            }
            None => Ok(MediaType {m_type, quality: None}),
        }
    }
}

impl<'media_type> TryFrom<&'media_type str> for MediaType {
    type Error = AcceptError;

    fn try_from(value: &'media_type str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct QualityParam(u32);

impl<'quality> TryFrom<&'quality [u8]> for QualityParam {
    type Error = AcceptError;

    fn try_from(value: &'quality [u8]) -> Result<Self, Self::Error> {
        let stringify = str::from_utf8(value).map_err(|_| AcceptError::InvalidUtf8)?;
        let mut val = stringify.rsplitn(2, "=");
        let quality = val.next().ok_or(AcceptError::InvalidSyntax)?;
        let quality_param = quality
            .parse::<f32>()
            .map_err(|_| AcceptError::InvalidSyntax)?;
        return Ok(QualityParam::new(quality_param));
    }
}

impl<'quality> TryFrom<&'quality str> for QualityParam {
    type Error = AcceptError;

    fn try_from(value: &'quality str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

impl QualityParam {
    pub fn new(q_value: f32) -> Self {
        let q_value = q_value.clamp(0.0_f32, 1.0_f32);
        QualityParam(q_value.to_bits())
    }
}

impl fmt::Display for QualityParam {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let q_val: f32 = f32::from_bits(self.0);
        write!(f, "q={}", q_val.to_string())
    }
}
