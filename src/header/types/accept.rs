use std::ops::{Deref, DerefMut};
use std::iter::{once, FromIterator};
use std::convert::TryFrom;
use std::hash::Hasher;
use std::hash::Hash;
use crate::header::map::TypedHeader;
use linked_hash_set::LinkedHashSet;
use crate::header::value::HeaderValue;
use crate::header::name::HeaderName;
use std::str::FromStr;
extern crate lazy_static;
use lazy_static::*;
use regex::Regex;
use crate::syntax;
use syntax::is_token;
use std::fmt::{self, Display, Formatter};
use std::string::ToString;
use itertools::Itertools;




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
///
/// let raw_header: Vec<HeaderValue> = vec![];
/// assert_eq!(Accept::decode(&mut raw_header.iter()).unwrap(), None);
///
/// let typed_header = vec![MediaType::new(MType::All, None), MediaType::new(MType::MajorAny(MMajorType::Video), Some(AcceptParams::new(0.5)))]
///     .into_iter()
///     .collect::<Accept>();
/// let raw_header = vec![HeaderValue::try_from("*/*, Video/* ;q=0.5").unwrap()];
/// assert_eq!(
///     Accept::decode(&mut raw_header.iter()).unwrap(),
///     Some(typed_header)
/// ); 
/// ```
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Accept(LinkedHashSet<MediaType>);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct AcceptError;

impl Deref for Accept {
    type Target = LinkedHashSet<MediaType>;

    fn deref(&self) -> &LinkedHashSet<MediaType> {
        &self.0
    }
}

impl DerefMut for Accept {
    fn deref_mut(&mut self) -> &mut LinkedHashSet<MediaType> {
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

    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let mut accept = LinkedHashSet::new();
        let mut present = false;
        for value in values {
            let media = value.as_str().split(',');
            for mtype in media {
                let t = MediaType::try_from(mtype)?;
                accept.insert(t);
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
    /// use rtsp::header::types::accept::MType;
    /// use rtsp::header::types::accept::MMajorType; 
    /// use rtsp::header::types::Accept;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let typed_header = vec![MediaType{m_type: MType::All, quality: None}, MediaType{m_type: MType::MajorAny(MMajorType::Video), quality: Some(AcceptParams(0.5))}]
    ///     .into_iter()
    ///     .collect::<Accept>();
    /// let expected_raw_headers = vec![
    ///     vec![HeaderValue::try_from("*/*").unwrap()],
    ///     vec![HeaderValue::try_from("Video/* ;q=0.5").unwrap()],
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert!(raw_header == expected_raw_headers[0] ||
    ///         raw_header == expected_raw_headers[1]);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>
    {
        // Unsafe Justification
        //
        // Header values must be valid UTF-8, and since we know that the [`RangeFormat`] type
        // guarantees valid ASCII-US (with no newlines), it satisfies the constraints.
        let value = self.iter().map(MediaType::as_str).join(", ");
        values.extend(once(unsafe { HeaderValue::from_string_unchecked(value) }));    }

    fn header_name() -> &'static HeaderName
    where   
        Self: Sized
    {
        &HeaderName::Accept
    }
}

/*
    Each media type or range MAY be followed by one or more accept-
    params, beginning with the "q" parameter to indicate a relative
    quality factor.  The first "q" parameter (if any) separates the media
    type or range's parameters from the accept-params.  Quality factors
    allow the user or user agent to indicate the relative degree of
    preference for that media type, using the qvalue scale from 0 to 1
    (Section 5.3.1 of [RFC7231]).  The default value is q=1.
*/
// #[derive(Clone, Hash, Debug, Eq, PartialEq)]
// pub enum MediaType{
//     Typed(String),
//     Typed_Quality((String, i8)) //dumb refactor this
// }
#[derive(Clone, Debug, PartialEq)]
pub struct MediaType{
    m_type: MType,
    quality: Option<AcceptParams>

} //mtype and msubtype followed by what may be a quality. Then the quality may be followed by a generic param....

impl Default for MediaType {

    fn default() -> Self {
        MediaType{
            m_type: MType::All,
            quality: None
        }
    }
}

impl Eq for MediaType {}

impl Hash for MediaType {

    fn hash<H: Hasher>(&self, state: &mut H) {
        self.m_type.hash(state);
    }
}

impl MediaType{

    pub fn new(m_type: MType, quality: Option<AcceptParams>) -> Self {
        MediaType{
            m_type,
            quality
        }
    }

    pub fn as_str(&self) -> String {
        let m_type = self.m_type.as_str();
        match &self.quality {
            Some(q) => format!("{} ;{}", m_type, q.as_str()),
            None => format!("{}", m_type)
        }
    }
}

impl<'accept> TryFrom<&'accept [u8]> for MediaType {
    type Error = AcceptError;

    fn try_from(value: &'accept [u8]) -> Result<Self, Self::Error> {
        let mut split = value.split(|element| element.clone() == ';' as u8);
        let mut decoded = MediaType::default();
        if let Some(mediatype) = split.next() {
            let mtype = MType::try_from(mediatype).unwrap();
            decoded.m_type = mtype;
            if let Some(quality) = split.next() { 
                let q = AcceptParams::try_from(quality).unwrap();
                decoded.quality = Some(q);
            }
        }
        Ok(decoded)
    }
}

impl<'accept> TryFrom<&'accept str> for MediaType {
    type Error = AcceptError;

    fn try_from(value: &'accept str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    } 
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MType {
    All,
    MajorAny(MMajorType),
    MajorMinor((MMajorType, MSubType))
}

impl<'mediatype> TryFrom<&'mediatype [u8]> for MType {
    type Error = AcceptError;

    fn try_from(value: &'mediatype [u8]) -> Result<Self, Self::Error> {
        let splitchar = '/' as u8;
        let all = '*' as u8;
        let major_sub = value.split(|element| element.clone() == splitchar).collect::<Vec<&[u8]>>();
        match major_sub.len() {
            2 => {
                if major_sub[0][0] == all {
                    if major_sub[1][0] == all {
                        return Ok(MType::All)
                    } else {
                        return Err(AcceptError)
                    }
                } else {
                    if major_sub[1][0] == all {
                        Ok(MType::MajorAny(MMajorType::try_from(major_sub[0]).unwrap()))
                    } else {
                        Ok(MType::MajorMinor((MMajorType::try_from(major_sub[0]).unwrap(), MSubType::try_from(major_sub[1]).unwrap())))
                    }
                }
            },
            _ => Err(AcceptError)
        } 
    }
    
}

impl<'mediatype> TryFrom<&'mediatype str> for MType {
    type Error = AcceptError;

    fn try_from(value: &'mediatype str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

impl MType {
    pub fn as_str(&self) -> String {
        use self::MType::*;

        match self {
            All => String::from("*/*"),
            MajorAny(majortype) => format!("{}/*", majortype.as_str()),
            MajorMinor((majortype, subtype)) => format!("{}/{}", majortype.as_str(), subtype.as_str())
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AcceptParams(f32);

impl<'acceptparams> TryFrom<&'acceptparams [u8]> for AcceptParams {
    type Error = AcceptError;

    fn try_from(value: &'acceptparams [u8]) -> Result<Self, Self::Error> {
        unimplemented!()
    }
}

impl<'acceptparams> TryFrom<&'acceptparams str> for AcceptParams {
    type Error = AcceptError;

    fn try_from(value: &'acceptparams str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

impl AcceptParams {

    pub fn new(q_value: f32) -> Self {
        let q_value = q_value.clamp(0.0_f32, 1.0_f32);
        AcceptParams(q_value)
    }

    pub fn as_str(&self) -> String {
        let q_val = self.0.to_string();
        format!("q={}", q_val)
    }
}

pub struct MediaTypeParseError(String); 

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MSubType {
    SubType(Token)
}

impl MSubType {
    pub fn as_str(&self) -> &str {
        use self::MSubType::*;

        match self {
            All => "*",
            SubType(token) => token.as_str()
        }
    }
}

impl<'subtype> TryFrom<&'subtype [u8]> for MSubType {
    type Error = AcceptError;

    fn try_from(value: &'subtype [u8]) -> Result<Self, Self::Error> {
        let all_char = '*' as u8;
        if value[0] == all_char {
            return Err(AcceptError)
        }
        Ok(MSubType::SubType(Token::try_from(value).unwrap()))
    }
}

impl<'subtype> TryFrom<&'subtype str> for MSubType {
    type Error = AcceptError;

    fn try_from(value: &'subtype str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

impl Display for MSubType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MMajorType{
    Video,
    Text,
    Image,
    Application,
    Message,
    Multipart,
    Extension(Token)
}

impl<'majortype> TryFrom<&'majortype [u8]> for MMajorType{
    type Error = AcceptError;
    
    fn try_from(value: &'majortype [u8]) -> Result<Self, Self::Error> {
        if is_token(value) {
            let mtype = unsafe {std::str::from_utf8_unchecked(value)}.to_ascii_lowercase();
            let mtype_str = mtype.as_str();
            match mtype_str {
                "video" => Ok(Self::Video),
                "text" => Ok(Self::Text),
                "image" => Ok(Self::Image),
                "application" => Ok(Self::Application),
                "message" => Ok(Self::Message),
                "multipart" => Ok(Self::Multipart),
                _ => Ok(Self::Extension(Token::try_from(mtype_str).unwrap()))
            }
        } else {
            Err(AcceptError)
        }
    }
}

impl MMajorType {

    pub fn as_str(&self) -> &str {
        use self::MMajorType::*;

        match self {
            Video => "Video",
            Text => "Text",
            Image => "Image",
            Application => "Application",
            Message => "Message",
            Multipart => "Multipart",
            Extension(token) => token.as_str()
        }
    }

}

impl Display for MMajorType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }

}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    Token(String) ,
    XToken(String)
}

impl Token {

    pub fn as_str(&self) -> &str {
        use self::Token::*;

        match self {
            Token(token) => &token,
            XToken(token) => &token
        }
    }
}

impl<'token> TryFrom<&'token [u8]> for Token {
    type Error = AcceptError;

    fn try_from(value: &'token [u8]) -> Result<Self, Self::Error> {
        let x = 'x' as u8;
        if is_token(value) && value[0] == x {
            let decoded = unsafe { std::str::from_utf8_unchecked(value) }.to_ascii_lowercase();
            Ok(Token::XToken(decoded))
        } else if is_token(value) {
            let decoded = unsafe { std::str::from_utf8_unchecked(value) }.to_ascii_lowercase();
            Ok(Token::Token(decoded))            
        } else{
            Err(AcceptError)
        } 
    }
}

impl<'token> TryFrom<&'token str> for Token {
    type Error = AcceptError;

    fn try_from(value: &'token str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        Self::try_from(bytes)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result{
        write!(f, "{}", self.as_str())
    }
}


