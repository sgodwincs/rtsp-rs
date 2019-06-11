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



/*
   The Accept request-header field can be used to specify certain
   presentation description and parameter media types [RFC6838] that are
   acceptable for the response to the DESCRIBE request.

     Accept            =  "Accept" HCOLON
                        [ accept-range *(COMMA accept-range) ]
    accept-range      =  media-type-range [SEMI accept-params]
    accept-params     =  "q" EQUAL qvalue *(SEMI generic-param )
    qvalue            =  ( "0" [ "." *3DIGIT ] )
                      /  ( "1" [ "." *3("0") ] )
    generic-param   =  token [ EQUAL gen-value ] //no example usage so not included
    gen-value       =  token / host / quoted-string
    host              = < As defined in RFC 3986>

    media-type-range  =  ( "*//*"
                            / ( m-type SLASH "*" )
                            / ( m-type SLASH m-subtype )
                                ) *( SEMI m-parameter )
    m-type             =  discrete-type / composite-type
    discrete-type      =  "text" / "image" / "audio" / "video"
                      /  "application" / extension-token
    composite-type   =  "message" / "multipart" / extension-token
    extension-token  =  ietf-token / x-token
    ietf-token       =  token
    x-token          =  "x-" token
    m-subtype        =  extension-token / iana-token
    iana-token       =  token
    m-parameter      =  m-attribute EQUAL m-value
    m-attribute      =  token
    m-value          =  token / quoted-string
    token           =  1*(%x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
                        /  %x41-5A / %x5E-7A / %x7C / %x7E) //any CHAR except CTLs or tspecials
*/
pub struct Accept(LinkedHashSet<MediaType>);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct AcceptError;

impl TypedHeader for Accept {
    type DecodeError = AcceptError;

    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        unimplemented!()
    }

    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>
    {
        unimplemented!()
    }

    fn header_name() -> &'static HeaderName
    where   
        Self: Sized
    {
        unimplemented!()
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
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
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

impl MediaType{

    pub fn new() -> Self {
        MediaType::default()
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
        let mut decoded = MediaType::new();
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

impl FromStr for MediaType {
    type Err = AcceptError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        unimplemented!()
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

impl Eq for AcceptParams {}

impl Hash for AcceptParams {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unimplemented!();
    }
}

impl<'acceptparams> TryFrom<&'acceptparams [u8]> for AcceptParams {
    type Error = AcceptError;

    fn try_from(value: &'acceptparams [u8]) -> Result<Self, Self::Error> {
        unimplemented!()
    }
}

impl AcceptParams {

    pub fn new(q_value: f32, generic_param: Option<(Token, Token)>) -> Self {
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
    Multipart
}

impl<'majortype> TryFrom<&'majortype [u8]> for MMajorType{
    type Error = AcceptError;
    
    fn try_from(value: &'majortype [u8]) -> Result<Self, Self::Error> {
        unimplemented!()
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result{
        write!(f, "{}", self.as_str())
    }
}


