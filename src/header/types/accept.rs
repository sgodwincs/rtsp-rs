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
    generic-param   =  token [ EQUAL gen-value ]
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
    m_type: MType, //put here an enum of all, all subtypes of a type and any specific combination
    m_sub_type: MSubType,
    generic_value: AcceptParams

} //mtype and msubtype followed by what may be a quality. Then the quality may be followed by a generic param....

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum MType {
    All,
    Major(MSubType),
    Any((MMajorType, MSubType))

}

#[derive(Clone, Debug, PartialEq)]
pub struct AcceptParams{
    q_value: f32,
    generic_param: Option<(Token, Token)>
}

impl Eq for AcceptParams {}

impl Hash for AcceptParams {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unimplemented!();
    }
}

impl AcceptParams {

    pub fn new(q_value: f32, generic_param: Option<(Token, Token)>) -> Self {
        let q_value = q_value.clamp(0.0_f32, 1.0_f32);
        AcceptParams{
            q_value,
            generic_param
        }
    }
}

pub struct MediaTypeParseError(String); 

impl MediaType{

    pub fn to_str(&self) -> &str {
        unimplemented!()
    }
}

impl FromStr for MediaType {
    type Err = MediaTypeParseError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        lazy_static!{
            static ref RE: Regex = Regex::new(r"(?P<mediatype>\{})").unwrap();
        }
        unimplemented!()
    }


}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum MSubType{
    Token(String),
    Xtoken(String)
} //maybe remove this in favour of simple token type, maybe even make this a type alias???

impl MSubType {

    pub fn as_str(&self) -> &str {
        use self::MSubType::*;

        match self {
            Token(token) => &token,
            Xtoken(token) => &token //do we make it such that the xtoken cannot store a value without x- or prepend it ourselves?
        }
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Token {
    Token(String),
    XToken(String)
}


