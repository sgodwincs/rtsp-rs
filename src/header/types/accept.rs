use crate::header::map::TypedHeader;
use linked_hash_set::LinkedHashSet;
use crate::header::value::HeaderValue;
use crate::header::name::HeaderName;


/*
   The Accept request-header field can be used to specify certain
   presentation description and parameter media types [RFC6838] that are
   acceptable for the response to the DESCRIBE request.
*/
#[derive(Clone, Debug, Default, Eq, PartialEq)]
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
#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub struct MediaType{
    mtype: String,
    quality: i8
}

impl MediaType {

    pub fn new(mtype: String, quality: Option<i8>) -> Self {
        if let Some(q) = quality {
            MediaType{
                mtype,
                quality: q
            }
        }else{
            MediaType{
                mtype,
                quality: 1
            }
        }
    }

    pub fn get_mtype(&self) -> &str {
        &self.mtype
    }

    pub fn get_quality(&self) -> &i8 {
        &self.quality
    }

}

