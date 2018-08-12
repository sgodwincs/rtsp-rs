use std::convert::TryFrom;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use uriparse::{
    Authority, Fragment, Host, InvalidAuthority, InvalidFragment, InvalidPath, InvalidQuery,
    InvalidScheme, InvalidURIReference, Password, Path, Query, Scheme, URIReference,
    URIReferenceBuilder, Username, URI,
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RequestURI {
    uri_reference: URIReference<'static>,
}

impl RequestURI {
    pub fn authority(&self) -> Option<&Authority<'static>> {
        self.uri_reference.authority()
    }

    pub fn builder(&self) -> RequestURIBuilder {
        RequestURIBuilder::new()
    }

    pub fn from_parts<
        'new_uri,
        SchemeType,
        AuthorityType,
        PathType,
        QueryType,
        FragmentType,
        SchemeError,
        AuthorityError,
        PathError,
        QueryError,
    >(
        scheme: Option<SchemeType>,
        authority: Option<AuthorityType>,
        path: PathType,
        query: Option<QueryType>,
    ) -> Result<RequestURI, InvalidRequestURI>
    where
        Scheme<'new_uri>: TryFrom<SchemeType, Error = SchemeError>,
        Authority<'new_uri>: TryFrom<AuthorityType, Error = AuthorityError>,
        Path<'new_uri>: TryFrom<PathType, Error = PathError>,
        Query<'new_uri>: TryFrom<QueryType, Error = QueryError>,
        InvalidURIReference:
            From<SchemeError> + From<AuthorityError> + From<PathError> + From<QueryError>,
    {
        let uri_reference =
            URIReference::from_parts(scheme, authority, path, query, None::<Fragment>)
                .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        RequestURI::try_from(uri_reference)
    }

    pub fn has_password(&self) -> bool {
        self.uri_reference.has_password()
    }

    pub fn has_query(&self) -> bool {
        self.uri_reference.has_query()
    }

    pub fn has_username(&self) -> bool {
        self.uri_reference.has_username()
    }

    pub fn host(&self) -> Option<&Host<'static>> {
        self.uri_reference.host()
    }

    pub fn into_builder(self) -> RequestURIBuilder<'static> {
        let (scheme, authority, path, query) = self.into_parts();
        let mut builder = RequestURIBuilder::new();
        builder
            .scheme(scheme)
            .authority(authority)
            .path(path)
            .query(query);
        builder
    }

    pub fn into_parts(
        self,
    ) -> (
        Option<Scheme<'static>>,
        Option<Authority<'static>>,
        Path<'static>,
        Option<Query<'static>>,
    ) {
        let (scheme, authority, path, query, _) = self.uri_reference.into_parts();
        (scheme, authority, path, query)
    }

    pub fn is_asterisk(&self) -> bool {
        self.uri_reference.is_relative_reference()
    }

    pub fn path(&self) -> &Path<'static> {
        self.uri_reference.path()
    }

    pub fn password(&self) -> Option<&Password<'static>> {
        self.uri_reference.password()
    }

    pub fn port(&self) -> Option<u16> {
        self.uri_reference.port()
    }

    pub fn query(&self) -> Option<&Query<'static>> {
        self.uri_reference.query()
    }

    pub fn scheme(&self) -> Option<&Scheme<'static>> {
        self.uri_reference.scheme()
    }

    pub fn username(&self) -> Option<&Username<'static>> {
        self.uri_reference.username()
    }
}

impl Display for RequestURI {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        self.uri_reference.fmt(formatter)
    }
}

impl From<RequestURI> for String {
    fn from(value: RequestURI) -> String {
        value.to_string()
    }
}

impl<'uri> TryFrom<&'uri [u8]> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: &'uri [u8]) -> Result<Self, Self::Error> {
        let uri_reference = URIReference::try_from(value)
            .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        RequestURI::try_from(uri_reference)
    }
}

impl<'uri> TryFrom<&'uri str> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: &'uri str) -> Result<Self, Self::Error> {
        RequestURI::try_from(value.as_bytes())
    }
}

impl<'uri> TryFrom<URI<'uri>> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: URI<'uri>) -> Result<Self, Self::Error> {
        RequestURI::try_from(URIReference::from(value))
    }
}

impl<'uri> TryFrom<URIReference<'uri>> for RequestURI {
    type Error = InvalidRequestURI;

    fn try_from(value: URIReference<'uri>) -> Result<Self, Self::Error> {
        if value.has_scheme() && !value.has_authority() {
            return Err(InvalidRequestURI::MissingAuthority);
        }

        if !value.has_scheme() && value.has_authority() {
            return Err(InvalidRequestURI::MissingScheme);
        }

        if value.is_relative_reference() {
            let segments = value.path().segments();

            if segments.len() != 1 || segments[0].as_str() != "*" {
                return Err(InvalidRequestURI::InvalidRelativeReference);
            }
        } else {
            match value.host().unwrap() {
                Host::RegisteredName(ref name) if name.as_str().is_empty() => {
                    return Err(InvalidRequestURI::EmptyHost)
                }
                _ => (),
            }
        }

        Ok(RequestURI {
            uri_reference: value.into_owned(),
        })
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct RequestURIBuilder<'uri> {
    uri_reference_builder: URIReferenceBuilder<'uri>,
}

impl<'uri> RequestURIBuilder<'uri> {
    pub fn authority<AuthorityType, AuthorityError>(
        &mut self,
        authority: Option<AuthorityType>,
    ) -> &mut Self
    where
        Authority<'uri>: TryFrom<AuthorityType, Error = AuthorityError>,
        InvalidAuthority: From<AuthorityError>,
    {
        self.uri_reference_builder.authority(authority);
        self
    }

    pub fn build(self) -> Result<RequestURI, InvalidRequestURI> {
        let uri_reference = self
            .uri_reference_builder
            .build()
            .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        RequestURI::try_from(uri_reference)
    }

    pub fn fragment<FragmentType, FragmentError>(
        &mut self,
        fragment: Option<FragmentType>,
    ) -> &mut Self
    where
        Fragment<'uri>: TryFrom<FragmentType, Error = FragmentError>,
        InvalidFragment: From<FragmentError>,
    {
        self.uri_reference_builder.fragment(fragment);
        self
    }

    pub fn new() -> Self {
        RequestURIBuilder::default()
    }

    pub fn path<PathType, PathError>(&mut self, path: PathType) -> &mut Self
    where
        Path<'uri>: TryFrom<PathType, Error = PathError>,
        InvalidPath: From<PathError>,
    {
        self.uri_reference_builder.path(path);
        self
    }

    pub fn query<QueryType, QueryError>(&mut self, query: Option<QueryType>) -> &mut Self
    where
        Query<'uri>: TryFrom<QueryType, Error = QueryError>,
        InvalidQuery: From<QueryError>,
    {
        self.uri_reference_builder.query(query);
        self
    }

    pub fn scheme<SchemeType, SchemeError>(&mut self, scheme: Option<SchemeType>) -> &mut Self
    where
        Scheme<'uri>: TryFrom<SchemeType, Error = SchemeError>,
        InvalidScheme: From<SchemeError>,
    {
        self.uri_reference_builder.scheme(scheme);
        self
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum InvalidRequestURI {
    AbsolutePathCannotStartWithTwoSlashes,
    EmptyHost,
    InvalidAuthority(InvalidAuthority),
    InvalidFragment(InvalidFragment),
    InvalidPath(InvalidPath),
    InvalidQuery(InvalidQuery),
    InvalidRelativeReference,
    InvalidScheme(InvalidScheme),
    MissingAuthority,
    MissingPath,
    MissingScheme,
    SchemelessPathCannotStartWithColonSegment,
}

impl Display for InvalidRequestURI {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for InvalidRequestURI {
    fn description(&self) -> &str {
        use self::InvalidRequestURI::*;

        match self {
            AbsolutePathCannotStartWithTwoSlashes => "absolute path cannot start with two slashes",
            EmptyHost => "empty host",
            InvalidAuthority(invalid_authority) => invalid_authority.description(),
            InvalidFragment(invalid_fragment) => invalid_fragment.description(),
            InvalidPath(invalid_path) => invalid_path.description(),
            InvalidQuery(invalid_query) => invalid_query.description(),
            InvalidRelativeReference => "invalid relative reference",
            InvalidScheme(invalid_scheme) => invalid_scheme.description(),
            MissingAuthority => "missing authority",
            MissingPath => "missing path",
            MissingScheme => "missing scheme",
            SchemelessPathCannotStartWithColonSegment => {
                "schemeless path cannot start with colon segment"
            }
        }
    }
}

impl TryFrom<InvalidURIReference> for InvalidRequestURI {
    type Error = ();

    fn try_from(value: InvalidURIReference) -> Result<Self, Self::Error> {
        use self::InvalidRequestURI::*;

        match value {
            InvalidURIReference::AbsolutePathCannotStartWithTwoSlashes => {
                Ok(AbsolutePathCannotStartWithTwoSlashes)
            }
            InvalidURIReference::InvalidAuthority(invalid_authority) => {
                Ok(InvalidAuthority(invalid_authority))
            }
            InvalidURIReference::InvalidFragment(invalid_fragment) => {
                Ok(InvalidFragment(invalid_fragment))
            }
            InvalidURIReference::InvalidPath(invalid_path) => Ok(InvalidPath(invalid_path)),
            InvalidURIReference::InvalidQuery(invalid_query) => Ok(InvalidQuery(invalid_query)),
            InvalidURIReference::InvalidScheme(invalid_scheme) => Ok(InvalidScheme(invalid_scheme)),
            InvalidURIReference::MissingPath => Ok(MissingPath),
            InvalidURIReference::SchemelessPathCannotStartWithColonSegment => {
                Ok(SchemelessPathCannotStartWithColonSegment)
            }
        }
    }
}
