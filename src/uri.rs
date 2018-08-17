use std::convert::TryFrom;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use uriparse::{
    Fragment, InvalidAuthority, InvalidFragment, InvalidPath, InvalidQuery, InvalidScheme,
    InvalidURIReference, Scheme as GenericScheme, URIReference, URIReferenceBuilder, URI,
};

// TODO(https://github.com/rust-lang/rust/issues/52118) Set lifetimes on these types to `'static`
// for convenience and export.
pub use uriparse::{Authority, Host, Password, Path, Query, Username};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RequestURI {
    scheme: Option<Scheme>,
    uri_reference: URIReference<'static>,
}

impl RequestURI {
    pub fn asterisk() -> RequestURI {
        let uri_reference = URIReference::from_parts(
            None::<Scheme>,
            None::<Authority>,
            "*",
            None::<Query>,
            None::<Fragment>,
        );

        RequestURI {
            scheme: None,
            uri_reference,
        }
    }

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
        GenericScheme<'new_uri>: TryFrom<SchemeType, Error = SchemeError>,
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

    pub fn has_authority(&self) -> bool {
        self.uri_reference.has_authority()
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

    pub fn host(&self) -> Option<&Host> {
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
        Option<Scheme>,
        Option<Authority<'static>>,
        Path<'static>,
        Option<Query<'static>>,
    ) {
        let (scheme, authority, path, query, _) = self.uri_reference.into_parts();
        let scheme = scheme.map(|scheme| Scheme::try_from(scheme).unwrap());
        (scheme, authority, path, query)
    }

    pub fn is_asterisk(&self) -> bool {
        self.uri_reference.is_relative_reference()
    }

    pub fn map_authority<Mapper>(&mut self, mapper: Mapper) -> Option<&Authority<'static>>
    where
        Mapper: FnOnce(Authority<'static>) -> Authority<'static>,
    {
        if !self.is_asterisk() {
            self.uri_reference
                .map_authority(|authority| Some(mapper(authority.unwrap())));
        }

        self.authority()
    }

    pub fn map_path<Mapper>(&mut self, mapper: Mapper) -> &Path<'static>
    where
        Mapper: FnOnce(Path<'static>) -> Path<'static>,
    {
        if !self.is_asterisk() {
            self.uri_reference.map_path(mapper);
        }

        self.path()
    }

    pub fn map_query<Mapper>(&mut self, mapper: Mapper) -> Option<&Query<'static>>
    where
        Mapper: FnOnce(Option<Query<'static>>) -> Option<Query<'static>>,
    {
        if !self.is_asterisk() {
            self.uri_reference.map_query(mapper);
        }

        self.query()
    }

    pub fn map_scheme<Mapper>(&mut self, mapper: Mapper) -> Option<Scheme>
    where
        Mapper: FnOnce(Scheme) -> Scheme,
    {
        if !self.is_asterisk() {
            let scheme = mapper(self.scheme.unwrap());
            self.set_scheme(scheme).unwrap();
        }

        self.scheme()
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

    pub fn scheme(&self) -> Option<Scheme> {
        self.scheme
    }

    pub fn set_authority<AuthorityType, AuthorityError>(
        &mut self,
        authority: AuthorityType,
    ) -> Result<Option<&Authority<'static>>, InvalidRequestURI>
    where
        Authority<'static>: TryFrom<AuthorityType, Error = AuthorityError>,
        InvalidURIReference: From<AuthorityError>,
    {
        if !self.is_asterisk() {
            self.uri_reference
                .set_authority(Some(authority))
                .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        }

        Ok(self.authority())
    }

    pub fn set_path<PathType, PathError>(
        &mut self,
        path: PathType,
    ) -> Result<&Path<'static>, InvalidRequestURI>
    where
        Path<'static>: TryFrom<PathType, Error = PathError>,
        InvalidURIReference: From<PathError>,
    {
        if !self.is_asterisk() {
            self.uri_reference
                .set_path(path)
                .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        }

        Ok(self.path())
    }

    pub fn set_query<QueryType, QueryError>(
        &mut self,
        query: Option<QueryType>,
    ) -> Result<Option<&Query<'static>>, InvalidRequestURI>
    where
        Query<'static>: TryFrom<QueryType, Error = QueryError>,
        InvalidURIReference: From<QueryError>,
    {
        if !self.is_asterisk() {
            self.uri_reference
                .set_query(query)
                .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        }

        Ok(self.query())
    }

    pub fn set_scheme<SchemeType, SchemeError>(
        &mut self,
        scheme: SchemeType,
    ) -> Result<Option<Scheme>, InvalidRequestURI>
    where
        Scheme: TryFrom<SchemeType, Error = SchemeError>,
        InvalidURIReference: From<SchemeError>,
    {
        if !self.is_asterisk() {
            self.scheme =
                Some(Scheme::try_from(scheme).map_err(|_| InvalidRequestURI::NonRTSPScheme)?);
        }

        Ok(self.scheme())
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
    fn from(value: RequestURI) -> Self {
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

    fn try_from(mut value: URIReference<'uri>) -> Result<Self, Self::Error> {
        if value.has_scheme() && !value.has_authority() {
            return Err(InvalidRequestURI::MissingAuthority);
        }

        if !value.has_scheme() && value.has_authority() {
            return Err(InvalidRequestURI::MissingScheme);
        }

        let scheme = if value.is_relative_reference() {
            let segments = value.path().segments();

            if segments.len() != 1
                || segments[0].as_str() != "*"
                || value.has_query()
                || value.has_fragment()
            {
                return Err(InvalidRequestURI::InvalidRelativeReference);
            }

            None
        } else {
            let scheme = match Scheme::try_from(value.scheme().unwrap()) {
                Ok(scheme) => scheme,
                Err(_) => return Err(InvalidRequestURI::NonRTSPScheme),
            };

            match value.host().unwrap() {
                Host::RegisteredName(ref name) if name.as_str().is_empty() => {
                    return Err(InvalidRequestURI::EmptyHost)
                }
                _ => (),
            }

            value.set_fragment(None::<Fragment>).unwrap();
            Some(scheme)
        };

        Ok(RequestURI {
            scheme,
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
        authority: AuthorityType,
    ) -> &mut Self
    where
        Authority<'uri>: TryFrom<AuthorityType, Error = AuthorityError>,
        InvalidAuthority: From<AuthorityError>,
    {
        self.uri_reference_builder.authority(Some(authority));
        self
    }

    pub fn build(self) -> Result<RequestURI, InvalidRequestURI> {
        let uri_reference = self
            .uri_reference_builder
            .build()
            .map_err(|error| InvalidRequestURI::try_from(error).unwrap())?;
        RequestURI::try_from(uri_reference)
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

    pub fn scheme<SchemeType, SchemeError>(&mut self, scheme: SchemeType) -> &mut Self
    where
        GenericScheme<'uri>: TryFrom<SchemeType, Error = SchemeError>,
        InvalidScheme: From<SchemeError>,
    {
        self.uri_reference_builder.scheme(Some(scheme));
        self
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Scheme {
    RTSP,
    RTSPS,
    RTSPU,
}

impl From<Scheme> for GenericScheme<'static> {
    fn from(value: Scheme) -> Self {
        use self::Scheme::*;

        match value {
            RTSP => GenericScheme::RTSP,
            RTSPS => GenericScheme::RTSPS,
            RTSPU => GenericScheme::RTSPU,
        }
    }
}

impl<'scheme> TryFrom<GenericScheme<'scheme>> for Scheme {
    type Error = ();

    fn try_from(value: GenericScheme<'scheme>) -> Result<Self, Self::Error> {
        Scheme::try_from(&value)
    }
}

impl<'a, 'scheme> TryFrom<&'a GenericScheme<'scheme>> for Scheme {
    type Error = ();

    fn try_from(value: &'a GenericScheme<'scheme>) -> Result<Self, Self::Error> {
        use self::GenericScheme::*;

        match value {
            RTSP => Ok(Scheme::RTSP),
            RTSPS => Ok(Scheme::RTSPS),
            RTSPU => Ok(Scheme::RTSPU),
            _ => Err(()),
        }
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
    NonRTSPScheme,
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
            NonRTSPScheme => "non-RTSP scheme",
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
