use ordered_multimap::list_ordered_multimap::{
    EntryValues as MultimapEntryValues, EntryValuesDrain as MultimapEntryValuesDrain,
    EntryValuesMut as MultimapEntryValuesMut, Iter as MultimapIter, IterMut as MultimapIterMut,
    KeyValuesDrain as MultimapKeyValuesDrain, KeyValuesEntryDrain as MultimapKeyValuesEntryDrain,
    KeyValuesMut as MultimapKeyValuesMut, OccupiedEntry as MultimapOccupiedEntry,
    VacantEntry as MultimapVacantEntry, Values as MultimapKeyValues, Values as MultimapValues,
    ValuesMut as MultimapValuesMut,
};
use ordered_multimap::ListOrderedMultimap;
use std::error::Error;
use std::fmt;
use std::mem;

use crate::header::{HeaderName, HeaderValue};

// TODO(https://github.com/rust-lang/rust/issues/49683): Export `Entry` as type alias.
pub use ordered_multimap::list_ordered_multimap::Entry;

pub type EntryValues<'map> = MultimapEntryValues<'map, HeaderName, HeaderValue>;
pub type EntryValuesDrain<'map> = MultimapEntryValuesDrain<'map, HeaderName, HeaderValue>;
pub type EntryValuesMut<'map> = MultimapEntryValuesMut<'map, HeaderName, HeaderValue>;
pub type Iter<'map> = MultimapIter<'map, HeaderName, HeaderValue>;
pub type IterMut<'map> = MultimapIterMut<'map, HeaderName, HeaderValue>;
pub type KeyValues<'map> = MultimapKeyValues<'map, HeaderName, HeaderValue>;
pub type KeyValuesDrain<'map> = MultimapKeyValuesDrain<'map, HeaderName, HeaderValue>;
pub type KeyValuesEntryDrain<'map> = MultimapKeyValuesEntryDrain<'map, HeaderName, HeaderValue>;
pub type KeyValuesMut<'map> = MultimapKeyValuesMut<'map, HeaderName, HeaderValue>;
pub type OccupiedEntry<'map> = MultimapOccupiedEntry<'map, HeaderName, HeaderValue>;
pub type VacantEntry<'map> = MultimapVacantEntry<'map, HeaderName, HeaderValue>;
pub type Values<'map> = MultimapValues<'map, HeaderName, HeaderValue>;
pub type ValuesMut<'map> = MultimapValuesMut<'map, HeaderName, HeaderValue>;

pub type HeaderMap = ListOrderedMultimap<HeaderName, HeaderValue>;

pub trait HeaderMapExtension: private::Sealed {
    fn typed_insert<Header>(&mut self, header: Header)
    where
        Header: TypedHeader;
    fn typed_get<Header>(&self) -> Option<Header>
    where
        Header: TypedHeader;
    fn typed_try_get<Header>(&self) -> Result<Option<Header>, Header::DecodeError>
    where
        Header: TypedHeader;
}

impl HeaderMapExtension for HeaderMap {
    fn typed_insert<Header>(&mut self, header: Header)
    where
        Header: TypedHeader,
    {
        let entry = self.entry(Header::header_name().clone());
        let mut extend_wrapper = ExtendWrapper {
            state: ExtendState::Initial(entry),
        };
        header.encode(&mut extend_wrapper);
    }

    fn typed_get<Header>(&self) -> Option<Header>
    where
        Header: TypedHeader,
    {
        self.typed_try_get().unwrap_or(None)
    }

    fn typed_try_get<Header>(&self) -> Result<Option<Header>, Header::DecodeError>
    where
        Header: TypedHeader,
    {
        let mut iter = self.get_all(Header::header_name());
        Header::decode(&mut iter)
    }
}

pub trait TypedHeader {
    type DecodeError;

    /// Returns the `HeaderName` associated with this `TypedHeader`.
    ///
    /// Ideally, this would be an associated constant, but this fails to work for extension headers
    /// since it is currently not possible to do something like
    /// `HeaderName::try_from("Extension-Header")` within a constant context.
    fn header_name() -> &'static HeaderName
    where
        Self: Sized;

    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Self: Sized,
        Iter: Iterator<Item = &'header HeaderValue>;

    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>;
}

struct ExtendWrapper<'map> {
    state: ExtendState<'map>,
}

impl<'map> Extend<HeaderValue> for ExtendWrapper<'map> {
    fn extend<Iter>(&mut self, iter: Iter)
    where
        Iter: IntoIterator<Item = HeaderValue>,
    {
        use self::ExtendState::*;

        for value in iter {
            let entry = match mem::replace(&mut self.state, Temporary) {
                Initial(Entry::Occupied(mut entry)) => {
                    entry.insert(value);
                    entry
                }
                Initial(Entry::Vacant(mut entry)) => entry.insert_entry(value),
                Rest(mut entry) => {
                    entry.append(value);
                    entry
                }
                _ => panic!("extend state should never become `Temporary`"),
            };
            self.state = Rest(entry);
        }
    }
}

enum ExtendState<'map> {
    Initial(Entry<'map, HeaderName, HeaderValue>),
    Rest(OccupiedEntry<'map>),
    Temporary,
}

/// A generic error type indicating that the parsing of the raw header values did not result in a
/// valid typed header for a given header.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct InvalidTypedHeader;

impl fmt::Display for InvalidTypedHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Error::description(self))
    }
}

impl Error for InvalidTypedHeader {
    fn description(&self) -> &str {
        "invalid typed header"
    }
}

mod private {
    use super::HeaderMap;

    pub trait Sealed {}

    impl Sealed for HeaderMap {}
}
