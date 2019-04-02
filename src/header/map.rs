use ordered_multimap::list_ordered_multimap::{
    Entry as MultimapEntry, EntryValues as MultimapEntryValues,
    EntryValuesDrain as MultimapEntryValuesDrain, EntryValuesMut as MultimapEntryValuesMut,
    Iter as MultimapIter, IterMut as MultimapIterMut, KeyValuesDrain as MultimapKeyValuesDrain,
    KeyValuesEntryDrain as MultimapKeyValuesEntryDrain, KeyValuesMut as MultimapKeyValuesMut,
    OccupiedEntry as MultimapOccupiedEntry, VacantEntry as MultimapVacantEntry,
    Values as MultimapKeyValues, Values as MultimapValues, ValuesMut as MultimapValuesMut,
};
use ordered_multimap::ListOrderedMultimap;
use std::mem;

use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;

pub type Entry<'map> = MultimapEntry<'map, HeaderName, HeaderValue>;
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

/// A multimap from header names to header values.
///
/// This maintains insertion order.
pub type HeaderMap = ListOrderedMultimap<HeaderName, HeaderValue>;

/// An extension trait for allowing the use of typed headers.
pub trait HeaderMapExtension: private::Sealed {
    /// Inserts the given typed header into the map, removing whatever was there before.
    fn typed_insert<THeader>(&mut self, header: THeader)
    where
        THeader: TypedHeader;

    /// Returns the given typed header, returning [`Option::None`] if it is not in the map or was
    /// invalid.
    fn typed_get<THeader>(&self) -> Option<THeader>
    where
        THeader: TypedHeader;

    /// Returns the given typed header, returning [`Result::Err`] if it could not be decoded
    /// and `Result::Ok(`[`Option::None`]`)` if it was not in the map.
    fn typed_try_get<THeader>(&self) -> Result<Option<THeader>, THeader::DecodeError>
    where
        THeader: TypedHeader;
}

impl HeaderMapExtension for HeaderMap {
    /// Inserts the given typed header into the map, removing whatever was there before.
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

    /// Returns the given typed header, returning [`Option::None`] if it is not in the map or was
    /// invalid.
    fn typed_get<Header>(&self) -> Option<Header>
    where
        Header: TypedHeader,
    {
        self.typed_try_get().unwrap_or(None)
    }

    /// Returns the given typed header, returning [`Result::Err`] if it could not be decoded
    /// and `Result::Ok(`[`Option::None`]`)` if it was not in the map.
    fn typed_try_get<Header>(&self) -> Result<Option<Header>, Header::DecodeError>
    where
        Header: TypedHeader,
    {
        let mut iter = self.get_all(Header::header_name());
        Header::decode(&mut iter)
    }
}

/// A trait representing a typed header.
///
/// A typed header can be encoded into [`HeaderValue`]s or decoded from a collection of
/// [`HeaderValue`]s.
pub trait TypedHeader {
    type DecodeError;

    /// Attempts to decode the given iterator of [`HeaderValue`]s into this typed header.
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Self: Sized,
        Iter: Iterator<Item = &'header HeaderValue>;

    /// Ecodes this typed header by extending the given target with [`HeaderValue`]s.
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>;

    /// Returns the [`HeaderName`] associated with this [`TypedHeader`].
    ///
    /// Ideally, this would be an associated constant, but this fails to work for extension headers
    /// since it is currently not possible to do something like
    /// `HeaderName::try_from("Extension-Header")` within a constant context.
    fn header_name() -> &'static HeaderName
    where
        Self: Sized;
}

/// A convenience type for making it easy to extend the underlying [`HeaderMap`] with encoded typed
/// headers.
struct ExtendWrapper<'map> {
    /// Internal state representing the underlying [`Entry`].
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
                Initial(Entry::Vacant(entry)) => entry.insert_entry(value),
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

/// The state of extending the [`HeaderMap`] with an encoded typed header.
enum ExtendState<'map> {
    /// The initial [`Entry`] in the map, which may or may not be occupied.
    Initial(Entry<'map>),

    /// An [`OccupiedEntry`] meaning at least one value from the typed header has already been added
    /// into this entry.
    Rest(OccupiedEntry<'map>),

    /// A temporary state used as a placeholder until the state can be changed back.
    Temporary,
}

mod private {
    use super::HeaderMap;

    pub trait Sealed {}

    impl Sealed for HeaderMap {}
}
