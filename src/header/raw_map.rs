use ordered_multimap::list_ordered_multimap::{
    Entry as MultimapEntry, EntryValues as MultimapEntryValues,
    EntryValuesDrain as MultimapEntryValuesDrain, EntryValuesMut as MultimapEntryValuesMut,
    Iter as MultimapIter, IterMut as MultimapIterMut, KeyValuesDrain as MultimapKeyValuesDrain,
    KeyValuesEntryDrain as MultimapKeyValuesEntryDrain, KeyValuesMut as MultimapKeyValuesMut,
    OccupiedEntry as MultimapOccupiedEntry, VacantEntry as MultimapVacantEntry,
    Values as MultimapKeyValues, Values as MultimapValues, ValuesMut as MultimapValuesMut,
};
use ordered_multimap::ListOrderedMultimap;

use crate::header::{HeaderName, HeaderValue};

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
pub type RawHeaderMap = ListOrderedMultimap<HeaderName, HeaderValue>;
pub type VacantEntry<'map> = MultimapVacantEntry<'map, HeaderName, HeaderValue>;
pub type Values<'map> = MultimapValues<'map, HeaderName, HeaderValue>;
pub type ValuesMut<'map> = MultimapValuesMut<'map, HeaderName, HeaderValue>;
