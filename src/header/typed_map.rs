//! RTSP Method
//!
//! This module contains structs to allow usage of typed headers instead of their raw counterparts.
//! The logic in this module is primarily based on the typed header usage defined by the
//! [Hyper](https://github.com/hyperium/hyper) library with some modifications.

use std::any::{Any, TypeId};
use std::cell::{Cell, UnsafeCell};
use std::collections::HashMap;
use std::error::Error;
use std::iter::FromIterator;
use std::{fmt, mem};

use header::{HeaderMap, HeaderName, HeaderValue};

use self::sealed::TypedHeaderClone;

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

/// A trait that represents a typed header for a given `HeaderName` allowing conversion to and from
/// raw header values.
pub trait TypedHeader: 'static + TypedHeaderClone + Send + Sync {
    /// Returns the `HeaderName` associated with this `Header`.
    ///
    /// Ideally, this would be an associated constant, but this fails to work for extension headers
    /// since it is currently not possible to do something like
    /// `HeaderName::try_from("Extension-Header")` within a constant context.
    fn header_name() -> &'static HeaderName
    where
        Self: Sized;

    /// Performs a conversion from the typed header to its raw header values. Since RTSP allows for
    /// multiple header lines with the same names, this function can also return a vector of values
    /// each indicating separate lines.
    fn to_header_raw(&self) -> Vec<HeaderValue>;

    /// Performs a conversion from raw header values t the typed header. Since it is possible that
    /// the raw header values do not represent valid values for the typed header, the conversion may
    /// fail.
    fn try_from_header_raw(header: &[HeaderValue]) -> Result<Self, InvalidTypedHeader>
    where
        Self: Sized;
}

/// Taken from hyper.
impl TypedHeader + Send + Sync {
    /// A trait object looks like this:
    ///
    /// TraitObject { data: *mut (), vtable: *mut () }
    ///
    /// So, we transmute &Trait into a (*mut (), *mut ()). This depends on the order the compiler
    /// has chosen to represent a TraitObject. It has been assured that this order will be stable.
    unsafe fn downcast_ref_unchecked<T: 'static>(&self) -> &T {
        &*(mem::transmute::<*const _, (*const (), *const ())>(self).0 as *const T)
    }

    unsafe fn downcast_mut_unchecked<T: 'static>(&mut self) -> &mut T {
        &mut *(mem::transmute::<*mut _, (*mut (), *mut ())>(self).0 as *mut T)
    }

    unsafe fn downcast_unchecked<T: 'static>(self: Box<Self>) -> T {
        *Box::from_raw(
            mem::transmute::<*mut _, (*mut (), *mut ())>(Box::into_raw(self)).0 as *mut T,
        )
    }
}

/// Due to the restriction that trait objects cannot directly implement the `Clone`, we add another
/// restriction to the `TypedHeader` trait that requires it to implement the `TypedHeaderClone`
/// which allows it to be cloned.
impl Clone for Box<TypedHeader + Send + Sync> {
    fn clone(&self) -> Box<TypedHeader + Send + Sync> {
        self.clone_box()
    }
}

mod sealed {
    use super::TypedHeader;

    pub trait TypedHeaderClone {
        fn clone_box(&self) -> Box<TypedHeader + Send + Sync>;
    }

    impl<T: TypedHeader + Clone> TypedHeaderClone for T {
        fn clone_box(&self) -> Box<TypedHeader + Send + Sync> {
            Box::new(self.clone())
        }
    }
}

/// A map type that maps header names to typed header values.
///
/// # Panics
///
/// Note that if two typed headers exist such that their header names are equal, they cannot both
/// be used in this map at the same time. Doing so will cause a panic.
#[derive(Clone, Eq, PartialEq)]
pub struct TypedHeaderMap(HashMap<HeaderName, TypedHeaderItem>);

impl TypedHeaderMap {
    /// Constructs a new `TypedHeaderMap` with no headers set.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    /// ```
    pub fn new() -> TypedHeaderMap {
        TypedHeaderMap(HashMap::new())
    }

    /// Constructs a new `TypedHeaderMap` with a given capacity.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::with_capacity(5);
    /// map.set(ContentLength::from(20));
    /// ```
    pub fn with_capacity(capacity: usize) -> TypedHeaderMap {
        TypedHeaderMap(HashMap::with_capacity(capacity))
    }

    /// Clears all currently set headers from the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(map.len(), 1);
    ///
    /// map.clear();
    ///
    /// assert_eq!(map.len(), 0);
    /// ```
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Returns the typed header associated with the given generic. Because there may not actually
    /// be a typed header already set, it may have to be parsed from the raw header values. And this
    /// parsing can fail.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(map.get::<ContentLength>().unwrap(), Ok(&ContentLength::from(20)));
    ///
    /// let raw = vec![HeaderValue::try_from("invalid content length").unwrap()];
    /// map.set_raw(HeaderName::ContentLength, raw);
    ///
    /// assert!(map.get::<ContentLength>().unwrap().is_err());
    /// ```
    pub fn get<H: TypedHeader>(&self) -> Option<Result<&H, InvalidTypedHeader>> {
        self.0.get(H::header_name()).map(|item| item.typed())
    }

    /// Similar to `get` but insteads returns a mutable reference to the underlying typed header.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// {
    ///     let header = map.get_mut::<ContentLength>().unwrap().unwrap();
    ///     *header = ContentLength::from(50);
    /// }
    ///
    /// assert_eq!(map.get::<ContentLength>().unwrap(), Ok(&ContentLength::from(50)));
    /// ```
    pub fn get_mut<H: TypedHeader>(&mut self) -> Option<Result<&mut H, InvalidTypedHeader>> {
        self.0
            .get_mut(H::header_name())
            .map(|item| item.typed_mut())
    }

    /// Returns the raw header values of the underlying header. If the underlying header is a typed
    /// header, it will be converted into its raw format and stored separately for future
    /// retrievals.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(
    ///     map.get_raw(&HeaderName::ContentLength).unwrap(),
    ///     &vec![HeaderValue::try_from("20").unwrap()]
    /// );
    /// ```
    pub fn get_raw(&self, name: &HeaderName) -> Option<&Vec<HeaderValue>> {
        self.0.get(name).map(|item| item.raw())
    }

    /// Similar to `get_raw` but instead returns a mutable reference to the underlying raw header
    /// values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// {
    ///     let header = map.get_raw_mut(&HeaderName::ContentLength).unwrap();
    ///     *header = vec![HeaderValue::try_from("50").unwrap()];
    /// }
    ///
    /// assert_eq!(map.get::<ContentLength>().unwrap(), Ok(&ContentLength::from(50)));
    /// ```
    pub fn get_raw_mut(&mut self, name: &HeaderName) -> Option<&mut Vec<HeaderValue>> {
        self.0.get_mut(name).map(|item| item.raw_mut())
    }

    /// Returns whether or not the given typed header is currently set in the map. Note that this
    /// will return `true` even if only the raw header values are set for this header.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert!(map.has::<ContentLength>());
    /// ```
    pub fn has<H: TypedHeader>(&self) -> bool {
        self.0.contains_key(H::header_name())
    }

    /// Returns whether or not the given header name is currently set in the map. Note that this
    /// will return `true` even if only the typed header is set for the header name.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert!(map.has_raw(&HeaderName::ContentLength));
    /// ```
    pub fn has_raw(&self, name: &HeaderName) -> bool {
        self.0.contains_key(name)
    }

    /// Returns an iterator over the currently set headers.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// for view in map.iter() {
    ///     // The following will print `Content-Length`.
    ///     println!("{}", view.name().canonical_name());
    /// }
    /// ```
    pub fn iter(&self) -> TypedHeaderItems {
        TypedHeaderItems {
            inner: self.0.iter(),
        }
    }

    /// Returns the number of headers currently set in the map.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    ///
    /// assert_eq!(map.len(), 0);
    ///
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(map.len(), 1);
    /// ```
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Removes the typed header from this map if it is currently set. If it was set, the typed
    /// header will be returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(map.remove::<ContentLength>().unwrap(), Ok(ContentLength::from(20)));
    /// assert!(!map.has::<ContentLength>())
    /// ```
    pub fn remove<H: TypedHeader>(&mut self) -> Option<Result<H, InvalidTypedHeader>> {
        self.0
            .remove(H::header_name())
            .map(|item| item.into_typed())
    }

    /// Removes the header name from this map if it is currently set. If it was set, the raw header
    /// values will be returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    ///
    /// assert_eq!(
    ///     map.remove_raw(&HeaderName::ContentLength).unwrap(),
    ///     vec![HeaderValue::try_from("20").unwrap()]
    /// );
    /// assert!(!map.has::<ContentLength>())
    /// ```
    pub fn remove_raw(&mut self, name: &HeaderName) -> Option<Vec<HeaderValue>> {
        self.0.remove(name).map(|item| item.into_raw())
    }

    /// Sets the typed header to the given value. This will overwrite any currently set header
    /// values for this header including either the typed header value or the raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// map.set(ContentLength::from(20));
    /// assert!(map.has::<ContentLength>())
    /// ```
    pub fn set<H: TypedHeader>(&mut self, value: H) {
        self.0
            .insert(H::header_name().clone(), TypedHeaderItem::new_typed(value));
    }

    /// Sets the raw header values to the given value. This will overwrite any currently set header
    /// values for this header including either the typed header value or the raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # use std::convert::TryFrom;
    /// #
    /// use rtsp::*;
    /// use rtsp::header::types::ContentLength;
    ///
    /// let mut map = TypedHeaderMap::new();
    /// let raw = vec![HeaderValue::try_from("20").unwrap()];
    /// map.set_raw(HeaderName::ContentLength, raw);
    /// assert!(map.has::<ContentLength>())
    /// ```
    pub fn set_raw(&mut self, name: HeaderName, value: Vec<HeaderValue>) {
        self.0.insert(name, TypedHeaderItem::new_raw(value));
    }
}

impl fmt::Debug for TypedHeaderMap {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map()
            .entries(self.iter().map(|view| (view.0.as_str(), view.1.raw())))
            .finish()
    }
}

impl Default for TypedHeaderMap {
    fn default() -> Self {
        TypedHeaderMap::new()
    }
}

impl From<HeaderMap<HeaderValue>> for TypedHeaderMap {
    fn from(mut value: HeaderMap<HeaderValue>) -> Self {
        let mut map = TypedHeaderMap::with_capacity(value.len());

        for (key, values) in value.drain() {
            let values = values.collect::<Vec<HeaderValue>>();
            map.set_raw(key, values);
        }

        map
    }
}

impl From<TypedHeaderMap> for HeaderMap<HeaderValue> {
    fn from(value: TypedHeaderMap) -> Self {
        let mut map = HeaderMap::with_capacity(value.len());

        for view in value.iter() {
            let key = view.name();

            for raw in view.raw() {
                map.append(key.clone(), raw.clone());
            }
        }

        map
    }
}

/// Immutable iterator over header items.
pub struct TypedHeaderItems<'a> {
    inner: ::std::collections::hash_map::Iter<'a, HeaderName, TypedHeaderItem>,
}

impl<'a> Iterator for TypedHeaderItems<'a> {
    type Item = TypedHeaderView<'a>;

    fn next(&mut self) -> Option<TypedHeaderView<'a>> {
        self.inner
            .next()
            .map(|(name, value)| TypedHeaderView(name, value))
    }
}

pub struct TypedHeaderView<'a>(&'a HeaderName, &'a TypedHeaderItem);

impl<'a> TypedHeaderView<'a> {
    pub fn is<H: TypedHeader>(&self) -> bool {
        H::header_name() == *self.0
    }

    pub fn name(&self) -> &HeaderName {
        self.0
    }

    pub fn raw(&self) -> &'a Vec<HeaderValue> {
        self.1.raw()
    }

    pub fn typed<H: TypedHeader>(&self) -> Result<&'a H, InvalidTypedHeader> {
        self.1.typed::<H>()
    }
}

impl<'a> fmt::Debug for TypedHeaderView<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for raw in self.raw().iter() {
            write!(f, "{}: {}\r\n", self.name().as_str(), raw.as_str())?;
        }

        Ok(())
    }
}
impl<'a> fmt::Display for TypedHeaderView<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl<'a> Extend<TypedHeaderView<'a>> for TypedHeaderMap {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = TypedHeaderView<'a>>,
    {
        for header in iter {
            self.0.insert(header.0.clone(), header.1.clone());
        }
    }
}

impl<'a> FromIterator<TypedHeaderView<'a>> for TypedHeaderMap {
    fn from_iter<I>(iter: I) -> TypedHeaderMap
    where
        I: IntoIterator<Item = TypedHeaderView<'a>>,
    {
        let mut map = TypedHeaderMap::new();
        map.extend(iter);
        map
    }
}

struct TypedHeaderItem {
    raw: UnsafeCell<Option<Vec<HeaderValue>>>,
    type_error: Cell<bool>,
    typed: UnsafeCell<Option<(TypeId, Box<TypedHeader + Send + Sync>)>>,
}

impl TypedHeaderItem {
    pub fn new_raw(value: Vec<HeaderValue>) -> Self {
        TypedHeaderItem {
            raw: UnsafeCell::new(Some(value)),
            type_error: Cell::new(false),
            typed: UnsafeCell::new(None),
        }
    }

    pub fn new_typed<H: TypedHeader>(value: H) -> Self {
        TypedHeaderItem {
            raw: UnsafeCell::new(None),
            type_error: Cell::new(false),
            typed: UnsafeCell::new(Some((TypeId::of::<H>(), Box::new(value)))),
        }
    }

    pub fn into_raw(self) -> Vec<HeaderValue> {
        let TypedHeaderItem { raw, typed, .. } = self;

        if let Some(raw) = raw.into_inner() {
            return raw;
        }

        typed
            .into_inner()
            .expect("`raw` and `typed` cannot both be `None`")
            .1
            .to_header_raw()
    }

    pub fn into_typed<H: TypedHeader>(self) -> Result<H, InvalidTypedHeader> {
        if self.type_error.get() {
            return Err(InvalidTypedHeader);
        }

        let header_type_id = TypeId::of::<H>();
        let TypedHeaderItem { raw, typed, .. } = self;

        match typed.into_inner() {
            Some((type_id, value)) => {
                assert_eq!(header_type_id, type_id);
                Ok(value)
            }
            None => {
                let raw_value = raw.into_inner().unwrap();
                H::try_from_header_raw(&raw_value)
                    .map(|value| -> Box<TypedHeader + Send + Sync> { Box::new(value) })
            }
        }.map(|typed| unsafe { typed.downcast_unchecked() })
    }

    pub fn raw(&self) -> &Vec<HeaderValue> {
        let raw = unsafe { &mut *self.raw.get() };

        if let Some(ref raw) = *raw {
            return raw;
        }

        let raw_value = unsafe { &*self.typed.get() }
            .as_ref()
            .expect("`raw` and `typed` cannot both be `None`")
            .1
            .to_header_raw();
        *raw = Some(raw_value);
        raw.as_ref().unwrap()
    }

    pub fn raw_mut(&mut self) -> &mut Vec<HeaderValue> {
        let raw = unsafe { &mut *self.raw.get() };

        if let Some(ref mut raw) = *raw {
            return raw;
        }

        let raw_value = unsafe { &*self.typed.get() }
            .as_ref()
            .expect("`raw` and `typed` cannot both be `None`")
            .1
            .to_header_raw();
        *raw = Some(raw_value);
        self.typed = UnsafeCell::new(None);
        raw.as_mut().unwrap()
    }

    pub fn typed<H: Any + TypedHeader>(&self) -> Result<&H, InvalidTypedHeader> {
        if self.type_error.get() {
            return Err(InvalidTypedHeader);
        }

        let header_type_id = TypeId::of::<H>();
        let typed = unsafe { &*self.typed.get() };

        match *typed {
            Some((type_id, ref value)) => {
                assert_eq!(header_type_id, type_id);
                Ok(&**value)
            }
            None => {
                let raw_value = unsafe { &*self.raw.get() }.as_ref().unwrap();

                match H::try_from_header_raw(raw_value) {
                    Ok(value) => {
                        let typed = unsafe { &mut *self.typed.get() };
                        *typed = Some((TypeId::of::<H>(), Box::new(value)));
                        Ok(&*typed.as_ref().unwrap().1)
                    }
                    Err(error) => {
                        self.type_error.set(true);
                        return Err(error);
                    }
                }
            }
        }.map(|typed| unsafe { typed.downcast_ref_unchecked() })
    }

    pub fn typed_mut<H: Any + TypedHeader>(&mut self) -> Result<&mut H, InvalidTypedHeader> {
        if self.type_error.get() {
            return Err(InvalidTypedHeader);
        }

        let header_type_id = TypeId::of::<H>();
        let typed = unsafe { &mut *self.typed.get() };

        match *typed {
            Some((type_id, ref mut value)) => {
                assert_eq!(header_type_id, type_id);
                self.raw = UnsafeCell::new(None);
                Ok(&mut **value)
            }
            None => {
                let raw_value = unsafe { &*self.raw.get() }.as_ref().unwrap();

                match H::try_from_header_raw(raw_value) {
                    Ok(value) => {
                        let typed = unsafe { &mut *self.typed.get() };
                        *typed = Some((TypeId::of::<H>(), Box::new(value)));
                        self.raw = UnsafeCell::new(None);
                        Ok(&mut *typed.as_mut().unwrap().1)
                    }
                    Err(error) => {
                        self.type_error.set(true);
                        return Err(error);
                    }
                }
            }
        }.map(|typed| unsafe { typed.downcast_mut_unchecked() })
    }
}

/// Clone has to be implemented without `derive` because `UnsafeCell` does not implement `Clone`
/// even if its container value does.
impl Clone for TypedHeaderItem {
    fn clone(&self) -> Self {
        TypedHeaderItem {
            raw: UnsafeCell::new(unsafe { &*self.raw.get() }.clone()),
            type_error: self.type_error.clone(),
            typed: UnsafeCell::new(unsafe { &*self.typed.get() }.clone()),
        }
    }
}

impl PartialEq for TypedHeaderItem {
    fn eq(&self, other: &TypedHeaderItem) -> bool {
        self.raw() == other.raw()
    }
}

impl Eq for TypedHeaderItem {}

#[cfg(test)]
mod test {
    use std::convert::TryFrom;

    use super::*;

    use header::{HeaderName, HeaderValue};
    use header::types::ContentLength;

    #[test]
    fn test_typed_map_get_mut() {
        let mut map = TypedHeaderMap::new();

        map.set(ContentLength::from(100));

        {
            let header = map.get_mut::<ContentLength>().unwrap().unwrap();
            *header = ContentLength::from(20);
        }

        assert_eq!(
            map.get::<ContentLength>().unwrap(),
            Ok(&ContentLength::from(20))
        );

        map.get_mut::<ContentLength>();

        assert_eq!(
            map.get_raw(&HeaderName::ContentLength).unwrap(),
            &vec![HeaderValue::try_from("20").unwrap()]
        );
    }

    #[test]
    fn test_typed_map_get_raw_mut() {
        let mut map = TypedHeaderMap::new();

        map.set(ContentLength::from(100));

        {
            let header = map.get_raw_mut(&HeaderName::ContentLength).unwrap();
            *header = vec![];
        }

        let header_values: Vec<HeaderValue> = vec![];

        assert_eq!(
            map.get::<ContentLength>().unwrap().unwrap(),
            &ContentLength::from(0)
        );
        assert_eq!(
            map.get_raw(&HeaderName::ContentLength).unwrap(),
            &header_values
        );
    }

    #[test]
    fn test_typed_map_set() {
        let mut map = TypedHeaderMap::new();

        map.set(ContentLength::from(5));

        assert_eq!(
            map.get::<ContentLength>().unwrap(),
            Ok(&ContentLength::from(5))
        );
        assert_eq!(
            map.get_raw(&HeaderName::ContentLength).unwrap(),
            &vec![HeaderValue::try_from("5").unwrap()]
        );
    }

    #[test]
    fn test_typed_map_set_raw() {
        let mut map = TypedHeaderMap::new();

        map.set_raw(
            HeaderName::ContentLength,
            vec![HeaderValue::try_from("1002").unwrap()],
        );

        assert_eq!(
            map.get::<ContentLength>().unwrap(),
            Ok(&ContentLength::from(1002))
        );
        assert_eq!(
            map.get_raw(&HeaderName::ContentLength).unwrap(),
            &vec![HeaderValue::try_from("1002").unwrap()]
        );

        map.set_raw(
            HeaderName::ContentLength,
            vec![HeaderValue::try_from("invalid content length").unwrap()],
        );

        assert!(map.get::<ContentLength>().unwrap().is_err());
        assert_eq!(
            map.get_raw(&HeaderName::ContentLength).unwrap(),
            &vec![HeaderValue::try_from("invalid content length").unwrap()]
        );
    }
}
