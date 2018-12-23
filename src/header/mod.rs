//! RTSP header types
//!
//! The module provides [`HeaderName`], [`HeaderMap`], and a number of types used for interacting
//! with [`HeaderMap`].
//!
//! # `HeaderName`
//!
//! The `HeaderName` type represents both standard header names as well as custom header names. The
//! type handles the case insensitive nature of header names and is used as the key portion of
//! `HeaderMap`. Header names are normalized to lower case. In other words, when creating a
//! `HeaderName` with a string, even if upper case characters are included, when getting a string
//! representation of the `HeaderName`, it will be all lower case. This allows for faster
//! `HeaderMap` comparison operations.
//!
//! The internal representation is optimized to efficiently handle the cases most commonly
//! encountered when working with RTSP. Standard header names are special cased and are represented
//! internally as an enum.
//!
//! ## Limitations
//!
//! `HeaderName` has a max length of 32,768 for header names. Attempting to parse longer names will
//! result in a panic.
//!
//! # `HeaderMap`
//!
//! `HeaderMap` is a map structure of header names highly optimized for use cases common with RTSP.
//! It is a [multimap] structure, where each header name may have multiple associated header values.
//! Given this, some of the APIs diverge from [`HashMap`].
//!
//! ## Overview
//!
//! Just like `HashMap` in Rust's stdlib, `HeaderMap` is based on [Robin Hood hashing]. This
//! algorithm tends to reduce the worst case search times in the table and enables high load factors
//! without seriously affecting performance. Internally, keys and values are stored in vectors. As
//! such, each insertion will not incur allocation overhead. However, once the underlying vector
//! storage is full, a larger vector must be allocated and all values copied.
//!
//! ## Deterministic ordering
//!
//! Unlike Rust's `HashMap`, values in `HeaderMap` are deterministically ordered. Roughly, values
//! are ordered by insertion. This means that a function that deterministically operates on a header
//! map can rely on the iteration order to remain consistent across processes and platforms.
//!
//! ## Adaptive hashing
//!
//! `HeaderMap` uses an adaptive hashing strategy in order to efficiently handle most common cases.
//! All standard headers have statically computed hash values which removes the need to perform any
//! hashing of these headers at runtime. The default hash function emphasizes performance over
//! robustness. However, `HeaderMap` detects high collision rates and switches to a secure hash
//! function in those events. The threshold is set such that only denial of service attacks should
//! trigger it.
//!
//! ## Limitations
//!
//! `HeaderMap` can store a maximum of 32,768 headers (header name / value pairs). Attempting to
//! insert more will result in a panic.
//!
//! [`HeaderName`]: #
//! [`HeaderMap`]: #
//! [multimap]: https://en.wikipedia.org/wiki/Multimap
//! [`HashMap`]: #
//! [Robin Hood hashing]: https://en.wikipedia.org/wiki/Hash_table#Robin_Hood_hashing

pub mod common;
pub mod map;
pub mod name;
pub mod types;
pub mod value;

pub use self::map::{HeaderMap, HeaderMapExtension, TypedHeader, InvalidTypedHeader};
pub use self::name::{HeaderName, InvalidHeaderName};
pub use self::value::{HeaderValue, InvalidHeaderValue};

/// Maximum length of a header name
///
/// Generally, 64KB for a header name is *way* too much than would ever be needed in practice.
/// Restricting it to this size enables using `u16` values to represent offsets when dealing with
/// header names.
const MAX_HEADER_NAME_LENGTH: usize = 1 << 16;
