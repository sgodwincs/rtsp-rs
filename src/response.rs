use header::{HeaderMap, HeaderValue};
use status::StatusCode;
use version::Version;

/// Represents an RTSP response.
///
/// An RTSP response consists of a header and a, potentially empty, body. The body component is
/// generic, enabling arbitrary types to represent the RTSP body.
#[derive(Clone, Eq, PartialEq)]
pub struct Response<T> {
    /// The body component of the response. This is generic to support arbitrary content types.
    body: T,

    /// Specifies a custom reason phrase for the given status code. RTSP allows agents to give
    /// custom reason phrases and even recommends it in specific cases. If it is detected that the
    /// status code is an extension or that the reason phrase is not the canonical reason phrase for
    /// the given status code, then this will be the custom reason phrase.
    custom_reason_phrase: Option<String>,

    /// A multimap of header names to values that maintains insertion order.
    headers: HeaderMap<HeaderValue>,

    /// The status code of the response.
    status: StatusCode,
}
