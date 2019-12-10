use bytes::BytesMut;

use crate::header::name::HeaderName;
use crate::response::Response;

/// Encodes `response` into `buffer`. This will set the `"Content-Length"` header to the size of the
/// body but will not modify the original response. Even if a content length header was already set,
/// it will be ignored.
pub fn encode<TBody>(response: &Response<TBody>, buffer: &mut BytesMut)
where
    TBody: AsRef<[u8]>,
{
    buffer.extend::<&[u8]>(response.version().as_encoded());
    buffer.extend(b" ");
    buffer.extend(response.status_code().to_string().as_bytes());
    buffer.extend(b" ");
    buffer.extend(response.reason_phrase().as_str().as_bytes());
    buffer.extend(b"\r\n");

    let body_size = response.body().as_ref().len();

    if body_size > 0 {
        buffer.extend(b"Content-Length: ");
        buffer.extend(body_size.to_string().as_bytes());
        buffer.extend(b"\r\n");
    }

    for (name, value) in response.headers().iter() {
        if name == &HeaderName::ContentLength {
            continue;
        }

        buffer.extend(name.canonical_name().as_bytes());
        buffer.extend(b": ");
        buffer.extend::<&[u8]>(value.as_ref());
        buffer.extend(b"\r\n");
    }

    buffer.extend(b"\r\n");
    buffer.extend(response.body().as_ref());
}
