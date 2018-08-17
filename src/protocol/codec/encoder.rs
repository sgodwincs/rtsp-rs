use bytes::BytesMut;

use header::HeaderName;
use request::Request;
use response::Response;

/// Encodes `request` into `buffer`. This will set the `Content-Length` header to the size of the
/// body but will not modify the original request. Even if a content length header was already set,
/// it will be ignored.
pub fn encode_request<B>(request: &Request<B>, buffer: &mut BytesMut)
where
    B: AsRef<[u8]>,
{
    buffer.extend::<&[u8]>(request.method().as_ref());
    buffer.extend(b" ");
    buffer.extend(request.uri().to_string().as_bytes());
    buffer.extend(b" ");
    buffer.extend::<&[u8]>(request.version().as_ref());
    buffer.extend(b"\r\n");

    let body_size = request.body().as_ref().len();

    if body_size > 0 {
        buffer.extend(b"Content-Length: ");
        buffer.extend(body_size.to_string().as_bytes());
        buffer.extend(b"\r\n");
    }

    for (name, value) in request.headers().iter() {
        if name == &HeaderName::ContentLength {
            continue;
        }

        buffer.extend(name.canonical_name().as_bytes());
        buffer.extend(b": ");
        buffer.extend(value.as_bytes());
        buffer.extend(b"\r\n");
    }

    buffer.extend(b"\r\n");
    buffer.extend(request.body().as_ref());
}

/// Encodes `response` into `buffer`. This will set the `Content-Length` header to the size of the
/// body but will not modify the original request. Even if a content length header was already set,
/// it will be ignored.
pub fn encode_response<B>(response: &Response<B>, buffer: &mut BytesMut)
where
    B: AsRef<[u8]>,
{
    buffer.extend::<&[u8]>(response.version().as_ref());
    buffer.extend(b" ");
    buffer.extend(response.status_code().to_string().as_bytes());
    buffer.extend(b" ");
    buffer.extend(response.reason().as_str().as_bytes());
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
        buffer.extend(value.as_bytes());
        buffer.extend(b"\r\n");
    }

    buffer.extend(b"\r\n");
    buffer.extend(response.body().as_ref());
}
