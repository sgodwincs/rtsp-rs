use bytes::BytesMut;

use header::{HeaderName, HeaderValue};
use request::Request;
use response::Response;

/// Encodes `request` into `buffer`. This will set the `Content-Length` header to the size of the
/// body.
pub fn encode_request<B>(request: &mut Request<B>, buffer: &mut BytesMut)
where
    B: AsRef<[u8]>,
{
    let body_size = request.body().as_ref().len();

    if body_size > 0 {
        let body_size = unsafe { HeaderValue::from_str_unchecked(body_size.to_string()) };
        request
            .headers_mut()
            .insert(HeaderName::ContentLength, body_size);
    }

    buffer.extend(request.method().as_str().as_bytes());
    buffer.extend(b" ");
    buffer.extend(request.uri().as_str().as_bytes());
    buffer.extend(b" ");
    buffer.extend(request.version().as_str().as_bytes());
    buffer.extend(b"\r\n");

    for (name, value) in request.headers().iter() {
        buffer.extend(name.canonical_name().as_bytes());
        buffer.extend(b": ");
        buffer.extend(value.as_bytes());
        buffer.extend(b"\r\n");
    }

    buffer.extend(b"\r\n");
    buffer.extend(request.body().as_ref());
}

/// Encodes `response` into `buffer`. This will set the `Content-Length` header to the size of the
/// body.
pub fn encode_response<B>(response: &mut Response<B>, buffer: &mut BytesMut)
where
    B: AsRef<[u8]>,
{
    let body_size = response.body().as_ref().len();

    if body_size > 0 {
        let body_size = unsafe { HeaderValue::from_str_unchecked(body_size.to_string()) };
        response
            .headers_mut()
            .insert(HeaderName::ContentLength, body_size);
    }

    buffer.extend(response.version().as_str().as_bytes());
    buffer.extend(b" ");
    buffer.extend(response.status_code().to_string().as_bytes());
    buffer.extend(b" ");
    buffer.extend(response.reason().as_str().as_bytes());
    buffer.extend(b"\r\n");

    for (name, value) in response.headers().iter() {
        buffer.extend(name.canonical_name().as_bytes());
        buffer.extend(b": ");
        buffer.extend(value.as_bytes());
        buffer.extend(b"\r\n");
    }

    buffer.extend(b"\r\n");
    buffer.extend(response.body().as_ref());
}
