use bytes::BytesMut;
use std::io;

use header::{Entry, HeaderValue, TypedHeader};
use header::types::ContentLength;

pub mod client;
pub mod server;

pub use self::client::{ClientCodec, InvalidResponse};
pub use self::server::{InvalidRequest, ServerCodec};

/// Consumes a line from the buffer, returning the line found and the index at which the `\r\n`
/// started.
fn consume_line(buffer: &mut BytesMut) -> Option<(BytesMut, usize)> {
    if let Some(i) = buffer.windows(2).position(|b| b == b"\r\n") {
        let line = buffer.split_to(i);
        buffer.split_to(2);
        Some((line, i))
    } else {
        None
    }
}

/// Given the `Entry` for the `Content-Length` header (assuming use of `HeaderMap`), this function
/// attempts to parse the given header and return the content length.
fn get_content_length(header_entry: Entry<HeaderValue>) -> io::Result<ContentLength> {
    match header_entry {
        Entry::Occupied(entry) => if entry.iter().count() > 1 {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "found multiple \"Content-Length\" headers".to_string(),
            ))
        } else {
            ContentLength::try_from_header_raw(&entry.iter().cloned().collect::<Vec<HeaderValue>>())
                .map_err(|_| {
                    io::Error::new(
                        io::ErrorKind::Other,
                        "invalid \"Content-Length\" header".to_string(),
                    )
                })
        },
        Entry::Vacant(_) => Ok(ContentLength::from(0)),
    }
}

/// A helper function to parse a header. This function will return two indices where one is
/// optional. The first index will be the end of the header, while the second index is an
/// `Option<usize>` of where the the header name and value are separated (the index of the `:`).
/// RTSP allows for having multiline headers as long as newlines contained within header values
/// start with a space or tab.
///
/// If the newline found is empty (meaning the header section is over), then the content length
/// of the request will be calculated. An absence of the `Content-Length` header implicitly
/// implies that the content length is 0. If there is more than one content length header or
/// the value of the header cannot be parsed correctly, then the connection will be closed on
/// return of the IO error.
fn parse_header_multiline(buffer: &mut BytesMut) -> Option<io::Result<(usize, Option<usize>)>> {
    let mut iter = buffer
        .windows(2)
        .enumerate()
        .filter(|&(_, b)| b == b"\r\n")
        .map(|(i, _)| i);

    match iter.next() {
        Some(0) => Some(Ok((0, None))),
        Some(mut i) => if let Some(j) = buffer.iter().take(i).position(|&b| b == b':') {
            loop {
                match buffer.get(i + 2) {
                    Some(&b) if b == b' ' || b == b'\t' => match iter.next() {
                        Some(k) => i = k,
                        None => break None,
                    },
                    Some(_) => break Some(Ok((i, Some(j)))),
                    None => break None,
                }
            }
        } else {
            Some(Err(io::Error::new(
                io::ErrorKind::Other,
                "failed to parse header".to_string(),
            )))
        },
        None => None,
    }
}

/// Parses a header of the request/response. The parsed header name will be trimmed on the right for
/// any spaces and tabs, but the header value will remained completely untouched. This is because
/// the syntax of an RTSP header is ambiguous as to whether whitespace is simply whitespace
/// allowed after the colon or if the whitespace is significant to the header value (since
/// header values are allowed to have whitespace as part of their value).
fn parse_header(buffer: &mut BytesMut) -> Option<io::Result<Option<(BytesMut, BytesMut)>>> {
    match parse_header_multiline(buffer) {
        Some(Ok((i, j))) => {
            let mut result = buffer.split_to(i);
            buffer.split_to(2);

            if let Some(j) = j {
                let header_name = result.split_to(j);
                result.split_to(1);
                Some(Ok(Some((header_name, result))))
            } else {
                Some(Ok(None))
            }
        }
        Some(Err(error)) => Some(Err(error)),
        None => None,
    }
}

/// Trims the `&[u8]` of any trailing spaces or tabs. It is possible that the header is actually
/// UTF-8 encoded and this function will create an invalid value, but since header names have to be
/// ASCII-US encoded, it should not cause any problems.
fn trim_header(mut header: &[u8]) -> &[u8] {
    while header.ends_with(&[b' ']) || header.ends_with(&[b'\t']) {
        header = &header[0..header.len() - 1];
    }

    header
}
