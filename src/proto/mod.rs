pub mod client;
pub mod server;

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

/// Trims the `&[u8]` of any trailing spaces or tabs. It is possible that the header is actually
/// UTF-8 encoded and this function will create an invalid value, but since header names have to be
/// ASCII-US encoded, it should not cause any problems.
fn trim_header(mut header: &[u8]) -> &[u8] {
    while header.ends_with(&[b' ']) || header.ends_with(&[b'\t']) {
        header = &header[0..header.len() - 1];
    }

    header
}
