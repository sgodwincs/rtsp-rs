use bytes::BytesMut;
use std::io;
use std::mem::replace;
use tokio_io::codec::{Decoder, Encoder};

use header::{Entry, Header, HeaderName, HeaderValue};
use header::types::ContentLength;
use request::{Builder, BuilderError, Request};

pub struct RequestCodec {
    body: Option<BytesMut>,
    builder: Builder,
    content_length: ContentLength,
    state: ParseState,
}

#[derive(Eq, PartialEq)]
pub enum ParseState {
    Body,
    End,
    Header,
    RequestLine,
}

fn consume_line(buffer: &mut BytesMut) -> Option<(BytesMut, usize)> {
    if let Some(i) = buffer.windows(2).position(|b| b == b"\r\n") {
        let line = buffer.split_to(i);
        buffer.split_to(2);
        Some((line, i))
    } else {
        None
    }
}

fn trim_header(mut header: &[u8]) -> &[u8] {
    while header.ends_with(&[b' ']) || header.ends_with(&[b'\t']) {
        header = &header[0..header.len() - 1];
    }

    header
}

impl RequestCodec {
    fn parse_body(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        if *self.content_length > buffer.len() {
            None
        } else {
            self.state = ParseState::End;
            self.body = Some(buffer.split_to(*self.content_length));
            Some(Ok(()))
        }
    }

    fn parse_header_multiline(
        &mut self,
        buffer: &mut BytesMut,
    ) -> Option<io::Result<(usize, Option<usize>)>> {
        let mut iter = buffer
            .windows(2)
            .enumerate()
            .filter(|&(_, b)| b == b"\r\n")
            .map(|(i, _)| i);

        match iter.next() {
            Some(0) => {
                self.state = ParseState::Body;

                self.content_length = match self.builder
                    .headers
                    .entry(HeaderName::ContentLength)
                    .unwrap()
                {
                    Entry::Occupied(entry) => if entry.iter().count() > 1 {
                        return Some(Err(io::Error::new(
                            io::ErrorKind::Other,
                            "found multiple \"Content-Length\" headers".to_string(),
                        )));
                    } else {
                        match ContentLength::from_header_raw(
                            entry.iter().collect::<Vec<&HeaderValue>>().as_slice(),
                        ) {
                            Ok(content_length) => content_length,
                            Err(_) => {
                                return Some(Err(io::Error::new(
                                    io::ErrorKind::Other,
                                    "invalid \"Content-Length\" header".to_string(),
                                )))
                            }
                        }
                    },
                    Entry::Vacant(_) => ContentLength::from(0),
                };

                Some(Ok((0, None)))
            }
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
                    "failed to parse request header".to_string(),
                )))
            },
            None => None,
        }
    }

    fn parse_header(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match self.parse_header_multiline(buffer) {
            Some(Ok((i, j))) => {
                let mut result = buffer.split_to(i);
                buffer.split_to(2);

                if let Some(j) = j {
                    let header_name = result.split_to(j);
                    let header_name = trim_header(header_name.as_ref());
                    result.split_to(1);

                    self.builder.header(header_name, result.as_ref());
                }

                Some(Ok(()))
            }
            Some(Err(error)) => Some(Err(error)),
            None => None,
        }
    }

    fn parse_request_line(&mut self, buffer: &mut BytesMut) -> Option<io::Result<()>> {
        match consume_line(buffer) {
            Some((_, 0)) => {
                buffer.split_to(2);
                None
            }
            Some((mut line, _)) => {
                if let Some(i) = line.iter().position(|&b| b == b' ') {
                    let method = line.split_to(i);
                    line.split_to(1);

                    if let Some(i) = line.iter().position(|&b| b == b' ') {
                        let uri = line.split_to(i);
                        line.split_to(1);

                        self.state = ParseState::Header;
                        self.builder
                            .method(method.as_ref())
                            .uri(uri.as_ref())
                            .version(line.as_ref());

                        return Some(Ok(()));
                    }
                }

                Some(Err(io::Error::new(
                    io::ErrorKind::Other,
                    "failed to parse request line".to_string(),
                )))
            }
            None => None,
        }
    }
}

impl Decoder for RequestCodec {
    type Item = Result<Request<BytesMut>, BuilderError>;
    type Error = io::Error;

    fn decode(&mut self, buffer: &mut BytesMut) -> io::Result<Option<Self::Item>> {
        use self::ParseState::*;

        loop {
            let parse_result = match self.state {
                Body => self.parse_body(buffer),
                Header => self.parse_header(buffer),
                RequestLine => self.parse_request_line(buffer),
                End => {
                    let request = self.builder.build(replace(&mut self.body, None).unwrap());
                    self.state = RequestLine;

                    break Ok(Some(request));
                }
            };

            match parse_result {
                None => break Ok(None),
                Some(Err(error)) => break Err(error),
                _ => continue,
            }
        }
    }
}

impl Encoder for RequestCodec {
    type Item = ();
    type Error = io::Error;

    fn encode(&mut self, message: Self::Item, buffer: &mut BytesMut) -> io::Result<()> {
        Ok(())
    }
}

impl Default for RequestCodec {
    fn default() -> Self {
        RequestCodec {
            body: None,
            builder: Builder::new(),
            content_length: ContentLength::from(0),
            state: ParseState::RequestLine,
        }
    }
}
