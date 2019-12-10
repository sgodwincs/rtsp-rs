extern crate bytes;
#[macro_use]
extern crate criterion;
extern crate rtsp;

use bytes::BytesMut;
use criterion::Criterion;
use rtsp::header::name::HeaderName;
use rtsp::header::value::HeaderValue;
use rtsp::method::Method;
use rtsp::protocol::codec::encoder::request;
use rtsp::protocol::codec::encoder::response;
use rtsp::request::Request;
use rtsp::response::Response;
use rtsp::uri::request::URI;
use std::convert::TryFrom;

fn encode_benchmark(criterion: &mut Criterion) {
    criterion.bench_function("encode request", |bencher| {
        let request = Request::<()>::builder()
            .with_method(Method::Setup)
            .with_uri(URI::try_from("rtsp://example.com/foo/bar/baz.rm").unwrap())
            .with_header(HeaderName::CSeq, HeaderValue::try_from("302").unwrap())
            .with_header(
                HeaderName::Transport,
                HeaderValue::try_from(
                    "RTP/AVP;multicast;mode=\"PLAY\",\r
	RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
	\"192.0.2.5:3457\";mode=\"PLAY\"",
                )
                .unwrap(),
            )
            .with_header(
                HeaderName::AcceptRanges,
                HeaderValue::try_from("npt, smpte, clock").unwrap(),
            )
            .with_header(
                HeaderName::UserAgent,
                HeaderValue::try_from("PhonyClient/1.2").unwrap(),
            )
            .with_body("")
            .build()
            .unwrap();

        // Make sure that allocating is not part of the benchmark.
        let mut buffer = BytesMut::with_capacity(500);

        bencher.iter(|| {
            buffer.clear();
            request::encode(&request, &mut buffer);
        });
    });

    criterion.bench_function("encode response", |bencher| {
        let response = Response::<()>::builder()
            .with_header(HeaderName::CSeq, HeaderValue::try_from("302").unwrap())
            .with_header(
                HeaderName::Date,
                HeaderValue::try_from("Fri, 20 Dec 2013 10:20:32 +0000").unwrap(),
            )
            .with_header(
                HeaderName::Session,
                HeaderValue::try_from("rQi1hBrGlFdiYld241FxUO").unwrap(),
            )
            .with_header(
                HeaderName::Transport,
                HeaderValue::try_from(
                    "RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
	\"192.0.2.5:3457\";src_addr=\"192.0.2.224:6256\"/\r
	\"192.0.2.224:6257\";mode=\"PLAY\"",
                )
                .unwrap(),
            )
            .with_header(
                HeaderName::AcceptRanges,
                HeaderValue::try_from("npt").unwrap(),
            )
            .with_header(
                HeaderName::MediaProperties,
                HeaderValue::try_from(
                    "Random-Access=0.6, Dynamic,\r
	Time-Limited=20081128T165900",
                )
                .unwrap(),
            )
            .with_body("")
            .build()
            .unwrap();

        // Make sure that allocating is not part of the benchmark.
        let mut buffer = BytesMut::with_capacity(500);

        bencher.iter(|| {
            buffer.clear();
            response::encode(&response, &mut buffer);
        });
    });
}

criterion_group!(benches, encode_benchmark);
criterion_main!(benches);
