extern crate bytes;
#[macro_use]
extern crate criterion;
extern crate rtsp;

use bytes::BytesMut;
use criterion::Criterion;
use rtsp::{HeaderName, Request, Response};
use rtsp::protocol::{encode_request, encode_response};

fn encode_benchmark(criterion: &mut Criterion) {
    criterion.bench_function("encode request", move |bencher| {
        let request = Request::builder()
            .method("SETUP")
            .uri("rtsp://example.com/foo/bar/baz.rm")
            .header(HeaderName::CSeq, "302")
            .header(
                HeaderName::Transport,
                "RTP/AVP;multicast;mode=\"PLAY\",\r
        RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
        \"192.0.2.5:3457\";mode=\"PLAY\"",
            )
            .header(HeaderName::AcceptRanges, "npt, smpte, clock")
            .header(HeaderName::UserAgent, "PhonyClient/1.2")
            .build("")
            .unwrap();

        // Make sure that allocating is not part of the benchmark.
        let mut buffer = BytesMut::with_capacity(500);

        bencher.iter(|| {
            buffer.clear();
            encode_request(&request, &mut buffer);
        }); 
    });

    criterion.bench_function("encode response", move |bencher| {
        let response = Response::builder()
            .header(HeaderName::CSeq, "302")
            .header(HeaderName::Date, "Fri, 20 Dec 2013 10:20:32 +0000")
            .header(HeaderName::Session, "rQi1hBrGlFdiYld241FxUO")
            .header(
                HeaderName::Transport,
                "RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
        \"192.0.2.5:3457\";src_addr=\"192.0.2.224:6256\"/\r
        \"192.0.2.224:6257\";mode=\"PLAY\"",
            )
            .header(HeaderName::AcceptRanges, "npt")
            .header(
                HeaderName::MediaProperties,
                "Random-Access=0.6, Dynamic,\r
        Time-Limited=20081128T165900",
            )
            .build("")
            .unwrap();

        // Make sure that allocating is not part of the benchmark.
        let mut buffer = BytesMut::with_capacity(500);

        bencher.iter(|| {
            buffer.clear();
            encode_response(&response, &mut buffer);
        });
    });
}

criterion_group!(benches, encode_benchmark);
criterion_main!(benches);
