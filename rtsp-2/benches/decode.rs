#[macro_use]
extern crate criterion;
extern crate rtsp;

use criterion::Criterion;
use rtsp::protocol::codec::decoder::request::Decoder as RequestDecoder;
use rtsp::protocol::codec::decoder::response::Decoder as ResponseDecoder;

fn decode_benchmark(criterion: &mut Criterion) {
    criterion.bench_function("decode request", |bencher| {
        let mut decoder = RequestDecoder::new();
        let buffer = "SETUP rtsp://example.com/foo/bar/baz.rm RTSP/2.0\r
CSeq: 302\r
Transport: RTP/AVP;multicast;mode=\"PLAY\",\r
	RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
	\"192.0.2.5:3457\";mode=\"PLAY\"\r
Accept-Ranges: npt, smpte, clock\r
User-Agent: PhonyClient/1.2\r
\r
";

        bencher.iter(|| {
            let (result, bytes_parsed) = decoder.decode(buffer);
            assert!(result.is_complete());
            assert_eq!(bytes_parsed, buffer.len());
        });
    });

    criterion.bench_function("decode response", |bencher| {
        let mut decoder = ResponseDecoder::new();
        let buffer = "RTSP/2.0 200 OK\r
CSeq: 302\r
Date: Fri, 20 Dec 2013 10:20:32 +0000\r
Session: rQi1hBrGlFdiYld241FxUO\r
Transport: RTP/AVP;unicast;dest_addr=\"192.0.2.5:3456\"/\r
	\"192.0.2.5:3457\";src_addr=\"192.0.2.224:6256\"/\r
	\"192.0.2.224:6257\";mode=\"PLAY\"\r
Accept-Ranges: npt\r
Media-Properties: Random-Access=0.6, Dynamic,\r
	Time-Limited=20081128T165900\r
\r
";

        bencher.iter(|| {
            let (result, bytes_parsed) = decoder.decode(buffer);
            assert!(result.is_complete());
            assert_eq!(bytes_parsed, buffer.len());
        });
    });
}

criterion_group!(benches, decode_benchmark);
criterion_main!(benches);
