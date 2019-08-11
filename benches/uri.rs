#[macro_use]
extern crate criterion;
extern crate rtsp;

use criterion::Criterion;
use rtsp::uri::request::URI;
use std::convert::TryFrom;

fn uri_benchmark(criterion: &mut Criterion) {
    let uri = "rtsp://user:pass@192.168.1.1:8080/this/is/a/test/path?thisis=aquery";

    criterion.bench_function("parse request URI", move |bencher| {
        bencher.iter(|| URI::try_from(uri).unwrap())
    });
}

criterion_group!(benches, uri_benchmark);
criterion_main!(benches);
