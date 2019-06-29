# rtsp-rs

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.org/sgodwincs/rtsp-rs.svg?branch=master)](https://travis-ci.org/sgodwincs/rtsp-rs)

An incomplete [RTSP 2.0](https://tools.ietf.org/html/rfc7826) implementation built using tokio.

A lot of the design is based on the [http](https://github.com/hyperium/http) and [hyper](https://github.com/hyperium/hyper) crates, since RTSP and HTTP have a lot of similarities.

Only low-level servers and clients can be made at the moment. For an example of a low-level server
with minimal processing, see [examples/server.rs](https://github.com/sgodwincs/rtsp-rs/blob/master/examples/server.rs).
I am currently considering an interface for a higher-level server abstraction that easily allows defining
media sources.

There is also an example of a client ([examples/client.rs](https://github.com/sgodwincs/rtsp-rs/blob/master/examples/client.rs)). 

# Development setup

```
rustup install nightly
cargo +nightly test
```
all the tests should pass (but there may be some warnings)

# Running the examples

The examples work together, if you first run the server:
```
cargo +nightly run --example server
```

Then in another terminal window run the client:
```
cargo +nightly run --example client
```

The client automatically connects to the server, and so you
should see the server print:
```
Received connection
```

and then the client prints:
```
response: Response { body: b"", reason_phrase: ReasonPhrase("OK"), headers: {CSeq: HeaderValue("292131050"), Date: HeaderValue("Sat, 29 Jun 2019 14:25:13 +0000")}, status_code: 200, version: RTSP/2.0 }
```


