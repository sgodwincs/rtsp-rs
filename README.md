# rtsp-rs

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.org/sgodwincs/rtsp-rs.svg?branch=master)](https://travis-ci.org/sgodwincs/rtsp-rs)

An incomplete [RTSP 2.0](https://tools.ietf.org/html/rfc7826) implementation built using tokio.

A lot of the design is based on the [http](https://github.com/hyperium/http) and [hyper](https://github.com/hyperium/hyper) crates, since RTSP and HTTP have a lot of similarities.

Encoders and decoders exist for requests and responses, but server and client implementations do not yet.
