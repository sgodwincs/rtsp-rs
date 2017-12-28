# rtsp-rs

[![LICENSE](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.org/sgodwincs/rtsp-rs.svg?branch=master)](https://travis-ci.org/sgodwincs/rtsp-rs)

An incomplete [RTSP 2.0](https://tools.ietf.org/html/rfc7826) implementation built using tokio.

A lot of the implementation is based on the [http](https://github.com/hyperium/http) and [hyper](https://github.com/hyperium/hyper) crate, since RTSP and HTTP have a lot of similarities.

Server and client codecs are completed, but an API to use them ergonomically does not yet exist. Wrapper types and tokio services still need to be made to make it easy to use both of them.
