extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;

use bytes::BytesMut;
use futures::Future;
use rtsp::{Client, Method, Request};
use std::net::SocketAddr;

fn main() {
    let address = "127.0.0.1:10500".parse::<SocketAddr>().unwrap();

    // Connect to the server. Currently, any requests sent by the server will be ignored by the
    // client. An API to support handling these requests will be added soonish.

    let client = Client::connect(address)
        .or_else(|error| {
            println!("error connecting to server: {}", error);
            Err(())
        })
        .and_then(|mut client| {
            let request = Request::builder()
                .method(Method::Setup)
                .uri("rtsp://127.0.0.1/")
                .build(BytesMut::new())
                .unwrap();
            client.send_request(request).then(|result| {
                match result {
                    Ok(response) => println!("response: {:?}", response),
                    Err(error) => println!("error sending request: {}", error),
                }

                Ok(())
            })
        });

    tokio::run(client);
}
