extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;

use bytes::BytesMut;
use futures::Future;
use rtsp::client::Client;
use rtsp::method::Method;
use rtsp::request::Request;
use rtsp::uri::request::URI;
use std::convert::TryFrom;
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
            let addr = client.server_address();
            println!("Connected to server: {}", addr);

            let mut builder = Request::builder();
            builder
                .method(Method::Setup)
                .uri(URI::try_from("rtsp://127.0.0.1/").unwrap())
                .body(BytesMut::new());
            let request = builder.build().unwrap();

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
