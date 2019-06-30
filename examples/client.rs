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
use rtsp::uri::{Authority,Path,Scheme};
use std::convert::TryFrom;
use std::net::SocketAddr;


fn main() {
    let input: Option<String> = std::env::args().nth(1);
    let  address_str: &str = match input {
        None => "127.0.0.1:10500",
        Some(ref addr) => addr.as_str(),
    };
    let address = address_str.parse::<SocketAddr>().unwrap();
    println!("Initiating connection to: {}", address);

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

            let ip_str = client.server_address
                            .ip().to_string();
            let authority = Authority::try_from(ip_str.as_str()).unwrap();
            let uri = URI::builder()
                .with_scheme(Scheme::RTSP)
                .with_authority(authority)
                .with_path(Path::try_from("").unwrap())
                .build()
                .unwrap();

            let mut builder = Request::builder();
            builder
                .method(Method::Setup)
                .uri(uri)
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
