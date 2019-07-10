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
use rtsp::uri::{Host, RTSP_DEFAULT_PORT};
use std::convert::TryFrom;
use std::net::IpAddr;
use std::net::SocketAddr;

fn main() {
    let uri_string = std::env::args()
        .nth(1)
        .unwrap_or(String::from("rtsp://127.0.0.1:10500"));

    let uri = URI::try_from(uri_string.as_str()).expect("Invalid URI");

    let host = uri.host().unwrap();
    let addr: IpAddr = match host {
        Host::IPv4Address(ip) => IpAddr::V4(*ip),
        Host::IPv6Address(ip) => IpAddr::V6(*ip),
        _ => {
            eprintln!(
                "Please provide ip address. Hostname not supported: {}",
                host
            );
            std::process::exit(1);
        }
    };

    let address = SocketAddr::new(addr, uri.port().unwrap_or(RTSP_DEFAULT_PORT));

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

            let mut builder = Request::builder();
            builder.method(Method::Setup).uri(uri).body(BytesMut::new());
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
