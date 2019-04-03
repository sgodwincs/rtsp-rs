extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;

use bytes::BytesMut;
use futures::{future, Async, Future, Poll, Stream};
use rtsp::protocol::connection::Connection;
use rtsp::request::Request;
use rtsp::response::Response;
use std::io;
use std::net::SocketAddr;
use tokio::net::TcpListener;
use tower_service::Service;

fn main() {
    let address = "0.0.0.0:10500".parse::<SocketAddr>().unwrap();
    let listener = TcpListener::bind(&address).unwrap();

    let server = listener.incoming().for_each(move |socket| {
        let (connection, handler, mut handle) = Connection::new(socket, Some(Application));
        println!("Received connection");

        // This will handle all reading and writing for the connection.

        tokio::spawn(connection);

        // This is a separate task that processes requests.

        tokio::spawn(handler.unwrap());

        // Only shutdown when the client has finished. The shutdown will be fired when both the
        // connection and handler spawned above have finished.

        tokio::spawn(handle.shutdown_receiver().then(|_| {
            std::mem::drop(handle);
            Ok(())
        }));

        Ok(())
    });

    tokio::run(server.map_err(|_| ()));
}

struct Application;

impl Service<Request<BytesMut>> for Application {
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _request: Request<BytesMut>) -> Self::Future {
        let mut builder = Response::builder();
        builder.body("".into());
        let response = builder.build().unwrap();
        Box::new(future::ok(response))
    }

    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        Ok(Async::Ready(()))
    }
}
