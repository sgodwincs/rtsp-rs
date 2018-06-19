extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;
extern crate tokio_timer;

use bytes::BytesMut;
use futures::{future, Future, Stream};
use rtsp::protocol::Connection;
use rtsp::{Request, Response, Service};
use std::io;
use std::net::SocketAddr;
use tokio::net::TcpListener;

fn main() {
    let address = "0.0.0.0:10500".parse::<SocketAddr>().unwrap();
    let listener = TcpListener::bind(&address).unwrap();

    let server = listener.incoming().for_each(move |socket| {
        let (connection, handler, mut handle) = Connection::new(socket, Some(Application));

        // This will handle all reading and writing for the connection.

        tokio::spawn(connection);

        // This is a separate task that processes requests.

        tokio::spawn(handler.unwrap());

        // Only shutdown when the client has finished. The shutdown will be fired when both the
        // connection and handler spawned above have finished.

        tokio::spawn(handle.take_shutdown().unwrap().then(|_| {
            std::mem::drop(handle);
            Ok(())
        }));

        Ok(())
    });

    tokio::run(server.map_err(|_| ()));
}

struct Application;

impl Service for Application {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, _request: Self::Request) -> Self::Future {
        let response = Response::builder().build("".into()).unwrap();
        Box::new(future::ok(response))
    }
}
