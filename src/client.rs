use futures::{Future, IntoFuture};
use futures::sync::oneshot;
use std::io;
use std::net::SocketAddr;
use std::rc::Rc;
use tokio::executor::current_thread;
use tokio::net::TcpStream;
use tokio_service::Service;

use request::Request;
use response::Response;
use proto;
use proto::Protocol;
use proto::codec::{RequestResult, ResponseResult};

pub struct Client {
    protocol: Rc<Protocol>,
    shutdown_oneshot: Option<oneshot::Sender<()>>,
}

impl Client {
    pub fn connect<S, B>(
        address: SocketAddr,
        service: S,
    ) -> impl Future<Item = Client, Error = io::Error>
    where
        S: Service<Request = RequestResult, Response = Response<B>, Error = ()> + 'static,
        <S as Service>::Future: 'static,
        B: AsRef<[u8]>,
    {
        TcpStream::connect(&address).and_then(|tcp_stream| {
            let (tx_shutdown, rx_shutdown) = oneshot::channel();
            let mut config = proto::Config::new();
            config.set_shutdown_future(rx_shutdown.map_err(|_| ()));
            let protocol = Rc::new(Protocol::with_config(tcp_stream, config));
            let protocol_handle = protocol.clone();

            current_thread::spawn(protocol.for_each_request(move |request| {
                let protocol_handle = protocol_handle.clone();

                service.call(request).and_then(move |response| {
                    protocol_handle
                        .send_response(response)
                        .into_future()
                        .map_err(|_| ())
                })
            }));

            Ok(Client {
                protocol,
                shutdown_oneshot: Some(tx_shutdown),
            })
        })
    }

    pub fn send_request<B>(
        &self,
        request: Request<B>,
    ) -> impl Future<Item = ResponseResult, Error = Rc<io::Error>>
    where
        B: AsRef<[u8]>,
    {
        self.protocol.send_request(request)
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        // This actually is not necessarily since if the sender is dropped, it will send a cancelled
        // error which will have the same result. But the compiler will complain about the sender
        // not being used, so this deals with that.

        self.shutdown_oneshot.take().unwrap().send(()).ok();
    }
}
