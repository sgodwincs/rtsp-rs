use futures::Future;
use std::io;
use std::net::SocketAddr;
use std::rc::Rc;
use tokio::net::TcpStream;

use request::Request;
use proto::Protocol;
use proto::codec::ResponseResult;

pub struct Client {
    protocol: Protocol,
}

impl Client {
    pub fn connect(address: SocketAddr) -> impl Future<Item = Client, Error = io::Error> {
        TcpStream::connect(&address).and_then(|tcp_stream| {
            Ok(Client {
                protocol: Protocol::new(tcp_stream)?,
            })
        })
    }

    pub fn send_request<B>(
        &mut self,
        request: Request<B>,
    ) -> impl Future<Item = ResponseResult, Error = Rc<io::Error>>
    where
        B: AsRef<[u8]>,
    {
        self.protocol.send_request(request)
    }
}
