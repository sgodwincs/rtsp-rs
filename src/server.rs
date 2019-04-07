use bytes::BytesMut;
use chrono::{self, offset, DateTime, Utc};
use futures::Stream;
use futures::{future, Async, Future, Poll};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::error::Error;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio_tcp::TcpListener;
use tower_service::Service;

use crate::header::types::Public;
use crate::method::Method;
use crate::protocol::connection::Connection;
use crate::protocol::connection::ConnectionHandle;
use crate::request::Request;
use crate::response::{Response, NOT_IMPLEMENTED_RESPONSE};
use crate::session::{Session, SessionID, SessionIDError, DEFAULT_SESSION_TIMEOUT};
use crate::status::StatusCode;

pub const SUPPORTED_METHODS: [Method; 2] = [Method::Options, Method::Setup];

/// Experimental high-level server implementation
pub struct Server {
    connections: Vec<ConnectionHandle>,
    sessions: HashMap<SessionID, Arc<Mutex<ServerSession>>>,
}

impl Server {
    fn new() -> Self {
        Server {
            connections: Vec::new(),
            sessions: HashMap::new(),
        }
    }

    pub fn run(address: SocketAddr) {
        let server = Arc::new(Mutex::new(Server::new()));
        let listener = TcpListener::bind(&address).unwrap();

        let serve = listener.incoming().for_each(move |socket| {
            let server = server.clone();
            let service = ConnectionService {
                session: None,
                server: server.clone(),
            };
            let (connection, handler, handle) = Connection::new(socket, Some(service));

            server.lock().unwrap().connections.push(handle);

            tokio::spawn(connection);
            tokio::spawn(handler.unwrap());

            Ok(())
        });

        tokio::run(serve.map_err(|_| ()));
    }
}

struct ConnectionService {
    session: Option<Arc<Mutex<ServerSession>>>,
    server: Arc<Mutex<Server>>,
}

impl ConnectionService {
    fn handle_method_options(
        &mut self,
        request: Request<BytesMut>,
    ) -> <Self as Service<Request<BytesMut>>>::Future {
        if let Some(session) = self.session.as_mut() {
            session.lock().unwrap().touch();
        }

        // Drop the body.
        let request = request.map(|_| BytesMut::new());

        // We do not support any media streams right now, so just always 404 on non-asterisk URI.
        let response = if request.uri().is_asterisk() {
            Response::<()>::builder()
                .with_typed_header(SUPPORTED_METHODS.iter().cloned().collect::<Public>())
                .with_body(BytesMut::new())
                .build()
                .unwrap()
        } else {
            Response::<()>::builder()
                .with_status_code(StatusCode::NotFound)
                .with_body(BytesMut::new())
                .build()
                .unwrap()
        };

        Box::new(future::ok(response))
    }

    fn handle_method_setup(
        &mut self,
        request: Request<BytesMut>,
    ) -> <Self as Service<Request<BytesMut>>>::Future {
        Box::new(future::ok(
            Response::<()>::builder()
                .with_body(BytesMut::new())
                .build()
                .unwrap(),
        ))
    }
}

impl Service<Request<BytesMut>> for ConnectionService {
    type Response = Response<BytesMut>;
    type Error = Box<Error + Send + 'static>;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, mut request: Request<BytesMut>) -> Self::Future {
        request.uri_mut().normalize();

        match request.method() {
            Method::Options => self.handle_method_options(request),
            Method::Setup => self.handle_method_setup(request),
            _ => Box::new(future::ok(NOT_IMPLEMENTED_RESPONSE.clone())),
        }
    }

    fn poll_ready(&mut self) -> Poll<(), Self::Error> {
        Ok(Async::Ready(()))
    }
}

pub struct ServerSession {
    expire_time: DateTime<Utc>,
    id: SessionID,
}

impl ServerSession {
    pub fn new<T>(active_client: ConnectionHandle) -> Self
    where
        SessionID: TryFrom<T, Error = SessionIDError>,
    {
        let expire_time = offset::Utc::now()
            .checked_add_signed(chrono::Duration::from_std(DEFAULT_SESSION_TIMEOUT).unwrap())
            .unwrap();

        ServerSession::with_timeout(expire_time, active_client)
    }

    pub fn with_timeout<T>(expire_time: DateTime<Utc>, active_client: ConnectionHandle) -> Self
    where
        SessionID: TryFrom<T, Error = SessionIDError>,
    {
        ServerSession {
            expire_time,
            id: SessionID::random(),
        }
    }

    fn touch(&mut self) {
        self.set_timeout(DEFAULT_SESSION_TIMEOUT).unwrap();
    }
}

impl Session for ServerSession {
    fn expire_time(&self) -> DateTime<Utc> {
        self.expire_time
    }

    fn id(&self) -> &SessionID {
        &self.id
    }

    fn set_expire_time(&mut self, expire_time: DateTime<Utc>) {
        self.expire_time = expire_time;
    }

    fn set_timeout(&mut self, timeout: Duration) -> Result<(), ()> {
        let timeout = chrono::Duration::from_std(timeout).map_err(|_| ())?;
        self.expire_time = offset::Utc::now().checked_add_signed(timeout).ok_or(())?;
        Ok(())
    }
}
