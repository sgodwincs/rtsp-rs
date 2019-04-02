use bytes::BytesMut;
use chrono::{self, offset, DateTime, Utc};
use futures::{future, Future};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::header::types::Public;
use crate::method::Method;
use crate::protocol::connection::ConnectionHandle;
use crate::protocol::service::Service;
use crate::request::Request;
use crate::response::{Response, NOT_IMPLEMENTED_RESPONSE};
use crate::session::{Session, SessionID, SessionIDError, DEFAULT_SESSION_TIMEOUT};

pub const SUPPORTED_METHODS: [Method; 1] = [Method::Options];

pub struct Server {
    // connections:
    sessions: Arc<Mutex<HashMap<SessionID, ServerSession>>>,
}

struct ClientHandler {
    connection: ConnectionHandle,
}

impl ClientHandler {
    fn handle_method_options(
        &mut self,
        request: Request<BytesMut>,
    ) -> impl Future<Item = Response<BytesMut>, Error = io::Error> {
        let mut builder = Response::builder();
        builder
            .typed_header(SUPPORTED_METHODS.iter().cloned().collect::<Public>())
            .body(BytesMut::new());
        let response = builder.build().unwrap();

        future::ok(response)
    }
}

impl Service for ClientHandler {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, mut request: Self::Request) -> Self::Future {
        request.uri_mut().normalize();

        match request.method() {
            Method::Options => Box::new(self.handle_method_options(request)),
            _ => Box::new(future::ok(NOT_IMPLEMENTED_RESPONSE.clone())),
        }
    }
}

pub struct ServerSession {
    active_client: ConnectionHandle,
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
            active_client,
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
