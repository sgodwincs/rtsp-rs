use bytes::BytesMut;
use chrono::{self, offset, DateTime, Utc};
use futures::{future, Future};
use std::collections::HashMap;
use std::convert::TryFrom;
use std::io;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use header::types::Public;
use header::TypedHeaderMap;
use method::Method;
use protocol::{ConnectionHandle, Service};
use request::{Request, TypedRequest};
use response::{Response, TypedResponse};
use session::{InvalidSessionID, Session, SessionID, DEFAULT_SESSION_TIMEOUT};
use status::StatusCode;

pub const SUPPORTED_METHODS: [Method; 1] = [Method::Options];

lazy_static! {
    static ref NOT_IMPLEMENTED_RESPONSE: Response<BytesMut> = Response::builder()
        .status_code(StatusCode::NotImplemented)
        .build(BytesMut::new())
        .expect("not implemented response should not be invalid");
}

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
        request: TypedRequest<BytesMut>,
    ) -> impl Future<Item = Response<BytesMut>, Error = io::Error> {
        let response = Response::typed_builder()
            .header(SUPPORTED_METHODS.iter().cloned().collect::<Public>())
            .build(BytesMut::new())
            .unwrap();

        future::ok(response.into())
    }
}

impl Service for ClientHandler {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, request: Self::Request) -> Self::Future {
        let request: Request<_, TypedHeaderMap> = request.into();

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
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        let expire_time = offset::Utc::now()
            .checked_add_signed(chrono::Duration::from_std(DEFAULT_SESSION_TIMEOUT).unwrap())
            .unwrap();

        ServerSession::with_timeout(expire_time, active_client)
    }

    pub fn with_timeout<T>(expire_time: DateTime<Utc>, active_client: ConnectionHandle) -> Self
    where
        SessionID: TryFrom<T, Error = InvalidSessionID>,
    {
        ServerSession {
            active_client: active_client,
            expire_time: expire_time,
            id: SessionID::random(),
        }
    }

    fn touch(&mut self) {
        self.set_timeout(DEFAULT_SESSION_TIMEOUT);
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
