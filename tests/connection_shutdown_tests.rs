extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;
extern crate tokio_tcp;
extern crate tokio_timer;

use bytes::BytesMut;
use futures::{future, lazy, Future};
use rtsp::protocol::{
    Connection, EmptyService, OperationError, ReadState, ShutdownType, WriteState,
    DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION,
};
use rtsp::{HeaderName, Method, Request, Response, Service, URI};
use std::io::{self, Read, Write};
use std::net::TcpListener;
use std::time::{Duration, Instant};
use std::{mem, thread};
use tokio_tcp::TcpStream;
use tokio_timer::Delay;

struct DummyService;

impl Service for DummyService {
    type Request = Request<BytesMut>;
    type Response = Response<BytesMut>;
    type Error = io::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send + 'static>;

    fn call(&mut self, request: Self::Request) -> Self::Future {
        let response = Response::builder()
            .header(
                HeaderName::CSeq,
                request.headers().get(HeaderName::CSeq).unwrap().clone(),
            )
            .build("".into())
            .unwrap();

        Box::new(future::ok(response))
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. The connection object is dropped.
/// 2. No service is provided.
/// 3. No requests are sent.
#[test]
fn test_connection_shutdown_connection_dropped_with_no_service_and_no_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![];
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        Connection::new::<_, EmptyService>(tcp_stream, None).unwrap();
        Ok(())
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. The connection object is dropped.
/// 2. A service is provided.
/// 3. No requests are sent.
#[test]
fn test_connection_shutdown_connection_dropped_with_service_and_no_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![];
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        Connection::new(tcp_stream, Some(DummyService)).unwrap();
        Ok(())
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. The connection object is dropped.
/// 2. No service is provided.
/// 3. A request is sent such that the connection object is dropped before the response is received.
#[test]
fn test_connection_shutdown_connection_dropped_with_no_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];
        socket.read(&mut buffer).unwrap();
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();

        Connection::new::<_, EmptyService>(tcp_stream, None)
            .unwrap()
            .send_request(request)
            .map(|_| ())
            .map_err(|_| panic!("expected a response"))
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. The connection object is dropped.
/// 2. A service is provided.
/// 3. A request is sent such that the connection object is dropped before the response is received.
#[test]
fn test_connection_shutdown_connection_dropped_with_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];
        socket.read(&mut buffer).unwrap();
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();

        Connection::new(tcp_stream, Some(DummyService))
            .unwrap()
            .send_request(request)
            .map(|_| ())
            .map_err(|_| panic!("expected a response"))
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Shutdown of the connection is forced via `shutdown`
/// 2. No service is provided.
/// 3. No requests are sent.
#[test]
fn test_connection_shutdown_forced_hard_with_no_service_and_no_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![];
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let mut connection = Connection::new::<_, EmptyService>(tcp_stream, None).unwrap();
        connection.shutdown(ShutdownType::Hard);
        Ok(())
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. A hard shutdown of the connection is forced via `shutdown`.
/// 2. No service is provided.
/// 3. A request is sent such that shutdown occurs before the response is received.
#[test]
fn test_connection_shutdown_forced_hard_with_no_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];

        socket.read(&mut buffer).unwrap();
        thread::sleep(Duration::from_millis(500));
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        assert!(socket.read_to_end(&mut buffer).is_err());
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let mut connection = Connection::new::<_, EmptyService>(tcp_stream, None).unwrap();
        let timer = Delay::new(Instant::now() + Duration::from_millis(100));
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();
        let future_response = connection
            .send_request(request)
            .map(|_| panic!("expected a cancelled request"))
            .map_err(|error| assert_eq!(error, OperationError::RequestCancelled));

        tokio::spawn(timer.then(|_| {
            connection.shutdown(ShutdownType::Hard);
            Delay::new(Instant::now() + Duration::from_millis(500)).then(|_| {
                mem::drop(connection);
                Ok(())
            })
        }));

        future_response
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. A hard shutdown of the connection is forced via `shutdown`.
/// 2. A service is provided.
/// 3. A request is sent such that shutdown occurs before the response is received.
#[test]
fn test_connection_shutdown_forced_hard_with_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];

        socket.read(&mut buffer).unwrap();
        thread::sleep(Duration::from_millis(500));
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        assert!(socket.read_to_end(&mut buffer).is_err());
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let mut connection = Connection::new(tcp_stream, Some(DummyService)).unwrap();
        let timer = Delay::new(Instant::now() + Duration::from_millis(100));
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();
        let future_response = connection
            .send_request(request)
            .map(|_| panic!("expected a cancelled request"))
            .map_err(|error| assert_eq!(error, OperationError::RequestCancelled));

        tokio::spawn(timer.then(|_| {
            connection.shutdown(ShutdownType::Hard);
            Delay::new(Instant::now() + Duration::from_millis(500)).then(|_| {
                mem::drop(connection);
                Ok(())
            })
        }));

        future_response
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. A soft shutdown of the connection is forced via `shutdown`.
/// 2. No service is provided.
/// 3. A request is sent such that shutdown occurs before the response is received.
#[test]
fn test_connection_shutdown_forced_soft_with_no_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];
        socket.read(&mut buffer).unwrap();
        thread::sleep(Duration::from_millis(500));
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let mut connection = Connection::new::<_, EmptyService>(tcp_stream, None).unwrap();
        let timer = Delay::new(Instant::now() + Duration::from_millis(100));
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();
        let future_response = connection
            .send_request(request)
            .map(|_| ())
            .map_err(|_| panic!("expected a response"));

        tokio::spawn(timer.then(|_| {
            connection.shutdown(ShutdownType::Soft(DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION));
            Delay::new(Instant::now() + Duration::from_millis(500)).then(move |_| {
                assert_eq!(connection.state(), (ReadState::None, WriteState::None));
                Ok(())
            })
        }));

        future_response
    }));
}

/// Test to make sure shutdown occurs in the case where:
/// 1. A soft shutdown of the connection is forced via `shutdown`.
/// 2. A service is provided.
/// 3. A request is sent such that shutdown occurs before the response is received.
#[test]
fn test_connection_shutdown_forced_soft_with_service_and_requests() {
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        let mut socket = server.accept().unwrap().0;
        let mut buffer = vec![0; 4096];
        socket.read(&mut buffer).unwrap();
        thread::sleep(Duration::from_millis(500));
        socket
            .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
            .unwrap();
        socket.read_to_end(&mut buffer).unwrap();
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    tokio::run(lazy(|| {
        let mut connection = Connection::new(tcp_stream, Some(DummyService)).unwrap();
        let timer = Delay::new(Instant::now() + Duration::from_millis(100));
        let request = Request::builder()
            .method(Method::Options)
            .uri(URI::Any)
            .build("")
            .unwrap();
        let future_response = connection
            .send_request(request)
            .map(|_| ())
            .map_err(|_| panic!("expected a response"));

        tokio::spawn(timer.then(|_| {
            connection.shutdown(ShutdownType::Soft(DEFAULT_SOFT_SHUTDOWN_TIMEOUT_DURATION));
            Delay::new(Instant::now() + Duration::from_millis(500)).then(|_| {
                assert_eq!(connection.state(), (ReadState::None, WriteState::None));
                mem::drop(connection);
                Ok(())
            })
        }));

        future_response
    }));
}
