extern crate bytes;
extern crate futures;
extern crate rtsp;
extern crate tokio;
extern crate tokio_tcp;
extern crate tokio_timer;

use bytes::BytesMut;
use futures::{future, lazy, Future};
use rtsp::protocol::{Connection, ConnectionHandle, OperationError, ShutdownType};
use rtsp::{HeaderMap, HeaderName, Method, Request, Response, Service};
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
                request.headers().get(&HeaderName::CSeq).unwrap().clone(),
            )
            .build("".into())
            .unwrap();

        Box::new(future::ok(response))
    }
}

fn create_test_base<ServerHandler, RequestService>(
    server_handler: ServerHandler,
    service: Option<RequestService>,
) -> impl Future<Item = ConnectionHandle, Error = ()>
where
    ServerHandler: FnOnce(TcpListener) -> () + Send + 'static,
    RequestService: Service<Request = Request<BytesMut>> + Send + 'static,
    RequestService::Future: Send + 'static,
    RequestService::Response: Into<Response<BytesMut>>,
{
    let server = TcpListener::bind("127.0.0.1:0").unwrap();
    let address = server.local_addr().unwrap();

    thread::spawn(move || {
        server_handler(server);
    });

    let tcp_stream = TcpStream::connect(&address).wait().unwrap();

    lazy(|| {
        let (connection, handler, handle) = Connection::new(tcp_stream, service);

        tokio::spawn(connection);

        if let Some(handler) = handler {
            tokio::spawn(handler);
        }

        Ok(handle)
    })
}

/// Test to make sure shutdown occurs in the case where:
/// 1. All connection handles are dropped.
/// 2. Either a service is or is not provided.
/// 3. There are no pending requests.
#[test]
fn test_connection_shutdown_connection_dropped_with_no_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read until EOF to ensure server does not cause a shutdown.

                    let mut buffer = vec![];
                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                // Wait for shutdown event.

                handle.take_shutdown().unwrap().map_err(|_| ())
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. All connection handles are dropped.
/// 2. Either a service is or is not provided.
/// 3. There are pending requests.
#[test]
fn test_connection_shutdown_connection_dropped_with_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read the incoming request.

                    let mut buffer = vec![0; 4096];
                    socket.read(&mut buffer).unwrap();

                    // Wait a bit to ensure the connection is dropped and the state machine is
                    // settled.

                    thread::sleep(Duration::from_millis(100));

                    // Write the response to the above request.

                    socket
                        .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
                        .unwrap();

                    // Read until EOF to ensure server does not cause a shutdown.

                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                // Prepare shutdown future for waiting for the shutdown event.

                let shutdown = handle.take_shutdown().unwrap().map_err(|_| ());

                // Send request and drop the connection handle before a response is returned.

                let request = Request::builder()
                    .method(Method::Options)
                    .uri("*")
                    .build("")
                    .unwrap();

                handle
                    .send_request(request)
                    .map(|_| ())
                    .map_err(|_| panic!("expected a response"))
                    .then(|_| shutdown)
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Immediate shutdown of the connection without dropping the connection handle.
/// 2. Either a service is or is not provided.
/// 3. There are no pending requests.
#[test]
fn test_connection_shutdown_immediate_with_no_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read until EOF to ensure server does not cause a shutdown.

                    let mut buffer = vec![];
                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                handle.shutdown(ShutdownType::Immediate);

                // Wait for shutdown event.

                handle.take_shutdown().unwrap().then(|_| {
                    // Drop handle here, so it is not dropped sooner.

                    mem::drop(handle);
                    Ok(())
                })
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Immediate shutdown of the connection without dropping the connection handle.
/// 2. Either a service is or is not provided.
/// 3. There are pending requests.
#[test]
fn test_connection_shutdown_immediate_with_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read until EOF to ensure server does not cause a shutdown. Even though a
                    // request is being sent, we do not need to respond, since it should be
                    // cancelled almost immediately.

                    let mut buffer = vec![];
                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                // Prepare shutdown future for waiting for the shutdown event.

                let shutdown = handle.take_shutdown().unwrap().map_err(|_| ());

                // Send request and do an immediate shutdown of the connection before a response is
                // received.

                let request = Request::builder()
                    .method(Method::Options)
                    .uri("*")
                    .build("")
                    .unwrap();
                let mut handle_clone = handle.clone();

                // We need to delay doing the shutdown so that it happens after the request is sent.
                // Otherwise, there is a possible race condition (though unlikely) in which the
                // response will be `OperationError::Closed` since the shutdown has been processed
                // before sending the request.

                tokio::spawn(
                    Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                        handle_clone.shutdown(ShutdownType::Immediate);
                        Ok(())
                    }),
                );

                handle
                    .send_request(request)
                    .map(|_| panic!("expected a cancelled request"))
                    .map_err(|error| assert_eq!(error, OperationError::RequestCancelled))
                    .then(|_| shutdown)
                    .then(|_| {
                        // Drop handle here, so it is not dropped sooner.

                        mem::drop(handle);
                        Ok(())
                    })
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Graceful shutdown of the connection without dropping the connection handle.
/// 2. Either a service is or is not provided.
/// 3. There are no pending requests.
#[test]
fn test_connection_shutdown_graceful_with_no_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read until EOF to ensure server does not cause a shutdown. Even though a
                    // request is being sent, we do not need to respond, since it should be
                    // cancelled almost immediately.

                    let mut buffer = vec![];
                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                handle.shutdown(ShutdownType::Graceful(Duration::from_millis(5)));

                // Wait for shutdown event.

                handle.take_shutdown().unwrap().then(|_| {
                    // Drop handle here, so it is not dropped sooner.

                    mem::drop(handle);
                    Ok(())
                })
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Graceful shutdown of the connection without dropping the connection handle.
/// 2. Either a service is or is not provided.
/// 3. There are pending requests.
#[test]
fn test_connection_shutdown_graceful_with_pending_requests() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read the incoming request.

                    let mut buffer = vec![0; 4096];
                    socket.read(&mut buffer).unwrap();

                    // Wait a bit to ensure the graceful shutdown has started.

                    thread::sleep(Duration::from_millis(200));

                    // Write the response to the above request.

                    socket
                        .write("RTSP/2.0 200 OK\r\nCSeq: 0\r\n\r\n".as_bytes())
                        .unwrap();

                    // Read until EOF to ensure server does not cause a shutdown.

                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                // Prepare shutdown future for waiting for the shutdown event.

                let shutdown = handle.take_shutdown().unwrap().map_err(|_| ());

                // Send request and do a graceful shutdown of the connection before a response is
                // received.

                let request = Request::builder()
                    .method(Method::Options)
                    .uri("*")
                    .build("")
                    .unwrap();
                let mut handle_clone = handle.clone();

                // We need to delay doing the shutdown so that it happens after the request is sent.
                // Otherwise, there is a possible race condition (though unlikely) in which the
                // response will be `OperationError::Closed` since the shutdown has been processed
                // before sending the request.

                tokio::spawn(
                    Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                        handle_clone.shutdown(ShutdownType::Graceful(Duration::from_secs(5)));
                        Ok(())
                    }),
                );

                handle
                    .send_request(request)
                    .map(|_| ())
                    .map_err(|_| panic!("expected a response"))
                    .then(|_| shutdown)
                    .then(|_| {
                        // Drop handle here, so it is not dropped sooner.

                        mem::drop(handle);
                        Ok(())
                    })
            }),
        );
    }
}

/// Test to make sure shutdown occurs in the case where:
/// 1. Graceful shutdown of the connection without dropping the connection handle.
/// 2. Either a service is or is not provided.
/// 3. There are pending requests.
/// 4. The timeout for the graceful shutdown will occur before a response is received.
#[test]
fn test_connection_shutdown_graceful_with_pending_requests_and_timeout() {
    for service in vec![None, Some(DummyService)] {
        tokio::run(
            create_test_base(
                |server| {
                    let mut socket = server.accept().unwrap().0;

                    // Read until EOF to ensure server does not cause a shutdown. Even though a
                    // request is being sent, we do not want to response, so we can test the
                    // graceful shutdown timeout.

                    let mut buffer = vec![];
                    socket.read_to_end(&mut buffer).unwrap();
                },
                service,
            )
            .and_then(|mut handle| {
                // Prepare shutdown future for waiting for the shutdown event.

                let shutdown = handle.take_shutdown().unwrap().map_err(|_| ());

                // Send request and do a graceful shutdown of the connection before a response is
                // received.

                let request = Request::builder()
                    .method(Method::Options)
                    .uri("*")
                    .build("")
                    .unwrap();
                let mut handle_clone = handle.clone();

                // We need to delay doing the shutdown so that it happens after the request is sent.
                // Otherwise, there is a possible race condition (though unlikely) in which the
                // response will be `OperationError::Closed` since the shutdown has been processed
                // before sending the request.

                tokio::spawn(
                    Delay::new(Instant::now() + Duration::from_millis(100)).then(move |_| {
                        handle_clone.shutdown(ShutdownType::Graceful(Duration::from_millis(100)));
                        Ok(())
                    }),
                );

                handle
                    .send_request(request)
                    .map(|_| panic!("expected a cancelled request"))
                    .map_err(|error| assert_eq!(error, OperationError::RequestCancelled))
                    .then(|_| shutdown)
                    .then(|_| {
                        // Drop handle here, so it is not dropped sooner.

                        mem::drop(handle);
                        Ok(())
                    })
            }),
        );
    }
}
