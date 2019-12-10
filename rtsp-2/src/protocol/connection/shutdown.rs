//! Connection Shutdown Handler
//!
//! This module contains the logic for dealing with connection shutdown events. Specifically, this
//! module only handles the shutdown of the the [`Connection`] task and is not involved with the
//! shutdown of the [`RequestHandler`] task.

use futures::sync::oneshot;
use futures::{try_ready, Async, Future, Poll};
use std::time::{Duration, Instant};
use tokio_timer::Delay;

/// The type responsible for managing deliberate shutdown of connections.
#[derive(Debug)]
#[must_use = "futures do nothing unless polled"]
pub struct ShutdownHandler {
    /// A receiver that will perform a shutdown on receiving a shutdown type. This can only occur
    /// once, and at present, can only occur from a [`ConnectionHandle`].
    rx_initiate_shutdown: Option<oneshot::Receiver<ShutdownType>>,

    /// When a graceful shutdown has been started, a timer is set for which the connection must end
    /// before switching to an immediate shutdown. This timer does not apply to the request handler
    /// task. All requests that are forwarded to the request handler will always be processed before
    /// it shuts down.
    timer: Option<Delay>,

    /// It is possible that a shutdown can occur without an explicit call from a
    /// [`ConnectionHandle`], but it may be necessary for users to know when it happens. The
    /// corresponding receiver is kept within the handles and will receive a message once an
    /// immediate shutdown has occurred.
    tx_shutdown_event: Option<oneshot::Sender<()>>,
}

impl ShutdownHandler {
    /// Constructs a new shutdown handler.
    pub fn new(
        rx_initiate_shutdown: oneshot::Receiver<ShutdownType>,
        tx_shutdown_event: oneshot::Sender<()>,
    ) -> Self {
        ShutdownHandler {
            rx_initiate_shutdown: Some(rx_initiate_shutdown),
            timer: None,
            tx_shutdown_event: Some(tx_shutdown_event),
        }
    }

    /// Forces an immediate shutdown to occur.
    pub fn force_shutdown(&mut self) {
        self.handle_shutdown(ShutdownType::Immediate);
    }

    /// An event handler for dealing with a specific shutdown type.
    ///
    /// If [`ShutdownType::Graceful`] is provided, a timer will be set with the provided duration.
    ///
    /// If [`ShutdownType::Immediate`] is provided, a message will be sent to the receiver of the
    /// provided shutdown event sender in [`ShutdownHandler::new`].
    fn handle_shutdown(&mut self, shutdown_type: ShutdownType) {
        match shutdown_type {
            ShutdownType::Graceful(duration) => {
                debug_assert!(self.state() != ShutdownState::Shutdown);

                let expire_time = Instant::now() + duration;
                self.rx_initiate_shutdown = None;
                self.timer = Some(Delay::new(expire_time));
            }
            ShutdownType::Immediate => {
                self.rx_initiate_shutdown = None;
                self.timer = None;

                if let Some(tx_shutdown_event) = self.tx_shutdown_event.take() {
                    let _ = tx_shutdown_event.send(());
                }
            }
        }
    }

    /// Polls the shutdown with the assumption that the current state is [`ShutdownState::Running`].
    ///
    /// Specifically, this function will check if a shutdown was initiated by an outside caller via
    /// the sender provided in [`ShutdownHandler::new`]. If the sender is dropped, this will cause
    /// an immediate shutdown to occur.
    ///
    /// If `Ok(Async::Ready(()))` is returned, this implies that a shutdown was initiated as
    /// described above.
    ///
    /// If `Ok(Async::NotReady)` is returned, then the current task will be notified when another
    /// poll needs to occur.
    ///
    /// The error `Err(())` will never be returned.
    fn poll_running(&mut self) -> Poll<(), ()> {
        match self
            .rx_initiate_shutdown
            .as_mut()
            .expect(
                "`ShutdownHandler::poll_running` should not be called if `ShutdownHandler.rx_initiate_shutdown` is `None`",
            )
            .poll()
        {
            Ok(Async::Ready(shutdown_type)) => {
                // A shutdown event has been sent by the sender, initiate a shutdown.
                self.handle_shutdown(shutdown_type);
                Ok(Async::Ready(()))
            }
            Err(_) => {
                // The sender has been dropped, initiate an immediate shutdown.
                self.handle_shutdown(ShutdownType::Immediate);
                Ok(Async::Ready(()))
            }
            _ => Ok(Async::NotReady),
        }
    }

    /// Polls the shutdown with the assumption that the current state is
    /// [`ShutdownState::ShuttingDown`].
    ///
    /// Specifically, this function will check if the graceful shutdown duration has passed. If so,
    /// an immediate shutdown will occur. Any timer error that happens will also cause an immediate
    /// shutdown.
    ///
    /// If `Ok(Async::Ready(()))` is returned, this implies that the timer has expired which will
    /// cause an immediate shutdown.
    ///
    /// If `Ok(Async::NotReady)` is returned, then the current task will be notified when another
    /// poll needs to occur.
    ///
    /// The error `Err(())` will never be returned.
    fn poll_shutting_down(&mut self) -> Poll<(), ()> {
        match self
            .timer
            .as_mut()
            .expect(
                "`ShutdownHandler::poll_shutting_down` should not be called if `ShutdownHandler.timer` is `None`",
            )
            .poll()
        {
            Ok(Async::Ready(_)) => {
                self.handle_shutdown(ShutdownType::Immediate);
                Ok(Async::Ready(()))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
            Err(ref error) if error.is_at_capacity() => {
                self.handle_shutdown(ShutdownType::Immediate);
                Ok(Async::Ready(()))
            }
            _ => panic!("shutdown timer should not be shutdown"),
        }
    }

    /// Returns the current state of the shutdown handler (i.e. the shutdown status of the
    /// connection).
    pub fn state(&self) -> ShutdownState {
        if self.rx_initiate_shutdown.is_some() {
            debug_assert!(self.timer.is_none());
            ShutdownState::Running
        } else if self.timer.is_some() {
            ShutdownState::ShuttingDown
        } else {
            debug_assert!(self.tx_shutdown_event.is_none());
            ShutdownState::Shutdown
        }
    }
}

impl Drop for ShutdownHandler {
    fn drop(&mut self) {
        self.force_shutdown();
    }
}

impl Future for ShutdownHandler {
    type Item = ();
    type Error = ();

    /// Polls for shutdown based on the current state.
    ///
    /// If `Ok(Async::Ready(ShutdownState))` is returned, this implies that the shutdown state needs
    /// to be dealt with by the caller by inspecting the result of a call to
    /// [`ShutdownHandler::state`].
    ///
    /// If `Ok(Async::NotReady)` is returned, then the current task will be notified when another
    /// poll needs to occur.
    ///
    /// The error `Err(())` will never be returned.
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        loop {
            match self.state() {
                ShutdownState::Running => try_ready!(self.poll_running()),
                ShutdownState::ShuttingDown => try_ready!(self.poll_shutting_down()),
                ShutdownState::Shutdown => return Ok(Async::Ready(())),
            }
        }
    }
}

/// The shutdown state of a connection. This does not have any relation to the shutdown state of the
/// request handler. But if the connection is shutdown, this does imply an eventual shutdown of the
/// request handler once all buffered requests have been processed.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownState {
    /// The connection is running normally.
    Running,

    /// The connection is shutdown. No more reading or writing will occur. Despite no more writing
    /// being possible, all buffered requests will be processed, there just will not be any
    /// responses sent back.
    Shutdown,

    /// The connection is shutting down. Only responses can be read or written and all pending
    /// requests must be matched within a time interval. Otherwise, they will be dropped and the
    /// connection will transition to a shutdown state.
    ShuttingDown,
}

/// The type of shutdown that the connection will process.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownType {
    /// During the provided duration from the time that this type is processed, only responses can
    /// be read or written. If all pending requests have not been matched by the end of this
    /// duration, they will be dropped, and the shutdown state will transition to
    /// [`ShutdownState::Shutdown`].
    ///
    /// This timer does not apply to the request handler task. All requests that are forwarded to
    /// the request handler will always be processed before it shuts down. But once a graceful
    /// shutdown starts, no more requests will be forwarded from that point onwards.
    Graceful(Duration),

    /// A complete and immediate shutdown of the connection is to occur. All pending requests will
    /// be dropped and no more messages can be read or written. All currently buffered requests will
    /// be processed before the request handler is shutdown, but the connection itself will shutdown
    /// immediately.
    Immediate,
}

#[cfg(test)]
mod test {
    use futures::future::{self, Either};
    use futures::sync::oneshot;
    use futures::Future;
    use std::mem;
    use std::time::{Duration, Instant};
    use tokio::runtime::current_thread;
    use tokio_timer::Delay;

    use crate::protocol::connection::shutdown::{ShutdownHandler, ShutdownState, ShutdownType};

    #[test]
    fn test_shutdown_drop() {
        let (_tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let shutdown = ShutdownHandler::new(rx_initiate_shutdown, tx_shutdown_event);
        assert_eq!(shutdown.state(), ShutdownState::Running);

        mem::drop(shutdown);
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());
    }

    #[test]
    fn test_shutdown_force_shutdown() {
        let (_tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let mut shutdown = ShutdownHandler::new(rx_initiate_shutdown, tx_shutdown_event);
        assert_eq!(shutdown.state(), ShutdownState::Running);

        shutdown.force_shutdown();
        assert_eq!(shutdown.state(), ShutdownState::Shutdown);

        assert!(current_thread::block_on_all(&mut shutdown).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());
        assert_eq!(shutdown.state(), ShutdownState::Shutdown);
    }

    #[test]
    fn test_shutdown_initiate_graceful_shutdown() {
        let (tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let shutdown = ShutdownHandler::new(rx_initiate_shutdown, tx_shutdown_event);
        assert_eq!(shutdown.state(), ShutdownState::Running);

        current_thread::block_on_all(future::lazy(|| {
            current_thread::spawn(shutdown);
            current_thread::spawn(
                Delay::new(Instant::now() + Duration::from_millis(150))
                    .map_err(|_| panic!())
                    .select2(rx_shutdown_event.map_err(|_| panic!()))
                    .then(|result| match result {
                        Ok(Either::A((_, rx_shutdown_event))) => rx_shutdown_event,
                        _ => panic!(),
                    }),
            );
            tx_initiate_shutdown
                .send(ShutdownType::Graceful(Duration::from_millis(200)))
                .unwrap();

            Ok::<_, ()>(())
        }))
        .unwrap();
    }

    #[test]
    fn test_shutdown_initiate_immediate_shutdown() {
        let (tx_initiate_shutdown, rx_initiate_shutdown) = oneshot::channel();
        let (tx_shutdown_event, rx_shutdown_event) = oneshot::channel();
        let mut shutdown = ShutdownHandler::new(rx_initiate_shutdown, tx_shutdown_event);
        assert_eq!(shutdown.state(), ShutdownState::Running);

        tx_initiate_shutdown.send(ShutdownType::Immediate).unwrap();

        assert!(current_thread::block_on_all(&mut shutdown).is_ok());
        assert!(current_thread::block_on_all(rx_shutdown_event).is_ok());
        assert_eq!(shutdown.state(), ShutdownState::Shutdown);
    }
}
