//! Connection Shutdown Handler
//!
//! This module contains the logic for dealing with shutdown events.

use futures::sync::oneshot;
use futures::{Async, Future, Poll};
use std::time::{Duration, Instant};
use tokio_timer::Delay;

/// The object responsible for managing deliberate shutdown of connections.
#[derive(Debug)]
pub struct Shutdown {
    /// A receiver that will perform a shutdown on receiving a shutdown type. This can only occur
    /// once, and at present, can only occur from a [`super::ConnectionHandle`].
    rx_initiate_shutdown: Option<oneshot::Receiver<ShutdownType>>,

    /// When a graceful shutdown has been started, a timer is set for which the connection must end
    /// before switching to an immediate shutdown.
    timer: Option<Delay>,

    /// It is possible that a shutdown can occur without an explicit call from a
    /// [`super::ConnectionHandle`], but it may be necessary for users to know when it happens.
    /// The corresponding receiver is kept within the handles and will receive a message once an
    /// immediate shutdown has occurred.
    tx_shutdown_event: Option<oneshot::Sender<()>>,
}

impl Shutdown {
    /// Constructs a new shutdown handler.
    ///
    /// # Arguments
    ///
    /// * `rx_initiate_shutdown` - A receiver for when the caller wants to start a shutdown
    ///   indirectly.
    /// * `tx_shutdown_event` - A sender for which this object will notify the caller when the
    ///   shutdown has completed.
    pub fn new(
        rx_initiate_shutdown: oneshot::Receiver<ShutdownType>,
        tx_shutdown_event: oneshot::Sender<()>,
    ) -> Self {
        Shutdown {
            rx_initiate_shutdown: Some(rx_initiate_shutdown),
            timer: None,
            tx_shutdown_event: Some(tx_shutdown_event),
        }
    }

    /// Simply ensures that a full (i.e. immediate) shutdown has occurred.
    pub fn ensure_shutdown(&mut self) {
        self.handle_shutdown(ShutdownType::Immediate);
    }

    /// An event handler for dealing with a specific shutdown type.
    ///
    /// If [`ShutdownType::Graceful`] is provided, a timer will be set with the provided duration.
    ///
    /// If [`ShutdownType::Immediate`] is provided, a message will be sent to the receiver of the
    /// provided shutdown event sender in `Shutdown::new`.
    ///
    /// # Arguments
    ///
    /// * `shutdown_type` - The type of shutdown to occur.
    fn handle_shutdown(&mut self, shutdown_type: ShutdownType) {
        match shutdown_type {
            ShutdownType::Graceful(duration) => {
                debug_assert!(self.state() != ShutdownState::Shutdown);

                let expire_time = Instant::now() + duration;
                self.rx_initiate_shutdown = None;
                self.timer = Some(Delay::new(expire_time));

                // We need to do a poll here even though it will most certainly not be ready. This
                // will register the current task to wake up when it is ready. If it fails for
                // whatever reason, then the current state will just switch to
                // [`ShutdownState::Shutdown`].

                self.poll_shutting_down().ok();
            }
            ShutdownType::Immediate => {
                self.rx_initiate_shutdown = None;
                self.timer = None;

                if let Some(tx_shutdown_event) = self.tx_shutdown_event.take() {
                    tx_shutdown_event.send(()).ok();
                }
            }
        }
    }

    /// Polls for shutdown based on the current state.
    ///
    /// # Return Value
    ///
    /// If `Ok(Async::Ready(()))` is returned, this implies that the shutdown state needs to be
    /// dealt with by the caller by inspecting the result of a call to [`Shutdown::state`].
    ///
    /// If `Ok(Async::NotReady)` is returned, then the current task will be notified when another
    /// poll needs to occur.
    ///
    /// The error `Err(())` will never be returned.
    pub fn poll(&mut self) -> Poll<(), ()> {
        match self.state() {
            ShutdownState::Running => self.poll_running(),
            ShutdownState::ShuttingDown => self.poll_shutting_down(),
            ShutdownState::Shutdown => Ok(Async::Ready(())),
        }
    }

    /// Polls the shutdown with the assumption that the current state is [`ShutdownState::Running`].
    ///
    /// Specifically, this function will check if a shutdown was initiated by an outside caller via
    /// the sender provided in [`Shutdown::new`]. If the sender is dropped, this will cause an
    /// immediate shutdown to occur.
    ///
    /// # Return Value
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
            .expect("`poll_running` should not be called if `rx_initiate_shutdown` is `None`")
            .poll()
        {
            Ok(Async::Ready(shutdown_type)) => {
                self.handle_shutdown(shutdown_type);
                Ok(Async::Ready(()))
            }
            Err(_) => {
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
    /// # Return Value
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
            .expect("`poll_shutting_down` should not be called if `timer` is `None`")
            .poll()
        {
            Ok(Async::Ready(_)) | Err(_) => {
                self.handle_shutdown(ShutdownType::Immediate);
                Ok(Async::Ready(()))
            }
            Ok(Async::NotReady) => Ok(Async::NotReady),
        }
    }

    /// Returns the current state of the shutdown object (i.e. the shutdown status of the
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

/// The shutdown state of a connection.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownState {
    /// The connection is running normally.
    Running,

    /// The connection is shutdown. No more reading or writing will occur.
    Shutdown,

    /// The connection is shutting down. Only responses can be read or written and all pending
    /// requests must be matched within a time interval. Otherwise, they will be dropped and the
    /// connection will transition to shutdown state.
    ShuttingDown,
}

/// The type of shutdown that the connection will process.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownType {
    /// During the provided duration from the time that this type is processed, only responses can
    /// be read or written. If all pending requests have not been matched by the end of this
    /// duration, they will be dropped, and the shutdown state will transition to
    /// [`ShutdownState::Shutdown`].
    Graceful(Duration),

    /// A complete and immediate shutdown of the connection is to occur. All pending requests will
    /// be dropped and no more messages can be read or written.
    Immediate,
}
