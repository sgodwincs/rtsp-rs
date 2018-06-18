use futures::sync::oneshot;
use futures::{Async, Future, Poll};
use std::time::{Duration, Instant};
use tokio_timer::Delay;

pub struct Shutdown {
    rx_initiate_shutdown: Option<oneshot::Receiver<ShutdownType>>,
    timer: Option<Delay>,
    tx_shutdown_event: Option<oneshot::Sender<()>>,
}

impl Shutdown {
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

    pub fn ensure_shutdown(&mut self) {
        self.handle_shutdown(ShutdownType::Immediate);
    }

    fn handle_shutdown(&mut self, shutdown_type: ShutdownType) {
        match shutdown_type {
            ShutdownType::Graceful(duration) => {
                let expire_time = Instant::now() + duration;
                self.rx_initiate_shutdown = None;
                self.timer = Some(Delay::new(expire_time));
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

    pub fn poll(&mut self) -> Poll<(), ()> {
        match self.state() {
            ShutdownState::Running => self.poll_running(),
            ShutdownState::ShuttingDown => self.poll_shutting_down(),
            ShutdownState::Shutdown => Ok(Async::Ready(())),
        }
    }

    fn poll_running(&mut self) -> Poll<(), ()> {
        if let Async::Ready(shutdown_type) = self
            .rx_initiate_shutdown
            .as_mut()
            .expect("`poll_running` should not be called if `rx_initiate_shutdown` is `None`")
            .poll()
            .map_err(|_| ())?
        {
            self.handle_shutdown(shutdown_type);
            Ok(Async::Ready(()))
        } else {
            Ok(Async::NotReady)
        }
    }

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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownState {
    Running,
    Shutdown,
    ShuttingDown,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ShutdownType {
    Graceful(Duration),
    Immediate,
}
