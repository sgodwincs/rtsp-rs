use futures::sync::oneshot;
use futures::{Async, Future, Poll};
use std::time::{Duration, Instant};
use tokio_timer::Delay;

pub struct Shutdown(ShutdownInner);

impl Shutdown {
    pub fn new(
        rx_shutdown: oneshot::Receiver<ShutdownType>,
        shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
    ) -> Self {
        Shutdown(ShutdownInner::new(rx_shutdown, shutdown_future))
    }

    pub fn poll(&mut self) -> Poll<(), ()> {
        self.0.poll()
    }

    pub fn state(&self) -> ShutdownState {
        self.0.state()
    }
}

enum ShutdownInner {
    Running(RunningState),
    Shutdown,
    ShuttingDown(Delay),
}

impl ShutdownInner {
    pub fn new(
        rx_shutdown: oneshot::Receiver<ShutdownType>,
        shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
    ) -> Self {
        ShutdownInner::Running(RunningState {
            rx_shutdown,
            shutdown_future,
        })
    }

    fn handle_shutdown(&mut self, shutdown_type: ShutdownType) {
        match shutdown_type {
            ShutdownType::Graceful(duration) => {
                let expire_time = Instant::now() + duration;
                *self = ShutdownInner::ShuttingDown(Delay::new(expire_time));
            }
            ShutdownType::Immediate => *self = ShutdownInner::Shutdown,
        }
    }

    pub fn poll(&mut self) -> Poll<(), ()> {
        loop {
            match self {
                ShutdownInner::Running(ref mut inner) => match self.poll_shutdown(inner) {
                    Ok(Async::Ready(shutdown_type)) => self.handle_shutdown(shutdown_type),
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                    Err(_) => self.handle_shutdown(ShutdownType::Immediate),
                },
                ShutdownInner::ShuttingDown(ref mut timer) => match timer.poll() {
                    Ok(Async::Ready(_)) | Err(_) => self.handle_shutdown(ShutdownType::Immediate),
                    Ok(Async::NotReady) => return Ok(Async::NotReady),
                },
                ShutdownInner::Shutdown => return Ok(Async::Ready(())),
            }
        }
    }

    fn poll_shutdown(&mut self, state: &mut RunningState) -> Poll<ShutdownType, ()> {
        if let Async::Ready(shutdown_type) = state.shutdown_future.poll()? {
            self.handle_shutdown(shutdown_type);
            return Ok(Async::Ready(shutdown_type));
        }

        if let Async::Ready(shutdown_type) = state.rx_shutdown.poll().map_err(|_| ())? {
            self.handle_shutdown(shutdown_type);
            return Ok(Async::Ready(shutdown_type));
        }

        Ok(Async::NotReady)
    }

    pub fn state(&self) -> ShutdownState {
        match self {
            ShutdownInner::Running(_) => ShutdownState::Running,
            ShutdownInner::ShuttingDown(_) => ShutdownState::ShuttingDown,
            ShutdownInner::Shutdown => ShutdownState::Shutdown,
        }
    }
}

struct RunningState {
    pub rx_shutdown: oneshot::Receiver<ShutdownType>,
    pub shutdown_future: Box<Future<Item = ShutdownType, Error = ()> + Send + 'static>,
}

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
