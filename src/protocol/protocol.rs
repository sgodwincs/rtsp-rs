use bytes::BytesMut;
use futures::future::{empty, loop_fn, Either, Empty, Loop};
use futures::prelude::*;
use futures::sync::mpsc::{channel, unbounded, Sender, UnboundedReceiver, UnboundedSender};
use std::sync::{Arc, Mutex, MutexGuard};
use std::time::{Duration, Instant};
use tokio_executor::{DefaultExecutor, Executor, SpawnError};
use tokio_io::{AsyncRead, AsyncWrite};
use tokio_timer::Delay;

use protocol::{Codec, CodecEvent, Error, Message, MessageResult};
use request::Request;
use response::Response;
use status::StatusCode;

#[derive(Debug)]
pub struct Protocol {
    state: Arc<Mutex<ProtocolState>>,
}

impl Protocol {
    pub fn new<IO>(io: IO) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite,
    {
        Protocol::with_config(io, Config::new())
    }

    pub fn with_config<IO>(io: IO, config: Config) -> Result<Self, SpawnError>
    where
        IO: AsyncRead + AsyncWrite,
    {
        let mut executor = DefaultExecutor::current();
        let (tx_state_change, rx_state_change) = unbounded();
        let (tx_codec_event, rx_codec_event) = unbounded();
        let state = Arc::new(Mutex::new(ProtocolState::new(tx_state_change)));
        let (sink, stream) = io.framed(Codec::with_events(tx_codec_event)).split();

        executor.spawn(Box::new(create_decoding_timer_task(
            state.clone(),
            rx_codec_event,
        )))?;

        Ok(Protocol { state })
    }
}

#[derive(Default)]
pub struct Config;

impl Config {
    pub fn new() -> Self {
        Config
    }
}

#[derive(Clone, Debug)]
pub enum ReadWriteState {
    All,
    Error(Error),
    Request,
    Response,
    None,
}

impl ReadWriteState {
    pub fn is_all(&self) -> bool {
        match *self {
            ReadWriteState::All => true,
            _ => false,
        }
    }

    pub fn is_request(&self) -> bool {
        match *self {
            ReadWriteState::Request => true,
            _ => false,
        }
    }

    pub fn is_response(&self) -> bool {
        match *self {
            ReadWriteState::Response => true,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        match *self {
            ReadWriteState::None => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match *self {
            ReadWriteState::Error(_) => true,
            _ => false,
        }
    }

    pub fn requests_allowed(&self) -> bool {
        self.is_all() || self.is_request()
    }

    pub fn responses_allowed(&self) -> bool {
        self.is_all() || self.is_response()
    }
}

impl Eq for ReadWriteState {}

impl PartialEq for ReadWriteState {
    fn eq(&self, other: &ReadWriteState) -> bool {
        use self::ReadWriteState::*;

        match *self {
            All => other.is_all(),
            Request => other.is_request(),
            Response => other.is_response(),
            None => other.is_none(),
            Error(_) => other.is_error(),
        }
    }
}

fn try_update_state(current: &mut ReadWriteState, new: ReadWriteState) -> bool {
    use self::ReadWriteState::*;

    match new {
        All => false,
        Request => if current.is_all() {
            *current = ReadWriteState::Request;
            true
        } else if current.is_response() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        Response => if current.is_all() {
            *current = ReadWriteState::Response;
            true
        } else if current.is_request() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        None => if !current.is_none() && !current.is_error() {
            *current = ReadWriteState::None;
            true
        } else {
            false
        },
        Error(error) => if !current.is_error() {
            *current = ReadWriteState::Error(error);
            true
        } else {
            false
        },
    }
}

#[derive(Debug)]
pub struct ProtocolState {
    read_state: ReadWriteState,
    tx_state_change: UnboundedSender<(ReadWriteState, ReadWriteState)>,
    write_state: ReadWriteState,
}

impl ProtocolState {
    pub fn new(tx_state_change: UnboundedSender<(ReadWriteState, ReadWriteState)>) -> Self {
        ProtocolState {
            read_state: ReadWriteState::All,
            tx_state_change,
            write_state: ReadWriteState::All,
        }
    }

    pub fn read_state(&self) -> ReadWriteState {
        self.read_state.clone()
    }

    pub fn state(&self) -> (ReadWriteState, ReadWriteState) {
        (self.read_state(), self.write_state())
    }

    pub fn update_read_state(&mut self, read_state: ReadWriteState) {
        if try_update_state(&mut self.read_state, read_state) {
            self.tx_state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn update_state(&mut self, read_state: ReadWriteState, write_state: ReadWriteState) {
        self.update_read_state(read_state);
        self.update_write_state(write_state);
    }

    pub fn update_write_state(&mut self, write_state: ReadWriteState) {
        if try_update_state(&mut self.write_state, write_state) {
            self.tx_state_change.unbounded_send(self.state()).ok();
        }
    }

    pub fn write_state(&self) -> ReadWriteState {
        self.write_state.clone()
    }
}

fn lock_state(state: &Arc<Mutex<ProtocolState>>) -> MutexGuard<ProtocolState> {
    state.lock().expect("acquiring state lock should not fail")
}

/// Constructs a task that manages the timer for decoding messages.
///
/// If the timer expires, then messages can no longer be read from the connection. But writing
/// responses is still allowed as long as the protocol state had not already passed that point.
///
/// Since the timer is managed via events from the codec, if the stream of the events end, then it
/// is implied that the connection is completely closed.
///
/// This task will only end if the timer expires or if the codec is dropped.
///
/// # Arguments
///
/// * `state` - A reference to the protocol state.
/// * `rx_codec_event` - A stream of [`CodecEvent`]s.
fn create_decoding_timer_task(
    state: Arc<Mutex<ProtocolState>>,
    rx_codec_event: UnboundedReceiver<CodecEvent>,
) -> impl Future<Item = (), Error = ()> {
    loop_fn(
        (Either::A(empty()), rx_codec_event, state),
        |(timer, rx_codec_event, state)| {
            timer
                .select2(rx_codec_event.into_future())
                .map_err(|_| panic!("decode timer and codec event receivers should not error"))
                .and_then(|item| {
                    Ok(match item {
                        Either::A(_) => {
                            // Decoding timer has expired. At this point, we consider any reads from
                            // the connection to be dead. The only writing allowed now are
                            // responses, since we would not be able to get any of the responses
                            // back for any requests we were to send.

                            lock_state(&state).update_state(
                                ReadWriteState::Error(Error::DecodingTimedOut),
                                ReadWriteState::Response,
                            );
                            Loop::Break(())
                        }
                        Either::B(((None, _), _)) => {
                            // The codec that handles decoding and encoding has been dropped. This
                            // implies that the connection is completely closed. It is likely that
                            // the protocol state has already been updated by this point, but we do
                            // it just in case, since this is the final protocol state.

                            lock_state(&state)
                                .update_state(ReadWriteState::None, ReadWriteState::None);
                            Loop::Break(())
                        }
                        Either::B(((Some(event), rx_codec_event), timer)) => match event {
                            CodecEvent::DecodingStarted => {
                                let expire_time = Instant::now() + Duration::from_secs(10);
                                let timer = Either::B(Delay::new(expire_time));
                                Loop::Continue((timer, rx_codec_event, state))
                            }
                            CodecEvent::DecodingEnded => {
                                Loop::Continue((Either::A(empty()), rx_codec_event, state))
                            }
                            _ => Loop::Continue((timer, rx_codec_event, state)),
                        },
                    })
                })
        },
    )
}
