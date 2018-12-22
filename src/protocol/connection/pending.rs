use bytes::BytesMut;
use futures::sync::mpsc::UnboundedSender;
use futures::sync::oneshot;
use futures::{Async, Future, Poll};
use std::error::Error;
use std::fmt;
use std::time::{Duration, Instant};
use tokio_timer::Delay;

use crate::header::types::CSeq;
use crate::protocol::{OperationError, RequestTimeoutType};
use crate::response::Response;

#[must_use = "futures do nothing unless polled"]
pub struct SendRequestFuture {
    max_timer: Option<Delay>,
    rx_response: oneshot::Receiver<PendingRequestResponse>,
    sequence_number: CSeq,
    timeout_duration: Option<Duration>,
    timer: Option<Delay>,
    tx_pending_request: UnboundedSender<PendingRequestUpdate>,
}

impl SendRequestFuture {
    pub(crate) fn new(
        rx_response: oneshot::Receiver<PendingRequestResponse>,
        tx_pending_request: UnboundedSender<PendingRequestUpdate>,
        sequence_number: CSeq,
        timeout_duration: Option<Duration>,
        max_timeout_duration: Option<Duration>,
    ) -> Self {
        let max_timer = max_timeout_duration.map(|duration| Delay::new(Instant::now() + duration));
        let timer = timeout_duration.map(|duration| Delay::new(Instant::now() + duration));

        SendRequestFuture {
            max_timer,
            rx_response,
            sequence_number,
            timer,
            timeout_duration,
            tx_pending_request,
        }
    }
}

impl Future for SendRequestFuture {
    type Item = Response<BytesMut>;
    type Error = OperationError;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        if let Async::Ready(response) = self
            .rx_response
            .poll()
            .expect("pending request response receiver should not error")
        {
            match response {
                PendingRequestResponse::Continue(rx_response) => {
                    self.rx_response = rx_response;
                    self.timer = self
                        .timeout_duration
                        .map(|duration| Delay::new(Instant::now() + duration));;
                }
                PendingRequestResponse::None => {
                    self.rx_response.close();
                    return Err(OperationError::RequestCancelled);
                }
                PendingRequestResponse::Response(response) => {
                    self.rx_response.close();
                    return Ok(Async::Ready(response));
                }
            }
        }

        if let Some(timer) = self.max_timer.as_mut() {
            if let Async::Ready(_) = timer.poll().expect("max timer should not error") {
                self.tx_pending_request
                    .unbounded_send(PendingRequestUpdate::RemovePendingRequest(
                        self.sequence_number,
                    ))
                    .ok();
                self.rx_response.close();
                return Err(OperationError::RequestTimedOut(RequestTimeoutType::Long));
            }
        }

        if let Some(timer) = self.timer.as_mut() {
            if let Async::Ready(_) = timer.poll().expect("timer should not error") {
                self.tx_pending_request
                    .unbounded_send(PendingRequestUpdate::RemovePendingRequest(
                        self.sequence_number,
                    ))
                    .ok();
                self.rx_response.close();
                return Err(OperationError::RequestTimedOut(RequestTimeoutType::Short));
            }
        }

        Ok(Async::NotReady)
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RequestOptions {
    max_timeout_duration: Option<Duration>,
    timeout_duration: Option<Duration>,
}

impl RequestOptions {
    pub fn builder() -> RequestOptionsBuilder {
        RequestOptionsBuilder::new()
    }

    pub fn new() -> Self {
        RequestOptions::default()
    }

    pub fn max_timeout_duration(&self) -> Option<Duration> {
        self.max_timeout_duration
    }

    pub fn timeout_duration(&self) -> Option<Duration> {
        self.timeout_duration
    }
}

impl Default for RequestOptions {
    fn default() -> Self {
        RequestOptions::builder()
            .build()
            .expect("default request options builder should be valid")
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RequestOptionsBuilder {
    max_timeout_duration: Option<Duration>,
    timeout_duration: Option<Duration>,
}

impl RequestOptionsBuilder {
    pub fn new() -> Self {
        RequestOptionsBuilder::default()
    }

    pub fn build(self) -> Result<RequestOptions, RequestOptionsBuilderError> {
        if let Some(timeout_duration) = self.timeout_duration {
            if timeout_duration.as_secs() == 0 {
                return Err(RequestOptionsBuilderError::InvalidTimeoutDuration);
            }
        }

        if let Some(max_timeout_duration) = self.max_timeout_duration {
            if max_timeout_duration.as_secs() == 0 {
                return Err(RequestOptionsBuilderError::InvalidMaxTimeoutDuration);
            }

            if let Some(timeout_duration) = self.timeout_duration {
                if timeout_duration > max_timeout_duration {
                    return Err(RequestOptionsBuilderError::TimeoutDurationGreaterThanMax);
                }
            }
        }

        Ok(RequestOptions {
            max_timeout_duration: self.max_timeout_duration,
            timeout_duration: self.timeout_duration,
        })
    }

    pub fn max_timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.max_timeout_duration = duration;
        self
    }

    pub fn timeout_duration(&mut self, duration: Option<Duration>) -> &mut Self {
        self.timeout_duration = duration;
        self
    }
}

impl Default for RequestOptionsBuilder {
    fn default() -> Self {
        RequestOptionsBuilder {
            max_timeout_duration: None,
            timeout_duration: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RequestOptionsBuilderError {
    InvalidTimeoutDuration,
    InvalidMaxTimeoutDuration,
    TimeoutDurationGreaterThanMax,
}

impl fmt::Display for RequestOptionsBuilderError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str(self.description())
    }
}

impl Error for RequestOptionsBuilderError {
    fn description(&self) -> &str {
        use self::RequestOptionsBuilderError::*;

        match self {
            InvalidTimeoutDuration => "invalid timeout duration",
            InvalidMaxTimeoutDuration => "invalid max timeout duration",
            TimeoutDurationGreaterThanMax => {
                "timeout duration is greater than the max timeout duration"
            }
        }
    }
}

#[derive(Debug)]
pub enum PendingRequestResponse {
    Continue(oneshot::Receiver<PendingRequestResponse>),
    None,
    Response(Response<BytesMut>),
}

#[derive(Debug)]
pub enum PendingRequestUpdate {
    AddPendingRequest((CSeq, oneshot::Sender<PendingRequestResponse>)),
    RemovePendingRequest(CSeq),
}
