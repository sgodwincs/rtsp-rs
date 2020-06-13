use chrono::{DateTime, TimeZone, Utc};
use std::convert::Infallible;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::iter::once;
use std::ops::Deref;

use crate::header::common::date::{self, DateTimeError};
use crate::header::map::TypedHeader;
use crate::header::name::HeaderName;
use crate::header::value::HeaderValue;

/// The `"Expires"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.21).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expires(DateTime<Utc>);

impl Deref for Expires {
    type Target = DateTime<Utc>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<TTimeZone> From<DateTime<TTimeZone>> for Expires
where
    TTimeZone: TimeZone,
{
    fn from(value: DateTime<TTimeZone>) -> Self {
        Expires(value.with_timezone(&Utc))
    }
}

impl TypedHeader for Expires {
    type DecodeError = ExpiresError;

    /// Converts the raw header values to the [`Expires`] header type. Based on the syntax
    /// provided by [RFC7826](https://tools.ietf.org/html/rfc7826#section-20) and
    /// [RFC5322](https://tools.ietf.org/html/rfc5322#section-3.3), this header has the following
    /// syntax.
    ///
    /// ```text
    /// Expires = "Expires" HCOLON RTSP-date
    /// RTSP-date = date-time ;
    /// CR = %x0D ; carriage return
    /// CRLF = CR LF ; Internet standard newline
    /// LF = %x0A ; linefeed
    /// DIGIT = %x30-39 ; 0-9
    /// HTAB = %x09 ; horizontal tab
    /// SP = %x20
    /// VCHAR = %x21-7E ; visible (printing) characters
    /// WSP = SP / HTAB ; white space
    /// FWS = ([*WSP CRLF] 1*WSP) /  obs-FWS ; Folding white space
    /// ctext = %d33-39 /  ; Printable US-ASCII
    ///         %d42-91 /  ;  characters not including
    ///         %d93-126 / ;  "(", ")", or "\"
    ///         obs-ctext
    /// ccontent = ctext / quoted-pair / comment
    /// comment = "(" *([FWS] ccontent) [FWS] ")"
    /// CFWS = (1*([FWS] comment) [FWS]) / FWS
    /// date-time = [ day-of-week "," ] date time [CFWS]
    /// day-of-week = ([FWS] day-name) / obs-day-of-week
    /// day-name = "Mon" / "Tue" / "Wed" / "Thu" / "Fri" / "Sat" / "Sun"
    /// date = day month year
    /// day = ([FWS] 1*2DIGIT FWS) / obs-day
    /// month = "Jan" / "Feb" / "Mar" / "Apr" /
    ///         "May" / "Jun" / "Jul" / "Aug" /
    ///         "Sep" / "Oct" / "Nov" / "Dec"
    /// year = (FWS 4*DIGIT FWS) / obs-year
    /// time = time-of-day zone
    /// time-of-day = hour ":" minute [ ":" second ]
    /// hour = 2DIGIT / obs-hour
    /// minute = 2DIGIT / obs-minute
    /// second = 2DIGIT / obs-second
    /// zone = (FWS ( "+" / "-" ) 4DIGIT) / obs-zone
    ///
    /// obs-FWS = 1*WSP *(CRLF 1*WSP)
    /// obs-NO-WS-CTRL = %d1-8 /   ; US-ASCII control
    ///                  %d11 /    ;  characters that do not
    ///                  %d12 /    ;  include the carriage
    ///                  %d14-31 / ;  return, line feed, and
    ///                  %d127     ;  white space characters
    /// obs-ctext = obs-NO-WS-CTL
    /// obs-qp = "\" (%d0 / obs-NO-WS-CTL / LF / CR)
    /// obs-day-of-week = [CFWS] day-name [CFWS]
    /// obs-day = [CFWS] 1*2DIGIT [CFWS]
    /// obs-year = [CFWS] 2*DIGIT [CFWS]
    /// obs-hour = [CFWS] 2DIGIT [CFWS]
    /// obs-minute = [CFWS] 2DIGIT [CFWS]
    /// obs-second = [CFWS] 2DIGIT [CFWS]
    /// obs-zone = "UT" / "GMT" /  ; Universal Time
    ///                            ; North American UT
    ///                            ; offsets
    ///            "EST" / "EDT" / ; Eastern:  - 5/ - 4
    ///            "CST" / "CDT" / ; Central:  - 6/ - 5
    ///            "MST" / "MDT" / ; Mountain: - 7/ - 6
    ///            "PST" / "PDT" / ; Pacific:  - 8/ - 7
    ///                            ;
    ///            %d65-73 /       ; Military zones - "A"
    ///            %d75-90 /       ; through "I" and "K"
    ///            %d97-105 /      ; through "Z", both
    ///            %d107-122       ; upper and lower case
    /// ```
    ///
    /// Note that all of the syntax is not used due to restrictions on valid RTSP header values.
    /// For example, `obs-ctext` and `obs-qp` are ignored because they allow non-printable ASCII-US
    /// characters whereas header values forbid these values.
    ///
    /// Given the above restrictions above and the requirements from the specification that the
    /// obselete format must be supported, there is some conflict. However, all of the syntax is
    /// adhered to whenever possible. Any deviation from the above syntax will result in a
    /// conversion error.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate chrono;
    /// # extern crate rtsp;
    /// #
    /// use chrono::{TimeZone, Utc};
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::Expires;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let raw_header: Vec<HeaderValue> = vec![];
    /// assert_eq!(Expires::decode(&mut raw_header.iter()).unwrap(), None);
    ///
    /// let typed_header = Expires::from(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
    /// let raw_header = vec![
    ///     HeaderValue::try_from("Thu, 10 Jul 2014 09:10:11 +0000").unwrap()
    /// ];
    /// assert_eq!(Expires::decode(&mut raw_header.iter()).unwrap(), Some(typed_header));
    /// ```
    fn decode<'header, Iter>(values: &mut Iter) -> Result<Option<Self>, Self::DecodeError>
    where
        Iter: Iterator<Item = &'header HeaderValue>,
    {
        let value = match values.next() {
            Some(value) => value,
            None => return Ok(None),
        };

        if values.next().is_some() {
            return Err(ExpiresError::MoreThanOneHeader);
        }

        let date_time = date::parse_date_time(value.as_str())?;
        Ok(Some(Expires(date_time)))
    }

    /// Converts the [`Expires`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # extern crate chrono;
    /// # extern crate rtsp;
    /// #
    /// use chrono::{TimeZone, Utc};
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::header::map::TypedHeader;
    /// use rtsp::header::types::Expires;
    /// use rtsp::header::value::HeaderValue;
    ///
    /// let typed_header = Expires::from(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
    /// let expected_raw_header = vec![
    ///     HeaderValue::try_from("Thu, 10 Jul 2014 09:10:11 +0000").unwrap()
    /// ];
    /// let mut raw_header = vec![];
    /// typed_header.encode(&mut raw_header);
    /// assert_eq!(raw_header, expected_raw_header);
    /// ```
    fn encode<Target>(&self, values: &mut Target)
    where
        Target: Extend<HeaderValue>,
    {
        // Unsafe Justification
        //
        // Header values must be valid UTF-8, and since we know that the [`Method`] type
        // guarantees valid ASCII-US (with no newlines), it satisfies the constraints.

        let value = self.format("%a, %e %b %Y %H:%M:%S %z").to_string();
        values.extend(once(unsafe { HeaderValue::from_string_unchecked(value) }));
    }

    /// Returns the statically assigned [`HeaderName`] for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Expires
    }
}

/// A possible error value when converting to a [`Expires`] from [`HeaderName`]s.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum ExpiresError {
    /// The `"Expires"` header represented an invalid date time.
    InvalidDateTime,

    /// There was more than one `"Expires"` header.
    MoreThanOneHeader,
}

impl Display for ExpiresError {
    fn fmt(&self, formatter: &mut Formatter) -> fmt::Result {
        use self::ExpiresError::*;

        match self {
            InvalidDateTime => write!(formatter, "invalid date time"),
            MoreThanOneHeader => write!(formatter, "more than one expires header"),
        }
    }
}

impl Error for ExpiresError {}

impl From<Infallible> for ExpiresError {
    fn from(_: Infallible) -> Self {
        ExpiresError::InvalidDateTime
    }
}

impl From<DateTimeError> for ExpiresError {
    fn from(_: DateTimeError) -> Self {
        ExpiresError::InvalidDateTime
    }
}
