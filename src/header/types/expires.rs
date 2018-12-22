use chrono::{DateTime, Utc};
use std::ops::{Deref, DerefMut};

use crate::header::common::parse_date_time;
use crate::header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};

/// The `"Expires"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.21).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Expires(pub DateTime<Utc>);

impl Deref for Expires {
    type Target = DateTime<Utc>;

    fn deref(&self) -> &DateTime<Utc> {
        &self.0
    }
}

impl DerefMut for Expires {
    fn deref_mut(&mut self) -> &mut DateTime<Utc> {
        &mut self.0
    }
}

impl TypedHeader for Expires {
    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Expires
    }

    /// Converts the [`Expires`] type to raw header values.
    ///
    /// # Examples
    ///
    /// ```
    /// # #![feature(try_from)]
    /// #
    /// # extern crate chrono;
    /// # extern crate rtsp;
    /// #
    /// use chrono::{TimeZone, Utc};
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Expires;
    ///
    /// let typed_header = Expires(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
    /// let raw_header = vec![
    ///     HeaderValue::try_from("Thu, 10 Jul 2014 09:10:11 +0000").unwrap()
    /// ];
    ///
    /// assert!(typed_header.to_header_raw() == raw_header);
    /// ```
    fn to_header_raw(&self) -> Vec<HeaderValue> {
        // Unsafe Justification
        //
        // Header values must be valid UTF-8, and since we know that the [`Method`] type
        // guarantees valid ASCII-US (with no newlines), it satisfies the constraints.

        let value = self.format("%a, %e %b %Y %H:%M:%S %z").to_string();
        vec![unsafe { HeaderValue::from_str_unchecked(value) }]
    }

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
    /// # #![feature(try_from)]
    /// #
    /// # extern crate chrono;
    /// # extern crate rtsp;
    /// #
    /// use chrono::{TimeZone, Utc};
    /// use std::convert::TryFrom;
    ///
    /// use rtsp::*;
    /// use rtsp::header::types::Expires;
    ///
    /// let typed_header = Expires(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
    /// let raw_header = vec![
    ///     HeaderValue::try_from("Thu, 10 Jul 2014 09:10:11 +0000").unwrap()
    /// ];
    ///
    /// assert!(Expires::try_from_header_raw(&raw_header).unwrap() == typed_header);
    /// ```
    fn try_from_header_raw(header: &[HeaderValue]) -> Result<Self, InvalidTypedHeader> {
        if header.len() == 0 {
            Err(InvalidTypedHeader)
        } else if header.len() > 1 {
            Err(InvalidTypedHeader)
        } else {
            let date_time = parse_date_time(header[0].as_str()).map_err(|_| InvalidTypedHeader)?;
            Ok(Expires(date_time))
        }
    }
}
