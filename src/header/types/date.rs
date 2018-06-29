use chrono::{DateTime, Datelike, FixedOffset, TimeZone, Utc, Weekday};
use std::ops::{Deref, DerefMut};
use std::str::Chars;

use header::{HeaderName, HeaderValue, InvalidTypedHeader, TypedHeader};

/// The `"Date"` typed header as described by
/// [RFC7826](https://tools.ietf.org/html/rfc7826#section-18.21).
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Date(pub DateTime<Utc>);

impl Date {
    /// Constructs a new header with no methods by default.
    pub fn new() -> Self {
        Date::default()
    }
}

impl Default for Date {
    fn default() -> Self {
        Date(Utc::now())
    }
}

impl Deref for Date {
    type Target = DateTime<Utc>;

    fn deref(&self) -> &DateTime<Utc> {
        &self.0
    }
}

impl DerefMut for Date {
    fn deref_mut(&mut self) -> &mut DateTime<Utc> {
        &mut self.0
    }
}

impl TypedHeader for Date {
    /// Returns the statically assigned `HeaderName` for this header.
    fn header_name() -> &'static HeaderName {
        &HeaderName::Date
    }

    /// Converts the [`Date`] type to raw header values.
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
    /// use rtsp::header::types::Date;
    ///
    /// let typed_header = Date(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
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

    /// Converts the raw header values to the [`Date`] header type. Based on the syntax
    /// provided by [RFC7826](https://tools.ietf.org/html/rfc7826#section-20) and
    /// [RFC5322](https://tools.ietf.org/html/rfc5322#section-3.3), this header has the following
    /// syntax.
    ///
    /// ```text
    /// Date = "Date" HCOLON RTSP-date
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
    /// use rtsp::header::types::Date;
    ///
    /// let typed_header = Date(Utc.ymd(2014, 7, 10).and_hms(9, 10, 11));
    /// let raw_header = vec![
    ///     HeaderValue::try_from("Thu, 10 Jul 2014 09:10:11 +0000").unwrap()
    /// ];
    ///
    /// assert!(Date::try_from_header_raw(&raw_header).unwrap() == typed_header);
    /// ```
    fn try_from_header_raw(header: &[HeaderValue]) -> Result<Self, InvalidTypedHeader> {
        if header.len() == 0 {
            Err(InvalidTypedHeader)
        } else if header.len() > 1 {
            Err(InvalidTypedHeader)
        } else {
            let date_time = parse_date_time(header[0].as_str())?;
            Ok(Date(date_time))
        }
    }
}

fn get_new_iter<'a, F>(
    value: &'a str,
    parse_function: F,
    bytes_parsed: &mut usize,
) -> Result<Chars<'a>, InvalidTypedHeader>
where
    F: Fn(&str) -> Result<(&str, &str), InvalidTypedHeader>,
{
    let (part_1, part_2) = parse_function(value.split_at(*bytes_parsed).1)?;
    *bytes_parsed += part_1.len();
    Ok(part_2.chars())
}

fn is_comment_text(char_: char) -> bool {
    (char_ >= '!' && char_ <= '\'')
        || (char_ >= '*' && char_ <= '[')
        || (char_ >= ']' && char_ <= '~')
}

fn parse_comment(value: &str) -> Result<(&str, &str), InvalidTypedHeader> {
    let mut bytes_parsed = 0;
    let mut chars = value.chars();

    if chars.next().ok_or(InvalidTypedHeader)? != '(' {
        return Err(InvalidTypedHeader);
    }

    bytes_parsed += 1;

    loop {
        match chars.next().ok_or(InvalidTypedHeader)? {
            ' ' | '\t' => chars = get_new_iter(value, parse_folding_whitespace, &mut bytes_parsed)?,
            '(' => chars = get_new_iter(value, parse_comment, &mut bytes_parsed)?,
            ')' => return Ok(value.split_at(bytes_parsed + 1)),
            '\\' => chars = get_new_iter(value, parse_quoted_pair, &mut bytes_parsed)?,
            char_ if is_comment_text(char_) => bytes_parsed += 1,
            _ => return Err(InvalidTypedHeader),
        }
    }
}

fn parse_comment_and_folding_whitespace(
    value: &str,
    optional: bool,
) -> Result<(&str, &str), InvalidTypedHeader> {
    let mut bytes_parsed = 0;
    let mut chars = value.chars();
    let mut first_found = false;

    loop {
        match chars.next() {
            Some('(') => {
                first_found = true;
                chars = get_new_iter(value, parse_comment, &mut bytes_parsed)?;
            }
            Some(' ') | Some('\t') => {
                first_found = true;
                chars = get_new_iter(value, parse_folding_whitespace, &mut bytes_parsed)?;
            }
            Some(_) | None => {
                return if first_found || optional {
                    Ok(value.split_at(bytes_parsed))
                } else {
                    Err(InvalidTypedHeader)
                };
            }
        }
    }
}

fn parse_date(value: &str) -> Result<(u32, u32, i32, &str), InvalidTypedHeader> {
    let (day, value) = parse_day(value)?;
    let (_, value) = parse_comment_and_folding_whitespace(value, false)?;
    let (month, value) = parse_month(value)?;
    let (_, value) = parse_comment_and_folding_whitespace(value, false)?;
    let (year, value) = parse_year(value)?;

    Ok((day, month, year, value))
}

fn parse_date_time(value: &str) -> Result<DateTime<Utc>, InvalidTypedHeader> {
    let (_, value) = parse_comment_and_folding_whitespace(value, true)?;

    let (weekday, value) = if value
        .chars()
        .next()
        .ok_or(InvalidTypedHeader)?
        .is_ascii_alphabetic()
    {
        let (weekday, value) = parse_day_of_week(value)?;
        let (_, value) = parse_comment_and_folding_whitespace(value, true)?;

        if value.chars().next().ok_or(InvalidTypedHeader)? != ',' {
            return Err(InvalidTypedHeader);
        }

        let (_, value) = parse_comment_and_folding_whitespace(value.split_at(1).1, true)?;

        (Some(weekday), value)
    } else {
        (None, value)
    };

    let (day, month, year, value) = parse_date(value)?;
    let (_, value) = parse_comment_and_folding_whitespace(value, false)?;
    let (hour, minute, second, offset, value) = parse_time(value)?;
    parse_comment_and_folding_whitespace(value, true)?;

    let offset = (offset / 100) * 3600 + (offset % 100) * 60;

    let date_time = FixedOffset::east_opt(offset)
        .ok_or(InvalidTypedHeader)?
        .ymd_opt(year, month, day)
        .and_hms_opt(hour, minute, second.unwrap_or(0))
        .single()
        .ok_or(InvalidTypedHeader)?
        .with_timezone(&Utc);

    if let Some(weekday) = weekday {
        if weekday != date_time.weekday() {
            return Err(InvalidTypedHeader);
        }
    }

    Ok(date_time)
}

fn parse_day(value: &str) -> Result<(u32, &str), InvalidTypedHeader> {
    let mut chars = value.chars();
    let first_digit = chars
        .next()
        .ok_or(InvalidTypedHeader)?
        .to_digit(10)
        .ok_or(InvalidTypedHeader)?;

    match chars.next() {
        Some(second_digit) => {
            if let Some(second_digit) = second_digit.to_digit(10) {
                return Ok((first_digit * 10 + second_digit, value.split_at(2).1));
            }
        }
        _ => (),
    }

    Ok((first_digit, value.split_at(1).1))
}

fn parse_day_of_week(value: &str) -> Result<(Weekday, &str), InvalidTypedHeader> {
    if value.len() < 3 {
        return Err(InvalidTypedHeader);
    }

    let (part_1, part_2) = value.split_at(3);
    let weekday = match part_1.to_lowercase().as_str() {
        "sun" => Weekday::Sun,
        "mon" => Weekday::Mon,
        "tue" => Weekday::Tue,
        "wed" => Weekday::Wed,
        "thu" => Weekday::Thu,
        "fri" => Weekday::Fri,
        "sat" => Weekday::Sat,
        _ => return Err(InvalidTypedHeader),
    };

    Ok((weekday, part_2))
}

fn parse_folding_whitespace(value: &str) -> Result<(&str, &str), InvalidTypedHeader> {
    enum ParseState {
        CRLF,
        InitialWhitespace,
        Whitespace(bool),
    }

    let mut parse_state = ParseState::InitialWhitespace;
    let mut bytes_parsed = 0;
    let mut chars = value.chars().peekable();

    loop {
        match parse_state {
            ParseState::CRLF => match chars.next() {
                Some('\r') => match chars.next() {
                    Some('\n') => {
                        bytes_parsed += 2;
                        parse_state = ParseState::Whitespace(true);
                    }
                    _ => return Err(InvalidTypedHeader),
                },
                None | Some(_) => return Ok(value.split_at(bytes_parsed)),
            },
            ParseState::Whitespace(mut required) => loop {
                match chars.peek() {
                    Some(&' ') | Some(&'\t') => {
                        chars.next();
                        bytes_parsed += 1;
                        required = false;
                    }
                    _ if required => return Err(InvalidTypedHeader),
                    Some(&'\r') => {
                        parse_state = ParseState::CRLF;
                        break;
                    }
                    _ => return Ok(value.split_at(bytes_parsed)),
                }
            },
            ParseState::InitialWhitespace => {
                let char_ = chars.next().ok_or(InvalidTypedHeader)?;
                bytes_parsed += 1;

                if char_ == ' ' || char_ == '\t' {
                    parse_state = ParseState::Whitespace(false);
                } else {
                    return Err(InvalidTypedHeader);
                }
            }
        }
    }
}

fn parse_month(value: &str) -> Result<(u32, &str), InvalidTypedHeader> {
    if value.len() < 3 {
        return Err(InvalidTypedHeader);
    }

    let (part_1, part_2) = value.split_at(3);
    let month = match part_1.to_lowercase().as_str() {
        "jan" => 1,
        "feb" => 2,
        "mar" => 3,
        "apr" => 4,
        "may" => 5,
        "jun" => 6,
        "jul" => 7,
        "aug" => 8,
        "sep" => 9,
        "oct" => 10,
        "nov" => 11,
        "dec" => 12,
        _ => return Err(InvalidTypedHeader),
    };

    Ok((month, part_2))
}

fn parse_quoted_pair(value: &str) -> Result<(&str, &str), InvalidTypedHeader> {
    let mut chars = value.chars();
    let char_1 = chars.next().ok_or(InvalidTypedHeader)?;
    let char_2 = chars.next().ok_or(InvalidTypedHeader)?;

    if char_1 == '\\' && ((char_2 > '!' && char_2 < '~') || char_2 == ' ' || char_2 == '\t') {
        Ok(value.split_at(2))
    } else {
        Err(InvalidTypedHeader)
    }
}

fn parse_time(value: &str) -> Result<(u32, u32, Option<u32>, i32, &str), InvalidTypedHeader> {
    let (hour, minute, second, value) = parse_time_of_day(value)?;
    let (_, value) = parse_comment_and_folding_whitespace(value, false)?;
    let (offset, value) = parse_time_zone(value)?;
    Ok((hour, minute, second, offset, value))
}

fn parse_time_of_day(value: &str) -> Result<(u32, u32, Option<u32>, &str), InvalidTypedHeader> {
    fn parse_2_digit_number(value: &str) -> Result<(u32, &str), InvalidTypedHeader> {
        let mut chars = value.chars();
        let first_digit = chars
            .next()
            .ok_or(InvalidTypedHeader)?
            .to_digit(10)
            .ok_or(InvalidTypedHeader)?;
        let second_digit = chars
            .next()
            .ok_or(InvalidTypedHeader)?
            .to_digit(10)
            .ok_or(InvalidTypedHeader)?;

        Ok((first_digit * 10 + second_digit, value.split_at(2).1))
    }

    fn parse_colon(original_value: &str, optional: bool) -> Result<&str, InvalidTypedHeader> {
        let (_, value) = parse_comment_and_folding_whitespace(original_value, true)?;

        match value.chars().next() {
            Some(':') => Ok(parse_comment_and_folding_whitespace(value.split_at(1).1, true)?.1),
            None | Some(_) => if optional {
                Ok(original_value)
            } else {
                Err(InvalidTypedHeader)
            },
        }
    }

    let (hour, value) = parse_2_digit_number(value)?;
    let value = parse_colon(value, false)?;
    let (minute, value) = parse_2_digit_number(value)?;
    let original_value = value;
    let value = parse_colon(value, true)?;

    if original_value.len() != value.len() {
        let (second, value) = parse_2_digit_number(value)?;
        Ok((hour, minute, Some(second), value))
    } else {
        Ok((hour, minute, None, original_value))
    }
}

fn parse_time_zone(value: &str) -> Result<(i32, &str), InvalidTypedHeader> {
    if value.starts_with('+') || value.starts_with('-') {
        let mut chars = value.chars();
        let sign = chars.next().unwrap();
        let offset = chars.take(4).collect::<String>();

        if offset.len() != 4 {
            return Err(InvalidTypedHeader);
        }

        let mut offset = offset.parse::<i32>().map_err(|_| InvalidTypedHeader)?;

        if sign == '-' {
            offset *= -1;
        }

        Ok((offset, value.split_at(5).1))
    } else {
        let timezone = value
            .chars()
            .take(3)
            .take_while(|&char_| char_.is_ascii_alphabetic())
            .map(|char_| char_.to_ascii_lowercase())
            .collect::<String>();

        let offset = match timezone.as_str() {
            "ut" | "gmt" => 0000,
            "edt" => -0400,
            "est" => -0500,
            "cdt" => -0500,
            "cst" => -0600,
            "mdt" => -0600,
            "mst" => -0700,
            "pdt" => -0700,
            "pst" => -0800,
            string if string.len() == 1 => match string.chars().next().unwrap() {
                'a'..='i' | 'k'..='z' => 0000,
                _ => return Err(InvalidTypedHeader),
            },
            _ => return Err(InvalidTypedHeader),
        };

        Ok((offset, value.split_at(timezone.len()).1))
    }
}

fn parse_year(value: &str) -> Result<(i32, &str), InvalidTypedHeader> {
    let mut bytes_parsed = 0;
    let mut chars = value.chars();
    let mut year: i32 = 0;

    loop {
        match chars.next() {
            Some(char_) => match char_.to_digit(10) {
                Some(digit) => {
                    year = year.checked_mul(10).ok_or(InvalidTypedHeader)? + digit as i32;
                    bytes_parsed += 1;
                }
                None => break,
            },
            None => break,
        }
    }

    match bytes_parsed {
        0 | 1 => return Err(InvalidTypedHeader),
        2 => if year >= 0 && year <= 49 {
            year += 2000;
        } else {
            year += 1900;
        },
        3 => year += 1900,
        _ => (),
    }

    Ok((year, value.split_at(bytes_parsed).1))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_comment() {
        assert_eq!(
            parse_comment("(test comment \r\n more comments\t) test"),
            Ok(("(test comment \r\n more comments\t)", " test"))
        );
        assert_eq!(parse_comment("()"), Ok(("()", "")));
        assert_eq!(
            parse_comment("(\\\"comment inside quotes\\\") test"),
            Ok(("(\\\"comment inside quotes\\\")", " test"))
        );
        assert_eq!(
            parse_comment("(here (are \r\n (some nested\t(comments))))"),
            Ok(("(here (are \r\n (some nested\t(comments))))", ""))
        );
    }

    #[test]
    fn test_parse_comment_and_folding_whitespace() {
        assert_eq!(
            parse_comment_and_folding_whitespace(" ", false),
            Ok((" ", ""))
        );
        assert_eq!(
            parse_comment_and_folding_whitespace("(comment) test", false),
            Ok(("(comment) ", "test"))
        );
        assert_eq!(
            parse_comment_and_folding_whitespace("((())) \r\n test", false),
            Ok(("((())) \r\n ", "test"))
        );
        assert_eq!(parse_comment_and_folding_whitespace("", true), Ok(("", "")));
        assert_eq!(
            parse_comment_and_folding_whitespace("test", true),
            Ok(("", "test"))
        );
        assert_eq!(
            parse_comment_and_folding_whitespace(" test", true),
            Ok((" ", "test"))
        );
        assert!(parse_comment_and_folding_whitespace("", false).is_err());
        assert!(parse_comment_and_folding_whitespace("test", false).is_err());
    }

    #[test]
    fn test_parse_date() {
        assert_eq!(parse_date("1 Jan 2018"), Ok((1, 1, 2018, "")));
        assert_eq!(parse_date("99 Feb 60"), Ok((99, 2, 1960, "")));
        assert_eq!(parse_date("09 Mar 20"), Ok((9, 3, 2020, "")));
        assert_eq!(
            parse_date("60       Dec \r\n\t130test"),
            Ok((60, 12, 2030, "test"))
        );
        assert!(parse_date("999 Jan 2018").is_err());
        assert!(parse_date("   1 Jan 2018").is_err());
        assert!(parse_date("1Jan 2018").is_err());
        assert!(parse_date("1 Jan2018").is_err());
        assert!(parse_date("1 February 2018").is_err());
    }

    #[test]
    fn test_parse_date_time() {
        assert_eq!(
            parse_date_time("Mon, 1 Jan 2018 08:00:00 +0000"),
            Ok(Utc.ymd(2018, 1, 1).and_hms(8, 0, 0))
        );
        assert_eq!(
            parse_date_time("1 Jan 2018 08:00 EDT"),
            Ok(Utc.ymd(2018, 1, 1).and_hms(12, 0, 0))
        );
        assert_eq!(
            parse_date_time(" \r\n\tTUE  ,   02 jAn()118 00:00 z(nested (comment))"),
            Ok(Utc.ymd(2018, 1, 2).and_hms(0, 0, 0))
        );
    }

    #[test]
    fn test_parse_day() {
        assert_eq!(parse_day("0"), Ok((0, "")));
        assert_eq!(parse_day("99"), Ok((99, "")));
        assert_eq!(parse_day("10000"), Ok((10, "000")));
        assert_eq!(parse_day("5test"), Ok((5, "test")));
        assert_eq!(parse_day("00test"), Ok((0, "test")));
        assert!(parse_day("").is_err());
        assert!(parse_day("test").is_err());
    }

    #[test]
    fn test_parse_day_of_week() {
        assert_eq!(parse_day_of_week("SuN,"), Ok((Weekday::Sun, ",")));
        assert_eq!(parse_day_of_week("Mon"), Ok((Weekday::Mon, "")));
        assert_eq!(parse_day_of_week("Tue test"), Ok((Weekday::Tue, " test")));
        assert_eq!(parse_day_of_week("wed"), Ok((Weekday::Wed, "")));
        assert_eq!(parse_day_of_week("THU"), Ok((Weekday::Thu, "")));
        assert_eq!(parse_day_of_week("friasd"), Ok((Weekday::Fri, "asd")));
        assert_eq!(parse_day_of_week("sAturDay"), Ok((Weekday::Sat, "urDay")));
        assert!(parse_day_of_week("").is_err());
        assert!(parse_day_of_week("test").is_err());
    }

    #[test]
    fn test_parse_folding_whitespace() {
        assert_eq!(parse_folding_whitespace(" \r\n\t "), Ok((" \r\n\t ", "")));
        assert_eq!(parse_folding_whitespace(" \n "), Ok((" ", "\n ")));
        assert_eq!(
            parse_folding_whitespace("\t    \r\n test"),
            Ok(("\t    \r\n ", "test"))
        );
        assert!(parse_folding_whitespace("\r\n ").is_err());
        assert!(parse_folding_whitespace(" \r\n").is_err());
        assert!(parse_folding_whitespace("\t\r\n \r").is_err());
        assert!(parse_folding_whitespace(" \r ").is_err());
    }

    #[test]
    fn test_parse_month() {
        assert_eq!(parse_month("JaN,"), Ok((1, ",")));
        assert_eq!(parse_month("Feb"), Ok((2, "")));
        assert_eq!(parse_month("Mar test"), Ok((3, " test")));
        assert_eq!(parse_month("apr"), Ok((4, "")));
        assert_eq!(parse_month("MAY"), Ok((5, "")));
        assert_eq!(parse_month("junasd"), Ok((6, "asd")));
        assert_eq!(parse_month("jUly"), Ok((7, "y")));
        assert!(parse_month("").is_err());
        assert!(parse_month("test").is_err());
    }

    #[test]
    fn test_parse_quoted_pair() {
        assert_eq!(parse_quoted_pair("\\\""), Ok(("\\\"", "")));
        assert_eq!(parse_quoted_pair("\\\\test"), Ok(("\\\\", "test")));
        assert!(parse_quoted_pair("\\\n").is_err());
        assert!(parse_quoted_pair("\\\u{3041}").is_err());
    }

    #[test]
    fn test_parse_time() {
        assert_eq!(parse_time("10:30:44 +0000"), Ok((10, 30, Some(44), 0, "")));
        assert_eq!(parse_time("10:30 EDT"), Ok((10, 30, None, -400, "")));
        assert!(parse_time("").is_err());
    }

    #[test]
    fn test_parse_time_of_day() {
        assert_eq!(parse_time_of_day("08:52:32"), Ok((8, 52, Some(32), "")));
        assert_eq!(
            parse_time_of_day("99 : \r\n\t99(comment): 99test"),
            Ok((99, 99, Some(99), "test"))
        );
        assert_eq!(parse_time_of_day("23:09"), Ok((23, 9, None, "")));
        assert_eq!(
            parse_time_of_day("23:09 \r\n (comment) test:"),
            Ok((23, 9, None, " \r\n (comment) test:"))
        );
    }

    #[test]
    fn test_parse_time_zone() {
        assert_eq!(parse_time_zone("+0000"), Ok((0, "")));
        assert_eq!(parse_time_zone("-0700"), Ok((-700, "")));
        assert_eq!(parse_time_zone("+9999test"), Ok((9999, "test")));
        assert_eq!(parse_time_zone("EDT"), Ok((-400, "")));
        assert_eq!(parse_time_zone("psttest"), Ok((-800, "test")));
        assert_eq!(parse_time_zone("a"), Ok((0, "")));
        assert_eq!(parse_time_zone("z"), Ok((0, "")));
        assert!(parse_time_zone("aaa").is_err());
        assert!(parse_time_zone("j").is_err());
    }

    #[test]
    fn test_parse_year() {
        assert_eq!(parse_year("2018"), Ok((2018, "")));
        assert_eq!(parse_year("1950test"), Ok((1950, "test")));
        assert_eq!(parse_year("105930"), Ok((105930, "")));
        assert_eq!(parse_year("00"), Ok((2000, "")));
        assert_eq!(parse_year("62"), Ok((1962, "")));
        assert_eq!(parse_year("182"), Ok((2082, "")));
        assert!(parse_year("").is_err());
        assert!(parse_year("9").is_err());
        assert!(parse_year("9999999999999999999").is_err());
    }
}
