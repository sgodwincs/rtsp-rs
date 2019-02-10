use regex::Regex;

#[macro_use]
macro_rules! byte_map {
    ($($flag:expr,)*) => ([
        $($flag != 0,)*
    ])
}

lazy_static! {
    static ref QUOTED_STRING_RE: Regex =
        Regex::new(r#""((?:[ !#-\[\]-~]|[^[:ascii:]]|\\"|\\)*)""#).unwrap();
}

#[rustfmt::skip]
pub const TOKEN_CHAR_MAP: [u8; 256] = [
 // 0     1     2     3     4     5     6     7     8     9     A     B     C     D     E     F
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // 0
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // 1
    0, b'!',    0, b'#', b'$', b'%', b'&',b'\'',    0,    0, b'*', b'+',    0, b'-', b'.',    0, // 2
 b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9',    0,    0,    0,    0,    0,    0, // 3
    0, b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O', // 4
 b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W', b'X', b'Y', b'Z',    0,    0,    0, b'^', b'_', // 5
    0, b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', // 6
 b'p', b'q', b'r', b's', b't', b'u', b'v', b'w', b'x', b'y', b'z',    0, b'|',    0, b'~',    0, // 7
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // 8
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // 9
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // A
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // B
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // C
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // D
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // E
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0, // F
];

/// A helper function used to determine whether a given byte slice is a valid token. Tokens as used
/// in [[RFC7826](https://tools.ietf.org/html/rfc782)] are encoded using ASCII-US with a limited
/// subset allowed for use.
pub fn is_token<T>(token: T) -> bool
where
    T: AsRef<[u8]>,
{
    let bytes = token.as_ref();

    if bytes.is_empty() {
        return false;
    }

    for &b in bytes {
        if TOKEN_CHAR_MAP[b as usize] == 0 {
            return false;
        }
    }

    true
}

/// A helper function used to get the inner text out of a quoted string. A quoted string must use
/// double quotes and is encoded using UTF-8 with some exceptions regarding ASCII-US characters. Any
/// use of `'\'` and `'"'` must be escaped with a preceding `'\'`. If the given string is not a
/// valid quoted string, the return value will be `None`.
pub fn extract_quoted_string(string: &str) -> Option<&str> {
    QUOTED_STRING_RE
        .find(string)
        .map(|m| &string[m.start() + 1..m.end() - 1])
}

/// A helper function used to trim whitespace as it is used in
/// [[RFC7826](https://tools.ietf.org/html/rfc7826)]. Specifically, whitespace includes `' '`,
/// `'\t'`, and `"\r\n"`. The trim functions defined on the `str` slice do not seem to be enough to
/// trim the whitespace in a one liner, so this function is used.
pub fn trim_whitespace(string: &str) -> &str {
    trim_whitespace_right(trim_whitespace_left(string))
}

pub fn trim_whitespace_left(mut string: &str) -> &str {
    let mut old = 0;

    while string.len() != old {
        string = string.trim_start_matches("\r\n");
        string = string.trim_start_matches(|c| c == ' ' || c == '\t');
        old = string.len();
    }

    string
}

pub fn trim_whitespace_right(mut string: &str) -> &str {
    let mut old = 0;

    while string.len() != old {
        string = string.trim_end_matches("\r\n");
        string = string.trim_end_matches(|c| c == ' ' || c == '\t');
        old = string.len();
    }

    string
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_is_token() {
        assert_eq!(true, is_token("this-is_a_~token~"));
        assert_eq!(true, is_token("token"));
        assert_eq!(false, is_token("not a token"));
    }

    #[test]
    fn test_extract_quoted_string() {
        assert_eq!(Some(""), extract_quoted_string(r#""""#));
        assert_eq!(Some("a"), extract_quoted_string(r#""a""#));
        assert_eq!(None, extract_quoted_string(r#""this is a test value"#));
        assert_eq!(
            Some("this is a test value"),
            extract_quoted_string(r#""this is a test value""#)
        );
    }

    #[test]
    fn test_trim_whitespace() {
        assert_eq!(
            "this is a test value",
            trim_whitespace("this is a test value")
        );
        assert_eq!(
            "this is a test value",
            trim_whitespace("   this is a test value\t\r\n")
        );
        assert_eq!(
            "this is a test value\n",
            trim_whitespace("this is a test value\n")
        );
    }
}
