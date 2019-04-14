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
pub fn is_token(token: &[u8]) -> bool {
    if token.is_empty() {
        return false;
    }

    for &byte in token {
        if TOKEN_CHAR_MAP[byte as usize] == 0 {
            return false;
        }
    }

    true
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
    use crate::syntax;

    #[test]
    fn test_is_token() {
        assert_eq!(true, syntax::is_token(b"this-is_a_~token~"));
        assert_eq!(true, syntax::is_token(b"token"));
        assert_eq!(false, syntax::is_token(b"not a token"));
    }

    #[test]
    fn test_trim_whitespace() {
        assert_eq!(
            "this is a test value",
            syntax::trim_whitespace("this is a test value")
        );
        assert_eq!(
            "this is a test value",
            syntax::trim_whitespace("   this is a test value\t\r\n")
        );
        assert_eq!(
            "this is a test value\n",
            syntax::trim_whitespace("this is a test value\n")
        );
    }
}
