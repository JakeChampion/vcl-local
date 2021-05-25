use hexponent::FloatLiteral;
use std::collections::HashMap;
use std::fmt;
use std::str::FromStr;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Plus,
    Semicolon,
    Colon,
    Slash,
    Star,

    // https://developer.fastly.com/reference/vcl/operators/
    // Conditional Operators
    Bang,
    And,
    Or,
    EqualEqual,
    BangEqual,
    Tilde,
    BangTilde,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Assignment operators
    Equal,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulus,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    LeftShift,
    RightShift,
    LeftRotate,
    RightRotate,
    LogicalAnd,
    LogicalOr,

    // Literals.
    Identifier,
    String,
    Integer,
    Float,
    Duration,
    AclEntry,

    // Keywords.
    Add,
    Synthetic,
    SyntheticBase64,
    Restart,
    Log,
    Remove,
    Esi,
    Error,
    Call,
    Else,
    ElseIf,
    False,
    Sub,
    If,
    Set,
    Unset,
    Return,
    True,
    Declare,
    Eof,
    Acl,
    Backend,
    Director,
    Include,
    Table,
    Pragma,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Identifier(String),
    Str(String),
    Float(f64),
    Integer(i64),
    Duration(f64, DurationUnit),
    AclEntry(String, u8),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DurationUnit {
    Milliseconds,
    Seconds,
    Minutes,
    Hours,
    Days,
    Years,
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub ty: TokenType,
    pub lexeme: Vec<u8>,
    pub literal: Option<Literal>,
    pub line: usize,
    pub col: i64,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token {{ ty: {:?}, lexeme: \"{}\", literal: {:?}, line: {:?}, col: {:?}}}",
            self.ty,
            String::from_utf8(self.lexeme.clone()).unwrap(),
            self.literal,
            self.line,
            self.col
        )
    }
}

pub fn scan_tokens(input: String) -> Result<Vec<Token>, String> {
    let mut scanner: Scanner = Scanner::default();

    scanner.scan_tokens(input);

    match scanner.err {
        Some(err) => Err(err),
        None => Ok(scanner.tokens),
    }
}

struct Scanner {
    source: Vec<u8>,
    tokens: Vec<Token>,
    err: Option<String>,
    start: usize,
    current: usize,
    line: usize,
    col: i64,
    keywords: HashMap<String, TokenType>,
}

impl Default for Scanner {
    fn default() -> Self {
        Self {
            source: Vec::new(),
            tokens: Vec::new(),
            err: None,
            start: 0,
            current: 0,
            line: 1,
            col: -1,
            keywords: vec![
                ("add", TokenType::Add),
                ("call", TokenType::Call),
                ("error", TokenType::Error),
                ("esi", TokenType::Esi),
                ("else", TokenType::Else),
                ("elseif", TokenType::ElseIf),
                ("elsif", TokenType::ElseIf),
                ("false", TokenType::False),
                ("if", TokenType::If),
                ("include", TokenType::Include),
                ("log", TokenType::Log),
                ("remove", TokenType::Remove),
                ("restart", TokenType::Restart),
                ("return", TokenType::Return),
                ("set", TokenType::Set),
                ("synthetic.base64", TokenType::SyntheticBase64),
                ("synthetic", TokenType::Synthetic),
                ("sub", TokenType::Sub),
                ("unset", TokenType::Unset),
                ("true", TokenType::True),
                ("acl", TokenType::Acl),
                ("backend", TokenType::Backend),
                ("director", TokenType::Director),
                ("table", TokenType::Table),
                ("pragma", TokenType::Pragma),
            ]
            .into_iter()
            .map(|(k, v)| (String::from(k), v))
            .collect(),
        }
    }
}

impl Scanner {
    fn scan_tokens(&mut self, input: String) {
        self.source = input.into_bytes();

        while !self.done() {
            self.start = self.current;
            self.scan_token();
        }

        match self.err {
            Some(_) => {}
            None => self.tokens.push(Token {
                ty: TokenType::Eof,
                lexeme: Vec::new(),
                literal: None,
                line: self.line,
                col: self.col,
            }),
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.col += 1;

        char::from(self.source[self.current - 1])
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => {
                // TODO: support heredoc in long string
                // https://developer.fastly.com/reference/vcl/types/string/
                if self.matches('"') {
                    self.long_string()
                } else {
                    self.add_token(TokenType::LeftBrace)
                }
            }
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => {
                if self.matches('=') {
                    self.add_token(TokenType::Subtraction)
                } else if Self::is_decimal_digit(self.peek()) {
                    self.number_or_duration()
                }
            }
            '%' => {
                if self.matches('=') {
                    self.add_token(TokenType::Modulus)
                }
            }
            '^' => {
                if self.matches('=') {
                    self.add_token(TokenType::BitwiseXor)
                }
            }
            '+' => {
                if self.matches('=') {
                    self.add_token(TokenType::Addition)
                } else {
                    self.add_token(TokenType::Plus)
                }
            }
            ';' => self.add_token(TokenType::Semicolon),
            ':' => self.add_token(TokenType::Colon),
            '*' => {
                if self.matches('=') {
                    self.add_token(TokenType::Multiplication)
                } else {
                    self.add_token(TokenType::Star)
                }
            }
            '~' => self.add_token(TokenType::Tilde),
            '!' => {
                if self.matches('=') {
                    self.add_token(TokenType::BangEqual)
                } else if self.matches('~') {
                    self.add_token(TokenType::BangTilde)
                } else {
                    self.add_token(TokenType::Bang)
                }
            }
            '=' => {
                let matches_eq = self.matches('=');
                self.add_token(if matches_eq {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                })
            }
            '|' => {
                if self.matches('|') {
                    if self.matches('=') {
                        self.add_token(TokenType::LogicalOr)
                    } else {
                        self.add_token(TokenType::Or)
                    }
                } else if self.matches('=') {
                    self.add_token(TokenType::BitwiseOr)
                } else {
                    panic!("reserved but not in use");
                }
            }
            '&' => {
                if self.matches('&') {
                    if self.matches('=') {
                        self.add_token(TokenType::LogicalAnd)
                    } else {
                        self.add_token(TokenType::And)
                    }
                } else if self.matches('=') {
                    self.add_token(TokenType::BitwiseAnd)
                } else {
                    panic!("reserved but not in use");
                }
            }
            '<' => {
                if self.matches('=') {
                    self.add_token(TokenType::LessEqual)
                } else if self.matches('<') && self.matches('=') {
                    self.add_token(TokenType::LeftShift)
                } else {
                    self.add_token(TokenType::Less)
                }
            }
            '>' => {
                if self.matches('=') {
                    self.add_token(TokenType::GreaterEqual)
                } else if self.matches('>') && self.matches('=') {
                    self.add_token(TokenType::RightShift)
                } else {
                    self.add_token(TokenType::Greater)
                }
            }
            '/' => {
                if self.matches('=') {
                    self.add_token(TokenType::Division)
                } else if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.matches('*') {
                    while (self.peek() != '*' && self.peek_next() != '/') && !self.is_at_end() {
                        self.advance();
                    }
                    if !self.is_at_end() {
                        self.advance();
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash)
                }
            }
            '#' => {
                while self.peek() != '\n' && !self.is_at_end() {
                    self.advance();
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
                self.col = 0
            }
            '"' => {
                // TODO: Handle percent encoding and null bytes early string termination
                // https://developer.fastly.com/reference/vcl/types/string/
                self.string_or_acl()
            }
            _ => {
                if Self::is_decimal_digit(c) {
                    self.number_or_duration()
                } else if Self::is_alpha(c) {
                    if let Some(token) = self.tokens.last() {
                        if token.ty == TokenType::Dot {
                            return self.identifier();
                        }
                    }
                    self.identifier_or_keyword_or_silly_assignment()
                } else {
                    self.err = Some(format!("scanner can't handle {}", c))
                }
            }
        }
    }

    fn is_alpha(c: char) -> bool {
        c.is_alphabetic()
    }

    fn is_decimal_digit(c: char) -> bool {
        c.is_digit(10)
    }

    fn is_hex_digit(c: char) -> bool {
        c.is_digit(16)
    }

    fn is_alphanumericunderscoredash(c: char) -> bool {
        Self::is_alpha(c) || Self::is_decimal_digit(c) || c == '_' || c == '-'
    }

    fn identifier(&mut self) {
        while Self::is_alphanumericunderscoredash(self.peek()) {
            self.advance();
        }
        let literal_val =
            String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();

        self.add_token_literal(
            TokenType::Identifier,
            Some(Literal::Identifier(literal_val)),
        )
    }

    fn identifier_or_keyword_or_silly_assignment(&mut self) {
        if self.previous() == 'r' && self.peek() == 'o' {
            if self.peek_at(1) == 'l' && self.peek_at(2) == '=' {
                self.advance();
                self.advance();
                self.advance();
                return self.add_token(TokenType::LeftRotate);
            } else if self.peek_at(1) == 'r' && self.peek_at(2) == '=' {
                self.advance();
                self.advance();
                self.advance();
                return self.add_token(TokenType::RightRotate);
            }
        } else if self.previous() == 'd'
            && self.peek() == 'e'
            && self.peek_at(1) == 'c'
            && self.peek_at(2) == 'l'
            && self.peek_at(3) == 'a'
            && self.peek_at(4) == 'r'
            && self.peek_at(5) == 'e'
            && self.peek_at(6) == ' '
            && self.peek_at(7) == 'l'
            && self.peek_at(8) == 'o'
            && self.peek_at(9) == 'c'
            && self.peek_at(10) == 'a'
            && self.peek_at(11) == 'l'
        {
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            return self.add_token(TokenType::Declare);
        } else if self.previous() == 's'
            && self.peek() == 'y'
            && self.peek_at(1) == 'n'
            && self.peek_at(2) == 't'
            && self.peek_at(3) == 'h'
            && self.peek_at(4) == 'e'
            && self.peek_at(5) == 't'
            && self.peek_at(6) == 'i'
            && self.peek_at(7) == 'c'
            && self.peek_at(8) == '.'
            && self.peek_at(9) == 'b'
            && self.peek_at(10) == 'a'
            && self.peek_at(11) == 's'
            && self.peek_at(12) == 'e'
            && self.peek_at(13) == '6'
            && self.peek_at(14) == '4'
        {
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            self.advance();
            return self.add_token(TokenType::SyntheticBase64);
        }

        while Self::is_alphanumericunderscoredash(self.peek()) {
            self.advance();
        }
        let literal_val =
            String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
        let token_type = if self.peek() == '.' {
            TokenType::Identifier
        } else {
            match self.keywords.get(&literal_val) {
                Some(kw_token_type) => *kw_token_type,
                None => {
                    if literal_val == "LF" {
                        TokenType::String
                    } else {
                        TokenType::Identifier
                    }
                }
            }
        };

        match token_type {
            TokenType::Identifier => self.add_token_literal(
                TokenType::Identifier,
                Some(Literal::Identifier(literal_val)),
            ),
            TokenType::String => {
                self.add_token_literal(TokenType::String, Some(Literal::Str("\n".to_string())))
            }
            _ => self.add_token(token_type),
        }
    }

    fn number_or_duration(&mut self) {
        let is_neg = self.previous() == '-';
        if is_neg {
            self.advance();
        }
        let is_hex = self.previous() == '0' && self.peek() == 'x';
        if is_hex {
            self.advance();
            while Self::is_hex_digit(self.peek()) {
                self.advance();
            }
        } else {
            while Self::is_decimal_digit(self.peek()) {
                self.advance();
            }
        }

        if self.peek() == 's'
            || (self.peek() == 'm' && self.peek_next() == 's')
            || self.peek() == 'm'
            || self.peek() == 'h'
            || self.peek() == 'd'
            || self.peek() == 'y'
        {
            let val: f64 = String::from_utf8(self.source[self.start..self.current].to_vec())
                .unwrap()
                .parse()
                .unwrap();
            let unit = match self.peek() {
                's' => DurationUnit::Seconds,
                'm' => {
                    if self.peek_next() == 's' {
                        DurationUnit::Milliseconds
                    } else {
                        DurationUnit::Minutes
                    }
                }
                'h' => DurationUnit::Hours,
                'd' => DurationUnit::Days,
                'y' => DurationUnit::Years,
                _ => unreachable!(),
            };
            if unit == DurationUnit::Milliseconds {
                self.advance();
            }
            self.advance();
            return self.add_token_literal(TokenType::Duration, Some(Literal::Duration(val, unit)));
        }

        let int_lit = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();

        if self.matches('.') {
            if is_hex {
                while Self::is_hex_digit(self.peek()) {
                    self.advance();
                }
            } else {
                while Self::is_decimal_digit(self.peek()) {
                    self.advance();
                }
            }
            let float_lit =
                String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
            if self.matches(if is_hex { 'p' } else { 'e' }) {
                if self.peek() == '-' || self.peek() == '+' {
                    self.advance();
                }
                while Self::is_decimal_digit(self.peek()) {
                    self.advance();
                }
                let lit =
                    String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
                let num = if is_hex {
                    let float_repr: FloatLiteral = lit.parse().unwrap();
                    float_repr.convert::<f64>().inner()
                } else {
                    f64::from_str(&lit).unwrap()
                };
                self.add_token_literal(TokenType::Float, Some(Literal::Float(num)))
            } else {
                let num = if is_hex {
                    let float_repr: FloatLiteral = float_lit.parse().unwrap();
                    float_repr.convert::<f64>().inner()
                } else {
                    f64::from_str(&float_lit).unwrap()
                };
                self.add_token_literal(TokenType::Float, Some(Literal::Float(num)))
            }
        } else if self.matches(if is_hex { 'p' } else { 'e' }) {
            if self.peek() == '-' || self.peek() == '+' {
                self.advance();
            }
            while Self::is_decimal_digit(self.peek()) {
                self.advance();
            }
            let lit = String::from_utf8(self.source[self.start..self.current].to_vec()).unwrap();
            let num = if is_hex {
                let float_repr: FloatLiteral = "0x3.4".parse().unwrap();
                float_repr.convert::<f64>().inner()
            } else {
                f64::from_str(&lit).unwrap()
            };
            self.add_token_literal(TokenType::Float, Some(Literal::Float(num)))
        } else {
            let num = if is_hex {
                let hex_without_suffix = if is_neg {
                    format!("-{}", int_lit.trim_start_matches("-0x"))
                } else {
                    int_lit.trim_start_matches("0x").to_string()
                };
                i64::from_str_radix(&hex_without_suffix, 16).unwrap()
            } else {
                int_lit.parse::<i64>().unwrap()
            };
            self.add_token_literal(TokenType::Integer, Some(Literal::Integer(num)))
        }
    }

    fn string_or_acl(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(format!("Unterminated string at line {}", self.line))
        }

        assert!(self.peek() == '"', "line: {} col: {}", self.line, self.col);

        self.advance();

        if self.peek() == '/' {
            let ip =
                String::from_utf8(self.source[self.start + 1..self.current - 1].to_vec()).unwrap();
            self.start = self.current;
            self.advance();
            while Self::is_decimal_digit(self.peek()) && !self.is_at_end() {
                self.advance();
            }
            let range: u8 = String::from_utf8(self.source[self.start + 1..self.current].to_vec())
                .unwrap()
                .parse()
                .unwrap();
            self.add_token_literal(TokenType::AclEntry, Some(Literal::AclEntry(ip, range)))
        } else {
            self.add_token_literal(
                TokenType::String,
                Some(Literal::Str(
                    String::from_utf8(self.source[self.start + 1..self.current - 1].to_vec())
                        .unwrap(),
                )),
            )
        }
    }

    fn long_string(&mut self) {
        loop {
            if self.peek() == '"' && self.peek_next() == '}' {
                break;
            }
            if self.is_at_end() {
                break;
            }
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }

        if self.is_at_end() {
            self.err = Some(format!("Unterminated long string at line {}", self.line))
        }

        assert!(self.peek() == '"', "line: {} col: {}", self.line, self.col);
        assert!(
            self.peek_next() == '}',
            "line: {} col: {}",
            self.line,
            self.col
        );

        self.advance();
        self.advance();

        self.add_token_literal(
            TokenType::String,
            Some(Literal::Str(
                String::from_utf8(self.source[self.start + 1..self.current - 1].to_vec()).unwrap(),
            )),
        )
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            char::from(self.source[self.current + 1])
        }
    }

    fn peek_at(&self, count: usize) -> char {
        if self.current + count >= self.source.len() {
            '\0'
        } else {
            char::from(self.source[self.current + count])
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            char::from(self.source[self.current])
        }
    }
    fn previous(&self) -> char {
        if self.current == 0 {
            '\0'
        } else {
            char::from(self.source[self.current - 1])
        }
    }

    fn matches(&mut self, c: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if char::from(self.source[self.current]) != c {
            return false;
        }

        self.current += 1;
        self.col += 1;
        true
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.add_token_literal(token_type, None)
    }

    fn add_token_literal(&mut self, token_type: TokenType, literal: Option<Literal>) {
        let text = self.source[self.start..self.current].to_vec();

        self.tokens.push(Token {
            ty: token_type,
            lexeme: text,
            literal,
            line: self.line,
            col: self.col,
        })
    }

    fn done(&self) -> bool {
        self.err.is_some() || self.is_at_end()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokens() {
        let tokens = scan_tokens("(".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LeftParen,
                    lexeme: b"(".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens(")".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::RightParen,
                    lexeme: b")".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens(",".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Comma,
                    lexeme: b",".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("{".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LeftBrace,
                    lexeme: b"{".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("}".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::RightBrace,
                    lexeme: b"}".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("[".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LeftBracket,
                    lexeme: b"[".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("]".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::RightBracket,
                    lexeme: b"]".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens(".".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Dot,
                    lexeme: b".".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("-9223372036854775808".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Integer,
                    lexeme: b"-9223372036854775808".to_vec(),
                    literal: Some(Literal::Integer(-9_223_372_036_854_775_808)),
                    line: 1,
                    col: 19,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 19
                }
            ]
        );

        let tokens = scan_tokens("-0x8000000000000000".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Integer,
                    lexeme: b"-0x8000000000000000".to_vec(),
                    literal: Some(Literal::Integer(-9_223_372_036_854_775_808)),
                    line: 1,
                    col: 18,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 18
                }
            ]
        );

        let tokens = scan_tokens("+".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Plus,
                    lexeme: b"+".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("*".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Star,
                    lexeme: b"*".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens(";".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Semicolon,
                    lexeme: b";".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("/".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Slash,
                    lexeme: b"/".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );

        let tokens = scan_tokens("!".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Bang,
                    lexeme: b"!".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );
        let tokens = scan_tokens(":".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Colon,
                    lexeme: b":".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );
        let tokens = scan_tokens("-=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Subtraction,
                    lexeme: b"-=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );

        let tokens = scan_tokens("%=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Modulus,
                    lexeme: b"%=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );

        let tokens = scan_tokens("^=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::BitwiseXor,
                    lexeme: b"^=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1,
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );

        let tokens = scan_tokens("+=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Addition,
                    lexeme: b"+=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );

        let tokens = scan_tokens("*=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Multiplication,
                    lexeme: b"*=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("~".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Tilde,
                    lexeme: b"~".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );
        let tokens = scan_tokens("!~".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::BangTilde,
                    lexeme: b"!~".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("!=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::BangEqual,
                    lexeme: b"!=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Equal,
                    lexeme: b"=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 0
                }
            ]
        );
        let tokens = scan_tokens("==".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::EqualEqual,
                    lexeme: b"==".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("||=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LogicalOr,
                    lexeme: b"||=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                }
            ]
        );
        let tokens = scan_tokens("||".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Or,
                    lexeme: b"||".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("|=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::BitwiseOr,
                    lexeme: b"|=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("&&=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LogicalAnd,
                    lexeme: b"&&=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                }
            ]
        );
        let tokens = scan_tokens("&&".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::And,
                    lexeme: b"&&".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("&=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::BitwiseAnd,
                    lexeme: b"&=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens(">=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::GreaterEqual,
                    lexeme: b">=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens(">>=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::RightShift,
                    lexeme: b">>=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                }
            ]
        );
        let tokens = scan_tokens("<=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LessEqual,
                    lexeme: b"<=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );
        let tokens = scan_tokens("<<=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LeftShift,
                    lexeme: b"<<=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                }
            ]
        );

        let tokens = scan_tokens("/=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Division,
                    lexeme: b"/=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 1
                }
            ]
        );

        let tokens = scan_tokens("rol=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::LeftRotate,
                    lexeme: b"rol=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 3
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 3
                }
            ]
        );
        let tokens = scan_tokens("ror=".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::RightRotate,
                    lexeme: b"ror=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 3
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 3
                }
            ]
        );
        let tokens = scan_tokens("3600s".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600s".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Seconds)),
                    line: 1,
                    col: 4
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 4
                }
            ]
        );

        let tokens = scan_tokens("3600d".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600d".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Days)),
                    line: 1,
                    col: 4
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 4
                }
            ]
        );
        let tokens = scan_tokens("3600ms".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600ms".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Milliseconds)),
                    line: 1,
                    col: 5
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 5
                }
            ]
        );
        let tokens = scan_tokens("3600m".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600m".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Minutes)),
                    line: 1,
                    col: 4
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 4
                }
            ]
        );
        let tokens = scan_tokens("3600h".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600h".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Hours)),
                    line: 1,
                    col: 4
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 4
                }
            ]
        );
        let tokens = scan_tokens("3600y".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Duration,
                    lexeme: b"3600y".to_vec(),
                    literal: Some(Literal::Duration(3600.0, DurationUnit::Years)),
                    line: 1,
                    col: 4
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 4
                }
            ]
        );

        let tokens = scan_tokens("synthetic.base64".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::SyntheticBase64,
                    lexeme: b"synthetic.base64".to_vec(),
                    literal: None,
                    line: 1,
                    col: 15
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 15
                }
            ]
        );

        let tokens = scan_tokens("sub vcl_recv {set req.http.a = \"test\";}".to_owned()).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    ty: TokenType::Sub,
                    lexeme: b"sub".to_vec(),
                    literal: None,
                    line: 1,
                    col: 2
                },
                Token {
                    ty: TokenType::Identifier,
                    lexeme: b"vcl_recv".to_vec(),
                    literal: Some(Literal::Identifier("vcl_recv".to_string())),
                    line: 1,
                    col: 11
                },
                Token {
                    ty: TokenType::LeftBrace,
                    lexeme: b"{".to_vec(),
                    literal: None,
                    line: 1,
                    col: 13
                },
                Token {
                    ty: TokenType::Set,
                    lexeme: b"set".to_vec(),
                    literal: None,
                    line: 1,
                    col: 16
                },
                Token {
                    ty: TokenType::Identifier,
                    lexeme: b"req".to_vec(),
                    literal: Some(Literal::Identifier("req".to_string())),
                    line: 1,
                    col: 20
                },
                Token {
                    ty: TokenType::Dot,
                    lexeme: b".".to_vec(),
                    literal: None,
                    line: 1,
                    col: 21
                },
                Token {
                    ty: TokenType::Identifier,
                    lexeme: b"http".to_vec(),
                    literal: Some(Literal::Identifier("http".to_string())),
                    line: 1,
                    col: 25
                },
                Token {
                    ty: TokenType::Dot,
                    lexeme: b".".to_vec(),
                    literal: None,
                    line: 1,
                    col: 26
                },
                Token {
                    ty: TokenType::Identifier,
                    lexeme: b"a".to_vec(),
                    literal: Some(Literal::Identifier("a".to_string())),
                    line: 1,
                    col: 27
                },
                Token {
                    ty: TokenType::Equal,
                    lexeme: b"=".to_vec(),
                    literal: None,
                    line: 1,
                    col: 29
                },
                Token {
                    ty: TokenType::String,
                    lexeme: b"\"test\"".to_vec(),
                    literal: Some(Literal::Str("test".to_string())),
                    line: 1,
                    col: 36
                },
                Token {
                    ty: TokenType::Semicolon,
                    lexeme: b";".to_vec(),
                    literal: None,
                    line: 1,
                    col: 37
                },
                Token {
                    ty: TokenType::RightBrace,
                    lexeme: b"}".to_vec(),
                    literal: None,
                    line: 1,
                    col: 38
                },
                Token {
                    ty: TokenType::Eof,
                    lexeme: b"".to_vec(),
                    literal: None,
                    line: 1,
                    col: 38
                }
            ]
        );
    }
}
