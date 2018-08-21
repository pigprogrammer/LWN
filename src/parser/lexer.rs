use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token<'input> {
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Semicolon,
    Comma,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    Not,
    NotEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    AmpAmp,
    PipePipe,

    Identifier(&'input str),
    String(&'input str),
    Float(f64),
    Int(i64),

    Else,
    False,
    Fn,
    Class,
    For,
    If,
    Nil,
    Return,
    This,
    True,
    Let,
    While,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Token::*;

        match *self {
            OpenBrace => write!(f, "{{"),
            CloseBrace => write!(f, "}}"),
            OpenBracket => write!(f, "["),
            CloseBracket => write!(f, "]"),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            Semicolon => write!(f, ";"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            Not => write!(f, "!"),
            NotEqual => write!(f, "!="),
            Equal => write!(f, "="),
            EqualEqual => write!(f, "=="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            AmpAmp => write!(f, "&&"),
            PipePipe => write!(f, "||"),

            Identifier(i) => write!(f, "{}", i),
            String(s) => write!(f, "\"{}\"", s),
            Int(n) => write!(f, "{}", n),
            Float(n) => write!(f,"{}",n),
            Else => write!(f, "else"),
            False => write!(f, "false"),
            Fn => write!(f, "fun"),
            For => write!(f, "for"),
            If => write!(f, "if"),
            Nil => write!(f, "nil"),
            Return => write!(f, "return"),
            This => write!(f, "this"),
            True => write!(f, "true"),
            Let => write!(f, "var"),
            While => write!(f, "while"),
            Class => write!(f,"class"),
        }
    }
}


use std::mem;
use std::str::CharIndices;

#[inline]
fn is_id_start(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphabetic()
}

#[inline]
fn is_id_continue(ch: char) -> bool {
    ch == '_' || ch.is_ascii_digit()
}

pub type Location = usize;

#[derive(Debug, Fail, PartialEq)]
pub enum LexicalError {
    #[fail(display = "Invalid character '{}' found at {}", ch, location)]
    InvalidCharacter { ch: char, location: Location },

    #[fail(display = "String starting at {} was not terminated", location)]
    UnterminatedString { location: Location },
}

pub type SpanResult<'input> = Result<(Location, Token<'input>, Location), LexicalError>;

pub struct Lexer<'input> {
    source: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
    lookahead2: Option<(usize, char)>,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Lexer<'input> {
        let mut chars = source.char_indices();
        let lookahead = chars.next();
        let lookahead2 = chars.next();

        Lexer {
            source,
            chars,
            lookahead,
            lookahead2,
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        mem::replace(
            &mut self.lookahead,
            mem::replace(&mut self.lookahead2, self.chars.next()),
        )
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Option<usize>
    where
        F: FnMut(char) -> bool,
    {
        while let Some((i, ch)) = self.lookahead {
            if terminate(ch) {
                return Some(i);
            } else {
                self.bump();
            }
        }

        None
    }

    fn take_while<F>(&mut self, mut condition: F) -> Option<usize>
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(|ch| !condition(ch))
    }

    fn skip_to_line_end(&mut self) {
        self.take_while(|ch| ch != '\n');
    }

    fn skip_whitespace(&mut self) {
        self.take_while(|ch| ch.is_whitespace());
    }

    fn read_string(&mut self, pos: usize) -> SpanResult<'input> {
        match self.take_until(|ch| ch == '"') {
            Some(i) => {
                self.bump();
                Ok((pos, Token::String(&self.source[pos + 1..i]), i + 1))
            }
            None => Err(LexicalError::UnterminatedString { location: pos }),
        }
    }

    fn read_number(&mut self, pos: usize) -> SpanResult<'input> {
        let mut end = self.take_while(|ch| ch.is_ascii_digit());

        if let Some((_, '.')) = self.lookahead {
            // Check if it's a decimal or a field access
            if let Some((_, next_ch)) = self.lookahead2 {
                if next_ch.is_ascii_digit() {
                    self.bump();
                    end = self.take_while(|ch| ch.is_ascii_digit());
                }
            }
            let end = end.unwrap_or_else(|| self.source.len());
            return Ok((
                pos,
                Token::Float(self.source[pos..end].parse().expect("unparsable float")),
                end,
            ));
        } else {
            let end = end.unwrap_or_else(|| self.source.len());
            return Ok((
                pos,
                Token::Int(self.source[pos..end].parse().expect("unparsable integer")),
                end,
            ));
        }
    }

    fn read_identifier(&mut self, pos: usize) -> SpanResult<'input> {
        let end = self
            .take_while(|ch| is_id_start(ch) || is_id_continue(ch))
            .unwrap_or_else(|| self.source.len());

        let token = match &self.source[pos..end] {
            "else" => Token::Else,
            "false" => Token::False,
            "fun" => Token::Fn,
            "for" => Token::For,
            "if" => Token::If,
            "nil" => Token::Nil,
            "return" => Token::Return,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Let,
            "var" => Token::Let,
            "while" => Token::While,
            "class" => Token::Class,
            id => Token::Identifier(id),
        };

        Ok((pos, token, end))
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = SpanResult<'input>;

    fn next(&mut self) -> Option<SpanResult<'input>> {
        self.skip_whitespace();

        if let Some((i, ch)) = self.bump() {
            match ch {
                '{' => Some(Ok((i, Token::OpenBrace, i + 1))),
                '}' => Some(Ok((i, Token::CloseBrace, i + 1))),
                '(' => Some(Ok((i, Token::OpenParen, i + 1))),
                ')' => Some(Ok((i, Token::CloseParen, i + 1))),
                '[' => Some(Ok((i, Token::OpenBracket, i + 1))),
                ']' => Some(Ok((i, Token::CloseBracket, i + 1))),
                ';' => Some(Ok((i, Token::Semicolon, i + 1))),
                ',' => Some(Ok((i, Token::Comma, i + 1))),
                '.' => Some(Ok((i, Token::Dot, i + 1))),
                '+' => Some(Ok((i, Token::Plus, i + 1))),
                '-' => Some(Ok((i, Token::Minus, i + 1))),
                '*' => Some(Ok((i, Token::Star, i + 1))),

                '/' => {
                    if let Some((_, '/')) = self.lookahead {
                        self.skip_to_line_end();
                        self.next()
                    } else {
                        Some(Ok((i, Token::Slash, i + 1)))
                    }
                }

                '!' => {
                    if let Some((_, '=')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::NotEqual, i + 2)))
                    } else {
                        Some(Ok((i, Token::Not, i + 1)))
                    }
                }

                '=' => {
                    if let Some((_, '=')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::EqualEqual, i + 2)))
                    } else {
                        Some(Ok((i, Token::Equal, i + 1)))
                    }
                }

                '>' => {
                    if let Some((_, '=')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::GreaterEqual, i + 2)))
                    } else {
                        Some(Ok((i, Token::Greater, i + 1)))
                    }
                }

                '<' => {
                    if let Some((_, '=')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::LessEqual, i + 2)))
                    } else {
                        Some(Ok((i, Token::Less, i + 1)))
                    }
                }

                '&' => {
                    if let Some((_, '&')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::AmpAmp, i + 2)))
                    } else {
                        Some(Err(LexicalError::InvalidCharacter { ch, location: i }))
                    }
                }

                '|' => {
                    if let Some((_, '|')) = self.lookahead {
                        self.bump();
                        Some(Ok((i, Token::PipePipe, i + 2)))
                    } else {
                        Some(Err(LexicalError::InvalidCharacter { ch, location: i }))
                    }
                }

                '"' => Some(self.read_string(i)),
                ch if is_id_start(ch) => Some(self.read_identifier(i)),
                ch if ch.is_ascii_digit() => Some(self.read_number(i)),

                ch => Some(Err(LexicalError::InvalidCharacter { ch, location: i })),
            }
        } else {
            None
        }
    }
}
