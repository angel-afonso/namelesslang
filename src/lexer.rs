use super::token::Token;
use std::{iter::Peekable, str::Chars};

/// # Lexer
/// The lexer contains the logic to handle with the input str
/// and to convert it into tokens to be parsed
/// ## Example
/// ```
/// let input = "int x = 1;";
/// let lexer = Lexer::new();
/// ```
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// Read the input and generate a new lexer
    fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.input.next() {
            Some('=') => {
                if self.peek_is('=') {
                    self.input.next();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            Some('!') => {
                if self.peek_is('=') {
                    self.input.next();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,
            Some('<') => Token::LowerThan,
            Some('>') => Token::GreaterThan,
            None => Token::EndOfFile,
            _ => Token::Illegal,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.input.next();
            };

            return;
        }
    }

    fn peek_is(&mut self, ch: char) -> bool {
        match self.input.peek() {
            Some(&next) => next == ch,
            None => false,
        }
    }
}
