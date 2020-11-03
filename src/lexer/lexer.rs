use super::token::{look_ident, Token};
use std::{iter::Peekable, str::Chars};

/// # Lexer
/// The lexer contains the logic to handle with the input str
/// and to convert it into tokens to be parsed
/// ## Example
/// ```
/// let input = "let x = 1;";
/// let lexer = Lexer::new();
/// ```
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// Read the input and generate a new lexer
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut vec = Vec::new();

        loop {
            let token = self.next_token();
            if token == Token::EndOfFile {
                return vec;
            }

            vec.push(token);
        }
    }

    ///  read the next char and return the next token to parse
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
            Some('&') => {
                if self.peek_is('&') {
                    self.input.next();
                    Token::Or
                } else {
                    Token::Illegal
                }
            }
            Some('|') => {
                if self.peek_is('|') {
                    self.input.next();
                    Token::Or
                } else {
                    Token::Illegal
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
            Some('"') => Token::String(self.read_string()),
            Some(ch) => {
                if ch.is_alphabetic() {
                    let literal = self.read_identifier(ch);
                    look_ident(&literal)
                } else if ch.is_numeric() {
                    let literal = self.read_number(ch);
                    Token::Int(literal)
                } else {
                    Token::Illegal
                }
            }
            None => Token::EndOfFile,
        }
    }

    /// Omit the whitespaces in the input
    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if !ch.is_whitespace() {
                return;
            }

            self.input.next();
        }
    }

    /// Tokenize identifier literal
    fn read_identifier(&mut self, first: char) -> String {
        let mut ident = String::from(first);
        while let Some(&ch) = self.input.peek() {
            if !ch.is_alphanumeric() {
                break;
            }

            ident.push(ch);
            self.input.next();
        }

        ident
    }

    /// Tokenize int literal
    fn read_number(&mut self, first: char) -> String {
        let mut number = String::from(first);
        while let Some(&ch) = self.input.peek() {
            if !ch.is_numeric() {
                break;
            }

            number.push(ch);
            self.input.next();
        }

        number
    }

    /// Tokenize string literal
    fn read_string(&mut self) -> String {
        let mut string = String::new();

        while let Some(&ch) = self.input.peek() {
            self.input.next();

            if ch == '"' {
                break;
            }

            string.push(ch);
        }

        string
    }

    /// Returns true if the peek char is equal to the given char
    fn peek_is(&mut self, ch: char) -> bool {
        match self.input.peek() {
            Some(&next) => next == ch,
            None => false,
        }
    }

    /// Returns true if the peek char is a alphabetic character
    fn peek_is_alphabetic(&mut self) -> bool {
        match self.input.peek() {
            Some(&ch) => ch.is_alphabetic(),
            None => false,
        }
    }

    /// Returns true if the peek char is a alphanumeric character
    fn peek_is_alphanumeric(&mut self) -> bool {
        match self.input.peek() {
            Some(&ch) => ch.is_alphanumeric(),
            None => false,
        }
    }
}
