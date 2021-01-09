use super::token::{look_ident, Token, TokenType};
use std::{iter::Peekable, str::Chars};

/// # Lexer
/// The lexer contains the logic to handle with the input str
/// and to convert it into tokens to be parsed
/// ## Example
/// ```
/// use nameless::Lexer;
///
/// let input = "let x = 1;";
/// let lexer = Lexer::new(input);
/// ```
pub struct Lexer<'a> {
    line: u32,
    column: u32,
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// Read the input and generate a new lexer
    pub fn new(input: &str) -> Lexer {
        Lexer {
            line: 1,
            column: 0,
            input: input.chars().peekable(),
        }
    }

    ///  read the next char and return the next token to parse
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let (token_type, line, column) = match self.skip_comments() {
            Some('=') => {
                if self.peek_is('=') {
                    self.input.next();
                    (TokenType::Equal, self.line, self.column)
                } else {
                    (TokenType::Assign, self.line, self.column)
                }
            }
            Some('!') => {
                if self.peek_is('=') {
                    self.input.next();
                    (TokenType::NotEqual, self.line, self.column)
                } else {
                    (TokenType::Bang, self.line, self.column)
                }
            }
            Some('&') => {
                if self.peek_is('&') {
                    self.input.next();
                    (TokenType::Or, self.line, self.column)
                } else {
                    (TokenType::Illegal, self.line, self.column)
                }
            }
            Some('|') => {
                if self.peek_is('|') {
                    self.input.next();
                    (TokenType::Or, self.line, self.column)
                } else {
                    (TokenType::VerticalBar, self.line, self.column)
                }
            }
            Some(',') => (TokenType::Comma, self.line, self.column),
            Some(';') => (TokenType::Semicolon, self.line, self.column),
            Some('(') => (TokenType::LParen, self.line, self.column),
            Some(')') => (TokenType::RParen, self.line, self.column),
            Some('{') => (TokenType::LBrace, self.line, self.column),
            Some('}') => (TokenType::RBrace, self.line, self.column),
            Some('[') => (TokenType::LBracket, self.line, self.column),
            Some(']') => (TokenType::RBracket, self.line, self.column),
            Some('+') => {
                if self.peek_is('+') {
                    self.input.next();
                    (TokenType::Increment, self.line, self.column)
                } else if self.peek_is('=') {
                    self.input.next();
                    (TokenType::PlusAssign, self.line, self.column)
                } else {
                    (TokenType::Plus, self.line, self.column)
                }
            }
            Some('-') => {
                if self.peek_is('-') {
                    self.input.next();
                    (TokenType::Decrement, self.line, self.column)
                } else if self.peek_is('=') {
                    (TokenType::MinusAssign, self.line, self.column)
                } else {
                    (TokenType::Minus, self.line, self.column)
                }
            }
            Some('*') => {
                if self.peek_is('=') {
                    self.input.next();
                    (TokenType::MultiplyAssign, self.line, self.column)
                } else {
                    (TokenType::Asterisk, self.line, self.column)
                }
            }
            Some('/') => {
                if self.peek_is('=') {
                    self.input.next();
                    (TokenType::DivideAssign, self.line, self.column)
                } else {
                    (TokenType::Slash, self.line, self.column)
                }
            }
            Some('<') => (TokenType::LowerThan, self.line, self.column),
            Some('>') => (TokenType::GreaterThan, self.line, self.column),
            Some('"') => {
                let (line, column) = (self.line, self.column);
                (TokenType::String(self.read_string()), line, column)
            }
            Some(ch) => {
                if ch.is_alphabetic() {
                    let (line, column) = (self.line, self.column);
                    let literal = self.read_identifier(ch);
                    (look_ident(&literal), line, column)
                } else if ch.is_numeric() {
                    let (line, column) = (self.line, self.column);
                    let literal = self.read_number(ch);

                    if literal.contains('.') {
                        (TokenType::Float(literal), line, column)
                    } else {
                        (TokenType::Int(literal), line, column)
                    }
                } else {
                    (TokenType::Illegal, self.line, self.column)
                }
            }
            None => (TokenType::EndOfFile, self.line, self.column),
        };

        Token {
            line,
            column,
            token_type,
        }
    }

    /// Omit the whitespaces in the input
    fn skip_whitespace(&mut self) {
        while let Some(&ch) = self.input.peek() {
            if !ch.is_whitespace() {
                return;
            }

            match self.input.next() {
                Some('\n') => {
                    self.increase_line();
                }
                Some(_) => {
                    self.increase_column();
                }
                None => {}
            }
        }
    }

    fn skip_comments(&mut self) -> Option<char> {
        match self.input.next() {
            Some('/') => {
                self.increase_column();
                if self.peek_is('/') {
                    self.input.next();
                    self.increase_column();
                    loop {
                        match self.input.next() {
                            Some('\n') => {
                                self.increase_line();
                                self.skip_whitespace();
                                self.increase_column();
                                return self.input.next();
                            }
                            _ => {
                                self.increase_column();
                            }
                        }
                    }
                }

                return Some('/');
            }
            current => {
                self.increase_column();
                return current;
            }
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
            self.increase_column();
        }

        ident
    }

    /// Tokenize int literal
    fn read_number(&mut self, first: char) -> String {
        let mut number = String::from(first);
        self.iterate_numeric(&mut number);

        if self.peek_is('.') {
            self.input.next();
            if let Some(&ch) = self.input.peek() {
                if ch.is_numeric() {
                    number.push('.');
                    self.iterate_numeric(&mut number);
                }
            }
        }

        number
    }

    fn iterate_numeric(&mut self, number: &mut String) {
        while let Some(&ch) = self.input.peek() {
            if !ch.is_numeric() {
                break;
            }

            number.push(ch);
            self.input.next();
            self.increase_column();
        }
    }

    /// Tokenize string literal
    fn read_string(&mut self) -> String {
        let mut string = String::new();

        while let Some(&ch) = self.input.peek() {
            self.input.next();
            self.increase_column();

            if ch == '"' {
                break;
            }

            if ch == '\n' {
                self.increase_line();
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

    fn increase_column(&mut self) {
        self.column += 1;
    }

    fn increase_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}
