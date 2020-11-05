use super::super::lexer::{Lexer, Token};
use super::ast::*;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            cur_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    fn next(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();

        while let Some(stmt) = self.parse_statement() {
            program.push(stmt);
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        self.next();

        let ident = match &self.cur_token {
            Token::Ident(lit) => Identifer(lit.clone()),
            _ => return None,
        };

        if !self.peek_token_is(&Token::Assign) {
            return None;
        }

        self.next();
        self.next();

        let value = self.parse_expression(Precedence::Lowest);

        if self.cur_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(Statement::Let(ident, value))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token_is(&Token::Semicolon) {
            self.next();
        }

        Some(Statement::Return(value))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let left_expr = match self.cur_token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer(),
            Token::True | Token::False => self.parse_boolean(),
            _ => None,
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedece() {
            match self.peek_token {
                _ => return left_expr,
            }
        }

        left_expr
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(ident) => Some(Expression::Identifer(Identifer(ident.clone()))),
            _ => None,
        }
    }

    fn parse_integer(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Int(int) => {
                let int_value = match int.parse::<i64>() {
                    Ok(value) => value,
                    Err(_) => {
                        self.errors
                            .push(format!("Could not parse {} as integer", int));
                        return None;
                    }
                };

                Some(Expression::Literal(Literal::Int(int_value)))
            }
            _ => None,
        }
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Literal(Literal::Bool(
            self.cur_token_is(&Token::True),
        )))
    }

    fn no_prefix_parse_function(&mut self, token: &Token) {
        self.errors
            .push(format!("No prefix parse function for {:?}", token));
    }

    fn cur_token_is(&mut self, token: &Token) -> bool {
        &self.cur_token == token
    }

    fn peek_token_is(&mut self, token: &Token) -> bool {
        &self.peek_token == token
    }

    fn peek_precedece(&mut self) -> Precedence {
        token_precedence(&self.peek_token)
    }

    pub fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }
}
