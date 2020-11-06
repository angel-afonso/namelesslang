use super::super::lexer::{Lexer, Token};
use super::ast::*;
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            cur_token,
            peek_token,
        }
    }

    fn next(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> (Program, Vec<ParseError>) {
        let mut program = Vec::new();
        let mut errors = Vec::new();

        while !self.cur_token_is(Token::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => program.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        (program, errors)
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::LBrace => Ok(Statement::Expr(Expression::Block(self.parse_block()?))),
            Token::If => Ok(Statement::Expr(self.parse_if_expression()?)),
            tok => Err(ParseError(format!("Unexpected token {:?}", tok))),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.next();

        let ident = match &self.cur_token {
            Token::Ident(lit) => Identifer(lit.clone()),
            tok => return Err(ParseError(format!("Expected an identifier, got {:?}", tok))),
        };

        self.next();

        if !self.cur_token_is(Token::Assign) {
            return Err(ParseError(format!(
                "Expected assign, got {:?}",
                self.peek_token
            )));
        }

        self.next();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next();

        if self.cur_token_is(Token::Semicolon) {
            self.next();
        }

        Ok(Statement::Let(ident, value))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next();

        if self.cur_token_is(Token::Semicolon) {
            self.next();
        }

        Ok(Statement::Return(value))
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
        if !self.cur_token_is(Token::LBrace) {
            return Err(ParseError(format!(
                "Expected Token::LBrace, got {:?}",
                self.cur_token
            )));
        }

        let mut stmts = Vec::new();

        self.next();

        while !self.cur_token_is(Token::RBrace) && !self.cur_token_is(Token::EndOfFile) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        self.next();

        Ok(Block(stmts))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_expr = match &self.cur_token {
            Token::Ident(ident) => Expression::Identifer(self.parse_identifier(ident.clone())),
            Token::Int(int) => Expression::Literal(self.parse_integer(int.clone())?),
            Token::True | Token::False => {
                Expression::Literal(self.parse_boolean(self.cur_token.clone()))
            }
            Token::Minus | Token::Bang => self.parse_prefix_expression()?,
            Token::LParen => self.parse_grouped_expression()?,
            tok => {
                return Err(ParseError(format!(
                    "No prefix parse function for {:?}",
                    tok
                )))
            }
        };

        while !self.peek_token_is(&Token::Semicolon) && precedence < self.peek_precedece() {
            self.next();

            left_expr = match self.cur_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::And
                | Token::Or
                | Token::Equal
                | Token::NotEqual
                | Token::LowerThan
                | Token::GreaterThan => self.parse_infix_expression(left_expr)?,
                _ => return Ok(left_expr),
            };
        }

        Ok(left_expr)
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParseError> {
        self.next();

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.next();

        let consequence = self.parse_block()?;

        if self.cur_token_is(Token::Else) {
            self.next();

            let alternative = if self.cur_token_is(Token::If) {
                Some(Box::new(self.parse_if_expression()?))
            } else {
                Some(Box::new(Expression::Block(self.parse_block()?)))
            };

            return Ok(Expression::If {
                condition,
                consequence,
                alternative,
            });
        }

        Ok(Expression::If {
            condition,
            consequence,
            alternative: None,
        })
    }

    fn parse_identifier(&self, ident: String) -> Identifer {
        Identifer(ident.clone())
    }

    fn parse_integer(&self, int: String) -> Result<Literal, ParseError> {
        match int.parse::<i64>() {
            Ok(value) => Ok(Literal::Int(value)),
            Err(_) => {
                return Err(ParseError(format!("Could not parse {} as int", int)));
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let operator = match &self.cur_token {
            Token::Minus => PrefixOperator::Minus,
            Token::Bang => PrefixOperator::Not,
            token => return Err(ParseError(format!("Unexpected {:?}", token))),
        };

        self.next();

        let right = self.parse_expression(Precedence::Prefix)?;

        self.next();

        return Ok(Expression::Prefix(operator, Box::new(right)));
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let operator = match &self.cur_token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::And => InfixOperator::And,
            Token::Or => InfixOperator::Or,
            Token::Equal => InfixOperator::Equal,
            Token::NotEqual => InfixOperator::NotEqual,
            Token::LowerThan => InfixOperator::LowerThan,
            Token::GreaterThan => InfixOperator::GreaterThan,
            token => return Err(ParseError(format!("Unexpected {:?}", token))),
        };

        let precedence = self.cur_precedece();
        self.next();
        let right = self.parse_expression(precedence)?;

        Ok(Expression::Infix(operator, Box::new(left), Box::new(right)))
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.peek_token_is(&Token::RParen) {
            return Err(ParseError(format!("Expected (")));
        }

        self.next();

        expr
    }

    fn parse_boolean(&self, boolean: Token) -> Literal {
        Literal::Bool(self.cur_token_is(boolean))
    }

    fn cur_token_is(&self, token: Token) -> bool {
        self.cur_token == token
    }

    fn peek_token_is(&self, token: &Token) -> bool {
        &self.peek_token == token
    }

    fn peek_precedece(&self) -> Precedence {
        token_precedence(&self.peek_token)
    }

    fn cur_precedece(&self) -> Precedence {
        token_precedence(&self.cur_token)
    }
}
