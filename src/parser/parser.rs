use super::super::lexer::{Lexer, Token, TokenType};
use super::ast::*;
use std::fmt;

/// Parse error representation
#[derive(Debug, Clone)]
pub struct ParseError(pub Location, pub String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.1, self.0)
    }
}

/// # Parser
/// handle the program parsing logic
/// ## Example
/// ```
/// let input = "let a = 10;";
/// let mut parser = Parser::new(Lexer::new(input));
/// let (program, errors) = parser.parse_program();
/// ```
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    /// Generate a new instance of parser
    pub fn new(mut lexer: Lexer<'a>) -> Parser<'a> {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Parser {
            lexer,
            cur_token,
            peek_token,
        }
    }

    /// Advance the tokens cursor
    fn next(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// Execute the program parsing
    pub fn parse_program(&mut self) -> (Program, Vec<ParseError>) {
        let mut program = Vec::new();
        let mut errors = Vec::new();

        while !self.cur_token_type_is(TokenType::EndOfFile) {
            match self.parse_statement() {
                Ok(stmt) => program.push(stmt),
                Err(err) => {
                    errors.push(err);
                    return (program, errors);
                }
            }
        }

        (program, errors)
    }

    /// Return a parsed statement
    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match &self.cur_token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::LBrace => Ok(Statement::Block(self.parse_block()?)),
            TokenType::If => Ok(Statement::If(self.parse_if()?)),
            TokenType::Function => Ok(Statement::Fn(self.parse_function()?)),
            TokenType::Ident(_) => {
                if self.peek_token_type_is(TokenType::Assign) {
                    self.parse_assignment()
                } else {
                    self.parse_call_statement()
                }
            }
            TokenType::For => self.parse_for_statement(),
            tok => Err(ParseError(
                Location {
                    line: self.cur_token.line,
                    column: self.cur_token.column,
                },
                format!("Unexpected token {:?}", tok),
            )),
        }
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParseError> {
        let location = Location {
            line: self.cur_token.line,
            column: self.cur_token.column,
        };

        self.next();

        let counter = Box::new(self.parse_statement()?);

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        let condition = self.parse_expression(Precedence::Lowest)?;
        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        let step = Box::new(self.parse_statement()?);

        let block = self.parse_block()?;

        Ok(Statement::For(For {
            location,
            counter,
            condition,
            step,
            block,
        }))
    }

    /// Parse a identifier assignment
    fn parse_assignment(&mut self) -> Result<Statement, ParseError> {
        let location = Location::from_token(&self.cur_token);

        let identifier = match &self.cur_token.token_type {
            TokenType::Ident(lit) => Identifer {
                location: Location::from_token(&self.cur_token),
                value: lit.clone(),
            },
            tok => {
                return Err(ParseError(
                    Location::from_token(&self.cur_token),
                    format!("Expected an identifier, got {:?}", tok),
                ))
            }
        };

        self.next();

        if !self.cur_token_type_is(TokenType::Assign) {
            return Err(ParseError(
                location,
                format!("Expected assign, got {:?}", self.cur_token),
            ));
        }

        self.next();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        Ok(Statement::Assignment(Assignment {
            location,
            identifier,
            value,
        }))
    }

    /// Parse a let statement
    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        self.next();

        let identifier = match &self.cur_token.token_type {
            TokenType::Ident(lit) => Identifer {
                location: Location {
                    line: self.cur_token.line,
                    column: self.cur_token.column,
                },
                value: lit.clone(),
            },
            tok => {
                return Err(ParseError(
                    Location {
                        line: self.cur_token.line,
                        column: self.cur_token.column,
                    },
                    format!("Expected an identifier, got {:?}", tok),
                ))
            }
        };

        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
            return Ok(Statement::Let(Let {
                location: Location {
                    line: self.cur_token.line,
                    column: self.cur_token.column,
                },
                identifier,
                value: None,
            }));
        }

        if !self.cur_token_type_is(TokenType::Assign) {
            return Err(ParseError(
                Location {
                    line: self.cur_token.line,
                    column: self.cur_token.column,
                },
                format!("Expected assign, got {:?}", self.cur_token),
            ));
        }

        self.next();

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        Ok(Statement::Let(Let {
            location: Location {
                line: self.cur_token.line,
                column: self.cur_token.column,
            },
            identifier,
            value: Some(value),
        }))
    }

    /// Parse a return statement
    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        let value = self.parse_expression(Precedence::Lowest)?;

        self.next();

        if self.cur_token_type_is(TokenType::Semicolon) {
            self.next();
        }

        Ok(Statement::Return(Some(value)))
    }

    /// Parse a statements block
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let token = self.cur_token.clone();

        if !self.cur_token_type_is(TokenType::LBrace) {
            return Err(ParseError(
                Location {
                    line: token.line,
                    column: token.column,
                },
                format!("Expected TokenType::LBrace, got {:?}", token),
            ));
        }

        let mut stmts = Vec::new();

        self.next();

        while !self.cur_token_type_is(TokenType::RBrace)
            && !self.cur_token_type_is(TokenType::EndOfFile)
        {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
        }

        self.next();

        Ok(Block {
            location: Location {
                line: token.line,
                column: token.column,
            },
            statements: stmts,
        })
    }

    /// Parse expressions with prefix and infix operators
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, ParseError> {
        let mut left_expr = match &self.cur_token.token_type {
            TokenType::Ident(ident) => Expression::Identifer(self.parse_identifier(ident.clone())),
            TokenType::Int(int) => Expression::Literal(self.parse_integer(int.clone())?),
            TokenType::String(string) => Expression::Literal(Literal::String(
                Location::from_token(&self.cur_token.clone()),
                string.clone(),
            )),
            TokenType::True | TokenType::False => Expression::Literal(self.parse_boolean()),
            TokenType::Minus | TokenType::Bang => self.parse_prefix_expression()?,
            TokenType::LParen => self.parse_grouped_expression()?,
            TokenType::LBrace => Expression::Block(self.parse_block()?),
            TokenType::LBracket => Expression::Array(self.parse_array_literal()?),
            tok => {
                return Err(ParseError(
                    Location {
                        line: self.cur_token.line,
                        column: self.cur_token.column,
                    },
                    format!("Unexpected {:?}", tok),
                ))
            }
        };

        while !self.peek_token_type_is(TokenType::Semicolon) && precedence < self.peek_precedece() {
            self.next();

            left_expr = match self.cur_token.token_type {
                TokenType::Plus
                | TokenType::Minus
                | TokenType::Asterisk
                | TokenType::Slash
                | TokenType::And
                | TokenType::Or
                | TokenType::Equal
                | TokenType::NotEqual
                | TokenType::LowerThan
                | TokenType::LBracket
                | TokenType::GreaterThan => self.parse_infix_expression(left_expr)?,
                TokenType::LParen => Expression::Call(self.parse_function_call(left_expr)?),
                _ => return Ok(left_expr),
            };
        }

        Ok(left_expr)
    }

    fn parse_if_condition(&mut self) -> Result<(Location, Box<Expression>, Block), ParseError> {
        let location = Location::from_token(&self.cur_token);

        self.next();

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.next();

        let consequence = self.parse_block()?;

        Ok((location, condition, consequence))
    }

    fn parse_if(&mut self) -> Result<If, ParseError> {
        let (location, condition, consequence) = self.parse_if_condition()?;

        let alternative = if self.cur_token_type_is(TokenType::Else) {
            Some(self.parse_else()?)
        } else {
            None
        };

        Ok(If {
            location,
            condition,
            consequence,
            alternative,
        })
    }

    fn parse_else(&mut self) -> Result<Else, ParseError> {
        let location = Location::from_token(&self.cur_token);

        self.next();

        match &self.cur_token.token_type {
            TokenType::If => Ok(Else::If(location, Box::new(self.parse_if()?))),
            TokenType::LBrace => Ok(Else::Block(location, self.parse_block()?)),
            tok => Err(ParseError(location, format!("Unexpected {:?}", tok))),
        }
    }

    fn parse_array_literal(&mut self) -> Result<Array, ParseError> {
        Ok(Array {
            location: Location {
                line: self.cur_token.line,
                column: self.cur_token.column,
            },
            expressions: Box::new(self.parse_expression_list(TokenType::RBracket)?),
        })
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let location = Location {
            line: self.cur_token.line,
            column: self.cur_token.column,
        };

        self.next();

        let index = self.parse_expression(Precedence::Lowest)?;

        if !self.peek_token_type_is(TokenType::RBracket) {
            return Err(ParseError(
                Location {
                    line: self.peek_token.line,
                    column: self.peek_token.column,
                },
                format!("Expected ]"),
            ));
        }

        self.next();

        Ok(Expression::Index(Index {
            location,
            left: Box::new(left.clone()),
            index: Box::new(index),
        }))
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Expression>, ParseError> {
        let mut list = Vec::new();

        if self.peek_token_type_is(end.clone()) {
            self.next();
            return Ok(list);
        }

        self.next();

        list.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_type_is(TokenType::Comma) {
            self.next();
            self.next();

            list.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.peek_token_type_is(end.clone()) {
            return Err(ParseError(
                Location {
                    line: self.cur_token.line,
                    column: self.cur_token.column,
                },
                format!("Expected {:?}", end),
            ));
        }

        self.next();
        Ok(list)
    }

    /// Parse a identifier
    fn parse_identifier(&self, value: String) -> Identifer {
        Identifer {
            location: Location {
                line: self.cur_token.line,
                column: self.cur_token.column,
            },
            value: value.clone(),
        }
    }

    /// Parse a integer literal
    fn parse_integer(&self, int: String) -> Result<Literal, ParseError> {
        match int.parse::<i64>() {
            Ok(value) => Ok(Literal::Int(Location::from_token(&self.cur_token), value)),
            Err(_) => {
                return Err(ParseError(
                    Location {
                        line: self.cur_token.line,
                        column: self.cur_token.column,
                    },
                    format!("Could not parse {} as int", int),
                ));
            }
        }
    }

    /// parse a prefix expression
    fn parse_prefix_expression(&mut self) -> Result<Expression, ParseError> {
        let location = Location::from_token(&self.cur_token);

        let operator = match &self.cur_token.token_type {
            TokenType::Minus => PrefixOperator::Minus,
            TokenType::Bang => PrefixOperator::Not,
            tok => {
                return Err(ParseError(
                    Location {
                        line: self.cur_token.line,
                        column: self.cur_token.column,
                    },
                    format!("Unexpected {:?}", tok),
                ))
            }
        };

        self.next();

        let expression = Box::new(self.parse_expression(Precedence::Prefix)?);

        self.next();

        return Ok(Expression::Prefix(Prefix {
            location,
            operator,
            expression,
        }));
    }

    /// Parse a infix expression
    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParseError> {
        let location = Location {
            line: self.cur_token.line,
            column: self.cur_token.column,
        };

        let operator = match &self.cur_token.token_type {
            TokenType::Plus => InfixOperator::Plus,
            TokenType::Minus => InfixOperator::Minus,
            TokenType::Asterisk => InfixOperator::Multiply,
            TokenType::Slash => InfixOperator::Divide,
            TokenType::And => InfixOperator::And,
            TokenType::Or => InfixOperator::Or,
            TokenType::Equal => InfixOperator::Equal,
            TokenType::NotEqual => InfixOperator::NotEqual,
            TokenType::LowerThan => InfixOperator::LowerThan,
            TokenType::GreaterThan => InfixOperator::GreaterThan,
            TokenType::LBracket => return self.parse_index_expression(left),
            token => return Err(ParseError(location, format!("Unexpected {:?}", token))),
        };

        let precedence = self.cur_precedece();
        self.next();
        let right = Box::new(self.parse_expression(precedence)?);

        Ok(Expression::Infix(Infix {
            location,
            operator,
            right,
            left: Box::new(left),
        }))
    }

    /// Parse a parentesis grouped expression
    fn parse_grouped_expression(&mut self) -> Result<Expression, ParseError> {
        self.next();

        let expr = self.parse_expression(Precedence::Lowest);

        if !self.peek_token_type_is(TokenType::RParen) {
            return Err(ParseError(
                Location {
                    line: self.peek_token.line,
                    column: self.peek_token.column,
                },
                format!("Expected ("),
            ));
        }

        self.next();

        expr
    }

    /// parse a function literal
    fn parse_function(&mut self) -> Result<Fn, ParseError> {
        let location = Location::from_token(&self.cur_token);

        let identifier = match &self.peek_token.token_type {
            TokenType::Ident(ident) => self.parse_identifier(ident.clone()),
            token => {
                return Err(ParseError(
                    Location {
                        line: self.peek_token.line,
                        column: self.peek_token.column,
                    },
                    format!("Expected identifier, got {:?}", token),
                ))
            }
        };

        self.next();

        if !self.peek_token_type_is(TokenType::LParen) {
            return Err(ParseError(
                Location {
                    line: self.peek_token.line,
                    column: self.peek_token.column,
                },
                format!("Expected (, got {:?}", self.peek_token),
            ));
        }

        self.next();

        let params = self.parse_function_params()?;

        self.next();

        let body = self.parse_block()?;

        return Ok(Fn {
            location,
            identifier,
            params,
            body,
        });
    }

    /// Parse the function params
    fn parse_function_params(&mut self) -> Result<Vec<Identifer>, ParseError> {
        let mut identifiers = vec![];

        if self.peek_token_type_is(TokenType::RParen) {
            self.next();
            return Ok(identifiers);
        }

        self.next();

        identifiers.push(match &self.cur_token.token_type {
            TokenType::Ident(ident) => self.parse_identifier(ident.clone()),
            tok => {
                return Err(ParseError(
                    Location::from_token(&self.cur_token),
                    format!("Expected identifier, got {:?}", tok),
                ))
            }
        });

        while self.peek_token_type_is(TokenType::Comma) {
            self.next();
            self.next();
            identifiers.push(match &self.cur_token.token_type {
                TokenType::Ident(ident) => self.parse_identifier(ident.clone()),
                tok => {
                    return Err(ParseError(
                        Location::from_token(&self.cur_token),
                        format!("Expected identifier, got {:?}", tok),
                    ))
                }
            });
        }

        if !self.peek_token_type_is(TokenType::RParen) {
            return Err(ParseError(
                Location::from_token(&self.peek_token),
                format!("Expected ), got {:?}", self.peek_token),
            ));
        }

        self.next();

        Ok(identifiers)
    }

    /// Parse a function call
    fn parse_function_call(&mut self, function: Expression) -> Result<Call, ParseError> {
        let location = Location::from_token(&self.cur_token);

        let arguments = self.parse_call_arguments()?;

        return Ok(Call {
            location,
            function: Box::new(function.clone()),
            arguments,
        });
    }

    /// Parse a comma separated function arguments
    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut args = Vec::new();

        if self.peek_token_type_is(TokenType::RParen) {
            self.next();
            return Ok(args);
        }

        self.next();

        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token_type_is(TokenType::Comma) {
            self.next();
            self.next();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if !self.peek_token_type_is(TokenType::RParen) {
            return Err(ParseError(
                Location::from_token(&self.peek_token),
                format!("expected )"),
            ));
        }

        self.next();

        Ok(args)
    }

    /// parse a call statement
    fn parse_call_statement(&mut self) -> Result<Statement, ParseError> {
        if !self.peek_token_type_is(TokenType::LParen) {
            return Err(ParseError(
                Location::from_token(&self.peek_token),
                format!("Unexpected identifier"),
            ));
        }

        let call = match self.parse_expression(Precedence::Lowest)? {
            Expression::Call(function) => {
                self.next();

                if self.cur_token_type_is(TokenType::Semicolon) {
                    self.next();
                }

                function
            }
            _ => {
                return Err(ParseError(
                    Location::from_token(&self.cur_token),
                    format!("Expected a function call"),
                ))
            }
        };

        Ok(Statement::Call(call))
    }

    /// Parse a boolean literal
    fn parse_boolean(&self) -> Literal {
        Literal::Bool(
            Location::from_token(&self.cur_token),
            self.cur_token_type_is(TokenType::True),
        )
    }

    /// Returns true if the current token is equal to the given token
    fn cur_token_type_is(&self, token: TokenType) -> bool {
        self.cur_token.token_type == token
    }

    /// Returns true if the peek token is equal to the given token
    fn peek_token_type_is(&self, token: TokenType) -> bool {
        self.peek_token.token_type == token
    }

    /// Returns the peek token precedence
    fn peek_precedece(&self) -> Precedence {
        token_precedence(&self.peek_token.token_type)
    }

    /// Returns the current token precedence
    fn cur_precedece(&self) -> Precedence {
        token_precedence(&self.cur_token.token_type)
    }
}
