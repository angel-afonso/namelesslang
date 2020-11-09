use super::super::lexer::Token;

/// Store all the parse program
pub type Program = Vec<Statement>;

/// Identifier like variables and function names
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifer(pub String);

/// Code block
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block(pub Vec<Statement>);

/// Conditional structure
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Box<Expression>>,
}

/// Function represetation
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fn {
    pub identifier: Identifer,
    pub params: Vec<Identifer>,
    pub body: Block,
}

/// Closure is like a anomymous function
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Closure {
    pub params: Vec<Identifer>,
    pub body: Block,
}

/// Represents a function or a closure call
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

/// Represents all the posible expressions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifer(Identifer),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Literal(Literal),
    Block(Block),
    If(If),
    CLosure(Closure),
    Call(Call),
}

/// Represents all the posible statements
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(Identifer, Expression),
    Return(Expression),
    Block(Block),
    If(If),
    Fn(Fn),
    Call(Call),
}

/// Represents the literal values
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
}

/// Prefix operators like - or !
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Not,
}

/// Infix operators like +, -, , && or ||
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    And,
    Or,
    LowerThan,
    GreaterThan,
}

/// enum to hanble with operator precedence
#[derive(Debug, PartialOrd, PartialEq, Clone, Copy)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

/// Return the precedence of the given token
pub fn token_precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal => Precedence::Equals,
        Token::NotEqual => Precedence::Equals,
        Token::LowerThan => Precedence::LessGreater,
        Token::GreaterThan => Precedence::LessGreater,
        Token::Plus => Precedence::Sum,
        Token::Minus => Precedence::Sum,
        Token::Slash => Precedence::Product,
        Token::Asterisk => Precedence::Product,
        Token::LParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}
