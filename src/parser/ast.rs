use super::super::lexer::Token;

pub type Program = Vec<Statement>;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifer(pub String);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block(pub Vec<Statement>);

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fn {
    pub identifier: Identifer,
    pub params: Vec<Identifer>,
    pub body: Block,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifer(Identifer),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Literal(Literal),
    Block(Block),
    If(If),
    Fn(Fn),
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(Identifer, Expression),
    Return(Expression),
    Block(Block),
    If(If),
    Fn(Fn),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Not,
}

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
