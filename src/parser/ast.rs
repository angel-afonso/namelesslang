use super::super::lexer::{Token, TokenType};
use std::fmt::Display;

/// Store all the parse program
pub type Program = Vec<Statement>;

/// Identifier like variables and function names
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifer {
    pub token: Token,
    pub value: String,
}

/// Code block
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub token: Token,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub token: Token,
    pub identifier: Identifer,
    pub value: Option<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub token: Token,
    pub identifier: Identifer,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prefix {
    pub token: Token,
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Infix {
    pub token: Token,
    pub operator: InfixOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

/// Conditional structure
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfStatement {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Box<Statement>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
    pub token: Token,
    pub counter: Box<Statement>,
    pub condition: Expression,
    pub step: Box<Statement>,
    pub block: Block,
}

/// Function represetation
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fn {
    pub token: Token,
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
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

/// Represents the literal values
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Int(Token, i64),
    Bool(Token, bool),
    String(Token, String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(_, value) => write!(f, "{}", value),
            Literal::Bool(_, value) => write!(f, "{}", value),
            Literal::String(_, value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Array {
    pub token: Token,
    pub expressions: Box<Vec<Expression>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index {
    pub token: Token,
    pub left: Box<Expression>,
    pub index: Box<Expression>,
}

/// Represents all the posible expressions
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Identifer(Identifer),
    Prefix(Prefix),
    Infix(Infix),
    Literal(Literal),
    Array(Array),
    Index(Index),
    Block(Block),
    If(IfExpression),
    CLosure(Closure),
    Call(Call),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifer(identifier) => write!(f, "{}", identifier.value),
            Expression::Prefix(prefix) => write!(f, "{}{}", prefix.operator, prefix.expression),
            Expression::Infix(infix) => {
                write!(f, "{} {} {}", infix.left, infix.operator, infix.right)
            }
            Expression::Literal(literal) => write!(f, "{}", literal),
            Expression::Array(Array(_, elems)) => write!(
                f,
                "{}",
                elems
                    .iter()
                    .map(|element| format!("{}", element.clone()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Index(index) => write!(f, "{}[{}]", index.left, index.index),
            Expression::Block(block) => write!(
                f,
                "{{\n {:?} \n}}",
                block
                    .statements
                    .iter()
                    .map(|stmt| format!("{:?}", stmt.clone()))
                    .collect::<Vec<String>>()
                    .join("\n")
            ),
            Expression::Call(call) => write!(
                f,
                "{}({})",
                call.function,
                call.arguments
                    .iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Expression::If(_) => todo!(),
            Expression::CLosure(_) => todo!(),
        }
    }
}

/// Represents all the posible statements
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(Let),
    Return(Expression),
    Block(Block),
    If(IfStatement),
    Fn(Fn),
    Call(Call),
    Assignment(Assignment),
    For(For),
}

/// Prefix operators like - or !
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum PrefixOperator {
    Plus,
    Minus,
    Not,
    LBracket,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOperator::Plus => write!(f, "+"),
            PrefixOperator::Minus => write!(f, "-"),
            PrefixOperator::Not => write!(f, "!"),
            PrefixOperator::LBracket => write!(f, "["),
        }
    }
}

/// Infix operators like +, -, , && or ||
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
impl Display for InfixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOperator::Plus => write!(f, "+"),
            InfixOperator::Minus => write!(f, "-"),
            InfixOperator::Multiply => write!(f, "*"),
            InfixOperator::Divide => write!(f, "/"),
            InfixOperator::Equal => write!(f, "="),
            InfixOperator::NotEqual => write!(f, "!="),
            InfixOperator::And => write!(f, "&&"),
            InfixOperator::Or => write!(f, "||"),
            InfixOperator::LowerThan => write!(f, "<"),
            InfixOperator::GreaterThan => write!(f, ""),
        }
    }
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
    Index,
}

/// Return the precedence of the given token
pub fn token_precedence(token: &TokenType) -> Precedence {
    match token {
        TokenType::Equal => Precedence::Equals,
        TokenType::NotEqual => Precedence::Equals,
        TokenType::LowerThan => Precedence::LessGreater,
        TokenType::GreaterThan => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Slash => Precedence::Product,
        TokenType::Asterisk => Precedence::Product,
        TokenType::LParen => Precedence::Call,
        TokenType::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}
