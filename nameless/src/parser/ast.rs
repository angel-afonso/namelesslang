use super::super::lexer::{Token, TokenType};
use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

impl Location {
    pub fn from_token(token: &Token) -> Location {
        Location {
            line: token.line,
            column: token.column,
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {} column: {}", self.line, self.column)
    }
}

/// Store all the parse program
pub type Program = Vec<Statement>;

/// Identifier like variables and function names
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Identifer {
    pub location: Location,
    pub value: String,
}

/// Code block
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block {
    pub location: Location,
    pub statements: Vec<Statement>,
}

/// Let statement struct
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub location: Location,
    pub identifier: Identifer,
    pub value: Option<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Assignment {
    pub location: Location,
    pub identifier: Identifer,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prefix {
    pub location: Location,
    pub operator: PrefixOperator,
    pub expression: Box<Expression>,
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Infix {
    pub location: Location,
    pub operator: InfixOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

/// Conditional structure
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub location: Location,
    pub condition: Box<Expression>,
    pub consequence: Block,
    pub alternative: Option<Else>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Else {
    If(Location, Box<If>),
    Block(Location, Block),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct For {
    pub location: Location,
    pub counter: Box<Statement>,
    pub condition: Expression,
    pub step: Box<Statement>,
    pub block: Block,
}

/// Function represetation
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Fn {
    pub location: Location,
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
    pub location: Location,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

/// Represents the literal values
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Literal {
    Int(Location, i64),
    Bool(Location, bool),
    String(Location, String),
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
    pub location: Location,
    pub expressions: Box<Vec<Expression>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index {
    pub location: Location,
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
            Expression::Array(array) => write!(
                f,
                "{}",
                array
                    .expressions
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
        }
    }
}

/// Represents all the posible statements
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Let(Let),
    Return(Option<Expression>),
    Block(Block),
    If(If),
    Fn(Fn),
    Call(Call),
    Assignment(Assignment),
    For(For),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(let_stmt) => write!(
                f,
                "let {}{};",
                let_stmt.identifier.value,
                match &let_stmt.value {
                    Some(value) => format!(" = {}", value),
                    None => String::new(),
                }
            ),
            Statement::Return(Some(expression)) => write!(f, "return{};", expression),
            Statement::Block(block) => write!(
                f,
                "{{\n{}\n}}",
                block
                    .statements
                    .iter()
                    .map(|stmt| format!("{}", stmt))
                    .collect::<Vec<String>>()
                    .join("\n"),
            ),
            _ => write!(f, ""),
        }
    }
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