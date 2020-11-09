use super::super::parser::ast::*;
use super::Env;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Vec<Identifer>, Block, Env),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Null => write!(f, "null"),
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "{}", string),
            Object::ReturnValue(return_value) => write!(f, "{}", return_value),
            Object::Error(error) => write!(f, "{}", error),
            Object::Function(_, _, _) => write!(f, "fn"),
        }
    }
}
