use super::super::parser::ast::*;
use super::Env;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Null,
    Integer,
    Boolean,
    String,
    ReturnValue,
    Error,
    Function,
}

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

impl Object {
    pub fn object_type(&self) -> Type {
        match &self {
            Object::Integer(_) => Type::Integer,
            Object::Boolean(_) => Type::Boolean,
            Object::String(_) => Type::String,
            Object::ReturnValue(_) => Type::ReturnValue,
            Object::Error(_) => Type::Error,
            Object::Function(_, _, _) => Type::Function,
            Object::Null => Type::Null,
        }
    }
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
