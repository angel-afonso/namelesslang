use super::super::parser::ast::*;
use super::Env;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Null,
    Void,
    Integer,
    Boolean,
    String,
    ReturnValue,
    Error,
    Function,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Void,
    Null,
    Integer(i64),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Error(String),
    Function(Identifer, Vec<Identifer>, Block, Env),
}

impl Object {
    pub fn object_type(&self) -> Type {
        match &self {
            Object::Integer(_) => Type::Integer,
            Object::Boolean(_) => Type::Boolean,
            Object::String(_) => Type::String,
            Object::ReturnValue(_) => Type::ReturnValue,
            Object::Error(_) => Type::Error,
            Object::Function(_, _, _, _) => Type::Function,
            Object::Void => Type::Void,
            Object::Null => Type::Null,
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Void => write!(f, ""),
            Object::Null => write!(f, "null"),
            Object::Integer(int) => write!(f, "{}", int),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::String(string) => write!(f, "{}", string),
            Object::ReturnValue(return_value) => write!(f, "{}", return_value),
            Object::Error(error) => write!(f, "{}", error),
            Object::Function(_, params, body, _) => write!(f, "fn {:?} {{{:?}}}", params, body),
        }
    }
}
