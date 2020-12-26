use super::super::parser::ast::*;
use super::builtin::Builtin;
use super::Environment;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Null,
    Void,
    Integer,
    Boolean,
    String,
    ReturnValue,
    Function,
    Array,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Null,
    Array(Box<Vec<Object>>),
    Integer(i64),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Function(Identifer, Vec<Identifer>, Block, Environment),
    Builtin(Builtin),
}

impl Object {
    pub fn object_type(&self) -> Type {
        match &self {
            Object::Integer(_) => Type::Integer,
            Object::Boolean(_) => Type::Boolean,
            Object::String(_) => Type::String,
            Object::ReturnValue(_) => Type::ReturnValue,
            Object::Function(_, _, _, _) => Type::Function,
            Object::Void => Type::Void,
            Object::Null => Type::Null,
            Object::Builtin(_) => Type::Function,
            Object::Array(_) => Type::Array,
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
            Object::Function(_, params, body, _) => write!(f, "fn {:?} {{{:?}}}", params, body),
            Object::Builtin(_) => write!(f, "fn builtin"),
            Object::Array(array) => write!(
                f,
                "[{}]",
                array
                    .iter()
                    .map(|obj| format!("{}", obj))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}
