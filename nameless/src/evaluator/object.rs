use super::super::parser::ast::*;
use super::builtin::Builtin;
use super::Environment;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Null,
    Void,
    Integer,
    Float,
    Boolean,
    String,
    ReturnValue,
    Function,
    Array,
}

/// # Object
/// Represents a value in nameless
#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Void,
    Null,
    Array(Box<Vec<Object>>),
    Integer(i64),
    Float(f64),
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
            Object::Float(_) => Type::Float,
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

    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    pub fn is_integer(&self) -> bool {
        self.object_type() == Type::Integer
    }

    fn get_int(&self) -> i64 {
        match self {
            Object::Integer(int) => *int,
            _ => 0,
        }
    }

    fn get_float(&self) -> f64 {
        match self {
            Object::Integer(int) => *int as f64,
            Object::Float(float) => *float,
            _ => 0 as f64,
        }
    }

    pub fn is_float(&self) -> bool {
        self.object_type() == Type::Float
    }

    pub fn add(self, other: Object) -> Result<Object, String> {
        if self.is_integer() && other.is_integer() {
            return Ok(Object::Integer(self.get_int() + other.get_int()));
        }

        if self.is_numeric() && self.is_numeric() {
            return Ok(Object::Float(self.get_float() + self.get_float()));
        }

        todo!()
    }

    pub fn sub(self, other: Object) -> Result<Object, String> {
        if self.is_integer() && other.is_integer() {
            return Ok(Object::Integer(self.get_int() - other.get_int()));
        }

        if self.is_numeric() && self.is_numeric() {
            return Ok(Object::Float(self.get_float() - self.get_float()));
        }

        todo!()
    }

    pub fn multiply(self, other: Object) -> Result<Object, String> {
        if self.is_integer() && other.is_integer() {
            return Ok(Object::Integer(self.get_int() * other.get_int()));
        }

        if self.is_numeric() && self.is_numeric() {
            return Ok(Object::Float(self.get_float() * self.get_float()));
        }

        todo!()
    }

    pub fn divide(self, other: Object) -> Result<Object, String> {
        if self.is_integer() && other.is_integer() {
            return Ok(Object::Integer(self.get_int() / other.get_int()));
        }

        if self.is_numeric() && self.is_numeric() {
            return Ok(Object::Float(self.get_float() / self.get_float()));
        }

        todo!()
    }

    pub fn greater_than(self, other: Object) -> Object {
        return Object::Boolean(self.get_float() > other.get_float());
    }
    pub fn lower_than(self, other: Object) -> Object {
        return Object::Boolean(self.get_float() < other.get_float());
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Void => write!(f, ""),
            Object::Null => write!(f, "null"),
            Object::Integer(int) => write!(f, "{}", int),
            Object::Float(float) => write!(f, "{}", float),
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
