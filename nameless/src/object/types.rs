use super::object::{Object, Type};
use std::fmt::{Display, Formatter, Result as FmtResult};

pub trait Value<T = Self> {
    fn value(&self) -> T;
    fn to_string(&self) -> String;
}

pub trait Operand {
    fn plus(&self, other: Object) -> Result<Object, String>;
    fn minus(&self, other: Object) -> Result<Object, String>;
    fn multiplied_by(&self, other: Object) -> Result<Object, String>;
    fn divided_by(&self, other: Object) -> Result<Object, String>;
    fn greater_than(&self, other: Object) -> Result<Object, String>;
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub struct Integer(pub i64);

impl Value<i64> for Integer {
    fn value(&self) -> i64 {
        self.0
    }

    fn to_string(&self) -> String {
        String(self.0.to_string())
    }
}

impl Operand for Integer {
    fn plus(&self, other: Object) -> Result<Object, String> {
        match other {
            Object::String(string) => Ok(Object::String(String(format!("{}{}", self, string)))),
            Object::Integer(integer) => Ok(Object::Integer(Integer(self.0 + integer.value()))),
            _ => Err(String(format!(
                "ERROR: invalid operator for types {:?} and {:?}",
                Type::Integer,
                other.get_type()
            ))),
        }
    }

    fn minus(&self, other: Object) -> Result<Object, String> {
        match other {
            Object::Integer(integer) => Ok(Object::Integer(Integer(self.0 - integer.value()))),
            _ => Err(String(format!(
                "ERROR: invalid operator for types {:?} and {:?}",
                Type::Integer,
                other.get_type()
            ))),
        }
    }

    fn multiplied_by(&self, other: Object) -> Result<Object, String> {
        match other {
            Object::Integer(integer) => Ok(Object::Integer(Integer(self.0 * integer.value()))),
            _ => Err(String(format!(
                "ERROR: invalid operator for types {:?} and {:?}",
                Type::Integer,
                other.get_type()
            ))),
        }
    }

    fn divided_by(&self, other: Object) -> Result<Object, String> {
        match other {
            Object::Integer(integer) => Ok(Object::Integer(Integer(self.0 / integer.value()))),
            _ => Err(String(format!(
                "ERROR: invalid operator for types {:?} and {:?}",
                Type::Integer,
                other.get_type()
            ))),
        }
    }

    fn greater_than(&self, other: Object) -> Result<Object, String> {
        match other {
            Object::Integer(integer) => Ok(Object::Boolean(Boolean(self.0 > integer.value()))),
            _ => Err(String(format!(
                "ERROR: invalid operator for types {:?} and {:?}",
                Type::Integer,
                other.get_type()
            ))),
        }
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Hash)]
pub struct Boolean(pub bool);

impl std::ops::Not for Boolean {
    type Output = Self;

    fn not(self) -> Self::Output {
        Boolean(!self.0)
    }
}

impl Value<bool> for Boolean {
    fn value(&self) -> bool {
        self.0
    }

    fn to_string(&self) -> String {
        String(self.0.to_string())
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl Display for String {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct String(pub std::string::String);

impl Value<std::string::String> for String {
    fn value(&self) -> std::string::String {
        self.0.clone()
    }

    fn to_string(&self) -> String {
        String(self.0.to_string())
    }
}
