use crate::compiler::Instructions;

use super::types::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    Integer,
    Boolean,
    String,
    Array,
    Function,
}

/// # Object
/// Represents a value in nameless
#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Object {
    Void,
    Integer(Integer),
    Boolean(Boolean),
    String(String),
    Function(Instructions),
    Array(Vec<Object>),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            Object::Void => write!(f, ""),
            Object::Integer(integer) => write!(f, "{}", integer),
            Object::String(string) => write!(f, "{}", string),
            Object::Boolean(boolean) => write!(f, "{}", boolean),
            Object::Array(array) => write!(
                f,
                "[{}]",
                array
                    .iter()
                    .map(|obj| format!("{}", obj))
                    .collect::<Vec<std::string::String>>()
                    .join(", ")
            ),
            Object::Function(ins) => write!(f, "{}", ins),
        }
    }
}

impl Object {
    pub fn is(&self, t: Type) -> bool {
        self.get_type() == t
    }

    pub fn is_numeric(&self) -> bool {
        self.is(Type::Integer)
    }

    pub fn get_type(&self) -> Type {
        match self {
            Object::Void => Type::Void,
            Object::String(_) => Type::String,
            Object::Boolean(_) => Type::Boolean,
            Object::Integer(_) => Type::Integer,
            Object::Array(_) => Type::Array,
            Object::Function(_) => Type::Function,
        }
    }

    pub fn integer(&self) -> Result<Integer, String> {
        match self {
            Object::Integer(integer) => Ok(integer.clone()),
            obj => Err(String(format!("ERROR: {} is not a integer", obj))),
        }
    }

    pub fn string(&self) -> Result<String, String> {
        match self {
            Object::String(string) => Ok(string.clone()),
            obj => Err(String(format!("ERROR: {} is not a string", obj))),
        }
    }

    pub fn bool(&self) -> Result<Boolean, String> {
        match self {
            Object::Boolean(boolean) => Ok(boolean.clone()),
            obj => Err(String(format!("ERROR: {} is not a boolean", obj))),
        }
    }

    pub fn plus(&self, other: Object) -> Result<Object, String> {
        match self {
            Object::Integer(integer) => integer.plus(other),
            Object::String(string) => Ok(Object::String(String(format!("{}{}", string, other)))),
            _ => Err(String(format!(
                "ERROR: invalid operand + for types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ))),
        }
    }

    pub fn minus(&self, other: Object) -> Result<Object, String> {
        match self {
            Object::Integer(integer) => integer.minus(other),
            _ => Err(String(format!(
                "ERROR: invalid operand - for types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ))),
        }
    }

    pub fn multiplied_by(&self, other: Object) -> Result<Object, String> {
        match self {
            Object::Integer(integer) => integer.multiplied_by(other),
            _ => Err(String(format!(
                "ERROR: invalid operand * for types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ))),
        }
    }

    pub fn divided_by(&self, other: Object) -> Result<Object, String> {
        match self {
            Object::Integer(integer) => integer.divided_by(other),
            _ => Err(String(format!(
                "ERROR: invalid operand / for types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ))),
        }
    }

    pub fn greater_than(&self, other: Object) -> Result<Object, String> {
        match self {
            Object::Integer(integer) => integer.greater_than(other),
            _ => Err(String(format!(
                "ERROR: invalid operand > for types {:?} and {:?}",
                self.get_type(),
                other.get_type()
            ))),
        }
    }

    pub fn not(&self) -> Result<Object, String> {
        match self {
            Object::Boolean(boolean) => Ok(Object::Boolean(!*boolean)),
            _ => Err(String(format!(
                "ERROR: invalid operand ! for type {:?}",
                self.get_type(),
            ))),
        }
    }
}
