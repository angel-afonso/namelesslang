use super::evaluator::{EvaluatorError, EvaluatorResult};
use super::Object;
use std::sync::mpsc::Sender;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Println,
    Len,
}

impl Builtin {
    pub fn lookup(identifier: &str) -> Option<Object> {
        match identifier {
            "println" => Some(Object::Builtin(Builtin::Println)),
            "len" => Some(Object::Builtin(Builtin::Len)),
            _ => None,
        }
    }

    pub fn call(&self, arg: &Object, out: &Sender<Object>) -> EvaluatorResult {
        match self {
            Builtin::Println => match out.send(arg.clone()) {
                Err(err) => return Err(EvaluatorError(format!("{}", err))),
                _ => {}
            },
            Builtin::Len => match arg {
                Object::Array(array) => return Ok(Object::Integer(array.len() as i64)),
                obj => return Err(EvaluatorError(format!("{} is not countable", obj))),
            },
        }

        Ok(Object::Void)
    }
}
