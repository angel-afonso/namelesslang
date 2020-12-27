use super::evaluator::{EvaluatorError, EvaluatorResult, Stream};
use super::Object;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Println,
    Print,
    Input,
    Len,
}

impl Builtin {
    pub fn lookup(identifier: &str) -> Option<Object> {
        match identifier {
            "println" => Some(Object::Builtin(Builtin::Println)),
            "len" => Some(Object::Builtin(Builtin::Len)),
            "print" => Some(Object::Builtin(Builtin::Print)),
            "input" => Some(Object::Builtin(Builtin::Input)),
            _ => None,
        }
    }

    pub fn call<OUT, IN>(&self, arg: &Object, stream: &Stream<OUT, IN>) -> EvaluatorResult
    where
        OUT: FnMut(String) + std::ops::Fn(String),
        IN: FnMut() -> String + std::ops::Fn() -> String,
    {
        match self {
            Builtin::Println => (stream.stdout)(format!("{}\n", arg.to_string())),
            Builtin::Print => (stream.stdout)(arg.to_string()),
            Builtin::Input => {
                if arg != &Object::Void {
                    (stream.stdout)(arg.to_string())
                }

                return Ok(Object::String((stream.stdin)()));
            }
            Builtin::Len => match arg {
                Object::Array(array) => return Ok(Object::Integer(array.len() as i64)),
                obj => return Err(EvaluatorError(format!("{} is not countable", obj))),
            },
        }

        Ok(Object::Void)
    }
}
