use super::evaluator::EvaluatorResult;
use super::Object;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Println,
}
impl Builtin {
    pub fn lookup(identifier: &str) -> Option<Object> {
        match identifier {
            "println" => Some(Object::Builtin(Builtin::Println)),
            _ => None,
        }
    }

    pub fn call(&self, arg: &Object) -> EvaluatorResult {
        match self {
            Builtin::Println => {
                println!("{}", arg);
            }
        }

        Ok(Object::Void)
    }
}
