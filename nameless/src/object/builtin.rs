use crate::Object;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BuiltIn {
    Println,
    Print,
    Input,
    Len,
}

pub fn builtin_fns() -> Vec<&'static str> {
    vec!["println"]
}

impl BuiltIn {
    pub fn lookup(identifier: &str) -> Object {
        match identifier {
            "println" => (Object::Builtin(BuiltIn::Println)),
            _ => unreachable!(),
        }
    }

    pub fn by_index(index: u32) -> Object {
        Self::lookup(builtin_fns()[index as usize])
    }

    pub fn call<OUT, IN>(&self, args: Vec<Object>, _stdin: IN, stdout: OUT) -> Object
    where
        OUT: FnMut(String) + std::ops::Fn(String),
        IN: FnMut() -> String + std::ops::Fn() -> String,
    {
        match self {
            BuiltIn::Println => println_builtin(args, stdout),
            _ => todo!(),
        }
    }
}

fn println_builtin<OUT>(args: Vec<Object>, stdout: OUT) -> Object
where
    OUT: FnMut(String) + std::ops::Fn(String),
{
    (stdout)(format!(
        "{}",
        args.iter()
            .map(|arg| format!("{}", arg))
            .collect::<Vec<String>>()
            .join(" "),
    ));
    Object::Void
}
