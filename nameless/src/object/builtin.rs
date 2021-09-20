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

    pub fn call(&self, args: Vec<Object>) -> Object {
        match self {
            BuiltIn::Println => println_builtin(args),
            _ => todo!(),
        }
    }
}

fn println_builtin(args: Vec<Object>) -> Object {
    println!(
        "{}",
        args.iter()
            .map(|arg| { format!("{}", arg) })
            .collect::<Vec<String>>()
            .join(" ")
    );
    Object::Void
}
