use super::super::compiler::{read_be_u16, Bytecode, Instructions, OpCode};
use super::super::{types::*, Object};

const STACK_SIZE: usize = 2048;
pub const GLOBALS_SIZE: usize = 65536;

type VMResult = Result<(), String>;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    pub globals: Vec<Object>,

    stack_pointer: usize,

    last_popped: Object,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> VM {
        VM {
            instructions: bytecode.instructions,
            constants: bytecode.constants,

            stack: Vec::with_capacity(STACK_SIZE),
            globals: Vec::with_capacity(GLOBALS_SIZE),
            stack_pointer: 0,

            last_popped: Object::Void,
        }
    }

    pub fn with_global_store(mut self, store: Vec<Object>) -> VM {
        self.globals = store;
        self
    }

    pub fn run(&mut self) -> VMResult {
        let mut index = 0;
        while index < self.instructions.len() {
            let op = OpCode::from_byte(self.instructions[index]);
            match op {
                OpCode::SetGlobal => {
                    index += 2;
                    let object = self.pop()?;
                    self.globals.push(object);
                }
                OpCode::GetGlobal => {
                    let global = self.read_operand(index);
                    index += 2;
                    let object = self.globals[global as usize].clone();
                    self.push(object)?;
                }
                OpCode::Constant => {
                    let const_index = read_be_u16(&self.instructions[(index + 1)..(index + 3)]);
                    index += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Add
                | OpCode::Sub
                | OpCode::Mul
                | OpCode::Div
                | OpCode::Equal
                | OpCode::NotEqual
                | OpCode::GreaterThan
                | OpCode::LowerThan => {
                    self.binary_operation(op)?;
                }
                OpCode::Not => self.unary_operation(op)?,
                OpCode::True => self.push(Object::Boolean(Boolean(true)))?,
                OpCode::False => self.push(Object::Boolean(Boolean(false)))?,
                OpCode::JumpNotTruthy => {
                    let position = read_be_u16(&self.instructions[(index + 1)..(index + 3)]);

                    match self.pop()? {
                        Object::Boolean(Boolean(false)) => index = position as usize - 1,
                        _ => {
                            index += 2;
                        }
                    }
                }
                OpCode::Jump => {
                    index = read_be_u16(&self.instructions[(index + 1)..(index + 3)]) as usize - 1;
                }
                OpCode::Void => {
                    self.push(Object::Void)?;
                }
                _ => todo!(),
            }
            index += 1;
        }

        Ok(())
    }

    fn read_operand(&self, index: usize) -> u16 {
        read_be_u16(&self.instructions[(index + 1)..(index + 3)])
    }

    fn unary_operation(&mut self, op: OpCode) -> VMResult {
        let value = self.pop()?;

        match op {
            OpCode::Not => self.push(value.not()?),
            _ => Err(String(format!(
                "ERROR: invalid unary operator for {}",
                value
            ))),
        }
    }

    fn binary_operation(&mut self, op: OpCode) -> VMResult {
        let right = self.pop()?;
        let left = self.pop()?;

        let result = match op {
            OpCode::Add => left.plus(right)?,
            OpCode::Sub => left.minus(right)?,
            OpCode::Mul => left.multiplied_by(right)?,
            OpCode::Div => left.divided_by(right)?,
            OpCode::GreaterThan => left.greater_than(right)?,
            OpCode::LowerThan => right.greater_than(left)?,
            OpCode::Equal => Object::Boolean(Boolean(left == right)),
            OpCode::NotEqual => Object::Boolean(Boolean(left != right)),
            _ => return Err(String(format!("ERROR: Invalid binary operator {:?}", op))),
        };

        self.push(result)
    }

    fn push(&mut self, object: Object) -> VMResult {
        if self.stack_pointer >= STACK_SIZE {
            return Err(String(format!("Stack overflow")));
        }

        self.stack.push(object);
        self.stack_pointer = self.stack.len();
        Ok(())
    }

    fn pop(&mut self) -> Result<Object, String> {
        if let Some(object) = self.stack.pop() {
            self.last_popped = object.clone();

            return Ok(object);
        }

        return Err(String(format!("Stack underflow")));
    }

    pub fn last_popped(&self) -> Object {
        self.last_popped.clone()
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.stack_pointer == 0 {
            None
        } else {
            self.stack.last()
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::Compiler;
    use super::super::super::{parser::ast::Program, Lexer, Object, Parser};
    use super::Integer;
    use super::VM;
    use std::fmt::Display;

    struct VMTestCase<T: Display> {
        pub input: String,
        pub expected: T,
    }

    #[test]
    fn test_string_expression() {
        let tests = vec![
            VMTestCase {
                input: r#"
					"nameless"
					"#
                .into(),
                expected: "nameless".to_string(),
            },
            VMTestCase {
                input: r#""nameless" + "lang""#.into(),
                expected: "namelesslang".to_string(),
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_global_let_statements() {
        let tests = vec![
            VMTestCase {
                input: r#"
					let one = 1; 
					one;"#
                    .into(),
                expected: 1,
            },
            VMTestCase {
                input: "let one = 1; let two = 2; one + two;".into(),
                expected: 3,
            },
            VMTestCase {
                input: "let one = 1; let two = one + one; one + two;".into(),
                expected: 3,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            VMTestCase {
                input: r#"
					if true {
						10;
					}
					"#
                .into(),
                expected: Object::Integer(Integer(10)),
            },
            VMTestCase {
                input: r#"
					if true {
						10;
					} else {
						20;
					}
					"#
                .into(),
                expected: Object::Integer(Integer(10)),
            },
            VMTestCase {
                input: r#"
					if false {
						10;
					} else {
						20;
					}
					"#
                .into(),
                expected: Object::Integer(Integer(20)),
            },
            VMTestCase {
                input: r#"
					if 1 < 2 {
						10;
					}"#
                .into(),
                expected: Object::Integer(Integer(10)),
            },
            VMTestCase {
                input: r#"
					if 1 > 2 {
						10;
					} else {
						20;
					}"#
                .into(),
                expected: Object::Integer(Integer(20)),
            },
            VMTestCase {
                input: r#"
					if 1 > 2 {
						10;
					} else if 1 < 2 {
						20;
					}"#
                .into(),
                expected: Object::Integer(Integer(20)),
            },
            VMTestCase {
                input: r#"
					if false {
						10;
					}"#
                .into(),
                expected: Object::Void,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_boolean_arithmetic() {
        let tests = vec![
            VMTestCase {
                input: "true".into(),
                expected: true,
            },
            VMTestCase {
                input: "!false".into(),
                expected: true,
            },
            VMTestCase {
                input: "false".into(),
                expected: false,
            },
            VMTestCase {
                input: "1 < 2;".into(),
                expected: true,
            },
            VMTestCase {
                input: "1 > 2;".into(),
                expected: false,
            },
            VMTestCase {
                input: "1 == 2;".into(),
                expected: false,
            },
            VMTestCase {
                input: "1 != 2;".into(),
                expected: true,
            },
        ];

        run_vm_tests(tests);
    }

    #[test]
    fn test_integer_arithmetic() {
        let test = vec![
            VMTestCase {
                input: "1".into(),
                expected: 1,
            },
            VMTestCase {
                input: "2".into(),
                expected: 2,
            },
            VMTestCase {
                input: "1 + 2;".into(),
                expected: 3,
            },
            VMTestCase {
                input: "1 - 1;".into(),
                expected: 0,
            },
            VMTestCase {
                input: "1 * 2;".into(),
                expected: 2,
            },
            VMTestCase {
                input: "2 / 2;".into(),
                expected: 1,
            },
            VMTestCase {
                input: "(1 + 2) * 4 ".into(),
                expected: 12,
            },
        ];

        run_vm_tests(test);
    }

    fn run_vm_tests<T: Display>(tests: Vec<VMTestCase<T>>) {
        for test in tests.iter() {
            let program = parse(&test.input);

            let mut compiler = Compiler::new();
            compiler.compile(program);

            let mut vm = VM::new(compiler.bytecode());

            vm.run().unwrap();

            assert_eq!(
                format!("{}", test.expected),
                format!("{}", vm.last_popped())
            )
        }
    }

    fn parse(input: &str) -> Program {
        let (program, _) = Parser::new(Lexer::new(input)).parse_program();
        program
    }
}
