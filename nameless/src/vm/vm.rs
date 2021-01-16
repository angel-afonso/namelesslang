use super::super::compiler::{read_be_u16, Bytecode, Instructions, OpCode};
use super::super::Object;

const STACK_SIZE: usize = 2048;

type VMResult = Result<(), String>;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    stack_pointer: usize,

    last_popped: Object,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> VM {
        VM {
            instructions: bytecode.instructions,
            constants: bytecode.constants,

            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,

            last_popped: Object::Void,
        }
    }

    pub fn run(&mut self) -> VMResult {
        let mut index = 0;
        while index < self.instructions.len() {
            let op = OpCode::from_byte(self.instructions[index]);
            match op {
                OpCode::Constant => {
                    let const_index = read_be_u16(&self.instructions[(index + 1)..(index + 3)]);
                    index += 2;
                    self.push(self.constants[const_index as usize].clone())?;
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::Div => {
                    self.binary_operation(op)?;
                }
                OpCode::Equal | OpCode::NotEqual | OpCode::GreaterThan | OpCode::LowerThan => {
                    self.comparation(op)?;
                }
                OpCode::Not => self.not_operator()?,
                OpCode::True => self.push(Object::Boolean(true))?,
                OpCode::False => self.push(Object::Boolean(false))?,
                _ => todo!(),
            }
            index += 1;
        }

        Ok(())
    }

    fn not_operator(&mut self) -> VMResult {
        let value = self.pop()?;

        match value {
            Object::Boolean(value) => self.push(Object::Boolean(!value)),
            _ => Err(format!("ERROR: invalid operator for {}", value)),
        }
    }

    fn binary_operation(&mut self, op: OpCode) -> VMResult {
        let right = self.pop()?;
        let left = self.pop()?;

        if right.is_numeric() && left.is_numeric() {
            return self.binary_numeric_operation(op, left, right);
        }

        return Err(format!(
            "ERROR: Unsuported types for binary operation, {:?} and {:?}",
            left.object_type(),
            right.object_type()
        ));
    }

    fn comparation(&mut self, op: OpCode) -> VMResult {
        let right = self.pop()?;
        let left = self.pop()?;

        if right.object_type() != left.object_type() {
            return Err(format!("ERROR: Cannot compare {} and {}", left, right));
        }

        if left.is_numeric() && right.is_numeric() {
            match op {
                OpCode::GreaterThan | OpCode::LowerThan => {
                    return self.integer_comparation(op, left, right);
                }
                _ => {}
            }
        }

        match op {
            OpCode::Equal => self.push(Object::Boolean(left == right)),
            OpCode::NotEqual => self.push(Object::Boolean(left != right)),
            _ => {
                return Err(format!(
                    "ERROR: Invalid operator {:?} for {} and {}",
                    op, left, right
                ))
            }
        }
    }

    fn integer_comparation(&mut self, op: OpCode, left: Object, right: Object) -> VMResult {
        match op {
            OpCode::GreaterThan => self.push(left.greater_than(right)),
            OpCode::LowerThan => self.push(right.greater_than(left)),
            _ => {
                return Err(format!(
                    "ERROR: Invalid operator {:?} for {} and {}",
                    op, left, right
                ))
            }
        }
    }

    fn binary_numeric_operation(&mut self, op: OpCode, left: Object, right: Object) -> VMResult {
        match op {
            OpCode::Add => self.push(left.add(right)?),
            OpCode::Sub => self.push(left.sub(right)?),
            OpCode::Mul => self.push(left.multiply(right)?),
            OpCode::Div => self.push(left.divide(right)?),
            _ => Err(format!("ERROR: Invalid operator {:?}", op)),
        }
    }

    fn push(&mut self, object: Object) -> VMResult {
        if self.stack_pointer >= STACK_SIZE {
            return Err(format!("Stack overflow"));
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

        return Err(format!("Stack underflow"));
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
    use super::super::super::{parser::ast::Program, Lexer, Parser};
    use super::VM;
    use std::fmt::Display;

    struct VMTestCase<T: Display> {
        pub input: String,
        pub expected: T,
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
