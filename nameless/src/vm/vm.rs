use super::super::compiler::{read_be_u16, Bytecode, Instructions, OpCode};
use super::super::Object;

const STACK_SIZE: usize = 2048;

type VMResult = Result<(), String>;

pub struct VM {
    constants: Vec<Object>,
    instructions: Instructions,

    stack: Vec<Object>,
    stack_pointer: usize,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> VM {
        VM {
            instructions: bytecode.instructions,
            constants: bytecode.constants,

            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,
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
                OpCode::Add => {
                    let right = self.pop()?;
                    let left = self.pop()?;

                    self.push(left.add(right)?)?;
                }
            }
            index += 1;
        }

        Ok(())
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
            return Ok(object);
        }

        return Err(format!("Stack underflow"));
    }

    pub fn stack_top(&self) -> Option<&Object> {
        if self.stack_pointer == 0 {
            None
        } else {
            Some(&self.stack[self.stack_pointer - 1])
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
                format!("{}", vm.stack_top().unwrap())
            )
        }
    }

    fn parse(input: &str) -> Program {
        let (program, _) = Parser::new(Lexer::new(input)).parse_program();
        program
    }
}
