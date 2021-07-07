use super::super::compiler::{read_be_u16, Bytecode, Instructions, OpCode};
use super::super::{types::*, Object};

fn execution_error(message: std::string::String) -> Result<(), String> {
    Err(String(format!("Runtime error: {}", message)))
}

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
                    let object = self.pop();
                    self.globals.push(object);
                }
                OpCode::UpdateGlobal => {
                    let global = self.read_operand(index);
                    index += 2;
                    self.globals[global as usize] = self.pop();
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
                    self.pop();
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

                    match self.pop() {
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
                OpCode::Array => {
                    let num_elements =
                        read_be_u16(&self.instructions[(index + 1)..(index + 3)]) as usize;
                    index += 2;

                    let array =
                        self.build_array(self.stack_pointer - num_elements, self.stack_pointer);
                    self.stack_pointer -= num_elements;

                    self.push(array)?
                }
                OpCode::Index => {
                    let index = self.pop();
                    let left = self.pop();

                    self.index_operation(left, index)?
                }
                OpCode::Invalid => execution_error("Invalid opcode".into())?,
            }
            index += 1;
        }

        Ok(())
    }

    fn read_operand(&self, index: usize) -> u16 {
        read_be_u16(&self.instructions[(index + 1)..(index + 3)])
    }

    fn index_operation(&mut self, left: Object, index: Object) -> VMResult {
        let index = match index {
            Object::Integer(int) => int.0,
            expr => return execution_error(format!("Invalid index {}", expr)),
        };

        match left {
            Object::Array(array) => self.push(array[index as usize].clone()),
            expr => execution_error(format!("Unsuported index for {}", expr)),
        }
    }

    fn unary_operation(&mut self, op: OpCode) -> VMResult {
        let value = self.pop();

        match op {
            OpCode::Not => self.push(value.not()?),
            _ => execution_error(format!("Invalid unary operator for {}", value)),
        }
    }

    fn binary_operation(&mut self, op: OpCode) -> VMResult {
        let right = self.pop();
        let left = self.pop();

        let result = match op {
            OpCode::Add => left.plus(right)?,
            OpCode::Sub => left.minus(right)?,
            OpCode::Mul => left.multiplied_by(right)?,
            OpCode::Div => left.divided_by(right)?,
            OpCode::GreaterThan => left.greater_than(right)?,
            OpCode::LowerThan => right.greater_than(left)?,
            OpCode::Equal => Object::Boolean(Boolean(left == right)),
            OpCode::NotEqual => Object::Boolean(Boolean(left != right)),
            _ => return execution_error(format!("Invalid binary operator {:?}", op)),
        };

        self.push(result)
    }

    fn build_array(&mut self, start_index: usize, end_index: usize) -> Object {
        Object::Array(self.stack[start_index..end_index].to_vec())
    }

    fn push(&mut self, object: Object) -> VMResult {
        if self.stack_pointer >= STACK_SIZE {
            return execution_error("Stack overflow".into());
        }

        self.stack.push(object);
        self.stack_pointer = self.stack.len();
        Ok(())
    }

    fn pop(&mut self) -> Object {
        if let Some(object) = self.stack.pop() {
            self.last_popped = object.clone();

            return object;
        }

        self.last_popped = Object::Void;
        Object::Void
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
