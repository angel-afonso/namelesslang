use crate::object::builtin::BuiltIn;

use super::super::compiler::{read_be_u16, Bytecode, Instructions, OpCode};
use super::super::{types::*, Object};
use std::string::String as StdString;

fn execution_error(message: std::string::String) -> Result<(), String> {
    Err(String(format!("Runtime error: {}", message)))
}

const STACK_SIZE: usize = 2048;
const MAX_FRAMES: usize = 1024;
pub const GLOBALS_SIZE: usize = 65536;

type VMResult = Result<(), String>;

#[derive(Clone, Debug)]
pub struct Frame {
    function: Instructions,
    pointer: usize,
    base: usize,
}

pub struct Stream<OUT, IN>
where
    OUT: FnMut(StdString) + std::ops::Fn(StdString),
    IN: FnMut() -> StdString + std::ops::Fn() -> StdString,
{
    pub stdout: OUT,
    pub stdin: IN,
}

impl<OUT, IN> Stream<OUT, IN>
where
    OUT: FnMut(StdString) + std::ops::Fn(StdString),
    IN: FnMut() -> StdString + std::ops::Fn() -> StdString,
{
    pub fn new(out: OUT, input: IN) -> Stream<OUT, IN> {
        return Stream {
            stdout: out,
            stdin: input,
        };
    }
}

impl Frame {
    pub fn new(function: Instructions, base: usize) -> Frame {
        Frame {
            function,
            pointer: 0,
            base,
        }
    }
}

pub struct VM<OUT, IN>
where
    OUT: FnMut(StdString) + std::ops::Fn(StdString),
    IN: FnMut() -> StdString + std::ops::Fn() -> StdString,
{
    constants: Vec<Object>,

    stack: Vec<Object>,
    stack_pointer: usize,

    pub globals: Vec<Object>,

    frames: Vec<Frame>,
    frames_index: usize,

    last_popped: Object,
    stream: Stream<OUT, IN>,
}

impl<OUT, IN> VM<OUT, IN>
where
    OUT: FnMut(StdString) + std::ops::Fn(StdString),
    IN: FnMut() -> StdString + std::ops::Fn() -> StdString,
{
    pub fn new(bytecode: Bytecode, stream: Stream<OUT, IN>) -> VM<OUT, IN>
    where
        OUT: FnMut(StdString) + std::ops::Fn(StdString),
        IN: FnMut() -> StdString + std::ops::Fn() -> StdString,
    {
        let frames = {
            let mut vec = Vec::with_capacity(MAX_FRAMES);
            vec.push(Frame::new(bytecode.instructions, 0));
            vec
        };

        VM {
            constants: bytecode.constants,

            stack: Vec::with_capacity(STACK_SIZE),
            stack_pointer: 0,

            globals: Vec::with_capacity(GLOBALS_SIZE),

            frames,
            frames_index: 0,

            last_popped: Object::Void,
            stream,
        }
    }

    fn instructions(&self) -> Instructions {
        self.current_frame().function
    }

    fn set_instruction_pointer(&mut self, pointer: usize) {
        self.frames[self.frames_index].pointer = pointer;
    }

    fn increment_instruction_pointer(&mut self, amount: usize) {
        self.frames[self.frames_index].pointer += amount;
    }

    fn current_instruction_pointer(&self) -> usize {
        self.frames[self.frames_index].pointer
    }

    fn current_frame(&self) -> Frame {
        self.frames[self.frames_index].clone()
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames.push(frame);
        self.frames_index = self.frames.len() - 1;
    }

    fn pop_frame(&mut self) -> Frame {
        self.frames_index -= 1;
        return self.frames.pop().unwrap();
    }

    pub fn with_global_store(mut self, store: Vec<Object>) -> VM<OUT, IN> {
        self.globals = store;
        self
    }

    pub fn run(&mut self) -> VMResult {
        while self.current_instruction_pointer() < self.instructions().len() {
            let instructions = self.instructions();
            let mut index = self.current_instruction_pointer();

            let op = OpCode::from_byte(instructions[index]);

            match op {
                OpCode::GetBuiltIn => {
                    index += 1;

                    let built_index = instructions[index] as u32;

                    self.push(BuiltIn::by_index(built_index))?
                }
                OpCode::SetGlobal => {
                    index += 2;
                    let object = self.pop();
                    self.globals.push(object);
                }
                OpCode::SetLocal => {
                    let local_index = instructions[index + 1] as usize;

                    let frame = self.current_frame();

                    self.stack[frame.base + local_index] = self.pop();

                    self.increment_instruction_pointer(2);
                    continue;
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
                OpCode::GetLocal => {
                    let local_index = instructions[index + 1] as usize;

                    self.increment_instruction_pointer(2);

                    let frame = self.current_frame();

                    self.push(self.stack[frame.base + local_index].clone())?;
                    continue;
                }
                OpCode::Constant => {
                    let const_index = read_be_u16(&instructions[(index + 1)..(index + 3)]);
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
                    let position = read_be_u16(&instructions[(index + 1)..(index + 3)]);

                    match self.pop() {
                        Object::Boolean(Boolean(false)) => index = position as usize - 1,
                        _ => {
                            index += 2;
                        }
                    }
                }
                OpCode::Jump => {
                    index = read_be_u16(&instructions[(index + 1)..(index + 3)]) as usize - 1;
                }
                OpCode::Void => {
                    self.push(Object::Void)?;
                }
                OpCode::Array => {
                    let num_elements =
                        read_be_u16(&instructions[(index + 1)..(index + 3)]) as usize;
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
                OpCode::Call => {
                    index += 1;

                    let num_args = instructions[index] as usize;

                    self.increment_instruction_pointer(2);

                    match self.stack[self.stack_pointer - 1 - num_args].clone() {
                        Object::Function(func) => self.call_function(func, num_args)?,
                        Object::Builtin(builtin) => self.call_built_in(builtin, num_args)?,
                        obj => return execution_error(format!("{} is not a function", obj)),
                    };

                    continue;
                }
                OpCode::ReturnValue => {
                    let value = self.pop();

                    let frame = self.pop_frame();

                    self.stack.resize(frame.base - 1, Object::Void);

                    self.stack_pointer = frame.base - 1;

                    self.push(value)?;

                    continue;
                }
                OpCode::Return => {
                    let frame = self.pop_frame();

                    self.stack_pointer = frame.base - 1;

                    self.push(Object::Void)?;

                    continue;
                }
                OpCode::Invalid => execution_error("Invalid opcode".into())?,
                code => todo!("{:?}", code),
            }
            self.set_instruction_pointer((index + 1) as usize);
        }

        Ok(())
    }

    fn call_built_in(&mut self, builtin: BuiltIn, args_len: usize) -> VMResult {
        let args = self.stack[(self.stack_pointer - args_len)..self.stack_pointer].to_vec();

        let result = builtin.call(args, &self.stream.stdin, &self.stream.stdout);

        self.stack_pointer -= args_len - 1;

        self.push(result)?;

        Ok(())
    }

    fn call_function(&mut self, function: Function, args_len: usize) -> VMResult {
        let frame = Frame::new(function.instructions, self.stack_pointer - args_len);

        self.stack
            .resize(frame.base + function.locals, Object::Void);

        self.stack_pointer = frame.base + function.locals;

        self.push_frame(frame);

        Ok(())
    }

    fn read_operand(&self, index: usize) -> u16 {
        read_be_u16(&self.instructions()[(index + 1)..(index + 3)])
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
