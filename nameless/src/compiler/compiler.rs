use super::symbol_table::SymbolTable;
use super::{make, Instructions, OpCode};
use crate::{parser::ast::*, types::*, Object};

type CompilerResult = Result<(), String>;

fn compilation_error(message: std::string::String) -> Result<(), String> {
    Err(String(format!("Compilation error: {}", message)))
}

/// # Bytecode
/// Holds the fully compiled program in bytecode format
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[derive(Clone, Debug)]
struct EmittedInstruction {
    op: OpCode,
    position: usize,
}

/// # Compiler
/// Handle the compilation process into bytecode
pub struct Compiler {
    instructions: Instructions,
    pub constants: Vec<Object>,

    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,

    pub symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: Instructions::new(Vec::new()),
            constants: Vec::new(),

            last_instruction: EmittedInstruction {
                op: OpCode::Invalid,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                op: OpCode::Invalid,
                position: 0,
            },

            symbol_table: SymbolTable::new(),
        }
    }

    pub fn with_state(mut self, symbol_table: SymbolTable, constants: Vec<Object>) -> Compiler {
        self.symbol_table = symbol_table;
        self.constants = constants;
        self
    }

    pub fn compile(&mut self, program: Program) -> CompilerResult {
        for stmt in program {
            self.compile_statement(stmt)?;
        }

        Ok(())
    }

    fn compile_statement(&mut self, statement: Statement) -> CompilerResult {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(OpCode::Pop, vec![]);
            }
            // Statement::If(stmt) => self.compile_if_statement(stmt)?,
            Statement::Block(block) => self.compile_block_statement(block)?,
            Statement::Let(stmt) => self.compile_let_statement(stmt)?,
            stmt => return compilation_error(format!("Expected statement, got: {}", stmt)),
        }

        Ok(())
    }

    pub fn compile_let_statement(&mut self, stmt: Let) -> CompilerResult {
        match stmt.value {
            Some(expression) => self.compile_expression(expression)?,
            None => {
                self.emit(OpCode::Void, vec![]);
            }
        }

        let symbol = self.symbol_table.define(&stmt.identifier.name);
        self.emit(OpCode::SetGlobal, vec![symbol.index]);

        Ok(())
    }

    pub fn compile_block_statement(&mut self, block: Block) -> CompilerResult {
        for stmt in block.statements.iter() {
            self.compile_statement(stmt.clone())?;
        }

        Ok(())
    }

    // fn compile_if_statement(&mut self, statement: If) -> CompilerResult {
    //     self.compile_expression(*statement.condition);
    //     let jump_not_true_position = self.emit(OpCode::JumpNotTruthy, vec![0]);
    //     self.compile_block_statement(statement.consequence)?;

    //     if self.last_instruction_is_pop() {
    //         self.remove_last_pop();
    //     }

    //     let jump_position = self.emit(OpCode::Jump, vec![0]);
    //     self.change_operands(jump_not_true_position, self.instructions.len() as u32);

    //     match statement.alternative {
    //         Some(Else::Block(_, block)) => {
    //             self.compile_block_statement(block)?;
    //             if self.last_instruction_is_pop() {
    //                 self.remove_last_pop();
    //             }
    //         }
    //         Some(Else::If(_, stmt)) => {
    //             self.compile_if_statement(*stmt);

    //             if self.last_instruction_is_pop() {
    //                 self.remove_last_pop();
    //             }
    //         }
    //         None => {
    //             self.emit(OpCode::Void, vec![]);
    //         }
    //     }

    //     self.change_operands(jump_position, self.instructions.len() as u32);
    //     self.emit(OpCode::Pop, vec![]);

    //     Ok(())
    // }

    fn compile_expression(&mut self, expression: Expression) -> CompilerResult {
        match expression {
            Expression::Infix(infix) => self.compile_infix(infix)?,
            Expression::Prefix(prefix) => self.compile_prefix(prefix)?,
            Expression::Literal(literal) => self.compile_literal(literal)?,
            Expression::Identifer(identifier) => self.compile_identifier(identifier)?,
            _ => return compilation_error(format!("Invalid expression {}", expression)),
        }

        Ok(())
    }

    fn compile_identifier(&mut self, identifier: Identifer) -> CompilerResult {
        let symbol = {
            match self.symbol_table.resolve(&identifier.name) {
                Some(symbol) => symbol.clone(),
                None => return compilation_error(format!("Undefined {}", identifier.name)),
            }
        };

        self.emit(OpCode::GetGlobal, vec![symbol.index]);

        Ok(())
    }

    fn compile_prefix(&mut self, prefix: Prefix) -> CompilerResult {
        self.compile_expression(*prefix.expression)?;

        match prefix.operator {
            PrefixOperator::Not => {
                self.emit(OpCode::Not, vec![]);
                Ok(())
            }
            op => compilation_error(format!("Invalid prefix operator {}", op)),
        }
    }

    fn compile_infix(&mut self, infix: Infix) -> CompilerResult {
        self.compile_expression(*infix.left)?;
        self.compile_expression(*infix.right)?;

        match infix.operator {
            InfixOperator::Plus => {
                self.emit(OpCode::Add, vec![]);
            }
            InfixOperator::Minus => {
                self.emit(OpCode::Sub, vec![]);
            }
            InfixOperator::Multiply => {
                self.emit(OpCode::Mul, vec![]);
            }
            InfixOperator::Divide => {
                self.emit(OpCode::Div, vec![]);
            }
            InfixOperator::Equals => {
                self.emit(OpCode::Equal, vec![]);
            }
            InfixOperator::NotEquals => {
                self.emit(OpCode::NotEqual, vec![]);
            }
            InfixOperator::GreaterThan => {
                self.emit(OpCode::GreaterThan, vec![]);
            }
            InfixOperator::LowerThan => {
                self.emit(OpCode::LowerThan, vec![]);
            }
            op => return compilation_error(format!("Unexpected {}", op)),
        }

        Ok(())
    }

    fn compile_literal(&mut self, literal: Literal) -> CompilerResult {
        match literal {
            Literal::Int(_, int) => {
                let integer = Object::Integer(Integer(int));
                let position = self.add_constant(integer);
                self.emit(OpCode::Constant, vec![position as u32]);
            }
            Literal::String(_, string) => {
                let position = self.add_constant(Object::String(String(string)));
                self.emit(OpCode::Constant, vec![position as u32]);
            }
            Literal::Bool(_, boolean) => {
                if boolean {
                    self.emit(OpCode::True, vec![]);
                } else {
                    self.emit(OpCode::False, vec![]);
                }
            }
            _ => return compilation_error(format!("Unexpected {}", literal)),
        }

        Ok(())
    }

    fn add_constant(&mut self, object: Object) -> usize {
        let position = self.constants.len();
        self.constants.push(object);
        position
    }

    fn replace_instruction(&mut self, position: usize, instruction: Instructions) {
        for (index, &ins) in instruction.iter().enumerate() {
            self.instructions[position + index] = ins;
        }
    }

    fn change_operands(&mut self, position: usize, operand: u32) {
        let op = OpCode::from_byte(self.instructions[position]);
        let instruction = make(op, vec![operand]);

        self.replace_instruction(position, instruction);
    }

    fn set_last_instruction(&mut self, op: OpCode, position: usize) {
        self.previous_instruction = self.last_instruction.clone();
        self.last_instruction = EmittedInstruction { op, position };
    }

    fn last_instruction_is_pop(&self) -> bool {
        self.last_instruction.op == OpCode::Pop
    }

    fn remove_last_pop(&mut self) {
        self.instructions =
            Instructions::new(self.instructions[..self.last_instruction.position].into());
    }

    fn emit(&mut self, op: OpCode, operands: Vec<u32>) -> usize {
        let instruction = make(op, operands);
        let position = self.add_instruction(instruction);

        self.set_last_instruction(op, position);

        position
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let position = self.instructions.len();
        self.instructions.push(&instruction);
        position
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.instructions.clone(),
            constants: self.constants.clone(),
        }
    }
}
