use std::fmt::{self, Display};

use super::symbol_table::{Scope as SymbolScope, SymbolTable};
use super::{make, Instructions, OpCode};
use crate::object::builtin::builtin_fns;
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

impl Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            r"
--------------Instructions-----------
{}
--------------Constants-----------
{}",
            self.instructions,
            self.constants
                .iter()
                .map(|constant| format!("\n{}\n", constant))
                .collect::<Vec<std::string::String>>()
                .join(""),
        )
    }
}
#[derive(Clone, Debug)]
struct EmittedInstruction {
    op: OpCode,
    position: usize,
}

struct Scope {
    instructions: Instructions,
    last_instruction: EmittedInstruction,
    previous_instruction: EmittedInstruction,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            instructions: Instructions::new(Vec::new()),
            last_instruction: EmittedInstruction {
                op: OpCode::Invalid,
                position: 0,
            },
            previous_instruction: EmittedInstruction {
                op: OpCode::Invalid,
                position: 0,
            },
        }
    }
}

/// # Compiler
/// Handle the compilation process into bytecode
pub struct Compiler {
    pub constants: Vec<Object>,

    pub symbol_table: SymbolTable,

    scopes: Vec<Scope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Compiler {
        let main_scope = Scope::new();

        let mut symbol_table = SymbolTable::new();

        for (index, &name) in builtin_fns().iter().enumerate() {
            symbol_table.define_built_in(name, index as u32);
        }

        Compiler {
            constants: Vec::new(),

            symbol_table,

            scopes: vec![main_scope],
            scope_index: 0,
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
            Statement::If(stmt) => self.compile_if_statement(stmt)?,
            Statement::Block(block) => self.compile_block_statement(block)?,
            Statement::Let(stmt) => self.compile_let_statement(stmt)?,
            Statement::Fn(function) => self.compile_function(function)?,
            Statement::Return(expr) => self.compile_return_statement(expr)?,
            Statement::Assignment(stmt) => self.compile_assignment(stmt)?,
            Statement::Call(call) => {
                self.compile_call(call)?;
                self.emit(OpCode::Pop, vec![]);
            }
            stmt => return compilation_error(format!("Expected statement, got: {}", stmt)),
        }

        Ok(())
    }

    fn compile_return_statement(&mut self, expr: Option<Expression>) -> CompilerResult {
        if let Some(expr) = expr {
            self.compile_expression(expr)?;
            self.emit(OpCode::ReturnValue, vec![]);
        } else {
            self.emit(OpCode::Return, vec![]);
        }

        Ok(())
    }

    fn compile_let_statement(&mut self, stmt: Let) -> CompilerResult {
        match stmt.value {
            Some(expression) => self.compile_expression(expression)?,
            None => {
                self.emit(OpCode::Void, vec![]);
            }
        }

        let symbol = self.symbol_table.define(&stmt.identifier.name);
        self.emit(
            match symbol.scope {
                SymbolScope::Local => OpCode::SetLocal,
                SymbolScope::Global => OpCode::SetGlobal,
                _ => unreachable!(),
            },
            vec![symbol.index],
        );

        Ok(())
    }

    fn compile_assignment(&mut self, stmt: Assignment) -> CompilerResult {
        let symbol = match self.symbol_table.resolve(&stmt.identifier.name) {
            Some(symbol) => symbol.clone(),
            None => return compilation_error(format!("Undefined {}", stmt.identifier.name)),
        };

        match stmt.operator {
            AssignOperator::PlusAssign => {
                self.emit(OpCode::GetGlobal, vec![symbol.index]);
                self.compile_expression(stmt.value)?;
                self.emit(OpCode::Add, vec![]);
            }
            AssignOperator::MinusAssign => {
                self.emit(OpCode::GetGlobal, vec![symbol.index]);
                self.compile_expression(stmt.value)?;
                self.emit(OpCode::Sub, vec![]);
            }
            AssignOperator::MultiplyAssign => {
                self.emit(OpCode::GetGlobal, vec![symbol.index]);
                self.compile_expression(stmt.value)?;
                self.emit(OpCode::Mul, vec![]);
            }
            AssignOperator::DivideAssign => {
                self.emit(OpCode::GetGlobal, vec![symbol.index]);
                self.compile_expression(stmt.value)?;
                self.emit(OpCode::Div, vec![]);
            }
            _ => {
                self.compile_expression(stmt.value)?;
            }
        }

        self.emit(OpCode::UpdateGlobal, vec![symbol.index]);

        Ok(())
    }

    fn compile_function(&mut self, function: Fn) -> CompilerResult {
        let symbol = self.symbol_table.define(&function.identifier.name);

        self.enter_scope();

        for param in &function.params {
            self.symbol_table.define(&param.name);
        }

        self.compile_block_statement(function.body)?;

        if !self.last_instruction_is(OpCode::ReturnValue)
            && !self.last_instruction_is(OpCode::Return)
        {
            self.emit(OpCode::Return, vec![]);
        }

        let instructions = self.leave_scope();

        let index = self.add_constant(Object::Function(Function {
            instructions,
            locals: self.symbol_table.length(),
        }));

        self.emit(OpCode::Constant, vec![index as u32]);

        self.emit(OpCode::SetGlobal, vec![symbol.index]);
        Ok(())
    }

    fn compile_closure(&mut self, closure: Closure) -> CompilerResult {
        self.enter_scope();

        self.compile_block_statement(closure.body)?;

        if !self.last_instruction_is(OpCode::ReturnValue)
            && !self.last_instruction_is(OpCode::Return)
        {
            self.emit(OpCode::Return, vec![]);
        }

        let instructions = self.leave_scope();

        let index = self.add_constant(Object::Function(Function {
            instructions,
            locals: self.symbol_table.length(),
        }));

        self.emit(OpCode::Constant, vec![index as u32]);

        Ok(())
    }

    fn compile_call(&mut self, call: Call) -> CompilerResult {
        self.compile_expression(*call.function)?;

        for arg in &call.arguments {
            self.compile_expression(arg.clone())?;
        }

        self.emit(OpCode::Call, vec![call.arguments.len() as u32]);

        Ok(())
    }

    fn compile_block_statement(&mut self, block: Block) -> CompilerResult {
        for stmt in block.statements.iter() {
            self.compile_statement(stmt.clone())?;
        }

        Ok(())
    }

    fn compile_if_statement(&mut self, statement: If) -> CompilerResult {
        let mut jumps = vec![];

        for stmt in statement.conditions {
            self.compile_expression(stmt.condition)?;
            let jump_not_true_position = self.emit(OpCode::JumpNotTruthy, vec![0]);
            self.compile_block_statement(stmt.consequence)?;

            if self.last_instruction_is_pop() {
                self.remove_last_pop();
            }

            jumps.push(self.emit(OpCode::Jump, vec![0]));
            self.change_operands(
                jump_not_true_position,
                self.current_instructions().len() as u32,
            );
        }

        match statement.alternative {
            Some(Else { consequence, .. }) => {
                self.compile_block_statement(consequence)?;

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }
            }
            None => {
                self.emit(OpCode::Void, vec![]);
            }
        }

        for jump in jumps {
            self.change_operands(jump, self.current_instructions().len() as u32);
        }

        self.emit(OpCode::Pop, vec![]);

        Ok(())
    }

    fn compile_expression(&mut self, expression: Expression) -> CompilerResult {
        match expression {
            Expression::Infix(infix) => self.compile_infix(infix)?,
            Expression::Prefix(prefix) => self.compile_prefix(prefix)?,
            Expression::Literal(literal) => self.compile_literal(literal)?,
            Expression::Identifer(identifier) => self.compile_identifier(identifier)?,
            Expression::Array(array) => self.compile_array(array)?,
            Expression::Index(index) => self.compile_index(index)?,
            Expression::Closure(closure) => self.compile_closure(closure)?,
            Expression::Call(call) => self.compile_call(call)?,
            _ => return compilation_error(format!("Invalid expression {}", expression)),
        }

        Ok(())
    }

    fn compile_array(&mut self, array: Array) -> CompilerResult {
        for expr in array.expressions.iter() {
            self.compile_expression(expr.clone())?;
        }

        self.emit(OpCode::Array, vec![array.expressions.len() as u32]);

        Ok(())
    }

    fn compile_identifier(&mut self, identifier: Identifer) -> CompilerResult {
        let symbol = match self.symbol_table.resolve(&identifier.name) {
            Some(symbol) => symbol.clone(),
            None => return compilation_error(format!("Undefined {}", identifier.name)),
        };

        self.emit(
            match symbol.scope {
                SymbolScope::Local => OpCode::GetLocal,
                SymbolScope::Global => OpCode::GetGlobal,
                SymbolScope::BuiltIn => OpCode::GetBuiltIn,
            },
            vec![symbol.index],
        );

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

    fn compile_index(&mut self, index: Index) -> CompilerResult {
        self.compile_expression(*index.left)?;

        self.compile_expression(*index.index)?;

        self.emit(OpCode::Index, vec![]);

        Ok(())
    }

    fn add_constant(&mut self, object: Object) -> usize {
        let position = self.constants.len();
        self.constants.push(object);
        position
    }

    fn replace_instruction(&mut self, position: usize, instruction: Instructions) {
        for (index, &ins) in instruction.iter().enumerate() {
            self.scopes[self.scope_index].instructions[position + index] = ins;
        }
    }

    fn change_operands(&mut self, position: usize, operand: u32) {
        let scope = self.scope();

        let op = OpCode::from_byte(scope.instructions[position]);
        let instruction = make(op, vec![operand]);

        self.replace_instruction(position, instruction);
    }

    fn current_instructions(&self) -> &Instructions {
        &self.scopes[self.scope_index].instructions
    }

    fn set_last_instruction(&mut self, op: OpCode, position: usize) {
        self.scopes[self.scope_index].previous_instruction =
            self.scopes[self.scope_index].last_instruction.clone();

        self.scopes[self.scope_index].last_instruction = EmittedInstruction { op, position };
    }

    fn last_instruction_is_pop(&self) -> bool {
        if self.current_instructions().len() == 0 {
            return false;
        }

        self.scopes[self.scope_index].last_instruction.op == OpCode::Pop
    }

    fn last_instruction_is(&self, op: OpCode) -> bool {
        if self.current_instructions().len() == 0 {
            return false;
        }

        self.scopes[self.scope_index].last_instruction.op == op
    }

    fn remove_last_pop(&mut self) {
        self.scopes[self.scope_index].instructions = Instructions::new(
            self.scopes[self.scope_index].instructions
                [..self.scopes[self.scope_index].last_instruction.position]
                .into(),
        );
    }

    fn enter_scope(&mut self) {
        let scope = Scope::new();

        self.scopes.push(scope);
        self.scope_index += 1;

        self.symbol_table = SymbolTable::new_enclosed(self.symbol_table.clone());
    }

    fn leave_scope(&mut self) -> Instructions {
        let instructions = self.current_instructions().clone();

        self.scopes.pop();
        self.scope_index -= 1;

        self.symbol_table = *self.symbol_table.outer.clone().unwrap();

        instructions
    }

    fn emit(&mut self, op: OpCode, operands: Vec<u32>) -> usize {
        let instruction = make(op, operands);
        let position = self.add_instruction(instruction);

        self.set_last_instruction(op, position);

        position
    }

    fn add_instruction(&mut self, instruction: Instructions) -> usize {
        let position = self.current_instructions().len();

        self.scopes[self.scope_index]
            .instructions
            .push(&instruction);

        position
    }

    fn scope(&self) -> &Scope {
        &self.scopes[self.scope_index]
    }

    pub fn bytecode(&self) -> Bytecode {
        Bytecode {
            instructions: self.current_instructions().clone(),
            constants: self.constants.clone(),
        }
    }
}
