use super::super::{parser::ast::*, Object};
use super::symbol_table::SymbolTable;
use super::{make, Instructions, OpCode};

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

    pub fn compile(&mut self, program: Program) {
        for stmt in program {
            self.compile_statement(stmt);
        }
    }

    fn compile_statement(&mut self, statement: Statement) {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression);
                self.emit(OpCode::Pop, vec![]);
            }
            Statement::If(stmt) => self.compile_if_statement(stmt),
            Statement::Block(block) => self.compile_block_statement(block),
            Statement::Let(stmt) => self.compile_let_statement(stmt),
            _ => todo!(),
        }
    }

    pub fn compile_let_statement(&mut self, stmt: Let) {
        match stmt.value {
            Some(expression) => self.compile_expression(expression),
            None => {
                self.emit(OpCode::Void, vec![]);
            }
        }

        let symbol = self.symbol_table.define(&stmt.identifier.name);
        self.emit(OpCode::SetGlobal, vec![symbol.index]);
    }

    pub fn compile_block_statement(&mut self, block: Block) {
        for stmt in block.statements.iter() {
            self.compile_statement(stmt.clone());
        }
    }

    fn compile_if_statement(&mut self, statement: If) {
        self.compile_expression(*statement.condition);
        let jump_not_true_position = self.emit(OpCode::JumpNotTruthy, vec![0]);
        self.compile_block_statement(statement.consequence);

        if self.last_instruction_is_pop() {
            self.remove_last_pop();
        }

        let jump_position = self.emit(OpCode::Jump, vec![0]);
        self.change_operands(jump_not_true_position, self.instructions.len() as u32);

        match statement.alternative {
            Some(Else::Block(_, block)) => {
                self.compile_block_statement(block);
                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }
            }
            Some(Else::If(_, stmt)) => {
                self.compile_if_statement(*stmt);

                if self.last_instruction_is_pop() {
                    self.remove_last_pop();
                }
            }
            None => {
                self.emit(OpCode::Void, vec![]);
            }
        }

        self.change_operands(jump_position, self.instructions.len() as u32);
        self.emit(OpCode::Pop, vec![]);
    }

    fn compile_expression(&mut self, expression: Expression) {
        match expression {
            Expression::Infix(infix) => self.compile_infix(infix),
            Expression::Prefix(prefix) => self.compile_prefix(prefix),
            Expression::Literal(literal) => self.compile_literal(literal),
            Expression::Identifer(identifier) => self.compile_identifier(identifier),
            _ => todo!(),
        }
    }

    fn compile_identifier(&mut self, identifier: Identifer) {
        let symbol = {
            match self.symbol_table.resolve(&identifier.name) {
                Some(symbol) => symbol.clone(),
                None => todo!(),
            }
        };

        self.emit(OpCode::GetGlobal, vec![symbol.index]);
    }

    fn compile_prefix(&mut self, prefix: Prefix) {
        self.compile_expression(*prefix.expression);

        match prefix.operator {
            PrefixOperator::Not => {
                self.emit(OpCode::Not, vec![]);
            }
            _ => todo!(),
        }
    }

    fn compile_infix(&mut self, infix: Infix) {
        self.compile_expression(*infix.left);
        self.compile_expression(*infix.right);

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
            InfixOperator::Equal => {
                self.emit(OpCode::Equal, vec![]);
            }
            InfixOperator::NotEqual => {
                self.emit(OpCode::NotEqual, vec![]);
            }
            InfixOperator::GreaterThan => {
                self.emit(OpCode::GreaterThan, vec![]);
            }
            InfixOperator::LowerThan => {
                self.emit(OpCode::LowerThan, vec![]);
            }
            _ => {}
        }
    }

    fn compile_literal(&mut self, literal: Literal) {
        match literal {
            Literal::Int(_, int) => {
                let integer = Object::Integer(int);
                let position = self.add_constant(integer);
                self.emit(OpCode::Constant, vec![position as u32]);
            }
            Literal::Bool(_, boolean) => {
                if boolean {
                    self.emit(OpCode::True, vec![]);
                } else {
                    self.emit(OpCode::False, vec![]);
                }
            }
            _ => todo!(),
        }
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

#[cfg(test)]
mod test {
    use super::super::super::{parser::ast::Program, Lexer, Object, Parser};
    use super::super::{make, OpCode};
    use super::Compiler;
    use super::Instructions;

    struct CompilerTestCase<T> {
        input: String,
        expected_constants: Vec<T>,
        expected_instruction: Vec<Instructions>,
    }

    #[test]
    fn test_global_let_statement() {
        let tests = vec![
            CompilerTestCase {
                input: r#"
				let one = 1;
				let two = 2;
				"#
                .into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::SetGlobal, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::SetGlobal, vec![1]),
                ],
            },
            CompilerTestCase {
                input: r#"
				let one = 1;
				one;
				"#
                .into(),
                expected_constants: vec![1],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::SetGlobal, vec![0]),
                    make(OpCode::GetGlobal, vec![0]),
                    make(OpCode::Pop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    #[test]
    fn test_conditionals() {
        let tests = vec![
            CompilerTestCase {
                input: r#"
					if(true){
						10;
					} 
					3333"#
                    .into(),
                expected_constants: vec![10, 3333],
                expected_instruction: vec![
                    make(OpCode::True, vec![]),
                    make(OpCode::JumpNotTruthy, vec![10]),
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Jump, vec![11]),
                    make(OpCode::Void, vec![]),
                    make(OpCode::Pop, vec![]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: r#"
					if true {
						10;
					} else {
						20;
					} 
					3333"#
                    .into(),
                expected_constants: vec![10, 20, 3333],
                expected_instruction: vec![
                    make(OpCode::True, vec![]),
                    make(OpCode::JumpNotTruthy, vec![10]),
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Jump, vec![13]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Pop, vec![]),
                    make(OpCode::Constant, vec![2]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: r#"
					if true {
						10;
					} else if false {
						20;
					} else {
						30;
					}
					3333"#
                    .into(),
                expected_constants: vec![10, 20, 30, 3333],
                expected_instruction: vec![
                    make(OpCode::True, vec![]),
                    make(OpCode::JumpNotTruthy, vec![10]),
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Jump, vec![23]),
                    make(OpCode::False, vec![]),
                    make(OpCode::JumpNotTruthy, vec![20]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Jump, vec![23]),
                    make(OpCode::Constant, vec![2]),
                    make(OpCode::Pop, vec![]),
                    make(OpCode::Constant, vec![3]),
                    make(OpCode::Pop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests)
    }

    #[test]
    fn test_integer_arithmetic() {
        let tests = vec![
            CompilerTestCase {
                input: "1 + 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Add, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 - 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Sub, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 * 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Mul, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 / 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Div, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 > 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::GreaterThan, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 < 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::LowerThan, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 == 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Equal, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1 != 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::NotEqual, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "1; 2;".into(),
                expected_constants: vec![1, 2],
                expected_instruction: vec![
                    make(OpCode::Constant, vec![0]),
                    make(OpCode::Pop, vec![]),
                    make(OpCode::Constant, vec![1]),
                    make(OpCode::Pop, vec![]),
                ],
            },
            CompilerTestCase {
                input: "true;".into(),
                expected_constants: vec![],
                expected_instruction: vec![make(OpCode::True, vec![]), make(OpCode::Pop, vec![])],
            },
            CompilerTestCase {
                input: "false;".into(),
                expected_constants: vec![],
                expected_instruction: vec![make(OpCode::False, vec![]), make(OpCode::Pop, vec![])],
            },
            CompilerTestCase {
                input: "!false".into(),
                expected_constants: vec![],
                expected_instruction: vec![
                    make(OpCode::False, vec![]),
                    make(OpCode::Not, vec![]),
                    make(OpCode::Pop, vec![]),
                ],
            },
        ];

        run_compiler_tests(tests);
    }

    fn run_compiler_tests<T: Clone + std::fmt::Debug>(tests: Vec<CompilerTestCase<T>>) {
        for test in tests.iter() {
            let program = parse(&test.input);
            let mut compiler = Compiler::new();
            compiler.compile(program);

            let bytecode = compiler.bytecode();

            test_instructions(test.expected_instruction.clone(), bytecode.instructions);
            test_constants(test.expected_constants.clone(), bytecode.constants);
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let concatted = concat_instructions(expected);

        assert_eq!(
            concatted.len(),
            actual.len(),
            "\nExpected:\n{} Got:\n{}\n",
            concatted,
            actual
        );
        for (index, &instruction) in concatted.iter().enumerate() {
            assert_eq!(
                instruction, actual[index],
                "\nExpected:\n{} Got:\n{}\n",
                concatted, actual
            );
        }
    }

    fn test_constants<T: std::fmt::Debug>(expected: Vec<T>, actual: Vec<Object>) {
        assert_eq!(expected.len(), actual.len());

        for (index, instruction) in expected.iter().enumerate() {
            assert_eq!(format!("{:?}", instruction), format!("{}", actual[index]));
        }
    }

    fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
        let mut out = Instructions::new(Vec::new());

        for ins in instructions.iter() {
            out.push(ins);
        }

        return out;
    }

    fn parse(input: &str) -> Program {
        let (program, _) = Parser::new(Lexer::new(input)).parse_program();
        program
    }
}
