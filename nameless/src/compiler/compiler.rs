use super::super::{parser::ast::*, Object};
use super::{make, Instructions, OpCode};

/// # Bytecode
/// Holds the fully compiled program in bytecode format
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

/// # Compiler
/// Handle the compilation process into bytecode
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            instructions: Instructions::new(Vec::new()),
            constants: Vec::new(),
        }
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
            _ => todo!(),
        }
    }

    fn compile_expression(&mut self, expression: Expression) {
        match expression {
            Expression::Infix(infix) => self.compile_infix(infix),
            Expression::Prefix(prefix) => self.compile_prefix(prefix),
            Expression::Literal(literal) => self.compile_literal(literal),
            _ => todo!(),
        }
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

    fn emit(&mut self, op: OpCode, operands: Vec<u32>) -> usize {
        self.add_instruction(make(op, operands))
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
            assert_eq!(instruction, actual[index]);
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
