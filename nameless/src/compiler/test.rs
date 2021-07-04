use super::{make, Instructions, OpCode};
use crate::parser::parse;
use crate::Compiler;
use crate::Object;

struct CompilerTestCase<T> {
    input: String,
    expected_constants: Vec<T>,
    expected_instruction: Vec<Instructions>,
}

#[test]
fn test_string_expression() {
    let tests = vec![
        CompilerTestCase {
            input: "\"nameless\"".into(),
            expected_constants: vec!["nameless".to_string()],
            expected_instruction: vec![make(OpCode::Constant, vec![0]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "\"nameless\" + \"lang\"".into(),
            expected_constants: vec!["nameless".to_string(), "lang".to_string()],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Add, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_let_statement() {
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
				"#
            .into(),
            expected_constants: vec![1],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
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
					if true {
						let a = 10;
					} 
					"#
            .into(),
            expected_constants: vec![10],
            expected_instruction: vec![
                make(OpCode::True, vec![]),
                make(OpCode::JumpNotTruthy, vec![13]),
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Jump, vec![14]),
                make(OpCode::Void, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r#"
		     if true {
				let a = 10;
			} else {
				let a = 20;
			}"#
            .into(),
            expected_constants: vec![10, 20],
            expected_instruction: vec![
                make(OpCode::True, vec![]),
                make(OpCode::JumpNotTruthy, vec![13]),
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Jump, vec![19]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r#"
				 if true {
					let a = 10;
				} else if false {
					let a = 20;
				} else {
					let a = 30;
				}"#
            .into(),
            expected_constants: vec![10, 20, 30],
            expected_instruction: vec![
                make(OpCode::True, vec![]),
                make(OpCode::JumpNotTruthy, vec![13]),
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Jump, vec![32]),
                make(OpCode::False, vec![]),
                make(OpCode::JumpNotTruthy, vec![26]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Jump, vec![32]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::SetGlobal, vec![0]),
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
            input: "true".into(),
            expected_constants: vec![],
            expected_instruction: vec![make(OpCode::True, vec![]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "false".into(),
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

fn run_compiler_tests<T: Clone + std::fmt::Display>(tests: Vec<CompilerTestCase<T>>) {
    for test in tests.iter() {
        let program = parse(&test.input, crate::parser::parser::Mode::REPL).unwrap();
        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();

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

fn test_constants<T: std::fmt::Display>(expected: Vec<T>, actual: Vec<Object>) {
    assert_eq!(expected.len(), actual.len());

    for (index, instruction) in expected.iter().enumerate() {
        assert_eq!(format!("{}", actual[index]), format!("{}", instruction));
    }
}

fn concat_instructions(instructions: Vec<Instructions>) -> Instructions {
    let mut out = Instructions::new(Vec::new());

    for ins in instructions.iter() {
        out.push(ins);
    }

    return out;
}
