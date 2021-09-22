use super::{make, Instructions, OpCode};
use crate::object::types::{self, Function, Integer};
use crate::parser::parse;
use crate::Compiler;
use crate::Object;

struct CompilerTestCase<T> {
    input: &'static str,
    expected_constants: Vec<T>,
    expected_instruction: Vec<Instructions>,
}

#[test]
fn test_string_expression() {
    let tests = vec![
        CompilerTestCase {
            input: "\"nameless\"",
            expected_constants: vec!["nameless".to_string()],
            expected_instruction: vec![make(OpCode::Constant, vec![0]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "\"nameless\" + \"lang\"",
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
				"#,
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
                make(OpCode::JumpNotTruthy, vec![10]),
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
            ],
        },
        CompilerTestCase {
            input: r#"
					if true {
						let a = 10;
					} else  {
						let a = 20;
					} 
					"#
            .into(),
            expected_constants: vec![10, 20],
            expected_instruction: vec![
                make(OpCode::True, vec![]),
                make(OpCode::JumpNotTruthy, vec![13]),
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Jump, vec![19]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![1]),
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
                make(OpCode::SetGlobal, vec![1]),
                make(OpCode::Jump, vec![32]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::SetGlobal, vec![2]),
            ],
        },
    ];

    run_compiler_tests(tests)
}

#[test]
fn test_integer_arithmetic() {
    let tests = vec![
        CompilerTestCase {
            input: "1 + 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Add, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 - 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Sub, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 * 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Mul, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 / 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Div, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 > 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::GreaterThan, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 < 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::LowerThan, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 == 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Equal, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1 != 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::NotEqual, vec![]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "1; 2;",
            expected_constants: vec![1, 2],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Pop, vec![]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: "true",
            expected_constants: vec![],
            expected_instruction: vec![make(OpCode::True, vec![]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "false",
            expected_constants: vec![],
            expected_instruction: vec![make(OpCode::False, vec![]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "!false",
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

#[test]
fn test_array_literal() {
    let tests = vec![
        CompilerTestCase {
            input: "[]",
            expected_constants: vec![],
            expected_instruction: vec![make(OpCode::Array, vec![0]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "[1,2,3]",
            expected_constants: vec![1, 2, 3],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::Array, vec![3]),
                make(OpCode::Pop, vec![]),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_index_expressions() {
    let tests = vec![CompilerTestCase {
        input: "[1, 2, 3][0]",
        expected_constants: vec![1, 2, 3, 0],
        expected_instruction: vec![
            make(OpCode::Constant, vec![0]),
            make(OpCode::Constant, vec![1]),
            make(OpCode::Constant, vec![2]),
            make(OpCode::Array, vec![3]),
            make(OpCode::Constant, vec![3]),
            make(OpCode::Index, vec![]),
            make(OpCode::Pop, vec![]),
        ],
    }];

    run_compiler_tests(tests);
}

#[test]
fn test_functions_and_closure() {
    let tests = vec![
        CompilerTestCase {
            input: "fn test(){return 5 + 10;}",
            expected_constants: vec![
                Object::Integer(Integer(5)),
                Object::Integer(Integer(10)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::Constant, vec![1]),
                        make(OpCode::Add, vec![]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![2]),
                make(OpCode::SetGlobal, vec![0]),
            ],
        },
        CompilerTestCase {
            input: "(){return 5 + 10;}",
            expected_constants: vec![
                Object::Integer(Integer(5)),
                Object::Integer(Integer(10)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::Constant, vec![1]),
                        make(OpCode::Add, vec![]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![make(OpCode::Constant, vec![2]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "fn test(){ let a = 10; return 5 + 10;}",
            expected_constants: vec![
                Object::Integer(Integer(10)),
                Object::Integer(Integer(5)),
                Object::Integer(Integer(10)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::SetLocal, vec![0]),
                        make(OpCode::Constant, vec![1]),
                        make(OpCode::Constant, vec![2]),
                        make(OpCode::Add, vec![]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![3]),
                make(OpCode::SetGlobal, vec![0]),
            ],
        },
        CompilerTestCase {
            input: "(){ let a = 10; return 5 + 10;}",
            expected_constants: vec![
                Object::Integer(Integer(10)),
                Object::Integer(Integer(5)),
                Object::Integer(Integer(10)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::SetLocal, vec![0]),
                        make(OpCode::Constant, vec![1]),
                        make(OpCode::Constant, vec![2]),
                        make(OpCode::Add, vec![]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![make(OpCode::Constant, vec![3]), make(OpCode::Pop, vec![])],
        },
        CompilerTestCase {
            input: "(){ }",
            expected_constants: vec![Object::Function(Function {
                instructions: concat_instructions(vec![make(OpCode::Return, vec![])]),
                locals: 0,
            })],
            expected_instruction: vec![make(OpCode::Constant, vec![0]), make(OpCode::Pop, vec![])],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_function_call() {
    let tests = vec![
        CompilerTestCase {
            input: "(){return 24;}();",
            expected_constants: vec![
                Object::Integer(Integer(24)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![1]),
                make(OpCode::Call, vec![0]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r"
				fn test() {return 24;}
				test();
			",
            expected_constants: vec![
                Object::Integer(Integer(24)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::GetGlobal, vec![0]),
                make(OpCode::Call, vec![0]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r"
				fn test(x) {return 24;}
				test(1);
			",
            expected_constants: vec![
                Object::Integer(Integer(24)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
                Object::Integer(Integer(1)),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::GetGlobal, vec![0]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::Call, vec![1]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r"
				fn test(x, y) {return x + y;}
				test(1, 2);
			",
            expected_constants: vec![
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
                Object::Integer(Integer(1)),
                Object::Integer(Integer(2)),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::GetGlobal, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Constant, vec![2]),
                make(OpCode::Call, vec![2]),
                make(OpCode::Pop, vec![]),
            ],
        },
        CompilerTestCase {
            input: r"
				fn test(x) {return x;}
				test(1);
			",
            expected_constants: vec![
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::GetLocal, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
                Object::Integer(Integer(1)),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::GetGlobal, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::Call, vec![1]),
                make(OpCode::Pop, vec![]),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_let_statement_scopes() {
    let tests = vec![
        CompilerTestCase {
            input: r"let num = 55;
		 fn test() { return num; }	
	   ",
            expected_constants: vec![
                Object::Integer(Integer(55)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::GetGlobal, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 0,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![0]),
                make(OpCode::SetGlobal, vec![0]),
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![1]),
            ],
        },
        CompilerTestCase {
            input: r"
		 fn test() { 
			let num = 55;
			return num;
		 }	
	   ",
            expected_constants: vec![
                Object::Integer(Integer(55)),
                Object::Function(Function {
                    instructions: concat_instructions(vec![
                        make(OpCode::Constant, vec![0]),
                        make(OpCode::SetLocal, vec![0]),
                        make(OpCode::GetLocal, vec![0]),
                        make(OpCode::ReturnValue, vec![]),
                    ]),
                    locals: 1,
                }),
            ],
            expected_instruction: vec![
                make(OpCode::Constant, vec![1]),
                make(OpCode::SetGlobal, vec![0]),
            ],
        },
    ];

    run_compiler_tests(tests);
}

#[test]
fn test_builtins() {
    let tests = vec![CompilerTestCase {
        input: "println(\"hello world\")",
        expected_constants: vec![Object::String(types::String("hello world".into()))],
        expected_instruction: vec![
            make(OpCode::GetBuiltIn, vec![0]),
            make(OpCode::Constant, vec![0]),
            make(OpCode::Call, vec![1]),
            make(OpCode::Pop, vec![]),
        ],
    }];

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
    assert_eq!(expected.len(), actual.len(), "{:?}", actual);

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
