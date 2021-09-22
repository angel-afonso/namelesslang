use crate::object::Object;
use crate::parser::parse;
use crate::Compiler;

use super::VM;
use crate::types::Integer;
use std::fmt::Display;

struct VMTestCase<T: Display> {
    pub input: &'static str,
    pub expected: T,
}

#[test]
fn test_string_expression() {
    let tests = vec![
        VMTestCase {
            input: r#"
					"nameless"
					"#,
            expected: "nameless".to_string(),
        },
        VMTestCase {
            input: r#""nameless" + "lang""#,
            expected: "namelesslang".to_string(),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_global_let_statements() {
    let tests = vec![
        VMTestCase {
            input: r#"
					let one = 1; 
					one;"#,
            expected: 1,
        },
        VMTestCase {
            input: "let one = 1; let two = 2; one + two;",
            expected: 3,
        },
        VMTestCase {
            input: "let one = 1; let two = one + one; one + two;",
            expected: 3,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_conditionals() {
    let tests = vec![
        VMTestCase {
            input: r#"
					let a = 0;
					if true {
						a = 10;
					}
					a
					"#,
            expected: Object::Integer(Integer(10)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if true {
						a = 10;
					} else {
						a = 20;
					}
					a;
					"#,
            expected: Object::Integer(Integer(10)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if false {
						a = 10;
					} else {
						a = 20;
					}
					a;
					"#,
            expected: Object::Integer(Integer(20)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if 1 < 2 {
						a = 10;
					}
					a;
			"#,
            expected: Object::Integer(Integer(10)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if 1 > 2 {
						a = 10;
					} else {
						a = 20;
					}
					a;
					"#,
            expected: Object::Integer(Integer(20)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if 1 > 2 {
						a = 10;
					} else if 1 < 2 {
						a = 20;
					}
					a;
					"#,
            expected: Object::Integer(Integer(20)),
        },
        VMTestCase {
            input: r#"
					let a = 0;
					if false {
						a =	10;
					}
					a;
					"#,
            expected: Object::Integer(Integer(0)),
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_boolean_arithmetic() {
    let tests = vec![
        VMTestCase {
            input: "true",
            expected: true,
        },
        VMTestCase {
            input: "!false",
            expected: true,
        },
        VMTestCase {
            input: "false",
            expected: false,
        },
        VMTestCase {
            input: "1 < 2;",
            expected: true,
        },
        VMTestCase {
            input: "1 > 2;",
            expected: false,
        },
        VMTestCase {
            input: "1 == 2;",
            expected: false,
        },
        VMTestCase {
            input: "1 != 2;",
            expected: true,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_integer_arithmetic() {
    let test = vec![
        VMTestCase {
            input: "1",
            expected: 1,
        },
        VMTestCase {
            input: "2",
            expected: 2,
        },
        VMTestCase {
            input: "1 + 2;",
            expected: 3,
        },
        VMTestCase {
            input: "1 - 1;",
            expected: 0,
        },
        VMTestCase {
            input: "1 * 2;",
            expected: 2,
        },
        VMTestCase {
            input: "2 / 2;",
            expected: 1,
        },
        VMTestCase {
            input: "(1 + 2) * 4 ",
            expected: 12,
        },
    ];

    run_vm_tests(test);
}

#[test]
fn test_assignment() {
    let tests = vec![
        VMTestCase {
            input: r"
			let a = 10;
			a += 1;
			",
            expected: 11,
        },
        VMTestCase {
            input: r"
			let a = 10;
			a -= 1;
			",
            expected: 9,
        },
        VMTestCase {
            input: r"
			let a = 10;
			a *= 1;
			",
            expected: 10,
        },
        VMTestCase {
            input: r"
			let a = 10;
			a /= 1;
			",
            expected: 10,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_index_expression() {
    let tests = vec![VMTestCase {
        input: "[1, 2, 3][1]",
        expected: 2,
    }];

    run_vm_tests(tests);
}

#[test]
fn test_funcion_call_with_args() {
    let tests = vec![
        VMTestCase {
            input: r"
			fn plusTen(x) {return x + 10;}
			plusTen(5);
        ",
            expected: 15,
        },
        VMTestCase {
            input: r"
				fn plusTen(x, y) {return x + y + 10;}
				plusTen(1, 2);
					",
            expected: 13,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_funcion_call_no_args() {
    let tests = vec![VMTestCase {
        input: r"
				fn fivePlusTen() {return 5 + 10;}
				fivePlusTen();
				",
        expected: 15,
    }];

    run_vm_tests(tests);
}

#[test]
fn test_function_binding() {
    let tests = vec![
        VMTestCase {
            input: r"
        		fn one() {let x = 1;return x;}
                one();
        		",
            expected: 1,
        },
        VMTestCase {
            input: r"
                let a = 10;
				fn plusOne() {
                    let x = 1;
                    return x + a;
                }
                plusOne();
				",
            expected: 11,
        },
    ];

    run_vm_tests(tests);
}

#[test]
fn test_builtin() {
    let tests = vec![VMTestCase {
        input: "println(\"hello world\")",
        expected: Object::Void,
    }];

    run_vm_tests(tests);
}

#[test]
fn test_recursion() {
    let tests = vec![VMTestCase {
        input: r"
            fn factorial(x) {
                if x == 1 {
                    return 1;
                }

                return x * factorial(x - 1);
            }

            factorial(5);
			",
        expected: 120,
    }];

    run_vm_tests(tests);
}

fn run_vm_tests<T: Display>(tests: Vec<VMTestCase<T>>) {
    for test in tests.iter() {
        let program = parse(&test.input, crate::parser::parser::Mode::REPL).unwrap();

        let mut compiler = Compiler::new();
        compiler.compile(program).unwrap();

        let mut vm = VM::new(compiler.bytecode());

        vm.run().unwrap();

        assert_eq!(
            format!("{}", test.expected),
            format!("{}", vm.last_popped())
        )
    }
}
