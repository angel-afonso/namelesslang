use super::super::lexer::Lexer;
use super::super::parser::Parser;
use super::Object;
use super::{Environment, Evaluator};

fn test_eval(input: &str) -> Object {
    let (program, err) = Parser::new(Lexer::new(&input)).parse_program();
    let env = Environment::new();

    let evaluator = Evaluator::new(|_| {}, || String::new());

    assert_eq!(err.len(), 0, "{:?}", err);

    evaluator.eval_repl(program, &env).unwrap()
}
struct TestCase<'a> {
    pub input: &'a str,
    pub expected: Object,
}

#[test]
fn test_eval_let() {
    let test_cases: Vec<TestCase> = vec![
        TestCase {
            input: r#"let a = 10;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let a = 10.42;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let a = 10 + 3;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let a = (10 + 11) * 2;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let hello = "hello world";"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let boolean = !false;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let boolean = !!false;"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let boolean = [1,2,3];"#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"let boolean = [1,2,3][1];"#,
            expected: Object::Void,
        },
    ];

    for tt in test_cases.iter() {
        assert_eq!(test_eval(tt.input), tt.expected)
    }
}

#[test]
fn test_eval_assignment() {
    let test_cases: Vec<TestCase> = vec![
        TestCase {
            input: r#"
                let a = 10;
                a = 20;
            "#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"
                let a = 10 + 3;
                a = 10;
            "#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"
                let a = (10 + 11) * 2;
                a = 2;
            "#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"
                let hello;
                hello = "hello world";
            "#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"
                let boolean;
                boolean = !false;
            "#,
            expected: Object::Void,
        },
        TestCase {
            input: r#"
                let boolean;
                boolean = !!false;
            "#,
            expected: Object::Void,
        },
    ];

    for tt in test_cases.iter() {
        assert_eq!(test_eval(tt.input), tt.expected)
    }
}

#[test]
fn test_eval_assign_operation() {
    let test_cases: Vec<TestCase> = vec![
        TestCase {
            input: r#"
				let a = 10;
				a += 20;
				return a;
			"#,
            expected: Object::ReturnValue(Box::new(Object::Integer(30))),
        },
        TestCase {
            input: r#"
				let a = 10;
				a -= 20;
				return a;
			"#,
            expected: Object::ReturnValue(Box::new(Object::Integer(-10))),
        },
        TestCase {
            input: r#"
				let a = 10;
				a *= 2;
				return a;
			"#,
            expected: Object::ReturnValue(Box::new(Object::Integer(20))),
        },
        TestCase {
            input: r#"
                let a = 20;
                a /= 2;
				return a;
            "#,
            expected: Object::ReturnValue(Box::new(Object::Integer(10))),
        },
    ];

    for tt in test_cases.iter() {
        assert_eq!(test_eval(tt.input), tt.expected)
    }
}

#[test]
fn test_eval_block_statement() {
    let input = r#"
        let a = 10;
        {
            let b = a;
        }
    "#;

    assert_eq!(test_eval(input), Object::Void);
}

#[test]
fn test_eval_function_call() {
    let input = r#"
        fn test() {
            let a = 10;
        }
        test();
    "#;

    test_eval(input);
}
#[test]
fn test_eval_function_with_params_call() {
    let input = r#"
        fn factorial(number) {    
            if number == 1 {      
                return 1;           
            }    
            
            return number * factorial(number - 1);    
        }       
        
        println(factorial(5));      
        

    "#;

    test_eval(input);
}

#[test]
fn test_eval_function_call_with_return() {
    let input = r#"
        fn test(b) {
           return b;
        }

        test(2);
    "#;

    assert_eq!(
        test_eval(input),
        Object::ReturnValue(Box::new(Object::Integer(2)))
    );
}
