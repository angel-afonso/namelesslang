use super::super::lexer::Lexer;
use super::super::parser::Parser;
use super::eval;
use super::Environment;
use super::Object;

fn test_eval(input: &str) -> Object {
    let (program, _) = Parser::new(Lexer::new(&input)).parse_program();
    let env = Environment::new();

    eval(program, &env).unwrap()
}

#[test]
fn test_eval_let() {
    struct TestCase<'a> {
        pub input: &'a str,
        pub expected: Object,
    }

    let test_cases = vec![
        TestCase {
            input: r#"let a = 10;"#,
            expected: Object::Integer(10),
        },
        TestCase {
            input: r#"let hello = "hello world";"#,
            expected: Object::String("hello world".into()),
        },
    ];

    for tt in test_cases.iter() {
        assert_eq!(test_eval(tt.input), tt.expected)
    }
}
