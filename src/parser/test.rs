use super::super::lexer::Lexer;
use super::{ast::*, Parser};

#[test]
fn test_let_statement() {
    struct TestLet<'a> {
        pub input: &'a str,
        pub expected_value: &'a str,
    }

    let tests: Vec<TestLet> = vec![
        TestLet {
            input: "let x = 5;",
            expected_value: "x",
        },
        TestLet {
            input: "let y = 10;",
            expected_value: "y",
        },
        TestLet {
            input: "let foobar = y;",
            expected_value: "foobar",
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(&mut Lexer::new(test.input));

        let program = parser.parse_program();

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Let(ident, _)) => {
                assert_eq!(ident.0, test.expected_value);
            }
            _ => panic!("Not a Statement::Let"),
        };
    }
}
