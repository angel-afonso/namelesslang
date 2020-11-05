use super::super::lexer::Lexer;
use super::{ast::*, Parser};

#[test]
fn test_let_statement() {
    struct TestLet<'a> {
        pub input: &'a str,
        pub expected_ident: &'a str,
        pub expected_value: Expression,
    }

    let tests: Vec<TestLet> = vec![
        TestLet {
            input: "let x = 5;",
            expected_ident: "x",
            expected_value: Expression::Literal(Literal::Int(5)),
        },
        TestLet {
            input: "let y = 10;",
            expected_ident: "y",
            expected_value: Expression::Literal(Literal::Int(10)),
        },
        TestLet {
            input: "let foobar = y;",
            expected_ident: "foobar",
            expected_value: Expression::Identifer(Identifer("y".into())),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));

        let program = parser.parse_program();

        check_parser_errors(&parser);

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Let(ident, expr)) => {
                assert_eq!(ident.0, test.expected_ident);
                assert_eq!(expr.clone().unwrap(), test.expected_value);
            }
            _ => panic!("Not a Statement::Let"),
        };
    }
}

#[test]
fn test_return_statement() {
    struct TestReturn<'a> {
        pub input: &'a str,
        pub value: Expression,
    }

    let tests: Vec<TestReturn> = vec![
        TestReturn {
            input: "return 5;",
            value: Expression::Literal(Literal::Int(5)),
        },
        TestReturn {
            input: "return 10;",
            value: Expression::Literal(Literal::Int(10)),
        },
        TestReturn {
            input: "return true;",
            value: Expression::Literal(Literal::Bool(true)),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));
        check_parser_errors(&parser);

        let program = parser.parse_program();

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Return(expr)) => {
                assert_eq!(expr.clone().unwrap(), test.value);
            }
            _ => panic!("Not a Statement::Let"),
        };
    }
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();

    if errors.len() == 0 {
        return;
    }

    println!("Parser has {} errors", errors.len());

    for error in errors.iter() {
        println!("{}", error);
    }

    panic!();
}
