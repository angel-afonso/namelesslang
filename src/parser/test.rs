use super::super::lexer::Lexer;
use super::{ast::*, parser::ParseError, Parser};

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
        TestLet {
            input: "let foo = !true;",
            expected_ident: "foo",
            expected_value: Expression::Prefix(
                PrefixOperator::Not,
                Box::new(Expression::Literal(Literal::Bool(true))),
            ),
        },
        TestLet {
            input: "let x = 1 + 2;",
            expected_ident: "x",
            expected_value: Expression::Infix(
                InfixOperator::Plus,
                Box::new(Expression::Literal(Literal::Int(1))),
                Box::new(Expression::Literal(Literal::Int(2))),
            ),
        },
        TestLet {
            input: "let x = 1 - 2;",
            expected_ident: "x",
            expected_value: Expression::Infix(
                InfixOperator::Minus,
                Box::new(Expression::Literal(Literal::Int(1))),
                Box::new(Expression::Literal(Literal::Int(2))),
            ),
        },
        TestLet {
            input: "let x = (1 + (3 * 2)) / 3;",
            expected_ident: "x",
            expected_value: Expression::Infix(
                InfixOperator::Divide,
                Box::new(Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Literal(Literal::Int(1))),
                    Box::new(Expression::Infix(
                        InfixOperator::Multiply,
                        Box::new(Expression::Literal(Literal::Int(3))),
                        Box::new(Expression::Literal(Literal::Int(2))),
                    )),
                )),
                Box::new(Expression::Literal(Literal::Int(3))),
            ),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));

        let (program, errors) = parser.parse_program();

        check_parser_errors(errors);

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Let(ident, expr)) => {
                assert_eq!(ident.0, test.expected_ident);
                assert_eq!(expr.clone(), test.expected_value);
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
        TestReturn {
            input: "return !true;",
            value: Expression::Prefix(
                PrefixOperator::Not,
                Box::new(Expression::Literal(Literal::Bool(true))),
            ),
        },
        TestReturn {
            input: "return (1 + 2) * 3;",
            value: Expression::Infix(
                InfixOperator::Multiply,
                Box::new(Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Literal(Literal::Int(1))),
                    Box::new(Expression::Literal(Literal::Int(2))),
                )),
                Box::new(Expression::Literal(Literal::Int(3))),
            ),
        },
        TestReturn {
            input: "return (1 * 2) / 3;",
            value: Expression::Infix(
                InfixOperator::Divide,
                Box::new(Expression::Infix(
                    InfixOperator::Multiply,
                    Box::new(Expression::Literal(Literal::Int(1))),
                    Box::new(Expression::Literal(Literal::Int(2))),
                )),
                Box::new(Expression::Literal(Literal::Int(3))),
            ),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));

        let (program, errors) = parser.parse_program();

        check_parser_errors(errors);

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Return(expr)) => {
                assert_eq!(expr.clone(), test.value);
            }
            _ => panic!("Not a Statement::Let"),
        };
    }
}

#[test]
fn test_parse_block_statements() {
    let input = r#"{ 
        let x = 5;
        let y = x;
    }"#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Block(Block(stmts))) => {
            assert_eq!(stmts.len(), 2);
        }
        _ => panic!("Not a Statement::Block"),
    }
}

#[test]
fn test_parse_if_expression() {
    let input = r#"
        if x > y {
            let a = 10;
        } else {
            let b = 10;
        }
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::If(If {
            condition,
            consequence,
            alternative,
        })) => {
            assert_eq!(
                condition,
                &Box::new(Expression::Infix(
                    InfixOperator::GreaterThan,
                    Box::new(Expression::Identifer(Identifer("x".into()))),
                    Box::new(Expression::Identifer(Identifer("y".into()))),
                ))
            );

            assert_eq!(
                consequence,
                &Block(vec![Statement::Let(
                    Identifer("a".into()),
                    Expression::Literal(Literal::Int(10))
                )])
            );

            assert_eq!(
                alternative,
                &Some(Box::new(Expression::Block(Block(vec![Statement::Let(
                    Identifer("b".into()),
                    Expression::Literal(Literal::Int(10))
                )]))))
            );
        }
        _ => panic!("Not a Statement::Block"),
    }
}

#[test]
fn test_parse_if_else_if_expression() {
    let input = r#"
        if x > y {
            let a = 10;
        } else if x < y{
            let b = 10;
        }
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::If(If {
            condition,
            consequence,
            alternative,
        })) => {
            assert_eq!(
                condition,
                &Box::new(Expression::Infix(
                    InfixOperator::GreaterThan,
                    Box::new(Expression::Identifer(Identifer("x".into()))),
                    Box::new(Expression::Identifer(Identifer("y".into()))),
                ))
            );

            assert_eq!(
                consequence,
                &Block(vec![Statement::Let(
                    Identifer("a".into()),
                    Expression::Literal(Literal::Int(10))
                )])
            );

            match alternative {
                Some(box_expr) => {
                    let expr = &**box_expr;

                    match expr {
                        Expression::If(If {
                            condition,
                            consequence,
                            alternative,
                        }) => {
                            assert_eq!(
                                condition,
                                &Box::new(Expression::Infix(
                                    InfixOperator::LowerThan,
                                    Box::new(Expression::Identifer(Identifer("x".into()))),
                                    Box::new(Expression::Identifer(Identifer("y".into()))),
                                ))
                            );

                            assert_eq!(
                                consequence,
                                &Block(vec![Statement::Let(
                                    Identifer("b".into()),
                                    Expression::Literal(Literal::Int(10))
                                )])
                            );

                            assert_eq!(alternative, &None);
                        }
                        _ => panic!("Not a else if"),
                    }
                }
                _ => panic!("Not a else if"),
            }
        }
        _ => panic!("Not a Statement"),
    }
}

#[test]
fn test_parse_function_literal() {
    let input = r#"
    fn plusTwo(x) {
        return x + 2;
    }
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Fn(Fn {
            identifier,
            params,
            body,
        })) => {
            assert_eq!(identifier, &Identifer("plusTwo".into()));

            assert_eq!(params.len(), 1);

            assert_eq!(params.first().unwrap(), &Identifer("x".into()));

            assert_eq!(body.0.len(), 1);

            assert_eq!(
                body,
                &Block(vec![Statement::Return(Expression::Infix(
                    InfixOperator::Plus,
                    Box::new(Expression::Identifer(Identifer("x".into()))),
                    Box::new(Expression::Literal(Literal::Int(2))),
                ))])
            );
        }
        _ => panic!("Not a Statement"),
    }
}

#[test]
fn test_parse_function_call() {
    let input = r#"
        plusTwo(2);
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Call(Call {
            function,
            arguments,
        })) => {
            match &**function {
                Expression::Identifer(ident) => assert_eq!(ident, &Identifer("plusTwo".into())),
                expr => panic!("Not a identifier, {:?}", expr),
            }

            assert_eq!(arguments.len(), 1);

            match arguments.first().unwrap() {
                Expression::Literal(literal) => assert_eq!(literal, &Literal::Int(2)),
                _ => panic!("Not a literal"),
            }
        }
        _ => panic!("Not a Statement"),
    }
}

fn check_parser_errors(errors: Vec<ParseError>) {
    if errors.len() == 0 {
        return;
    }

    println!("Parser has {} errors", errors.len());

    for error in errors.iter() {
        println!("{}", error);
    }

    panic!();
}
