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
            expected_value: Expression::Literal(Literal::Int(Location { line: 1, column: 9 }, 5)),
        },
        TestLet {
            input: "let y = 10;",
            expected_ident: "y",
            expected_value: Expression::Literal(Literal::Int(Location { line: 1, column: 9 }, 10)),
        },
        TestLet {
            input: r#"let hello = "hello world";"#,
            expected_ident: "hello",
            expected_value: Expression::Literal(Literal::String(
                Location {
                    line: 1,
                    column: 13,
                },
                String::from("hello world"),
            )),
        },
        TestLet {
            input: "let foobar = y;",
            expected_ident: "foobar",
            expected_value: Expression::Identifer(Identifer {
                location: Location {
                    line: 1,
                    column: 14,
                },
                value: "y".into(),
            }),
        },
        TestLet {
            input: "let foo = !true;",
            expected_ident: "foo",
            expected_value: Expression::Prefix(Prefix {
                location: Location {
                    line: 1,
                    column: 11,
                },
                operator: PrefixOperator::Not,
                expression: Box::new(Expression::Literal(Literal::Bool(
                    Location {
                        line: 1,
                        column: 12,
                    },
                    true,
                ))),
            }),
        },
        TestLet {
            input: "let x = 1 + 2;",
            expected_ident: "x",
            expected_value: Expression::Infix(Infix {
                location: Location {
                    line: 1,
                    column: 11,
                },
                operator: InfixOperator::Plus,
                left: Box::new(Expression::Literal(Literal::Int(
                    Location { line: 1, column: 9 },
                    1,
                ))),
                right: Box::new(Expression::Literal(Literal::Int(
                    Location {
                        line: 1,
                        column: 13,
                    },
                    2,
                ))),
            }),
        },
        TestLet {
            input: "let x = 1 - 2;",
            expected_ident: "x",
            expected_value: Expression::Infix(Infix {
                location: Location {
                    line: 1,
                    column: 11,
                },
                operator: InfixOperator::Minus,
                left: Box::new(Expression::Literal(Literal::Int(
                    Location { line: 1, column: 9 },
                    1,
                ))),
                right: Box::new(Expression::Literal(Literal::Int(
                    Location {
                        line: 1,
                        column: 13,
                    },
                    2,
                ))),
            }),
        },
        TestLet {
            input: "let x = (1 + (3 * 2)) / 3;",
            expected_ident: "x",
            expected_value: Expression::Infix(Infix {
                location: Location {
                    line: 1,
                    column: 23,
                },
                operator: InfixOperator::Divide,
                left: Box::new(Expression::Infix(Infix {
                    location: Location {
                        line: 1,
                        column: 12,
                    },
                    operator: InfixOperator::Plus,
                    left: Box::new(Expression::Literal(Literal::Int(
                        Location {
                            line: 1,
                            column: 10,
                        },
                        1,
                    ))),
                    right: Box::new(Expression::Infix(Infix {
                        location: Location {
                            line: 1,
                            column: 17,
                        },
                        operator: InfixOperator::Multiply,
                        left: Box::new(Expression::Literal(Literal::Int(
                            Location {
                                line: 1,
                                column: 15,
                            },
                            3,
                        ))),
                        right: Box::new(Expression::Literal(Literal::Int(
                            Location {
                                line: 1,
                                column: 19,
                            },
                            2,
                        ))),
                    })),
                })),
                right: Box::new(Expression::Literal(Literal::Int(
                    Location {
                        line: 1,
                        column: 25,
                    },
                    3,
                ))),
            }),
        },
        TestLet {
            input: "let x = [1, 2, 3]",
            expected_ident: "x",
            expected_value: Expression::Array(Array {
                location: Location { line: 1, column: 9 },
                expressions: Box::new(vec![
                    Expression::Literal(Literal::Int(
                        Location {
                            line: 1,
                            column: 10,
                        },
                        1,
                    )),
                    Expression::Literal(Literal::Int(
                        Location {
                            line: 1,
                            column: 13,
                        },
                        2,
                    )),
                    Expression::Literal(Literal::Int(
                        Location {
                            line: 1,
                            column: 16,
                        },
                        3,
                    )),
                ]),
            }),
        },
        TestLet {
            input: "let x = [1, 2, 3][1]",
            expected_ident: "x",
            expected_value: Expression::Index(Index {
                location: Location {
                    line: 1,
                    column: 18,
                },
                left: Box::new(Expression::Array(Array {
                    location: Location { line: 1, column: 9 },
                    expressions: Box::new(vec![
                        Expression::Literal(Literal::Int(
                            Location {
                                line: 1,
                                column: 10,
                            },
                            1,
                        )),
                        Expression::Literal(Literal::Int(
                            Location {
                                line: 1,
                                column: 13,
                            },
                            2,
                        )),
                        Expression::Literal(Literal::Int(
                            Location {
                                line: 1,
                                column: 16,
                            },
                            3,
                        )),
                    ]),
                })),
                index: Box::new(Expression::Literal(Literal::Int(
                    Location {
                        line: 1,
                        column: 19,
                    },
                    1,
                ))),
            }),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));

        let (program, errors) = parser.parse_program();

        check_parser_errors(errors);

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Let(let_stmt)) => {
                assert_eq!(let_stmt.identifier.value, test.expected_ident);
                assert_eq!(let_stmt.value.clone().unwrap(), test.expected_value);
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
            value: Expression::Literal(Literal::Int(Location { line: 1, column: 8 }, 5)),
        },
        TestReturn {
            input: "return 10;",
            value: Expression::Literal(Literal::Int(Location { line: 1, column: 8 }, 10)),
        },
        TestReturn {
            input: "return true;",
            value: Expression::Literal(Literal::Bool(Location { line: 1, column: 8 }, true)),
        },
        TestReturn {
            input: "return !true;",
            value: Expression::Prefix(Prefix {
                location: Location { column: 8, line: 1 },
                operator: PrefixOperator::Not,
                expression: Box::new(Expression::Literal(Literal::Bool(
                    Location { line: 1, column: 9 },
                    true,
                ))),
            }),
        },
    ];

    for test in tests.iter() {
        let mut parser = Parser::new(Lexer::new(test.input));

        let (program, errors) = parser.parse_program();

        check_parser_errors(errors);

        assert_eq!(program.len(), 1);

        match program.first() {
            Some(Statement::Return(expr)) => {
                assert_eq!(expr.clone().unwrap(), test.value);
            }
            _ => panic!("Not a Statement::Return"),
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
        Some(Statement::Block(Block {
            location: Location { line: 1, column: 1 },
            statements: stmts,
        })) => {
            assert_eq!(stmts.len(), 2);
        }
        _ => panic!("Not a Statement::Block"),
    }
}

#[test]
fn test_parse_if() {
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
            location,
            condition,
            consequence,
            alternative,
        })) => {
            assert_eq!(location, &Location { column: 9, line: 2 });

            assert_eq!(
                condition,
                &Box::new(Expression::Infix(Infix {
                    location: Location {
                        line: 2,
                        column: 14,
                    },
                    operator: InfixOperator::GreaterThan,
                    left: Box::new(Expression::Identifer(Identifer {
                        location: Location {
                            line: 2,
                            column: 12
                        },
                        value: "x".into(),
                    })),
                    right: Box::new(Expression::Identifer(Identifer {
                        location: Location {
                            line: 2,
                            column: 16
                        },
                        value: "y".into(),
                    })),
                }))
            );

            assert_eq!(
                consequence,
                &Block {
                    location: Location {
                        line: 2,
                        column: 18
                    },
                    statements: vec![Statement::Let(Let {
                        location: Location { line: 4, column: 9 },
                        identifier: Identifer {
                            location: Location {
                                line: 3,
                                column: 17
                            },
                            value: "a".into()
                        },
                        value: Some(Expression::Literal(Literal::Int(
                            Location {
                                line: 3,
                                column: 21
                            },
                            10
                        )))
                    })]
                }
            );
            assert_eq!(
                alternative,
                &Some(Else::Block(
                    Location {
                        line: 4,
                        column: 11
                    },
                    Block {
                        location: Location {
                            line: 4,
                            column: 16
                        },
                        statements: vec![Statement::Let(Let {
                            location: Location { line: 6, column: 9 },
                            identifier: Identifer {
                                location: Location {
                                    line: 5,
                                    column: 17
                                },
                                value: "b".into()
                            },
                            value: Some(Expression::Literal(Literal::Int(
                                Location {
                                    line: 5,
                                    column: 21
                                },
                                10
                            )))
                        })]
                    }
                ))
            );
        }
        _ => panic!("Not a if statement"),
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

    match program.first().unwrap() {
        Statement::If(If {
            location,
            condition,
            consequence,
            alternative,
        }) => {
            assert_eq!(location, &Location { line: 2, column: 9 });
            assert_eq!(
                **condition,
                Expression::Infix(Infix {
                    location: Location {
                        line: 2,
                        column: 14
                    },
                    operator: InfixOperator::GreaterThan,
                    left: Box::new(Expression::Identifer(Identifer {
                        location: Location {
                            line: 2,
                            column: 12
                        },
                        value: "x".into()
                    })),
                    right: Box::new(Expression::Identifer(Identifer {
                        location: Location {
                            line: 2,
                            column: 16
                        },
                        value: "y".into(),
                    }))
                })
            );

            assert_eq!(
                *consequence,
                Block {
                    location: Location {
                        line: 2,
                        column: 18
                    },
                    statements: vec![Statement::Let(Let {
                        location: Location { line: 4, column: 9 },
                        identifier: Identifer {
                            location: Location {
                                line: 3,
                                column: 17
                            },
                            value: "a".into()
                        },
                        value: Some(Expression::Literal(Literal::Int(
                            Location {
                                line: 3,
                                column: 21
                            },
                            10
                        )))
                    })]
                }
            );

            assert_eq!(
                alternative,
                &Some(Else::If(
                    Location {
                        line: 4,
                        column: 11
                    },
                    Box::new(If {
                        location: Location {
                            line: 4,
                            column: 16,
                        },
                        condition: Box::new(Expression::Infix(Infix {
                            location: Location {
                                line: 4,
                                column: 21,
                            },
                            operator: InfixOperator::LowerThan,
                            left: Box::new(Expression::Identifer(Identifer {
                                location: Location {
                                    line: 4,
                                    column: 19,
                                },
                                value: "x".into(),
                            })),
                            right: Box::new(Expression::Identifer(Identifer {
                                location: Location {
                                    line: 4,
                                    column: 23,
                                },
                                value: "y".into(),
                            }))
                        })),
                        consequence: Block {
                            location: Location {
                                line: 4,
                                column: 24,
                            },
                            statements: vec![Statement::Let(Let {
                                location: Location { line: 6, column: 9 },
                                identifier: Identifer {
                                    location: Location {
                                        line: 5,
                                        column: 17
                                    },
                                    value: "b".into(),
                                },
                                value: Some(Expression::Literal(Literal::Int(
                                    Location {
                                        line: 5,
                                        column: 21
                                    },
                                    10
                                )))
                            })]
                        },
                        alternative: None
                    })
                ))
            );
        }
        _ => panic!("expected if"),
    }
}

#[test]
fn test_parse_function_literal() {
    let input = r#"
    fn plusTwo(x, y) {
        return x + y;
    }
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Fn(Fn {
            location,
            identifier,
            params,
            body,
        })) => {
            assert_eq!(location, &Location { line: 2, column: 5 });

            assert_eq!(
                identifier,
                &Identifer {
                    location: Location { line: 2, column: 5 },
                    value: "plusTwo".into(),
                }
            );

            assert_eq!(
                params,
                &vec![
                    Identifer {
                        location: Location {
                            line: 2,
                            column: 16,
                        },
                        value: "x".into(),
                    },
                    Identifer {
                        location: Location {
                            line: 2,
                            column: 19,
                        },
                        value: "y".into(),
                    },
                ]
            );

            assert_eq!(
                body,
                &Block {
                    location: Location {
                        line: 2,
                        column: 22,
                    },
                    statements: vec![Statement::Return(Some(Expression::Infix(Infix {
                        location: Location {
                            line: 3,
                            column: 18,
                        },
                        operator: InfixOperator::Plus,
                        left: Box::new(Expression::Identifer(Identifer {
                            location: Location {
                                line: 3,
                                column: 16,
                            },
                            value: "x".into(),
                        }),),
                        right: Box::new(Expression::Identifer(Identifer {
                            location: Location {
                                line: 3,
                                column: 20,
                            },
                            value: "y".into(),
                        }),),
                    },),),),],
                }
            );
        }
        _ => panic!("Not a Statement"),
    }
}

#[test]
fn test_parse_function_call() {
    let input = r#"
        plusTwo(2, 3);
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Call(Call {
            location,
            function,
            arguments,
        })) => {
            assert_eq!(
                location,
                &Location {
                    line: 2,
                    column: 16
                }
            );
            match &**function {
                Expression::Identifer(ident) => assert_eq!(
                    ident,
                    &Identifer {
                        location: Location { line: 2, column: 9 },
                        value: "plusTwo".into()
                    }
                ),
                expr => panic!("Not a identifier, {:?}", expr),
            }

            assert_eq!(arguments.len(), 2);

            match arguments.first().unwrap() {
                Expression::Literal(literal) => assert_eq!(
                    literal,
                    &Literal::Int(
                        Location {
                            line: 2,
                            column: 17
                        },
                        2
                    )
                ),
                _ => panic!("Not a literal"),
            }
        }
        _ => panic!("Not a Statement"),
    }
}

#[test]
fn test_parse_assignment() {
    let input = "a = 10;";

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::Assignment(Assignment {
            location,
            identifier,
            value,
        })) => {
            assert_eq!(location, &Location { line: 1, column: 1 });

            assert_eq!(
                identifier,
                &Identifer {
                    location: Location { line: 1, column: 1 },
                    value: "a".into()
                }
            );
            assert_eq!(
                value,
                &Expression::Literal(Literal::Int(Location { line: 1, column: 5 }, 10))
            );
        }
        _ => panic!("Not a Statement"),
    }
}

#[test]
fn test_parse_for_statement() {
    let input = r#"
        for let i = 0; i < 10; i = i + 1 {
            let a = i;
        }
    "#;

    let mut parser = Parser::new(Lexer::new(input));

    let (program, errors) = parser.parse_program();

    check_parser_errors(errors);

    assert_eq!(program.len(), 1);

    match program.first() {
        Some(Statement::For(For {
            location,
            counter,
            condition,
            step,
            block,
        })) => {
            assert_eq!(location, &Location { line: 2, column: 9 });

            assert_eq!(
                **counter,
                Statement::Let(Let {
                    location: Location {
                        line: 2,
                        column: 24
                    },
                    identifier: Identifer {
                        location: Location {
                            line: 2,
                            column: 17
                        },
                        value: "i".into()
                    },
                    value: Some(Expression::Literal(Literal::Int(
                        Location {
                            line: 2,
                            column: 21
                        },
                        0
                    )))
                })
            );

            assert_eq!(
                condition,
                &Expression::Infix(Infix {
                    location: Location {
                        line: 2,
                        column: 26
                    },
                    operator: InfixOperator::LowerThan,
                    left: Box::new(Expression::Identifer(Identifer {
                        location: Location {
                            line: 2,
                            column: 24
                        },
                        value: "i".into()
                    })),
                    right: Box::new(Expression::Literal(Literal::Int(
                        Location {
                            line: 2,
                            column: 28
                        },
                        10
                    )))
                })
            );

            assert_eq!(
                **step,
                Statement::Assignment(Assignment {
                    location: Location {
                        line: 2,
                        column: 32
                    },
                    identifier: Identifer {
                        location: Location {
                            line: 2,
                            column: 32
                        },
                        value: "i".into()
                    },
                    value: Expression::Infix(Infix {
                        location: Location {
                            line: 2,
                            column: 38
                        },
                        operator: InfixOperator::Plus,
                        left: Box::new(Expression::Identifer(Identifer {
                            location: Location {
                                line: 2,
                                column: 36
                            },
                            value: "i".into()
                        })),
                        right: Box::new(Expression::Literal(Literal::Int(
                            Location {
                                line: 2,
                                column: 40
                            },
                            1
                        )))
                    })
                })
            );

            assert_eq!(
                block,
                &Block {
                    location: Location {
                        line: 2,
                        column: 42
                    },
                    statements: vec![Statement::Let(Let {
                        location: Location { line: 4, column: 9 },
                        identifier: Identifer {
                            location: Location {
                                line: 3,
                                column: 17
                            },
                            value: "a".into()
                        },
                        value: Some(Expression::Identifer(Identifer {
                            location: Location {
                                line: 3,
                                column: 21
                            },
                            value: "i".into()
                        }))
                    })]
                }
            );
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

    panic!()
}
