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
        // TestReturn {
        //     input: "return 10;",
        //     value: Expression::Literal(Literal::Int(10)),
        // },
        // TestReturn {
        //     input: "return true;",
        //     value: Expression::Literal(Literal::Bool(true)),
        // },
        // TestReturn {
        //     input: "return !true;",
        //     value: Expression::Prefix(
        //         PrefixOperator::Not,
        //         Box::new(Expression::Literal(Literal::Bool(true))),
        //     ),
        // },
        // TestReturn {
        //     input: "return (1 + 2) * 3;",
        //     value: Expression::Infix(
        //         InfixOperator::Multiply,
        //         Box::new(Expression::Infix(
        //             InfixOperator::Plus,
        //             Box::new(Expression::Literal(Literal::Int(1))),
        //             Box::new(Expression::Literal(Literal::Int(2))),
        //         )),
        //         Box::new(Expression::Literal(Literal::Int(3))),
        //     ),
        // },
        // TestReturn {
        //     input: "return (1 * 2) / 3;",
        //     value: Expression::Infix(
        //         InfixOperator::Divide,
        //         Box::new(Expression::Infix(
        //             InfixOperator::Multiply,
        //             Box::new(Expression::Literal(Literal::Int(1))),
        //             Box::new(Expression::Literal(Literal::Int(2))),
        //         )),
        //         Box::new(Expression::Literal(Literal::Int(3))),
        //     ),
        // },
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

// #[test]
// fn test_parse_block_statements() {
//     let input = r#"{
//         let x = 5;
//         let y = x;
//     }"#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::Block(Block(stmts))) => {
//             assert_eq!(stmts.len(), 2);
//         }
//         _ => panic!("Not a Statement::Block"),
//     }
// }

// #[test]
// fn test_parse_if_expression() {
//     let input = r#"
//         if x > y {
//             let a = 10;
//         } else {
//             let b = 10;
//         }
//     "#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::If(IfStatement {
//             condition,
//             consequence,
//             alternative,
//         })) => {
//             assert_eq!(
//                 condition,
//                 &Box::new(Expression::Infix(
//                     InfixOperator::GreaterThan,
//                     Box::new(Expression::Identifer(Identifer("x".into()))),
//                     Box::new(Expression::Identifer(Identifer("y".into()))),
//                 ))
//             );

//             assert_eq!(
//                 consequence,
//                 &Block(vec![Statement::Let(
//                     Identifer("a".into()),
//                     Some(Expression::Literal(Literal::Int(10)))
//                 )])
//             );

//             assert_eq!(
//                 alternative,
//                 &Some(Box::new(Statement::Block(Block(vec![Statement::Let(
//                     Identifer("b".into()),
//                     Some(Expression::Literal(Literal::Int(10)))
//                 )]))))
//             );
//         }
//         _ => panic!("Not a Statement::Block"),
//     }
// }

// #[test]
// fn test_parse_if_else_if_expression() {
//     let input = r#"
//         if x > y {
//             let a = 10;
//         } else if x < y{
//             let b = 10;
//         }
//     "#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::If(IfStatement {
//             condition,
//             consequence,
//             alternative,
//         })) => {
//             assert_eq!(
//                 condition,
//                 &Box::new(Expression::Infix(
//                     InfixOperator::GreaterThan,
//                     Box::new(Expression::Identifer(Identifer("x".into()))),
//                     Box::new(Expression::Identifer(Identifer("y".into()))),
//                 ))
//             );

//             assert_eq!(
//                 consequence,
//                 &Block(vec![Statement::Let(
//                     Identifer("a".into()),
//                     Some(Expression::Literal(Literal::Int(10)))
//                 )])
//             );

//             match alternative {
//                 Some(box_expr) => {
//                     let expr = &**box_expr;

//                     match expr {
//                         Statement::If(IfStatement {
//                             condition,
//                             consequence,
//                             alternative,
//                         }) => {
//                             assert_eq!(
//                                 condition,
//                                 &Box::new(Expression::Infix(
//                                     InfixOperator::LowerThan,
//                                     Box::new(Expression::Identifer(Identifer("x".into()))),
//                                     Box::new(Expression::Identifer(Identifer("y".into()))),
//                                 ))
//                             );

//                             assert_eq!(
//                                 consequence,
//                                 &Block(vec![Statement::Let(
//                                     Identifer("b".into()),
//                                     Some(Expression::Literal(Literal::Int(10)))
//                                 )])
//                             );

//                             assert_eq!(alternative, &None);
//                         }
//                         _ => panic!("Not a else if"),
//                     }
//                 }
//                 _ => panic!("Not a else if"),
//             }
//         }
//         _ => panic!("Not a Statement"),
//     }
// }

// #[test]
// fn test_parse_function_literal() {
//     let input = r#"
//     fn plusTwo(x, y) {
//         return x + y;
//     }
//     "#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::Fn(Fn {
//             identifier,
//             params,
//             body,
//         })) => {
//             assert_eq!(identifier, &Identifer("plusTwo".into()));

//             assert_eq!(params.len(), 2);

//             assert_eq!(params.first().unwrap(), &Identifer("x".into()));

//             assert_eq!(body.0.len(), 1);

//             assert_eq!(
//                 body,
//                 &Block(vec![Statement::Return(Expression::Infix(
//                     InfixOperator::Plus,
//                     Box::new(Expression::Identifer(Identifer("x".into()))),
//                     Box::new(Expression::Identifer(Identifer("y".into()))),
//                 ))])
//             );
//         }
//         _ => panic!("Not a Statement"),
//     }
// }

// #[test]
// fn test_parse_function_call() {
//     let input = r#"
//         plusTwo(2, 3);
//     "#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::Call(Call {
//             function,
//             arguments,
//         })) => {
//             match &**function {
//                 Expression::Identifer(ident) => assert_eq!(ident, &Identifer("plusTwo".into())),
//                 expr => panic!("Not a identifier, {:?}", expr),
//             }

//             assert_eq!(arguments.len(), 2);

//             match arguments.first().unwrap() {
//                 Expression::Literal(literal) => assert_eq!(literal, &Literal::Int(2)),
//                 _ => panic!("Not a literal"),
//             }
//         }
//         _ => panic!("Not a Statement"),
//     }
// }

// #[test]
// fn test_parse_assignment() {
//     let input = "a = 10;";

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::Assignment(ident, expr)) => {
//             assert_eq!(ident, &Identifer("a".into()));
//             assert_eq!(expr, &Expression::Literal(Literal::Int(10)));
//         }
//         _ => panic!("Not a Statement"),
//     }
// }
// #[test]
// fn test_parse_for_statement() {
//     let input = r#"
//         for let i = 0; i < 10; i = i + 1 {
//             let a = i;
//         }
//     "#;

//     let mut parser = Parser::new(Lexer::new(input));

//     let (program, errors) = parser.parse_program();

//     check_parser_errors(errors);

//     assert_eq!(program.len(), 1);

//     match program.first() {
//         Some(Statement::For(For {
//             counter,
//             condition,
//             step,
//             block,
//         })) => {
//             assert_eq!(
//                 **counter,
//                 Statement::Let(
//                     Identifer("i".into()),
//                     Some(Expression::Literal(Literal::Int(0)))
//                 )
//             );

//             assert_eq!(
//                 *condition,
//                 Expression::Infix(
//                     InfixOperator::LowerThan,
//                     Box::new(Expression::Identifer(Identifer("i".into()))),
//                     Box::new(Expression::Literal(Literal::Int(10)))
//                 )
//             );

//             assert_eq!(
//                 **step,
//                 Statement::Assignment(
//                     Identifer("i".into()),
//                     Expression::Infix(
//                         InfixOperator::Plus,
//                         Box::new(Expression::Identifer(Identifer("i".into()))),
//                         Box::new(Expression::Literal(Literal::Int(1)))
//                     )
//                 )
//             );

//             assert_eq!(
//                 *block,
//                 Block(vec![Statement::Let(
//                     Identifer("a".into()),
//                     Some(Expression::Identifer(Identifer("i".into())))
//                 )])
//             );
//         }
//         _ => panic!("Not a Statement"),
//     }
// }

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
