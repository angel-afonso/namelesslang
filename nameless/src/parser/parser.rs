use pest::{
    error::Error,
    iterators::{Pair, Pairs},
    Parser,
};

use super::ast::*;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct GrammarParser;

type ParseResult<T> = Result<T, Error<Rule>>;

pub enum Mode {
    Program,
    REPL,
}

/// Parse the input code and returns the program AST
pub fn parse(input: &str, mode: Mode) -> ParseResult<Program> {
    Ok(parse_program(GrammarParser::parse(
        match mode {
            Mode::Program => Rule::Program,
            Mode::REPL => Rule::REPL,
        },
        input,
    )?)?)
}

/// Iterate the code pairs and transforms the coincidences into AST nodes
fn parse_program(code: Pairs<Rule>) -> ParseResult<Program> {
    let mut program = Program::new();

    for pair in code {
        let statement = match pair.as_rule() {
            Rule::Statement => parse_statement(pair.into_inner().next().unwrap())?,
            Rule::Expression => {
                Statement::Expression(parse_expression(pair.into_inner().next().unwrap())?)
            }
            Rule::EOI => return Ok(program),
            rule => unreachable!("{:?}", rule),
        };

        program.push(statement);
    }

    Ok(program)
}

/// Parse a code statement
fn parse_statement(pair: Pair<Rule>) -> ParseResult<Statement> {
    match pair.as_rule() {
        Rule::LetStatement => parse_let_statement(pair),
        Rule::IfStatement => parse_if_statement(pair),
        Rule::ReturnStatement => parse_return_statement(pair),
        Rule::Block => Ok(Statement::Block(parse_block(pair)?)),
        Rule::AssignStatement => parse_assignment(pair),
        Rule::Function => parse_function(pair),
        Rule::Call => Ok(Statement::Call(parse_call(pair)?)),
        rule => todo!("{:?}", rule),
    }
}

/// Generate a parsed `Assignment` AST node
fn parse_assignment(pair: Pair<Rule>) -> ParseResult<Statement> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Statement::Assignment(Assignment {
        location,
        identifier: parse_identifier(pairs.next().unwrap()),
        operator: parse_assignment_operator(pairs.next().unwrap()),
        value: parse_expression(pairs.next().unwrap())?,
    }))
}

fn parse_function(pair: Pair<Rule>) -> ParseResult<Statement> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Statement::Fn(Fn {
        location,
        identifier: parse_identifier(pairs.next().unwrap()),
        params: {
            match pairs.peek().unwrap().as_rule() {
                Rule::Params => parse_params(pairs.next().unwrap()),
                _ => vec![],
            }
        },
        body: parse_block(pairs.next().unwrap())?,
    }))
}

fn parse_closure(pair: Pair<Rule>) -> ParseResult<Expression> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Expression::Closure(Closure {
        location,
        params: {
            match pairs.peek().unwrap().as_rule() {
                Rule::Params => parse_params(pairs.next().unwrap()),
                _ => vec![],
            }
        },
        body: parse_block(pairs.next().unwrap())?,
    }))
}

fn parse_params(pair: Pair<Rule>) -> Vec<Identifer> {
    let pairs = pair.into_inner();
    let mut params = vec![];

    for pair in pairs.into_iter() {
        params.push(parse_identifier(pair));
    }

    params
}

fn parse_call(pair: Pair<Rule>) -> ParseResult<Call> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Call {
        location,
        function: Box::new(parse_expression(pairs.next().unwrap())?),
        arguments: if let Some(pair) = pairs.next() {
            parse_args(pair)?
        } else {
            vec![]
        },
    })
}

fn parse_args(pair: Pair<Rule>) -> ParseResult<Vec<Expression>> {
    let pairs = pair.into_inner();

    let mut args = vec![];

    for pair in pairs.into_iter() {
        args.push(parse_expression(pair)?);
    }

    Ok(args)
}

fn parse_assignment_operator(pair: Pair<Rule>) -> AssignOperator {
    match pair.as_rule() {
        Rule::Assign => AssignOperator::Assign,
        Rule::PlusAssign => AssignOperator::PlusAssign,
        Rule::MinusAssign => AssignOperator::MinusAssign,
        Rule::MultiplyAssign => AssignOperator::MultiplyAssign,
        Rule::DivideAssign => AssignOperator::DivideAssign,
        Rule::ModuleAssign => AssignOperator::ModuleAssign,
        _ => unreachable!(),
    }
}

/// Generate a parsed block AST node
fn parse_block(pair: Pair<Rule>) -> ParseResult<Block> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let pairs = pair.into_inner();

    let mut stmts = Vec::new();

    for pair in pairs.into_iter() {
        stmts.push(parse_statement(pair.into_inner().next().unwrap())?)
    }

    Ok(Block {
        location,
        statements: stmts,
    })
}

fn parse_if_statement(pair: Pair<Rule>) -> ParseResult<Statement> {
    let pairs = pair.into_inner();

    let mut conditions = vec![];

    for pair in pairs.into_iter() {
        match pair.as_rule() {
            Rule::Condition => {
                conditions.push(parse_condition(pair)?);
            }
            Rule::Alternative => {
                return Ok(Statement::If(If {
                    conditions,
                    alternative: Some(parse_alternative(pair)?),
                }))
            }
            rule => unreachable!("{:?}", rule),
        }
    }

    Ok(Statement::If(If {
        conditions,
        alternative: None,
    }))
}

fn parse_condition(pair: Pair<Rule>) -> ParseResult<Condition> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Condition {
        location,
        condition: parse_expression(pairs.next().unwrap())?,
        consequence: parse_block(pairs.next().unwrap())?,
    })
}

fn parse_alternative(pair: Pair<Rule>) -> ParseResult<Else> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    Ok(Else {
        location,
        consequence: parse_block(pairs.next().unwrap())?,
    })
}

/// Generate a parsed let statement AST node
fn parse_let_statement(pair: Pair<Rule>) -> ParseResult<Statement> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    let identifier = parse_identifier(pairs.next().unwrap());

    let expr = match pairs.next() {
        Some(expr) => Some(parse_expression(expr.into_inner().next().unwrap())?),
        _ => None,
    };

    Ok(Statement::Let(Let {
        location,
        identifier,
        value: expr,
    }))
}

/// Generate return AST node
fn parse_return_statement(pair: Pair<Rule>) -> ParseResult<Statement> {
    Ok(Statement::Return(match pair.into_inner().next() {
        Some(expr) => Some(parse_expression(expr.into_inner().next().unwrap())?),
        None => None,
    }))
}

/// Transform the pair into an AST expression
fn parse_expression(pair: Pair<Rule>) -> ParseResult<Expression> {
    match pair.as_rule() {
        Rule::Identifier => Ok(Expression::Identifer(parse_identifier(pair))),
        Rule::Integer | Rule::Float | Rule::String | Rule::Char | Rule::Boolean => {
            Ok(Expression::Literal(parse_literal(pair)?))
        }
        Rule::PrefixExpression => parse_prefix_expression(pair),
        Rule::InfixExpression => parse_infix_expression(pair),
        Rule::Array => parse_array(pair),
        Rule::GroupedExpression => parse_grouped_expression(pair),
        Rule::Index => parse_index(pair),
        Rule::Expression => parse_expression(pair.into_inner().next().unwrap()),
        Rule::Closure => parse_closure(pair),
        Rule::Call => Ok(Expression::Call(parse_call(pair)?)),
        rule => todo!("{:?}", rule),
    }
}

/// Generate `Identifier` from `Pair<Rule>`
fn parse_identifier(pair: Pair<Rule>) -> Identifer {
    Identifer {
        location: Location::from_position(&pair.as_span().start_pos()),
        name: pair.as_str().to_string(),
    }
}

/// Generate an `Literal` AST node
fn parse_literal(pair: Pair<Rule>) -> ParseResult<Literal> {
    match pair.as_rule() {
        Rule::Integer => parse_integer(pair),
        Rule::String => Ok(parse_string(pair)),
        Rule::Float => parse_float(pair),
        Rule::Char => Ok(parse_char(pair)),
        Rule::Boolean => Ok(parse_boolean(pair)),
        rule => unreachable!("{:?}", rule),
    }
}

/// Transforms the pair into `Literal::String`
fn parse_string(pair: Pair<Rule>) -> Literal {
    let string = pair.as_str().to_string();

    Literal::String(
        Location::from_position(&pair.as_span().start_pos()),
        string[1..string.len() - 1].into(),
    )
}

/// Transforms the pair into `Literal::Int`
fn parse_integer(pair: Pair<Rule>) -> ParseResult<Literal> {
    let location = Location::from_position(&pair.as_span().start_pos());

    match pair.as_str().parse::<i64>() {
        Ok(integer) => Ok(Literal::Int(location, integer)),
        Err(_) => Err(Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: format!("Cannot parse {} into i64", pair.as_str()),
            },
            pair.as_span().start_pos(),
        )),
    }
}

/// Transforms the pair into `Literal::Float`
fn parse_float(pair: Pair<Rule>) -> ParseResult<Literal> {
    let location = Location::from_position(&pair.as_span().start_pos());

    match pair.as_str().parse::<f64>() {
        Ok(integer) => Ok(Literal::Float(location, integer)),
        Err(_) => Err(Error::new_from_pos(
            pest::error::ErrorVariant::CustomError {
                message: format!("Cannot parse {} into i64", pair.as_str()),
            },
            pair.as_span().start_pos(),
        )),
    }
}

/// Transforms the input into `Literal::Char`
fn parse_char(pair: Pair<Rule>) -> Literal {
    let string = pair.as_str().to_string();

    Literal::Char(
        Location::from_position(&pair.as_span().start_pos()),
        string[1..string.len() - 1].chars().next().unwrap(),
    )
}

/// Transforms the pair into `Literal::Bool`
fn parse_boolean(pair: Pair<Rule>) -> Literal {
    Literal::Bool(
        Location::from_position(&pair.as_span().start_pos()),
        pair.as_str() == "true",
    )
}

/// Parse and return a Prefix operation AST node
fn parse_prefix_expression(pair: Pair<Rule>) -> ParseResult<Expression> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    let operator = parse_prefix_operator(pairs.next().unwrap());

    let expression = parse_expression(pairs.next().unwrap().into_inner().next().unwrap())?;

    return Ok(Expression::Prefix(Prefix {
        location,
        operator,
        expression: Box::new(expression),
    }));
}

fn parse_index(pair: Pair<Rule>) -> ParseResult<Expression> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    let expr = parse_expression(pairs.next().unwrap())?;

    let index = parse_expression(pairs.next().unwrap())?;

    Ok(Expression::Index(Index {
        location,
        left: Box::new(expr),
        index: Box::new(index),
    }))
}

fn parse_array(pair: Pair<Rule>) -> ParseResult<Expression> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let pairs = pair.into_inner();

    let mut exprs = Vec::new();

    for pair in pairs.into_iter() {
        exprs.push(parse_expression(pair.into_inner().next().unwrap())?)
    }

    Ok(Expression::Array(Array {
        location,
        expressions: Box::new(exprs),
    }))
}

/// Returns the prefix operator corresponding to the rule in pair
fn parse_prefix_operator(pair: Pair<Rule>) -> PrefixOperator {
    match pair.as_rule() {
        Rule::Plus => PrefixOperator::Plus,
        Rule::Minus => PrefixOperator::Minus,
        Rule::Bang => PrefixOperator::Not,
        rule => todo!("{:?}", rule),
    }
}

/// Returns the infix operator corresponding to the rule in pair
fn parse_infix_operator(pair: Pair<Rule>) -> InfixOperator {
    match pair.as_rule() {
        Rule::Plus => InfixOperator::Plus,
        Rule::Minus => InfixOperator::Minus,
        Rule::Slash => InfixOperator::Divide,
        Rule::Asterisk => InfixOperator::Multiply,
        Rule::And => InfixOperator::And,
        Rule::Or => InfixOperator::Or,
        Rule::GreaterEqualsThan => InfixOperator::GreaterEqualsThan,
        Rule::GreaterThan => InfixOperator::GreaterThan,
        Rule::LowerEqualsThan => InfixOperator::LowerEqualsThan,
        Rule::LowerThan => InfixOperator::LowerThan,
        Rule::Equals => InfixOperator::Equals,
        Rule::NotEquals => InfixOperator::NotEquals,
        rule => todo!("{:?}", rule),
    }
}

/// Returns a parsed grouped expression
fn parse_grouped_expression(pair: Pair<Rule>) -> ParseResult<Expression> {
    parse_expression(
        pair.into_inner()
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap(),
    )
}

/// Returns an Infix operation AST node
fn parse_infix_expression(pair: Pair<Rule>) -> ParseResult<Expression> {
    let mut pairs = pair.into_inner();

    let (expr, operator, location) = extract_expr_with_operator(&mut pairs)?;

    parse_infix_operation(pairs.next().unwrap(), expr, operator, location)
}

fn extract_expr_with_operator(
    pairs: &mut Pairs<Rule>,
) -> ParseResult<(Expression, InfixOperator, Location)> {
    let expr = parse_expression(pairs.next().unwrap())?;

    let location = Location::from_position(&pairs.peek().unwrap().as_span().start_pos());

    let operator = parse_infix_operator(pairs.next().unwrap());

    Ok((expr, operator, location))
}

fn parse_infix_operation(
    pair: Pair<Rule>,
    left: Expression,
    left_operator: InfixOperator,
    location: Location,
) -> ParseResult<Expression> {
    match pair.as_rule() {
        Rule::InfixExpression => {
            let mut pairs = pair.into_inner();

            let (expr, operator, right_location) = extract_expr_with_operator(&mut pairs)?;

            if left_operator.precedence() < operator.precedence() {
                Ok(Expression::Infix(Infix {
                    location: right_location,
                    left: Box::new(left),
                    operator: left_operator,
                    right: Box::new(parse_infix_operation(
                        pairs.next().unwrap(),
                        expr,
                        operator,
                        location,
                    )?),
                }))
            } else {
                let right_location =
                    Location::from_position(&pairs.peek().unwrap().as_span().start_pos());

                parse_infix_operation(
                    pairs.next().unwrap(),
                    Expression::Infix(Infix {
                        location: right_location,
                        left: Box::new(left),
                        right: Box::new(expr),
                        operator: left_operator,
                    }),
                    operator,
                    location,
                )
            }
        }
        _ => Ok(Expression::Infix(Infix {
            location,
            left: Box::new(left),
            right: Box::new(parse_expression(pair)?),
            operator: left_operator,
        })),
    }
}
