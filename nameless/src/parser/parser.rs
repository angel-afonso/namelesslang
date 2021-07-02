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
            Rule::Expression => Statement::Expression(parse_expression(
                pair.into_inner().next().unwrap(),
                Precedence::Lowest,
            )?),
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
        Rule::ReturnStatement => parse_return_statement(pair),
        rule => todo!("{:?}", rule),
    }
}

/// Generate a parsed let statement AST node
fn parse_let_statement(pair: Pair<Rule>) -> ParseResult<Statement> {
    let location = Location::from_position(&pair.as_span().start_pos());
    let mut pairs = pair.into_inner();

    let identifier = parse_identifier(pairs.next().unwrap());

    let expr = match pairs.next() {
        Some(expr) => Some(parse_expression(
            expr.into_inner().next().unwrap(),
            Precedence::Lowest,
        )?),
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
        Some(expr) => Some(parse_expression(
            expr.into_inner().next().unwrap(),
            Precedence::Lowest,
        )?),
        None => None,
    }))
}

/// Transform the pair into an AST expression
fn parse_expression(pair: Pair<Rule>, precedence: Precedence) -> ParseResult<Expression> {
    match pair.as_rule() {
        Rule::Identifier => Ok(Expression::Identifer(parse_identifier(pair))),
        Rule::Integer | Rule::Float | Rule::String | Rule::Char | Rule::Boolean => {
            Ok(Expression::Literal(parse_literal(pair)?))
        }
        Rule::PrefixExpression => parse_prefix_expression(pair),
        Rule::InfixExpression => parse_infix_expression(pair, Precedence::Lowest),
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

    let expression = parse_expression(
        pairs.next().unwrap().into_inner().next().unwrap(),
        Precedence::Lowest,
    )?;

    return Ok(Expression::Prefix(Prefix {
        location,
        operator,
        expression: Box::new(expression),
    }));
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
        rule => todo!("{:?}", rule),
    }
}

fn parse_infix_expression(pair: Pair<Rule>, precedence: Precedence) -> ParseResult<Expression> {
    let mut pairs = pair.into_inner();
    let mut left = parse_expression(pairs.next().unwrap(), Precedence::Lowest)?;

    while let Some(pair) = pairs.next() {
        let location = Location::from_position(&pair.as_span().start_pos());

        let operator = parse_infix_operator(pair);
        println!("{:?} {:?}", precedence, operator.precedence());

        left = if precedence < operator.precedence() {
            Expression::Infix(parse_infix_operation(
                pairs.next().unwrap(),
                left,
                operator,
            )?)
        } else {
            Expression::Infix(Infix {
                location,
                operator,
                left: Box::new(left),
                right: Box::new(parse_infix_expression(
                    pairs.next().unwrap(),
                    operator.precedence(),
                )?),
            })
        }
    }

    Ok(left)
}

fn parse_infix_operation(
    pair: Pair<Rule>,
    left: Expression,
    operator: InfixOperator,
) -> ParseResult<Infix> {
    let location = Location::from_position(&pair.as_span().start_pos());

    let right = parse_expression(pair, operator.precedence())?;

    return Ok(Infix {
        location,
        operator,
        left: Box::new(left),
        right: Box::new(right),
    });
}

fn next_rule_is(pairs: &mut Pairs<Rule>, rule: Rule) -> bool {
    match pairs.next() {
        Some(pair) => pair.as_rule() == rule,
        _ => false,
    }
}

fn pair_precendence(pair: &Pair<Rule>) -> Precedence {
    match pair.as_rule() {
        Rule::Equals | Rule::NotEquals => Precedence::Equals,
        Rule::LowerThan | Rule::GreaterThan => Precedence::LessGreater,
        Rule::Plus | Rule::Minus => Precedence::Sum,
        Rule::Slash | Rule::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}
