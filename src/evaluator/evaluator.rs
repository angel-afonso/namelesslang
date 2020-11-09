use super::super::parser::ast::*;
use super::Env;
use super::Object;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct EvaluatorError(String);

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval(program: Program, env: &Env) -> Result<Object, EvaluatorError> {
    let mut result = Object::Null;

    for stmt in program.iter() {
        match eval_statement(&stmt, env)? {
            Object::ReturnValue(value) => return Ok(*value.clone()),
            Object::Error(value) => return Err(EvaluatorError(value.clone())),
            value => result = value.clone(),
        }
    }

    return Ok(result);
}

fn eval_statement(statement: &Statement, env: &Env) -> Result<Object, EvaluatorError> {
    match statement {
        Statement::Let(identifier, expression) => eval_let_statement(identifier, expression, env),
        // Statement::Return(_) => {}
        // Statement::Block(_) => {}
        // Statement::If(_) => {}
        // Statement::Fn(_) => {}
        // Statement::Call(_) => {}
        stmt => Err(EvaluatorError(format!("Unknown statement {:?}", stmt))),
    }
}

fn eval_let_statement(
    identifier: &Identifer,
    expression: &Expression,
    env: &Env,
) -> Result<Object, EvaluatorError> {
    let value = eval_expression(expression, env)?;

    env.borrow_mut().set(identifier.0.clone(), &value);

    Ok(value.clone())
}

fn eval_expression(expression: &Expression, env: &Env) -> Result<Object, EvaluatorError> {
    match expression {
        Expression::Identifer(identifier) => eval_identifier(identifier, env),
        Expression::Literal(literal) => eval_literal(literal),
        // Expression::Prefix(_, _) => {}
        // Expression::Infix(_, _, _) => {}
        // Expression::Block(_) => {}
        // Expression::If(_) => {}
        // Expression::CLosure(_) => {}
        // Expression::Call(_) => {}
        expr => Err(EvaluatorError(format!("Unknown expression {:?}", expr))),
    }
}

fn eval_identifier(identifier: &Identifer, env: &Env) -> Result<Object, EvaluatorError> {
    match env.borrow().get(&identifier.0) {
        Some(value) => Ok(value),
        None => Err(EvaluatorError(format!(
            "Not found {} in this scope",
            identifier.0
        ))),
    }
}

fn eval_literal(literal: &Literal) -> Result<Object, EvaluatorError> {
    match literal {
        Literal::Int(int) => Ok(Object::Integer(int.clone())),
        Literal::Bool(boolean) => Ok(Object::Boolean(boolean.clone())),
        Literal::String(string) => Ok(Object::String(string.clone())),
    }
}
