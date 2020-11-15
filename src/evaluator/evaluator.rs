use super::super::parser::ast::*;
use super::builtin::Builtin;
use super::{Env, Environment};
use super::{Object, Type};
use std::fmt::{Display, Formatter};

pub type EvaluatorResult = Result<Object, EvaluatorError>;

#[derive(Debug)]
pub struct EvaluatorError(String);

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval(program: Program, env: &Env) -> EvaluatorResult {
    eval_top_level_statements(&program, env)?;

    let result = eval_identifier(&Identifer("main".into()), env)?;

    match result {
        Object::Function(identifier, _, _, env_fn) => {
            eval_fn_call(&Expression::Identifer(identifier), &vec![], &env_fn)
        }
        obj => return Err(EvaluatorError(format!("Unexpected {}", obj))),
    }
}

pub fn eval_repl(program: Program, env: &Env) -> EvaluatorResult {
    eval_statements(&program, env)
}

fn eval_top_level_statements(statements: &Vec<Statement>, env: &Env) -> EvaluatorResult {
    for stmt in statements.iter() {
        match stmt {
            Statement::Let(identifier, expression) => {
                eval_let_statement(identifier, expression, env)?;
            }
            Statement::Fn(function) => {
                eval_function(function, env)?;
            }
            statement => return Err(EvaluatorError(format!("Unexpected {:?}", statement))),
        }
    }

    Ok(Object::Void)
}

fn eval_statement(statement: &Statement, env: &Env) -> EvaluatorResult {
    match statement {
        Statement::Let(identifier, expression) => eval_let_statement(identifier, expression, env),
        Statement::Assignment(identifier, expression) => {
            eval_assignment(identifier, expression, env)
        }
        Statement::Block(Block(statements)) => {
            let extended_env = Environment::new_enclosed(env.clone());
            eval_statements(statements, &extended_env)
        }
        Statement::Fn(function) => eval_function(function, env),
        Statement::Return(expression) => eval_return_statement(expression, env),
        Statement::If(IfStatement {
            condition,
            consequence,
            alternative,
        }) => eval_if_statement(condition, consequence, alternative, env),
        Statement::Call(Call {
            function,
            arguments,
        }) => eval_function_call(function, arguments, env),
        Statement::For(For {
            counter,
            condition,
            step,
            block,
        }) => eval_for_statement(counter, condition, step, block, env),
    }
}

fn eval_statements(statements: &Vec<Statement>, env: &Env) -> EvaluatorResult {
    for stmt in statements.iter() {
        match eval_statement(&stmt, env)? {
            Object::ReturnValue(value) => return Ok(Object::ReturnValue(value)),
            Object::Error(value) => return Err(EvaluatorError(value.clone())),
            _ => {}
        }
    }

    Ok(Object::Void)
}

fn eval_expression(expression: &Expression, env: &Env) -> EvaluatorResult {
    match expression {
        Expression::Identifer(identifier) => eval_identifier(identifier, env),
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Prefix(operator, right) => eval_prefix_expression(operator, &**right, env),
        Expression::Infix(operator, left, right) => {
            let left_obj = eval_expression(&*left, env)?;
            let right_obj = eval_expression(&*right, env)?;
            eval_infix_expression(operator, &left_obj, &right_obj)
        }
        Expression::Call(Call {
            function,
            arguments,
        }) => match eval_function_call(function, arguments, env)? {
            Object::ReturnValue(value) => Ok(*value.clone()),
            Object::Error(err) => return Err(EvaluatorError(err.clone())),
            Object::Void => Err(EvaluatorError("Cannot use void as expression".into())),
            obj => Ok(obj),
        },
        Expression::If(_) => todo!(),
        expr => Err(EvaluatorError(format!("Unknown expression {:?}", expr))),
    }
}

fn eval_expressions(exprs: &Vec<Expression>, env: &Env) -> Result<Vec<Object>, EvaluatorError> {
    let mut result = Vec::new();

    for expr in exprs.iter() {
        result.push(eval_expression(expr, env)?);
    }

    Ok(result)
}

fn eval_if_statement(
    condition: &Expression,
    consequence: &Block,
    alternative: &Option<Box<Statement>>,
    env: &Env,
) -> EvaluatorResult {
    let condition = eval_expression(condition, env)?;

    match condition {
        Object::Boolean(value) => {
            if value {
                let extended_env = Environment::new_enclosed(env.clone());
                return eval_statements(&consequence.0, &extended_env);
            }

            match alternative {
                Some(stmt) => {
                    eval_statement(&*stmt, env)?;
                }
                _ => {}
            }

            Ok(Object::Void)
        }
        obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
    }
}

fn eval_if_expression(
    condition: &Expression,
    consequence: &Block,
    alternative: &Option<Box<Expression>>,
    env: &Env,
) -> EvaluatorResult {
    let condition = eval_expression(condition, env)?;

    match condition {
        Object::Boolean(value) => {
            if value {
                let extended_env = Environment::new_enclosed(env.clone());
                return eval_statements(&consequence.0, &extended_env);
            }

            match alternative {
                Some(expression) => eval_expression(expression, env),
                _ => Ok(Object::Void),
            }
        }
        obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
    }
}

fn eval_for_statement(
    counter: &Statement,
    condition: &Expression,
    step: &Statement,
    block: &Block,
    env: &Env,
) -> EvaluatorResult {
    let for_env = Environment::new_enclosed(env.clone());

    eval_statement(counter, &for_env)?;

    loop {
        match eval_expression(condition, &for_env)? {
            Object::Boolean(value) => {
                if value {
                    eval_statements(&block.0, &for_env)?;
                    eval_statement(step, &for_env)?;
                } else {
                    return Ok(Object::Void);
                }
            }
            obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
        }
    }
}

fn eval_return_statement(expression: &Expression, env: &Env) -> EvaluatorResult {
    let result = eval_expression(expression, env)?;

    Ok(Object::ReturnValue(Box::new(result)))
}

fn eval_function(function: &Fn, env: &Env) -> EvaluatorResult {
    if env.borrow().exists(&function.identifier.0) {
        return Err(EvaluatorError(format!(
            "{} is already defined",
            &function.identifier.0
        )));
    }

    let function_obj = Object::Function(
        function.identifier.clone(),
        function.params.clone(),
        function.body.clone(),
        env.clone(),
    );

    env.borrow_mut()
        .set(function.identifier.0.clone(), &function_obj);

    return Ok(function_obj);
}

fn eval_builtin_function_call(
    builtin: Builtin,
    args: &Vec<Expression>,
    env: &Env,
) -> EvaluatorResult {
    let args = eval_expressions(args, env)?;

    return builtin.call(args.first().unwrap());
}

fn eval_fn_call(func: &Expression, args: &Vec<Expression>, env: &Env) -> EvaluatorResult {
    let (fn_params, block, func_env) = match eval_expression(func, env)? {
        Object::Function(_, params, Block(body), fn_env) => (params, body, fn_env),
        _ => return Err(EvaluatorError(format!("{:?} is not a function", func))),
    };

    let args = eval_expressions(args, env)?;

    let extended_env = Environment::new_enclosed(func_env.clone());

    for (idx, arg) in fn_params.iter().enumerate() {
        extended_env.borrow_mut().set(arg.0.clone(), &args[idx]);
    }

    eval_statements(&block, &extended_env)
}

fn eval_function_call(func: &Expression, args: &Vec<Expression>, env: &Env) -> EvaluatorResult {
    match eval_expression(func, env)? {
        Object::Function(_, _, _, _) => eval_fn_call(func, args, env),
        Object::Builtin(builtin) => eval_builtin_function_call(builtin, args, env),
        _ => return Err(EvaluatorError(format!("{:?} is not a function", func))),
    }
}

fn eval_let_statement(
    identifier: &Identifer,
    expression: &Option<Expression>,
    env: &Env,
) -> EvaluatorResult {
    if env.borrow().exists(&identifier.0) {
        return Err(EvaluatorError(format!(
            "{} is already defined",
            &identifier.0
        )));
    }

    let value = match expression {
        Some(expr) => eval_expression(expr, env)?,
        None => Object::Null,
    };

    env.borrow_mut().set(identifier.0.clone(), &value);

    Ok(value.clone())
}

fn eval_assignment(identifier: &Identifer, expression: &Expression, env: &Env) -> EvaluatorResult {
    if !env.borrow().exists(&identifier.0) {
        return Err(EvaluatorError(format!("{} is not defined", &identifier.0)));
    }

    let result = eval_expression(expression, env)?;

    env.borrow_mut().set(identifier.0.clone(), &result);

    Ok(result)
}

fn eval_identifier(identifier: &Identifer, env: &Env) -> EvaluatorResult {
    match env.borrow().get(&identifier.0) {
        Some(value) => Ok(value),
        None => match Builtin::lookup(&identifier.0) {
            Some(buitin) => Ok(buitin),
            None => Err(EvaluatorError(format!(
                "Not found {} in this scope",
                identifier.0
            ))),
        },
    }
}

fn eval_literal(literal: &Literal) -> EvaluatorResult {
    match literal {
        Literal::Int(int) => Ok(Object::Integer(int.clone())),
        Literal::Bool(boolean) => Ok(Object::Boolean(boolean.clone())),
        Literal::String(string) => Ok(Object::String(string.clone())),
    }
}

fn eval_prefix_expression(
    operator: &PrefixOperator,
    right: &Expression,
    env: &Env,
) -> EvaluatorResult {
    match operator {
        PrefixOperator::Not => eval_not_operator(right, env),
        PrefixOperator::Plus => todo!(),
        PrefixOperator::Minus => todo!(),
    }
}

fn eval_not_operator(right: &Expression, env: &Env) -> EvaluatorResult {
    match right {
        Expression::Literal(Literal::Bool(boolean)) => Ok(Object::Boolean(!boolean.clone())),
        Expression::Identifer(Identifer(ident)) => {
            let value = match env.borrow().get(ident) {
                Some(obj) => obj,
                None => return Err(EvaluatorError(format!("{} not found", ident))),
            };

            match value {
                Object::Boolean(boolean) => Ok(Object::Boolean(!boolean)),
                obj_value => Err(EvaluatorError(format!(
                    "Invalid operator for {:?}",
                    obj_value
                ))),
            }
        }
        Expression::Prefix(operator, right) => {
            match eval_prefix_expression(operator, &**right, env)? {
                Object::Boolean(boolean) => Ok(Object::Boolean(!boolean)),
                obj_value => Err(EvaluatorError(format!(
                    "Invalid operator for {:?}",
                    obj_value
                ))),
            }
        }
        epxr => Err(EvaluatorError(format!("Invalid operator for {:?}", epxr))),
    }
}

fn eval_infix_expression(
    operator: &InfixOperator,
    left: &Object,
    right: &Object,
) -> EvaluatorResult {
    if left.object_type() == Type::Integer && right.object_type() == Type::Integer {
        return eval_integer_infix(operator, left, right);
    }

    match operator {
        InfixOperator::Equal => {
            if left.object_type() != right.object_type() {
                return Err(EvaluatorError(format!(
                    "Cannot compare, {:?} with {:?}",
                    left.object_type(),
                    right.object_type()
                )));
            }

            return Ok(Object::Boolean(left == right));
        }
        InfixOperator::Plus => {
            if left.object_type() == Type::String || right.object_type() == Type::String {
                return Ok(Object::String(format!("{}{}", left, right)));
            }

            return Err(EvaluatorError(format!(
                "Invalid operator for {:?} and {:?}",
                left.object_type(),
                right.object_type()
            )));
        }
        InfixOperator::NotEqual => {
            if left.object_type() != right.object_type() {
                return Err(EvaluatorError(format!(
                    "Cannot compare, {:?} with {:?}",
                    left.object_type(),
                    right.object_type()
                )));
            }

            return Ok(Object::Boolean(left == right));
        }
        _ => {
            return Err(EvaluatorError(format!(
                "Invalid operator for {:?} and {:?}",
                left.object_type(),
                right.object_type()
            )))
        }
    }
}

fn eval_integer_infix(operator: &InfixOperator, left: &Object, right: &Object) -> EvaluatorResult {
    let left_val = match left {
        Object::Integer(val) => val.clone(),
        _ => return Err(EvaluatorError(format!("Expected int"))),
    };

    let right_val = match right {
        Object::Integer(val) => val.clone(),
        _ => return Err(EvaluatorError(format!("Expected int"))),
    };

    match operator {
        InfixOperator::Plus => Ok(Object::Integer(left_val + right_val)),
        InfixOperator::Minus => Ok(Object::Integer(left_val - right_val)),
        InfixOperator::Multiply => Ok(Object::Integer(left_val * right_val)),
        InfixOperator::Divide => Ok(Object::Integer(left_val / right_val)),
        InfixOperator::Equal => Ok(Object::Boolean(left_val == right_val)),
        InfixOperator::NotEqual => Ok(Object::Boolean(left_val != right_val)),
        InfixOperator::LowerThan => Ok(Object::Boolean(left_val < right_val)),
        InfixOperator::GreaterThan => Ok(Object::Boolean(left_val > right_val)),
        _ => Err(EvaluatorError(format!("Invalid operator for int types"))),
    }
}
