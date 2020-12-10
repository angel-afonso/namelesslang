use super::super::parser::ast::*;
use super::builtin::Builtin;
use super::{Env, Environment};
use super::{Object, Type};
use std::fmt::{Display, Formatter};

pub type EvaluatorResult = Result<Object, EvaluatorError>;

#[derive(Debug)]
pub struct EvaluatorError(pub String);

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval(program: Program, env: &Env) -> EvaluatorResult {
    eval_top_level_statements(&program, env)?;

    let result = eval_environment("main", env)?;

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
            Statement::Let(let_stmt) => {
                eval_let_statement(&let_stmt.identifier, &let_stmt.value, env)?;
            }
            Statement::Fn(function) => {
                eval_function(function, env)?;
            }
            statement => return Err(EvaluatorError(format!("Unexpected {}", statement))),
        }
    }

    Ok(Object::Void)
}

fn eval_statement(statement: &Statement, env: &Env) -> EvaluatorResult {
    match statement {
        Statement::Let(let_stmt) => eval_let_statement(&let_stmt.identifier, &let_stmt.value, env),
        Statement::Assignment(assignment) => {
            eval_assignment(&assignment.identifier, &assignment.value, env)
        }
        Statement::Block(block) => {
            let extended_env = Environment::new_enclosed(env.clone());
            eval_statements(&block.statements, &extended_env)
        }
        Statement::Fn(function) => eval_function(function, env),
        Statement::Return(expression) => eval_return_statement(expression, env),
        Statement::If(if_stmt) => eval_if_statement(
            &if_stmt.condition,
            &if_stmt.consequence,
            &if_stmt.alternative,
            env,
        ),
        Statement::Call(call) => eval_function_call(&call.function, &call.arguments, env),
        Statement::For(for_stmt) => eval_for_statement(
            &for_stmt.counter,
            &for_stmt.condition,
            &for_stmt.step,
            &for_stmt.block,
            env,
        ),
    }
}

fn eval_statements(statements: &Vec<Statement>, env: &Env) -> EvaluatorResult {
    for stmt in statements.iter() {
        match eval_statement(&stmt, env)? {
            Object::ReturnValue(value) => return Ok(Object::ReturnValue(value)),
            _ => {}
        }
    }

    Ok(Object::Void)
}

fn eval_expression(expression: &Expression, env: &Env) -> EvaluatorResult {
    match expression {
        Expression::Identifer(identifier) => eval_identifier(identifier, env),
        Expression::Literal(literal) => eval_literal(literal),
        Expression::Prefix(prefix) => {
            eval_prefix_expression(&prefix.operator, &*prefix.expression, env)
        }
        Expression::Infix(infix) => {
            let left_obj = eval_expression(&*infix.left, env)?;
            let right_obj = eval_expression(&*infix.right, env)?;
            eval_infix_expression(&infix.operator, &left_obj, &right_obj)
        }
        Expression::Array(array) => eval_array(&array.expressions, env),
        Expression::Call(call) => match eval_function_call(&call.function, &call.arguments, env)? {
            Object::ReturnValue(value) => Ok(*value),
            Object::Void => Err(EvaluatorError("Cannot use void as expression".into())),
            obj => Ok(obj),
        },
        Expression::Index(index) => eval_index_expression(&index.left, &index.index, env),
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
    alternative: &Option<Else>,
    env: &Env,
) -> EvaluatorResult {
    let condition = eval_expression(condition, env)?;

    match condition {
        Object::Boolean(value) => {
            if value {
                let extended_env = Environment::new_enclosed(env.clone());
                return eval_statements(&consequence.statements, &extended_env);
            }

            Ok(Object::Void)
        }
        obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
    }
}

fn eval_else(alternative: &Else, env: &Env) -> EvaluatorResult {
    todo!();
}

fn eval_array(exprs: &Vec<Expression>, env: &Env) -> EvaluatorResult {
    let elements = eval_expressions(exprs, env)?;
    Ok(Object::Array(Box::new(elements)))
}

fn eval_index_expression(left: &Expression, index: &Expression, env: &Env) -> EvaluatorResult {
    let array = match eval_expression(left, env)? {
        Object::Array(array) => *array,
        expr => {
            return Err(EvaluatorError(format!(
                "index not suported for {:?}",
                expr.object_type()
            )))
        }
    };

    let index = match eval_expression(index, env)? {
        Object::Integer(int) => int,
        expr => {
            return Err(EvaluatorError(format!(
                "invalid index {:?}",
                expr.object_type()
            )))
        }
    };

    Ok(array[index as usize].clone())
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
                return eval_statements(&consequence.statements, &extended_env);
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
                    eval_statements(&block.statements, &for_env)?;
                    eval_statement(step, &for_env)?;
                } else {
                    return Ok(Object::Void);
                }
            }
            obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
        }
    }
}

fn eval_return_statement(expression: &Option<Expression>, env: &Env) -> EvaluatorResult {
    match expression {
        Some(expr) => Ok(Object::ReturnValue(Box::new(eval_expression(expr, env)?))),
        None => Ok(Object::ReturnValue(Box::new(Object::Void))),
    }
}

fn eval_function(function: &Fn, env: &Env) -> EvaluatorResult {
    if env.borrow().exists(&function.identifier.value) {
        return Err(EvaluatorError(format!(
            "{} is already defined",
            &function.identifier.value
        )));
    }

    let function_obj = Object::Function(
        function.identifier.clone(),
        function.params.clone(),
        function.body.clone(),
        env.clone(),
    );

    env.borrow_mut()
        .set(function.identifier.value.clone(), &function_obj);

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
        Object::Function(_, params, block, fn_env) => (params, block.statements, fn_env),
        _ => return Err(EvaluatorError(format!("{:?} is not a function", func))),
    };

    let args = eval_expressions(args, env)?;

    let extended_env = Environment::new_enclosed(func_env.clone());

    for (idx, arg) in fn_params.iter().enumerate() {
        extended_env.borrow_mut().set(arg.value.clone(), &args[idx]);
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
    if env.borrow().exists(&identifier.value) {
        return Err(EvaluatorError(format!(
            "{} is already defined",
            &identifier.value
        )));
    }

    let value = match expression {
        Some(expr) => eval_expression(expr, env)?,
        None => Object::Null,
    };

    env.borrow_mut().set(identifier.value.clone(), &value);

    Ok(value.clone())
}

fn eval_assignment(identifier: &Identifer, expression: &Expression, env: &Env) -> EvaluatorResult {
    if !env.borrow().exists(&identifier.value) {
        return Err(EvaluatorError(format!(
            "{} is not defined",
            &identifier.value
        )));
    }

    let result = eval_expression(expression, env)?;

    env.borrow_mut().set(identifier.value.clone(), &result);

    Ok(result)
}

fn eval_identifier(identifier: &Identifer, env: &Env) -> EvaluatorResult {
    eval_environment(&identifier.value, env)
}

fn eval_environment(name: &str, env: &Env) -> EvaluatorResult {
    match env.borrow().get(name) {
        Some(value) => Ok(value),
        None => match Builtin::lookup(name) {
            Some(buitin) => Ok(buitin),
            None => Err(EvaluatorError(format!("Not found {} in this scope", name))),
        },
    }
}

fn eval_literal(literal: &Literal) -> EvaluatorResult {
    match literal {
        Literal::Int(_, int) => Ok(Object::Integer(int.clone())),
        Literal::Bool(_, boolean) => Ok(Object::Boolean(boolean.clone())),
        Literal::String(_, string) => Ok(Object::String(string.clone())),
    }
}

fn eval_prefix_expression(
    operator: &PrefixOperator,
    right: &Expression,
    env: &Env,
) -> EvaluatorResult {
    match operator {
        PrefixOperator::Not => eval_not_operator(right, env),
        PrefixOperator::LBracket => todo!(),
        PrefixOperator::Plus => todo!(),
        PrefixOperator::Minus => todo!(),
    }
}

fn eval_not_operator(right: &Expression, env: &Env) -> EvaluatorResult {
    match right {
        Expression::Literal(Literal::Bool(_, boolean)) => Ok(Object::Boolean(!boolean.clone())),
        Expression::Identifer(identifier) => {
            let value = match env.borrow().get(&identifier.value) {
                Some(obj) => obj,
                None => return Err(EvaluatorError(format!("{} not found", identifier.value))),
            };

            match value {
                Object::Boolean(boolean) => Ok(Object::Boolean(!boolean)),
                obj_value => Err(EvaluatorError(format!(
                    "Invalid operator for {:?}",
                    obj_value
                ))),
            }
        }
        Expression::Prefix(prefix) => {
            match eval_prefix_expression(&prefix.operator, &*prefix.expression, env)? {
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
