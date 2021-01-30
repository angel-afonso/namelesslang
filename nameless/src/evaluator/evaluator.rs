use super::super::parser::ast::*;
use super::builtin::Builtin;
use super::Environment;
use super::{Object, Type};
use std::fmt::{Display, Formatter};
pub type EvaluatorResult = Result<Object, EvaluatorError>;

#[derive(Debug)]
pub struct EvaluatorError(pub String);

pub struct Stream<OUT, IN>
where
    OUT: FnMut(String) + std::ops::Fn(String),
    IN: FnMut() -> String + std::ops::Fn() -> String,
{
    pub stdout: OUT,
    pub stdin: IN,
}

impl Display for EvaluatorError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Evaluator<OUT, IN>
where
    OUT: FnMut(String) + std::ops::Fn(String),
    IN: std::ops::Fn() -> String,
{
    stream: Stream<OUT, IN>,
}

impl<OUT, IN> Evaluator<OUT, IN>
where
    OUT: FnMut(String) + std::ops::Fn(String),
    IN: FnMut() -> String + std::ops::Fn() -> String,
{
    pub fn new(stdout: OUT, stdin: IN) -> Evaluator<OUT, IN> {
        Evaluator {
            stream: Stream { stdout, stdin },
        }
    }

    pub fn eval(&self, program: Program, env: &Environment) -> EvaluatorResult {
        self.eval_top_level_statements(&program, env)?;

        let result = self.eval_environment("main", env, None)?;

        match result {
            Object::Function(identifier, _, _, env_fn) => {
                self.eval_fn_call(&Expression::Identifer(identifier), &vec![], &env_fn)
            }
            obj => return Err(EvaluatorError(format!("Unexpected {}", obj))),
        }
    }

    pub fn eval_repl(&self, program: Program, env: &Environment) -> EvaluatorResult {
        self.eval_statements(&program, env)
    }

    fn eval_top_level_statements(
        &self,
        statements: &Vec<Statement>,
        env: &Environment,
    ) -> EvaluatorResult {
        for stmt in statements.iter() {
            match stmt {
                Statement::Let(let_stmt) => {
                    self.eval_let_statement(&let_stmt.identifier, &let_stmt.value, env)?;
                }
                Statement::Fn(function) => {
                    self.eval_function(function, env)?;
                }
                statement => return Err(EvaluatorError(format!("Unexpected {}", statement))),
            }
        }

        Ok(Object::Void)
    }

    fn eval_statement(&self, statement: &Statement, env: &Environment) -> EvaluatorResult {
        match statement {
            Statement::Let(let_stmt) => {
                self.eval_let_statement(&let_stmt.identifier, &let_stmt.value, env)
            }
            Statement::Assignment(assignment) => {
                self.eval_assignment(&assignment.identifier, &assignment.value, env)
            }
            Statement::Block(block) => {
                let extended_env = Environment::new_enclosed(env.clone());
                self.eval_statements(&block.statements, &extended_env)
            }
            Statement::Fn(function) => self.eval_function(function, env),
            Statement::Return(expression) => self.eval_return_statement(expression, env),
            Statement::If(if_stmt) => self.eval_if_statement(
                &if_stmt.condition,
                &if_stmt.consequence,
                &if_stmt.alternative,
                env,
            ),
            Statement::Call(call) => self.eval_function_call(&call.function, &call.arguments, env),
            Statement::For(for_stmt) => self.eval_for_statement(
                &for_stmt.counter,
                &for_stmt.condition,
                &for_stmt.step,
                &for_stmt.block,
                env,
            ),
            _ => todo!(),
        }
    }

    fn eval_statements(&self, statements: &Vec<Statement>, env: &Environment) -> EvaluatorResult {
        for stmt in statements.iter() {
            match self.eval_statement(&stmt, env)? {
                Object::ReturnValue(value) => return Ok(Object::ReturnValue(value)),
                _ => {}
            }
        }

        Ok(Object::Void)
    }

    fn eval_expression(&self, expression: &Expression, env: &Environment) -> EvaluatorResult {
        match expression {
            Expression::Identifer(identifier) => self.eval_identifier(identifier, env),
            Expression::Literal(literal) => self.eval_literal(literal),
            Expression::Prefix(prefix) => {
                self.eval_prefix_expression(&prefix.operator, &*prefix.expression, env)
            }
            Expression::Infix(infix) => {
                let left_obj = self.eval_expression(&*infix.left, env)?;
                let right_obj = self.eval_expression(&*infix.right, env)?;
                self.eval_infix_expression(&infix.operator, &left_obj, &right_obj)
            }
            Expression::Array(array) => self.eval_array(&array.expressions, env),
            Expression::Call(call) => {
                match self.eval_function_call(&call.function, &call.arguments, env)? {
                    Object::ReturnValue(value) => Ok(*value),
                    Object::Void => Err(EvaluatorError("Cannot use void as expression".into())),
                    obj => Ok(obj),
                }
            }
            Expression::Index(index) => self.eval_index_expression(&index.left, &index.index, env),
            expr => Err(EvaluatorError(format!("Unknown expression {:?}", expr))),
        }
    }

    fn eval_expressions(
        &self,
        exprs: &Vec<Expression>,
        env: &Environment,
    ) -> Result<Vec<Object>, EvaluatorError> {
        let mut result = Vec::new();

        for expr in exprs.iter() {
            result.push(self.eval_expression(expr, env)?);
        }

        Ok(result)
    }

    fn eval_if_statement(
        &self,
        condition: &Expression,
        consequence: &Block,
        alternative: &Option<Else>,
        env: &Environment,
    ) -> EvaluatorResult {
        let condition = self.eval_expression(condition, env)?;

        match condition {
            Object::Boolean(value) => {
                if value {
                    let extended_env = Environment::new_enclosed(env.clone());
                    return self.eval_statements(&consequence.statements, &extended_env);
                }

                match alternative {
                    Some(else_stmt) => return self.eval_else(else_stmt, &env),
                    None => {}
                }

                Ok(Object::Void)
            }
            obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
        }
    }

    fn eval_else(&self, alternative: &Else, env: &Environment) -> EvaluatorResult {
        match alternative {
            Else::If(_, if_stmt) => {
                let extended_env = Environment::new_enclosed(env.clone());
                self.eval_if_statement(
                    &if_stmt.condition,
                    &if_stmt.consequence,
                    &if_stmt.alternative,
                    &extended_env,
                )
            }
            Else::Block(
                _,
                Block {
                    location: _,
                    statements,
                },
            ) => {
                let extended_env = Environment::new_enclosed(env.clone());
                self.eval_statements(statements, &extended_env)
            }
        }
    }

    fn eval_array(&self, exprs: &Vec<Expression>, env: &Environment) -> EvaluatorResult {
        let elements = self.eval_expressions(exprs, env)?;
        Ok(Object::Array(Box::new(elements)))
    }

    fn eval_index_expression(
        &self,
        left: &Expression,
        index: &Expression,
        env: &Environment,
    ) -> EvaluatorResult {
        let array = match self.eval_expression(left, env)? {
            Object::Array(array) => *array,
            expr => {
                return Err(EvaluatorError(format!(
                    "index not suported for {:?}",
                    expr.object_type()
                )))
            }
        };

        let index = match self.eval_expression(index, env)? {
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

    fn eval_for_statement(
        &self,
        counter: &Statement,
        condition: &Expression,
        step: &Statement,
        block: &Block,
        env: &Environment,
    ) -> EvaluatorResult {
        let for_env = Environment::new_enclosed(env.clone());

        self.eval_statement(counter, &for_env)?;

        loop {
            match self.eval_expression(condition, &for_env)? {
                Object::Boolean(value) => {
                    if value {
                        match self.eval_statements(&block.statements, &for_env)? {
                            Object::ReturnValue(return_val) => return Ok(*return_val.clone()),
                            _ => {
                                self.eval_statement(step, &for_env)?;
                            }
                        }
                    } else {
                        return Ok(Object::Void);
                    }
                }
                obj => return Err(EvaluatorError(format!("{} is not a boolean value", obj))),
            }
        }
    }

    fn eval_return_statement(
        &self,
        expression: &Option<Expression>,
        env: &Environment,
    ) -> EvaluatorResult {
        match expression {
            Some(expr) => Ok(Object::ReturnValue(Box::new(
                self.eval_expression(expr, env)?,
            ))),
            None => Ok(Object::ReturnValue(Box::new(Object::Void))),
        }
    }

    fn eval_function(&self, function: &Fn, env: &Environment) -> EvaluatorResult {
        if env.exists(&function.identifier.name) {
            return Err(EvaluatorError(format!(
                "{} is already defined",
                &function.identifier.name
            )));
        }

        let function_obj = Object::Function(
            function.identifier.clone(),
            function.params.clone(),
            function.body.clone(),
            env.clone(),
        );

        env.set(function.identifier.name.clone(), &function_obj);

        return Ok(function_obj);
    }

    fn eval_builtin_function_call(
        &self,
        builtin: Builtin,
        args: &Vec<Expression>,
        env: &Environment,
    ) -> EvaluatorResult {
        let args = self.eval_expressions(args, env)?;

        return builtin.call(args.first().unwrap_or(&Object::Void), &self.stream);
    }

    fn eval_fn_call(
        &self,
        func: &Expression,
        args: &Vec<Expression>,
        env: &Environment,
    ) -> EvaluatorResult {
        let (fn_params, block, func_env) = match self.eval_expression(func, env)? {
            Object::Function(_, params, block, fn_env) => (params, block.statements, fn_env),
            _ => return Err(EvaluatorError(format!("{:?} is not a function", func))),
        };

        let args = self.eval_expressions(args, env)?;

        let extended_env = Environment::new_enclosed(func_env.clone());

        for (idx, arg) in fn_params.iter().enumerate() {
            extended_env.set(arg.name.clone(), &args[idx]);
        }

        self.eval_statements(&block, &extended_env)
    }

    fn eval_function_call(
        &self,
        func: &Expression,
        args: &Vec<Expression>,
        env: &Environment,
    ) -> EvaluatorResult {
        match self.eval_expression(func, env)? {
            Object::Function(_, _, _, _) => self.eval_fn_call(func, args, env),
            Object::Builtin(builtin) => self.eval_builtin_function_call(builtin, args, env),
            _ => return Err(EvaluatorError(format!("{:?} is not a function", func))),
        }
    }

    fn eval_let_statement(
        &self,
        identifier: &Identifer,
        expression: &Option<Expression>,
        env: &Environment,
    ) -> EvaluatorResult {
        if env.exists(&identifier.name) {
            return Err(EvaluatorError(format!(
                "{} is already defined",
                &identifier.name
            )));
        }

        let value = match expression {
            Some(expr) => self.eval_expression(expr, env)?,
            None => Object::Null,
        };

        env.set(identifier.name.clone(), &value);

        Ok(value.clone())
    }

    fn eval_assignment(
        &self,
        identifier: &Identifer,
        expression: &Expression,
        env: &Environment,
    ) -> EvaluatorResult {
        if !env.exists(&identifier.name) {
            return Err(EvaluatorError(format!(
                "{} is not defined",
                &identifier.name
            )));
        }

        let result = self.eval_expression(expression, env)?;

        env.set(identifier.name.clone(), &result);

        Ok(result)
    }

    fn eval_identifier(&self, identifier: &Identifer, env: &Environment) -> EvaluatorResult {
        self.eval_environment(&identifier.name, env, Some(&identifier.location))
    }

    fn eval_environment(
        &self,
        name: &str,
        env: &Environment,
        location: Option<&Location>,
    ) -> EvaluatorResult {
        match env.get(name) {
            Some(value) => Ok(value),
            None => match Builtin::lookup(name) {
                Some(buitin) => Ok(buitin),
                None => Err(EvaluatorError(format!(
                    "Not found {} in this scope.{}",
                    name,
                    match location {
                        Some(loc) => format!(" {}", loc),
                        None => "".into(),
                    }
                ))),
            },
        }
    }

    fn eval_literal(&self, literal: &Literal) -> EvaluatorResult {
        match literal {
            Literal::Int(_, int) => Ok(Object::Integer(*int)),
            Literal::Float(_, float) => Ok(Object::Float(*float)),
            Literal::Bool(_, boolean) => Ok(Object::Boolean(*boolean)),
            Literal::String(_, string) => Ok(Object::String(string.clone())),
        }
    }

    fn eval_prefix_expression(
        &self,
        operator: &PrefixOperator,
        right: &Expression,
        env: &Environment,
    ) -> EvaluatorResult {
        match operator {
            PrefixOperator::Not => self.eval_not_operator(right, env),
            PrefixOperator::Plus => todo!(),
            PrefixOperator::Minus => self.eval_minus_operator(right, env),
        }
    }

    fn eval_minus_operator(&self, right: &Expression, env: &Environment) -> EvaluatorResult {
        match right {
            Expression::Literal(Literal::Int(_, int)) => Ok(Object::Integer(-int)),
            Expression::Identifer(identifier) => {
                let value = match env.get(&identifier.value) {
                    Some(obj) => obj,
                    None => return Err(EvaluatorError(format!("{} not found", identifier.value))),
                };

                match value {
                    Object::Integer(integer) => Ok(Object::Integer(-integer)),
                    obj_value => Err(EvaluatorError(format!(
                        "Invalid operator for {:?}",
                        obj_value
                    ))),
                }
            }
            Expression::Prefix(prefix) => {
                match self.eval_prefix_expression(&prefix.operator, &*prefix.expression, env)? {
                    Object::Integer(int) => Ok(Object::Integer(-int)),
                    obj_value => Err(EvaluatorError(format!(
                        "Invalid operator for {:?}",
                        obj_value
                    ))),
                }
            }
            epxr => Err(EvaluatorError(format!("Invalid operator for {:?}", epxr))),
        }
    }

    fn eval_not_operator(&self, right: &Expression, env: &Environment) -> EvaluatorResult {
        match right {
            Expression::Literal(Literal::Bool(_, boolean)) => Ok(Object::Boolean(!boolean.clone())),
            Expression::Identifer(identifier) => {
                let value = match env.get(&identifier.name) {
                    Some(obj) => obj,
                    None => return Err(EvaluatorError(format!("{} not found", identifier.name))),
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
                match self.eval_prefix_expression(&prefix.operator, &*prefix.expression, env)? {
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
        &self,
        operator: &InfixOperator,
        left: &Object,
        right: &Object,
    ) -> EvaluatorResult {
        if left.is_numeric() && right.is_numeric() {
            return self.eval_numeric_infix(operator, left, right);
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

    fn eval_numeric_infix(
        &self,
        operator: &InfixOperator,
        left: &Object,
        right: &Object,
    ) -> EvaluatorResult {
        let left_val = match left {
            Object::Integer(val) => *val as f64,
            Object::Float(val) => *val,
            _ => return Err(EvaluatorError(format!("Expected int"))),
        };

        let right_val = match right {
            Object::Integer(val) => *val as f64,
            Object::Float(val) => *val,
            _ => return Err(EvaluatorError(format!("Expected int"))),
        };

        let result = match operator {
            InfixOperator::Plus => left_val + right_val,
            InfixOperator::Minus => left_val - right_val,
            InfixOperator::Multiply => left_val * right_val,
            InfixOperator::Divide => left_val / right_val,
            InfixOperator::Equal => return Ok(Object::Boolean(left_val == right_val)),
            InfixOperator::NotEqual => return Ok(Object::Boolean(left_val != right_val)),
            InfixOperator::LowerThan => return Ok(Object::Boolean(left_val < right_val)),
            InfixOperator::GreaterThan => return Ok(Object::Boolean(left_val > right_val)),
            _ => return Err(EvaluatorError(format!("Invalid operator for int types"))),
        };

        if left.is_integer() && right.is_integer() {
            return Ok(Object::Integer(result as i64));
        }

        Ok(Object::Float(result))
    }
}
