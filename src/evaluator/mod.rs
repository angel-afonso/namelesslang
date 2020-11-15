mod builtin;
mod environment;
mod evaluator;
mod object;

#[cfg(test)]
mod test;

pub use environment::{Env, Environment};
pub use evaluator::{eval, eval_repl};
pub use object::{Object, Type};
