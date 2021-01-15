#![allow(dead_code)]
#![allow(unused_imports)]

mod compiler;
mod evaluator;
mod lexer;
mod parser;
mod utils;
mod vm;

pub use compiler::Compiler;
pub use evaluator::{Environment, Evaluator, Object};
pub use lexer::Lexer;
pub use parser::Parser;
pub use utils::print_errors;
pub use vm::VM;
