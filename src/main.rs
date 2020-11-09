#![allow(dead_code)]
mod evaluator;
mod lexer;
mod parser;
mod repl;

use repl::start_repl;

fn main() {
    start_repl();
}
