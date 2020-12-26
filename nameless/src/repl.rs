use super::evaluator::eval_repl;
use super::evaluator::Environment;
use super::lexer::Lexer;
use super::parser::Parser;
use super::utils::print_errors;
use std::io::{self, BufRead};

pub fn start_repl() {
    let stdin = io::stdin();
    let env = Environment::new();

    loop {
        let input = stdin.lock().lines().next().unwrap().unwrap();

        let (program, errors) = Parser::new(Lexer::new(&input)).parse_program();

        if errors.len() > 0 {
            print_errors(errors);
            continue;
        }

        match eval_repl(program, &env) {
            Err(error) => println!("ERROR: {}", error),
            _ => {}
        }
    }
}
