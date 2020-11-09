use super::evaluator::eval;
use super::evaluator::Environment;
use super::lexer::Lexer;
use super::parser::{ParseError, Parser};
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

        match eval(program, &env) {
            Ok(evaluated) => println!("{}", evaluated),
            Err(error) => println!("error {}", error),
        }
    }
}

fn print_errors(errors: Vec<ParseError>) {
    for error in errors.iter() {
        println!("{}", error.0);
    }
}