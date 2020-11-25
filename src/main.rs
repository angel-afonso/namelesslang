#![allow(dead_code)]
// mod evaluator;
mod lexer;
mod parser;
// mod repl;
// mod utils;

// use clap::{App, Arg};
// use evaluator::eval;
// use evaluator::Environment;
// use lexer::Lexer;
// use parser::Parser;
// use repl::start_repl;
// use std::fs;
// use utils::print_errors;

fn main() {
    // let matches = App::new("Nameless interpreter")
    //     .version("0.0.1")
    //     .arg(Arg::with_name("file").required(false))
    //     .get_matches();

    // if let Some(filename) = matches.value_of("file") {
    //     let file = fs::read_to_string(filename).expect("Something went wrong reading the file");
    //     let env = Environment::new();

    //     let (program, errors) = Parser::new(Lexer::new(&file)).parse_program();

    //     if errors.len() > 0 {
    //         print_errors(errors);
    //         return;
    //     }

    //     match eval(program, &env) {
    //         Err(error) => println!("error {}", error),
    //         _ => {}
    //     }
    // } else {
    //     start_repl()
    // }
}
