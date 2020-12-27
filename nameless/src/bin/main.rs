use clap::{App, Arg};
use nameless::{print_errors, Environment, Evaluator, Lexer, Parser};
use std::fs;
use std::io::{self, BufRead};

fn main() {
    let matches = App::new("Nameless interpreter")
        .version("0.1.0")
        .arg(Arg::with_name("file").required(false))
        .get_matches();

    if let Some(filename) = matches.value_of("file") {
        let file = fs::read_to_string(filename).expect("Something went wrong reading the file");
        let env = Environment::new();

        let (program, errors) = Parser::new(Lexer::new(&file)).parse_program();

        if errors.len() > 0 {
            print_errors(errors);
            return;
        }

        let evaluator = Evaluator::new(|out| print!("{}", out));

        match evaluator.eval(program, &env) {
            Err(error) => println!("ERROR: {}", error),
            _ => {}
        }
    } else {
        let stdin = io::stdin();
        let env = Environment::new();

        loop {
            let input = stdin.lock().lines().next().unwrap().unwrap();

            let (program, errors) = Parser::new(Lexer::new(&input)).parse_program();

            if errors.len() > 0 {
                print_errors(errors);
                continue;
            }

            let evaluator = Evaluator::new(|out| println!("{}", out));

            match evaluator.eval_repl(program, &env) {
                Err(error) => println!("ERROR: {}", error),
                _ => {}
            }
        }
    }
}
