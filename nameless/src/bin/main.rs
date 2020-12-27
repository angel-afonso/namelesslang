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

        let evaluator = Evaluator::new(output, input);

        match evaluator.eval(program, &env) {
            Err(error) => println!("ERROR: {}", error),
            _ => {}
        }
    } else {
        let stdin = io::stdin();
        let env = Environment::new();

        loop {
            let line = stdin.lock().lines().next().unwrap().unwrap();

            let (program, errors) = Parser::new(Lexer::new(&line)).parse_program();

            if errors.len() > 0 {
                print_errors(errors);
                continue;
            }

            let evaluator = Evaluator::new(output, input);

            match evaluator.eval_repl(program, &env) {
                Err(error) => println!("ERROR: {}", error),
                _ => {}
            }
        }
    }
}

fn output(out: String) {
    print!("{}", out);
}

fn input() -> String {
    let stdin = io::stdin();

    let mut lines = stdin.lock().lines();
    match lines.next() {
        Some(line) => match line {
            Ok(line) => line,
            _ => String::new(),
        },
        _ => String::new(),
    }
}
