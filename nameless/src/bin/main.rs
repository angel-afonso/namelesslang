use clap::{App, Arg};
use nameless::{eval, print_errors, start_repl, Environment, Lexer, Parser};
use std::fs;

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

        match eval(program, &env) {
            Err(error) => println!("ERROR: {}", error),
            _ => {}
        }
    } else {
        start_repl()
    }
}
