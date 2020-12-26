use clap::{App, Arg};
use nameless::{print_errors, Environment, Evaluator, Lexer, Object, Parser};
use std::fs;
use std::io::{self, BufRead};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
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

        let (tx, rx): (Sender<Object>, Receiver<Object>) = channel();
        let evaluator = Evaluator::new(tx);

        thread::spawn(move || {
            while let Ok(out) = rx.recv() {
                println!("{}", out);
            }
        });

        match evaluator.eval(program, &env) {
            Err(error) => println!("ERROR: {}", error),
            out => print!("{:?}", out),
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

            let (tx, rx): (Sender<Object>, Receiver<Object>) = channel();

            thread::spawn(move || {
                while let Ok(out) = rx.recv() {
                    println!("{}", out);
                }
            });

            let evaluator = Evaluator::new(tx);

            match evaluator.eval_repl(program, &env) {
                Err(error) => println!("ERROR: {}", error),
                _ => {}
            }
        }
    }
}
