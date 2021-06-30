use nameless::parser::parser::parse;
use nameless::parser::parser::GrammarParser;
use pest::Parser;
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();

    if rl.load_history("history.txt").is_err() {}

    loop {
        let readline = rl.readline("$> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                match parse(&line, nameless::parser::parser::Mode::REPL) {
                    Ok(program) => println!("{:?}", program),
                    Err(e) => print!("{}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                break;
            }
            Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
