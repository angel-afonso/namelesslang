use nameless::{Compiler, Lexer, Object, Parser, SymbolTable, GLOBALS_SIZE, VM};
use rustyline::error::ReadlineError;
use rustyline::Editor;

fn main() {
    let mut rl = Editor::<()>::new();

    if rl.load_history("history.txt").is_err() {}

    let mut constants: Vec<Object> = Vec::new();
    let mut globals: Vec<Object> = Vec::with_capacity(GLOBALS_SIZE);
    let mut symbol_table = SymbolTable::new();

    loop {
        let readline = rl.readline("$> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let (program, errors) = Parser::new(Lexer::new(&line)).parse_program();

                if !errors.is_empty() {
                    println!("Compilation error:");
                    for error in errors.iter() {
                        println!("{}", error);
                    }
                    continue;
                }

                let mut compiler =
                    Compiler::new().with_state(symbol_table.clone(), constants.clone());

                if let Err(err) = compiler.compile(program) {
                    println!("{}", err);
                    continue;
                }

                let mut machine = VM::new(compiler.bytecode()).with_global_store(globals.clone());

                if let Err(error) = machine.run() {
                    println!("{}", error);
                    continue;
                }

                println!("{}", machine.last_popped());

                constants = compiler.constants;
                symbol_table = compiler.symbol_table;
                globals = machine.globals;
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
