use clap::{App, Arg, SubCommand};
use nameless::builtin_fns;
use nameless::parser::parser::parse;
use nameless::Compiler;
use nameless::Object;
use nameless::Stream;
use nameless::SymbolTable;
use nameless::GLOBALS_SIZE;
use nameless::VM;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::fs;
use std::io::{self, BufRead};

fn main() {
    let matches = App::new("Nameless")
        .version("1.0")
        .author("Ángel Afonso <angelafonso60@gmail.com>")
        .about("Nameless programming language")
        .subcommands(vec![
            SubCommand::with_name("compile")
                .about("Compile a nameless file")
                .arg(Arg::with_name("file").required(true)),
            SubCommand::with_name("run")
                .about("Build and run a nameless file")
                .arg(Arg::with_name("file").required(true)),
        ])
        .get_matches();

    match matches.subcommand() {
        ("compile", Some(compile)) => {
            let file =
                fs::read_to_string(compile.value_of("file").unwrap()).expect("Error reading file");

            match parse(&file, nameless::parser::parser::Mode::Program) {
                Ok(program) => {
                    let mut compiler = Compiler::new();
                    if let Err(err) = compiler.compile(program) {
                        println!("{}", err);
                    }
                }
                Err(e) => print!("{}", e),
            }
        }
        ("run", Some(run)) => {
            let file =
                fs::read_to_string(run.value_of("file").unwrap()).expect("Error reading file");

            match parse(&file, nameless::parser::parser::Mode::Program) {
                Ok(program) => {
                    let mut compiler = Compiler::new();
                    if let Err(err) = compiler.compile(program) {
                        println!("{}", err);
                    }

                    let stream = Stream::new(output, input);

                    let mut machine = VM::new(compiler.bytecode(), stream);

                    if let Err(error) = machine.run() {
                        println!("{}", error);
                    }
                }
                Err(e) => print!("{}", e),
            }
        }
        _ => {
            repl();
        }
    }
}

fn repl() {
    let mut rl = Editor::<()>::new();

    if rl.load_history("history.txt").is_err() {}

    let mut constants: Vec<Object> = Vec::new();
    let mut globals: Vec<Object> = Vec::with_capacity(GLOBALS_SIZE);
    let mut symbol_table = SymbolTable::new();

    for (index, &name) in builtin_fns().iter().enumerate() {
        symbol_table.define_built_in(name, index);
    }

    loop {
        let readline = rl.readline("$> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());

                match parse(&line, nameless::parser::parser::Mode::REPL) {
                    Ok(program) => {
                        let mut compiler =
                            Compiler::new().with_state(symbol_table.clone(), constants.clone());

                        if let Err(err) = compiler.compile(program) {
                            println!("{}", err);
                            continue;
                        }

                        let stream = Stream::new(output, input);

                        let mut machine =
                            VM::new(compiler.bytecode(), stream).with_global_store(globals.clone());

                        if let Err(error) = machine.run() {
                            println!("{}", error);
                            continue;
                        }

                        constants = compiler.constants;
                        symbol_table = compiler.symbol_table;
                        globals = machine.globals;
                    }
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
