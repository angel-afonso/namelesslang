use nameless::{Compiler, Lexer, Object, Parser, SymbolTable, GLOBALS_SIZE, VM};
use std::io::{stdin, stdout, Write};
use termion::input::TermRead;

fn main() {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    let stdin = stdin();
    let mut stdin = stdin.lock();

    let mut constants: Vec<Object> = Vec::new();
    let mut globals: Vec<Object> = Vec::with_capacity(GLOBALS_SIZE);
    let mut symbol_table = SymbolTable::new();

    loop {
        stdout.write_all(b">> ").unwrap();
        stdout.flush().unwrap();

        let pass = stdin.read_line();

        if let Ok(Some(pass)) = pass {
            let (program, errors) = Parser::new(Lexer::new(&pass)).parse_program();

            if !errors.is_empty() {
                stdout.write_all(b"Compilation error: ").unwrap();
                for error in errors.iter() {
                    stdout.write_all(format!("{}\n", error).as_bytes()).unwrap();
                }
                continue;
            }

            let mut compiler = Compiler::new().with_state(symbol_table.clone(), constants.clone());
            compiler.compile(program);

            let mut machine = VM::new(compiler.bytecode()).with_global_store(globals.clone());

            if let Err(error) = machine.run() {
                stdout.write_all(format!("{}\n", error).as_bytes()).unwrap();
                continue;
            }

            stdout
                .write_all(format!("{}\n", machine.last_popped()).as_bytes())
                .unwrap();

            constants = compiler.constants;
            symbol_table = compiler.symbol_table;
            globals = machine.globals;
        } else {
            stdout.write_all(b"Error\n").unwrap();
        }
    }
}
