use nameless::{Compiler, Lexer, Parser, VM};
use std::io::{stdin, stdout, Write};
use termion::input::TermRead;

fn main() {
    let stdout = stdout();
    let mut stdout = stdout.lock();
    let stdin = stdin();
    let mut stdin = stdin.lock();

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

            let mut compiler = Compiler::new();
            compiler.compile(program);

            let mut machine = VM::new(compiler.bytecode());

            if let Err(error) = machine.run() {
                stdout.write_all(format!("{}\n", error).as_bytes()).unwrap();
                continue;
            }

            stdout
                .write_all(format!("{}\n", machine.stack_top().unwrap()).as_bytes())
                .unwrap();
        } else {
            stdout.write_all(b"Error\n").unwrap();
        }
    }
}
