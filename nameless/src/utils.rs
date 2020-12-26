use super::parser::ParseError;

pub fn print_errors(errors: Vec<ParseError>) {
    for error in errors.iter() {
        println!("{}", error);
    }
}
