use super::lexer::Token;
use std::{iter::Peekable, vec::IntoIter};

pub struct Parser {
    token: Peekable<IntoIter<Token>>,
}
