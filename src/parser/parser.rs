use super::super::lexer::{Lexer, Token};
use super::ast::{Program, Statement};
use std::{iter::Peekable, vec::IntoIter};

pub struct Parser {
    token: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(lexer: &mut Lexer) -> Parser {
        Parser {
            token: lexer.tokenize().into_iter().peekable(),
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Vec::new();

        while let Some(stmt) = self.parse_statement() {
            program.push(stmt);
        }

        program
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.token.next() {
            // Some(Token::Let) => self.parse_let_statement(),
            _ => None,
        }
    }

    // fn parse_let_statement(&mut self) -> Option<Statement> {
    //     let name = match self.token.next() {
    //         Some(token) => match token {
    //             Token::Ident(lit) => lit,
    //             _ => return None,
    //         },
    //         None => return None,
    //     };

    //     // temporal
    //     loop {
    //         if let Some(tok) = self.token.next() {
    //             if tok == Token::Semicolon {
    //                 return Some(Statement::Let(, None));
    //             }
    //         }
    //     }
    // }
}
