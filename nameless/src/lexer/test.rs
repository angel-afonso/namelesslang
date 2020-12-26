use super::Lexer;
use super::{Token, TokenType};

#[test]
fn test_next_token() {
    let input = r#"
// this is a comment
let five = 5;
let ten = 10;

fn add(x, y) {
    return x + y;
}

let str = "hello world";

if (5 < 10) {
    return true;
} else {
    return false
}

let six = five++;

!-/*5;
10--;
    "#;

    let tests = vec![
        Token {
            token_type: TokenType::Let,
            line: 3,
            column: 1,
        },
        Token {
            token_type: TokenType::Ident(String::from("five")),
            line: 3,
            column: 5,
        },
        Token {
            token_type: TokenType::Assign,
            line: 3,
            column: 10,
        },
        Token {
            token_type: TokenType::Int(String::from("5")),
            line: 3,
            column: 12,
        },
        Token {
            token_type: TokenType::Semicolon,
            line: 3,
            column: 13,
        },
        Token {
            token_type: TokenType::Let,
            line: 4,
            column: 1,
        },
        Token {
            token_type: TokenType::Ident(String::from("ten")),
            line: 4,
            column: 5,
        },
        Token {
            token_type: TokenType::Assign,
            line: 4,
            column: 9,
        },
        Token {
            token_type: TokenType::Int(String::from("10")),
            line: 4,
            column: 11,
        },
        Token {
            token_type: TokenType::Semicolon,
            line: 4,
            column: 13,
        },
        Token {
            token_type: TokenType::Function,
            line: 6,
            column: 1,
        },
        Token {
            token_type: TokenType::Ident(String::from("add")),
            line: 6,
            column: 4,
        },
        Token {
            token_type: TokenType::LParen,
            line: 6,
            column: 7,
        },
        Token {
            token_type: TokenType::Ident(String::from("x")),
            line: 6,
            column: 8,
        },
        Token {
            token_type: TokenType::Comma,
            line: 6,
            column: 9,
        },
        Token {
            token_type: TokenType::Ident(String::from("y")),
            line: 6,
            column: 11,
        },
        Token {
            token_type: TokenType::RParen,
            line: 6,
            column: 12,
        },
        Token {
            token_type: TokenType::LBrace,
            line: 6,
            column: 14,
        },
        Token {
            token_type: TokenType::Return,
            line: 7,
            column: 5,
        },
        Token {
            token_type: TokenType::Ident(String::from("x")),
            line: 7,
            column: 12,
        },
        Token {
            token_type: TokenType::Plus,
            line: 7,
            column: 14,
        },
        Token {
            token_type: TokenType::Ident(String::from("y")),
            line: 7,
            column: 16,
        },
        Token {
            token_type: TokenType::Semicolon,
            line: 7,
            column: 17,
        },
        Token {
            token_type: TokenType::RBrace,
            line: 8,
            column: 1,
        },
        // TokenType::Let,
        // TokenType::Ident(String::from("str")),
        // TokenType::Assign,
        // TokenType::String(String::from("hello world")),
        // TokenType::Semicolon,
        // TokenType::If,
        // TokenType::LParen,
        // TokenType::Int(String::from("5")),
        // TokenType::LowerThan,
        // TokenType::Int(String::from("10")),
        // TokenType::RParen,
        // TokenType::LBrace,
        // TokenType::Return,
        // TokenType::True,
        // TokenType::Semicolon,
        // TokenType::RBrace,
        // TokenType::Else,
        // TokenType::LBrace,
        // TokenType::Return,
        // TokenType::False,
        // TokenType::RBrace,
        // TokenType::Let,
        // TokenType::Ident(String::from("six")),
        // TokenType::Assign,
        // TokenType::Ident(String::from("five")),
        // TokenType::Increment,
        // TokenType::Semicolon,
        // TokenType::Bang,
        // TokenType::Minus,
        // TokenType::Slash,
        // TokenType::Asterisk,
        // TokenType::Int(String::from("5")),
        // TokenType::Semicolon,
        // TokenType::Int(String::from("10")),
        // TokenType::Decrement,
        // TokenType::Semicolon,
    ];

    let mut lexer = Lexer::new(input);

    for tt in tests.iter() {
        let token = lexer.next_token();
        assert_eq!(&token, tt);
    }
}
