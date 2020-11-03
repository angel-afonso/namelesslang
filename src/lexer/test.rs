use super::Lexer;
use super::Token;

#[test]
fn test_next_token() {
    let input = r#"
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
    "#;

    let tests = vec![
        Token::Let,
        Token::Ident(String::from("five")),
        Token::Assign,
        Token::Int(String::from("5")),
        Token::Semicolon,
        Token::Let,
        Token::Ident(String::from("ten")),
        Token::Assign,
        Token::Int(String::from("10")),
        Token::Semicolon,
        Token::Function,
        Token::Ident(String::from("add")),
        Token::LParen,
        Token::Ident(String::from("x")),
        Token::Comma,
        Token::Ident(String::from("y")),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::Ident(String::from("x")),
        Token::Plus,
        Token::Ident(String::from("y")),
        Token::Semicolon,
        Token::RBrace,
        Token::Let,
        Token::Ident(String::from("str")),
        Token::Assign,
        Token::String(String::from("hello world")),
        Token::Semicolon,
        Token::If,
        Token::LParen,
        Token::Int(String::from("5")),
        Token::LowerThan,
        Token::Int(String::from("10")),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RBrace,
        Token::Else,
        Token::LBrace,
        Token::Return,
        Token::False,
        Token::RBrace,
    ];

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    println!("{:#?}", tokens);
    assert_eq!(tokens.len(), tests.len(), "wrong length");

    for (i, tt) in tests.iter().enumerate() {
        assert_eq!(&tokens[i], tt);
    }
}
