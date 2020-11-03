///# Token
///the tokens are a representation of the identifiers found in the input code
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    /// EOF
    EndOfFile,

    /// Represents an identifier, for example: `foo`, `bar`
    Ident(String),
    /// Represents an integer literal
    Int(String),
    /// Represents a string literal
    String(String),
    /// Represents a char literal
    Char(String),

    // Delimeters
    /// Represents a `,` character
    Comma,
    /// Represents a `;` character
    Semicolon,
    /// Represents a `(` character
    LParen,
    /// Represents a `)` character
    RParen,
    /// Represents a `{` character
    LBrace,
    /// Represents a `}` character
    RBrace,

    // Keywords
    /// Represents a `function` keyword
    Function,
    /// Represents a `let` keyword
    Let,
    /// Represents a `true` keyword
    True,
    /// Represents a `false` keyword
    False,
    /// Represents a `if` keyword
    If,
    /// Represents a `else` keyword
    Else,
    /// Represents a `return` keyword
    Return,
    /// Represents a `for` keyword
    For,

    // Operators
    /// Represents a `=` operator
    Assign,
    /// Represents a `+` operator
    Plus,
    /// Represents a `++` operator
    Increment,
    /// Represents a `-` operator
    Minus,
    /// Represents a `--` operator
    Decrement,
    /// Represents a `!` operator
    Bang,
    /// Represents a `*` operator
    Asterisk,
    /// Represents a `/` operator
    Slash,
    /// Represents a `<` operator
    LowerThan,
    /// Represents a `>` operator
    GreaterThan,
    /// Represents a `==` operator
    Equal,
    /// Represents a `!=` operator
    NotEqual,
    /// Represents a `&&` operator
    And,
    /// Represents a `||` operator
    Or,
}

/// look if `ident` is a keyword
/// ### Example
/// ```
/// let ident = "let";
/// look_ident(ident) // returns Token::Let;
/// ```
pub fn look_ident(ident: &str) -> Token {
    match ident {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "return" => Token::Return,
        "if" => Token::If,
        "else" => Token::Else,
        "for" => Token::For,
        _ => Token::Ident(ident.to_string()),
    }
}