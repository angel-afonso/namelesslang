///# Token
///The tokens are a representation of the identifiers found in the input code
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub line: u32,
    pub column: u32,
    pub token_type: TokenType,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    /// EOF
    EndOfFile,

    /// Represents an identifier, for example: `foo`, `bar`
    Ident(String),
    /// Represents an integer literal
    Int(String),
    /// Represents an float literal
    Float(String),
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
    /// Represents a `|` character
    VerticalBar,
    /// Represents a `[` character
    LBracket,
    /// Represents a `]` character
    RBracket,

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
    /// Represents a `+=` operator
    PlusAssign,
    /// Represents a `-=` operator
    MinusAssign,
    /// Represents a `*=` operator
    MultiplyAssign,
    /// Represents a `/=` operator
    DivideAssign,
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
pub fn look_ident(ident: &str) -> TokenType {
    match ident {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "return" => TokenType::Return,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "for" => TokenType::For,
        _ => TokenType::Ident(ident.to_string()),
    }
}
