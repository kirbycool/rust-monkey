#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(String),

    // Identifiers + literals
    Identifier(String),
    Int(String),
    True,
    False,
    If,
    Else,
    Return,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    GreaterThan,
    LessThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Keywords
    Function,
    Let,
}
