#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(String),
    EOF(String),

    // Identifiers + literals
    Identifier(String),
    Int(String),

    // Operators
    Assign,
    Plus,

    // Delimiters
    Comma,
    Semicolon,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,

    // Keywords
    Function(String),
    Let(String),
}
