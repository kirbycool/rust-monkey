use crate::parser::ast::Precedence;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal(String),

    // Idents + literals
    Ident(String),
    Int(String),
    True,
    False,
    If,
    Else,
    Return,
    Boolean(bool),

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::GreaterThan => write!(f, ">"),
            Token::LessThan => write!(f, "<"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Illegal(value) => write!(f, "{}", value),
            _ => write!(f, ""),
        }
    }
}

impl Token {
    pub fn precedence(&self) -> Precedence {
        match self {
            &Token::Plus => Precedence::Sum,
            &Token::Minus => Precedence::Sum,
            &Token::Asterisk => Precedence::Product,
            &Token::Slash => Precedence::Product,
            &Token::GreaterThan => Precedence::LessGreater,
            &Token::LessThan => Precedence::LessGreater,
            &Token::Equal => Precedence::Equals,
            &Token::NotEqual => Precedence::Equals,
            _ => Precedence::Lowest,
        }
    }
}
