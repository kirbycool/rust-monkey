use crate::parser::ast::Precedence;
use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
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
    Str(String),

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
    Colon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

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

pub struct BindingPower {
    pub left: Precedence,
    pub right: Precedence,
}

impl Token {
    pub fn precedence(&self) -> BindingPower {
        match self {
            &Token::Plus => BindingPower {
                left: Precedence::Sum,
                right: Precedence::Sum,
            },
            &Token::Minus => BindingPower {
                left: Precedence::Sum,
                right: Precedence::Sum,
            },
            &Token::Asterisk => BindingPower {
                left: Precedence::Product,
                right: Precedence::Product,
            },
            &Token::Slash => BindingPower {
                left: Precedence::Product,
                right: Precedence::Product,
            },
            &Token::GreaterThan => BindingPower {
                left: Precedence::LessGreater,
                right: Precedence::LessGreater,
            },
            &Token::LessThan => BindingPower {
                left: Precedence::LessGreater,
                right: Precedence::LessGreater,
            },
            &Token::Equal => BindingPower {
                left: Precedence::Equals,
                right: Precedence::Equals,
            },
            &Token::NotEqual => BindingPower {
                left: Precedence::Equals,
                right: Precedence::Equals,
            },
            &Token::LBracket => BindingPower {
                left: Precedence::Index,
                right: Precedence::Lowest,
            },
            _ => BindingPower {
                left: Precedence::Lowest,
                right: Precedence::Lowest,
            },
        }
    }
}
