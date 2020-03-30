use crate::token::Token;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { name: String, value: Expression },
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Int,
}
