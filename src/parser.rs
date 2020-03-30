use crate::ast::{Expression, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;

pub struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut statements = Vec::new();
        while let Some(_) = self.lexer.peek() {
            statements.push(self.parse_statement().unwrap());
        }

        Program { statements }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        self.lexer.peek().and_then(|token| match token {
            Token::Let => self.parse_let(),
            _ => None,
        })
    }

    fn parse_let(&mut self) -> Option<Statement> {
        if self.lexer.next() != Some(Token::Let) {
            return None;
        }

        let name = match self.lexer.next() {
            Some(Token::Identifier(name)) => name,
            _ => return None,
        };

        if self.lexer.next() != Some(Token::Assign) {
            return None;
        }

        if self.lexer.peek() != Some(&Token::Semicolon) {
            self.lexer.next();
        }

        Some(Statement::Let {
            name,
            value: Expression::Int,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn assert_ast(input: String, expected_ast: Program) {
        let parser = Parser::new(Lexer::new(input));
        let ast = parser.parse();
        assert_eq!(ast, expected_ast)
    }

    #[test]
    fn test_let() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
      ";

        let expected = Program {
            statements: vec![
                Statement::Let {
                    name: String::from("x"),
                    value: Expression::Int,
                },
                Statement::Let {
                    name: String::from("y"),
                    value: Expression::Int,
                },
                Statement::Let {
                    name: String::from("foobar"),
                    value: Expression::Int,
                },
            ],
        };

        assert_ast(String::from(input), expected)
    }
}
