use crate::ast::{Expression, Precedence, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;
use std::str;

pub struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Program, Vec<String>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while let Some(_) = self.lexer.peek() {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(error),
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(Program { statements })
    }

    //////
    /// Statement parsers
    //////
    fn parse_statement(&mut self) -> Result<Statement, String> {
        let result = match self.lexer.peek() {
            Some(&Token::Let) => self.parse_let(),
            Some(&Token::Return) => self.parse_return(),
            _ => self.parse_expression_statement(),
        };
        result.or_else(|error| {
            self.skip_statement();
            Err(error)
        })
    }

    fn parse_let(&mut self) -> Result<Statement, String> {
        self.lexer.next(); // Consume Let

        let name = match self.lexer.next() {
            Some(Token::Identifier(name)) => name,
            token => return Err(token_error(token, "Identifier")),
        };

        match self.lexer.next() {
            Some(Token::Assign) => (),
            token => return Err(token_error(token, "=")),
        };

        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Statement::Let(name, expr));

        self.lexer.next(); // Consume semicolon
        expression
    }

    fn parse_return(&mut self) -> Result<Statement, String> {
        self.lexer.next(); // Consume return

        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Statement::Return(expr));

        self.lexer.next(); // Consume semicolon
        expression
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Statement::Expression(expr));

        if let Some(&Token::Semicolon) = self.lexer.peek() {
            self.lexer.next();
        }
        expression
    }

    fn skip_statement(&mut self) -> () {
        while let Some(token) = self.lexer.next() {
            if token == Token::Semicolon {
                break;
            }
        }
    }

    //////
    /// Expression parsers
    //////
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, String> {
        let mut left = match self.lexer.peek() {
            Some(&Token::Identifier(_)) => self.parse_identifier(),
            Some(&Token::Int(_)) => self.parse_int(),
            Some(&Token::Bang) | Some(&Token::Minus) => self.parse_prefix(),
            _ => Err(String::from("Unrecognized expression")),
        }?;

        while let Some(token) = self.lexer.peek() {
            if token.precedence() < precedence {
                break;
            }
            left = match token {
                &Token::Plus
                | &Token::Minus
                | &Token::Asterisk
                | &Token::Slash
                | &Token::Equal
                | &Token::NotEqual
                | &Token::LessThan
                | &Token::GreaterThan => self.parse_infix(left)?,
                _ => left,
            }
        }

        Ok(left)
    }

    fn parse_identifier(&mut self) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(Token::Identifier(name)) => Ok(Expression::Identifier(name)),
            token => Err(token_error(token, "Identifier")),
        }
    }

    fn parse_int(&mut self) -> Result<Expression, String> {
        match self.lexer.next() {
            Some(Token::Int(value)) => match str::parse::<i32>(value.as_str()) {
                Ok(value) => Ok(Expression::Int(value)),
                Err(error) => Err(error.to_string()),
            },
            token => Err(token_error(token, "Identifier")),
        }
    }

    fn parse_prefix(&mut self) -> Result<Expression, String> {
        let operator = self.lexer.next().unwrap();
        let operand = self.parse_expression(Precedence::Prefix);
        operand.map(|expr| Expression::Prefix(operator, Box::new(expr)))
    }

    fn parse_infix(&mut self, left: Expression) -> Result<Expression, String> {
        let operator = self.lexer.next().unwrap();
        let right = self.parse_expression(operator.precedence());
        right.map(|expr| Expression::Infix(Box::new(left), operator, Box::new(expr)))
    }
}

fn token_error(actual: Option<Token>, expected: &str) -> String {
    match actual {
        Some(token) => format!(
            "Expected next token to be {}, got {:?} instead",
            expected, token
        ),
        None => format!(
            "Expected next token to be {}, got end of input instead",
            expected
        ),
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Program, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

    fn assert_result(input: String, result: Result<Program, Vec<String>>) {
        let mut parser = Parser::new(Lexer::new(input));
        assert_eq!(parser.parse(), result)
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
                Statement::Let(String::from("x"), Expression::Int(5)),
                Statement::Let(String::from("y"), Expression::Int(10)),
                Statement::Let(String::from("foobar"), Expression::Int(838383)),
            ],
        };

        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_return() {
        let input = "
return 5;
return 1;
        ";
        let expected = Program {
            statements: vec![
                Statement::Return(Expression::Int(5)),
                Statement::Return(Expression::Int(1)),
            ],
        };

        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_identifier() {
        let input = "foobar;";
        let expected = Program {
            statements: vec![Statement::Expression(Expression::Identifier(String::from(
                "foobar",
            )))],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_int() {
        let input = "5;";
        let expected = Program {
            statements: vec![Statement::Expression(Expression::Int(5))],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_prefix() {
        let input = "!5; -15;";
        let expected = Program {
            statements: vec![
                Statement::Expression(Expression::Prefix(
                    Token::Bang,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Prefix(
                    Token::Minus,
                    Box::new(Expression::Int(15)),
                )),
            ],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_infix() {
        let input = "\
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;";

        let expected = Program {
            statements: vec![
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Plus,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Minus,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Asterisk,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Slash,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::GreaterThan,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::LessThan,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::Equal,
                    Box::new(Expression::Int(5)),
                )),
                Statement::Expression(Expression::Infix(
                    Box::new(Expression::Int(5)),
                    Token::NotEqual,
                    Box::new(Expression::Int(5)),
                )),
            ],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn test_precedence() {
        let cases = vec![("-a * b", "((-a) * b)")];
        for (input, output) in cases.iter() {
            let lexer = Lexer::new(String::from(*input));
            let program = Parser::new(lexer).parse().unwrap();
            assert_eq!(program.to_string(), String::from(*output))
        }
    }

    #[test]
    fn test_errors() {
        let input = "
let x 1;
let 4;
      ";

        let errors = vec![
            String::from("Expected next token to be =, got Int(\"1\") instead"),
            String::from("Expected next token to be Identifier, got Int(\"4\") instead"),
        ];

        assert_result(String::from(input), Err(errors))
    }
}
