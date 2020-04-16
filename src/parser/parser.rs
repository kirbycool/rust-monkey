use crate::parser::ast::{Expr, Precedence, Program, Stmt};
use crate::parser::token::Token;
use crate::parser::Lexer;
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
    /// Stmt parsers
    //////
    fn parse_statement(&mut self) -> Result<Stmt, String> {
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

    fn parse_let(&mut self) -> Result<Stmt, String> {
        self.lexer.next(); // Consume Let

        let name = match self.lexer.next() {
            Some(Token::Ident(name)) => name,
            token => return Err(token_error(token.as_ref(), "Ident")),
        };

        match self.lexer.next() {
            Some(Token::Assign) => (),
            token => return Err(token_error(token.as_ref(), "=")),
        };

        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Stmt::Let(name, expr));

        self.lexer.next(); // Consume semicolon
        expression
    }

    fn parse_return(&mut self) -> Result<Stmt, String> {
        self.lexer.next(); // Consume return

        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Stmt::Return(expr));

        self.lexer.next(); // Consume semicolon
        expression
    }

    fn parse_expression_statement(&mut self) -> Result<Stmt, String> {
        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Stmt::Expr(expr));

        if let Some(&Token::Semicolon) = self.lexer.peek() {
            self.lexer.next();
        }
        expression
    }

    fn parse_block_statement(&mut self) -> Result<Stmt, String> {
        let mut statements = Vec::new();
        while let Some(token) = self.lexer.peek() {
            if token == &Token::RightBrace {
                break;
            }

            statements.push(self.parse_statement()?);
        }

        Ok(Stmt::BlockStmt(statements))
    }

    fn skip_statement(&mut self) -> () {
        while let Some(token) = self.lexer.next() {
            if token == Token::Semicolon {
                break;
            }
        }
    }

    //////
    /// Expr parsers
    //////
    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expr, String> {
        let mut left = match self.lexer.peek() {
            Some(&Token::Ident(_)) => self.parse_identifier(),
            Some(&Token::Int(_)) => self.parse_int(),
            Some(&Token::True) | Some(&Token::False) => self.parse_boolean(),
            Some(&Token::String(_)) => self.parse_string(),
            Some(&Token::Bang) | Some(&Token::Minus) => self.parse_prefix(),
            Some(&Token::LeftParen) => self.parse_grouped_expression(),
            Some(&Token::RightParen) => {
                Err(String::from("Got RightParen without a matching LeftParen"))
            }
            Some(&Token::If) => self.parse_if(),
            Some(&Token::Function) => self.parse_function_literal(),
            token => Err(format!(
                "Illegal token \"{}\"",
                token.map(|t| t.to_string()).unwrap_or("EOF".to_string())
            )),
        }?;

        while let Some(token) = self.lexer.peek() {
            if token.precedence() <= precedence {
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

    fn parse_identifier(&mut self) -> Result<Expr, String> {
        let identifier = match self.lexer.next() {
            Some(Token::Ident(name)) => Expr::Ident(name),
            token => return Err(token_error(token.as_ref(), "Ident")),
        };

        match self.lexer.peek() {
            Some(&Token::LeftParen) => self.lexer.next(),
            _ => return Ok(identifier),
        };

        let args = self.parse_call_arguments()?;

        match self.lexer.peek() {
            Some(&Token::RightParen) => self.lexer.next(),
            token => return Err(token_error(token, "RightParen")),
        };

        Ok(Expr::Call {
            func: Box::new(identifier),
            args,
        })
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expr>, String> {
        let mut args = Vec::new();
        while let Some(token) = self.lexer.peek() {
            if token == &Token::RightParen {
                return Ok(args);
            }

            let expr = self.parse_expression(Precedence::Lowest)?;
            args.push(expr);

            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            }
        }

        Ok(args)
    }

    fn parse_int(&mut self) -> Result<Expr, String> {
        match self.lexer.next() {
            Some(Token::Int(value)) => match str::parse::<i64>(value.as_str()) {
                Ok(value) => Ok(Expr::Int(value)),
                Err(error) => Err(error.to_string()),
            },
            token => Err(token_error(token.as_ref(), "Int")),
        }
    }

    fn parse_boolean(&mut self) -> Result<Expr, String> {
        match self.lexer.next() {
            Some(Token::True) => Ok(Expr::Bool(true)),
            Some(Token::False) => Ok(Expr::Bool(false)),
            token => Err(token_error(token.as_ref(), "True or False")),
        }
    }

    fn parse_string(&mut self) -> Result<Expr, String> {
        match self.lexer.next() {
            Some(Token::String(value)) => Ok(Expr::String(value)),
            token => Err(token_error(token.as_ref(), "String")),
        }
    }

    fn parse_prefix(&mut self) -> Result<Expr, String> {
        let op = self.lexer.next().unwrap();
        let right = self.parse_expression(Precedence::Prefix);
        right.map(|expr| Expr::Prefix {
            op,
            right: Box::new(expr),
        })
    }

    fn parse_infix(&mut self, left: Expr) -> Result<Expr, String> {
        let op = self.lexer.next().unwrap();
        let right = self.parse_expression(op.precedence());
        right.map(|expr| Expr::Infix {
            left: Box::new(left),
            op,
            right: Box::new(expr),
        })
    }

    fn parse_grouped_expression(&mut self) -> Result<Expr, String> {
        self.lexer.next(); // Consume the left paren

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.next_if(|t| t == &Token::RightParen, "RightParen")?;

        Ok(expr)
    }

    fn parse_if(&mut self) -> Result<Expr, String> {
        self.lexer.next(); // Consume the if
        self.next_if(|t| t == &Token::LeftParen, "LeftParen")?;

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.next_if(|t| t == &Token::RightParen, "RightParen")?;
        self.next_if(|t| t == &Token::LeftBrace, "LeftBrace")?;

        let consequence = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RightBrace, "RightBrace")?;

        let alternative = match self.lexer.peek() {
            Some(&Token::Else) => Some(Box::new(self.parse_else()?)),
            _ => None,
        };

        Ok(Expr::If {
            test: Box::new(condition),
            consequent: Box::new(consequence),
            alternative,
        })
    }

    fn parse_else(&mut self) -> Result<Stmt, String> {
        self.lexer.next(); // Consume the else
        self.next_if(|t| t == &Token::LeftBrace, "LeftBrace")?;

        let alt = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RightBrace, "RightBrace")?;

        Ok(alt)
    }

    fn parse_function_literal(&mut self) -> Result<Expr, String> {
        self.lexer.next(); // Consume the fn
        self.next_if(|t| t == &Token::LeftParen, "LeftParen")?;

        let params = self.parse_function_params()?;

        self.next_if(|t| t == &Token::RightParen, "RightParen")?;
        self.next_if(|t| t == &Token::LeftBrace, "LeftBrace")?;

        let body = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RightBrace, "RightBrace")?;

        Ok(Expr::FunctionLiteral {
            params,
            body: Box::new(body),
        })
    }

    fn parse_function_params(&mut self) -> Result<Vec<Expr>, String> {
        let mut params = Vec::new();
        while let Some(&Token::Ident(_)) = self.lexer.peek() {
            match self.lexer.next() {
                Some(Token::Ident(value)) => params.push(Expr::Ident(value)),
                token => return Err(token_error(token.as_ref(), "Ident")),
            }

            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            }
        }

        Ok(params)
    }

    fn next_if<F>(&mut self, checker: F, expected: &str) -> Result<(), String>
    where
        F: FnOnce(&Token) -> bool,
    {
        let peek = self.lexer.peek();
        match peek.filter(|token| checker(*token)) {
            None => Err(token_error(peek, expected)),
            _ => {
                self.lexer.next();
                Ok(())
            }
        }
    }
}

fn token_error(actual: Option<&Token>, expected: &str) -> String {
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
    use crate::parser::ast::{Expr, Program, Stmt};
    use crate::parser::token::Token;
    use crate::parser::Lexer;
    use crate::parser::Parser;

    fn assert_result(input: String, result: Result<Program, Vec<String>>) {
        let mut parser = Parser::new(Lexer::new(input));
        assert_eq!(parser.parse(), result)
    }

    #[test]
    fn let_statement() {
        let cases = [
            ("let x = 5", Stmt::Let("x".to_string(), Expr::Int(5))),
            ("let y = true", Stmt::Let("y".to_string(), Expr::Bool(true))),
            (
                "let foobar = y",
                Stmt::Let("foobar".to_string(), Expr::Ident("y".to_string())),
            ),
        ];

        for (input, stmt) in cases.iter().cloned() {
            let expected = Program {
                statements: vec![stmt],
            };

            assert_result(input.to_string(), Ok(expected))
        }
    }

    #[test]
    fn return_statement() {
        let cases = [
            ("return 5", Stmt::Return(Expr::Int(5))),
            ("return true", Stmt::Return(Expr::Bool(true))),
            (
                "return x + y",
                Stmt::Return(Expr::Infix {
                    left: Box::new(Expr::Ident("x".to_string())),
                    op: Token::Plus,
                    right: Box::new(Expr::Ident("y".to_string())),
                }),
            ),
        ];
        for (input, stmt) in cases.iter().cloned() {
            let expected = Program {
                statements: vec![stmt],
            };

            assert_result(input.to_string(), Ok(expected))
        }
    }

    #[test]
    fn identifier() {
        let input = "foobar;";
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::Ident(String::from("foobar")))],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn int() {
        let input = "5;";
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::Int(5))],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn booleans() {
        let input = "true; false;";
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::Bool(true)), Stmt::Expr(Expr::Bool(false))],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn string() {
        let input = "\"foo\"; \"bar\";";
        let expected = Program {
            statements: vec![
                Stmt::Expr(Expr::String("foo".to_string())),
                Stmt::Expr(Expr::String("bar".to_string())),
            ],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn prefix() {
        let input = "!5; -15;";
        let expected = Program {
            statements: vec![
                Stmt::Expr(Expr::Prefix {
                    op: Token::Bang,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Prefix {
                    op: Token::Minus,
                    right: Box::new(Expr::Int(15)),
                }),
            ],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn infix() {
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
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::Plus,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::Minus,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::Asterisk,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::Slash,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::GreaterThan,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::LessThan,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::Equal,
                    right: Box::new(Expr::Int(5)),
                }),
                Stmt::Expr(Expr::Infix {
                    left: Box::new(Expr::Int(5)),
                    op: Token::NotEqual,
                    right: Box::new(Expr::Int(5)),
                }),
            ],
        };
        assert_result(String::from(input), Ok(expected))
    }

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";
        let condition = Expr::Infix {
            left: Box::new(Expr::Ident(String::from("x"))),
            op: Token::LessThan,
            right: Box::new(Expr::Ident(String::from("y"))),
        };
        let consequence = Stmt::BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("x")))]);
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::If {
                test: Box::new(condition),
                consequent: Box::new(consequence),
                alternative: None,
            })],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "\
if ((x < y)) {
    x;
};";
        assert_eq!(program.to_string(), String::from(expected_string))
    }

    #[test]
    fn if_else() {
        let input = "if (x < y) { x } else { y }";
        let condition = Expr::Infix {
            left: Box::new(Expr::Ident(String::from("x"))),
            op: Token::LessThan,
            right: Box::new(Expr::Ident(String::from("y"))),
        };
        let consequence = Stmt::BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("x")))]);
        let alternative = Stmt::BlockStmt(vec![Stmt::Expr(Expr::Ident(String::from("y")))]);
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::If {
                test: Box::new(condition),
                consequent: Box::new(consequence),
                alternative: Some(Box::new(alternative)),
            })],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "\
if ((x < y)) {
    x;
} else {
    y;
};";
        assert_eq!(program.to_string(), String::from(expected_string))
    }

    #[test]
    fn function_literal() {
        let input = "fn(x, y) { x + y; }";
        let params = vec![Expr::Ident("x".to_string()), Expr::Ident("y".to_string())];
        let body = Stmt::BlockStmt(vec![Stmt::Expr(Expr::Infix {
            left: Box::new(Expr::Ident(String::from("x"))),
            op: Token::Plus,
            right: Box::new(Expr::Ident(String::from("y"))),
        })]);
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::FunctionLiteral {
                params,
                body: Box::new(body),
            })],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "\
fn (x, y) {
    (x + y);
};";
        assert_eq!(program.to_string(), expected_string.to_string())
    }

    #[test]
    fn call() {
        let input = "add(1, 2 * 3, 4 + 5);";
        let arg1 = Expr::Int(1);
        let arg2 = Expr::Infix {
            left: Box::new(Expr::Int(2)),
            op: Token::Asterisk,
            right: Box::new(Expr::Int(3)),
        };
        let arg3 = Expr::Infix {
            left: Box::new(Expr::Int(4)),
            op: Token::Plus,
            right: Box::new(Expr::Int(5)),
        };
        let expected = Program {
            statements: vec![Stmt::Expr(Expr::Call {
                func: Box::new(Expr::Ident("add".to_string())),
                args: vec![arg1, arg2, arg3],
            })],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "add(1, (2 * 3), (4 + 5));";
        assert_eq!(program.to_string(), expected_string.to_string())
    }

    #[test]
    fn precedence() {
        let cases = vec![
            ("-a * b", "((-a) * b);"),
            ("!-a", "(!(-a));"),
            ("a + b + c", "((a + b) + c);"),
            ("a + b - c", "((a + b) - c);"),
            ("a * b * c", "((a * b) * c);"),
            ("a * b / c", "((a * b) / c);"),
            ("a + b / c", "(a + (b / c));"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f);"),
            ("3 + 4; -5 * 5", "(3 + 4);\n((-5) * 5);"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));",
            ),
            ("true", "true;"),
            ("false", "false;"),
            ("3 > 5 == false", "((3 > 5) == false);"),
            ("3 < 5 == true", "((3 < 5) == true);"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2", "((5 + 5) * 2);"),
            ("2 / (5 + 5)", "(2 / (5 + 5));"),
            ("-(5 + 5)", "(-(5 + 5));"),
            ("!(true == true)", "(!(true == true));"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d);"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));",
            ),
            (
                "add(a + b + c * d / f + g);",
                "add((((a + b) + ((c * d) / f)) + g));",
            ),
        ];

        for (input, output) in cases.iter() {
            let lexer = Lexer::new(String::from(*input));
            let program = Parser::new(lexer).parse().unwrap();
            assert_eq!(program.to_string(), String::from(*output))
        }
    }

    #[test]
    fn errors() {
        let input = "
let x 1;
let 4;
      ";

        let errors = vec![
            "Expected next token to be =, got Int(\"1\") instead".to_string(),
            "Expected next token to be Ident, got Int(\"4\") instead".to_string(),
        ];

        assert_result(input.to_string(), Err(errors))
    }
}
