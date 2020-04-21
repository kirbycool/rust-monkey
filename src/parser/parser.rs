use crate::parser::ast::{Expr, Precedence, Program, Stmt};
use crate::parser::token::Token;
use crate::parser::Lexer;
use std::iter::Peekable;
use std::str;

type ParseResult<T> = Result<T, String>;

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
    fn parse_statement(&mut self) -> ParseResult<Stmt> {
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

    fn parse_let(&mut self) -> ParseResult<Stmt> {
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

    fn parse_return(&mut self) -> ParseResult<Stmt> {
        self.lexer.next(); // Consume return

        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Stmt::Return(expr));

        self.lexer.next(); // Consume semicolon
        expression
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Stmt> {
        let expression = self
            .parse_expression(Precedence::Lowest)
            .map(|expr| Stmt::Expr(expr));

        if let Some(&Token::Semicolon) = self.lexer.peek() {
            self.lexer.next();
        }
        expression
    }

    fn parse_block_statement(&mut self) -> ParseResult<Stmt> {
        let mut statements = Vec::new();
        while let Some(token) = self.lexer.peek() {
            if token == &Token::RBrace {
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
    fn parse_expression(&mut self, precedence: Precedence) -> ParseResult<Expr> {
        let mut left = match self.lexer.peek() {
            Some(&Token::Ident(_)) => self.parse_identifier(),
            Some(&Token::Int(_)) => self.parse_int(),
            Some(&Token::True) | Some(&Token::False) => self.parse_boolean(),
            Some(&Token::Str(_)) => self.parse_string(),
            Some(&Token::Bang) | Some(&Token::Minus) => self.parse_prefix(),
            Some(&Token::LParen) => self.parse_grouped_expression(),
            Some(&Token::RParen) => Err(String::from("Got RParen without a matching LParen")),
            Some(&Token::LBracket) => self.parse_array_literal(),
            Some(&Token::RBracket) => Err(String::from("Got RBracket without a matching LBracket")),
            Some(&Token::LBrace) => self.parse_hash_literal(),
            Some(&Token::RBrace) => Err(String::from("Got RBrace without a matching LBrace")),
            Some(&Token::If) => self.parse_if(),
            Some(&Token::Function) => self.parse_function_literal(),
            token => Err(format!(
                "Illegal token \"{}\"",
                token.map(|t| t.to_string()).unwrap_or("EOF".to_string())
            )),
        }?;

        while let Some(token) = self.lexer.peek() {
            if token.precedence().left <= precedence {
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
                &Token::LBracket => self.parse_transfix(left)?,
                _ => left,
            }
        }

        Ok(left)
    }

    fn parse_identifier(&mut self) -> ParseResult<Expr> {
        let identifier = match self.lexer.next() {
            Some(Token::Ident(name)) => Expr::Ident(name),
            token => return Err(token_error(token.as_ref(), "Ident")),
        };

        match self.lexer.peek() {
            Some(&Token::LParen) => self.lexer.next(),
            _ => return Ok(identifier),
        };

        let args = self.parse_call_arguments()?;

        match self.lexer.peek() {
            Some(&Token::RParen) => self.lexer.next(),
            token => return Err(token_error(token, "RParen")),
        };

        Ok(Expr::Call {
            func: Box::new(identifier),
            args,
        })
    }

    fn parse_call_arguments(&mut self) -> ParseResult<Vec<Expr>> {
        let mut args = Vec::new();
        while let Some(token) = self.lexer.peek() {
            if token == &Token::RParen {
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

    fn parse_int(&mut self) -> ParseResult<Expr> {
        match self.lexer.next() {
            Some(Token::Int(value)) => match str::parse::<i64>(value.as_str()) {
                Ok(value) => Ok(Expr::Int(value)),
                Err(error) => Err(error.to_string()),
            },
            token => Err(token_error(token.as_ref(), "Int")),
        }
    }

    fn parse_boolean(&mut self) -> ParseResult<Expr> {
        match self.lexer.next() {
            Some(Token::True) => Ok(Expr::Bool(true)),
            Some(Token::False) => Ok(Expr::Bool(false)),
            token => Err(token_error(token.as_ref(), "True or False")),
        }
    }

    fn parse_string(&mut self) -> ParseResult<Expr> {
        match self.lexer.next() {
            Some(Token::Str(value)) => Ok(Expr::String(value)),
            token => Err(token_error(token.as_ref(), "String")),
        }
    }

    fn parse_prefix(&mut self) -> ParseResult<Expr> {
        let op = self.lexer.next().unwrap();
        let right = self.parse_expression(Precedence::Prefix);
        right.map(|expr| Expr::Prefix {
            op,
            right: Box::new(expr),
        })
    }

    fn parse_infix(&mut self, left: Expr) -> ParseResult<Expr> {
        let op = self.lexer.next().unwrap();
        let right = self.parse_expression(op.precedence().right);
        right.map(|expr| Expr::Infix {
            left: Box::new(left),
            op,
            right: Box::new(expr),
        })
    }

    fn parse_transfix(&mut self, left: Expr) -> ParseResult<Expr> {
        let op = self.lexer.next().unwrap();
        let right = self.parse_expression(op.precedence().right)?;

        match op {
            Token::LBracket => {
                self.next_if(|t| t == &Token::RBracket, "RBracket")?;
                Ok(Expr::Index {
                    left: Box::new(left),
                    index: Box::new(right),
                })
            }
            _ => Err(format!(
                "Unrecognized transfix operator '{}'",
                op.to_string()
            )),
        }
    }

    fn parse_grouped_expression(&mut self) -> ParseResult<Expr> {
        self.lexer.next(); // Consume the left paren

        let expr = self.parse_expression(Precedence::Lowest)?;

        self.next_if(|t| t == &Token::RParen, "RParen")?;

        Ok(expr)
    }

    fn parse_array_literal(&mut self) -> ParseResult<Expr> {
        self.lexer.next(); // Consume the left bracket

        let mut items = Vec::new();
        while self.lexer.peek() != Some(&Token::RBracket) {
            let item = self.parse_expression(Precedence::Lowest)?;
            items.push(item);

            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            }
        }

        self.lexer.next(); // Consume the right bracket

        Ok(Expr::Array(items))
    }

    fn parse_hash_literal(&mut self) -> ParseResult<Expr> {
        self.lexer.next(); // Consume the left brace

        let mut entries = Vec::new();
        while self.lexer.peek() != Some(&Token::RBrace) {
            let key = self.parse_expression(Precedence::Lowest)?;
            self.next_if(|t| t == &Token::Colon, "Colon")?;
            let value = self.parse_expression(Precedence::Lowest)?;
            entries.push((key, value));

            if self.lexer.peek() == Some(&Token::Comma) {
                self.lexer.next();
            }
        }
        self.lexer.next(); // Consume the right brace
        Ok(Expr::Hash(entries))
    }

    fn parse_if(&mut self) -> ParseResult<Expr> {
        self.lexer.next(); // Consume the if
        self.next_if(|t| t == &Token::LParen, "LParen")?;

        let condition = self.parse_expression(Precedence::Lowest)?;

        self.next_if(|t| t == &Token::RParen, "RParen")?;
        self.next_if(|t| t == &Token::LBrace, "LBrace")?;

        let consequence = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RBrace, "RBrace")?;

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

    fn parse_else(&mut self) -> ParseResult<Stmt> {
        self.lexer.next(); // Consume the else
        self.next_if(|t| t == &Token::LBrace, "LBrace")?;

        let alt = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RBrace, "RBrace")?;

        Ok(alt)
    }

    fn parse_function_literal(&mut self) -> ParseResult<Expr> {
        self.lexer.next(); // Consume the fn
        self.next_if(|t| t == &Token::LParen, "LParen")?;

        let params = self.parse_function_params()?;

        self.next_if(|t| t == &Token::RParen, "RParen")?;
        self.next_if(|t| t == &Token::LBrace, "LBrace")?;

        let body = self.parse_block_statement()?;

        self.next_if(|t| t == &Token::RBrace, "RBrace")?;

        Ok(Expr::FunctionLiteral {
            params,
            body: Box::new(body),
        })
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<Expr>> {
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

    fn next_if<F>(&mut self, checker: F, expected: &str) -> ParseResult<()>
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
    fn array_literal() {
        let input = "[1, 1 + 2]";
        let expr = Expr::Array(vec![
            Expr::Int(1),
            Expr::Infix {
                left: Box::new(Expr::Int(1)),
                op: Token::Plus,
                right: Box::new(Expr::Int(2)),
            },
        ]);
        let expected = Program {
            statements: vec![Stmt::Expr(expr)],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "[1, (1 + 2)];";
        assert_eq!(program.to_string(), expected_string.to_string())
    }

    #[test]
    fn hash_literal() {
        let input = "{ (\"a\" + \"b\"): 1, \"c\": 2 + 3 }";
        let expr = Expr::Hash(vec![
            (
                Expr::Infix {
                    left: Box::new(Expr::String("a".to_string())),
                    op: Token::Plus,
                    right: Box::new(Expr::String("b".to_string())),
                },
                Expr::Int(1),
            ),
            (
                Expr::String("c".to_string()),
                Expr::Infix {
                    left: Box::new(Expr::Int(2)),
                    op: Token::Plus,
                    right: Box::new(Expr::Int(3)),
                },
            ),
        ]);
        let expected = Program {
            statements: vec![Stmt::Expr(expr)],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "\
{
    (a + b): 1,
    c: (2 + 3),
};";
        assert_eq!(program.to_string(), expected_string.to_string())
    }

    #[test]
    fn index() {
        let input = "foo[1 + 2]";

        let expr = Expr::Index {
            left: Box::new(Expr::Ident("foo".to_string())),
            index: Box::new(Expr::Infix {
                left: Box::new(Expr::Int(1)),
                op: Token::Plus,
                right: Box::new(Expr::Int(2)),
            }),
        };
        let expected = Program {
            statements: vec![Stmt::Expr(expr)],
        };

        let lexer = Lexer::new(String::from(input));
        let program = Parser::new(lexer).parse().unwrap();
        assert_eq!(program, expected);

        let expected_string = "foo[(1 + 2)];";
        assert_eq!(program.to_string(), expected_string.to_string())
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
