use crate::parser::token::Token;
use crate::parser::token::Token::*;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            return None;
        }
        Some(self.input[self.read_position])
    }

    fn read_char(&mut self) -> Option<char> {
        self.peek_char().map(|c| {
            self.position = self.read_position;
            self.read_position += 1;
            c
        })
    }
    fn consume_whitespace(&mut self) {
        while self.peek_char().map_or(false, |c| c.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> String {
        let start = self.position;
        while self.peek_char().map_or(false, |c| is_identifier(c)) {
            self.read_char();
        }
        self.input[start..self.read_position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let start = self.position;
        while self.peek_char().map_or(false, |c| c.is_ascii_digit()) {
            self.read_char();
        }
        self.input[start..self.read_position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let start = self.read_position;
        while self.peek_char().map_or(false, |c| c != '"') {
            self.read_char();
        }
        let result = self.input[start..self.read_position].iter().collect();
        self.read_char();
        result
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.consume_whitespace();

        let next_char = self.read_char();
        next_char.map(|c| match c {
            '=' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    Equal
                }
                _ => Assign,
            },
            '+' => Plus,
            '-' => Minus,
            '!' => match self.peek_char() {
                Some('=') => {
                    self.read_char();
                    NotEqual
                }
                _ => Bang,
            },
            '*' => Asterisk,
            '/' => Slash,
            '<' => LessThan,
            '>' => GreaterThan,
            '(' => LParen,
            ')' => RParen,
            '{' => LBrace,
            '}' => RBrace,
            '[' => LBracket,
            ']' => RBracket,
            ',' => Comma,
            ';' => Semicolon,
            '"' => Str(self.read_string()),
            c => {
                if is_identifier_start(c) {
                    let literal = self.read_identifier();
                    lookup_identifier(literal)
                } else if c.is_ascii_digit() {
                    let literal = self.read_number();
                    Int(literal)
                } else {
                    Illegal(c.to_string())
                }
            }
        })
    }
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn lookup_identifier(identifier: String) -> Token {
    match &identifier[..] {
        "fn" => Function,
        "let" => Let,
        "true" => True,
        "false" => False,
        "if" => If,
        "else" => Else,
        "return" => Return,
        _ => Ident(identifier),
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::token::Token;
    use crate::parser::token::Token::*;
    use crate::parser::Lexer;

    fn assert_tokens(input: String, tokens: &[Token]) -> () {
        let lexer = Lexer::new(input);

        for (actual, expected) in lexer.zip(tokens.iter()) {
            assert_eq!(actual, *expected)
        }
    }

    #[test]
    fn next_literals() {
        let input = String::from("=+(){},;");
        let tokens = [
            Assign, Plus, LParen, RParen, LBrace, RBrace, Comma, Semicolon,
        ];

        assert_tokens(input, &tokens)
    }

    #[test]
    fn string_literal() {
        let cases = vec![
            ("\"foobar\"".to_string(), [Str("foobar".to_string())]),
            ("\"\"".to_string(), [Str("".to_string())]),
        ];
        for (input, output) in cases.into_iter() {
            assert_tokens(input, &output)
        }
    }

    #[test]
    fn array() {
        let cases = vec![(
            "[1, 2]".to_string(),
            [
                LBracket,
                Int("1".to_string()),
                Comma,
                Int("2".to_string()),
                RBracket,
            ],
        )];
        for (input, output) in cases.into_iter() {
            assert_tokens(input, &output)
        }
    }

    #[test]
    fn next_kitchen_sink() {
        let input = String::from(
            "
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

1 != 2;
2 == 2;

if (5 < 10) {
    return true;
} else {
    return false;
}
            ",
        );
        let tokens = [
            Let,
            Ident(String::from("five")),
            Assign,
            Int(String::from("5")),
            Semicolon,
            Let,
            Ident(String::from("ten")),
            Assign,
            Int(String::from("10")),
            Semicolon,
            Let,
            Ident(String::from("add")),
            Assign,
            Function,
            LParen,
            Ident(String::from("x")),
            Comma,
            Ident(String::from("y")),
            RParen,
            LBrace,
            Ident(String::from("x")),
            Plus,
            Ident(String::from("y")),
            Semicolon,
            RBrace,
            Semicolon,
            Let,
            Ident(String::from("result")),
            Assign,
            Ident(String::from("add")),
            LParen,
            Ident(String::from("five")),
            Comma,
            Ident(String::from("ten")),
            RParen,
            Semicolon,
            Bang,
            Minus,
            Slash,
            Asterisk,
            Int(String::from("5")),
            Semicolon,
            Int(String::from("5")),
            LessThan,
            Int(String::from("10")),
            GreaterThan,
            Int(String::from("5")),
            Semicolon,
            Int(String::from("1")),
            NotEqual,
            Int(String::from("2")),
            Semicolon,
            Int(String::from("2")),
            Equal,
            Int(String::from("2")),
            Semicolon,
            If,
            LParen,
            Int(String::from("5")),
            LessThan,
            Int(String::from("10")),
            RParen,
            LBrace,
            Return,
            True,
            Semicolon,
            RBrace,
            Else,
            LBrace,
            Return,
            False,
            Semicolon,
            RBrace,
        ];

        assert_tokens(input, &tokens)
    }
}
