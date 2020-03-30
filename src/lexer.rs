use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.consume_whitespace();

        let next_char = self.read_char();
        next_char.map(|c| match c {
            '=' => match self.peek_char() {
                Some('=') => Token::Equal,
                _ => Token::Assign,
            },
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => match self.peek_char() {
                Some('=') => Token::NotEqual,
                _ => Token::Bang,
            },
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            c => {
                if is_identifier_start(c) {
                    let literal = self.read_identifier();
                    lookup_identifier(literal)
                } else if c.is_ascii_digit() {
                    let literal = self.read_number();
                    Token::Int(literal)
                } else {
                    Token::Illegal(c.to_string())
                }
            }
        })
    }
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input: input.clone().chars().collect(),
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
}

fn is_identifier_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_identifier(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

fn lookup_identifier(identifier: String) -> Token {
    match &identifier[..] {
        "fn" => Token::Function,
        "let" => Token::Let,
        "true" => Token::True,
        "false" => Token::False,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        _ => Token::Identifier(identifier),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;

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
            Token::Assign,
            Token::Plus,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        assert_tokens(input, &tokens)
    }

    #[test]
    fn next_kitchen_sync() {
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

if (5 < 10) {
    return true;
} else {
    return false;
}
            ",
        );
        let tokens = [
            Token::Let,
            Token::Identifier(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LeftParen,
            Token::Identifier(String::from("x")),
            Token::Comma,
            Token::Identifier(String::from("y")),
            Token::RightParen,
            Token::LeftBrace,
            Token::Identifier(String::from("x")),
            Token::Plus,
            Token::Identifier(String::from("y")),
            Token::Semicolon,
            Token::RightBrace,
            Token::Semicolon,
            Token::Let,
            Token::Identifier(String::from("result")),
            Token::Assign,
            Token::Identifier(String::from("add")),
            Token::LeftParen,
            Token::Identifier(String::from("five")),
            Token::Comma,
            Token::Identifier(String::from("ten")),
            Token::RightParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::GreaterThan,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::LeftParen,
            Token::Int(String::from("5")),
            Token::LessThan,
            Token::Int(String::from("10")),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RightBrace,
        ];

        assert_tokens(input, &tokens)
    }
}
