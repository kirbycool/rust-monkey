use crate::token::Token;
use std::str::Chars;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
}

impl Lexer {
    fn new(input: String) -> Self {
        Lexer {
            input: input.clone().chars().collect(),
            position: 0,
            read_position: 0,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        if self.read_position >= self.input.len() {
            return None;
        }
        let c = self.input[self.read_position];
        self.position = self.read_position;
        self.read_position += 1;
        Some(c)
    }

    fn next_token(&mut self) -> Option<Token> {
        let next_char = self.read_char();
        next_char.map(|c| match c {
            '=' => Token::Assign,
            '+' => Token::Plus,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            ';' => Token::Semicolon,
            _ => Token::Illegal(c.to_string()),
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::lexer::Lexer;
    use crate::token::Token;

    #[test]
    fn next_token() {
        let input = String::from("=+(){},;");
        let expected_tokens = [
            Token::Assign,
            Token::Plus,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::RightBrace,
            Token::Comma,
            Token::Semicolon,
        ];

        let mut lexer = Lexer::new(input);

        for token in expected_tokens.iter() {
            assert_eq!(lexer.next_token().unwrap(), *token)
        }
    }
}
