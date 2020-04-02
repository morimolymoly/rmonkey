use crate::token;

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut  l = Lexer{
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        return l;
    }

    pub fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let ch = self.ch;
        /*
        if let Some(c) = ch {
            println!("current char {}", c);
        }*/
        self.read_char();
        match ch {
            Some('=') => token::Token::Assign,
            Some(';') => token::Token::Semicolon,
            Some('(') => token::Token::LParen,
            Some(')') => token::Token::RParen,
            Some(',') => token::Token::Comma,
            Some('+') => token::Token::Plus,
            Some('{') => token::Token::LBrace,
            Some('}') => token::Token::RBrace,
            None => token::Token::EOF,
            Some(c) => {
                if Lexer::is_letter(c) {
                    self.read_identifier()
                } else if Lexer::is_digit(c) {
                    self.read_number()
                } else{
                    token::Token::Illegal
                }
            },
        }
    }

    fn read_identifier(&mut self) -> token::Token {
        let pos = self.position;
        loop {
            if let Some(s) = self.ch {
                if Lexer::is_letter(s) {
                    self.read_char();
                } else {
                    break;
                }
            }
        }

        let mut str1 = "".to_string();
        for (i, c) in self.input.chars().enumerate() {
            if i >= pos-1 && i < self.position {
                str1.push(c);
            }
        }

        token::token_from_literal(str1)
    }

    fn read_number(&mut self) -> token::Token {
        let pos = self.position;
        loop {
            if let Some(s) = self.ch {
                if Lexer::is_digit(s) {
                    self.read_char();
                } else {
                    break;
                }
            }
        }

        let mut str1 = "".to_string();
        for (i, c) in self.input.chars().enumerate() {
            if i >= pos-1 && i < self.position {
                str1.push(c);
            }
        }

        let num: u32 = str1.parse().unwrap();
        token::Token::Int(num)
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.ch {
                if c == ' ' || c == '\t' || c == '\n' || c == '\r'{
                    self.read_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.input.chars().nth(self.read_position);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn is_letter(c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }
    fn is_digit(c: char) -> bool {
        c.is_digit(10)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn TestNextToken() {
        let input = String::from("let five = 5;
            let ten = 10;

            let add = fn(x,y) {
                x+y;
            };

            let result = add(five, ten);
        ");

        let tests = vec![
            token::Token::Let,
            token::Token::Ident("five".to_string()),
            token::Token::Assign,
            token::Token::Int(5),
            token::Token::Semicolon,
            token::Token::Let,
            token::Token::Ident("ten".to_string()),
            token::Token::Assign,
            token::Token::Int(10),
            token::Token::Semicolon,
            token::Token::Let,
            token::Token::Ident("add".to_string()),
            token::Token::Assign,
            token::Token::Function,
            token::Token::LParen,
            token::Token::Ident("x".to_string()),
            token::Token::Comma,
            token::Token::Ident("y".to_string()),
            token::Token::RParen,
            token::Token::LBrace,
            token::Token::Ident("x".to_string()),
            token::Token::Plus,
            token::Token::Ident("y".to_string()),
            token::Token::Semicolon,
            token::Token::RBrace,
            token::Token::Semicolon,
            token::Token::Let,
            token::Token::Ident("result".to_string()),
            token::Token::Assign,
            token::Token::Ident("add".to_string()),
            token::Token::LParen,
            token::Token::Ident("five".to_string()),
            token::Token::Comma,
            token::Token::Ident("ten".to_string()),
            token::Token::RParen,
            token::Token::Semicolon,
            token::Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for (i, expected_token) in tests.iter().enumerate() {
            let tok = l.next_token();
            if tok != *expected_token {
                panic!("test[{}] is failed! mismatched token expected:{:?} got:{:?}", i, expected_token, tok);
            }
        }
    }
}
