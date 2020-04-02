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
            Some('=') => token::Token {
                literal: String::from("="),
                ttype: token::ASSIGN,
            },
            Some(';') => token::Token {
                literal: String::from(";"),
                ttype: token::SEMICOLON,
            },
            Some('(') => token::Token {
                literal: String::from("("),
                ttype: token::LPAREN,
            },
            Some(')') => token::Token {
                literal: String::from(")"),
                ttype: token::RPAREN,
            },
            Some(',') => token::Token {
                literal: String::from(","),
                ttype: token::COMMA,
            },
            Some('+') => token::Token {
                literal: String::from("+"),
                ttype: token::PLUS,
            },
            Some('{') => token::Token {
                literal: String::from("{"),
                ttype: token::LBRACE,
            },
            Some('}') => token::Token {
                literal: String::from("}"),
                ttype: token::RBRACE,
            },
            None => token::Token {
                literal: String::from(""),
                ttype: token::EOF,
            },
            Some(c) => {
                if Lexer::is_letter(c) {
                    self.read_identifier()
                } else if Lexer::is_digit(c) {
                    self.read_number()
                } else{
                    token::Token { literal: format!("{}", c), ttype: token::ILLEGAL}
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
        token::Token {
            ttype: token::token_from_literal(&str1),
            literal: str1,
        }
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
        token::Token {
            ttype: token::INT,
            literal: str1,
        }
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
            token::Token { ttype: token::LET, literal: String::from("let") },
            token::Token { ttype: token::IDENT, literal: String::from("five") },
            token::Token { ttype: token::ASSIGN, literal: String::from("=") },
            token::Token { ttype: token::INT, literal: String::from("5") },
            token::Token { ttype: token::SEMICOLON, literal: String::from(";") },
            token::Token { ttype: token::LET, literal: String::from("let") },
            token::Token { ttype: token::IDENT, literal: String::from("ten") },
            token::Token { ttype: token::ASSIGN, literal: String::from("=") },
            token::Token { ttype: token::INT, literal: String::from("10") },
            token::Token { ttype: token::SEMICOLON, literal: String::from(";") },
            token::Token { ttype: token::LET, literal: String::from("let") },
            token::Token { ttype: token::IDENT, literal: String::from("add") },
            token::Token { ttype: token::ASSIGN, literal: String::from("=") },
            token::Token { ttype: token::FUNCTION, literal: String::from("fn") },
            token::Token { ttype: token::LPAREN, literal: String::from("(") },
            token::Token { ttype: token::IDENT, literal: String::from("x") },
            token::Token { ttype: token::COMMA, literal: String::from(",") },
            token::Token { ttype: token::IDENT, literal: String::from("y") },
            token::Token { ttype: token::RPAREN, literal: String::from(")") },
            token::Token { ttype: token::LBRACE, literal: String::from("{") },
            token::Token { ttype: token::IDENT, literal: String::from("x") },
            token::Token { ttype: token::PLUS, literal: String::from("+") },
            token::Token { ttype: token::IDENT, literal: String::from("y") },
            token::Token { ttype: token::SEMICOLON, literal: String::from(";") },
            token::Token { ttype: token::RBRACE, literal: String::from("}") },
            token::Token { ttype: token::SEMICOLON, literal: String::from(";") },
            token::Token { ttype: token::LET, literal: String::from("let") },
            token::Token { ttype: token::IDENT, literal: String::from("result") },
            token::Token { ttype: token::ASSIGN, literal: String::from("=") },
            token::Token { ttype: token::IDENT, literal: String::from("add") },
            token::Token { ttype: token::LPAREN, literal: String::from("(") },
            token::Token { ttype: token::IDENT, literal: String::from("five") },
            token::Token { ttype: token::COMMA, literal: String::from(",") },
            token::Token { ttype: token::IDENT, literal: String::from("ten") },
            token::Token { ttype: token::RPAREN, literal: String::from(")") },
            token::Token { ttype: token::SEMICOLON, literal: String::from(";") },
            token::Token { ttype: token::EOF, literal: String::from("") },
        ];

        let mut l = Lexer::new(input);

        for (i, expected_token) in tests.iter().enumerate() {
            let tok = l.next_token();
            if tok.ttype != expected_token.ttype {
                panic!("tests[{}] tokentype wrong; expected:{} ( {} ) got:{} ( {} )", i, expected_token.ttype, expected_token.literal,  tok.ttype, tok.literal);
            } else if tok.literal != expected_token.literal {
                panic!("tests[{}] tokenliteral wrong; expected:{} ( {} ) got:{} ( {} )", i, expected_token.ttype, expected_token.literal,  tok.ttype, tok.literal);
            }
        }
    }
}
