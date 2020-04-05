use crate::ast;
use crate::lexer;
use crate::token;

use ast::traits::*;
use std::any::Any;

pub struct Parser {
    l: lexer::Lexer,
    cur_token: token::Token,
    peek_token: token::Token,
    errors: Vec<String>,
}

impl Parser {
    fn new(l: lexer::Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: token::Token::Illegal,
            peek_token: token::Token::Illegal,
            errors: Vec::new(),
        };
        p.next_token();
        p.next_token();

        return p;
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
    }

    fn parse_statement(&mut self) -> Option<Box<dyn ast::traits::Prog>> {
        match self.cur_token {
            token::Token::Let => {
                let ps = self.parse_let_statement();
                match ps {
                    Some(p) => Some(Box::new(p)),
                    None => None,
                }
            }
            _ => None,
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::nodes::LetStatement> {
        let mut stmt = ast::nodes::LetStatement::new();
        stmt.token = self.cur_token.clone();

        if !self.expect_peek(token::Token::Ident(String::new())) {
            return None;
        }

        stmt.name = ast::nodes::Identifier::new();
        stmt.name.token = self.cur_token.clone();

        if !self.expect_peek(token::Token::Assign) {
            return None;
        }

        while !self.cur_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn cur_token_is(&self, token: token::Token) -> bool {
        match token {
            token::Token::Ident(_) => match self.cur_token {
                token::Token::Ident(_) => true,
                _ => false,
            },
            _ => self.cur_token == token,
        }
    }

    fn peek_token_is(&self, token: token::Token) -> bool {
        match token {
            token::Token::Ident(_) => match self.peek_token {
                token::Token::Ident(_) => true,
                _ => false,
            },
            _ => self.peek_token == token,
        }
    }

    fn expect_peek(&mut self, t: token::Token) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t.clone());
            false
        }
    }

    pub fn parse_program(&mut self) -> Option<ast::Program> {
        let mut p = ast::Program::new();
        while !self.cur_token_is(token::Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                p.statements.push(s);
            }
            self.next_token();
        }
        Some(p)
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, t: token::Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead.",
            t, self.peek_token
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_let_statements() {
        let input = String::from(
            "
        let x = 5;
        let y = 10;
        let foobar = 114514;
        ",
        );
        let mut l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        if let None = program {
            panic!("parse_program returned None");
        }

        let program = program.unwrap();

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements, got={}",
                program.statements.len()
            );
        }

        let expected_identifier_name = vec!["x", "y", "foobar"];

        for (i, tt) in expected_identifier_name.iter().enumerate() {
            let stmt = &program.statements[i];
            test_let_statement(stmt, tt.to_string());
        }
    }

    fn test_let_statement(s: &Box<dyn ast::traits::Prog>, name: String) {
        if s.token_literal() != String::from("let") {
            panic!("s.tolen_literal not 'let', got={}", s.token_literal());
        }
        let let_s = match s.as_any().downcast_ref::<ast::nodes::LetStatement>() {
            Some(letstatement) => letstatement,
            None => panic!("s is not a ast::nodes::LetStatement!"),
        };

        if let_s.name.token_literal() != name {
            panic!(
                "let_s.name.token is not token::Token::Ident({}), got=token::Token::Ident({})",
                name,
                let_s.name.token_literal()
            );
        }
        println!("{} {}", let_s.token_literal(), let_s.name.token_literal());
    }

    fn check_parser_errors(p: &Parser) {
        let errs = p.get_errors();
        if errs.len() == 0 {
            return;
        }

        println!("parser has {} errors", errs.len());
        for msg in errs.iter() {
            println!("{}", msg);
        }
    }
}
