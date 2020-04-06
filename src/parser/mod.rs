#![allow(dead_code)]

use crate::ast;
use crate::lexer;
use crate::token;

use ast::traits::*;
use std::any::Any;
use std::cmp::PartialOrd;

type PrefixParseFn = fn() -> Box<dyn ast::traits::Expression>;
type InFixParseFn = fn(Box<dyn ast::traits::Expression>) -> Box<dyn ast::traits::Expression>;

#[derive(PartialEq, PartialOrd)]
enum Priority {
    LOWEST = 1,
    EQUALS = 2,
    LESSGREATER = 3,
    SUM = 4,
    PRODUCT = 5,
    PREFIX = 7,
    CALL = 8,
}

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
            token::Token::Return => {
                let ps = self.parse_return_statement();
                match ps {
                    Some(p) => Some(Box::new(p)),
                    None => None,
                }
            }
            _ => {
                let ps = self.parse_expression_statement();
                match ps {
                    Some(p) => Some(Box::new(p)),
                    None => None,
                }
            }
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

    fn parse_return_statement(&mut self) -> Option<ast::nodes::ReturnStatement> {
        let mut stmt = ast::nodes::ReturnStatement::new();
        stmt.token = self.cur_token.clone();

        self.next_token();

        while !self.cur_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_expression_statement(&mut self) -> Option<ast::nodes::ExpressionStatement> {
        let mut stmt = ast::nodes::ExpressionStatement::new();
        stmt.expression = self.parse_expression(Priority::LOWEST);

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }
    fn parse_expression(&mut self, p: Priority) -> Option<Box<dyn ast::traits::Expression>> {
        let mut leftexp = self.prefix_parse().unwrap();

        while !self.peek_token_is(token::Token::Semicolon) && p < self.peek_priority() {
            self.next_token();
            leftexp = self.parse_infix_expression(leftexp.clone());
            println!("papa{:?}", self.cur_token);
        }
        Some(leftexp)
    }

    fn prefix_parse(&mut self) -> Option<Box<dyn ast::traits::Expression>> {
        match &self.cur_token {
            token::Token::Ident(_) => {
                let mut tok = ast::nodes::Identifier::new();
                tok.token = self.cur_token.clone();
                Some(Box::new(tok))
            }
            token::Token::Int(d) => {
                let mut tok = ast::nodes::IntegerLiteral::new();
                tok.token = self.cur_token.clone();
                Some(Box::new(tok))
            }
            token::Token::Bang => self.prefix_parse_ops(),
            token::Token::Minus => self.prefix_parse_ops(),
            _ => None,
        }
    }

    fn prefix_parse_ops(&mut self) -> Option<Box<dyn ast::traits::Expression>> {
        let mut tok = ast::nodes::PrefixExpression::new();
        tok.token = self.cur_token.clone();
        tok.operator = self.cur_token.clone();
        self.next_token();
        tok.right = self.parse_expression(Priority::PREFIX);
        Some(Box::new(tok))
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

    fn peek_priority(&self) -> Priority {
        self.get_priority(&self.peek_token)
    }

    fn cur_priority(&self) -> Priority {
        self.get_priority(&self.cur_token)
    }

    fn get_priority(&self, tok: &token::Token) -> Priority {
        match *tok {
            token::Token::Equal => Priority::EQUALS,
            token::Token::NotEqual => Priority::EQUALS,
            token::Token::LT => Priority::LESSGREATER,
            token::Token::GT => Priority::LESSGREATER,
            token::Token::Plus => Priority::SUM,
            token::Token::Minus => Priority::SUM,
            token::Token::Slash => Priority::PRODUCT,
            token::Token::Asterisk => Priority::PRODUCT,
            _ => Priority::LOWEST,
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn ast::traits::Expression>,
    ) -> Box<dyn ast::traits::Expression> {
        let mut exp = ast::nodes::InfixExpression::new();
        exp.token = self.cur_token.clone();
        exp.operator = self.cur_token.clone();
        exp.left = Some(left);

        let priority = self.cur_priority();
        self.next_token();
        exp.right = self.parse_expression(priority);

        Box::new(exp)
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

    #[test]
    fn test_return_statements() {
        let input = String::from(
            "
        return 5;
        return 10;
        return 114514;
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

        for stmt in program.statements {
            let let_s = match stmt.as_any().downcast_ref::<ast::nodes::ReturnStatement>() {
                Some(laststatement) => laststatement,
                None => panic!("stmt is not a ast::nodes::ReturnStatement!"),
            };
            if let_s.token_literal() != "return" {
                panic!(
                    "let_s.token_literal not return. got {}",
                    let_s.token_literal()
                );
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");
        let mut l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        if let None = program {
            panic!("parse_program returned None");
        }

        let program = program.unwrap();

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements, got={}",
                program.statements.len()
            );
        }
        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(laststatement) => laststatement,
            None => panic!("stmt is not a ast::nodes::ExpressionStatement!"),
        };

        let expression = match &stmt.expression {
            Some(s) => s,
            None => panic!("expression is None!"),
        };

        let ident = match expression.as_any().downcast_ref::<ast::nodes::Identifier>() {
            Some(s) => s,
            None => panic!("expression is not Identifier"),
        };

        if ident.token != token::Token::Ident(String::from("foobar")) {
            panic!("ident is not a Ident(foobar), got:{:?}", ident.token);
        }
        println!("got token{:?}", ident.token);
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");
        let mut l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        if let None = program {
            panic!("parse_program returned None");
        }

        let program = program.unwrap();

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 1 statements, got={}",
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(laststatement) => laststatement,
            None => panic!("stmt is not a ast::nodes::ExpressionStatement!"),
        };

        let expression = match &stmt.expression {
            Some(s) => s,
            None => panic!("expression is None!"),
        };
        test_integer_literal(expression, 5);
    }

    struct prefixTest {
        input: String,
        operator: token::Token,
        integer: token::Token,
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let mut prefix_tests = vec![
            prefixTest {
                input: String::from("!5;"),
                operator: token::Token::Bang,
                integer: token::Token::Int(5),
            },
            prefixTest {
                input: String::from("-15;"),
                operator: token::Token::Minus,
                integer: token::Token::Int(15),
            },
        ];

        for test in prefix_tests.iter() {
            let mut l = lexer::Lexer::new(test.input.clone());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);
            if let None = program {
                panic!("parse_program returned None");
            }

            let program = program.unwrap();

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }

            let stmt = match program.statements[0]
                .as_any()
                .downcast_ref::<ast::nodes::ExpressionStatement>()
            {
                Some(laststatement) => laststatement,
                None => panic!("stmt is not a ast::nodes::ExpressionStatement!"),
            };

            let exp = match stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<ast::nodes::PrefixExpression>()
            {
                Some(pe) => pe,
                None => panic!("stmt is not a ast::nodes::PrefixExpression!"),
            };

            if exp.operator != test.operator {
                panic!(
                    "exp.operator is not {:?}, got={:?}",
                    test.operator, exp.operator
                );
            }

            let test_num = match test.integer {
                token::Token::Int(d) => d,
                _ => 0,
            };

            let right = match &exp.right {
                Some(r) => r,
                None => panic!("right expression is None!"),
            };

            if !test_integer_literal(&right, test_num) {
                return;
            }
        }
    }

    struct infixTest {
        input: String,
        left: token::Token,
        operator: token::Token,
        right: token::Token,
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let mut infix_tests = vec![
            infixTest {
                input: String::from("5 + 5;"),
                left: token::Token::Int(5),
                operator: token::Token::Plus,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 - 5;"),
                left: token::Token::Int(5),
                operator: token::Token::Minus,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 * 5;"),
                left: token::Token::Int(5),
                operator: token::Token::Asterisk,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 / 5;"),
                left: token::Token::Int(5),
                operator: token::Token::Slash,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 > 5;"),
                left: token::Token::Int(5),
                operator: token::Token::GT,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 < 5;"),
                left: token::Token::Int(5),
                operator: token::Token::LT,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 == 5;"),
                left: token::Token::Int(5),
                operator: token::Token::Equal,
                right: token::Token::Int(5),
            },
            infixTest {
                input: String::from("5 != 5;"),
                left: token::Token::Int(5),
                operator: token::Token::NotEqual,
                right: token::Token::Int(5),
            },
        ];

        for test in infix_tests.iter() {
            let mut l = lexer::Lexer::new(test.input.clone());
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);
            if let None = program {
                panic!("parse_program returned None");
            }

            let program = program.unwrap();

            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }

            let stmt = match program.statements[0]
                .as_any()
                .downcast_ref::<ast::nodes::ExpressionStatement>()
            {
                Some(laststatement) => laststatement,
                None => panic!("stmt is not a ast::nodes::ExpressionStatement!"),
            };

            let exp = match stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<ast::nodes::InfixExpression>()
            {
                Some(pe) => pe,
                None => panic!("stmt is not a ast::nodes::InfixExpression!"),
            };

            if exp.operator != test.operator {
                panic!(
                    "exp.operator is not {:?}, got={:?}",
                    test.operator, exp.operator
                );
            }

            let test_num = match test.left {
                token::Token::Int(d) => d,
                _ => 0,
            };

            let left = match &exp.left {
                Some(r) => r,
                None => panic!("left expression is None!"),
            };

            if !test_integer_literal(&left, test_num) {
                return;
            }

            let test_num = match test.right {
                token::Token::Int(d) => d,
                _ => 0,
            };

            let right = match &exp.right {
                Some(r) => r,
                None => panic!("right expression is None!"),
            };

            if !test_integer_literal(&right, test_num) {
                return;
            }
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

    fn test_integer_literal(il: &Box<dyn ast::traits::Expression>, value: u32) -> bool {
        let integer = match il.as_any().downcast_ref::<ast::nodes::IntegerLiteral>() {
            Some(s) => s,
            None => {
                println!("expression is not Identifier");
                return false;
            }
        };

        if integer.token != token::Token::Int(value) {
            println!("ident is not a Ident(foobar), got:{:?}", integer.token);
            return false;
        }
        println!("got token{:?}", integer.token);
        return true;
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
