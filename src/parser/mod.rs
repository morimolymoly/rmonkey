#![allow(dead_code)]

use crate::ast;
use crate::lexer;
use crate::token;

use ast::*;

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
    pub fn new(l: lexer::Lexer) -> Parser {
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

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut p = Program::new();

        while !self.cur_token_is(token::Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                p.statements.push(s);
            }
            self.next_token();
        }
        Some(p)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            token::Token::Let => {
                let ps = self.parse_let_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
            token::Token::Return => {
                let ps = self.parse_return_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
            _ => {
                let ps = self.parse_expression_statement();
                match ps {
                    Some(p) => Some(p),
                    None => None,
                }
            }
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(token::Token::Ident(String::new())) {
            return None;
        }

        let ident = match self.cur_token.clone() {
            token::Token::Ident(s) => Expression::Ident(s.clone()),
            _ => Expression::Ident(String::from("")),
        };

        if !self.expect_peek(token::Token::Assign) {
            return None;
        }

        self.next_token();

        let exp = match self.parse_expression(Priority::LOWEST) {
            Some(s) => s,
            None => Expression::Literal(Literal::Unit),
        };

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Let(ident, exp))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let value = self.parse_expression(Priority::LOWEST).unwrap();

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let exp = self.parse_expression(Priority::LOWEST).unwrap();

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(Statement::ExpStatement(exp))
    }

    fn parse_expression(&mut self, p: Priority) -> Option<Expression> {
        let mut leftexp = self.prefix_parse().unwrap();
        while !self.peek_token_is(token::Token::Semicolon) && p < self.peek_priority() {
            self.next_token();
            leftexp = self.parse_infix_expression(leftexp.clone());
        }
        Some(leftexp)
    }

    fn prefix_parse(&mut self) -> Option<Expression> {
        match &self.cur_token {
            token::Token::Ident(d) => Some(Expression::Ident(d.clone())),
            token::Token::Int(d) => Some(Expression::Literal(Literal::Int(*d))),
            token::Token::Bang => self.prefix_parse_ops(),
            token::Token::Minus => self.prefix_parse_ops(),
            token::Token::Boolean(d) => Some(Expression::Literal(Literal::Bool(d.clone()))),
            token::Token::LParen => {
                self.next_token();
                let exp = self.parse_expression(Priority::LOWEST);
                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }
                exp
            }
            token::Token::If => {
                let _ = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                self.next_token();
                let condition = self.parse_expression(Priority::LOWEST).unwrap();

                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                let consequence = self.parse_block_statement().unwrap();
                let mut alternative: Option<Box<Expression>> = None;
                if self.peek_token_is(token::Token::Else) {
                    self.next_token();

                    if !self.expect_peek(token::Token::LBrace) {
                        return None;
                    }

                    alternative = match self.parse_block_statement() {
                        Some(s) => Some(Box::new(s)),
                        None => None,
                    }
                }

                Some(Expression::If(
                    Box::new(condition),
                    Box::new(consequence),
                    alternative,
                ))
            }
            token::Token::Function => {
                let _ = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                let parameters = self.parse_function_parameters();

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                let body = self.parse_block_statement().unwrap();

                Some(Expression::Function(parameters, Box::new(body)))
            }
            _ => None,
        }
    }

    fn prefix_parse_ops(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        self.next_token();
        let exp = self.parse_expression(Priority::PREFIX).unwrap();
        Some(Expression::Prefix(token.clone(), Box::new(exp)))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        match self.cur_token {
            token::Token::LParen => {
                let function = Box::new(left);
                let _ = self.cur_token.clone();
                let arguments = self.parse_call_arguments();
                Expression::Call(function, arguments)
            }
            _ => {
                let _ = self.cur_token.clone();
                let operator = self.cur_token.clone();

                let priority = self.cur_priority();
                self.next_token();
                let right = self.parse_expression(priority).unwrap();

                Expression::Infix(operator.clone(), Box::new(left), Box::new(right))
            }
        }
    }

    fn parse_block_statement(&mut self) -> Option<Expression> {
        let mut statements: Vec<Box<Statement>> = Vec::new();

        self.next_token();

        while !self.cur_token_is(token::Token::RBrace) && !self.cur_token_is(token::Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                statements.push(Box::new(s.clone()));
            }
            self.next_token();
        }

        Some(Expression::Block(statements))
    }

    fn parse_function_parameters(&mut self) -> Vec<Box<Expression>> {
        let mut ret: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(token::Token::RParen) {
            self.next_token();
            return ret;
        }
        self.next_token();

        let ident_name = match self.cur_token.clone() {
            token::Token::Ident(s) => s.clone(),
            _ => String::from(""),
        };
        let ident = Expression::Ident(ident_name);

        ret.push(Box::new(ident));

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();

            let ident_name = match self.cur_token.clone() {
                token::Token::Ident(s) => s.clone(),
                _ => String::from(""),
            };
            let ident = Expression::Ident(ident_name);
            ret.push(Box::new(ident));
        }

        if self.expect_peek(token::Token::RParen) {
            return ret;
        }

        ret
    }

    fn parse_call_arguments(&mut self) -> Vec<Box<Expression>> {
        let mut args: Vec<Box<Expression>> = Vec::new();

        if self.peek_token_is(token::Token::RParen) {
            self.next_token();
            return args;
        }

        self.next_token();

        match self.parse_expression(Priority::LOWEST) {
            Some(s) => args.push(Box::new(s.clone())),
            None => {}
        };

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_expression(Priority::LOWEST) {
                Some(s) => args.push(Box::new(s.clone())),
                None => {}
            };
        }

        if !self.expect_peek(token::Token::RParen) {
            return args;
        }

        args
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
            token::Token::LParen => Priority::CALL,
            _ => Priority::LOWEST,
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
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

    fn expect_peek(&mut self, t: token::Token) -> bool {
        if self.peek_token_is(t.clone()) {
            self.next_token();
            true
        } else {
            self.peek_error(t.clone());
            false
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

    fn peek_error(&mut self, t: token::Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead.",
            t, self.peek_token
        ));
    }

    pub fn get_errors(&self) -> &Vec<String> {
        &self.errors
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![
            Test {
                input: String::from("let x = 5;"),
                expected: Statement::Let(
                    Expression::Ident(String::from("x")),
                    Expression::Literal(Literal::Int(5)),
                ),
            },
            Test {
                input: String::from("let y = true;"),
                expected: Statement::Let(
                    Expression::Ident(String::from("y")),
                    Expression::Literal(Literal::Bool(true)),
                ),
            },
            Test {
                input: String::from("let foobar = y;"),
                expected: Statement::Let(
                    Expression::Ident(String::from("foobar")),
                    Expression::Ident(String::from("y")),
                ),
            },
        ];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![
            Test {
                input: String::from("return 5;"),
                expected: Statement::Return(Expression::Literal(Literal::Int(5))),
            },
            Test {
                input: String::from("return true;"),
                expected: Statement::Return(Expression::Literal(Literal::Bool(true))),
            },
            Test {
                input: String::from("return foobar;"),
                expected: Statement::Return(Expression::Ident(String::from("foobar"))),
            },
        ];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_identifier() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![Test {
            input: String::from("foobar;"),
            expected: Statement::ExpStatement(Expression::Ident(String::from("foobar"))),
        }];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![Test {
            input: String::from("5;"),
            expected: Statement::ExpStatement(Expression::Literal(Literal::Int(5))),
        }];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![
            Test {
                input: String::from("!5;"),
                expected: Statement::ExpStatement(Expression::Prefix(
                    token::Token::Bang,
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("-15;"),
                expected: Statement::ExpStatement(Expression::Prefix(
                    token::Token::Minus,
                    Box::new(Expression::Literal(Literal::Int(15))),
                )),
            },
        ];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![
            Test {
                input: String::from("5+5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::Plus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5-5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::Minus,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5*5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::Asterisk,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5/5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::Slash,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5>5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::GT,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5<5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::LT,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5==5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::Equal,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
            Test {
                input: String::from("5!=5;"),
                expected: Statement::ExpStatement(Expression::Infix(
                    token::Token::NotEqual,
                    Box::new(Expression::Literal(Literal::Int(5))),
                    Box::new(Expression::Literal(Literal::Int(5))),
                )),
            },
        ];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_operator_priority_parsing() {
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![
            Test {
                input: String::from("-a + b;"),
                expected: String::from("((-a) + b)"),
            },
            Test {
                input: String::from("!-a;"),
                expected: String::from("(!(-a))"),
            },
            Test {
                input: String::from("a + b + c;"),
                expected: String::from("((a + b) + c)"),
            },
            Test {
                input: String::from("a + b - c;"),
                expected: String::from("((a + b) - c)"),
            },
            Test {
                input: String::from("a * b * c;"),
                expected: String::from("((a * b) * c)"),
            },
            Test {
                input: String::from("a * b / c;"),
                expected: String::from("((a * b) / c)"),
            },
            Test {
                input: String::from("a + b / c;"),
                expected: String::from("(a + (b / c))"),
            },
            Test {
                input: String::from("a + b * c + d / e - f;"),
                expected: String::from("(((a + (b * c)) + (d / e)) - f)"),
            },
            Test {
                input: String::from("3 + 4; -5 + 5;"),
                expected: String::from("(3 + 4)((-5) + 5)"),
            },
            Test {
                input: String::from("5 > 4 == 3 < 4;"),
                expected: String::from("((5 > 4) == (3 < 4))"),
            },
            Test {
                input: String::from("5 < 4 != 3 > 4;"),
                expected: String::from("((5 < 4) != (3 > 4))"),
            },
            Test {
                input: String::from("3 + 4 * 5 == 3 * 1 + 4 * 5;"),
                expected: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            },
            Test {
                input: String::from("true;"),
                expected: String::from("true"),
            },
            Test {
                input: String::from("false;"),
                expected: String::from("false"),
            },
            Test {
                input: String::from("3 > 5 == false;"),
                expected: String::from("((3 > 5) == false)"),
            },
            Test {
                input: String::from("3 < 5 == false;"),
                expected: String::from("((3 < 5) == false)"),
            },
            Test {
                input: String::from("1 + (2 + 3) + 4;"),
                expected: String::from("((1 + (2 + 3)) + 4)"),
            },
            Test {
                input: String::from("(5 + 5) * 2;"),
                expected: String::from("((5 + 5) * 2)"),
            },
            Test {
                input: String::from("2 / (5 + 5);"),
                expected: String::from("(2 / (5 + 5))"),
            },
            Test {
                input: String::from("-(5 + 5);"),
                expected: String::from("(-(5 + 5))"),
            },
            Test {
                input: String::from("!(true == true);"),
                expected: String::from("(!(true == true))"),
            },
            Test {
                input: String::from("a + add(b * c) + d;"),
                expected: String::from("((a + add((b * c))) + d)"),
            },
            Test {
                input: String::from("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));"),
                expected: String::from("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            },
            Test {
                input: String::from("add(a + b + c * d / f + g);"),
                expected: String::from("add((((a + b) + ((c * d) / f)) + g))"),
            },
        ];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            assert_eq!(format!("{}", program), test.expected);
        }
    }

    #[test]
    fn test_if_expression() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![Test {
            input: String::from("if (x < y) { x };"),
            expected: Statement::ExpStatement(Expression::If(
                Box::new(Expression::Infix(
                    token::Token::LT,
                    Box::new(Expression::Ident(String::from("x"))),
                    Box::new(Expression::Ident(String::from("y"))),
                )),
                Box::new(Expression::Block(vec![Box::new(Statement::ExpStatement(
                    Expression::Ident(String::from("x")),
                ))])),
                None,
            )),
        }];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![Test {
            input: String::from("fn(x,y){x + y};"),
            expected: Statement::ExpStatement(Expression::Function(
                vec![
                    Box::new(Expression::Ident(String::from("x"))),
                    Box::new(Expression::Ident(String::from("y"))),
                ],
                Box::new(Expression::Block(vec![Box::new(Statement::ExpStatement(
                    Expression::Infix(
                        token::Token::Plus,
                        Box::new(Expression::Ident(String::from("x"))),
                        Box::new(Expression::Ident(String::from("y"))),
                    ),
                ))])),
            )),
        }];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        struct Test {
            input: String,
            expected: Statement,
        }

        let tests = vec![Test {
            input: String::from("add(1, 2 * 3, 4 + 5);"),
            expected: Statement::ExpStatement(Expression::Call(
                Box::new(Expression::Ident(String::from("add"))),
                vec![
                    Box::new(Expression::Literal(Literal::Int(1))),
                    Box::new(Expression::Infix(
                        token::Token::Asterisk,
                        Box::new(Expression::Literal(Literal::Int(2))),
                        Box::new(Expression::Literal(Literal::Int(3))),
                    )),
                    Box::new(Expression::Infix(
                        token::Token::Plus,
                        Box::new(Expression::Literal(Literal::Int(4))),
                        Box::new(Expression::Literal(Literal::Int(5))),
                    )),
                ],
            )),
        }];

        for test in tests.iter() {
            let program = get_program(test.input.clone());
            if program.statements.len() != 1 {
                panic!(
                    "program.statements does not contain 1 statements, got={}",
                    program.statements.len()
                );
            }
            assert_eq!(program.statements[0], test.expected);
        }
    }
    fn get_program(input: String) -> Program {
        let l = lexer::Lexer::new(input.clone());
        let mut p = Parser::new(l);
        let program = p.parse_program();
        check_parser_errors(&p);
        if let None = program {
            panic!("parse_program returned None");
        }
        program.unwrap()
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
