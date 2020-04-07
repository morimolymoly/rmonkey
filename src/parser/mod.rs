#![allow(dead_code)]

use crate::ast;
use crate::lexer;
use crate::token;

use ast::traits::*;
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

        self.next_token();

        stmt.expression = self.parse_expression(Priority::LOWEST);

        if self.peek_token_is(token::Token::Semicolon) {
            self.next_token();
        }

        Some(stmt)
    }

    fn parse_return_statement(&mut self) -> Option<ast::nodes::ReturnStatement> {
        let mut stmt = ast::nodes::ReturnStatement::new();
        stmt.token = self.cur_token.clone();

        self.next_token();

        stmt.return_value = self.parse_expression(Priority::LOWEST);

        if self.peek_token_is(token::Token::Semicolon) {
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
    fn parse_expression(&mut self, p: Priority) -> Option<Box<dyn ast::traits::Exp>> {
        let mut leftexp = self.prefix_parse().unwrap();
        while !self.peek_token_is(token::Token::Semicolon) && p < self.peek_priority() {
            self.next_token();
            leftexp = self.parse_infix_expression(leftexp.clone());
        }
        Some(leftexp)
    }

    fn prefix_parse(&mut self) -> Option<Box<dyn ast::traits::Exp>> {
        match &self.cur_token {
            token::Token::Ident(_) => {
                let mut tok = ast::nodes::Identifier::new();
                tok.token = self.cur_token.clone();
                Some(Box::new(tok))
            }
            token::Token::Int(_) => {
                let mut tok = ast::nodes::IntegerLiteral::new();
                tok.token = self.cur_token.clone();
                Some(Box::new(tok))
            }
            token::Token::Bang => self.prefix_parse_ops(),
            token::Token::Minus => self.prefix_parse_ops(),
            token::Token::Boolean(_) => {
                let mut tok = ast::nodes::Boolean::new();
                tok.token = self.cur_token.clone();
                Some(Box::new(tok))
            }
            token::Token::LParen => {
                self.next_token();
                let exp = self.parse_expression(Priority::LOWEST);
                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }
                exp
            }
            token::Token::If => {
                let mut tok = ast::nodes::IfExpression::new();
                tok.token = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                self.next_token();
                tok.condition = self.parse_expression(Priority::LOWEST);

                if !self.expect_peek(token::Token::RParen) {
                    return None;
                }

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                tok.consequence = self.parse_block_statement();

                if self.peek_token_is(token::Token::Else) {
                    self.next_token();

                    if !self.expect_peek(token::Token::LBrace) {
                        return None;
                    }

                    tok.alternative = self.parse_block_statement();
                }

                Some(Box::new(tok))
            }
            token::Token::Function => {
                let mut tok = ast::nodes::FunctionLiteral::new();
                tok.token = self.cur_token.clone();

                if !self.expect_peek(token::Token::LParen) {
                    return None;
                }

                tok.parameters = self.parse_function_parameters();

                if !self.expect_peek(token::Token::LBrace) {
                    return None;
                }

                tok.body = self.parse_block_statement();

                Some(Box::new(tok))
            }
            _ => None,
        }
    }

    fn parse_function_parameters(&mut self) -> Vec<Box<dyn ast::traits::Exp>> {
        let mut ret: Vec<Box<dyn ast::traits::Exp>> = Vec::new();

        if self.peek_token_is(token::Token::RParen) {
            self.next_token();
            return ret;
        }
        self.next_token();

        let mut ident = ast::nodes::Identifier::new();
        ident.token = self.cur_token.clone();

        ret.push(Box::new(ident));

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();

            let mut ident = ast::nodes::Identifier::new();
            ident.token = self.cur_token.clone();
            ret.push(Box::new(ident));
        }

        if self.expect_peek(token::Token::RParen) {
            return ret;
        }

        ret
    }

    fn parse_block_statement(&mut self) -> Option<ast::nodes::BlockStatement> {
        let mut block = ast::nodes::BlockStatement::new();

        self.next_token();

        while !self.cur_token_is(token::Token::RBrace) && !self.cur_token_is(token::Token::EOF) {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                block.statements.push(s.clone());
            }
            self.next_token();
        }

        Some(block)
    }

    fn prefix_parse_ops(&mut self) -> Option<Box<dyn ast::traits::Exp>> {
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
            token::Token::LParen => Priority::CALL,
            _ => Priority::LOWEST,
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: Box<dyn ast::traits::Exp>,
    ) -> Box<dyn ast::traits::Exp> {
        match self.cur_token {
            token::Token::LParen => {
                let mut exp = ast::nodes::CallExpression::new();
                exp.function = Some(left);
                exp.token = self.cur_token.clone();
                exp.arguments = self.parse_call_arguments();
                Box::new(exp)
            }
            _ => {
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
    }

    fn parse_call_arguments(&mut self) -> Vec<Box<dyn Exp>> {
        let mut args: Vec<Box<dyn Exp>> = Vec::new();

        if self.peek_token_is(token::Token::RParen) {
            self.next_token();
            return args;
        }

        self.next_token();

        match self.parse_expression(Priority::LOWEST) {
            Some(s) => args.push(s),
            None => {}
        };

        while self.peek_token_is(token::Token::Comma) {
            self.next_token();
            self.next_token();
            match self.parse_expression(Priority::LOWEST) {
                Some(s) => args.push(s),
                None => {}
            };
        }

        if !self.expect_peek(token::Token::RParen) {
            return args;
        }

        args
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::any::Any;
    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            expected_identifier: String,
            expected_value: Box<dyn Any>,
        }

        let tests = vec![
            Test {
                input: String::from("let x = 5;"),
                expected_identifier: String::from("x"),
                expected_value: Box::new(5),
            },
            Test {
                input: String::from("let y = true;"),
                expected_identifier: String::from("y"),
                expected_value: Box::new(true),
            },
            Test {
                input: String::from("let foobar = y;"),
                expected_identifier: String::from("foobar"),
                expected_value: Box::new(String::from("y")),
            },
        ];

        for test in tests.iter() {
            let l = lexer::Lexer::new(test.input.clone());
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
            let stmt = &program.statements[0];
            if !test_let_statement(stmt, test.expected_identifier.clone()) {
                return;
            }

            let exp = match stmt.as_any().downcast_ref::<ast::nodes::LetStatement>() {
                Some(s) => s,
                None => panic!("stmt is not a ast::nodes::LetStatement"),
            };

            if !test_literal_expression(&exp.expression.as_ref().unwrap(), Box::new(&test.expected_value)) {
                return;
            }
        }
    }

    fn test_return_statements() {
        struct Test {
            input: String,
            expected_value: Box<dyn Any>,
        }

        let tests = vec![
            Test {
                input: String::from("return 5;"),
                expected_value: Box::new(5),
            },
            Test {
                input: String::from("return 114514;"),
                expected_value: Box::new(114514),
            },
            Test {
                input: String::from("return 20;"),
                expected_value: Box::new(20),
            },
        ];

        for test in tests.iter() {
            let l = lexer::Lexer::new(test.input.clone());
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
            let stmt = &program.statements[0];
            let exp = match stmt.as_any().downcast_ref::<ast::nodes::ReturnStatement>() {
                Some(s) => s,
                None => panic!("stmt is not a ast::nodes::ReturnStatement"),
            };

            if !test_literal_expression(&exp.return_value.as_ref().unwrap(), Box::new(&test.expected_value)) {
                return;
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = String::from("foobar;");
        let l = lexer::Lexer::new(input);
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
        test_identifier(expression, String::from("foobar"));
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = String::from("5;");
        let l = lexer::Lexer::new(input);
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

    #[test]
    fn test_parsing_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: token::Token,
            value: Box<dyn Any>,
        }
        let prefix_tests = vec![
            PrefixTest {
                input: String::from("!5;"),
                operator: token::Token::Bang,
                value: Box::new(5),
            },
            PrefixTest {
                input: String::from("-15;"),
                operator: token::Token::Minus,
                value: Box::new(15),
            },
        ];

        for test in prefix_tests.iter() {
            let l = lexer::Lexer::new(test.input.clone());
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
            test_literal_expression(&exp.right.as_ref().unwrap(), Box::new(&test.value));
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct InfixTest {
            input: String,
            left: Box<dyn Any>,
            operator: String,
            right: Box<dyn Any>,
        }
        let infix_tests = vec![
            InfixTest {
                input: String::from("5 + 5;"),
                left: Box::new(5),
                operator: String::from("+"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 - 5;"),
                left: Box::new(5),
                operator: String::from("-"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 * 5;"),
                left: Box::new(5),
                operator: String::from("*"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 / 5;"),
                left: Box::new(5),
                operator: String::from("/"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 > 5;"),
                left: Box::new(5),
                operator: String::from(">"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 < 5;"),
                left: Box::new(5),
                operator: String::from("<"),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 == 5;"),
                left: Box::new(5),
                operator: String::from("=="),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("5 != 5;"),
                left: Box::new(5),
                operator: String::from("!="),
                right: Box::new(5),
            },
            InfixTest {
                input: String::from("true == true;"),
                left: Box::new(true),
                operator: String::from("=="),
                right: Box::new(true),
            },
            InfixTest {
                input: String::from("true != false;"),
                left: Box::new(true),
                operator: String::from("!="),
                right: Box::new(false),
            },
            InfixTest {
                input: String::from("false == false;"),
                left: Box::new(false),
                operator: String::from("=="),
                right: Box::new(false),
            },
        ];

        for test in infix_tests.iter() {
            let  l = lexer::Lexer::new(test.input.clone());
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

            let stmt = &program.statements[0];
            test_infix_expression(Box::new(stmt), Box::new(&test.left), test.operator.clone(), Box::new(&test.right));
        }
    }

    #[test]
    fn test_boolean_expression() {
        let input = String::from(
            "
        true;
        false;",
        );

        let expected_vec = vec![true, false];

        let  l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        if let None = program {
            panic!("parse_program returned None");
        }

        let program = program.unwrap();

        if program.statements.len() != 2 {
            panic!(
                "program.statements does not contain 2 statements, got={}",
                program.statements.len()
            );
        }

        for (i, boo) in expected_vec.iter().enumerate() {
            let stmt = match program.statements[i]
                .as_any()
                .downcast_ref::<ast::nodes::ExpressionStatement>()
            {
                Some(laststatement) => laststatement,
                None => panic!("stmt is not a ast::nodes::ExpressionStatement!"),
            };

            let expression = match stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<ast::nodes::Boolean>()
            {
                Some(s) => s,
                None => panic!("expression is None!"),
            };

            if let token::Token::Boolean(b) = expression.token {
                if b != *boo {
                    panic!("expected bool is {} got: {}", expected_vec[i], b);
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestCase {
            input: String,
            expected: String,
        }

        let  tests = vec![
            TestCase {
                input: String::from("true;"),
                expected: String::from("true"),
            },
            TestCase {
                input: String::from("false;"),
                expected: String::from("false"),
            },
            TestCase {
                input: String::from("3>5 == false;"),
                expected: String::from("((3 > 5) == false)"),
            },
            TestCase {
                input: String::from("1 + (2 + 3) + 4;"),
                expected: String::from("((1 + (2 + 3)) + 4)"),
            },
            TestCase {
                input: String::from("(5 + 5) * 2;"),
                expected: String::from("((5 + 5) * 2)"),
            },
            TestCase {
                input: String::from("2 / (5 + 5);"),
                expected: String::from("(2 / (5 + 5))"),
            },
            TestCase {
                input: String::from("-(5 + 5);"),
                expected: String::from("(-(5 + 5))"),
            },
            TestCase {
                input: String::from("!(true == true);"),
                expected: String::from("(!(true == true))"),
            },
            TestCase {
                input: String::from("a + add(b * c) + d;"),
                expected: String::from("((a + add((b * c))) + d)"),
            },
            TestCase {
                input: String::from("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));"),
                expected: String::from("add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
            },
            TestCase {
                input: String::from("add(a + b + c * d / f + g;"),
                expected: String::from("add((((a + b) + ((c * d) / f)) + g))"),
            },
        ];

        for t in tests.iter() {
            let  l = lexer::Lexer::new(t.input.clone());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            check_parser_errors(&p);
            if program.string() != t.expected {
                panic!("actual:{}, got:{}", t.expected, program.string());
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = String::from("if(x<y) {x};");
        let  l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        check_parser_errors(&p);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 2 statements, got={}",
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(s) => s,
            None => panic!("program.statements[0] does not ast::nodes::ExpressionStatement"),
        };

        let exp = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<ast::nodes::IfExpression>()
        {
            Some(s) => s,
            None => panic!("stmt does not ast::nodes::IfExpression>"),
        };

        if !test_infix_expression(
            Box::new(exp.condition.as_ref().unwrap()),
            Box::new(String::from("x")),
            String::from("<").clone(),
            Box::new(String::from("y")),
        ) {
            return;
        }

        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "exp.consequence.statements does not contain 1 statements, got={}",
                exp.consequence.as_ref().unwrap().statements.len()
            )
        }

        let consequence = match exp.consequence.as_ref().unwrap().statements[0].as_any().downcast_ref::<ast::nodes::ExpressionStatement>() {
            Some(s) => s,
            None => panic!("exp.consequence.as_ref().unwrap().statements[0] is not a ast::nodes::ExpressionStatement"),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), String::from("x")) {
            return;
        }

        if let Some(_) = exp.alternative {
            panic!("exp.alternative should be a None");
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = String::from("if(x<y) {x} else {y}");
        let  l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        check_parser_errors(&p);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 2 statements, got={}",
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(s) => s,
            None => panic!("program.statements[0] does not ast::nodes::ExpressionStatement"),
        };

        let exp = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<ast::nodes::IfExpression>()
        {
            Some(s) => s,
            None => panic!("stmt does not ast::nodes::IfExpression>"),
        };

        if !test_infix_expression(
            Box::new(exp.condition.as_ref().unwrap()),
            Box::new(String::from("x")),
            String::from("<").clone(),
            Box::new(String::from("y")),
        ) {
            return;
        }

        if exp.consequence.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "exp.consequence.statements does not contain 1 statements, got={}",
                exp.consequence.as_ref().unwrap().statements.len()
            )
        }

        let consequence = match exp.consequence.as_ref().unwrap().statements[0].as_any().downcast_ref::<ast::nodes::ExpressionStatement>() {
            Some(s) => s,
            None => panic!("exp.consequence.as_ref().unwrap().statements[0] is not a ast::nodes::ExpressionStatement"),
        };

        if !test_identifier(consequence.expression.as_ref().unwrap(), String::from("x")) {
            return;
        }

        let alternative = match exp.alternative.as_ref().unwrap().statements[0].as_any().downcast_ref::<ast::nodes::ExpressionStatement>() {
            Some(s) => s,
            None => panic!("exp.consequence.as_ref().unwrap().statements[0] is not a ast::nodes::ExpressionStatement"),
        };

        if !test_identifier(alternative.expression.as_ref().unwrap(), String::from("y")) {
            return;
        }
    }

    #[test]
    fn test_function_lireral_parsing() {
        let input = String::from("fn(x,y){x+y;}");
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        check_parser_errors(&p);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain 2 statements, got={}",
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(s) => s,
            None => panic!("program.statements[0] does not ast::nodes::ExpressionStatement"),
        };

        let function = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<ast::nodes::FunctionLiteral>()
        {
            Some(s) => s,
            None => panic!("stmt does not ast::nodes::FunctionLiteral>"),
        };

        if function.parameters.len() != 2 {
            panic!(
                "function.parameters is not 2. got {}",
                function.parameters.len()
            );
        }

        test_literal_expression(&function.parameters[0], Box::new(String::from("x")));
        test_literal_expression(&function.parameters[1], Box::new(String::from("y")));

        if function.body.as_ref().unwrap().statements.len() != 1 {
            panic!(
                "function.body.statements has not 1 statements, got:{}",
                function.body.as_ref().unwrap().statements.len()
            );
        }

        let body = match function.body.as_ref().unwrap().statements[0].as_any().downcast_ref::<ast::nodes::ExpressionStatement>() {
            Some(s) => s,
            None => panic!("function.body.as_ref().unwrap().statements[0] is not a ast::nodes::ExpressionStatement"),
        };

        test_infix_expression(
            Box::new(body.expression.as_ref().unwrap()),
            Box::new(String::from("x")),
            String::from("+").clone(),
            Box::new(String::from("y")),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct Test {
            input: String,
            expected_params: Vec<String>,
        }

        let inputs = vec![
            Test {
                input: String::from("fn() {};"),
                expected_params: Vec::new(),
            },
            Test {
                input: String::from("fn(x) {};"),
                expected_params: vec![String::from("x")],
            },
            Test {
                input: String::from("fn(x,y,z) {};"),
                expected_params: vec![String::from("x"), String::from("y"), String::from("z")],
            },
        ];

        for input in inputs.iter() {
            let l = lexer::Lexer::new(input.input.clone());
            let mut p = Parser::new(l);
            let program = p.parse_program().unwrap();
            check_parser_errors(&p);

            let stmt = match program.statements[0]
                .as_any()
                .downcast_ref::<ast::nodes::ExpressionStatement>()
            {
                Some(s) => s,
                None => panic!("program.statements[0] does not ast::nodes::ExpressionStatement"),
            };

            let function = match stmt
                .expression
                .as_ref()
                .unwrap()
                .as_any()
                .downcast_ref::<ast::nodes::FunctionLiteral>()
            {
                Some(s) => s,
                None => panic!("stmt does not ast::nodes::FunctionLiteral>"),
            };

            if function.parameters.len() != input.expected_params.len() {
                panic!(
                    "length parameters wrong want {}, got {}",
                    function.parameters.len(),
                    input.expected_params.len()
                );
            }

            for (i, ident) in input.expected_params.iter().enumerate() {
                test_literal_expression(&function.parameters[i], Box::new(ident));
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = String::from(
            "add(1, 2 * 3, 4 + 5);
        ",
        );
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        check_parser_errors(&p);

        if program.statements.len() != 1 {
            panic!(
                "program.statements does not contain {} statements, got={}",
                1,
                program.statements.len()
            );
        }

        let stmt = match program.statements[0]
            .as_ref()
            .as_any()
            .downcast_ref::<ast::nodes::ExpressionStatement>()
        {
            Some(s) => s,
            None => panic!("program.statements[0] is not a ast::nodes::ExpressionStatement"),
        };

        let exp = match stmt
            .expression
            .as_ref()
            .unwrap()
            .as_any()
            .downcast_ref::<ast::nodes::CallExpression>()
        {
            Some(s) => s,
            None => panic!("stmt.expression is not a ast::nodes::CallExpression"),
        };

        if !test_identifier(exp.function.as_ref().unwrap(), String::from("add")) {
            return;
        }

        if exp.arguments.len() != 3 {
            panic!("wrong length of arguments. got={}", exp.arguments.len());
        }

        test_literal_expression(&exp.arguments[0], Box::new(1));
        test_infix_expression(
            Box::new(exp.arguments[1].clone()),
            Box::new(2),
            String::from("*"),
            Box::new(3),
        );
        test_infix_expression(
            Box::new(exp.arguments[2].clone()),
            Box::new(4),
            String::from("+"),
            Box::new(5),
        );
    }

    fn test_let_statement(s: &Box<dyn ast::traits::Prog>, name: String) -> bool {
        let let_s = match s.as_any().downcast_ref::<ast::nodes::LetStatement>() {
            Some(letstatement) => letstatement,
            None => {
                println!("s is not a ast::nodes::LetStatement!");
                return false;
            }
        };

        if let_s.name.token != token::Token::Ident(name.clone()) {
            println!(
                "let_s.name.token is not token::Token::Ident({}), got={:?})",
                name, let_s.name.token
            );
            return false;
        }
        return true;
    }

    fn test_literal_expression(exp: &Box<dyn ast::traits::Exp>, expected: Box<dyn Any>) -> bool {
        if let Some(v) = expected.downcast_ref::<String>() {
            return test_identifier(exp, v.clone());
        } else if let Some(v) = expected.downcast_ref::<u32>() {
            return test_integer_literal(exp, *v);
        } else if let Some(v) = expected.downcast_ref::<bool>() {
            return test_bootlean_literal(exp, *v);
        }
        false
    }

    fn test_infix_expression(exp: Box<dyn Any>, left: Box<dyn Any>, operator: String, right: Box<dyn Any>) -> bool {
        let infix = match exp.downcast_ref::<Box<dyn ast::traits::Prog>>() {
            Some(s) => match s.as_any().downcast_ref::<ast::nodes::ExpressionStatement>() {
                Some(laststatement) => {
                    match laststatement
                        .expression
                        .as_ref()
                        .unwrap()
                        .as_any()
                        .downcast_ref::<ast::nodes::InfixExpression>()
                    {
                        Some(s) => s,
                        None => {
                            return false;
                        }
                    }
                }
                None => match exp.downcast_ref::<Box<dyn ast::traits::Exp>>() {
                    Some(s) => {
                        match s
                            .as_ref()
                            .as_any()
                            .downcast_ref::<ast::nodes::InfixExpression>()
                        {
                            Some(s) => s,
                            None => {
                                return false;
                            }
                        }
                    }
                    None => {
                        return false;
                    }
                },
            },
            None => {
                return false;
            }
        };
        if !test_literal_expression(infix.left.as_ref().unwrap(), left) {
            return false;
        }
        if infix.operator != token::token_from_literal(operator.clone()) {
            println!("exp.operator is not {}, got={:?}", operator, infix.operator);
            return false;
        }
        if !test_literal_expression(infix.right.as_ref().unwrap(), right) {
            return false;
        }

        true
    }

    fn test_identifier(exp: &Box<dyn ast::traits::Exp>, value: String) -> bool {
        let ident = match exp.as_any().downcast_ref::<ast::nodes::Identifier>() {
            Some(s) => s,
            None => {
                println!("expression is not Identifier");
                return false;
            }
        };

        if ident.token != token::Token::Ident(value.clone()) {
            println!("ident is not {}, got {:?}", value, ident.token);
            return false;
        }

        true
    }

    fn test_integer_literal(il: &Box<dyn ast::traits::Exp>, value: u32) -> bool {
        let integer = match il.as_any().downcast_ref::<ast::nodes::IntegerLiteral>() {
            Some(s) => s,
            None => {
                println!("expression is not IntegerLiteral");
                return false;
            }
        };

        if integer.token != token::Token::Int(value) {
            println!("ident is not a Ident({}), got:{:?}", value, integer.token);
            return false;
        }
        println!("got token{:?}", integer.token);
        return true;
    }

    fn test_bootlean_literal(il: &Box<dyn ast::traits::Exp>, value: bool) -> bool {
        let boolean = match il.as_any().downcast_ref::<ast::nodes::Boolean>() {
            Some(s) => s,
            None => {
                println!("expression is not Boolean");
                return false;
            }
        };

        if boolean.token != token::Token::Boolean(value) {
            println!(
                "boolean is not a Boolean({}), got:{:?}",
                value, boolean.token
            );
            return false;
        }
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
