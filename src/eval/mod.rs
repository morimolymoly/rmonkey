#![allow(dead_code)]

use crate::ast;
use crate::lexer;
use crate::object;
use crate::parser;
use crate::token;

use ast::*;

const TRUE: object::Object = object::Object::Boolean(true);
const FALSE: object::Object = object::Object::Boolean(false);
const NULL: object::Object = object::Object::Null;

const ERR_TYPE_MISMATCH: &'static str = "type mismatch:";
const ERR_UNKNOWN_OPS: &'static str = "unkown operator:";

pub fn eval(p: Program) -> Option<object::Object> {
    eval_statements(p.statements)
}

fn eval_statements(stms: Vec<Box<Statement>>) -> Option<object::Object> {
    let mut results = Vec::new();
    for s in stms {
        let result = eval_statement(*s);
        results.push(result.clone());
        match result.unwrap() {
            object::Object::ReturnValue(d) => return Some(*d),
            object::Object::Error(e) => return Some(object::Object::Error(e)),
            _ => ""
        };
    }
    return results[0].clone();
}

fn eval_block_statements(stms: Vec<Box<Statement>>) -> Option<object::Object> {
    let mut results = Vec::new();
    for s in stms {
        let result = eval_statement(*s);
        results.push(result.clone());
        match result.as_ref().unwrap() {
            object::Object::ReturnValue(_) => return Some(result.unwrap()),
            object::Object::Error(e) => return Some(object::Object::Error(e.to_string())),
            _ => "",
        };
    }
    return results[0].clone();
}

fn eval_statement(s: Statement) -> Option<object::Object> {
    match s {
        Statement::ExpStatement(e) => eval_expression(e),
        Statement::Return(e) => {
            if let Some(s) = eval_expression(e) {
                return Some(object::Object::ReturnValue(Box::new(s)));
            }
            return Some(NULL);
        }
        _ => Some(NULL),
    }
}

fn eval_expression(e: Expression) -> Option<object::Object> {
    match e {
        Expression::Literal(d) => eval_literal(d),
        Expression::Prefix(tok, right) => eval_prefix(tok, right),
        Expression::Infix(tok, left, right) => eval_infix(tok, left, right),
        Expression::Block(s) => eval_block_statements(s),
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative)
        }
        _ => Some(NULL),
    }
}

fn eval_literal(l: Literal) -> Option<object::Object> {
    match l {
        Literal::Int(d) => Some(object::Object::Integer(d)),
        Literal::Bool(d) => {
            if d {
                Some(TRUE)
            } else {
                Some(FALSE)
            }
        }
        _ => Some(NULL),
    }
}

fn eval_prefix(tok: token::Token, right: Box<Expression>) -> Option<object::Object> {
    let exp = eval_expression(ast::unbox(right)).unwrap();

    if exp.is_err() {
        return Some(exp);
    }

    match tok {
        token::Token::Bang => {
            return eval_bang_operator_expression(exp);
        }
        token::Token::Minus => {
            return eval_minus_operator_expression(exp);
        }
        _ => Some(NULL),
    }
}

fn eval_infix(
    tok: token::Token,
    left: Box<Expression>,
    right: Box<Expression>,
) -> Option<object::Object> {
    let left = eval_expression(ast::unbox(left)).unwrap();
    if left.is_err() {
        return Some(left)
    }

    let right = eval_expression(ast::unbox(right)).unwrap();
    if right.is_err() {
        return Some(right)
    }

    if left.mytype() == object::INTEGER && right.mytype() == object::INTEGER {
        return eval_integer_infix_expression(&tok, &left, &right);
    } else if left.mytype() == object::BOOLEAN && right.mytype() == object::BOOLEAN {
        return eval_boolean_infix_expression(&tok, &left, &right);
    } else {
        return Some(object::Object::Error(format!("{} {} {}", ERR_TYPE_MISMATCH, left.mytype(), right.mytype())));
    }
}

fn eval_integer_infix_expression(
    token: &token::Token,
    left: &object::Object,
    right: &object::Object,
) -> Option<object::Object> {
    let (left, right) = match left {
        object::Object::Integer(l) => match right {
            object::Object::Integer(r) => (*l, *r),
            _ => (*l, 0),
        },
        _ => (0, 0),
    };

    match token {
        token::Token::Plus => return Some(object::Object::Integer(left + right)),
        token::Token::Minus => return Some(object::Object::Integer(left - right)),
        token::Token::Asterisk => return Some(object::Object::Integer(left * right)),
        token::Token::Slash => return Some(object::Object::Integer(left / right)),
        token::Token::GT => return Some(native_bool_to_boolean_object(left > right)),
        token::Token::LT => return Some(native_bool_to_boolean_object(left < right)),
        token::Token::Equal => return Some(native_bool_to_boolean_object(left == right)),
        token::Token::NotEqual => return Some(native_bool_to_boolean_object(left != right)),
        _ => Some(object::Object::Error(format!("{} {} {}", ERR_UNKNOWN_OPS, object::INTEGER, object::INTEGER))),
    }
}

fn eval_boolean_infix_expression(
    token: &token::Token,
    left: &object::Object,
    right: &object::Object,
) -> Option<object::Object> {
    let (left, right) = match left {
        object::Object::Boolean(l) => match right {
            object::Object::Boolean(r) => (*l, *r),
            _ => (*l, false),
        },
        _ => (false, false),
    };

    match token {
        token::Token::Equal => return Some(native_bool_to_boolean_object(left == right)),
        token::Token::NotEqual => return Some(native_bool_to_boolean_object(left != right)),
        _ => Some(object::Object::Error(format!("{} {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN, object::BOOLEAN))),
    }
}

fn eval_if_expression(
    condition: Box<Expression>,
    consequence: Box<Expression>,
    alternative: Option<Box<Expression>>,
) -> Option<object::Object> {
    let condition = eval_expression(*condition);
    if condition.as_ref().unwrap().is_err() {
        return condition;
    }

    if let Some(condition) = condition {
        if is_truthy(condition) {
            return eval_expression(*consequence);
        } else if let Some(alternative) = alternative {
            return eval_expression(*alternative);
        }
        return Some(NULL);
    }
    Some(NULL)
}

fn is_truthy(obj: object::Object) -> bool {
    match obj {
        NULL => false,
        TRUE => true,
        FALSE => false,
        object::Object::Integer(0) => false,
        _ => true,
    }
}

fn is_integer_left_and_right(left: &object::Object, right: &object::Object) -> (bool, i64, i64) {
    match left {
        object::Object::Integer(l) => match right {
            object::Object::Integer(r) => (true, *l, *r),
            _ => (false, *l, 0),
        },
        _ => (false, 0, 0),
    }
}

fn eval_bang_operator_expression(obj: object::Object) -> Option<object::Object> {
    let obj = native_bool_to_boolean_object(is_truthy(obj));
    match obj {
        TRUE => Some(FALSE),
        FALSE => Some(TRUE),
        NULL => Some(NULL),
        _ => Some(FALSE),
    }
}

fn eval_minus_operator_expression(obj: object::Object) -> Option<object::Object> {
    match obj {
        object::Object::Integer(d) => Some(object::Object::Integer(-d)),
        _ => Some(object::Object::Error(format!("{} {}", ERR_UNKNOWN_OPS, obj.mytype()))),
    }
}

fn native_bool_to_boolean_object(b: bool) -> object::Object {
    if b {
        return TRUE;
    } else {
        return FALSE;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "5;".to_string(),
                expected: 5,
            },
            Test {
                input: "10;".to_string(),
                expected: 10,
            },
            Test {
                input: "-5;".to_string(),
                expected: -5,
            },
            Test {
                input: "-10;".to_string(),
                expected: -10,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10;".to_string(),
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2;".to_string(),
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50;".to_string(),
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10;".to_string(),
                expected: 20,
            },
            Test {
                input: "5 + 2 * 10;".to_string(),
                expected: 25,
            },
            Test {
                input: "20 + 2 * -10;".to_string(),
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10;".to_string(),
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10);".to_string(),
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10;".to_string(),
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10;".to_string(),
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10;".to_string(),
                expected: 50,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_integer_object(program, t.expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "true;".to_string(),
                expected: true,
            },
            Test {
                input: "false;".to_string(),
                expected: false,
            },
            Test {
                input: "1 < 2;".to_string(),
                expected: true,
            },
            Test {
                input: "1 > 2;".to_string(),
                expected: false,
            },
            Test {
                input: "1 < 1;".to_string(),
                expected: false,
            },
            Test {
                input: "1 > 1;".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 1;".to_string(),
                expected: true,
            },
            Test {
                input: "1 != 1;".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 2;".to_string(),
                expected: false,
            },
            Test {
                input: "true==true;".to_string(),
                expected: true,
            },
            Test {
                input: "false==false;".to_string(),
                expected: true,
            },
            Test {
                input: "true==false;".to_string(),
                expected: false,
            },
            Test {
                input: "true != false;".to_string(),
                expected: true,
            },
            Test {
                input: "false != true;".to_string(),
                expected: true,
            },
            Test {
                input: "(1<2) == true;".to_string(),
                expected: true,
            },
            Test {
                input: "(1<2) == false;".to_string(),
                expected: false,
            },
            Test {
                input: "(1>2) == true;".to_string(),
                expected: false,
            },
            Test {
                input: "(1>2) == false;".to_string(),
                expected: true,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_boolean_object(program, t.expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = vec![
            Test {
                input: "!true;".to_string(),
                expected: false,
            },
            Test {
                input: "!false;".to_string(),
                expected: true,
            },
            Test {
                input: "!5;".to_string(),
                expected: false,
            },
            Test {
                input: "!!true;".to_string(),
                expected: true,
            },
            Test {
                input: "!!false;".to_string(),
                expected: false,
            },
            Test {
                input: "!!5;".to_string(),
                expected: true,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            println!("unkounko!");
            test_boolean_object(program, t.expected);
        }
    }

    #[test]
    fn test_if_else_expression() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: "if(true){10};".to_string(),
                expected: object::Object::Integer(10),
            },
            Test {
                input: "if(false){10};".to_string(),
                expected: NULL,
            },
            Test {
                input: "if(1){10};".to_string(),
                expected: object::Object::Integer(10),
            },
            Test {
                input: "if(1<2){10};".to_string(),
                expected: object::Object::Integer(10),
            },
            Test {
                input: "if(1>2){10};".to_string(),
                expected: NULL,
            },
            Test {
                input: "if(1>2){10}else{20};".to_string(),
                expected: object::Object::Integer(20),
            },
            Test {
                input: "if(1<2){10}else{20};".to_string(),
                expected: object::Object::Integer(10),
            },
            Test {
                input: "if((1000 / 2) + 250 * 2 == 1000){9999};".to_string(),
                expected: object::Object::Integer(9999),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected)
        }
    }

    #[test]
    fn test_return_statement() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: "return 10;".to_string(),
                expected: 10,
            },
            Test {
                input: "return 10; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "return 2*5; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "
                if(10>1) {
                    if(10>1) {
                        return 10;
                    }
                    return 1;
                "
                .to_string(),
                expected: 10,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_integer_object(program, t.expected)
        }
    }

    #[test]
    fn test_error_handling() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![
            Test {
                input: String::from("5 + true;"),
                expected: format!("ERROR: {} {} {}", ERR_TYPE_MISMATCH, object::INTEGER, object::BOOLEAN),
            },
            Test {
                input: String::from("5 + true; 5;"),
                expected: format!("ERROR: {} {} {}", ERR_TYPE_MISMATCH, object::INTEGER, object::BOOLEAN),
            },
            Test {
                input: String::from("5; 5 + true;"),
                expected: format!("ERROR: {} {} {}", ERR_TYPE_MISMATCH, object::INTEGER, object::BOOLEAN),
            },
            Test {
                input: String::from("-true;"),
                expected: format!("ERROR: {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN),
            },
            Test {
                input: String::from("true + false;"),
                expected: format!("ERROR: {} {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN, object::BOOLEAN),
            },
            Test {
                input: String::from("if(10>1){true + false;};"),
                expected: format!("ERROR: {} {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN, object::BOOLEAN),
            },
            Test {
                input: "
                if(10>1) {
                    if(10>1) {
                        return true+false;
                    }
                    return 1;
                "
                .to_string(),
                expected: format!("ERROR: {} {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN, object::BOOLEAN),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program.inspect(), t.expected)
        }
    }

    fn eval_program(input: String) -> object::Object {
        let l = lexer::Lexer::new(input.clone());
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        if let None = program {
            panic!("parse_program returned None");
        }
        eval(program.unwrap()).unwrap()
    }

    fn test_integer_object(obj: object::Object, expected: i64) {
        match obj {
            object::Object::Integer(d) => assert_eq!(d, expected),
            _ => panic!("result is not a object::Object::Integer"),
        }
    }

    fn test_boolean_object(obj: object::Object, expected: bool) {
        match obj {
            object::Object::Boolean(d) => assert_eq!(d, expected),
            _ => panic!("result is not a object::Object::Integer"),
        }
    }
}
