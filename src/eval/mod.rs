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

pub fn eval(p: Program) -> Option<object::Object> {
    eval_statements(p.statements)
}

fn eval_statements(stms: Vec<Statement>) -> Option<object::Object> {
    for s in stms {
        return eval_statement(s);
    }
    return Some(NULL);
}

fn eval_statement(s: Statement) -> Option<object::Object> {
    match s {
        Statement::ExpStatement(e) => eval_expression(e),
        _ => Some(NULL),
    }
}

fn eval_expression(e: Expression) -> Option<object::Object> {
    match e {
        Expression::Literal(d) => eval_literal(d),
        Expression::Prefix(tok, right) => eval_prefix(tok, right),
        Expression::Infix(tok, left, right) => eval_infix(tok, left, right),
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
    match tok {
        token::Token::Bang => {
            return eval_bang_operator_expression(exp);
        }
        token::Token::Minus => {
            return eval_minus_operator_expression(exp);
        }
        _ => None,
    }
}

fn eval_infix(
    tok: token::Token,
    left: Box<Expression>,
    right: Box<Expression>,
) -> Option<object::Object> {
    let left = eval_expression(ast::unbox(left)).unwrap();
    let right = eval_expression(ast::unbox(right)).unwrap();

    if left.mytype() == object::INTEGER && left.mytype() == object::INTEGER {
        return eval_integer_infix_expression(&tok, &left, &right);
    } else if left.mytype() == object::BOOLEAN && left.mytype() == object::BOOLEAN {
        return eval_boolean_infix_expression(&tok, &left, &right);
    } else {
        None
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
        _ => return Some(NULL),
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
        _ => return Some(NULL),
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
        _ => Some(NULL),
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
                input: "((5 + 10 * 2 + 15) / 3) * 2;".to_string(),
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
                expected: false,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_boolean_object(program, t.expected);
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

    fn test_integer_object(obj: object::Object, expected: i64) -> bool {
        match obj {
            object::Object::Integer(d) => {
                if expected == d {
                    return true;
                } else {
                    return false;
                }
            }
            _ => panic!("result is not a object::Object::Integer"),
        }
    }

    fn test_boolean_object(obj: object::Object, expected: bool) -> bool {
        match obj {
            object::Object::Boolean(d) => {
                if expected == d {
                    return true;
                } else {
                    return false;
                }
            }
            _ => panic!("result is not a object::Object::Integer"),
        }
    }
}
