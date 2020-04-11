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
        Expression::Prefix(tok, expression) => {
            let exp = eval_expression(ast::unbox(expression)).unwrap();
            match tok {
                token::Token::Bang => {
                    return eval_bang_operator_expression(exp);
                },
                token::Token::Minus => {
                    return eval_minus_operator_expression(exp);
                }
                _ => None,
            }
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
