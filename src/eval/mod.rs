#![allow(dead_code)]

use crate::ast;
use crate::lexer;
use crate::token;
use crate::parser;
use crate::object;

use ast::*;

fn eval(p: Program) -> Option<object::Object> {
    eval_statements(p.statements)
}

fn eval_statements(stms: Vec<Statement>) -> Option<object::Object> {
    for s in stms {
        return eval_statement(s);
    }
    return None;
}

fn eval_statement(s: Statement) -> Option<object::Object> {
    match s {
        Statement::ExpStatement(e) => {
            eval_expression(e)
        },
        _ => None,
    }
}

fn eval_expression(e: Expression) -> Option<object::Object> {
    match e {
        Expression::Literal(d) => {
            eval_literal(d)
        },
        _ => None,
    }
}

fn eval_literal(l: Literal) -> Option<object::Object> {
    match l {
        Literal::Int(d) => Some(object::Object::Integer(d)),
        _ => None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: u32,
        }

        let tests = vec![
            Test{
                input: "5".to_string(),
                expected: 5,
            },
            Test{
                input: "10".to_string(),
                expected: 10,
            }
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_integer_object(program, t.expected);
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

    fn test_integer_object(obj: object::Object, expected: u32) ->  bool {
        match obj {
            object::Object::Integer(d) => {
                if expected == d {
                    return true;
                } else {
                    return false;
                }
            },
            _ => panic!("result is not a object::Object::Integer"),
        }
    }
}
