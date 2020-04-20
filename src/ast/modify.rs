use crate::ast;
use crate::object::environment::Environment;
use crate::token;

pub fn modify(
    p: &mut ast::Program,
    env: &mut Environment,
    func: fn(ast::Expression, &mut Environment) -> ast::Expression,
) -> ast::Program {
    let mut new_stmt: Vec<Box<ast::Statement>> = Vec::new();
    for stmt in p.statements.iter() {
        match modify_statement(*stmt.clone(), env, func) {
            Some(s) => {
                new_stmt.push(Box::new(s));
            }
            None => {
                new_stmt.push(stmt.clone());
            }
        }
    }
    p.statements = new_stmt;
    p.clone()
}

pub fn modify_statement(
    stmt: ast::Statement,
    env: &mut Environment,
    func: fn(ast::Expression, &mut Environment) -> ast::Expression,
) -> Option<ast::Statement> {
    match stmt {
        ast::Statement::ExpStatement(e) => match modify_expression(e, env, func) {
            Some(s) => Some(ast::Statement::ExpStatement(s)),
            None => None,
        },
        ast::Statement::Return(e) => match modify_expression(e, env, func) {
            Some(s) => Some(ast::Statement::Return(s)),
            None => None,
        },
        ast::Statement::Let(e, value) => match modify_expression(value, env, func) {
            Some(s) => Some(ast::Statement::Let(e, s)),
            None => None,
        },
    }
}

pub fn modify_expression(
    e: ast::Expression,
    env: &mut Environment,
    func: fn(ast::Expression, &mut Environment) -> ast::Expression,
) -> Option<ast::Expression> {
    match e {
        ast::Expression::Literal(_) => Some(func(e.clone(), env)),
        ast::Expression::Infix(token, left, right) => Some(ast::Expression::Infix(
            token,
            Box::new(modify_expression(*left.clone(), env, func).unwrap()),
            Box::new(modify_expression(*right.clone(), env, func).unwrap()),
        )),
        ast::Expression::Prefix(token, right) => Some(ast::Expression::Prefix(
            token,
            Box::new(modify_expression(*right.clone(), env, func).unwrap()),
        )),
        ast::Expression::Index(left, right) => Some(ast::Expression::Index(
            Box::new(modify_expression(*left.clone(), env, func).unwrap()),
            Box::new(modify_expression(*right.clone(), env, func).unwrap()),
        )),
        ast::Expression::If(cond, cons, alt) => Some(ast::Expression::If(
            Box::new(modify_expression(*cond.clone(), env, func).unwrap()),
            Box::new(modify_expression(*cons.clone(), env, func).unwrap()),
            Some(Box::new(
                modify_expression(*alt.unwrap().clone(), env, func).unwrap(),
            )),
        )),
        ast::Expression::Block(stmt) => {
            let mut new_stmt: Vec<Box<ast::Statement>> = Vec::new();
            for s in stmt.iter() {
                new_stmt.push(Box::new(modify_statement(*s.clone(), env, func).unwrap()));
            }
            Some(ast::Expression::Block(new_stmt))
        }
        ast::Expression::Function(params, body) => {
            let mut new_params: Vec<Box<ast::Expression>> = Vec::new();
            for s in params.iter() {
                new_params.push(Box::new(modify_expression(*s.clone(), env, func).unwrap()));
            }
            let body = Box::new(modify_expression(*body, env, func).unwrap());
            Some(ast::Expression::Function(new_params, body))
        }
        ast::Expression::Array(args) => {
            let mut new_args: Vec<Box<ast::Expression>> = Vec::new();
            for s in args.iter() {
                new_args.push(Box::new(modify_expression(*s.clone(), env, func).unwrap()));
            }
            Some(ast::Expression::Array(new_args))
        }
        ast::Expression::Hashmap(args) => {
            let mut new_args: Vec<ast::HashItem> = Vec::new();
            for s in args.iter() {
                new_args.push(ast::HashItem {
                    key: modify_expression(s.key.clone(), env, func).unwrap(),
                    value: modify_expression(s.value.clone(), env, func).unwrap(),
                });
            }
            Some(ast::Expression::Hashmap(new_args))
        }
        ast::Expression::Call(_, _) => {
            /*
             let mut new_params: Vec<Box<ast::Expression>> = Vec::new();
            for s in arguments.iter() {
                new_params.push(Box::new(modify_expression(*s.clone(), func).unwrap()));
            }*/
            Some(func(e.clone(), env))
        }
        _ => Some(e.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modify() {
        let one = || ast::Expression::Literal(ast::Literal::Int(1));
        let two = || ast::Expression::Literal(ast::Literal::Int(2));

        let turn_one_into_two = |e: ast::Expression, _env: &mut Environment| {
            if let ast::Expression::Literal(ast::Literal::Int(_)) = e {
                return ast::Expression::Literal(ast::Literal::Int(2));
            } else {
                return e;
            }
        };

        struct Test {
            input: ast::Program,
            expected: ast::Program,
        }

        let tests = vec![
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(one()))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(two()))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Infix(
                            token::Token::Plus,
                            Box::new(two()),
                            Box::new(one()),
                        ),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Infix(
                            token::Token::Plus,
                            Box::new(two()),
                            Box::new(two()),
                        ),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Infix(
                            token::Token::Plus,
                            Box::new(one()),
                            Box::new(two()),
                        ),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Infix(
                            token::Token::Plus,
                            Box::new(two()),
                            Box::new(two()),
                        ),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Prefix(token::Token::Plus, Box::new(one())),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Prefix(token::Token::Plus, Box::new(two())),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Index(Box::new(one()), Box::new(one())),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Index(Box::new(two()), Box::new(two())),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(ast::Expression::If(
                        Box::new(one()),
                        Box::new(ast::Expression::Block(vec![Box::new(
                            ast::Statement::ExpStatement(one()),
                        )])),
                        Some(Box::new(ast::Expression::Block(vec![Box::new(
                            ast::Statement::ExpStatement(one()),
                        )]))),
                    )))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(ast::Expression::If(
                        Box::new(two()),
                        Box::new(ast::Expression::Block(vec![Box::new(
                            ast::Statement::ExpStatement(two()),
                        )])),
                        Some(Box::new(ast::Expression::Block(vec![Box::new(
                            ast::Statement::ExpStatement(two()),
                        )]))),
                    )))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::Return(one()))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::Return(two()))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::Let(
                        ast::Expression::Ident(String::from("aaaa")),
                        one(),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::Let(
                        ast::Expression::Ident(String::from("aaaa")),
                        two(),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Function(
                            vec![],
                            Box::new(ast::Expression::Block(vec![Box::new(
                                ast::Statement::ExpStatement(one()),
                            )])),
                        ),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Function(
                            vec![],
                            Box::new(ast::Expression::Block(vec![Box::new(
                                ast::Statement::ExpStatement(two()),
                            )])),
                        ),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Array(vec![Box::new(one()), Box::new(one())]),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Array(vec![Box::new(two()), Box::new(two())]),
                    ))],
                },
            },
            Test {
                input: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Hashmap(vec![ast::HashItem {
                            key: one(),
                            value: one(),
                        }]),
                    ))],
                },
                expected: ast::Program {
                    statements: vec![Box::new(ast::Statement::ExpStatement(
                        ast::Expression::Hashmap(vec![ast::HashItem {
                            key: two(),
                            value: two(),
                        }]),
                    ))],
                },
            },
        ];

        for input in tests.iter() {
            let modified = modify(
                &mut input.input.clone(),
                &mut Environment::new(),
                turn_one_into_two,
            );
            assert_eq!(modified, input.expected)
        }
    }
}
