use crate::ast::modify;
use crate::ast::{Expression, Literal, Program, Statement};
use crate::eval;
use crate::lexer;
use crate::object;
use crate::object::environment::Environment;
use crate::parser;
use crate::token;

pub fn define_macros(program: &mut Program, env: &mut Environment) {
    let mut def_index: Vec<usize> = Vec::new();

    for (i, stmt) in program.statements.iter().enumerate() {
        if is_macro_def(&stmt) {
            add_macro(*stmt.clone(), env);
            def_index.push(i);
        }
    }

    for i in def_index.iter() {
        program.statements.remove(*i);
    }
}

fn is_macro_def(stmt: &Statement) -> bool {
    if let Statement::Let(_, value) = stmt {
        if let Expression::Macro(_, _) = value {
            return true;
        }
    }
    return false;
}

fn add_macro(stmt: Statement, env: &mut object::environment::Environment) {
    if let Statement::Let(ident, value) = stmt {
        if let Expression::Macro(params, body) = value {
            let mmm = object::Object::Macro(params, body, env.clone());
            if let Expression::Ident(s) = ident {
                env.set(s, mmm);
            }
        }
    }
}

fn func(e: Expression, env: &mut Environment) -> Expression {
    match &e {
        Expression::Call(_function, _args) => {
            let mymacro = is_macro_call(e.clone(), env);
            match mymacro {
                Some(s) => {
                    let args = quote_args(e.clone());
                    let mut eval_env = extend_macro_env(s.clone(), args);

                    if let object::Object::Macro(_, body, _) = s {
                        let evaluated = eval::eval_expression(*body, &mut eval_env).unwrap();
                        if let object::Object::Quote(quote_e) = evaluated {
                            return *quote_e;
                        } else {
                            panic!("we only suppot returning AST nodes from macros")
                        }
                    } else {
                        e
                    }
                }
                None => e,
            }
        }
        _ => e,
    }
}

fn is_macro_call(exp: Expression, env: &mut Environment) -> Option<object::Object> {
    return match exp {
        Expression::Call(function, _) => match *function {
            Expression::Ident(s) => env.get(&s),
            _ => None,
        },
        _ => None,
    };
}

fn quote_args(exp: Expression) -> Vec<object::Object> {
    let mut ret: Vec<object::Object> = Vec::new();
    match exp {
        Expression::Call(_, args) => {
            for a in args.iter() {
                ret.push(object::Object::Quote(a.clone()));
            }
        }
        _ => {}
    }
    ret
}

pub fn expand_macros(program: &mut Program, env: &mut Environment) -> Program {
    modify::modify(program, env, func)
}

fn extend_macro_env(mymacro: object::Object, args: Vec<object::Object>) -> Environment {
    match mymacro {
        object::Object::Macro(arg, _, env) => {
            let mut extended = Environment::new_enclosed_environment(env);
            for (idx, a) in arg.iter().enumerate() {
                if let Expression::Ident(s) = &**a {
                    extended.set(s.to_string(), args[idx].clone());
                }
            }
            extended
        }
        _ => Environment::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_define_macro() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: object::Object,
        }

        let mut environment = Environment::new();
        environment.set(String::from("number"), object::Object::Integer(1));
        environment.set(
            String::from("function"),
            object::Object::Function(
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
                Environment::new(),
            ),
        );

        let tests = vec![Test {
            input: String::from(
                "let number=1; let function=fn(x,y){x+y}; let mymacro=macro(x,y){x+y};",
            ),
            expected: object::Object::Macro(
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
                environment,
            ),
        }];

        for t in tests.iter() {
            let l = lexer::Lexer::new(t.input.clone());
            let mut p = parser::Parser::new(l);
            let mut program = p.parse_program().unwrap();
            let mut env = Environment::new();
            define_macros(&mut program, &mut env);

            {
                let number = env.get(&String::from("number"));
                assert_eq!(number, None);
            }

            {
                let mut environment = Environment::new();
                environment.set(String::from("number"), object::Object::Integer(1));
                let function = env.get(&String::from("function"));
                assert_eq!(function, None);
            }

            {
                let mymacro = env.get(&String::from("mymacro")).unwrap();
                if let object::Object::Macro(_, _, _) = mymacro {
                } else {
                    panic!("mymacro is not a macro");
                }
                assert_eq!(mymacro.inspect(), "macro(x, y){(x + y)}")
            }

            let stmt = program.statements.last().unwrap();
            if let Statement::Let(_, Expression::Macro(_, _)) = *stmt.clone() {
                panic!("mymacro is in your program");
            }

            if let Statement::Let(_, Expression::Function(_, _)) = *stmt.clone() {
            } else {
                panic!("function is not in your program");
            }
        }
    }

    #[test]
    fn test_expand_macro() {
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![
            Test {
                input: String::from(
                    "
                let infixexp = macro() {quote(1+2); };
                infixexp();
                ",
                ),
                expected: String::from("(1 + 2)"),
            },
            Test {
                input: String::from(
                    "
                let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };
                reverse(2 + 2, 10 - 5)
                ",
                ),
                expected: String::from("((10 - 5) - (2 + 2))"),
            },
            Test {
                input: String::from(
                    "
                let unless = macro(condition, consequence, alternative) {
                    quote(if (!(unquote(condition))) {
                        unquote(consequence);
                    } else {
                        unquote(alternative);
                    });
                };

                unless(10>5, puts(\"not greater\"), puts(\"greater\"));
                ",
                ),
                expected: String::from("if(!(10 > 5)){puts(not greater)}else{puts(greater)}"),
            },
        ];
        for t in tests.iter() {
            let l = lexer::Lexer::new(t.input.clone());
            let mut p = parser::Parser::new(l);
            let mut program = p.parse_program().unwrap();
            let mut env = Environment::new();
            define_macros(&mut program, &mut env);
            let expanded = expand_macros(&mut program, &mut env);
            assert_eq!(format!("{}", expanded), t.expected);
        }
    }
}
