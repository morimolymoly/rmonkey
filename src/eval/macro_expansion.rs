use crate::ast::{Expression, Program, Statement};
use crate::eval;
use crate::lexer;
use crate::object;
use crate::object::environment::Environment;
use crate::parser;
use crate::token;

fn define_macro(program: &mut Program, env: &mut Environment) {
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
            define_macro(&mut program, &mut env);

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
                if let object::Object::Macro(_, _, _)  = mymacro {
                }else{
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
}
