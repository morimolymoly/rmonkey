#![allow(dead_code)]
#![allow(unused_imports)]

use crate::ast;
use crate::lexer;
use crate::object;
use crate::parser;
use crate::token;

mod builtin;
mod errmsg;

use builtin::*;
use errmsg::*;

use ast::*;
use object::environment::Environment;

const TRUE: object::Object = object::Object::Boolean(true);
const FALSE: object::Object = object::Object::Boolean(false);
const NULL: object::Object = object::Object::Null;

fn get_final_val(results: Vec<Option<object::Object>>) -> Option<object::Object> {
    if results.len() == 0 {
        return Some(NULL);
    }
    results[results.len() - 1].clone()
}

pub fn eval(p: Program, env: &mut Environment) -> Option<object::Object> {
    eval_statements(p.statements, env)
}

fn eval_statements(stms: Vec<Box<Statement>>, env: &mut Environment) -> Option<object::Object> {
    let mut results = Vec::new();
    for s in stms {
        let result = eval_statement(*s, env);
        results.push(result.clone());
        match result.unwrap() {
            object::Object::ReturnValue(d) => return Some(*d),
            object::Object::Error(e) => return Some(object::Object::Error(e)),
            _ => "",
        };
    }
    get_final_val(results)
}

fn eval_block_statements(
    stms: Vec<Box<Statement>>,
    env: &mut Environment,
) -> Option<object::Object> {
    let mut results = Vec::new();
    for s in stms {
        let result = eval_statement(*s, env);
        results.push(result.clone());
        match result.as_ref().unwrap() {
            object::Object::ReturnValue(_) => return Some(result.unwrap()),
            object::Object::Error(e) => return Some(object::Object::Error(e.to_string())),
            _ => "",
        };
    }
    get_final_val(results)
}

fn eval_statement(s: Statement, env: &mut Environment) -> Option<object::Object> {
    match s {
        Statement::ExpStatement(e) => eval_expression(e, env),
        Statement::Return(e) => {
            if let Some(s) = eval_expression(e, env) {
                return Some(object::Object::ReturnValue(Box::new(s)));
            }
            return Some(NULL);
        }
        Statement::Let(ident, value) => {
            let value = eval_expression(value, env);
            if value.as_ref().unwrap().is_err() {
                return value;
            }
            match ident {
                Expression::Ident(s) => {
                    env.set(s, value.unwrap());
                }
                _ => (),
            };
            Some(NULL)
        }
    }
}

fn eval_expression(e: Expression, env: &mut Environment) -> Option<object::Object> {
    match e {
        Expression::Literal(d) => eval_literal(d, env),
        Expression::Prefix(tok, right) => eval_prefix(tok, right, env),
        Expression::Infix(tok, left, right) => eval_infix(tok, left, right, env),
        Expression::Block(s) => eval_block_statements(s, env),
        Expression::If(condition, consequence, alternative) => {
            eval_if_expression(condition, consequence, alternative, env)
        }
        Expression::Ident(name) => eval_ident(name, env),
        Expression::Function(args, body) => Some(object::Object::Function(args, body, env.clone())),
        Expression::Call(function, args) => {
            let function = eval_expression(*function, env).unwrap();
            if function.is_err() {
                return Some(function);
            }
            let args = eval_expressions(args, env);
            if args.len() == 1 && args[0].is_err() {
                return Some(args[0].clone());
            }
            Some(apply_function(function, args, env))
        }
        Expression::Array(args) => {
            let args = eval_expressions(args, env);
            if args.len() == 1 && args[0].is_err() {
                return Some(args[0].clone());
            }
            let mut args2: Vec<Box<object::Object>> = Vec::new();
            for a in args.iter() {
                args2.push(Box::new(a.clone()))
            }
            Some(object::Object::Array(args2))
        }
        Expression::Index(exp1, index) => {
            let exp1 = eval_expression(*exp1, env).unwrap();
            if exp1.is_err() {
                return Some(NULL);
            }
            let index = eval_expression(*index, env).unwrap();
            if index.is_err() {
                return Some(NULL);
            }
            Some(eval_index_expression(&exp1, &index))
        }
        _ => Some(NULL),
    }
}

fn eval_expressions(
    expressions: Vec<Box<Expression>>,
    env: &mut Environment,
) -> Vec<object::Object> {
    let mut ret: Vec<object::Object> = Vec::new();
    for exp in expressions {
        let evaluated = eval_expression(*exp, env).unwrap();
        if evaluated.is_err() {
            ret.push(evaluated);
            return ret;
        }
        ret.push(evaluated);
    }
    ret
}

fn apply_function(
    function: object::Object,
    args: Vec<object::Object>,
    env: &mut Environment,
) -> object::Object {
    if let object::Object::Function(_, body, _) = function.clone() {
        let mut extended_env = extend_function_env(function, args, env);
        let evaluated = eval_expression(*body, &mut extended_env);
        return unwrap_return_value(evaluated.unwrap());
    }
    if let object::Object::BuiltinFunc(s) = function.clone() {
        match s {
            Some(f) => return f(args),
            None => return object::Object::Error(format!("not a function {}", function.mytype())),
        }
    }
    if let object::Object::DebugFunction = function.clone() {
        return NULL;
    }
    return object::Object::Error(format!("not a function {}", function.mytype()));
}

fn extend_function_env(
    function: object::Object,
    args: Vec<object::Object>,
    env: &mut Environment,
) -> Environment {
    match function {
        object::Object::Function(params, _, _) => {
            let mut newenv = Environment::new_enclosed_environment(env.clone());
            for (i, p) in params.iter().enumerate() {
                if let ast::Expression::Ident(s) = &**p {
                    newenv.set(s.clone(), args[i].clone());
                }
            }
            return newenv;
        }
        _ => Environment::new(),
    }
}

fn unwrap_return_value(obj: object::Object) -> object::Object {
    if let object::Object::ReturnValue(value) = obj {
        return *value;
    }
    return obj;
}

fn builtin_function(name: String, env: &mut Environment) -> Option<object::Object> {
    match &*name {
        "len" => Some(object::Object::BuiltinFunc(Some(builtin_len_function))),
        "stoi" => Some(object::Object::BuiltinFunc(Some(builtin_stoi_function))),
        "first" => Some(object::Object::BuiltinFunc(Some(builtin_first_function))),
        "last" => Some(object::Object::BuiltinFunc(Some(builtin_last_function))),
        "rest" => Some(object::Object::BuiltinFunc(Some(builtin_rest_function))),
        "push" => Some(object::Object::BuiltinFunc(Some(builtin_push_function))),
        "dbg_env_print" => {
            println!("env: {:?}", env);
            Some(object::Object::DebugFunction)
        }
        "dbg_print" => {
            println!("dbg!");
            Some(object::Object::DebugFunction)
        }
        _ => None,
    }
}

fn eval_ident(name: String, env: &mut Environment) -> Option<object::Object> {
    match &env.get(&name) {
        Some(s) => Some(s.clone()),
        None => match builtin_function(name.clone(), env) {
            Some(s) => Some(s.clone()),
            None => Some(object::Object::Error(format!(
                "{} {}",
                ERR_UNKNOWN_IDENT,
                name.clone()
            ))),
        },
    }
}

fn eval_literal(l: Literal, _env: &mut Environment) -> Option<object::Object> {
    match l {
        Literal::Int(d) => Some(object::Object::Integer(d)),
        Literal::Bool(d) => {
            if d {
                Some(TRUE)
            } else {
                Some(FALSE)
            }
        }
        Literal::String(s) => Some(object::Object::String(s)),
        _ => Some(NULL),
    }
}

fn eval_prefix(
    tok: token::Token,
    right: Box<Expression>,
    env: &mut Environment,
) -> Option<object::Object> {
    let exp = eval_expression(ast::unbox(right), env).unwrap();

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
    env: &mut Environment,
) -> Option<object::Object> {
    let left = eval_expression(ast::unbox(left), env).unwrap();
    if left.is_err() {
        return Some(left);
    }

    let right = eval_expression(ast::unbox(right), env).unwrap();
    if right.is_err() {
        return Some(right);
    }

    if left.mytype() == object::INTEGER && right.mytype() == object::INTEGER {
        return eval_integer_infix_expression(&tok, &left, &right, env);
    } else if left.mytype() == object::BOOLEAN && right.mytype() == object::BOOLEAN {
        return eval_boolean_infix_expression(&tok, &left, &right, env);
    } else if left.mytype() == object::STRING && right.mytype() == object::STRING {
        return eval_string_infix_expression(&tok, &left, &right, env);
    } else {
        return Some(object::Object::Error(format!(
            "{} {} {}",
            ERR_TYPE_MISMATCH,
            left.mytype(),
            right.mytype()
        )));
    }
}

fn eval_integer_infix_expression(
    token: &token::Token,
    left: &object::Object,
    right: &object::Object,
    _env: &mut Environment,
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
        _ => Some(object::Object::Error(format!(
            "{} {} {}",
            ERR_UNKNOWN_OPS,
            object::INTEGER,
            object::INTEGER
        ))),
    }
}

fn eval_boolean_infix_expression(
    token: &token::Token,
    left: &object::Object,
    right: &object::Object,
    _env: &mut Environment,
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
        _ => Some(object::Object::Error(format!(
            "{} {} {}",
            ERR_UNKNOWN_OPS,
            object::BOOLEAN,
            object::BOOLEAN
        ))),
    }
}

fn eval_string_infix_expression(
    token: &token::Token,
    left: &object::Object,
    right: &object::Object,
    _env: &mut Environment,
) -> Option<object::Object> {
    if *token != token::Token::Plus {
        return Some(object::Object::Error(format!(
            "{} {} {}",
            ERR_UNKNOWN_OPS,
            left.inspect(),
            right.inspect()
        )));
    }
    Some(object::Object::String(format!("{}{}", left, right)))
}

fn eval_if_expression(
    condition: Box<Expression>,
    consequence: Box<Expression>,
    alternative: Option<Box<Expression>>,
    env: &mut Environment,
) -> Option<object::Object> {
    let condition = eval_expression(*condition, env);
    if condition.as_ref().unwrap().is_err() {
        return condition;
    }

    if let Some(condition) = condition {
        if is_truthy(condition) {
            return eval_expression(*consequence, env);
        } else if let Some(alternative) = alternative {
            return eval_expression(*alternative, env);
        }
        return Some(NULL);
    }
    Some(NULL)
}

fn eval_index_expression(left: &object::Object, index: &object::Object) -> object::Object {
    if left.mytype() == object::ARRAY && index.mytype() == object::INTEGER {
        return eval_array_index_expression(left, index);
    } else {
        return object::Object::Error(format!("index operator not supported {}", left.mytype()));
    }
}

fn eval_array_index_expression(left: &object::Object, index: &object::Object) -> object::Object {
    if let object::Object::Array(args) = left {
        if let object::Object::Integer(d) = index {
            let d = *d as usize;
            if d > args.len() - 1 {
                return NULL;
            }
            return *args[d].clone();
        }
    }
    NULL
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
        _ => Some(object::Object::Error(format!(
            "{} {}",
            ERR_UNKNOWN_OPS,
            obj.mytype()
        ))),
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
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_TYPE_MISMATCH,
                    object::INTEGER,
                    object::BOOLEAN
                ),
            },
            Test {
                input: String::from("5 + true; 5;"),
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_TYPE_MISMATCH,
                    object::INTEGER,
                    object::BOOLEAN
                ),
            },
            Test {
                input: String::from("5; 5 + true;"),
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_TYPE_MISMATCH,
                    object::INTEGER,
                    object::BOOLEAN
                ),
            },
            Test {
                input: String::from("-true;"),
                expected: format!("ERROR: {} {}", ERR_UNKNOWN_OPS, object::BOOLEAN),
            },
            Test {
                input: String::from("true + false;"),
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_UNKNOWN_OPS,
                    object::BOOLEAN,
                    object::BOOLEAN
                ),
            },
            Test {
                input: String::from("if(10>1){true + false;};"),
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_UNKNOWN_OPS,
                    object::BOOLEAN,
                    object::BOOLEAN
                ),
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
                expected: format!(
                    "ERROR: {} {} {}",
                    ERR_UNKNOWN_OPS,
                    object::BOOLEAN,
                    object::BOOLEAN
                ),
            },
            Test {
                input: String::from("foobar;"),
                expected: format!("ERROR: {} foobar", ERR_UNKNOWN_IDENT),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program.inspect(), t.expected)
        }
    }

    #[test]
    fn test_let_statements() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: String::from("let a = 5; a;"),
                expected: 5,
            },
            Test {
                input: String::from("let a = 5 * 5; a;"),
                expected: 25,
            },
            Test {
                input: String::from("let a = 5; let b = a; b;"),
                expected: 5,
            },
            Test {
                input: String::from("let a = 5; let b = a; let c = a + b + 5; c;"),
                expected: 15,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_integer_object(program, t.expected);
        }
    }

    #[test]
    fn test_function_object() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: String,
        }

        let tests = vec![Test {
            input: String::from("fn(x) {x+2};"),
            expected: String::from("fn(x){(x + 2)}"),
        }];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program.inspect(), t.expected);
        }
    }

    #[test]
    fn test_function_application() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = vec![
            Test {
                input: String::from("let identity = 10; let baba = fn(x) {x;}; baba(5);"),
                expected: 5,
            },
            Test {
                input: String::from("let identity = fn(x) {return x;}; identity(5);"),
                expected: 5,
            },
            Test {
                input: String::from("let double = fn(x) {x*2;}; double(5);"),
                expected: 10,
            },
            Test {
                input: String::from("let add = fn(x, y) {x+y;}; add(5, 5);"),
                expected: 10,
            },
            Test {
                input: String::from("let add = fn(x, y) {x + y}; add(5 + 5, add(5, 5));"),
                expected: 20,
            },
            Test {
                input: String::from("fn(x){x;}(5);"),
                expected: 5,
            },
            Test {
                input: String::from(
                    "
                    let add = fn(a, b) {a + b};
                    let sub = fn(a, b) {a - b};
                    let applyfunc = fn(a, b, func) { func(a, b)};
                    applyfunc(2, 2, add);
                ",
                ),
                expected: 4,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            test_integer_object(program, t.expected);
        }
    }

    #[test]
    fn test_string_literal() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("\"Hello, World!\""),
                expected: object::Object::String("Hello, World!".to_string()),
            },
            Test {
                input: String::from("\"クラブisnot家!\""),
                expected: object::Object::String("クラブisnot家!".to_string()),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_string_concat() {
        #[derive(Debug)]
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("\"Hello\" + \" \" + \"World!\""),
                expected: object::Object::String("Hello World!".to_string()),
            },
            Test {
                input: String::from("\"クラブ\" + \" isnot \" + \"家!\""),
                expected: object::Object::String("クラブ isnot 家!".to_string()),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_len() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("len(\"\");"),
                expected: object::Object::Integer(0),
            },
            Test {
                input: String::from("len(\"four\");"),
                expected: object::Object::Integer(4),
            },
            Test {
                input: String::from("len(\"hello world\");"),
                expected: object::Object::Integer(11),
            },
            Test {
                input: String::from("len(1);"),
                expected: object::Object::Error(format!(
                    "{}{}",
                    ERR_BUILT_IN_LEN_ARG_SUPPORT,
                    object::Object::Integer(1).mytype()
                )),
            },
            Test {
                input: String::from("len(\"one\", \"two\");"),
                expected: object::Object::Error(format!("{}2", ERR_BUILT_IN_ARG_NUM)),
            },
            Test {
                input: String::from("let array = [10, 20, 30]; len(array);"),
                expected: object::Object::Integer(3),
            },
            Test {
                input: String::from("len([10]);"),
                expected: object::Object::Integer(1),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_stoi() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("stoi(\"\");"),
                expected: object::Object::Error(
                    "cannot parse integer from empty string".to_string(),
                ),
            },
            Test {
                input: String::from("stoi(\"four\");"),
                expected: object::Object::Error("invalid digit found in string".to_string()),
            },
            Test {
                input: String::from("stoi(\"29\");"),
                expected: object::Object::Integer(29),
            },
            Test {
                input: String::from("stoi(\"19\");"),
                expected: object::Object::Integer(19),
            },
            Test {
                input: String::from("stoi(1);"),
                expected: object::Object::Error(format!(
                    "{}{}",
                    ERR_BUILT_IN_LEN_ARG_SUPPORT,
                    object::Object::Integer(1).mytype()
                )),
            },
            Test {
                input: String::from("stoi(\"10\", \"20\");"),
                expected: object::Object::Error(format!("{}2", ERR_BUILT_IN_ARG_NUM)),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_first() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("first([]);"),
                expected: object::Object::Error(ERR_BUILT_IN_ARRAY_INDEX_OUT_OF_RANGE.to_string()),
            },
            Test {
                input: String::from("first([10]);"),
                expected: object::Object::Integer(10),
            },
            Test {
                input: String::from("first([10, 20, 30]);"),
                expected: object::Object::Integer(10),
            },
            Test {
                input: String::from("let arr = [10, 20, 30]; first(arr);"),
                expected: object::Object::Integer(10),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_last() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("last([]);"),
                expected: object::Object::Error(ERR_BUILT_IN_ARRAY_INDEX_OUT_OF_RANGE.to_string()),
            },
            Test {
                input: String::from("last([10]);"),
                expected: object::Object::Integer(10),
            },
            Test {
                input: String::from("last([10, 20, 30]);"),
                expected: object::Object::Integer(30),
            },
            Test {
                input: String::from("let arr = [10, 20, 30]; last(arr);"),
                expected: object::Object::Integer(30),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_array() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![Test {
            input: String::from("[1, 2, 3, 4];"),
            expected: object::Object::Array(vec![
                Box::new(object::Object::Integer(1)),
                Box::new(object::Object::Integer(2)),
                Box::new(object::Object::Integer(3)),
                Box::new(object::Object::Integer(4)),
            ]),
        }];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_index() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("[1, 2, 3, 4][0];"),
                expected: object::Object::Integer(1),
            },
            Test {
                input: String::from("[1, 2, 3][0];"),
                expected: object::Object::Integer(1),
            },
            Test {
                input: String::from("[1, 2, 3][1];"),
                expected: object::Object::Integer(2),
            },
            Test {
                input: String::from("[1, 2, 3][2];"),
                expected: object::Object::Integer(3),
            },
            Test {
                input: String::from("let i = 0; [1][i];"),
                expected: object::Object::Integer(1),
            },
            Test {
                input: String::from("[1, 2, 3][1 + 1];"),
                expected: object::Object::Integer(3),
            },
            Test {
                input: String::from("let myArray = [1,2,3]; myArray[2];"),
                expected: object::Object::Integer(3),
            },
            Test {
                input: String::from("let myArray = [1,2,3]; myArray[0] + myArray[1] + myArray[2];"),
                expected: object::Object::Integer(6),
            },
            Test {
                input: String::from("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];"),
                expected: object::Object::Integer(2),
            },
            Test {
                input: String::from("[1, 2, 3][3];"),
                expected: NULL,
            },
            Test {
                input: String::from("[1, 2, 3][-1];"),
                expected: NULL,
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_rest() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("rest([1, 2, 3, 4]);"),
                expected: object::Object::Array(vec![
                    Box::new(object::Object::Integer(2)),
                    Box::new(object::Object::Integer(3)),
                    Box::new(object::Object::Integer(4)),
                ]),
            },
            Test {
                input: String::from("rest([1]);"),
                expected: object::Object::Array(vec![]),
            },
            Test {
                input: String::from("rest([]);"),
                expected: object::Object::Array(vec![]),
            },
            Test {
                input: String::from("let mya = [1, 2, 3];rest(mya);mya;"),
                expected: object::Object::Array(vec![
                    Box::new(object::Object::Integer(1)),
                    Box::new(object::Object::Integer(2)),
                    Box::new(object::Object::Integer(3)),
                ]),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    #[test]
    fn test_builtin_push() {
        struct Test {
            input: String,
            expected: object::Object,
        }

        let tests = vec![
            Test {
                input: String::from("let a = [1,2,3,4]; push(a, 5);"),
                expected: object::Object::Array(vec![
                    Box::new(object::Object::Integer(1)),
                    Box::new(object::Object::Integer(2)),
                    Box::new(object::Object::Integer(3)),
                    Box::new(object::Object::Integer(4)),
                    Box::new(object::Object::Integer(5)),
                ]),
            },
            Test {
                input: String::from("let a = [1,2,3,4]; push(a, 5);a;"),
                expected: object::Object::Array(vec![
                    Box::new(object::Object::Integer(1)),
                    Box::new(object::Object::Integer(2)),
                    Box::new(object::Object::Integer(3)),
                    Box::new(object::Object::Integer(4)),
                ]),
            },
        ];

        for t in tests.iter() {
            let program = eval_program(t.input.clone());
            assert_eq!(program, t.expected);
        }
    }

    fn eval_program(input: String) -> object::Object {
        let l = lexer::Lexer::new(input.clone());
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        if let None = program {
            panic!("parse_program returned None");
        }
        let mut env = Environment::new();
        eval(program.unwrap(), &mut env).unwrap()
    }

    fn test_integer_object(obj: object::Object, expected: i64) {
        match obj {
            object::Object::Integer(d) => assert_eq!(d, expected),
            NULL => panic!("NULL~~~"),
            _ => panic!("result is not a object::Object::Integer"),
        }
    }

    fn test_boolean_object(obj: object::Object, expected: bool) {
        match obj {
            object::Object::Boolean(d) => assert_eq!(d, expected),
            NULL => panic!("NULL~~~"),
            _ => panic!("result is not a object::Object::Integer"),
        }
    }
}
