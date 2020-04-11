use crate::ast::Expression;
#[allow(dead_code)]
use std::fmt;
pub mod environment;

pub const INTEGER: &'static str = "INTEGER";
pub const BOOLEAN: &'static str = "BOOLEAN";
pub const RETURN: &'static str = "RETURN";
pub const ERROR: &'static str = "ERROR";
pub const NULL: &'static str = "NULL";
pub const FUNCTION: &'static str = "FUNCTION";

pub trait ObjectTrait {
    fn mytype(&self) -> String;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
    // args body env
    Function(
        Vec<Box<Expression>>,
        Box<Expression>,
        environment::Environment,
    ),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Object::Integer(d) => format!("{}", d),
            Object::Boolean(d) => format!("{}", d),
            Object::Null => "NULL".to_string(),
            _ => "".to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Object::Integer(d) => format!("{}", d),
            Object::Boolean(d) => format!("{}", d),
            Object::Null => format!("null"),
            Object::ReturnValue(d) => format!("{}", d.inspect()),
            Object::Error(d) => format!("{}: {}", ERROR, d),
            Object::Function(args, body, _) => {
                let mut arg_strings: Vec<String> = Vec::new();
                for a in args.iter() {
                    arg_strings.push(format!("{}", a))
                }
                format!("fn({}){{{}}}", arg_strings.join(", "), body)
            }
        }
    }
    pub fn mytype(&self) -> String {
        match self {
            Object::Integer(_) => INTEGER.to_string(),
            Object::Boolean(_) => BOOLEAN.to_string(),
            Object::Null => NULL.to_string(),
            Object::ReturnValue(_) => RETURN.to_string(),
            Object::Error(_) => ERROR.to_string(),
            Object::Function(_, _, _) => FUNCTION.to_string(),
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            Object::Error(_) => true,
            _ => false,
        }
    }
}
