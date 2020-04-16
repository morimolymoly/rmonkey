#[allow(dead_code)]
use crate::ast::Expression;
use std::fmt;
pub mod environment;
use std::collections::HashMap;

pub const INTEGER: &'static str = "INTEGER";
pub const BOOLEAN: &'static str = "BOOLEAN";
pub const RETURN: &'static str = "RETURN";
pub const ERROR: &'static str = "ERROR";
pub const NULL: &'static str = "NULL";
pub const FUNCTION: &'static str = "FUNCTION";
pub const STRING: &'static str = "STRING";
pub const BUILTIN_FUNCTION: &'static str = "BUILTIN_FUNCTION";
pub const ARRAY: &'static str = "ARRAY";
pub const DEBUG_FUNCTION: &'static str = "DEBUG_FUNCTION";
pub const HASH: &'static str = "HASH";

type BuiltInFunction = fn(Vec<Object>) -> Object;

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
    String(String),
    BuiltinFunc(Option<BuiltInFunction>),
    Array(Vec<Box<Object>>),
    DebugFunction,
    Hash(HashType),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Object::Integer(d) => format!("{}", d),
            Object::Boolean(d) => format!("{}", d),
            Object::Null => "NULL".to_string(),
            Object::String(s) => format!("{}", s),
            Object::BuiltinFunc(_) => format!("{}", BUILTIN_FUNCTION),
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
            Object::String(s) => format!("\"{}\"", s),
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
            Object::BuiltinFunc(_) => format!("{}", BUILTIN_FUNCTION),
            Object::Array(args) => {
                let mut arg_strings: Vec<String> = Vec::new();
                for a in args.iter() {
                    arg_strings.push(format!("{}", a))
                }
                format!("[{}]", arg_strings.join(", "))
            }
            Object::DebugFunction => format!("debug function"),
            Object::Hash(hash) => format!("{:?}", hash),
        }
    }
    pub fn mytype(&self) -> String {
        match self {
            Object::Integer(_) => INTEGER.to_string(),
            Object::Boolean(_) => BOOLEAN.to_string(),
            Object::String(_) => STRING.to_string(),
            Object::Null => NULL.to_string(),
            Object::ReturnValue(_) => RETURN.to_string(),
            Object::Error(_) => ERROR.to_string(),
            Object::Function(_, _, _) => FUNCTION.to_string(),
            Object::BuiltinFunc(_) => BUILTIN_FUNCTION.to_string(),
            Object::Array(_) => ARRAY.to_string(),
            Object::DebugFunction => DEBUG_FUNCTION.to_string(),
            Object::Hash(_) => HASH.to_string(),
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            Object::Error(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HashKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashType {
    pub pairs: HashMap<HashKey, Object>,
}
