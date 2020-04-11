#[allow(dead_code)]
use std::fmt;

pub const INTEGER: &'static str = "INTEGER";
pub const BOOLEAN: &'static str = "BOOLEAN";
pub const RETURN: &'static str = "RETURN";
pub const ERROR: &'static str = "ERROR";
pub const NULL: &'static str = "NULL";

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
        }
    }
    pub fn mytype(&self) -> String {
        match self {
            Object::Integer(_) => INTEGER.to_string(),
            Object::Boolean(_) => BOOLEAN.to_string(),
            Object::Null => NULL.to_string(),
            Object::ReturnValue(_) => RETURN.to_string(),
            Object::Error(_) => ERROR.to_string(),
            _ => "".to_string(),
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            Object::Error(_) => true,
            _ => false,
        }
    }
}
