#[allow(dead_code)]
use std::fmt;

type ObjectType = String;
pub trait ObjectTrait {
    fn otype(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Object::Integer(d) => format!("{}", d),
            Object::Boolean(d) => format!("{}", d),
            Object::Null=> "NULL".to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Object {
    fn inspect(&self) -> String {
        format!("{}", self)
    }
    fn mytype(&self) -> String {
        match self {
            Object::Integer(_) => "INTEGER".to_string(),
            Object::Boolean(_) => "BOOLEAN".to_string(),
            _ => "".to_string(),
        }
    }
}
