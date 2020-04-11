#[allow(dead_code)]
use std::fmt;

type ObjectType =  String;
pub trait ObjectTrait {
    fn otype(&self) -> ObjectType;
    fn inspect(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(u32),
    Boolean(bool),
    Null,
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = match self {
            Object::Integer(d) => {
                format!("{}", d)
            },
            Object::Boolean(d) => {
                format!("{}", d)
            },
            _ => "".to_string(),
        };
        write!(f, "{}", string)
    }
}

impl Object {
    fn inspect(&self) -> String {
        format!("{}" , self)
    }
    fn mytype(&self) -> String {
        match self {
            Object::Integer(d) => {
                "INTEGER".to_string()
            },
            Object::Boolean(d) => {
                "BOOLEAN".to_string()
            },
            _ => "".to_string(),
        }
    }
}
