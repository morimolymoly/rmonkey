#[allow(dead_code)]
pub mod modify;
use crate::token;
use std::fmt;

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut string = String::new();
        for s in self.statements.iter() {
            string.push_str(&format!("{}", s));
        }
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
    Unit,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Literal::Int(d) => format!("{}", d),
            Literal::Bool(d) => format!("{}", d),
            Literal::String(d) => d.clone(),
            Literal::Unit => String::from("()"),
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct HashItem {
    pub value: Expression,
    pub key: Expression,
}

impl fmt::Display for HashItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = format!("{}:{}", self.value, self.key);
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Expression {
    Literal(Literal),
    Block(Vec<Box<Statement>>),
    // op right
    Prefix(token::Token, Box<Expression>),
    // op left right
    Infix(token::Token, Box<Expression>, Box<Expression>),
    // condition consequence alternative
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    // name parameters body
    Function(Vec<Box<Expression>>, Box<Expression>),
    // function arguments
    Call(Box<Expression>, Vec<Box<Expression>>),
    Ident(String),
    Array(Vec<Box<Expression>>),
    Index(Box<Expression>, Box<Expression>),
    Hashmap(Vec<HashItem>),
    // parameters body
    Macro(Vec<Box<Expression>>, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Expression::Literal(literal) => format!("{}", literal),
            Expression::Block(d) => {
                let mut string = String::new();
                for s in d.iter() {
                    string.push_str(&format!("{}", &s));
                }
                string
            }
            Expression::Prefix(tok, left) => {
                format!("({}{})", token::string_from_token(tok.clone()), left)
            }
            Expression::Infix(tok, left, right) => format!(
                "({} {} {})",
                left,
                token::string_from_token(tok.clone()),
                right
            ),
            Expression::If(condition, consequence, alternative) => {
                let string = format!("if{} {}", condition, consequence);
                let string2 = match alternative {
                    Some(alt) => format!(" else {}", alt),
                    None => String::from(""),
                };
                format!("{}{}", string, string2)
            }
            Expression::Function(params, body) => {
                let mut param_strings: Vec<String> = Vec::new();
                for p in params.iter() {
                    param_strings.push(format!("{}", p));
                }
                format!("fn({}){}", param_strings.join(","), body)
            }
            Expression::Call(function, arguments) => {
                let mut args: Vec<String> = Vec::new();
                for a in arguments.iter() {
                    args.push(format!("{}", a));
                }
                format!("{}({})", function, args.join(", "))
            }
            Expression::Ident(name) => format!("{}", name),
            Expression::Array(arguments) => {
                let mut args: Vec<String> = Vec::new();
                for a in arguments.iter() {
                    args.push(format!("{}", a));
                }
                format!("[{}]", args.join(", "))
            }
            Expression::Index(name, idx) => format!("({}[{}])", name, idx),
            Expression::Hashmap(arguments) => {
                let mut args: Vec<String> = Vec::new();
                for a in arguments.iter() {
                    args.push(format!("{}", a));
                }
                format!("{{{}}}", args.join(", "))
            }
            Expression::Macro(params, body) => {
                let mut param_strings: Vec<String> = Vec::new();
                for p in params.iter() {
                    param_strings.push(format!("{}", p));
                }
                format!("macro({}){}", param_strings.join(","), body)
            }
        };
        write!(f, "{}", string)
    }
}

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Statement {
    // ident value
    Let(Expression, Expression),
    // value
    Return(Expression),
    // expression
    ExpStatement(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Statement::Let(e1, e2) => format!("let {} = {};", e1, e2),
            Statement::Return(e1) => format!("return {};", e1),
            Statement::ExpStatement(e1) => format!("{}", e1),
        };
        write!(f, "{}", string)
    }
}
