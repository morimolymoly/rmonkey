use crate::ast::traits::*;
use crate::token;
use std::any::Any;

/*
statementes needs to implement Prog, Node, Statement traits.
others need to implement Node.
*/

pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub expression: Option<Box<dyn Expression>>,
}

impl LetStatement {
    pub fn new() -> LetStatement {
        LetStatement {
            token: token::Token::Illegal,
            name: Identifier::new(),
            expression: None,
        }
    }
}

impl Prog for LetStatement {}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        String::from("let")
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Identifier {
    pub token: token::Token,
}

impl Identifier {
    pub fn new() -> Identifier {
        Identifier {
            token: token::Token::Illegal,
        }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        match &self.token {
            token::Token::Ident(s) => s.clone(),
            _ => String::new(),
        }
    }
}

pub struct ReturnStatement {
    pub token: token::Token,
    pub return_value: Option<Box<dyn Expression>>,
}

impl ReturnStatement {
    pub fn new() -> ReturnStatement {
        ReturnStatement {
            token: token::Token::Illegal,
            return_value: None,
        }
    }
}

impl Prog for ReturnStatement {}

impl Statement for ReturnStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        String::from("return")
    }
}
