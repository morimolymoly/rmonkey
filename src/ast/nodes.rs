use crate::ast::traits::*;
use crate::token;
use std::any::Any;

/*
statementes needs to implement Prog, Node, Statement traits.
others need to implement Node.

Implemented Statement
* LetStatement
* ReturnStatement
* ExpressionStatement
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

impl Node for LetStatement {}

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
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

impl Node for ReturnStatement {}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<dyn Expression>>,
}

impl Prog for ExpressionStatement {}

impl ExpressionStatement {
    pub fn new() -> ExpressionStatement {
        ExpressionStatement {
            token: token::Token::Illegal,
            expression: None,
        }
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for ExpressionStatement {}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: token::Token,
    pub right: Option<Box<dyn Expression>>,
}

impl PrefixExpression {
    pub fn new() -> PrefixExpression {
        PrefixExpression {
            token: token::Token::Illegal,
            operator: token::Token::Illegal,
            right: None,
        }
    }
}

impl Node for PrefixExpression {}

impl Expression for PrefixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: token::Token,
    pub operator: token::Token,
    pub left: Option<Box<dyn Expression>>,
    pub right: Option<Box<dyn Expression>>,
}

impl InfixExpression {
    pub fn new() -> InfixExpression {
        InfixExpression {
            token: token::Token::Illegal,
            operator: token::Token::Illegal,
            left: None,
            right: None,
        }
    }
}

impl Node for InfixExpression {}

impl Expression for InfixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
}

impl IntegerLiteral {
    pub fn new() -> IntegerLiteral {
        IntegerLiteral {
            token: token::Token::Illegal,
        }
    }
}

impl Node for IntegerLiteral {}

impl Expression for IntegerLiteral {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: token::Token,
}

impl Identifier {
    pub fn new() -> Identifier {
        Identifier {
            token: token::Token::Illegal,
        }
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for Identifier {}

impl Expression for Identifier {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub token: token::Token,
}

impl Boolean {
    pub fn new() -> Boolean {
        Boolean {
            token: token::Token::Illegal,
        }
    }
}

impl Node for Boolean {}

impl Expression for Boolean {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}
