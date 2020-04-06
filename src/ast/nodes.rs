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

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        String::from("")
    }
}

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

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        let operator_string = match self.operator {
            token::Token::Bang => String::from("!"),
            token::Token::Minus => String::from("-"),
            _ => String::from(""),
        };
        return operator_string;
    }
}

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

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        let operator_string = match self.operator {
            _ => String::from(""),
        };
        return operator_string;
    }
}

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

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        if let token::Token::Int(i) = self.token {
            return i.to_string();
        }
        String::from("")
    }
}

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

impl Node for Identifier {
    fn token_literal(&self) -> String {
        match &self.token {
            token::Token::Ident(s) => s.clone(),
            _ => String::new(),
        }
    }
}

impl Expression for Identifier {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}
