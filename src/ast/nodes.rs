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
    fn String(&self) -> String {
        format!("let {} = {};", self.name.String(), "")
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
    fn String(&self) -> String {
        format!("return {}", "")
    }
}

pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<dyn Exp>>,
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
        token::string_from_token(self.token.clone())
    }
    fn String(&self) -> String {
        if let Some(d) = &self.expression {
            return d.String();
        }
        String::from("")
    }
}

#[derive(Clone)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: token::Token,
    pub right: Option<Box<dyn Exp>>,
}

impl Exp for PrefixExpression {
    fn box_clone(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
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
        token::string_from_token(self.token.clone())
    }
    fn String(&self) -> String {
        format!(
            "( {} {} )",
            token::string_from_token(self.token.clone()),
            ""
        )
    }
}

impl Expression for PrefixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct InfixExpression {
    pub token: token::Token,
    pub operator: token::Token,
    pub left: Option<Box<dyn Exp>>,
    pub right: Option<Box<dyn Exp>>,
}

impl Exp for InfixExpression {
    fn box_clone(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
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
        token::string_from_token(self.token.clone())
    }
    fn String(&self) -> String {
        format!(
            "( {} {} {} )",
            self.left.as_ref().unwrap().String(),
            token::string_from_token(self.operator.clone()),
            self.right.as_ref().unwrap().String()
        )
    }
}

impl Expression for InfixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
}

impl Exp for IntegerLiteral {
    fn box_clone(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
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
        String::from("INT")
    }
    fn String(&self) -> String {
        if let token::Token::Int(d) = &self.token {
            return format!("{}", d);
        }
        String::from("INT")
    }
}

impl Expression for IntegerLiteral {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: token::Token,
}

impl Exp for Identifier {
    fn box_clone(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
}

impl Identifier {
    pub fn new() -> Identifier {
        Identifier {
            token: token::Token::Illegal,
        }
    }
    pub fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        String::from("IDENT")
    }
    fn String(&self) -> String {
        if let token::Token::Ident(s) = &self.token {
            return s.clone();
        }
        String::from("")
    }
}

impl Expression for Identifier {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub token: token::Token,
}

impl Exp for Boolean {
    fn box_clone(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
}

impl Boolean {
    pub fn new() -> Boolean {
        Boolean {
            token: token::Token::Illegal,
        }
    }
}

impl Node for Boolean {
    fn token_literal(&self) -> String {
        token::string_from_token(self.token.clone())
    }
    fn String(&self) -> String {
        token::string_from_token(self.token.clone())
    }
}

impl Expression for Boolean {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
}
