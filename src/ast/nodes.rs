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

#[derive(Clone)]
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

impl Prog for LetStatement {
    fn box_clone_prog(&self) -> Box<dyn Prog> {
        Box::new((*self).clone())
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        String::from("let")
    }
    fn string(&self) -> String {
        format!("let {} = {};", self.name.string(), "")
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) {}

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone)]
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

impl Prog for ReturnStatement {
    fn box_clone_prog(&self) -> Box<dyn Prog> {
        Box::new((*self).clone())
    }
}

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
    fn string(&self) -> String {
        format!("return {}", "")
    }
}

#[derive(Clone)]
pub struct ExpressionStatement {
    pub token: token::Token,
    pub expression: Option<Box<dyn Exp>>,
}

impl Prog for ExpressionStatement {
    fn box_clone_prog(&self) -> Box<dyn Prog> {
        Box::new((*self).clone())
    }
}

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
    fn string(&self) -> String {
        if let Some(d) = &self.expression {
            return d.string();
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
    fn box_clone_exp(&self) -> Box<dyn Exp> {
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
    fn string(&self) -> String {
        if let None = self.right {
            return String::from("");
        }
        format!(
            "({}{})",
            token::string_from_token(self.token.clone()),
            self.right.as_ref().unwrap().string()
        )
    }
}

impl Expression for PrefixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
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
    fn box_clone_exp(&self) -> Box<dyn Exp> {
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
    fn string(&self) -> String {
        if let None = self.left {
            return String::from("");
        }
        if let None = self.right {
            return String::from("");
        }

        format!(
            "({} {} {})",
            self.left.as_ref().unwrap().string(),
            token::string_from_token(self.operator.clone()),
            self.right.as_ref().unwrap().string()
        )
    }
}

impl Expression for InfixExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
}

impl Exp for IntegerLiteral {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
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
    fn string(&self) -> String {
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
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct Identifier {
    pub token: token::Token,
}

impl Exp for Identifier {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
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
    fn string(&self) -> String {
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
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct Boolean {
    pub token: token::Token,
}

impl Exp for Boolean {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
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
    fn string(&self) -> String {
        token::string_from_token(self.token.clone())
    }
}

impl Expression for Boolean {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct IfExpression {
    pub token: token::Token,
    pub condition: Option<Box<dyn Exp>>,
    pub consequence: Option<BlockStatement>,
    pub alternative: Option<BlockStatement>,
}

impl Exp for IfExpression {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
}

impl IfExpression {
    pub fn new() -> IfExpression {
        IfExpression {
            token: token::Token::Illegal,
            condition: None,
            consequence: None,
            alternative: None,
        }
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        token::string_from_token(self.token.clone())
    }
    fn string(&self) -> String {
        if let None = self.consequence {
            return String::from("");
        }
        if let None = self.condition {
            return String::from("");
        }
        let mut string = format!(
            "if{} {}",
            self.condition.as_ref().unwrap().string(),
            self.consequence.as_ref().unwrap().string()
        );
        if let None = self.alternative {
            return string;
        }
        format!(
            "{}else {}",
            string,
            self.alternative.as_ref().unwrap().string()
        )
    }
}

impl Expression for IfExpression {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct BlockStatement {
    pub token: token::Token,
    pub statements: Vec<Box<dyn Prog>>,
}

impl BlockStatement {
    pub fn new() -> BlockStatement {
        BlockStatement {
            token: token::Token::Illegal,
            statements: Vec::new(),
        }
    }
}

impl Exp for BlockStatement {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
}

impl Node for BlockStatement {
    fn token_literal(&self) -> String {
        token::string_from_token(self.token.clone())
    }
    fn string(&self) -> String {
        let mut string = String::new();
        for s in self.statements.iter() {
            string.push_str(&s.string());
        }
        string
    }
}

impl Expression for BlockStatement {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Clone)]
pub struct FunctionLiteral {
    pub token: token::Token,
    pub parameters: Vec<Box<dyn Exp>>,
    pub body: Option<BlockStatement>,
}

impl FunctionLiteral {
    pub fn new() -> FunctionLiteral {
        FunctionLiteral {
            token: token::Token::Illegal,
            parameters: Vec::new(),
            body: None,
        }
    }
}

impl Exp for FunctionLiteral {
    fn box_clone_exp(&self) -> Box<dyn Exp> {
        Box::new((*self).clone())
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> String {
        token::string_from_token(self.token.clone())
    }
    fn string(&self) -> String {
        let mut param_strings: Vec<String> = Vec::new();
        for p in self.parameters.iter() {
            param_strings.push(p.string());
        }
        let body_string = match &self.body {
            Some(s) => s.string(),
            None => String::from(""),
        };
        format!(
            "{}({}){}",
            self.token_literal(),
            param_strings.join(","),
            body_string
        )
    }
}

impl Expression for FunctionLiteral {
    fn expresison_node(&self) {}
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn box_clone_expression(&self) -> Box<dyn Expression> {
        Box::new((*self).clone())
    }
}
