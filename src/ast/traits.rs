use std::any::Any;

pub trait Prog: Node + Statement {
    fn box_clone_prog(&self) -> Box<dyn Prog>;
}

impl Clone for Box<dyn Prog> {
    fn clone(&self) -> Box<dyn Prog> {
        self.box_clone_prog()
    }
}

pub trait Exp: Node + Expression {
    fn box_clone_exp(&self) -> Box<dyn Exp>;
}

impl Clone for Box<dyn Exp> {
    fn clone(&self) -> Box<dyn Exp> {
        self.box_clone_exp()
    }
}

pub trait Node {
    fn token_literal(&self) -> String;
    fn string(&self) -> String;
}

pub trait Statement {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression {
    fn expresison_node(&self);
    fn as_any(&self) -> &dyn Any;
    fn box_clone_expression(&self) -> Box<dyn Expression>;
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.box_clone_expression()
    }
}
