use std::any::Any;

pub trait Prog: Node + Statement {}
pub trait Exp: Node + Expression {
    fn box_clone(&self) -> Box<dyn Exp>;
}

impl Clone for Box<dyn Exp> {
    fn clone(&self) -> Box<dyn Exp> {
        self.box_clone()
    }
}
//impl<T: Node + Statement> Prog for T {}

pub trait Node {
    fn token_literal(&self) -> String;
    fn String(&self) -> String;
}

pub trait Statement {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression {
    fn expresison_node(&self);
    fn as_any(&self) -> &dyn Any;
}
