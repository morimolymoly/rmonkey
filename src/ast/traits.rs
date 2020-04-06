use std::any::Any;

pub trait Prog: Node + Statement {}
//impl<T: Node + Statement> Prog for T {}

pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression {
    fn expresison_node(&self);
    fn as_any(&self) -> &dyn Any;
}
