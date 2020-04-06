use std::any::Any;

pub trait Prog: Node + Statement {}
//impl<T: Node + Statement> Prog for T {}

pub trait Node {}

pub trait Statement {
    fn statement_node(&self);
    fn as_any(&self) -> &dyn Any;
}

pub trait Expression {
    fn expresison_node(&self);
    fn as_any(&self) -> &dyn Any;
    fn box_clone(&self) -> Box<Expression>;
}

impl Clone for Box<dyn Expression> {
    fn clone(&self) -> Box<dyn Expression> {
        self.box_clone()
    }
}
