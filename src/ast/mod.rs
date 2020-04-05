pub trait Node {
    pub fn token_literal(&self) -> String;
}

pub trait Statement {
    pub fn statement_node();
}

pub trait Expression {
    pub fn expresison_node();
}

pub struct Program<T: Node + Statement> {
    pub statements Vec<T>,
}

impl Node for Program {
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new();
        }
    }
}
