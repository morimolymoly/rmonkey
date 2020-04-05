use crate::token;

pub mod nodes;
pub mod traits;
use traits::*;

pub struct Program {
    pub statements: Vec<Box<dyn Prog>>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}
