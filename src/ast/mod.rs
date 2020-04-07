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
            self.statements[0].string()
        } else {
            String::from("")
        }
    }

    fn string(&self) -> String {
        let mut string = String::from("");
        for s in self.statements.iter() {
            string.push_str(&format!("{}", s.string()))
        }
        string
    }
}
