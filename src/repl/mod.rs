#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

use crate::ast;
use crate::eval;
use crate::lexer;
use crate::object;
use crate::parser;
use crate::token;

use std::io::{self, stdin, Read, Write};

pub fn start() {
    let mut env = object::environment::Environment::new();
    loop {
        print!(">> ");
        io::stdout().flush().unwrap();

        let mut s = String::new();
        stdin().read_line(&mut s).expect("failed to read stdin");

        let l = lexer::Lexer::new(s);

        let mut p = parser::Parser::new(l);
        let program = p.parse_program();

        let evaluated = eval::eval(program.unwrap(), &mut env).unwrap();

        println!("{}", evaluated.inspect());
    }
}
