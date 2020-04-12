#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

use crate::ast;
use crate::eval;
use crate::lexer;
use crate::object;
use crate::parser;
use crate::token;

use std::io::{self, Read, Write};

const PROMPT: &str = ">> ";

pub fn start(in_io: &mut dyn Read, out_io: &mut dyn Write) {
    const BUFFER_SIZE: usize = 32 * 1024;
    let mut env = object::environment::Environment::new();
    loop {
        let mut buf = [0; BUFFER_SIZE];
        println!("{}", PROMPT);

        if let Ok(n) = in_io.read(&mut buf) {
            if n == 0 {
                return;
            }
            let l = lexer::Lexer::new(String::from_utf8(buf[0..n].to_vec()).unwrap());
            let mut p = parser::Parser::new(l);
            let program = p.parse_program();
            let evaluated = eval::eval(program.unwrap(), &mut env).unwrap();
            println!("{}", evaluated.inspect());
        } else {
            return;
        }
    }
}
