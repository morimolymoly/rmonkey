#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::lexer;
use crate::token;
use std::io::{self, Read, Write};

const PROMPT: &str = ">> ";

pub fn start(in_io: &mut dyn Read, out_io: &mut dyn Write) {
    const BUFFER_SIZE: usize = 32 * 1024;
    loop {
        let mut buf = [0; BUFFER_SIZE];
        println!("{}", PROMPT);

        if let Ok(n) = in_io.read(&mut buf) {
            if n == 0 {
                return;
            }
            println!("{}", String::from_utf8(buf.to_vec()).unwrap());

            let mut l = lexer::Lexer::new(String::from_utf8(buf[0..n].to_vec()).unwrap());
            loop {
                let tok = l.next_token();
                if tok == token::Token::EOF {
                    return;
                }
                println!("{:?}", tok);
            }
        } else {
            return;
        }
    }
}
