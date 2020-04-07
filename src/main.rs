mod ast;
mod lexer;
mod parser;
mod repl;
mod token;
use std::io;

use ast::traits::Node;

fn main() {
    /*
    let r = io::stdin();
    let mut reader = r.lock();
    let w = io::stdout();
    let mut writer = w.lock();
    repl::start(&mut reader, &mut writer);
    */
    let l = lexer::Lexer::new(String::from(
        "
        add(1, 2 * 3, 4 + 5);
",
    ));
    let mut p = parser::Parser::new(l);
    let program = p.parse_program();
    println!("{}", program.unwrap().string());
}
