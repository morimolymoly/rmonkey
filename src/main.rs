mod ast;
mod lexer;
mod parser;
mod repl;
mod token;
use std::io;

fn main() {
    let r = io::stdin();
    let mut reader = r.lock();
    let w = io::stdout();
    let mut writer = w.lock();
    repl::start(&mut reader, &mut writer);
}
