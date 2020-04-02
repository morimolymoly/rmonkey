mod token;
mod lexer;
mod repl;
use std::io;

fn main() {
    let r = io::stdin();
    let mut reader = r.lock();
    let w = io::stdout();
    let mut writer = w.lock();
    repl::Start(&mut reader, &mut writer);
}
