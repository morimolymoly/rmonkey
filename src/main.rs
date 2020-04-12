mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

fn main() {
    repl::start();
}
