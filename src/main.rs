
mod data;
mod parsing;

use std::io::{self, Write};

pub fn main() {
    loop {
        print!("> ");
        io::stdout().flush().unwrap(); // TODO error
        let input = read().unwrap(); // TODO handle error scenario here
        let tokens = parsing::lexer::lex(input).unwrap(); // TODO error
        let ast = parsing::parser::parse(tokens).unwrap(); // TODO error

        println!("{:?}", ast);


    }
}

fn read() -> io::Result<Box<str>> {
    let mut s = String::new();
    io::stdin().read_line(&mut s)?;
    Ok(s.into())
}