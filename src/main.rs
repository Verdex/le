
mod data;
mod parsing;

use std::io::{self, Write};

pub fn main() {
    loop {
        print!("> ");

        match io::stdout().flush() {
            Err(e) => panic!("encountered io error: {e}"),
            _ => { },
        }

        let input = match read() {
            Ok(input) => input,
            Err(e) => panic!("encountered io error: {e}"),
        };

        let tokens = match parsing::lexer::lex(input) {
            Ok(tokens) => tokens,
            Err(e) => panic!("encountered lexing error: {e}"),
        };

        // TODO
        let _ = tokens.iter().map(|x| x.meta()).collect::<Vec<_>>();

        let ast = parsing::parser::parse(tokens).unwrap(); // TODO error


        println!("{:?}", ast);


    }
}

fn read() -> io::Result<Box<str>> {
    let mut s = String::new();
    io::stdin().read_line(&mut s)?;
    Ok(s.into())
}