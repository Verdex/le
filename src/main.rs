
mod data;
mod parsing;

use std::rc::Rc;
use std::io::{self, Write};

use crate::data::{Meta};

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

fn read() -> io::Result<Rc<str>> {
    let mut s = String::new();
    io::stdin().read_line(&mut s)?;
    Ok(s.into())
}

fn report_error(input : Rc<str>, m : Meta) -> String {
    let mut prev = vec![];
    let mut line = vec![];
    let mut prev_line_end_index = 0;

    let mut input = input.char_indices();
    let mut underline_prefix_len = 0;

    while let Some((index, c)) = input.next() {
        if index == m.start {
            underline_prefix_len = line.len();
        }

        if c == '\n' || c == '\r' {
            if prev_line_end_index <= m.start {
                break;
            }
            
            prev_line_end_index = index;
            prev = line;
            line = vec![];
        }
        else {
            line.push(c);
        }
    }

    let next = input.map(|w| w.1).take_while(|w| *w != '\n' && *w != '\r').collect::<String>();
    let line = line.into_iter().collect::<String>();
    let prev = prev.into_iter().collect::<String>();

    let underline = format!("{}{}", " ".repeat(underline_prefix_len), "-".repeat(m.end - m.start));

    [prev, line, underline, next].into_iter().filter(|x| x.len() > 0).collect::<Vec<_>>().join("\n")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn blarg() {
        let output = report_error("single\ndouble\ntriple".into(), Meta::single(7));
        assert_eq!(output, "");
    }
}