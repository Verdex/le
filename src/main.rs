
mod data;
mod parsing;

use std::rc::Rc;
use std::io::{self, Write};

use crate::parsing::lexer::{self, LexError};
use crate::parsing::parser::{self, ParseError};
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

        let tokens = match lexer::lex(Rc::clone(&input)) {
            Ok(tokens) => tokens,
            Err(e) => {

                let mut error_highlights = lex_error_locs(&e)
                    .into_iter()
                    .map(|x| report_error(Rc::clone(&input), x, x))
                    .collect::<Vec<_>>();

                error_highlights.sort();
                error_highlights.dedup();
                
                let error_highlight = error_highlights.join("\n");

                panic!("encountered lexing error:\n{error_highlight}\n\n{e}");
            }
        };

        let ast = match parser::parse(tokens) {
            Ok(ast) => ast,
            // TODO if you hit an unexpected end of file then you
            // can wait for more input, try again, etc
            //Err(ParseError::UnexpectedEof) => { },
            Err(e) => {
                let mut error_highlights = parse_error_locs(&e)
                    .into_iter()
                    .map(|x| report_error(Rc::clone(&input), x.start, x.end))
                    .collect::<Vec<_>>();

                error_highlights.sort();
                error_highlights.dedup();
                
                let error_highlight = error_highlights.join("\n");

                panic!("encountered parsing error:\n{error_highlight}\n\n{e}");
            }
        };


        println!("{:?}", ast);


    }
}

fn read() -> io::Result<Rc<str>> {
    let mut s = String::new();
    io::stdin().read_line(&mut s)?;
    Ok(s.into())
}

fn lex_error_locs(error : &LexError) -> Vec<usize> {
    match error {
        LexError::StringEof => vec![],
        LexError::BlockCommentEof => vec![],
        LexError::UnexpectedEof => vec![], 
        LexError::UnknownEscape(_, n) => vec![*n],
        LexError::UnexpectedChar(_, _, loc) => vec![*loc],
        LexError::NumberWithMultipleDots(_, loc) => vec![*loc],
        LexError::NegativeNumberNeedsDigits(loc) => vec![*loc],
        LexError::Aggregate(errors) => 
            errors.into_iter().flat_map(lex_error_locs).collect(),
    }
}

fn parse_error_locs(error : &ParseError) -> Vec<Meta> {
    match error {
        ParseError::UnexpectedToken(t) => {
            let m = t.meta();
            vec![m]
        },
        ParseError::UnexpectedEof => vec![],
        ParseError::Aggregate(errors) => 
            errors.into_iter().flat_map(parse_error_locs).collect(),
    }
}

fn report_error(input : Rc<str>, start : usize, end : usize) -> String {
    let mut prev = vec![];
    let mut line = vec![];
    let mut prev_line_end_index = 0;

    let mut input = input.char_indices();
    let mut underline_prefix_len = 0;

    while let Some((index, c)) = input.next() {
        if index == start {
            underline_prefix_len = line.len();
        }

        if c == '\n' || c == '\r' {
            if prev_line_end_index <= start && end <= index {
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

    let underline = format!("{}{}", " ".repeat(underline_prefix_len), "-".repeat(end - start + 1));

    [prev, line, underline, next].into_iter().filter(|x| x.len() > 0).collect::<Vec<_>>().join("\n")
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn should_handle_initial_line_failure() {
        let output = report_error("single\ndouble\ntriple".into(), 1, 1);
        assert_eq!(output, "single\n -\ndouble");
    }

    #[test]
    fn should_handle_final_line_failure() {
        let output = report_error("single\ndouble\ntriple".into(), 14, 14);
        assert_eq!(output, "double\ntriple\n-");
    }

    #[test]
    fn should_handle_middle_line_failure() {
        let output = report_error("single\ndouble\ntriple".into(), 8, 8);
        assert_eq!(output, "single\ndouble\n -\ntriple");
    }

    #[test]
    fn should_handle_range_underline() {
        let output = report_error("single\ndouble\ntriple".into(), 9, 11);
        assert_eq!(output, "single\ndouble\n  ---\ntriple");
    }
}