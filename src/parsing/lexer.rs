
use std::str::CharIndices as I;

#[derive(Debug)]
pub enum LexError {

}

#[derive(Debug)]
pub enum Token {

}

pub fn lex(input : &mut I) -> Result<Vec<Token>, LexError> {

    #[derive(PartialEq)]
    enum State {
        StartSlash,
        Idle,
    }

    let mut state = State::Idle;
    loop {
        match input.next() {
            Some((_, '/')) if state == State::StartSlash => { 
                state = State::Idle;
                line_comment(input)?; 
            },
            Some((_, '/')) => { state = State::StartSlash; },
            Some((_, '*')) if state == State::StartSlash => { 
                state = State::Idle;
                block_comment(input)?;
            },
            //Some((_, x)) if x.is_digit() => ,
            //Some((_, x)) if x.is_digit() => ,
            _ => todo!(),
        }
    }
}


fn line_comment(input : &mut I) -> Result<(), LexError> {
    loop {
        match input.next() {
            None => { return Ok(()); },
            Some((_, x)) if x == '\n' || x == '\r' => { return Ok(()); },
            _ => { },
        }
    }
}

fn block_comment(input : &mut I) -> Result<(), LexError> {

    todo!()
}

#[cfg(test)] 
mod test {
    use super::*;

}