
use std::str::CharIndices as I;

#[derive(Debug)]
pub enum LexError {
    BlockCommentEof,
}

#[derive(Debug)]
pub enum Token {
    Number(Box<str>, usize, usize),
}

pub fn lex(input : &mut I) -> Result<Vec<Token>, LexError> {

    #[derive(PartialEq)]
    enum State {
        StartSlash,
        Idle,
    }

    let mut ts : Vec<Token> = vec![];

    let mut left_over : Option<(usize, char)> = None;
    let mut state = State::Idle;
    loop {
        match left_over.or_else(|| input.next()) {
            Some((_, '/')) if state == State::StartSlash => { 
                state = State::Idle;
                line_comment(input)?; 
            },
            Some((_, '/')) => { state = State::StartSlash; },
            Some((_, '*')) if state == State::StartSlash => { 
                state = State::Idle;
                block_comment(input)?;
            },
            Some((n, x)) if x.is_digit(10) || x == '-' => {
                state = State::Idle;
                let (lo, num) = number(n, x, input)?;
                left_over = lo;
                ts.push(num);
            },
            None if state == State::Idle => { break; },
            _ => todo!(),
        }
    }

    Ok(ts)
}

fn number(first : usize, init : char, input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    let mut last = 0;
    let mut left_over = None;
    let mut xs = vec![init];
    loop {
        match input.next() {
            Some((n, x)) if x.is_digit(10) => {
                last = n;
                xs.push(x);
            },
            Some((n, '.')) => { 
                last = n;
                xs.push('.');
            },
            None => { break; },
            x => { left_over = x; break; },
        }
    }

    Ok((left_over, Token::Number(xs.into_iter().collect::<String>().into(), first, last)))
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

    #[derive(PartialEq)]
    enum State {
        StartSlash,
        EndStar,
        Idle,
    }

    let mut state = State::Idle;
    let mut nest_level = 1;

    loop {
        match input.next() {
            Some((_, '*')) if state == State::StartSlash => {  
                state = State::Idle;
                nest_level += 1;
            },

            Some((_, '*')) => { state = State::EndStar; },

            Some((_, '/')) if state == State::EndStar => { 
                state = State::Idle;
                nest_level -= 1; 
            },

            Some((_, '/')) => { state = State::StartSlash; },

            None => {
                return Err(LexError::BlockCommentEof);
            },

            _ => {
                state = State::Idle;
            },
        }

        if nest_level == 0 {
            return Ok(());
        }
    }
}

#[cfg(test)] 
mod test {
    use super::*;

}