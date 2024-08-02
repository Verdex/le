
use std::str::CharIndices as I;

#[derive(Debug)]
pub enum LexError {
    BlockCommentEof,
    UnexpectedEof(char),
    UnexpectedUnknown{ before: char, unexpected : char, loc : usize },
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexError::BlockCommentEof => write!(f, "end of file encountered mid block comment"),
            LexError::UnexpectedEof(c) => write!(f, "end of file encountered after '{}'", c),
            LexError::UnexpectedUnknown { before, unexpected, loc } 
                => write!(f, "{} followed by unexpected {} at {}", before, unexpected, loc),
        }
    }
}

impl std::error::Error for LexError { }

#[derive(Debug)]
pub enum Token {
    Number(Box<str>, usize, usize),
    Symbol(Box<str>, usize, usize),
    String(Box<str>, usize, usize),
    LSquare(usize),
    RSquare(usize),
    LCurl(usize),
    RCurl(usize),
    LParen(usize),
    RParen(usize),
    LAngle(usize),
    RAngle(usize),
    Dot(usize),
    Comma(usize),
    Semicolon(usize),
    Colon(usize),
    Equal(usize),
    RArrow(usize, usize),
    R2Arrow(usize, usize),
    Triangle(usize, usize, usize), // TODO |> |2>
}

pub fn lex(input : &mut I) -> Result<Vec<Token>, LexError> {

    let mut ts : Vec<Token> = vec![];

    let mut left_over : Option<(usize, char)> = None;
    loop {
        match left_over.or_else(|| input.next()) {
            Some((_, c)) if c.is_whitespace() => {
                left_over = whitespace(input)?;
            },
            Some((_, '/')) => {
                line_or_block_comment(input)?;
            },
            Some((n, '(')) => {
                ts.push(Token::LParen(n));
                left_over = None;
            },
            Some((n, ')')) => {
                ts.push(Token::RParen(n));
                left_over = None;
            },
            Some((n, '<')) => {
                ts.push(Token::LAngle(n));
                left_over = None;
            },
            Some((n, '>')) => {
                ts.push(Token::RAngle(n));
                left_over = None;
            },
            Some((n, '{')) => {
                ts.push(Token::LCurl(n));
                left_over = None;
            },
            Some((n, '}')) => {
                ts.push(Token::RCurl(n));
                left_over = None;
            },
            Some((n, '[')) => {
                ts.push(Token::LSquare(n));
                left_over = None;
            },
            Some((n, ']')) => {
                ts.push(Token::RSquare(n));
                left_over = None;
            },
            Some((n, ':')) => {
                ts.push(Token::Colon(n));
                left_over = None;
            },
            Some((n, ';')) => {
                ts.push(Token::Semicolon(n));
                left_over = None;
            },
            Some((n, '.')) => {
                ts.push(Token::Dot(n));
                left_over = None;
            },
            Some((n, ',')) => {
                ts.push(Token::Comma(n));
                left_over = None;
            },
            Some((n, '=')) => {
                let (lo, x) = equal_or_right_2_arrow(n, input)?;
                left_over = lo;
                ts.push(x);
            },
            Some((n, x)) if x.is_digit(10) || x == '-' => {
                let (lo, num) = number_or_right_arrow(n, x, input)?;
                left_over = lo;
                ts.push(num);
            },
            None => { break; },
            _ => todo!(),
        }
    }

    Ok(ts)
}

fn equal_or_right_2_arrow(first : usize, input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    todo!()
}

fn number_or_right_arrow(first : usize, init : char, input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    if init != '-' {
        number(first, (init, None), input)
    }
    else {
        match input.next() {
            None => Err(LexError::UnexpectedEof('-')),
            Some(x @ (_, c)) if c.is_digit(10) || c == '.' => number(first, (init, Some(x)), input),
            Some((n, c)) if c == '>' => Ok((None, Token::RArrow(first, n))),
            _ => todo!(),
        }
    }
}

fn number(first : usize, init : (char, Option<(usize, char)>), input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    let mut last = first;
    let mut left_over = None;
    let mut xs = vec![init.0];
    if init.1.is_some() {
        let (n, c) = init.1.unwrap();
        xs.push(c);
        last = n;
    }
    loop {
        // TODO sci not
        // TODO make sure multiple .s don't show up
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

fn whitespace(input : &mut I) -> Result<Option<(usize, char)>, LexError> {
    loop {
        match input.next() {
            None => { return Ok(None); },
            Some((_, c)) if c.is_whitespace() => { },
            Some(x) => { return Ok(Some(x)); },
        }
    }
}

fn line_or_block_comment(input : &mut I) -> Result<(), LexError> {
    match input.next() {
        None => Err(LexError::UnexpectedEof('/')),
        Some((_, '*')) => block_comment(input),
        Some((_, '/')) => line_comment(input),
        Some((n, c)) => Err(LexError::UnexpectedUnknown { before: '/', unexpected: c, loc: n }),
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

    fn proj_num(input : &Token) -> String {
        match input {
            Token::Number(x, _, _) => x.to_string(),
            _ => panic!("not a number"),
        }
    }

    #[test]
    fn should_lex_number() {
        let mut input = "100".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "100");
    }

    #[test]
    fn should_lex_negative_number() {
        let mut input = "-100".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "-100");
    }

    #[test]
    fn should_lex_decimal_number() {
        let mut input = "100.25".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "100.25");
    }

    #[test]
    fn should_lex_negative_decimal_number() {
        let mut input = "-100.25".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "-100.25");
    }

    #[test]
    fn should_lex_line_comment() {
        let mut input = "// ~~~~ ".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_block_comment() {
        let mut input = "/* ~~
        ~~ */".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_nested_block_comment() {
        let mut input = "/* ~
        ~/*~~*/~ */".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_whitespace_ending_in_eof() {
        let mut input = "   ".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_whitespace_ending_in_number() {
        let mut input = "   8".char_indices();
        let output = lex(&mut input).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "8");
    }
    // TODO Sci Notation tests
}