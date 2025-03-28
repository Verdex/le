
use super::buffer::Buffer;
use crate::data::{ Meta, Token };

macro_rules! lex_char {
    ($name:ident, $target : literal) => {
        fn $name(input : &mut Buffer<char>) -> Result<char, LexError> {
            let index = input.index();
            let c = input.get(LexError::UnexpectedEof)?;
            if *c == $target {
                Ok(*c)
            }
            else {
                Err(LexError::UnexpectedChar($target.to_string().into(), *c, index))
            }
        }
    }
}

macro_rules! punct {
    ($name:ident, $target:literal, $result:expr) => {
        fn $name(input : &mut Buffer<char>) -> Result<Token, LexError> {
            lex_char!(lexer, $target);

            let index = input.index();
            lexer(input)?;
            Ok($result(index))
        }
    }
}

#[derive(Debug)]
pub enum LexError {
    BlockCommentEof,
    StringEof, 
    UnexpectedEof,
    UnknownEscape(char, usize),
    UnexpectedChar(Box<str>, char, usize),
    NumberWithMultipleDots(Box<str>, usize),
    NegativeNumberNeedsDigits(usize),
    Aggregate(Vec<LexError>),
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexError::StringEof => write!(f, "end of file encountered mid string"),
            LexError::BlockCommentEof => write!(f, "end of file encountered mid block comment"),
            LexError::UnexpectedEof => write!(f, "unexpected end of file encountered"),
            LexError::UnknownEscape(c, n) => write!(f, "unknown escape character {} found in string at {}", c, n),
            LexError::UnexpectedChar(expected, unknown, loc) => write!(f, "expected {} but found {} at {}", expected, unknown, loc),
            LexError::NumberWithMultipleDots(number, loc) => 
                write!(f, "encountered number with too many decimal points: {} at {}", number, loc),
            LexError::NegativeNumberNeedsDigits(loc) => write!(f, "a negative number needs digits at: {}", loc),
            LexError::Aggregate(errors) => write!(f, "encountered error list:\n{}", 
                errors.into_iter().map(|x| format!("  {}\n", x)).collect::<Vec<_>>().join("")),
        }
    }
}

impl From<Vec<LexError>> for LexError {
    fn from(item : Vec<LexError>) -> Self {
        LexError::Aggregate(item)
    }
}

impl std::error::Error for LexError { }

pub fn lex(input : Box<str>) -> Result<Vec<Token>, LexError> {
    let input = input.chars().collect::<Vec<_>>();
    let mut buffer = Buffer::new(&input);

    let mut tokens = vec![];

    while !buffer.end() {
        whitespace(&mut buffer);
        line_comment(&mut buffer);
        block_comment(&mut buffer)?;

        if buffer.end() { break; }

        let token = buffer.or(
            [
                symbol, 
                lparen,
                rparen,
                dot,
                triangle,
                comma,
                semicolon,
                colon,
                r2arrow,
                rarrow,
                equal,
                lsquare,
                rsquare,
                lcurl,
                rcurl,
                langle,
                rangle,
                number,
            ])?;
        tokens.push(token);
    }

    Ok(tokens)

    /*let mut ts : Vec<Token> = vec![];

    let mut left_over : Option<(usize, char)> = None;
    loop {
        match left_over.or_else(|| input.next()) {
            Some((_, c)) if c.is_whitespace() => {
                left_over = whitespace(input)?;
            },
            Some((n, c)) if c.is_alphabetic() || c == '_' => {
                let (lo, num) = symbol(n, c, input)?;
                left_over = lo;
                ts.push(num);
            },
            Some((_, '/')) => {
                line_or_block_comment(input)?;
                left_over = None;
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
            Some((n, '|')) => {
                let x = triangle(n, input)?;
                left_over = None;
                ts.push(x);
            },
            Some((n, x)) if x.is_digit(10) || x == '-' => {
                let (lo, num) = number_or_right_arrow(n, x, input)?;
                left_over = lo;
                ts.push(num);
            },
            Some((n, '"')) => {
                let s = string(n, input)?;
                left_over = None;
                ts.push(s);
            },
            None => { break; },
            Some((n, x)) => { return Err(LexError::UnexpectedChar(x, n)); },
        }
    }

    Ok(ts)*/
}

punct!(lsquare, '[', |index| Token::LSquare(Meta::single(index)));
punct!(rsquare, ']', |index| Token::RSquare(Meta::single(index)));
punct!(lcurl, '{', |index| Token::LCurl(Meta::single(index)));
punct!(rcurl, '}', |index| Token::RCurl(Meta::single(index)));
punct!(langle, '<', |index| Token::LAngle(Meta::single(index)));
punct!(rangle, '>', |index| Token::RAngle(Meta::single(index)));
punct!(lparen, '(', |index| Token::LParen(Meta::single(index)));
punct!(rparen, ')', |index| Token::RParen(Meta::single(index)));
punct!(dot, '.', |index| Token::Dot(Meta::single(index)));
punct!(comma, ',', |index| Token::Comma(Meta::single(index)));
punct!(semicolon, ';', |index| Token::Semicolon(Meta::single(index)));
punct!(colon, ':', |index| Token::Colon(Meta::single(index)));
punct!(equal, '=', |index| Token::Equal(Meta::single(index)));

fn r2arrow(input : &mut Buffer<char>) -> Result<Token, LexError> {
    lex_char!(minus, '=');
    lex_char!(rangle, '>');

    let index = input.index();
    minus(input)?;
    rangle(input)?;
    Ok(Token::R2Arrow(Meta::range(index, index + 1)))
}

fn rarrow(input : &mut Buffer<char>) -> Result<Token, LexError> {
    lex_char!(minus, '-');
    lex_char!(rangle, '>');

    let index = input.index();
    minus(input)?;
    rangle(input)?;
    Ok(Token::RArrow(Meta::range(index, index + 1)))
}

fn triangle(input : &mut Buffer<char>) -> Result<Token, LexError> {
    lex_char!(bar, '|');
    lex_char!(rangle, '>');

    let index = input.index();
    bar(input)?;
    let ds = input.list(digit)?;
    let number : usize = if ds.len() > 0 {
        ds.iter().collect::<String>().parse().unwrap()
    }
    else {
        0
    };
    rangle(input)?;

    Ok(Token::Triangle(number, Meta::range(index, index + 1 + ds.len())))
}

fn symbol(input : &mut Buffer<char>) -> Result<Token, LexError> {
    let start = input.index();
    let first = input.or([letter, underscore])?;
    let mut rest = input.list(|input| input.or([letter, digit, underscore]))?;
    rest.insert(0, first);

    let s : Box<str> = rest.into_iter().collect();
    let len = s.len();
    Ok(Token::Symbol(s, Meta::range(start, start + len)))
}

fn number(input : &mut Buffer<char>) -> Result<Token, LexError> {
    lex_char!(dot, '.');

    let start = input.index();
    let first = input.or([digit, minus])?;
    let mut rest = input.list(|input| input.or([digit, dot]))?;
    rest.insert(0, first);

    if rest.iter().filter(|x| **x == '.').count() > 1 {
        Err(LexError::NumberWithMultipleDots(rest.into_iter().collect(), start))
    }
    else if rest.len() == 1 && first == '-' {
        Err(LexError::NegativeNumberNeedsDigits(start))
    }
    else {
        let n : Box<str> = rest.into_iter().collect();
        let len = n.len();
        Ok(Token::Number(n, Meta::range(start, start + len)))
    }
}

fn line_comment(input : &mut Buffer<char>) {
    lex_char!(slash, '/');

    let _ : Result<(), LexError> = input.with_rollback(|input| {

        slash(input)?;
        slash(input)?;

        loop {
            match input.get(()) {
                Ok('\n' | '\r') => { break; },
                Err(_) => { break; },
                _ => { },
            }
        }

        Ok(())
    });
}

fn block_comment(input : &mut Buffer<char>) -> Result<(), LexError> {
    lex_char!(slash, '/');
    lex_char!(star, '*');

    enum State {
        StartSlash,
        EndStar,
        Idle,
    }

    let result : Result<(), LexError> = input.with_rollback(|input| {

        slash(input)?;
        star(input)?;

        let mut state = State::Idle;
        let mut nest_level = 1;

        loop {
            match (input.get(LexError::BlockCommentEof)?, state) {
                ('*', State::StartSlash) => {  
                    state = State::Idle;
                    nest_level += 1;
                },

                ('*', _) => { state = State::EndStar; },

                ('/', State::EndStar) => { 
                    state = State::Idle;
                    nest_level -= 1; 
                },

                ('/', _) => { state = State::StartSlash; },

                _ => {
                    state = State::Idle;
                },
            }

            if nest_level == 0 {
                return Ok::<(), LexError>(());
            }
        }
    });

    match result {
        Ok(_) => Ok(()),
        Err(e @ LexError::BlockCommentEof) => Err(e),
        Err(_) => Ok(()),
    }
}

lex_char!(underscore, '_');
lex_char!(minus, '-');

fn letter(input : &mut Buffer<char>) -> Result<char, LexError> {
    let index = input.index();
    let c = input.get(LexError::UnexpectedEof)?;
    if c.is_alphabetic() {
        Ok(*c)
    }
    else {
        Err(LexError::UnexpectedChar("[a-zA-Z]".into(), *c, index))
    }
}

fn digit(input : &mut Buffer<char>) -> Result<char, LexError> {
    let index = input.index();
    let c = input.get(LexError::UnexpectedEof)?;
    if c.is_digit(10) {
        Ok(*c)
    }
    else {
        Err(LexError::UnexpectedChar("[0-9]".into(), *c, index))
    }
}

fn whitespace(input : &mut Buffer<char>) {
    loop {
        let result = input.with_rollback(|input|
            if input.get(LexError::UnexpectedEof)?.is_whitespace() {
                Ok(())
            }
            else {
                Err(LexError::UnexpectedEof)
            }
        );

        if result.is_err() {
            break;
        }
    }
}

/*
fn string(first : usize, input : &mut I) -> Result<Token, LexError> {
    let last;
    let mut xs = vec![];
    let mut escape = false;
    loop {
        match input.next() {
            None => { return Err(LexError::StringEof); },
            Some((_, '"')) if escape => {
                xs.push('"');
                escape = false;
            },
            Some((_, '0')) if escape => {
                xs.push('\0');
                escape = false;
            },
            Some((_, 't')) if escape => {
                xs.push('\t');
                escape = false;
            },
            Some((_, 'r')) if escape => {
                xs.push('\r');
                escape = false;
            },
            Some((_, 'n')) if escape => {
                xs.push('\n');
                escape = false;
            },
            Some((_, '\\')) if escape => {
                xs.push('\\');
                escape = false;
            },
            Some((_, '\\')) => {
                escape = true;
            },
            Some((n, c)) if escape => { return Err(LexError::UnknownEscape(c, n)); },
            Some((n, '"')) => { 
                last = n; 
                break; 
            },
            Some((_, c)) => {
                xs.push(c);
            },
        }
    }

    Ok(Token::String(xs.into_iter().collect::<String>().into(), first, last))
}

fn triangle(first : usize, input : &mut I) -> Result<Token, LexError> {
    match input.next() {
        None => Err(LexError::UnexpectedEof('|')),
        Some((n, '>')) => Ok(Token::Triangle{ param: 0, start : first, end: n}),
        Some((_, x)) if x.is_digit(10) => {
            let mut ds = vec![x];
            let end = loop {
                match input.next() {
                    None => { return Err(LexError::UnexpectedEof(ds[ds.len() - 1])); },
                    Some((_, x)) if x.is_digit(10) => { ds.push(x); },
                    Some((end, '>')) => { break end; },
                    Some((n, c)) => { 
                        return Err(LexError::UnexpectedUnknown { before: ds[ds.len() - 1], unexpected: c, loc: n}); 
                    },
                }
            };
            let param = ds.into_iter().collect::<String>().parse::<usize>().unwrap();
            Ok(Token::Triangle { param, start: first, end })
        },
        Some((n, c)) => { Err(LexError::UnexpectedUnknown { before: '|', unexpected: c, loc: n}) },
    }
}

fn equal_or_right_2_arrow(first : usize, input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    match input.next() {
        None => Err(LexError::UnexpectedEof('=')),
        Some((n, '>')) => Ok((None, Token::R2Arrow(first, n))),
        Some(x) => Ok((Some(x), Token::Equal(first))),
    }
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
            Some((loc, unexpected))=> Err(LexError::UnexpectedUnknown{before: '-', unexpected, loc}),
        }
    }
}

fn symbol(first : usize, init : char, input : &mut I) -> Result<(Option<(usize, char)>, Token), LexError> {
    let mut last = first;
    let mut left_over = None;
    let mut xs = vec![init];
    loop { 
        match input.next() {
            Some((n, c)) if c.is_alphabetic() || c == '_' || c.is_digit(10) => {
                last = n;
                xs.push(c);
            },
            None => { break; },
            x => { left_over = x; break; },
        }
    }

    Ok((left_over, Token::Symbol(xs.into_iter().collect::<String>().into(), first, last)))
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
*/
#[cfg(test)] 
mod test {
    use super::*;

    fn proj_num(input : &Token) -> String {
        match input {
            Token::Number(x, _) => x.to_string(),
            _ => panic!("not a number"),
        }
    }

    fn proj_sym(input : &Token) -> String {
        match input {
            Token::Symbol(x, _) => x.to_string(),
            _ => panic!("not a symbol"),
        }
    }

    fn proj_str(input : &Token) -> String {
        match input {
            Token::String(x, _) => x.to_string(),
            _ => panic!("not a string"),
        }
    }

    #[test]
    fn should_lex_number() {
        let output = lex("100".into()).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "100");
    }

    #[test]
    fn should_lex_negative_number() {
        let output = lex("-100".into()).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "-100");
    }

    #[test]
    fn should_lex_decimal_number() {
        let output = lex("100.25".into()).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "100.25");
    }

    #[test]
    fn should_lex_negative_decimal_number() {
        let output = lex("-100.25".into()).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "-100.25");
    }

    #[test]
    fn should_lex_line_comment() {
        let output = lex("// ~~~~ ".into()).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_block_comment() {
        let input = "/* ~~
        ~~ */";
        let output = lex(input.into()).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_nested_block_comment() {
        let mut input = "/* ~
        ~/*~~*/~ */";
        let output = lex(input.into()).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_whitespace_ending_in_eof() {
        let output = lex("   ".into()).unwrap();

        assert_eq!(output.len(), 0);
    }

    #[test]
    fn should_lex_whitespace_ending_in_number() {
        let output = lex("   8".into()).unwrap();

        assert_eq!(output.len(), 1);
        assert_eq!(proj_num(&output[0]), "8");
    }
    // TODO Sci Notation tests

    #[test]
    fn should_lex_number_followed_by_r_arrow() {
        let output = lex("25.->".into()).unwrap();

        assert_eq!(output.len(), 2);
        assert!(matches!(output[0], Token::Number(_, _)));
        assert!(matches!(output[1], Token::RArrow(_)));

        assert_eq!(proj_num(&output[0]), "25.");
    }

    #[test]
    fn should_lex_punct() {
        let output = lex("->==>()[]{}<>,:;.".into()).unwrap();

        assert_eq!(output.len(), 15);
        assert!(matches!(output[0], Token::RArrow(_)));
        assert!(matches!(output[1], Token::Equal(_)));
        assert!(matches!(output[2], Token::R2Arrow(_)));
        assert!(matches!(output[3], Token::LParen(_)));
        assert!(matches!(output[4], Token::RParen(_)));
        assert!(matches!(output[5], Token::LSquare(_)));
        assert!(matches!(output[6], Token::RSquare(_)));
        assert!(matches!(output[7], Token::LCurl(_)));
        assert!(matches!(output[8], Token::RCurl(_)));
        assert!(matches!(output[9], Token::LAngle(_)));
        assert!(matches!(output[10], Token::RAngle(_)));
        assert!(matches!(output[11], Token::Comma(_)));
        assert!(matches!(output[12], Token::Colon(_)));
        assert!(matches!(output[13], Token::Semicolon(_)));
        assert!(matches!(output[14], Token::Dot(_)));
    }

    #[test]
    fn should_lex_triangle() {
        fn a(t : &Token, p : usize) {
            match t {
                Token::Triangle(param, _) => {
                    assert_eq!(*param, p);
                },
                _ => { panic!("Encountered unexpected token"); },
            }
        }

        let output = lex("|> |1> |10> |100>".into()).unwrap();

        assert_eq!(output.len(), 4);
        a(&output[0], 0);
        a(&output[1], 1);
        a(&output[2], 10);
        a(&output[3], 100);
    }

    #[test]
    fn should_lex_symbol() {
        let output = lex("symbol. _symbol _1 Symbol_8".into()).unwrap();

        assert_eq!(output.len(), 5);
        assert_eq!(proj_sym(&output[0]), "symbol");
        assert!(matches!(output[1], Token::Dot(_)));
        assert_eq!(proj_sym(&output[2]), "_symbol");
        assert_eq!(proj_sym(&output[3]), "_1");
        assert_eq!(proj_sym(&output[4]), "Symbol_8");
    }

    #[test]
    fn should_lex_string() {
        let output = lex(r#" "string !@#$%^&* \n\t\r\0\\\"". "#.into()).unwrap();

        assert_eq!(output.len(), 2);
        assert_eq!(proj_str(&output[0]), "string !@#$%^&* \n\t\r\0\\\"");
        assert!(matches!(output[1], Token::Dot(_)));
    }
}