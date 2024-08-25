
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
    Triangle { param : usize, start : usize, end : usize }, 
}