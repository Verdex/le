
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Meta {
    pub start : usize,
    pub end : usize,
}

impl Meta { 
    pub fn range(start : usize, end : usize) -> Self {
        Meta { start, end }
    }
    pub fn single(start : usize) -> Self {
        Meta { start, end: start }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Number(Rc<str>, Meta),
    Symbol(Rc<str>, Meta),
    String(Rc<str>, Meta),
    LSquare(Meta),
    RSquare(Meta),
    LCurl(Meta),
    RCurl(Meta),
    LParen(Meta),
    RParen(Meta),
    LAngle(Meta),
    RAngle(Meta),
    Dot(Meta),
    Comma(Meta),
    Semicolon(Meta),
    Colon(Meta),
    Equal(Meta),
    RArrow(Meta),
    R2Arrow(Meta),
    Triangle(usize, Meta), 
}

impl Token { 
    pub fn error(&self) -> (usize, usize) {
        match self {
            Token::Number(_, m) => (m.start, m.end),
            Token::Symbol(_, m) => (m.start, m.end),
            Token::String(_, m) => (m.start, m.end),
            Token::LSquare(m) => (m.start, m.end),
            Token::RSquare(m) => (m.start, m.end),
            Token::LCurl(m) => (m.start, m.end),
            Token::RCurl(m) => (m.start, m.end),
            Token::LParen(m) => (m.start, m.end),
            Token::RParen(m) => (m.start, m.end),
            Token::LAngle(m) => (m.start, m.end),
            Token::RAngle(m) => (m.start, m.end),
            Token::Dot(m) => (m.start, m.end),
            Token::Comma(m) => (m.start, m.end),
            Token::Semicolon(m) => (m.start, m.end),
            Token::Colon(m) => (m.start, m.end),
            Token::Equal(m) => (m.start, m.end),
            Token::RArrow(m) => (m.start, m.end),
            Token::R2Arrow(m) => (m.start, m.end),
            Token::Triangle(_, m) => (m.start, m.end), 
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    Number(Rc<str>), 
    Slot { name : Rc<str>, ttype : Box<Ast> },
    SimpleType(Rc<str>),
    IndexType{ name : Rc<str>, params : Vec<Ast> },
    Variable(Rc<str>),
    Call { 
        fun_expr : Box<Ast>,
        inputs : Vec<Ast>,
    },
    Function {
        name : Rc<str>,
        params : Vec<Ast>,
        return_type : Box<Ast>,
        body : Box<Ast>,
    },
}