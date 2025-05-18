
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
    pub fn meta(&self) -> Meta {
        match self {
            Token::Number(_, m) => m.clone(),
            Token::Symbol(_, m) => m.clone(),
            Token::String(_, m) => m.clone(),
            Token::LSquare(m) => m.clone(),
            Token::RSquare(m) => m.clone(),
            Token::LCurl(m) => m.clone(),
            Token::RCurl(m) => m.clone(),
            Token::LParen(m) => m.clone(),
            Token::RParen(m) => m.clone(),
            Token::LAngle(m) => m.clone(),
            Token::RAngle(m) => m.clone(),
            Token::Dot(m) => m.clone(),
            Token::Comma(m) => m.clone(),
            Token::Semicolon(m) => m.clone(),
            Token::Colon(m) => m.clone(),
            Token::Equal(m) => m.clone(),
            Token::RArrow(m) => m.clone(),
            Token::R2Arrow(m) => m.clone(),
            Token::Triangle(_, m) => m.clone(),
        }
    }
}

#[derive(Debug)]
pub enum LeType {
    SimpleType(Rc<str>),
    IndexType { name : Rc<str>, params : Vec<LeType> },
}

#[derive(Debug)]
pub struct Slot {
    pub name : Rc<str>,
    pub ttype : LeType,
}

#[derive(Debug)]
pub enum Ast {
    Number(Rc<str>), 
    Var(Rc<str>),
    Call { 
        fun_expr : Box<Ast>,
        args : Vec<Ast>,
    },
    Fun {
        name : Rc<str>,
        params : Vec<Slot>,
        return_type : LeType,
        body : Box<Ast>,
    },
}