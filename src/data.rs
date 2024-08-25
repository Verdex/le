
use dealize::pattern::*;

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

#[derive(Debug, PartialEq)]
pub enum Ast {
    Number(Box<str>), // TODO : should probably be a symbol instead of str
    Symbol(Box<str>),
    Slot { name : Box<Ast>, ttype : Box<Ast> },
    SimpleType(Box<Ast>),
    IndexType{ name : Box<Ast>, params : Box<Ast> },
    Variable(Box<Ast>),
    Call { 
        func_expr : Box<Ast>,
        inputs : Box<Ast>,
    },
    Function {
        name : Box<Ast>,
        params : Box<Ast>,
        return_type : Box<Ast>,
        body : Box<Ast>,
    },
    SyntaxList(Vec<Ast>),
}

impl Matchable for Ast {
    type Atom = Box<str>;

    fn kind<'a>(&'a self) -> MatchKind<'a, Self> {
        match self {
            Ast::Symbol(n) => MatchKind::Atom(n),
            Ast::SyntaxList(ls) => MatchKind::List(ls),
            Ast::Number(_) => MatchKind::Cons("number", vec![]),
            Ast::Slot { name, ttype } => MatchKind::Cons("slot", vec![&*name, ttype]),
            Ast::SimpleType(name) => MatchKind::Cons("simple-type", vec![&*name]),
            Ast::IndexType { name, params } => MatchKind::Cons("index-type", vec![&*name, params]),
            Ast::Variable(name) => MatchKind::Cons("variable", vec![&*name]),
            Ast::Call { func_expr, inputs } => MatchKind::Cons("call", vec![&*func_expr, inputs]),
            Ast::Function { name, params, return_type, body } => 
                MatchKind::Cons("function", vec![name, params, return_type, body]),
        }
    }
}

#[derive(Debug)]
pub enum Linear {
    
}