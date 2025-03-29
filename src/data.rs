
use dealize::pattern::*;
use dealize::seq::Seqable;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Token {
    Number(Box<str>, Meta),
    Symbol(Box<str>, Meta),
    String(Box<str>, Meta),
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
    Number(Box<str>), 
    Symbol(Box<str>),
    Slot { name : Box<Ast>, ttype : Box<Ast> },
    SimpleType(Box<Ast>),
    IndexType{ name : Box<Ast>, params : Box<Ast> },
    Variable(Box<Ast>),
    Call { 
        fun_expr : Box<Ast>,
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
            Ast::Call { fun_expr, inputs } => MatchKind::Cons("call", vec![&*fun_expr, inputs]),
            Ast::Function { name, params, return_type, body } => 
                MatchKind::Cons("function", vec![name, params, return_type, body]),
        }
    }
}

impl<'a> Seqable<'a> for Ast {
    fn seq_next(&self) -> Vec<&Self> {
        match self {
            Ast::Symbol(_) => vec![],
            Ast::SyntaxList(ls) => ls.iter().collect(),
            Ast::Number(_) => vec![],
            Ast::Slot { name, ttype } => vec![&*name, ttype],
            Ast::SimpleType(name) => vec![&*name],
            Ast::IndexType { name, params } => vec![&*name, params],
            Ast::Variable(name) => vec![&*name],
            Ast::Call { fun_expr, inputs } => vec![&*fun_expr, inputs],
            Ast::Function { name, params, return_type, body } => vec![name, params, return_type, body],
        }
    }
}

pub mod vm {
    use std::rc::Rc;

    #[derive(Debug, Clone)]
    pub enum LAddr {
        Local(usize),
        Env(usize),
    }

    #[derive(Debug, Clone)]
    pub enum Lit {
        Float(f64),
        Ref(LAddr),
        Unit,
    }

    #[derive(Debug)]
    pub enum Stmt {
        Deref(LAddr, usize),
        Add(LAddr, LAddr),
        Cons(Vec<Lit>),
        Return(LAddr),
        Call(Rc<Fun>, Vec<LAddr>),
        DPrint(Vec<LAddr>),
    }

    #[derive(Debug)]
    pub struct Fun { 
        pub name : Box<str>,
        pub body : Vec<Stmt>,
    }
}