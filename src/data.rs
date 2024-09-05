
use dealize::pattern::*;
use dealize::seq::Seqable;

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

impl Token { 
    pub fn error(&self) -> (usize, usize) {
        match self {
            Token::Number(_, s, e) => (*s, *e),
            Token::Symbol(_, s, e) => (*s, *e),
            Token::String(_, s, e) => (*s, *e),
            Token::LSquare(x) => (*x, *x),
            Token::RSquare(x) => (*x, *x),
            Token::LCurl(x) => (*x, *x),
            Token::RCurl(x) => (*x, *x),
            Token::LParen(x) => (*x, *x),
            Token::RParen(x) => (*x, *x),
            Token::LAngle(x) => (*x, *x),
            Token::RAngle(x) => (*x, *x),
            Token::Dot(x) => (*x, *x),
            Token::Comma(x) => (*x, *x),
            Token::Semicolon(x) => (*x, *x),
            Token::Colon(x) => (*x, *x),
            Token::Equal(x) => (*x, *x),
            Token::RArrow(s, e) => (*s, *e),
            Token::R2Arrow(s, e) => (*s, *e),
            Token::Triangle { start, end, .. } => (*start, *end), 
        }
    }
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

// Linear

/*
#[derive(Debug)]
pub enum Type {
    Simple(Box<str>),
    Index { name : Box<str>, params : Vec<Type> },
}

#[derive(Debug)]
pub enum Val {
    Num(f64),
}

#[derive(Debug)] 
pub enum Sym {
    Name(Box<str>),
    Gen(Box<str>, usize),
}

#[derive(Debug)]
pub enum FunAddr { 
    Global(Box<str>),
    Var(Box<str>),
    Anon(usize),
}

#[derive(Debug)]
pub enum Stmt {
    SetNameVar(Box<str>, Val),
    SetAnonVar(usize, Val),
    ReturnNameVar(Box<str>),
    ReturnAnonVar(usize),
    Call(FunAddr, Vec<Box<str>>),
}

#[derive(Debug)]
pub enum Linear {
    Fun { name : Sym, params : Vec<(Box<str>, Type)>, return_type : Type, body : Vec<Stmt> }
}

*/

pub mod vm {
    use std::rc::Rc;

    #[derive(Debug, Clone, Copy)]
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