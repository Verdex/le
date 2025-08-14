
use std::rc::Rc;

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
pub enum Expr {
    LeString(Rc<str>),
    Number(Rc<str>), 
    Var(Rc<str>),
    Call { 
        fun_expr : Box<Expr>,
        args : Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Def {
    Fun {
        name : Rc<str>,
        params : Vec<Slot>,
        return_type : LeType,
        body : Expr,
    },
}

#[derive(Debug)]
pub enum DefOrExpr {
    Def(Def),
    Expr(Expr),
}