
use std::error::Error;
use std::cell::RefCell;
use std::rc::Rc;

use dealize::jerboa::{self, Rule};

use super::lexer::*;

#[derive(Debug)]
enum ParseError {

}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f : &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            //ParseError::?? => write!(f, "", ... ),
            _ => todo!(),
        }
    }
}

impl Error for ParseError { }


#[derive(Debug)]
pub enum Type {
    Simple(Box<str>),
    Index(Box<str>, Vec<Type>),
}

#[derive(Debug)]
pub struct Slot {
    name : Box<str>,
    ttype : Type,
}

#[derive(Debug)]
pub enum Ast {
    Number(Box<str>),
    Type(Type),
    Function {
        name : Box<str>,
        parameters : Vec<Slot>,
        return_type : Type,
        body : Vec<Ast>,
    },
}


thread_local!{
    static RULE : RefCell<Rc<Rule<Token, Ast>>> = RefCell::new(init_rules());
}

// TODO:  Use Jerboa
pub fn parse(input : Vec<Token>) -> Result<Vec<Ast>, Box<dyn Error>> {

    let _ = RULE.with_borrow(|rule| jerboa::parse(&input, Rc::clone(rule)));

    let ret = input.into_iter().map(|x| match x {
        Token::Number(n, s, e) => Ast::Number(n),
    }).collect::<Vec<Ast>>();

    Ok(ret)
}

fn init_rules() -> Rc<Rule<Token, Ast>> {
    todo!()
}