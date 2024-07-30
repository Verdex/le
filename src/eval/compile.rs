
use crate::parsing::parser::Ast;

pub enum Ir {
    Number(f64),
}

pub fn to_interpreter(ast : Vec<Ast>) -> Vec<Ir> {

}