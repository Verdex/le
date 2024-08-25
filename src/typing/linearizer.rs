

use crate::data::{Linear, Ast};

pub fn linearize(ast : Vec<Ast>) -> Result<Vec<Linear>, ()> {
    ast.into_iter().map(to_linear).collect()
}

fn to_linear(ast : Ast) -> Result<Linear, ()> {
    todo!()
}