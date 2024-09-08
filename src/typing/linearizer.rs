
use std::collections::HashSet;
use crate::data::{Ast, vm};


pub fn linearize(ast : Vec<Ast>) -> Vec<vm::Fun> {
    ast.into_iter().map(to_linear).collect()
}

// Note:: Top level Ast is allowed to be Expr even though
// the intepreter won't accept a top level Expr because
// a REPL will want to parse Expr and then wrap in an
// artifical main function.
fn to_linear(ast : Ast) -> vm::Fun {
    match ast {
        Ast::Number(s) => todo!(),
        Ast::Variable(sym) => todo!(),
        Ast::Call { fun_expr, inputs } => {
            // TODO : env will need to be pre-existing items for repl runs
            todo!()
        },

        // name : symbol
        // params : syntax list of slots
        // return_type : index type or simple type
        // body : expr
        Ast::Function { name, params, return_type, body } => todo!(),
        _ => todo!(),
    }
}


/*
fn repl_main_fun_wrapper(body : Vec<Stmt>) -> Linear {
    Linear::Fun { name : gen_sym("repl_main_wrapper")
                , params : vec![] 
                , return_type : Type::Simple("unit".into())
                , body 
                }
}
*/

fn anon() -> usize {
    static mut index : usize = 0;
    unsafe { let v = index; index += 1; v }
}

#[cfg(test)]
mod test {
    use super::*;

}