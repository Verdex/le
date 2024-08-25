

use crate::data::{Ast, Linear, Type, Val, Sym, FunAddr, Stmt};


pub fn linearize(ast : Vec<Ast>) -> Result<Vec<Linear>, ()> {
    ast.into_iter().map(to_linear).collect()
}

// Note:: Top level Ast is allowed to be Expr even though
// the intepreter won't accept a top level Expr because
// a REPL will want to parse Expr and then wrap in an
// artifical main function.
fn to_linear(ast : Ast) -> Result<Linear, ()> {
    match ast {
        Ast::Number(s) => todo!(),
        Ast::Variable(sym) => todo!(),
        // func_expr : expr
        // inputs : syntax list of expr
        Ast::Call { func_expr, inputs } => todo!(),

        // name : symbol
        // params : syntax list of slots
        // return_type : index type or simple type
        // body : expr
        Ast::Function { name, params, return_type, body } => todo!(),
        _ => Err(()), // TODO: LinearError => unsupported top level
    }
}

fn call(func_expr : Ast, inputs : Ast) -> Vec<Stmt> {
    todo!()
}