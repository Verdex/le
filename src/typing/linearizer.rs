
use std::collections::HashSet;
/*
use crate::data::{Ast, Linear, Type, Val, Sym, FunAddr, Stmt};


pub fn linearize(ast : Vec<Ast>) -> Vec<Linear> {
    ast.into_iter().map(to_linear).collect()
}

// Note:: Top level Ast is allowed to be Expr even though
// the intepreter won't accept a top level Expr because
// a REPL will want to parse Expr and then wrap in an
// artifical main function.
fn to_linear(ast : Ast) -> Linear {
    match ast {
        Ast::Number(s) => todo!(),
        Ast::Variable(sym) => todo!(),
        Ast::Call { fun_expr, inputs } => {
            // TODO : env will need to be pre-existing items for repl runs
            let (a, mut c) = call(*fun_expr, *inputs, HashSet::new());
            c.push(Stmt::ReturnAnonVar(a));
            repl_main_fun_wrapper(c)
        },

        // name : symbol
        // params : syntax list of slots
        // return_type : index type or simple type
        // body : expr
        Ast::Function { name, params, return_type, body } => todo!(),
        _ => todo!(),
    }
}

// func_expr : expr
// inputs : syntax list of expr
fn call(fun_expr : Ast, inputs : Ast, env : HashSet<Box<str>>) -> (usize, Vec<Stmt>) {
   /* match fun_expr {
        Ast::Call { fun_expr, inputs } => call(*fun_expr, *inputs, env),
        Ast::Variable(sym) => {
            match *sym {
                Ast::Symbol(s) if env.contains(&s) => { },
                x => { panic!("variable should contain symbol, but found {:?}", x); },
            }
        },
        _ => todo!(),
    }*/
    todo!()
}

fn repl_main_fun_wrapper(body : Vec<Stmt>) -> Linear {
    Linear::Fun { name : gen_sym("repl_main_wrapper")
                , params : vec![] 
                , return_type : Type::Simple("unit".into())
                , body 
                }
}

fn anon() -> usize {
    static mut index : usize = 0;
    unsafe { let v = index; index += 1; v }
}

fn gen_sym(name : &str) -> Sym {
    static mut index : usize = 0;
    let v = unsafe { let v = index; index += 1; v };
    Sym::Gen(name.into(), v)
}

#[cfg(test)]
mod test {
    use super::*;

}
*/