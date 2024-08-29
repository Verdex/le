
/*
use std::collections::HashMap;

use crate::data::Ast;

#[derive(Debug)]
pub enum Value {
    Number(f64),
    Print,
    Closure(usize, Vec<Value>),
    // TODO closure (addr + state?)
    // TODO function address

    // Note:  These will be cleared out after compilation is over
    FunName(FunName), 
}

#[derive(Debug)]
pub struct Local(usize);

#[derive(Debug)]
pub enum Statement {
    Return(Local),
    Call(Local, Vec<Local>),
    SetLocal(usize, Value),
    SetLocalFromRet(usize),
}

#[derive(Debug)]
pub struct Function { 
    meta : Box<str>,
    execute : Vec<Statement>,
}

#[derive(Debug)]
pub struct Program {
    functions : Vec<Function>,    
    //structure_meta : Vec<>,
    execute : Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum FunName {
    Name(Box<str>),
    Gen(Box<str>, usize),
}

#[derive(Debug)]
enum T {
    Fun(FunName, Vec<Statement>),
}


pub fn to_interpreter(ast : Vec<Ast>) -> Result<Program, ()> {
    let mut fun_map : HashMap<FunName, usize> = HashMap::new();
    let mut functions = vec![];
    //let mut execute = None;
    for x in ast {
        match to_interp(x)? {
            T::Fun(name, execute) => {
                let meta : Box<str> = format!("{:?}", name).into();
                fun_map.insert(name, functions.len());
                functions.push(Function { meta, execute });
            }
            _ => todo!(),
        }
    }
    // TODO: Replace all Value::FunName with  Closure and FunAddress
    todo!()
}

fn to_interp(ast : Ast) -> Result<T, ()> {
    todo!()
}
*/