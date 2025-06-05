
use an_a_vm::data::{ Fun as VmFun, Op };

use crate::data::*;


pub fn compile(input : Vec<Ast>) -> Vec<VmFun> {
    todo!()
}

fn fn blarg(input : Fun) -> VmFun {
    todo!()
}
/*
ast fun
name : Rc<str>,
params : Vec<Slot>,
return_type : LeType,
body : Box<Ast>,

*/

/*

an a vm

pub enum Op {
    Gen(usize, Vec<usize>),
    Call(usize, Vec<usize>),
    ReturnLocal(usize), 
    Return,
    Branch(usize),
    DynCall(Vec<usize>),
    Yield(usize),
    Finish,
    Resume(usize),
    FinishSetBranch(usize),
    Drop(usize),
    Dup(usize),
    Swap(usize, usize),
    PushRet,
}

pub struct Fun {
    pub name : Box<str>,
    pub instrs : Vec<Op>,
}


pub struct OpEnv<'a, T, S> {
    pub locals : &'a mut Vec<Vec<T>>,
    pub globals : &'a mut Vec<S>,
    pub ret : &'a mut Option<T>,
    pub branch : &'a mut bool,
    pub dyn_call : &'a mut Option<usize>,
}

pub struct GenOp<T, S> {
    pub name : Box<str>,
    pub op : for<'a> fn(env : OpEnv<'a, T, S>, params : &Vec<usize>) -> Result<(), Box<dyn std::error::Error>>,
}


*/