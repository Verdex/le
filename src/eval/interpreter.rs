

#[derive(Debug)]
pub struct Interpreter {

}

// TODO:  le patterns probably need:  generators, foreach, seq, and anon-structs (or row poly or pattern env types)

impl Interpreter {
    pub fn new() -> Self { 
        Interpreter { }
    }
}

use std::rc::Rc;

#[derive(Debug)]
pub enum Val {

}

#[derive(Debug)]
pub enum Stmt {

}

#[derive(Debug)]
pub struct Fun { 
    name : Box<str>,
    params : Vec<usize>,
    body : Vec<Stmt>,
}

// Assume:  every body has a return at the end
pub fn run(m : &mut Interpreter, main : Rc<Fun>, env : Vec<Val>) {
    let mut ip = 0;
    loop {
        match main.body[ip] {
            _ => todo!(),
        }
    }
}
