
#[derive(Debug)]
pub struct Frame { 
    ip : usize,
    locals : Vec<usize>,
}

#[derive(Debug)]
pub struct Interpreter {
    heap : Vec<Val>,
    stack : Vec<Frame>,
    ret : Option<usize>,
}

// TODO:  le patterns probably need:  generators, foreach, seq, and anon-structs (or row poly or pattern env types)

impl Interpreter {
    pub fn new() -> Self { 
        Interpreter { heap : vec![] 
                    , stack : vec![]
                    , ret : None
                    }
    }
}

use std::rc::Rc;

#[derive(Debug)]
pub enum Val {

}

#[derive(Debug)]
pub enum Stmt {
    Return(Option<usize>),
}

#[derive(Debug)]
pub struct Fun { 
    name : Box<str>,
    params : Vec<usize>,
    body : Vec<Stmt>,
}

// Assume:  every body has a return at the end
pub fn run(m : &mut Interpreter, main : Rc<Fun>, env : &[usize]) {
    let mut ip : usize = 0;
    let mut locals : Vec<usize> = env.iter().map(|x| *x).collect();
    loop {
        match main.body[ip] {
            _ => todo!(),
        }
    }
}


/*
    fun blah(1, 2, 3) {
        4 = v
        5 = v
        6 = call(other, 1, 5)
        return 5 
    }


*/