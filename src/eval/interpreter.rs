
#[derive(Debug, Clone, Copy)]
pub struct HAddr(usize);

#[derive(Debug, Clone, Copy)]
pub struct LAddr(usize);

#[derive(Debug)]
pub struct Frame { 
    ip : usize,
    locals : Vec<HAddr>,
}

#[derive(Debug)]
pub struct Interpreter {
    heap : Vec<Val>,
    stack : Vec<Frame>,
    ret : Option<HAddr>,
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
    Return(LAddr),
}

#[derive(Debug)]
pub struct Fun { 
    name : Box<str>,
    params : Vec<HAddr>,
    body : Vec<Stmt>,
}

trait SafeAccess { 
    fn sget(&self, index : LAddr) -> HAddr;
}

impl SafeAccess for Vec<HAddr> {
    fn sget(&self, index : LAddr) -> HAddr { 
        self[index.0]
    }
}

// Assume:  every body has a return at the end
pub fn run(m : &mut Interpreter, main : Rc<Fun>, env : &[HAddr]) {
    let mut ip : usize = 0;
    let mut locals : Vec<HAddr> = env.iter().map(|x| *x).collect();
    loop {
        match main.body[ip] {
            Stmt::Return(local) => {

                m.ret = Some(locals.sget(local));
            },
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