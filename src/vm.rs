
#[derive(Debug, Clone, Copy)]
pub struct HAddr(usize);

#[derive(Debug, Clone, Copy)]
pub struct LAddr(usize);

#[derive(Debug)]
pub struct Frame { 
    ip : usize,
    locals : Vec<HAddr>,
    fun : Rc<Fun>,
}

#[derive(Debug)]
pub struct Vm {
    heap : Vec<Val>,
    stack : Vec<Frame>,
    ret : Option<HAddr>,
}

// TODO:  le patterns probably need:  generators, foreach, seq, and anon-structs (or row poly or pattern env types)

impl Vm {
    pub fn new() -> Self { 
        Vm { heap : vec![] 
           , stack : vec![]
           , ret : None
           }
    }
}

use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Val {
    Float(f64),
    Unit,
}

#[derive(Debug)]
pub enum Stmt {
    Return(LAddr),
    ConsVal(Val),
    Call(Rc<Fun>, Vec<LAddr>),
    DPrint(Vec<LAddr>),
}

#[derive(Debug)]
pub struct Fun { 
    name : Box<str>,
    params : Vec<HAddr>,
    body : Vec<Stmt>,
}

trait LocalAccess { 
    fn sget(&self, index : LAddr) -> HAddr;
}

impl LocalAccess for Vec<HAddr> {
    fn sget(&self, index : LAddr) -> HAddr { 
        self[index.0]
    }
}

trait HeapAccess { 
    fn sget(&mut self, index : HAddr) -> &mut Val;
}

impl HeapAccess for Vec<Val> {
    fn sget(&mut self, index : HAddr) -> &mut Val { 
        self.get_mut(index.0).unwrap()
    }
}

// Assume:  every body has a return at the end
pub fn run(m : &mut Vm, main : Rc<Fun>, env : &[HAddr]) {
    let mut ip : usize = 0;
    let mut locals : Vec<HAddr> = env.iter().map(|x| *x).collect();
    let mut f = main;
    loop {
        match f.body[ip] {
            Stmt::Return(local) => {
                m.ret = Some(locals.sget(local));
                if let Some(frame) = m.stack.pop() {
                    ip = frame.ip;
                    locals = frame.locals;
                    f = frame.fun;
                }
                else {
                    return;
                }
            },
            Stmt::ConsVal(ref v) => {
                let addr = m.heap.len();
                m.heap.push(v.clone());
                locals.push(HAddr(addr));
                ip += 1;
            },
            Stmt::Call(ref fun, ref params) => {
                let fun = fun.clone();
                let new_locals = params.iter().map(|x| locals.sget(*x)).collect::<Vec<_>>();
                let ls = std::mem::replace(&mut locals, new_locals);
                m.stack.push(Frame { fun, ip: ip + 1, locals: ls });
                ip = 0;
            },
            Stmt::DPrint(ref params) => {
                let targets = params.iter().map(|x| format!("{:?}", m.heap.sget(locals.sget(*x)))).collect::<Vec<_>>();
                println!("{:?}", targets);
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

#[cfg(test)]
mod test {
    use super::*;

}