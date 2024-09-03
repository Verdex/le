
use std::rc::Rc;

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
}

// TODO:  le patterns probably need:  generators, foreach, seq, and anon-structs (or row poly or pattern env types)

impl Vm {
    pub fn new() -> Self { 
        Vm { heap : vec![] 
           , stack : vec![]
           }
    }

    pub fn run(&mut self, main : Rc<Fun>, env : &[HAddr]) -> HAddr {
        run_vm(self, main, env)
    }

    pub fn get_val(&mut self, addr : HAddr) -> &mut Val {
        self.heap.sget(addr)
    }
}

#[derive(Debug, Clone)]
pub enum Lit {
    Float(f64),
    Ref(LAddr),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Val {
    Float(f64),
    Ref(HAddr),
    Unit,
}

impl Val {
    pub fn float(&self) -> f64 {
        match self {
            Val::Float(x) => *x,
            x => panic!("Expected Float but found {:?}", x),
        }
    }
    pub fn reference(&self) -> HAddr {
        match self {
            Val::Ref(x) => *x,
            x => panic!("Expected Ref but found {:?}", x),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Deref(LAddr, usize),
    Add(LAddr, LAddr),
    Cons(Vec<Lit>),
    Return(LAddr),
    Call(Rc<Fun>, Vec<LAddr>),
    DPrint(Vec<LAddr>),
}

#[derive(Debug)]
pub struct Fun { 
    name : Box<str>,
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

trait Heap { 
    fn sget(&mut self, index : HAddr) -> &mut Val;
    fn cons_vals(&mut self, v : Vec<Val>) -> HAddr;
}

impl Heap for Vec<Val> {
    fn sget(&mut self, index : HAddr) -> &mut Val { 
        self.get_mut(index.0).unwrap()
    }
    fn cons_vals(&mut self, mut vs : Vec<Val>) -> HAddr {
        let addr = self.len();
        self.append(&mut vs);
        HAddr(addr)
    }
}

// Assume:  every body has a return at the end
fn run_vm(m : &mut Vm, main : Rc<Fun>, env : &[HAddr]) -> HAddr {
    let mut ip : usize = 0;
    let mut locals : Vec<HAddr> = env.iter().map(|x| *x).collect();
    let mut current = main;
    loop {
        match current.body[ip] {
            Stmt::Deref(local, offset) => {
                let r = m.heap.sget(locals.sget(local)).reference();
                locals.push(HAddr(r.0 + offset));
                ip += 1;
            },
            Stmt::Add(a, b) => {
                let a = m.heap.sget(locals.sget(a)).float();
                let b = m.heap.sget(locals.sget(b)).float();
                let addr = m.heap.cons_vals(vec![Val::Float(a + b)]);
                locals.push(addr);
                ip += 1;
            },
            Stmt::Cons(ref ls) => {
                let addr = m.heap.cons_vals(ls.iter().map(|x| lit_to_val(x, &locals)).collect());
                locals.push(addr);
                ip += 1;
            },
            Stmt::Return(local) => {
                let ret = locals.sget(local);
                if let Some(frame) = m.stack.pop() {
                    ip = frame.ip;
                    locals = frame.locals;
                    current = frame.fun;

                    locals.push(ret);
                }
                else {
                    return ret;
                }
            },
            Stmt::Call(ref new, ref params) => {
                let new_locals = params.iter().map(|x| locals.sget(*x)).collect::<Vec<_>>();
                let ls = std::mem::replace(&mut locals, new_locals);
                m.stack.push(Frame { fun: Rc::clone(&current), ip: ip + 1, locals: ls });
                ip = 0;
                current = Rc::clone(new);
            },
            Stmt::DPrint(ref params) => {
                let targets = params.iter().map(|x| format!("{:?}", m.heap.sget(locals.sget(*x)))).collect::<Vec<_>>();
                println!("{:?}", targets);
                ip += 1;
            },
            _ => todo!(),
        }
    }
}

fn lit_to_val(lit : &Lit, locals : &Vec<HAddr>) -> Val {
    match lit {
        Lit::Float(f) => Val::Float(*f),
        Lit::Unit => Val::Unit,
        Lit::Ref(laddr) => Val::Ref(locals.sget(*laddr)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! proj {
        ($input:expr, $p:pat, $e:expr) => { 
            match $input {
                $p => $e,
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn should_call() {
        let mut vm = Vm::new();

        let adder_body = 
            vec![ Stmt::Add(LAddr(0), LAddr(1))
                , Stmt::Return(LAddr(2)) 
                ];
        
        let adder : Rc<Fun> = Fun { name: "adder".into()
                                  , body: adder_body
                                  }.into();

        let main_body = 
            vec![ Stmt::Cons(vec![Lit::Float(1.0)])
                , Stmt::Cons(vec![Lit::Float(2.0)])
                , Stmt::Call(adder, vec![LAddr(0), LAddr(1)])
                , Stmt::Return(LAddr(2)) 
                ];

        let main = Fun { name: "main".into()
                       , body: main_body 
                       };

        let ret = vm.run(main.into(), &[]);

        let v = vm.get_val(ret);
        assert_eq!(proj!(v, Val::Float(x), *x), 3.0);
    }

    #[test]
    fn should_add_env_with_local() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::Cons(vec![Lit::Float(1.0)])
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let ret = vm.run(f.into(), &[]);

        let body = vec![ Stmt::Cons(vec![Lit::Float(2.0)])
                       , Stmt::Add(LAddr(0), LAddr(1))
                       , Stmt::Return(LAddr(2))
                       ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let env = [ret];
        let ret = vm.run(f.into(), &env);
        
        let v = vm.get_val(ret);
        assert_eq!(proj!(v, Val::Float(x), *x), 3.0);
    }

    #[test]
    fn should_return_val_ref() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::Cons(vec![Lit::Float(1.0)])
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let ret = vm.run(f.into(), &[]);

        let v = vm.get_val(ret);
        assert_eq!(proj!(v, Val::Float(x), *x), 1.0);
    }

    #[test]
    fn should_return_env_ref() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::Cons(vec![Lit::Float(1.0)])
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let ret = vm.run(f.into(), &[]);

        let body = vec![Stmt::Return(LAddr(0))];

        let f = Fun { name: "x".into()
                    , body
                    };

        let env = [ret];
        let ret = vm.run(f.into(), &env);
        
        let v = vm.get_val(ret);
        assert_eq!(proj!(v, Val::Float(x), *x), 1.0);
    }

    #[test]
    fn should_cons_locals() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::Cons(vec![Lit::Float(1.0)])
                , Stmt::Cons(vec![Lit::Float(2.0)])
                , Stmt::Cons(vec![Lit::Ref(LAddr(0)), Lit::Ref(LAddr(1))])
                , Stmt::Deref(LAddr(2), 0)
                , Stmt::Deref(LAddr(2), 1)
                , Stmt::Add(LAddr(3), LAddr(4))
                , Stmt::Return(LAddr(5)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let ret = vm.run(f.into(), &[]);

        let v = vm.get_val(ret);
        assert_eq!(proj!(v, Val::Float(x), *x), 3.0);
    }
}