
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

    pub fn run(&mut self, main : Rc<Fun>, env : &[HAddr]) {
        run_vm(self, main, env);
    }

    pub fn ret_val(&mut self) -> Option<&mut Val> {
        Some(self.heap.sget(self.ret?))
    }
}


#[derive(Debug, Clone)]
pub enum Val {
    Float(f64),
    Unit,
}

impl Val {
    pub fn float(&self) -> f64 {
        match self {
            Val::Float(x) => *x,
            x => panic!("Expected Float but found {:?}", x),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Add(LAddr, LAddr),
    ConsVal(Val),
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
    fn cons_val(&mut self, v : Val) -> HAddr;
}

impl Heap for Vec<Val> {
    fn sget(&mut self, index : HAddr) -> &mut Val { 
        self.get_mut(index.0).unwrap()
    }
    fn cons_val(&mut self, v : Val) -> HAddr {
        let addr = self.len();
        self.push(v);
        HAddr(addr)
    }
}

// Assume:  every body has a return at the end
fn run_vm(m : &mut Vm, main : Rc<Fun>, env : &[HAddr]) {
    let mut ip : usize = 0;
    let mut locals : Vec<HAddr> = env.iter().map(|x| *x).collect();
    let mut current = main;
    loop {
        match current.body[ip] {
            Stmt::Add(a, b) => {
                let a = m.heap.sget(locals.sget(a)).float();
                let b = m.heap.sget(locals.sget(b)).float();
                let addr = m.heap.cons_val(Val::Float(a + b));
                locals.push(addr);
                ip += 1;
            },
            Stmt::ConsVal(ref v) => {
                let addr = m.heap.cons_val(v.clone());
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
                    m.ret = Some(ret);
                    return;
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
            vec![ Stmt::ConsVal(Val::Float(1.0))
                , Stmt::ConsVal(Val::Float(2.0))
                , Stmt::Call(adder, vec![LAddr(0), LAddr(1)])
                , Stmt::Return(LAddr(2)) 
                ];

        let main = Fun { name: "main".into()
                       , body: main_body 
                       };

        vm.run(main.into(), &[]);

        let v = vm.ret_val().unwrap();
        assert_eq!(proj!(v, Val::Float(x), *x), 3.0);
    }

    #[test]
    fn should_add_env_with_local() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::ConsVal(Val::Float(1.0))
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        vm.run(f.into(), &[]);

        let body = vec![ Stmt::ConsVal(Val::Float(2.0))
                       , Stmt::Add(LAddr(0), LAddr(1))
                       , Stmt::Return(LAddr(2))
                       ];

        let f = Fun { name: "x".into()
                    , body
                    };

        let env = [vm.ret.unwrap()];
        vm.run(f.into(), &env);
        
        let v = vm.ret_val().unwrap();
        assert_eq!(proj!(v, Val::Float(x), *x), 3.0);
    }

    #[test]
    fn should_return_val_ref() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::ConsVal(Val::Float(1.0))
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        vm.run(f.into(), &[]);

        assert!(vm.ret.is_some());

        let v = vm.ret_val().unwrap();
        assert_eq!(proj!(v, Val::Float(x), *x), 1.0);
    }

    #[test]
    fn should_return_env_ref() {
        let mut vm = Vm::new();

        let body = 
            vec![ Stmt::ConsVal(Val::Float(1.0))
                , Stmt::Return(LAddr(0)) 
                ];

        let f = Fun { name: "x".into()
                    , body
                    };

        vm.run(f.into(), &[]);

        let body = vec![Stmt::Return(LAddr(0))];

        let f = Fun { name: "x".into()
                    , body
                    };

        let env = [vm.ret.unwrap()];
        vm.run(f.into(), &env);
        
        let v = vm.ret_val().unwrap();
        assert_eq!(proj!(v, Val::Float(x), *x), 1.0);
    }
}