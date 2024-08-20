
use std::rc::Rc;

/*
pub enum Instr {
    Goto(usize),
    Ret,
    Call(usize),
    PushParam,
    PopParam,
    PushStack,
    PopStack,
    DupStack,
    TakeStack(usize), // index from end of stack; so zero is equivalent to PopStack
    PushMemory(VmData), // also drops the reference of this memory into return register
    // take data from $ret; the vm needs to push the returned data to memory and put the ref into $ret
    Unary(fn(&VmData) -> VmData), 
    // needs two indices and operates like TakeStack for each
    Binary(usize, usize, fn(&VmData, &VmData) -> VmData), 
    Action(fn(&VmData)),
    ActionMut(Rc<dyn FnMut(&VmData)>),
    // TODO need a way to construct memory from other items in memory
}

pub enum Ir {

}

pub enum VmData {

}

*/