
pub enum Instr {
    Goto(usize),
    Ret,
    Call(usize),
    // TODO the below instrs need some thought
    PushParam(usize),
    PushStack(VmData),
    SetStack(usize, VmData),
    GetStack(usize),
    SetMemory(usize, VmData),
    GetMemory(usize),
    PushMemory(VmData),
}

pub enum Ir {

}

pub enum VmData {

}
