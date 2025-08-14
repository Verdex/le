
use std::collections::HashMap;
use an_a_vm::data::*;
use crate::data::runtime:: {Local, Global};

pub type GenOpList = Vec<GenOp<Local, Global>>;

pub fn gen_op_list() -> GenOpList {
    vec![]
}

pub fn to_index(gen_ops : &GenOpList, name : &str) -> Option<usize> {
    gen_ops.iter().enumerate().filter(|x| get_name(x.1) == name).map(|x| x.0).nth(0)
}

fn get_name<'a, T, S>(x : &'a GenOp<T, S>) -> &'a str {
    match x {
        GenOp::Vm { name, .. } => name,
        GenOp::Global { name, .. } => name,
        GenOp::Local { name, .. } => name,
        GenOp::Frame { name, .. } => name,
    }
}