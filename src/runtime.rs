
use std::collections::HashMap;
use an_a_vm::data::*;
use crate::data::runtime:: {Local, Global};


pub fn gen_ops() -> HashMap<Box<str>, GenOp<Local, Global>> {
    HashMap::new()
}