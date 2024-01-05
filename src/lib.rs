
pub enum Status<T> {
    Yield(T),
    Final(T),
    Incremental,
    Dead,
}

pub trait IncrementalState {
    type Error : std::error::Error;
    type Memory;

    fn step(&mut self, n : usize) -> Result<Status<Self::Memory>, Self::Error>;
    fn single_step(&mut self) -> Result<Status<Self::Memory>, Self::Error>;
    fn step_to_next_result(&mut self) -> Result<Status<Self::Memory>, Self::Error>;
}

pub trait LeVm : Clone {
    type Error : std::error::Error;
    type Memory;
    type Incrmental<'a> : IncrementalState where Self : 'a;

    fn run(&mut self, input : Vec<Self::Memory>) -> Result<Status<Self::Memory>, Self::Error>;
    fn run_incremental<'a>(&'a mut self, input : Vec<Self::Memory>) -> Self::Incrmental<'a>;
}


#[cfg(test)]
mod tests {
    use super::*;

}
