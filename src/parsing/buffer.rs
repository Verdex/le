
pub struct Buffer<T> {
    input : Vec<T>,
    index : usize,
}

impl<T> Buffer<T> {
    fn parse<S, F : Fn(&mut Ops<T>) -> Result<S, ()>>(&mut self, with_buffer : F) -> Result<S, ()> {
        let mut ops = Ops { input: &self.input, index: self.index };

        let ret = with_buffer(&mut ops)?;
        self.index = ops.index;

        Ok(ret)
    }
}

pub struct Ops<'a, T> {
    input : &'a [T],
    index : usize,
}

impl<'a, T> Ops<'a, T> {
    fn or<S, const N : usize>(&mut self, targets : [fn(&mut Ops<'a, T>) -> Result<S, ()>; N]) -> Result<S, ()> {
        for w in targets {
            w(self);
        }
        todo!()
    }

}
