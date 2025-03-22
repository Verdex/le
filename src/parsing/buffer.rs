
pub struct Buffer<T> {
    input : Vec<T>,
    index : usize,
}

impl<T> Buffer<T> {
    fn parse<S, E, F : Fn(&mut Ops<T>) -> Result<S, E>>(&mut self, with_buffer : F) -> Result<S, E> {
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
    fn or<S, E, const N : usize>(&mut self, targets : [fn(&mut Ops<'a, T>) -> Result<S, E>; N]) -> Result<S, Vec<E>> {
        let mut errors = vec![];
        for w in targets {
            match w(self) {
                Ok(s) => { return Ok(s); },
                Err(e) => { errors.push(e); },
            }
        }

        Err(errors)
    }
}
