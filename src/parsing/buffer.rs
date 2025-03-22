
pub struct Buffer<T> {
    input : Vec<T>,
    index : usize,
}

impl<T> Buffer<T> {
    fn parse<S, E, F : Fn(&mut Ops<T>) -> Result<S, E>>(&mut self, f : F) -> Result<S, E> {
        let mut ops = Ops { input: &self.input, index: self.index };

        let ret = f(&mut ops)?;
        self.index = ops.index;

        Ok(ret)
    }
}

pub struct Ops<'a, T> {
    input : &'a [T],
    index : usize,
}

impl<'a, T> Clone for Ops<'a, T> {
    fn clone(&self) -> Self {
        Ops { input: self.input, index: self.index}
    }
}

impl<'a, T> Ops<'a, T> {
    fn or<S, E, const N : usize>(&mut self, targets : [fn(&mut Ops<'a, T>) -> Result<S, E>; N]) -> Result<S, Vec<E>> {
        let mut errors = vec![];
        for target in targets {
            let mut ops = self.clone();
            match target(&mut ops) {
                Ok(s) => { 
                    self.index = ops.index;
                    return Ok(s); 
                },
                Err(e) => { errors.push(e); },
            }
        }

        Err(errors)
    }

    fn peek<E>(&self, e : E) -> Result<&T, E> {
        if self.index < self.input.len() {
            let r = &self.input[self.index];
            Ok(r)
        }
        else {
            Err(e)
        }
    }

    fn get<E>(&mut self, e : E) -> Result<&T, E> {
        if self.index < self.input.len() {
            let r = &self.input[self.index];
            self.index += 1;
            Ok(r)
        }
        else {
            Err(e)
        }
    }

    fn with_rollback<S, E, F : Fn(&mut Ops<'a, T>) -> Result<S, E>>(&mut self, f : F) -> Result<S, E> {
        let mut ops = self.clone();
        let r = f(&mut ops)?;
        self.index = ops.index;
        Ok(r)
    }
}
