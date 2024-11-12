use std::collections::VecDeque;

#[derive(Debug)]
pub struct Memory<T> {
    buffer: Vec<Option<T>>,
    freed: VecDeque<usize>,
}

impl<T> Memory<T> {
    pub fn new() -> Self {
        Self {
            buffer: Vec::<Option<T>>::new(),
            freed: VecDeque::<usize>::new(),
        }
    }

    pub fn allocate(&mut self, value: T) -> usize {
        match self.freed.pop_front() {
            Some(index) => {
                self.buffer[index] = Some(value);
                index
            }
            None => {
                self.buffer.push(Some(value));
                self.buffer.len() - 1
            }
        }
    }

    pub fn deallocate(&mut self, index: usize) {
        if self.buffer.len() <= index {
            return;
        }
        self.buffer[index] = None;
        self.freed.push_back(index);
    }

    pub fn access(&self, index: usize) -> Option<&T> {
        if self.buffer.len() <= index {
            None
        } else {
            self.buffer[index].as_ref()
        }
    }

    pub fn take(&mut self, index: usize) -> Option<T> {
        if self.buffer.len() <= index {
            None
        } else {
            let res = self.buffer[index].take();
            self.deallocate(index);
            res
        }
    }

    pub fn modify(&mut self, index: usize, value: T) {
        if self.buffer.len() <= index {
            return;
        }
        self.buffer[index] = Some(value);
    }
}

impl<T> Memory<T>
where
    T: std::fmt::Debug,
{
    #[allow(dead_code)]
    pub fn dump(&self) {
        print!("{self:?}");
    }
}

impl<T> Clone for Memory<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            buffer: self.buffer.clone(),
            freed: self.freed.clone(),
        }
    }
}