use std::marker::PhantomData;

pub struct Handle<T>{
    index: usize,
    _marker: PhantomData<T>
}

pub struct HandleVec<T> {
    data: Vec<T>
}

impl<T> HandleVec<T> {
    pub fn alloc(&mut self, val: T) -> Handle<T> {
        let index = self.data.len();

        self.data.push(val);

        Handle{
            index,
            _marker: Default::default()
        }
    }

    pub fn get(&self, handle: Handle<T>) -> &T {
        &self.data[handle.index]
    }

    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        &mut self.data[handle.index]
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl<T> Handle<T> {
    pub fn index(&self) -> usize {
        self.index
    }
}

impl<T> Default for HandleVec<T> {
    fn default() -> Self {
        Self {
            data: vec!()
        }
    }
}

impl<T> std::fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let handle_ty = std::any::type_name::<T>().split("::").last().unwrap();
        write!(f,"{}#{}",handle_ty,self.index)
    }
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Self {
            index: self.index,
            _marker: Default::default()
        }
    }
}

impl<T> Copy for Handle<T> {}