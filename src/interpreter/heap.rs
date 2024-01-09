use super::value::HeapValue;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct HeapId(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Heap(Vec<Option<HeapValue>>);

impl Heap {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn alloc(&mut self) -> HeapId {
        let id = HeapId(self.0.len());
        self.0.push(None);
        id
    }

    pub fn get(&self, id: HeapId) -> &HeapValue {
        self.0[id.0].as_ref().unwrap()
    }

    pub fn get_mut(&mut self, id: HeapId) -> &mut HeapValue {
        self.0[id.0].as_mut().unwrap()
    }

    pub fn set(&mut self, id: HeapId, value: HeapValue) {
        self.0[id.0] = Some(value);
    }
}
