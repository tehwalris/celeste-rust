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

    pub fn map_in_place(&mut self, f: impl Fn(HeapValue) -> HeapValue) {
        for value in self.0.iter_mut() {
            let old_value = value.take();
            if let Some(old_value) = old_value {
                *value = Some(f(old_value));
            }
        }
    }
}
