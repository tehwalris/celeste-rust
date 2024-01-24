use std::collections::HashMap;

use super::{
    heap::{Heap, HeapId},
    local_env::LocalEnv,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct State {
    pub heap: Heap,
    pub local_env: LocalEnv,
    // TODO add commented fields
    // pub outer_local_envs: Vec<HashMap<LocalId, Value>>,
    pub global_env: HashMap<String, HeapId>,
    // pub prints: Vec<String>,
    // pub vector_size: usize,
}
