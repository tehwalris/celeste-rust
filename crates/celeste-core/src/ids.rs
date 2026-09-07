//! The IR identifier newtypes, folded down here when `celeste-ir` was
//! deleted (the CFG and its frontend went with the recipe/compile pipeline).
//!
//! These are all that survived: `LocalId` still tags per-lane vectors in the
//! interpreter's `LocalEnv`, and `GlobalId` still names closures in `Value`
//! and the block bridge. They live in `celeste-core` so the interpreter and
//! the bridge can name them without a crate that no longer exists.

use serde::{Deserialize, Serialize};

/// SSA value name, unique per definition within a CFG. Kept because the
/// interpreter's `LocalEnv` and the merge/vectorize paths still key on it.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct LocalId(usize);

impl From<LocalId> for usize {
    fn from(id: LocalId) -> Self {
        id.0
    }
}

impl From<usize> for LocalId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

/// A global (function) name. Stored by `Value::Closure` and the block
/// bridge's cell translation.
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct GlobalId(String);

impl GlobalId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for GlobalId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// Maps a `LocalId` (unique per definition, SSA) to the physical slot it
/// lives in at run time (a small dense array in `LocalEnv`). Folded down
/// with the ids when the CFG was deleted: the CFG that owned it is gone, but
/// the interpreter's `LocalEnv` still keys its per-lane storage on it.
///
/// It matters because `LocalEnv` used to be indexed by `LocalId` directly, so
/// it cost `max LocalId + 1` slots, and every `filter_by_mask` clones it.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlotMap {
    /// Slot for each `LocalId`. Empty means the identity map, i.e. exactly the
    /// old behaviour, which is what an un-allocated CFG gets.
    of_local: Vec<u32>,
    num_slots: usize,
}

impl SlotMap {
    /// Every value gets its own slot, numbered by `LocalId`. Reproduces the
    /// pre-slot behaviour exactly.
    pub fn identity() -> Self {
        Self { of_local: Vec::new(), num_slots: 0 }
    }

    pub fn from_vec(of_local: Vec<u32>) -> Self {
        let num_slots = of_local
            .iter()
            .filter(|s| **s != u32::MAX)
            .map(|s| *s as usize + 1)
            .max()
            .unwrap_or(0);
        Self { of_local, num_slots }
    }

    #[inline]
    pub fn slot_of(&self, id: LocalId) -> usize {
        if self.of_local.is_empty() {
            usize::from(id)
        } else {
            self.of_local
                .get(usize::from(id))
                .copied()
                .unwrap_or(u32::MAX) as usize
        }
    }

    pub fn num_slots(&self) -> usize {
        self.num_slots
    }
}
