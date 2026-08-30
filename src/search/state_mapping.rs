//! Mapping between the *canonical* game state and a rewritten program's state.
//!
//! The canonical representation is the one the unmodified ("plain") program
//! uses across frames - the cross-frame heap contract that `verify` observes.
//! A rewritten program is a *specialization*: it may use a different (cheaper)
//! representation internally and at its own frame boundaries, as long as there
//! is a mechanical mapping in both directions. That mapping is what lets us
//!
//!   * **deopt**: when a specialized program hits a premise it baked in (say
//!     `#objects == 1` from `collapse_loop`) on a state that violates it, we
//!     map the frame-input state to canonical, re-run the frame under the
//!     plain program, and map the result back;
//!   * **switch**: later, move a state between differently-specialized
//!     programs (per input class, per room phase) through the canonical form.
//!
//! The mapping is derived from the recipe, not hand-written: each rule that
//! changes the cross-frame representation contributes its transform. Today
//! exactly one rule does - `promote_capture` - so the mapping is the set of
//! `(closure function, capture index)` pairs it promoted:
//!
//!   * canonical:   `captures[i] = Pointer -> cell -> value`  (a write-once box)
//!   * specialized: `captures[i] = value`
//!
//! `from_canonical` unboxes those captures; `to_canonical` reboxes them. The
//! box is write-once and read-only (that is `promote_capture`'s precondition),
//! so a fresh box per capture is behaviorally identical to the original shared
//! one, and the difference is invisible to `observe_state`, which unboxes
//! captures on both sides. `rewrite deoptcheck` certifies the whole cycle by
//! forcing every frame through to_canonical -> plain program -> from_canonical
//! and comparing observations against a plain baseline run.
//!
//! Both directions are loud on unexpected shapes: a capture that does not look
//! like the representation we claim it has is an error, never a silent skip.
//! If a future rule legitimately promotes a capture whose *value* is itself a
//! pointer to a value cell, `to_canonical`'s double-boxing guard below will
//! refuse it - at which point the mapping needs real representation tagging,
//! not a weaker check.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

use crate::interpreter::heap::HeapId;
use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, Value};

use crate::program::recipe::{Recipe, Rule};

/// The representation difference between a rewritten program's state and the
/// canonical (plain-program) state. Derived from a recipe; see module docs.
#[derive(Debug, Clone, Default)]
pub struct StateMapping {
    /// Closure function name -> capture indices promoted by the recipe.
    promoted: FxHashMap<String, Vec<usize>>,
}

impl StateMapping {
    pub fn from_recipe(recipe: &Recipe) -> Self {
        let mut promoted: FxHashMap<String, Vec<usize>> = FxHashMap::default();
        for entry in &recipe.entries {
            if let Rule::PromoteCapture { function, index } = &entry.rule {
                promoted.entry(function.clone()).or_default().push(*index);
            }
        }
        for indices in promoted.values_mut() {
            indices.sort_unstable();
            indices.dedup();
        }
        Self { promoted }
    }

    /// True if the specialized representation *is* the canonical one.
    pub fn is_identity(&self) -> bool {
        self.promoted.is_empty()
    }

    /// How many `(function, index)` pairs the mapping covers.
    pub fn pair_count(&self) -> usize {
        self.promoted.values().map(|v| v.len()).sum()
    }

    /// Closures of the promoted functions in `state`, as
    /// `(heap id, function name, captures, promoted indices)`.
    fn promoted_closures<'a>(
        &'a self,
        state: &State,
    ) -> Vec<(HeapId, crate::ir::GlobalId, Vec<Value>, &'a [usize])> {
        let mut out = Vec::new();
        for i in 0..state.heap.len() {
            let id = HeapId::from_raw(i);
            let Some(HeapValue::Closure(name, captures)) = state.heap.get_opt(id) else {
                continue;
            };
            let Some(indices) = self.promoted.get(name.as_str()) else { continue };
            out.push((id, name.clone(), captures.clone(), indices.as_slice()));
        }
        out
    }

    /// Specialized -> canonical: rebox every promoted capture.
    ///
    /// Allocates a fresh write-once cell per capture. The plain program only
    /// ever loads these cells, so per-capture boxes behave identically to the
    /// original shared box, and the extra cells become garbage again after
    /// `from_canonical` and are dropped by the next `gc`.
    pub fn to_canonical(&self, state: &mut State) -> Result<()> {
        for (id, name, mut captures, indices) in self.promoted_closures(state) {
            for &index in indices {
                let value = captures
                    .get(index)
                    .ok_or_else(|| {
                        anyhow!(
                            "to_canonical: closure {} has {} captures, expected index {}",
                            name.as_str(),
                            captures.len(),
                            index
                        )
                    })?
                    .clone();
                // Guard against double-conversion: a specialized capture must
                // not already look like a canonical one. See module docs.
                if let Value::Pointer(target) = &value {
                    if matches!(state.heap.get_opt(*target), Some(HeapValue::Value(_))) {
                        return Err(anyhow!(
                            "to_canonical: capture {} of {} already points at a value \
                             cell - this state looks canonical already",
                            index,
                            name.as_str()
                        ));
                    }
                }
                let cell = state.heap.alloc();
                state.heap.set(cell, HeapValue::Value(value));
                captures[index] = Value::Pointer(cell);
            }
            state.heap.set(id, HeapValue::Closure(name, captures));
        }
        Ok(())
    }

    /// Canonical -> specialized: unbox every promoted capture.
    ///
    /// The boxes become unreferenced and are dropped by the next `gc`.
    pub fn from_canonical(&self, state: &mut State) -> Result<()> {
        for (id, name, mut captures, indices) in self.promoted_closures(state) {
            for &index in indices {
                let capture = captures.get(index).cloned().ok_or_else(|| {
                    anyhow!(
                        "from_canonical: closure {} has {} captures, expected index {}",
                        name.as_str(),
                        captures.len(),
                        index
                    )
                })?;
                let Value::Pointer(target) = capture else {
                    return Err(anyhow!(
                        "from_canonical: capture {} of {} is not a pointer - this \
                         state does not look canonical",
                        index,
                        name.as_str()
                    ));
                };
                let Some(HeapValue::Value(value)) = state.heap.get_opt(target) else {
                    return Err(anyhow!(
                        "from_canonical: capture {} of {} does not point at a value \
                         cell - this state does not look canonical",
                        index,
                        name.as_str()
                    ));
                };
                captures[index] = value.clone();
            }
            state.heap.set(id, HeapValue::Closure(name, captures));
        }
        Ok(())
    }
}
