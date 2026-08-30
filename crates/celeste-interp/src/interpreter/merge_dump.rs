//! Naming heap cells by the field path that reaches them.
//!
//! Used by the merge partitioning (`CELESTE_PARTITION_CELLS`) to resolve a
//! cell pattern like `player.spd.x` to the concrete heap cell it names, and
//! by the probes that report per-column structure.

use super::heap::HeapId;
use super::state::State;
use super::value::HeapValue;

/// Resolves heap cells to the field path that reaches them, so a column can
/// be named `player.spd.x` rather than `cell 49`.
pub fn cell_names(state: &State) -> std::collections::HashMap<usize, String> {
    let mut names: std::collections::HashMap<usize, String> = Default::default();
    for (global, id) in state.global_env.iter() {
        names.insert(id.raw(), global.clone());
    }
    // Several passes so nesting resolves even when a table is visited before
    // its parent is named. Names propagate through pointer cells and array
    // elements, so objects reached only via `objects[i]` still get full paths
    // (objects.1.rem.x) rather than falling back to cellN.x.
    for _ in 0..5 {
        for i in 0..state.heap.len() {
            match state.heap.get_opt(HeapId::from_raw(i)) {
                Some(HeapValue::ObjectTable(fields)) => {
                    let parent = names.get(&i).cloned();
                    for (field, child) in fields.iter() {
                        let label = match &parent {
                            Some(p) => format!("{}.{}", p, field),
                            None => format!("cell{}.{}", i, field),
                        };
                        names.entry(child.raw()).or_insert(label);
                    }
                }
                Some(HeapValue::ArrayTable(items)) => {
                    let parent = names.get(&i).cloned();
                    for (k, child) in items.iter().enumerate() {
                        let label = match &parent {
                            Some(p) => format!("{}.{}", p, k + 1),
                            None => format!("cell{}.{}", i, k + 1),
                        };
                        names.entry(child.raw()).or_insert(label);
                    }
                }
                Some(HeapValue::Value(super::value::Value::Pointer(target))) => {
                    if let Some(name) = names.get(&i).cloned() {
                        names.entry(target.raw()).or_insert(name);
                    }
                }
                _ => {}
            }
        }
    }
    names
}
