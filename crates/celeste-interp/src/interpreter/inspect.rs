//! Read-only heap navigation: `StateHelper`, the reference `State`'s
//! object-by-type lookup that the abstraction layer's widenings and
//! `concrete_run` use.

use std::fmt;

use super::{
    heap::HeapId,
    state::State,
    value::{HeapValue, Value},
};

/// Error type for heap inspection operations
#[derive(Debug, Clone)]
pub enum InspectError {
    /// Expected an ArrayTable but got something else
    ExpectedArrayTable { heap_id: HeapId, actual: String },
}

impl fmt::Display for InspectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InspectError::ExpectedArrayTable { heap_id, actual } => {
                write!(f, "expected ArrayTable at {:?}, got {}", heap_id, actual)
            }
        }
    }
}

impl std::error::Error for InspectError {}

/// Helper for navigating and inspecting heap structure
pub struct StateHelper<'a> {
    state: &'a State,
}

impl<'a> StateHelper<'a> {
    pub fn new(state: &'a State) -> Self {
        Self { state }
    }

    /// Get a global variable's heap ID
    pub fn find_global(&self, name: &str) -> Option<HeapId> {
        self.state.global_env.get(name).copied()
    }

    /// Get the objects array HeapId (dereferencing the global pointer)
    pub fn get_objects_array_id(&self) -> Option<HeapId> {
        let global_id = self.find_global("objects")?;
        match self.load(global_id) {
            HeapValue::Value(Value::Pointer(arr_id)) => Some(*arr_id),
            _ => None,
        }
    }

    /// Load a heap value
    pub fn load(&self, id: HeapId) -> &HeapValue {
        self.state.heap.get(id)
    }

    /// Get a pointer from a heap value (unwrap Value::Pointer)
    pub fn unwrap_pointer(&self, value: &HeapValue) -> Option<HeapId> {
        match value {
            HeapValue::Value(Value::Pointer(id)) => Some(*id),
            _ => None,
        }
    }

    /// Find objects in an array table that have a specific type.
    /// Returns an error if array_id doesn't point to an ArrayTable.
    pub fn find_objects_by_type(&self, array_id: HeapId, type_name: &str) -> Result<Vec<HeapId>, InspectError> {
        let items = match self.load(array_id) {
            HeapValue::ArrayTable(items) => items,
            other => return Err(InspectError::ExpectedArrayTable {
                heap_id: array_id,
                actual: format!("{:?}", other),
            }),
        };

        // Get the type function's heap ID by dereferencing the global
        let global_type_target = self.find_global(type_name)
            .and_then(|global_heap_id| match self.load(global_heap_id) {
                HeapValue::Value(Value::Pointer(target_id)) => Some(*target_id),
                _ => None,
            });

        let global_type_target = match global_type_target {
            Some(id) => id,
            None => return Ok(vec![]),  // Type not found is valid (no matches)
        };

        let mut results = Vec::new();
        for item_ptr in items {
            if let HeapValue::Value(Value::Pointer(obj_id)) = self.load(*item_ptr) {
                if let HeapValue::ObjectTable(obj) = self.load(*obj_id) {
                    if let Some(type_ptr) = obj.get("type") {
                        if let HeapValue::Value(Value::Pointer(type_heap_id)) = self.load(*type_ptr) {
                            // Check if this matches our type
                            if *type_heap_id == global_type_target {
                                results.push(*obj_id);
                            }
                        }
                    }
                }
            }
        }
        Ok(results)
    }
}
