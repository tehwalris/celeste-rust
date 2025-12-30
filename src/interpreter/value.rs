use std::hash::BuildHasherDefault;

use itertools::Itertools;
use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::heap::HeapId;
use crate::{ir::GlobalId, pico8_num::{Pico8Num, Pico8NumInterval}};

// Use FxHashMap for faster hashing in ObjectTable
type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MaybeVector<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    Scalar(T),
    // TODO do we want a link to some kind of size provider?
    Vector(Vec<T>),
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> MaybeVector<T> {
    pub fn map(&self, f: impl Fn(&T) -> T) -> Self {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => MaybeVector::Vector(v.iter().map(f).collect()),
        }
    }

    /// Maps over values, potentially changing the type
    pub fn map_to<O: std::fmt::Debug + Clone + PartialEq + Eq>(
        &self,
        f: impl Fn(&T) -> O,
    ) -> MaybeVector<O> {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => MaybeVector::Vector(v.iter().map(f).collect()),
        }
    }

    pub fn map2<O: std::fmt::Debug + Clone + PartialEq + Eq>(
        a: &Self,
        b: &Self,
        f: impl Fn(&T, &T) -> O,
    ) -> MaybeVector<O> {
        match (a, b) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => MaybeVector::Scalar(f(a, b)),
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => {
                MaybeVector::Vector(a.iter().zip_eq(b.iter()).map(|(a, b)| f(a, b)).collect())
            }
            // Broadcast scalar to match vector size
            (MaybeVector::Scalar(a), MaybeVector::Vector(b)) => {
                MaybeVector::Vector(b.iter().map(|bi| f(a, bi)).collect())
            }
            (MaybeVector::Vector(a), MaybeVector::Scalar(b)) => {
                MaybeVector::Vector(a.iter().map(|ai| f(ai, b)).collect())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Value {
    Number(MaybeVector<Pico8Num>),
    NumberInterval(MaybeVector<Pico8NumInterval>),
    Bool(MaybeVector<bool>),
    UnknownBool,
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
}

fn filter_vec_by_mask<T: Clone + PartialEq>(vec: Vec<T>, mask: &[bool]) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
    // Count true values to pre-allocate exact capacity
    let true_count = mask.iter().filter(|&&b| b).count();

    if true_count == 1 {
        // Single element - find it and return as scalar
        for (v, &m) in vec.into_iter().zip(mask.iter()) {
            if m {
                return MaybeVector::Scalar(v);
            }
        }
        unreachable!("true_count was 1 but no true found")
    } else {
        // Multiple elements - collect into pre-allocated Vec
        let mut filtered = Vec::with_capacity(true_count);
        for (v, &m) in vec.into_iter().zip(mask.iter()) {
            if m {
                filtered.push(v);
            }
        }
        MaybeVector::Vector(filtered)
    }
}

/// Filter a vector by mask, cloning elements (for use with references)
fn filter_vec_by_mask_ref<T: Clone + PartialEq>(vec: &[T], mask: &[bool]) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
    // Count true values to pre-allocate exact capacity
    let true_count = mask.iter().filter(|&&b| b).count();

    if true_count == 1 {
        // Single element - find it and return as scalar
        for (v, &m) in vec.iter().zip(mask.iter()) {
            if m {
                return MaybeVector::Scalar(v.clone());
            }
        }
        unreachable!("true_count was 1 but no true found")
    } else {
        // Multiple elements - collect into pre-allocated Vec
        let mut filtered = Vec::with_capacity(true_count);
        for (v, &m) in vec.iter().zip(mask.iter()) {
            if m {
                filtered.push(v.clone());
            }
        }
        MaybeVector::Vector(filtered)
    }
}

impl Value {
    pub fn filter_vectors(self, mask: &[bool]) -> Self {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => Value::Bool(filter_vec_by_mask(vec, mask)),
            Value::Number(MaybeVector::Vector(vec)) => Value::Number(filter_vec_by_mask(vec, mask)),
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Value::NumberInterval(filter_vec_by_mask(vec, mask))
            }
            _ => self,
        }
    }

    /// Filter vectors, returning Some(new_value) only if the value is a vector.
    /// Returns None for scalars (no transformation needed).
    /// This avoids cloning scalar values that don't need transformation.
    pub fn filter_vectors_if_vector(&self, mask: &[bool]) -> Option<Self> {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => Some(Value::Bool(filter_vec_by_mask_ref(vec, mask))),
            Value::Number(MaybeVector::Vector(vec)) => Some(Value::Number(filter_vec_by_mask_ref(vec, mask))),
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Some(Value::NumberInterval(filter_vec_by_mask_ref(vec, mask)))
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum HeapValue {
    Value(Value),
    ObjectTable(FxHashMap<String, HeapId>),
    ArrayTable(Vec<HeapId>),
    UnknownTable,
    Closure(GlobalId, Vec<Value>),
    BuiltinFun(String),
}

impl HeapValue {
    /// Filter vectors in this heap value, returning Some(new_value) only if transformation is needed.
    /// Returns None for values that don't contain vectors (no transformation needed).
    /// This avoids cloning non-vector values during filter operations.
    pub fn filter_vectors_if_needed(&self, mask: &[bool]) -> Option<Self> {
        match self {
            HeapValue::Value(v) => v.filter_vectors_if_vector(mask).map(HeapValue::Value),
            HeapValue::Closure(id, captures) => {
                // Check if any capture is a vector
                let mut any_vector = false;
                for cap in captures {
                    if matches!(cap,
                        Value::Bool(MaybeVector::Vector(_)) |
                        Value::Number(MaybeVector::Vector(_)) |
                        Value::NumberInterval(MaybeVector::Vector(_))
                    ) {
                        any_vector = true;
                        break;
                    }
                }
                if any_vector {
                    // Need to transform - clone and filter
                    let new_captures: Vec<Value> = captures
                        .iter()
                        .map(|v| v.filter_vectors_if_vector(mask).unwrap_or_else(|| v.clone()))
                        .collect();
                    Some(HeapValue::Closure(id.clone(), new_captures))
                } else {
                    None
                }
            }
            // These don't contain vectorizable values
            HeapValue::ObjectTable(_)
            | HeapValue::ArrayTable(_)
            | HeapValue::UnknownTable
            | HeapValue::BuiltinFun(_) => None,
        }
    }
}
