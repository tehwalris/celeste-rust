use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::heap::HeapId;
use crate::{ir::GlobalId, pico8_num::{Pico8Num, Pico8NumInterval}};

// Use FxHashMap for faster hashing in ObjectTable
type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MaybeVector<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    Scalar(T),
    /// The lane payload is behind an `Arc`, so cloning a vector value - a
    /// `load`, a `store`, a select on a uniform mask, an argument gather -
    /// is a refcount bump instead of a per-lane copy. Writers that mutate
    /// in place (`expand_lanes`) go through `Arc::make_mut`, paying the
    /// copy only when the payload is actually shared.
    // TODO do we want a link to some kind of size provider?
    Vector(std::sync::Arc<Vec<T>>),
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> PartialEq for MaybeVector<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => a == b,
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => {
                // Shared payloads are common (that is the point of the Arc),
                // so the pointer check short-circuits most comparisons.
                std::sync::Arc::ptr_eq(a, b) || a == b
            }
            _ => false,
        }
    }
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> Eq for MaybeVector<T> {}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> MaybeVector<T> {
    /// A vector value from freshly-built lanes.
    pub fn vector(lanes: Vec<T>) -> Self {
        MaybeVector::Vector(std::sync::Arc::new(lanes))
    }
    pub fn map(&self, f: impl Fn(&T) -> T) -> Self {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => {
                let t = crate::op_census::start();
                let out: Vec<T> = v.iter().map(f).collect();
                crate::op_census::record(
                    crate::op_census::Cat::Map,
                    v.len(),
                    2 * v.len() * std::mem::size_of::<T>(),
                    t,
                );
                MaybeVector::vector(out)
            }
        }
    }

    /// Maps over values, potentially changing the type
    pub fn map_to<O: std::fmt::Debug + Clone + PartialEq + Eq>(
        &self,
        f: impl Fn(&T) -> O,
    ) -> MaybeVector<O> {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => {
                let t = crate::op_census::start();
                let out: Vec<O> = v.iter().map(f).collect();
                crate::op_census::record(
                    crate::op_census::Cat::Map,
                    v.len(),
                    v.len() * (std::mem::size_of::<T>() + std::mem::size_of::<O>()),
                    t,
                );
                MaybeVector::vector(out)
            }
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
                // One length check up front instead of `zip_eq`'s check per
                // element - this is the arithmetic inner loop, and the
                // per-element branch blocks auto-vectorization.
                assert_eq!(a.len(), b.len(), "map2 on vectors of different sizes");
                let t = crate::op_census::start();
                let out: Vec<O> = a.iter().zip(b.iter()).map(|(a, b)| f(a, b)).collect();
                crate::op_census::record(
                    crate::op_census::Cat::Binop,
                    a.len(),
                    a.len() * (2 * std::mem::size_of::<T>() + std::mem::size_of::<O>()),
                    t,
                );
                MaybeVector::vector(out)
            }
            // Broadcast scalar to match vector size
            (MaybeVector::Scalar(a), MaybeVector::Vector(b)) => {
                let t = crate::op_census::start();
                let out: Vec<O> = b.iter().map(|bi| f(a, bi)).collect();
                crate::op_census::record(
                    crate::op_census::Cat::Binop,
                    b.len(),
                    b.len() * (std::mem::size_of::<T>() + std::mem::size_of::<O>()),
                    t,
                );
                MaybeVector::vector(out)
            }
            (MaybeVector::Vector(a), MaybeVector::Scalar(b)) => {
                let t = crate::op_census::start();
                let out: Vec<O> = a.iter().map(|ai| f(ai, b)).collect();
                crate::op_census::record(
                    crate::op_census::Cat::Binop,
                    a.len(),
                    a.len() * (std::mem::size_of::<T>() + std::mem::size_of::<O>()),
                    t,
                );
                MaybeVector::vector(out)
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

/// Count true values in a mask using SIMD-friendly byte sum
#[inline(always)]
pub fn count_true(mask: &[bool]) -> usize {
    // Since bool is represented as 0 or 1, we can sum directly
    // This is more SIMD-friendly than filter().count()
    mask.iter().map(|&b| b as usize).sum()
}

/// Filter a vector down to the lanes in `kept` (sorted indices of the
/// mask's true entries, computed once per state filter). O(kept) per
/// vector instead of O(mask): a filter touches every vector value in the
/// state, so re-scanning the full mask per vector was the dominant cost of
/// `filter_branch`.
#[inline]
fn filter_vec_gather_ref<T: Clone + PartialEq>(vec: &[T], kept: &[u32]) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
    if let [only] = kept {
        return MaybeVector::Scalar(vec[*only as usize].clone());
    }
    let t = crate::op_census::start();
    let mut filtered = Vec::with_capacity(kept.len());
    for &i in kept {
        filtered.push(vec[i as usize].clone());
    }
    crate::op_census::record(
        crate::op_census::Cat::Filter,
        kept.len(),
        kept.len() * (2 * std::mem::size_of::<T>() + 4),
        t,
    );
    MaybeVector::vector(filtered)
}

impl Value {
    /// Filter vectors, returning Some(new_value) only if the value is a vector.
    /// Returns None for scalars (no transformation needed).
    /// This avoids cloning scalar values that don't need transformation.
    #[inline]
    pub fn filter_vectors_if_vector(&self, kept: &[u32]) -> Option<Self> {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => Some(Value::Bool(filter_vec_gather_ref(vec, kept))),
            Value::Number(MaybeVector::Vector(vec)) => Some(Value::Number(filter_vec_gather_ref(vec, kept))),
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Some(Value::NumberInterval(filter_vec_gather_ref(vec, kept)))
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
    #[inline]
    pub fn filter_vectors_if_needed(&self, kept: &[u32]) -> Option<Self> {
        match self {
            HeapValue::Value(v) => v.filter_vectors_if_vector(kept).map(HeapValue::Value),
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
                        .map(|v| v.filter_vectors_if_vector(kept).unwrap_or_else(|| v.clone()))
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
