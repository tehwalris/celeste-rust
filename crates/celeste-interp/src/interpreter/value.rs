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
    ///
    /// Collapses a uniform payload to `Scalar` on the spot. The cardinality
    /// census found instruction outputs that are constant across ~50k lanes
    /// (a select whose arms agree wherever its mask varies, a comparison that
    /// resolves the same way in every lane of a scalar-specialised fragment) -
    /// and a uniform vector is pure downstream cost: every op over it is
    /// per-lane work a `Scalar` gets for free, and it re-enters the merge's
    /// dedup key where a `Scalar` drops out. The check early-exits at the
    /// first differing lane, so genuinely varying vectors pay a few
    /// comparisons; the full-scan cost lands only on vectors that were about
    /// to make everything downstream more expensive.
    ///
    /// A single-lane payload also collapses, matching what `filter` already
    /// did. An empty payload stays a vector - `Scalar` cannot represent
    /// "no lanes", and zero-lane states are the caller's bug to surface.
    pub fn vector(mut lanes: Vec<T>) -> Self {
        let uniform = match lanes.split_first() {
            Some((first, rest)) => rest.iter().all(|v| v == first),
            None => false,
        };
        if uniform {
            return MaybeVector::Scalar(lanes.swap_remove(0));
        }
        MaybeVector::Vector(std::sync::Arc::new(lanes))
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

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum HeapValue {
    Value(Value),
    ObjectTable(FxHashMap<String, HeapId>),
    ArrayTable(Vec<HeapId>),
    UnknownTable,
    Closure(GlobalId, Vec<Value>),
    BuiltinFun(String),
}
