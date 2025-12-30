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
