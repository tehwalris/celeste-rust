use std::sync::Arc;

use itertools::Itertools;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use super::heap::HeapId;
use crate::{ir::GlobalId, pico8_num::{Pico8Num, Pico8NumInterval}};

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
    let filtered: Vec<T> = vec
        .into_iter()
        .zip(mask.iter())
        .filter_map(|(v, m)| if *m { Some(v) } else { None })
        .collect();

    // Convert single-element vectors back to scalars (like OCaml's value_unvectorize_if_possible)
    if filtered.len() == 1 {
        MaybeVector::Scalar(filtered.into_iter().next().unwrap())
    } else {
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

    /// Extract a single element from a vectorized value at the given index.
    /// For scalar values, returns a clone regardless of index.
    /// This is more efficient than filter_vectors for single-element extraction.
    pub fn extract_at_index(&self, index: usize) -> Self {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => {
                Value::Bool(MaybeVector::Scalar(vec[index]))
            }
            Value::Number(MaybeVector::Vector(vec)) => {
                Value::Number(MaybeVector::Scalar(vec[index]))
            }
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Value::NumberInterval(MaybeVector::Scalar(vec[index]))
            }
            _ => self.clone(),
        }
    }
}

/// Wrapper for closure captured values using Arc for O(1) cloning.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CapturedValues(Arc<Vec<Value>>);

impl CapturedValues {
    pub fn new(values: Vec<Value>) -> Self {
        Self(Arc::new(values))
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Value> {
        self.0.get(index)
    }

    /// Map each value, creating new CapturedValues with the results.
    pub fn map(&self, f: impl Fn(&Value) -> Value) -> Self {
        Self(Arc::new(self.0.iter().map(f).collect()))
    }
}

impl PartialEq for CapturedValues {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl Eq for CapturedValues {}

impl FromIterator<Value> for CapturedValues {
    fn from_iter<T: IntoIterator<Item = Value>>(iter: T) -> Self {
        Self(Arc::new(iter.into_iter().collect()))
    }
}

impl IntoIterator for CapturedValues {
    type Item = Value;
    type IntoIter = std::vec::IntoIter<Value>;

    fn into_iter(self) -> Self::IntoIter {
        // Note: this clones the Vec if Arc is shared
        Arc::try_unwrap(self.0)
            .unwrap_or_else(|arc| (*arc).clone())
            .into_iter()
    }
}

impl<'a> IntoIterator for &'a CapturedValues {
    type Item = &'a Value;
    type IntoIter = std::slice::Iter<'a, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl std::ops::Index<usize> for CapturedValues {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

/// Arc-wrapped builtin function name for O(1) cloning.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BuiltinName(Arc<String>);

impl BuiltinName {
    pub fn new(s: String) -> Self {
        Self(Arc::new(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for BuiltinName {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

impl std::fmt::Display for BuiltinName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Serialize for BuiltinName {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for BuiltinName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Self(Arc::new(s)))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum HeapValue {
    Value(Value),
    ObjectTable(FxHashMap<String, HeapId>),
    ArrayTable(Vec<HeapId>),
    UnknownTable,
    Closure(GlobalId, CapturedValues),
    BuiltinFun(BuiltinName),
}

impl HeapValue {
    /// Extract a single element from vectorized values in this HeapValue at the given index.
    pub fn extract_at_index(&self, index: usize) -> Self {
        match self {
            HeapValue::Value(v) => HeapValue::Value(v.extract_at_index(index)),
            HeapValue::Closure(id, captures) => {
                HeapValue::Closure(
                    id.clone(),
                    captures.iter().map(|v| v.extract_at_index(index)).collect(),
                )
            }
            // Non-vectorizable values are just cloned
            HeapValue::ObjectTable(t) => HeapValue::ObjectTable(t.clone()),
            HeapValue::ArrayTable(a) => HeapValue::ArrayTable(a.clone()),
            HeapValue::UnknownTable => HeapValue::UnknownTable,
            HeapValue::BuiltinFun(n) => HeapValue::BuiltinFun(n.clone()),
        }
    }
}
