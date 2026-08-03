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

/// Lane counts below this run per-lane loops sequentially; above it, the
/// loop splits across threads. Deep frames run millions of lanes, where a
/// single vector op costs milliseconds single-threaded.
pub(crate) const PAR_LANES_THRESHOLD: usize = 1 << 17;

/// Build an `n`-element vector from per-chunk builders, in parallel above
/// the lane threshold - via the persistent lane pool, because this runs per
/// vector *instruction* and cannot afford thread spawns. Chunks are
/// concatenated in order, so the result is identical to the sequential
/// build.
pub(crate) fn par_concat<T: Send>(
    n: usize,
    f: impl Fn(std::ops::Range<usize>) -> Vec<T> + Sync,
) -> Vec<T> {
    let threads = super::par_pool::pool_threads();
    if n < PAR_LANES_THRESHOLD || threads == 1 {
        return f(0..n);
    }
    let chunk = n.div_ceil(threads);
    let parts: Vec<std::sync::Mutex<Option<Vec<T>>>> =
        (0..threads).map(|_| std::sync::Mutex::new(None)).collect();
    super::par_pool::run(threads, &|t| {
        let start = t * chunk;
        let end = ((t + 1) * chunk).min(n);
        *parts[t].lock().unwrap() = Some(f(start..end));
    });
    let mut out = Vec::with_capacity(n);
    for part in parts {
        out.extend(part.into_inner().unwrap().expect("chunk ran"));
    }
    out
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq + Send + Sync> MaybeVector<T> {
    /// A vector value from freshly-built lanes.
    pub fn vector(lanes: Vec<T>) -> Self {
        MaybeVector::Vector(std::sync::Arc::new(lanes))
    }
    pub fn map(&self, f: impl Fn(&T) -> T + Sync) -> Self {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) if v.len() < PAR_LANES_THRESHOLD => {
                MaybeVector::vector(v.iter().map(f).collect())
            }
            MaybeVector::Vector(v) => MaybeVector::vector(par_concat(v.len(), |r| {
                v[r].iter().map(&f).collect()
            })),
        }
    }

    /// Maps over values, potentially changing the type
    pub fn map_to<O: std::fmt::Debug + Clone + PartialEq + Eq + Send + Sync>(
        &self,
        f: impl Fn(&T) -> O + Sync,
    ) -> MaybeVector<O> {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) if v.len() < PAR_LANES_THRESHOLD => {
                MaybeVector::vector(v.iter().map(f).collect())
            }
            MaybeVector::Vector(v) => MaybeVector::vector(par_concat(v.len(), |r| {
                v[r].iter().map(&f).collect()
            })),
        }
    }

    pub fn map2<O: std::fmt::Debug + Clone + PartialEq + Eq + Send + Sync>(
        a: &Self,
        b: &Self,
        f: impl Fn(&T, &T) -> O + Sync,
    ) -> MaybeVector<O> {
        match (a, b) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => MaybeVector::Scalar(f(a, b)),
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => {
                // One length check up front instead of `zip_eq`'s check per
                // element - this is the arithmetic inner loop, and the
                // per-element branch blocks auto-vectorization. The direct
                // loop is kept as the small-vector path: routing it through
                // the chunk builder costs inlining of `f`.
                assert_eq!(a.len(), b.len(), "map2 on vectors of different sizes");
                if a.len() < PAR_LANES_THRESHOLD {
                    MaybeVector::vector(a.iter().zip(b.iter()).map(|(a, b)| f(a, b)).collect())
                } else {
                    MaybeVector::vector(par_concat(a.len(), |r| {
                        a[r.clone()]
                            .iter()
                            .zip(&b[r])
                            .map(|(a, b)| f(a, b))
                            .collect()
                    }))
                }
            }
            // Broadcast scalar to match vector size
            (MaybeVector::Scalar(a), MaybeVector::Vector(b)) => {
                if b.len() < PAR_LANES_THRESHOLD {
                    MaybeVector::vector(b.iter().map(|bi| f(a, bi)).collect())
                } else {
                    MaybeVector::vector(par_concat(b.len(), |r| {
                        b[r].iter().map(|bi| f(a, bi)).collect()
                    }))
                }
            }
            (MaybeVector::Vector(a), MaybeVector::Scalar(b)) => {
                if a.len() < PAR_LANES_THRESHOLD {
                    MaybeVector::vector(a.iter().map(|ai| f(ai, b)).collect())
                } else {
                    MaybeVector::vector(par_concat(a.len(), |r| {
                        a[r].iter().map(|ai| f(ai, b)).collect()
                    }))
                }
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
    T: std::fmt::Debug + Clone + PartialEq + Eq + Send + Sync,
{
    if let [only] = kept {
        return MaybeVector::Scalar(vec[*only as usize].clone());
    }
    let mut filtered = Vec::with_capacity(kept.len());
    for &i in kept {
        filtered.push(vec[i as usize].clone());
    }
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
