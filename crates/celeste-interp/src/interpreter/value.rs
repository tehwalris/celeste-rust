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
    /// A per-lane tri-state bool: `Some(b)` where the comparison was
    /// definite for that lane, `None` where it genuinely straddles.
    ///
    /// TRANSIENT - see plans/tristate-plan.md. This value exists only
    /// between the instruction that computes it and that instruction's
    /// result being assigned, where `resolve_maybe_bool` turns it into a
    /// definite `Bool` by duplicating the ambiguous lanes. It must never
    /// reach the heap, a checkpoint or a row hash.
    ///
    /// The invariant is enforced rather than accommodated: the lane
    /// filters, splitters and expanders reject it loudly. A per-lane
    /// variant they merely *ignored* would not be resized with its
    /// neighbours, silently desyncing every downstream lane index -
    /// which is precisely the bug class this design avoids.
    ///
    /// Only ever constructed MIXED (at least one `Some` and one `None`):
    /// all-definite is a `Bool`, all-unknown is an `UnknownBool`, so
    /// every pre-existing path keeps its exact previous behaviour.
    ///
    /// Kept LAST in the enum on purpose: the variants above keep their
    /// bincode discriminants, so checkpoints written before this existed
    /// stay byte-compatible. Adding it mid-enum shifted every later
    /// variant and changed `states.bin` for a purely inert change - which
    /// would have silently invalidated a certified checkpoint universe.
    MaybeBool(MaybeVector<Option<bool>>),
}

/// Panic message for the lane-structural handlers. Centralised so the
/// invariant reads the same everywhere it is enforced.
pub const MAYBE_BOOL_ESCAPED: &str =
    "Value::MaybeBool escaped into a stored state - it is transient and must be \
     resolved by resolve_maybe_bool at the instruction that produced it \
     (see plans/tristate-plan.md)";

/// The lanes a filter keeps, as sorted half-open `[start, end)` ranges.
///
/// Kept lanes come in contiguous stretches - measured 30.6 lanes per run at
/// `filter_branch`, 19.5 at `filter_split_flr` - because adjacent lanes
/// share history (concat + first-occurrence dedup preserve arrival order),
/// so they usually agree on a branch condition. Ranges turn the per-lane
/// gather every filtered vector pays into a few `extend_from_slice` calls
/// (memcpy for the `Copy` payloads all lanes are), and they are computed
/// once per state filter, not per vector.
pub struct KeptLanes {
    ranges: Vec<(u32, u32)>,
    total: usize,
}

impl KeptLanes {
    pub fn from_mask(mask: &[bool]) -> Self {
        let mut ranges = Vec::new();
        let mut total = 0usize;
        let mut i = 0usize;
        while i < mask.len() {
            if mask[i] {
                let start = i;
                while i < mask.len() && mask[i] {
                    i += 1;
                }
                ranges.push((start as u32, i as u32));
                total += i - start;
            } else {
                i += 1;
            }
        }
        Self { ranges, total }
    }

    pub fn len(&self) -> usize {
        self.total
    }

    pub fn is_empty(&self) -> bool {
        self.total == 0
    }

    pub fn first(&self) -> Option<u32> {
        self.ranges.first().map(|&(s, _)| s)
    }
}

impl Value {
}

impl HeapValue {
}

/// Filter a vector down to the kept lanes. O(kept) per vector instead of
/// O(mask): a filter touches every vector value in the state, so
/// re-scanning the full mask per vector was the dominant cost of
/// `filter_branch`. Range-at-a-time on top of that: `extend_from_slice`
/// per run instead of an indexed copy per lane.
#[inline]
fn filter_vec_gather_ref<T: Clone + PartialEq>(vec: &[T], kept: &KeptLanes) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
    if kept.total == 1 {
        let only = kept.first().expect("total 1 implies a range");
        return MaybeVector::Scalar(vec[only as usize].clone());
    }
    let mut filtered = Vec::with_capacity(kept.total);
    for &(start, end) in &kept.ranges {
        filtered.extend_from_slice(&vec[start as usize..end as usize]);
    }
    MaybeVector::vector(filtered)
}

impl Value {
    /// Filter vectors, returning Some(new_value) only if the value is a vector.
    /// Returns None for scalars (no transformation needed).
    /// This avoids cloning scalar values that don't need transformation.
    #[inline]
    pub fn filter_vectors_if_vector(&self, kept: &KeptLanes) -> Option<Self> {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => Some(Value::Bool(filter_vec_gather_ref(vec, kept))),
            Value::Number(MaybeVector::Vector(vec)) => Some(Value::Number(filter_vec_gather_ref(vec, kept))),
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Some(Value::NumberInterval(filter_vec_gather_ref(vec, kept)))
            }
            // A vector NOT gathered here keeps its old lane count while its
            // neighbours shrink - silent desync. MaybeBool is transient and
            // cannot arrive, so say so rather than falling through to None.
            Value::MaybeBool(_) => panic!("{}", MAYBE_BOOL_ESCAPED),
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
    pub fn filter_vectors_if_needed(&self, kept: &KeptLanes) -> Option<Self> {
        match self {
            HeapValue::Value(v) => v.filter_vectors_if_vector(kept).map(HeapValue::Value),
            HeapValue::Closure(id, captures) => {
                // Check if any capture is a vector
                let mut any_vector = false;
                for cap in captures {
                    if matches!(cap,
                        Value::Bool(MaybeVector::Vector(_)) |
                        Value::Number(MaybeVector::Vector(_)) |
                        Value::NumberInterval(MaybeVector::Vector(_)) |
                        // As above: route to the guard rather than skip.
                        Value::MaybeBool(_)
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
