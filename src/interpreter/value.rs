use std::hash::BuildHasherDefault;
use std::sync::{Arc, OnceLock};

use itertools::Itertools;
use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use super::heap::HeapId;
use crate::{ir::GlobalId, pico8_num::{Pico8Num, Pico8NumInterval}};

// Use FxHashMap for faster hashing in ObjectTable
type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// A value that is either a scalar or a vector (possibly lazy-filtered).
/// Vectors are reference-counted for efficient sharing across cloned states.
#[derive(Debug, Clone)]
pub enum MaybeVector<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    Scalar(T),
    /// Materialized vector - data is reference-counted for sharing
    Vector(Arc<Vec<T>>),
    /// Lazy filtered vector - stores original data and a mask.
    /// The effective elements are those where mask[i] is true.
    /// This avoids materializing filtered data until it's actually needed.
    /// The `materialized` cache is shared across clones - once any clone
    /// materializes the vector, all clones can use the cached result.
    LazyVector {
        data: Arc<Vec<T>>,
        mask: Arc<Vec<bool>>,
        /// Cached count of true values in mask (= effective length)
        len: usize,
        /// Cached materialized result - shared across clones via Arc<OnceLock<...>>.
        /// Using OnceLock ensures thread-safe one-time initialization.
        /// The inner Arc<Vec<T>> allows cheap cloning of the result.
        materialized: Arc<OnceLock<Arc<Vec<T>>>>,
    },
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> PartialEq for MaybeVector<T> {
    fn eq(&self, other: &Self) -> bool {
        // For equality, we compare the effective values
        match (self, other) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => a == b,
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => a.as_ref() == b.as_ref(),
            // For lazy vectors, materialize and compare
            _ => {
                let a = self.materialize_if_lazy();
                let b = other.materialize_if_lazy();
                match (a, b) {
                    (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => a == b,
                    (MaybeVector::Vector(a), MaybeVector::Vector(b)) => a.as_ref() == b.as_ref(),
                    _ => false,
                }
            }
        }
    }
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> Eq for MaybeVector<T> {}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq + Serialize> Serialize for MaybeVector<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Always serialize as materialized form, matching the derived enum format
        #[derive(Serialize)]
        enum MaybeVectorHelper<'a, T> {
            Scalar(&'a T),
            Vector(&'a Vec<T>),
        }

        match self.materialize_if_lazy() {
            MaybeVector::Scalar(v) => MaybeVectorHelper::Scalar(&v).serialize(serializer),
            MaybeVector::Vector(v) => MaybeVectorHelper::Vector(v.as_ref()).serialize(serializer),
            MaybeVector::LazyVector { .. } => unreachable!("materialize_if_lazy returned LazyVector"),
        }
    }
}

impl<'de, T: std::fmt::Debug + Clone + PartialEq + Eq + Deserialize<'de>> Deserialize<'de> for MaybeVector<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        // Match the derived enum format
        #[derive(Deserialize)]
        enum MaybeVectorHelper<T> {
            Scalar(T),
            Vector(Vec<T>),
        }

        match MaybeVectorHelper::deserialize(deserializer)? {
            MaybeVectorHelper::Scalar(v) => Ok(MaybeVector::Scalar(v)),
            MaybeVectorHelper::Vector(v) => Ok(MaybeVector::Vector(Arc::new(v))),
        }
    }
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> MaybeVector<T> {
    /// Returns the effective length of this vector (1 for scalar)
    #[inline]
    pub fn len(&self) -> usize {
        match self {
            MaybeVector::Scalar(_) => 1,
            MaybeVector::Vector(v) => v.len(),
            MaybeVector::LazyVector { len, .. } => *len,
        }
    }

    /// Materialize a lazy vector into a regular vector (or scalar if len=1)
    /// Returns self unchanged if not lazy.
    ///
    /// Uses a shared cache so that multiple clones of the same LazyVector
    /// only compute the materialization once.
    pub fn materialize_if_lazy(&self) -> Self {
        match self {
            MaybeVector::LazyVector { data, mask, len, materialized } => {
                if *len == 1 {
                    // Single element - find it and return as scalar
                    // (No caching needed for scalars - they're cheap)
                    for (v, &m) in data.iter().zip(mask.iter()) {
                        if m {
                            return MaybeVector::Scalar(v.clone());
                        }
                    }
                    unreachable!("len was 1 but no true found in mask")
                } else {
                    // Check cache first (via get_or_init for thread-safety)
                    let cached = materialized.get_or_init(|| {
                        // Compute the filtered vector
                        let mut filtered = Vec::with_capacity(*len);
                        for (v, &m) in data.iter().zip(mask.iter()) {
                            if m {
                                filtered.push(v.clone());
                            }
                        }
                        Arc::new(filtered)
                    });
                    // Return a Vector sharing the cached Arc - just an Arc::clone!
                    MaybeVector::Vector(Arc::clone(cached))
                }
            }
            // Already materialized
            other => other.clone(),
        }
    }

    /// Materialize in place, converting lazy to concrete
    pub fn materialize_in_place(&mut self) {
        if matches!(self, MaybeVector::LazyVector { .. }) {
            *self = self.materialize_if_lazy();
        }
    }

    /// Sparsity threshold: if active elements / original length < this ratio,
    /// eagerly materialize instead of creating a lazy vector.
    /// This prevents keeping huge original vectors around for small results.
    /// Higher values = more aggressive eager materialization.
    const SPARSITY_THRESHOLD: f64 = 1.0;

    /// Apply a filter mask to create a lazy vector.
    /// If already lazy, combines masks efficiently.
    /// If the result would be too sparse (< SPARSITY_THRESHOLD), eagerly materializes.
    pub fn filter_lazy(self, new_mask: &[bool], new_len: usize) -> Self {
        if new_len == 0 {
            panic!("Cannot filter to zero elements");
        }

        match self {
            MaybeVector::Scalar(v) => {
                // Scalar stays scalar (mask should be [true])
                debug_assert!(new_mask.len() == 1 && new_mask[0]);
                MaybeVector::Scalar(v)
            }
            MaybeVector::Vector(v) => {
                if new_len == 1 {
                    // Collapse to scalar
                    for (val, &m) in v.iter().zip(new_mask.iter()) {
                        if m {
                            return MaybeVector::Scalar(val.clone());
                        }
                    }
                    unreachable!("new_len was 1 but no true found")
                } else if new_len == v.len() {
                    // No filtering needed
                    MaybeVector::Vector(v)
                } else {
                    // Check sparsity - if too sparse, materialize eagerly
                    let sparsity = new_len as f64 / v.len() as f64;
                    if sparsity < Self::SPARSITY_THRESHOLD {
                        // Eagerly materialize
                        let mut filtered = Vec::with_capacity(new_len);
                        for (val, &m) in v.iter().zip(new_mask.iter()) {
                            if m {
                                filtered.push(val.clone());
                            }
                        }
                        MaybeVector::Vector(Arc::new(filtered))
                    } else {
                        // Create lazy vector - v is already Arc<Vec<T>>
                        MaybeVector::LazyVector {
                            data: v,
                            mask: Arc::new(new_mask.to_vec()),
                            len: new_len,
                            materialized: Arc::new(OnceLock::new()),
                        }
                    }
                }
            }
            MaybeVector::LazyVector { data, mask, len: old_len, materialized } => {
                debug_assert_eq!(new_mask.len(), old_len);

                if new_len == old_len {
                    // No change
                    return MaybeVector::LazyVector { data, mask, len: old_len, materialized };
                }

                // Combine masks: for each position in the new mask (which has old_len positions),
                // map it to the corresponding position in the original mask
                let mut combined_mask = Vec::with_capacity(data.len());
                let mut new_mask_iter = new_mask.iter();

                for &orig_m in mask.iter() {
                    if orig_m {
                        // This position was active, check new mask
                        let &new_m = new_mask_iter.next().expect("new_mask too short");
                        combined_mask.push(new_m);
                    } else {
                        // This position was already masked out
                        combined_mask.push(false);
                    }
                }

                if new_len == 1 {
                    // Collapse to scalar
                    for (val, &m) in data.iter().zip(combined_mask.iter()) {
                        if m {
                            return MaybeVector::Scalar(val.clone());
                        }
                    }
                    unreachable!("new_len was 1 but no true found")
                }

                // Check sparsity relative to ORIGINAL data length
                let sparsity = new_len as f64 / data.len() as f64;
                if sparsity < Self::SPARSITY_THRESHOLD {
                    // Too sparse - eagerly materialize to avoid keeping huge original data
                    let mut filtered = Vec::with_capacity(new_len);
                    for (val, &m) in data.iter().zip(combined_mask.iter()) {
                        if m {
                            filtered.push(val.clone());
                        }
                    }
                    MaybeVector::Vector(Arc::new(filtered))
                } else {
                    // When combining masks, the cache is invalidated (new mask = new result)
                    MaybeVector::LazyVector {
                        data,
                        mask: Arc::new(combined_mask),
                        len: new_len,
                        materialized: Arc::new(OnceLock::new()),
                    }
                }
            }
        }
    }

    /// Create a lazy-filtered version by reference (doesn't consume self)
    pub fn filter_lazy_ref(&self, new_mask: &[bool], new_len: usize) -> Self {
        if new_len == 0 {
            panic!("Cannot filter to zero elements");
        }

        match self {
            MaybeVector::Scalar(v) => {
                debug_assert!(new_mask.len() == 1 && new_mask[0]);
                MaybeVector::Scalar(v.clone())
            }
            MaybeVector::Vector(v) => {
                if new_len == 1 {
                    for (val, &m) in v.iter().zip(new_mask.iter()) {
                        if m {
                            return MaybeVector::Scalar(val.clone());
                        }
                    }
                    unreachable!("new_len was 1 but no true found")
                } else if new_len == v.len() {
                    MaybeVector::Vector(Arc::clone(v))
                } else {
                    // Check sparsity - if too sparse, materialize eagerly
                    let sparsity = new_len as f64 / v.len() as f64;
                    if sparsity < Self::SPARSITY_THRESHOLD {
                        let mut filtered = Vec::with_capacity(new_len);
                        for (val, &m) in v.iter().zip(new_mask.iter()) {
                            if m {
                                filtered.push(val.clone());
                            }
                        }
                        MaybeVector::Vector(Arc::new(filtered))
                    } else {
                        // Share the existing Arc, don't clone the Vec
                        MaybeVector::LazyVector {
                            data: Arc::clone(v),
                            mask: Arc::new(new_mask.to_vec()),
                            len: new_len,
                            materialized: Arc::new(OnceLock::new()),
                        }
                    }
                }
            }
            MaybeVector::LazyVector { data, mask, len: old_len, materialized } => {
                debug_assert_eq!(new_mask.len(), *old_len);

                if new_len == *old_len {
                    // No change - return a clone sharing the same cache
                    return MaybeVector::LazyVector {
                        data: Arc::clone(data),
                        mask: Arc::clone(mask),
                        len: *old_len,
                        materialized: Arc::clone(materialized),
                    };
                }

                let mut combined_mask = Vec::with_capacity(data.len());
                let mut new_mask_iter = new_mask.iter();

                for &orig_m in mask.iter() {
                    if orig_m {
                        let &new_m = new_mask_iter.next().expect("new_mask too short");
                        combined_mask.push(new_m);
                    } else {
                        combined_mask.push(false);
                    }
                }

                if new_len == 1 {
                    for (val, &m) in data.iter().zip(combined_mask.iter()) {
                        if m {
                            return MaybeVector::Scalar(val.clone());
                        }
                    }
                    unreachable!("new_len was 1 but no true found")
                }

                // Check sparsity relative to ORIGINAL data length
                let sparsity = new_len as f64 / data.len() as f64;
                if sparsity < Self::SPARSITY_THRESHOLD {
                    // Too sparse - eagerly materialize
                    let mut filtered = Vec::with_capacity(new_len);
                    for (val, &m) in data.iter().zip(combined_mask.iter()) {
                        if m {
                            filtered.push(val.clone());
                        }
                    }
                    MaybeVector::Vector(Arc::new(filtered))
                } else {
                    // New mask = new cache
                    MaybeVector::LazyVector {
                        data: Arc::clone(data),
                        mask: Arc::new(combined_mask),
                        len: new_len,
                        materialized: Arc::new(OnceLock::new()),
                    }
                }
            }
        }
    }

    pub fn map(&self, f: impl Fn(&T) -> T) -> Self {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => MaybeVector::Vector(Arc::new(v.iter().map(f).collect())),
            MaybeVector::LazyVector { data, mask, len, .. } => {
                // Only map the active elements
                let mut result = Vec::with_capacity(*len);
                for (v, &m) in data.iter().zip(mask.iter()) {
                    if m {
                        result.push(f(v));
                    }
                }
                if result.len() == 1 {
                    MaybeVector::Scalar(result.pop().unwrap())
                } else {
                    MaybeVector::Vector(Arc::new(result))
                }
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
            MaybeVector::Vector(v) => MaybeVector::Vector(Arc::new(v.iter().map(f).collect())),
            MaybeVector::LazyVector { data, mask, len, .. } => {
                let mut result = Vec::with_capacity(*len);
                for (v, &m) in data.iter().zip(mask.iter()) {
                    if m {
                        result.push(f(v));
                    }
                }
                if result.len() == 1 {
                    MaybeVector::Scalar(result.pop().unwrap())
                } else {
                    MaybeVector::Vector(Arc::new(result))
                }
            }
        }
    }

    pub fn map2<O: std::fmt::Debug + Clone + PartialEq + Eq>(
        a: &Self,
        b: &Self,
        f: impl Fn(&T, &T) -> O,
    ) -> MaybeVector<O> {
        // Handle scalar cases first (most common in practice)
        match (a, b) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => MaybeVector::Scalar(f(a, b)),
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => {
                MaybeVector::Vector(Arc::new(a.iter().zip_eq(b.iter()).map(|(a, b)| f(a, b)).collect()))
            }
            (MaybeVector::Scalar(a), MaybeVector::Vector(b)) => {
                MaybeVector::Vector(Arc::new(b.iter().map(|bi| f(a, bi)).collect()))
            }
            (MaybeVector::Vector(a), MaybeVector::Scalar(b)) => {
                MaybeVector::Vector(Arc::new(a.iter().map(|ai| f(ai, b)).collect()))
            }
            // Lazy cases - materialize and compute
            _ => {
                let a_mat = a.materialize_if_lazy();
                let b_mat = b.materialize_if_lazy();
                Self::map2(&a_mat, &b_mat, f)
            }
        }
    }

    /// Check if this is a lazy vector
    #[inline]
    pub fn is_lazy(&self) -> bool {
        matches!(self, MaybeVector::LazyVector { .. })
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

#[inline]
fn filter_vec_by_mask<T: Clone + PartialEq>(vec: Vec<T>, mask: &[bool], true_count: usize) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
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
        MaybeVector::Vector(Arc::new(filtered))
    }
}

/// Filter a vector by mask, cloning elements (for use with references)
#[inline]
fn filter_vec_by_mask_ref<T: Clone + PartialEq>(vec: &[T], mask: &[bool], true_count: usize) -> MaybeVector<T>
where
    T: std::fmt::Debug + Clone + PartialEq + Eq,
{
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
        MaybeVector::Vector(Arc::new(filtered))
    }
}

impl Value {
    /// Filter vectors using lazy evaluation
    pub fn filter_vectors_lazy(self, mask: &[bool], true_count: usize) -> Self {
        match self {
            Value::Bool(v) => Value::Bool(v.filter_lazy(mask, true_count)),
            Value::Number(v) => Value::Number(v.filter_lazy(mask, true_count)),
            Value::NumberInterval(v) => Value::NumberInterval(v.filter_lazy(mask, true_count)),
            other => other,
        }
    }

    /// Filter vectors using lazy evaluation (by reference)
    pub fn filter_vectors_lazy_ref(&self, mask: &[bool], true_count: usize) -> Self {
        match self {
            Value::Bool(v) => Value::Bool(v.filter_lazy_ref(mask, true_count)),
            Value::Number(v) => Value::Number(v.filter_lazy_ref(mask, true_count)),
            Value::NumberInterval(v) => Value::NumberInterval(v.filter_lazy_ref(mask, true_count)),
            other => other.clone(),
        }
    }

    pub fn filter_vectors(self, mask: &[bool], true_count: usize) -> Self {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => Value::Bool(filter_vec_by_mask(vec.to_vec(), mask, true_count)),
            Value::Number(MaybeVector::Vector(vec)) => Value::Number(filter_vec_by_mask(vec.to_vec(), mask, true_count)),
            Value::NumberInterval(MaybeVector::Vector(vec)) => {
                Value::NumberInterval(filter_vec_by_mask(vec.to_vec(), mask, true_count))
            }
            _ => self,
        }
    }

    /// Filter vectors, returning Some(new_value) only if the value is a vector.
    /// Returns None for scalars (no transformation needed).
    /// This avoids cloning scalar values that don't need transformation.
    /// Now uses lazy filtering for efficiency.
    #[inline]
    pub fn filter_vectors_if_vector(&self, mask: &[bool], true_count: usize) -> Option<Self> {
        match self {
            Value::Bool(v @ MaybeVector::Vector(_)) | Value::Bool(v @ MaybeVector::LazyVector { .. }) => {
                Some(Value::Bool(v.filter_lazy_ref(mask, true_count)))
            }
            Value::Number(v @ MaybeVector::Vector(_)) | Value::Number(v @ MaybeVector::LazyVector { .. }) => {
                Some(Value::Number(v.filter_lazy_ref(mask, true_count)))
            }
            Value::NumberInterval(v @ MaybeVector::Vector(_)) | Value::NumberInterval(v @ MaybeVector::LazyVector { .. }) => {
                Some(Value::NumberInterval(v.filter_lazy_ref(mask, true_count)))
            }
            _ => None,
        }
    }

    /// Check if this value contains any lazy vectors
    pub fn has_lazy_vectors(&self) -> bool {
        match self {
            Value::Bool(v) => v.is_lazy(),
            Value::Number(v) => v.is_lazy(),
            Value::NumberInterval(v) => v.is_lazy(),
            _ => false,
        }
    }

    /// Materialize any lazy vectors in this value (mutable)
    pub fn materialize_lazy(&mut self) {
        match self {
            Value::Bool(v) => v.materialize_in_place(),
            Value::Number(v) => v.materialize_in_place(),
            Value::NumberInterval(v) => v.materialize_in_place(),
            _ => {}
        }
    }

    /// Return a materialized copy of this value (converting lazy vectors to concrete)
    pub fn materialize_if_lazy(&self) -> Self {
        match self {
            Value::Bool(v) => Value::Bool(v.materialize_if_lazy()),
            Value::Number(v) => Value::Number(v.materialize_if_lazy()),
            Value::NumberInterval(v) => Value::NumberInterval(v.materialize_if_lazy()),
            other => other.clone(),
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
    /// Filter vectors in this heap value using lazy evaluation.
    /// Returns Some(new_value) only if transformation is needed.
    /// Returns None for values that don't contain vectors (no transformation needed).
    #[inline]
    pub fn filter_vectors_if_needed(&self, mask: &[bool], true_count: usize) -> Option<Self> {
        match self {
            HeapValue::Value(v) => v.filter_vectors_if_vector(mask, true_count).map(HeapValue::Value),
            HeapValue::Closure(id, captures) => {
                // Check if any capture is a vector or lazy vector
                let mut any_vector = false;
                for cap in captures {
                    if matches!(cap,
                        Value::Bool(MaybeVector::Vector(_) | MaybeVector::LazyVector { .. }) |
                        Value::Number(MaybeVector::Vector(_) | MaybeVector::LazyVector { .. }) |
                        Value::NumberInterval(MaybeVector::Vector(_) | MaybeVector::LazyVector { .. })
                    ) {
                        any_vector = true;
                        break;
                    }
                }
                if any_vector {
                    // Need to transform - filter lazily
                    let new_captures: Vec<Value> = captures
                        .iter()
                        .map(|v| v.filter_vectors_if_vector(mask, true_count).unwrap_or_else(|| v.clone()))
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

    /// Materialize all lazy vectors in this heap value
    pub fn materialize_lazy_vectors(&mut self) {
        match self {
            HeapValue::Value(v) => v.materialize_lazy(),
            HeapValue::Closure(_, captures) => {
                for cap in captures {
                    cap.materialize_lazy();
                }
            }
            // These don't contain vectorizable values
            HeapValue::ObjectTable(_)
            | HeapValue::ArrayTable(_)
            | HeapValue::UnknownTable
            | HeapValue::BuiltinFun(_) => {}
        }
    }
}
