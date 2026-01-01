//! TraceCache: caches execution traces for reuse.
//!
//! Phase 3: Symbolic caching with path condition checking.
//!
//! The cache stores traces indexed by (shape, abstract_path). Multiple traces
//! can exist for the same key if they take different concrete branches.
//! When looking up, we check if the input state satisfies any cached trace's
//! path conditions (symbolic expressions that were true during tracing).

use rustc_hash::FxHashMap;

use crate::interpreter::state::State;
use crate::interpreter::vectorize::StateShape;
use crate::interpreter::symbolic::{ConcreteValue, SymExpr, Substitution, evaluate_sym_expr};
use crate::interpreter::value::{Value, HeapValue, MaybeVector};

use super::PathCounter;
use super::tracer::{ConcretePath, InputSymbolMap, HeapSymbolMap};

/// A cached execution trace with symbolic information.
#[derive(Clone, Debug)]
pub struct CachedTrace {
    /// The abstract path taken through the code (UnknownBool branches).
    pub path: PathCounter,
    /// The concrete path taken (value-dependent branch outcomes).
    pub concrete_path: ConcretePath,
    /// Number of forced choices made on this path (for path enumeration).
    pub forced_choices: usize,
    /// Number of concrete branches taken.
    pub concrete_branches: usize,
    /// Template output state (structure is correct, values are from the original trace).
    /// Used as a template for reconstruction.
    pub output_state: State,
    /// Symbolic expressions for output heap values.
    /// Maps HeapId -> SymExpr showing how the value was computed.
    pub heap_symbols: HeapSymbolMap,
    /// Mapping from input symbols to source heap locations.
    /// Maps SymbolId -> HeapId in the *input* state.
    pub input_symbols: InputSymbolMap,
    /// Path conditions: symbolic boolean expressions that must all evaluate to true
    /// for this trace to be applicable to an input state.
    pub path_conditions: Vec<SymExpr>,
}

impl CachedTrace {
    /// Check if this trace's path conditions are satisfied by the given input state.
    /// Returns true if all path conditions evaluate to true.
    pub fn check_conditions(&self, input_state: &State) -> bool {
        if self.path_conditions.is_empty() {
            return true;
        }

        let substitution = self.build_substitution(input_state);

        for condition in &self.path_conditions {
            let result = evaluate_sym_expr(condition, &substitution);
            match result {
                ConcreteValue::Bool(true) => continue,
                ConcreteValue::Bool(false) => return false,
                // Non-boolean conditions are treated as "unknown" - don't match
                _ => return false,
            }
        }

        true
    }

    /// Apply this cached trace to a new input state, producing an output state.
    ///
    /// 1. Build substitution: for each input symbol, get the concrete value from input_state
    /// 2. Clone the template output state
    /// 3. For each heap value with a symbol, evaluate the symbol and update the value
    pub fn apply(&self, input_state: &State) -> State {
        // Build substitution map: SymbolId -> ConcreteValue
        let substitution = self.build_substitution(input_state);

        // Clone the template output state
        let mut output_state = self.output_state.clone();

        // Apply substitutions to heap values
        for (&heap_id, sym_expr) in &self.heap_symbols {
            let concrete_value = evaluate_sym_expr(sym_expr, &substitution);
            let new_value = concrete_value_to_value(concrete_value);

            // Update the heap value
            if let Some(heap_val) = output_state.heap.get_opt(heap_id) {
                if let HeapValue::Value(_) = heap_val {
                    output_state.heap.set(heap_id, HeapValue::Value(new_value));
                }
            }
        }

        output_state
    }

    /// Build a substitution map from the input state.
    pub fn build_substitution(&self, input_state: &State) -> Substitution {
        let mut subst = Substitution::new();

        for (&sym_id, &heap_id) in &self.input_symbols {
            // Get the value at this heap location in the input state
            if let Some(heap_val) = input_state.heap.get_opt(heap_id) {
                if let HeapValue::Value(value) = heap_val {
                    if let Some(concrete) = value_to_concrete_value(value) {
                        subst.insert(sym_id, concrete);
                    }
                }
            }
        }

        subst
    }
}

/// Convert a Value to a ConcreteValue (for substitution).
fn value_to_concrete_value(value: &Value) -> Option<ConcreteValue> {
    use std::sync::Arc;
    match value {
        Value::Number(MaybeVector::Scalar(n)) => Some(ConcreteValue::Number(*n)),
        Value::NumberInterval(MaybeVector::Scalar(i)) => Some(ConcreteValue::NumberInterval(*i)),
        Value::Bool(MaybeVector::Scalar(b)) => Some(ConcreteValue::Bool(*b)),
        Value::String(s) => Some(ConcreteValue::String(Arc::new(s.clone()))),
        Value::Nil(_) => Some(ConcreteValue::Nil),
        Value::Pointer(h) => Some(ConcreteValue::Pointer(*h)),
        // Vectors and abstract values - shouldn't appear in scalar tracing
        _ => None,
    }
}

/// Convert a ConcreteValue back to a Value.
fn concrete_value_to_value(concrete: ConcreteValue) -> Value {
    match concrete {
        ConcreteValue::Number(n) => Value::Number(MaybeVector::Scalar(n)),
        ConcreteValue::NumberInterval(i) => Value::NumberInterval(MaybeVector::Scalar(i)),
        ConcreteValue::Bool(b) => Value::Bool(MaybeVector::Scalar(b)),
        ConcreteValue::UnknownBool => Value::UnknownBool,
        ConcreteValue::String(s) => Value::String((*s).clone()),
        ConcreteValue::Nil => Value::Nil(None),
        ConcreteValue::Pointer(h) => Value::Pointer(h),
    }
}

/// Cache key: (shape, abstract_path) - no concrete_path!
/// Multiple traces with different concrete paths can exist for the same key.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CacheKey {
    pub shape: StateShape,
    /// Abstract path choices (for UnknownBool branches)
    pub path: Vec<(usize, usize)>,
}

/// Cache for execution traces.
///
/// The cache maps (shape, abstract_path) to a list of traces. When looking up,
/// we check each trace's path conditions against the input state to find a match.
pub struct TraceCache {
    /// Traces indexed by (shape, path). Multiple traces per key are possible.
    traces: FxHashMap<CacheKey, Vec<CachedTrace>>,
    /// Statistics
    pub hits: usize,
    pub misses: usize,
    /// Number of condition checks performed
    pub condition_checks: usize,
}

impl TraceCache {
    pub fn new() -> Self {
        Self {
            traces: FxHashMap::default(),
            hits: 0,
            misses: 0,
            condition_checks: 0,
        }
    }

    /// Look up a cached trace for the given shape and abstract path.
    /// Checks path conditions to find a matching trace.
    /// Returns the matching trace if found.
    pub fn get_matching(
        &mut self,
        shape: &StateShape,
        path: &PathCounter,
        input_state: &State,
    ) -> Option<&CachedTrace> {
        let key = CacheKey {
            shape: shape.clone(),
            path: path.choices().to_vec(),
        };

        if let Some(traces) = self.traces.get(&key) {
            for trace in traces {
                self.condition_checks += 1;
                if trace.check_conditions(input_state) {
                    self.hits += 1;
                    return Some(trace);
                }
            }
        }

        self.misses += 1;
        None
    }

    /// Insert a new trace into the cache.
    pub fn insert(
        &mut self,
        shape: StateShape,
        path: PathCounter,
        concrete_path: ConcretePath,
        forced_choices: usize,
        concrete_branches: usize,
        output_state: State,
        heap_symbols: HeapSymbolMap,
        input_symbols: InputSymbolMap,
        path_conditions: Vec<SymExpr>,
    ) {
        let key = CacheKey {
            shape,
            path: path.choices().to_vec(),
        };

        let trace = CachedTrace {
            path,
            concrete_path,
            forced_choices,
            concrete_branches,
            output_state,
            heap_symbols,
            input_symbols,
            path_conditions,
        };

        self.traces.entry(key).or_insert_with(Vec::new).push(trace);
    }

    /// Get the number of cached trace entries (total traces across all keys).
    pub fn len(&self) -> usize {
        self.traces.values().map(|v| v.len()).sum()
    }

    /// Get the number of unique cache keys.
    pub fn num_keys(&self) -> usize {
        self.traces.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.traces.is_empty()
    }

    /// Get cache statistics.
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            num_traces: self.len(),
            num_keys: self.num_keys(),
            hits: self.hits,
            misses: self.misses,
            condition_checks: self.condition_checks,
            hit_rate: if self.hits + self.misses > 0 {
                self.hits as f64 / (self.hits + self.misses) as f64
            } else {
                0.0
            },
        }
    }

    /// Reset statistics.
    pub fn reset_stats(&mut self) {
        self.hits = 0;
        self.misses = 0;
        self.condition_checks = 0;
    }

    /// Clear all cached traces.
    pub fn clear(&mut self) {
        self.traces.clear();
        self.reset_stats();
    }
}

impl Default for TraceCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache statistics.
#[derive(Clone, Debug)]
pub struct CacheStats {
    pub num_traces: usize,
    pub num_keys: usize,
    pub hits: usize,
    pub misses: usize,
    pub condition_checks: usize,
    pub hit_rate: f64,
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cache: {} traces in {} keys, {} hits, {} misses ({:.1}% hit rate), {} condition checks",
            self.num_traces,
            self.num_keys,
            self.hits,
            self.misses,
            self.hit_rate * 100.0,
            self.condition_checks
        )
    }
}
