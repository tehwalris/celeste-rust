//! TraceCache: caches execution traces for reuse.
//!
//! Phase 3: Symbolic caching with substitution.
//! Stores symbolic expressions that can be evaluated with different inputs.

use rustc_hash::FxHashMap;

use crate::interpreter::state::State;
use crate::interpreter::vectorize::StateShape;
use crate::interpreter::symbolic::{ConcreteValue, Substitution, evaluate_sym_expr};
use crate::interpreter::value::{Value, HeapValue, MaybeVector};

use super::PathCounter;
use super::tracer::{InputSymbolMap, HeapSymbolMap};

/// A cached execution trace with symbolic information.
#[derive(Clone, Debug)]
pub struct CachedTrace {
    /// The path taken through the code.
    pub path: PathCounter,
    /// Number of forced choices made on this path (for path enumeration).
    pub forced_choices: usize,
    /// Template output state (structure is correct, values are from the original trace).
    /// Used as a template for reconstruction.
    pub output_state: State,
    /// Symbolic expressions for output heap values.
    /// Maps HeapId -> SymExpr showing how the value was computed.
    pub heap_symbols: HeapSymbolMap,
    /// Mapping from input symbols to source heap locations.
    /// Maps SymbolId -> HeapId in the *input* state.
    pub input_symbols: InputSymbolMap,
}

impl CachedTrace {
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
    fn build_substitution(&self, input_state: &State) -> Substitution {
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
        ConcreteValue::String(s) => Value::String((*s).clone()),
        ConcreteValue::Nil => Value::Nil(None),
        ConcreteValue::Pointer(h) => Value::Pointer(h),
    }
}

/// Cache key: (shape, path)
/// Two states with the same shape taking the same path will produce
/// structurally identical output states (with different values).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CacheKey {
    pub shape: StateShape,
    pub path: Vec<(usize, usize)>,
}

/// Cache for execution traces.
pub struct TraceCache {
    /// Traces indexed by (shape, path).
    traces: FxHashMap<CacheKey, CachedTrace>,
    /// Statistics
    pub hits: usize,
    pub misses: usize,
}

impl TraceCache {
    pub fn new() -> Self {
        Self {
            traces: FxHashMap::default(),
            hits: 0,
            misses: 0,
        }
    }

    /// Look up a cached trace for the given shape and path.
    pub fn get(&mut self, shape: &StateShape, path: &PathCounter) -> Option<&CachedTrace> {
        let key = CacheKey {
            shape: shape.clone(),
            path: path.choices().to_vec(),
        };
        if let Some(trace) = self.traces.get(&key) {
            self.hits += 1;
            Some(trace)
        } else {
            self.misses += 1;
            None
        }
    }

    /// Insert a new trace into the cache.
    pub fn insert(
        &mut self,
        shape: StateShape,
        path: PathCounter,
        forced_choices: usize,
        output_state: State,
        heap_symbols: HeapSymbolMap,
        input_symbols: InputSymbolMap,
    ) {
        let key = CacheKey {
            shape,
            path: path.choices().to_vec(),
        };
        self.traces.insert(key, CachedTrace {
            path,
            forced_choices,
            output_state,
            heap_symbols,
            input_symbols,
        });
    }

    /// Get the number of cached traces.
    pub fn len(&self) -> usize {
        self.traces.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.traces.is_empty()
    }

    /// Get cache statistics.
    pub fn stats(&self) -> CacheStats {
        CacheStats {
            num_traces: self.traces.len(),
            hits: self.hits,
            misses: self.misses,
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
    pub hits: usize,
    pub misses: usize,
    pub hit_rate: f64,
}

impl std::fmt::Display for CacheStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Cache: {} traces, {} hits, {} misses ({:.1}% hit rate)",
            self.num_traces,
            self.hits,
            self.misses,
            self.hit_rate * 100.0
        )
    }
}
