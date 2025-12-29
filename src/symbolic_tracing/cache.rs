//! TraceCache: caches execution traces for reuse.
//!
//! Phase 1: Simple path-based caching (shape, path) -> output_state
//! Later phases will add symbolic expressions for more flexible matching.

use rustc_hash::FxHashMap;

use crate::interpreter::state::State;
use crate::interpreter::vectorize::StateShape;

use super::PathCounter;

/// A cached execution trace.
#[derive(Clone, Debug)]
pub struct CachedTrace {
    /// The path taken through the code.
    pub path: PathCounter,
    /// The output state produced by this path.
    /// In Phase 1, this is a concrete state.
    /// In later phases, this will be a symbolic template.
    pub output_state: State,
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
    pub fn insert(&mut self, shape: StateShape, path: PathCounter, output_state: State) {
        let key = CacheKey {
            shape,
            path: path.choices().to_vec(),
        };
        self.traces.insert(key, CachedTrace {
            path,
            output_state,
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
