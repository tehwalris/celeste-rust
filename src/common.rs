//! Common type aliases used throughout the codebase.
//!
//! This module provides fast hash map and hash set types using the FxHasher algorithm,
//! which is optimized for short keys like integer IDs.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

/// Type alias for HashMap using FxHasher for faster hashing of small keys.
pub type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// Type alias for HashSet using FxHasher for faster hashing of small keys.
pub type FxHashSet<T> = HashSet<T, BuildHasherDefault<FxHasher>>;

/// Type alias for the hasher builder used in FxHashMap and FxHashSet.
pub type FxBuildHasher = BuildHasherDefault<FxHasher>;
