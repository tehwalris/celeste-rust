//! Which conditional branches actually split the state set.
//!
//! A `ConditionalBranch` is free when its condition is uniform across lanes:
//! every lane goes the same way and nothing is cloned. It is expensive when the
//! condition varies, because then the state is filtered in two and both halves
//! are executed and later merged - and merging is 60% of runtime.
//!
//! The distinction is invisible in the program text, so choosing which branches
//! to rewrite has to be driven by this. `if_convert` was aimed at the 145
//! `and`/`or` triangles and removed only 4.6% of splits, because those
//! conditions are usually uniform. This module is how that would have been
//! known in advance.
//!
//! Recording is gated on tracing being enabled, so it costs one relaxed atomic
//! load per branch otherwise.

use std::sync::Mutex;

use rustc_hash::FxHashMap;

#[derive(Default, Clone, Copy)]
pub struct BranchSite {
    /// Times both edges received lanes, so the state had to be split.
    pub splits: u64,
    /// Times every lane went the same way.
    pub uniform: u64,
}

lazy_static::lazy_static! {
    static ref SITES: Mutex<FxHashMap<(String, String), BranchSite>> =
        Mutex::new(FxHashMap::default());
}

pub fn record(function: &str, block: &str, split: bool) {
    if !crate::interpreter::tracing::is_tracing_enabled() {
        return;
    }
    let mut sites = SITES.lock().unwrap();
    let entry = sites
        .entry((function.to_string(), block.to_string()))
        .or_default();
    if split {
        entry.splits += 1;
    } else {
        entry.uniform += 1;
    }
}

pub fn reset() {
    SITES.lock().unwrap().clear();
}

/// Sites that split at least once, worst first.
pub fn report() -> Vec<(String, String, BranchSite)> {
    let sites = SITES.lock().unwrap();
    let mut out: Vec<(String, String, BranchSite)> = sites
        .iter()
        .filter(|(_, s)| s.splits > 0)
        .map(|((f, b), s)| (f.clone(), b.clone(), *s))
        .collect();
    out.sort_by_key(|(f, b, s)| (std::cmp::Reverse(s.splits), f.clone(), b.clone()));
    out
}

/// (splitting executions, total executions, distinct splitting sites)
pub fn totals() -> (u64, u64, usize) {
    let sites = SITES.lock().unwrap();
    let splits: u64 = sites.values().map(|s| s.splits).sum();
    let total: u64 = sites.values().map(|s| s.splits + s.uniform).sum();
    let distinct = sites.values().filter(|s| s.splits > 0).count();
    (splits, total, distinct)
}
