//! How often a `create_if_missing` accessor actually creates anything.
//!
//! `get_field %r.f create` is how the frontend compiles the *left* side of
//! `r.f = v`: an assignment needs a cell to store into, and Lua says assigning
//! to a missing field brings it into existence. So one instruction carries two
//! behaviours - a read, and a heap mutation - and which one happens is a
//! property of the run, not of the program text.
//!
//! That distinction is what blocks stage D. An arm of a branch cannot be
//! speculated if it might mutate the heap, and every field assignment inside an
//! arm is exactly that. All 61 accessors blocking an `if_convert` triangle are
//! `create` accessors; not one is a plain read.
//!
//! But the mutation is nearly always a fiction. Objects are built once with all
//! their fields, and `r.f = v` thereafter finds the cell already there. This
//! module is how that stops being a guess. Recording is gated on tracing, so it
//! costs one relaxed atomic load per accessor otherwise.

use std::sync::atomic::{AtomicU64, Ordering};

/// Which accessor did the work. They behave completely differently and the
/// aggregate over all three hides it.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Site {
    Global = 0,
    Field = 1,
    Index = 2,
}

impl Site {
    pub fn name(self) -> &'static str {
        match self {
            Site::Global => "get_global create",
            Site::Field => "get_field create",
            Site::Index => "get_index create",
        }
    }
}

static FOUND: [AtomicU64; 3] = [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)];
static CREATED: [AtomicU64; 3] = [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)];

/// `created` is false when the cell was already there, so the instruction
/// behaved as a plain read.
pub fn record(site: Site, created: bool) {
    if !crate::interpreter::tracing::is_tracing_enabled() {
        return;
    }
    let counters = if created { &CREATED } else { &FOUND };
    counters[site as usize].fetch_add(1, Ordering::Relaxed);
}

pub fn reset() {
    for i in 0..3 {
        FOUND[i].store(0, Ordering::Relaxed);
        CREATED[i].store(0, Ordering::Relaxed);
    }
}

/// `(site, found, created)` for each kind.
pub fn summary() -> Vec<(Site, u64, u64)> {
    [Site::Global, Site::Field, Site::Index]
        .into_iter()
        .map(|site| {
            (
                site,
                FOUND[site as usize].load(Ordering::Relaxed),
                CREATED[site as usize].load(Ordering::Relaxed),
            )
        })
        .collect()
}
