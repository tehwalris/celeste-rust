//! The earliest-convergence census (`CELESTE_WOULD_DEDUP=1`).
//!
//! Every end-of-frame merge removes ~90% of lanes as duplicates. The
//! question this census answers: *where in the frame did those lanes
//! become duplicates?* At each block execution it takes the states flowing
//! through, groups them by shape, hashes their rows exactly as the merge
//! would, and counts how many lanes would dedup if a merge ran right
//! there. Two counts per site:
//!
//!   * `dupes`: duplicates under the real columns - lanes that have
//!     already converged and are pure downstream waste from this point on;
//!   * `dupes_widened`: duplicates when the `__button_states` cells are
//!     ignored - lanes that *would* converge if dead button values were
//!     widened away (the `widen_buttons` idea), an upper bound on what
//!     widening + early dedup could reclaim at this point.
//!
//! Hash-only counting: a 64-bit collision undercounts distinct rows by an
//! astronomically small amount, which is fine for a census. The pass is
//! expensive (it hashes every state at every block) and exists only behind
//! the env var; measurement runs must not be timed.

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

use rustc_hash::{FxHashMap, FxHashSet};

use super::state::State;
use super::value::HeapValue;
use super::vectorize::shape_of_state;
use super::virtual_merge::{collect_columns_labeled, hash_rows, Column, Origin};

fn enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_WOULD_DEDUP").is_some())
}

#[derive(Default, Clone, Copy)]
struct SiteStats {
    execs: u64,
    lanes: u64,
    dupes: u64,
    dupes_widened: u64,
}

lazy_static::lazy_static! {
    static ref SITES: Mutex<FxHashMap<(String, String), SiteStats>> =
        Mutex::new(FxHashMap::default());
}
static TOTAL_LANES: AtomicU64 = AtomicU64::new(0);

/// The heap cells reachable from the `__button_states` array - the cells
/// the widened variant ignores.
fn button_cells(state: &State) -> FxHashSet<usize> {
    let mut cells = FxHashSet::default();
    let Some(root) = state.global_env.get("__button_states") else {
        return cells;
    };
    cells.insert(root.raw());
    let mut queue = vec![*root];
    while let Some(id) = queue.pop() {
        match state.heap.get_opt(id) {
            Some(HeapValue::ArrayTable(items)) => {
                for &child in items {
                    if cells.insert(child.raw()) {
                        queue.push(child);
                    }
                }
            }
            Some(HeapValue::ObjectTable(fields)) => {
                for (_, &child) in fields.iter() {
                    if cells.insert(child.raw()) {
                        queue.push(child);
                    }
                }
            }
            _ => {}
        }
    }
    cells
}

fn distinct_rows(columns: &[&Column], total_rows: usize) -> usize {
    if columns.is_empty() {
        return 1.min(total_rows);
    }
    let hashes = hash_rows(columns, total_rows);
    let mut seen: FxHashSet<u64> =
        FxHashSet::with_capacity_and_hasher(total_rows / 2, Default::default());
    for h in hashes {
        seen.insert(h);
    }
    seen.len()
}

/// Count what a merge at this site would remove, without changing anything.
pub fn record(function: &str, block: &str, states: &[State]) {
    if !enabled() || states.is_empty() {
        return;
    }
    let mut groups: FxHashMap<u64, Vec<&State>> = FxHashMap::default();
    for state in states {
        groups
            .entry(shape_of_state(state).cached_hash())
            .or_default()
            .push(state);
    }
    let mut lanes = 0u64;
    let mut dupes = 0u64;
    let mut dupes_widened = 0u64;
    for group in groups.values() {
        let owned: Vec<State> = group.iter().map(|s| (*s).clone()).collect();
        let total_rows: usize = owned.iter().map(|s| s.vector_size).sum();
        lanes += total_rows as u64;
        let Some((columns, origins)) = collect_columns_labeled(&owned) else {
            continue;
        };
        let key: Vec<&Column> = columns.iter().filter(|c| !c.is_uniform()).collect();
        dupes += (total_rows - distinct_rows(&key, total_rows)) as u64;

        let buttons = button_cells(&owned[0]);
        let widened_key: Vec<&Column> = columns
            .iter()
            .zip(&origins)
            .filter(|(c, origin)| {
                !c.is_uniform()
                    && !matches!(origin, Origin::Heap(cell) if buttons.contains(cell))
            })
            .map(|(c, _)| c)
            .collect();
        dupes_widened += (total_rows - distinct_rows(&widened_key, total_rows)) as u64;
    }
    TOTAL_LANES.fetch_add(lanes, Ordering::Relaxed);
    let mut sites = SITES.lock().unwrap();
    let entry = sites
        .entry((function.to_string(), block.to_string()))
        .or_default();
    entry.execs += 1;
    entry.lanes += lanes;
    entry.dupes += dupes;
    entry.dupes_widened += dupes_widened;
}

/// Print the convergence profile: sites where merging early would remove
/// the most lanes, real and widened.
pub fn report() {
    if !enabled() {
        return;
    }
    let sites = SITES.lock().unwrap();
    if sites.is_empty() {
        return;
    }
    let mut rows: Vec<(&(String, String), &SiteStats)> = sites.iter().collect();
    rows.sort_by_key(|(_, s)| std::cmp::Reverse(s.dupes_widened));
    eprintln!(
        "would-dedup census (lanes that a merge at each site would remove):"
    );
    eprintln!(
        "{:<52} {:>6} {:>10} {:>9} {:>9} {:>7}",
        "function::block", "execs", "lanes", "dupes", "+widened", "wid%"
    );
    for ((function, block), s) in rows.iter().take(20) {
        eprintln!(
            "{:<52} {:>6} {:>10} {:>9} {:>9} {:>6.1}%",
            format!("{}::{}", function, block),
            s.execs,
            s.lanes,
            s.dupes,
            s.dupes_widened,
            100.0 * s.dupes_widened as f64 / s.lanes.max(1) as f64,
        );
    }
}
