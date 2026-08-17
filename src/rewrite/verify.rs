//! Differential verification: does the rewritten program still behave
//! identically to the original?
//!
//! This is the real safety net. Per-rule verifiers check that each individual
//! transformation was legitimate; this checks the whole thing end to end, and
//! it is strong precisely because the abstract interpreter summarises
//! enormously many concrete runs at once. Thirty frames of the abstract search
//! costs well under a second and covers 15,250 distinct input sequences.
//!
//! The invariant that makes it work: **a rewrite may change anything inside a
//! frame, but must not change the cross-frame heap representation.** The frame
//! function loads the game state from the heap at entry and stores it back at
//! exit with the same shape the original program had. So the observation is
//! simply the canonical form of the heap at each frame boundary.
//!
//! Canonicalisation has to be done carefully. `gc()` renumbers heap ids
//! deterministically, which handles allocation-order differences. But lanes are
//! only meaningful as a set, and two runs may produce them in a different
//! order - so lane rows are sorted. Crucially the rows are sorted *as whole
//! tuples across all slots*, not per slot: sorting each slot independently
//! would lose the correlation between fields and would call two genuinely
//! different state sets equal.
//!
//! One exception to "the representation must not change", and it is worth being
//! explicit about because more will follow: **a closure capture is observed by
//! the value it denotes, not by the identity of the box holding it.** See
//! `unbox_closure_captures`. `promote_capture` removes those boxes on purpose,
//! and stage C will remove more heap cells still, so the premise that the
//! cross-frame heap graph is literally invariant does not survive the rewrite
//! sequence. Each place it is relaxed has to be narrow, stated, and applied to
//! both sides.

use anyhow::{Context, Result};
use std::collections::BTreeSet;
use std::hash::{Hash, Hasher};

use crate::game_runner::{create_initial_state_with_builtins, inject_tile_flag_at_builtin};
use crate::interpreter::glue::{interpret_cfg, interpret_prepared_cfg};
use crate::interpreter::abstraction::make_state_abstract;
use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};
use crate::interpreter::vectorize::vectorize_states;
use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::program::Program;

/// One value in one lane, in a form that can be ordered and hashed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
enum Cell {
    Num(i32),
    Interval(i32, i32),
    Bool(bool),
    UnknownBool,
    Str(String),
    Nil(Option<String>),
    Pointer(usize),
    NilPointer(String),
    Unset,
    Table(Vec<String>),
    Array(Vec<usize>),
    UnknownTable,
    Closure(String),
    Builtin(String),
}

fn value_cell(value: &Value, lane: usize) -> Cell {
    fn pick<T: Copy>(v: &MaybeVector<T>, lane: usize) -> T
    where
        T: std::fmt::Debug + Clone + PartialEq + Eq,
    {
        match v {
            MaybeVector::Scalar(s) => *s,
            MaybeVector::Vector(items) => items[lane.min(items.len().saturating_sub(1))],
        }
    }
    match value {
        Value::Number(n) => Cell::Num(raw_num(pick(n, lane))),
        Value::NumberInterval(i) => {
            let iv: Pico8NumInterval = pick(i, lane);
            Cell::Interval(raw_num(iv.low), raw_num(iv.high))
        }
        Value::Bool(b) => Cell::Bool(pick(b, lane)),
        Value::UnknownBool => Cell::UnknownBool,
        // Transient (value.rs): never present in an observed state.
        Value::MaybeBool(_) => panic!("{}", crate::interpreter::value::MAYBE_BOOL_ESCAPED),
        Value::String(s) => Cell::Str(s.clone()),
        Value::Nil(hint) => Cell::Nil(hint.clone()),
        Value::Pointer(id) => Cell::Pointer(id.raw()),
        Value::NilPointer(s) => Cell::NilPointer(s.clone()),
    }
}

fn raw_num(n: Pico8Num) -> i32 {
    n.as_raw_u32() as i32
}

/// The canonical form of one state: its structure, plus the sorted set of lane
/// rows.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct StateObservation {
    /// Per-heap-slot structure, independent of lane.
    structure: Vec<Cell>,
    /// Global name -> heap id.
    globals: Vec<(String, usize)>,
    /// One row per lane, sorted. Each row has one entry per per-lane slot, in
    /// heap order.
    rows: BTreeSet<Vec<Cell>>,
    prints: Vec<String>,
}

impl StateObservation {
    pub fn digest(&self) -> u64 {
        let mut hasher = rustc_hash::FxHasher::default();
        self.hash_into(&mut hasher);
        hasher.finish()
    }

    fn hash_into<H: Hasher>(&self, hasher: &mut H) {
        self.structure.hash(hasher);
        self.globals.hash(hasher);
        for row in &self.rows {
            row.hash(hasher);
        }
        self.prints.hash(hasher);
    }
}

/// Observe a closure's capture by the value it *denotes*, not by the identity
/// of the box holding it.
///
/// A Lua local that a closure captures is mutable, so the frontend gives it a
/// heap cell and the closure captures a pointer to that cell. `promote_capture`
/// removes the box where it is written once, which is a deliberate change to the
/// cross-frame heap graph - the box was reachable only from the closure, so
/// afterwards `gc` drops it and the heap is one cell smaller. Compared naively,
/// every such rewrite reads as a divergence.
///
/// So both sides are normalised first: a capture that points at a cell holding a
/// value is replaced by that value, and the now-unreferenced box disappears in
/// the `gc` that follows. This is the one place the observation is deliberately
/// blind to a representation difference, and it is exactly the difference the
/// rule claims to make. It stays sensitive to *what* is captured - a capture
/// pointed at the wrong thing still shows up, because the denoted values differ.
///
/// Deliberately single-level: a box holding a pointer to another box is not
/// unwrapped. That direction is safe - failing to normalise can only produce a
/// spurious divergence, never a spurious pass.
fn unbox_closure_captures(state: &mut State) {
    let mut updates: Vec<(crate::interpreter::heap::HeapId, HeapValue)> = Vec::new();
    for i in 0..state.heap.len() {
        let id = crate::interpreter::heap::HeapId::from_raw(i);
        let Some(HeapValue::Closure(name, captures)) = state.heap.get_opt(id) else {
            continue;
        };
        let mut changed = false;
        let unboxed: Vec<Value> = captures
            .iter()
            .map(|capture| {
                let Value::Pointer(target) = capture else { return capture.clone() };
                match state.heap.get_opt(*target) {
                    Some(HeapValue::Value(value)) => {
                        changed = true;
                        value.clone()
                    }
                    _ => capture.clone(),
                }
            })
            .collect();
        if changed {
            updates.push((id, HeapValue::Closure(name.clone(), unboxed)));
        }
    }
    for (id, value) in updates {
        state.heap.set(id, value);
    }
}

/// Canonical observation of one state. Normalises closure captures and GCs a
/// copy first, so heap ids are deterministic.
pub fn observe_state(state: &State) -> StateObservation {
    let mut state = state.clone();
    unbox_closure_captures(&mut state);
    state.gc();

    let mut structure = Vec::with_capacity(state.heap.len());
    let mut lane_slots: Vec<usize> = Vec::new();

    for i in 0..state.heap.len() {
        let id = crate::interpreter::heap::HeapId::from_raw(i);
        let cell = match state.heap.get_opt(id) {
            None => Cell::Unset,
            Some(HeapValue::Value(v)) => {
                if is_per_lane(v) {
                    lane_slots.push(i);
                    // Structure records only that this slot is per-lane; the
                    // values go in the rows.
                    Cell::Num(i32::MIN)
                } else {
                    value_cell(v, 0)
                }
            }
            Some(HeapValue::ObjectTable(table)) => {
                let mut keys: Vec<String> = table.keys().cloned().collect();
                keys.sort();
                Cell::Table(
                    keys.into_iter()
                        .map(|k| format!("{}={}", k, table[&k].raw()))
                        .collect(),
                )
            }
            Some(HeapValue::ArrayTable(items)) => {
                Cell::Array(items.iter().map(|i| i.raw()).collect())
            }
            Some(HeapValue::UnknownTable) => Cell::UnknownTable,
            Some(HeapValue::Closure(name, captures)) => Cell::Closure(format!(
                "{}[{}]",
                name.as_str(),
                captures
                    .iter()
                    .map(|c| format!("{:?}", value_cell(c, 0)))
                    .collect::<Vec<_>>()
                    .join(",")
            )),
            Some(HeapValue::BuiltinFun(name)) => Cell::Builtin(name.clone()),
        };
        structure.push(cell);
    }

    let mut rows = BTreeSet::new();
    for lane in 0..state.vector_size.max(1) {
        let mut row = Vec::with_capacity(lane_slots.len());
        for &slot in &lane_slots {
            let id = crate::interpreter::heap::HeapId::from_raw(slot);
            if let Some(HeapValue::Value(v)) = state.heap.get_opt(id) {
                row.push(value_cell(v, lane));
            }
        }
        rows.insert(row);
    }

    let globals: Vec<(String, usize)> = state
        .global_env
        .iter()
        .map(|(k, v)| (k.clone(), v.raw()))
        .collect();

    StateObservation { structure, globals, rows, prints: state.prints.clone() }
}

/// Every vectorizable value goes to the rows - INCLUDING uniform scalars.
/// A uniform value is just a per-lane vector whose lanes agree; recording
/// uniformity in `structure` would make the observation depend on how lanes
/// are grouped into states (a partition class turns lane-varying cells
/// uniform), which is exactly the implementation detail the observation
/// must be blind to (partition-agnostic verification, 2026-08-17).
fn is_per_lane(value: &Value) -> bool {
    matches!(
        value,
        Value::Number(_) | Value::NumberInterval(_) | Value::Bool(_)
    )
}

/// Canonical observation of a whole frame: the multiset of state observations.
/// Grouping-canonical: states that differ only in how lanes are packaged
/// into vectorized states (partitioned merges, chunk boundaries) observe
/// identically - same-(structure, globals, prints) observations merge their
/// row sets. Soundness anchor: batching invariance (#97 simdcheck) - no
/// per-lane result ever depends on which lanes share a state. A lost or
/// corrupted lane still fails (the row multiset differs).
pub fn observe_frame(states: &[State]) -> BTreeSet<StateObservation> {
    let mut merged: std::collections::BTreeMap<
        (Vec<Cell>, Vec<(String, usize)>, Vec<String>),
        BTreeSet<Vec<Cell>>,
    > = std::collections::BTreeMap::new();
    for state in states {
        let o = observe_state(state);
        merged
            .entry((o.structure, o.globals, o.prints))
            .or_default()
            .extend(o.rows);
    }
    merged
        .into_iter()
        .map(|((structure, globals, prints), rows)| StateObservation {
            structure,
            globals,
            rows,
            prints,
        })
        .collect()
}

/// Runs the abstract search and yields the observation after each frame.
pub struct AbstractRun {
    states: Vec<State>,
    fixed_env: crate::interpreter::fixed_env::FixedEnv,
    /// Prepared once, not per frame. `interpret_cfg` clones the CFG and
    /// recomputes its label set, which is free at 100 blocks and is not at the
    /// 1545 a fully inlined program has.
    frame_cfg: crate::interpreter::fixed_env::PreparedCfg,
    /// How many separate states each frame produced, before they were merged
    /// back into one.
    ///
    /// This is the number that decides how expensive a frame is. Re-merging is
    /// 60% of runtime and its cost is per state, so a rewrite that removes
    /// branches should be judged by this and not by its `filter_branch` share.
    /// See BENCHMARK_DATA.md.
    states_before_merge: Vec<usize>,
    /// Frontier-only search (CELESTE_FRONTIER_ONLY=1): the persistent
    /// cross-frame row table (dense ids + per-frame watermarks; 128-bit
    /// keys - see `vectorize::subtract_visited` for the collision note).
    visited_rows: Option<crate::interpreter::visited::Visited>,
    /// Use only the historic rem widening at boundaries (for the widen-check,
    /// which applies the conservative widenings post hoc instead).
    rem_only_abstraction: bool,
    /// Deopt-to-plain support; `None` means a failing frame is a hard error.
    deopt: Option<DeoptTarget>,
    /// Shape-dispatched variants; `None` means every state runs the base
    /// program (see `Variant`).
    variants: Option<VariantDispatch>,
    /// Precision-refinement band restriction (plans/refinement-plan.md):
    /// lanes whose coarsened row is outside the previous level's band are
    /// dropped at each boundary.
    band: Option<BandFilter>,
    /// Position-transition recording (`rewrite::pos_graph`). `Some` only
    /// when a caller asked for it; the probe reads the frame body's input
    /// and output positions and injects nothing, so a recorded run takes
    /// exactly the same path as an unrecorded one.
    pos_obs: Option<super::pos_graph::PosObserver>,
    /// Suppress the per-frame deopt/variant reporting. For the screening
    /// runs, whose stdout is a JSONL stream of accepted entries and has to
    /// stay pipeable; a hundred trials' telemetry in the middle of it is
    /// not telemetry, it is corruption. The counters are still tallied.
    quiet: bool,
}

/// The previous precision level's result, used to confine this level's
/// forward pass to states that can still be on a winning path.
pub struct BandFilter {
    pub prev_table: crate::interpreter::row_table::RowTable,
    /// Min frames to the exit per previous-level row id (`sweep::save_g`).
    pub g_prev: Vec<u16>,
    pub horizon: u32,
    /// The previous level's rem precision (what to coarsen to).
    pub prev_precision: crate::interpreter::abstraction::LadderPrecision,
}

/// Everything needed to re-run a frame under the plain program when the
/// specialized one hits a premise it baked in (see `state_mapping`).
///
/// The plain program has its own `FixedEnv` - its function definitions differ
/// from the specialized ones - and the mapping translates the frame-input
/// state to canonical before the re-run and the outputs back after it.
struct DeoptTarget {
    plain_cfg: crate::interpreter::fixed_env::PreparedCfg,
    plain_env: crate::interpreter::fixed_env::FixedEnv,
    mapping: super::state_mapping::StateMapping,
    /// Deopt *every* state instead of only failing ones. This is the
    /// certification mode of `rewrite deoptcheck`: it pushes every frame of
    /// every state through to_canonical -> plain -> from_canonical, so a
    /// mapping bug shows up as an observation divergence rather than waiting
    /// for the first real deopt at frame 59.
    force: bool,
    /// Run every frame in collect mode (origin column injected up front)
    /// instead of attempt-then-retry. Env-gated: CELESTE_DEOPT_COLLECT_FIRST.
    collect_first: bool,
    /// (states, lanes) deopted over the whole run.
    total_events: (usize, usize),
}

/// A per-shape specialized program (plans/room00-plan.md, "shape-dispatched
/// variants"): a state whose object-array shape (`abstraction::object_shape`)
/// matches one of `shapes` runs its frames under this program instead of
/// the base one.
///
/// Boundary states always stay in the BASE program's layout: a variant
/// frame round-trips base -> canonical -> variant on the way in and
/// variant -> canonical -> base on the way out. Dispatch is therefore
/// invisible to checkpoints, row hashing, band coarsening and the sweep -
/// a run with variants must produce exactly the same boundary states as a
/// run without, and that equivalence (lane counts per frame, observations)
/// is the gate for registering one.
pub struct Variant {
    pub label: String,
    /// The object-array shapes (type-name sequences) this program is
    /// specialized for, e.g. [["player"], ["player_spawn"]].
    pub shapes: Vec<Vec<String>>,
    pub frame_cfg: crate::interpreter::fixed_env::PreparedCfg,
    pub fixed_env: crate::interpreter::fixed_env::FixedEnv,
    /// This variant's layout <-> canonical.
    pub mapping: super::state_mapping::StateMapping,
}

struct VariantDispatch {
    /// The base program's layout <-> canonical.
    base_mapping: super::state_mapping::StateMapping,
    variants: Vec<Variant>,
    /// (states, lanes) that ran under some variant, whole run.
    total_events: (usize, usize),
    /// States that matched a shape but whose variant frame failed and fell
    /// back to the base path. A registered variant's premises must hold for
    /// every state of its shape, so anything nonzero is a registry bug -
    /// loud per event, and reported at the end of the run.
    total_fallbacks: usize,
}

/// Records a coarse phase duration on drop (see `crate::metrics`).
struct ScopedPhase(&'static str, std::time::Instant);
impl ScopedPhase {
    fn new(name: &'static str) -> Self {
        Self(name, std::time::Instant::now())
    }
}
impl Drop for ScopedPhase {
    fn drop(&mut self) {
        crate::metrics::record(self.0, self.1.elapsed());
    }
}

/// Aggregated counters for the streaming boundary pipeline.
#[derive(Default)]
struct StreamCounters {
    band_before: usize,
    band_after: usize,
    band_missing: usize,
    sub_before: usize,
    sub_after: usize,
}

impl StreamCounters {
    /// Fold a worker's per-chunk counters in. Called in input order so the
    /// totals do not depend on which thread finished first.
    fn absorb(&mut self, other: &StreamCounters) {
        self.band_before += other.band_before;
        self.band_after += other.band_after;
        self.band_missing += other.band_missing;
        self.sub_before += other.sub_before;
        self.sub_after += other.sub_after;
    }
}

/// Streaming boundary pipeline (the standard path for frontier-only runs):
/// abstract, band-filter and visited-subtract one frame-output state
/// as soon as it is produced, so only SURVIVING lanes are ever held for the
/// end-of-frame merge. Without this, every chunk's raw outputs accumulate
/// until the frame ends - ~30M pre-dedup lanes at room-(0,0) f89, which is
/// what kept OOMing the level-0 extends no matter how inputs were chunked.
/// Semantics are unchanged: the per-state pipeline is exactly the phased
/// one (abstraction and band tests are per-lane; the visited set is "rows
/// ever seen", so subtracting incrementally as it grows kills cross-chunk
/// duplicates the same way the final dedup did).
fn stream_boundary_one(
    state: State,
    band: Option<&BandFilter>,
    frame: u32,
    visited: &mut crate::interpreter::visited::Visited,
    counters: &mut StreamCounters,
) -> Result<Vec<State>> {
    let prepared = stream_boundary_prepare(state, band, frame, counters, visited)?;
    Ok(stream_boundary_subtract(prepared, visited, counters))
}

/// The THREAD-SAFE prefix of `stream_boundary_one`: straddle-split,
/// abstract, band-filter and gc. Every step here is a pure function of its
/// input state, so a worker thread can run it for its own chunk (see
/// `step_parallel`); only the visited-set subtraction that follows touches
/// shared mutable state.
///
/// The counters it fills are per-call and merged by the caller in input
/// order, so the aggregate is identical whichever thread did the work.
fn stream_boundary_prepare(
    state: State,
    band: Option<&BandFilter>,
    frame: u32,
    counters: &mut StreamCounters,
    visited: &crate::interpreter::visited::Visited,
) -> Result<Vec<PreparedRows>> {
    let mut kept_out = Vec::new();
    for state in crate::interpreter::abstraction::split_precision_straddles(state) {
        let state = make_state_abstract(state);
        let state = if let Some(band) = band {
            counters.band_before += state.vector_size;
            let budget = band.horizon.saturating_sub(frame);
            let mut coarse = crate::interpreter::abstraction::coarsen_to(
                state.clone(),
                band.prev_precision,
            );
            coarse.gc();
            let keys = super::sweep::row_keys(&coarse)?;
            let mask: Vec<bool> = keys
                .iter()
                .map(|k| match band.prev_table.id_of(*k) {
                    Some(id) => {
                        let e = band.prev_table.earliest_frame(id).unwrap_or(u32::MAX);
                        let g = band.g_prev[id as usize];
                        e <= frame
                            && g != super::sweep::G_UNREACHABLE
                            && (g as u32) <= budget
                    }
                    // TODO(soundness, band): A MISS AGAINST AN UNBANDED
                    // PREVIOUS LEVEL (i.e. k = 1, whose previous level is 0)
                    // MUST BE ZERO, AND SHOULD BE FATAL. Today every miss is
                    // just a counter and a log line and the lane is silently
                    // dropped - the quiet direction, because a lost reachable
                    // state means we can report an optimum that is too slow,
                    // or refute a horizon that is actually achievable.
                    //
                    // The distinction matters and is easy to get backwards.
                    // Against an UNBANDED table a miss is impossible under a
                    // correct abstraction: level k's row coarsens to a level
                    // k-1 row that k-1 itself reached (simulation), so "not
                    // in the table at all" means the coarsening is wrong.
                    // Against a BANDED table - every k >= 2 - a miss is
                    // NORMAL: k-1 dropped that row as out-of-band, so it
                    // never entered k-1's visited set, and refusing its
                    // refinements is exactly how pruning propagates. Room
                    // (1,0)'s certified campaign has thousands of misses at
                    // k2/k3/k6 and ZERO at k1, across every horizon, which is
                    // precisely this pattern.
                    //
                    // This has already fired once for real. `make_state_abstract_rem`
                    // widened a fruit's bob counter `off` but left its `y`
                    // concrete, while the coarse level derives `y` FROM the
                    // widened `off` by interval arithmetic. The coarsened row
                    // was a soundly over-approximating state that k-1 never
                    // produces - it lands BETWEEN rungs - so every fruit-alive
                    // lane of the exact level was dropped. Room (0,0) hid it
                    // (its fruit exists only after the wall break, which the
                    // optimal path never does); room (2,0) has a fruit alive
                    // from frame 1 and k16 came out empty at frame 2.
                    //
                    // The property being violated is NOT soundness of the
                    // widening (the point y really was inside the band). It is
                    // that widen must be CANONICAL: `widen(s)` has to be
                    // exactly the representation the coarse level would itself
                    // have produced, because the lookup that follows is exact
                    // equality, not intersection. Idempotence does not imply
                    // it - the buggy widen was idempotent.
                    //
                    // The k=1 gate would have caught the fruit bug the
                    // first time a room with a LIVE fruit ran, because level 0
                    // is unbanded. It did not fire earlier only because
                    // room (1,0) has no objects at all and room (0,0)'s fruit
                    // exists only after a wall break the optimal path never
                    // performs.
                    //
                    // Two fixes, neither done:
                    //   1. assert band_missing == 0 whenever the previous
                    //      level is unbanded. Exact, free (the lookup already
                    //      happens), and it turns this whole class from a
                    //      wrong answer into a stopped campaign. For k >= 2 it
                    //      cannot be used as-is; distinguishing a legitimate
                    //      out-of-band miss from a bad coarsening there needs
                    //      k-1's PRE-band table, which is not stored.
                    //   2. replace the exact lookup with the INTERSECTION test
                    //      it is an optimization of - "does this level-k state
                    //      intersect any level-(k-1) state?" - which is what we
                    //      actually mean and does not require widen to be
                    //      canonical at all. More expensive, but only on the
                    //      band path, and it removes the failure class at
                    //      every k rather than just k=1.
                    None => {
                        counters.band_missing += 1;
                        false
                    }
                })
                .collect();
            let kept = mask.iter().filter(|b| **b).count();
            counters.band_after += kept;
            if kept == state.vector_size {
                state
            } else if kept > 0 {
                state.filter_by_mask_clone(&mask, crate::interpreter::state::FILTER_BAND)
            } else {
                continue;
            }
        } else {
            state
        };
        // Row hashing is heap-layout-sensitive on raw interpreter fragments;
        // the phased path subtracts vectorize-canonicalized states, and the
        // witness/extract probes gc before hashing and match those tables -
        // gc IS the canonicalizer. Without this, the same logical row
        // arrives under different hashes on different paths and the visited
        // set double-counts (measured: 2x visited, +10% spurious frontier).
        let mut state = state;
        state.gc();
        // Phase 1 of the frontier subtract - hashing every lane into its
        // 128-bit row key and looking it up read-only. This is where the
        // per-lane cache miss lives, and it needs only `&RowTable`, so it
        // belongs on this side of the parallel/serial line.
        let keys = crate::interpreter::vectorize::visited_row_keys(&state, visited);
        kept_out.push(PreparedRows { state, keys });
    }
    Ok(kept_out)
}

/// A boundary state with its visited-set keys already computed and probed
/// (`visited_row_keys`). Only the id-assigning insert is left, and that has
/// to happen in input order.
struct PreparedRows {
    state: State,
    keys: Option<crate::interpreter::vectorize::VisitedKeys>,
}

/// The SERIAL suffix: subtract the rows this run has already reached.
///
/// Kept out of `stream_boundary_prepare` because the row table is the one
/// piece of shared mutable state in the frame pipeline, and because row ids
/// are assigned in insertion order - running this in input order is what
/// makes a parallel frame produce byte-identical checkpoints to a serial
/// one.
fn stream_boundary_subtract(
    prepared: Vec<PreparedRows>,
    visited: &mut crate::interpreter::visited::Visited,
    counters: &mut StreamCounters,
) -> Vec<State> {
    decided_survivors(prepared, visited, counters)
        .into_iter()
        .filter_map(|(state, survivors)| {
            crate::interpreter::vectorize::subtract_apply(state, survivors)
        })
        .collect()
}

/// The serial half of the subtract: assign ids to the new rows and say
/// which lanes survive, WITHOUT gathering them.
///
/// The gather is `subtract_apply`, which is pure and goes back on a worker
/// thread (`step_parallel`). It touches every column of the state, so
/// leaving it in the serial phase put ~2% of a frame's lanes x ~58 columns
/// of copying on the one thread that cannot be parallelised.
fn decided_survivors(
    prepared: Vec<PreparedRows>,
    visited: &mut crate::interpreter::visited::Visited,
    counters: &mut StreamCounters,
) -> Vec<(State, crate::interpreter::vectorize::Survivors)> {
    use crate::interpreter::vectorize::Survivors;
    let mut out = Vec::with_capacity(prepared.len());
    for PreparedRows { state, keys } in prepared {
        let (survivors, before) =
            crate::interpreter::vectorize::subtract_decide(&state, keys, visited);
        counters.sub_before += before;
        counters.sub_after += match &survivors {
            Survivors::All => state.vector_size,
            Survivors::Some(kept) => kept.len(),
        };
        out.push((state, survivors));
    }
    out
}

/// Outcome of offering a state to the variant registry for one frame.
enum VariantOutcome {
    /// The variant ran the frame; outputs are back in base layout.
    Ran(Vec<State>),
    /// No variant matched (or the variant failed - already logged): the
    /// caller runs the state under the base path.
    Base(State),
}

/// Run one state's frame under `variant`, through the canonical form both
/// ways. Any error or panic falls back to the base path with a loud print;
/// the snapshot clone is what makes that fallback possible.
///
/// Takes the registry by shared reference and tallies into the caller's
/// per-call counters, so a worker thread can run it. Dispatch is a pure
/// function of the state (its object-array shape picks the program), which
/// is why nothing here has to be sequenced; registering a variant used to
/// disable the chunk-parallel path outright, and that cost 3.6x on room
/// (0,0) - far more than any variant could win back.
fn dispatch_variant_frame(
    vd: &VariantDispatch,
    state: State,
    counters: &mut FrameEventCounters,
) -> VariantOutcome {
    let shape = match crate::interpreter::abstraction::object_shape(&state) {
        Ok(shape) => shape,
        Err(err) => {
            // A state whose shape cannot be read is not dispatchable; the
            // base program is the sound answer. Loud anyway - this means
            // the heap looks structurally unlike the game.
            println!("  variant: shape probe failed ({:#}); base path", err);
            return VariantOutcome::Base(state);
        }
    };
    let Some(idx) = vd
        .variants
        .iter()
        .position(|v| v.shapes.iter().any(|s| *s == shape))
    else {
        return VariantOutcome::Base(state);
    };
    let snapshot = state.clone();
    let lanes = state.vector_size;
    let variant = &vd.variants[idx];
    let attempt = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        run_variant_frame(&vd.base_mapping, variant, state)
    }));
    match attempt {
        Ok(Ok(outputs)) => {
            counters.variant.0 += 1;
            counters.variant.1 += lanes;
            VariantOutcome::Ran(outputs)
        }
        Ok(Err(err)) => {
            let one_line = format!("{:#}", err).replace('\n', " | ");
            println!(
                "  variant {}: frame FAILED ({}); falling back to the base program - \
                 a registered variant's premises must hold for its shape, fix the registry",
                vd.variants[idx].label,
                one_line.chars().take(600).collect::<String>()
            );
            counters.variant_fallbacks += 1;
            VariantOutcome::Base(snapshot)
        }
        Err(panic) => {
            println!(
                "  variant {}: frame PANICKED ({}); falling back to the base program",
                vd.variants[idx].label,
                panic_text(&panic).chars().take(200).collect::<String>()
            );
            counters.variant_fallbacks += 1;
            VariantOutcome::Base(snapshot)
        }
    }
}

fn run_variant_frame(
    base_mapping: &super::state_mapping::StateMapping,
    variant: &Variant,
    mut state: State,
) -> Result<Vec<State>> {
    base_mapping
        .to_canonical(&mut state)
        .context("variant: mapping base layout to canonical")?;
    variant
        .mapping
        .from_canonical(&mut state)
        .context("variant: mapping canonical to the variant layout")?;
    let result = interpret_prepared_cfg(&variant.frame_cfg, state, &variant.fixed_env)?;
    let mut out = Vec::with_capacity(result.len());
    for (mut s, _) in result {
        variant
            .mapping
            .to_canonical(&mut s)
            .context("variant: mapping a frame output back to canonical")?;
        base_mapping
            .from_canonical(&mut s)
            .context("variant: mapping a frame output back to base layout")?;
        out.push(s);
    }
    Ok(out)
}

/// Per-frame deopt and variant event tallies, reported by
/// `report_frame_events`.
#[derive(Default)]
struct FrameEventCounters {
    /// (states, lanes) that re-ran under the plain program.
    deopt: (usize, usize),
    /// (states, lanes) that ran under shape variants.
    variant: (usize, usize),
    /// States whose variant frame failed and took the base path instead.
    /// See `VariantDispatch::total_fallbacks`: anything nonzero is a bug.
    variant_fallbacks: usize,
}

impl FrameEventCounters {
    fn absorb(&mut self, other: &FrameEventCounters) {
        self.deopt.0 += other.deopt.0;
        self.deopt.1 += other.deopt.1;
        self.variant.0 += other.variant.0;
        self.variant.1 += other.variant.1;
        self.variant_fallbacks += other.variant_fallbacks;
    }
}

/// Worker threads for chunk-parallel frame execution
/// (`CELESTE_FRAME_THREADS`; 1 = off, the default).
///
/// This is the lane axis, not the state axis: `chunk_states` cuts a wide
/// boundary state into independent lane chunks and each worker runs one
/// chunk through the whole frame body. Parallelising across the states
/// *inside* a flow step was measured twice and rejected (see
/// BENCHMARK_DATA.md) - after a boundary merge a frame is a handful of very
/// wide states, so there is nothing to spread there. Chunking creates the
/// work items that parallelism then consumes, which is why the two only
/// pay off together.
fn frame_threads() -> usize {
    static N: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *N.get_or_init(|| {
        std::env::var("CELESTE_FRAME_THREADS")
            .ok()
            .and_then(|v| v.parse::<usize>().ok())
            .unwrap_or_else(crate::interpreter::virtual_merge::worker_threads)
            .max(1)
    })
}

/// Lane-chunking: split states above N lanes into <=N-lane chunks before
/// the frame. Lanes are independent, so running chunks separately and
/// re-merging at the boundary is semantics-preserving; what it changes is
/// the PEAK - the mid-frame transient (fragments plus the heap's
/// append-only storage) is proportional to the widest state in flight,
/// and at room-(0,0) scale a single 12.5M-lane frame peaked at 98 GB.
/// The cost is re-running per-chunk the work that is uniform across
/// lanes.
///
/// The cap defaults to 1M lanes (the room-(0,0) campaign value; it never
/// fires on runs below that scale). CELESTE_MAX_STATE_LANES overrides;
/// 0 disables chunking entirely.
/// The lane cap `chunk_states` will actually use, as a value.
///
/// Exposed because it belongs in the campaign fingerprint and the
/// fingerprint must hash what the run WILL DO, not which environment
/// variables happen to be set. Note the default is a function of the
/// thread count, so a run that sets neither variable still has a definite
/// cap - and two runs at different thread counts have DIFFERENT caps
/// without either one naming a chunk setting.
pub fn effective_chunk_cap() -> usize {
    const SERIAL_CAP: usize = 1_000_000;
    const PARALLEL_CAP: usize = 8_000;
    let default_cap = if frame_threads() > 1 { PARALLEL_CAP } else { SERIAL_CAP };
    std::env::var("CELESTE_MAX_STATE_LANES")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(default_cap)
}

/// The fruit-shape lane cap `chunk_states` will actually use, as a value.
/// Clamped by the general cap exactly as the chunking does.
pub fn effective_fruit_chunk_cap() -> usize {
    let cap = effective_chunk_cap();
    std::env::var("CELESTE_FRUIT_CHUNK_LANES")
        .ok()
        .and_then(|v| v.parse::<usize>().ok())
        .unwrap_or(8_000)
        .max(1)
        .min(cap.max(1))
}

fn chunk_states(states: Vec<State>) -> Vec<State> {
    // The cap and the thread count are ONE setting, not two. Measured on
    // room (1,0), 60 frames (2026-08-08):
    //
    //   1 thread,  no chunking        89.2 s   4.31 GB
    //   1 thread,  cap 8k             ~110 s   (the tiling tax: ~23%, from
    //                                          re-running per chunk the work
    //                                          that is uniform across lanes)
    //   16 threads, no chunking       57.0 s  11.83 GB  (1.6x, and 2.7x the
    //                                          memory - 16 chunks' raw
    //                                          outputs in flight is exactly
    //                                          what streaming existed to
    //                                          avoid)
    //   16 threads, cap 8k            21.9 s   2.55 GB  (4.1x AND less
    //                                          memory than serial)
    //
    // Each alone is a bad trade; together they are the whole win, because
    // the chunks are what give the threads work and the threads are what
    // pay for the chunking. So the parallel default cap is derived here
    // rather than left to the caller to remember.
    let cap = effective_chunk_cap();
    if cap == 0 {
        return states;
    }
    let mut out = Vec::with_capacity(states.len());
    for state in states {
        // Fruit-bearing states get a 10x tighter cap: their frames run
        // under the plain program (the recipe path hits
        // select-on-UnknownBool) where the widened fruit's UnknownBool
        // collide branches copy ALL lanes down both arms repeatedly - the
        // transient per input lane is an order of magnitude above a normal
        // state's (h89 OOMed on exactly this with the uniform cap).
        let cap = match crate::interpreter::abstraction::object_shape(&state) {
            Ok(shape) if shape.iter().any(|t| t == "fruit") => {
                // Fruit chunks are capped in ABSOLUTE lanes, not as a
                // fraction of the ordinary cap.
                //
                // It used to be `cap / 10`, calibrated when the ordinary
                // cap was 1,000,000 - so a fruit chunk was 100,000 lanes.
                // When the parallel default brought the base to 8,000 that
                // silently became 800, and since a fruit state runs the
                // PLAIN program (its widened collide check is UnknownBool,
                // which the specialized select cannot take), the result was
                // one plain-program invocation per 800 lanes: room (0,0)
                // f77 spent most of a 165-second frame in 1,355 separate
                // plain runs of 1.08M deopted lanes.
                //
                // 8,000 keeps the in-flight fruit transient at 16 threads x
                // 8,000 = 128k lanes, next to the 1 x 100,000 the serial
                // build carried, while cutting the invocation count 10x.
                // The sweep still wants far less (its origin column blocks
                // all dedup, so the UnknownBool doubling multiplies on the
                // full 64-input fan-out) and sets the env var.
                effective_fruit_chunk_cap()
            }
            _ => cap,
        };
        if state.vector_size <= cap {
            out.push(state);
            continue;
        }
        let n = state.vector_size;
        for start in (0..n).step_by(cap) {
            let end = (start + cap).min(n);
            // A chunk is one contiguous run, so hand the filter that run
            // directly. Materialising an n-length bool mask per chunk made
            // chunking quadratic in the state width - at depth a 5M-lane
            // state is 625 chunks, i.e. 3e9 mask writes for one state, all
            // of it on the serial path.
            out.push(state.filter_by_kept_clone(
                &crate::interpreter::value::KeptLanes::from_range(start, end),
                crate::interpreter::state::FILTER_CHUNK,
            ));
        }
    }
    out
}

impl AbstractRun {
    pub fn start(program: &Program) -> Result<Self> {
        crate::interpreter::vectorize::set_merge_partition_patterns(
            &program.merge_partition_cells,
        );
        let fixed_env = program.fixed_env();
        let initial = create_initial_state_with_builtins(&fixed_env);
        let init_states = interpret_cfg(program.init_cfg().clone(), initial, &fixed_env)
            .context("init failed")?;
        let mut states: Vec<State> = init_states.into_iter().map(|(s, _)| s).collect();
        for state in &mut states {
            inject_tile_flag_at_builtin(state);
        }
        let frame_cfg = crate::interpreter::fixed_env::PreparedCfg::new(
            program.frame_cfg().clone(),
        );
        let visited_rows = if std::env::var_os("CELESTE_FRONTIER_ONLY").is_some() {
            println!("frontier-only search ENABLED (128-bit hashed visited set)");
            Some(crate::interpreter::visited::Visited::in_memory())
        } else {
            None
        };
        Ok(Self {
            states,
            fixed_env,
            frame_cfg,
            states_before_merge: Vec::new(),
            visited_rows,
            rem_only_abstraction: false,
            deopt: None,
            variants: None,
            band: None,
            pos_obs: None,
            quiet: false,
        })
    }

    /// Widen only rem at boundaries; used by `rewrite widencheck`.
    pub fn start_rem_only(program: &Program) -> Result<Self> {
        let mut run = Self::start(program)?;
        run.rem_only_abstraction = true;
        Ok(run)
    }

    /// Like `start`, but a frame that fails under `program` deopts: the
    /// frame-input state is mapped to canonical, the frame re-runs under
    /// `plain`, and the outputs are mapped back. With `force`, *every* state
    /// takes that path (certification mode; see `DeoptTarget::force`).
    ///
    /// `mapping` must be the one derived from the recipe that produced
    /// `program`, and `plain` must be the unrewritten program.
    pub fn start_with_deopt(
        program: &Program,
        plain: &Program,
        mapping: super::state_mapping::StateMapping,
        force: bool,
    ) -> Result<Self> {
        let mut run = Self::start(program)?;
        run.deopt = Some(DeoptTarget {
            plain_cfg: crate::interpreter::fixed_env::PreparedCfg::new(
                plain.frame_cfg().clone(),
            ),
            plain_env: plain.fixed_env(),
            mapping,
            force,
            collect_first: std::env::var_os("CELESTE_DEOPT_COLLECT_FIRST").is_some(),
            total_events: (0, 0),
        });
        Ok(run)
    }

    /// Turn the frontier subtract off for this run regardless of the
    /// environment.
    ///
    /// For `simdcheck`'s probe runs. The subtract removes rows this run has
    /// already seen, and a batched probe seeds its history from the whole
    /// sampled group while each single-lane probe seeds from one lane - so
    /// with it on, the two differ for a reason that has nothing to do with
    /// lane independence. Turning it off HERE, rather than forbidding it
    /// for the whole command, lets the forward walk stay frontier-only and
    /// therefore actually reach the frames where straddling happens
    /// (without it, room (0,0) exhausts 100 GB before frame 70).
    pub fn without_frontier_subtract(mut self) -> Self {
        self.visited_rows = None;
        self
    }

    /// (states, lanes) that deopted to the plain program so far.
    pub fn deopt_events(&self) -> (usize, usize) {
        self.deopt.as_ref().map_or((0, 0), |d| d.total_events)
    }

    /// Register shape-dispatched variants. `base_mapping` is the mapping of
    /// the recipe that built THIS run's base program (needed to reach the
    /// canonical form from base-layout states).
    pub fn set_variants(
        &mut self,
        base_mapping: super::state_mapping::StateMapping,
        variants: Vec<Variant>,
    ) {
        self.variants = Some(VariantDispatch {
            base_mapping,
            variants,
            total_events: (0, 0),
            total_fallbacks: 0,
        });
    }

    /// (states, lanes, fallbacks) run under shape variants so far.
    /// Fallbacks should be zero; see `VariantDispatch::total_fallbacks`.
    pub fn variant_events(&self) -> (usize, usize, usize) {
        self.variants
            .as_ref()
            .map_or((0, 0, 0), |v| (v.total_events.0, v.total_events.1, v.total_fallbacks))
    }

    /// The frontier visited set, when frontier-only search is enabled.
    pub fn visited_table(&self) -> Option<&crate::interpreter::visited::Visited> {
        self.visited_rows.as_ref()
    }

    /// Wire the visited set to the checkpoint dir: `.rowkeys` are written
    /// there at every boundary, and CELESTE_VISITED_ENGINE=mmap swaps the
    /// in-RAM map for the fp-run + mmap engine (fresh runs only; resumes
    /// hand a ready engine to `restore`). Call before the first `step`.
    pub fn configure_visited_dir(&mut self, dir: &std::path::Path) {
        let Some(v) = self.visited_rows.as_mut() else { return };
        if crate::interpreter::visited::mmap_engine_selected() {
            assert!(v.is_empty(), "engine swap after frames were recorded");
            *v = crate::interpreter::visited::Visited::mmap_new(dir);
            println!("visited engine: mmap (fp-runs + rowkeys)");
        } else {
            v.set_dir(dir);
        }
    }

    /// Turn frontier subtraction off (regardless of the env flag). The
    /// backward sweep expands saved frontier batches one frame at a time and
    /// must see every successor lane, not just never-seen ones.
    pub fn disable_frontier(&mut self) {
        self.visited_rows = None;
    }

    /// Confine the forward pass to the previous level's band.
    pub fn set_band(&mut self, band: BandFilter) {
        self.band = Some(band);
    }

    /// Record the position-transition table while stepping
    /// (`rewrite::pos_graph`). Read-only: it does not change what any frame
    /// computes, only what is observed about it.
    pub fn record_pos_graph(&mut self) {
        self.pos_obs = Some(super::pos_graph::PosObserver::default());
    }

    /// Record while stepping, but keep everything an earlier run of the
    /// same search already recorded - the resumed case.
    pub fn record_pos_graph_from(&mut self, graph: super::pos_graph::PosGraph) {
        self.pos_obs = Some(super::pos_graph::PosObserver::seeded(graph));
    }

    /// Pairs recorded so far, for progress reporting.
    pub fn pos_graph_pairs(&self) -> Option<usize> {
        self.pos_obs.as_ref().map(|o| o.pairs())
    }

    /// The finished table; `None` when recording was never enabled.
    pub fn take_pos_graph(
        &mut self,
        frames: u32,
        fingerprint: &str,
    ) -> Option<super::pos_graph::PosGraph> {
        self.pos_obs.take().map(|o| o.build(frames, fingerprint))
    }

    /// Drop lanes that have exited the room (global room.x reached the
    /// configured win value) from the frontier: win states are absorbing
    /// for a room-scoped search.
    pub fn absorb_won_lanes(&mut self) {
        let mut kept = Vec::new();
        for state in std::mem::take(&mut self.states) {
            let mask: Vec<bool> = crate::interpreter::abstraction::win_lane_mask(&state)
                .into_iter()
                .map(|w| !w)
                .collect();
            let keep = mask.iter().filter(|b| **b).count();
            if keep == state.vector_size {
                kept.push(state);
            } else if keep > 0 {
                kept.push(state.filter_by_mask_clone(
                    &mask,
                    crate::interpreter::state::FILTER_BAND,
                ));
            }
        }
        self.states = kept;
    }

    /// Restore from a checkpoint: boundary states, row table and deopt
    /// counters as of some completed frame. The caller continues stepping
    /// from the following frame. Refuses to attach a row table when
    /// frontier-only search is off (the fingerprint should have caught the
    /// flag mismatch already; this is the belt to that suspender).
    pub fn restore(
        &mut self,
        states: Vec<State>,
        visited: Option<crate::interpreter::visited::Visited>,
        deopt_events: (usize, usize),
    ) -> Result<()> {
        if visited.is_some() != self.visited_rows.is_some() {
            return Err(anyhow::anyhow!(
                "checkpoint frontier state ({}) does not match this run ({})",
                if visited.is_some() { "present" } else { "absent" },
                if self.visited_rows.is_some() { "enabled" } else { "disabled" },
            ));
        }
        self.states = states;
        self.visited_rows = visited;
        if let Some(deopt) = self.deopt.as_mut() {
            deopt.total_events = deopt_events;
        }
        Ok(())
    }

    /// One abstract frame: run every state through the frame program
    /// (shape-variant dispatch, then the configured deopt mode), then push
    /// the outputs through the boundary pipeline - abstract, band filter,
    /// frontier subtract, merge.
    ///
    /// Two boundary arrangements exist:
    ///
    /// * STREAMING (frontier runs): each input state's raw outputs go
    ///   through the whole pipeline immediately (`stream_boundary_one`) and
    ///   only band-surviving, never-visited lanes are held - raw outputs
    ///   never accumulate across the frame. This is what broke the
    ///   room-(0,0) memory wall: unmerged raw outputs accumulating until
    ///   frame end were the real 100 GB peak.
    /// * PHASED (everything else): outputs accumulate, then each stage runs
    ///   over the whole frame's outputs at once.
    ///
    /// Both produce the same lane sets (equivalence-gated on room (1,0):
    /// identical per-frame new-lane and visited counts, 174,938 lanes /
    /// 673,503 visited at f40); they differ in peak memory and in the
    /// on-disk order of visited rows.
    pub fn step(&mut self) -> Result<()> {
        let result = self.step_inner();
        // Fold this frame's observations in, so the pending list stays a
        // frame's worth rather than a run's. Done even on failure: what was
        // observed before the error is still true.
        if let Some(obs) = self.pos_obs.as_ref() {
            obs.flush();
        }
        result
    }

    fn step_inner(&mut self) -> Result<()> {
        // Every frontier run streams (the CELESTE_STREAM_BOUNDARY opt-in
        // graduated after the modes were shown set-equivalent; see the
        // equivalence note above). Streaming needs the frontier subtract
        // and is meaningless under the widencheck's rem-only abstraction -
        // that corner still takes the phased path.
        let stream = self.visited_rows.is_some() && !self.rem_only_abstraction;
        // Recording used to be refused on a streaming run: the per-lane
        // column would ride into the boundary's row keys and forbid the
        // frontier dedup, which is what sets how coarse the abstraction is,
        // so a recorded streaming run was a DIFFERENT SEARCH.
        //
        // The tag is now stripped at the end of every chunk, before the
        // states reach the boundary (see `interpret_state_base`), so it
        // cannot reach a row key and the objection no longer applies. That
        // is what lets the recording FUSE into the forward pass instead of
        // costing a whole second pass over the room - 3,567 s and a 101 GB
        // peak on room (0,0), the largest stage of the ladder and the
        // reason horizon 95 did not fit.
        //
        // Mid-frame grouping still differs, so this is gated by measurement
        // rather than argument: the fused run's row table must come out
        // element-wise equal to the unfused one's.
        let frame_no = self.states_before_merge.len() as u32 + 1;
        let input_states = chunk_states(std::mem::take(&mut self.states));
        // Chunk-parallel path. Everything else keeps the serial loop below -
        // one code path per arrangement, and the parallel one is opt-in.
        if frame_threads() > 1 {
            return if stream {
                self.step_parallel(input_states, frame_no)
            } else {
                self.step_parallel_phased(input_states, frame_no)
            };
        }
        let mut counters = FrameEventCounters::default();
        let mut stream_counters = StreamCounters::default();
        let mut stream_survivors: Vec<State> = Vec::new();
        let mut new_states: Vec<State> = Vec::new();
        for state in input_states {
            let outputs = {
                let _t = ScopedPhase::new("fwd.interpret");
                interpret_state_base(
                    self.variants.as_ref(),
                    self.deopt.as_ref(),
                    &self.frame_cfg,
                    &self.fixed_env,
                    state,
                    &mut counters,
                    self.pos_obs.as_ref(),
                )?
            };
            if stream {
                // Drain this input state's outputs through the boundary
                // pipeline immediately, so raw outputs never accumulate
                // across chunks.
                let _t = ScopedPhase::new("fwd.boundary_stream");
                let band = self.band.as_ref();
                let visited = self
                    .visited_rows
                    .as_mut()
                    .expect("stream implies a visited table");
                for out in outputs {
                    stream_survivors.extend(stream_boundary_one(
                        out,
                        band,
                        frame_no,
                        visited,
                        &mut stream_counters,
                    )?);
                }
            } else {
                new_states.extend(outputs);
            }
        }
        self.report_frame_events(&counters);
        if stream {
            self.finish_streaming_boundary(stream_survivors, &stream_counters)
        } else {
            self.finish_phased_boundary(new_states, frame_no)
        }
    }

    /// `step` with the frame body and the pure boundary prefix spread over
    /// `frame_threads()` worker threads.
    ///
    /// DETERMINISM is the whole design constraint, because the row table
    /// assigns ids in insertion order and those ids are what checkpoints,
    /// bands and the backward sweep are written in terms of. So:
    ///
    /// * work is handed out in BATCHES of `threads` consecutive chunks;
    /// * inside a batch each worker runs `interpret_state_base` plus
    ///   `stream_boundary_prepare` on its own chunk, touching nothing
    ///   shared;
    /// * the batch's results are then folded in INPUT ORDER on this
    ///   thread - `stream_boundary_subtract` and the counters both.
    ///
    /// So the sequence of rows offered to the visited set is exactly the
    /// serial path's, and the run is byte-identical to a single-threaded
    /// one. The batch (rather than a queue) is also what bounds memory:
    /// at most `threads` chunks' raw outputs are alive at once, which is
    /// why the chunk cap has to come down as the thread count goes up.
    fn step_parallel(&mut self, input_states: Vec<State>, frame_no: u32) -> Result<()> {
        let threads = frame_threads();
        let mut counters = FrameEventCounters::default();
        let mut stream_counters = StreamCounters::default();
        let mut stream_survivors: Vec<State> = Vec::new();
        let variants = self.variants.as_ref();
        let deopt = self.deopt.as_ref();
        let frame_cfg = &self.frame_cfg;
        let fixed_env = &self.fixed_env;
        let band = self.band.as_ref();
        let pos_obs = self.pos_obs.as_ref();

        let mut batch: Vec<State> = Vec::with_capacity(threads);
        let mut queue = input_states.into_iter();
        loop {
            batch.clear();
            for state in queue.by_ref().take(threads) {
                batch.push(state);
            }
            if batch.is_empty() {
                break;
            }
            type Prepared = Result<(Vec<PreparedRows>, FrameEventCounters, StreamCounters)>;
            let results: Vec<Prepared> = {
                let _t = ScopedPhase::new("fwd.interpret");
                let visited_ro: &crate::interpreter::visited::Visited = self
                    .visited_rows
                    .as_ref()
                    .expect("stream implies a visited table");
                std::thread::scope(|scope| {
                    let handles: Vec<_> = batch
                        .drain(..)
                        .map(|state| {
                            scope.spawn(move || -> Prepared {
                                // This thread IS the parallelism; the row
                                // hashing inside must not fan out again.
                                crate::interpreter::virtual_merge::set_nested_parallel(true);
                                let mut ev = FrameEventCounters::default();
                                let mut sc = StreamCounters::default();
                                let outputs = interpret_state_base(
                                    variants, deopt, frame_cfg, fixed_env, state, &mut ev,
                                    pos_obs,
                                )?;
                                let mut prepared = Vec::new();
                                for out in outputs {
                                    prepared.extend(stream_boundary_prepare(
                                        out, band, frame_no, &mut sc, visited_ro,
                                    )?);
                                }
                                Ok((prepared, ev, sc))
                            })
                        })
                        .collect();
                    handles
                        .into_iter()
                        .map(|h| match h.join() {
                            Ok(r) => r,
                            // A worker panicked. The serial path lets the
                            // panic unwind into the deopt handler; here the
                            // frame is already past that point, so surface
                            // it as an error rather than losing the chunk.
                            Err(panic) => Err(anyhow::anyhow!(
                                "chunk-parallel frame worker: {}",
                                panic_text(&panic)
                            )),
                        })
                        .collect()
                })
            };
            let _t = ScopedPhase::new("fwd.boundary_stream");
            let visited = self
                .visited_rows
                .as_mut()
                .expect("stream implies a visited table");
            let mut decided = Vec::new();
            for result in results {
                let (prepared, ev, sc) = result?;
                counters.absorb(&ev);
                stream_counters.absorb(&sc);
                decided.extend(decided_survivors(prepared, visited, &mut stream_counters));
            }
            drop(_t);
            // Phase 3: gather the survivors. Pure given the decision above,
            // so it goes back on the workers - it copies every column of
            // every state that lost lanes.
            let _t = ScopedPhase::new("fwd.boundary_gather");
            let gathered: Vec<Vec<State>> = std::thread::scope(|scope| {
                let chunk = decided.len().div_ceil(threads).max(1);
                decided
                    .chunks_mut(chunk)
                    .map(|part| {
                        scope.spawn(move || {
                            let mut out = Vec::new();
                            for (state, survivors) in part {
                                let survivors = std::mem::replace(
                                    survivors,
                                    crate::interpreter::vectorize::Survivors::All,
                                );
                                if let Some(s) = crate::interpreter::vectorize::subtract_apply(
                                    std::mem::replace(state, State::new()),
                                    survivors,
                                ) {
                                    out.push(s);
                                }
                            }
                            out
                        })
                    })
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(|h| h.join().expect("boundary gather worker"))
                    .collect()
            });
            for part in gathered {
                stream_survivors.extend(part);
            }
        }
        self.report_frame_events(&counters);
        self.finish_streaming_boundary(stream_survivors, &stream_counters)
    }

    /// `step_parallel` for the PHASED arrangement - the backward sweep's
    /// replay, and any run without a visited table.
    ///
    /// Much simpler than the streaming one, because there is nothing
    /// shared: the phase accumulates raw outputs and merges them at the
    /// end, so a worker's only job is `interpret_state_base` and the
    /// results just have to be concatenated in input order. That ordering
    /// is what makes it identical to the serial path, since the merge that
    /// follows is order-sensitive in its lane layout.
    ///
    /// This is the sweep's whole cost. The replay was already ~4x the
    /// forward pass on the same frame (f90: 2747 s vs 740 s) because the
    /// origin column forbids boundary dedup, and it had been left entirely
    /// serial while the forward pass got 5x faster.
    ///
    /// Batched by thread count for the same reason as the streaming path:
    /// the sweep's transient per chunk is what OOMed room (0,0) at h88, so
    /// the chunk cap has to come down as the thread count goes up
    /// (CELESTE_MAX_STATE_LANES, which the sweep sets explicitly).
    fn step_parallel_phased(&mut self, input_states: Vec<State>, frame_no: u32) -> Result<()> {
        let threads = frame_threads();
        let mut counters = FrameEventCounters::default();
        let mut new_states: Vec<State> = Vec::new();
        let variants = self.variants.as_ref();
        let deopt = self.deopt.as_ref();
        let frame_cfg = &self.frame_cfg;
        let fixed_env = &self.fixed_env;
        let pos_obs = self.pos_obs.as_ref();

        let mut batch: Vec<State> = Vec::with_capacity(threads);
        let mut queue = input_states.into_iter();
        loop {
            batch.clear();
            for state in queue.by_ref().take(threads) {
                batch.push(state);
            }
            if batch.is_empty() {
                break;
            }
            type Ran = Result<(Vec<State>, FrameEventCounters)>;
            let results: Vec<Ran> = {
                let _t = ScopedPhase::new("fwd.interpret");
                std::thread::scope(|scope| {
                    let handles: Vec<_> = batch
                        .drain(..)
                        .map(|state| {
                            scope.spawn(move || -> Ran {
                                crate::interpreter::virtual_merge::set_nested_parallel(true);
                                let mut ev = FrameEventCounters::default();
                                let outputs = interpret_state_base(
                                    variants, deopt, frame_cfg, fixed_env, state, &mut ev,
                                    pos_obs,
                                )?;
                                Ok((outputs, ev))
                            })
                        })
                        .collect();
                    handles
                        .into_iter()
                        .map(|h| match h.join() {
                            Ok(r) => r,
                            Err(panic) => Err(anyhow::anyhow!(
                                "chunk-parallel frame worker: {}",
                                panic_text(&panic)
                            )),
                        })
                        .collect()
                })
            };
            for result in results {
                let (outputs, ev) = result?;
                counters.absorb(&ev);
                new_states.extend(outputs);
            }
        }
        self.report_frame_events(&counters);
        self.finish_phased_boundary(new_states, frame_no)
    }

}

/// One input state's whole frame: shape dispatch first (a state whose
/// object-array shape has a registered variant runs under it, through
/// canonical both ways, and skips the base program entirely; the loud
/// fallback of a failing variant frame continues into the base program),
/// then the configured deopt mode.
///
/// A free function taking only shared borrows, so a worker thread can run
/// it - that is the whole point of the split. `run_deopt_frame` and
/// `run_deopt_frame_granular` already take `&DeoptTarget`; the only
/// mutation left was the event counters, which are per-call and aggregated
/// by the caller in deterministic order.
fn interpret_state_base(
    variants: Option<&VariantDispatch>,
    deopt: Option<&DeoptTarget>,
    frame_cfg: &crate::interpreter::fixed_env::PreparedCfg,
    fixed_env: &crate::interpreter::fixed_env::FixedEnv,
    mut state: State,
    counters: &mut FrameEventCounters,
    pos_obs: Option<&super::pos_graph::PosObserver>,
) -> Result<Vec<State>> {
    // Tag each input lane with its own cell before the state is consumed.
    // This is the only place that has both sides of a chunk's transition.
    // The variant path sits below the tag and above the record, so a
    // dispatched frame contributes its transitions like any other.
    if let Some(obs) = pos_obs {
        obs.tag(&mut state)?;
    }
    let mut new_states = Vec::new();
    let declined = match variants {
        Some(vd) => match dispatch_variant_frame(vd, state, counters) {
            VariantOutcome::Ran(outputs) => {
                new_states = outputs;
                None
            }
            VariantOutcome::Base(state) => Some(state),
        },
        None => Some(state),
    };
    if let Some(state) = declined {
        match deopt {
            None => {
                let result = interpret_prepared_cfg(frame_cfg, state, fixed_env)
                    .context("frame failed")?;
                new_states.extend(result.into_iter().map(|(s, _)| s));
            }
            Some(deopt) if deopt.force => {
                counters.deopt.0 += 1;
                counters.deopt.1 += state.vector_size;
                new_states.extend(run_deopt_frame(deopt, state)?);
            }
            Some(deopt) if deopt.collect_first => {
                // Collect-first: every frame runs in collect mode with the
                // origin column, so a failing state pays one specialized
                // run instead of two (attempt + retry). The cost is the
                // origin column's overhead on clean frames - measured
                // before this became reachable (CELESTE_DEOPT_COLLECT_FIRST).
                let (states, plain_lanes) =
                    run_deopt_frame_granular(deopt, frame_cfg, fixed_env, state, false)?;
                if plain_lanes > 0 {
                    counters.deopt.0 += 1;
                    counters.deopt.1 += plain_lanes;
                }
                new_states.extend(states);
            }
            Some(deopt) => {
                // Optimistic: run the specialized frame; deopt on failure.
                // Panics are caught too - a speculated instruction may
                // assert on values the verify horizon never showed it
                // (same failure class `screen_trial` unwinds across).
                let snapshot = state.clone();
                let attempt = std::panic::catch_unwind(std::panic::AssertUnwindSafe(
                    || interpret_prepared_cfg(frame_cfg, state, fixed_env),
                ));
                match attempt {
                    Ok(Ok(result)) => {
                        new_states.extend(result.into_iter().map(|(s, _)| s))
                    }
                    Ok(Err(err)) => {
                        log_deopt(&mut counters.deopt, &snapshot, &format!("{:#}", err));
                        let (states, plain_lanes) =
                            run_deopt_frame_granular(deopt, frame_cfg, fixed_env, snapshot, true)?;
                        counters.deopt.1 += plain_lanes;
                        new_states.extend(states);
                    }
                    Err(panic) => {
                        log_deopt(&mut counters.deopt, &snapshot, &panic_text(&panic));
                        let (states, plain_lanes) =
                            run_deopt_frame_granular(deopt, frame_cfg, fixed_env, snapshot, true)?;
                        counters.deopt.1 += plain_lanes;
                        new_states.extend(states);
                    }
                }
            }
        }
    }
    if let Some(obs) = pos_obs {
        obs.record(&new_states)?;
        // STRIP THE TAG HERE, before the states go anywhere else.
        //
        // The tag exists only to attribute this frame's outputs to this
        // frame's inputs; nothing downstream wants it. Removing it now is
        // what lets recording run on a STREAMING frontier pass at all: the
        // objection to a tagged forward pass was that a per-lane column
        // rides into the boundary row keys and forbids the frontier dedup
        // that sets how coarse the abstraction is. A tag that never reaches
        // the boundary cannot do that.
        //
        // What this does NOT make identical is mid-frame grouping: the
        // column is per-lane distinct, so lane merges inside the frame
        // still see it and still decline. That is deliberate - a merge
        // would fold two source cells into one lane and the table would
        // MISS a pair, which is the unsound direction (g too large, band
        // over-prunes, nothing reports it). So the fragments differ from an
        // untagged run and the reachable set has to be CHECKED equal, not
        // assumed - see the gate in BENCHMARK_DATA.md.
        for state in &mut new_states {
            state.global_env.remove(super::pos_graph::POS_ORIGIN);
            state.gc();
        }
    }
    Ok(new_states)
}

impl AbstractRun {
    /// Aggregate the frame's deopt/variant events and print the per-frame
    /// lines the campaign logs grep for.
    fn report_frame_events(&mut self, counters: &FrameEventCounters) {
        let quiet = self.quiet;
        if let Some(deopt) = self.deopt.as_mut() {
            deopt.total_events.0 += counters.deopt.0;
            deopt.total_events.1 += counters.deopt.1;
            if counters.deopt.0 > 0 && !deopt.force && !quiet {
                println!(
                    "  deopt: {} state(s) / {} lanes re-ran under the plain program",
                    counters.deopt.0, counters.deopt.1
                );
            }
        }
        if let Some(vd) = self.variants.as_mut() {
            vd.total_events.0 += counters.variant.0;
            vd.total_events.1 += counters.variant.1;
            vd.total_fallbacks += counters.variant_fallbacks;
        }
        if counters.variant.0 > 0 && !self.quiet {
            println!(
                "  variant: {} state(s) / {} lanes ran under shape variants",
                counters.variant.0, counters.variant.1
            );
        }
    }

    /// Streaming epilogue: everything already went through abstraction,
    /// band and subtract per state; merge the survivors and report the
    /// aggregate counters in the usual formats.
    fn finish_streaming_boundary(
        &mut self,
        stream_survivors: Vec<State>,
        stream_counters: &StreamCounters,
    ) -> Result<()> {
        self.states_before_merge.push(stream_survivors.len());
        self.states = {
            let _t = ScopedPhase::new("fwd.merge");
            let _trace = crate::interpreter::tracing::TraceSpan::new(
                "merge_frame_boundary",
                "merge_site",
            );
            vectorize_states(stream_survivors)
        };
        if self.band.is_some() {
            println!(
                "  band: {} -> {} lanes in band{}",
                stream_counters.band_before,
                stream_counters.band_after,
                if stream_counters.band_missing > 0 {
                    format!(
                        " ({} lanes with unknown coarse rows dropped)",
                        stream_counters.band_missing
                    )
                } else {
                    String::new()
                }
            );
        }
        let visited = self.visited_rows.as_mut().expect("stream implies visited");
        visited.end_frame()?;
        println!(
            "  frontier-only: {} -> {} new lanes, visited total {}",
            stream_counters.sub_before,
            stream_counters.sub_after,
            visited.len()
        );
        self.boundary_gc_if_enabled();
        Ok(())
    }

    /// Phased epilogue: abstract the whole frame's outputs, merge, then
    /// band-filter and frontier-subtract the merged states.
    fn finish_phased_boundary(&mut self, new_states: Vec<State>, frame_no: u32) -> Result<()> {
        let new_states: Vec<State> = {
            let _t = ScopedPhase::new("fwd.abstract");
            if self.rem_only_abstraction {
                new_states
                    .into_iter()
                    .map(crate::interpreter::abstraction::make_state_abstract_rem_only)
                    .collect()
            } else {
                new_states
                    .into_iter()
                    .flat_map(crate::interpreter::abstraction::split_precision_straddles)
                    .map(make_state_abstract)
                    .collect()
            }
        };
        self.states_before_merge.push(new_states.len());
        self.states = {
            let _t = ScopedPhase::new("fwd.merge");
            let _trace = crate::interpreter::tracing::TraceSpan::new(
                "merge_frame_boundary",
                "merge_site",
            );
            vectorize_states(new_states)
        };
        self.apply_band_filter(frame_no)?;
        self.subtract_frontier()?;
        self.boundary_gc_if_enabled();
        Ok(())
    }

    /// Drop lanes whose coarsened row is outside the previous precision
    /// level's band (phased path; the streaming path does this per state in
    /// `stream_boundary_one`).
    fn apply_band_filter(&mut self, frame: u32) -> Result<()> {
        let Some(band) = &self.band else { return Ok(()) };
        let budget = band.horizon.saturating_sub(frame);
        let mut kept_states = Vec::new();
        let (mut before, mut after, mut missing) = (0usize, 0usize, 0usize);
        for state in std::mem::take(&mut self.states) {
            before += state.vector_size;
            let mut coarse = crate::interpreter::abstraction::coarsen_to(
                state.clone(),
                band.prev_precision,
            );
            coarse.gc();
            let keys = super::sweep::row_keys(&coarse)?;
            let mask: Vec<bool> = keys
                .iter()
                .map(|k| match band.prev_table.id_of(*k) {
                    Some(id) => {
                        let e = band
                            .prev_table
                            .earliest_frame(id)
                            .unwrap_or(u32::MAX);
                        let g = band.g_prev[id as usize];
                        e <= frame
                            && g != super::sweep::G_UNREACHABLE
                            && (g as u32) <= budget
                    }
                    None => {
                        // Sound to drop: the coarse row was never even
                        // reachable at the previous level, so nothing
                        // that maps to it can be on a winning path. A
                        // nonzero count at the FIRST refinement level is
                        // a canonicalization bug though - watch it.
                        missing += 1;
                        false
                    }
                })
                .collect();
            let kept = mask.iter().filter(|b| **b).count();
            after += kept;
            if kept == state.vector_size {
                kept_states.push(state);
            } else if kept > 0 {
                kept_states.push(state.filter_by_mask_clone(
                    &mask,
                    crate::interpreter::state::FILTER_BAND,
                ));
            }
        }
        self.states = kept_states;
        println!(
            "  band: {} -> {} lanes in band{}",
            before,
            after,
            if missing > 0 {
                format!(" ({} lanes with unknown coarse rows dropped)", missing)
            } else {
                String::new()
            }
        );
        Ok(())
    }

    /// Frontier-only subtract (phased path): drop lanes whose row was seen
    /// in any earlier frame, and record this frame's rows.
    fn subtract_frontier(&mut self) -> Result<()> {
        let Some(visited) = self.visited_rows.as_mut() else { return Ok(()) };
        let (kept, before, after) = crate::interpreter::vectorize::subtract_visited(
            std::mem::take(&mut self.states),
            visited,
        );
        visited.end_frame()?;
        println!(
            "  frontier-only: {} -> {} new lanes, visited total {}",
            before, after, visited.len()
        );
        self.states = kept;
        Ok(())
    }

    /// Boundary compaction (env-gated: CELESTE_BOUNDARY_GC). The heap's
    /// storage is append-only and shared by Arc: without a gc, boundary
    /// states pin the whole mid-frame append log - every superseded
    /// HeapValue of every store - and the search's residency runs ~100x
    /// the frontier's materialized size (room (0,0) f79: ~70 GB resident
    /// vs 549 MB for the same states freshly loaded from disk). gc
    /// rebuilds each state's heap from its reachable cells, dropping the
    /// log. Row hashing is layout-independent (the probe paths gc before
    /// row_keys and match the search's tables), so hashes and standing
    /// checkpoints are unaffected.
    fn boundary_gc_if_enabled(&mut self) {
        if std::env::var_os("CELESTE_BOUNDARY_GC").is_some() {
            let _trace = crate::interpreter::tracing::TraceSpan::new(
                "boundary_gc",
                "gc",
            );
            for state in &mut self.states {
                state.gc();
            }
        }
    }

    pub fn states(&self) -> &[State] {
        &self.states
    }

    pub fn lane_count(&self) -> usize {
        self.states.iter().map(|s| s.vector_size).sum()
    }

    /// (total, mean, max) states produced per frame before merging.
    pub fn states_before_merge(&self) -> (usize, f64, usize) {
        let total: usize = self.states_before_merge.iter().sum();
        let n = self.states_before_merge.len().max(1);
        (
            total,
            total as f64 / n as f64,
            self.states_before_merge.iter().copied().max().unwrap_or(0),
        )
    }
}

/// Lane-granular deopt: retry the failing frame under the specialized program
/// with a per-lane origin column and the interpreter in collect mode (see
/// `deopt_collect`), so premise-violating lanes are captured instead of
/// aborting the frame; then re-run only the captured lanes under the plain
/// program. Falls back to the whole-state path (`run_deopt_frame`) on
/// anything unexpected - a retry that fails for a non-premise reason, a
/// panic, or a lane-coverage accounting mismatch.
///
/// Returns the frame outputs plus how many lanes actually re-ran under plain.
fn run_deopt_frame_granular(
    deopt: &DeoptTarget,
    frame_cfg: &crate::interpreter::fixed_env::PreparedCfg,
    fixed_env: &crate::interpreter::fixed_env::FixedEnv,
    snapshot: State,
    expect_failure: bool,
) -> Result<(Vec<State>, usize)> {
    use crate::interpreter::deopt_collect;
    use crate::interpreter::state::FILTER_DEOPT;

    let n = snapshot.vector_size;
    let mut tagged = snapshot.clone();
    inject_origin(&mut tagged);
    deopt_collect::begin();
    let attempt = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        interpret_prepared_cfg(frame_cfg, tagged, fixed_env)
    }));
    let captured = deopt_collect::take();
    let result = match attempt {
        // Collect-first mode: no premise fired - the common case. Strip the
        // origin column and hand the outputs straight back.
        Ok(Ok(result)) if captured.is_empty() && !expect_failure => {
            let mut out = Vec::with_capacity(result.len());
            for (mut state, _) in result {
                state.global_env.remove(deopt_collect::ORIGIN_GLOBAL);
                out.push(state);
            }
            return Ok((out, 0));
        }
        Ok(Ok(result)) if !captured.is_empty() => result,
        Ok(Ok(_)) => {
            // The first attempt failed but the retry captured nothing and
            // succeeded - nondeterminism somewhere. Ground truth is plain.
            println!("  deopt: retry captured nothing; whole-state fallback");
            return Ok((run_deopt_frame(deopt, snapshot)?, n));
        }
        Ok(Err(err)) => {
            let one_line = format!("{:#}", err).replace('\n', " | ");
            println!(
                "  deopt: retry failed for a non-premise reason ({}); whole-state fallback",
                one_line.chars().take(400).collect::<String>()
            );
            return Ok((run_deopt_frame(deopt, snapshot)?, n));
        }
        Err(panic) => {
            println!(
                "  deopt: retry panicked ({}); whole-state fallback",
                panic_text(&panic).chars().take(400).collect::<String>()
            );
            return Ok((run_deopt_frame(deopt, snapshot)?, n));
        }
    };

    let failed: rustc_hash::FxHashSet<u32> = captured.iter().copied().collect();

    // Accounting: every input lane must end up either in the specialized
    // outputs or in the captured set. A lane in neither vanished silently -
    // that is a machinery bug, and the whole-state path is the sound answer.
    let mut covered = vec![false; n];
    let mut in_range = true;
    for &o in &captured {
        match covered.get_mut(o as usize) {
            Some(c) => *c = true,
            None => in_range = false,
        }
    }

    let mut out = Vec::new();
    for (state, _) in result {
        let origins = deopt_collect::read_origins(&state);
        for &o in &origins {
            match covered.get_mut(o as usize) {
                Some(c) => *c = true,
                None => in_range = false,
            }
        }
        let mask: Vec<bool> = origins.iter().map(|o| !failed.contains(o)).collect();
        let mut state = if mask.iter().all(|k| *k) {
            state
        } else if mask.iter().any(|k| *k) {
            state.filter_by_mask_clone(&mask, FILTER_DEOPT)
        } else {
            continue;
        };
        state.global_env.remove(deopt_collect::ORIGIN_GLOBAL);
        out.push(state);
    }

    if !in_range || !covered.iter().all(|c| *c) {
        println!(
            "  deopt: lane accounting mismatch on the retry; whole-state fallback"
        );
        return Ok((run_deopt_frame(deopt, snapshot)?, n));
    }

    let plain_mask: Vec<bool> = (0..n).map(|i| failed.contains(&(i as u32))).collect();
    let plain_input = snapshot.filter_by_mask_clone(&plain_mask, FILTER_DEOPT);
    let plain_lanes = plain_input.vector_size;
    out.extend(run_deopt_frame(deopt, plain_input)?);
    Ok((out, plain_lanes))
}

/// Give every lane of `state` a distinct origin index (a synthetic global the
/// program never reads; see `deopt_collect::ORIGIN_GLOBAL`). The index is the
/// lane position, bijectively encoded in the raw Pico8Num bits.
fn inject_origin(state: &mut State) {
    let ids: Vec<u32> = (0..state.vector_size.max(1) as u32).collect();
    crate::interpreter::deopt_collect::inject_named(
        state,
        crate::interpreter::deopt_collect::ORIGIN_GLOBAL,
        &ids,
    );
}

/// One frame of one state under the plain program: map the input to canonical,
/// run, map the outputs back to the specialized representation.
///
/// An error here is terminal on purpose: the plain program is ground truth, so
/// a state that fails under it too is a real bug, not a missed specialization.
fn run_deopt_frame(deopt: &DeoptTarget, state: State) -> Result<Vec<State>> {
    let started = std::time::Instant::now();
    let out = run_deopt_frame_inner(deopt, state);
    crate::op_census::record_deopt_nanos(started.elapsed().as_nanos() as u64);
    out
}

fn run_deopt_frame_inner(deopt: &DeoptTarget, mut state: State) -> Result<Vec<State>> {
    deopt
        .mapping
        .to_canonical(&mut state)
        .context("deopt: mapping the frame input to canonical")?;
    let result = interpret_prepared_cfg(&deopt.plain_cfg, state, &deopt.plain_env)
        .context("deopt: the frame failed under the plain program too")?;
    let mut out = Vec::with_capacity(result.len());
    for (mut s, _) in result {
        deopt
            .mapping
            .from_canonical(&mut s)
            .context("deopt: mapping a frame output back from canonical")?;
        out.push(s);
    }
    Ok(out)
}

/// Count a deopt event; print the trigger for the first one each frame (the
/// rest are usually the same premise firing across states, and the per-frame
/// summary line carries the count). The *lane* count is added by the caller -
/// with the granular path, only the lanes that actually re-ran under plain.
fn log_deopt(frame_events: &mut (usize, usize), state: &State, reason: &str) {
    frame_events.0 += 1;
    if frame_events.0 == 1 {
        let one_line = reason.replace('\n', " | ");
        let short: String = one_line.chars().take(240).collect();
        println!("  deopt trigger (state of {} lanes): {}", state.vector_size, short);
    }
}

fn panic_text(panic: &Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = panic.downcast_ref::<String>() {
        format!("panic: {}", s)
    } else if let Some(s) = panic.downcast_ref::<&str>() {
        format!("panic: {}", s)
    } else {
        "panic with a non-string payload".to_string()
    }
}

pub struct Divergence {
    pub frame: u32,
    pub detail: String,
}

/// The canonical observation after each of `frames` frames, plus the initial
/// one. Precomputed once so that screening many candidates against the same
/// baseline does not re-run the baseline every time.
pub fn observation_trace(
    program: &Program,
    frames: u32,
) -> Result<Vec<BTreeSet<StateObservation>>> {
    let mut run = AbstractRun::start(program)?;
    let mut out = vec![observe_frame(run.states())];
    for _ in 1..=frames {
        run.step()?;
        out.push(observe_frame(run.states()));
    }
    Ok(out)
}

/// Runs `candidate` against a precomputed baseline trace.
///
/// Same check as `differential_abstract`, but for screening a batch of
/// candidates against one baseline. Note that a candidate can also *fail* -
/// `select` refuses values it cannot represent per lane - which is the expected
/// outcome for a good fraction of `if_convert` sites and is reported as a
/// divergence rather than propagated.
pub fn differential_against_trace(
    baseline: &[BTreeSet<StateObservation>],
    candidate: &Program,
    frames: u32,
) -> Result<Option<Divergence>> {
    let mut run = match AbstractRun::start(candidate) {
        Ok(run) => run,
        Err(e) => {
            return Ok(Some(Divergence { frame: 0, detail: format!("init failed: {:#}", e) }))
        }
    };
    for frame in 0..=frames {
        if frame > 0 {
            if let Err(e) = run.step() {
                return Ok(Some(Divergence { frame, detail: format!("{:#}", e) }));
            }
        }
        let observed = observe_frame(run.states());
        let Some(expected) = baseline.get(frame as usize) else { break };
        if &observed != expected {
            return Ok(Some(Divergence {
                frame,
                detail: describe(
                    expected,
                    &observed,
                    expected.len(),
                    run.lane_count(),
                ),
            }));
        }
    }
    Ok(None)
}

/// Runs a shape VARIANT against a precomputed baseline trace.
///
/// `host` runs every state whose object-array shape the variant does not
/// claim; the variant runs the rest, through the canonical form both ways.
/// The baseline to pass is the host's own trace: a variant's contract is
/// that dispatch is INVISIBLE (see `Variant`), so "the host with this
/// variant registered observes what the host alone observes" is exactly the
/// claim, and it is checkable at a depth where the variant's shape actually
/// occurs. The host's own equivalence to the plain program is a separate
/// gate that `verify` already runs.
///
/// Two failure modes are specific to this mode and both are checked here,
/// because neither shows up as a divergence:
///
///   * a variant frame that FAILS falls back to the host and produces the
///     right answer, so a variant whose premises never hold would otherwise
///     screen clean. Any fallback is a rejection.
///   * a variant that never dispatched at all was never exercised, so the
///     run says nothing about it. That is precisely how 129 entries on
///     never-executed object functions got into a recipe unchecked
///     (see the `rewrites-room00.jsonl` commit); refuse it instead.
pub fn differential_variant_against_trace(
    baseline: &[BTreeSet<StateObservation>],
    host: &Program,
    host_mapping: super::state_mapping::StateMapping,
    variant: Variant,
    frames: u32,
) -> Result<Option<Divergence>> {
    let mut run = match AbstractRun::start(host) {
        Ok(run) => run,
        Err(e) => {
            return Ok(Some(Divergence { frame: 0, detail: format!("init failed: {:#}", e) }))
        }
    };
    run.set_variants(host_mapping, vec![variant]);
    run.quiet = true;
    for frame in 0..=frames {
        if frame > 0 {
            if let Err(e) = run.step() {
                return Ok(Some(Divergence { frame, detail: format!("{:#}", e) }));
            }
        }
        let (_, _, fallbacks) = run.variant_events();
        if fallbacks > 0 {
            return Ok(Some(Divergence {
                frame,
                detail: format!("{} variant frame(s) fell back to the host program", fallbacks),
            }));
        }
        let observed = observe_frame(run.states());
        let Some(expected) = baseline.get(frame as usize) else { break };
        if &observed != expected {
            return Ok(Some(Divergence {
                frame,
                detail: describe(expected, &observed, expected.len(), run.lane_count()),
            }));
        }
    }
    let (states, _, _) = run.variant_events();
    if states == 0 {
        return Ok(Some(Divergence {
            frame: frames,
            detail: format!(
                "the variant never dispatched in {} frames - nothing was exercised",
                frames
            ),
        }));
    }
    Ok(None)
}

/// Runs both programs for `frames` frames, comparing the canonical observation
/// after each one. Stops at the first divergence.
pub fn differential_abstract(
    baseline: &Program,
    candidate: &Program,
    frames: u32,
) -> Result<Option<Divergence>> {
    let mut a = AbstractRun::start(baseline)?;
    let mut b = AbstractRun::start(candidate)?;

    let obs_a = observe_frame(a.states());
    let obs_b = observe_frame(b.states());
    if obs_a != obs_b {
        return Ok(Some(Divergence {
            frame: 0,
            detail: describe(&obs_a, &obs_b, a.lane_count(), b.lane_count()),
        }));
    }

    for frame in 1..=frames {
        a.step()?;
        b.step()?;
        let obs_a = observe_frame(a.states());
        let obs_b = observe_frame(b.states());
        if obs_a != obs_b {
            return Ok(Some(Divergence {
                frame,
                detail: describe(&obs_a, &obs_b, a.lane_count(), b.lane_count()),
            }));
        }
    }
    Ok(None)
}

fn describe(
    a: &BTreeSet<StateObservation>,
    b: &BTreeSet<StateObservation>,
    lanes_a: usize,
    lanes_b: usize,
) -> String {
    let only_a = a.difference(b).count();
    let only_b = b.difference(a).count();
    let mut detail = format!(
        "baseline: {} states / {} lanes, candidate: {} states / {} lanes; \
         {} state(s) only in baseline, {} only in candidate",
        a.len(),
        lanes_a,
        b.len(),
        lanes_b,
        only_a,
        only_b
    );

    // If exactly one state differs on each side, say how.
    if let (Some(x), Some(y)) = (a.difference(b).next(), b.difference(a).next()) {
        if x.structure.len() != y.structure.len() {
            detail.push_str(&format!(
                "\n  heap size differs: {} vs {}",
                x.structure.len(),
                y.structure.len()
            ));
        } else {
            let differing: Vec<usize> = x
                .structure
                .iter()
                .zip(y.structure.iter())
                .enumerate()
                .filter(|(_, (p, q))| p != q)
                .map(|(i, _)| i)
                .take(8)
                .collect();
            if !differing.is_empty() {
                detail.push_str(&format!("\n  structure differs at heap slots {:?}", differing));
                // Name the differing slots: which cell kinds disagree is
                // usually the whole diagnosis (2026-08-16, the room (2,0)
                // v6 divergence hunt).
                for &i in differing.iter().take(4) {
                    detail.push_str(&format!(
                        "\n    slot {}: baseline {:?} vs candidate {:?}",
                        i, x.structure[i], y.structure[i]
                    ));
                }
            } else if x.rows != y.rows {
                detail.push_str(&format!(
                    "\n  same structure, {} vs {} distinct lane rows",
                    x.rows.len(),
                    y.rows.len()
                ));
            } else if x.globals != y.globals {
                detail.push_str("\n  globals differ");
            }
        }
    }
    detail
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::Instruction;

    /// A differential checker that never fails is worthless. Deliberately
    /// corrupt the program and confirm the checker notices.
    ///
    /// The corruption is a single changed numeric constant in a function that
    /// runs every frame - about the smallest semantic change expressible.
    #[test]
    fn differential_run_catches_a_changed_constant() {
        let baseline = match Program::compile_from_disk() {
            Ok(p) => p,
            // The test needs lua/ next to the working directory; skip rather
            // than fail when run from somewhere else.
            Err(_) => return,
        };
        let mut broken = baseline.clone();

        let fun = broken
            .get_mut("player_spawn.update_24")
            .expect("player_spawn.update_24 exists");
        let mut patched = false;
        for block in std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut()) {
            for (_, instr) in block.instructions.iter_mut() {
                if let Instruction::NumberConstant { value } = instr {
                    *value = *value + crate::pico8_num::Pico8Num::from_i16(1);
                    patched = true;
                    break;
                }
            }
            if patched {
                break;
            }
        }
        assert!(patched, "expected a numeric constant to corrupt");

        let result = differential_abstract(&baseline, &broken, 26)
            .expect("differential run should complete");
        assert!(
            result.is_some(),
            "differential verification passed a program with a changed constant"
        );
    }

    /// End-to-end check of the lane-granular deopt machinery: corrupt the
    /// rewritten program with a synthetic premise that only *some* lanes
    /// satisfy - an `assert_true` on an `expand`-produced button bool, so
    /// half the expanded lanes falsify it every frame - and run with deopt.
    /// The captured lanes re-run under the plain program through the
    /// canonical-state mapping; the result must match the unmodified
    /// rewritten program's observations exactly, every frame.
    ///
    /// This exercises: origin injection and stripping, collect-mode capture
    /// and mid-fragment filtering, the lane-coverage accounting, the plain
    /// re-run of only the failed lanes, and the merge of both output sets.
    #[test]
    fn granular_deopt_reproduces_the_baseline() {
        // Serialise against the partition-toggle tests: this test compares
        // a baseline run against a candidate run, and a toggle flip
        // between the two makes them diverge spuriously.
        let _partition =
            crate::interpreter::partition_straddles_test_lock();
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        let recipe = crate::rewrite::recipe::Recipe::load("rewrites.jsonl").expect("load recipe");
        let (program, _) = crate::rewrite::recipe::build(&recipe).expect("build rewritten");
        let plain = Program::compile_from_disk().expect("compile plain");
        let mapping = crate::rewrite::state_mapping::StateMapping::from_recipe(&recipe);
        assert!(!mapping.is_identity());

        // Corrupt: assert the outputs of the first few comparisons in the
        // fused frame body. Comparisons on player state are lane-mixed once
        // the input fan-out starts, so some lanes falsify these synthetic
        // premises every frame - the partial-capture path - while scalar
        // frames and uniform fragments exercise the capture-all path.
        let mut corrupted = program.clone();
        let fun = corrupted
            .get_mut("anonymous_61")
            .expect("the fused frame body exists");
        let mut next_id: usize = std::iter::once(&fun.cfg.entry)
            .chain(fun.cfg.named.values())
            .flat_map(|b| {
                b.instructions
                    .iter()
                    .map(|(id, _)| usize::from(*id))
                    .chain(std::iter::once(usize::from(b.terminator.0)))
            })
            .max()
            .unwrap_or(0)
            + 1;
        let mut inserted = 0;
        for block in std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut()) {
            let mut index = 0;
            while index < block.instructions.len() && inserted < 3 {
                if matches!(
                    block.instructions[index].1,
                    Instruction::BinaryOp {
                        op: crate::ir::BinaryOp::LessThan | crate::ir::BinaryOp::GreaterThan,
                        ..
                    }
                ) {
                    let target = block.instructions[index].0;
                    block.instructions.insert(
                        index + 1,
                        (
                            crate::ir::LocalId::from(next_id),
                            Instruction::AssertTrue { value: target },
                        ),
                    );
                    next_id += 1;
                    inserted += 1;
                    index += 1;
                }
                index += 1;
            }
            if inserted >= 3 {
                break;
            }
        }
        assert!(inserted > 0, "no comparison found to corrupt");

        let frames = 28;
        let baseline = observation_trace(&program, frames).expect("baseline trace");
        let mut run = AbstractRun::start_with_deopt(&corrupted, &plain, mapping, false)
            .expect("start deopt run");
        for frame in 1..=frames {
            run.step().expect("step");
            assert_eq!(
                observe_frame(run.states()),
                baseline[frame as usize],
                "granular deopt diverged from the baseline at frame {}",
                frame
            );
        }
        let (states, lanes) = run.deopt_events();
        assert!(states > 0, "the synthetic premise never fired");
        assert!(lanes > 0, "no lanes re-ran under the plain program");
    }

    /// End-to-end check of the shape-dispatch machinery (`Variant`): run the
    /// plain-compiled program as the base with the full recipe registered as
    /// a variant for the singleton shapes. Every state in room (1,0) is a
    /// singleton, so every frame of every state dispatches to the variant -
    /// base -> canonical -> variant on the way in, back on the way out - and
    /// the observations must match a plain-only run exactly, every frame.
    ///
    /// This exercises: the object-shape probe, shape matching, both mapping
    /// directions around a variant frame, and the zero-fallback invariant.
    #[test]
    fn shape_variant_dispatch_reproduces_the_baseline() {
        // Serialise against the partition-toggle tests: this test compares
        // a baseline run against a candidate run, and a toggle flip
        // between the two makes them diverge spuriously.
        let _partition =
            crate::interpreter::partition_straddles_test_lock();
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        let plain = Program::compile_from_disk().expect("compile plain");
        let recipe = crate::rewrite::recipe::Recipe::load("rewrites.jsonl").expect("load recipe");
        let (rewritten, _) = crate::rewrite::recipe::build(&recipe).expect("build rewritten");
        let mapping = crate::rewrite::state_mapping::StateMapping::from_recipe(&recipe);
        assert!(!mapping.is_identity());

        let frames = 28;
        let baseline = observation_trace(&plain, frames).expect("baseline trace");
        let mut run = AbstractRun::start(&plain).expect("start base run");
        // Shape probe sanity: room (1,0) starts as a lone player_spawn.
        assert_eq!(
            crate::interpreter::abstraction::object_shape(&run.states()[0]).expect("shape probe"),
            vec!["player_spawn".to_string()]
        );
        run.set_variants(
            // The base program is the plain one: its layout IS canonical.
            crate::rewrite::state_mapping::StateMapping::default(),
            vec![Variant {
                label: "rewrites.jsonl[test]".to_string(),
                shapes: vec![
                    vec!["player".to_string()],
                    vec!["player_spawn".to_string()],
                ],
                frame_cfg: crate::interpreter::fixed_env::PreparedCfg::new(
                    rewritten.frame_cfg().clone(),
                ),
                fixed_env: rewritten.fixed_env(),
                mapping,
            }],
        );
        for frame in 1..=frames {
            run.step().expect("step");
            assert_eq!(
                observe_frame(run.states()),
                baseline[frame as usize],
                "variant dispatch diverged from the baseline at frame {}",
                frame
            );
        }
        let (states, lanes, fallbacks) = run.variant_events();
        assert!(states > 0 && lanes > 0, "no frames ran under the variant");
        assert_eq!(fallbacks, 0, "variant frames fell back to the base program");
    }

    /// And it must not cry wolf: the program compared against itself is equal.
    #[test]
    fn differential_run_accepts_an_identical_program() {
        let Ok(baseline) = Program::compile_from_disk() else { return };
        let candidate = baseline.clone();
        let result = differential_abstract(&baseline, &candidate, 26)
            .expect("differential run should complete");
        assert!(result.is_none(), "identical programs reported as diverging");
    }
}
