//! The abstract forward search driver.
//!
//! `AbstractRun` is one forward run of the game over a *set* of states: start
//! from the initial state, and step frame by frame, where a step feeds every
//! reachable button combination to `FrameEngine::step` and then dedups,
//! merges and re-chunks the successors. Everything that decides how wide a
//! frame is allowed to get - the chunk caps, the band filter, the partition
//! filter - lives here.
//!
//! It also defines the canonical **observation** of a state
//! (`StateObservation` / `observe_frame`): the heap structure plus the sorted
//! set of lane rows, with heap ids renumbered by `gc()`. That is the form two
//! runs are compared in, and `super::differential` is what compares them.

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

use crate::program::Program;

/// One value in one lane, in a form that can be ordered and hashed.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub(super) enum Cell {
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
    pub(super) structure: Vec<Cell>,
    /// Global name -> heap id.
    pub(super) globals: Vec<(String, usize)>,
    /// One row per lane, sorted. Each row has one entry per per-lane slot, in
    /// heap order.
    pub(super) rows: BTreeSet<Vec<Cell>>,
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
    /// Position-transition recording (`search::pos_graph`). `Some` only
    /// when a caller asked for it; the probe reads the frame body's input
    /// and output positions and injects nothing, so a recorded run takes
    /// exactly the same path as an unrecorded one.
    pos_obs: Option<super::pos_graph::PosObserver>,
    /// Suppress the per-frame deopt/variant reporting. For the screening
    /// runs, whose stdout is a JSONL stream of accepted entries and has to
    /// stay pipeable; a hundred trials' telemetry in the middle of it is
    /// not telemetry, it is corruption. The counters are still tallied.
    pub(super) quiet: bool,
    /// The compiled frame body (`CELESTE_COMPILED_FORWARD`); `None` means
    /// every chunk runs the interpreter. See `CompiledForward`.
    pub(super) compiled: Option<&'static CompiledForward>,
    /// Stop the boundary after the abstraction and the GC, leaving the
    /// frame's output as FRAGMENTS instead of merging them by shape. See
    /// `skip_boundary_merge`.
    skip_merge: bool,
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
    /// Partition-class conditions extending the dispatch key from shape to
    /// (shape, pm1 class): `(cell pattern, required value)` pairs, matched
    /// with the same name rule as `partition_merge` cells (exact or
    /// `.<pattern>` suffix). Empty means shape-only dispatch.
    ///
    /// A state matches only if every pattern resolves to at least one cell
    /// and every resolved cell holds a per-state scalar number equal to the
    /// required value. Boundary states are class-uniform by construction
    /// (the partitioned merge splits by these very cells), so a vector
    /// value here means the state straddles classes and soundly falls
    /// through to the base program. Dispatch stays a pure function of the
    /// state, so the chunk-parallel path is unaffected.
    pub pm1: Vec<(String, crate::pico8_num::Pico8Num)>,
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
    let prepared = stream_boundary_prepare(state, band, frame, counters, visited, true, None)?;
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
/// `filter_now` selects which phase-1 the fragment gets. `true` is the
/// classic path: `visited_row_keys` filters in the worker with a
/// per-FRAGMENT seen set. `false` is the partitioned path (D4): only hash,
/// and leave the filtering to `partition_filter`, whose per-thread seen
/// sets persist for the whole frame - D2 measured the difference at 218 vs
/// 6 ns per offered row, because a fragment-scoped set re-probes every new
/// key ~8x and one mmap probe costs ~700 ns.
fn stream_boundary_prepare(
    state: State,
    band: Option<&BandFilter>,
    frame: u32,
    counters: &mut StreamCounters,
    visited: &crate::interpreter::visited::Visited,
    filter_now: bool,
    // ENGINE keys carried from the compiled body (Option 1). `Some` => the
    // frontier is engine-keyed and these ARE the per-lane keys, so the
    // interpreter re-hash is skipped. Only supported when the state does not
    // rung-split (level 0, which is where Option 1 runs) and the partitioned
    // filter is on.
    carried: Option<Vec<(u64, u64)>>,
) -> Result<Vec<PreparedRows>> {
    let mut kept_out = Vec::new();
    let t_abs = std::time::Instant::now();
    let split = crate::interpreter::abstraction::split_precision_straddles(state);
    if carried.is_some() {
        assert_eq!(
            split.len(),
            1,
            "engine-keyed frontier (Option 1) does not support a rung split; run level 0"
        );
        assert!(!filter_now, "engine-keyed frontier needs the partitioned filter");
    }
    let mut abs_ns = t_abs.elapsed().as_nanos() as u64;
    for state in split {
        let t_abs = std::time::Instant::now();
        let state = make_state_abstract(state);
        abs_ns += t_abs.elapsed().as_nanos() as u64;
        add_worker_ns(WORKER_ABSTRACT, std::mem::take(&mut abs_ns));
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
        let t_gc = std::time::Instant::now();
        state.gc();
        add_worker_ns(WORKER_GC, t_gc.elapsed().as_nanos() as u64);
        // Phase 1 of the frontier subtract - hashing every lane into its
        // 128-bit row key and looking it up read-only. This is where the
        // per-lane cache miss lives, and it needs only `&RowTable`, so it
        // belongs on this side of the parallel/serial line.
        let t_keys = std::time::Instant::now();
        let keys = if let Some(ck) = &carried {
            // Engine keys carried from the compiled body: the frontier is
            // engine-keyed, so use them directly (no interpreter re-hash).
            assert_eq!(
                ck.len(),
                state.vector_size,
                "carried engine keys ({}) != lanes ({}) after abstraction",
                ck.len(),
                state.vector_size
            );
            PreparedKeys::Raw(Some(ck.clone()))
        } else if filter_now {
            PreparedKeys::Filtered(crate::interpreter::vectorize::visited_row_keys(
                &state, visited,
            ))
        } else {
            PreparedKeys::Raw(crate::interpreter::vectorize::visited_lane_keys(&state))
        };
        add_worker_ns(WORKER_KEYS, t_keys.elapsed().as_nanos() as u64);
        kept_out.push(PreparedRows { state, keys });
    }
    Ok(kept_out)
}

/// A boundary state with its visited-set keys computed. Only the
/// id-assigning insert is left, and that has to happen in input order.
struct PreparedRows {
    state: State,
    keys: PreparedKeys,
}

enum PreparedKeys {
    /// Filtered in the worker (`visited_row_keys`): candidates ready for
    /// `insert_new`. `None` = no columns = every lane new.
    Filtered(Option<crate::interpreter::vectorize::VisitedKeys>),
    /// Hash-only (`visited_lane_keys`): one key per lane, filtering
    /// pending in `partition_filter`. Same `None` meaning.
    Raw(Option<Vec<(u64, u64)>>),
}

/// The partitioned filter's default is ON; `CELESTE_PARTITIONED_FILTER=0`
/// restores the classic in-worker filter. Not in the campaign fingerprint
/// for the same reason the visited engine choice is not: the outputs are
/// gated identical (H=68 sidecar set-identity plus byte-identical id
/// assignment by construction), so checkpoints are interchangeable.
/// Option 1 (`CELESTE_FRONTIER_SKIP=1`): the traced kernels probe the FROZEN
/// frontier and skip materializing rows already in it. Requires the frontier
/// be frozen (run with `CELESTE_FRONTIER_BUFFERED=1`), else the worker leaves
/// it off (see the guard). Not in the fingerprint: outputs are byte-identical
/// (only frontier rows are skipped, which the subtract would have dropped).
fn frontier_skip_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_FRONTIER_SKIP").is_some())
}

thread_local! {
    /// Per-output-state ENGINE row keys stashed by `CompiledForward::run_chunk`
    /// and consumed by the streaming worker on the SAME thread immediately
    /// after `interpret_state_base` returns (Option 1's engine-keyed frontier).
    /// `Some(keys)` = engine keys carried from a kernel/merged block; `None` =
    /// an interpreter/fallback state that takes the interpreter key path.
    static CARRIED_KEYS: std::cell::RefCell<Vec<Option<Vec<(u64, u64)>>>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

fn set_carried_keys(keys: Vec<Option<Vec<(u64, u64)>>>) {
    CARRIED_KEYS.with(|c| *c.borrow_mut() = keys);
}

/// Take (and clear) the keys stashed for the just-produced outputs. Returns
/// empty when the frame body did not carry any (interpreter/deopt path).
fn take_carried_keys() -> Vec<Option<Vec<(u64, u64)>>> {
    CARRIED_KEYS.with(|c| std::mem::take(&mut *c.borrow_mut()))
}

/// Whether the compiled forward path keys the frontier by the ENGINE key it
/// carries. This is the ONE predicate that couples every engine-keyed piece
/// (routing through step_parallel, the carry, the frozen-frontier guard): the
/// frozen-frontier skip (Option 1) OR the within-frame skip (Option 4) turns it
/// on, because the within-frame skip probes and populates the SAME engine key
/// space and would be unsound/divergent on an interpreter-keyed frontier. So
/// there is no path where the within-frame skip is on but the frontier is not
/// engine-keyed.
fn engine_keyed_frontier() -> bool {
    frontier_skip_on() || within_frame_skip_on()
}

/// Option 4: the racy within-frame skip. Implies the engine-keyed frontier
/// (it probes the same key space) and requires a frozen frontier.
pub(crate) fn within_frame_skip_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_WITHIN_FRAME_SKIP").is_some())
}

/// The process-wide within-frame set, reused across frames (cleared per frame).
/// 2^22 slots (~4M) for ~1.2M distinct successors/frame - load factor ~0.3.
fn within_frame_set() -> &'static crate::compiled::dispatch::WithinFrameSet {
    static SET: std::sync::OnceLock<crate::compiled::dispatch::WithinFrameSet> =
        std::sync::OnceLock::new();
    SET.get_or_init(|| crate::compiled::dispatch::WithinFrameSet::with_bits(22))
}

fn partitioned_filter_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| {
        std::env::var("CELESTE_PARTITIONED_FILTER").map_or(true, |v| v != "0")
    })
}

/// D4's filter stage: turn a batch of `Raw` fragments into `Filtered` ones
/// using per-thread seen sets that PERSIST ACROSS the frame's batches,
/// with keys hash-partitioned so each thread owns a disjoint slice of key
/// space and needs no coordination.
///
/// Byte-identical to the classic path by this argument: a candidate whose
/// key `insert_new` would reject can be added or removed freely without
/// changing survivors or ids, and the two paths' candidate lists differ
/// ONLY in such entries. The classic path offers first-in-fragment
/// non-historic occurrences (later fragments re-offer, `insert_new`
/// rejects); this path offers first-in-frame non-historic occurrences,
/// scanned in the same fragment-then-lane order the serial phase uses. The
/// first occurrence of each genuinely-new key is a candidate in both, so
/// `insert_new` sees it at the same point in the same order - same
/// survivor set, same ids, byte-identical sidecars (gated at H=68).
fn partition_filter(
    prepared: &mut [PreparedRows],
    seen: &mut [rustc_hash::FxHashSet<(u64, u64)>],
    visited: &crate::interpreter::visited::Visited,
) {
    let frags: Vec<Option<&Vec<(u64, u64)>>> = prepared
        .iter()
        .map(|p| match &p.keys {
            PreparedKeys::Raw(Some(k)) => Some(k),
            _ => None,
        })
        .collect();
    let threads = seen.len();
    let census = crate::interpreter::vectorize::dedup_census_on();
    // out[thread][fragment] = this thread's candidates for that fragment,
    // in lane order (one thread scans lanes in order, so its own list is
    // sorted; the per-fragment merge below interleaves the threads').
    let per_thread: Vec<Vec<Vec<(u32, (u64, u64))>>> = std::thread::scope(|scope| {
        seen.iter_mut()
            .enumerate()
            .map(|(p, seen_p)| {
                let frags = &frags;
                scope.spawn(move || {
                    let mut out: Vec<Vec<(u32, (u64, u64))>> = vec![Vec::new(); frags.len()];
                    let mut probes = 0u64;
                    // Per-OCCURRENCE quadrant census (instrumentation only,
                    // does not change any decision): classify every offered
                    // occurrence on (frozen-frontier hit, within-frame dup).
                    // Index = (frontier as usize)*2 + (wf_dup as usize).
                    let quad = crate::interpreter::vectorize::dedup_quadrants_on();
                    let mut qb = [0u64; 4];
                    for (fi, frag) in frags.iter().enumerate() {
                        let Some(keys) = frag else { continue };
                        for (lane, &key) in keys.iter().enumerate() {
                            // Owner from the UPPER key bits: the low bits
                            // of key.0 index the fp-runs and the census
                            // sample, so they are the one place structure
                            // could hide.
                            if ((key.0 >> 32) as usize) % threads != p {
                                continue;
                            }
                            let is_new = seen_p.insert(key);
                            // `visited` is FROZEN mid-frame (run with
                            // CELESTE_FRONTIER_BUFFERED=1), so this is the
                            // frontier AS IT STOOD AT FRAME START. Probed
                            // once per key (on the first occurrence for the
                            // dedup, and here for every occurrence when the
                            // quadrant census is on).
                            let frontier =
                                if quad || is_new { visited.contains_historic(key) } else { false };
                            if quad {
                                qb[(frontier as usize) * 2 + (!is_new as usize)] += 1;
                            }
                            if is_new {
                                probes += 1;
                                if !frontier {
                                    out[fi].push((lane as u32, key));
                                }
                            }
                        }
                    }
                    if census {
                        crate::interpreter::vectorize::add_global_probes(probes);
                    }
                    if quad {
                        crate::interpreter::vectorize::add_quadrants(qb);
                    }
                    out
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|h| h.join().expect("partition filter worker"))
            .collect()
    });
    for (fi, p) in prepared.iter_mut().enumerate() {
        if !matches!(&p.keys, PreparedKeys::Raw(Some(_))) {
            if let PreparedKeys::Raw(None) = p.keys {
                p.keys = PreparedKeys::Filtered(None);
            }
            continue;
        }
        let mut candidates: Vec<(u32, (u64, u64))> = Vec::new();
        for t in &per_thread {
            candidates.extend_from_slice(&t[fi]);
        }
        candidates.sort_unstable_by_key(|(lane, _)| *lane);
        p.keys = PreparedKeys::Filtered(Some(
            crate::interpreter::vectorize::VisitedKeys::from_candidates(
                candidates,
                p.state.vector_size,
            ),
        ));
    }
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
        let keys = match keys {
            PreparedKeys::Filtered(keys) => keys,
            PreparedKeys::Raw(_) => {
                unreachable!("a Raw fragment reached the serial phase; partition_filter must run first")
            }
        };
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

/// Does `state` sit in the pm1 class the conditions describe? Every pattern
/// must resolve to at least one cell (same name rule as
/// `vectorize::resolve_partition_cells`: exact match or `.<pattern>` suffix)
/// and every resolved cell must be a scalar number equal to the required
/// value. Anything else - missing cell, non-number, per-lane vector - is
/// simply "not this class": the state takes the base path, which is always
/// sound. The guards inside a keyed overlay are the certifier; this
/// predicate only has to be pure and total.
fn pm1_matches(
    conditions: &[(String, Pico8Num)],
    names: &std::collections::HashMap<usize, String>,
    state: &State,
) -> bool {
    use crate::interpreter::heap::HeapId;
    conditions.iter().all(|(pattern, want)| {
        let suffix = format!(".{}", pattern);
        let mut found = false;
        for (&cell, name) in names.iter() {
            if name != pattern && !name.ends_with(&suffix) {
                continue;
            }
            found = true;
            match state.heap.get_opt(HeapId::from_raw(cell)) {
                Some(HeapValue::Value(Value::Number(MaybeVector::Scalar(n)))) if *n == *want => {}
                _ => return false,
            }
        }
        found
    })
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
    // The (shape, pm1) key: cell names are only resolved when some
    // shape-matching candidate actually has a pm1 condition.
    let mut cell_names: Option<std::collections::HashMap<usize, String>> = None;
    let Some(idx) = vd.variants.iter().position(|v| {
        v.shapes.iter().any(|s| *s == shape)
            && (v.pm1.is_empty() || {
                let names = cell_names
                    .get_or_insert_with(|| crate::interpreter::merge_dump::cell_names(&state));
                pm1_matches(&v.pm1, names, &state)
            })
    }) else {
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

/// The compiled frame engine (`compiled::FrameEngine`) wired into the
/// campaign's frame body - P1 stage 3.
///
/// Opt-in, because it is not a drop-in: it executes a DIFFERENT program
/// (`rewrites-compile.jsonl`, the compile-only overlay on top of the
/// campaign recipe) and returns a different partition of the same rows, so
/// a compiled run's checkpoints and `g.bin` are isomorphic to an
/// interpreted run's rather than byte-identical with them. What is claimed,
/// and what `check` mode verifies chunk by chunk, is that the frame's
/// output row SET is the same.
///
/// * `CELESTE_COMPILED_FORWARD=1` - use it.
/// * `CELESTE_COMPILED_FORWARD=check` - use it AND run the interpreter on
///   every chunk as well, comparing canonical row-key sets and aborting on
///   the first difference. This is the gate; it is roughly 2x the cost of
///   the interpreted run, so it is for gating and not for campaigns.
pub struct CompiledForward {
    engine: crate::compiled::FrameEngine,
    check: bool,
}

/// A `check`-mode row-set mismatch, as its own error type so the optimistic
/// deopt arm can tell it from a premise failure. Without the distinction a
/// mismatch on a deopt-eligible chunk is CAUGHT by the arm's catch, logged
/// as a deopt, and the chunk silently re-run by the interpreter - the gate
/// reports success while the thing it gates just failed. That is exactly
/// the frame range (f58+) where the 24-row divergence lives, so the gate
/// was blind precisely where it was needed.
#[derive(Debug)]
struct CheckMismatch(String);

impl std::fmt::Display for CheckMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CheckMismatch {}

impl CompiledForward {
    /// One chunk's frame body. `Ok` carries the compiled engine's output
    /// states, which are NOT the interpreter's states - see the type doc.
    fn run_chunk(
        &self,
        frame_cfg: &crate::interpreter::fixed_env::PreparedCfg,
        fixed_env: &crate::interpreter::fixed_env::FixedEnv,
        state: State,
    ) -> Result<Vec<State>> {
        if !self.check {
            // Carry the engine keys (b.row_keys) out-of-band so the frontier is
            // ENGINE-keyed (Option 1) without threading them through every
            // interpret/variant/deopt path: the worker reads them right after
            // this returns, on the same thread. See `take_carried_keys`.
            let keyed = self.engine.run_frame_chunk(&state, Some((frame_cfg, fixed_env)));
            let (states, keys): (Vec<State>, Vec<Option<Vec<(u64, u64)>>>) =
                keyed.into_iter().unzip();
            set_carried_keys(keys);
            return Ok(states);
        }
        let reference: Vec<State> = interpret_prepared_cfg(frame_cfg, state.clone(), fixed_env)
            .context("frame failed (compiled-forward check: reference side)")?
            .into_iter()
            .map(|(s, _)| s)
            .collect();
        let got = self.engine.run_frame_chunk(&state, Some((frame_cfg, fixed_env)));
        let got_states: Vec<State> = got.iter().map(|(s, _)| s.clone()).collect();
        // The comparator must compare AT THE CONFIGURED RUNG's
        // abstraction. `FrameEngine::row_key_set` funnels through
        // `Rt2::boundary`, which widens at Bits(0) - fine (and cheap)
        // when the rung IS Bits(0), but at a finer rung two
        // rung-distinct row sets can coarsen equal, so the level-0 keys
        // are too weak a claim there. The rung comparator abstracts both
        // sides with the campaign's own `split_precision_straddles` +
        // `make_state_abstract` and compares `sweep::row_keys` sets -
        // sound to compare across the two engines' different lane
        // groupings because every step is a per-lane function of the
        // lane's values and its state's (identical) heap structure.
        let level0 =
            crate::interpreter::abstraction::rem_precision_from_env()
                == crate::interpreter::abstraction::RemPrecision::Bits(0);
        let (want_keys, got_keys) = if level0 {
            (self.engine.row_key_set(&reference), self.engine.row_key_set(&got_states))
        } else {
            (
                rung_row_key_set(&reference)
                    .context("rung-abstracting the reference side of the check")?,
                rung_row_key_set(&got_states)
                    .context("rung-abstracting the compiled side of the check")?,
            )
        };
        let missing: Vec<_> = want_keys.difference(&got_keys).collect();
        let extra: Vec<_> = got_keys.difference(&want_keys).collect();
        if !missing.is_empty() || !extra.is_empty() {
            // The keys themselves, because "24 rows differ" was exactly the
            // level of detail that left the divergence unexplained for a
            // day. A handful is enough to grep for in a rowkeys sidecar.
            let fmt_keys = |keys: &[&(u64, u64)]| {
                keys.iter()
                    .take(8)
                    .map(|(lo, hi)| format!("{:016x}{:016x}", lo, hi))
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            return Err(anyhow::Error::new(CheckMismatch(format!(
                "compiled-forward check FAILED on a {}-lane chunk: {} rows the \
                 interpreter produced are missing from the compiled output \
                 [{}], {} rows are extra [{}] (interpreter {} rows, compiled \
                 {} rows)",
                state.vector_size,
                missing.len(),
                fmt_keys(&missing),
                extra.len(),
                fmt_keys(&extra),
                want_keys.len(),
                got_keys.len(),
            ))));
        }
        // Carry the REFERENCE states forward, not the engine's. The two
        // partitions carry the same row set (just verified), but the
        // trajectories they induce differ: the engine's regrouping can
        // form states the interpreter's own flow never would, and on
        // room (2,0) f40 the reference side of the NEXT frame ground for
        // hours in split_by_condition on exactly such states. Check mode
        // gates "the engine reproduces the interpreter's transition on
        // the interpreter's own frontier", frame by frame - so the
        // frontier stays the interpreter's, and an engine-side partition
        // pathology cannot compound across frames or contaminate the
        // reference it is being judged against.
        drop(got);
        // Check mode carries the interpreter's reference states forward (see
        // above); they take the interpreter key path (None carried) downstream.
        set_carried_keys(vec![None; reference.len()]);
        Ok(reference)
    }
}

/// The canonical row-key set of some raw frame-output states at the
/// CONFIGURED ladder rung: the campaign's own boundary abstraction
/// (`split_precision_straddles` + `make_state_abstract`), then
/// `sweep::row_keys` per surviving state. This is check mode's comparator
/// at every rung above Bits(0) - see the call site.
fn rung_row_key_set(
    states: &[State],
) -> Result<rustc_hash::FxHashSet<(u64, u64)>> {
    let mut keys: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
    for s in states {
        if s.vector_size == 0 {
            continue;
        }
        for st in crate::interpreter::abstraction::split_precision_straddles(s.clone()) {
            let mut st = make_state_abstract(st);
            st.gc();
            keys.extend(super::sweep::row_keys(&st)?);
        }
    }
    Ok(keys)
}

/// The process's compiled forward engine, if this run opted into one.
///
/// Built once - loading and replaying `rewrites-compile.jsonl` compiles the
/// whole cart, which is seconds. `program` is the CAMPAIGN's program, passed
/// only so the two can be checked against each other; the engine runs the
/// compile recipe's.
fn compiled_forward(program: &Program) -> Result<Option<&'static CompiledForward>> {
    /// The recipe the kernels and the name tables were generated from. The
    /// engine MUST execute this program and not the campaign's - see
    /// `FrameEngine::new`, which carries the story of the afternoon that
    /// cost.
    const COMPILE_RECIPE: &str = "rewrites-compile.jsonl";

    let mode = match std::env::var("CELESTE_COMPILED_FORWARD") {
        Err(_) => return Ok(None),
        Ok(v) if v == "0" => return Ok(None),
        Ok(v) => v,
    };
    let check = match mode.as_str() {
        "1" => false,
        "check" => true,
        other => anyhow::bail!(
            "CELESTE_COMPILED_FORWARD={:?}: expected 1, check or 0",
            other
        ),
    };

    // What the compiled path cannot serve, refused up front rather than
    // silently mis-abstracted (plans/kernel-ladder.md). The LEVEL-0 set
    // bakes the Bits(0) widenings into the graph and its boundary
    // re-applies them, so any other rung through it would be a DIFFERENT
    // (coarser) abstraction wearing the rung's name - the exact failure
    // mode the "never widen a field without a rung that narrows it back"
    // rule exists to prevent. The RUNG-AGNOSTIC set hands back exact rows
    // and the campaign boundary applies the rung, so it serves every rem
    // rung whose blocks carry rem as an interval - Bits(0..=15) - and the
    // EXACT set serves the top rung, whose blocks carry rem as a plain
    // number. The refusals left are the cross-matches an explicit
    // CELESTE_TRACED_SET override can produce, and the spd rungs.
    use crate::compiled::dispatch::{traced_mode, TracedMode};
    use crate::interpreter::abstraction::{RemPrecision, SpdPrecision};
    let rem = crate::interpreter::abstraction::rem_precision_from_env();
    match (traced_mode(), rem) {
        (TracedMode::Level0, RemPrecision::Bits(0)) => {}
        (TracedMode::Level0, other) => anyhow::bail!(
            "CELESTE_TRACED_SET=traced with rem precision {:?}: the level-0 \
             set implements Bits(0) only; unset CELESTE_TRACED_SET to let \
             the rung pick the set",
            other
        ),
        (TracedMode::Level0Agnostic, RemPrecision::Bits(_)) => {}
        (TracedMode::Level0Agnostic, RemPrecision::Exact) => anyhow::bail!(
            "CELESTE_TRACED_SET=ladder with exact rem: an exact block \
             carries rem as a plain number, which the rung-agnostic set's \
             interval slot refuses at bind; unset CELESTE_TRACED_SET to \
             let the rung pick the exact set"
        ),
        (TracedMode::ExactRem, RemPrecision::Exact) => {}
        (TracedMode::ExactRem, other) => anyhow::bail!(
            "CELESTE_TRACED_SET=exact with rem precision {:?}: that rung's \
             blocks carry rem as an interval, which the exact set's number \
             slot refuses at bind; unset CELESTE_TRACED_SET to let the \
             rung pick the set",
            other
        ),
    }
    let spd = crate::interpreter::abstraction::spd_precision_from_env();
    if spd != SpdPrecision::Exact {
        anyhow::bail!(
            "CELESTE_COMPILED_FORWARD with spd precision {:?}: the compiled \
             boundary does not implement the spd rung",
            spd
        );
    }

    static ENGINE: std::sync::OnceLock<CompiledForward> = std::sync::OnceLock::new();
    if ENGINE.get().is_none() {
        // The RECIPE is still parsed - `StateMapping::from_recipe` reads
        // the instruction list as DATA - but it is no longer replayed:
        // the program itself comes from the frozen artifact next to it.
        let recipe = crate::program::recipe::Recipe::load(COMPILE_RECIPE)
            .with_context(|| format!("loading {} (run from the repo root)", COMPILE_RECIPE))?;
        let compile_program = crate::program::frozen::rewritten(COMPILE_RECIPE)
            .with_context(|| format!("loading the frozen {}", COMPILE_RECIPE))?;
        // The pm1 partition cells are a PROCESS GLOBAL that
        // `AbstractRun::start` has already set from the campaign's program,
        // and `FrameEngine::new` is about to set from the compile one. If
        // they disagree the second write silently re-merges the campaign's
        // states by a different key. They agree today because the compile
        // recipe is the campaign recipe plus a compile-only overlay; hold
        // it rather than trust it.
        anyhow::ensure!(
            compile_program.merge_partition_cells == program.merge_partition_cells,
            "{} and the campaign recipe disagree on the merge partition cells \
             ({:?} vs {:?}); the compiled engine would re-key the campaign's merges",
            COMPILE_RECIPE,
            compile_program.merge_partition_cells,
            program.merge_partition_cells,
        );
        let mut engine = crate::compiled::FrameEngine::new_for_start_room(&compile_program)?;
        // The plain-program path for the kernels' deopt sub-chunks (dying
        // representatives, class-leaving rows). The mapping must be the
        // COMPILE recipe's - the sub-chunks are in the layout the compile
        // program produced. Without this, one dying representative fails
        // the specialized fallback, the whole compiled attempt is
        // discarded, and the outer deopt arm re-runs the entire state
        // (plans/shape-tag-plan.md, "Phase C scoping measurement").
        let plain_program = Program::compile_from_disk()
            .context("compiling the plain program for the engine's deopt path")?;
        engine.set_plain_path(crate::compiled::PlainPath {
            plain_cfg: crate::interpreter::fixed_env::PreparedCfg::new(
                plain_program.frame_cfg().clone(),
            ),
            plain_env: plain_program.fixed_env(),
            mapping: super::state_mapping::StateMapping::from_recipe(&recipe),
        });
        let _ = ENGINE.set(CompiledForward { engine, check });
    }
    let engine = ENGINE.get().expect("just initialized");
    anyhow::ensure!(
        engine.check == check,
        "CELESTE_COMPILED_FORWARD changed mid-process"
    );
    println!(
        "compiled forward engine ENABLED ({}), program {}",
        if check { "check mode: both paths, row-key sets compared" } else { "compiled only" },
        COMPILE_RECIPE
    );
    Ok(Some(engine))
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
            compiled: compiled_forward(program)?,
            skip_merge: false,
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

    /// Leave the frame's output as fragments: abstract and GC them, but do
    /// not group and merge them by shape.
    ///
    /// For callers that read something off the boundary rows and then throw
    /// the states away - the backward sweep, which reads `(origin, row
    /// key)` pairs per lane and discards everything else. Merging costs
    /// nearly half of that replay (`fwd.merge` 22.6 s of `bwdt.replay`'s
    /// 48.3 s at H=68) and buys the sweep nothing: its per-lane origin
    /// column makes every row distinct, so the dedup inside the merge finds
    /// almost nothing, and the k-way concatenation it does instead is
    /// immediately discarded.
    ///
    /// NOT for a searching run. The merge is what keeps the frontier from
    /// fragmenting without bound across frames, and the dedup inside it is
    /// part of the abstraction. This only makes sense where the states do
    /// not survive the frame.
    ///
    /// One VISIBLE difference: without the merge's dedup, two output lanes
    /// that are identical rows are read twice instead of once. Every
    /// consumer of the pairs must therefore be idempotent per (origin, key)
    /// - the sweep's `newly` bitset is - and pure COUNTS over the pairs
    /// change. `out_of_table` is the one that does; it is a diagnostic.
    pub fn skip_boundary_merge(&mut self) {
        self.skip_merge = true;
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
    /// (`search::pos_graph`). Read-only: it does not change what any frame
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
        // Close the offered-key dump if this frame opened one (D2's input
        // data; see `offered_dump_begin_frame`). After step_inner so every
        // return path - serial, parallel, phased - is covered.
        crate::interpreter::vectorize::offered_dump_end_frame();
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
        crate::interpreter::vectorize::offered_dump_begin_frame(frame_no);
        let input_states = chunk_states(std::mem::take(&mut self.states));
        // Chunk-parallel path. Everything else keeps the serial loop below -
        // one code path per arrangement, and the parallel one is opt-in.
        // The engine-keyed frontier (Option 1) and the within-frame skip
        // (Option 4) are wired into step_parallel, NOT the serial loop below.
        // Routing the streaming engine-keyed case through step_parallel even at
        // one thread keeps the two thread counts in the SAME key space, so a
        // serial run is byte-identical to a parallel one (parcheck). Without
        // this, threads==1 falls to the interpreter-key path below and diverges.
        if frame_threads() > 1 || (stream && engine_keyed_frontier()) {
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
                    self.compiled,
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
        let compiled = self.compiled;

        let partitioned = partitioned_filter_on();
        // One seen set per thread, disjoint by key hash, alive for the
        // WHOLE frame - this scope is the entire point (D2: fragment- and
        // worker-scoped sets were measured 8.5x and 1.46x weaker).
        let mut partition_seen: Vec<rustc_hash::FxHashSet<(u64, u64)>> =
            (0..threads).map(|_| Default::default()).collect();
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
            // The partitioned filter (D4): workers only HASH their
            // fragments; the seen+probe filter runs after the join,
            // hash-partitioned across threads with sets that persist for
            // the whole frame. CELESTE_PARTITIONED_FILTER=0 restores the
            // classic in-worker filter (identical results either way; the
            // classic path re-probes every new key once per fragment it
            // appears in, ~8x - see BENCHMARK_DATA.md "Dedup roofline").
            type Prepared = Result<(Vec<PreparedRows>, FrameEventCounters, StreamCounters)>;
            let results: Vec<Prepared> = {
                let _t = ScopedPhase::new("fwd.interpret");
                let visited_ro: &crate::interpreter::visited::Visited = self
                    .visited_rows
                    .as_ref()
                    .expect("stream implies a visited table");
                // Option 4: point the frame's chunks at a SHARED within-frame
                // set so a successor a sibling chunk already emitted is skipped
                // before materialization. Frozen frontier required (same reason
                // as Option 1); races are sound (see `WithinFrameSet`).
                let wf_active = within_frame_skip_on() && visited_ro.is_frozen();
                if wf_active {
                    let set = within_frame_set();
                    set.clear();
                    crate::compiled::dispatch::set_within_frame(set);
                }
                let scope_out = std::thread::scope(|scope| {
                    let handles: Vec<_> = batch
                        .drain(..)
                        .map(|state| {
                            scope.spawn(move || -> Prepared {
                                // This thread IS the parallelism; the row
                                // hashing inside must not fan out again.
                                crate::interpreter::virtual_merge::set_nested_parallel(true);
                                let mut ev = FrameEventCounters::default();
                                let mut sc = StreamCounters::default();
                                let t0 = std::time::Instant::now();
                                // Option 1 (CELESTE_FRONTIER_SKIP): point this
                                // thread's kernels at the FROZEN frontier so they
                                // skip materializing rows already in it. Requires a
                                // frozen frontier (buffered/mmap); else a mid-frame
                                // probe would be timing-dependent, so we do not set
                                // it (no skip, still sound and byte-identical).
                                let _fg = (engine_keyed_frontier() && visited_ro.is_frozen())
                                    .then(|| crate::compiled::dispatch::with_frozen_frontier(visited_ro));
                                let outputs = interpret_state_base(
                                    variants, deopt, frame_cfg, fixed_env, state, &mut ev,
                                    pos_obs, compiled,
                                )?;
                                drop(_fg);
                                // Engine keys the compiled body carried for these
                                // outputs (Option 1). Empty on the interpreter path.
                                let carried = take_carried_keys();
                                let use_carried = engine_keyed_frontier() && !carried.is_empty();
                                let t1 = std::time::Instant::now();
                                let mut prepared = Vec::new();
                                for (oi, out) in outputs.into_iter().enumerate() {
                                    let ck = if use_carried {
                                        carried.get(oi).cloned().flatten()
                                    } else {
                                        None
                                    };
                                    prepared.extend(stream_boundary_prepare(
                                        out,
                                        band,
                                        frame_no,
                                        &mut sc,
                                        visited_ro,
                                        !partitioned,
                                        ck,
                                    )?);
                                }
                                add_worker_ns(WORKER_BODY, (t1 - t0).as_nanos() as u64);
                                add_worker_ns(
                                    WORKER_PREPARE,
                                    t1.elapsed().as_nanos() as u64,
                                );
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
                });
                if wf_active {
                    crate::compiled::dispatch::clear_within_frame();
                }
                scope_out
            };
            // Collect the batch's fragments in input order first: the
            // partition filter wants the whole batch (its threads scan
            // fragments in this order, which is what makes its candidate
            // order the serial order), and the serial phase then walks the
            // same list.
            let mut all_prepared: Vec<PreparedRows> = Vec::new();
            for result in results {
                let (prepared, ev, sc) = result?;
                counters.absorb(&ev);
                stream_counters.absorb(&sc);
                all_prepared.extend(prepared);
            }
            if partitioned {
                let _t = ScopedPhase::new("fwd.partition_filter");
                let visited_ro: &crate::interpreter::visited::Visited = self
                    .visited_rows
                    .as_ref()
                    .expect("stream implies a visited table");
                partition_filter(&mut all_prepared, &mut partition_seen, visited_ro);
            }
            let _t = ScopedPhase::new("fwd.boundary_stream");
            let visited = self
                .visited_rows
                .as_mut()
                .expect("stream implies a visited table");
            let mut decided = decided_survivors(all_prepared, visited, &mut stream_counters);
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
        let compiled = self.compiled;

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
                                    pos_obs, compiled,
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
/// Thread-time inside `fwd.interpret`'s workers, split between the FRAME
/// BODY and the boundary PREPARE (canonicalize, row keys, visited probe).
///
/// `fwd.interpret` is a wall-clock slice of the main thread that wraps the
/// whole worker scope, so it silently bundles the two - which is how the
/// campaign cost breakdown came to read as "79% interpreter" when a large
/// part of it is row keying. These are summed over workers, so they are
/// thread-seconds and do not compare directly to the wall figure; the
/// RATIO between them is the point.
const WORKER_BODY: usize = 0;
const WORKER_PREPARE: usize = 1;
const WORKER_ABSTRACT: usize = 2;
const WORKER_GC: usize = 3;
const WORKER_KEYS: usize = 4;
/// The deopt path's input snapshot - a full `State` clone taken on every
/// chunk so a failing frame can be re-run under the plain program.
const WORKER_SNAPSHOT: usize = 5;
/// Time spent re-running lanes under the PLAIN (unrewritten) program
/// because a specialization premise fired.
const WORKER_DEOPT: usize = 6;
/// The origin-tagged run of the SPECIALIZED program inside the deopt
/// machinery. It is the frame itself under collect-first, and a SECOND
/// frame under the optimistic path.
const WORKER_TAGGED: usize = 7;
static WORKER_NS: [std::sync::atomic::AtomicU64; 8] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

fn add_worker_ns(which: usize, ns: u64) {
    WORKER_NS[which].fetch_add(ns, std::sync::atomic::Ordering::Relaxed);
}

/// Print and reset the worker split. No-op if no streaming frame ran.
pub fn print_worker_phase_times() {
    let ns: Vec<u64> = WORKER_NS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    let total = (ns[WORKER_BODY] + ns[WORKER_PREPARE]) as f64;
    if total == 0.0 {
        return;
    }
    println!("inside fwd.interpret (thread-seconds, summed over workers):");
    for (name, v) in [
        ("frame body", ns[WORKER_BODY]),
        ("  ...deopt input snapshot", ns[WORKER_SNAPSHOT]),
        ("  ...deopt origin-tagged specialized run", ns[WORKER_TAGGED]),
        ("  ...deopt re-run (plain program)", ns[WORKER_DEOPT]),
        ("boundary prepare", ns[WORKER_PREPARE]),
        ("  ...abstract", ns[WORKER_ABSTRACT]),
        ("  ...gc", ns[WORKER_GC]),
        ("  ...row keys + visited probe", ns[WORKER_KEYS]),
    ] {
        println!("  {:<30} {:8.2}s  {:5.1}%", name, v as f64 / 1e9, 100.0 * v as f64 / total);
    }
}

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
    compiled: Option<&'static CompiledForward>,
) -> Result<Vec<State>> {
    // What the compiled frame body cannot serve. Refused here rather than
    // at construction because it is set AFTER `start` and can be turned
    // on at any point in a run; a check that only ran once would miss
    // exactly the case it exists for.
    //
    // Not fundamental: variants need the compiled path to know the
    // per-shape program a variant selects. (Pos-graph recording used to
    // be refused here too; the engine carries the per-lane origin as
    // block metadata now - `Rt2::origin`, plans/kernel-ladder.md "the
    // passthrough column" - so tagged chunks bind their kernels like any
    // other.)
    if compiled.is_some() && variants.is_some() {
        anyhow::bail!("CELESTE_COMPILED_FORWARD does not support --variant dispatch");
    }
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
                // The compiled engine substitutes for exactly THIS call -
                // the plain frame body of one chunk, no deopt, no variant
                // dispatch. Everything around it (the boundary, the
                // frontier subtract, the band, the row table) stays the
                // campaign's; see `FrameEngine::run_frame_chunk`.
                let result = match compiled {
                    Some(c) => c.run_chunk(frame_cfg, fixed_env, state)?,
                    None => interpret_prepared_cfg(frame_cfg, state, fixed_env)
                        .context("frame failed")?
                        .into_iter()
                        .map(|(s, _)| s)
                        .collect(),
                };
                new_states.extend(result);
            }
            Some(deopt) if deopt.force => {
                counters.deopt.0 += 1;
                counters.deopt.1 += state.vector_size;
                new_states.extend(run_deopt_frame(deopt, state)?);
            }
            Some(deopt) if deopt.collect_first && compiled.is_none() => {
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
                //
                // The compiled engine takes this arm even when the run
                // asked for COLLECT-FIRST, because collect-first's whole
                // mechanism is an origin column injected into the state,
                // and a per-lane-distinct column changes the shape hash so
                // that no kernel binds. Collect-first is a bet that
                // failures are common enough for the retry to cost more
                // than the column; on a compiled run the column costs the
                // entire compiled path, so the bet is off. Where the
                // premises never fire - room (1,0) since task #96 - the
                // two are the same work anyway.
                // Timed because it is not small: this clone happens on
                // EVERY chunk to serve a deopt that fires on <1% of lanes.
                let t_snap = std::time::Instant::now();
                let snapshot = state.clone();
                add_worker_ns(WORKER_SNAPSHOT, t_snap.elapsed().as_nanos() as u64);
                let attempt = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                    match compiled {
                        Some(c) => c.run_chunk(frame_cfg, fixed_env, state),
                        None => interpret_prepared_cfg(frame_cfg, state, fixed_env)
                            .map(|r| r.into_iter().map(|(s, _)| s).collect()),
                    }
                }));
                match attempt {
                    Ok(Ok(result)) => new_states.extend(result),
                    Ok(Err(err)) => {
                        // A check-mode row-set mismatch is the GATE failing,
                        // not a premise failing - propagate it instead of
                        // deopting over it. See `CheckMismatch`.
                        if err.downcast_ref::<CheckMismatch>().is_some() {
                            return Err(err);
                        }
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
        // The kill ratio, split: how much of it is against rows this same
        // frame produced, and how much against history? See
        // `vectorize::dedup_census_on`.
        if let Some(distinct) = crate::interpreter::vectorize::dedup_census_take() {
            let offered = stream_counters.sub_before as u64;
            let kept = stream_counters.sub_after as u64;
            let probes = crate::interpreter::vectorize::global_probes_take();
            println!(
                "  dedup census: offered {} -> ~{} distinct in-frame -> {} new  \
                 (within-frame {:.1}:1, cross-frame {:.1}:1, total {:.1}:1); \
                 {} global probes = {:.1}x the distinct count",
                offered,
                distinct,
                kept,
                offered as f64 / distinct.max(1) as f64,
                distinct as f64 / kept.max(1) as f64,
                offered as f64 / kept.max(1) as f64,
                probes,
                probes as f64 / distinct.max(1) as f64,
            );
            // The hash/probe split of visited_row_keys, per offered row -
            // worker CPU ns, so the two halves sum to roughly the phase
            // table's WORKER_KEYS row divided by offered lanes.
            let (hash_ns, probe_ns) = crate::interpreter::vectorize::keys_phase_ns_take();
            println!(
                "  keys split: hash {:.1} ns/row, filter+probe {:.1} ns/row \
                 (worker CPU ns over {} offered)",
                hash_ns as f64 / offered.max(1) as f64,
                probe_ns as f64 / offered.max(1) as f64,
                offered,
            );
        }
        // Per-OCCURRENCE quadrant census (CELESTE_DEDUP_QUADRANTS=1): what
        // fraction of duplicate occurrences does Option 1 (a frozen-frontier
        // check before materialize) actually cover, counting the popular
        // cross-frame rows' within-frame REPEATS as frontier hits (which the
        // sequential cascade miscredits to "within-frame")?
        if let Some(q) = crate::interpreter::vectorize::quadrants_take() {
            // index = (frontier as usize)*2 + (wf_dup as usize)
            let (nn, ny, yn, yy) = (q[0], q[1], q[2], q[3]);
            let total = nn + ny + yn + yy;
            let dups = total.saturating_sub(nn); // occurrences that are not genuinely-new-distinct
            let opt1 = yn + yy; // frozen-frontier hits = Option 1 coverage
            let pct = |a: u64, b: u64| 100.0 * a as f64 / b.max(1) as f64;
            println!(
                "  dedup quadrants (occurrences): (frontier N, wf N)={} new, \
                 (frontier N, wf Y)={} within-frame-only, \
                 (frontier Y, wf N)={} frontier-1st, \
                 (frontier Y, wf Y)={} frontier-repeat; total offered {}",
                nn, ny, yn, yy, total
            );
            println!(
                "  Option 1 (frozen-frontier check) covers {} occurrences = {:.1}% of duplicates, \
                 {:.1}% of offered; within-frame-only (needs Option 4) {} = {:.1}% of duplicates; \
                 genuinely new {}",
                opt1, pct(opt1, dups), pct(opt1, total), ny, pct(ny, dups), nn
            );
        }
        if engine_keyed_frontier() {
            let probes = crate::compiled::dispatch::FRONTIER_PROBES.swap(0, std::sync::atomic::Ordering::Relaxed);
            let hits = crate::compiled::dispatch::FRONTIER_HITS.swap(0, std::sync::atomic::Ordering::Relaxed);
            let none = crate::compiled::dispatch::FRONTIER_NONE.swap(0, std::sync::atomic::Ordering::Relaxed);
            println!("  frontier skip: {} probes ({} thread-local-unset), {} hits", probes, none, hits);
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
            if self.skip_merge {
                crate::interpreter::vectorize::gc_states(new_states)
            } else {
                vectorize_states(new_states)
            }
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
            for state in &mut self.states {
                state.gc();
            }
        }
    }

    pub fn states(&self) -> &[State] {
        &self.states
    }

    /// Take the frame's output states, leaving the run empty.
    ///
    /// For a caller that consumes the output and then resets the run
    /// anyway - the backward sweep, which reads `(origin, row key)` pairs
    /// and calls `restore` with an empty frontier. Borrowing forced it to
    /// CLONE every state to strip the origin column, and at H=68 that
    /// clone was most of `bwdt.keys`.
    pub fn take_states(&mut self) -> Vec<State> {
        std::mem::take(&mut self.states)
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

/// `run_deopt_frame` with its thread-time attributed to `WORKER_DEOPT`.
/// The plain program is the UNREWRITTEN one, so a lane that falls back
/// costs far more than a lane that does not - and until this was timed,
/// that cost sat inside "frame body" and looked like interpretation.
fn timed_deopt_frame(deopt: &DeoptTarget, state: State) -> Result<Vec<State>> {
    let t = std::time::Instant::now();
    let out = run_deopt_frame(deopt, state);
    add_worker_ns(WORKER_DEOPT, t.elapsed().as_nanos() as u64);
    out
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
    // Same clone as the optimistic path's, for the same reason, and timed
    // the same way - collect-first keeps the input around so a captured
    // lane can be re-run under the plain program.
    let t_snap = std::time::Instant::now();
    let mut tagged = snapshot.clone();
    add_worker_ns(WORKER_SNAPSHOT, t_snap.elapsed().as_nanos() as u64);
    inject_origin(&mut tagged);
    deopt_collect::begin();
    // The origin-tagged run of the SPECIALIZED program. On the
    // collect-first path this is the frame; on the optimistic path it is a
    // second frame after the first one failed, which is what collect-first
    // exists to avoid.
    let t_tagged = std::time::Instant::now();
    let attempt = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        interpret_prepared_cfg(frame_cfg, tagged, fixed_env)
    }));
    add_worker_ns(WORKER_TAGGED, t_tagged.elapsed().as_nanos() as u64);
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
            return Ok((timed_deopt_frame(deopt, snapshot)?, n));
        }
        Ok(Err(err)) => {
            let one_line = format!("{:#}", err).replace('\n', " | ");
            println!(
                "  deopt: retry failed for a non-premise reason ({}); whole-state fallback",
                one_line.chars().take(400).collect::<String>()
            );
            return Ok((timed_deopt_frame(deopt, snapshot)?, n));
        }
        Err(panic) => {
            println!(
                "  deopt: retry panicked ({}); whole-state fallback",
                panic_text(&panic).chars().take(400).collect::<String>()
            );
            return Ok((timed_deopt_frame(deopt, snapshot)?, n));
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
    out.extend(timed_deopt_frame(deopt, plain_input)?);
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
    let _started = std::time::Instant::now();
    let out = run_deopt_frame_inner(deopt, state);
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

