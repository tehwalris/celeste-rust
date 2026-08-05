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
use crate::interpreter::inspect::make_state_abstract;
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

fn is_per_lane(value: &Value) -> bool {
    matches!(
        value,
        Value::Number(MaybeVector::Vector(_))
            | Value::NumberInterval(MaybeVector::Vector(_))
            | Value::Bool(MaybeVector::Vector(_))
    )
}

/// Canonical observation of a whole frame: the multiset of state observations.
pub fn observe_frame(states: &[State]) -> BTreeSet<StateObservation> {
    states.iter().map(observe_state).collect()
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
    /// Frontier-only search (CELESTE_FRONTIER_ONLY=1): persistent cross-frame
    /// visited set of canonical row hashes, keyed by shape hash. Experimental,
    /// hash-only; see `vectorize::subtract_visited` for the soundness note.
    visited_rows: Option<
        rustc_hash::FxHashMap<u64, rustc_hash::FxHashSet<u64>>,
    >,
    /// Use only the historic rem widening at boundaries (for the widen-check,
    /// which applies the conservative widenings post hoc instead).
    rem_only_abstraction: bool,
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
            println!("frontier-only search ENABLED (experimental, hash-only visited set)");
            Some(Default::default())
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
        })
    }

    /// Widen only rem at boundaries; used by `rewrite widencheck`.
    pub fn start_rem_only(program: &Program) -> Result<Self> {
        let mut run = Self::start(program)?;
        run.rem_only_abstraction = true;
        Ok(run)
    }

    pub fn step(&mut self) -> Result<()> {
        let mut new_states = Vec::new();
        for state in std::mem::take(&mut self.states) {
            let result = interpret_prepared_cfg(&self.frame_cfg, state, &self.fixed_env)
                .context("frame failed")?;
            new_states.extend(result.into_iter().map(|(s, _)| s));
        }
        let new_states: Vec<State> = if self.rem_only_abstraction {
            new_states
                .into_iter()
                .map(crate::interpreter::inspect::make_state_abstract_rem_only)
                .collect()
        } else {
            new_states.into_iter().map(make_state_abstract).collect()
        };
        let new_states: Vec<State> = if std::env::var_os("CELESTE_PRUNE_DEATHS").is_some() {
            let before = new_states.len();
            let kept: Vec<State> = new_states
                .into_iter()
                .filter(|s| !crate::interpreter::inspect::is_death_state(s))
                .collect();
            if kept.len() != before {
                println!("  (death pruning: dropped {} states)", before - kept.len());
            }
            kept
        } else {
            new_states
        };
        self.states_before_merge.push(new_states.len());
        self.states = {
            let _trace = crate::interpreter::tracing::TraceSpan::new(
                "merge_frame_boundary",
                "merge_site",
            );
            vectorize_states(new_states)
        };
        if let Some(visited) = self.visited_rows.as_mut() {
            let (kept, before, after) = crate::interpreter::vectorize::subtract_visited(
                std::mem::take(&mut self.states),
                visited,
            );
            let visited_total: usize = visited.values().map(|s| s.len()).sum();
            println!(
                "  frontier-only: {} -> {} new lanes, visited total {}",
                before, after, visited_total
            );
            self.states = kept;
        }
        Ok(())
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
