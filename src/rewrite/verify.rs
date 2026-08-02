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

use anyhow::{anyhow, Result};
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

/// Canonical observation of one state. GCs a copy first, so heap ids are
/// deterministic.
pub fn observe_state(state: &State) -> StateObservation {
    let mut state = state.clone();
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
}

impl AbstractRun {
    pub fn start(program: &Program) -> Result<Self> {
        let fixed_env = program.fixed_env();
        let initial = create_initial_state_with_builtins(&fixed_env);
        let init_states = interpret_cfg(program.init_cfg().clone(), initial, &fixed_env)
            .map_err(|e| anyhow!("init failed: {}", e))?;
        let mut states: Vec<State> = init_states.into_iter().map(|(s, _)| s).collect();
        for state in &mut states {
            inject_tile_flag_at_builtin(state);
        }
        let frame_cfg = crate::interpreter::fixed_env::PreparedCfg::new(
            program.frame_cfg().clone(),
        );
        Ok(Self { states, fixed_env, frame_cfg })
    }

    pub fn step(&mut self) -> Result<()> {
        let mut new_states = Vec::new();
        for state in std::mem::take(&mut self.states) {
            let result = interpret_prepared_cfg(&self.frame_cfg, state, &self.fixed_env)
                .map_err(|e| anyhow!("frame failed: {}", e))?;
            new_states.extend(result.into_iter().map(|(s, _)| s));
        }
        let new_states: Vec<State> = new_states.into_iter().map(make_state_abstract).collect();
        self.states = vectorize_states(new_states);
        Ok(())
    }

    pub fn states(&self) -> &[State] {
        &self.states
    }

    pub fn lane_count(&self) -> usize {
        self.states.iter().map(|s| s.vector_size).sum()
    }
}

pub struct Divergence {
    pub frame: u32,
    pub detail: String,
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
