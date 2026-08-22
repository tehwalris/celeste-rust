//! The tracer's BOUNDARY: which heap slots are a frame's INPUTS, and how
//! to read the same slots back out afterwards.
//!
//! Everything else in `trace` treats the heap as an implementation
//! detail. A kernel cannot: it is a function from a fixed list of input
//! cells to a fixed list of output cells, so somebody has to say WHICH
//! slots those are and in what order. That is this module, and it is
//! deliberately the tracer's own answer rather than the boundary's
//! (`celeste_names::FIELD_NAMES`) - lining the two numberings up is a
//! separate job, and doing it first would have meant debugging two
//! things at once.
//!
//! A slot is named by its PATH from the globals table (`objects[0].spd.x`)
//! rather than by a table id, because a frame can replace an object -
//! death allocates a new player - and the id would then name nothing. A
//! path either resolves in the output state or it does not, and "does not"
//! is a shape difference, which is a real answer.

use std::collections::BTreeSet;

use anyhow::{anyhow, bail, Result};

use crate::pico8_num::Pico8Num as P8;
use crate::transpile::graph::Op;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::state::State;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Step {
    Key(String),
    /// A slot of the ARRAY part, zero-based.
    Idx(usize),
    /// An integer key outside the array part (`Table::ints`), by its Lua
    /// key. Separate from `Idx` because they are different places: the
    /// array part is what `#` measures.
    Int(i16),
}

pub type Path = Vec<Step>;

pub fn show(p: &Path) -> String {
    let mut s = String::new();
    for step in p {
        match step {
            Step::Key(k) => {
                if !s.is_empty() {
                    s.push('.');
                }
                s.push_str(k);
            }
            Step::Idx(i) => s.push_str(&format!("[{}]", i)),
            Step::Int(i) => s.push_str(&format!("[#{}]", i)),
        }
    }
    if s.is_empty() {
        "_G".to_string()
    } else {
        s
    }
}

pub fn key(k: &str) -> Step {
    Step::Key(k.to_string())
}

/// A scalar outside any domain - what a slot concretely holds. The
/// differential check needs to talk about both sides' values in one
/// vocabulary, and neither domain's own type is that vocabulary.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Conc {
    Num(P8),
    Bool(bool),
}

pub fn get<D: Domain>(st: &State<D>, p: &[Step]) -> Option<Value<D>> {
    let mut cur = Value::Table(st.globals);
    for step in p {
        let Value::Table(t) = cur else { return None };
        let tab = st.heap.tables.get(&t)?;
        cur = match step {
            Step::Key(k) => tab.hash.get(k)?.clone(),
            Step::Idx(i) => tab.arr.get(*i)?.clone(),
            Step::Int(i) => tab.ints.get(i)?.clone(),
        };
    }
    Some(cur)
}

pub fn set<D: Domain>(st: &mut State<D>, p: &[Step], v: Value<D>) -> Result<()> {
    let (last, init) = p.split_last().ok_or_else(|| anyhow!("cannot assign _G"))?;
    let Some(Value::Table(t)) = get(st, init) else {
        bail!("{}: no such table", show(&init.to_vec()))
    };
    let tab = st.heap.tables.get_mut(&t).unwrap();
    match last {
        Step::Key(k) => {
            tab.hash.insert(k.clone(), v);
        }
        Step::Idx(i) => {
            *tab.arr
                .get_mut(*i)
                .ok_or_else(|| anyhow!("{}: index past the end", show(&p.to_vec())))? = v;
        }
        Step::Int(i) => {
            tab.ints.insert(*i, v);
        }
    }
    Ok(())
}

/// Every scalar slot reachable from `root`, in a deterministic order.
///
/// Deterministic because the hash part is a `BTreeMap` and the array part
/// is a `Vec`, so the order is a property of the CONTENT and not of the
/// allocation history - which is the same reason `state::canonical_order`
/// exists, and the same thing that makes two runs comparable.
///
/// A slot gets the FIRST path that reaches it, and the heap is a graph:
/// `spring.tile` is named `objects[2].type.tile` in a room that contains
/// a spring, because the object list is walked before the globals that
/// alias it. So a path names a slot only RELATIVE TO A SHAPE. Comparing
/// two states of the same shape by path is exact, which is what the
/// differential check does; comparing across shapes is not, and would
/// need a canonical name rather than a first-found one.
pub fn scalars<D: Domain>(st: &State<D>, root: &[Step]) -> Result<Vec<Path>> {
    let v = get(st, root).ok_or_else(|| anyhow!("{}: no such slot", show(&root.to_vec())))?;
    let mut out = Vec::new();
    let mut seen = BTreeSet::new();
    collect(st, root.to_vec(), &v, &mut seen, &mut out);
    Ok(out)
}

fn collect<D: Domain>(
    st: &State<D>,
    p: Path,
    v: &Value<D>,
    seen: &mut BTreeSet<u32>,
    out: &mut Vec<Path>,
) {
    match v {
        Value::Num(_) | Value::Bool(_) => out.push(p),
        Value::Table(t) => {
            // Cycles are real: every object has a `type` pointing at a
            // table that the object list also reaches.
            if !seen.insert(*t) {
                return;
            }
            let tab = &st.heap.tables[t];
            for (k, sub) in &tab.hash {
                let mut q = p.clone();
                q.push(Step::Key(k.clone()));
                collect(st, q, sub, seen, out);
            }
            for (i, sub) in tab.arr.iter().enumerate() {
                let mut q = p.clone();
                q.push(Step::Idx(i));
                collect(st, q, sub, seen, out);
            }
            for (i, sub) in &tab.ints {
                let mut q = p.clone();
                q.push(Step::Int(*i));
                collect(st, q, sub, seen, out);
            }
        }
        _ => {}
    }
}

/// The input cells of one traced frame: cell `i` lives at `slots[i]` and
/// held `init[i]` before it was replaced by a graph leaf.
pub struct Iface {
    pub slots: Vec<Path>,
    pub init: Vec<Conc>,
}

/// Replace every scalar under `roots` with a fresh `Op::Cell` leaf, and
/// remember what it used to be.
///
/// Under the given roots, never the whole heap: the tracer's leverage
/// comes from the heap staying concrete, and `btn(k_left)` indexes a
/// table with a global that would become a cell if this were applied
/// everywhere. Which slots are the right ones is a MEASUREMENT, not a
/// principle - a slot left concrete is a specialization, and the check
/// is valid either way because both sides specialize the same.
///
/// Overlapping roots are fine: a slot reached twice gets one cell, and
/// the cell numbering follows the order the roots are given in.
pub fn symbolize(d: &mut Symbolic, st: &mut State<Symbolic>, roots: &[Path]) -> Result<Iface> {
    let mut slots: Vec<Path> = Vec::new();
    for r in roots {
        for p in scalars(st, r)? {
            if !slots.contains(&p) {
                slots.push(p);
            }
        }
    }
    let mut init = Vec::new();
    for (i, p) in slots.iter().enumerate() {
        let cell = d.graph.leaf(Op::Cell(i as u32));
        let (c, new) = match get(st, p).unwrap() {
            Value::Num(n) => {
                let k = d
                    .as_const(&n)
                    .ok_or_else(|| anyhow!("{} was already symbolic", show(p)))?;
                (Conc::Num(k), Value::Num(cell))
            }
            Value::Bool(b) => {
                let k = d
                    .decide(&b)
                    .ok_or_else(|| anyhow!("{} was already symbolic", show(p)))?;
                (Conc::Bool(k), Value::Bool(cell))
            }
            other => bail!("{} is {:?}, not a scalar", show(p), other),
        };
        init.push(c);
        set(st, p, new)?;
    }
    Ok(Iface { slots, init })
}

/// Read every scalar under `root` as a CONCRETE value. This is what the
/// oracle side of the differential check produces, and it fails loudly
/// rather than skipping a slot that stayed symbolic - a frame run with
/// concrete inputs that leaves something symbolic behind is exactly the
/// bug this is looking for.
pub fn read_concrete(d: &Symbolic, st: &State<Symbolic>, root: &[Step]) -> Result<Vec<(Path, Conc)>> {
    let mut out = Vec::new();
    for p in scalars(st, root)? {
        let c = match get(st, &p).unwrap() {
            Value::Num(n) => Conc::Num(
                d.as_const(&n)
                    .ok_or_else(|| anyhow!("{} did not stay concrete", show(&p)))?,
            ),
            Value::Bool(b) => Conc::Bool(
                d.decide(&b)
                    .ok_or_else(|| anyhow!("{} did not stay concrete", show(&p)))?,
            ),
            _ => unreachable!("scalars only yields scalars"),
        };
        out.push((p, c));
    }
    Ok(out)
}
