//! The tracer's BOUNDARY: which heap slots are a frame's INPUTS (in order),
//! and how to read the same slots back out afterwards. This numbering is the
//! tracer's own, not `celeste_names::FIELD_NAMES`.
//!
//! A slot is named by its PATH from the globals (`objects[0].spd.x`), not a
//! table id: a frame can replace an object (death allocates a new player),
//! and a path that no longer resolves is a shape difference.

use std::collections::BTreeSet;

use anyhow::{anyhow, bail, Result};

use crate::pico8_num::Pico8Num as P8;
use crate::transpile::graph::{NodeId, Op};

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::state::State;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Step {
    Key(String),
    /// A slot of the ARRAY part, zero-based.
    Idx(usize),
    /// An integer key outside the array part (`Table::ints`, not measured by
    /// `#`), by its Lua key.
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

/// A scalar outside any domain: what a slot concretely holds.
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

/// Every scalar slot reachable from `root`, in an order that depends only on
/// the content (the hash part is a `BTreeMap`), not the allocation history.
///
/// A slot gets the FIRST path that reaches it (the heap is a graph: aliases
/// are found through `objects` first), so a path names a slot only RELATIVE
/// TO A SHAPE: comparing two states of the same shape by path is exact.
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
            // Cycles and aliases are real (`type` tables).
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
///
/// `pins`: `(cell index, value)` for inputs BAKED INTO the body. The cell is
/// still an input, so `pin_guard` can check that the state agrees.
pub struct Iface {
    pub slots: Vec<Path>,
    pub init: Vec<Conc>,
    pub pins: Vec<(usize, Conc)>,
    /// Parallel to `slots`: a number slot holds an INTERVAL (the widened
    /// fields; `init` still records a point), which the emitter makes an
    /// `ival` input. A boolean slot may be unknown in a lane (a near level's
    /// floor `collideable`), which the emitter makes a `ubool` input.
    pub ival: Vec<bool>,
}

/// Replace every scalar under `roots` with a fresh `Op::Cell` leaf, and
/// remember what it held.
///
/// Only under the roots, never the whole heap: the tracer's leverage is the
/// heap staying concrete (`btn(k_left)` indexes with a global). A slot
/// reached twice gets one cell; cells are numbered in root order.
///
/// `pin` holds slots at the CALLER's value instead of reading a cell, so the
/// trace folds through the constant. A pinned slot is still an input cell,
/// read only by `pin_guard`, which turns the assumption into a check.
pub fn symbolize(
    d: &mut Symbolic,
    st: &mut State<Symbolic>,
    roots: &[Path],
    pin: &[(Path, Conc)],
    ival: &[Path],
) -> Result<Iface> {
    let mut slots: Vec<Path> = Vec::new();
    for r in roots {
        for p in scalars(st, r)? {
            if !slots.contains(&p) {
                slots.push(p);
            }
        }
    }
    let mut pins: Vec<(usize, Conc)> = Vec::new();
    for (p, c) in pin {
        let i = slots
            .iter()
            .position(|q| q == p)
            .ok_or_else(|| anyhow!("pinned {} is not an input slot", show(p)))?;
        if pins.iter().any(|(j, _)| *j == i) {
            bail!("{} pinned twice", show(p));
        }
        pins.push((i, *c));
    }
    let mut init = Vec::new();
    for (i, p) in slots.iter().enumerate() {
        let pinned = pins.iter().find(|(j, _)| *j == i).map(|(_, c)| *c);
        let cell = d.graph.leaf(Op::Cell(i as u32));
        let held = match get(st, p).unwrap() {
            Value::Num(n) => Conc::Num(
                d.as_const(&n)
                    .ok_or_else(|| anyhow!("{} was already symbolic", show(p)))?,
            ),
            Value::Bool(b) => Conc::Bool(
                d.decide(&b)
                    .ok_or_else(|| anyhow!("{} was already symbolic", show(p)))?,
            ),
            other => bail!("{} is {:?}, not a scalar", show(p), other),
        };
        let (c, new) = match (pinned, held) {
            (Some(Conc::Num(v)), Conc::Num(_)) => {
                let n = d.num(v);
                (Conc::Num(v), Value::Num(n))
            }
            (Some(Conc::Bool(v)), Conc::Bool(_)) => {
                let b = d.boolean(v);
                (Conc::Bool(v), Value::Bool(b))
            }
            (Some(v), h) => bail!("{}: pinned {:?} but the slot holds {:?}", show(p), v, h),
            (None, Conc::Num(v)) => (Conc::Num(v), Value::Num(cell)),
            (None, Conc::Bool(v)) => (Conc::Bool(v), Value::Bool(cell)),
        };
        d.graph.set_cell_kind(
            i as u32,
            match (&c, ival.contains(p)) {
                (Conc::Bool(_), _) => crate::transpile::graph::CellKind::Bool,
                (_, true) => crate::transpile::graph::CellKind::Ival,
                (Conc::Num(_), _) => crate::transpile::graph::CellKind::Num,
            },
        );
        init.push(c);
        set(st, p, new)?;
    }
    let ival: Vec<bool> = slots.iter().map(|p| ival.contains(p)).collect();
    // Only NUMBER slots in `ival` are interval cells.
    d.ival_cells = ival
        .iter()
        .zip(&init)
        .enumerate()
        .filter(|(_, (b, c))| **b && matches!(c, Conc::Num(_)))
        .map(|(i, _)| i as u32)
        .collect();
    d.forget_intervals();
    Ok(Iface { slots, init, pins, ival })
}

/// The obligation a pinned body carries: every pinned cell holds the
/// value the body was compiled for.
///
/// Its negation is an ERROR of the frame, not a `guard`: a disagreeing lane
/// is real and must be declined loudly; a guard would silently drop it.
/// `ConstBool(true)` when nothing is pinned.
pub fn pin_guard(d: &mut Symbolic, iface: &Iface) -> NodeId {
    let mut acc = d.graph.leaf(Op::ConstBool(true));
    for (i, c) in &iface.pins {
        let cell = d.graph.leaf(Op::Cell(*i as u32));
        let t = match c {
            Conc::Num(v) => {
                let k = d.num(*v);
                d.graph.fold(Op::Eq, vec![cell, k])
            }
            Conc::Bool(true) => cell,
            Conc::Bool(false) => d.graph.fold(Op::Not, vec![cell]),
        };
        acc = d.graph.fold(Op::And, vec![acc, t]);
    }
    acc
}

/// Read every scalar under `root` as a CONCRETE value (the oracle side of the
/// differential check). A slot that stayed symbolic is an error, never skipped.
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
