//! THE CENSUS (plans/float-solver.md, section 12): how many states a forward
//! meets in one arithmetic, and how many distinct speeds, against the other
//! - the measurement that decides how much of the float state must be
//! abstracted (section 7).
//!
//! A REFERENCE-ENGINE forward over NODES: the level `r0sx`'s states (every
//! object exact, the player's remainder widened to the whole `[-1/2, 1/2)`),
//! from the room's start, under every input, deduplicated exactly across
//! frames, the wins not expanded - what the forward with every hull widened
//! (`hull::WIDEN`) holds. Generic over the arithmetic, so the two columns of
//! a comparison are one code. Its 16.16 counts are checked against that
//! forward's pinned ones (`gates/ckhash_room10_f000-044_widened.txt`, the
//! test below), which is what makes its node identity the forward's.
//!
//! A node's identity is its state read through the widening table at
//! `r0sx` (`widening::TABLE`'s `Always` entries): a slot the table stores
//! as a constant or a range (the remainder, the dead timers, the berry's
//! bob) is left out, `AtLeast(k)` is `max(v, k)`, a `FullPeriod` interval
//! is left out (a point kept); the buttons, written unknown at every
//! boundary, are left out too.
//!
//! An `exact` census keeps the remainder: it counts the CONCRETE states
//! (what the concrete search walks) and their distinct remainders. The two
//! differ in kind in float: widened to the whole pixel, a node's remainder
//! holds the few values next to `1/2` whose sum with the speed ROUNDS to the
//! next pixel's cut (`fl(pred(1/2) - 2) = -3/2`: one pixel moved, not two),
//! outcomes no fixed node has - an over-approximation the arcs undo, but one
//! the widened forward pays for.

use std::collections::BTreeMap;

use anyhow::{bail, ensure, Result};
use celeste_core::arith::Arithmetic;
use celeste_engine::widening::{Flag, Stored, Target, TABLE};
use rustc_hash::FxHashSet;

use crate::trace::heap::{TableId, Value};
use crate::trace::iface;
use crate::trace::refdomain::RefDomain;
use crate::trace::refengine::ReferenceEngine;
use crate::trace::state::State;

type St<A> = State<RefDomain<A>>;

/// The most fork paths one node's frame may take (its leaves are held).
const PATH_CAP: usize = 1 << 14;

/// One frame of a census.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Frame {
    pub frame: u32,
    /// Nodes first met at this frame (wins included, not expanded).
    pub new: usize,
    /// Of them, wins.
    pub wins: usize,
    /// Nodes met so far.
    pub total: usize,
    /// Distinct player `spd.x` / `spd.y` values among the frame's new
    /// nodes, and among every node so far.
    pub spd_x: usize,
    pub spd_y: usize,
    pub spd_x_all: usize,
    pub spd_y_all: usize,
    /// Distinct new nodes with the player's `spd.x` left out (the speed
    /// bundles of plans/speed-bundles.md).
    pub bundles: usize,
    /// Distinct player `rem.x` / `rem.y` values among every node so far
    /// (`exact` censuses; a widened one holds the one interval).
    pub rem_x_all: usize,
    pub rem_y_all: usize,
}

/// Every number and boolean of a table, by path, `type` and functions left
/// out (in the hash part's order: sorted).
fn scalars<A: Arithmetic>(st: &St<A>, t: TableId, prefix: &str, out: &mut BTreeMap<String, Val>, seen: &mut FxHashSet<TableId>) {
    if !seen.insert(t) {
        return;
    }
    let tab = &st.heap.tables[&t];
    for (k, v) in &tab.hash {
        if k == "type" {
            continue;
        }
        let path = if prefix.is_empty() { k.clone() } else { format!("{prefix}.{k}") };
        match v {
            Value::Table(s) => scalars(st, *s, &path, out, seen),
            Value::Num(iv) => {
                let (lo, hi) = A::bounds(*iv);
                out.insert(path, Val::Num(A::bits(lo), A::bits(hi)));
            }
            Value::Bool(b) => {
                out.insert(path, Val::Bool(*b));
            }
            _ => {}
        }
    }
    for (i, v) in tab.arr.iter().enumerate() {
        let path = format!("{prefix}[{i}]");
        match v {
            Value::Table(s) => scalars(st, *s, &path, out, seen),
            Value::Num(iv) => {
                let (lo, hi) = A::bounds(*iv);
                out.insert(path, Val::Num(A::bits(lo), A::bits(hi)));
            }
            Value::Bool(b) => {
                out.insert(path, Val::Bool(*b));
            }
            _ => {}
        }
    }
}

/// A scalar of a node: a number's `[lo, hi]` bits (a point: equal), a boolean.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Val {
    Num(u64, u64),
    Bool(bool),
}

/// The name of the global holding `t`.
fn type_name<A: Arithmetic>(st: &St<A>, t: TableId) -> Option<String> {
    st.heap.tables[&st.globals].hash.iter().find_map(|(k, v)| (*v == Value::Table(t) && k != "objects").then(|| k.clone()))
}

/// What the table does to a slot at `r0sx`: left out, or `max(v, k)`.
enum Rule {
    Out,
    AtLeast(i32),
    /// Left out where an interval (a point kept).
    FullPeriod,
}

fn rule(stored: Stored) -> Option<Rule> {
    match stored {
        Stored::Range { .. } | Stored::Band { .. } | Stored::Phase { .. } | Stored::UnknownNum | Stored::UnknownBool | Stored::Num(_) | Stored::Bool(_) | Stored::SameAs(_) => Some(Rule::Out),
        Stored::AtLeast(k) => Some(Rule::AtLeast(k)),
        Stored::FullPeriod(_) => Some(Rule::FullPeriod),
        Stored::AbsentAsZero => None,
    }
}

fn apply<A: Arithmetic>(fields: &mut BTreeMap<String, Val>, path: &str, r: &Rule) -> Result<()> {
    match (r, fields.get(path).copied()) {
        (_, None) => {}
        (Rule::Out, Some(_)) => {
            fields.remove(path);
        }
        (Rule::FullPeriod, Some(Val::Num(lo, hi))) if lo != hi => {
            fields.remove(path);
        }
        (Rule::FullPeriod, Some(_)) => {}
        (Rule::AtLeast(k), Some(Val::Num(lo, hi))) => {
            ensure!(lo == hi, "{path}: an interval where the table clamps a number");
            let k = A::from_fixed_raw(*k);
            let n = A::from_bits(lo).ok_or_else(|| anyhow::anyhow!("{path}: the bits {lo:#x} are no number"))?;
            let m = A::max(n, k);
            fields.insert(path.to_string(), Val::Num(A::bits(m), A::bits(m)));
        }
        (Rule::AtLeast(_), Some(Val::Bool(_))) => bail!("{path}: a boolean where the table clamps a number"),
    }
    Ok(())
}

/// A node's identity: its objects (type, fields) and its globals' scalars,
/// read through the widening table at `r0sx` (module doc). Exact: two
/// states share it iff the forward would store them as one state.
pub fn node_key<A: Arithmetic>(st: &St<A>, exact: bool) -> Result<Vec<u8>> {
    let globals = &st.heap.tables[&st.globals];
    let mut key: Vec<u8> = Vec::new();
    let put = |key: &mut Vec<u8>, fields: &BTreeMap<String, Val>| {
        for (k, v) in fields {
            key.extend_from_slice(k.as_bytes());
            key.push(0);
            match v {
                Val::Num(lo, hi) => {
                    key.push(1);
                    key.extend_from_slice(&lo.to_le_bytes());
                    key.extend_from_slice(&hi.to_le_bytes());
                }
                Val::Bool(b) => key.extend_from_slice(&[2, *b as u8]),
            }
        }
    };
    // The objects, in order.
    let Some(Value::Table(objects)) = globals.hash.get("objects") else { bail!("no `objects`") };
    let mut type_tables = FxHashSet::default();
    for o in st.heap.tables[objects].arr.iter() {
        let Value::Table(t) = o else {
            key.extend_from_slice(b"\xffnil\0");
            continue;
        };
        let Some(Value::Table(ty)) = st.heap.tables[t].hash.get("type") else { bail!("an object without a type") };
        type_tables.insert(*ty);
        let name = type_name(st, *ty).ok_or_else(|| anyhow::anyhow!("an object type no global names"))?;
        let mut fields = BTreeMap::new();
        scalars(st, *t, "", &mut fields, &mut FxHashSet::default());
        // (An exact census keeps the remainder: `remainder` below.)
        for e in TABLE.iter().filter(|e| e.flag == Flag::Always && e.target == Target::Objects(leak(&name)) && !(exact && remainder(e))) {
            for s in e.slots {
                if let Some(r) = rule(s.stored) {
                    apply::<A>(&mut fields, &s.field.join("."), &r)?;
                }
            }
        }
        key.extend_from_slice(b"\xffobj ");
        key.extend_from_slice(name.as_bytes());
        key.push(0);
        put(&mut key, &fields);
    }
    // The globals' scalars (the object types and `objects` aside).
    let mut fields = BTreeMap::new();
    let mut seen: FxHashSet<TableId> = type_tables;
    seen.insert(*objects);
    for (k, v) in &globals.hash {
        if k == crate::trace::block::BUTTON_GLOBAL || k == "types" {
            continue;
        }
        match v {
            Value::Table(t) => scalars(st, *t, k, &mut fields, &mut seen),
            Value::Num(iv) => {
                let (lo, hi) = A::bounds(*iv);
                fields.insert(k.clone(), Val::Num(A::bits(lo), A::bits(hi)));
            }
            Value::Bool(b) => {
                fields.insert(k.clone(), Val::Bool(*b));
            }
            _ => {}
        }
    }
    for e in TABLE.iter().filter(|e| e.flag == Flag::Always && e.target == Target::Globals) {
        for s in e.slots {
            if let Some(r) = rule(s.stored) {
                apply::<A>(&mut fields, &s.field.join("."), &r)?;
            }
        }
    }
    key.extend_from_slice(b"\xffglobals\0");
    put(&mut key, &fields);
    Ok(key)
}

/// The table's entry for the player's remainder.
fn remainder(e: &celeste_engine::widening::Entry) -> bool {
    e.target == Target::Objects("player") && e.slots.iter().any(|s| s.field == ["rem", "x"])
}

/// A `&'static str` equal to `s` among the table's object names.
fn leak(s: &str) -> &'static str {
    TABLE
        .iter()
        .find_map(|e| match e.target {
            Target::Objects(n) if n == s => Some(n),
            _ => None,
        })
        .unwrap_or("")
}

/// The player's remainder widened to the whole `[-1/2, 1/2)` (the forward's
/// boundary: the table's "player remainder"), where there is a player.
fn widen_remainder<A: Arithmetic>(st: &mut St<A>) -> Result<()> {
    let half = A::div(A::from_i16(1), A::from_i16(2)).expect("1/2");
    let (lo, hi) = (A::neg(half).expect("-1/2"), A::below(half).expect("below 1/2"));
    for p in crate::trace::widen::objects_of_type(st, "player") {
        for axis in ["x", "y"] {
            let path = crate::trace::widen::field(&p, &["rem", axis]);
            iface::set(st, &path, Value::Num(A::ival(lo, hi)))?;
        }
    }
    Ok(())
}

/// The room a state is in.
fn room_of<A: Arithmetic>(st: &St<A>) -> Option<(u64, u64)> {
    let n = |k: &str| match iface::get(st, &[iface::key("room"), iface::key(k)]) {
        Some(Value::Num(v)) => A::as_point(v).map(A::bits),
        _ => None,
    };
    Some((n("x")?, n("y")?))
}

/// THE CENSUS of arithmetic `A` under `s`: frames 1..=`to`, stopping early
/// once more than `max_nodes` nodes are met; `threads` engines. `exact`:
/// over CONCRETE states instead (the remainder exact, every state
/// distinct), the real diversity of speeds and remainders.
pub fn run<A: Arithmetic>(s: &crate::settings::Settings, to: u32, max_nodes: usize, threads: usize, exact: bool, mut each: impl FnMut(&Frame)) -> Result<Vec<Frame>> {
    ensure!(s.steps_per_frame() == 1, "the census runs whole frames: no --split-frame");
    ensure!(s.win_at.is_none(), "the census wins by leaving the room: no --win-at");
    let mut engines: Vec<ReferenceEngine<A>> = (0..threads.max(1)).map(|_| ReferenceEngine::<A>::new(s)).collect::<Result<_>>()?;
    for e in &mut engines {
        e.set_path_cap(PATH_CAP);
    }
    let mut start = engines[0].start_state().clone();
    let room = room_of(&start);
    if !exact {
        widen_remainder(&mut start)?;
    }
    let mut visited: FxHashSet<Vec<u8>> = FxHashSet::default();
    visited.insert(node_key(&start, exact)?);
    let mut frontier = vec![start];
    let (mut all_x, mut all_y) = (FxHashSet::default(), FxHashSet::default());
    let (mut rem_x, mut rem_y) = (FxHashSet::default(), FxHashSet::default());
    let mut out = Vec::new();
    for frame in 1..=to {
        if frontier.is_empty() || visited.len() > max_nodes {
            break;
        }
        // Expand in chunks, one engine a worker; merged in chunk order, so
        // the census is a function of the frame, not of the threads.
        // Each worker keeps only the leaves new to the census and to itself
        // (a state's leaves are mostly one node many times).
        let chunk = frontier.len().div_ceil(engines.len()).max(1);
        let seen = &visited;
        let parts: Vec<Result<Vec<(Vec<u8>, bool, St<A>)>>> = std::thread::scope(|sc| {
            let hs: Vec<_> = frontier
                .chunks(chunk)
                .zip(engines.iter_mut())
                .map(|(part, eng)| {
                    std::thread::Builder::new()
                        .stack_size(64 << 20)
                        .spawn_scoped(sc, move || -> Result<Vec<(Vec<u8>, bool, St<A>)>> {
                            let mut out = Vec::new();
                            let mut mine: FxHashSet<Vec<u8>> = FxHashSet::default();
                            for st in part {
                                for mut leaf in eng.frame_all_inputs(st)? {
                                    let won = room_of(&leaf) != room;
                                    // The frame's garbage (its scopes, dead closures) goes:
                                    // a node carries what is reachable only.
                                    leaf.gc();
                                    if !exact {
                                        widen_remainder(&mut leaf)?;
                                    }
                                    let key = node_key(&leaf, exact)?;
                                    if !seen.contains(&key) && mine.insert(key.clone()) {
                                        out.push((key, won, leaf));
                                    }
                                }
                            }
                            Ok(out)
                        })
                        .expect("spawn a census worker")
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a census worker panicked")).collect()
        });
        let mut next = Vec::new();
        let mut f = Frame { frame, ..Default::default() };
        let (mut fx, mut fy, mut bundles) = (FxHashSet::default(), FxHashSet::default(), FxHashSet::default());
        for part in parts {
            for (key, won, leaf) in part? {
                if !visited.insert(key) {
                    continue;
                }
                f.new += 1;
                let pl = crate::trace::widen::objects_of_type(&leaf, "player");
                if let Some(p) = pl.first() {
                    let num = |path: &[&str]| match iface::get(&leaf, &crate::trace::widen::field(p, path)) {
                        Some(Value::Num(v)) => A::as_point(v).map(A::bits),
                        _ => None,
                    };
                    let (x, y) = (num(&["spd", "x"]), num(&["spd", "y"]));
                    rem_x.insert(num(&["rem", "x"]));
                    rem_y.insert(num(&["rem", "y"]));
                    fx.insert(x);
                    fy.insert(y);
                    all_x.insert(x);
                    all_y.insert(y);
                    let mut without = leaf.clone();
                    iface::set(&mut without, &crate::trace::widen::field(p, &["spd", "x"]), Value::Num(A::point(A::from_i16(0))))?;
                    bundles.insert(node_key(&without, exact)?);
                }
                if won {
                    f.wins += 1;
                } else {
                    next.push(leaf);
                }
            }
        }
        f.total = visited.len();
        (f.spd_x, f.spd_y, f.spd_x_all, f.spd_y_all, f.bundles) = (fx.len(), fy.len(), all_x.len(), all_y.len(), bundles.len());
        (f.rem_x_all, f.rem_y_all) = (rem_x.len(), rem_y.len());
        each(&f);
        out.push(f);
        frontier = next;
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// THE CENSUS'S NODES ARE THE FORWARD'S: in 16.16, room (1,0) from its
    /// start, the new nodes of every frame to f27 are the widened forward's
    /// (the forward with every hull the whole circle) pinned counts.
    #[test]
    fn the_fixed_census_counts_the_widened_forwards_states() {
        let s = crate::settings::Settings { threads: Some(8), ..Default::default() };
        let pinned = std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/gates/ckhash_room10_f000-044_widened.txt")).expect("the gate");
        let want: Vec<usize> = pinned.lines().map(|l| l.split_whitespace().nth(1).expect("a count").parse().expect("a count")).collect();
        let frames = run::<celeste_core::arith::Fixed>(&s, 27, usize::MAX, 8, false, |_| {}).expect("census");
        for f in &frames {
            assert_eq!(f.new, want[f.frame as usize], "frame {}: the census's new nodes against the widened forward's", f.frame);
        }
        assert_eq!(frames.len(), 27);
    }
}
