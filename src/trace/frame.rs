//! Tracing ONE frame as a kernel (`trace_frame`). A trace is a FUNCTION:
//! input cells in, output cells out, plus per outcome a `guard` (when it
//! applies) and a derived `error` (where its row is undefined). The split
//! pass (`split`) makes it executable under abstract inputs.

use anyhow::{anyhow, bail, Result};

use crate::transpile::graph::NodeId;
use crate::transpile::kernel::{ArcRoots, AxisRoots, Roots};
use crate::transpile::graph::Ty;

use super::domain::{Domain, Symbolic};
use crate::transpile::garith::GraphArith;
use celeste_core::arith::Fixed;
use super::heap::Value;
use super::iface::{self, Conc, Iface, Path};
use super::interp::{Flow, Interp};
use super::state::State;

pub struct FrameOut<A: GraphArith = Fixed> {
    pub guard: NodeId,
    /// Where this outcome's row is undefined (`trace::error`); live lanes on
    /// which it holds decline. Derived, never carried.
    pub error: NodeId,
    /// Every scalar reachable from the globals, by path, with the KIND the
    /// tracer knows (not re-derivable: a `Cell` or `Sel` can be either).
    pub fields: Vec<(Path, NodeId, Ty)>,
    /// Slots DEAD at the frame boundary: the six button cells and the shared
    /// output unknown. The tracer runs `__reset_button_states()` at the START
    /// (production at the end), so recording these dead makes the boundaries
    /// agree and lets lanes dedup. CLAIMED, not proven: the differential check
    /// would catch a `btn` read outliving the reset.
    pub ubool: Vec<Path>,
    /// What kept this outcome from merging with its siblings (only shape can).
    pub shape: super::heap::Shape,
    /// The ENGINE structure of this outcome's end state, and the cell each of
    /// `fields`/`ubool` lands on: NOT comparable with `Frame::in_cells` unless
    /// the shape was kept.
    pub rt2: celeste_engine::runtime2::Rt2<A>,
    pub cells: Vec<u32>,
    pub ubool_cells: Vec<u32>,
    /// The state itself, so a shape walk can step forward from it.
    pub st: State<Symbolic<A>>,
    /// THE TRANSFER ROOTS (`search::rem::decode`, level-0 traces only).
    /// Never stored in the row.
    pub arc: Option<ArcRoots>,
}

impl<A: GraphArith> FrameOut<A> {
    /// What the outcome computes: its stored fields, guard (`live`), error
    /// and transfer roots.
    pub fn roots(&self) -> Roots {
        Roots { fields: self.fields.iter().map(|f| f.1).collect(), live: self.guard, error: self.error, arc: self.arc }
    }
}

impl<A: GraphArith> Clone for FrameOut<A> {
    /// For `split_undecided_selects`: `Rt2::clone_block` shares the cart.
    fn clone(&self) -> Self {
        FrameOut {
            guard: self.guard,
            error: self.error,
            fields: self.fields.clone(),
            ubool: self.ubool.clone(),
            shape: self.shape.clone(),
            rt2: self.rt2.clone_block(),
            cells: self.cells.clone(),
            ubool_cells: self.ubool_cells.clone(),
            st: self.st.clone(),
            arc: self.arc,
        }
    }
}

pub struct Frame<A: GraphArith = Fixed> {
    pub iface: Iface<A>,
    pub fork_origins: Vec<(u8, String)>,
    pub forks: u8,
    /// The forks' arities AS TRACED (a later frame overwrites the arena's;
    /// `lower::bind` installs these).
    pub fork_ways: Vec<u8>,
    pub outs: Vec<FrameOut<A>>,
    /// THE RAISE ROW's liveness: the OR of the path guards at every
    /// `Interp::poison` site (`false` where nothing raises). Not in `outs`: a
    /// raise has no field values, and `outs` consumers index positionally.
    pub raise: NodeId,
    /// The canonical cell each `Iface` slot names in the START state: the
    /// engine's numbering for `Op::Cell(i)`.
    pub in_cells: Vec<u32>,
    /// The engine structure of that state, the block shape a kernel runs on.
    pub in_rt2: celeste_engine::runtime2::Rt2<A>,
}

/// Every scalar the frame ends with, as graph nodes, split into data and the
/// dead slots (`FrameOut::ubool`).
fn out_fields<A: GraphArith>(
    st: &State<Symbolic<A>>,
    d: &Symbolic<A>,
) -> Result<(Vec<(Path, NodeId, Ty)>, Vec<Path>)> {
    let mut out = Vec::new();
    let mut ubool = Vec::new();
    // A near level's floor `state`s are an interval column in every outcome.
    let ival_always: Vec<Path> = super::widen::near_floor_states(st, d.level);
    for p in iface::scalars(st, &[])? {
        if p.first() == Some(&iface::key(super::game::BUTTONS)) {
            ubool.push(p);
            continue;
        }
        // The canonical output unknown: the same uniform `AV::UBool` in every
        // body, so no root.
        if let Some(Value::Bool(b)) = iface::get(st, &p) {
            if d.is_unknown_output(b) {
                ubool.push(p);
                continue;
            }
        }
        // The column's engine TYPE, asked of the graph: writing an interval as
        // a number would be an unchecked narrowing.
        let (n, ty) = match iface::get(st, &p).unwrap() {
            Value::Num(n) => {
                let ty = if d.is_interval(&n) || ival_always.contains(&p) { Ty::Ival } else { Ty::Num };
                (n, ty)
            }
            Value::Bool(n) => (n, Ty::Bool),
            _ => unreachable!("scalars only yields scalars"),
        };
        out.push((p, n, ty));
    }
    Ok((out, ubool))
}

pub struct TraceSpec<'s, A: GraphArith = Fixed> {
    /// The input slots: every scalar under them is a cell.
    pub roots: &'s [Path],
    /// Slots the body reads as a constant (`Op::Pin`).
    pub pin: &'s [(Path, Conc<A>)],
    /// Slots the boundary WIDENS to an interval (`rem`).
    pub ival: &'s [Path],
    /// Input slots BOUNDED to a range (literals, inclusive), read through
    /// `Op::Restrict`; outside it is the restriction's own error.
    pub bounds: &'s [(Path, (A::Lit, A::Lit))],
    /// Input slots NO MERGE SELECTS ON: a condition that reads one makes two
    /// successors (a respawn's exploration, `walk::explore_no_player`).
    pub merge_refused_on: &'s [Path],
    /// A concrete check's trace: no boundary widening inside the frame (a
    /// row hashes on what it stores) and no arc capture.
    pub concrete: bool,
    /// The level traced, where not the tracer's own.
    pub level: Option<crate::abstraction::Level>,
    /// Level -1's trace: `move`'s fork takes the static range's full width
    /// (`Symbolic::uncapped_ways`) and undecided selects stay selects
    /// (`Symbolic::no_known_forks`).
    pub level_minus_one: bool,
}

impl<A: GraphArith> Default for TraceSpec<'_, A> {
    fn default() -> Self {
        TraceSpec { roots: &[], pin: &[], ival: &[], bounds: &[], merge_refused_on: &[], concrete: false, level: None, level_minus_one: false }
    }
}

/// Symbolize `spec.roots`, free the buttons, and trace one frame (`chunks`'
/// reset, then its frame), the tracer's level and flags as `spec` says and
/// restored after.
pub fn trace_frame<'a, A: GraphArith>(it: &mut Interp<'a, Symbolic<A>>, chunks: &'a super::cart::Chunks, st: State<Symbolic<A>>, spec: &TraceSpec<A>) -> Result<Frame<A>> {
    let saved = (it.d.level, it.d.uncapped_ways, it.d.no_known_forks);
    if let Some(level) = spec.level {
        it.d.level = level;
    }
    if spec.level_minus_one {
        (it.d.uncapped_ways, it.d.no_known_forks) = (true, true);
    }
    let f = trace(it, chunks, st, spec);
    (it.d.level, it.d.uncapped_ways, it.d.no_known_forks) = saved;
    f
}

fn trace<'a, A: GraphArith>(it: &mut Interp<'a, Symbolic<A>>, chunks: &'a super::cart::Chunks, st: State<Symbolic<A>>, spec: &TraceSpec<A>) -> Result<Frame<A>> {
    let TraceSpec { roots, pin, ival, bounds, merge_refused_on, .. } = *spec;
    let widen = !spec.concrete;
    it.trace_start_nodes = it.d.node_count();
    let mut st = st;
    // A fresh frame: nothing an earlier trace (one that failed part-way
    // included) accumulated survives.
    it.d.frame = Default::default();
    it.d.graph.reset_forks();
    it.raised.clear();
    it.arc_capture = widen;
    let iface = iface::symbolize(&mut it.d, &mut st, roots, pin, ival)?;
    let refused = merge_refused_on
        .iter()
        .map(|p| iface.slots.iter().position(|q| q == p).map(|i| i as u32).ok_or_else(|| anyhow!("merge-refused {} is not an input slot", iface::show(p))))
        .collect::<Result<Vec<u32>>>()?;
    it.d.set_merge_refused(refused);
    // The lattice's pins (their slots read the constants; the raw cells feed
    // only these checks).
    for (i, c) in &iface.pins {
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(*i as u32));
        let c = match *c {
            iface::Conc::Num(v) => crate::transpile::graph::Pinned::Num(A::lit(v), A::lit(v)),
            iface::Conc::Bool(b) => crate::transpile::graph::Pinned::Bool(b),
        };
        it.d.pin(cell, c);
    }
    // The BOUNDED inputs (the region's square, speed and remainder; the
    // no-player phase's hulls), each read through its restriction. Nothing
    // seeds the raw cell, so nothing folds the check away.
    for (p, (lo, hi)) in bounds {
        let i = iface.slots.iter().position(|q| q == p).ok_or_else(|| anyhow!("bounded {} is not an input slot", iface::show(p)))?;
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(i as u32));
        let held = match iface::get(&st, p) {
            Some(Value::Num(n)) => n,
            other => bail!("bounded {} holds {other:?}, not a number", iface::show(p)),
        };
        if held != cell {
            // Pinned: the pin guard checks the lane, the range the pin.
            let v = it.d.as_const(&held).ok_or_else(|| anyhow!("bounded {} is neither its cell nor pinned", iface::show(p)))?;
            let (l, h) = (A::from_lit(*lo), A::from_lit(*hi));
            anyhow::ensure!(A::le(l, v) && A::le(v, h), "bounded {} is pinned to {}, outside [{}, {}]", iface::show(p), A::hex(v), A::hex(l), A::hex(h));
            continue;
        }
        let r = it.d.restrict(cell, *lo, *hi);
        iface::set(&mut st, p, Value::Num(r))?;
    }
    // The engine numbering for the INPUT shape.
    let Some(cache) = it.cache.clone() else { bail!("tracing a frame needs the room's collision cache") };
    let in_rt2 = super::block::skeleton(&st, cache.clone())?;
    // The canonical cell of each `Op::Cell(i)`. Inputs only: outputs bind
    // against their own shape.
    let in_cells = super::bind::resolve_all(&in_rt2, &iface.slots)?;
    let mut st = st;
    // The level's widened slots read as the table says, after the input
    // shape, before any read.
    super::widen::read_inputs(&mut st, &mut it.d)?;
    // THE KERNEL'S ADMISSIBLE INPUTS: every pin holds; a lane outside is an
    // error of the whole frame.
    let admissible = super::error::pins_hold(&mut it.d);
    // The arc capture is per FRAME; drop a representative's old one.
    st.arc = if it.arc_capture {
        let (x, y) = (super::state::ArcAxis::none(&mut it.d), super::state::ArcAxis::none(&mut it.d));
        Some(Box::new([x, y]))
    } else {
        None
    };
    let st = super::cart::run_one(it, &chunks.reset, st)?;
    let finished = it.exec_block(chunks.frame.nodes(), st)?;
    // The whole frame's error (inputs this kernel was not built for), OR-ed
    // into every outcome's.
    let global = {
        use super::domain::Domain;
        it.d.not(&admissible)
    };
    let mut outs = Vec::new();
    for (s, f) in finished {
        anyhow::ensure!(!matches!(f, Flow::Break), "break at frame toplevel");
        outs.push(finish_outcome(it, s, widen, global, &cache)?);
    }
    // Split selects on conditions a lane can hold undecided, until none is.
    if !it.d.no_known_forks {
        outs = super::split::split_undecided_selects(&mut it.d, outs)?;
        // The conditions operators were EVALUATED under are read by the error
        // derivation: three-valued, like the guards (unknown reads as error).
        super::split::three_valued_evaluated(&mut it.d, &outs);
    }
    // ERROR, DERIVED once the graph is final (`trace::error`), of everything
    // an outcome computes: its stored fields, guard and transfer roots (the
    // remainder before the widening stores no field, and an overflow there
    // would be a wrong transfer).
    let roots: Vec<Vec<NodeId>> = outs.iter().map(|o| o.roots().flat()).collect();
    let derived = super::error::for_outcomes(&mut it.d, &roots);
    // A PIN DISCONNECTS its input: nothing the outcomes compute - values,
    // guards, transfers, derived errors - may read a pinned input; only the
    // pin's own check does (`error::pins_hold`, in `global`).
    {
        let pinned: Vec<NodeId> = it.d.frame.assumptions.iter().filter(|a| matches!(it.d.graph.get(**a).op, crate::transpile::graph::Op::Pin(_))).map(|a| it.d.graph.get(*a).args[0]).collect();
        if !pinned.is_empty() {
            let mut reads: Vec<NodeId> = derived.clone();
            for o in &outs {
                reads.extend(o.fields.iter().map(|(_, n, _)| *n).chain([o.guard]).chain(o.arc.iter().flat_map(ArcRoots::nodes)));
            }
            let reach = it.d.graph.reachable(&reads);
            for x in pinned {
                anyhow::ensure!(!reach[x as usize], "a pinned input (node {x}, {:?}) is read past its pin", it.d.graph.get(x).op);
            }
        }
    }
    for (o, e) in outs.iter_mut().zip(derived) {
        o.error = it.d.graph.fold(crate::transpile::graph::Op::Or, vec![o.error, e]);
    }
    let fork_ways: Vec<u8> = (0..it.d.frame.forks).map(|d| it.d.graph.fork_ways(d)).collect();
    let raise = {
        use super::domain::Domain;
        let raised = std::mem::take(&mut it.raised);
        let mut r = it.d.boolean(false);
        for (g, _) in &raised {
            r = it.d.or(&r, g);
        }
        r
    };
    Ok(Frame { iface, fork_origins: it.d.frame.fork_origins.clone(), forks: it.d.frame.forks, fork_ways, outs, raise, in_cells, in_rt2 })
}

/// A finished path of the frame as an outcome: its absent-as-zero fields
/// written, its transfer roots read, widened, its fields typed and bound;
/// its error the frame's (`global`), where its model ended, and what its
/// widenings owe.
fn finish_outcome<A: GraphArith>(
    it: &mut Interp<'_, Symbolic<A>>,
    mut s: State<Symbolic<A>>,
    widen: bool,
    global: NodeId,
    cache: &std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
) -> Result<FrameOut<A>> {
    s.gc();
    // Integer keys as a block holds them: a state traced from the outcome's
    // block names its slots alike.
    super::block::dense_ints(&mut s)?;
    // Absent-as-zero fields: shape, not precision, so every level.
    super::widen::materialize_absent_fields(&mut s, &mut it.d)?;
    // The transfer roots: the remainder read BEFORE the widenings.
    let arc = match s.arc.as_deref() {
        None => None,
        Some(axes) => {
            let fins = super::state::arc_fins(&s)?;
            let zero = A::lit(A::from_i16(0));
            let [fx, fy] = fins.unwrap_or_else(|| [it.d.graph.leaf(crate::transpile::graph::Op::Const(zero, zero)); 2]);
            let axis = |a: &super::state::ArcAxis<Symbolic<A>>, fin| AxisRoots { took: a.took, pre: a.pre, frag: a.frag, ox: a.ox, fin };
            Some(ArcRoots { axes: [axis(&axes[0], fx), axis(&axes[1], fy)], fin: fins.is_some() })
        }
    };
    // THE WIDENINGS: before `out_fields` (reads the state), after `gc`.
    let owed = if widen { super::widen::widen(&mut s, &mut it.d)? } else { Vec::new() };
    let (fields, ubool) = out_fields(&s, &it.d)?;
    // Owed beyond its operators: the frame's, where the model ended, and the
    // widenings'.
    let error = owed
        .iter()
        .filter(|(p, _)| fields.iter().any(|(q, _, _)| q == p))
        .fold(it.d.graph.fold(crate::transpile::graph::Op::Or, vec![global, s.ended]), |e, (_, w)| it.d.graph.fold(crate::transpile::graph::Op::Or, vec![e, *w]));
    let rt2 = super::block::skeleton(&s, cache.clone())?;
    let paths: Vec<Path> =
        fields.iter().map(|(p, _, _)| p.clone()).chain(ubool.iter().cloned()).collect();
    let all = super::bind::resolve_all(&rt2, &paths)?;
    let (cells, ubool_cells) = all.split_at(fields.len());
    let (guard, shape) = (s.guard.clone(), s.shape()?);
    Ok(FrameOut {
        guard,
        error,
        fields,
        ubool,
        shape,
        rt2,
        cells: cells.to_vec(),
        ubool_cells: ubool_cells.to_vec(),
        st: s,
        arc,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::cart;
    use crate::transpile::graph::{Eval, Graph, Val};

    /// Input cells `cells` (in `Iface` order) as exact values.
    fn exact(cells: &[Conc]) -> std::collections::HashMap<u32, Val> {
        cells.iter().enumerate().map(|(i, c)| (i as u32, match c {
            Conc::Num(n) => Val::exact_num(*n),
            Conc::Bool(b) => Val::Bool(Some(*b)),
        })).collect()
    }

    /// The four "pm1 key" paths (`has_dashed`, `freeze`, and the player's
    /// `dash_time`, `djump`). The held buttons (`p_dash`, `p_jump`) are no
    /// part of it: every level reads them both ways (C7), so no body reads
    /// their cells.
    fn pm1_paths(player: &Path) -> Vec<Path> {
        let mut v: Vec<Path> = vec![vec![iface::key("has_dashed")], vec![iface::key("freeze")]];
        for f in ["dash_time", "djump"] {
            let mut q = player.clone();
            q.push(iface::key(f));
            v.push(q);
        }
        v
    }

    /// The pm1 key a state is in, as `trace_frame`'s pin list.
    fn pm1_key<A: GraphArith>(player: &Path, st: &State<Symbolic<A>>, d: &Symbolic<A>) -> Result<Vec<(Path, Conc<A>)>> {
        let mut out = Vec::new();
        for p in pm1_paths(player) {
            let c = match iface::get(st, &p) {
                Some(Value::Num(n)) => Conc::Num(
                    d.as_const(&n)
                        .ok_or_else(|| anyhow!("pm1 cell {} is symbolic", iface::show(&p)))?,
                ),
                Some(Value::Bool(b)) => Conc::Bool(
                    d.decide(&b)
                        .ok_or_else(|| anyhow!("pm1 cell {} is symbolic", iface::show(&p)))?,
                ),
                other => bail!("pm1 cell {} is {:?}, not a scalar", iface::show(&p), other),
            };
            out.push((p, c));
        }
        Ok(out)
    }

    /// A traced frame's graph with its six buttons RESOLVED to `bits` (in
    /// `__button_states` order) for the concrete evaluator, and the buttons'
    /// memory, which every level widens (C7), as a state with no button held
    /// has it: the held buttons (`p_jump`, `p_dash`, read both ways)
    /// released, the jump buffer's unknown number (the level's only
    /// countdown) 0. Its comparisons stay undecided atoms, so a buffered
    /// jump's outcome may claim a lane too. The outcomes' cone specialized
    /// into a fresh graph, and the map. Other forks stay standing.
    fn at_buttons<A: GraphArith>(g: &Graph<A>, f: &Frame<A>, bits: &[bool; 6]) -> Result<(Graph<A>, Vec<NodeId>)> {
        let forks = |origin: &str| -> Vec<u8> { f.fork_origins.iter().filter(|(_, o)| o == origin).map(|(d, _)| *d).collect() };
        let buttons = forks(super::super::domain::UNKNOWN_BOOL_ORIGIN);
        if buttons.len() != 6 {
            bail!("the frame minted {} unknown booleans, not the six buttons", buttons.len());
        }
        let mut cfg = vec![crate::transpile::graph::OPEN; 256];
        for (d, b) in buttons.iter().zip(bits) {
            cfg[*d as usize] = *b as u8;
        }
        for held in ["p_jump", "p_dash"] {
            for d in forks(held) {
                cfg[d as usize] = 0;
            }
        }
        let roots: Vec<NodeId> = f.outs.iter().flat_map(|o| o.fields.iter().map(|x| x.1).chain([o.guard, o.error])).collect();
        let need = g.reachable(&roots);
        let mut forked = g.like();
        let map = g.specialize_subset_into(&cfg, Some(&need), &mut forked);
        let mut out = forked.like();
        let unbuffered = forked.rebuild_into(None, &mut out, |_, node, _, out| {
            use crate::transpile::graph::Op;
            (node.op == Op::UnknownNum { countdown: true }).then(|| out.leaf(Op::Const(A::lit(A::from_i16(0)), A::lit(A::from_i16(0)))))
        });
        Ok((out, map.iter().map(|&n| if n == crate::transpile::graph::UNBUILT { n } else { unbuffered[n as usize] }).collect()))
    }

    /// A body compiled for one pm1 key must REFUSE a state in another: a
    /// perturbed pinned cell's `Op::Pin` check declines the lane in every
    /// outcome that claims it (with the jump buffer unknown, a lane with no
    /// button held both jumps and does not).
    #[test]
    fn a_body_pinned_to_a_pm1_key_refuses_any_other_key() {
        let s = crate::settings::Settings::default();
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        let frame = &chunks.frame;
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let mut st = cart::boot(&mut it, &chunks, s.room).expect("boot");
        let mut player = None;
        for _ in 0..40 {
            if let Some(p) = super::super::shapes::player_path(&st) {
                player = Some(p);
                break;
            }
            st = cart::run_one(&mut it, frame, st).expect("warm-up");
        }
        let player = player.expect("a player");
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }
        let key = pm1_key(&player, &st, &it.d).expect("pm1 key");
        assert_eq!(key.len(), 4, "the pm1 key is four cells");
        let f = trace_frame(&mut it, &chunks, st, &TraceSpec { roots: &roots, pin: &key, concrete: true, ..Default::default() }).expect("trace");
        assert_eq!(f.iface.pins.len(), 4, "all four pinned");

        // No error in whichever outcome claims this assignment.
        let (g, map) = at_buttons(&it.d.graph, &f, &[false; 6]).expect("the six buttons");
        let room = crate::transpile::graph::Room { cache: it.cache.clone().expect("cache") };
        let ok_at = |cells: &[Conc]| -> bool {
            let v = g.eval(&exact(cells), Eval::Exact, Some(&room)).expect("the frame at a point");
            let live: Vec<&FrameOut> = f.outs.iter().filter(|o| v[map[o.guard as usize] as usize] == Val::Bool(Some(true))).collect();
            assert!(!live.is_empty(), "an outcome claims the lane");
            let ok: Vec<bool> = live.iter().map(|o| v[map[o.error as usize] as usize] == Val::Bool(Some(false))).collect();
            assert!(ok.iter().all(|&k| k == ok[0]), "the claiming outcomes disagree on the key");
            ok[0]
        };

        assert!(ok_at(&f.iface.init), "the key it was compiled for is accepted");

        for (i, c) in &f.iface.pins {
            let mut cells = f.iface.init.clone();
            cells[*i] = match c {
                Conc::Num(v) => Conc::Num(v.checked_add(crate::pico8_num::Pico8Num::from_parts(1, 0)).expect("a small constant")),
                Conc::Bool(b) => Conc::Bool(!*b),
            };
            assert!(
                !ok_at(&cells),
                "{} moved off its pin and the body still accepted the lane",
                iface::show(&f.iface.slots[*i])
            );
        }
    }

    /// `ice_at` (`tile_flag_at(.., 4)`) agrees with the tile scan in every
    /// room: true somewhere in rooms with ice, false everywhere in others.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: every room's map, ~20 CPU-s"]
    fn ice_at_answers_the_tile_scan_in_every_room() {
        let s = crate::settings::Settings::default();
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        // Parsed before the interpreter, which borrows the ASTs it runs.
        let rects: Vec<(i16, i16, i16, i16)> = (0..16i16)
            .flat_map(|i| (0..16i16).map(move |j| (i * 8, j * 8)))
            .flat_map(|(x, y)| [(x, y, 8, 8), (x + 1, y + 3, 6, 5)])
            .collect();
        let probes: Vec<_> = rects
            .iter()
            .map(|(x, y, w, h)| full_moon::parse(&format!("ice_probe = ice_at({x},{y},{w},{h})")).expect("parse probe"))
            .collect();
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let st = cart::boot(&mut it, &chunks, s.room).expect("boot");
        let cd = &celeste_core::cart_data::CART;
        let scan = |rx: i16, ry: i16, x: i16, y: i16, w: i16, h: i16| -> bool {
            let p = |v: i16| crate::pico8_num::Pico8Num::from_i16(v);
            (y.max(0) / 8..=((y + h - 1).div_euclid(8)).min(15)).any(|ty| {
                (x.max(0) / 8..=((x + w - 1).div_euclid(8)).min(15)).any(|tx| {
                    let t = cd.mget(p(rx * 16 + tx), p(ry * 16 + ty)).expect("mget");
                    cd.fget(p(t as i16), p(4)).expect("fget")
                })
            })
        };
        // The rooms in parallel, each worker on its own COPY of the interpreter.
        let rooms: Vec<(i16, i16)> = (0..8i16).flat_map(|rx| (0..4i16).map(move |ry| (rx, ry))).collect();
        let base = std::sync::Mutex::new((it, st));
        let icy_rooms: Vec<bool> = crate::par::Pool::new(16)
            .stack(64 << 20)
            .map_with(
                rooms.len(),
                || {
                    let b = base.lock().expect("the base interpreter");
                    Ok((b.0.clone(), b.1.clone()))
                },
                |(it, st), r| {
                    let (rx, ry) = rooms[r];
                    it.cache = Some(crate::compiled::collision_cache((rx, ry)));
                    let mut o = st.clone();
                    for (k, v) in [("x", rx), ("y", ry)] {
                        let n = it.d.num(crate::pico8_num::Pico8Num::from_i16(v));
                        iface::set(&mut o, &[iface::key("room"), iface::key(k)], Value::Num(n))?;
                    }
                    let mut any = false;
                    for (&(x, y, wd, h), probe) in rects.iter().zip(&probes) {
                        let s = cart::run_one(it, probe, o.clone())?;
                        let Some(Value::Bool(b)) = iface::get(&s, &[iface::key("ice_probe")]) else { bail!("ice_at did not return a boolean") };
                        let want = scan(rx, ry, x, y, wd, h);
                        assert_eq!(it.d.decide(&b), Some(want), "room ({rx},{ry}) ice_at({x},{y},{wd},{h})");
                        any |= want;
                    }
                    Ok(any)
                },
            )
            .expect("every room");
        let icy = icy_rooms.iter().filter(|&&b| b).count();
        let dry = icy_rooms.len() - icy;
        eprintln!("[ice] {icy} rooms with ice, {dry} without");
        assert!(icy > 0 && dry > 0, "the map has rooms with and without ice");
    }

    /// The native `tile_flag_at` (the cached maps and the scan) answers what
    /// the CART'S OWN Lua `tile_flag_at` answers - its loop with a fractional
    /// limit - at every box near the room's edges, outside it included, in every
    /// room (a box wholly left of or above the room scans nothing).
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: every room's map through the Lua, ~20 CPU-s"]
    fn native_tile_flag_at_answers_the_carts_lua_loop() {
        let s = crate::settings::Settings::default();
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        let keep = full_moon::parse("lua_tile_flag_at = tile_flag_at").expect("parse");
        // Positions -24..8 and 120..152 on each axis, five boxes: the cached
        // sizes, an odd one, and the ice map's.
        let probe = full_moon::parse(
            "
boxes = { {1, 1, 0}, {6, 5, 0}, {8, 8, 0}, {3, 10, 0}, {6, 5, 4} }
tfa_bad = 0
tfa_hits = 0
tfa_x = 0
tfa_y = 0
tfa_k = 0
for xi=0,65 do
  local x = xi - 24
  if xi > 32 then x = xi + 87 end
  for yi=0,65 do
    local y = yi - 24
    if yi > 32 then y = yi + 87 end
    for k=1,5 do
      local b = boxes[k]
      local lua = lua_tile_flag_at(x, y, b[1], b[2], b[3])
      local native = tile_flag_at(x, y, b[1], b[2], b[3])
      if lua then tfa_hits = tfa_hits + 1 end
      if (lua and not native) or (native and not lua) then
        if tfa_bad == 0 then
          tfa_x = x
          tfa_y = y
          tfa_k = k
        end
        tfa_bad = tfa_bad + 1
      end
    end
  end
end
",
        )
        .expect("parse probe");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        it.cache = Some(crate::compiled::collision_cache(s.room));
        // `cart::boot`, with the Lua `tile_flag_at` kept under another name.
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let st = cart::run_one(&mut it, &chunks.top, st).expect("toplevel");
        let mut st = cart::run_one(&mut it, &keep, st).expect("keep the Lua tile_flag_at");
        cart::inject_tile_flag_at(&mut st);
        let st = cart::run_one(&mut it, &chunks.init, st).expect("_init");
        let rooms: Vec<(i16, i16)> = (0..8i16).flat_map(|rx| (0..4i16).map(move |ry| (rx, ry))).collect();
        let base = std::sync::Mutex::new((it, st));
        let hits: u64 = crate::par::Pool::new(16)
            .stack(64 << 20)
            .map_with(
                rooms.len(),
                || {
                    let b = base.lock().expect("the base interpreter");
                    Ok((b.0.clone(), b.1.clone()))
                },
                |(it, st), r| {
                    let (rx, ry) = rooms[r];
                    it.cache = Some(crate::compiled::collision_cache((rx, ry)));
                    let mut o = st.clone();
                    for (k, v) in [("x", rx), ("y", ry)] {
                        let n = it.d.num(crate::pico8_num::Pico8Num::from_i16(v));
                        iface::set(&mut o, &[iface::key("room"), iface::key(k)], Value::Num(n))?;
                    }
                    let s = cart::run_one(it, &probe, o)?;
                    let num = |name: &str| -> i32 {
                        let Some(Value::Num(n)) = iface::get(&s, &[iface::key(name)]) else { panic!("{name} is not a number") };
                        it.d.as_const(&n).and_then(|v| v.as_i16()).unwrap_or_else(|| panic!("{name} is not a constant integer")) as i32
                    };
                    assert_eq!(
                        num("tfa_bad"),
                        0,
                        "room ({rx},{ry}): the native tile_flag_at disagrees with the cart's loop, first at box {} at ({}, {})",
                        num("tfa_k"),
                        num("tfa_x"),
                        num("tfa_y")
                    );
                    Ok(num("tfa_hits") as u64)
                },
            )
            .expect("every room")
            .into_iter()
            .sum();
        assert!(hits > 0, "some box finds a flagged tile");
    }

    /// `break` in a loop with a symbolic bound (unreachable from the cart):
    /// trace once, evaluate at each concrete limit against the hand answer.
    #[test]
    fn break_leaves_a_loop_whose_bound_is_symbolic() {
        // The loop's unroll bound is its annotation, as in the cart.
        let src = "
function f(amount)
  local n = 0
  for i=0,abs(amount) do --@unroll 8
    if i >= 2 then break end
    n = n + 1
  end
  return n
end
";
        let top = full_moon::parse(src).expect("parse");
        let call = full_moon::parse("result = f(amount)").expect("parse call");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = cart::run_one(&mut it, &top, st).expect("toplevel");
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(0));
        iface::set(&mut st, &[iface::key("amount")], Value::Num(cell)).expect("set amount");
        let st = cart::run_one(&mut it, &call, st).expect("call f");
        let Some(Value::Num(node)) = iface::get(&st, &[iface::key("result")]) else {
            panic!("f did not return a number")
        };
        // 100 is past the unroll bound on purpose: after `break` it is moot.

        for a in [0i16, 1, 2, 3, 5, 8, 100] {
            let v = it.d.graph.eval(&exact(&[Conc::Num(crate::pico8_num::Pico8Num::from_i16(a))]), Eval::Exact, None).expect("the graph at a point");
            let want = (a + 1).min(2);
            assert_eq!(v[node as usize], Val::exact_num(crate::pico8_num::Pico8Num::from_i16(want)), "amount = {a}");
            assert_eq!(v[st.ended as usize], Val::Bool(Some(false)), "amount = {a}: the trace declined");
        }
    }
}
