//! Tracing ONE frame as a kernel, and checking it against the oracle.
//!
//! This is the first thing in `trace` that treats a trace as a FUNCTION
//! rather than as a walk: input cells in, output cells out, plus the two
//! booleans every outcome carries (`guard` - when does this outcome
//! apply; `ok` - did the tracer compute it correctly).
//!
//! The check is the point. Run the frame twice from the same state:
//!
//! * symbolically, with the player's fields replaced by `Op::Cell` leaves
//!   and the six buttons left free, which produces a graph;
//! * concretely, with real numbers in those fields and a real button
//!   assignment, which produces numbers.
//!
//! Then evaluate the graph at that assignment and compare. Both runs are
//! the SAME interpreter over the SAME domain - the "concrete" one is just
//! the symbolic domain with every leaf already a constant, which folds -
//! so a disagreement can only be the compilation itself: a bad merge, a
//! guard that claims the wrong lanes, a select on the wrong condition.
//! Nothing else is being tested, which is what makes a failure readable.

use anyhow::{anyhow, bail, Result};
use full_moon::ast;

use crate::transpile::graph::NodeId;

use super::domain::{Domain, Symbolic};
use super::heap::Value;
use super::iface::{self, Conc, Iface, Path, Step};
use super::interp::{Flow, Interp};
use super::state::State;

/// One traced outcome of a frame.
pub struct FrameOut {
    pub guard: NodeId,
    pub ok: NodeId,
    /// Every scalar reachable from the globals table, by path, with the
    /// KIND the tracer knows it to be. Carrying the kind rather than
    /// re-deriving it from the node's op matters: `Op::Cell` and `Op::Sel`
    /// are both, and a guess there is a guess about the boundary.
    pub fields: Vec<(Path, NodeId, bool)>,
    /// What kept this outcome from merging with its siblings. Only a
    /// shape difference can, so keeping it is what turns "twelve
    /// outcomes" into a statement about the program.
    pub shape: super::heap::Shape,
}

pub struct Frame {
    pub iface: Iface,
    pub outs: Vec<FrameOut>,
}

/// Run one chunk and require it to end normally in exactly one state.
/// The oracle side has to: with no unknowns there is nothing to branch on.
pub fn run_one<'a, D: Domain>(
    it: &mut Interp<'a, D>,
    ast: &'a ast::Ast,
    st: State<D>,
) -> Result<State<D>> {
    let out = it.exec_block(ast.nodes(), st)?;
    if out.len() != 1 {
        bail!("expected one state, got {}", out.len());
    }
    let (s, f) = out.into_iter().next().unwrap();
    if let Flow::Break = f {
        bail!("break at chunk toplevel");
    }
    Ok(s)
}

/// Every scalar the state ends the frame holding, as graph nodes.
fn out_fields(st: &State<Symbolic>) -> Result<Vec<(Path, NodeId, bool)>> {
    let mut out = Vec::new();
    for p in iface::scalars(st, &[])? {
        let (n, is_bool) = match iface::get(st, &p).unwrap() {
            Value::Num(n) => (n, false),
            Value::Bool(n) => (n, true),
            _ => unreachable!("scalars only yields scalars"),
        };
        out.push((p, n, is_bool));
    }
    Ok(out)
}

/// Symbolize `root`, free the buttons, and trace one frame.
pub fn trace_frame<'a>(
    it: &mut Interp<'a, Symbolic>,
    reset: &'a ast::Ast,
    frame: &'a ast::Ast,
    st: State<Symbolic>,
    roots: &[Path],
) -> Result<Frame> {
    let mut st = st;
    let iface = iface::symbolize(&mut it.d, &mut st, roots)?;
    let st = run_one(it, reset, st)?;
    let mut outs = Vec::new();
    for (s, f) in it.exec_block(frame.nodes(), st)? {
        if let Flow::Break = f {
            bail!("break at frame toplevel");
        }
        let mut s = s;
        s.gc();
        outs.push(FrameOut {
            guard: s.guard,
            ok: s.ok,
            fields: out_fields(&s)?,
            shape: s.shape()?,
        });
    }
    Ok(Frame { iface, outs })
}

/// Write a concrete button assignment, for the oracle side.
pub fn set_buttons(d: &mut Symbolic, st: &mut State<Symbolic>, bits: &[bool; 6]) -> Result<()> {
    for (i, b) in bits.iter().enumerate() {
        let v = d.boolean(*b);
        iface::set(st, &[iface::key("__button_states"), Step::Idx(i)], Value::Bool(v))?;
    }
    Ok(())
}

/// The input vector for one PERTURBATION of the traced state: the values
/// the frame was traced at, with some slots replaced.
///
/// This is what makes an input cell a variable rather than a constant.
/// Checking the graph only at the values it was traced at would pass for
/// a graph that had folded every one of them away, which is the one bug
/// the whole design is exposed to.
pub fn cells_with(iface: &Iface, over: &[(Path, Conc)]) -> Result<Vec<Conc>> {
    let mut v = iface.init.clone();
    for (p, c) in over {
        let i = iface
            .slots
            .iter()
            .position(|q| q == p)
            .ok_or_else(|| anyhow!("{} is not an input cell", iface::show(p)))?;
        v[i] = *c;
    }
    Ok(v)
}

/// Check one traced frame against the oracle at one (inputs, buttons)
/// point. Returns `(which outcome claimed it, fields compared)` - the
/// outcome index so a caller can tell whether the guards ever
/// discriminate, and the count so it can tell "agreed about everything"
/// from "agreed about nothing, because the paths did not line up".
pub fn check_at(
    it: &Interp<'_, Symbolic>,
    f: &Frame,
    cells: &[Conc],
    bits: &[bool; 6],
    oracle: &[(Path, Conc)],
) -> Result<(usize, usize)> {
    let env = super::eval::Env {
        cells,
        frees: bits,
        cart: it.cart.clone(),
        cache: it.cache.clone(),
    };
    let g = &it.d.graph;
    let mut live: Vec<usize> = Vec::new();
    for (i, o) in f.outs.iter().enumerate() {
        if super::eval::eval(g, o.guard, &env)? == Conc::Bool(true) {
            live.push(i);
        }
    }
    // The frontier's guards are pairwise disjoint and cover everything
    // (see `State::guard`), so exactly one outcome claims this lane.
    // Anything else is a broken invariant, not a rounding difference.
    if live.len() != 1 {
        bail!("{:?}: {} outcomes claim this assignment, not 1", bits, live.len());
    }
    let o = &f.outs[live[0]];
    if super::eval::eval(g, o.ok, &env)? != Conc::Bool(true) {
        bail!("{:?}: the trace declined this assignment (ok is false)", bits);
    }
    let mut n = 0;
    for (p, want) in oracle {
        let got = o
            .fields
            .iter()
            .find(|(q, _, _)| q == p)
            .ok_or_else(|| anyhow!("{:?}: traced state has no {}", bits, iface::show(p)))?;
        let got = super::eval::eval(g, got.1, &env)?;
        if got != *want {
            bail!("{:?}: {} is {:?}, oracle says {:?}", bits, iface::show(p), got, want);
        }
        n += 1;
    }
    if o.fields.len() != oracle.len() {
        bail!(
            "{:?}: traced state has {} scalars, oracle has {}",
            bits,
            o.fields.len(),
            oracle.len()
        );
    }
    Ok((live[0], n))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::cart;

    /// The player is the object with a `djump` field. Naming it by
    /// position would be wrong the moment an object dies: `objects` is a
    /// list and things are deleted from it.
    fn find_player(st: &State<Symbolic>) -> Option<Path> {
        let objs = vec![iface::key("objects")];
        let Some(Value::Table(t)) = iface::get(st, &objs) else { return None };
        for i in 0..st.heap.tables[&t].arr.len() {
            let mut p = objs.clone();
            p.push(Step::Idx(i));
            if let Some(Value::Table(o)) = iface::get(st, &p) {
                if st.heap.tables[&o].hash.contains_key("djump") {
                    return Some(p);
                }
            }
        }
        None
    }

    /// Can the KERNEL EMITTER lower a traced graph?
    ///
    /// This is the join the whole campaign is for: `transpile::lower` is
    /// the emitter the generated crates already use, and it consumes a
    /// `Graph`. The tracer produces a `Graph` without any of the ~13,000
    /// rewrites the old front half needed. If the second goes through the
    /// first, the rewrites have nothing left to do.
    ///
    /// A PROBE, not a gate: it reports what the emitter said. Every
    /// refusal names something the tracer emits that the emitter cannot
    /// represent, which is the list of work between here and a kernel.
    #[test]
    fn the_kernel_emitter_lowers_a_traced_graph() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");
        let mut player = None;
        for _ in 0..40 {
            if let Some(p) = find_player(&st) {
                player = Some(p);
                break;
            }
            st = match run_one(&mut it, &frame, st) {
                Ok(s) => s,
                Err(e) => return eprintln!("[emit] warm-up stopped at: {:#}", e),
            };
        }
        let Some(player) = player else { return eprintln!("[emit] no player") };
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in ["freeze", "frames", "will_restart", "delay_restart", "max_djump"] {
            roots.push(vec![iface::key(g)]);
        }
        let f = match trace_frame(&mut it, &reset, &frame, st, &roots) {
            Ok(f) => f,
            Err(e) => return eprintln!("[emit] trace stopped at: {:#}", e),
        };

        // The tracer's OWN numbering: input cells are `Op::Cell(i)` in
        // `Iface` order, and outputs are numbered after them. Not the
        // engine's - see `trace::emit`.
        // UNIFORM vs PER-LANE is a boundary decision the tracer does
        // not model yet, and the emitter needs it: `tile_flag_at` takes
        // its width and height as block-uniform `P8`, so a per-lane
        // hitbox is a narrowing it refuses. A hitbox IS uniform - it is
        // fixed per object type and never written during a frame - so
        // saying so here is a stand-in for the classification, not a
        // fudge. Getting it from the boundary is part of the numbering
        // work still to come.
        let mut inputs: Vec<(u32, &'static str)> = Vec::new();
        let mut uni: Vec<(u32, &'static str)> = Vec::new();
        for (i, c) in f.iface.init.iter().enumerate() {
            let kind = match c {
                Conc::Num(_) => "num",
                Conc::Bool(_) => "bool",
            };
            let path = iface::show(&f.iface.slots[i]);
            if path.contains(".hitbox.") {
                uni.push((i as u32, kind));
            } else {
                inputs.push((i as u32, kind));
            }
        }
        let base = inputs.len() as u32;
        let g = std::mem::take(&mut it.d.graph);
        for (n, o) in f.outs.iter().enumerate() {
            // The BUTTON cells are not ordinary outputs. They end the
            // frame holding next frame's free choices, so they are
            // button-dependent by construction and would distinguish all
            // 64 assignments on their own - which is why the emitter has
            // `OutFields::ubool` to keep them out of `fields`.
            let outputs: Vec<(u32, crate::transpile::graph::NodeId, &'static str)> = o
                .fields
                .iter()
                .enumerate()
                .filter(|(_, (p, _, _))| !iface::show(p).starts_with("__button_states"))
                .map(|(i, (_, node, is_bool))| {
                    (base + i as u32, *node, if *is_bool { "ZB" } else { "ZN" })
                })
                .collect();
            let lowered = super::super::emit::lower_frame(
                &g, &inputs, &uni, &outputs, o.guard, o.ok,
            );
            match &lowered {
                Ok(ref l) => eprintln!(
                    "[emit] outcome {}: {} lines, {} variants, {} outputs",
                    n,
                    l.body.len(),
                    l.variants,
                    outputs.len()
                ),
                Err(e) => eprintln!("[emit] outcome {} REFUSED: {:#}", n, e),
            }
            // WHY 64 variants, where the existing `steady` kernel
            // collapses 64 button assignments to 36? The emitter dedups
            // an assignment on (every output, `ok`, `live`) together, so
            // count each key on its own and find out which one splits,
            // rather than telling a story about the guard.
            let l_variants = match &lowered {
                Ok(l) => l.variants,
                Err(_) => 0,
            };
            {
                let mut sp = crate::transpile::graph::Graph::new();
                let maps: Vec<Vec<crate::transpile::graph::NodeId>> =
                    (0u8..64).map(|m| g.specialize_into(m, &mut sp)).collect();
                let count = |with_ok: bool, with_live: bool| -> usize {
                    let mut seen = std::collections::BTreeSet::new();
                    for m in 0..64usize {
                        let mut k: Vec<crate::transpile::graph::NodeId> = o
                            .fields
                            .iter()
                            .filter(|(p, _, _)| !iface::show(p).starts_with("__button_states"))
                            .map(|(_, nd, _)| maps[m][*nd as usize])
                            .collect();
                        if with_ok {
                            k.push(maps[m][o.ok as usize]);
                        }
                        if with_live {
                            k.push(maps[m][o.guard as usize]);
                        }
                        seen.insert(k);
                    }
                    seen.len()
                };
                eprintln!(
                    "[emit]   outcome {} distinct assignments: outputs {}, +ok {}, +live {}",
                    n,
                    count(false, false),
                    count(true, false),
                    count(true, true)
                );
                // WHICH cells actually differ between assignments? "40
                // variants" is a count; the question is what the buttons
                // are still reaching in a state with no objects left.
                let mut varying: Vec<(String, usize)> = Vec::new();
                for (p, nd, _) in &o.fields {
                    if iface::show(p).starts_with("__button_states") {
                        continue;
                    }
                    let vals: std::collections::BTreeSet<_> =
                        (0..64usize).map(|m| maps[m][*nd as usize]).collect();
                    if vals.len() > 1 {
                        varying.push((iface::show(p), vals.len()));
                    }
                }
                // How many DISTINCT SUCCESSOR ROWS can actually come
                // out? That is the honest meaning of a variant: not "how
                // many assignments produce structurally different
                // expressions" (which is what the emitter's dedup key
                // measures, and which over-counts whenever two
                // expressions are equal without being identical), but
                // "how many different rows can the search see". Anything
                // above this number is generated code that no input can
                // tell apart.
                //
                // Only assignments whose guard holds at the point count -
                // the others describe a different outcome there - and
                // only those the trace did not decline, since a declining
                // lane leaves for the interpreter rather than becoming a
                // row.
                // EXACT paths. `ends_with(".y")` finds
                // `objects[0].dash_accel.y` first, so an earlier version
                // of this perturbed the dash accelerator instead of the
                // player and never reached the death or next-room
                // outcomes at all.
                let base = iface::show(&player);
                let idx = |field: &str| {
                    let want = format!("{}.{}", base, field);
                    f.iface.slots.iter().position(|q| iface::show(q) == want)
                };
                let gidx = |name: &str| {
                    f.iface.slots.iter().position(|q| iface::show(q) == name)
                };
                // A SWEEP, not a nudge. The death, restart and next-room
                // outcomes are only live where the player actually dies
                // or leaves, so a handful of one-pixel perturbations
                // never reaches them and the measurement comes out
                // vacuously zero.
                let n16 = crate::pico8_num::Pico8Num::from_i16;
                let mut points: Vec<Vec<Conc>> = Vec::new();
                for dx in [-8i16, 0, 8] {
                    for dy in [-120i16, -8, 0, 8, 24, 64] {
                        for sy in [None, Some(2i16)] {
                            let mut p = f.iface.init.clone();
                            for (field, d) in [("x", dx), ("y", dy)] {
                                if let Some(i) = idx(field) {
                                    if let Conc::Num(v) = p[i] {
                                        p[i] = Conc::Num(v + n16(d));
                                    }
                                }
                            }
                            if let (Some(v), Some(i)) = (sy, idx("spd.y")) {
                                p[i] = Conc::Num(n16(v));
                            }
                            points.push(p);
                        }
                    }
                }
                // The cells `freeze` and `has_dashed` actually depend on
                // are the DASH AND JUMP state, and a sweep over position
                // and velocity never touches them - so a row count taken
                // from position alone is a lower bound measured in the
                // wrong dimensions.
                for (field, vals) in [
                    ("djump", [0i16, 1].as_slice()),
                    ("dash_time", [0, 1, 3].as_slice()),
                    ("grace", [0, 1, 6].as_slice()),
                    ("dash_effect_time", [0, 5].as_slice()),
                ] {
                    if let Some(i) = idx(field) {
                        for v in vals {
                            let mut p = f.iface.init.clone();
                            p[i] = Conc::Num(n16(*v));
                            points.push(p);
                        }
                    }
                }
                for field in ["p_dash", "p_jump"] {
                    if let Some(i) = idx(field) {
                        for v in [false, true] {
                            let mut p = f.iface.init.clone();
                            p[i] = Conc::Bool(v);
                            points.push(p);
                        }
                    }
                }
                if let (Some(w), Some(d)) = (gidx("will_restart"), gidx("delay_restart")) {
                    let mut p = f.iface.init.clone();
                    p[w] = Conc::Bool(true);
                    p[d] = Conc::Num(n16(1));
                    points.push(p);
                }
                let mut most_rows = 0usize;
                let mut live_at_most = 0usize;
                for pt in &points {
                    let mut rows: std::collections::BTreeSet<Vec<(u8, i32)>> = Default::default();
                    let mut live_here = 0usize;
                    for m in 0u8..64 {
                        let mut bits = [false; 6];
                        for (i, b) in bits.iter_mut().enumerate() {
                            *b = m & (1 << i) != 0;
                        }
                        let env = super::super::eval::Env {
                            cells: pt,
                            frees: &bits,
                            cart: it.cart.clone(),
                            cache: it.cache.clone(),
                        };
                        let ev = |nd| super::super::eval::eval(&g, nd, &env);
                        let holds = |nd| matches!(ev(nd), Ok(Conc::Bool(true)));
                        if !holds(o.guard) || !holds(o.ok) {
                            continue;
                        }
                        live_here += 1;
                        let mut row = Vec::new();
                        for (q, nd, _) in &o.fields {
                            if iface::show(q).starts_with("__button_states") {
                                continue;
                            }
                            match ev(*nd) {
                                Ok(Conc::Num(v)) => row.push((0u8, v.as_raw_u32() as i32)),
                                Ok(Conc::Bool(b)) => row.push((1u8, b as i32)),
                                Err(_) => row.push((2u8, 0)),
                            }
                        }
                        rows.insert(row);
                    }
                    if rows.len() > most_rows {
                        most_rows = rows.len();
                        live_at_most = live_here;
                    }
                }
                eprintln!(
                    "[emit]   outcome {}: emitter keeps {} variants; the most DISTINCT ROWS any \
                     of {} input points produces is {} (from {} live assignments)",
                    n,
                    l_variants,
                    points.len(),
                    most_rows,
                    live_at_most
                );

                // WHY do assignments that produce the same VALUE keep
                // different trees? The emitter's dedup key is structural
                // node identity after specialisation, so they collapse
                // exactly when they fold to the same tree. Print the
                // first two that differ, shallowly.
                if n == 1 {
                    let show = |sp: &crate::transpile::graph::Graph,
                                root: crate::transpile::graph::NodeId,
                                depth: usize|
                     -> String {
                        fn go(
                            g: &crate::transpile::graph::Graph,
                            id: crate::transpile::graph::NodeId,
                            d: usize,
                        ) -> String {
                            let nd = g.get(id);
                            if d == 0 {
                                return format!("{:?}#{}", nd.op, id);
                            }
                            if nd.args.is_empty() {
                                return format!("{:?}", nd.op);
                            }
                            let kids: Vec<String> =
                                nd.args.iter().map(|a| go(g, *a, d - 1)).collect();
                            format!("{:?}({})", nd.op, kids.join(", "))
                        }
                        go(sp, root, depth)
                    };
                    if let Some((fp, fnode, _)) =
                        o.fields.iter().find(|(q, _, _)| iface::show(q) == "freeze")
                    {
                        let mut seen: std::collections::BTreeMap<
                            crate::transpile::graph::NodeId,
                            Vec<u8>,
                        > = Default::default();
                        for m in 0u8..64 {
                            seen.entry(maps[m as usize][*fnode as usize])
                                .or_default()
                                .push(m);
                        }
                        eprintln!(
                            "[emit]   {} has {} distinct trees over 64 assignments",
                            iface::show(fp),
                            seen.len()
                        );
                        for (node, ms) in seen.iter().take(3) {
                            eprintln!(
                                "[emit]     masks {:?}..: {}",
                                &ms[..ms.len().min(3)],
                                show(&sp, *node, 3)
                            );
                        }
                    }
                }

                varying.sort_by_key(|(_, k)| std::cmp::Reverse(*k));
                eprintln!(
                    "[emit]   outcome {}: {} of {} output cells vary; top: {:?}",
                    n,
                    varying.len(),
                    o.fields.len(),
                    varying.iter().take(6).collect::<Vec<_>>()
                );
            }
        }
    }

    /// `ice_at` is `tile_flag_at(.., 4)`, and only flag 0 is modelled.
    /// Answering `false` for every other flag is right exactly where the
    /// room has no such tile - so this checks BOTH halves: that it
    /// answers in a room without ice, and that it RAISES in one with.
    ///
    /// The second half is the point. A guard that never fires is
    /// indistinguishable from no guard, and this one was wrong in the
    /// interpreter too, so no differential test between the two could
    /// have found it.
    #[test]
    fn ice_answers_without_ice_and_raises_with_it() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let probe = full_moon::parse("ice_probe = ice_at(0,0,8,8)").expect("parse probe");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd =
            std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let st = run_one(&mut it, &init, st).expect("_init");

        // The start room has no ice, so the answer is false and exact.
        let s = run_one(&mut it, &probe, st.clone()).expect("start room should answer");
        let Some(Value::Bool(b)) = iface::get(&s, &[iface::key("ice_probe")]) else {
            panic!("ice_at did not return a boolean")
        };
        assert_eq!(it.d.decide(&b), Some(false), "the start room has no ice");

        // Somewhere on the map there IS ice, and there it must raise
        // rather than quietly answer false. Searched rather than
        // hard-coded, so the test cannot pass by looking in the wrong
        // place.
        let mut refused = 0;
        let mut answered = 0;
        for rx in 0..8i16 {
            for ry in 0..4i16 {
                let mut o = st.clone();
                for (k, v) in [("x", rx), ("y", ry)] {
                    let n = it.d.num(crate::pico8_num::Pico8Num::from_i16(v));
                    iface::set(&mut o, &[iface::key("room"), iface::key(k)], Value::Num(n))
                        .expect("set room");
                }
                match run_one(&mut it, &probe, o) {
                    Ok(_) => answered += 1,
                    Err(e) => {
                        assert!(
                            format!("{:#}", e).contains("CONTAINS that flag"),
                            "refused for the wrong reason: {:#}",
                            e
                        );
                        refused += 1;
                    }
                }
            }
        }
        eprintln!("[ice] {} rooms answer false, {} raise", answered, refused);
        assert!(refused > 0, "no room on the map has ice - then this guard is untested");
        assert!(answered > 0, "every room raised - the guard is too coarse");
    }

    /// `break` in a loop whose bound the tracer CANNOT know.
    ///
    /// The PICO-8 corpus cannot reach this. `run_for_symbolic` only runs
    /// when the limit is unknown, and every program PICO-8 can also run is
    /// concrete, so the corpus exercises `run_for` and nothing else. That
    /// is exactly why the bug lived here and not there: `for_body`
    /// rewrote `Flow::Break` to `Flow::Normal`, the state went back into
    /// the frontier and ran the body again, and `break` did nothing.
    ///
    /// So: trace the loop once with a symbolic limit, then evaluate the
    /// resulting graph at each concrete limit and compare with the answer
    /// worked out by hand. One graph, every point - the same discipline as
    /// the frame check.
    #[test]
    fn break_leaves_a_loop_whose_bound_is_symbolic() {
        // `abs(amount)` because `unroll_bound` is keyed on the limit
        // expression's SOURCE TEXT, and that is the cart's own spelling
        // (`move_x`/`move_y`), which is where this actually bites.
        let src = "
function f(amount)
  local n = 0
  for i=0,abs(amount) do
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
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        let cell = it.d.graph.leaf(crate::transpile::graph::Op::Cell(0));
        iface::set(&mut st, &[iface::key("amount")], Value::Num(cell)).expect("set amount");
        let st = run_one(&mut it, &call, st).expect("call f");
        let Some(Value::Num(node)) = iface::get(&st, &[iface::key("result")]) else {
            panic!("f did not return a number")
        };
        // 100 is well past the unroll bound of 8 on purpose: once a
        // `break` is reached the bound stops mattering, so `ok` has to be
        // true there too. Before the fix the loop ran on and the "it
        // finished" obligation made this lane deopt.
        for a in [0i16, 1, 2, 3, 5, 8, 100] {
            let cells = [Conc::Num(crate::pico8_num::Pico8Num::from_i16(a))];
            let env = super::super::eval::Env {
                cells: &cells,
                frees: &[false; 6],
                cart: None,
                cache: None,
            };
            let got = super::super::eval::eval(&it.d.graph, node, &env).expect("eval result");
            let want = (a + 1).min(2);
            assert_eq!(
                got,
                Conc::Num(crate::pico8_num::Pico8Num::from_i16(want)),
                "amount = {}",
                a
            );
            let ok = super::super::eval::eval(&it.d.graph, st.ok, &env).expect("eval ok");
            assert_eq!(ok, Conc::Bool(true), "amount = {}: the trace declined", a);
        }
    }

    /// Trace ONE frame with the player's fields symbolic and the six
    /// buttons free, then check that one graph against the oracle at
    /// every point of a position/speed sweep crossed with all 64 button
    /// assignments.
    ///
    /// The sweep is the part that matters. Checking only at the values
    /// the frame was traced at would pass for a graph that had constant
    /// -folded every input away, and re-tracing per point would not test
    /// anything: the claim is that ONE graph answers for all of them.
    ///
    /// Still a PROBE where it cannot get far enough - it prints and
    /// returns rather than panicking, because each stop names the next
    /// thing to implement and a panic hides the ones behind it. Anything
    /// it does reach, it asserts about.
    #[test]
    fn a_traced_frame_agrees_with_the_oracle() {
        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let reset = full_moon::parse("__reset_button_states()").expect("parse reset");
        let frame = full_moon::parse("_update()").expect("parse frame");

        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(celeste_core::cart_data::CartData::load("cart").expect("cart"));
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        ));
        it.cart = Some(cd);

        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");

        // Warm up with the buttons held concrete (the toplevel leaves
        // them false and `btn` writes concrete values back, so nothing
        // symbolic enters). This gets past the spawn animation, which
        // reads no buttons and would make the check vacuous.
        let mut player = None;
        for n in 0..40 {
            if let Some(p) = find_player(&st) {
                player = Some((n, p));
                break;
            }
            st = match run_one(&mut it, &frame, st) {
                Ok(s) => s,
                Err(e) => return eprintln!("[verify] warm-up frame {} stopped at: {:#}", n, e),
            };
        }
        let Some((warm, player)) = player else {
            return eprintln!("[verify] no player after 40 frames");
        };
        eprintln!("[verify] player at {} after {} warm-up frames", iface::show(&player), warm);

        // The rest of the frame's INPUT state. Everything else reachable
        // from the globals table is static configuration - the `k_*`
        // button numbers, each type's `tile`, `room.x/y` - or the button
        // slots, which `__reset_button_states` makes free choices rather
        // than cells.
        let mut roots: Vec<Path> = vec![player.clone()];
        for g in [
            "deaths",
            "delay_restart",
            "frames",
            "freeze",
            "has_dashed",
            "has_key",
            "max_djump",
            "minutes",
            "pause_player",
            "seconds",
            "will_restart",
        ] {
            roots.push(vec![iface::key(g)]);
        }

        let before = it.d.graph.len();
        let f = match trace_frame(&mut it, &reset, &frame, st.clone(), &roots) {
            Ok(f) => f,
            Err(e) => return eprintln!("[verify] symbolic frame stopped at: {:#}", e),
        };
        eprintln!(
            "[verify] {} input cells, {} outcome(s), {} nodes ({} new)",
            f.iface.slots.len(),
            f.outs.len(),
            it.d.graph.len(),
            it.d.graph.len() - before
        );

        // PERTURB THE INPUTS as well as the buttons. Without this the
        // graph is only ever evaluated at the values it was traced at,
        // which a graph that folded every input away would also pass.
        // The same override goes into both sides: written into the heap
        // for the oracle, into the cell vector for the graph. Nothing is
        // re-traced - reusing one graph across all of these is the claim
        // being tested.
        let px = |k: &str| {
            let mut p = player.clone();
            p.push(iface::key(k));
            p
        };
        let sub = |k: &str, s: &str| {
            let mut p = player.clone();
            p.push(iface::key(k));
            p.push(iface::key(s));
            p
        };
        let at = |p: &Path| match f.iface.init[f.iface.slots.iter().position(|q| q == p).unwrap()] {
            Conc::Num(n) => n,
            Conc::Bool(_) => panic!("{} is a boolean", iface::show(p)),
        };
        let n = crate::pico8_num::Pico8Num::from_i16;
        // A cross product rather than a random sample: the interesting
        // structure is where the player is relative to the tiles, and a
        // sweep over position and speed hits walls, floors and the pit
        // below the room, which is what the non-trivial output SHAPES are.
        let mut perts: Vec<(String, Vec<(Path, Conc)>)> = Vec::new();
        for dx in [-8i16, -1, 0, 1, 8] {
            for dy in [-8i16, -1, 0, 1, 8, 24, 64] {
                // A falling speed of 8 makes `move_y` step further than
                // the unroll bound, so it is the REFUSAL case - kept on
                // one column of the sweep so that path stays covered
                // without spending a quarter of the run on it.
                let sys: &[Option<i16>] =
                    if dx == 0 { &[None, Some(-2), Some(2), Some(8)] } else { &[None, Some(-2), Some(2)] };
                for sy in sys.iter().copied() {
                    let mut over = vec![
                        (px("x"), Conc::Num(at(&px("x")) + n(dx))),
                        (px("y"), Conc::Num(at(&px("y")) + n(dy))),
                    ];
                    if let Some(v) = sy {
                        over.push((sub("spd", "y"), Conc::Num(n(v))));
                    }
                    perts.push((format!("dx{} dy{} sy{:?}", dx, dy, sy), over));
                }
            }
        }
        // The control-flow inputs, one at a time rather than crossed with
        // the position sweep: each of these changes which BRANCH the
        // frame takes rather than where it lands, so crossing them would
        // multiply the run without touching anything new.
        let g = |k: &str| vec![iface::key(k)];
        for (name, over) in [
            ("freeze", vec![(g("freeze"), Conc::Num(n(1)))]),
            ("pause_player", vec![(g("pause_player"), Conc::Bool(true))]),
            ("will_restart", vec![(g("will_restart"), Conc::Bool(true))]),
            (
                "restarting",
                vec![
                    (g("will_restart"), Conc::Bool(true)),
                    (g("delay_restart"), Conc::Num(n(1))),
                ],
            ),
            ("max_djump", vec![(g("max_djump"), Conc::Num(n(2)))]),
            ("has_dashed", vec![(g("has_dashed"), Conc::Bool(true))]),
            ("no djump", vec![(px("djump"), Conc::Num(n(0)))]),
            ("no grace", vec![(px("grace"), Conc::Num(n(0)))]),
            ("dashing", vec![(px("dash_time"), Conc::Num(n(3)))]),
            ("dash effect", vec![(px("dash_effect_time"), Conc::Num(n(5)))]),
            ("frames", vec![(g("frames"), Conc::Num(n(29)))]),
            // Out of the TOP of the room (`this.y < -4`), which is
            // `next_room()` - a whole new object list, and the only way
            // to reach the largest of the output shapes.
            ("top edge", vec![(px("y"), Conc::Num(at(&px("y")) - n(120)))]),
            ("right edge", vec![(px("x"), Conc::Num(at(&px("x")) + n(124)))]),
            ("left edge", vec![(px("x"), Conc::Num(at(&px("x")) - n(16)))]),
        ] {
            perts.push((name.to_string(), over));
        }

        // WHAT are the outcomes? Shape divergence is the only thing that
        // can leave more than one, so two outcomes with the SAME shape
        // would be a `collapse` bug, and two with the same scalar count
        // but different shapes are worth looking at closely - that is how
        // the body-interning bug was found, where ten of twelve outcomes
        // were the same 110 scalars and differed only in `BodyId`s.
        {
            let mut by_len: std::collections::BTreeMap<usize, usize> = Default::default();
            for o in &f.outs {
                *by_len.entry(o.fields.len()).or_default() += 1;
            }
            eprintln!("[verify] outcomes by scalar count: {:?}", by_len);
            // The object list is what actually distinguishes them: a
            // player, a player_spawn, nothing at all, or a whole new
            // room's worth.
            for (i, o) in f.outs.iter().enumerate() {
                let mut objs: std::collections::BTreeSet<String> = Default::default();
                for (q, _, _) in &o.fields {
                    let pre = iface::show(&q[..q.len().saturating_sub(1)].to_vec());
                    if pre.starts_with("objects") {
                        objs.insert(pre);
                    }
                }
                eprintln!("[verify]   outcome {}: {} scalars, {:?}", i, o.fields.len(), objs);
            }
            let mut same = 0;
            for i in 0..f.outs.len() {
                for j in (i + 1)..f.outs.len() {
                    if f.outs[i].shape == f.outs[j].shape {
                        same += 1;
                    }
                }
            }
            assert_eq!(same, 0, "two outcomes with equal shapes did not merge");
            // Where the first same-size pair parts company.
            'pair: for i in 0..f.outs.len() {
                for j in (i + 1)..f.outs.len() {
                    let (a, b) = (&f.outs[i].shape, &f.outs[j].shape);
                    if f.outs[i].fields.len() != f.outs[j].fields.len() || a == b {
                        continue;
                    }
                    if a.tables.len() != b.tables.len() {
                        eprintln!("[verify] {} vs {}: {} tables vs {}", i, j, a.tables.len(), b.tables.len());
                        break 'pair;
                    }
                    if a.scopes.len() != b.scopes.len() {
                        eprintln!("[verify] {} vs {}: {} scopes vs {}", i, j, a.scopes.len(), b.scopes.len());
                        break 'pair;
                    }
                    for (k, (x, y)) in a.tables.iter().zip(b.tables.iter()).enumerate() {
                        if x != y {
                            eprintln!("[verify] {} vs {}: table {} differs\n   {:?}\n   {:?}", i, j, k, x, y);
                            break 'pair;
                        }
                    }
                    for (k, (x, y)) in a.scopes.iter().zip(b.scopes.iter()).enumerate() {
                        if x != y {
                            eprintln!("[verify] {} vs {}: scope {} differs\n   {:?}\n   {:?}", i, j, k, x, y);
                            break 'pair;
                        }
                    }
                }
            }
        }

        let mut checked = 0usize;
        let mut compared = 0usize;
        let mut declined = 0usize;
        let mut declined_at: std::collections::BTreeMap<String, usize> = Default::default();
        let mut used: std::collections::BTreeSet<usize> = Default::default();
        for (label, over) in &perts {
            let label = label.as_str();
            let cells = cells_with(&f.iface, over).expect("overrides name input cells");
            for mask in 0u8..64 {
                let mut bits = [false; 6];
                for (i, b) in bits.iter_mut().enumerate() {
                    *b = mask & (1 << i) != 0;
                }
                let mut o = st.clone();
                for (p, c) in over {
                    let v = match c {
                        Conc::Num(n) => Value::Num(it.d.num(*n)),
                        Conc::Bool(b) => Value::Bool(it.d.boolean(*b)),
                    };
                    iface::set(&mut o, p, v).expect("override a heap slot");
                }
                if let Err(e) = set_buttons(&mut it.d, &mut o, &bits) {
                    return eprintln!("[verify] {} {:?}: {:#}", label, bits, e);
                }
                let o = match run_one(&mut it, &frame, o) {
                    Ok(s) => s,
                    Err(e) => {
                        return eprintln!("[verify] oracle {} {:?} stopped at: {:#}", label, bits, e)
                    }
                };
                let want = match iface::read_concrete(&it.d, &o, &[]) {
                    Ok(w) => w,
                    Err(e) => {
                        return eprintln!("[verify] oracle {} {:?} not concrete: {:#}", label, bits, e)
                    }
                };
                match check_at(&it, &f, &cells, &bits, &want) {
                    Ok((which, n)) => {
                        checked += 1;
                        compared += n;
                        used.insert(which);
                    }
                    // A trace that declined a point is not a wrong
                    // answer, it is a refusal - count it and keep going,
                    // because how OFTEN it refuses is the number that
                    // matters and one panic would hide it.
                    Err(e) if format!("{}", e).contains("declined") => {
                        declined += 1;
                        *declined_at.entry(label.to_string()).or_default() += 1;
                    }
                    Err(e) => return eprintln!("[verify] MISMATCH {} {:#}", label, e),
                }
            }
        }
        eprintln!(
            "[verify] {} points agree ({} declined), {} field comparisons, {}/{} outcomes claimed something",
            checked,
            declined,
            compared,
            used.len(),
            f.outs.len()
        );
        eprintln!(
            "[verify] outcome scalar counts claimed: {:?}, never claimed: {:?}",
            used.iter().map(|i| f.outs[*i].fields.len()).collect::<Vec<_>>(),
            (0..f.outs.len())
                .filter(|i| !used.contains(i))
                .map(|i| f.outs[i].fields.len())
                .collect::<Vec<_>>()
        );
        if !declined_at.is_empty() {
            eprintln!(
                "[verify] declined at {} of the {} sweep points: {:?}",
                declined_at.len(),
                perts.len(),
                declined_at.keys().collect::<Vec<_>>()
            );
        }
        assert_eq!(checked + declined, perts.len() * 64, "every point should have been checked");
        assert!(compared > 0, "nothing was actually compared");
        // Every outcome the tracer produced has to be REACHABLE, or it
        // is a successor the program does not have. A new one that this
        // sweep cannot reach is a thing to explain - either extend the
        // sweep to reach it, or find out why the tracer kept it.
        assert_eq!(
            used.len(),
            f.outs.len(),
            "some outcome was never reached by the sweep"
        );
    }
}
