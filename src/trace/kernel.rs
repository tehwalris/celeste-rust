//! The traced-frame KERNEL pipeline's front half: trace a room's shapes,
//! bind and lower each one, and hand the result to the ASM backend
//! (`compiled::asm_kernel`, which retraces at startup and consumes
//! `lattice_kernel_refs` through `trace::emit::asm_fused`).
//!
//! `Reference` (one traced frame, everything owned), `room_kernels_in`
//! (trace and lower the start room's shapes), `room_constant_lattice`
//! (the per-shape constant fixpoint the specialization bakes in), and the
//! analysis probes (`specialize_probe`, `room_constants`) behind
//! `bin/transpile`.

use anyhow::Result;

use super::emit::{Bound, Lowered};
use super::verify::Frame;


/// One traced frame, everything owned, ready to run or to hand on.
///
/// The tracer's `Interp` borrows the parsed ASTs, so a caller outside
/// this crate cannot easily set one up. Nothing in here borrows them -
/// `Frame`, `Graph` and `Rt2` are all owned - so the whole thing can be
/// handed across a crate boundary, which is what the out-of-tree check
/// harness needs.
pub struct Reference {
    pub frame: Frame,
    pub graph: crate::transpile::graph::Graph,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// One kernel per heap SHAPE the room reaches - the CONSTANT-LATTICE
/// level-0 references (the production base set). The walk-based
/// generator this used to name was retired with the non-lattice sets
/// (plans/specialize.md "Spec: latticeify everything, all rooms, one
/// table").
///
/// No pm1 pin: a kernel covers every key of its shape (the pin is worth
/// -6.3% (T13) and costs a kernel per key). The LATTICE pins are a
/// different thing: fields constant across the room's reachable states,
/// guarded by `pin_guard` in `ok`.
///
/// REFUSES rather than returns a partial set. A shape the fixpoint could
/// not trace is a kernel that will not exist, and under the never-deopt
/// doctrine that is a run that stops. Better to fail here, where the
/// reason is in hand.
pub fn room_kernels_in(root: &std::path::Path) -> Result<Vec<Reference>> {
    lattice_kernel_refs(root, super::shapes::WalkOpts::LEVEL0)
}

/// Restate a lowering failure with its `Cell(n)`s NAMED.
///
/// The emitter works on the bound graph, where an input is a canonical
/// engine cell number and nothing downstream of the tracer knows what a
/// cell means. The interface does, so translating the numbers back is a
/// lookup - and the difference between "arms disagree in Sel(.., Cell(272))"
/// and knowing 272 is a particular object's field is the whole diagnosis.
fn name_cells(f: &super::verify::Frame, e: anyhow::Error) -> anyhow::Error {
    let msg = format!("{:#}", e);
    let mut seen: std::collections::BTreeSet<u32> = Default::default();
    let mut at = msg.as_str();
    while let Some(i) = at.find("Cell(") {
        at = &at[i + 5..];
        let end = at.find(')').unwrap_or(0);
        if let Ok(n) = at[..end].parse::<u32>() {
            seen.insert(n);
        }
    }
    let mut lines = Vec::new();
    for c in seen {
        let name = f
            .in_cells
            .iter()
            .position(|x| *x == c)
            .map(|i| super::iface::show(&f.iface.slots[i]))
            .unwrap_or_else(|| "not an input of this frame".to_string());
        lines.push(format!("  Cell({}) = {}", c, name));
    }
    anyhow::anyhow!("{}\nwhere\n{}", msg, lines.join("\n"))
}


/// The constant-lattice `Reference`s for the configured start room, in
/// the (deterministic) shape-key order of the fixpoint. A shape that
/// fails to bind, lower or render is FATAL: this is the sole production
/// generator, and a missing kernel is a runtime coverage gap under
/// strict mode, not a fallback.
pub(crate) fn lattice_kernel_refs(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<Vec<Reference>> {
    let mut lw = room_constant_lattice(root, opts)?;
    let room = crate::transpile::graph::Room { cart: lw.cart.clone(), cache: lw.cache.clone() };
    let mut refs: Vec<Reference> = Vec::new();
    for (i, (_k, f)) in std::mem::take(&mut lw.frames).into_iter().enumerate() {
        let bound = super::emit::bind(&f, &lw.graph, opts.widen)
            .map_err(|e| anyhow::anyhow!("lattice shape {} bind: {:#}", i, e))?;
        let lowered = super::emit::lower_frame(
            &bound.graph,
            &bound.outcomes,
            Some(room.clone()),
            bound.forks,
        )
        .map_err(|e| anyhow::anyhow!("lattice shape {} lower: {:#}", i, name_cells(&f, e)))?;
        refs.push(Reference {
            frame: f,
            graph: lw.graph.clone(),
            bound,
            lowered,
            cart: lw.cart.clone(),
            cache: lw.cache.clone(),
        });
    }
    Ok(refs)
}


#[cfg(test)]
mod tests {

    /// The ASM cutover's compute gate (plans/asm-and-posgraph-execution.md
    /// B): pull out the FUSED graph (`emit::asm_fused` ->
    /// `lower::specialize_frame`, the same fused, fork-free graph the Rust
    /// kernel is emitted from) and assemble it, for EVERY start-room shape.
    ///
    /// Because the graph is fused - every fork resolved to `Frag`/const,
    /// hash-consed - no `Free`/`Split` survives, so all real shapes compile
    /// and load, not just the fork-free ones. This proves the whole pipeline
    /// (trace -> specialize -> assemble) end-to-end on real kernel graphs.
    /// One assembly kernel per shape.
    #[test]
    fn every_start_room_kernel_graph_asm_compiles_the_fused_graph() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        assert!(!refs.is_empty(), "no start-room kernels traced");
        let mut total_roots = 0usize;
        for (si, r) in refs.iter().enumerate() {
            let room = crate::transpile::graph::Room {
                cart: r.cart.clone(),
                cache: r.cache.clone(),
            };
            let (fused, bodies, roots, reprs) =
                crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
                    .unwrap_or_else(|e| panic!("shape {si} fused extraction failed: {e:#}"));
            assert!(!bodies.is_empty(), "shape {si}: no bodies");
            assert!(!roots.is_empty(), "shape {si}: no roots");
            let (compiled, _loaded) = crate::transpile::asm::compile_and_load_reprs(
                &fused,
                &roots,
                &format!("fused{si}"),
                &reprs,
            )
            .unwrap_or_else(|e| {
                let msg = format!("{e:#}");
                // Pull "node N" out and report its consumers, so a domain
                // mismatch names the op that mishandled it.
                let mut consumers = String::new();
                if let Some(rest) = msg.split("node ").nth(1) {
                    if let Ok(n) = rest
                        .split(|c: char| !c.is_ascii_digit())
                        .next()
                        .unwrap_or("")
                        .parse::<u32>()
                    {
                        for id in 0..fused.len() as u32 {
                            if fused.get(id).args.contains(&n) {
                                consumers.push_str(&format!(
                                    " {}={:?}",
                                    id,
                                    fused.get(id).op
                                ));
                            }
                        }
                    }
                }
                panic!(
                    "shape {si}: fused asm compile+load failed: {msg}\n  consumers:{consumers}"
                )
            });
            assert_eq!(compiled.n_roots, roots.len(), "shape {si}: root count");
            total_roots += roots.len();
        }
        eprintln!(
            "[asm] {} start-room shapes: fused graph compiled+loaded, {} roots total",
            refs.len(),
            total_roots
        );
    }


}

// ---------------------------------------------------------------------------
// Specialization probe (plans/specialize.md). NOT production: it re-traces one
// shape with the player position, the springs, and a pm1 key pinned, and
// reports how far the graph collapses. Driven by `transpile --spec-probe`.
// ---------------------------------------------------------------------------

/// Pin the player XY, the springs' XY, and a pm1 key on one shape, re-trace,
/// and report the node/fork/body collapse against the unpinned base.
pub fn specialize_probe(
    root: &std::path::Path,
    shape_idx: usize,
    player_xy: (i16, i16),
    spring_xy: &[(i16, i16)],
) -> Result<String> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::iface::{self, Conc, Step};
    use super::verify::{run_one, trace_frame};
    use super::heap::Value;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    use celeste_core::pico8_num::Pico8Num as P8;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st0 = cart::fresh_state::<Symbolic>(&mut it.d);
    let st0 = run_one(&mut it, &top, st0)?;
    let mut st0 = st0;
    cart::inject_tile_flag_at(&mut st0);
    let st0 = run_one(&mut it, &init, st0)?;
    let w = shapes::walk(&mut it, &reset, &fr, st0, 400, shapes::WalkOpts::LEVEL0)?;

    if shape_idx >= w.shapes.len() {
        bail!("shape {} out of range ({} shapes)", shape_idx, w.shapes.len());
    }
    let st = w.shapes[shape_idx].state.clone();
    let base_forks = w.shapes[shape_idx].frame.forks;

    // Locate the player and the spring objects by type.
    let objects_of = |st: &super::state::State<Symbolic>, name: &str| -> Vec<Vec<Step>> {
        let mut out = Vec::new();
        let Some(Value::Table(want)) = iface::get(st, &[iface::key(name)]) else { return out };
        let Some(Value::Table(objects)) = iface::get(st, &[iface::key("objects")]) else { return out };
        let n = st.heap.tables[&objects].arr.len();
        for i in 0..n {
            let base = vec![iface::key("objects"), Step::Idx(i)];
            let mut ty = base.clone();
            ty.push(iface::key("type"));
            if iface::get(st, &ty) == Some(Value::Table(want)) { out.push(base); }
        }
        out
    };
    // Object-identity check: for each spring, does its `collide`
    // closure's captured `obj` point to the same table as the live
    // spring? (CELESTE_SPEC_OBJID)
    if std::env::var("CELESTE_SPEC_OBJID").is_ok() {
        let springs_ck = objects_of(&st, "spring");
        for (i, sp) in springs_ck.iter().enumerate() {
            let live_tid = match iface::get(&st, sp) { Some(Value::Table(t)) => t, _ => { eprintln!("spring {} not a table", i); continue } };
            // the collide closure
            let mut cp = sp.clone(); cp.push(iface::key("collide"));
            let cl = match iface::get(&st, &cp) { Some(Value::Func(c)) => c, other => { eprintln!("spring {} collide = {:?}", i, other); continue } };
            let env = st.heap.closures.get(&cl).map(|c| c.env);
            let cap_obj = env.and_then(|e| st.heap.scopes.get(&e)).and_then(|sc| sc.vars.get("obj")).cloned();
            let cap_tid = match cap_obj { Some(Value::Table(t)) => Some(t), _ => None };
            eprintln!("spring {}: live table {}, collide.env captured obj = {:?} (match: {})",
                i, live_tid, cap_tid, cap_tid == Some(live_tid));
            // also print the captured obj's x vs live x
            if let Some(ct) = cap_tid {
                let cap_x = st.heap.tables.get(&ct).and_then(|t| t.hash.get("x")).cloned();
                let live_x = st.heap.tables.get(&live_tid).and_then(|t| t.hash.get("x")).cloned();
                eprintln!("    captured obj.x node = {:?}, live obj.x node = {:?}", cap_x.map(|v| format!("{:?}", v)), live_x.map(|v| format!("{:?}", v)));
            }
        }
    }
    let players = objects_of(&st, "player");
    let springs = objects_of(&st, "spring");
    let fld = |base: &[Step], f: &[&str]| -> Vec<Step> {
        let mut p = base.to_vec();
        for x in f { p.push(iface::key(x)); }
        p
    };
    let num = |v: i16| Conc::Num(P8::from_i16(v));

    let mut pin: Vec<(Vec<Step>, Conc)> = Vec::new();
    // The player position.
    let nopos = std::env::var("CELESTE_SPEC_NOPOS").is_ok();
    if let Some(pl) = players.first() {
        if !nopos { pin.push((fld(pl, &["x"]), num(player_xy.0))); pin.push((fld(pl, &["y"]), num(player_xy.1))); }
        // pm1 key on the player, canonical "steady" values.
        pin.push((fld(pl, &["dash_time"]), num(0)));
        pin.push((fld(pl, &["p_dash"]), Conc::Bool(false)));
        pin.push((fld(pl, &["p_jump"]), Conc::Bool(false)));
    }
    // Optional extra pin groups, so the graph's fork sources can be
    // isolated without recompiling. CELESTE_SPEC_GROUPS is a comma list:
    //   spd        - pin the player spd.x/spd.y to 0
    //   springall  - pin every spring scalar (freeze the springs)
    //   playerall  - pin every player scalar except rem (the fork input)
    let groups: std::collections::HashSet<String> = std::env::var("CELESTE_SPEC_GROUPS")
        .unwrap_or_default().split(',').map(|s| s.trim().to_string()).collect();
    for (i, sp) in springs.iter().enumerate() {
        if let Some((x, y)) = spring_xy.get(i) {
            pin.push((fld(sp, &["x"]), num(*x)));
            pin.push((fld(sp, &["y"]), num(*y)));
        }
        // CELESTE_SPEC_SFIELDS: dotted spring field paths to pin, e.g.
        // "spr,hide_for,hide_in,delay". spr defaults to 18 (active).
        for spec in std::env::var("CELESTE_SPEC_SFIELDS").unwrap_or_default().split(',') {
            let spec = spec.trim();
            if spec.is_empty() { continue; }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(sp, &parts);
            let v = if spec == "spr" { num(18) } else { num(0) };
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(false))),
                    Some(Value::Num(_)) => pin.push((path, v)),
                    _ => {}
                }
            }
        }
        if groups.contains("springall") {
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                if f.starts_with(sp) && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    if let Some(pl) = players.first() {
        if groups.contains("spd") {
            pin.push((fld(pl, &["spd", "x"]), num(0)));
            pin.push((fld(pl, &["spd", "y"]), num(0)));
        }
        // CELESTE_SPEC_PFIELDS: dotted player field paths to pin to 0,
        // e.g. "spd.x,spd.y,djump,grace,dash_effect_time".
        for spec in std::env::var("CELESTE_SPEC_PFIELDS").unwrap_or_default().split(',') {
            let mut spec = spec.trim();
            if spec.is_empty() { continue; }
            // trailing "=1" pins a bool to true.
            let want_true = spec.ends_with("=1");
            if want_true { spec = &spec[..spec.len()-2]; }
            let mut numval = 0i16;
            if let Some((f, v)) = spec.split_once(':') { spec = f; numval = v.parse().unwrap_or(0); }
            let parts: Vec<&str> = spec.split('.').collect();
            let path = fld(pl, &parts);
            if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r == &path) && !pin.iter().any(|(q,_)| q==&path) {
                match iface::get(&st, &path) {
                    Some(Value::Bool(_)) => pin.push((path, Conc::Bool(want_true))),
                    Some(Value::Num(_)) => pin.push((path, num(numval))),
                    _ => {}
                }
            }
        }
        if groups.contains("playerall") {
            for f in super::shapes::state_paths(&st).unwrap_or_default() {
                let is_rem = f.starts_with(pl) && f.iter().any(|s| format!("{:?}", s).contains("rem"));
                if f.starts_with(pl) && !is_rem && !pin.iter().any(|(q, _)| q == &f) {
                    match iface::get(&st, &f) {
                        Some(Value::Bool(_)) => pin.push((f, Conc::Bool(false))),
                        Some(Value::Num(_)) => pin.push((f, num(0))),
                        _ => {}
                    }
                }
            }
        }
    }
    // CELESTE_SPEC_ALLGEOM: pin EVERY object's x,y and hitbox to
    // constants (spread positions, default hitbox) - tests whether
    // baking in the static object geometry folds the collision graph.
    if std::env::var("CELESTE_SPEC_ALLGEOM").is_ok() {
        let all: Vec<Vec<Step>> = {
            let mut v = Vec::new();
            if let Some(Value::Table(objs)) = iface::get(&st, &[iface::key("objects")]) {
                let n = st.heap.tables[&objs].arr.len();
                for i in 0..n { v.push(vec![iface::key("objects"), Step::Idx(i)]); }
            }
            v
        };
        let player_bases: Vec<Vec<Step>> = players.clone();
        for (i, ob) in all.iter().enumerate() {
            if player_bases.iter().any(|pb| pb == ob) { continue; }
            for (f, v) in [("x", (i as i16) * 12 + 4), ("y", 40)] {
                let path = fld(ob, &[f]);
                if super::shapes::state_paths(&st).unwrap_or_default().iter().any(|r| r==&path) && !pin.iter().any(|(q,_)| q==&path) {
                    pin.push((path, num(v)));
                }
            }
            // hitboxes are pinned by CELESTE_SPEC_HITBOX, not here.
        }
    }
    // CELESTE_SPEC_HITBOX: pin player hitbox (1,3,6,5) and spring
    // hitbox (0,0,8,8) - the type-fixed values - to test whether the
    // symbolic hitbox is what blocks collide from folding.
    if std::env::var("CELESTE_SPEC_HITBOX").is_ok() {
        if let Some(pl) = players.first() {
            for (f, v) in [("x", 1), ("y", 3), ("w", 6), ("h", 5)] {
                pin.push((fld(pl, &["hitbox", f]), num(v)));
            }
        }
        for sp in &springs {
            for (f, v) in [("x", 0), ("y", 0), ("w", 8), ("h", 8)] {
                pin.push((fld(sp, &["hitbox", f]), num(v)));
            }
        }
    }
    // pm1 globals.
    pin.push((vec![iface::key("freeze")], num(0)));
    pin.push((vec![iface::key("has_dashed")], Conc::Bool(false)));

    // Only keep pins whose path is actually a scalar in this state.
    let roots = shapes::state_paths(&st)?;
    let ival = shapes::ival_paths(&st);
    pin.retain(|(p, _)| roots.iter().any(|r| r == p));

    let pinned_paths: Vec<String> = pin.iter().map(|(p, _)| iface::show(p)).collect();

    // Measure one traced frame: reachable node count, distinct LIVE
    // forks (Op::Split still referenced), and an op census.
    fn measure(g: &crate::transpile::graph::Graph, f: &super::verify::Frame) -> (usize, std::collections::BTreeSet<u8>, Vec<String>) {
        use crate::transpile::graph::Op;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs {
            for (_, nd, _) in &o.fields { roots_n.push(*nd); }
            roots_n.push(o.guard);
            roots_n.push(o.ok);
        }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let nodes = reach.iter().filter(|b| **b).count();
        let mut forks = std::collections::BTreeSet::new();
        let mut census: std::collections::BTreeMap<String, usize> = Default::default();
        for id in 0..g.len() {
            if reach[id] {
                let op = &g.get(id as u32).op;
                if let Op::Split(d) | Op::SplitValid(d) = op { forks.insert(*d); }
                *census.entry(format!("{:?}", op).split('(').next().unwrap().to_string()).or_default() += 1;
            }
        }
        let mut v: Vec<_> = census.into_iter().collect();
        v.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        let top = v.into_iter().take(12).map(|(k, n)| format!("{} {}", k, n)).collect();
        (nodes, forks, top)
    }

    // Base: same shape, NO pins.
    let base = trace_frame(&mut it, &reset, &fr, st.clone(), &roots, &[], &ival, Some(crate::trace::widen::WidenMode::Level0))
        .map_err(|e| anyhow!("base trace of shape {}: {:#}", shape_idx, e))?;
    let (bn, bf, _) = measure(&it.d.graph, &base);

    // Pinned.
    let f = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, Some(crate::trace::widen::WidenMode::Level0))
        .map_err(|e| anyhow!("pinned trace of shape {}: {:#}", shape_idx, e))?;
    let (pn, pf, ptop) = measure(&it.d.graph, &f);

    // Slot -> path, so Cell(i) in the dump can be read.
    if std::env::var("CELESTE_SPEC_SLOTS").is_ok() {
        for (i, sp) in f.iface.slots.iter().enumerate() {
            eprintln!("  slot {} = {}", i, iface::show(sp));
        }
    }
    let mut out = String::new();
    out.push_str(&format!("shape {}: {} outcomes, {} players, {} springs\n", shape_idx, f.outs.len(), players.len(), springs.len()));
    out.push_str(&format!("  pinned: {}\n", pinned_paths.join(", ")));
    out.push_str(&format!("  BASE   : {} nodes, {} live forks {:?}, counter {}\n", bn, bf.len(), bf, base_forks));
    out.push_str(&format!("  PINNED : {} nodes, {} live forks {:?}, counter {}\n", pn, pf.len(), pf, f.forks));
    out.push_str(&format!("  pinned op census: {}\n", ptop.join(", ")));
    // CELESTE_SPEC_CMPS: dump symbolic comparison nodes (box tests etc).
    if std::env::var("CELESTE_SPEC_CMPS").is_ok() {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        let mut seen = std::collections::BTreeSet::new();
        for id in 0..g.len() {
            if reach[id] {
                if matches!(g.get(id as u32).op, Op::Gt|Op::Ge|Op::Lt|Op::Le) {
                    let t = super::emit::show_tree(g, id as u32, 4);
                    if seen.insert(t.clone()) { out.push_str(&format!("  cmp: {}\n", t)); }
                }
            }
        }
    }
    // CELESTE_SPEC_BTNFORKS: for each of the 64 button assignments,
    // resolve the buttons (specialize_config_into) and count how many
    // forks remain LIVE. If per-button the count is ~2 (rem x,y), the
    // emitter's 2^fork_depth enumeration is over-counting per config.
    if std::env::var("CELESTE_SPEC_BTNFORKS").is_ok() {
        use crate::transpile::graph::{Graph, Op};
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let mut hist: std::collections::BTreeMap<usize, usize> = Default::default();
        for m in 0u8..64 {
            let mut sp = Graph::new();
            let mapped = g.specialize_config_into(m, None, &mut sp);
            let sroots: Vec<_> = roots_n.iter().map(|r| mapped[*r as usize]).collect();
            let reach = crate::transpile::bdd::reachable(&sp, &sroots);
            let mut forks = std::collections::BTreeSet::new();
            for id in 0..sp.len() {
                if reach[id] { if let Op::Split(d) = sp.get(id as u32).op { forks.insert(d); } }
            }
            *hist.entry(forks.len()).or_default() += 1;
        }
        out.push_str(&format!("  live forks per button assignment (fork_depth {}): {:?}\n", f.forks, hist));
    }
    // What each LIVE fork forks on.
    {
        use crate::transpile::graph::Op;
        let g = &it.d.graph;
        let mut roots_n: Vec<crate::transpile::graph::NodeId> = Vec::new();
        for o in &f.outs { for (_, nd, _) in &o.fields { roots_n.push(*nd); } roots_n.push(o.guard); roots_n.push(o.ok); }
        let reach = crate::transpile::bdd::reachable(g, &roots_n);
        for id in 0..g.len() {
            if reach[id] {
                if let Op::Split(d) = g.get(id as u32).op {
                    let operand = g.get(id as u32).args[0];
                    out.push_str(&format!("  fork {}: {}\n", d, super::emit::show_tree(g, operand, 8)));
                }
            }
        }
    }
    Ok(out)
}

/// The per-shape constant lattice by FIXPOINT (plans/specialize.md D1).
///
/// Seeds from the concrete spawn state (all fields constant) and traces
/// forward keeping each shape's known-constant fields CONCRETE (pinned)
/// rather than abstracting them - so a field the frame never changes
/// (spring `spd`, static positions) stays a constant and folds the
/// collisions that depend on it. On reaching a shape, the outcome's
/// constant fields are INTERSECTED into that shape's lattice; a shape
/// whose lattice shrinks is re-processed. Monotone (fields only go
/// constant->abstract), so it terminates.
///
/// Returns `(shape key -> constant field map)` plus a blanked
/// representative state per shape for later re-tracing.
///
/// `opts` is the widening axis (plans/kernel-ladder.md): the fixpoint
/// and every traced frame run under the variant's own options, so the
/// shape set and the emitted frames are self-consistent per variant.
/// The baked constants come out the same either way - a field the
/// boundary widens is excluded from `field_constants` regardless - but
/// EXACT's shapes differ (rem as a plain number), so the fixpoint must
/// see the variant it generates for.
pub fn room_constant_lattice(
    root: &std::path::Path,
    opts: super::shapes::WalkOpts,
) -> Result<LatticeWalk> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{run_one, trace_frame};
    use super::domain::Domain;
    use super::{cart, shapes};
    use anyhow::{anyhow, bail};
    type Cmap = std::collections::BTreeMap<super::iface::Path, super::iface::Conc>;

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()").map_err(|e| anyhow!("reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(&cart_data, rx, ry)?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let st = run_one(&mut it, &top, st)?;
    let mut st = st;
    cart::inject_tile_flag_at(&mut st);
    let start = run_one(&mut it, &init, st)?;

    let key = |st: &super::state::State<Symbolic>| -> Result<String> { Ok(format!("{:?}", st.shape()?)) };

    let mut lattice: std::collections::BTreeMap<String, Cmap> = Default::default();
    let mut reps: std::collections::BTreeMap<String, super::state::State<Symbolic>> = Default::default();
    let mut forks: std::collections::BTreeMap<String, usize> = Default::default();
    let mut frames: std::collections::BTreeMap<String, super::verify::Frame> = Default::default();
    let mut forkops: std::collections::BTreeMap<String, Vec<String>> = Default::default();
    let mut refused: std::collections::BTreeMap<String, String> = Default::default();

    let sk = key(&start)?;
    lattice.insert(sk.clone(), shapes::field_constants(&start, &it.d)?);
    reps.insert(sk.clone(), start.clone());
    let mut work: Vec<String> = vec![sk];
    let room0 = shapes::room_of(&start, &it.d);
    let mut guard = 0usize;

    while let Some(k) = work.pop() {
        guard += 1;
        if guard > 20000 { bail!("constant-lattice fixpoint did not converge"); }
        let st = reps[&k].clone();
        let roots = shapes::state_paths(&st)?;
        let ival = if opts.ival { shapes::ival_paths(&st) } else { Vec::new() };
        // Pin the shape's known constants (only those that are real scalar
        // inputs here), everything else abstract.
        let pin: Vec<(super::iface::Path, super::iface::Conc)> = lattice[&k]
            .iter()
            .filter(|(p, _)| roots.iter().any(|r| r == *p))
            .map(|(p, c)| (p.clone(), *c))
            .collect();
        let f = match trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, opts.widen_mode()) {
            Ok(f) => f,
            Err(e) => {
                // Remember the refusal instead of silently skipping: a
                // shape that never traces is a MISSING KERNEL, and the
                // post-fixpoint check below turns that into a hard
                // error. (A refusal on an early pass that a later
                // re-trace of the same shape survives is fine - the
                // successful frame lands in `frames`.)
                refused.insert(k.clone(), format!("{:#}", e));
                continue;
            }
        };
        refused.remove(&k);
        // Live forks of THIS (converged-so-far) trace of shape k.
        {
            use crate::transpile::graph::Op;
            let mut rn: Vec<crate::transpile::graph::NodeId> = Vec::new();
            for o in &f.outs { for (_, nd, _) in &o.fields { rn.push(*nd); } rn.push(o.guard); rn.push(o.ok); }
            let reach = crate::transpile::bdd::reachable(&it.d.graph, &rn);
            let mut fs = std::collections::BTreeSet::new();
            for id in 0..it.d.graph.len() { if reach[id] { if let Op::Split(d) = it.d.graph.get(id as u32).op { fs.insert(d); } } }
            forks.insert(k.clone(), fs.len());
            if fs.len() > 2 {
                let mut ops = Vec::new();
                for id in 0..it.d.graph.len() {
                    if reach[id] { if let Op::Split(_) = it.d.graph.get(id as u32).op {
                        let operand = it.d.graph.get(id as u32).args[0];
                        ops.push(super::emit::show_tree(&it.d.graph, operand, 5));
                    } }
                }
                forkops.insert(k.clone(), ops);
            }
        }
        // Keep the converged frame for generation (last trace wins).
        for o in &f.outs {
            if it.d.decide(&o.ok) == Some(false) { continue; }
            if shapes::room_of(&o.st, &it.d) != room0 { continue; }
            let tk = key(&o.st)?;
            let fc = shapes::field_constants(&o.st, &it.d)?;
            let changed = match lattice.get_mut(&tk) {
                None => {
                    lattice.insert(tk.clone(), fc);
                    let mut rep = o.st.clone();
                    shapes::blank(&mut rep, &mut it.d)?;
                    reps.insert(tk.clone(), rep);
                    true
                }
                Some(m) => {
                    let before = m.len();
                    m.retain(|p, v| fc.get(p) == Some(v));
                    m.len() != before
                }
            };
            if changed && !work.contains(&tk) { work.push(tk); }
        }
        frames.insert(k.clone(), f);
    }
    // Every reachable shape must have a frame. A shape whose every
    // trace refused would otherwise just be MISSING from the generated
    // set - a silent runtime coverage gap, fatal under strict mode and
    // invisible until then.
    let missing: Vec<String> = lattice
        .keys()
        .filter(|k| !frames.contains_key(*k))
        .map(|k| {
            format!(
                "shape {}: {}",
                k,
                refused.get(k).map(String::as_str).unwrap_or("never traced")
            )
        })
        .collect();
    if !missing.is_empty() {
        bail!(
            "the constant-lattice fixpoint could not trace {} of {} shapes:\n{}",
            missing.len(),
            lattice.len(),
            missing.join("\n")
        );
    }
    let graph = std::mem::take(&mut it.d.graph);
    Ok(LatticeWalk { lattice, reps, forks, frames, graph, cart: cart_data, cache, forkops })
}

/// The output of `room_constant_lattice`.
pub struct LatticeWalk {
    pub lattice: std::collections::BTreeMap<String, std::collections::BTreeMap<super::iface::Path, super::iface::Conc>>,
    pub reps: std::collections::BTreeMap<String, super::state::State<super::domain::Symbolic>>,
    pub forks: std::collections::BTreeMap<String, usize>,
    pub frames: std::collections::BTreeMap<String, super::verify::Frame>,
    pub graph: crate::transpile::graph::Graph,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
    pub forkops: std::collections::BTreeMap<String, Vec<String>>,
}

/// Report the constant lattice for `transpile --room-consts`.
pub fn room_constants(root: &std::path::Path) -> Result<String> {
    let LatticeWalk { lattice, forks, mut frames, graph, cart, cache, forkops, .. } =
        room_constant_lattice(root, super::shapes::WalkOpts::LEVEL0)?;
    let room = crate::transpile::graph::Room { cart: cart.clone(), cache: cache.clone() };
    // Bind+lower each converged frame to get the emitted size.
    let mut bodies_by_shape: std::collections::BTreeMap<String, usize> = Default::default();
    for (k, f) in frames.iter_mut() {
        if let Ok(bound) = super::emit::bind(f, &graph, true) {
            if let Ok(low) = super::emit::lower_frame(&bound.graph, &bound.outcomes, Some(room.clone()), bound.forks) {
                bodies_by_shape.insert(k.clone(), low.bodies);
            }
        }
    }
    let mut out = String::new();
    out.push_str(&format!("{} shapes reached (constant-lattice fixpoint)\n", lattice.len()));
    for (i, (k, cm)) in lattice.iter().enumerate() {
        let spd: Vec<String> = cm.keys().map(super::iface::show)
            .filter(|s| s.contains("objects[") && s.contains(".spd")).collect();
        out.push_str(&format!("shape {}: {} const fields, {} forks, {} bodies\n",
            i, cm.len(), forks.get(k).copied().unwrap_or(999), bodies_by_shape.get(k).copied().unwrap_or(0)));
        if let Some(ops) = forkops.get(k) {
            for o in ops.iter().take(4) { out.push_str(&format!("    fork: {}\n", o)); }
        }
        let _ = spd;
    }
    out.push_str(&format!("total bodies (lattice): {}\n", bodies_by_shape.values().sum::<usize>()));
    Ok(out)
}
