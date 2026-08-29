//! The traced-frame KERNEL pipeline's front half: trace a room's shapes,
//! bind and lower each one, and hand the result on.
//!
//! The back half used to be here too - `render`, which assembled a
//! lowered frame into the checked-in per-room Rust kernel crates, plus
//! the `write_room_kernels*`/`merge_kernel_sets` file plumbing around
//! it. That whole emitter went with the generated crates (2026-08-29,
//! plans/asm-and-posgraph-execution.md B): the runtime backend is now
//! `compiled::asm_kernel`, which retraces at startup and consumes the
//! SAME `lattice_kernel_refs` output this module still produces, via
//! `trace::emit::asm_fused` instead of rendered Rust source.
//!
//! What remains: `Reference` (one traced frame, everything owned),
//! `reference_frame`/`room_kernels_in` (trace and lower the start
//! room's shapes), `input_block`, `room_constant_lattice` (the
//! per-shape constant fixpoint the specialization bakes in), and the
//! analysis probes (`specialize_probe`, `room_constants`) behind
//! `bin/transpile`.

use anyhow::Result;

use celeste_engine::runtime2::{Col, AV};

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

/// Trace the frame this kernel is rendered from: room (1,0), warmed up
/// to the first frame that has a player, pinned to its pm1 key.
pub fn reference_frame() -> Result<Reference> {
    reference_frame_in(std::path::Path::new("."))
}

/// As `reference_frame`, with the repo root given explicitly - the run
/// check is a crate outside the workspace, so its working directory is
/// not the repo root.
pub fn reference_frame_in(root: &std::path::Path) -> Result<Reference> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{pm1_key, run_one, trace_frame};
    use super::{cart, iface};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse(cart::FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let mut st = run_one(&mut it, &init, st)?;
    let mut player: Option<super::iface::Path> = None;
    for _ in 0..40 {
        if let Some(p) = super::verify::find_player(&st) {
            player = Some(p);
            break;
        }
        st = run_one(&mut it, &fr, st)?;
    }
    let Some(player) = player else { bail!("no player after 40 frames") };

    let mut roots = vec![player.clone()];
    for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
        roots.push(vec![iface::key(g)]);
    }
    let pin = pm1_key(&player, &st, &it.d)?;
    let frame = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &[], false)?;

    let graph = std::mem::take(&mut it.d.graph);
    let bound = super::emit::bind(&frame, &graph, true)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let lowered = super::emit::lower_frame(
        &bound.graph,
        &bound.inputs,
        &bound.uni,
        &bound.outcomes,
        Some(room),
        bound.forks,
    )?;
    Ok(Reference { frame, graph, bound, lowered, cart: cart_data, cache })
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

/// A block holding `rows` concrete input assignments, one per lane.
///
/// The kernel reads its inputs off a block by path, so checking it
/// against the graph needs a block that holds exactly the values the
/// graph is evaluated at. This builds one: the traced input shape, with
/// each row's values written into the cells the interface named.
pub fn input_block(r: &Reference, rows: &[Vec<super::iface::Conc>]) -> Result<celeste_engine::Rt2> {
    use super::iface::Conc;

    let mut b = celeste_engine::slots::reshape(&r.frame.in_rt2, rows.len());
    // The BLOCK-UNIFORM inputs go in as uniform columns, because that is
    // what the kernel's `bind` accepts - it takes them as `P8`, not as a
    // lane array, and a per-lane column there is a narrowing it refuses.
    // Every row therefore has to agree about them, and this says so
    // rather than silently taking row 0.
    let uni: std::collections::BTreeSet<u32> = r.bound.uni.iter().map(|(c, _)| *c).collect();
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if !uni.contains(cell) {
            continue;
        }
        for (k, row) in rows.iter().enumerate() {
            if row[i] != rows[0][i] {
                anyhow::bail!(
                    "row {} disagrees with row 0 about the block-uniform slot {}",
                    k,
                    super::iface::show(&r.frame.iface.slots[i])
                );
            }
        }
        b.cols[*cell as usize] = Col::U(match rows[0][i] {
            Conc::Num(v) => AV::Num(v),
            Conc::Bool(v) => AV::Bool(v),
        });
    }
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if uni.contains(cell) {
            continue;
        }
        let col = match rows[0].get(i) {
            Some(Conc::Num(_)) => Col::N(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Num(v) => v,
                        Conc::Bool(_) => celeste_core::pico8_num::Pico8Num::from_i16(0),
                    })
                    .collect(),
            ),
            Some(Conc::Bool(_)) => Col::V(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Bool(v) => AV::Bool(v),
                        Conc::Num(_) => AV::Nil,
                    })
                    .collect(),
            ),
            None => anyhow::bail!("row is shorter than the interface"),
        };
        b.cols[*cell as usize] = col;
    }
    Ok(b)
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
            &bound.inputs,
            &bound.uni,
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

    /// DEBUG probe (Bits(2) divergence): dump each LADDER kernel shape's
    /// `player.rem` output node - is it a passthrough (`Op::Cell`) or the
    /// real `rem += spd; rem -= flr(rem+0.5)` update DAG?
    #[test]
    #[ignore]
    fn dump_rem_output_node() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists() {
            return;
        }
        let refs = super::lattice_kernel_refs(
            std::path::Path::new("."),
            crate::trace::shapes::WalkOpts::LADDER,
        )
        .expect("trace ladder");
        for (si, r) in refs.iter().enumerate() {
            if si != 1 {
                continue;
            }
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                let has_ival = o.outputs.iter().any(|(_, _, ty)| ty.contains("ZI"));
                if !has_ival {
                    continue;
                }
                eprintln!(
                    "shape {si} outcome {oi} OK node {}:\n  {}",
                    o.ok,
                    crate::trace::emit::show_tree(&r.bound.graph, o.ok, 9),
                );
            }
        }
    }

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


    /// How much of the row amplification is STATICALLY removable?
    ///
    /// `variants` dedups on the whole `(outputs, live, ok)` tuple, so two
    /// button assignments that write identical values and differ only in
    /// which lanes they claim stay separate - and each appends its own
    /// copy of the same row. Grouping PER OUTCOME on `(outputs, ok, bd)`
    /// and unioning the live masks would remove those before they are
    /// written.
    ///
    /// This counts what that would save, from the expressions alone, so
    /// the restructuring is sized before it is built rather than after.
    #[test]
    #[ignore]
    fn how_many_variants_write_the_same_row() {
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let n = r.lowered.variants.len();
            let outcomes = r.lowered.outs.len();
            let mut per: Vec<usize> = Vec::new();
            for oi in 0..outcomes {
                // OUTPUTS only. `ok` is a per-variant variable NAME
                // (`ok_v{mask}`), so including it groups nothing - it
                // was 1.0x on the first attempt for exactly that reason.
                //
                // Dropping it is not a shortcut: the rows a variant
                // appends are `live & !deopt`, and two variants with
                // identical output VALUES append identical rows, so the
                // union of their take masks is exact whatever their `ok`
                // says. Deopt REPORTING still ORs per variant, which is
                // a separate quantity.
                let mut groups: std::collections::BTreeSet<String> = Default::default();
                for v in &r.lowered.variants {
                    let Some(p) = v.per[oi].as_ref() else {
                        continue;
                    };
                    let outs: Vec<String> =
                        p.outputs.iter().map(|(c, e)| format!("{}={}", c, e)).collect();
                    groups.insert(outs.join(";"));
                }
                per.push(groups.len());
            }
            let total: usize = per.iter().sum();
            eprintln!(
                "[dedup] shape {}: {} variants x {} outcomes = {} appends; \
                 grouped {:?} = {} ({:.1}x fewer)",
                si,
                n,
                outcomes,
                n * outcomes,
                per,
                total,
                (n * outcomes) as f64 / total.max(1) as f64
            );
        }
    }

    /// How much of a frame exists only to compute values the boundary
    /// ERASES a moment later?
    ///
    /// `player.rem.x/y` are overwritten with the widened constant, and
    /// the four timers are pinned to 0. The frame computes all of them -
    /// through the fork, in `rem`'s case - writes them into the output
    /// block, and the boundary discards them.
    ///
    /// Philippe's proposal is to compile the widenings INTO the kernel,
    /// at which point those chains are dead and can be deleted. This
    /// counts what that would remove.
    #[test]
    #[ignore]
    fn how_much_of_a_frame_is_erased_immediately() {
        use crate::transpile::graph::NodeId;
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        let erased = ["rem.x", "rem.y", "frames", "seconds", "minutes", "deaths"];
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let reach = |roots: &[NodeId]| -> usize {
                let mut seen = vec![false; g.len()];
                let mut st = roots.to_vec();
                let mut n = 0;
                while let Some(x) = st.pop() {
                    if seen[x as usize] {
                        continue;
                    }
                    seen[x as usize] = true;
                    n += 1;
                    st.extend(g.get(x).args.iter().copied());
                }
                n
            };
            let mut all: Vec<NodeId> = Vec::new();
            let mut kept: Vec<NodeId> = Vec::new();
            let mut dropped = 0;
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                for (k, (cell, node, _)) in o.outputs.iter().enumerate() {
                    let _ = cell;
                    all.push(*node);
                    let path = crate::trace::iface::show(&r.frame.outs[oi].fields[k].0);
                    if erased.iter().any(|e| path.ends_with(e)) {
                        dropped += 1;
                    } else {
                        kept.push(*node);
                    }
                }
                all.push(o.live);
                all.push(o.ok);
                kept.push(o.live);
                kept.push(o.ok);
            }
            let (a, k) = (reach(&all), reach(&kept));
            eprintln!(
                "[erase] shape {}: {} nodes reachable from all outputs, {} without the {} \
                 erased cells -> {} nodes ({:.1}%) exist only for values the boundary discards",
                si, a, k, dropped, a - k, 100.0 * (a - k) as f64 / a as f64
            );
        }
    }

    /// What specializing the fork WOULD cost, in nodes - the measurement
    /// that refuted it (2026-08-23).
    ///
    /// The runtime loop runs the tail of the frame once per fork
    /// configuration; specialization runs each configuration's nodes
    /// once, sharing everything the configurations agree on. Which is
    /// cheaper is entirely a question of how much of the frame is
    /// downstream of the split, and that is a reachability fact this
    /// answers before anything is built.
    ///
    /// Reported per shape: the specialized arena's live node count with
    /// the splits left standing (what is emitted today, inside a loop)
    /// against the same count with all 2^forks configurations resolved.
    ///
    /// The room (1,0) answer - 3.92x of a maximum 4.00x, 1.9% shared -
    /// was WRONG TWICE, and the wrongness is the reason this prints as
    /// much as it does now. It ran on the only room that existed, which
    /// forks twice and is the weakest point on the curve; and it blended
    /// all 64 button assignments into the arena, so button divergence
    /// dominated and the fork question was never actually asked.
    ///
    /// Holding the buttons fixed and running room (2,0), the ratio is
    /// flat at ~2.3-3.2x while the configuration count goes 4 -> 16 ->
    /// 256, i.e. sharing goes 40.6% -> 82-86% -> 98.7-99.0%.
    /// Specialization gets CHEAPER, relatively, the more forks there
    /// are.
    ///
    /// Node counts alone do not decide it, so this also reports the
    /// runtime comparison. The loop re-executes every node above its
    /// fork level once per configuration, so its work is
    /// `sum_l count_l * 2^l`; the flat form executes each node once, so
    /// its work IS its node count. Those two are the numbers that
    /// matter, and the level histogram says where a shape sits between
    /// them.
    #[test]
    #[ignore]
    fn what_specializing_the_fork_would_cost() {
        use crate::transpile::graph::{Graph, NodeId};
        let refs = match super::room_kernels_in(std::path::Path::new(".")) {
            Ok(r) => r,
            Err(e) => panic!("{:#}", e),
        };
        for (si, r) in refs.iter().enumerate() {
            let g = &r.bound.graph;
            let forks = r.bound.forks;
            let base: Vec<NodeId> = {
                let mut v = Vec::new();
                for o in r.bound.outcomes.iter() {
                    v.extend(o.outputs.iter().map(|(_, n, _)| *n));
                    v.push(o.live);
                    v.push(o.ok);
                }
                v
            };
            let live_count = |arena: &Graph, roots: &[NodeId]| -> usize {
                crate::transpile::bdd::reachable(arena, roots)
                    .iter()
                    .filter(|b| **b)
                    .count()
            };
            // Today: ONE assignment, splits left as nodes. One rather
            // than all 64, because the question is what the FORK
            // configurations share, and holding the buttons fixed is
            // what isolates it - mixing both makes the ratio a blend of
            // two effects.
            let mut loop_arena = Graph::new();
            let mut loop_roots: Vec<NodeId> = Vec::new();
            {
                let map = g.specialize_config_into(0, None, &mut loop_arena);
                loop_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            // Specialized: ONE assignment x 2^forks configurations.
            //
            // `let ns: u8 = 1 << forks` was the third member of today's
            // shift-overflow family (see `ChoiceSet`): at 8 forks it
            // wraps to 0 and the inner loop does not run, so the test
            // would have reported perfect sharing for the shapes that
            // share least. It only ever ran on room (1,0), where forks
            // is 2, so it was never wrong in practice - which is
            // exactly how the ChoiceSet one survived too.
            //
            // Enumeration is 2^forks, so it is capped and the cap is
            // ANNOUNCED. A silent skip here would read as "measured and
            // fine" for precisely the shapes the question is about.
            const MAX_FORKS: u8 = 14;
            if forks > MAX_FORKS {
                eprintln!(
                    "[fork] shape {}: {} forks - SKIPPED, 2^{} configurations is past \
                     the {} cap. Not measured, not zero.",
                    si, forks, forks, MAX_FORKS
                );
                continue;
            }
            let ns: u64 = 1u64 << forks;
            let mut flat_arena = Graph::new();
            let mut flat_roots: Vec<NodeId> = Vec::new();
            // Configurations that FOLD AWAY. Resolving a split turns
            // `FragOk` into ordinary arithmetic, and arithmetic folds:
            // a configuration whose every outcome is statically dead is
            // one the flat form never emits, while the runtime loop
            // still enters it and discovers `valid == 0`. So this is
            // both a discount on the flat size and a count of trips the
            // loop takes for nothing.
            let mut dead = 0u64;
            let mut live_roots: Vec<NodeId> = Vec::new();
            // How many DISTINCT successors the configurations produce.
            //
            // Sharing the compute is only half a flat kernel: each
            // configuration also has to WRITE its row, and the write
            // does not hash-cons - it is emitted code. But two
            // configurations that land on the same output tuple are the
            // same successor for every lane (the same argument
            // `specialize_into` makes for buttons), so they need ONE
            // write between them. This counts the tuples, which is the
            // number of write sites a flat kernel actually needs, as
            // against `ns` if none of them coincide.
            let mut tuples: std::collections::HashSet<Vec<NodeId>> =
                std::collections::HashSet::new();
            for c in 0..ns {
                let map = g.specialize_config_into(0, Some(c), &mut flat_arena);
                let alive = r.bound.outcomes.iter().any(|o| {
                    !matches!(
                        flat_arena.get(map[o.live as usize]).op,
                        crate::transpile::graph::Op::ConstBool(false)
                    )
                });
                if alive {
                    live_roots.extend(base.iter().map(|x| map[*x as usize]));
                    tuples.insert(base.iter().map(|x| map[*x as usize]).collect());
                } else {
                    dead += 1;
                }
                flat_roots.extend(base.iter().map(|x| map[*x as usize]));
            }
            let (a, b) = (
                live_count(&loop_arena, &loop_roots),
                live_count(&flat_arena, &flat_roots),
            );
            let b_live = live_count(&flat_arena, &live_roots);
            eprintln!(
                "[fork]   shape {}: {} of {} configurations fold away \
                 ({:.1}%); flat nodes {} -> {} once they are dropped",
                si,
                dead,
                ns,
                100.0 * dead as f64 / ns as f64,
                b,
                b_live,
            );
            eprintln!(
                "[fork]   shape {}: {} distinct successors from {} configurations \
                 ({} write sites, not {})",
                si,
                tuples.len(),
                ns,
                tuples.len(),
                ns,
            );

            // WHERE each fork comes from.
            //
            // `Iface::ival` says the only INTERVAL input is the player's
            // `rem.x`/`rem.y`, so a shape forking 14 times is forking on
            // twelve values that became intervals during the frame. That
            // is either six more objects whose motion genuinely inherits
            // the player's uncertainty, or an imprecision worth
            // narrowing - and the difference is the difference between
            // 16,384 real successors and 16,384 spurious ones. Naming
            // the cells under each fork is what tells them apart.
            let mut in_path: std::collections::BTreeMap<u32, String> =
                std::collections::BTreeMap::new();
            for (i, cell) in r.frame.in_cells.iter().enumerate() {
                in_path.insert(*cell, crate::trace::iface::show(&r.frame.iface.slots[i]));
            }
            let ivals: Vec<String> = r
                .frame
                .iface
                .ival
                .iter()
                .enumerate()
                .filter(|(_, b)| **b)
                .map(|(i, _)| crate::trace::iface::show(&r.frame.iface.slots[i]))
                .collect();
            eprintln!("[fork]   shape {}: interval INPUTS are {:?}", si, ivals);
            for d in 0..forks {
                let split = (0..g.len() as NodeId)
                    .find(|id| matches!(g.get(*id).op, crate::transpile::graph::Op::Split(x) if x == d));
                let Some(split) = split else {
                    eprintln!("[fork]   shape {} fork {}: no Split node (folded away)", si, d);
                    continue;
                };
                let operand = g.get(split).args[0];
                let reach = crate::transpile::bdd::reachable(g, &[operand]);
                let mut objs: Vec<String> = Vec::new();
                let mut n_cells = 0usize;
                for id in 0..g.len() as NodeId {
                    if !reach[id as usize] {
                        continue;
                    }
                    if let crate::transpile::graph::Op::Cell(c) = g.get(id).op {
                        if let Some(p) = in_path.get(&c) {
                            n_cells += 1;
                            // "objects[3].spd.y" -> "objects[3]"
                            let head = match p.find(']') {
                                Some(k) => p[..=k].to_string(),
                                None => p.clone(),
                            };
                            objs.push(head);
                        }
                    }
                }
                objs.sort();
                objs.dedup();
                eprintln!(
                    "[fork]   shape {} fork {}: {} input cells under it, from {:?}",
                    si, d, n_cells, objs
                );
            }

            // The other direction, which is the one that names an
            // object: not "what does this fork read" but "whose
            // position does it write". A fork reads most of the state
            // (every object's motion is guarded by the same globals),
            // so the read cone identifies nothing; the WRITE cone does,
            // because `objects[k].rem.x` is written by exactly the
            // `move` call on object k.
            //
            // `bound.outcomes` and `frame.outs` are the same list in the
            // same order - `bind` renumbers cells, not roots - so the
            // node ids come from the first and the paths from the
            // second.
            let cones = g.split_cones();
            for (oi, o) in r.bound.outcomes.iter().enumerate() {
                let paths = &r.frame.outs[oi].fields;
                assert_eq!(
                    paths.len(),
                    o.outputs.len(),
                    "outcome {} has {} paths but {} outputs - the two lists are \
                     supposed to be the same roots in the same order",
                    oi,
                    paths.len(),
                    o.outputs.len()
                );
                let mut by_obj: std::collections::BTreeMap<String, u64> =
                    std::collections::BTreeMap::new();
                for ((path, _, _), (_, node, _)) in paths.iter().zip(o.outputs.iter()) {
                    let p = crate::trace::iface::show(path);
                    let head = match p.find(']') {
                        Some(k) => p[..=k].to_string(),
                        None => p.clone(),
                    };
                    *by_obj.entry(head).or_insert(0) |= cones[*node as usize];
                }
                for (obj, mask) in by_obj {
                    if mask == 0 {
                        continue;
                    }
                    let which: Vec<u8> = (0..forks).filter(|d| mask & (1 << d) != 0).collect();
                    eprintln!(
                        "[fork]   shape {} outcome {}: {} depends on forks {:?}",
                        si, oi, obj, which
                    );
                }
                // The whole outcome, VALUES AND MASKS. `live` and `ok`
                // matter as much as the fields: an outcome whose values
                // ignore every fork can still be gated on fork
                // validity, and an outcome is only cheap to specialize
                // if the mask is cheap too.
                let whole = o
                    .outputs
                    .iter()
                    .map(|(_, n, _)| cones[*n as usize])
                    .chain([cones[o.live as usize], cones[o.ok as usize]])
                    .fold(0u64, |a, b| a | b);
                let which: Vec<u8> = (0..forks).filter(|d| whole & (1 << d) != 0).collect();
                eprintln!(
                    "[fork]   OUTCOME shape {} outcome {}: {} of {} forks {:?} \
                     -> {} configurations",
                    si,
                    oi,
                    which.len(),
                    forks,
                    which,
                    1u64 << which.len()
                );

                // The number that decides whether flat is BUILDABLE.
                //
                // `emit_body` already specializes 64 button assignments
                // into one arena and dedups them by SIGNATURE into
                // representatives, and each representative becomes an
                // emitted body. Resolving forks the same way makes them
                // another variant dimension - but the existing
                // signature spans EVERY outcome at once, and that is
                // exactly the aggregation that made 16,384
                // configurations look distinct this morning. Per
                // outcome, over only that outcome's own forks, is the
                // version that can collapse.
                //
                // So: 64 buttons x 2^(this outcome's forks), deduped by
                // this outcome's signature alone. That count is how
                // many bodies a flat kernel emits for this outcome.
                // Philippe: "if an outcome is involved in a loop that
                // is irrelevant for its values, I'd expect all the
                // redundant copies the loop creates to collapse."
                //
                // Right, and the enumeration above already only walks
                // an outcome's OWN forks, so there is no such redundancy
                // left to collapse. The question that remains is
                // whether the forks it DOES depend on are in its VALUES
                // or only in its MASK - because those want completely
                // different treatment. 256 distinct value tuples is 256
                // rows and there is nothing to be done. 256 distinct
                // masks over ONE value tuple is one row whose lane mask
                // is an OR, which is a single body.
                let fields_cone = o
                    .outputs
                    .iter()
                    .map(|(_, n, _)| cones[*n as usize])
                    .fold(0u64, |a, b| a | b);
                let pop = |m: u64| -> u32 { m.count_ones() };
                let nc = 1u64 << which.len();
                // The 64-button body count is the expensive half (64 x
                // 2^k specializations of a 20k-node graph); the
                // contradiction count below needs one button. Shape 1
                // is the 14-fork case the question is about, so the
                // expensive half runs there and the cheap half runs
                // everywhere.
                let full = si == 1;
                let mut sigs: std::collections::HashSet<Vec<NodeId>> =
                    std::collections::HashSet::new();
                let mut vals: std::collections::HashSet<Vec<NodeId>> =
                    std::collections::HashSet::new();
                let mut lives: std::collections::HashSet<NodeId> =
                    std::collections::HashSet::new();
                let mut oks: std::collections::HashSet<NodeId> =
                    std::collections::HashSet::new();
                let mut arena = Graph::new();
                for m in 0u8..if full { 64 } else { 1 } {
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(m, Some(sm), &mut arena);
                        let v: Vec<NodeId> =
                            o.outputs.iter().map(|(_, n, _)| map[*n as usize]).collect();
                        let mut sig = v.clone();
                        sig.push(map[o.ok as usize]);
                        sig.push(map[o.live as usize]);
                        vals.insert(v);
                        lives.insert(map[o.live as usize]);
                        oks.insert(map[o.ok as usize]);
                        sigs.insert(sig);
                    }
                }
                // The GRAPH itself, for the outcomes the question is
                // about. Summary statistics have been wrong six times
                // today; this prints what `live` and `ok` are actually
                // made of - the AND-conjuncts, each with its op and the
                // forks its cone touches - so the structure can be read
                // rather than inferred.
                if si == 1 && which.len() > 2 {
                    fn conj(g: &Graph, n: NodeId, out: &mut Vec<NodeId>) {
                        match &g.get(n).op {
                            crate::transpile::graph::Op::And => {
                                for a in g.get(n).args.clone() {
                                    conj(g, a, out);
                                }
                            }
                            crate::transpile::graph::Op::ConstBool(true) => {}
                            _ => out.push(n),
                        }
                    }
                    for (label, root) in [("live", o.live), ("ok", o.ok)] {
                        let mut cs = Vec::new();
                        conj(g, root, &mut cs);
                        eprintln!(
                            "[fork]   GRAPH shape {} outcome {} {}: {} conjuncts",
                            si,
                            oi,
                            label,
                            cs.len()
                        );
                        for c in cs.iter().take(24) {
                            let m = cones[*c as usize];
                            let f: Vec<u8> =
                                (0..forks).filter(|d| m & (1 << d) != 0).collect();
                            let kids: Vec<String> = g
                                .get(*c)
                                .args
                                .iter()
                                .map(|a| format!("{:?}", g.get(*a).op))
                                .collect();
                            eprintln!(
                                "[fork]     {} {:?}({}) forks {:?}",
                                label,
                                g.get(*c).op,
                                kids.join(", "),
                                f
                            );
                        }
                    }
                }

                // The count above is PRE-SIMPLIFICATION, and the real
                // emitter does not stop there. `Emit::bare` sets
                // `decide: true` for the traced path, so `emit_body`
                // runs ival -> bdd -> ival over the SPECIALIZED arena
                // before it computes any signature - the pass whose own
                // doc records 10,510 nodes going to 4,714 on a traced
                // frame, and whose whole point is that guard algebra
                // only collapses once the choices are constants.
                //
                // Resolving a fork makes it a constant exactly as
                // resolving a button does. So the honest body count is
                // the one taken AFTER that pass, and every number I
                // have reported so far skipped it.
                if si == 1 && !which.is_empty() {
                    let room = crate::transpile::graph::Room {
                        cart: r.cart.clone(),
                        cache: r.cache.clone(),
                    };
                    let mut a3 = Graph::new();
                    let mut roots: Vec<NodeId> = Vec::new();
                    let per = o.outputs.len() + 2;
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(0, Some(sm), &mut a3);
                        roots.extend(o.outputs.iter().map(|(_, n, _)| map[*n as usize]));
                        roots.push(map[o.ok as usize]);
                        roots.push(map[o.live as usize]);
                    }
                    let raw: std::collections::HashSet<&[NodeId]> =
                        roots.chunks(per).collect();
                    let (g1, m1, _) = crate::transpile::ival::fold(&a3, &roots, Some(&room))
                        .expect("interval fold");
                    let r1: Vec<NodeId> = roots.iter().map(|x| m1[*x as usize]).collect();
                    let (g2, m2, _) =
                        crate::transpile::bdd::simplify_until_stable(&g1, &r1, 1 << 22, 4);
                    let r2: Vec<NodeId> = r1.iter().map(|x| m2[*x as usize]).collect();
                    let (_g3, m3, _) = crate::transpile::ival::fold(&g2, &r2, Some(&room))
                        .expect("interval fold 2");
                    let r3: Vec<NodeId> = r2.iter().map(|x| m3[*x as usize]).collect();
                    let decided: std::collections::HashSet<&[NodeId]> =
                        r3.chunks(per).collect();
                    eprintln!(
                        "[fork]   DECIDED shape {} outcome {}: {} configs, {} distinct \
                         raw -> {} distinct after ival/bdd/ival",
                        si,
                        oi,
                        nc,
                        raw.len(),
                        decided.len()
                    );
                }

                // Philippe: "10,000 of anything seems wrong. There is
                // no way to get 10,000 distinct outcomes from one input
                // frame. My prior is that none of this explosion is
                // real. Strong prior."
                //
                // The test of that: an outcome reachable on ONE control
                // flow path cannot have two paths' forks both live, so
                // most of its 2^k configurations should be
                // CONTRADICTORY - `live` folds to false. Count them. If
                // the count is high the explosion is an artifact of
                // enumerating combinations that cannot co-occur; if it
                // is zero, either they really can co-occur or the
                // folder cannot see that they cannot, and those want
                // very different fixes.
                let mut dead_here = 0u64;
                let mut ok_false = 0u64;
                {
                    let mut a2 = Graph::new();
                    for k in 0..nc {
                        let mut sm = 0u64;
                        for (i, d) in which.iter().enumerate() {
                            if k & (1 << i) != 0 {
                                sm |= 1u64 << d;
                            }
                        }
                        let map = g.specialize_config_into(0, Some(sm), &mut a2);
                        if matches!(
                            a2.get(map[o.live as usize]).op,
                            crate::transpile::graph::Op::ConstBool(false)
                        ) {
                            dead_here += 1;
                        }
                        if matches!(
                            a2.get(map[o.ok as usize]).op,
                            crate::transpile::graph::Op::ConstBool(false)
                        ) {
                            ok_false += 1;
                        }
                    }
                }
                eprintln!(
                    "[fork]   CONTRADICTORY shape {} outcome {}: {} of {} configurations \
                     have live=false, {} have ok=false",
                    si, oi, dead_here, nc, ok_false
                );
                eprintln!(
                    "[fork]   BODIES shape {} outcome {}: 64 buttons x {} configs = {} \
                     -> {} bodies; VALUES {} (cone {} forks), live {} (cone {}), \
                     ok {} (cone {})",
                    si,
                    oi,
                    nc,
                    64 * nc,
                    sigs.len(),
                    vals.len(),
                    pop(fields_cone),
                    lives.len(),
                    pop(cones[o.live as usize]),
                    oks.len(),
                    pop(cones[o.ok as usize]),
                );
            }

            // WHERE the loop's nodes sit, which is what decides its
            // runtime: a node whose split cone's highest bit is `d`
            // lives inside loops 0..=d and is therefore executed up to
            // 2^(d+1) times. Same `level` the emitter places by
            // (`transpile::lower`), so this histogram is literally the
            // shape of the emitted loop nest.
            let live_loop = crate::transpile::bdd::reachable(&loop_arena, &loop_roots);
            let scone = loop_arena.split_cones();
            let mut per_level = vec![0usize; forks as usize + 1];
            for id in 0..loop_arena.len() as NodeId {
                if !live_loop[id as usize] {
                    continue;
                }
                let m = scone[id as usize];
                let lvl = if m == 0 {
                    0
                } else {
                    (crate::transpile::graph::ChoiceSet::BITS - m.leading_zeros()) as usize
                };
                per_level[lvl.min(forks as usize)] += 1;
            }
            // The loop's WORK, with no pruning: each level's nodes run
            // once per configuration of the forks below them. This is an
            // upper bound - `if valid == 0 { continue }` skips
            // configurations no lane reaches - so a flat form cheaper
            // than this is not yet proof, but a flat form cheaper than
            // the loop's SIZE times a handful is a strong hint.
            let loop_work: u64 = per_level
                .iter()
                .enumerate()
                .map(|(l, n)| *n as u64 * (1u64 << l))
                .sum();
            eprintln!(
                "[fork]   shape {} levels {:?}; loop work <= {} node-evals, \
                 flat work = {} ({:.2}x cheaper at most)",
                si,
                per_level,
                loop_work,
                b,
                loop_work as f64 / b.max(1) as f64,
            );
            // The two are equal in WORK - the loop evaluates its tail
            // up to 2^forks times, specialization evaluates 2^forks
            // copies once each - so the ratio is pure code size, and
            // anything near 2^forks means the configurations share
            // nothing and the trade is all cost.
            eprintln!(
                "[fork] shape {}: {} forks, traced graph {} nodes; \
                 emitted live nodes {} (loop) -> {} (specialized), {:.2}x \
                 of a maximum {:.2}x, so {:.1}% shared.",
                si,
                forks,
                g.len(),
                a,
                b,
                b as f64 / a.max(1) as f64,
                ns as f64,
                100.0 * (1.0 - b as f64 / (ns as f64 * a.max(1) as f64)),
            );
        }
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
    let base = trace_frame(&mut it, &reset, &fr, st.clone(), &roots, &[], &ival, true)
        .map_err(|e| anyhow!("base trace of shape {}: {:#}", shape_idx, e))?;
    let (bn, bf, _) = measure(&it.d.graph, &base);

    // Pinned.
    let f = trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, true)
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
        let f = match trace_frame(&mut it, &reset, &fr, st, &roots, &pin, &ival, opts.widen) {
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
    let mut lines_by_shape: std::collections::BTreeMap<String, usize> = Default::default();
    for (k, f) in frames.iter_mut() {
        if let Ok(bound) = super::emit::bind(f, &graph, true) {
            if let Ok(low) = super::emit::lower_frame(&bound.graph, &bound.inputs, &bound.uni, &bound.outcomes, Some(room.clone()), bound.forks) {
                lines_by_shape.insert(k.clone(), low.body.len());
            }
        }
    }
    let mut out = String::new();
    out.push_str(&format!("{} shapes reached (constant-lattice fixpoint)\n", lattice.len()));
    for (i, (k, cm)) in lattice.iter().enumerate() {
        let spd: Vec<String> = cm.keys().map(super::iface::show)
            .filter(|s| s.contains("objects[") && s.contains(".spd")).collect();
        out.push_str(&format!("shape {}: {} const fields, {} forks, {} body lines\n",
            i, cm.len(), forks.get(k).copied().unwrap_or(999), lines_by_shape.get(k).copied().unwrap_or(0)));
        if let Some(ops) = forkops.get(k) {
            for o in ops.iter().take(4) { out.push_str(&format!("    fork: {}\n", o)); }
        }
        let _ = spd;
    }
    out.push_str(&format!("total body lines (lattice): {}\n", lines_by_shape.values().sum::<usize>()));
    Ok(out)
}
