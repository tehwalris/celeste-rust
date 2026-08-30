//! Differential verification: does the rewritten program still behave
//! identically to the original?
//!
//! This is the real safety net. It is strong precisely because the abstract
//! interpreter summarises enormously many concrete runs at once. Thirty
//! frames of the abstract search costs well under a second and covers 15,250
//! distinct input sequences.
//!
//! The invariant that makes it work: **a rewrite may change anything inside a
//! frame, but must not change the cross-frame heap representation.** The frame
//! function loads the game state from the heap at entry and stores it back at
//! exit with the same shape the original program had. So the observation is
//! simply the canonical form of the heap at each frame boundary
//! (`super::run::observe_frame`).
//!
//! Canonicalisation has to be done carefully. `gc()` renumbers heap ids
//! deterministically, which handles allocation-order differences. But lanes are
//! only meaningful as a set, and two runs may produce them in a different
//! order - so lane rows are sorted. Crucially the rows are sorted *as whole
//! tuples across all slots*, not per slot: sorting each slot independently
//! would lose the correlation between fields and would call two genuinely
//! different state sets equal.
//!
//! One exception to "the representation must not change": **a closure capture
//! is observed by the value it denotes, not by the identity of the box holding
//! it.** The rewrite sequence removes those boxes on purpose, so the premise
//! that the cross-frame heap graph is literally invariant does not survive it.
//! Each place it is relaxed has to be narrow, stated, and applied to both
//! sides.

use anyhow::Result;
use std::collections::BTreeSet;

use super::run::{AbstractRun, StateObservation, Variant, observe_frame};
use crate::program::Program;

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

/// Runs a shape VARIANT against a precomputed baseline trace.
///
/// `host` runs every state whose object-array shape the variant does not
/// claim; the variant runs the rest, through the canonical form both ways.
/// The baseline to pass is the host's own trace: a variant's contract is
/// that dispatch is INVISIBLE (see `Variant`), so "the host with this
/// variant registered observes what the host alone observes" is exactly the
/// claim, and it is checkable at a depth where the variant's shape actually
/// occurs. The host's own equivalence to the plain program is a separate
/// gate that `verify` already runs.
///
/// Two failure modes are specific to this mode and both are checked here,
/// because neither shows up as a divergence:
///
///   * a variant frame that FAILS falls back to the host and produces the
///     right answer, so a variant whose premises never hold would otherwise
///     screen clean. Any fallback is a rejection.
///   * a variant that never dispatched at all was never exercised, so the
///     run says nothing about it. That is precisely how 129 entries on
///     never-executed object functions got into a recipe unchecked
///     (see the `rewrites-room00.jsonl` commit); refuse it instead.
pub fn differential_variant_against_trace(
    baseline: &[BTreeSet<StateObservation>],
    host: &Program,
    host_mapping: super::state_mapping::StateMapping,
    variant: Variant,
    frames: u32,
) -> Result<Option<Divergence>> {
    let mut run = match AbstractRun::start(host) {
        Ok(run) => run,
        Err(e) => {
            return Ok(Some(Divergence { frame: 0, detail: format!("init failed: {:#}", e) }))
        }
    };
    run.set_variants(host_mapping, vec![variant]);
    run.quiet = true;
    for frame in 0..=frames {
        if frame > 0 {
            if let Err(e) = run.step() {
                return Ok(Some(Divergence { frame, detail: format!("{:#}", e) }));
            }
        }
        let (_, _, fallbacks) = run.variant_events();
        if fallbacks > 0 {
            return Ok(Some(Divergence {
                frame,
                detail: format!("{} variant frame(s) fell back to the host program", fallbacks),
            }));
        }
        let observed = observe_frame(run.states());
        let Some(expected) = baseline.get(frame as usize) else { break };
        if &observed != expected {
            return Ok(Some(Divergence {
                frame,
                detail: describe(expected, &observed, expected.len(), run.lane_count()),
            }));
        }
    }
    let (states, _, _) = run.variant_events();
    if states == 0 {
        return Ok(Some(Divergence {
            frame: frames,
            detail: format!(
                "the variant never dispatched in {} frames - nothing was exercised",
                frames
            ),
        }));
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
                // Name the differing slots: which cell kinds disagree is
                // usually the whole diagnosis (2026-08-16, the room (2,0)
                // v6 divergence hunt).
                for &i in differing.iter().take(4) {
                    detail.push_str(&format!(
                        "\n    slot {}: baseline {:?} vs candidate {:?}",
                        i, x.structure[i], y.structure[i]
                    ));
                }
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

    /// Phase 4 (plans/keying-widening-flow.md): under
    /// `CELESTE_FRONTIER_ONLY`, the compiled-forward check no longer
    /// diverges once the widening is in the graph. This is the exact
    /// configuration that produced the Bits(2) "720 rows missing"
    /// artifact - the kernel's `chunk_skip` dropped rows on the
    /// PRE-widening key while the frontier stored the widened key. With
    /// `CELESTE_WIDEN_IN_GRAPH=1` the kernel keys the widened rem, and
    /// check mode now compares FULL per-chunk outputs ("off for both":
    /// `chunk_skip` is a per-chunk-asymmetric export optimization the
    /// boundary re-does, so it is auto-disabled in check). A `CheckMismatch`
    /// surfaces as a `step()` error and fails the test.
    #[test]
    fn frontier_only_check_agrees_with_widening_in_graph_at_bits2() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        std::env::set_var("CELESTE_REM_BITS", "2");
        std::env::set_var("CELESTE_ASM_KERNELS", "1");
        std::env::set_var("CELESTE_WIDEN_IN_GRAPH", "1");
        std::env::set_var("CELESTE_FRONTIER_ONLY", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        let mut run = AbstractRun::start(&program).expect("start");
        // Through frame 27 - the rem fork is frame 25, and the artifact
        // showed at 25 (and grew at 26/27), so this covers the whole
        // window where it appeared.
        for frame in 1..=27 {
            run.step().unwrap_or_else(|e| {
                panic!("frontier-only check diverged at frame {}: {:#}", frame, e)
            });
        }
        assert_eq!(
            crate::compiled::dispatch::missed_lanes(),
            0,
            "the widen-in-graph kernels missed lanes under frontier-only"
        );
    }

    /// The per-rung kernel registry serves EACH rung correctly within ONE
    /// process (the in-process `rewrite ladder`'s core need). With the
    /// widening in the graph the ladder kernels specialize per rem
    /// precision, and the registry is now indexed by precision - so
    /// switching the session precision mid-process (as the ladder does via
    /// `set_rem_precision`) must build and serve the RIGHT set each time,
    /// not freeze at the first rung's. Runs the compiled `check` forward at
    /// Bits(1) then Bits(2) in one process; a stale registry would MISS
    /// (strict panic) or DIVERGE (check mismatch).
    #[test]
    fn per_rung_registry_serves_each_rung_in_one_process() {
        use crate::interpreter::abstraction::{set_rem_precision, RemPrecision};
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        std::env::set_var("CELESTE_ASM_KERNELS", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        for k in [1u8, 2u8] {
            set_rem_precision(RemPrecision::Bits(k));
            assert_eq!(
                crate::interpreter::abstraction::rem_precision_from_env(),
                RemPrecision::Bits(k)
            );
            let mut run = AbstractRun::start(&program).expect("start");
            assert!(run.compiled.is_some(), "compiled engine did not engage at Bits({})", k);
            for frame in 1..=26 {
                run.step().unwrap_or_else(|e| {
                    panic!("Bits({}) frame {}: {:#}", k, frame, e)
                });
            }
        }
        assert!(
            crate::compiled::dispatch::traced_lanes() > 0,
            "no chunk reached a kernel - the per-rung registry checked nothing"
        );
        assert_eq!(
            crate::compiled::dispatch::missed_lanes(),
            0,
            "a rung's kernels missed lanes - the per-rung registry served the wrong set"
        );
    }

    /// The assert-noop guard end to end (plans/keying-widening-flow.md,
    /// Phase 1 point 1): with the widening in the graph
    /// (`CELESTE_WIDEN_IN_GRAPH=1`) and `CELESTE_KERNEL_WIDEN_NOOP=1`,
    /// every kernel output state is a FIXED POINT of the campaign's
    /// `make_state_abstract` - re-abstracting it changes no row key. A
    /// panic here means the graph UNDER-widened a field the boundary
    /// still moves (the fruit `off`/`y` class). Room (1,0) at Bits(2),
    /// through frame 26 so the rem fork is exercised; check mode compares
    /// against the interpreter on top.
    #[test]
    fn kernel_widen_is_a_noop_at_bits2() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        std::env::set_var("CELESTE_REM_BITS", "2");
        std::env::set_var("CELESTE_ASM_KERNELS", "1");
        std::env::set_var("CELESTE_WIDEN_IN_GRAPH", "1");
        std::env::set_var("CELESTE_KERNEL_WIDEN_NOOP", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        let mut run = AbstractRun::start(&program).expect("start");
        for frame in 1..=26 {
            run.step().unwrap_or_else(|e| panic!("frame {}: {:#}", frame, e));
        }
        assert!(
            crate::compiled::dispatch::traced_lanes() > 0,
            "no chunk reached a widen-in-graph kernel - the assert-noop checked nothing"
        );
        assert_eq!(
            crate::compiled::dispatch::missed_lanes(),
            0,
            "the widen-in-graph kernels missed lanes"
        );
    }

    /// The widen-in-graph kernels ASSEMBLE for a LIVE-FRUIT room (2,0)
    /// (plans/keying-widening-flow.md, Phase 2): `LADDER_WIDEN` bakes the
    /// fruit `off` -> [0,39] / `y` -> bob band into the graph (`Op::Span`
    /// over a symbolic `start`, `Sel(Or, Sel, Cell(28))` in this room),
    /// which the ASM codegen must lower and gcc must compile. A room (1,0)
    /// run never has a fruit, so this is the only place the fruit widening
    /// is exercised through the real pipeline.
    ///
    /// An ASSEMBLE gate, not a runtime A-vs-B: room (2,0)'s fruit chunks go
    /// to the campaign fallback rather than a ladder kernel (the compile
    /// overlay is the (1,0) one; see `FrameEngine::run_frame_chunk`), so
    /// there is no bound fruit kernel to run a row comparison against - and
    /// stepping the interpreter frontier explodes at spd Exact. The build
    /// succeeding IS the signal: the fruit widening lowered and compiled.
    /// `#[ignore]`d for cost - retracing room (2,0) and shelling out to gcc
    /// per shape is ~5 min. The fruit widening's CORRECTNESS is covered by
    /// `widen_fruit` being byte-identical to the production level-0 fruit
    /// code and by the assert-noop guard on any fruit-alive chunk.
    #[test]
    #[ignore = "retraces room (2,0) + gcc per shape, ~5 min"]
    fn widen_in_graph_assembles_for_a_fruit_room_2_0() {
        use crate::compiled::asm_kernel::Registry;
        use crate::trace::shapes::WalkOpts;
        use std::path::Path;

        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        std::env::set_var("CELESTE_START_ROOM", "2,0");
        std::env::set_var("CELESTE_REM_BITS", "2");
        let reg: Registry = std::thread::Builder::new()
            .stack_size(512 * 1024 * 1024)
            .spawn(|| {
                Registry::build_for_start_room(Path::new("."), WalkOpts::LADDER_WIDEN, true)
                    .expect("build B (LADDER_WIDEN) for room (2,0)")
            })
            .expect("spawn builder")
            .join()
            .expect("builder panicked");
        assert!(
            reg.len() > 0,
            "the room (2,0) widen-in-graph set assembled no shapes"
        );
    }

    /// Node-count de-risk (plans/keying-widening-flow.md, Phase 1a):
    /// baking the rem widening into the graph (`LADDER_WIDEN`) must be
    /// CHEAP - the hypothesis is that fusion + hash-consing share the rem
    /// fork with the frame's existing arithmetic, so the fused graphs
    /// grow by a small handful of nodes per shape, not a multiple. If this
    /// ever regresses (a widening that duplicates a large downstream cone
    /// per fork configuration), the assertion fires before the change goes
    /// wider. Prints the per-set totals so the number is on the record.
    #[test]
    fn widen_in_graph_is_cheap_in_nodes_at_bits2() {
        use crate::transpile::graph::Room;
        use crate::trace::shapes::WalkOpts;
        use std::path::Path;

        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        std::env::set_var("CELESTE_REM_BITS", "2");

        let total = |opts: WalkOpts| -> (usize, usize) {
            std::thread::Builder::new()
                .stack_size(512 * 1024 * 1024)
                .spawn(move || {
                    let refs = crate::trace::kernel::lattice_kernel_refs(Path::new("."), opts)
                        .expect("retrace");
                    let mut nodes = 0usize;
                    for r in &refs {
                        let room = Room { cart: r.cart.clone(), cache: r.cache.clone() };
                        let (fused, _, _, _) =
                            crate::trace::emit::asm_fused(&r.bound, Some(&room), true)
                                .expect("fuse");
                        nodes += fused.len();
                    }
                    (refs.len(), nodes)
                })
                .expect("spawn")
                .join()
                .expect("panicked")
        };

        let (shapes_a, nodes_a) = total(WalkOpts::LADDER);
        let (shapes_b, nodes_b) = total(WalkOpts::LADDER_WIDEN);
        eprintln!(
            "[widen-nodes] LADDER: {} shapes, {} fused nodes; \
             LADDER_WIDEN: {} shapes, {} fused nodes ({:+.1}%)",
            shapes_a,
            nodes_a,
            shapes_b,
            nodes_b,
            100.0 * (nodes_b as f64 - nodes_a as f64) / nodes_a as f64,
        );
        assert_eq!(shapes_a, shapes_b, "the two sets cover different shape counts");
        // Fusion + hash-consing keep it cheap: a small multiple, not a
        // large one. If this fires, the rem fork stopped sharing with the
        // frame's arithmetic - measure before going wider.
        assert!(
            nodes_b < nodes_a + nodes_a / 2,
            "widen-in-graph grew the fused node count by more than 50% ({} -> {})",
            nodes_a,
            nodes_b
        );
    }

    /// `engine_row_keys` (the canonical row key, recomputed from a state's
    /// content by re-importing and re-hashing) reproduces the keys a
    /// compiled forward CARRIES out of its boundary, byte for byte, per lane.
    ///
    /// This is the property the backward sweep rests on: it recomputes a
    /// saved lane's key and looks it up in the row table the forward built
    /// from these carried keys. If the recompute did not match the carry,
    /// the lookup would miss - exactly the "not in the row table" bug this
    /// unification fixes.
    #[test]
    fn engine_row_keys_reproduce_the_carried_keys() {
        // The init CFG interpret recurses deeper than a nextest test
        // thread's default stack; run the body on a big-stack thread (the
        // production sweep does not run init, so it is unaffected).
        std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(engine_row_keys_reproduce_the_carried_keys_body)
            .expect("spawn")
            .join()
            .expect("join");
    }

    fn engine_row_keys_reproduce_the_carried_keys_body() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        // Build from the COMPILE recipe - the program whose boundary defines
        // the key, and the one `engine_row_keys` uses internally.
        let compile_program =
            crate::program::frozen::rewritten("rewrites-compile.jsonl").expect("frozen compile");
        crate::interpreter::vectorize::set_merge_partition_patterns(
            &compile_program.merge_partition_cells,
        );
        let engine = crate::compiled::FrameEngine::new_for_start_room(&compile_program)
            .expect("build engine");

        // Seed exactly as AbstractRun::start does (via the reference
        // interpreter now that the CFG interpreter is gone).
        let mut states: Vec<crate::interpreter::state::State> =
            vec![crate::trace::refengine::RefEngine::new()
                .expect("reference engine")
                .initial_state()
                .expect("init")];

        let mut checked_lanes = 0usize;
        // 26 frames: enough to reach the first fork (frame 25) so the check
        // spans multi-lane blocks, not just the single spawn lane.
        for frame in 1..=26 {
            let mut next = Vec::new();
            for state in std::mem::take(&mut states) {
                if state.vector_size == 0 {
                    continue;
                }
                let outputs = engine.run_frame_chunk(&state);
                for (out, carried) in outputs {
                    let carried = carried.unwrap_or_else(|| {
                        panic!("frame {}: the engine did not carry keys", frame)
                    });
                    let recomputed =
                        crate::compiled::engine_row_keys(&out).expect("engine_row_keys");
                    assert_eq!(
                        recomputed, carried,
                        "frame {}: engine_row_keys does not reproduce the carried keys \
                         ({} lanes)",
                        frame,
                        out.vector_size
                    );
                    checked_lanes += out.vector_size;
                    next.push(out);
                }
            }
            states = next;
            assert!(!states.is_empty(), "frame {}: no states carried forward", frame);
        }
        assert!(
            checked_lanes > 0,
            "no lanes were checked - the engine produced no keyed output"
        );
    }

    /// Adding the player position to the merge partition (what the pos-graph
    /// recorder does) must NOT change the reachable row set - position is
    /// content and concrete per lane, so partitioning on it only regroups
    /// lanes into more, narrower states. This is the soundness gate for the
    /// recorder: if partitioning changed the search, the recorded table
    /// would describe a different one. Compare the per-frame row-key SET
    /// with the partition off vs on.
    #[test]
    fn position_partition_preserves_the_forward_row_set() {
        std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(position_partition_preserves_the_forward_row_set_body)
            .expect("spawn")
            .join()
            .expect("join");
    }

    fn position_partition_preserves_the_forward_row_set_body() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        // Compiled-only search now: drive the compiled engine both ways.
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");

        // Past the first rem straddle (frame 25), where lanes fan out to
        // distinct positions that merge by shape - which is exactly what the
        // position partition then splits back apart.
        let frames = 30;
        type Frame = (std::collections::BTreeSet<(u64, u64)>, usize);
        let per_frame = |partition: bool| -> Vec<Frame> {
            crate::interpreter::vectorize::set_partition_player_position(partition);
            let mut run = AbstractRun::start(&program).expect("start");
            let mut out = Vec::new();
            for _ in 1..=frames {
                run.step().expect("step");
                let mut rows = std::collections::BTreeSet::new();
                for s in run.states() {
                    rows.extend(crate::search::sweep::row_keys(s).expect("row keys"));
                }
                out.push((rows, run.states().len()));
            }
            crate::interpreter::vectorize::set_partition_player_position(false);
            out
        };

        let plain = per_frame(false);
        let split = per_frame(true);
        let mut split_grew = false;
        for f in 0..frames as usize {
            assert_eq!(
                plain[f].0, split[f].0,
                "frame {}: the position partition changed the reachable row set \
                 ({} rows plain vs {} split)",
                f + 1,
                plain[f].0.len(),
                split[f].0.len()
            );
            // Same rows, but the partition holds them in MORE states (one per
            // position) once lanes occupy more than one cell.
            assert!(
                split[f].1 >= plain[f].1,
                "frame {}: the partition produced fewer states than plain",
                f + 1
            );
            split_grew |= split[f].1 > plain[f].1;
        }
        // The partition must actually have SPLIT something by frame 30, or
        // the equality above proves nothing.
        assert!(
            split_grew,
            "the position partition never split a state - the forward pass is \
             too small, or the partition cells are wrong"
        );
    }

    /// End-to-end recorder gate: recording a real forward pass must run
    /// without `input_cell` ever erroring - which it does IFF the position
    /// partition makes every frame-input chunk uniform in position (the
    /// recorder's soundness precondition) - and must accumulate transitions.
    /// This is what validates `partition_position_cells` names the right
    /// cells: a wrong cell would leave chunks non-uniform and the run would
    /// bail loudly here.
    #[test]
    fn recording_a_forward_pass_keeps_chunks_uniform_in_position() {
        std::thread::Builder::new()
            .stack_size(256 * 1024 * 1024)
            .spawn(recording_a_forward_pass_keeps_chunks_uniform_in_position_body)
            .expect("spawn")
            .join()
            .expect("join");
    }

    fn recording_a_forward_pass_keeps_chunks_uniform_in_position_body() {
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        // Streaming path (the one the balloon lived on): frontier-only +
        // multi-thread routes through step_parallel, exactly production.
        std::env::set_var("CELESTE_FRONTIER_ONLY", "1");
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");
        std::env::set_var("CELESTE_KERNEL_STRICT", "1");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        let mut run = AbstractRun::start(&program).expect("start");
        run.record_pos_graph();
        for _ in 1..=20 {
            // A non-uniform chunk makes `input_cell` return Err, which
            // propagates here - so a clean run IS the uniformity guarantee.
            run.step().expect("recording step stayed uniform in position");
        }
        let graph = run.take_pos_graph(20, "test-fingerprint").expect("recording enabled");
        assert!(
            graph.pairs() > 0,
            "the recording pass observed no position transitions"
        );
    }

}
