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

    /// End-to-end check of the lane-granular deopt machinery: corrupt the
    /// rewritten program with a synthetic premise that only *some* lanes
    /// satisfy - an `assert_true` on an `expand`-produced button bool, so
    /// half the expanded lanes falsify it every frame - and run with deopt.
    /// The captured lanes re-run under the plain program through the
    /// canonical-state mapping; the result must match the unmodified
    /// rewritten program's observations exactly, every frame.
    ///
    /// This exercises: origin injection and stripping, collect-mode capture
    /// and mid-fragment filtering, the lane-coverage accounting, the plain
    /// re-run of only the failed lanes, and the merge of both output sets.
    #[test]
    fn granular_deopt_reproduces_the_baseline() {
        // Serialise against the partition-toggle tests: this test compares
        // a baseline run against a candidate run, and a toggle flip
        // between the two makes them diverge spuriously.
        let _partition =
            crate::interpreter::partition_straddles_test_lock();
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        let recipe = crate::program::recipe::Recipe::load("rewrites.jsonl").expect("load recipe");
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        let plain = Program::compile_from_disk().expect("compile plain");
        let mapping = crate::search::state_mapping::StateMapping::from_recipe(&recipe);
        assert!(!mapping.is_identity());

        // Corrupt: assert the outputs of the first few comparisons in the
        // fused frame body. Comparisons on player state are lane-mixed once
        // the input fan-out starts, so some lanes falsify these synthetic
        // premises every frame - the partial-capture path - while scalar
        // frames and uniform fragments exercise the capture-all path.
        let mut corrupted = program.clone();
        let fun = corrupted
            .get_mut("anonymous_61")
            .expect("the fused frame body exists");
        let mut next_id: usize = std::iter::once(&fun.cfg.entry)
            .chain(fun.cfg.named.values())
            .flat_map(|b| {
                b.instructions
                    .iter()
                    .map(|(id, _)| usize::from(*id))
                    .chain(std::iter::once(usize::from(b.terminator.0)))
            })
            .max()
            .unwrap_or(0)
            + 1;
        // Entry first, then the named blocks BY LABEL. `named` is an
        // `FxHashMap`, so `values_mut()` visits it in an order that
        // depends on how the map was BUILT - and the program is loaded
        // from a frozen artifact now, i.e. `collect()`ed in one go
        // rather than grown one rule at a time. That changed which three
        // comparisons got corrupted, the new three happened to hold on
        // every lane, and the premise never fired - while the program
        // under test was identical. Sorting is what `blocks_in_order`
        // does for printing, for exactly this reason.
        let mut order: Vec<crate::ir::Label> = fun.cfg.named.keys().cloned().collect();
        order.sort();
        let mut inserted = 0;
        let corrupt = |block: &mut crate::ir::Block, next_id: &mut usize, n: &mut usize| {
            let mut index = 0;
            while index < block.instructions.len() && *n < 3 {
                if matches!(
                    block.instructions[index].1,
                    Instruction::BinaryOp {
                        op: crate::ir::BinaryOp::LessThan | crate::ir::BinaryOp::GreaterThan,
                        ..
                    }
                ) {
                    let target = block.instructions[index].0;
                    block.instructions.insert(
                        index + 1,
                        (
                            crate::ir::LocalId::from(*next_id),
                            Instruction::AssertTrue { value: target },
                        ),
                    );
                    *next_id += 1;
                    *n += 1;
                    index += 1;
                }
                index += 1;
            }
        };
        corrupt(&mut fun.cfg.entry, &mut next_id, &mut inserted);
        for label in &order {
            if inserted >= 3 {
                break;
            }
            let block = fun.cfg.named.get_mut(label).expect("label came from the map");
            corrupt(block, &mut next_id, &mut inserted);
        }
        assert!(inserted > 0, "no comparison found to corrupt");

        let frames = 28;
        let baseline = observation_trace(&program, frames).expect("baseline trace");
        let mut run = AbstractRun::start_with_deopt(&corrupted, &plain, mapping, false)
            .expect("start deopt run");
        for frame in 1..=frames {
            run.step().expect("step");
            assert_eq!(
                observe_frame(run.states()),
                baseline[frame as usize],
                "granular deopt diverged from the baseline at frame {}",
                frame
            );
        }
        let (states, lanes) = run.deopt_events();
        assert!(states > 0, "the synthetic premise never fired");
        assert!(lanes > 0, "no lanes re-ran under the plain program");
    }

    /// The compiled frame body - the TRACED kernels - produces the
    /// interpreter's rows (P1 stage 3, tracing.md stage 5).
    ///
    /// `CELESTE_COMPILED_FORWARD=check` runs BOTH engines on every chunk
    /// and compares canonical row-key SETS, failing the step on the first
    /// difference. Set equality is the right claim and the only one
    /// available: the compiled path returns a different PARTITION of the
    /// same rows (different block count, lane order and heap layout), so
    /// `observe_frame` would differ for a correct run. Every chunk the
    /// traced set claims is therefore checked against the interpreter.
    ///
    /// Lane counts are checked against an interpreted baseline on top,
    /// because a bug that dropped a row from both sides symmetrically
    /// would pass the key comparison. The traced-lane assertion matters
    /// more: the traced set is indexed by heap SHAPE, so a set generated
    /// for shapes this run never reaches would miss every chunk, fall
    /// through to the interpreter, and pass this test having run none of
    /// the code it names.
    ///
    /// Sets a process-global env var and relies on nextest's
    /// process-per-test isolation, like the other global-state tests here.
    #[test]
    fn traced_kernels_reproduce_the_interpreter() {
        let _partition = crate::interpreter::partition_straddles_test_lock();
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
            || !std::path::Path::new("rewrites-compile.jsonl").exists()
        {
            return;
        }
        let program = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");

        let frames = 12;
        let mut baseline = AbstractRun::start(&program).expect("start baseline");
        let mut want = Vec::new();
        for _ in 1..=frames {
            baseline.step().expect("baseline step");
            want.push(baseline.lane_count());
        }
        drop(baseline);

        std::env::set_var("CELESTE_COMPILED_FORWARD", "check");
        let mut run = AbstractRun::start(&program).expect("start compiled run");
        assert!(run.compiled.is_some(), "the compiled engine did not engage");
        for (frame, want) in (1..=frames).zip(want) {
            run.step().unwrap_or_else(|e| panic!("frame {}: {:#}", frame, e));
            assert_eq!(
                run.lane_count(),
                want,
                "the traced kernels have a different lane count at frame {}",
                frame
            );
        }
        assert!(
            crate::compiled::dispatch::traced_lanes() > 0,
            "no chunk ever reached a traced kernel - the set covers no shape this run \
             produces, so this test checked nothing"
        );
    }

    /// End-to-end check of the shape-dispatch machinery (`Variant`): run the
    /// plain-compiled program as the base with the full recipe registered as
    /// a variant for the singleton shapes. Every state in room (1,0) is a
    /// singleton, so every frame of every state dispatches to the variant -
    /// base -> canonical -> variant on the way in, back on the way out - and
    /// the observations must match a plain-only run exactly, every frame.
    ///
    /// This exercises: the object-shape probe, shape matching, both mapping
    /// directions around a variant frame, and the zero-fallback invariant.
    #[test]
    fn shape_variant_dispatch_reproduces_the_baseline() {
        // Serialise against the partition-toggle tests: this test compares
        // a baseline run against a candidate run, and a toggle flip
        // between the two makes them diverge spuriously.
        let _partition =
            crate::interpreter::partition_straddles_test_lock();
        if !std::path::Path::new("lua/celeste-minimal.lua").exists()
            || !std::path::Path::new("rewrites.jsonl").exists()
        {
            return;
        }
        let plain = Program::compile_from_disk().expect("compile plain");
        let recipe = crate::program::recipe::Recipe::load("rewrites.jsonl").expect("load recipe");
        let rewritten = crate::program::frozen::rewritten("rewrites.jsonl").expect("frozen");
        let mapping = crate::search::state_mapping::StateMapping::from_recipe(&recipe);
        assert!(!mapping.is_identity());

        let frames = 28;
        let baseline = observation_trace(&plain, frames).expect("baseline trace");
        let mut run = AbstractRun::start(&plain).expect("start base run");
        // Shape probe sanity: room (1,0) starts as a lone player_spawn.
        assert_eq!(
            crate::interpreter::abstraction::object_shape(&run.states()[0]).expect("shape probe"),
            vec!["player_spawn".to_string()]
        );
        run.set_variants(
            // The base program is the plain one: its layout IS canonical.
            crate::search::state_mapping::StateMapping::default(),
            vec![Variant {
                label: "rewrites.jsonl[test]".to_string(),
                shapes: vec![
                    vec!["player".to_string()],
                    vec!["player_spawn".to_string()],
                ],
                pm1: Vec::new(),
                frame_cfg: crate::interpreter::fixed_env::PreparedCfg::new(
                    rewritten.frame_cfg().clone(),
                ),
                fixed_env: rewritten.fixed_env(),
                mapping,
            }],
        );
        for frame in 1..=frames {
            run.step().expect("step");
            assert_eq!(
                observe_frame(run.states()),
                baseline[frame as usize],
                "variant dispatch diverged from the baseline at frame {}",
                frame
            );
        }
        let (states, lanes, fallbacks) = run.variant_events();
        assert!(states > 0 && lanes > 0, "no frames ran under the variant");
        assert_eq!(fallbacks, 0, "variant frames fell back to the base program");
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
