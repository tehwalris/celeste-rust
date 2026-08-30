//! The SAMPLED KERNEL GATE: the ongoing correctness guarantee that the
//! runtime-assembled ASM kernels agree with the REFERENCE interpreter
//! (`RefDomain`/`RefEngine`), per lane, on real checkpoint states.
//!
//! This replaces the old interpreter-based kernel gates
//! (`asm_kernels_reproduce_the_interpreter` and friends). Those proved
//! `kernels == old CFG interpreter`; once the CFG interpreter is deleted the
//! only surviving oracle is `RefEngine`, and the bridge gate
//! (`refbridge::gate_frame_matches_reference`) already proved
//! `RefEngine == old CFG interpreter` exactly. This gate closes the loop
//! that remains after the interpreter is gone: `kernels == RefEngine`.
//!
//! For each sampled checkpoint frame, and up to `MAXLANES` sampled lanes, it
//! runs BOTH engines on the SINGLE-LANE input state:
//!
//! * the compiled `FrameEngine::run_frame_chunk` - which dispatches the lane
//!   to the ASM kernels (`compiled::asm_kernel`), and
//! * `RefEngine::run_frame` - the reference AST interpreter (`RefDomain`),
//!
//! funnels BOTH output sets through the SAME campaign abstraction + keying
//! the search applies (`refbridge::abstract_keys`), and asserts the row-key
//! SETS are equal. It asserts `dispatch::traced_lanes() > 0` and
//! `dispatch::missed_lanes() == 0` so a run where nothing reached a kernel -
//! or where a lane silently fell through to the reference fallback - fails
//! rather than passing vacuously (mirrors
//! `asm_kernels_reproduce_the_interpreter`).
//!
//! It reads the level-0 (rem-widened) forward checkpoints in `kfwd/room1`;
//! `run_frame_chunk`'s boundary hardcodes the level-0 rem widening, so the
//! gate runs at the default `CELESTE_REM_BITS=0`.

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::path::Path;

    use crate::compiled::FrameEngine;
    use crate::interpreter::state::State as OState;
    use crate::trace::refbridge::abstract_keys;
    use crate::trace::refengine::RefEngine;

    const CKPT: &str = "/var/tmp/celeste-checkpoints/kfwd/room1";
    const COMPILE_RECIPE: &str = "rewrites-compile.jsonl";

    /// Filter a multi-lane boundary state down to a single lane.
    fn single_lane(s: &OState, lane: usize) -> OState {
        if s.vector_size == 1 {
            return s.clone();
        }
        let mask: Vec<bool> = (0..s.vector_size).map(|i| i == lane).collect();
        s.filter_by_mask_clone(&mask, crate::interpreter::state::FILTER_CHUNK)
    }

    /// The keystone. `FRAMES` (comma-separated, default `5,30,60`) and
    /// `MAXLANES` (per frame, default 16) tune coverage.
    #[test]
    #[ignore]
    fn sampled_kernels_match_reference() {
        let dir = Path::new(CKPT);
        if !dir.exists()
            || !Path::new("lua/celeste-minimal.lua").exists()
            || !Path::new(COMPILE_RECIPE).exists()
        {
            eprintln!("sampled_kernels_match_reference: inputs missing, skipping");
            return;
        }

        let frames: Vec<u32> = std::env::var("FRAMES")
            .ok()
            .map(|s| s.split(',').filter_map(|p| p.trim().parse().ok()).collect())
            .unwrap_or_else(|| vec![5, 30, 60]);
        let max_lanes: usize =
            std::env::var("MAXLANES").ok().and_then(|s| s.parse().ok()).unwrap_or(16);

        // Both engines are built from the COMPILE program (the one the
        // kernels and name tables were traced from - see
        // `FrameEngine::new`), so the boundary/partition settings match on
        // both sides and `abstract_keys` keys consistently.
        let program =
            crate::program::frozen::rewritten(COMPILE_RECIPE).expect("compile program");
        crate::interpreter::vectorize::set_merge_partition_patterns(
            &program.merge_partition_cells,
        );
        let engine =
            FrameEngine::new_for_start_room(&program).expect("build compiled engine");

        let mut refeng = RefEngine::new().expect("build reference engine");

        // Zero the dispatch counters (this swaps them to 0).
        crate::compiled::dispatch::print_kernel_hits();

        let mut lanes_checked = 0usize;
        let mut matched = 0usize;
        let mut mism = 0usize;

        for &frame in &frames {
            let states = match crate::search::checkpoint::load_frame_states(dir, frame) {
                Ok(s) => s,
                Err(e) => panic!("frame {}: load failed: {:#}", frame, e),
            };
            let mut this_frame = 0usize;
            'lanes: for (si, s) in states.iter().enumerate() {
                for lane in 0..s.vector_size {
                    if this_frame >= max_lanes {
                        break 'lanes;
                    }
                    this_frame += 1;
                    lanes_checked += 1;

                    let one = single_lane(s, lane);

                    // KERNEL side: the compiled engine dispatches the lane to
                    // the ASM kernels; its output states funnel through the
                    // campaign abstraction + keying.
                    let a: BTreeSet<(u64, u64)> = engine
                        .run_frame_chunk(&one)
                        .into_iter()
                        .flat_map(|(os, _keys)| abstract_keys(os))
                        .collect();

                    // REFERENCE side: RefEngine enumerates the fork tree for
                    // the lane; the same abstraction + keying.
                    let b: BTreeSet<(u64, u64)> = refeng
                        .run_frame(&one)
                        .unwrap_or_else(|e| panic!("f{} s{} l{}: RefEngine: {:#}", frame, si, lane, e))
                        .into_iter()
                        .flat_map(abstract_keys)
                        .collect();

                    if a == b {
                        matched += 1;
                    } else {
                        mism += 1;
                        let only_a: Vec<_> = a.difference(&b).collect();
                        let only_b: Vec<_> = b.difference(&a).collect();
                        eprintln!(
                            "f{} s{} l{}: MISMATCH |kernel|={} |ref|={} kernel-only={} ref-only={}",
                            frame, si, lane, a.len(), b.len(), only_a.len(), only_b.len()
                        );
                    }
                }
            }
            eprintln!("frame {}: sampled {} lanes", frame, this_frame);
        }

        let traced = crate::compiled::dispatch::traced_lanes();
        let missed = crate::compiled::dispatch::missed_lanes();
        eprintln!(
            "sampled_kernels_match_reference: {} lanes | matched {} | mismatched {} | \
             kernel lanes traced {} missed {}",
            lanes_checked, matched, mism, traced, missed
        );

        assert!(lanes_checked > 0, "no lanes sampled - the checkpoints are empty");
        assert_eq!(mism, 0, "{} lanes mismatched between kernels and reference", mism);
        assert!(
            traced > 0,
            "no chunk ever reached an ASM kernel - the gate checked nothing"
        );
        assert_eq!(
            missed, 0,
            "{} lanes missed the ASM kernels (fell through to the fallback)",
            missed
        );
    }

}
