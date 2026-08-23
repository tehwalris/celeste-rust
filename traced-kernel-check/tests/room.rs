//! The room, END TO END, on traced kernels only.
//!
//! Everything else in this crate checks one frame against one graph.
//! This runs the loop: `_init`, then N frames through
//! `celeste_rust::trace::run::Run`, with no interpreter anywhere in the
//! path - the kernels are the whole engine.
//!
//! The oracle is the interpreter, run separately: `AbstractRun` over the
//! rewritten program, which is what the campaign runs and what
//! BENCHMARK_DATA's numbers came from. Compared by ROW KEY SET per
//! frame, not by count. Two runs can agree on how many rows survived and
//! disagree about which, and the count is the weaker claim of the two.

use std::collections::BTreeSet;

use traced_kernel_check::kernels;

/// How many frames to run. Enough to be past the spawn animation and
/// into real play, which is where the shapes stop being trivial.
const FRAMES: usize = 30;

#[test]
fn the_room_runs_on_kernels_alone() {
    // From the REPO ROOT, because the oracle side reads `rewrites.jsonl`,
    // `lua/` and `cart/` by relative path and there is no `_in(root)`
    // form of it. Its own process, so nothing else sees this: an
    // integration test is a separate binary, which is the whole reason
    // this file is not in `src/`.
    std::env::set_current_dir("..").expect("cd to the repo root");
    let root = std::path::Path::new(".");
    let (start, cart, cache) =
        celeste_rust::trace::run::start_block(root).expect("the block after _init");
    let mut run = celeste_rust::trace::run::Run::new(kernels::KERNELS, start, cart, cache)
        .expect("index the kernels by shape");

    let mut mine: Vec<BTreeSet<(u64, u64)>> = Vec::new();
    for _ in 0..FRAMES {
        let st = match run.step() {
            Ok(s) => s,
            Err(e) => panic!("{:#}", e),
        };
        eprintln!(
            "[room] frame {:>3}: {:>7} rows in -> {:>7} out in {} block(s)",
            st.frame, st.rows_in, st.rows_out, st.blocks_out
        );
        mine.push(st.keys.iter().copied().collect());
    }

    // The oracle, second, so a failure in the fast path costs nothing.
    let recipe = celeste_rust::rewrite::recipe::Recipe::load("rewrites.jsonl")
        .expect("load the recipe");
    let (program, _) =
        celeste_rust::rewrite::recipe::build(&recipe).expect("build the rewritten program");
    let engine = celeste_rust::compiled::FrameEngine::new_for_start_room(&program)
        .expect("build the reference engine");
    let mut oracle =
        celeste_rust::rewrite::verify::AbstractRun::start(&program).expect("start the oracle");

    for (i, want) in mine.iter().enumerate() {
        oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", i + 1, e));
        let got: BTreeSet<(u64, u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
        let missing = got.difference(want).count();
        let extra = want.difference(&got).count();
        assert!(
            missing == 0 && extra == 0,
            "frame {}: the kernels have {} rows, the interpreter {} - {} missing, {} extra",
            i + 1,
            want.len(),
            got.len(),
            missing,
            extra
        );
    }
}
