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

    // The oracle, IN LOCKSTEP. Built before the loop and stepped inside
    // it, so a run that stops at frame 25 has still checked 24 frames.
    // Collecting one side first and comparing afterwards checks nothing
    // at all when the fast side stops early, which is exactly the case
    // this is for.
    let recipe = celeste_rust::rewrite::recipe::Recipe::load("rewrites.jsonl")
        .expect("load the recipe");
    let (program, _) =
        celeste_rust::rewrite::recipe::build(&recipe).expect("build the rewritten program");
    let engine = celeste_rust::compiled::FrameEngine::new_for_start_room(&program)
        .expect("build the reference engine");
    let mut oracle =
        celeste_rust::rewrite::verify::AbstractRun::start(&program).expect("start the oracle");

    let mut stopped: Option<String> = None;
    let mut checked = 0usize;
    for frame in 1..=FRAMES {
        let st = match run.step() {
            Ok(s) => s,
            Err(e) => {
                stopped = Some(format!("{:#}", e));
                break;
            }
        };
        oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
        let want: BTreeSet<(u64, u64)> = st.keys.iter().copied().collect();
        let got: BTreeSet<(u64, u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
        eprintln!(
            "[room] frame {:>3}: {:>7} rows in -> {:>7} out in {} block(s); oracle {}",
            st.frame,
            st.rows_in,
            st.rows_out,
            st.blocks_out,
            got.len()
        );
        if want != got {
            explain(&run, &oracle, &engine);
            panic!(
                "frame {}: the kernels have {} rows, the interpreter {} - {} missing, {} extra",
                frame,
                want.len(),
                got.len(),
                got.difference(&want).count(),
                want.difference(&got).count()
            );
        }
        checked += 1;
    }

    if let Some(why) = stopped {
        panic!("{} frames agreed, then frame {} stopped: {}", checked, checked + 1, why);
    }
}

/// Where two blocks of the same frame disagree, cell by cell.
///
/// A row-key difference says only that they disagree. The keys are
/// hashes over every live cell, so recovering WHICH cell from them is
/// impossible - and the interesting failures ("the shape is the same but
/// one scalar differs" versus "the structures differ at all") look
/// identical until someone walks both.
fn explain(
    run: &celeste_rust::trace::run::Run,
    oracle: &celeste_rust::rewrite::verify::AbstractRun,
    engine: &celeste_rust::compiled::FrameEngine,
) {
    let mut theirs: Vec<celeste_engine::Rt2> = oracle
        .states()
        .iter()
        .filter(|s| s.vector_size > 0)
        .map(|s| {
            let mut b = celeste_rust::compiled::bridge::import_block(
                s,
                engine.cart(),
                engine.cache(),
            );
            b.boundary(engine.ids());
            b
        })
        .collect();
    let mine = run.blocks();
    eprintln!("[diff] kernels: {} block(s), interpreter: {}", mine.len(), theirs.len());
    for (i, a) in mine.iter().enumerate() {
        eprintln!(
            "[diff] kernel block {}: shape {:#x}, {} lanes, {} cells",
            i,
            a.shape_hash_of(),
            a.width,
            a.structure.len()
        );
    }
    for (i, b) in theirs.iter().enumerate() {
        eprintln!(
            "[diff] interp block {}: shape {:#x}, {} lanes, {} cells",
            i,
            b.shape_hash_of(),
            b.width,
            b.structure.len()
        );
    }
    // Pair by shape where possible, else by index. A shape that appears
    // on one side only is itself the answer, but "the hashes differ" is
    // not - the hash is over the whole structure plus the globals, so the
    // useful report is the FIRST place they diverge.
    for (i, a) in mine.iter().enumerate() {
        let b = match theirs.iter_mut().find(|b| b.shape_hash_of() == a.shape_hash_of()) {
            Some(b) => b,
            None => {
                eprintln!(
                    "[diff] no interpreter block has shape {:#x}; comparing with block {} anyway",
                    a.shape_hash_of(),
                    i
                );
                match theirs.get_mut(i) {
                    Some(b) => b,
                    None => continue,
                }
            }
        };
        if a.globals != b.globals {
            for (g, (x, y)) in a.globals.iter().zip(b.globals.iter()).enumerate() {
                if x != y {
                    eprintln!(
                        "[diff] global {} ({:?}): cell {:?} vs {:?}",
                        g,
                        celeste_names::gen::GLOBAL_NAMES.get(g),
                        x,
                        y
                    );
                }
            }
        }
        if a.structure.len() != b.structure.len() {
            eprintln!(
                "[diff] {} cells vs {}",
                a.structure.len(),
                b.structure.len()
            );
        }
        let mut shown = 0;
        for c in 0..a.structure.len().min(b.structure.len()) {
            if a.structure[c] != b.structure[c] {
                if shown < 10 {
                    eprintln!(
                        "[diff] cell {}: structure {:?} vs {:?}",
                        c, a.structure[c], b.structure[c]
                    );
                }
                shown += 1;
                continue;
            }
            for lane in 0..a.width.min(b.width) {
                if a.cols[c].at(lane) != b.cols[c].at(lane) {
                    if shown < 10 {
                        eprintln!(
                            "[diff] cell {} lane {}: {:?} vs {:?}",
                            c,
                            lane,
                            a.cols[c].at(lane),
                            b.cols[c].at(lane)
                        );
                    }
                    shown += 1;
                    break;
                }
            }
        }
        eprintln!("[diff] {} cells differ in block {}", shown, i);
    }
}
