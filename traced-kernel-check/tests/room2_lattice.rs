//! Gate 2 for the constant-lattice kernels (plans/specialize.md): run
//! room (2,0) on the lattice set ALONE and confirm it (a) covers every
//! reachable shape and (b) never declines a block (bd never fires) - the
//! lattice's soundness. With ROOM2_ORACLE set, also compare per-frame
//! row-key SETS to the interpreter.
use std::collections::BTreeSet;
use traced_kernel_check::lattice;

#[test]
fn room2_lattice_runs_and_covers() {
    std::env::set_var("CELESTE_START_ROOM", "2,0");
    std::env::set_current_dir("..").expect("cd to repo root");
    let root = std::path::Path::new(".");
    let frames: usize = std::env::var("ROOM2_FRAMES").ok().and_then(|v| v.parse().ok()).unwrap_or(30);

    let (start, cart, cache) =
        celeste_rust::trace::run::start_block(root).expect("room-2 start block");
    let mut run = celeste_rust::trace::run::Run::new(lattice::KERNELS, start, cart, cache)
        .expect("index lattice kernels by shape");

    let oracle_on = std::env::var_os("ROOM2_ORACLE").is_some();
    let (mut oracle, engine) = if oracle_on {
        let program = celeste_rust::program::frozen::rewritten("rewrites.jsonl").expect("frozen program");
        let engine = celeste_rust::compiled::FrameEngine::new_for_start_room(&program).expect("reference engine");
        let oracle = celeste_rust::search::run::AbstractRun::start(&program).expect("oracle");
        (Some(oracle), Some(engine))
    } else { (None, None) };

    for frame in 1..=frames {
        let st = run.step().unwrap_or_else(|e| panic!("lattice frame {}: {:#}\n(a declined block or an uncovered shape means the lattice over-claimed a constant)", frame, e));
        eprintln!("[r2-lattice] frame {:>3}: {:>7} in -> {:>7} out, {} blocks", st.frame, st.rows_in, st.rows_out, st.blocks_out);
        if let (Some(oracle), Some(engine)) = (oracle.as_mut(), engine.as_ref()) {
            oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
            let want: BTreeSet<(u64, u64)> = st.keys.iter().copied().collect();
            let got: BTreeSet<(u64, u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
            assert_eq!(want, got, "frame {}: lattice {} rows vs interpreter {} - {} missing, {} extra",
                frame, want.len(), got.len(), got.difference(&want).count(), want.difference(&got).count());
        }
    }
    eprintln!("[r2-lattice] {} frames clean{}", frames, if oracle_on { " and matching the interpreter" } else { " (coverage + no declines)" });
}
