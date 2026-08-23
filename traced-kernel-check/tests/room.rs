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
    run.census = std::env::var_os("ROOM_CENSUS").is_some();

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
    // Both sides timed SEPARATELY, in one process, on the same data.
    // The comparison itself (importing the oracle's states into blocks
    // and hashing them) is counted on neither - it is the test's cost,
    // not either engine's.
    let mut t_kernels = std::time::Duration::ZERO;
    let mut t_phase = (
        std::time::Duration::ZERO,
        std::time::Duration::ZERO,
        std::time::Duration::ZERO,
        std::time::Duration::ZERO,
    );
    let mut t_oracle = std::time::Duration::ZERO;
    for frame in 1..=FRAMES {
        let t0 = std::time::Instant::now();
        let stepped = run.step();
        t_kernels += t0.elapsed();
        let st = match stepped {
            Ok(s) => s,
            Err(e) => {
                stopped = Some(format!("{:#}", e));
                break;
            }
        };
        t_phase.0 += st.t_kernel;
        t_phase.1 += st.t_merge;
        t_phase.2 += st.t_boundary;
        let t1 = std::time::Instant::now();
        oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
        t_oracle += t1.elapsed();
        let want: BTreeSet<(u64, u64)> = st.keys.iter().copied().collect();
        let got: BTreeSet<(u64, u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
        eprintln!(
            "[room] frame {:>3}: {:>7} in -> {:>9} raw -> {:>7} out ({:>4}x dropped) in {} block(s); oracle {}",
            st.frame,
            st.rows_in,
            st.rows_raw,
            st.rows_out,
            st.rows_raw / st.rows_out.max(1),
            st.rows_distinct,
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

    // PER-LANE duplication, which is what decides whether a
    // slice-local dedup is worth building. A one-lane block gives
    // exactly one input lane's candidate rows: `Run::step` passes
    // `n = 1`, so `append` writes only lane 0.
    //
    // The aggregate figure (82 appended, 10.4 distinct per lane at
    // frame 30) assumes rows from DIFFERENT lanes never coincide. This
    // does not assume it.
    if std::env::var_os("ROOM_LANE_CENSUS").is_some() {
        let width = run.blocks().first().map(|b| b.width).unwrap_or(0);
        let step = (width / 200).max(1);
        let mut hist: std::collections::BTreeMap<usize, usize> = Default::default();
        let mut tot_raw = 0usize;
        let mut tot_dist = 0usize;
        for lane in (0..width).step_by(step) {
            let Some(b) = run.blocks().first() else { break };
            if lane >= b.width {
                continue;
            }
            let one = b.slice_lanes(lane, lane + 1);
            let mut probe = celeste_rust::trace::run::Run::new(
                kernels::KERNELS,
                one,
                engine.cart(),
                engine.cache(),
            )
            .expect("one-lane run");
            probe.census = true;
            match probe.step() {
                Ok(st) => {
                    tot_raw += st.rows_raw;
                    tot_dist += st.rows_distinct;
                    let f = st.rows_raw / st.rows_distinct.max(1);
                    *hist.entry(f).or_default() += 1;
                }
                Err(e) => eprintln!("[lane] lane {}: {:#}", lane, e),
            }
        }
        // The SAME block, whole: aggregate duplication against the sum
        // of per-lane duplication. The gap between them is rows that
        // coincide ACROSS lanes, which a slice-local dedup cannot see
        // and a global one can.
        if let Some(b) = run.blocks().first() {
            let mut whole = celeste_rust::trace::run::Run::new(
                kernels::KERNELS,
                b.clone_block(),
                engine.cart(),
                engine.cache(),
            )
            .expect("whole-block run");
            whole.census = true;
            if let Ok(st) = whole.step() {
                let per_lane_distinct = (tot_dist as f64 / hist.values().sum::<usize>() as f64)
                    * st.rows_in as f64;
                eprintln!(
                    "[lane] WHOLE block: {} in -> {} appended -> {} distinct ({:.1}x); \
                     per-lane distinct extrapolates to {:.0} ({:.1}x cross-lane)",
                    st.rows_in,
                    st.rows_raw,
                    st.rows_distinct,
                    st.rows_raw as f64 / st.rows_distinct.max(1) as f64,
                    per_lane_distinct,
                    per_lane_distinct / st.rows_distinct.max(1) as f64
                );
            }
        }

        eprintln!(
            "[lane] {} lanes sampled of {}: {} appended -> {} distinct ({:.1}x overall)",
            hist.values().sum::<usize>(),
            width,
            tot_raw,
            tot_dist,
            tot_raw as f64 / tot_dist.max(1) as f64
        );
        for (f, n) in &hist {
            eprintln!("[lane]   {:>3}x duplication: {:>4} lanes", f, n);
        }
    }

    eprintln!(
        "[phase] kernel {:?}, merge {:?}, boundary {:?}",
        t_phase.0, t_phase.1, t_phase.2
    );
    eprintln!(
        "[time] {} frames: kernels {:?}, interpreter {:?} ({:.2}x)",
        checked,
        t_kernels,
        t_oracle,
        t_oracle.as_secs_f64() / t_kernels.as_secs_f64().max(1e-9)
    );

    if let Some(why) = stopped {
        panic!("{} frames agreed, then frame {} stopped: {}", checked, checked + 1, why);
    }
}

/// The rows one side has and the other does not, DECODED.
///
/// A row key is a hash, so "8 extra, 4 missing" names nothing. But the
/// key belongs to a lane, and a lane is a column index into a block, so
/// the values are all there - this finds the lanes whose keys are
/// unmatched and prints their scalars by path.
fn explain(
    run: &celeste_rust::trace::run::Run,
    oracle: &celeste_rust::rewrite::verify::AbstractRun,
    engine: &celeste_rust::compiled::FrameEngine,
) {
    let theirs: Vec<celeste_engine::Rt2> = oracle
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

    // key -> (which block, which lane), both sides.
    let index = |bs: &[celeste_engine::Rt2]| {
        let mut m: std::collections::HashMap<(u64, u64), (usize, usize)> = Default::default();
        for (bi, b) in bs.iter().enumerate() {
            for (lane, k) in b.row_keys.iter().enumerate() {
                m.insert(*k, (bi, lane));
            }
        }
        m
    };
    let a = index(mine);
    let b = index(&theirs);

    let show = |bs: &[celeste_engine::Rt2], at: (usize, usize), what: &str| {
        let blk = &bs[at.0];
        let path = cell_paths(blk);
        let mut fields: Vec<String> = Vec::new();
        for c in 0..blk.structure.len() {
            if !matches!(blk.structure[c], celeste_engine::runtime2::Cell2::Val) {
                continue;
            }
            let Some(name) = path.get(&(c as u32)) else { continue };
            // Only what varies between rows is worth printing; a
            // constant is the same on both sides by construction.
            let v = blk.cols[c].at(at.1);
            let uniform = (0..blk.width).all(|l| blk.cols[c].at(l) == v);
            if uniform {
                continue;
            }
            fields.push(format!("{}={:?}", name, v));
        }
        eprintln!("[row] {} block {} lane {}: {}", what, at.0, at.1, fields.join(" "));
    };

    // Where the two sides put the player, as a histogram. A row-level
    // diff shows which rows differ; this shows whether the DISTRIBUTION
    // differs, which is what says a movement rule disagreed rather than
    // a few lanes being mislabelled.
    let hist = |bs: &[celeste_engine::Rt2], what: &str| {
        let mut h: std::collections::BTreeMap<(i32, i32), usize> = Default::default();
        for blk in bs {
            let path = cell_paths(blk);
            let cell = |name: &str| {
                (0..blk.structure.len() as u32).find(|c| path.get(c).map(|s| s.as_str()) == Some(name))
            };
            let (Some(cx), Some(cs)) = (cell("objects[0].x"), cell("objects[0].spd.x")) else {
                continue;
            };
            for lane in 0..blk.width {
                let g = |c: u32| match blk.cols[c as usize].at(lane) {
                    celeste_engine::runtime2::AV::Num(n) => n.as_raw_u32() as i32 >> 16,
                    _ => i32::MIN,
                };
                *h.entry((g(cx), g(cs))).or_default() += 1;
            }
        }
        let neg: Vec<String> = h
            .iter()
            .filter(|((x, _), _)| *x < 0)
            .map(|((x, s), n)| format!("x={} spd={} : {}", x, s, n))
            .collect();
        eprintln!("[hist] {} rows at negative x: {}", what, neg.join(", "));
    };
    hist(mine, "KERNELS");
    hist(&theirs, "INTERP ");

    let mut extra: Vec<_> = a.iter().filter(|(k, _)| !b.contains_key(*k)).collect();
    let mut missing: Vec<_> = b.iter().filter(|(k, _)| !a.contains_key(*k)).collect();
    extra.sort();
    missing.sort();
    eprintln!("[row] {} extra (kernels only), {} missing (interpreter only)", extra.len(), missing.len());
    for (_, at) in extra.iter().take(8) {
        show(mine, **at, "EXTRA");
    }
    for (_, at) in missing.iter().take(8) {
        show(&theirs, **at, "MISSING");
    }
}

/// The first path from the globals to each cell. A cell reported as a
/// number is a puzzle; reported as `objects[0].spd.x` it is an answer.
fn cell_paths(b: &celeste_engine::Rt2) -> std::collections::HashMap<u32, String> {
    use celeste_engine::runtime2::{Cell2, Col, AV};
    let mut out: std::collections::HashMap<u32, String> = Default::default();
    let mut queue: Vec<(u32, String)> = Vec::new();
    for (g, cell) in b.globals.iter().enumerate() {
        if *cell == celeste_engine::runtime2::NONE {
            continue;
        }
        let name = celeste_names::gen::GLOBAL_NAMES
            .get(g)
            .map(|s| s.to_string())
            .unwrap_or_else(|| format!("g{}", g));
        queue.push((*cell, name));
    }
    let mut i = 0;
    while i < queue.len() {
        let (c, p) = queue[i].clone();
        i += 1;
        if out.contains_key(&c) {
            continue;
        }
        out.insert(c, p.clone());
        match &b.structure[c as usize] {
            Cell2::Val => {
                if let Col::U(AV::Ptr(t)) = &b.cols[c as usize] {
                    queue.push((*t, p.clone()));
                }
            }
            Cell2::Obj(fields) => {
                for (f, t) in fields {
                    let name = celeste_names::gen::FIELD_NAMES
                        .get(*f as usize)
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("f{}", f));
                    queue.push((*t, format!("{}.{}", p, name)));
                }
            }
            Cell2::Arr(items) => {
                for (k, t) in items.iter().enumerate() {
                    queue.push((*t, format!("{}[{}]", p, k)));
                }
            }
            _ => {}
        }
    }
    out
}
