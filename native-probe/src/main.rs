//! Driver for the compiled abstract engine (native-compile probe).
//!
//! What runs a frame here is a generated kernel per shape class
//! (`kernel_gen_*`), with the celeste-rust interpreter as the fallback for
//! any chunk a kernel declines. `gen.rs` is no longer a program: it is the
//! interned NAME TABLES (globals, fields, fns, sites) that the boundary
//! hashes and that `import`/`export` translate through.
//!
//! Modes: `--abstract N` runs the forward search N frames; `--abstract-bench
//! DIR FRAME` is the one-frame dev loop and its row-key gate; `--row-census`,
//! `--emit-shape` and `--kernel-bench` are the kernel-authoring tools.

mod builtins;
mod import;
mod runtime2;
pub mod kernel;

// The GENERATED modules, and the only ones allowed to carry dead code.
//
// They emit a complete surface - every name table, every per-cell
// constant, the `apply` for each class - and any one consumer uses a
// subset of it, so `dead_code` here means "this build did not need that
// entry", not "someone forgot to delete something". The allow is scoped
// to these four modules deliberately: it used to be crate-wide
// (`#![allow(unused_variables, unused_assignments, unused_mut,
// unreachable_code, dead_code)]`, for the transpiled program body that no
// longer exists), and a crate-wide version of this is exactly how ~150
// lines of hand-written dead code hid here before.
#[allow(dead_code)]
mod gen;
#[allow(dead_code)]
mod kernel_gen_dash;
#[allow(dead_code)]
mod kernel_gen_frozen;
#[allow(dead_code)]
mod kernel_gen_steady;
use kernel_gen_steady as kernel_gen;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use celeste_rust::cart_data::CartData;
use celeste_rust::collision_cache::CollisionCache;

/// Load boundary states from either checkpoint layout: `frames/fNNN.bin`
/// (the campaign frame batches) or `fNNN/states.bin` (`rewrite bench
/// --checkpoint-dir`). For the bench layout the fingerprint check is
/// self-supplied from meta.json - the census wants states, not resume
/// safety.
fn load_states_any(dir: &str, frame: u32) -> Vec<celeste_rust::interpreter::state::State> {
    use celeste_rust::rewrite::checkpoint;
    let path = std::path::Path::new(dir);
    if path.join("frames").join(format!("f{:03}.bin", frame)).exists() {
        return checkpoint::load_frame_states(path, frame).expect("load frame states");
    }
    let meta_path = path.join(format!("f{:03}", frame)).join("meta.json");
    let meta: serde_json::Value = serde_json::from_str(
        &std::fs::read_to_string(&meta_path)
            .unwrap_or_else(|e| panic!("read {}: {}", meta_path.display(), e)),
    )
    .expect("parse meta.json");
    let fp = meta["fingerprint"].as_str().expect("meta.json fingerprint");
    let (_, states) = checkpoint::load_light(path, frame, fp).expect("load_light states");
    states
}

fn main() {
    let mut abstract_frames: Option<u32> = None;
    let mut abstract_bench: Option<(String, u32)> = None;
    let mut row_census: Option<(String, u32)> = None;
    let mut emit_shape: Option<(String, u32, String, String)> = None;
    let mut kernel_bench: Option<(String, u32)> = None;
    let mut reps: u32 = 10;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--abstract" => {
                abstract_frames =
                    Some(args.next().expect("--abstract needs FRAMES").parse().unwrap())
            }
            "--abstract-bench" => {
                let dir = args.next().expect("--abstract-bench needs DIR FRAME");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                abstract_bench = Some((dir, frame));
            }
            "--reps" => reps = args.next().expect("value").parse().unwrap(),
            "--row-census" => {
                let dir = args.next().expect("--row-census needs DIR FRAME");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                row_census = Some((dir, frame));
            }
            "--emit-shape" => {
                let dir = args.next().expect("--emit-shape needs DIR FRAME OUT [CLASS]");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                let out = args.next().expect("OUT");
                let class = args.next().unwrap_or_else(|| "steady".to_string());
                emit_shape = Some((dir, frame, out, class));
            }
            "--kernel-bench" => {
                let dir = args.next().expect("--kernel-bench needs DIR FRAME");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                kernel_bench = Some((dir, frame));
            }
            other => panic!("unknown argument {:?}", other),
        }
    }

    if let Some(n) = abstract_frames {
        run_abstract(n);
    } else if let Some((dir, frame)) = row_census {
        run_row_census(&dir, frame);
    } else if let Some((dir, frame, out, class)) = emit_shape {
        run_emit_shape(&dir, frame, &out, &class);
    } else if let Some((dir, frame)) = kernel_bench {
        run_kernel_bench(&dir, frame, reps);
    } else if let Some((dir, frame)) = abstract_bench {
        run_abstract_bench(&dir, frame, reps);
    } else {
        panic!("nothing to do - pass --abstract N, --abstract-bench DIR FRAME, --row-census, --emit-shape or --kernel-bench");
    }
}

fn boundary_ids() -> runtime2::BoundaryIds {
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    runtime2::BoundaryIds {
        g_objects: g("objects"),
        g_player: g("player"),
        g_timers: ["frames", "seconds", "minutes", "deaths"].iter().map(|n| g(n)).collect(),
        f_type: f("type"),
        f_rem: f("rem"),
        f_spd: f("spd"),
        f_x: f("x"),
        f_y: f("y"),
        f_dash_effect_time: f("dash_effect_time"),
        // The recipe's partition_merge (pm1) key. `has_dashed` and
        // `freeze` are globals; the rest are player fields.
        g_pm1: ["has_dashed", "freeze"].iter().map(|n| g(n)).collect(),
        f_pm1: ["dash_time", "djump", "p_dash", "p_jump"].iter().map(|n| f(n)).collect(),
    }
}

/// The reference frame: the celeste-rust INTERPRETER, on a block exported
/// back to an interpreter `State` (plans/k4-retirement-plan.md stage 2).
///
/// This is what a chunk falls back to when the kernels decline it, and it
/// is the reference in the strongest sense available: the same `Program`,
/// the same `interpret_prepared_cfg`, the same code the campaign and
/// `concrete_run` execute. The compiled engine's own reference path
/// (`gen::call_fn` over the transpiled program) is a SECOND
/// implementation of those semantics, which is exactly why it is being
/// retired - two implementations of a reference is one too many.
///
/// It also removes the SplitReq dance: a genuinely lane-divergent branch
/// used to panic out, partition the frame-start block by the condition's
/// per-origin truth, and rerun both halves. The interpreter splits
/// internally and simply returns more than one output state.
///
/// Speed does not matter here and that is a measured claim, not a hope:
/// since the class kernels reached 100% of player lanes, this path runs
/// on spawn shapes only (0.72 ms at f20).
struct Fallback {
    frame_cfg: celeste_rust::interpreter::fixed_env::PreparedCfg,
    fixed_env: celeste_rust::interpreter::fixed_env::FixedEnv,
    init_cfg: celeste_rust::ir::Cfg,
}

/// The recipe `gen.rs` and the kernels were generated from. It MUST be
/// this one and not the plain program: `transpile --recipe
/// rewrites-compile.jsonl` is the canonical regen (plans/columnar-engine
/// .md), so the compiled engine executes the REWRITTEN program, and so
/// must anything claiming to be its reference.
///
/// Cost me an afternoon: gen.rs's generated header says "Source program:
/// Program::compile_executable_from_disk()" unconditionally, which is
/// simply false when --recipe was passed. The plain program's frame boxes
/// a captured `self` where the recipe's `demote_create` does not, so the
/// output heap gained one cell, every later cell id shifted, and the gate
/// reported 204 rows missing and 204 extra - a total mismatch produced by
/// an aliasing difference in ONE closure capture. The header is fixed in
/// the emitter.
const COMPILE_RECIPE: &str = "rewrites-compile.jsonl";

/// Built once per process - loading the recipe and applying it compiles
/// the whole cart, which is seconds, and every worker thread shares the
/// result by reference (both halves are Sync; verify.rs's parallel frame
/// already shares exactly these two).
fn fallback() -> &'static Fallback {
    static FALLBACK: std::sync::OnceLock<Fallback> = std::sync::OnceLock::new();
    FALLBACK.get_or_init(|| {
        let recipe = celeste_rust::rewrite::recipe::Recipe::load(COMPILE_RECIPE)
            .unwrap_or_else(|e| panic!("loading {} (run from the repo root): {}", COMPILE_RECIPE, e));
        let (program, _) = celeste_rust::rewrite::recipe::build(&recipe)
            .unwrap_or_else(|e| panic!("applying {}: {}", COMPILE_RECIPE, e));
        // The recipe's partition_merge (pm1) cells are a PROCESS GLOBAL in
        // the interpreter, and `AbstractRun::start` sets them before it
        // runs anything. Any frame this process interprets - the fallback,
        // and the init below - has to run under the same setting or it
        // merges differently from the reference it claims to be.
        celeste_rust::interpreter::vectorize::set_merge_partition_patterns(
            &program.merge_partition_cells,
        );
        Fallback {
            frame_cfg: celeste_rust::interpreter::fixed_env::PreparedCfg::new(
                program.frame_cfg().clone(),
            ),
            fixed_env: program.fixed_env(),
            init_cfg: program.init_cfg().clone(),
        }
    })
}

/// Cart data and the collision cache for the campaign's start room.
///
/// These used to be a by-product of `build_rt`, which ran `__init` on the
/// scalar runtime and was mostly wanted for its side effects; the scalar
/// runtime is gone, so they are loaded directly.
fn world() -> &'static (std::sync::Arc<CartData>, std::sync::Arc<CollisionCache>) {
    static WORLD: std::sync::OnceLock<(std::sync::Arc<CartData>, std::sync::Arc<CollisionCache>)> =
        std::sync::OnceLock::new();
    WORLD.get_or_init(|| {
        let (room_x, room_y) = celeste_rust::game_runner::start_room();
        // Runnable both from the repo root and from native-probe/.
        let cart_base = if std::path::Path::new("cart").exists() { "cart" } else { "../cart" };
        let cart = std::sync::Arc::new(CartData::load(cart_base).expect("failed to load cart data"));
        let cache = std::sync::Arc::new(
            CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
        );
        eprintln!("[native-probe] collision cache for room ({}, {})", room_x, room_y);
        (cart, cache)
    })
}

/// The frame-0 frontier: run the program's `__init` through the
/// interpreter and import the resulting states as blocks.
///
/// This is the same construction as `verify.rs`'s `AbstractRun::start` -
/// same program, same `create_initial_state_with_builtins`, same
/// `inject_tile_flag_at_builtin` afterwards - which is what makes
/// `--abstract N` comparable to `rewrite bench --frames N` at all. The
/// scalar runtime used to reimplement this by executing the transpiled
/// `__init` and hand-placing the builtin cells; two implementations of a
/// starting position is one too many, and this one cannot drift.
fn initial_blocks() -> Vec<runtime2::Rt2> {
    let fb = fallback();
    let (cart, cache) = world();
    let initial = celeste_rust::game_runner::create_initial_state_with_builtins(&fb.fixed_env);
    let states = celeste_rust::interpreter::glue::interpret_cfg(
        fb.init_cfg.clone(),
        initial,
        &fb.fixed_env,
    )
    .expect("init failed");
    states
        .into_iter()
        .map(|(mut s, _)| {
            celeste_rust::game_runner::inject_tile_flag_at_builtin(&mut s);
            import::import_block(&s, cart.clone(), cache.clone())
        })
        .collect()
}

/// Run one frame of `block` through the interpreter and return the
/// boundary blocks. The output goes through the SAME `Rt2::boundary` the
/// compiled path uses, so the canonical form has one implementation
/// whichever engine produced the rows.
fn run_chunk_interpreted(
    block: runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
) -> Vec<runtime2::Rt2> {
    let (cart, cache) = (block.cart.clone(), block.cache.clone());
    // CELESTE_FALLBACK_ROUNDTRIP=1 checks export against import on the
    // way in, which is what separates "the exporter lost something" from
    // "the frame did something different" when the gate disagrees.
    if std::env::var_os("CELESTE_FALLBACK_ROUNDTRIP").is_some() {
        import::assert_block_round_trips(&block);
    }
    let state = import::export_block(&block);
    drop(block);
    let fb = fallback();
    let outputs = celeste_rust::interpreter::glue::interpret_prepared_cfg(
        &fb.frame_cfg,
        state,
        &fb.fixed_env,
    )
    .expect("the interpreter fallback failed a frame");
    outputs
        .into_iter()
        .map(|(s, _)| s)
        // A frame can split a chunk into pieces and leave one empty; an
        // empty block has no rows to contribute and `import_block` has
        // nothing to build a shape from.
        .filter(|s| s.vector_size > 0)
        .map(|s| {
            let mut b = import::import_block(&s, cart.clone(), cache.clone());
            b.boundary(ids);
            b
        })
        .collect()
}

/// One abstract frame forward: pre-partition (freeze, moving key), chunk,
/// run tiles across threads (SplitReq -> partition + rerun), boundary,
/// cross-block dedup, k-way same-shape merge. Rows in -> rows out.
fn frame_step(
    blocks: Vec<runtime2::Rt2>,
    ids: &runtime2::BoundaryIds,
    g_freeze: u32,
    census_total: &mut rustc_hash::FxHashMap<&'static str, (u64, u64, u64)>,
) -> Vec<runtime2::Rt2> {
    // The lane kernels are the compiled engine (plans/kernel-plan.md);
    // chunks they refuse fall through to the reference. The retired tile
    // engines (CELESTE_TILE=1 concrete-button tiles, =2 dynamic expand)
    // are gone - the kernels cover every player class and are ~5x faster
    // (plans/k4-retirement-plan.md). CELESTE_KERNEL=0 routes everything
    // to the reference for A/B.
    let use_kernel = std::env::var("CELESTE_KERNEL").map(|v| v != "0").unwrap_or(true);
    // CELESTE_PHASE_TIME=1: print the per-frame wall split across the
    // serial/parallel phases (goal 7's measurement harness).
    let phase_time = std::env::var("CELESTE_PHASE_TIME").is_ok();
    let mut t_mark = std::time::Instant::now();
    let mut phase = |name: &str| {
        if phase_time {
            eprintln!("    phase {:8} {:9.3?}", name, t_mark.elapsed());
        }
        t_mark = std::time::Instant::now();
    };
    let mut ran: Vec<runtime2::Rt2> = Vec::new();
    let mut pending: Vec<runtime2::Rt2> = Vec::new();
    // Chunk cap. Cross-chunk dedup at the boundary makes chunking
    // invisible to the result (batching invariance is the certified
    // doctrine), so this is purely a cost knob - and pre-dedup INVERTED
    // it. While every emitted row was materialized, a chunk's mid-frame
    // traffic dominated and small chunks won; now duplicates die as a
    // hash probe and a bigger chunk simply catches more of them, so the
    // dedup ratio wins instead. Measured at f35 (min of 5 reps, peak
    // RSS), all gates exact:
    //
    //   lanes:  64      128     256     512     1024    4096
    //   before: 348 ms  489     584     673     -       -
    //   after:  133 ms  98      91      86      82      85
    //   RSS:    0.99 GB  -      1.12    1.44    2.02    4.82
    //
    // 256 is the knee: 1.46x over the old default for +13% memory, and
    // the mean stays as tight as the min (512's does not).
    let chunk_rows: usize = std::env::var("CELESTE_CHUNK_ROWS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(256);
    for block in blocks {
        let freeze_cell = block.globals[g_freeze as usize];
        assert!(freeze_cell != runtime2::NONE);
        for sub in block.partition_by_cell(freeze_cell) {
            let parts = match sub.moving_key(ids) {
                Some(key) => {
                    let key = key.clone();
                    sub.partition_by_key(&key)
                }
                None => vec![sub],
            };
            for part in parts {
                if part.width <= chunk_rows {
                    pending.push(part);
                } else {
                    let n = part.width;
                    let mut at = 0;
                    while at < n {
                        let hi = (at + chunk_rows).min(n);
                        pending.push(part.slice_lanes(at, hi));
                        at = hi;
                    }
                }
            }
        }
    }
    phase("part");
    // Chunks are independent (lane independence is the certified
    // batching-invariance property); run them across threads. Each
    // worker owns a local pending stack seeded round-robin.
    let n_workers = std::thread::available_parallelism()
        .map(|n| n.get().saturating_sub(2).max(1))
        .unwrap_or(1)
        .min(pending.len().max(1));
    // (block, kernel_ok): kernel-deopted leftovers and SplitReq halves
    // must not re-enter the kernel (a lane the kernel deopted once would
    // deopt forever - an infinite requeue).
    let queues: Vec<Vec<(runtime2::Rt2, bool)>> = {
        let mut qs: Vec<Vec<(runtime2::Rt2, bool)>> = (0..n_workers).map(|_| Vec::new()).collect();
        for (i, b) in pending.drain(..).enumerate() {
            qs[i % n_workers].push((b, true));
        }
        qs
    };
    let results: Vec<Vec<runtime2::Rt2>> = std::thread::scope(|scope| {
        let handles: Vec<_> = queues
            .into_iter()
            .map(|mut local| {
                let ids = &ids;
                scope.spawn(move || {
                    let mut done: Vec<runtime2::Rt2> = Vec::new();
                    while let Some((block, kernel_ok)) = local.pop() {
                        if use_kernel && kernel_ok {
                            // KERNEL mode (plans/kernel-plan.md K3): the
                            // steady-class lane kernel first; rows it
                            // deopts re-enter the worklist for the
                            // reference paths; a chunk it cannot bind or
                            // whose uniform premise fails falls through
                            // whole.
                            if run_chunk_kernel(&block, ids, &mut done, &mut local) {
                                continue;
                            }
                        }
                        // The reference: the interpreter, on the block
                        // exported back to a State. This replaced the
                        // transpiled-program path (gen::call_fn over the
                        // Rt2 Engine impl) and its SplitReq worklist -
                        // a divergent branch used to panic out so the
                        // driver could partition the frame-start block
                        // by the condition's per-origin truth and rerun
                        // both halves; the interpreter splits internally
                        // and just returns more than one output state.
                        done.extend(run_chunk_interpreted(block, ids));
                    }
                    done
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    });
    phase("run");
    for done in results {
        ran.extend(done);
    }
    for sub in ran.iter_mut() {
        sub.drain_census(census_total);
    }
    // Drop rows already seen this frame (SHARDED parallel dedup: a
    // row's shard is a function of its key, so shards are
    // independent; first-occurrence order within the block sequence
    // is preserved per shard, and the surviving SET - which is all
    // identity requires - is order-independent), then k-way merge
    // same-shape blocks.
    let n_shards = 32usize;
    let keeps: Vec<Vec<u32>> = {
        // (block, lane, key) triples grouped by shard, in block order.
        let mut per_shard_keeps: Vec<Vec<Vec<u32>>> =
            (0..n_shards).map(|_| vec![Vec::new(); ran.len()]).collect();
        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..n_shards)
                .map(|shard| {
                    let ran = &ran;
                    scope.spawn(move || {
                        let mut seen: rustc_hash::FxHashMap<(u64, u64), ()> =
                            Default::default();
                        let mut keeps: Vec<Vec<u32>> = vec![Vec::new(); ran.len()];
                        for (bi, sub) in ran.iter().enumerate() {
                            for (i, &k) in sub.row_keys.iter().enumerate() {
                                if (k.0 as usize) % n_shards != shard {
                                    continue;
                                }
                                if let std::collections::hash_map::Entry::Vacant(e) =
                                    seen.entry(k)
                                {
                                    e.insert(());
                                    keeps[bi].push(i as u32);
                                }
                            }
                        }
                        keeps
                    })
                })
                .collect();
            for (shard, h) in handles.into_iter().enumerate() {
                per_shard_keeps[shard] = h.join().unwrap();
            }
        });
        // Merge shards' keeps per block, sorted (retain_lanes needs
        // ascending indices).
        (0..ran.len())
            .map(|bi| {
                let mut keep: Vec<u32> = per_shard_keeps
                    .iter()
                    .flat_map(|s| s[bi].iter().copied())
                    .collect();
                keep.sort_unstable();
                keep
            })
            .collect()
    };
    phase("dedup");
    // Group for the k-way merge by (shape, pm1 key): blocks stay
    // partitioned by the recipe's fork-condition cells instead of
    // densifying into one wide block per shape. This is the
    // interpreter's fragment representation (its measured 2-3x edge at
    // depth): key-correlated columns stay Col::U through storage,
    // merge, and the next frame's boundary hashing.
    let mut groups: Vec<((u64, u64), Vec<runtime2::Rt2>)> = Vec::new();
    for (mut sub, keep) in ran.into_iter().zip(keeps) {
        sub.retain_lanes(&keep);
        if sub.width == 0 {
            continue;
        }
        for part in sub.partition_pm1(ids) {
            if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
                let mixed = part
                    .pm1_cells(ids)
                    .iter()
                    .filter(|c| !matches!(part.cols[**c as usize], runtime2::Col::U(_)))
                    .count();
                if mixed > 0 {
                    static ONCE2: std::sync::Once = std::sync::Once::new();
                    ONCE2.call_once(|| {
                        eprintln!(
                            "[partition_pm1] part width {} still has {} mixed pm1 columns; cells {:?}",
                            part.width,
                            mixed,
                            part.pm1_cells(ids)
                        );
                        for c in part.pm1_cells(ids) {
                            let d = match &part.cols[c as usize] {
                                runtime2::Col::U(_) => "uniform".to_string(),
                                runtime2::Col::N(vs) => {
                                    let mut s: Vec<String> =
                                        vs.iter().map(|v| format!("{:?}", v)).collect();
                                    s.sort();
                                    s.dedup();
                                    format!("N {:?}", s)
                                }
                                runtime2::Col::V(vs) => {
                                    let mut s: Vec<String> =
                                        vs.iter().map(|v| format!("{:?}", v)).collect();
                                    s.sort();
                                    s.dedup();
                                    format!("V {:?}", s)
                                }
                                runtime2::Col::I(_) => "I".to_string(),
                            };
                            eprintln!("    cell {}: {}", c, d);
                        }
                    });
                }
            }
            let key = (part.shape_hash, part.pm1_key_hash(ids));
            match groups.iter_mut().find(|(h, _)| *h == key) {
                Some((_, g)) => g.push(part),
                None => groups.push((key, vec![part])),
            }
        }
    }
    phase("retain");
    let out: Vec<runtime2::Rt2> = groups
        .into_iter()
        .map(|(_, g)| runtime2::Rt2::merge_many(g))
        .collect();
    phase("merge");
    if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
        let mut mixed = 0usize;
        for b in &out {
            for c in b.pm1_cells(ids) {
                if !matches!(b.cols[c as usize], runtime2::Col::U(_)) {
                    mixed += 1;
                }
            }
        }
        eprintln!(
            "[frame_step] {} out blocks, {} non-uniform pm1 columns",
            out.len(),
            mixed
        );
    }
    out
}

/// The columnar abstract engine (plans/columnar-engine.md): run the level-0
/// abstract search natively for N frames from the room start, printing
/// per-frame lane counts - gate 1 is exact equality with
/// `rewrite bench --frames N` (room (1,0), CELESTE_REM_BITS unset).
///
/// The frontier is a LIST of blocks (per shape); each block is
/// pre-partitioned by the freeze global before the frame runs (the
/// update-side freeze gate is a real per-lane branch - pm1's precedent);
/// after the boundary's canonical compaction, same-shape blocks merge and
/// cross-block duplicate rows drop.
fn run_abstract(num_frames: u32) {
    let ids = boundary_ids();
    let g_freeze = gen::global_id("freeze").expect("no freeze global");
    let mut blocks: Vec<runtime2::Rt2> = initial_blocks();
    let mut census_total: rustc_hash::FxHashMap<&'static str, (u64, u64, u64)> =
        Default::default();
    let start = std::time::Instant::now();
    for frame in 1..=num_frames {
        let t0 = std::time::Instant::now();
        // Pre-partition each block by the freeze value (the known
        // frame-start divergent gate), then run. A block that hits another
        // genuinely divergent branch throws a SplitReq with the
        // per-frame-start-lane truth of the condition; partition the
        // frame-start block by it and rerun both sides.
        blocks = frame_step(blocks, &ids, g_freeze, &mut census_total);
        let lanes: usize = blocks.iter().map(|b| b.width).sum();
        let (splits, appended, arena_peak): (u64, u64, usize) = blocks.iter().fold(
            (0, 0, 0),
            |(s, a, p), b| (s.max(b.stat_splits), a.max(b.stat_appended), p.max(b.stat_arena_peak)),
        );
        println!(
            "frame {:3}: {:8} lanes in {} block(s)  {:9.3?}  ({} splits, {} appended, arena peak {})",
            frame,
            lanes,
            blocks.len(),
            t0.elapsed(),
            splits,
            appended,
            arena_peak,
        );
    }
    println!("abstract: {} frames in {:.3?}", num_frames, start.elapsed());
    let shapes: Vec<String> =
        blocks.iter().map(|b| format!("{:#018x}", b.shape_hash)).collect();
    println!("final shapes: {}", shapes.join(", "));
    if !census_total.is_empty() {
        let mut rows: Vec<_> = census_total.into_iter().collect();
        rows.sort_by_key(|(_, (ns, _, _))| std::cmp::Reverse(*ns));
        println!("op census (name, total ms, calls):");
        for (name, (ns, calls, _)) in rows {
            println!("  {:14} {:9.1} ms  {:>12} calls", name, ns as f64 / 1e6, calls);
        }
    }
}

/// K0 recon for the kernel emitter (plans/kernel-plan.md): per-column
/// type census over the real boundary blocks of one frame, split by
/// (shape, pm1 class). Answers: which columns are Num-only / Interval /
/// mixed across the steady class, how many rows the steady kernel covers,
/// and which columns are uniform-per-block (broadcast candidates).
fn run_row_census(dir: &str, frame: u32) {
    let (cart, cache) = world();
    let states = load_states_any(dir, frame);
    // pm1 class per state, via the interpreter-side cell names (states
    // are class-uniform at the boundary).
    let class_of = |st: &celeste_rust::interpreter::state::State| -> (String, String) {
        use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
        let names = celeste_rust::interpreter::merge_dump::cell_names(st);
        let shape = celeste_rust::interpreter::abstraction::object_shape(st)
            .map(|s| s.join(","))
            .unwrap_or_else(|_| "?".into());
        let mut freeze = String::from("?");
        let mut dash = String::from("?");
        for (cell, name) in names.iter() {
            let is_freeze = name == "freeze";
            let is_dash = name.ends_with(".dash_time");
            if !is_freeze && !is_dash {
                continue;
            }
            let text = match st
                .heap
                .get_opt(celeste_rust::interpreter::heap::HeapId::from_raw(*cell))
            {
                Some(HeapValue::Value(Value::Number(MaybeVector::Scalar(n)))) => {
                    format!("{}", n.whole_part_as_i16())
                }
                Some(HeapValue::Value(Value::Number(MaybeVector::Vector(_)))) => "vec".into(),
                _ => "?".into(),
            };
            if is_freeze {
                freeze = text;
            } else {
                dash = text;
            }
        }
        (shape, format!("freeze={} dash={}", freeze, dash))
    };
    let mut by_class: std::collections::BTreeMap<(String, String), (usize, usize)> =
        Default::default();
    let mut steady_blocks: Vec<runtime2::Rt2> = Vec::new();
    let mut col_names: Vec<String> = Vec::new();
    for st in &states {
        let (shape, class) = class_of(st);
        let e = by_class.entry((shape.clone(), class.clone())).or_default();
        e.0 += 1;
        e.1 += st.vector_size;
        if shape == "player" && class == "freeze=0 dash=0" {
            steady_blocks.push(import::import_block(st, cart.clone(), cache.clone()));
            if col_names.is_empty() {
                let (_, rev) =
                    import::import_block_mapped(st, cart.clone(), cache.clone());
                let names = celeste_rust::interpreter::merge_dump::cell_names(st);
                col_names = rev
                    .iter()
                    .map(|h| {
                        h.and_then(|h| names.get(&h.raw()).cloned())
                            .unwrap_or_else(|| "?".into())
                    })
                    .collect();
            }
        }
    }
    let total: usize = states.iter().map(|s| s.vector_size).sum();
    println!("row census f{:03}: {} states, {} lanes", frame, states.len(), total);
    for ((shape, class), (n, lanes)) in &by_class {
        println!(
            "  [{}] {}: {} state(s), {} lanes ({:.1}%)",
            shape,
            class,
            n,
            lanes,
            100.0 * *lanes as f64 / total as f64
        );
    }
    // Column type occupancy across the steady blocks.
    let ncols = steady_blocks.iter().map(|b| b.cols.len()).max().unwrap_or(0);
    println!(
        "steady blocks: {} ({} lanes), {} columns max",
        steady_blocks.len(),
        steady_blocks.iter().map(|b| b.width).sum::<usize>(),
        ncols
    );
    println!("cell  uniform varyN varyI varyV  (V content)   <- per steady block counts");
    for c in 0..ncols {
        let (mut u, mut n, mut i, mut v) = (0usize, 0usize, 0usize, 0usize);
        let mut vkinds: std::collections::BTreeSet<&'static str> = Default::default();
        let mut ukinds: std::collections::BTreeSet<&'static str> = Default::default();
        let kind = |a: &runtime2::AV| -> &'static str {
            match a {
                runtime2::AV::Num(_) => "num",
                runtime2::AV::Ival(_, _) => "ival",
                runtime2::AV::Bool(_) => "bool",
                runtime2::AV::UBool => "ubool",
                runtime2::AV::Str(_) => "str",
                runtime2::AV::Nil => "nil",
                runtime2::AV::Ptr(_) => "ptr",
                runtime2::AV::NilPtr => "nilptr",
            }
        };
        for b in &steady_blocks {
            match b.cols.get(c) {
                None => {}
                Some(runtime2::Col::U(a)) => {
                    u += 1;
                    ukinds.insert(kind(a));
                }
                Some(runtime2::Col::N(_)) => n += 1,
                Some(runtime2::Col::I(_)) => i += 1,
                Some(runtime2::Col::V(vals)) => {
                    v += 1;
                    for a in vals {
                        vkinds.insert(kind(a));
                    }
                }
            }
        }
        if n + i + v > 0 || ukinds.iter().any(|k| *k == "ival" || *k == "ubool") {
            println!(
                "{:>4}  {:>7} {:>5} {:>5} {:>5}  V={:?} U={:?}  {}",
                c,
                u,
                n,
                i,
                v,
                vkinds,
                ukinds,
                col_names.get(c).map(|s| s.as_str()).unwrap_or("?")
            );
        }
    }
}

/// Emit the KERNEL SHAPE WITNESS (plans/kernel-plan.md K1): the steady
/// class's cell topology + column classification, as JSON the transpiler's
/// kernel emitter consumes. Topology (globals, object fields, arrays,
/// closures) is by NAME so the consumer needs no shared interner; the
/// varying set is the UNION over all steady blocks of the frame.
/// Cells that must be recorded as PER-LANE even if this frame's blocks
/// happen to hold one value for them. A witness is a sample: a cell the
/// sampled frame agrees on can still vary in a block the engine produces
/// a frame later, and the kernel would then refuse to bind (that is
/// exactly what dash_effect_time did - see plans/kernel-plan.md). Only
/// the pm1 cells are uniform by CONSTRUCTION, because the partitioner
/// splits on them; anything else is uniform by luck.
///
/// Set CELESTE_FORCE_VARY=name,name to extend the list.
fn force_vary_names() -> Vec<String> {
    let mut names: Vec<String> = vec!["dash_effect_time".to_string()];
    if let Ok(v) = std::env::var("CELESTE_FORCE_VARY") {
        names.extend(v.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()));
    }
    names
}

fn run_emit_shape(dir: &str, frame: u32, out_path: &str, class: &str) {
    use serde_json::json;
    let (cart, cache) = world();
    let states = load_states_any(dir, frame);
    let steady: Vec<&celeste_rust::interpreter::state::State> = states
        .iter()
        .filter(|st| {
            use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
            let names = celeste_rust::interpreter::merge_dump::cell_names(st);
            let shape_ok = celeste_rust::interpreter::abstraction::object_shape(st)
                .map(|s| s == vec!["player".to_string()])
                .unwrap_or(false);
            let val_of = |want: &str| -> Option<i32> {
                names.iter().find_map(|(cell, name)| {
                    if (want == "freeze" && name == "freeze")
                        || (want == "dash" && name.ends_with(".dash_time"))
                    {
                        match st.heap.get_opt(
                            celeste_rust::interpreter::heap::HeapId::from_raw(*cell),
                        ) {
                            Some(HeapValue::Value(Value::Number(MaybeVector::Scalar(n)))) => {
                                Some(n.as_raw_u32() as i32)
                            }
                            _ => None,
                        }
                    } else {
                        None
                    }
                })
            };
            let (fz, da) = (val_of("freeze"), val_of("dash"));
            let class_ok = match class {
                "steady" => fz == Some(0) && da == Some(0),
                "dash" => fz == Some(0) && matches!(da, Some(v) if v > 0),
                "frozen" => matches!(fz, Some(v) if v > 0),
                other => panic!("unknown class {:?}", other),
            };
            shape_ok && class_ok
        })
        .collect();
    assert!(!steady.is_empty(), "no {}-class blocks at f{}", class, frame);
    let blocks: Vec<(runtime2::Rt2, Vec<Option<celeste_rust::interpreter::heap::HeapId>>)> =
        steady
            .iter()
            .map(|st| import::import_block_mapped(st, cart.clone(), cache.clone()))
            .collect();
    let (b0, rev0) = &blocks[0];
    // Shape agreement across blocks (same structure => same canonical ids).
    for (b, _) in &blocks {
        assert_eq!(b.shape_hash, b0.shape_hash, "steady blocks disagree on shape");
    }
    let names0 = celeste_rust::interpreter::merge_dump::cell_names(steady[0]);
    let ncells = b0.structure.len();
    let mut vary = vec![false; ncells];
    for (b, _) in &blocks {
        for (c, col) in b.cols.iter().enumerate() {
            if !matches!(col, runtime2::Col::U(_)) {
                vary[c] = true;
            }
        }
    }
    let forced = force_vary_names();
    for c in 0..ncells {
        let name = rev0[c].and_then(|h| names0.get(&h.raw()).cloned()).unwrap_or_default();
        if forced.iter().any(|f| name == *f || name.ends_with(&format!(".{}", f))) && !vary[c] {
            eprintln!("[emit-shape] forcing cell {} ({}) to per-lane", c, name);
            vary[c] = true;
        }
    }
    let av_kind = |a: &runtime2::AV| -> serde_json::Value {
        match a {
            runtime2::AV::Num(_) => json!("num"),
            runtime2::AV::Ival(_, _) => json!("ival"),
            runtime2::AV::Bool(_) => json!("bool"),
            runtime2::AV::UBool => json!("ubool"),
            runtime2::AV::Str(_) => json!("str"),
            runtime2::AV::Nil => json!("nil"),
            runtime2::AV::Ptr(t) => json!({ "ptr": t }),
            runtime2::AV::NilPtr => json!("nilptr"),
        }
    };
    let cells: Vec<serde_json::Value> = (0..ncells)
        .map(|c| {
            let name = rev0[c]
                .and_then(|h| names0.get(&h.raw()).cloned())
                .unwrap_or_default();
            // Uniform num value, when every steady block agrees: the
            // kernel emitter may FOLD on it, guarded by a bind-time pin.
            let stable_val: Option<i32> = match &b0.cols[c] {
                runtime2::Col::U(runtime2::AV::Num(n)) => {
                    let raw = n.as_raw_u32() as i32;
                    if blocks.iter().all(|(b, _)| {
                        matches!(&b.cols[c], runtime2::Col::U(runtime2::AV::Num(m))
                            if m.as_raw_u32() as i32 == raw)
                    }) {
                        Some(raw)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            let mut o = match &b0.structure[c] {
                runtime2::Cell2::Val => {
                    let content = match &b0.cols[c] {
                        runtime2::Col::U(a) => av_kind(a),
                        runtime2::Col::N(_) => json!("num"),
                        runtime2::Col::I(_) => json!("ival"),
                        runtime2::Col::V(vals) => {
                            // per-lane mixed: report the set
                            let mut kinds: Vec<serde_json::Value> =
                                vals.iter().map(av_kind).collect();
                            kinds.dedup();
                            if kinds.len() == 1 { kinds.remove(0) } else { json!(kinds) }
                        }
                    };
                    json!({ "k": "val", "content": content })
                }
                runtime2::Cell2::Obj(fields) => {
                    let m: serde_json::Map<String, serde_json::Value> = fields
                        .iter()
                        .map(|(f, c)| (gen::FIELD_NAMES[*f as usize].to_string(), json!(c)))
                        .collect();
                    json!({ "k": "obj", "fields": m })
                }
                runtime2::Cell2::Arr(items) => json!({ "k": "arr", "items": items }),
                runtime2::Cell2::Unk => json!({ "k": "unk" }),
                runtime2::Cell2::Clo(f, caps) => json!({
                    "k": "clo",
                    "fn": gen::FN_NAMES[*f as usize],
                    "caps": caps.len(),
                }),
                runtime2::Cell2::Bi(b) => {
                    json!({ "k": "bi", "name": crate::builtins::BUILTIN_NAMES[*b as usize] })
                }
            };
            let obj = o.as_object_mut().unwrap();
            obj.insert("name".into(), json!(name));
            if vary[c] {
                obj.insert("vary".into(), json!(true));
            }
            if let (Some(v), false) = (stable_val, vary[c]) {
                obj.insert("val".into(), json!(v));
            }
            o
        })
        .collect();
    let globals: serde_json::Map<String, serde_json::Value> = gen::GLOBAL_NAMES
        .iter()
        .enumerate()
        .filter(|(gi, _)| b0.globals[*gi] != runtime2::NONE)
        .map(|(gi, name)| (name.to_string(), json!(b0.globals[gi])))
        .collect();
    let witness = json!({
        "frame": frame,
        "shape_hash": format!("{:x}", b0.shape_hash),
        "steady_blocks": blocks.len(),
        "globals": globals,
        "cells": cells,
    });
    std::fs::write(out_path, serde_json::to_string_pretty(&witness).unwrap()).unwrap();
    println!(
        "shape witness: {} cells ({} varying) from {} steady blocks -> {}",
        ncells,
        vary.iter().filter(|v| **v).count(),
        blocks.len(),
        out_path
    );
}

/// The compiled engine (plans/kernel-plan.md K3): run one chunk through the
/// steady-class lane kernel. Returns true if the chunk was handled -
/// output blocks (boundary applied) pushed to `done`, any deopted input
/// rows re-queued on `local` for the reference paths. Returns false
/// (nothing committed) when the chunk cannot bind or a uniform premise
/// fails (bd): the whole chunk then takes the reference path.
fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
    local: &mut Vec<(runtime2::Rt2, bool)>,
) -> bool {
    // The class registry: each kernel's own guards reject wrong-class
    // chunks (bd on the first slice - cheap), so trying in coverage
    // order is both sound and fast.
    // CELESTE_KERNEL_CLASSES=steady,dash (default: all) - diagnostic
    // knob for bisecting a class kernel against the reference path.
    let mask = kernel_class_mask();
    if mask & 1 != 0 && run_class_kernel_steady(chunk, ids, done, local) {
        KERNEL_HITS[0].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    if mask & 2 != 0 && run_class_kernel_dash(chunk, ids, done, local) {
        KERNEL_HITS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    if mask & 4 != 0 && run_class_kernel_frozen(chunk, ids, done, local) {
        KERNEL_HITS[2].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[3].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
    // Why did every kernel refuse? A shape hash that matches some kernel
    // means the CLASS guard rejected it (a pm1 the overlays do not
    // cover); no match at all means the heap shape itself is new. The
    // two want completely different work, so record which.
    if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
        let known = [
            ("steady", kernel_gen_steady::SHAPE_HASH),
            ("dash", kernel_gen_dash::SHAPE_HASH),
            ("frozen", kernel_gen_frozen::SHAPE_HASH),
        ];
        let same_shape: Vec<&str> = known
            .iter()
            .filter(|(_, h)| *h == chunk.shape_hash)
            .map(|(n, _)| *n)
            .collect();
        let mut miss = KERNEL_MISS.lock().unwrap();
        *miss.entry((chunk.shape_hash, same_shape.join("/"))).or_insert(0u64) +=
            chunk.width as u64;
    }
    false
}

/// Missed chunks by (shape hash, which kernels share that shape).
static KERNEL_MISS: std::sync::Mutex<std::collections::BTreeMap<(u64, String), u64>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

/// Where a class kernel refused a chunk, by (class, step). Diagnostic
/// only (CELESTE_KERNEL_MISS=1): "shape" is a different heap, "bind" a
/// uniform/kind mismatch, "rows" a per-lane kind mismatch, "guard" the
/// kernel's own class or premise check.
static KERNEL_MISS_WHY: std::sync::Mutex<
    std::collections::BTreeMap<(&'static str, &'static str), u64>,
> = std::sync::Mutex::new(std::collections::BTreeMap::new());

/// Which block-uniform input a kernel could not bind, by cell and by the
/// column kind that was there instead. Diagnostic only.
static BIND_FAIL: std::sync::Mutex<std::collections::BTreeMap<(u32, &'static str), u64>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

fn note_bind_failure(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    uni_cells: &[(u32, &str)],
) {
    if std::env::var("CELESTE_KERNEL_MISS").is_err() {
        return;
    }
    // One-shot detail on the first refusal: which cells vary, with the
    // distinct values, so a "this cell varies" line can be traced back
    // to a partition that should have split it.
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        eprintln!("kernel miss detail: chunk width {}", chunk.width);
        for (cell, want) in uni_cells {
            let vals: Vec<String> = match &chunk.cols[*cell as usize] {
                runtime2::Col::N(vs) => {
                    let mut d: Vec<String> =
                        vs.iter().map(|n| format!("{:?}", n)).collect();
                    d.sort();
                    d.dedup();
                    d
                }
                runtime2::Col::V(vs) => {
                    let mut d: Vec<String> = vs.iter().map(|v| format!("{:?}", v)).collect();
                    d.sort();
                    d.dedup();
                    d
                }
                _ => continue,
            };
            eprintln!(
                "  cell {} (want {}) has {} distinct values: {}",
                cell,
                want,
                vals.len(),
                vals.iter().take(4).cloned().collect::<Vec<_>>().join(", ")
            );
        }
        eprintln!("  pm1 cells: {:?}", chunk.pm1_cells(ids));
    });
    let mut fail = BIND_FAIL.lock().unwrap();
    *fail
        .entry((
            chunk.player_objects(ids).len() as u32,
            "players; pm1 cells below",
        ))
        .or_insert(0) += chunk.width as u64;
    *fail
        .entry((chunk.pm1_cells(ids).len() as u32, "pm1 cells"))
        .or_insert(0) += chunk.width as u64;
    for (cell, want) in uni_cells {
        let got: &'static str = match &chunk.cols[*cell as usize] {
            runtime2::Col::U(runtime2::AV::Num(_)) => "U(num)",
            runtime2::Col::U(runtime2::AV::Bool(_)) => "U(bool)",
            runtime2::Col::U(runtime2::AV::Ival(..)) => "U(ival)",
            runtime2::Col::U(runtime2::AV::UBool) => "U(ubool)",
            runtime2::Col::U(_) => "U(other)",
            runtime2::Col::N(_) => "N(varying)",
            runtime2::Col::I(_) => "I(varying)",
            runtime2::Col::V(_) => "V(varying)",
        };
        let ok = matches!((*want, got), ("num", "U(num)") | ("bool", "U(bool)"));
        if !ok {
            *fail.entry((*cell, got)).or_insert(0) += chunk.width as u64;
        }
    }
}

fn note_miss(class: &'static str, step: &'static str, lanes: usize) {
    if std::env::var("CELESTE_KERNEL_MISS").is_err() {
        return;
    }
    *KERNEL_MISS_WHY.lock().unwrap().entry((class, step)).or_insert(0) += lanes as u64;
}

/// Pre-dedup rows out of the kernel's registers before materializing
/// them (plans/dedup-on-the-fly-plan.md). `CELESTE_PREDEDUP=0` restores
/// the materialize-everything path for A/B measurement.
fn prededup_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_PREDEDUP").map(|v| v != "0").unwrap_or(true))
}

/// Which boundary canonicalizations apply to this chunk's key cells.
/// `mark_walk` answers it from the heap walk; the kernel only knows cell
/// ids. Computed on the INPUT block and re-checked against the OUTPUT
/// block before the boundary, because skipping a cell that is NOT a rem
/// cell downstream would merge rows that differ.
fn key_plan(
    b: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    key_cells: &[u32],
) -> kernel::KeyPlan {
    let (rem, det) = b.mark_walk(ids);
    let mut plan = kernel::KeyPlan::default();
    for (j, c) in key_cells.iter().enumerate() {
        if rem.contains(c) {
            plan.rem |= 1 << j;
        }
        if det.contains(c) {
            plan.det |= 1 << j;
        }
    }
    plan
}

/// Which class kernels are enabled (bit 0 steady, 1 dash, 2 frozen).
/// Default all; `CELESTE_KERNEL_CLASSES=steady,frozen` restricts, and
/// the disabled classes fall through to the reference path.
fn kernel_class_mask() -> u8 {
    use std::sync::atomic::{AtomicU8, Ordering};
    static MASK: AtomicU8 = AtomicU8::new(0xff);
    let m = MASK.load(Ordering::Relaxed);
    if m != 0xff {
        return m;
    }
    let m = match std::env::var("CELESTE_KERNEL_CLASSES") {
        Ok(v) => v.split(',').fold(0u8, |acc, s| {
            acc | match s.trim() {
                "steady" => 1,
                "dash" => 2,
                "frozen" => 4,
                "" => 0,
                other => panic!("unknown kernel class {:?}", other),
            }
        }),
        Err(_) => 7,
    };
    MASK.store(m, Ordering::Relaxed);
    m
}

/// Lanes handled per class kernel [steady, dash, frozen, missed].
static KERNEL_HITS: [std::sync::atomic::AtomicU64; 4] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Kernel rows [materialized by append_out, surviving within-chunk dedup].
static KROWS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

pub fn print_kernel_hits() {
    let v: Vec<u64> = KERNEL_HITS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    if v.iter().any(|x| *x > 0) {
        eprintln!(
            "kernel lanes: steady {} dash {} frozen {} missed {}",
            v[0], v[1], v[2], v[3]
        );
    }
    {
        let mut miss = KERNEL_MISS.lock().unwrap();
        for ((hash, same), lanes) in miss.iter() {
            eprintln!(
                "kernel miss: shape {:#x} lanes {} ({})",
                hash,
                lanes,
                if same.is_empty() { "new shape - no kernel has it" } else { same }
            );
        }
        miss.clear();
        let mut why = KERNEL_MISS_WHY.lock().unwrap();
        for ((class, step), lanes) in why.iter() {
            eprintln!("kernel miss: {} refused at {} ({} lanes)", class, step, lanes);
        }
        why.clear();
        let mut fail = BIND_FAIL.lock().unwrap();
        for ((cell, got), lanes) in fail.iter() {
            eprintln!("kernel miss: cell {} is {} ({} lanes)", cell, got, lanes);
        }
        fail.clear();
    }
    let rows: Vec<u64> = KROWS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    if rows[0] > 0 {
        eprintln!(
            "kernel rows: materialized {} -> {} after within-chunk dedup ({:.1}:1)",
            rows[0],
            rows[1],
            rows[0] as f64 / rows[1].max(1) as f64
        );
    }
}

macro_rules! class_kernel_runner {
    ($fname:ident, $m:ident) => {
fn $fname(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
    local: &mut Vec<(runtime2::Rt2, bool)>,
) -> bool {
    if chunk.shape_hash != $m::SHAPE_HASH {
        note_miss(stringify!($m), "shape", chunk.width);
        return false;
    }
    let Some(uni) = $m::bind(chunk) else {
        note_miss(stringify!($m), "bind", chunk.width);
        note_bind_failure(chunk, ids, $m::UNI_CELLS);
        return false;
    };
    let g = $m::G { cart: &chunk.cart, cache: &chunk.cache };
    let mut acc = $m::acc_init(chunk);
    let plan = key_plan(chunk, ids, $m::KEY_CELLS);
    // Pre-dedup: a chunk emits one row per (lane, fork config, button
    // variant), and at f35 8.3 of every 9 of those are duplicates that
    // boundary would throw away AFTER they were materialized and hashed.
    // Key them straight out of the kernel's output registers instead and
    // materialize only the first occurrence.
    let mut seen: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
    let mut keys = [(0u64, 0u64); kernel::W];
    let mut deopt_rows: std::collections::BTreeSet<u32> = Default::default();
    let mut bd_hit = false;
    let mut lo = 0usize;
    while lo < chunk.width && !bd_hit {
        let n = (chunk.width - lo).min(kernel::W);
        let width_mask: u16 = if n == kernel::W { 0xffff } else { (1u16 << n) - 1 };
        let Some(rin) = $m::rows(chunk, lo) else {
            note_miss(stringify!($m), "rows", chunk.width);
            return false;
        };
        $m::frame(&uni, &rin, &g, &mut |_b, osh, kout| {
            if kout.bd {
                bd_hit = true;
                return;
            }
            let dead = kout.deopt & kout.valid & width_mask;
            for i in 0..n {
                if dead & (1 << i) != 0 {
                    deopt_rows.insert((lo + i) as u32);
                }
            }
            let mut live = kout.valid & !kout.deopt & width_mask;
            if live == 0 {
                return;
            }
            if prededup_on() {
                $m::row_keys(chunk, lo, n, osh, kout, &plan, &mut keys);
                for i in 0..n {
                    if live & (1 << i) != 0 && !seen.insert(keys[i]) {
                        live &= !(1 << i);
                    }
                }
                if live == 0 {
                    return;
                }
            }
            let mut ug = false; // uniform-output cross-config guard
            $m::append_out(&mut acc, chunk, lo, n, live, osh, kout, &mut ug);
            if ug {
                bd_hit = true;
            }
        });
        lo += kernel::W;
    }
    if bd_hit {
        // A guard inside the kernel refused: wrong class, or a uniform
        // premise that did not hold.
        note_miss(stringify!($m), "guard", chunk.width);
        return false;
    }
    if acc.width > 0 {
        // Dedup-census (plans/dedup-on-the-fly-plan.md): how much of the
        // 45:1 duplication is reachable WITHIN a chunk? That is the
        // ceiling for deduping before materializing.
        let before = acc.width as u64;
        // The key plan was read off the INPUT block; hold that it still
        // describes the OUTPUT block, since a stale rem bit would merge
        // rows that boundary keeps apart.
        let out_plan = key_plan(&acc, ids, $m::KEY_CELLS);
        assert!(
            out_plan.rem == plan.rem && out_plan.det == plan.det,
            "key plan changed across the frame: in {:?} out {:?}",
            plan,
            out_plan
        );
        acc.boundary(ids);
        KROWS[0].fetch_add(before, std::sync::atomic::Ordering::Relaxed);
        KROWS[1].fetch_add(acc.width as u64, std::sync::atomic::Ordering::Relaxed);
        done.push(acc);
    }
    if !deopt_rows.is_empty() {
        let keep: Vec<u32> = deopt_rows.iter().copied().collect();
        let mut sub = chunk.clone_block();
        sub.retain_lanes(&keep);
        local.push((sub, false));
    }
    true
}
    };
}
class_kernel_runner!(run_class_kernel_steady, kernel_gen_steady);
class_kernel_runner!(run_class_kernel_dash, kernel_gen_dash);
class_kernel_runner!(run_class_kernel_frozen, kernel_gen_frozen);

/// A width-`n` copy of lanes [lo, lo+n) of a block (structure shared by
/// clone, varying columns sliced) - the canvas kernel outputs land on.
fn slice_block(b: &runtime2::Rt2, lo: usize, n: usize) -> runtime2::Rt2 {
    let mut out = runtime2::Rt2::empty(n, b.globals.len(), &[], b.cart.clone(), b.cache.clone());
    out.strings = b.strings.clone();
    out.globals = b.globals.clone();
    out.structure = b.structure.clone();
    out.cols = b
        .cols
        .iter()
        .map(|c| match c {
            runtime2::Col::U(_) => c.clone(),
            runtime2::Col::N(v) => runtime2::Col::N(v[lo..lo + n].to_vec()),
            runtime2::Col::I(v) => runtime2::Col::I(v[lo..lo + n].to_vec()),
            runtime2::Col::V(v) => runtime2::Col::V(v[lo..lo + n].to_vec()),
        })
        .collect();
    out.shape_hash = b.shape_hash;
    out
}

/// K2 (plans/kernel-plan.md): the standalone kernel microbench.
/// Gate first - the kernel's boundary row-key SET must equal the
/// reference engine's on every steady block - then the timing.
fn run_kernel_bench(dir: &str, frame: u32, reps: u32) {
    use std::time::Instant;
    let (cart, cache) = world();
    let ids = boundary_ids();
    let states = load_states_any(dir, frame);
    // The kernel's class is (shape, pm1): shape_hash is structural only,
    // so the pm1 cells are checked on the interpreter side.
    let is_steady = |st: &celeste_rust::interpreter::state::State| -> bool {
        use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
        let names = celeste_rust::interpreter::merge_dump::cell_names(st);
        names.iter().all(|(cell, name)| {
            if name != "freeze" && !name.ends_with(".dash_time") {
                return true;
            }
            matches!(
                st.heap
                    .get_opt(celeste_rust::interpreter::heap::HeapId::from_raw(*cell)),
                Some(HeapValue::Value(Value::Number(MaybeVector::Scalar(n))))
                    if n.whole_part_as_i16() == 0 && n.fraction_part_as_u16() == 0
            )
        })
    };
    let steady: Vec<runtime2::Rt2> = states
        .iter()
        .filter(|st| is_steady(st))
        .map(|st| import::import_block(st, cart.clone(), cache.clone()))
        .filter(|b| b.shape_hash == kernel_gen::SHAPE_HASH)
        .collect();
    let lanes_in: usize = steady.iter().map(|b| b.width).sum();
    println!(
        "[kernel-bench] f{:03}: {} steady blocks, {} lanes (of {} total)",
        frame,
        steady.len(),
        lanes_in,
        states.iter().map(|s| s.vector_size).sum::<usize>()
    );
    let g = kernel_gen::G { cart, cache };

    // ---- gate: row-key set equality vs the certified frame pipeline ----
    // CELESTE_KERNEL_GATE=0 skips it (profiling runs: the perf data
    // then covers only the timed kernel loop).
    let run_gate = std::env::var("CELESTE_KERNEL_GATE").map(|v| v != "0").unwrap_or(true);
    let g_freeze = gen::global_id("freeze").expect("no freeze global");
    let mut census: rustc_hash::FxHashMap<&'static str, (u64, u64, u64)> = Default::default();
    if run_gate {
    eprintln!("[gate] running the reference pipeline...");
    let ref_blocks = frame_step(
        steady.iter().map(|b| b.clone_block()).collect(),
        &ids,
        g_freeze,
        &mut census,
    );
    eprintln!("[gate] reference done; running the kernel...");
    let mut ref_keys: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
    for b in &ref_blocks {
        ref_keys.extend(b.row_keys.iter().copied());
    }
    let mut kern_keys: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
    let (mut deopt_lanes, mut bd_slices) = (0u64, 0u64);
    for chunk in &steady {
        let uni = kernel_gen::bind(chunk).expect("bind failed on a steady block");
        // Input rows any variant deopted on: they take the reference
        // path afterwards (exactly the integration architecture).
        let mut deopt_rows: std::collections::BTreeSet<u32> = Default::default();
        let mut lo = 0usize;
        while lo < chunk.width {
            let n = (chunk.width - lo).min(kernel::W);
            let width_mask: u16 =
                if n == kernel::W { 0xffff } else { (1u16 << n) - 1 };
            let rin = kernel_gen::rows(chunk, lo).expect("rows");
            kernel_gen::frame(&uni, &rin, &g, &mut |_b, osh, kout| {
                if kout.bd {
                    bd_slices += 1;
                    for i in 0..n {
                        deopt_rows.insert((lo + i) as u32);
                    }
                    return;
                }
                let dead = kout.deopt & kout.valid & width_mask;
                deopt_lanes += dead.count_ones() as u64;
                for i in 0..n {
                    if dead & (1 << i) != 0 {
                        deopt_rows.insert((lo + i) as u32);
                    }
                }
                let live = kout.valid & !kout.deopt & width_mask;
                if live == 0 {
                    return;
                }
                let mut ob = slice_block(chunk, lo, n);
                kernel_gen::apply(osh, kout, &mut ob, n);
                if dead != 0 {
                    let keep: Vec<u32> =
                        (0..n as u32).filter(|i| live & (1 << i) != 0).collect();
                    ob.retain_lanes(&keep);
                }
                ob.boundary(&ids);
                kern_keys.extend(ob.row_keys.iter().copied());
            });
            lo += n;
        }
        if !deopt_rows.is_empty() {
            // Reference re-run of the deopted input rows (all 64 button
            // variants come from the UBool fan-out; overlap with lanes
            // the kernel did handle is harmless under set semantics).
            let keep: Vec<u32> = deopt_rows.iter().copied().collect();
            let mut sub = chunk.clone_block();
            sub.retain_lanes(&keep);
            for b in frame_step(vec![sub], &ids, g_freeze, &mut census) {
                kern_keys.extend(b.row_keys.iter().copied());
            }
        }
    }
    let missing: Vec<_> = ref_keys.difference(&kern_keys).collect();
    let extra: Vec<_> = kern_keys.difference(&ref_keys).collect();
    println!(
        "gate: ref {} keys, kernel {} keys, {} missing, {} extra, {} deopt lane-variants, {} bd slices",
        ref_keys.len(),
        kern_keys.len(),
        missing.len(),
        extra.len(),
        deopt_lanes,
        bd_slices
    );
    if !missing.is_empty() || !extra.is_empty() {
        println!("gate: FAILED");
        std::process::exit(1);
    }
    println!("gate: row-key SET EQUAL - kernel + deopt-to-reference EXACT on the steady class");
    } // run_gate

    // ---- timing: kernel-only (bind + gather + frame), no materialize ----
    let mut best = f64::INFINITY;
    for _ in 0..reps {
        let t0 = Instant::now();
        let mut sink = 0u64;
        for chunk in &steady {
            let uni = kernel_gen::bind(chunk).expect("bind");
            let mut lo = 0usize;
            while lo < chunk.width {
                let rin = kernel_gen::rows(chunk, lo).expect("rows");
                kernel_gen::frame(&uni, &rin, &g, &mut |b, osh, kout| {
                    sink = sink.wrapping_add(kout.deopt as u64).wrapping_add(b as u64);
                    std::hint::black_box(osh);
                    std::hint::black_box(kout);
                });
                lo += kernel::W;
            }
        }
        std::hint::black_box(sink);
        let dt = t0.elapsed().as_secs_f64();
        best = best.min(dt);
    }
    let row_btns = lanes_in as f64 * 64.0;
    println!(
        "kernel: {} lanes x 64 inputs, best of {}: {:.2} ms  ({:.1} ns per row-input-frame, {:.0} ns/input-lane)",
        lanes_in,
        reps,
        best * 1e3,
        best * 1e9 / row_btns,
        best * 1e9 / lanes_in as f64
    );
}

/// The dev-loop benchmark (plans/columnar-engine.md): ONE abstract frame
/// forward from REAL boundary states of an existing checkpoint dir, with
/// the interpreter's own next-frame lane count as a built-in oracle.
///
///   native-probe --abstract-bench ~/celeste-checkpoints/room10-newlua-bench 35 --reps 10
fn run_abstract_bench(dir: &str, frame: u32, reps: u32) {
    let (cart, cache) = world();
    let ids = boundary_ids();
    let g_freeze = gen::global_id("freeze").expect("no freeze global");

    let t_load = std::time::Instant::now();
    let states = load_states_any(dir, frame);
    let blocks: Vec<runtime2::Rt2> = states
        .iter()
        .map(|st| import::import_block(st, cart.clone(), cache.clone()))
        .collect();
    // Slot binding relies on import ids == canonical ids (option b of
    // the binding design); hold it loudly.
    for (i, b) in blocks.iter().enumerate() {
        assert!(
            b.is_canonical_order(),
            "imported block {} is not in canonical order",
            i
        );
    }
    let lanes_in: usize = blocks.iter().map(|b| b.width).sum();
    // The interpreter's own answer at the NEXT EXISTING checkpoint (bench
    // dirs save every few frames): run that many frames once for the
    // oracle, then time single frames.
    let next_ckpt = (1..=5u32).find(|k| {
        let p = std::path::Path::new(dir);
        p.join("frames").join(format!("f{:03}.bin", frame + k)).exists()
            || p.join(format!("f{:03}", frame + k)).join("meta.json").exists()
    });
    let ref_out: Option<(u32, usize)> = next_ckpt.map(|k| {
        (
            k,
            load_states_any(dir, frame + k).iter().map(|s| s.vector_size).sum(),
        )
    });
    // Row-storage shape (the 300m projection input): varying columns
    // and their typed bytes per lane, averaged over blocks.
    let (mut n_vary, mut bytes_row) = (0usize, 0usize);
    for b in &blocks {
        for c in &b.cols {
            match c {
                runtime2::Col::U(_) => {}
                runtime2::Col::N(_) => {
                    n_vary += 1;
                    bytes_row += 4;
                }
                runtime2::Col::I(_) => {
                    n_vary += 1;
                    bytes_row += 8;
                }
                runtime2::Col::V(_) => {
                    n_vary += 1;
                    bytes_row += 16;
                }
            }
        }
    }
    eprintln!(
        "[abstract-bench] f{:03}: {} lanes in {} block(s), loaded+imported in {:.2?} \
         (avg {:.0} varying cols, {:.0} typed B/row)",
        frame,
        lanes_in,
        blocks.len(),
        t_load.elapsed(),
        n_vary as f64 / blocks.len().max(1) as f64,
        bytes_row as f64 / blocks.len().max(1) as f64
    );

    let mut census_total: rustc_hash::FxHashMap<&'static str, (u64, u64, u64)> =
        Default::default();

    // Oracle: run to the next existing checkpoint once and compare -
    // lane COUNT (gate 1) and canonical row-key SET (gate 2). Both
    // sides funnel through the SAME canonicalizer: the interpreter's
    // states are imported and run through `boundary` (its widenings
    // are idempotent on boundary states), so key equality means the
    // engine's surviving row set IS the interpreter's, not just the
    // same size.
    let check = match ref_out {
        Some((k, r)) => {
            let mut chase: Vec<runtime2::Rt2> = blocks.iter().map(|b| b.clone_block()).collect();
            for _ in 0..k {
                chase = frame_step(chase, &ids, g_freeze, &mut census_total);
            }
            let got: usize = chase.iter().map(|b| b.width).sum();
            let mut ref_keys: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
            for st in &load_states_any(dir, frame + k) {
                let mut b = import::import_block(st, cart.clone(), cache.clone());
                b.boundary(&ids);
                ref_keys.extend(b.row_keys.iter().copied());
            }
            let eng_keys: rustc_hash::FxHashSet<(u64, u64)> = chase
                .iter()
                .flat_map(|b| b.row_keys.iter().copied())
                .collect();
            let missing = ref_keys.difference(&eng_keys).count();
            let extra = eng_keys.difference(&ref_keys).count();
            if got == r && missing == 0 && extra == 0 {
                format!(
                    "f{:03} lanes {} == interpreter, row-key SET EQUAL (gate 2) OK",
                    frame + k,
                    got
                )
            } else {
                // Row keys are seeded with the block shape hash, so a
                // total mismatch with equal counts usually means shape
                // divergence, not row divergence - print both sides.
                let eng_shapes: Vec<String> =
                    chase.iter().map(|b| format!("{:#x} w{}", b.shape_hash, b.width)).collect();
                let ref_shapes: Vec<String> = load_states_any(dir, frame + k)
                    .iter()
                    .map(|st| {
                        let mut b =
                            import::import_block(st, cart.clone(), cache.clone());
                        b.boundary(&ids);
                        format!("{:#x} w{}", b.shape_hash, b.width)
                    })
                    .collect();
                eprintln!("  engine shapes: {}", eng_shapes.join(", "));
                eprintln!("  ref shapes:    {}", ref_shapes.join(", "));
                // Structural diff of the first block on each side. Dumping
                // only the Obj cells was not enough: on the interpreter
                // fallback's first run the two sides' Obj cells were
                // IDENTICAL and the shape hashes still differed, which
                // said nothing about where. Print EVERY cell's kind, and
                // print only the cells that actually differ.
                let kinds = |b: &runtime2::Rt2| -> Vec<String> {
                    b.structure
                        .iter()
                        .map(|cell| match cell {
                            runtime2::Cell2::Val => "Val".to_string(),
                            // Field TARGETS, not just names: two blocks
                            // can agree on every field set and still
                            // differ in heap SHARING, which is what the
                            // shape hash covers and what a name-only
                            // dump hides.
                            runtime2::Cell2::Obj(fields) => format!(
                                "Obj[{}]",
                                fields
                                    .iter()
                                    .map(|(f, c)| format!(
                                        "{}->{}",
                                        gen::FIELD_NAMES[*f as usize], c
                                    ))
                                    .collect::<Vec<_>>()
                                    .join(",")
                            ),
                            runtime2::Cell2::Arr(items) => format!("Arr{:?}", items),
                            runtime2::Cell2::Unk => "Unk".to_string(),
                            // Captures can hold POINTERS, so they are part
                            // of the heap topology too - a cap-only
                            // difference shifts every later cell id and
                            // looks like "the objects moved".
                            runtime2::Cell2::Clo(f, caps) => format!(
                                "Clo({}, caps {:?})",
                                gen::FN_NAMES[*f as usize],
                                caps.iter().map(|c| c.at(0)).collect::<Vec<_>>()
                            ),
                            runtime2::Cell2::Bi(b) => format!("Bi({})", b),
                        })
                        .collect()
                };
                let eng = chase.first().map(kinds).unwrap_or_default();
                let refk = load_states_any(dir, frame + k)
                    .first()
                    .map(|st| {
                        let mut b =
                            import::import_block(st, cart.clone(), cache.clone());
                        b.boundary(&ids);
                        kinds(&b)
                    })
                    .unwrap_or_default();
                if eng.len() != refk.len() {
                    eprintln!("  cell COUNT differs: engine {} vs ref {}", eng.len(), refk.len());
                }
                let mut shown = 0;
                for i in 0..eng.len().max(refk.len()) {
                    let (a, b) = (eng.get(i), refk.get(i));
                    if a != b {
                        eprintln!("  cell {}: engine {:?} vs ref {:?}", i, a, b);
                        shown += 1;
                        if shown == 40 {
                            eprintln!("  ... (more differing cells suppressed)");
                            break;
                        }
                    }
                }
                // Who reaches the cells only one side has? A cell count
                // that differs by one says nothing on its own; its PARENT
                // names the mechanism.
                if let Some(b) = chase.first() {
                    for extra in refk.len()..eng.len() {
                        let extra = extra as u32;
                        for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
                            if b.globals[gi] == extra {
                                eprintln!("  cell {} is the global {:?}", extra, name);
                            }
                        }
                        for (ci, cell) in b.structure.iter().enumerate() {
                            match cell {
                                runtime2::Cell2::Obj(fields) => {
                                    for (f, c) in fields {
                                        if *c == extra {
                                            eprintln!(
                                                "  cell {} is {}.{}",
                                                extra, ci, gen::FIELD_NAMES[*f as usize]
                                            );
                                        }
                                    }
                                }
                                runtime2::Cell2::Arr(items) => {
                                    for (i, c) in items.iter().enumerate() {
                                        if *c == extra {
                                            eprintln!("  cell {} is {}[{}]", extra, ci, i);
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                        eprintln!("  cell {} value: {:?}", extra, b.cols[extra as usize]);
                    }
                }
                if shown == 0 && eng.len() == refk.len() {
                    eprintln!(
                        "  structures are IDENTICAL cell for cell - the difference is in \
                         the COLUMNS or the globals table, not the shape"
                    );
                }
                format!(
                    "MISMATCH at f{:03}: engine {} vs interpreter {} (row keys: {} missing, {} extra)",
                    frame + k,
                    got,
                    r,
                    missing,
                    extra
                )
            }
        }
        None => "(no later checkpoint to compare against)".to_string(),
    };

    let mut times: Vec<f64> = Vec::new();
    let mut lanes_out = 0usize;
    let mut splits = 0u64;
    for _ in 0..reps {
        let run: Vec<runtime2::Rt2> = blocks.iter().map(|b| b.clone_block()).collect();
        let t0 = std::time::Instant::now();
        let out = frame_step(run, &ids, g_freeze, &mut census_total);
        times.push(t0.elapsed().as_secs_f64() * 1e3);
        print_kernel_hits();
        lanes_out = out.iter().map(|b| b.width).sum();
        splits = out.iter().map(|b| b.stat_splits).max().unwrap_or(0);
    }
    times.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let min = times.first().copied().unwrap_or(0.0);
    let mean = times.iter().sum::<f64>() / times.len().max(1) as f64;
    println!(
        "abstract-bench f{:03}: {} -> {} lanes  [{}]\n  {} reps: min {:.2} ms, mean {:.2} ms  ({:.0} ns/input-lane min, {} splits)",
        frame,
        lanes_in,
        lanes_out,
        check,
        reps,
        min,
        mean,
        min * 1e6 / lanes_in as f64,
        splits
    );
    if !census_total.is_empty() {
        let mut rows: Vec<_> = census_total.into_iter().collect();
        rows.sort_by_key(|(_, (ns, _, _))| std::cmp::Reverse(*ns));
        println!("op census (name, total ms over all reps, calls):");
        for (name, (ns, calls, _)) in rows {
            println!("  {:14} {:9.1} ms  {:>12} calls", name, ns as f64 / 1e6, calls);
        }
    }
}

