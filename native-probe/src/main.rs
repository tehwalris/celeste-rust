//! Bench and gate harness for the compiled abstract engine.
//!
//! The ENGINE is not here any more. `celeste_rust::compiled::FrameEngine`
//! is one frame - kernels where they bind, the interpreter where they do
//! not - and it lives in celeste-rust so the campaign can call it too
//! (task #150). What is left in this binary is what only a harness wants:
//! run N frames and print lane counts (`--abstract`), run ONE frame from a
//! real checkpoint and check its row-key set against the interpreter's
//! (`--abstract-bench`, gate 2), and the kernel-authoring tools
//! (`--row-census`, `--emit-shape`).
//!
//! It stays a separate binary on purpose: the gates drive the engine
//! directly, one frame at a time, from checkpoint states, which is not
//! something the campaign binaries do.

use celeste_engine::runtime2;
use celeste_names as gen;

use celeste_rust::compiled::bridge as import;
use celeste_rust::compiled::dispatch;
use celeste_rust::compiled::FrameEngine;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

/// The recipe the name tables and the kernels were generated from. It MUST
/// be this one and not the plain program: `transpile --recipe
/// rewrites-compile.jsonl` is the canonical regen (plans/columnar-engine
/// .md), so the compiled engine executes the REWRITTEN program, and so must
/// anything claiming to be its reference. See `FrameEngine::new`, which
/// carries the story of the afternoon this cost.
const COMPILE_RECIPE: &str = "rewrites-compile.jsonl";

/// Built once per process - loading the recipe and applying it compiles the
/// whole cart, which is seconds, and every mode here shares the result.
fn engine() -> &'static FrameEngine {
    static ENGINE: std::sync::OnceLock<FrameEngine> = std::sync::OnceLock::new();
    ENGINE.get_or_init(|| {
        // The RECIPE is still parsed - `StateMapping::from_recipe` below
        // reads the instruction list as DATA - but it is not replayed:
        // the program comes from the frozen artifact next to it.
        let recipe = celeste_rust::program::recipe::Recipe::load(COMPILE_RECIPE).unwrap_or_else(
            |e| panic!("loading {} (run from the repo root): {}", COMPILE_RECIPE, e),
        );
        let program = celeste_rust::program::frozen::rewritten(COMPILE_RECIPE)
            .unwrap_or_else(|e| panic!("loading the frozen {}: {}", COMPILE_RECIPE, e));
        let (cart, cache) = world();
        let mut engine = FrameEngine::new(&program, cart.clone(), cache.clone());
        // The plain-program path for kernel deopt sub-chunks (dying
        // representatives etc.), which fail the specialized program's
        // premises by construction. Same wiring as the campaign's
        // `compiled_engine`.
        let plain_program = celeste_rust::program::Program::compile_from_disk()
            .expect("compiling the plain program for the engine's deopt path");
        engine.set_plain_path(celeste_rust::compiled::PlainPath {
            plain_cfg: celeste_rust::interpreter::fixed_env::PreparedCfg::new(
                plain_program.frame_cfg().clone(),
            ),
            plain_env: plain_program.fixed_env(),
            mapping: celeste_rust::search::state_mapping::StateMapping::from_recipe(&recipe),
        });
        engine
    })
}

/// Cart data and the collision cache for the campaign's start room.
fn world() -> &'static (std::sync::Arc<CartData>, std::sync::Arc<CollisionCache>) {
    static WORLD: std::sync::OnceLock<(std::sync::Arc<CartData>, std::sync::Arc<CollisionCache>)> =
        std::sync::OnceLock::new();
    WORLD.get_or_init(|| {
        let (room_x, room_y) = celeste_rust::game_runner::start_room();
        let cart = std::sync::Arc::new(CartData::load("cart").expect("failed to load cart data"));
        let cache = std::sync::Arc::new(
            CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
        );
        eprintln!("[native-probe] collision cache for room ({}, {})", room_x, room_y);
        (cart, cache)
    })
}

/// Load boundary states from either checkpoint layout: `frames/fNNN.bin`
/// (the campaign frame batches) or `fNNN/states.bin` (`rewrite bench
/// --checkpoint-dir`). For the bench layout the fingerprint check is
/// self-supplied from meta.json - the census wants states, not resume
/// safety.
fn load_states_any(dir: &str, frame: u32) -> Vec<celeste_rust::interpreter::state::State> {
    use celeste_rust::search::checkpoint;
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
    let mut emit_shape: Option<(String, u32, String, String, String)> = None;
    let mut interp_bench: Option<(String, u32)> = None;
    let mut frame_diff: Option<(String, u32, String)> = None;
    let mut dedup_bench: Option<(String, u32)> = None;
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
                let dir = args.next().expect("--emit-shape needs DIR FRAME OUT [CLASS] [SHAPE]");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                let out = args.next().expect("OUT");
                let class = args.next().unwrap_or_else(|| "steady".to_string());
                // Comma-separated object shape to select, e.g.
                // "fruit,spring,spring,player" for room (2,0). Default is
                // the room (1,0) [player] shape.
                let shape = args.next().unwrap_or_else(|| "player".to_string());
                emit_shape = Some((dir, frame, out, class, shape));
            }
            "--interp-bench" => {
                let dir = args.next().expect("--interp-bench needs DIR FRAME");
                let f: u32 = args.next().expect("FRAME").parse().unwrap();
                interp_bench = Some((dir, f));
            }
            "--dedup-bench" => {
                let dir = args.next().expect("--dedup-bench needs DIR FRAME");
                let f: u32 = args.next().expect("FRAME").parse().unwrap();
                dedup_bench = Some((dir, f));
            }
            "--frame-diff" => {
                let dir = args.next().expect("--frame-diff needs DIR FRAME OUTDIR");
                let f: u32 = args.next().expect("FRAME").parse().unwrap();
                let out = args.next().expect("OUTDIR");
                frame_diff = Some((dir, f, out));
            }
            other => panic!("unknown argument {:?}", other),
        }
    }

    if let Some(n) = abstract_frames {
        run_abstract(n);
    } else if let Some((dir, frame)) = row_census {
        run_row_census(&dir, frame);
    } else if let Some((dir, frame, out, class, shape)) = emit_shape {
        run_emit_shape(&dir, frame, &out, &class, &shape);
    } else if let Some((dir, frame)) = interp_bench {
        run_interp_bench(&dir, frame, reps);
    } else if let Some((dir, frame, out)) = frame_diff {
        run_frame_diff(&dir, frame, &out);
    } else if let Some((dir, frame)) = dedup_bench {
        run_dedup_bench(&dir, frame, reps.min(3));
    } else if let Some((dir, frame)) = abstract_bench {
        run_abstract_bench(&dir, frame, reps);
    } else {
        panic!("nothing to do - pass --abstract N, --abstract-bench DIR FRAME, --row-census, --emit-shape, --interp-bench, --frame-diff, --dedup-bench or --abstract-bench DIR FRAME");
    }
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
    let eng = engine();
    let mut blocks: Vec<runtime2::Rt2> = eng.initial_blocks();
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
        blocks = eng.step(blocks, &mut census_total);
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

fn run_emit_shape(dir: &str, frame: u32, out_path: &str, class: &str, shape: &str) {
    use serde_json::json;
    let (cart, cache) = world();
    let states = load_states_any(dir, frame);
    let steady: Vec<&celeste_rust::interpreter::state::State> = states
        .iter()
        .filter(|st| {
            use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
            let names = celeste_rust::interpreter::merge_dump::cell_names(st);
            let want_shape: Vec<String> = shape.split(',').map(|s| s.trim().to_string()).collect();
            let shape_ok = celeste_rust::interpreter::abstraction::object_shape(st)
                .map(|s| s == want_shape)
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
    let all_blocks: Vec<(runtime2::Rt2, Vec<Option<celeste_rust::interpreter::heap::HeapId>>)> =
        steady
            .iter()
            .map(|st| import::import_block_mapped(st, cart.clone(), cache.clone()))
            .collect();
    // Class blocks may come in several structural shapes (e.g. a spring
    // field nil in some blocks, a number in others). The kernel binds per
    // shape hash - the slot gate rejects other shapes to the interpreter -
    // so the witness takes the DOMINANT hash and logs what it drops.
    // No silent caps: the dropped fraction is deopt population, price it.
    let mut by_hash: std::collections::BTreeMap<u64, (usize, usize)> =
        std::collections::BTreeMap::new(); // hash -> (block count, lane count)
    for (i, (b, _)) in all_blocks.iter().enumerate() {
        let e = by_hash.entry(b.shape_hash).or_insert((0usize, 0usize));
        e.0 += 1;
        e.1 += steady[i].vector_size;
    }
    // CELESTE_EMIT_SHAPE_HASH overrides the max-lane pick: the ENGINE's
    // chunk population can be dominated by a hash the reference frames
    // under-sample (the (2,0) fruit widening makes interval-kind chunks
    // dominant engine-side while the saved frames lean num-kind), and the
    // kernel must match what the engine actually sees.
    let dominant: u64 = match std::env::var("CELESTE_EMIT_SHAPE_HASH") {
        Ok(h) => {
            let h = u64::from_str_radix(h.trim_start_matches("0x"), 16)
                .expect("CELESTE_EMIT_SHAPE_HASH must be a hex shape hash");
            assert!(
                by_hash.contains_key(&h),
                "CELESTE_EMIT_SHAPE_HASH {:x} not among this frame's {}-class hashes: {:?}",
                h,
                class,
                by_hash.keys().map(|k| format!("{:x}", k)).collect::<Vec<_>>()
            );
            h
        }
        Err(_) => *by_hash
            .iter()
            .max_by_key(|(_, (_, lanes))| *lanes)
            .map(|(h, _)| h)
            .unwrap(),
    };
    let (dom_blocks, dom_lanes) = by_hash[&dominant];
    let total_lanes: usize = by_hash.values().map(|(_, l)| *l).sum();
    if by_hash.len() > 1 {
        eprintln!(
            "[emit-shape] {} {}-class shape hashes; dominant {:x} = {}/{} blocks, {}/{} lanes ({:.1}%); the rest deopt",
            by_hash.len(), class, dominant, dom_blocks, all_blocks.len(),
            dom_lanes, total_lanes, 100.0 * dom_lanes as f64 / total_lanes as f64
        );
        for (h, (bc, lc)) in &by_hash {
            eprintln!("[emit-shape]   hash {:x}: {} blocks, {} lanes", h, bc, lc);
        }
    }
    let keep: Vec<bool> = all_blocks.iter().map(|(b, _)| b.shape_hash == dominant).collect();
    let steady: Vec<&celeste_rust::interpreter::state::State> = steady
        .into_iter()
        .zip(keep.iter())
        .filter_map(|(s, &k)| if k { Some(s) } else { None })
        .collect();
    let blocks: Vec<(runtime2::Rt2, Vec<Option<celeste_rust::interpreter::heap::HeapId>>)> =
        all_blocks
            .into_iter()
            .zip(keep.iter())
            .filter_map(|(b, &k)| if k { Some(b) } else { None })
            .collect();
    let (b0, rev0) = &blocks[0];
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
                    json!({ "k": "bi", "name": celeste_rust::builtins::BUILTIN_NAMES[*b as usize] })
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

/// The INTERPRETER's frame body on the same input, for scale.
///
/// `--interp-bench DIR FRAME` was the class-kernel `--kernel-bench`'s
/// counterpart (that mode went with the class kernels): same checkpoint
/// states, "frame body only" boundary (no abstraction, no dedup, no
/// merge), best-of-N, normalization. What it runs is
/// `interpret_prepared_cfg` on the CAMPAIGN's recipe (`rewrites.jsonl`),
/// not the compile overlay - the overlay's `expand_bool` was measured at
/// +19% on the interpreter, so this is the interpreter at its best rather
/// than the interpreter handicapped by the compiled path's program.
///
/// Two knobs: `CELESTE_KERNEL_BENCH_THREADS=T` and
/// `CELESTE_INTERP_BENCH_LANES=N` (the per-chunk lane cap, which is what
/// the interpreter's vectorization amortizes over).
fn run_interp_bench(dir: &str, frame: u32, reps: u32) {
    use celeste_rust::interpreter::state::State;
    use std::time::Instant;

    let program =
        celeste_rust::program::frozen::rewritten("rewrites.jsonl").expect("the frozen program");
    celeste_rust::interpreter::vectorize::set_merge_partition_patterns(
        &program.merge_partition_cells,
    );
    let frame_cfg =
        celeste_rust::interpreter::fixed_env::PreparedCfg::new(program.frame_cfg().clone());
    let fixed_env = program.fixed_env();

    let states = load_states_any(dir, frame);
    let lanes_in: usize = states.iter().map(|s| s.vector_size).sum();
    let cap: usize = std::env::var("CELESTE_INTERP_BENCH_LANES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(8000);
    let chunks: Vec<State> = {
        use celeste_rust::interpreter::value::KeptLanes;
        let mut out = Vec::new();
        for st in &states {
            let n = st.vector_size;
            if n <= cap {
                out.push(st.clone());
                continue;
            }
            for lo in (0..n).step_by(cap) {
                let hi = (lo + cap).min(n);
                out.push(st.filter_by_kept_clone(
                    &KeptLanes::from_range(lo, hi),
                    celeste_rust::interpreter::state::FILTER_CHUNK,
                ));
            }
        }
        out
    };
    let threads: usize = std::env::var("CELESTE_KERNEL_BENCH_THREADS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1);
    eprintln!(
        "[interp-bench] f{:03}: {} lanes in {} state(s) -> {} chunk(s) of <={} lanes, {} thread(s)",
        frame,
        lanes_in,
        states.len(),
        chunks.len(),
        cap,
        threads
    );

    let mut best = f64::INFINITY;
    for _ in 0..reps {
        // Cloned outside the timer: the interpreter consumes its state.
        let work: Vec<State> = chunks.clone();
        let t0 = Instant::now();
        std::thread::scope(|scope| {
            for tid in 0..threads {
                let work = &work;
                let frame_cfg = &frame_cfg;
                let fixed_env = &fixed_env;
                scope.spawn(move || {
                    celeste_rust::interpreter::virtual_merge::set_nested_parallel(threads > 1);
                    let mut sink = 0usize;
                    for (i, st) in work.iter().enumerate() {
                        if i % threads != tid {
                            continue;
                        }
                        let out = celeste_rust::interpreter::glue::interpret_prepared_cfg(
                            frame_cfg,
                            st.clone(),
                            fixed_env,
                        )
                        .expect("frame failed");
                        sink += out.len();
                    }
                    std::hint::black_box(sink);
                });
            }
        });
        best = best.min(t0.elapsed().as_secs_f64());
    }
    let row_btns = lanes_in as f64 * 64.0;
    println!(
        "interp: {} lanes x 64 inputs, {} thread(s), cap {}, best of {}: {:.2} ms  \
         ({:.2} ns per row-input-frame, {:.0} ns/input-lane)",
        lanes_in,
        threads,
        cap,
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
    let eng = engine();
    let ids = eng.ids();
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
                chase = eng.step(chase, &mut census_total);
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
            // Frontier-aware verdict: a frontier-only campaign's saved
            // states are the NEW rows of the frame, while `step` returns
            // the raw successor set - so "extra" keys that are simply
            // rows visited in EARLIER frames are not a divergence at
            // all.
            //
            // The visited set is computed IN THE ENGINE'S KEY SPACE, by
            // importing every saved frame's states and running `boundary`
            // - the same canonicalizer as both sides of the gate. A first
            // version of this check read the `frames/*.rowkeys` sidecars
            // instead, got 0 overlap, and concluded the extras were novel
            // fabricated rows; that was wrong. The sidecars store the
            // INTERPRETER's keys (`vectorize::visited_row_keys`:
            // `row_key_hashes` over State columns), which relate to the
            // engine's Rt2 boundary keys only by the D1 BIJECTION - raw
            // value intersection across the two spaces is empty by
            // construction and refutes nothing.
            let mut accounted = 0usize;
            if extra > 0 {
                let mut visited: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
                let mut frames_seen = 0u32;
                for n in 0..=frame {
                    let p = std::path::Path::new(dir);
                    if !p.join("frames").join(format!("f{:03}.bin", n)).exists()
                        && !p.join(format!("f{:03}", n)).join("meta.json").exists()
                    {
                        continue;
                    }
                    frames_seen += 1;
                    for st in &load_states_any(dir, n) {
                        let mut b = import::import_block(st, cart.clone(), cache.clone());
                        b.boundary(&ids);
                        visited.extend(b.row_keys.iter().copied());
                    }
                }
                if !visited.is_empty() {
                    accounted = eng_keys
                        .difference(&ref_keys)
                        .filter(|k| visited.contains(k))
                        .count();
                    eprintln!(
                        "  frontier check: {} of {} extra keys are rows visited in f000..f{:03} \
                         ({} frames on disk, {} visited rows; {} truly unexplained)",
                        accounted,
                        extra,
                        frame,
                        frames_seen,
                        visited.len(),
                        extra - accounted
                    );
                }
            }
            if got == r && missing == 0 && extra == 0 {
                format!(
                    "f{:03} lanes {} == interpreter, row-key SET EQUAL (gate 2) OK",
                    frame + k,
                    got
                )
            } else if missing == 0 && extra > 0 && accounted == extra {
                // The 2026-08-19 "step-gate mystery", resolved: on a
                // frontier-only campaign dir the saved frame is the NEW
                // rows only, so the raw successor set is the reference
                // PLUS re-visited rows. Row-for-row this held on room
                // (1,0) f065->f066: engine 8,715,348 = 4,591,412 ref +
                // 4,123,936 all found in f000..f065's visited rows.
                format!(
                    "f{:03} engine {} = {} ref + {} revisited, row-key set equal \
                     MODULO VISITED (frontier-aware gate 2) OK",
                    frame + k,
                    got,
                    r,
                    accounted
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
                // TWIN DIAGNOSIS: two ENGINE blocks with the same (shape,
                // width) that did not merge differ in some column - name
                // it. This is the signature of a divergence between two
                // engine paths (kernel vs fallback vs plain) emitting the
                // same logical rows with one cell disagreeing.
                'twin: for i in 0..chase.len() {
                    for j in i + 1..chase.len() {
                        let (a, b) = (&chase[i], &chase[j]);
                        if a.shape_hash != b.shape_hash || a.width != b.width || a.width < 100 {
                            continue;
                        }
                        eprintln!(
                            "  twin blocks {} and {} (shape {:#x}, w{}): differing columns:",
                            i, j, a.shape_hash, a.width
                        );
                        let gname = |cell: usize| -> String {
                            for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
                                if a.globals.get(gi) == Some(&(cell as u32)) {
                                    return format!(" (global {})", name);
                                }
                            }
                            String::new()
                        };
                        let mut shown = 0;
                        for c in 0..a.cols.len().min(b.cols.len()) {
                            let (ca, cb) = (&a.cols[c], &b.cols[c]);
                            let same = match (ca, cb) {
                                (runtime2::Col::U(x), runtime2::Col::U(y)) => x == y,
                                (runtime2::Col::N(x), runtime2::Col::N(y)) => {
                                    x.first() == y.first() && x.last() == y.last()
                                }
                                _ => format!("{:?}", ca).len() == format!("{:?}", cb).len(),
                            };
                            if !same {
                                let show = |col: &runtime2::Col| -> String {
                                    let s = format!("{:?}", col);
                                    s.chars().take(60).collect()
                                };
                                eprintln!(
                                    "    cell {}{}: {} vs {}",
                                    c, gname(c), show(ca), show(cb)
                                );
                                shown += 1;
                                if shown == 12 {
                                    eprintln!("    ... (suppressed)");
                                    break;
                                }
                            }
                        }
                        break 'twin;
                    }
                }
                // EXTRA-LANE AUTOPSY: decode a few of the engine's extra
                // rows against their NEAREST reference lane (fewest
                // differing cells). The twin diagnosis names the cell two
                // engine blocks disagree on; this names the cell(s) an
                // extra row disagrees with the INTERPRETER on - the
                // fabricated field should pop out, concrete-vs-unknown
                // included, because `Col::at` compares content.
                if missing == 0 && extra > 0 {
                    let ref_blocks: Vec<runtime2::Rt2> = load_states_any(dir, frame + k)
                        .iter()
                        .map(|st| {
                            let mut b = import::import_block(st, cart.clone(), cache.clone());
                            b.boundary(&ids);
                            b
                        })
                        .collect();
                    let mut extras: Vec<(u64, u64)> =
                        eng_keys.difference(&ref_keys).copied().collect();
                    extras.sort_unstable();
                    let mut eng_index: rustc_hash::FxHashMap<(u64, u64), (usize, usize)> =
                        Default::default();
                    for (bi, b) in chase.iter().enumerate() {
                        for (i, k) in b.row_keys.iter().enumerate() {
                            eng_index.entry(*k).or_insert((bi, i));
                        }
                    }
                    // pm1 cells resolve in recipe order: present globals
                    // first, then the player-object fields.
                    const PM1_NAMES: [&str; 6] =
                        ["has_dashed", "freeze", "dash_time", "djump", "p_dash", "p_jump"];
                    for key in extras.iter().take(3) {
                        let Some(&(bi, lane)) = eng_index.get(key) else { continue };
                        let eb = &chase[bi];
                        let pm1 = eb.pm1_cells(&ids);
                        let label = |cell: usize| -> String {
                            for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
                                if eb.globals.get(gi) == Some(&(cell as u32)) {
                                    return format!(" (global {})", name);
                                }
                            }
                            if let Some(j) = pm1.iter().position(|&c| c as usize == cell) {
                                if let Some(n) = PM1_NAMES.get(j) {
                                    return format!(" (pm1 {})", n);
                                }
                            }
                            String::new()
                        };
                        // Nearest ref lane, same-shape blocks preferred.
                        let mut best: Option<(usize, usize, usize)> = None;
                        for pass in 0..2 {
                            for (rbi, rb) in ref_blocks.iter().enumerate() {
                                let same_shape = rb.shape_hash == eb.shape_hash;
                                if (pass == 0) != same_shape || rb.cols.len() != eb.cols.len() {
                                    continue;
                                }
                                let mut mism = vec![0u32; rb.width];
                                for c in 0..eb.cols.len() {
                                    let ev = eb.cols[c].at(lane);
                                    match &rb.cols[c] {
                                        runtime2::Col::U(v) => {
                                            if *v != ev {
                                                for m in mism.iter_mut() {
                                                    *m += 1;
                                                }
                                            }
                                        }
                                        col => {
                                            for (l, m) in mism.iter_mut().enumerate() {
                                                if col.at(l) != ev {
                                                    *m += 1;
                                                }
                                            }
                                        }
                                    }
                                }
                                if let Some((l, m)) =
                                    mism.iter().enumerate().min_by_key(|(_, m)| **m)
                                {
                                    if best.is_none() || (*m as usize) < best.unwrap().0 {
                                        best = Some((*m as usize, rbi, l));
                                    }
                                }
                            }
                            if best.is_some() {
                                break;
                            }
                        }
                        match best {
                            Some((m, rbi, rlane)) => {
                                let rb = &ref_blocks[rbi];
                                eprintln!(
                                    "  AUTOPSY extra {:#018x}:{:#018x} = engine block {} lane {} \
                                     (shape {:#x}); nearest ref block {} lane {}{}: {} cell(s) differ:",
                                    key.0,
                                    key.1,
                                    bi,
                                    lane,
                                    eb.shape_hash,
                                    rbi,
                                    rlane,
                                    if rb.shape_hash == eb.shape_hash {
                                        ""
                                    } else {
                                        " [DIFFERENT SHAPE]"
                                    },
                                    m
                                );
                                let mut shown = 0;
                                for c in 0..eb.cols.len() {
                                    let (ev, rv) = (eb.cols[c].at(lane), rb.cols[c].at(rlane));
                                    if ev != rv {
                                        eprintln!(
                                            "    cell {}{}: engine {:?} vs ref {:?}",
                                            c,
                                            label(c),
                                            ev,
                                            rv
                                        );
                                        shown += 1;
                                        if shown == 20 {
                                            eprintln!("    ... (suppressed)");
                                            break;
                                        }
                                    }
                                }
                            }
                            None => eprintln!(
                                "  AUTOPSY extra {:#018x}:{:#018x}: no comparable ref block",
                                key.0, key.1
                            ),
                        }
                    }
                }
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
        let out = eng.step(run, &mut census_total);
        times.push(t0.elapsed().as_secs_f64() * 1e3);
        dispatch::print_kernel_hits();
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

/// D0's microscope (plans/dedup-roofline-plan.md): run ONE frame from a
/// checkpoint under BOTH engines and dump every output lane as a readable
/// canonical-boundary row, so a key divergence can be read as a VALUE
/// divergence instead of a hash. The rowkeys sidecar diff located the
/// 24-row divergence at f25; this names the fields.
///
/// Both outputs go through the campaign's own canonicalization
/// (straddle split -> abstraction -> gc), then each lane is rendered by a
/// heap walk in id order. Writes `interp.rows` / `compiled.rows` (one line
/// per lane, sorted) and `interp.shapes` / `compiled.shapes` (the full
/// shape debug per distinct shape hash) into OUTDIR; diff them with
/// standard tools.
fn run_frame_diff(dir: &str, frame: u32, outdir: &str) {
    use celeste_rust::interpreter::state::State;
    use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
    use std::fmt::Write as _;

    // Interpreter side: the CAMPAIGN program, exactly as run_interp_bench.
    let program =
        celeste_rust::program::frozen::rewritten("rewrites.jsonl").expect("the frozen program");
    celeste_rust::interpreter::vectorize::set_merge_partition_patterns(
        &program.merge_partition_cells,
    );
    let frame_cfg =
        celeste_rust::interpreter::fixed_env::PreparedCfg::new(program.frame_cfg().clone());
    let fixed_env = program.fixed_env();
    let eng = engine();

    let inputs = load_states_any(dir, frame - 1);
    let lanes_in: usize = inputs.iter().map(|s| s.vector_size).sum();
    eprintln!(
        "[frame-diff] f{:03} from f{:03}: {} lanes in {} state(s)",
        frame,
        frame - 1,
        lanes_in,
        inputs.len()
    );

    let canon = |states: Vec<State>| -> Vec<State> {
        let mut out = Vec::new();
        for s in states {
            for s2 in celeste_rust::interpreter::abstraction::split_precision_straddles(s) {
                out.push(celeste_rust::interpreter::abstraction::make_state_abstract(s2));
            }
        }
        celeste_rust::interpreter::vectorize::gc_states(out)
    };

    let mut out_interp = Vec::new();
    let mut out_compiled = Vec::new();
    for st in &inputs {
        out_interp.extend(
            celeste_rust::interpreter::glue::interpret_prepared_cfg(
                &frame_cfg,
                st.clone(),
                &fixed_env,
            )
            .expect("interpreter frame failed")
            .into_iter()
            .map(|(s, _)| s),
        );
        out_compiled.extend(eng.run_frame_chunk(st, None).into_iter().map(|(s, _)| s));
    }
    let a = canon(out_interp);
    let b = canon(out_compiled);

    // One line per lane: shape hash prefix + heap walk in id order. The
    // walk renders vectorizable leaves per lane and everything else (the
    // shape-carried values) inline, so BOTH key channels are visible.
    fn render_lanes(states: &[State], rows: &mut Vec<String>) {
        for state in states {
            let shape = celeste_rust::interpreter::vectorize::shape_hash_of_state(state);
            for lane in 0..state.vector_size {
                let mut line = String::new();
                write!(line, "shape={:016x}", shape).unwrap();
                for i in 0..state.heap.len() {
                    let id = celeste_rust::interpreter::heap::HeapId::from_raw(i);
                    let Some(hv) = state.heap.get_opt(id) else {
                        write!(line, " h{}=empty", i).unwrap();
                        continue;
                    };
                    match hv {
                        HeapValue::Value(v) => {
                            write!(line, " h{}={}", i, render_value(v, lane)).unwrap()
                        }
                        HeapValue::ObjectTable(t) => {
                            let mut fields: Vec<_> =
                                t.iter().map(|(k, v)| (k.clone(), *v)).collect();
                            fields.sort();
                            write!(line, " h{}=obj{{", i).unwrap();
                            for (k, v) in fields {
                                write!(line, "{}:h{},", k, v.raw()).unwrap();
                            }
                            line.push('}');
                        }
                        HeapValue::ArrayTable(items) => {
                            write!(line, " h{}=arr{:?}", i, items
                                .iter()
                                .map(|x| x.raw())
                                .collect::<Vec<_>>())
                            .unwrap()
                        }
                        HeapValue::UnknownTable => write!(line, " h{}=unknowntable", i).unwrap(),
                        HeapValue::Closure(gid, caps) => {
                            write!(line, " h{}=clos({:?}", i, gid).unwrap();
                            for c in caps {
                                write!(line, ",{}", render_value(c, lane)).unwrap();
                            }
                            line.push(')');
                        }
                        HeapValue::BuiltinFun(name) => {
                            write!(line, " h{}=builtin({})", i, name).unwrap()
                        }
                    }
                }
                for (k, v) in state.global_env.iter() {
                    write!(line, " g:{}=h{}", k, v.raw()).unwrap();
                }
                if !state.prints.is_empty() {
                    write!(line, " prints={:?}", state.prints).unwrap();
                }
                rows.push(line);
            }
        }
    }

    fn render_value(v: &Value, lane: usize) -> String {
        match v {
            Value::Number(MaybeVector::Scalar(x)) => format!("n:{}", x.to_bits()),
            Value::Number(MaybeVector::Vector(x)) => format!("n:{}", x[lane].to_bits()),
            Value::NumberInterval(MaybeVector::Scalar(x)) => {
                format!("iv:{}..{}", x.low.to_bits(), x.high.to_bits())
            }
            Value::NumberInterval(MaybeVector::Vector(x)) => {
                format!("iv:{}..{}", x[lane].low.to_bits(), x[lane].high.to_bits())
            }
            Value::Bool(MaybeVector::Scalar(x)) => format!("b:{}", x),
            Value::Bool(MaybeVector::Vector(x)) => format!("b:{}", x[lane]),
            Value::UnknownBool => "ubool".to_string(),
            Value::String(s) => format!("str:{:?}", s),
            Value::Nil(hint) => format!("nil:{:?}", hint),
            Value::Pointer(id) => format!("ptr:h{}", id.raw()),
            Value::NilPointer(s) => format!("nilptr:{:?}", s),
            other => format!("other:{:?}", other),
        }
    }

    fn write_side(outdir: &str, name: &str, states: &[State]) {
        let mut rows = Vec::new();
        render_lanes(states, &mut rows);
        rows.sort();
        let n = rows.len();
        std::fs::write(
            format!("{}/{}.rows", outdir, name),
            rows.join("\n") + "\n",
        )
        .expect("write rows");
        let mut shapes = String::new();
        let mut seen = std::collections::BTreeMap::new();
        for state in states {
            let h = celeste_rust::interpreter::vectorize::shape_hash_of_state(state);
            seen.entry(h)
                .or_insert_with(|| celeste_rust::interpreter::vectorize::shape_of_state(state));
        }
        for (h, s) in &seen {
            writeln!(shapes, "=== shape {:016x} ===\n{:#?}\n", h, s).unwrap();
        }
        std::fs::write(format!("{}/{}.shapes", outdir, name), shapes).expect("write shapes");
        eprintln!(
            "[frame-diff] {}: {} lanes, {} state(s), {} distinct shape(s)",
            name,
            n,
            states.len(),
            seen.len()
        );
    }

    std::fs::create_dir_all(outdir).expect("create outdir");
    write_side(outdir, "interp", &a);
    write_side(outdir, "compiled", &b);
    eprintln!(
        "[frame-diff] wrote {}/{{interp,compiled}}.{{rows,shapes}} - diff with comm/diff",
        outdir
    );
}

/// Replays one frame's OFFERED key stream (dumped by the campaign with
/// `CELESTE_DUMP_OFFERED`, fragment-delimited, real probe order) against
/// the real mmap'd visited structure as it stood entering that frame, and
/// times the FILTER side of `visited_row_keys` - the local seen set plus
/// the global `contains_historic` probe - under several designs. Key
/// COMPUTATION is deliberately absent: the campaign's census now prints
/// its own hash/filter split, and this harness owns the filter half.
///
/// Every variant must produce the same candidate count - the decisions are
/// identical by construction (first-wins is order-independent), so a
/// mismatch is a harness bug, not a design result.
fn run_dedup_bench(dir: &str, frame: u32, reps: u32) {
    use celeste_rust::interpreter::visited::{FrameKeys, Visited};
    let dirp = std::path::Path::new(dir);

    // The visited set as of the START of `frame`: frames 1..frame-1.
    let mut watermarks = Vec::new();
    let mut total = 0u32;
    for f in 1..frame {
        let fk = FrameKeys::open(dirp, f).expect("open rowkeys sidecar");
        total += fk.count() as u32;
        watermarks.push(total);
    }
    let t0 = std::time::Instant::now();
    let visited = Visited::mmap_open(dirp, watermarks).expect("open visited");
    eprintln!(
        "[dedup-bench] visited as of f{:03}: {} rows, opened in {:.2?}",
        frame - 1,
        visited.len(),
        t0.elapsed()
    );

    // The offered stream: [u32 len][len * (u64,u64)] fragments.
    let raw = std::fs::read(
        dirp.join("offered").join(format!("f{:03}.offered", frame)),
    )
    .expect("read offered dump (run the campaign with CELESTE_DUMP_OFFERED)");
    let mut keys: Vec<(u64, u64)> = Vec::new();
    let mut frag_lens: Vec<u32> = Vec::new();
    {
        let mut off = 0usize;
        while off < raw.len() {
            let len = u32::from_le_bytes(raw[off..off + 4].try_into().unwrap());
            off += 4;
            frag_lens.push(len);
            for _ in 0..len {
                let lo = u64::from_le_bytes(raw[off..off + 8].try_into().unwrap());
                let hi = u64::from_le_bytes(raw[off + 8..off + 16].try_into().unwrap());
                keys.push((lo, hi));
                off += 16;
            }
        }
    }
    drop(raw);
    eprintln!(
        "[dedup-bench] f{:03}: {} offered keys in {} fragments (mean {:.0}/fragment)",
        frame,
        keys.len(),
        frag_lens.len(),
        keys.len() as f64 / frag_lens.len().max(1) as f64
    );

    let n = keys.len();
    let report = |name: &str, secs: f64, candidates: usize| {
        println!(
            "  {:<26} {:>8.2} ns/row  ({:.3} s, {} candidates)",
            name,
            secs * 1e9 / n as f64,
            secs,
            candidates
        );
    };

    // What the SERIAL phase would keep: the distinct not-historic keys.
    // Computed once, untimed - it is also the frame-seen variant's answer,
    // and today's 8x-duplicated candidate stream collapses to exactly this
    // in `insert_new`.
    let want_candidates = {
        let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
            rustc_hash::FxHashSet::default();
        let mut cand = 0usize;
        for &key in &keys {
            if seen.insert(key) && !visited.contains_historic(key) {
                cand += 1;
            }
        }
        cand
    };

    // Baseline: TODAY. Per-fragment seen set, local-first (the mmap
    // engine's order), global probe once per fragment-distinct key.
    // Its candidate count is the DUPLICATED event stream the serial phase
    // receives - reported, not asserted, because collapsing it is phase
    // 2's job.
    for rep in 0..reps {
        let t = std::time::Instant::now();
        let mut candidates = 0usize;
        let mut idx = 0usize;
        for &len in &frag_lens {
            let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
                rustc_hash::FxHashSet::default();
            for &key in &keys[idx..idx + len as usize] {
                if seen.insert(key) && !visited.contains_historic(key) {
                    candidates += 1;
                }
            }
            idx += len as usize;
        }
        let secs = t.elapsed().as_secs_f64();
        if rep == 0 {
            report("today (fragment seen)", secs, candidates);
        } else if rep == reps - 1 {
            report("today (fragment seen) min", secs, candidates);
        }
    }

    // Variant: ONE seen set for the whole frame. What the census predicts:
    // 8.5x fewer global probes than today.
    for rep in 0..reps {
        let t = std::time::Instant::now();
        let mut candidates = 0usize;
        let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
            rustc_hash::FxHashSet::default();
        for &key in &keys {
            if seen.insert(key) && !visited.contains_historic(key) {
                candidates += 1;
            }
        }
        let secs = t.elapsed().as_secs_f64();
        assert_eq!(candidates, want_candidates, "frame-seen changed a decision");
        if rep == 0 || rep == reps - 1 {
            report(
                if rep == 0 { "frame seen" } else { "frame seen min" },
                secs,
                candidates,
            );
        }
    }

    // Variant: frame seen, capacity preallocated (measures rehash cost).
    {
        let t = std::time::Instant::now();
        let mut candidates = 0usize;
        let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
            rustc_hash::FxHashSet::with_capacity_and_hasher(
                n / 8,
                Default::default(),
            );
        for &key in &keys {
            if seen.insert(key) && !visited.contains_historic(key) {
                candidates += 1;
            }
        }
        assert_eq!(candidates, want_candidates);
        report("frame seen prealloc", t.elapsed().as_secs_f64(), candidates);
    }

    // Variant: hash-partitioned frame seen, sequential - the cache story
    // of the per-worker partition (each partition's set is ~1/16 the
    // size), without thread-scaling effects. Stream is re-read per
    // partition, which is the streaming-friendly direction.
    for parts in [4usize, 16, 64] {
        let t = std::time::Instant::now();
        let mut candidates = 0usize;
        for p in 0..parts {
            let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
                rustc_hash::FxHashSet::default();
            for &key in &keys {
                if (key.0 as usize) % parts != p {
                    continue;
                }
                if seen.insert(key) && !visited.contains_historic(key) {
                    candidates += 1;
                }
            }
        }
        assert_eq!(candidates, want_candidates);
        report(
            &format!("partitioned x{} (serial)", parts),
            t.elapsed().as_secs_f64(),
            candidates,
        );
    }

    // Variant: hash-partitioned across REAL threads. Each thread owns a
    // disjoint key partition - no coordination, the design the plan
    // prefers for determinism option (a).
    for threads in [4usize, 8, 16] {
        let t = std::time::Instant::now();
        let candidates: usize = std::thread::scope(|s| {
            let mut handles = Vec::new();
            for p in 0..threads {
                let keys = &keys;
                let visited = &visited;
                handles.push(s.spawn(move || {
                    let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
                        rustc_hash::FxHashSet::default();
                    let mut cand = 0usize;
                    for &key in keys {
                        if (key.0 as usize) % threads != p {
                            continue;
                        }
                        if seen.insert(key) && !visited.contains_historic(key) {
                            cand += 1;
                        }
                    }
                    cand
                }));
            }
            handles.into_iter().map(|h| h.join().unwrap()).sum()
        });
        assert_eq!(candidates, want_candidates);
        report(
            &format!("partitioned {} threads", threads),
            t.elapsed().as_secs_f64(),
            candidates,
        );
    }

    // Variant: WORKER-persistent seen. 16 threads, each takes every 16th
    // FRAGMENT (round-robin, like the worker pool) and keeps ONE seen set
    // across its fragments. This is the drop-in integration: no key
    // resharding, no ordering questions - the seen filter is sound at any
    // scope, so decisions are identical to today by construction. Reports
    // the residual probe count too (between the frame-distinct floor and
    // today's fragment-distinct count).
    for threads in [16usize] {
        let t = std::time::Instant::now();
        let frag_starts: Vec<usize> = {
            let mut v = Vec::with_capacity(frag_lens.len());
            let mut acc = 0usize;
            for &len in &frag_lens {
                v.push(acc);
                acc += len as usize;
            }
            v
        };
        let (candidates, probes): (usize, usize) = std::thread::scope(|s| {
            let mut handles = Vec::new();
            for p in 0..threads {
                let keys = &keys;
                let visited = &visited;
                let frag_lens = &frag_lens;
                let frag_starts = &frag_starts;
                handles.push(s.spawn(move || {
                    let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
                        rustc_hash::FxHashSet::default();
                    let (mut cand, mut probes) = (0usize, 0usize);
                    for f in (p..frag_lens.len()).step_by(threads) {
                        let lo = frag_starts[f];
                        let hi = lo + frag_lens[f] as usize;
                        for &key in &keys[lo..hi] {
                            if seen.insert(key) {
                                probes += 1;
                                if !visited.contains_historic(key) {
                                    cand += 1;
                                }
                            }
                        }
                    }
                    (cand, probes)
                }));
            }
            handles
                .into_iter()
                .map(|h| h.join().unwrap())
                .fold((0, 0), |a, b| (a.0 + b.0, a.1 + b.1))
        });
        report(
            &format!("worker seen {} threads", threads),
            t.elapsed().as_secs_f64(),
            candidates,
        );
        println!(
            "    (worker-persistent seen: {} global probes vs today's fragment-level count)",
            probes
        );
    }

    // Reference points without the global probe at all: what the seen-set
    // machinery itself costs, fragment- and frame-wide.
    {
        let t = std::time::Instant::now();
        let mut acc = 0usize;
        let mut idx = 0usize;
        for &len in &frag_lens {
            let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
                rustc_hash::FxHashSet::default();
            for &key in &keys[idx..idx + len as usize] {
                if seen.insert(key) {
                    acc += 1;
                }
            }
            idx += len as usize;
        }
        report("fragment seen only (no probe)", t.elapsed().as_secs_f64(), acc);
        let t = std::time::Instant::now();
        let mut seen: rustc_hash::FxHashSet<(u64, u64)> =
            rustc_hash::FxHashSet::default();
        let mut acc = 0usize;
        for &key in &keys {
            if seen.insert(key) {
                acc += 1;
            }
        }
        report("frame seen only (no probe)", t.elapsed().as_secs_f64(), acc);
    }
}
