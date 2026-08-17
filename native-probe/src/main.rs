//! Driver for the transpiled Celeste IR (native-compile probe, task #124).
//!
//! Validation mode mirrors `concrete_run`'s per-frame output exactly
//! (`src/bin/concrete_run.rs` print_frame / format_num), so
//! `diff <(concrete_run ...) <(native_probe ...)` on the `^Frame` lines is
//! the hex-exactness gate. `--bench N` reruns the same trajectory N times
//! from the post-init snapshot and reports ns/frame.

// The generated code declares every local up front and keeps the IR's
// (sometimes dead) assignments; silence the corresponding lints crate-wide.
#![allow(unused_variables, unused_assignments, unused_mut, unreachable_code, dead_code)]

mod gen;
mod import;
mod runtime;
mod runtime2;

use celeste_rust::cart_data::CartData;
use celeste_rust::collision_cache::CollisionCache;
use celeste_rust::pico8_num::Pico8Num;
use runtime::{Cell, Rt, BUILTIN_NAMES, V};

fn format_input(byte: u8) -> String {
    // concrete.rs:27.
    const NAMES: [&str; 6] = ["L", "R", "U", "D", "J", "X"];
    let pressed: Vec<&str> = (0..6)
        .filter(|i| byte >> i & 1 == 1)
        .map(|i| NAMES[i as usize])
        .collect();
    if pressed.is_empty() {
        "-".to_string()
    } else {
        pressed.join("+")
    }
}

/// concrete_run.rs:93 - floor.frac_hex, NOT sign-magnitude.
fn format_num(n: Pico8Num) -> String {
    let whole = n.whole_part_as_i16();
    let frac = n.fraction_part_as_u16();
    if frac == 0 {
        format!("{}", whole)
    } else {
        format!("{}.{:04x}", whole, frac)
    }
}

/// Cached interned ids for the printer / button poke.
struct Probe {
    g_button_states: u32,
    g_objects: Option<u32>,
    g_freeze: Option<u32>,
    g_player: Option<u32>,
    g_player_spawn: Option<u32>,
    f_type: Option<u32>,
    f_x: Option<u32>,
    f_y: Option<u32>,
    f_spd: Option<u32>,
    f_rem: Option<u32>,
}

impl Probe {
    fn new() -> Self {
        Probe {
            g_button_states: gen::global_id("__button_states")
                .expect("program never mentions __button_states"),
            g_objects: gen::global_id("objects"),
            g_freeze: gen::global_id("freeze"),
            g_player: gen::global_id("player"),
            g_player_spawn: gen::global_id("player_spawn"),
            f_type: gen::field_id("type"),
            f_x: gen::field_id("x"),
            f_y: gen::field_id("y"),
            f_spd: gen::field_id("spd"),
            f_rem: gen::field_id("rem"),
        }
    }
}

/// concrete.rs:45 set_concrete_buttons.
fn set_buttons(rt: &mut Rt, probe: &Probe, byte: u8) {
    let cell = rt.globals[probe.g_button_states as usize];
    assert!(cell != runtime::NONE, "no __button_states global");
    let arr = match &rt.heap[cell as usize] {
        Cell::Val(V::Ptr(id)) => *id,
        Cell::Arr(_) => cell,
        other => panic!("__button_states shape: {:?}", other),
    };
    let items = match &rt.heap[arr as usize] {
        Cell::Arr(items) => items.clone(),
        other => panic!("button array shape: {:?}", other),
    };
    for (i, item) in items.iter().enumerate() {
        let pressed = byte >> i & 1 == 1;
        let target = match &rt.heap[*item as usize] {
            Cell::Val(V::Ptr(id)) => *id,
            _ => *item,
        };
        rt.heap[target as usize] = Cell::Val(V::Bool(pressed));
    }
}

/// Dereference a global to its target cell: the global cell holds
/// `Val(Ptr(target))` (inspect.rs find_global + load convention).
fn global_target(rt: &Rt, g: Option<u32>) -> Option<u32> {
    let cell = rt.globals[g? as usize];
    if cell == runtime::NONE {
        return None;
    }
    match &rt.heap[cell as usize] {
        Cell::Val(V::Ptr(id)) => Some(*id),
        _ => None,
    }
}

fn obj_field_cell(rt: &Rt, obj: u32, f: Option<u32>) -> Option<u32> {
    let f = f?;
    match &rt.heap[obj as usize] {
        Cell::Obj(fields) => fields.iter().find(|(k, _)| *k == f).map(|(_, c)| *c),
        _ => None,
    }
}

fn obj_number_field(rt: &Rt, obj: u32, f: Option<u32>) -> Option<Pico8Num> {
    let cell = obj_field_cell(rt, obj, f)?;
    match &rt.heap[cell as usize] {
        Cell::Val(V::Num(n)) => Some(*n),
        _ => None,
    }
}

fn obj_xy_field(rt: &Rt, obj: u32, f: Option<u32>, probe: &Probe) -> Option<(Pico8Num, Pico8Num)> {
    let cell = obj_field_cell(rt, obj, f)?;
    let sub = match &rt.heap[cell as usize] {
        Cell::Val(V::Ptr(id)) => *id,
        _ => return None,
    };
    Some((
        obj_number_field(rt, sub, probe.f_x)?,
        obj_number_field(rt, sub, probe.f_y)?,
    ))
}

/// inspect.rs:394 find_objects_by_type, first match.
fn find_object(rt: &Rt, probe: &Probe, type_global: Option<u32>) -> Option<u32> {
    let type_target = global_target(rt, type_global)?;
    let objects_arr = global_target(rt, probe.g_objects)?;
    let items = match &rt.heap[objects_arr as usize] {
        Cell::Arr(items) => items,
        _ => return None,
    };
    for item in items {
        let obj = match &rt.heap[*item as usize] {
            Cell::Val(V::Ptr(id)) => *id,
            _ => continue,
        };
        if !matches!(&rt.heap[obj as usize], Cell::Obj(_)) {
            continue;
        }
        if let Some(type_cell) = obj_field_cell(rt, obj, probe.f_type) {
            if matches!(&rt.heap[type_cell as usize], Cell::Val(V::Ptr(t)) if *t == type_target) {
                return Some(obj);
            }
        }
    }
    None
}

fn get_freeze(rt: &Rt, probe: &Probe) -> Option<Pico8Num> {
    let g = probe.g_freeze?;
    let cell = rt.globals[g as usize];
    if cell == runtime::NONE {
        return None;
    }
    match &rt.heap[cell as usize] {
        Cell::Val(V::Num(n)) => Some(*n),
        _ => None,
    }
}

/// concrete_run.rs:103 print_frame, verbatim format.
fn print_frame(rt: &Rt, probe: &Probe, frame_num: u32, input_byte: u8) {
    let freeze = get_freeze(rt, probe)
        .map(format_num)
        .unwrap_or_else(|| "?".to_string());
    if let Some(player) = find_object(rt, probe, probe.g_player) {
        let (x, y) = match (
            obj_number_field(rt, player, probe.f_x),
            obj_number_field(rt, player, probe.f_y),
        ) {
            (Some(x), Some(y)) => (x, y),
            _ => {
                println!(
                    "Frame {}: no player/player_spawn found, freeze={} input={}",
                    frame_num,
                    freeze,
                    format_input(input_byte)
                );
                return;
            }
        };
        let (spd_x, spd_y) = obj_xy_field(rt, player, probe.f_spd, probe)
            .unwrap_or((Pico8Num::from_i16(0), Pico8Num::from_i16(0)));
        let rem_str = match obj_xy_field(rt, player, probe.f_rem, probe) {
            Some((rx, ry)) => format!(" rem=({}, {})", format_num(rx), format_num(ry)),
            None => String::new(),
        };
        println!(
            "Frame {}: player at ({}, {}) spd=({}, {}){} freeze={} input={}",
            frame_num,
            format_num(x),
            format_num(y),
            format_num(spd_x),
            format_num(spd_y),
            rem_str,
            freeze,
            format_input(input_byte)
        );
    } else if let Some(spawn) = find_object(rt, probe, probe.g_player_spawn) {
        let x = obj_number_field(rt, spawn, probe.f_x);
        let y = obj_number_field(rt, spawn, probe.f_y);
        if let (Some(x), Some(y)) = (x, y) {
            println!(
                "Frame {}: player_spawn at ({}, {}) freeze={} input={}",
                frame_num,
                format_num(x),
                format_num(y),
                freeze,
                format_input(input_byte)
            );
        }
    } else {
        println!(
            "Frame {}: no player/player_spawn found, freeze={} input={}",
            frame_num,
            freeze,
            format_input(input_byte)
        );
    }
}

fn print_frame_zero(rt: &Rt, probe: &Probe) {
    // concrete_run.rs:184-199.
    for (global, name) in [(probe.g_player_spawn, "player_spawn"), (probe.g_player, "player")] {
        if let Some(obj) = find_object(rt, probe, global) {
            if let (Some(x), Some(y)) = (
                obj_number_field(rt, obj, probe.f_x),
                obj_number_field(rt, obj, probe.f_y),
            ) {
                println!("Frame 0: {} at ({}, {})", name, format_num(x), format_num(y));
                return;
            }
        }
    }
}

fn build_rt() -> Rt {
    let (room_x, room_y) = celeste_rust::game_runner::start_room();
    // Runnable both from the repo root and from native-probe/.
    let cart_base = if std::path::Path::new("cart").exists() { "cart" } else { "../cart" };
    let cart = std::sync::Arc::new(CartData::load(cart_base).expect("failed to load cart data"));
    let cache = std::sync::Arc::new(
        CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
    );
    eprintln!("[native-probe] collision cache for room ({}, {})", room_x, room_y);

    let mut rt = Rt::new(cart, cache, gen::GLOBAL_NAMES.len(), gen::STRINGS);
    // create_initial_state_with_builtins (game_runner.rs:878): one cell per
    // builtin, bound to its global name.
    for (i, name) in BUILTIN_NAMES.iter().enumerate() {
        let cell = rt.alloc(Cell::Bi(i as u32));
        if let Some(g) = gen::global_id(name) {
            rt.globals[g as usize] = cell;
        }
    }
    gen::call_fn(&mut rt, gen::FN_INIT, &[], &[]);
    // inject_tile_flag_at_builtin (game_runner.rs:889): fresh cell, repoint
    // the global at it (the Lua init shadowed the registration).
    let cell = rt.alloc(Cell::Bi(runtime::BI_TILE_FLAG_AT));
    let g = gen::global_id("tile_flag_at").expect("no tile_flag_at global");
    rt.globals[g as usize] = cell;
    rt
}

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

/// Gap census (plans/native-probe.md): run real snapshot lanes through the
/// compiled frame with receiver logging, and report which get_field/
/// get_index sites resolve to a single cell per shape (columnizable, with
/// a runtime guard) vs many (the shape's overlay to-do list).
fn run_census(dir: &str, frame: u32, census_frames: u32, max_lanes: usize) {
    let states = load_states_any(dir, frame);
    let cart_base = if std::path::Path::new("cart").exists() { "cart" } else { "../cart" };
    let (room_x, room_y) = celeste_rust::game_runner::start_room();
    let cart =
        std::sync::Arc::new(CartData::load(cart_base).expect("failed to load cart data"));
    let cache = std::sync::Arc::new(
        CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
    );
    let probe = Probe::new();

    // Global receiver lattice + failure counts.
    let mut acc: Vec<u64> = vec![0; gen::SITE_INFO.len()];
    let (mut lanes_run, mut lanes_panicked) = (0usize, 0usize);
    let total_lanes: usize = states.iter().map(|s| s.vector_size).sum();
    let stride = (total_lanes / max_lanes.max(1)).max(1);

    let mut lane_cursor = 0usize;
    'outer: for state in &states {
        for lane in 0..state.vector_size {
            lane_cursor += 1;
            if lane_cursor % stride != 0 {
                continue;
            }
            if lanes_run + lanes_panicked >= max_lanes {
                break 'outer;
            }
            // A fresh world per lane; cart/cache shared.
            let mut rt = Rt::new(
                cart.clone(),
                cache.clone(),
                gen::GLOBAL_NAMES.len(),
                gen::STRINGS,
            );
            rt.site_log = vec![0; gen::SITE_INFO.len()];
            import::import_lane(&mut rt, state, lane, true);
            // Builtin bindings are init-invariants, not state: snapshots
            // saved before a builtin existed (bench-r1-fresh predates
            // __split_at) lack its global; backfill exactly like
            // create_initial_state_with_builtins (game_runner.rs:878).
            for (i, name) in BUILTIN_NAMES.iter().enumerate() {
                if let Some(g) = gen::global_id(name) {
                    if rt.globals[g as usize] == runtime::NONE {
                        let cell = rt.alloc(Cell::Bi(i as u32));
                        rt.globals[g as usize] = cell;
                    }
                }
            }
            let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                for step in 0..census_frames {
                    // Vary inputs a little so both input arms are exercised.
                    let byte = [2u8, 18, 34, 0][(lane + step as usize) % 4];
                    set_buttons(&mut rt, &probe, byte);
                    gen::call_fn(&mut rt, gen::FN_FRAME, &[], &[]);
                }
            }));
            match outcome {
                Ok(()) => lanes_run += 1,
                Err(_) => lanes_panicked += 1,
            }
            // Join this lane's lattice into the accumulator (panicked lanes
            // included: sites they DID reach before failing still count).
            for (a, &s) in acc.iter_mut().zip(rt.site_log.iter()) {
                if s == 0 {
                    continue;
                }
                *a = match *a {
                    0 => s,
                    x if x == s => x,
                    _ => u64::MAX,
                };
            }
        }
    }

    let mut single = 0usize;
    let mut multi: Vec<usize> = Vec::new();
    let mut unseen = 0usize;
    for (i, &s) in acc.iter().enumerate() {
        match s {
            0 => unseen += 1,
            u64::MAX => multi.push(i),
            _ => single += 1,
        }
    }
    println!(
        "census: {} lanes run, {} panicked (premise failures), {} sites: {} single-receiver, {} multi, {} unreached",
        lanes_run, lanes_panicked, acc.len(), single, multi.len(), unseen
    );
    let mut by_fn: std::collections::BTreeMap<&str, Vec<String>> = Default::default();
    for i in multi {
        let (kind, fn_name, f, iid) = gen::SITE_INFO[i];
        let label = if kind == "field" {
            format!("site {} %{} get_field .{}", i, iid, gen::FIELD_NAMES[f as usize])
        } else {
            format!("site {} %{} get_index", i, iid)
        };
        by_fn.entry(fn_name).or_default().push(label);
    }
    println!("== gaps (multi-receiver sites) by function ==");
    for (fn_name, sites) in &by_fn {
        println!("{} ({}):", fn_name, sites.len());
        for s in sites {
            println!("    {}", s);
        }
    }
}

/// SIMD-sizing measurement: one snapshot lane, one frame, all 64 input
/// bytes - which branch sites diverge across the fan-out? (Sites that
/// stay uniform run lockstep in a SIMD engine; divergent sites need
/// both-arms + blend.)
fn run_branch_census(dir: &str, frame: u32) {
    let states = load_states_any(dir, frame);
    // The biggest state: the lane axis needs real lane diversity.
    let state = states
        .iter()
        .max_by_key(|s| s.vector_size)
        .expect("no states");
    let cart_base = if std::path::Path::new("cart").exists() { "cart" } else { "../cart" };
    let (room_x, room_y) = celeste_rust::game_runner::start_room();
    let cart =
        std::sync::Arc::new(CartData::load(cart_base).expect("failed to load cart data"));
    let cache = std::sync::Arc::new(
        CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
    );
    let probe = Probe::new();
    // Per site: first run's sequence hash + divergence flag.
    let mut first: Vec<Option<u64>> = vec![None; gen::BRANCH_INFO.len()];
    let mut div: Vec<bool> = vec![false; gen::BRANCH_INFO.len()];
    // Distinct nonzero sequence hashes per site: a divergent site with ONE
    // nonzero hash is a pure SHADOW of an upstream gate (it always does the
    // same thing when it runs at all); real divergence needs >= 2.
    let mut nonzero: Vec<Vec<u64>> = vec![Vec::new(); gen::BRANCH_INFO.len()];
    let mut panics = 0usize;
    // Axis 1: input fan-out (one lane, 64 bytes). Axis 2: lane batch
    // (byte 2 = hold right, up to 256 lanes spread across the state) -
    // the union sizes the SIMD design.
    let max_lane = state.vector_size.min(256);
    eprintln!(
        "[branch-census] state with {} lanes; {} input runs + {} lane runs",
        state.vector_size, 64, max_lane
    );
    let runs: Vec<(u8, usize)> = (0u8..64)
        .map(|b| (b, 0usize))
        .chain((0..max_lane).map(|l| (2u8, l)))
        .collect();
    for (byte, lane) in runs {
        let mut rt = Rt::new(
            cart.clone(),
            cache.clone(),
            gen::GLOBAL_NAMES.len(),
            gen::STRINGS,
        );
        rt.branch_log = vec![0; gen::BRANCH_INFO.len()];
        import::import_lane(&mut rt, state, lane, true);
        for (i, name) in BUILTIN_NAMES.iter().enumerate() {
            if let Some(g) = gen::global_id(name) {
                if rt.globals[g as usize] == runtime::NONE {
                    let cell = rt.alloc(Cell::Bi(i as u32));
                    rt.globals[g as usize] = cell;
                }
            }
        }
        set_buttons(&mut rt, &probe, byte);
        let outcome = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            gen::call_fn(&mut rt, gen::FN_FRAME, &[], &[]);
        }));
        if outcome.is_err() {
            panics += 1;
            continue; // a panicked run's partial sequence is not comparable
        }
        for (i, &h) in rt.branch_log.iter().enumerate() {
            match first[i] {
                None => first[i] = Some(h),
                Some(f) if f != h => div[i] = true,
                _ => {}
            }
            if h != 0 && !nonzero[i].contains(&h) {
                nonzero[i].push(h);
            }
        }
    }
    let executed = first.iter().filter(|f| **f != Some(0) && f.is_some()).count();
    let divergent: Vec<usize> = div
        .iter()
        .enumerate()
        .filter(|(_, &d)| d)
        .map(|(i, _)| i)
        .collect();
    println!(
        "branch census: {} sites total, {} executed, {} DIVERGENT (sequence differs across runs), {} panicked runs",
        first.len(),
        executed,
        divergent.len(),
        panics
    );
    for i in &divergent {
        let n = nonzero[*i].len();
        println!(
            "  site {}: {} [{} distinct nonzero sequence(s){}]",
            i,
            gen::BRANCH_INFO[*i],
            n,
            if n <= 1 { " - SHADOW of an upstream gate" } else { "" }
        );
    }
}

fn main() {
    let mut inputs: Vec<u8> = Vec::new();
    let mut frames: Option<u32> = None;
    let mut bench_reps: Option<u32> = None;
    let mut from_checkpoint: Option<(String, u32)> = None;
    let mut abstract_frames: Option<u32> = None;
    let mut census_frames: u32 = 2;
    let mut max_lanes: usize = 256;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--from-checkpoint" => {
                let dir = args.next().expect("--from-checkpoint needs DIR");
                let frame = args
                    .next()
                    .expect("--from-checkpoint needs DIR FRAME")
                    .parse()
                    .unwrap();
                from_checkpoint = Some((dir, frame));
            }
            "--branch-census" => {
                let dir = args.next().expect("--branch-census needs DIR FRAME");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                run_branch_census(&dir, frame);
                return;
            }
            "--census-frames" => {
                census_frames = args.next().expect("value").parse().unwrap()
            }
            "--max-lanes" => max_lanes = args.next().expect("value").parse().unwrap(),
            "-i" => {
                inputs = args
                    .next()
                    .expect("-i needs a value")
                    .split(',')
                    .map(|s| s.trim().parse().expect("invalid input byte"))
                    .collect()
            }
            "-f" => frames = Some(args.next().expect("-f needs a value").parse().unwrap()),
            "--abstract" => {
                abstract_frames =
                    Some(args.next().expect("--abstract needs FRAMES").parse().unwrap())
            }
            "--bench" => {
                bench_reps = Some(args.next().expect("--bench needs a value").parse().unwrap())
            }
            other => panic!("unknown argument {:?}", other),
        }
    }
    let num_frames = frames.unwrap_or(inputs.len() as u32);

    if let Some((dir, frame)) = from_checkpoint {
        run_census(&dir, frame, census_frames, max_lanes);
        return;
    }

    if let Some(n) = abstract_frames {
        run_abstract(n);
        return;
    }

    let probe = Probe::new();
    let mut rt = build_rt();

    if let Some(reps) = bench_reps {
        // Snapshot the post-init world; each rep restores and replays the
        // whole trajectory, so the restore cost amortises over num_frames.
        let heap0 = rt.heap.clone();
        let globals0 = rt.globals.clone();
        let nstrings0 = rt.strings.len();
        let start = std::time::Instant::now();
        for _ in 0..reps {
            rt.heap.clear();
            rt.heap.extend_from_slice(&heap0);
            rt.globals.copy_from_slice(&globals0);
            rt.strings.truncate(nstrings0);
            rt.prints.clear();
            for frame_num in 1..=num_frames {
                let byte = *inputs.get(frame_num as usize - 1).unwrap_or(&0);
                set_buttons(&mut rt, &probe, byte);
                gen::call_fn(&mut rt, gen::FN_FRAME, &[], &[]);
            }
        }
        let elapsed = start.elapsed();
        let total_frames = reps as u64 * num_frames as u64;
        println!(
            "bench: {} reps x {} frames = {} frames in {:.3?} -> {:.0} ns/frame",
            reps,
            num_frames,
            total_frames,
            elapsed,
            elapsed.as_nanos() as f64 / total_frames as f64
        );
        // One validation line so a bench run can't silently diverge.
        let byte = *inputs.get(num_frames as usize - 1).unwrap_or(&0);
        print_frame(&rt, &probe, num_frames, byte);
        return;
    }

    print_frame_zero(&rt, &probe);
    for frame_num in 1..=num_frames {
        let byte = *inputs.get(frame_num as usize - 1).unwrap_or(&0);
        set_buttons(&mut rt, &probe, byte);
        gen::call_fn(&mut rt, gen::FN_FRAME, &[], &[]);
        print_frame(&rt, &probe, frame_num, byte);
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
    let rt = build_rt();
    let rt2 = runtime2::Rt2::from_scalar(&rt);
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    let ids = runtime2::BoundaryIds {
        g_objects: g("objects"),
        g_player: g("player"),
        g_timers: ["frames", "seconds", "minutes", "deaths"].iter().map(|n| g(n)).collect(),
        f_type: f("type"),
        f_rem: f("rem"),
        f_spd: f("spd"),
        f_x: f("x"),
        f_y: f("y"),
        f_dash_effect_time: f("dash_effect_time"),
    };
    let g_freeze = g("freeze");
    // SplitReq panics are control flow, not errors - keep them off stderr.
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        if info.payload().downcast_ref::<runtime2::SplitReq>().is_none() {
            prev_hook(info);
        }
    }));
    let mut blocks: Vec<runtime2::Rt2> = vec![rt2];
    let start = std::time::Instant::now();
    for frame in 1..=num_frames {
        let t0 = std::time::Instant::now();
        // Pre-partition each block by the freeze value (the known
        // frame-start divergent gate), then run. A block that hits another
        // genuinely divergent branch throws a SplitReq with the
        // per-frame-start-lane truth of the condition; partition the
        // frame-start block by it and rerun both sides.
        let mut ran: Vec<runtime2::Rt2> = Vec::new();
        let mut pending: Vec<runtime2::Rt2> = Vec::new();
        for block in blocks.drain(..) {
            let freeze_cell = block.globals[g_freeze as usize];
            assert!(freeze_cell != runtime2::NONE);
            pending.extend(block.partition_by_cell(freeze_cell));
        }
        while let Some(block) = pending.pop() {
            let snapshot = block.clone_block();
            let mut sub = block;
            sub.begin_frame();
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                gen::call_fn(&mut sub, gen::FN_FRAME, &[], &[]);
                sub.boundary(&ids);
            }));
            match result {
                Ok(()) => ran.push(sub),
                Err(payload) => match payload.downcast::<runtime2::SplitReq>() {
                    Ok(req) => {
                        let trues: Vec<u32> = (0..snapshot.width as u32)
                            .filter(|&i| req.origin_truth[i as usize])
                            .collect();
                        let falses: Vec<u32> = (0..snapshot.width as u32)
                            .filter(|&i| !req.origin_truth[i as usize])
                            .collect();
                        assert!(!trues.is_empty() && !falses.is_empty());
                        let mut a = snapshot.clone_block();
                        a.retain_lanes(&trues);
                        let mut b = snapshot;
                        b.retain_lanes(&falses);
                        pending.push(a);
                        pending.push(b);
                    }
                    Err(other) => std::panic::resume_unwind(other),
                },
            }
        }
        // Merge same-shape blocks; drop rows already seen this frame.
        let mut seen: rustc_hash::FxHashMap<(u64, u64), ()> = Default::default();
        let mut merged: Vec<runtime2::Rt2> = Vec::new();
        for mut sub in ran {
            let keep: Vec<u32> = (0..sub.width as u32)
                .filter(|&i| {
                    let k = sub.row_keys[i as usize];
                    if seen.contains_key(&k) {
                        false
                    } else {
                        seen.insert(k, ());
                        true
                    }
                })
                .collect();
            sub.retain_lanes(&keep);
            if sub.width == 0 {
                continue;
            }
            match merged.iter_mut().find(|b| b.shape_hash == sub.shape_hash) {
                Some(host) => host.concat(&sub),
                None => merged.push(sub),
            }
        }
        blocks = merged;
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
}
