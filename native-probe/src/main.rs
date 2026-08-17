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
mod runtime3;

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

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
fn run_census(
    dir: &str,
    frame: u32,
    census_frames: u32,
    max_lanes: usize,
    emit_slots: Option<&str>,
) {
    let states = load_states_any(dir, frame);
    let cart_base = if std::path::Path::new("cart").exists() { "cart" } else { "../cart" };
    let (room_x, room_y) = celeste_rust::game_runner::start_room();
    let cart =
        std::sync::Arc::new(CartData::load(cart_base).expect("failed to load cart data"));
    let cache = std::sync::Arc::new(
        CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache"),
    );
    let probe = Probe::new();

    // Slot-dump mode: verify the scalar and vectorized importers assign
    // identical (canonical) cell ids, so result-cell ids ARE slot ids.
    // The census SHAPE (canonical structure hash) goes into the dump:
    // slot cell ids are meaningless on any other shape, so the tile
    // path deopts to the reference engine off-shape (spawn/death/other
    // rooms) - the bug this caught was frame 1's spawn shape binding
    // steady-state cell ids.
    let mut census_shape: u64 = 0;
    if emit_slots.is_some() {
        let s0 = &states[0];
        let mut rt = Rt::new(cart.clone(), cache.clone(), gen::GLOBAL_NAMES.len(), gen::STRINGS);
        import::import_lane(&mut rt, s0, 0, true);
        let block = import::import_block(s0, cart.clone(), cache.clone());
        import::assert_lane_matches_block(&rt, &block);
        census_shape = block.shape_hash_of();
        println!(
            "emit-slots: scalar/block importer agreement verified ({} cells, shape {:#018x})",
            rt.heap.len(),
            census_shape
        );
    }

    // Global receiver + result-cell lattices, failure counts. `taint`
    // holds every result code seen at a site that is multi-result in ANY
    // lane or across lanes - cells in it cannot be slots.
    let mut acc: Vec<u64> = vec![0; gen::SITE_INFO.len()];
    let mut acc_result: Vec<u64> = vec![0; gen::SITE_INFO.len()];
    let mut taint: std::collections::HashSet<u64> = Default::default();
    let mut imported_cells: Option<usize> = None;
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
            if emit_slots.is_some() {
                rt.result_log = vec![0; gen::SITE_INFO.len()];
            }
            import::import_lane(&mut rt, state, lane, true);
            // Cells imported at the boundary are the canonical (slot-
            // eligible) ones; anything alloc'd later (builtin backfill,
            // mid-frame allocs) is not. Same shape => same count.
            match imported_cells {
                None => imported_cells = Some(rt.heap.len()),
                Some(n) => assert_eq!(
                    n,
                    rt.heap.len(),
                    "states in {} disagree on boundary cell count (mixed shapes?)",
                    dir
                ),
            }
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
            // Join this lane's lattices into the accumulators (panicked
            // lanes included: sites they DID reach before failing still
            // count).
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
            for (a, &s) in acc_result.iter_mut().zip(rt.result_log.iter()) {
                if s == 0 {
                    continue;
                }
                *a = match *a {
                    0 => s,
                    x if x == s => x,
                    x => {
                        // Cross-lane multi-result: both codes are aliased.
                        if x != u64::MAX {
                            taint.insert(x);
                        }
                        taint.insert(s);
                        u64::MAX
                    }
                };
            }
            taint.extend(rt.result_taint.iter().copied());
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

    // Slot dump (plans/columnar-engine.md "Slot compilation" step 1):
    // group slot-eligible sites by their result cell. Eligible = single
    // receiver, single result cell, result cell is a BOUNDARY cell
    // (imported id < boundary count) - those ids ARE canonical ids
    // (BFS importer, verified above), so the transpiler and Rt3 can use
    // them directly.
    if let Some(path) = emit_slots {
        let n_boundary = imported_cells.expect("no lanes imported") as u64;
        let mut by_cell: std::collections::BTreeMap<u64, Vec<usize>> = Default::default();
        let (mut n_unseen, mut n_multi_recv, mut n_multi_result, mut n_nonboundary) =
            (0usize, 0usize, 0usize, 0usize);
        let mut n_aliased = 0usize;
        for (i, (&recv, &res)) in acc.iter().zip(acc_result.iter()).enumerate() {
            match (recv, res) {
                (0, _) | (_, 0) => n_unseen += 1,
                (u64::MAX, _) => n_multi_recv += 1,
                (_, u64::MAX) => n_multi_result += 1,
                (_, res) if res - 1 >= n_boundary => n_nonboundary += 1,
                // A multi-result site somewhere also produced this cell:
                // it has an access path outside the binding - no slot.
                (_, res) if taint.contains(&res) => n_aliased += 1,
                (_, res) => by_cell.entry(res - 1).or_default().push(i),
            }
        }
        let slots: Vec<serde_json::Value> = by_cell
            .iter()
            .enumerate()
            .map(|(slot, (&cell, sites))| {
                serde_json::json!({
                    "slot": slot,
                    "cell": cell,
                    "sites": sites
                        .iter()
                        .map(|&i| {
                            let (kind, fn_name, f, iid) = gen::SITE_INFO[i];
                            serde_json::json!({
                                "site": i,
                                "fn": fn_name,
                                "iid": iid,
                                "kind": kind,
                                "field": if kind == "field" {
                                    serde_json::json!(gen::FIELD_NAMES[f as usize])
                                } else {
                                    serde_json::Value::Null
                                },
                                "recv_cell": acc[i] - 1,
                            })
                        })
                        .collect::<Vec<_>>(),
                })
            })
            .collect();
        let out = serde_json::json!({
            "dir": dir,
            "frame": frame,
            "census_frames": census_frames,
            "lanes_run": lanes_run,
            "boundary_cells": n_boundary,
            "shape_hash": census_shape,
            "n_slots": slots.len(),
            "n_sites_bound": slots.iter().map(|s| s["sites"].as_array().unwrap().len()).sum::<usize>(),
            "slots": slots,
            "skipped": {
                "unseen_or_no_result": n_unseen,
                "multi_recv": n_multi_recv,
                "multi_result": n_multi_result,
                "nonboundary_result": n_nonboundary,
                "aliased": n_aliased,
            },
        });
        std::fs::write(path, serde_json::to_string_pretty(&out).unwrap())
            .unwrap_or_else(|e| panic!("write {}: {}", path, e));
        println!(
            "emit-slots: {} slots covering {} sites -> {} (skipped: {} multi-recv, {} multi-result, {} non-boundary, {} aliased, {} unseen/no-result)",
            out["n_slots"], out["n_sites_bound"], path,
            n_multi_recv, n_multi_result, n_nonboundary, n_aliased, n_unseen
        );
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
    // Which executed sites run more than once per frame? A sequence hash of
    // a single taken/not-taken outcome is one of two constants; anything
    // else means the site executed repeatedly - i.e., it sits in a LOOP.
    // (h(true) = 0*31+2 = 2, h(false) = 1.)
    eprintln!("executed sites (seq-hash 1/2 = straight-line once; other = loop or multi-visit):");
    for (i, f) in first.iter().enumerate() {
        if let Some(h) = f {
            if *h != 0 {
                let kind = if *h == 1 || *h == 2 { "once" } else { "MULTI" };
                eprintln!("  [{}] {} {}", kind, i, gen::BRANCH_INFO[i]);
            }
        }
    }
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
    let mut abstract_bench: Option<(String, u32)> = None;
    let mut reps: u32 = 10;
    let mut census_frames: u32 = 2;
    let mut max_lanes: usize = 256;
    let mut emit_slots: Option<String> = None;
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
            "--emit-slots" => emit_slots = Some(args.next().expect("--emit-slots needs FILE")),
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
            "--abstract-bench" => {
                let dir = args.next().expect("--abstract-bench needs DIR FRAME");
                let frame: u32 = args.next().expect("FRAME").parse().unwrap();
                abstract_bench = Some((dir, frame));
            }
            "--reps" => reps = args.next().expect("value").parse().unwrap(),
            "--bench" => {
                bench_reps = Some(args.next().expect("--bench needs a value").parse().unwrap())
            }
            other => panic!("unknown argument {:?}", other),
        }
    }
    let num_frames = frames.unwrap_or(inputs.len() as u32);

    if let Some((dir, frame)) = from_checkpoint {
        run_census(&dir, frame, census_frames, max_lanes, emit_slots.as_deref());
        return;
    }

    if let Some(n) = abstract_frames {
        run_abstract(n);
        return;
    }

    if let Some((dir, frame)) = abstract_bench {
        run_abstract_bench(&dir, frame, reps);
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
    }
}

/// Install the panic hook that keeps SplitReq control-flow panics quiet.
fn install_split_hook() {
    let prev_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        if info.payload().downcast_ref::<runtime2::SplitReq>().is_none()
            && info.payload().downcast_ref::<runtime3::TileBail>().is_none()
        {
            prev_hook(info);
        }
    }));
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
    // CELESTE_TILE=1: concrete-button tiles (64 variants outside the
    // kernel). CELESTE_TILE=2: dynamic-expand tiles (one boundary row,
    // the input fan-out grows the lane axis IN-tile - trunk shared).
    let tile_mode: u8 = match std::env::var("CELESTE_TILE") {
        Ok(v) if v == "2" => 2,
        Ok(_) => 1,
        Err(_) => 0,
    };
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
    // Chunk cap: mid-frame width is ~64x the input width (the btn
    // fan-out), and per-op column traffic goes through DRAM once the
    // working set leaves cache. ~512 input lanes keep a varying
    // column's mid-frame buffer (~32k lanes x 16 B = 512 KB) L2-ish.
    // Cross-chunk dedup at the boundary makes chunking invisible
    // (batching invariance is the certified doctrine).
    const CHUNK: usize = 64;
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
                if part.width <= CHUNK {
                    pending.push(part);
                } else {
                    let n = part.width;
                    let mut at = 0;
                    while at < n {
                        let hi = (at + CHUNK).min(n);
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
    let queues: Vec<Vec<runtime2::Rt2>> = {
        let mut qs: Vec<Vec<runtime2::Rt2>> = (0..n_workers).map(|_| Vec::new()).collect();
        for (i, b) in pending.drain(..).enumerate() {
            qs[i % n_workers].push(b);
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
                    while let Some(block) = local.pop() {
                        if tile_mode > 0 {
                            let out = match tile_mode {
                                2 => run_chunk_dynexp(&block, ids),
                                _ => run_chunk_tiled(&block, ids),
                            };
                            if let Some(out) = out {
                                done.push(out);
                                continue;
                            }
                            // bail: fall through to the reference engine
                        }
                        let snapshot = block.clone_block();
                        let mut sub = block;
                        sub.begin_frame();
                        let result =
                            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                gen::call_fn(&mut sub, gen::FN_FRAME, &[], &[]);
                                sub.boundary(ids);
                            }));
                        match result {
                            Ok(()) => done.push(sub),
                            Err(payload) => {
                                match payload.downcast::<runtime2::SplitReq>() {
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
                                        local.push(a);
                                        local.push(b);
                                    }
                                    Err(other) => std::panic::resume_unwind(other),
                                }
                            }
                        }
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
    let mut groups: Vec<(u64, Vec<runtime2::Rt2>)> = Vec::new();
    for (mut sub, keep) in ran.into_iter().zip(keeps) {
        sub.retain_lanes(&keep);
        if sub.width == 0 {
            continue;
        }
        match groups.iter_mut().find(|(h, _)| *h == sub.shape_hash) {
            Some((_, g)) => g.push(sub),
            None => groups.push((sub.shape_hash, vec![sub])),
        }
    }
    phase("retain");
    let out: Vec<runtime2::Rt2> = groups
        .into_iter()
        .map(|(_, g)| runtime2::Rt2::merge_many(g))
        .collect();
    phase("merge");
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
    let rt = build_rt();
    let rt2 = runtime2::Rt2::from_scalar(&rt);
    let ids = boundary_ids();
    let g_freeze = gen::global_id("freeze").expect("no freeze global");
    install_split_hook();
    let mut blocks: Vec<runtime2::Rt2> = vec![rt2];
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
    print_bails();
    print_gate_rejects();
    if !census_total.is_empty() {
        let mut rows: Vec<_> = census_total.into_iter().collect();
        rows.sort_by_key(|(_, (ns, _, _))| std::cmp::Reverse(*ns));
        println!("op census (name, total ms, calls):");
        for (name, (ns, calls, _)) in rows {
            println!("  {:14} {:9.1} ms  {:>12} calls", name, ns as f64 / 1e6, calls);
        }
    }
}

/// The dev-loop benchmark (plans/columnar-engine.md): ONE abstract frame
/// forward from REAL boundary states of an existing checkpoint dir, with
/// the interpreter's own next-frame lane count as a built-in oracle.
///
///   native-probe --abstract-bench ~/celeste-checkpoints/room10-newlua-bench 35 --reps 10
fn run_abstract_bench(dir: &str, frame: u32, reps: u32) {
    let rt = build_rt(); // cart + collision cache (the __init run is incidental)
    let ids = boundary_ids();
    let g_freeze = gen::global_id("freeze").expect("no freeze global");
    install_split_hook();

    let t_load = std::time::Instant::now();
    let states = load_states_any(dir, frame);
    let blocks: Vec<runtime2::Rt2> = states
        .iter()
        .map(|st| import::import_block(st, rt.cart.clone(), rt.cache.clone()))
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
                let mut b = import::import_block(st, rt.cart.clone(), rt.cache.clone());
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
                            import::import_block(st, rt.cart.clone(), rt.cache.clone());
                        b.boundary(&ids);
                        format!("{:#x} w{}", b.shape_hash, b.width)
                    })
                    .collect();
                eprintln!("  engine shapes: {}", eng_shapes.join(", "));
                eprintln!("  ref shapes:    {}", ref_shapes.join(", "));
                // Structural diff of the first block on each side: the
                // Obj field sets, by name.
                let dump_objs = |b: &runtime2::Rt2, tag: &str| {
                    for (ci, cell) in b.structure.iter().enumerate() {
                        if let runtime2::Cell2::Obj(fields) = cell {
                            let names: Vec<&str> = fields
                                .iter()
                                .map(|(f, _)| gen::FIELD_NAMES[*f as usize])
                                .collect();
                            eprintln!("  {} cell {}: Obj[{}]", tag, ci, names.join(","));
                        }
                    }
                };
                if let Some(b) = chase.first() {
                    dump_objs(b, "engine");
                }
                if let Some(st) = load_states_any(dir, frame + k).first() {
                    let mut b = import::import_block(st, rt.cart.clone(), rt.cache.clone());
                    b.boundary(&ids);
                    dump_objs(&b, "ref");
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
    print_bails();
    print_gate_rejects();
    runtime3::TILE_CENSUS.print();
}

/// Tile-mode chunk executor (plans/columnar-engine.md "Rt3"): split the
/// chunk into TILE-lane tiles; per tile, loop the 64 input variants with
/// concrete buttons and the counter-replay tape for straddles; raw-concat
/// every pass's surviving lanes (structures must agree - checked) and run
/// ONE boundary per chunk. Returns None if any tile bails or structures
/// diverge (the caller reruns the whole chunk on the reference engine).
fn run_chunk_tiled(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
) -> Option<runtime2::Rt2> {
    // The slot binding (and the zero-divergence premise behind the tile
    // kernel) is scoped to the census SHAPE. Off-shape blocks (spawn,
    // death, other rooms) run the reference engine.
    if gen::N_SLOTS > 0 && chunk.shape_hash != gen::SLOT_SHAPE {
        record_gate_reject(chunk.shape_hash);
        return None;
    }
    let g_btn = gen::global_id("__button_states").expect("no __button_states");
    let mut acc: Option<(runtime2::Rt2, u64)> = None;
    let mut at = 0usize;
    while at < chunk.width {
        let hi = (at + runtime3::TILE).min(chunk.width);
        let template: runtime3::Rt3<0> = runtime3::Rt3::from_rt2(chunk, at, hi);
        for byte in 0u8..64 {
            if !run_tile_variant_dyn(&template, g_btn, byte, &mut acc) {
                return None;
            }
        }
        at = hi;
    }
    let (mut acc, _) = acc?;
    acc.boundary(ids);
    Some(acc)
}

/// Dynamic-expand chunk executor (CELESTE_TILE=2): ONE boundary row per
/// tile; the lane axis carries the input fan-out - `expand` doubles it
/// in-tile (1 -> 64), so everything before the first button read (the
/// obj.move physics, spikes, collision) runs ONCE per row as uniform
/// ops, shared by all 64 input variants. Straddle splits still use the
/// counter-replay tape. No set_buttons, no per-variant monomorphization.
fn run_chunk_dynexp(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
) -> Option<runtime2::Rt2> {
    // Slot binding is shape-scoped: deopt off-shape (see run_chunk_tiled).
    if gen::N_SLOTS > 0 && chunk.shape_hash != gen::SLOT_SHAPE {
        record_gate_reject(chunk.shape_hash);
        return None;
    }
    let mut acc: Option<(runtime2::Rt2, u64)> = None;
    // ONE template per chunk (the only structure deep-clone); per row
    // the working tile is reset via the undo log and refilled with the
    // row's column values - no allocation, capacities retained.
    let template: runtime3::Rt3<0xFF> = runtime3::Rt3::from_rt2(chunk, 0, 1);
    let mut rt3 = template.clone();
    for row in 0..chunk.width {
        let mut tape: Vec<u8> = Vec::new();
        loop {
            rt3.reset_from(&template);
            rt3.load_row(chunk, row);
            rt3.tape.clear();
            rt3.tape.extend_from_slice(&tape);
            rt3.begin_pass();
            rt3.bind_slots();
            let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                gen::call_fn(&mut rt3, gen::FN_FRAME, &[], &[]);
            }));
            match ok {
                Ok(_) => {
                    rt3.writeback_slots();
                    let fp = rt3.structure_fp();
                    match &mut acc {
                        None => {
                            let mut seed = rt3.seed_rt2();
                            rt3.append_into(&mut seed);
                            acc = Some((seed, fp));
                        }
                        Some((seed, afp)) => {
                            if *afp != fp {
                                return None;
                            }
                            rt3.append_into(seed);
                        }
                    }
                    if !rt3.advance_tape() {
                        break;
                    }
                    tape = rt3.tape.clone();
                }
                Err(payload) => match payload.downcast_ref::<runtime3::TileBail>() {
                    Some(b) => {
                        record_bail(b.0);
                        return None;
                    }
                    None => std::panic::resume_unwind(payload),
                },
            }
        }
    }
    let (mut acc, _) = acc?;
    acc.boundary(ids);
    Some(acc)
}

/// Off-shape gate rejections: count + the distinct shapes seen (the
/// diagnostic for "the tile path silently stopped running").
static GATE_REJECTS: std::sync::Mutex<Option<std::collections::HashMap<u64, u64>>> =
    std::sync::Mutex::new(None);

fn record_gate_reject(shape: u64) {
    *GATE_REJECTS
        .lock()
        .unwrap()
        .get_or_insert_with(Default::default)
        .entry(shape)
        .or_insert(0) += 1;
}

fn print_gate_rejects() {
    if let Some(map) = GATE_REJECTS.lock().unwrap().take() {
        let total: u64 = map.values().sum();
        let shapes: Vec<String> = map.iter().map(|(s, n)| format!("{:#018x} x{}", s, n)).collect();
        println!(
            "tile shape-gate rejected {} chunks (SLOT_SHAPE {:#018x}): {}",
            total,
            gen::SLOT_SHAPE,
            shapes.join(", ")
        );
    }
}

/// Aggregate bail sites (printed by the bench when nonempty).
static BAILS: std::sync::Mutex<Option<std::collections::HashMap<String, u64>>> =
    std::sync::Mutex::new(None);

fn record_bail(loc: &std::panic::Location) {
    let mut b = BAILS.lock().unwrap();
    *b.get_or_insert_with(Default::default)
        .entry(format!("{}:{}", loc.file(), loc.line()))
        .or_insert(0) += 1;
}

fn print_bails() {
    if let Some(map) = BAILS.lock().unwrap().take() {
        let mut rows: Vec<_> = map.into_iter().collect();
        rows.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        println!("tile bails by site:");
        for (site, n) in rows.iter().take(10) {
            println!("  {:>10}  {}", n, site);
        }
    }
}

/// Monomorphic dispatch over the JUMP and DASH bits (4 variants): the
/// two buttons that gate the largest code regions fold to compile-time
/// constants; the direction buttons stay concrete runtime data (their
/// expand sites see a concrete Bool - still exact, just unfolded).
/// The full 64-way monomorphization was measured to explode compile
/// time (>10 min); revisit with a separate codegen crate if the
/// 4-variant win says it is worth it.
fn run_tile_variant_dyn(
    template: &runtime3::Rt3<0>,
    g_btn: u32,
    byte: u8,
    acc: &mut Option<(runtime2::Rt2, u64)>,
) -> bool {
    match byte & 0x30 {
        0x00 => run_tile_variant::<0x00>(template, g_btn, byte, acc),
        0x10 => run_tile_variant::<0x10>(template, g_btn, byte, acc),
        0x20 => run_tile_variant::<0x20>(template, g_btn, byte, acc),
        0x30 => run_tile_variant::<0x30>(template, g_btn, byte, acc),
        _ => unreachable!(),
    }
}

fn run_tile_variant<const B: u8>(
    template: &runtime3::Rt3<0>,
    g_btn: u32,
    byte: u8,
    acc: &mut Option<(runtime2::Rt2, u64)>,
) -> bool {
    let mut tape: Vec<u8> = Vec::new();
    loop {
        let mut rt3: runtime3::Rt3<B> = template.clone().into_variant::<B>();
        rt3.set_buttons(g_btn, byte);
        rt3.tape = tape.clone();
        rt3.begin_pass();
        rt3.bind_slots();
        let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            gen::call_fn(&mut rt3, gen::FN_FRAME, &[], &[]);
        }));
        match ok {
            Ok(_) => {
                rt3.writeback_slots();
                let fp = rt3.structure_fp();
                match acc {
                    None => {
                        let mut seed = rt3.seed_rt2();
                        rt3.append_into(&mut seed);
                        *acc = Some((seed, fp));
                    }
                    Some((seed, afp)) => {
                        if *afp != fp {
                            return false;
                        }
                        rt3.append_into(seed);
                    }
                }
                if !rt3.advance_tape() {
                    return true;
                }
                tape = rt3.tape.clone();
            }
            Err(payload) => match payload.downcast_ref::<runtime3::TileBail>() {
                Some(b) => {
                    record_bail(b.0);
                    return false;
                }
                None => std::panic::resume_unwind(payload),
            },
        }
    }
}
