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
mod runtime;

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
    let cart = CartData::load(cart_base).expect("failed to load cart data");
    let cache =
        CollisionCache::new(&cart, room_x, room_y).expect("failed to create collision cache");
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

fn main() {
    let mut inputs: Vec<u8> = Vec::new();
    let mut frames: Option<u32> = None;
    let mut bench_reps: Option<u32> = None;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "-i" => {
                inputs = args
                    .next()
                    .expect("-i needs a value")
                    .split(',')
                    .map(|s| s.trim().parse().expect("invalid input byte"))
                    .collect()
            }
            "-f" => frames = Some(args.next().expect("-f needs a value").parse().unwrap()),
            "--bench" => {
                bench_reps = Some(args.next().expect("--bench needs a value").parse().unwrap())
            }
            other => panic!("unknown argument {:?}", other),
        }
    }
    let num_frames = frames.unwrap_or(inputs.len() as u32);

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
