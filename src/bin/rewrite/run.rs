//! ONE CONCRETE RUN (`rewrite run`): the reference engine runs an input
//! sequence from the room's entry, the player printed per frame, or the
//! start state dumped as a real PICO-8's (`pico8_diff/replay.py --dump`).

use anyhow::Result;
use celeste_engine::runtime2::{Rt2, AV, NONE, P8};
use celeste_rust::concrete::{format_input, num_at, objects_of_type};
use celeste_rust::settings::Settings;
use celeste_rust::engines::reference::RefEngine;

#[derive(clap::Args)]
pub struct Args {
    /// Comma-separated input bytes (e.g. "52,0,0,0,0,0"), or a `tas/`
    /// file: bit 0 left, 1 right, 2 up, 3 down, 4 jump, 5 dash.
    #[arg(short, long)]
    inputs: String,
    /// Frames to run (default: the inputs'; past them, no input).
    #[arg(short, long)]
    frames: Option<u32>,
    /// Also print the first object of this type's number fields each
    /// frame (e.g. `fly_fruit`: its `step`, `y`, `spd`, `rem`).
    #[arg(long)]
    object: Option<String>,
    /// Print the start state (every object's fields, as
    /// `pico8_diff/replay.py --dump` prints them on a real PICO-8) and
    /// exit: the search's room entry, to compare field by field with a
    /// real chain's (`pico8_diff/chain.py --check-start`).
    #[arg(long)]
    dump_start: bool,
}

/// `run`: the inputs through the reference engine, the player printed per
/// frame.
pub fn run(cfg: &Settings, a: Args) -> Result<()> {
    let Args { inputs, frames, object, dump_start } = a;
    let mut eng = RefEngine::new(cfg)?;
    let mut state = eng.initial()?;
    if dump_start {
        dump_start_state(&state);
        return Ok(());
    }
    let inputs = celeste_rust::concrete::read_inputs(&inputs)?;
    let frames = frames.unwrap_or(inputs.len() as u32);
    println!("Running {frames} frames with {} inputs", inputs.len());
    println!("Decoded: {}", inputs.iter().map(|&i| format_input(i)).collect::<Vec<_>>().join(", "));
    if let Some((x, y)) = first_object(&state, "player_spawn").and_then(|p| position(&state, p)) {
        println!("Frame 0: player_spawn at ({}, {})", format_num(x), format_num(y));
    } else if let Some((x, y)) = first_object(&state, "player").and_then(|p| position(&state, p)) {
        println!("Frame 0: player at ({}, {})", format_num(x), format_num(y));
    }
    for f in 1..=frames {
        let byte = inputs.get(f as usize - 1).copied().unwrap_or(0);
        let mut out = eng.frame(&state, byte)?;
        anyhow::ensure!(out.len() == 1, "frame {f}: {} leaves (an `rnd` fork: give the balloons' seeds)", out.len());
        state = out.remove(0);
        print_frame(&state, f, byte);
        if let Some(t) = &object {
            print_object(&state, t);
        }
    }
    Ok(())
}

#[derive(clap::Args)]
pub struct WitnessArgs {
    /// The witness: comma-separated input bytes, or a `tas/` file, from the
    /// room's load.
    #[arg(long)]
    inputs: String,
}

/// `witness`: `concrete::witness` of the inputs, as JSON on stdout.
pub fn witness(cfg: &Settings, a: WitnessArgs) -> Result<()> {
    let inputs = celeste_rust::concrete::read_inputs(&a.inputs)?;
    println!("{}", serde_json::to_string(&celeste_rust::concrete::witness(&inputs, cfg)?)?);
    Ok(())
}

/// The object types `--dump-start` names (pico8_diff/replay.py TYPES).
const TYPES: &[&str] = &["player", "player_spawn", "spring", "balloon", "fall_floor", "smoke", "fruit", "fly_fruit", "lifeup", "fake_wall", "key", "chest", "platform", "message", "big_chest", "orb", "flag", "room_title"];

/// PICO-8's `tostr(v, true)`: the 16.16 bits as `0xwwww.ffff`.
fn hex(n: P8) -> String {
    let b = n.to_bits();
    format!("0x{:04x}.{:04x}", b >> 16, b & 0xffff)
}

/// One value as `replay.py`'s dump prints it; a table of numbers inline.
fn dump_value(st: &Rt2, c: u32) -> Option<String> {
    use celeste_engine::runtime2::Cell2;
    Some(match st.cols[c as usize].at(0) {
        AV::Num(n) => hex(n),
        AV::Bool(b) => b.to_string(),
        AV::Ptr(t) => {
            let Cell2::Obj(fields) = &st.structure[t as usize] else { return None };
            let mut s = String::new();
            for k in ["x", "y", "w", "h"] {
                if let Some(&(_, fc)) = fields.iter().find(|(f, _)| Some(*f) == celeste_names::field_id(k)) {
                    match st.cols[fc as usize].at(0) {
                        AV::Num(n) => s += &format!("{k}={},", hex(n)),
                        AV::Bool(v) => s += &format!("{k}={v},"),
                        _ => {}
                    }
                }
            }
            format!("{{{s}}}")
        }
        _ => return None,
    })
}

/// `obj I TYPE k=v ...` per object in `objects` order, keys sorted.
fn dump_start_state(st: &Rt2) {
    use celeste_engine::runtime2::Cell2;
    let mut names = std::collections::HashMap::new();
    for t in TYPES {
        for o in objects_of_type(st, t) {
            names.insert(o, *t);
        }
    }
    let Some(arr) = celeste_names::global_id("objects").and_then(|g| st.global_target(g)) else { return };
    let Cell2::Arr(items) = &st.structure[arr as usize] else { return };
    for (i, &item) in items.iter().enumerate() {
        let AV::Ptr(o) = st.cols[item as usize].at(0) else { continue };
        let Cell2::Obj(fields) = &st.structure[o as usize] else { continue };
        let mut kv: Vec<(String, String)> = fields
            .iter()
            .filter_map(|&(f, c)| {
                let k = celeste_names::FIELD_NAMES[f as usize];
                (k != "type" && k != "hair").then(|| Some((k.to_string(), dump_value(st, c)?))).flatten()
            })
            .collect();
        kv.sort();
        let kv: Vec<String> = kv.into_iter().map(|(k, v)| format!("{k}={v}")).collect();
        println!("obj {} {} {}", i + 1, names.get(&o).unwrap_or(&"?"), kv.join(" "));
    }
}

/// Formats as floor.frac_hex, NOT sign-magnitude: `-4.76ec` is
/// (-4 << 16) | 0x76ec = -3.5355, not -(4 + 0x76ec/65536).
fn format_num(n: P8) -> String {
    let whole = n.whole_part_as_i16();
    let frac = n.fraction_part_as_u16();
    if frac == 0 {
        format!("{}", whole)
    } else {
        format!("{}.{:04x}", whole, frac)
    }
}

fn first_object(st: &Rt2, type_name: &str) -> Option<u32> {
    objects_of_type(st, type_name).first().copied()
}

fn xy(st: &Rt2, obj: u32, field: &str) -> Option<(P8, P8)> {
    Some((num_at(st, obj, &[field, "x"])?, num_at(st, obj, &[field, "y"])?))
}

fn position(st: &Rt2, obj: u32) -> Option<(P8, P8)> {
    Some((num_at(st, obj, &["x"])?, num_at(st, obj, &["y"])?))
}

/// One object's number fields for `--object`.
fn print_object(st: &Rt2, type_name: &str) {
    let Some(id) = first_object(st, type_name) else {
        return println!("  {type_name}: none");
    };
    let mut parts = Vec::new();
    for f in ["x", "y", "step", "start", "off", "state", "delay"] {
        if let Some(n) = num_at(st, id, &[f]) {
            parts.push(format!("{f}={}", format_num(n)));
        }
    }
    for f in ["spd", "rem"] {
        if let Some((x, y)) = xy(st, id, f) {
            parts.push(format!("{f}=({}, {})", format_num(x), format_num(y)));
        }
    }
    println!("  {type_name}: {}", parts.join(" "));
}

fn freeze(st: &Rt2) -> Option<P8> {
    let cell = st.globals[celeste_names::global_id("freeze")? as usize];
    match (cell != NONE).then(|| st.cols[cell as usize].at(0))? {
        AV::Num(n) => Some(n),
        _ => None,
    }
}

fn print_frame(st: &Rt2, frame_num: u32, input_byte: u8) {
    let freeze = freeze(st).map(format_num).unwrap_or("?".to_string());
    let input = format_input(input_byte);
    if let Some((player, (x, y))) = first_object(st, "player").and_then(|p| Some((p, position(st, p)?))) {
        let (spd_x, spd_y) = xy(st, player, "spd").unwrap_or((P8::from_i16(0), P8::from_i16(0)));
        let rem = match xy(st, player, "rem") {
            Some((rx, ry)) => format!(" rem=({}, {})", format_num(rx), format_num(ry)),
            None => String::new(),
        };
        println!(
            "Frame {frame_num}: player at ({}, {}) spd=({}, {}){rem} freeze={freeze} input={input}",
            format_num(x),
            format_num(y),
            format_num(spd_x),
            format_num(spd_y),
        );
    } else if let Some((x, y)) = first_object(st, "player_spawn").and_then(|p| position(st, p)) {
        println!("Frame {frame_num}: player_spawn at ({}, {}) freeze={freeze} input={input}", format_num(x), format_num(y));
    } else {
        println!("Frame {frame_num}: no player/player_spawn found, freeze={freeze} input={input}");
    }
}
