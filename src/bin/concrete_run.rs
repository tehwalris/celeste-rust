//! Runs the game (the reference engine) with a fixed input sequence and
//! prints the player per frame: the basis for checking a witness against a
//! real PICO-8 (`pico8_diff/replay.py`, same frame convention).

use anyhow::Result;
use celeste_engine::runtime2::{Rt2, AV, NONE};
use celeste_rust::concrete::{format_input, num_at, objects_of_type};
use celeste_rust::pico8_num::Pico8Num;
use celeste_rust::trace::refengine::RefEngine;
use clap::Parser;

#[derive(Parser)]
#[command(name = "concrete_run")]
#[command(about = "Run game with specific input sequence")]
struct Cli {
    /// Comma-separated input bytes (e.g., "52,0,0,0,0,0")
    /// Each byte encodes buttons: bit0=left, bit1=right, bit2=up, bit3=down, bit4=jump, bit5=dash
    #[arg(short, long)]
    inputs: String,

    /// Number of frames to run (default: length of inputs)
    #[arg(short, long)]
    frames: Option<u32>,

    /// Also print the first object of this type's number fields each frame
    /// (e.g. `fly_fruit`: its `step`, `y`, `spd`, `rem`).
    #[arg(long)]
    object: Option<String>,
}

/// CAREFUL when parsing this output: the format is floor.frac_hex, NOT
/// sign-magnitude. For negative numbers the whole part is the FLOOR and the
/// fraction is the positive offset above it: `-4.76ec` means
/// (-4 << 16) | 0x76ec = -0x3.8914 (-3.5355), not -(4 + 0x76ec/65536).
fn format_num(n: Pico8Num) -> String {
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

fn xy(st: &Rt2, obj: u32, field: &str) -> Option<(Pico8Num, Pico8Num)> {
    Some((num_at(st, obj, &[field, "x"])?, num_at(st, obj, &[field, "y"])?))
}

fn position(st: &Rt2, obj: u32) -> Option<(Pico8Num, Pico8Num)> {
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

fn freeze(st: &Rt2) -> Option<Pico8Num> {
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
        let (spd_x, spd_y) = xy(st, player, "spd").unwrap_or((Pico8Num::from_i16(0), Pico8Num::from_i16(0)));
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

fn main() -> Result<()> {
    let cli = Cli::parse();

    let inputs: Vec<u8> = cli
        .inputs
        .split(',')
        .map(|s| s.trim().parse::<u8>().expect("Invalid input byte"))
        .collect();

    let num_frames = cli.frames.unwrap_or(inputs.len() as u32);

    println!("Running {} frames with {} inputs", num_frames, inputs.len());
    println!("Input sequence: {:?}", inputs);
    println!("Decoded: {}", inputs.iter().map(|&i| format_input(i)).collect::<Vec<_>>().join(", "));
    println!();

    println!("Running game init...");
    let mut eng = RefEngine::new()?;
    let mut state = eng.initial()?;
    println!("Init complete.\n");

    if let Some((x, y)) = first_object(&state, "player_spawn").and_then(|p| position(&state, p)) {
        println!("Frame 0: player_spawn at ({}, {})", format_num(x), format_num(y));
    } else if let Some((x, y)) = first_object(&state, "player").and_then(|p| position(&state, p)) {
        println!("Frame 0: player at ({}, {})", format_num(x), format_num(y));
    }

    for frame_num in 1..=num_frames {
        // Input for this frame (0 if past the input sequence)
        let input_byte = inputs.get(frame_num as usize - 1).copied().unwrap_or(0);
        state = eng.step_one(&state, input_byte)?.into_rt2();
        print_frame(&state, frame_num, input_byte);
        if let Some(t) = &cli.object {
            print_object(&state, t);
        }
    }

    Ok(())
}
