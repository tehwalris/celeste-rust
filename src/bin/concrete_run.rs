//! Concrete interpreter - runs the game with specific input sequences.
//!
//! This is for debugging and validation: run a known TAS input sequence
//! and observe the resulting player positions. The program and the
//! per-frame plumbing come from `program` / `concrete`, so this
//! runs exactly what the abstract search runs.

use anyhow::Result;
use celeste_rust::concrete;
use celeste_rust::interpreter::fixed_env::PreparedCfg;
use celeste_rust::interpreter::inspect::StateHelper;
use celeste_rust::interpreter::heap::HeapId;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
use celeste_rust::pico8_num::Pico8Num;
use celeste_rust::program::Program;
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
}

/// Read a scalar-number field of an object behind `player_id`.
fn object_number_field(helper: &StateHelper, object_id: HeapId, field: &str) -> Option<Pico8Num> {
    let object = match helper.load(object_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };
    object.get(field).and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })
}

/// Read an (x, y) pair off a sub-object field like `spd` or `rem`.
fn object_xy_field(
    helper: &StateHelper,
    object_id: HeapId,
    field: &str,
) -> Option<(Pico8Num, Pico8Num)> {
    let object = match helper.load(object_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };
    let sub_id = match helper.load(*object.get(field)?) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };
    let x = object_number_field(helper, sub_id, "x")?;
    let y = object_number_field(helper, sub_id, "y")?;
    Some((x, y))
}

/// The first object of `type_name` in the objects array, if any.
fn find_object(state: &State, type_name: &str) -> Option<HeapId> {
    let helper = StateHelper::new(state);
    let objects_id = helper.find_global("objects")?;
    let objects_array_id = match helper.load(objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };
    helper
        .find_objects_by_type(objects_array_id, type_name)
        .ok()?
        .first()
        .copied()
}

/// Get freeze global value
fn get_freeze(state: &State) -> Option<Pico8Num> {
    let helper = StateHelper::new(state);
    let freeze_id = helper.find_global("freeze")?;
    match helper.load(freeze_id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    }
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

fn print_frame(state: &State, frame_num: u32, input_byte: u8) {
    let helper = StateHelper::new(state);
    let freeze = get_freeze(state).map(format_num).unwrap_or("?".to_string());
    let position = |id| {
        Some((
            object_number_field(&helper, id, "x")?,
            object_number_field(&helper, id, "y")?,
        ))
    };
    if let Some((x, y)) = find_object(state, "player").and_then(&position) {
        let player_id = find_object(state, "player").unwrap();
        let (spd_x, spd_y) = object_xy_field(&helper, player_id, "spd")
            .unwrap_or((Pico8Num::from_i16(0), Pico8Num::from_i16(0)));
        let rem_str = match object_xy_field(&helper, player_id, "rem") {
            Some((rem_x, rem_y)) => {
                format!(" rem=({}, {})", format_num(rem_x), format_num(rem_y))
            }
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
            concrete::format_input(input_byte)
        );
    } else if let Some((x, y)) = find_object(state, "player_spawn").and_then(&position) {
        println!(
            "Frame {}: player_spawn at ({}, {}) freeze={} input={}",
            frame_num,
            format_num(x),
            format_num(y),
            freeze,
            concrete::format_input(input_byte)
        );
    } else {
        println!(
            "Frame {}: no player/player_spawn found, freeze={} input={}",
            frame_num,
            freeze,
            concrete::format_input(input_byte)
        );
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
    println!(
        "Decoded: {}",
        inputs
            .iter()
            .map(|&i| concrete::format_input(i))
            .collect::<Vec<_>>()
            .join(", ")
    );
    println!();

    let program = Program::compile_executable_from_disk()?;
    let fixed_env = program.fixed_env();
    let frame_cfg = PreparedCfg::new(program.frame_cfg().clone());

    println!("Running game init...");
    let mut state = concrete::initial_state(&program, &fixed_env)?;
    println!("Init complete.\n");

    // Print initial state
    let helper = StateHelper::new(&state);
    if let Some(id) = find_object(&state, "player_spawn") {
        if let (Some(x), Some(y)) = (
            object_number_field(&helper, id, "x"),
            object_number_field(&helper, id, "y"),
        ) {
            println!("Frame 0: player_spawn at ({}, {})", format_num(x), format_num(y));
        }
    } else if let Some(id) = find_object(&state, "player") {
        if let (Some(x), Some(y)) = (
            object_number_field(&helper, id, "x"),
            object_number_field(&helper, id, "y"),
        ) {
            println!("Frame 0: player at ({}, {})", format_num(x), format_num(y));
        }
    }
    drop(helper);

    for frame_num in 1..=num_frames {
        // Input for this frame (0 if past the input sequence)
        let input_byte = if (frame_num as usize) <= inputs.len() {
            inputs[(frame_num as usize) - 1]
        } else {
            0
        };
        state = concrete::step_frame(&frame_cfg, state, &fixed_env, input_byte)?;
        print_frame(&state, frame_num, input_byte);
    }

    Ok(())
}
