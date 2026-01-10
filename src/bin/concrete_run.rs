//! Concrete interpreter - runs the game with specific input sequences.
//!
//! This is for debugging and validation: run a known TAS input sequence
//! and observe the resulting player positions.

use anyhow::Result;
use celeste_rust::frontend;
use celeste_rust::game_runner::{create_fixed_env_with_game_builtins, create_initial_state_with_builtins};
use celeste_rust::interpreter::glue::interpret_cfg;
use celeste_rust::interpreter::inspect::StateHelper;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
use celeste_rust::pico8_num::Pico8Num;
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

    /// Starting frame offset (for player_spawn phase, typically 24)
    #[arg(long, default_value_t = 0)]
    start_frame: u32,
}

fn decode_input(input_byte: u8) -> [bool; 6] {
    [
        (input_byte & 1) != 0,   // left
        (input_byte & 2) != 0,   // right
        (input_byte & 4) != 0,   // up
        (input_byte & 8) != 0,   // down
        (input_byte & 16) != 0,  // jump
        (input_byte & 32) != 0,  // dash
    ]
}

fn format_input(input_byte: u8) -> String {
    let buttons = decode_input(input_byte);
    let names = ["L", "R", "U", "D", "J", "X"];
    let pressed: Vec<&str> = buttons.iter()
        .zip(names.iter())
        .filter(|(pressed, _)| **pressed)
        .map(|(_, name)| *name)
        .collect();
    if pressed.is_empty() {
        "-".to_string()
    } else {
        pressed.join("+")
    }
}

/// Set button states to concrete values in the state
fn set_concrete_buttons(state: &mut State, input_byte: u8) {
    let buttons = decode_input(input_byte);

    // Find __button_states global (may be a pointer to the array)
    let button_states_global_id = state.global_env.get("__button_states")
        .expect("__button_states not found in global env");

    // Dereference if it's a pointer
    let button_states_id = match state.heap.get(*button_states_global_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        HeapValue::ArrayTable(_) => *button_states_global_id,
        other => panic!("__button_states global is unexpected type: {:?}", other),
    };

    // Get the array
    let array_items = match state.heap.get(button_states_id) {
        HeapValue::ArrayTable(items) => items.clone(),
        other => panic!("__button_states is not an array table, got: {:?}", other),
    };

    // Each item in the array is a pointer to a value
    for (i, &pressed) in buttons.iter().enumerate() {
        let item_ptr_id = array_items[i];
        // The array element might be a pointer to the actual value, or the value itself
        let value_id = match state.heap.get(item_ptr_id) {
            HeapValue::Value(Value::Pointer(id)) => *id,
            HeapValue::Value(_) => item_ptr_id,
            _ => item_ptr_id,
        };
        state.heap.set(value_id, HeapValue::Value(Value::Bool(MaybeVector::Scalar(pressed))));
    }
}

/// Extract player position from state
fn get_player_position(state: &State) -> Option<(Pico8Num, Pico8Num)> {
    let helper = StateHelper::new(state);

    // Get objects array
    let objects_id = helper.find_global("objects")?;
    let objects_array_id = match helper.load(objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };

    // Find player object
    let players = helper.find_objects_by_type(objects_array_id, "player").ok()?;
    let player_id = players.first()?;

    // Get x and y
    let player = match helper.load(*player_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    let x = player.get("x").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    let y = player.get("y").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    Some((x, y))
}

/// Extract player_spawn position from state
fn get_player_spawn_position(state: &State) -> Option<(Pico8Num, Pico8Num)> {
    let helper = StateHelper::new(state);

    let objects_id = helper.find_global("objects")?;
    let objects_array_id = match helper.load(objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };

    let spawns = helper.find_objects_by_type(objects_array_id, "player_spawn").ok()?;
    let spawn_id = spawns.first()?;

    let spawn = match helper.load(*spawn_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    let x = spawn.get("x").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    let y = spawn.get("y").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    Some((x, y))
}

/// Get player speed
fn get_player_speed(state: &State) -> Option<(Pico8Num, Pico8Num)> {
    let helper = StateHelper::new(state);

    let objects_id = helper.find_global("objects")?;
    let objects_array_id = match helper.load(objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };

    let players = helper.find_objects_by_type(objects_array_id, "player").ok()?;
    let player_id = players.first()?;

    let player = match helper.load(*player_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    // Get spd.x and spd.y
    let spd_ptr = player.get("spd")?;
    let spd_id = match helper.load(*spd_ptr) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };
    let spd = match helper.load(spd_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    let spd_x = spd.get("x").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    let spd_y = spd.get("y").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    Some((spd_x, spd_y))
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

/// Get player rem (subpixel position)
fn get_player_rem(state: &State) -> Option<(Pico8Num, Pico8Num)> {
    let helper = StateHelper::new(state);

    let objects_id = helper.find_global("objects")?;
    let objects_array_id = match helper.load(objects_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };

    let players = helper.find_objects_by_type(objects_array_id, "player").ok()?;
    let player_id = players.first()?;

    let player = match helper.load(*player_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    // Get rem.x and rem.y
    let rem_ptr = player.get("rem")?;
    let rem_id = match helper.load(*rem_ptr) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        _ => return None,
    };
    let rem = match helper.load(rem_id) {
        HeapValue::ObjectTable(obj) => obj,
        _ => return None,
    };

    let rem_x = rem.get("x").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    let rem_y = rem.get("y").and_then(|id| match helper.load(*id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(*n),
        _ => None,
    })?;

    Some((rem_x, rem_y))
}

fn format_num(n: Pico8Num) -> String {
    let whole = n.whole_part_as_i16();
    let frac = n.fraction_part_as_u16();
    if frac == 0 {
        format!("{}", whole)
    } else {
        format!("{}.{:04x}", whole, frac)
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Parse inputs
    let inputs: Vec<u8> = cli.inputs
        .split(',')
        .map(|s| s.trim().parse::<u8>().expect("Invalid input byte"))
        .collect();

    let num_frames = cli.frames.unwrap_or(inputs.len() as u32);

    println!("Running {} frames with {} inputs", num_frames, inputs.len());
    println!("Input sequence: {:?}", inputs);
    println!("Decoded: {}", inputs.iter().map(|&i| format_input(i)).collect::<Vec<_>>().join(", "));
    println!();

    // Load and compile the game
    let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")?;
    let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = std::fs::read_to_string("lua/celeste-minimal.lua")?;

    // For concrete execution, we don't call __reset_button_states after init
    // We'll set buttons manually before each frame
    let init_suffix = r#"
_init()
__reset_button_states()
"#;

    let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, init_suffix);

    let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
    let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

    let mut fixed_env = create_fixed_env_with_game_builtins();
    for fun_def in fun_defs {
        fixed_env.add_fun_def(fun_def);
    }

    let initial_state = create_initial_state_with_builtins(&fixed_env);

    println!("Running game init...");
    let init_result = interpret_cfg(cfg, initial_state, &fixed_env)
        .expect("Init interpretation failed");

    // Should be exactly one state for concrete execution
    assert_eq!(init_result.len(), 1, "Expected exactly one state after init (got {})", init_result.len());
    let mut state = init_result.into_iter().next().unwrap().0;

    println!("Init complete.\n");

    // Print initial state
    if let Some((x, y)) = get_player_spawn_position(&state) {
        println!("Frame 0: player_spawn at ({}, {})", format_num(x), format_num(y));
    } else if let Some((x, y)) = get_player_position(&state) {
        println!("Frame 0: player at ({}, {})", format_num(x), format_num(y));
    }

    // Compile frame code (without button reset - we'll do it manually)
    let frame_code = r#"
_update()
_draw()
"#;
    let frame_ast = full_moon::parse(frame_code).expect("Failed to parse frame code");
    let (frame_cfg, frame_fun_defs) = frontend::compile(&frame_ast).expect("Failed to compile frame");
    assert!(frame_fun_defs.is_empty());

    // Run frames
    for frame_num in 1..=num_frames {
        // Get input for this frame (0 if past the input sequence)
        let input_byte = if (frame_num as usize) <= inputs.len() {
            inputs[(frame_num as usize) - 1]
        } else {
            0
        };

        // Set concrete button states
        set_concrete_buttons(&mut state, input_byte);

        // Run frame
        let result = interpret_cfg(frame_cfg.clone(), state, &fixed_env)
            .expect("Frame interpretation failed");

        // Should be exactly one state for concrete execution
        assert_eq!(result.len(), 1, "Frame {}: Expected 1 state, got {} (branching occurred!)", frame_num, result.len());
        state = result.into_iter().next().unwrap().0;

        // Reset button states to prepare for next manual setting
        // (we need the array to exist but values don't matter since we'll overwrite)
        let reset_code = "__reset_button_states()";
        let reset_ast = full_moon::parse(reset_code).expect("Failed to parse reset");
        let (reset_cfg, _) = frontend::compile(&reset_ast).expect("Failed to compile reset");
        let reset_result = interpret_cfg(reset_cfg, state, &fixed_env)
            .expect("Reset interpretation failed");
        assert_eq!(reset_result.len(), 1);
        state = reset_result.into_iter().next().unwrap().0;

        // Print state
        let freeze = get_freeze(&state).map(format_num).unwrap_or("?".to_string());

        if let Some((x, y)) = get_player_position(&state) {
            let (spd_x, spd_y) = get_player_speed(&state).unwrap_or((Pico8Num::from_i16(0), Pico8Num::from_i16(0)));
            let rem_str = if let Some((rem_x, rem_y)) = get_player_rem(&state) {
                format!(" rem=({}, {})", format_num(rem_x), format_num(rem_y))
            } else {
                String::new()
            };
            println!("Frame {}: player at ({}, {}) spd=({}, {}){} freeze={} input={}",
                frame_num,
                format_num(x), format_num(y),
                format_num(spd_x), format_num(spd_y),
                rem_str,
                freeze,
                format_input(input_byte));
        } else if let Some((x, y)) = get_player_spawn_position(&state) {
            println!("Frame {}: player_spawn at ({}, {}) freeze={} input={}",
                frame_num,
                format_num(x), format_num(y),
                freeze,
                format_input(input_byte));
        } else {
            println!("Frame {}: no player/player_spawn found, freeze={} input={}",
                frame_num, freeze, format_input(input_byte));
        }
    }

    Ok(())
}
