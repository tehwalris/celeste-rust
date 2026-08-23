//! Single-lane concrete execution.
//!
//! Every tool that replays a specific input sequence (`concrete_run`,
//! `measure_k`, the TAS walks in the `rewrite` binary) goes through this
//! plumbing. The concrete path runs the same interpreter as the abstract
//! search - there is no separate "simple" interpreter to drift out of
//! sync - and the same program assembly (`program::Program`), so
//! a tool can never end up with a franken-room or a frame chunk that
//! forgets the button-state reset (both happened when each binary
//! hand-assembled its own copy).

use anyhow::{anyhow, Result};

use crate::interpreter::fixed_env::{FixedEnv, PreparedCfg};
use crate::interpreter::glue::{interpret_cfg, interpret_prepared_cfg};
use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};
use crate::program::Program;

/// Button flags of an input byte:
/// bit 0 = left, 1 = right, 2 = up, 3 = down, 4 = jump, 5 = dash.
pub fn decode_input(byte: u8) -> [bool; 6] {
    std::array::from_fn(|i| byte >> i & 1 == 1)
}

/// Human-readable input byte, e.g. "R+J", or "-" for none.
pub fn format_input(byte: u8) -> String {
    const NAMES: [&str; 6] = ["L", "R", "U", "D", "J", "X"];
    let buttons = decode_input(byte);
    let pressed: Vec<&str> = NAMES
        .iter()
        .enumerate()
        .filter(|(i, _)| buttons[*i])
        .map(|(_, name)| *name)
        .collect();
    if pressed.is_empty() {
        "-".to_string()
    } else {
        pressed.join("+")
    }
}

/// Overwrite the six concrete button cells of a plain-program state from an
/// input byte.
pub fn set_concrete_buttons(state: &mut State, byte: u8) -> Result<()> {
    let cell = *state
        .global_env
        .get("__button_states")
        .ok_or_else(|| anyhow!("no __button_states"))?;
    let arr = match state.heap.get_opt(cell) {
        Some(HeapValue::Value(Value::Pointer(id))) => *id,
        Some(HeapValue::ArrayTable(_)) => cell,
        other => return Err(anyhow!("__button_states shape: {:?}", other)),
    };
    let items = match state.heap.get_opt(arr) {
        Some(HeapValue::ArrayTable(items)) => items.clone(),
        other => return Err(anyhow!("button array shape: {:?}", other)),
    };
    for (i, item) in items.iter().enumerate() {
        let pressed = byte >> i & 1 == 1;
        let target = match state.heap.get_opt(*item) {
            Some(HeapValue::Value(Value::Pointer(id))) => *id,
            _ => *item,
        };
        state
            .heap
            .set(target, HeapValue::Value(Value::Bool(MaybeVector::Scalar(pressed))));
    }
    Ok(())
}

/// Run the program's init chunk to the single pre-frame-1 concrete state
/// (the canonical spawn state), with the native `tile_flag_at` injected -
/// exactly what the abstract search starts from.
pub fn initial_state(program: &Program, fixed_env: &FixedEnv) -> Result<State> {
    let initial = crate::game_runner::create_initial_state_with_builtins(fixed_env);
    let init_states = interpret_cfg(program.init_cfg().clone(), initial, fixed_env)?;
    let mut states: Vec<_> = init_states.into_iter().map(|(s, _)| s).collect();
    for s in &mut states {
        crate::game_runner::inject_tile_flag_at_builtin(s);
    }
    if states.len() != 1 {
        return Err(anyhow!("init produced {} states", states.len()));
    }
    Ok(states.pop().unwrap())
}

/// One concrete frame: set the buttons, run the frame chunk (which ends
/// with the button-state reset), and require the run to stay single-lane.
pub fn step_frame(
    frame_cfg: &PreparedCfg,
    state: State,
    fixed_env: &FixedEnv,
    byte: u8,
) -> Result<State> {
    let mut state = state;
    set_concrete_buttons(&mut state, byte)?;
    let result = interpret_prepared_cfg(frame_cfg, state, fixed_env)?;
    if result.len() != 1 {
        return Err(anyhow!("frame branched into {} states", result.len()));
    }
    Ok(result.into_iter().next().unwrap().0)
}
