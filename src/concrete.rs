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

use crate::interpreter::state::State;
use crate::interpreter::value::{HeapValue, MaybeVector, Value};

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

/// Single-lane concrete execution, backed by the REFERENCE interpreter
/// (`RefEngine`, the AST oracle) now that the CFG interpreter is gone. Hold
/// one across a walk: `RefEngine` owns the parsed cart and the registered
/// function bodies, so rebuilding per frame would re-parse the cart.
///
/// It runs the ORIGINAL Lua (equivalent to the plain program - proven by
/// `refgate`'s concrete walk), so it needs no `Program`; the layout it
/// produces is the plain program's.
pub struct ConcreteEngine {
    eng: crate::trace::refengine::RefEngine,
}

impl ConcreteEngine {
    pub fn new() -> Result<Self> {
        Ok(Self { eng: crate::trace::refengine::RefEngine::new()? })
    }

    /// The single pre-frame-1 concrete spawn state, with the native
    /// `tile_flag_at` injected - what the abstract search starts from.
    pub fn initial_state(&self) -> Result<State> {
        self.eng.initial_state()
    }

    /// One concrete frame: set the buttons and run `_update();_draw()`
    /// (no reset, no forking), requiring the run to stay single-lane.
    pub fn step_frame(&mut self, mut state: State, byte: u8) -> Result<State> {
        set_concrete_buttons(&mut state, byte)?;
        self.eng.run_frame_concrete(&state)
    }
}
