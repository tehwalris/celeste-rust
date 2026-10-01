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
/// It runs the ORIGINAL Lua, so the layout it produces is the plain
/// program's.
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

/// Copy the `__button_states` array of `from` over `to`'s: after a
/// concrete frame the successor carries the frame's buttons, and the
/// search's states must agree on them (the initial state's).
pub fn restore_buttons(
    from: &State,
    to: &mut State,
) -> Result<()> {
    use crate::interpreter::value::{HeapValue, Value};
    let arr_of = |st: &State| -> Result<Vec<_>> {
        let cell = *st
            .global_env
            .get("__button_states")
            .ok_or_else(|| anyhow::anyhow!("no __button_states"))?;
        let arr = match st.heap.get_opt(cell) {
            Some(HeapValue::Value(Value::Pointer(id))) => *id,
            _ => cell,
        };
        let items = match st.heap.get_opt(arr) {
            Some(HeapValue::ArrayTable(items)) => items.clone(),
            other => anyhow::bail!("button array shape: {:?}", other),
        };
        Ok(items
            .iter()
            .map(|item| match st.heap.get_opt(*item) {
                Some(HeapValue::Value(Value::Pointer(id))) => *id,
                _ => *item,
            })
            .collect())
    };
    let src = arr_of(from)?;
    let dst = arr_of(to)?;
    anyhow::ensure!(src.len() == dst.len(), "button arrays differ in length");
    for (s, d) in src.iter().zip(&dst) {
        let v = from.heap.get(*s).clone();
        to.heap.set(*d, v);
    }
    Ok(())
}
// DFS with memoized dead ends per (key, cell).

/// The moving platforms' fields a world fixes (`platform_worlds`): `x`,
/// `last`, `rem.x`, `spd.x`, then `y` and `dir` - constant, what a traced
/// state's platform is matched to a world's by (`widen::platform_inputs`) -
/// in that order, raw 16.16.
pub const WORLD_FIELDS: usize = 6;

/// THE PLATFORM WORLDS of the start room (plans/graph-model.md step 5): every
/// arrangement of its moving platforms in the first `frames` frames, each
/// platform's `(x, last, rem.x, spd.x, y, dir)` in the order the platforms
/// stand in `objects` at the load - NOT necessarily a traced state's order
/// (a row's canonical structure reorders objects), deduplicated.
///
/// A platform moves by `dir * 0.65` a frame and nothing the player does
/// moves it, so the arrangement is a function of one number - how many
/// frames the platforms have updated since the room loaded - and running
/// the room with no input visits every arrangement a search of up to
/// `frames` frames can meet (a freeze or a restart only makes that number
/// smaller). What a search further than `frames` would need is not here,
/// and the caller must refuse such a search.
pub fn platform_worlds(frames: usize) -> Result<Vec<Vec<[i32; WORLD_FIELDS]>>> {
    use crate::interpreter::inspect::StateHelper;
    let mut ce = ConcreteEngine::new()?;
    let mut state = ce.initial_state()?;
    let mut seen: std::collections::BTreeSet<Vec<[i32; WORLD_FIELDS]>> = Default::default();
    let mut worlds = Vec::new();
    for _ in 0..=frames {
        let helper = StateHelper::new(&state);
        let objects = helper.find_global("objects").ok_or_else(|| anyhow!("no `objects`"))?;
        let HeapValue::Value(Value::Pointer(array)) = helper.load(objects) else { return Err(anyhow!("`objects` is not a table")) };
        let number = |id: crate::interpreter::heap::HeapId, field: &str| -> Option<i32> {
            let HeapValue::ObjectTable(obj) = helper.load(id) else { return None };
            match helper.load(*obj.get(field)?) {
                HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => Some(n.as_raw_u32() as i32),
                _ => None,
            }
        };
        let sub = |id: crate::interpreter::heap::HeapId, field: &str| -> Option<crate::interpreter::heap::HeapId> {
            let HeapValue::ObjectTable(obj) = helper.load(id) else { return None };
            match helper.load(*obj.get(field)?) {
                HeapValue::Value(Value::Pointer(p)) => Some(*p),
                _ => None,
            }
        };
        let mut world = Vec::new();
        for p in helper.find_objects_by_type(*array, "platform").map_err(|e| anyhow!("{e:?}"))? {
            let field = |v: Option<i32>, name: &str| v.ok_or_else(|| anyhow!("a platform without a numeric `{name}`"));
            world.push([
                field(number(p, "x"), "x")?,
                field(number(p, "last"), "last")?,
                field(sub(p, "rem").and_then(|r| number(r, "x")), "rem.x")?,
                field(sub(p, "spd").and_then(|s| number(s, "x")), "spd.x")?,
                field(number(p, "y"), "y")?,
                field(number(p, "dir"), "dir")?,
            ]);
        }
        drop(helper);
        if seen.insert(world.clone()) {
            worlds.push(world);
        }
        state = ce.step_frame(state, 0)?;
    }
    Ok(worlds)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// THE SPLIT FRAME (`CELESTE_SPLIT_FRAME`, lua/celeste-minimal-split.lua)
    /// must run exactly the original frame: after its two steps a state is
    /// the unsplit frame's, down to the SHAPE - equal game states in two
    /// shapes never dedupe. Room (1,0)'s witness dashes on frame 24, so
    /// frames 25-26 take the freeze path, whose part a sets `__frozen` and
    /// whose part b clears it. The cleared global used to stay behind as an
    /// explicit nil slot, so every state that had sat in a freeze was its
    /// own shape (room (2,1): 2.8-3.8x the unsplit states,
    /// plans/room21-2026-10-01.md); the tracer now removes the key, as Lua
    /// does (`Table::set_global`). It failed at frame 1 before: `__phase`,
    /// cleared every frame, was a slot every unsplit state lacks.
    #[test]
    fn split_frame_lands_on_the_unsplit_states_through_a_dash_freeze() {
        use crate::frame::Block;
        let tas = std::fs::read_to_string("tas/room_1_0_exit_frame_99.txt").expect("tas");
        let bytes: Vec<u8> = tas
            .lines()
            .filter(|l| !l.starts_with('#'))
            .flat_map(|l| l.split(','))
            .filter(|s| !s.trim().is_empty())
            .map(|s| s.trim().parse().expect("input byte"))
            .collect();
        assert_eq!(bytes.len(), 99);
        let fingerprint = |st: &State| {
            let b = Block::from_state(st).expect("block");
            (b.rt2().shape_hash_of(), b.keys().to_vec())
        };
        // The cart is read when an engine is built; nextest runs this test
        // in its own process.
        std::env::remove_var("CELESTE_SPLIT_FRAME");
        let mut whole = ConcreteEngine::new().expect("engine");
        std::env::set_var("CELESTE_SPLIT_FRAME", "1");
        let mut split = ConcreteEngine::new().expect("split engine");
        std::env::remove_var("CELESTE_SPLIT_FRAME");

        let mut a = whole.initial_state().expect("init");
        let mut b = split.initial_state().expect("split init");
        assert_eq!(fingerprint(&a), fingerprint(&b), "the initial states differ");
        for (f, &byte) in bytes.iter().enumerate() {
            a = whole.step_frame(a, byte).expect("frame");
            b = split.step_frame(b, byte).expect("part a");
            b = split.step_frame(b, byte).expect("part b");
            assert_eq!(
                fingerprint(&a),
                fingerprint(&b),
                "frame {}: after the split frame's two steps the state is not the unsplit frame's",
                f + 1
            );
        }
    }
}
