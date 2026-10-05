//! Concrete inputs and states: input bytes, the start states, reading a
//! concrete row's fields, and the room's platform worlds. The concrete step
//! itself is `RefEngine::step`.

use anyhow::{anyhow, Result};
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{Rt2, AV};

use crate::trace::refengine::RefEngine;

/// Human-readable input byte, e.g. "R+J", or "-" for none (bit 0 left,
/// 1 right, 2 up, 3 down, 4 jump, 5 dash).
pub fn format_input(byte: u8) -> String {
    const NAMES: [&str; 6] = ["L", "R", "U", "D", "J", "X"];
    let pressed: Vec<&str> = NAMES.iter().enumerate().filter(|(i, _)| byte >> i & 1 == 1).map(|(_, n)| *n).collect();
    if pressed.is_empty() {
        "-".to_string()
    } else {
        pressed.join("+")
    }
}

/// Input bytes from a `tas/`-format file (`#` comment lines) or a comma list.
pub fn read_inputs(spec: &str) -> Result<Vec<u8>> {
    let text = std::fs::read_to_string(spec).unwrap_or_else(|_| spec.to_string());
    text.lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .flat_map(|l| l.split(',').map(|t| t.trim().to_string()).filter(|t| !t.is_empty()).collect::<Vec<_>>())
        .map(|t| t.parse::<u8>().map_err(|e| anyhow!("input byte {t:?}: {e}")))
        .collect()
}

/// The objects of type `name` (a global type table) in `objects`, in array
/// order, as cells of a one-row block.
pub fn objects_of_type(rt2: &Rt2, name: &str) -> Vec<u32> {
    match celeste_names::global_id(name) {
        Some(g) => rt2.objects_of_type(crate::compiled::ids(), g),
        None => Vec::new(),
    }
}

/// The number at `path` (field names, e.g. `["spd", "x"]`) below the object
/// cell `obj` of a one-row block; `None` if absent or not a number.
pub fn num_at(rt2: &Rt2, obj: u32, path: &[&str]) -> Option<P8> {
    let mut cell = obj;
    for (i, name) in path.iter().enumerate() {
        let c = rt2.obj_field_cell(cell, celeste_names::field_id(name)?)?;
        match rt2.cols[c as usize].at(0) {
            AV::Ptr(t) if i + 1 < path.len() => cell = t,
            AV::Num(n) if i + 1 == path.len() => return Some(n),
            _ => return None,
        }
    }
    None
}

/// The platform fields a world fixes, raw 16.16, in order: `x`, `last`,
/// `rem.x`, `spd.x`, then the constant `y` and `dir` (what
/// `widen::platform_inputs` matches a traced platform by).
pub const WORLD_FIELDS: usize = 6;

/// THE PLATFORM WORLDS of the start room: every arrangement of its moving
/// platforms in the first `frames` frames (`WORLD_FIELDS` per platform, in
/// `objects` order), deduplicated.
///
/// Nothing the player does moves a platform, so the arrangement depends only
/// on how many frames the platforms have updated (a freeze or restart only
/// lowers that), and a no-input run visits every arrangement a search of up
/// to `frames` frames can meet. The caller must refuse a longer search.
pub fn platform_worlds(frames: usize) -> Result<Vec<Vec<[i32; WORLD_FIELDS]>>> {
    let mut eng = RefEngine::new()?;
    let mut state = eng.initial()?;
    let mut seen: std::collections::BTreeSet<Vec<[i32; WORLD_FIELDS]>> = Default::default();
    let mut worlds = Vec::new();
    for _ in 0..=frames {
        let mut world = Vec::new();
        for p in objects_of_type(&state, "platform") {
            let field = |path: &[&str]| {
                num_at(&state, p, path).map(|n| n.as_raw_u32() as i32).ok_or_else(|| anyhow!("a platform without a numeric `{}`", path.join(".")))
            };
            world.push([field(&["x"])?, field(&["last"])?, field(&["rem", "x"])?, field(&["spd", "x"])?, field(&["y"])?, field(&["dir"])?]);
        }
        if seen.insert(world.clone()) {
            worlds.push(world);
        }
        state = eng.step_one(&state, 0)?.into_rt2();
    }
    Ok(worlds)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The split frame (`CELESTE_SPLIT_FRAME`) must run exactly the original
    /// frame, down to the SHAPE (equal states in two shapes never dedupe).
    /// The witness's dash on frame 24 covers the freeze path, where part b
    /// clears `__frozen` (a global set to nil must be removed, as in Lua).
    #[test]
    fn split_frame_lands_on_the_unsplit_states_through_a_dash_freeze() {
        let bytes = read_inputs("tas/room_1_0_exit_frame_99.txt").expect("tas");
        assert_eq!(bytes.len(), 99);
        let fingerprint = |r: &Rt2| (r.shape_hash_of(), r.clone_block().row_keys_canonical());
        // The cart is read when an engine is built (nextest: own process).
        std::env::remove_var("CELESTE_SPLIT_FRAME");
        let mut whole = RefEngine::new().expect("engine");
        std::env::set_var("CELESTE_SPLIT_FRAME", "1");
        let mut split = RefEngine::new().expect("split engine");
        std::env::remove_var("CELESTE_SPLIT_FRAME");

        let mut a = whole.initial().expect("init");
        let mut b = split.initial().expect("split init");
        assert_eq!(fingerprint(&a), fingerprint(&b), "the initial states differ");
        for (f, &byte) in bytes.iter().enumerate() {
            a = whole.step_one(&a, byte).expect("frame").into_rt2();
            b = split.step_one(&b, byte).expect("part a").into_rt2();
            b = split.step_one(&b, byte).expect("part b").into_rt2();
            assert_eq!(fingerprint(&a), fingerprint(&b), "frame {}: after the split frame's two steps the state is not the unsplit frame's", f + 1);
        }
    }
}
