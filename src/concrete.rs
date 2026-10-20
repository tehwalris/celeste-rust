//! Concrete inputs and states: input bytes, reading a concrete row's
//! fields, and the room's platform worlds. The concrete step itself is
//! `RefEngine::step`.

use anyhow::{anyhow, Context, Result};
use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Rt2, AV};

use crate::engines::reference::ReferenceEngine;

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

/// THE TAS FILE FORMATS, parsed once: ours (`tas/`: input bytes, comma
/// separated, from the room's load; `#` comment lines) and the
/// tasdatabase's (`[s1,s2,..]`: the balloons' and chests' seeds in creation
/// order, then the bytes from the PLAYER'S FIRST FRAME). The seeds (`None`
/// without a `[..]` header) and the bytes.
pub fn parse_tas(text: &str) -> Result<(Option<Vec<String>>, Vec<u8>)> {
    let body: String = text.lines().filter(|l| !l.trim_start().starts_with('#')).collect::<Vec<_>>().join("\n");
    let (seeds, body) = match body.trim_start().strip_prefix('[') {
        Some(rest) => {
            let (head, body) = rest.split_once(']').ok_or_else(|| anyhow!("a `[seeds]` header without its `]`"))?;
            (Some(head.split(',').map(str::trim).filter(|t| !t.is_empty()).map(str::to_string).collect()), body.to_string())
        }
        None => (None, body),
    };
    let bytes = body.split(|c: char| c == ',' || c.is_whitespace()).filter(|t| !t.is_empty()).map(|t| t.parse::<u8>().map_err(|e| anyhow!("input byte {t:?}: {e}"))).collect::<Result<_>>()?;
    Ok((seeds, bytes))
}

/// Input bytes from the room's load: a `tas/`-format file or a comma list
/// (`parse_tas`). A tasdatabase file (a `[seeds]` header) is refused: its
/// bytes start at the player's first frame.
pub fn read_inputs(spec: &str) -> Result<Vec<u8>> {
    let text = match std::fs::read_to_string(spec) {
        Ok(t) => t,
        Err(_) if spec.chars().all(|c| c.is_ascii_digit() || c == ',' || c.is_whitespace()) => spec.to_string(),
        Err(e) => anyhow::bail!("{spec}: neither a readable file ({e}) nor a comma list of input bytes"),
    };
    match parse_tas(&text).with_context(|| spec.to_string())? {
        (None, bytes) => Ok(bytes),
        (Some(_), _) => anyhow::bail!("{spec}: a tasdatabase file (`[seeds]` header): its inputs start at the player's first frame, not the room's load"),
    }
}

/// The objects of type `name` (a global type table) in `objects`, in array
/// order, as cells of a one-row block.
pub fn objects_of_type<N: Arithmetic>(rt2: &Rt2<N>, name: &str) -> Vec<u32> {
    match celeste_names::global_id(name) {
        Some(g) => rt2.objects_of_type(g),
        None => Vec::new(),
    }
}

/// The number at `path` (field names, e.g. `["spd", "x"]`) below the object
/// cell `obj` of a one-row block; `None` if absent or not a number.
pub fn num_at<N: Arithmetic>(rt2: &Rt2<N>, obj: u32, path: &[&str]) -> Option<N> {
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

/// The platform fields a world fixes, as the arithmetic's numbers, in
/// order: `x`, `last`, `rem.x`, `spd.x`, then the constant `y` and `dir`
/// (what `widen::platform_inputs` matches a traced platform by).
pub const WORLD_FIELDS: usize = 6;

/// A room's platform worlds (`platform_worlds`): per world, per platform,
/// its `WORLD_FIELDS`.
pub struct PlatformWorlds<A: Arithmetic>(pub Vec<Vec<[A; WORLD_FIELDS]>>);

/// THE PLATFORM WORLDS of the start room: every arrangement of its moving
/// platforms in the first `frames` frames (`WORLD_FIELDS` per platform, in
/// `objects` order), deduplicated.
///
/// Nothing the player does moves a platform, so the arrangement depends only
/// on how many frames the platforms have updated (a freeze or restart only
/// lowers that), and a no-input run visits every arrangement a search of up
/// to `frames` frames can meet. The caller must refuse a longer search.
/// `s`: the run's settings (its room); `A`: the arithmetic, whose reference
/// engine runs the platforms (a double's `0.65` moves them otherwise).
pub fn platform_worlds<A: Arithmetic>(frames: usize, s: &crate::settings::Settings) -> Result<PlatformWorlds<A>>
where
{
    let mut eng = ReferenceEngine::<A>::new(s)?;
    let mut state = eng.initial()?;
    let mut seen: rustc_hash::FxHashSet<Vec<[A; WORLD_FIELDS]>> = Default::default();
    let mut worlds = Vec::new();
    for _ in 0..=frames {
        let mut world = Vec::new();
        for p in objects_of_type(&state, "platform") {
            let field = |path: &[&str]| num_at(&state, p, path).ok_or_else(|| anyhow!("a platform without a numeric `{}`", path.join(".")));
            // Its `x` and `last` are integers (the tracer's platform hook
            // declares the input so: `Symbolic::integral_inputs`).
            for path in [&["x"][..], &["last"]] {
                let v = field(path)?;
                anyhow::ensure!(A::num_eq(A::flr(v), v), "a platform's `{}` is {v:?}, not an integer", path.join("."));
            }
            world.push([field(&["x"])?, field(&["last"])?, field(&["rem", "x"])?, field(&["spd", "x"])?, field(&["y"])?, field(&["dir"])?]);
        }
        if seen.insert(world.clone()) {
            worlds.push(world);
        }
        state = eng.step_one(&state, 0)?;
    }
    Ok(PlatformWorlds(worlds))
}

/// A WITNESS AS THE TOOLS NEED IT (`rewrite witness`; the category
/// runner): `inputs` replayed on the reference engine - the minimal cart,
/// the search's model - from the room's start under `s`, the start room's
/// chests seeded by `--balloon-seeds` as the TAS tools seed them.
#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub struct Witness {
    /// The frame the start room is left during (`frame::exits_of`; `None`:
    /// never). The replay stops there.
    pub exit: Option<u32>,
    /// The inputs with the jump bit kept only where a jump FIRES: a fresh
    /// press (not held the input before) after which the player's `spd.y`
    /// is the jump's -2. The original cart buffers a press for 4 frames
    /// (`jbuffer`); the minimal cart drops one that cannot jump at once,
    /// so a press that does nothing here - one copied from a community TAS
    /// by `--prefer` - can fire later there. With every press firing at
    /// once, the buffer never holds one: the same route in both carts.
    /// Inputs past the exit are kept as they are.
    pub canonical: Vec<u8>,
    /// In a 100% category (the berry is the win's): the seed list with
    /// each chest's entry replaced by every other class of its draws - a
    /// chest's berry appears at x = start + s, s in [-1, 2), and only which
    /// side of a pixel it lies on matters to a whole-pixel player (the
    /// search steps the last shake as every draw, so its witness may take
    /// the berry where the file's seed does not put it). Empty elsewhere.
    pub chest_seed_variants: Vec<Vec<String>>,
}

/// The jump's speed: `spd.y` right after a jump, ground or wall.
const JUMP_SPD_Y: i16 = -2;

/// `Witness` of `inputs` under `s` (16.16, the real game).
pub fn witness(inputs: &[u8], s: &crate::settings::Settings) -> Result<Witness> {
    use celeste_core::pico8_num::Pico8Num as P8;
    let seeds: Vec<String> = s.balloon_seeds.as_deref().unwrap_or("").split(',').map(str::trim).filter(|t| !t.is_empty()).map(str::to_string).collect();
    let slots: Vec<&'static str> = crate::trace::cart::room_objects(s.room).into_iter().filter(|o| o.seeded).map(|o| o.kind).collect();
    let mut vals = seeds;
    while vals.len() < slots.len() {
        vals.push("0".to_string());
    }
    let chest_seed_variants = if s.category.hundred() {
        let classes = ["-1", "-0.5", "0", "0.5", "1", "1.5"];
        slots.iter().enumerate().filter(|(_, k)| **k == "chest").flat_map(|(i, _)| classes.iter().filter(|c| vals[i] != **c).map(|c| [&vals[..i], &[c.to_string()], &vals[i + 1..]].concat()).collect::<Vec<_>>()).collect()
    } else {
        Vec::new()
    };
    let mut eng = crate::engines::reference::RefEngine::new(&crate::settings::Settings { seed_chests: true, ..s.clone() })?;
    let mut state = eng.initial()?;
    let (mut canonical, mut exit, mut prev) = (Vec::with_capacity(inputs.len()), None, 0u8);
    for (i, &b) in inputs.iter().enumerate() {
        if exit.is_some() {
            canonical.push(b);
            continue;
        }
        let mut out = eng.frame(&state, b)?;
        anyhow::ensure!(out.len() == 1, "frame {}: {} leaves (an unseeded `rnd`)", i + 1, out.len());
        state = out.remove(0);
        let spd_y = objects_of_type(&state, "player").first().and_then(|&p| num_at(&state, p, &["spd", "y"]));
        let fired = b & 16 != 0 && prev & 16 == 0 && spd_y == Some(P8::from_i16(JUMP_SPD_Y));
        canonical.push(b & !16 | if fired { 16 } else { 0 });
        prev = b;
        if crate::frame::exits_of(&state, s)?[0] {
            exit = Some(i as u32 + 1);
        }
    }
    Ok(Witness { exit, canonical, chest_seed_variants })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engines::reference::RefEngine;

    /// The split frame (`--split-frame`) must run exactly the original
    /// frame, down to the SHAPE (equal states in two shapes never dedupe).
    /// The witness's dash on frame 24 covers the freeze path, where part b
    /// clears `__frozen` (a global set to nil must be removed, as in Lua).
    #[test]
    fn split_frame_lands_on_the_unsplit_states_through_a_dash_freeze() {
        let bytes = read_inputs("tas/room_1_0_exit_frame_99.txt").expect("tas");
        assert_eq!(bytes.len(), 99);
        let fingerprint = |r: &Rt2| {
            let mut c = r.clone_block();
            c.canonical();
            (r.shape_hash_of(), celeste_engine::exact::exact_rows(&c), crate::search::pos_graph::block_cells(r).expect("cells"))
        };
        // The cart is read when an engine is built, under its settings.
        let mut whole = RefEngine::new(&Default::default()).expect("engine");
        let mut split = RefEngine::new(&crate::settings::Settings { split_frame: true, ..Default::default() }).expect("split engine");

        let mut a = whole.initial().expect("init");
        let mut b = split.initial().expect("split init");
        assert_eq!(fingerprint(&a), fingerprint(&b), "the initial states differ");
        for (f, &byte) in bytes.iter().enumerate() {
            a = whole.step_one(&a, byte).expect("frame");
            b = split.step_one(&b, byte).expect("part a");
            b = split.step_one(&b, byte).expect("part b");
            assert_eq!(fingerprint(&a), fingerprint(&b), "frame {}: after the split frame's two steps the state is not the unsplit frame's", f + 1);
        }
    }

    /// A WITNESS KEEPS ONLY THE JUMP PRESSES THAT FIRE: room (1,0)'s exit
    /// witness (its nine presses all jump) with three presses added - before
    /// the player exists, held after a jump, and fresh in mid-air - canonicalizes
    /// back to the witness, which leaves the room on frame 99; a tasdatabase
    /// file is not a witness from the room's load.
    #[test]
    fn a_witness_keeps_only_the_jump_presses_that_fire() {
        let w = read_inputs("tas/room_1_0_exit_frame_99.txt").expect("tas");
        let mut pressed = w.clone();
        for k in [0, 31, 35] {
            assert_eq!(pressed[k] & 16, 0);
            pressed[k] |= 16;
        }
        let s = crate::settings::Settings { balloon_seeds: Some("0".into()), ..Default::default() };
        assert_eq!(witness(&pressed, &s).expect("witness"), Witness { exit: Some(99), canonical: w, chest_seed_variants: Vec::new() });
        assert_eq!(parse_tas("[0.5,0,]1,2\n3").expect("tasdatabase"), (Some(vec!["0.5".into(), "0".into()]), vec![1, 2, 3]));
        assert!(read_inputs("tas/tasdatabase/100/upload/TAS5.tas").is_err(), "a tasdatabase file is refused");
    }
}
