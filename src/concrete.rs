//! Concrete inputs and states: input bytes and reading a concrete row's
//! fields. The concrete step itself is `RefEngine::step`.

use anyhow::{anyhow, Context, Result};
use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Rt2, AV};


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

/// A WITNESS AS THE TOOLS NEED IT (`rewrite witness`; the category
/// runner): `inputs` replayed on the reference engine - the minimal cart,
/// the search's model - from the room's start under `s`, the start room's
/// chests seeded by `--balloon-seeds` as the TAS tools seed them.
#[derive(Debug, PartialEq, Eq, serde::Serialize)]
pub struct Witness {
    /// The frame the start room is left during, ALIVE (`leaving`; `None`:
    /// never, or in an update the player died). The replay stops at the
    /// first frame that leaves it.
    pub exit: Option<u32>,
    /// In a 100% category (the berry is the win's): the seed list with
    /// each chest's entry replaced by every other class of its draws - a
    /// chest's berry appears at x = start + s, s in [-1, 2), and only which
    /// side of a pixel it lies on matters to a whole-pixel player (the
    /// search steps the last shake as every draw, so its witness may take
    /// the berry where the file's seed does not put it). Empty elsewhere.
    pub chest_seed_variants: Vec<Vec<String>>,
}

/// `Witness` of `inputs` under `s` (16.16, the real game).
pub fn witness(inputs: &[u8], s: &crate::settings::Settings) -> Result<Witness> {
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
    let mut exit = None;
    for (i, &b) in inputs.iter().enumerate() {
        let mut out = eng.frame(&state, b)?;
        anyhow::ensure!(out.len() == 1, "frame {}: {} leaves (an unseeded `rnd`)", i + 1, out.len());
        state = out.remove(0);
        if let Some(alive) = leaving(&state, s)? {
            exit = alive.then_some(i as u32 + 1);
            break;
        }
    }
    Ok(Witness { exit, chest_seed_variants })
}

/// How a concrete state stands to the start room: still in it (`None`), or
/// out of it (`frame::exits_of`) alive (`Some(true)`) or in the update its
/// player DIED (`Some(false)`): a dying exit is no exit, as it is no win
/// (`frame::wins_of`, plans/correctness.md C8).
pub(crate) fn leaving<N: Arithmetic>(state: &Rt2<N>, s: &crate::settings::Settings) -> Result<Option<bool>> {
    if !crate::frame::exits_of(state, s)?[0] {
        return Ok(None);
    }
    Ok(Some(!crate::frame::died(state)?[0]))
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
            (r.shape_hash(), celeste_engine::exact::exact_rows(&c), crate::storage::cells::block_cells(r).expect("cells"))
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

    /// THE JUMP BUFFER AS A REAL PICO-8 RUNS IT (plans/correctness.md C7):
    /// in room (1,0) a press in the air jumps on the landing two updates
    /// later (the buffer keeps it 4), and the fresh press after it
    /// wall-jumps on the next update. The original cart on PICO-8 (`rewrite
    /// pico8 replay --original`) has the player at (49, 109), speed
    /// (-2, -2), after frame 42; without the buffer it was (49, 112),
    /// (0, -2).
    #[test]
    fn a_buffered_jump_then_a_fresh_one_as_pico8_runs_them() {
        use celeste_core::pico8_num::Pico8Num as P8;
        let runs: [(u8, usize); 8] = [(0, 23), (2, 10), (34, 1), (0, 4), (16, 1), (0, 2), (16, 1), (0, 6)];
        let inputs: Vec<u8> = runs.iter().flat_map(|&(b, n)| std::iter::repeat(b).take(n)).collect();
        let mut eng = RefEngine::new(&Default::default()).expect("engine");
        let mut state = eng.initial().expect("start");
        for &b in &inputs[..42] {
            state = eng.frame(&state, b).expect("frame").pop().expect("one leaf");
        }
        let p = *objects_of_type(&state, "player").first().expect("a player");
        let at = |path: &[&str]| num_at(&state, p, path).expect("a number");
        assert_eq!((at(&["x"]), at(&["y"]), at(&["spd", "x"]), at(&["spd", "y"])), (P8::from_i16(49), P8::from_i16(109), P8::from_i16(-2), P8::from_i16(-2)));
    }

    /// A WITNESS IS PLAYED AS IT IS (the cart buffers a jump press as the
    /// original does: nothing is re-timed): room (1,0)'s exit witness leaves
    /// the room on frame 99; a tasdatabase file is not a witness from the
    /// room's load.
    #[test]
    fn a_witness_is_played_as_it_is() {
        let w = read_inputs("tas/room_1_0_exit_frame_99.txt").expect("tas");
        let s = crate::settings::Settings { balloon_seeds: Some("0".into()), ..Default::default() };
        assert_eq!(witness(&w, &s).expect("witness"), Witness { exit: Some(99), chest_seed_variants: Vec::new() });
        assert_eq!(parse_tas("[0.5,0,]1,2\n3").expect("tasdatabase"), (Some(vec!["0.5".into(), "0".into()]), vec![1, 2, 3]));
        assert!(read_inputs("tas/tasdatabase/100/upload/TAS5.tas").is_err(), "a tasdatabase file is refused");
    }
}
