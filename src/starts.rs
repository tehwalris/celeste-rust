//! The start states real play can enter a room with (`rewrite room`):
//! the room's LOADING FRAME runs its objects' updates from the leaving
//! player's index J (`--loading-jank J`, `trace::cart`), and J depends on
//! how the previous room was played. Every J a play of the previous room can
//! give, and the J's grouped by the start state they make: one search per
//! class covers every history (the category runner searches each).
//!
//! The ground truth for ONE history stays the boot chain on a real PICO-8
//! (`pico8_diff/chain.py`): this is the sound over-approximation of all
//! of them.

use anyhow::{Context, Result};

use crate::trace::cart::{room_objects, RoomObject};
use crate::engines::reference::RefEngine;

/// Object tiles whose object a play can destroy before the exit: a berry
/// taken (26), a fly fruit taken or flown off (28), a key taken (8), a chest
/// opened (20), a fake wall broken (64). The spawn is always replaced by the
/// player.
const DESTROYABLE: &[u8] = &[26, 28, 8, 20, 64];

/// The room title is drawn away on the 36th frame after the load; the
/// spawn's smoke lasts 15 frames, 6 of them before the player exists
/// (`J` counts them while they are in `objects`).
const TITLE_FRAMES: u32 = 36;
const SMOKE_CLIMB: u32 = 10;

#[derive(serde::Serialize, Debug)]
pub struct Starts {
    pub room: (i16, i16),
    /// The cart's level (1-based, as the tasdatabase names it).
    pub level: i16,
    /// The previous room and its objects (the original cart's), `None` for
    /// the first room (the boot's loading frame updates no object).
    pub previous: Option<(i16, i16)>,
    pub previous_objects: Vec<RoomObject>,
    /// The frame the previous room's player exists from, IL-loaded with no
    /// input (the reference engine).
    pub spawn_frame: Option<u32>,
    /// Every feasible J, ascending.
    pub feasible: Vec<usize>,
    /// The feasible J's grouped by the exact start state they give, in J order.
    pub classes: Vec<Vec<usize>>,
    /// The J Celia's IL load uses (cctas.lua `load_level`, offset 0): the
    /// previous room's object count at its load, the title left out.
    pub celia: Option<usize>,
}

/// The room before `(x, y)` in the game (`next_room`'s inverse).
fn previous((x, y): (i16, i16)) -> Option<(i16, i16)> {
    match (x, y) {
        (0, 0) => None,
        (0, y) => Some((7, y - 1)),
        (x, y) => Some((x - 1, y)),
    }
}

/// The J Celia's IL load into `room` applies (cctas.lua `load_level`, offset
/// 0): the previous room's object count at its load, the title left out;
/// `None` for the first room (no previous room, no jank).
pub fn celia_jank(room: (i16, i16)) -> Result<Option<usize>> {
    let Some(prev) = previous(room) else { return Ok(None) };
    Ok(Some(room_objects(prev).len()))
}

/// The starts of `s.room` under settings `s` (the category, the balloon
/// seeds; `--loading-jank` is what this enumerates, so refused).
pub fn starts(s: &crate::settings::Settings) -> Result<Starts> {
    anyhow::ensure!(s.loading_jank.is_none(), "rewrite room enumerates the room's starts: no --loading-jank");
    let level = crate::game_runner::level_index(s.room.0, s.room.1) + 1;
    let Some(prev) = previous(s.room) else {
        return Ok(Starts { room: s.room, level, previous: None, previous_objects: Vec::new(), spawn_frame: None, feasible: Vec::new(), classes: Vec::new(), celia: None });
    };
    let objs = room_objects(prev);
    let celia = celia_jank(s.room)?;
    // J = 1 + the objects before the player when it leaves: the previous
    // room's objects but the spawn, less any subset of the destroyable ones,
    // plus the room title if it is still up and the spawn's smoke if it can
    // be: only when the exit can come that soon, bounded by the spawn's own
    // frames and the climb at the region's speed bound.
    let speed = s.region_grid().context("rewrite room: the starts' climb is bounded by the region's speed (--region px,S)")?.speed as u32;
    let k = objs.len();
    let d = objs.iter().filter(|o| DESTROYABLE.contains(&o.tile)).count();
    let spawn_y = objs.iter().find(|o| o.kind == "player_spawn").with_context(|| format!("room {prev:?} has no spawn"))?.ty as u32 * 8;
    let start = spawn_frame(s, prev)?;
    let climb = (spawn_y + 5).div_ceil(speed);
    // start - 1: in real play the previous room's own loading frame may have
    // updated its spawn too (a frame ahead).
    let title: &[usize] = if (start - 1) + climb - 1 <= TITLE_FRAMES { &[0, 1] } else { &[0] };
    let smoke: &[usize] = if climb <= SMOKE_CLIMB { &[0, 1] } else { &[0] };
    let mut feasible: Vec<usize> = (0..=d).flat_map(|x| title.iter().flat_map(move |&t| smoke.iter().map(move |&sm| 1 + (k - 1 - x) + t + sm))).collect();
    feasible.sort_unstable();
    feasible.dedup();
    // The classes: the start state each J gives, compared exactly.
    let mut classes: Vec<(celeste_engine::exact::ExactRow, u32, Vec<usize>)> = Vec::new();
    for &j in &feasible {
        let sj = crate::settings::Settings { loading_jank: Some(j), ..s.clone() };
        let b = RefEngine::new(&sj)?.initial()?.canonicalized();
        let row = celeste_engine::exact::exact_rows(&b).swap_remove(0);
        let cell = crate::search::pos_graph::block_cells(&b)?[0];
        match classes.iter_mut().find(|(r, c, _)| *r == row && *c == cell) {
            Some((_, _, js)) => js.push(j),
            None => classes.push((row, cell, vec![j])),
        }
    }
    Ok(Starts {
        room: s.room,
        level,
        previous: Some(prev),
        celia,
        previous_objects: objs,
        spawn_frame: Some(start),
        feasible,
        classes: classes.into_iter().map(|(_, _, js)| js).collect(),
    })
}

/// The frame room `room`'s player exists from after an IL load with no
/// input (frame f: the f-th `_update(); _draw()` after `_init()`).
fn spawn_frame(s: &crate::settings::Settings, room: (i16, i16)) -> Result<u32> {
    let sp = crate::settings::Settings { room, loading_jank: None, ..s.clone() };
    let mut eng = RefEngine::new(&sp)?;
    let mut state = eng.initial()?;
    for f in 1..=120 {
        let mut next = eng.frame(&state, 0)?;
        anyhow::ensure!(next.len() == 1, "room {room:?} frame {f}: {} leaves without input (seed its balloons: --balloon-seeds)", next.len());
        state = next.remove(0);
        if !crate::concrete::objects_of_type(&state, "player").is_empty() {
            return Ok(f);
        }
    }
    anyhow::bail!("room {room:?}: no player by frame 120")
}

#[cfg(test)]
mod tests {
    /// The starts of two rooms as the Python model gives them
    /// (pico8_diff/chain.py on PICO-8, one `rewrite run --dump-start` per
    /// J): room (3,0) takes J 3 or 4 into ONE class, room (4,1) J 2 and 3
    /// into two.
    #[test]
    fn the_starts_of_two_rooms() {
        let at = |room: (i16, i16)| {
            let s = crate::settings::Settings { room, balloon_seeds: Some("0".into()), region: Some((16, 6)), ..Default::default() };
            let st = super::starts(&s).expect("starts");
            (st.feasible, st.classes, st.celia, st.spawn_frame)
        };
        assert_eq!(at((3, 0)), (vec![3, 4], vec![vec![3, 4]], Some(4), Some(26)));
        assert_eq!(at((4, 1)), (vec![2, 3], vec![vec![2], vec![3]], Some(3), Some(26)));
    }

    #[test]
    fn the_room_before_wraps_rows() {
        assert_eq!(super::previous((0, 0)), None);
        assert_eq!(super::previous((0, 2)), Some((7, 1)));
        assert_eq!(super::previous((3, 1)), Some((2, 1)));
    }
}
