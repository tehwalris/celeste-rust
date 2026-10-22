//! Game setup: the start-room configuration the whole search agrees on (the
//! cart substitution, the win room, the level index).

use anyhow::{anyhow, Result};

/// The room that means "won" from start room `(x, y)`: the one the cart's
/// `next_room` loads after it, `(x + 1, y)`, or `(0, y + 1)` from a row's
/// last room. Compare BOTH coordinates: the wrapped exit changes `room.y`.
pub fn win_room((x, y): (i16, i16)) -> (i16, i16) {
    if x == 7 {
        (0, y + 1)
    } else {
        (x + 1, y)
    }
}

/// The room before `(x, y)` in the game (`win_room`'s inverse; none before
/// the first).
pub fn previous_room((x, y): (i16, i16)) -> Option<(i16, i16)> {
    match (x, y) {
        (0, 0) => None,
        (0, y) => Some((7, y - 1)),
        (x, y) => Some((x - 1, y)),
    }
}

/// The cart's `level_index()` of room (x, y).
pub fn level_index(x: i16, y: i16) -> i16 {
    x % 8 + y * 8
}

/// The room of LEVEL `level` (1-based, as the tasdatabase numbers its
/// files: `TAS<level>.tas`): `level_index`'s inverse.
pub fn room_of_level(level: i16) -> (i16, i16) {
    ((level - 1) % 8, (level - 1) / 8)
}

/// The level whose big chest holds the orb (`max_djump=2`): room (5,2).
pub const ORB_LEVEL: i16 = 21;

/// Point the game's `_init` at the configured start room. Strict: the lua
/// must contain the default call exactly once, so an edit cannot silently
/// disable the substitution.
pub fn apply_start_room(game_lua: &str, s: &crate::settings::Settings) -> Result<String> {
    const PAT: &str = "load_room(1, 0)";
    let count = game_lua.matches(PAT).count();
    if count != 1 {
        return Err(anyhow!(
            "expected exactly one {:?} in the game lua, found {}",
            PAT,
            count
        ));
    }
    let (x, y) = s.room;
    // The orb sets `max_djump=2` for the rest of the game; a later room
    // must start as a play-through reaches it, with the orb taken.
    let orb = if level_index(x, y) > ORB_LEVEL && !s.category.gemskip() { "max_djump=2 " } else { "" };
    Ok(game_lua.replacen(PAT, &format!("{}load_room({}, {})", orb, x, y), 1))
}

#[cfg(test)]
mod tests {
    use super::apply_start_room;

    #[test]
    fn win_room_of_the_default_room() {
        assert_eq!(super::win_room((1, 0)), (2, 0));
        assert_eq!(super::win_room((7, 1)), (0, 2));
        // The room order and its inverse, over every room.
        for y in 0..4 {
            for x in 0..8 {
                assert_eq!(super::previous_room(super::win_room((x, y))), Some((x, y)));
            }
        }
        assert_eq!(super::previous_room((0, 0)), None);
    }

    #[test]
    fn apply_start_room_is_identity_for_default_room() {
        let src = "function _init()\n\tload_room(1, 0)\nend\n";
        assert_eq!(apply_start_room(src, &Default::default()).unwrap(), src);
    }

    /// A room past the orb's starts with the second dash; the orb's room and
    /// earlier ones do not.
    #[test]
    fn rooms_past_the_orb_start_with_two_dashes() {
        use super::{level_index, ORB_LEVEL};
        assert_eq!(level_index(5, 2), ORB_LEVEL);
        assert!(level_index(6, 2) > ORB_LEVEL && level_index(1, 0) < ORB_LEVEL);
    }

    #[test]
    fn apply_start_room_rejects_missing_or_duplicated_call() {
        assert!(apply_start_room("load_room(7,3)", &Default::default()).is_err());
        assert!(apply_start_room("load_room(1, 0) load_room(1, 0)", &Default::default()).is_err());
    }
}
