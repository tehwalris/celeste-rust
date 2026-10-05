//! Game setup: the start-room configuration the whole search agrees on (the
//! cart substitution, the collision cache's room, the win room).

use anyhow::{anyhow, Result};

/// The room the search starts in, from `CELESTE_START_ROOM` ("x,y"),
/// default (1, 0). The single source of truth for the `_init` substitution,
/// the collision cache's room and the checkpoint fingerprint, which must agree.
pub fn start_room() -> (i16, i16) {
    static ROOM: std::sync::OnceLock<(i16, i16)> = std::sync::OnceLock::new();
    *ROOM.get_or_init(|| match std::env::var("CELESTE_START_ROOM") {
        Ok(s) => {
            let (x, y) = s
                .split_once(',')
                .unwrap_or_else(|| panic!("CELESTE_START_ROOM must be \"x,y\", got {:?}", s));
            (
                x.trim().parse().expect("CELESTE_START_ROOM x must be an integer"),
                y.trim().parse().expect("CELESTE_START_ROOM y must be an integer"),
            )
        }
        Err(_) => (1, 0),
    })
}

/// The room that means "won": the one the cart's `next_room` loads after
/// the start room, `(x + 1, y)`, or `(0, y + 1)` from a row's last room.
/// Compare BOTH coordinates: the wrapped exit changes `room.y`.
pub fn win_room() -> (i16, i16) {
    let (x, y) = start_room();
    if x == 7 {
        (0, y + 1)
    } else {
        (x + 1, y)
    }
}

/// The cart's `level_index()` of room (x, y).
pub fn level_index(x: i16, y: i16) -> i16 {
    x % 8 + y * 8
}

/// The level whose big chest holds the orb (`max_djump=2`): room (5,2).
pub const ORB_LEVEL: i16 = 21;

/// Point the game's `_init` at the configured start room. Strict: the lua
/// must contain the default call exactly once, so an edit cannot silently
/// disable the substitution.
pub fn apply_start_room(game_lua: &str) -> Result<String> {
    const PAT: &str = "load_room(1, 0)";
    let count = game_lua.matches(PAT).count();
    if count != 1 {
        return Err(anyhow!(
            "expected exactly one {:?} in the game lua, found {}",
            PAT,
            count
        ));
    }
    let (x, y) = start_room();
    // The orb sets `max_djump=2` for the rest of the game; a later room
    // must start as a play-through reaches it, with the orb taken.
    let orb = if level_index(x, y) > ORB_LEVEL { "max_djump=2 " } else { "" };
    Ok(game_lua.replacen(PAT, &format!("{}load_room({}, {})", orb, x, y), 1))
}

#[cfg(test)]
mod tests {
    use super::apply_start_room;

    // start_room() is a process-wide OnceLock: tests see only room (1,0).
    #[test]
    fn win_room_of_the_default_room() {
        assert_eq!(super::win_room(), (2, 0));
    }

    // In the default room the substitution is the identity.
    #[test]
    fn apply_start_room_is_identity_for_default_room() {
        let src = "function _init()\n\tload_room(1, 0)\nend\n";
        assert_eq!(apply_start_room(src).unwrap(), src);
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
        assert!(apply_start_room("load_room(7,3)").is_err());
        assert!(apply_start_room("load_room(1, 0) load_room(1, 0)").is_err());
    }
}
