//! Game setup: the start-room configuration the whole search agrees on.
//!
//! This file used to also hold the reference interpreter's builtin
//! implementations (`min`, `max`, `flr`, `tile_flag_at`, ...) and the
//! `FixedEnv` that registered them. Those fed the CFG interpreter, which is
//! gone (the tracer's `RefEngine` is the reference now and reimplements the
//! builtins over its own domain), so the builtins and `FixedEnv` went with it
//! (`fixed_env.rs`). What is left is the start-room configuration, which is
//! still the single source of truth for the cart substitution, the collision
//! cache room, and the checkpoint fingerprint.

use anyhow::Result;

/// The room the search starts in, from `CELESTE_START_ROOM` ("x,y"),
/// default (1, 0). Drives the `_init` load_room substitution
/// (`apply_start_room`), the collision cache room, and the checkpoint config
/// fingerprint - all must agree, which is why this is the single source of
/// truth.
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

/// The `room.x` value that means "won" for the configured start room: the
/// room the player lands in after exiting. Progression in this cart is
/// `room.x + 1` along a map row (`next_room` only wraps at x == 7, which no
/// supported start room reaches - asserted). Comparing `room.x` alone is
/// enough because a same-row exit never changes `room.y`.
pub fn win_room_x() -> i16 {
    let (x, _y) = start_room();
    assert!(
        x < 7,
        "win_room_x: start room x={} would wrap to the next map row",
        x
    );
    x + 1
}

/// Directory-name stem for per-room checkpoint trees under a base dir:
/// level 0 lives at `<base>/<stem>`, level k at `<base>/<stem>-k<k>`.
/// The default room keeps its historical name "room1" so existing
/// checkpoint trees on disk stay addressable; other rooms get "room<x><y>"
/// (e.g. "room00").
pub fn room_dir_stem() -> String {
    let (x, y) = start_room();
    if (x, y) == (1, 0) {
        "room1".to_string()
    } else {
        format!("room{}{}", x, y)
    }
}

/// Point the game's `_init` at the configured start room. Strict: the
/// checked-in lua must contain the default call exactly once, so a source
/// edit can never silently disable the substitution.
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
    Ok(game_lua.replacen(PAT, &format!("load_room({}, {})", x, y), 1))
}

#[cfg(test)]
mod tests {
    use super::apply_start_room;

    // start_room() is a process-wide OnceLock, so tests can only exercise
    // the default (1,0) configuration; the non-default path is exercised by
    // the room-(0,0) pipeline runs.
    #[test]
    fn win_and_dir_stem_for_default_room() {
        assert_eq!(super::win_room_x(), 2);
        assert_eq!(super::room_dir_stem(), "room1");
    }

    // These run with the default start room (1,0), where the substitution
    // must be an exact identity - and the strictness must still hold.
    #[test]
    fn apply_start_room_is_identity_for_default_room() {
        let src = "function _init()\n\tload_room(1, 0)\nend\n";
        assert_eq!(apply_start_room(src).unwrap(), src);
    }

    #[test]
    fn apply_start_room_rejects_missing_or_duplicated_call() {
        assert!(apply_start_room("load_room(7,3)").is_err());
        assert!(apply_start_room("load_room(1, 0) load_room(1, 0)").is_err());
    }
}
