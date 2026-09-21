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

/// The room that means "won" for the configured start room: the room the
/// player lands in after exiting, as the cart's `next_room` loads it -
/// `(x + 1, y)` along a map row, and `(0, y + 1)` from a row's last room
/// (x == 7: rooms (7,0), (7,1), (7,2); until 2026-09-21 this asserted x < 7,
/// which room (7,0) hit). Compare BOTH coordinates: the wrapped exit changes
/// `room.y`.
pub fn win_room() -> (i16, i16) {
    let (x, y) = start_room();
    if x == 7 {
        (0, y + 1)
    } else {
        (x + 1, y)
    }
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
    let out = game_lua.replacen(PAT, &format!("load_room({}, {})", x, y), 1);
    // EXPERIMENT (room (3,0), 2026-09-17, plans/room30.md): with
    // `CELESTE_EXPERIMENT_NO_FLY_FRUIT` set, the cart never registers the fly
    // fruit type, so no fly fruit spawns. A DIFFERENT GAME: it measures how
    // level 0 grows without the fly fruit's time-dependent fields, and no
    // horizon it reports is an answer for the real cart.
    if std::env::var_os("CELESTE_EXPERIMENT_NO_FLY_FRUIT").is_some() {
        const TYPE: &str = "add(types,fly_fruit)";
        let n = out.matches(TYPE).count();
        if n != 1 {
            return Err(anyhow!("CELESTE_EXPERIMENT_NO_FLY_FRUIT: expected exactly one {:?} in the game lua, found {}", TYPE, n));
        }
        static BANNER: std::sync::Once = std::sync::Once::new();
        BANNER.call_once(|| eprintln!("[experiment] NO FLY FRUIT: the cart's fly fruit type is removed; this is not the real game"));
        return Ok(out.replacen(TYPE, "-- (experiment) no fly fruit", 1));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::apply_start_room;

    // start_room() is a process-wide OnceLock, so tests can only exercise
    // the default (1,0) configuration; the non-default path is exercised by
    // the room-(0,0) pipeline runs.
    #[test]
    fn win_and_dir_stem_for_default_room() {
        assert_eq!(super::win_room(), (2, 0));
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
