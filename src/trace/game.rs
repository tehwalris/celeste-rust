//! THE GAME'S NAMES the tracer and the reference engine read off a state by
//! path: the player and its spawn (objects by `type`), the remainder and
//! position fields, the room, the button states. The search reads its own
//! as ids (`compiled::BoundaryIds`), the widening table its own as data.

/// The player object's type, and its spawn's (before the player exists).
pub const PLAYER: &str = "player";
pub const PLAYER_SPAWN: &str = "player_spawn";
/// The sub-pixel remainder table of a moving object (`rem.x`, `rem.y`).
pub const REM: &str = "rem";
/// A table's coordinate fields, x then y (`rem`, `spd`, a position, `room`).
pub const AXES: [&str; 2] = ["x", "y"];
/// The room's coordinates (`room.x`, `room.y`).
pub const ROOM: &str = "room";
/// The six buttons' states, a global reset at every frame's start.
pub const BUTTONS: &str = "__button_states";
/// The whole pixels `move` steps on each axis: its locals `ox`, `oy`, which
/// the arc capture reads at the player's split.
pub const MOVE_STEP: [&str; 2] = ["ox", "oy"];
