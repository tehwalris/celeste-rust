//! The LEVEL a forward runs at: which of the room's objects it widens, as a
//! value (`Level`) and as the process-global level the engine dispatches to
//! (`set_level` / `current_level`).
//!
//! The player's sub-pixel remainder is widened to [-1/2, 1/2) at every
//! level's boundary and tracked EXACTLY beside the rows, as the transfers of
//! the recorded edges (the rotation graph, `search::arcs`); the player's
//! speed and position are exact. A level's flags widen the objects (and the
//! held buttons): an over-approximation, so the search's optimum over such a
//! level is a lower bound, and the concrete count-up (`arc_dp`) is what
//! makes it the game's.
//!
//! The widenings themselves are in the traced graph (`trace::widen`, the
//! kernels' boundary) and in the block model (`Rt2::widen_to`, a row's
//! projection onto a level); this module only names them.

/// A LEVEL: which objects the forward widens (plans/abstractions.md). Each
/// widening over-approximates; the concrete search refutes it.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Level {
    /// `h`: the player's held-button trails `p_jump` / `p_dash` unknown at
    /// the boundary and forked both ways inside the frame (holding a button
    /// may then re-trigger a press).
    pub held: bool,
    /// `f`: the fly fruit's `step` and `y` unknown numbers, `spd.y` / `rem.y`
    /// their whole ranges, `fly` unknown (`widen::fork_fruit_inputs`,
    /// `widen::widen_fly_fruit`).
    pub fruit: bool,
    /// `n`: the countdowns (floor `delay`, balloon `timer`, the spring's) the
    /// unknown number, each fall floor's `state` [0, 2] and `collideable`
    /// unknown except where the player overlaps the floor at the frame's end
    /// (`runtime2::floor_player_window`), the objects' phases their ranges
    /// (`widen::widen_near_floors`, `widen::phase_paths`).
    pub floors_near: bool,
    /// `p`: every moving platform's `x` and `last` the interval of its whole
    /// path, its `rem.x` the whole remainder (its phase forgotten).
    pub platforms: bool,
}

impl Level {
    /// Every object exact (`r0sx`).
    pub const EXACT: Level = Level { held: false, fruit: false, floors_near: false, platforms: false };

    /// A level from `r0sx[h][f][n][p]` (the remainder rung 0 and the exact
    /// speed every level has, then the flags): `h` held buttons unknown, `f`
    /// the fly fruit unknown, `n` the fall floors and the objects' phases
    /// widened except where the player overlaps a floor, `p` the moving
    /// platforms unknown (each absent = exact).
    pub fn parse(spec: &str) -> Result<Level, String> {
        let s = spec.trim();
        let mut rest = s
            .strip_prefix("r0sx")
            .ok_or_else(|| format!("level {spec:?}: expected r0sx[h][f][n][p] (the remainder is the arcs', the speed exact)"))?;
        let mut flag = |c: char| -> bool {
            match rest.strip_prefix(c) {
                Some(t) => {
                    rest = t;
                    true
                }
                None => false,
            }
        };
        let level = Level { held: flag('h'), fruit: flag('f'), floors_near: flag('n'), platforms: flag('p') };
        if !rest.is_empty() {
            return Err(format!("level {spec:?}: unexpected {rest:?} (flags are h, f, n, p in that order)"));
        }
        Ok(level)
    }
}

impl std::fmt::Display for Level {
    /// The spec `parse` reads.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r0sx")?;
        for (on, c) in [(self.held, 'h'), (self.fruit, 'f'), (self.floors_near, 'n'), (self.platforms, 'p')] {
            if on {
                write!(f, "{c}")?;
            }
        }
        Ok(())
    }
}

/// The process-global level (`set_level`): what the engine dispatches to.
/// Unset, it is `Level::EXACT` (every object exact).
static LEVEL: std::sync::Mutex<Option<Level>> = std::sync::Mutex::new(None);

/// The level whose kernels the engine dispatches to, from here on.
pub fn set_level(l: Level) {
    *LEVEL.lock().unwrap() = Some(l);
}

pub fn current_level() -> Level {
    LEVEL.lock().unwrap().unwrap_or(Level::EXACT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn levels_parse_and_print() {
        assert_eq!(Level::parse("r0sx").unwrap(), Level::EXACT);
        for spec in ["r0sx", "r0sxh", "r0sxhf", "r0sxhn", "r0sxhnp", "r0sxn", "r0sxf"] {
            assert_eq!(Level::parse(spec).unwrap().to_string(), spec);
        }
        let hn = Level::parse("r0sxhn").unwrap();
        assert!(hn.held && hn.floors_near && !hn.fruit && !hn.platforms);
        // The rem rungs, speed buckets and position buckets are gone.
        assert!(Level::parse("r1sx").is_err());
        assert!(Level::parse("rxsx").is_err());
        assert!(Level::parse("r0s16").is_err());
        assert!(Level::parse("y2r0sx").is_err());
        assert!(Level::parse("r0sxhb").is_err(), "floors unknown `b` is gone");
        assert!(Level::parse("r0sxph").is_err(), "flags in order");
        // The process-global level round-trips.
        set_level(hn);
        assert_eq!(current_level(), hn);
    }
}

/// A SYNTHETIC win target for cheap end-to-end tests: `CELESTE_WIN_AT_XY=x,y`
/// makes "won" mean "the player is at whole-pixel (x, y)" instead of "the
/// player left the room".
///
/// Why this exists. The real win is a room transition, so any test of the
/// forward/sweep pipeline that produces NON-VACUOUS `g` has to run to the
/// frame where the room is actually exited - 90+ on room (1,0), tens of
/// minutes. Moving the finish line to a position the optimal run passes
/// through EARLY gives the same pipeline, with real wins, at a fraction of
/// the horizon; two such points then bracket a cost extrapolation.
///
/// A checkpoint or a `g` produced under a synthetic win describes a
/// different search, and must never be resumable from, or comparable to, a
/// real campaign's. Read once.
pub fn synthetic_win_xy() -> Option<(i16, i16)> {
    static TARGET: std::sync::OnceLock<Option<(i16, i16)>> = std::sync::OnceLock::new();
    *TARGET.get_or_init(|| {
        let raw = std::env::var("CELESTE_WIN_AT_XY").ok()?;
        let (x, y) = raw
            .split_once(',')
            .unwrap_or_else(|| panic!("CELESTE_WIN_AT_XY must be \"x,y\", got {:?}", raw));
        let parse = |s: &str, which: &str| -> i16 {
            s.trim()
                .parse()
                .unwrap_or_else(|e| panic!("CELESTE_WIN_AT_XY {} coordinate {:?}: {}", which, s, e))
        };
        let target = (parse(x, "x"), parse(y, "y"));
        println!(
            "SYNTHETIC WIN: a lane counts as won at player ({}, {}), NOT at the \
             room exit - this is a test configuration and is in the fingerprint",
            target.0, target.1
        );
        Some(target)
    })
}
