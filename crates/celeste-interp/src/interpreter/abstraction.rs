//! The LEVELS of the precision ladder: what each rung widens, as a value
//! (`Level`) and as the process-global level the engine dispatches to
//! (`set_level` / `current_level`).
//!
//! The widenings themselves are in the traced graph (`trace::widen`, the
//! kernels' boundary) and in the block model (`Rt2::widen_to`, the mark
//! filter's projection); this module only names them. Every widening must be
//! an OVER-approximation, and every one must be narrowed back by a finer
//! level: the ladder ends exact in every coordinate (`Level::parse_ladder`).

/// The rem precision ladder (plans/refinement-plan.md).
///
/// * `Bits(0)` - the historic widening: rem -> the full interval [-0.5, 0.5).
/// * `Bits(k)`, k in 1..=15 - quantize rem to floor-aligned buckets of width
///   2^-k: the interval `[floor(rem * 2^k) / 2^k, +2^-k)`. Each level nests
///   inside the previous one, so level k-1 over-approximates level k.
/// * `Exact` - no rem widening at all (the concrete rem dynamics).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum RemPrecision {
    Bits(u8),
    Exact,
}

impl RemPrecision {
    /// Does `self` widen rem AT LEAST as much as `finer` - i.e. is every
    /// `finer` bucket contained in one of `self`'s?
    ///
    /// `Bits(k)`'s buckets are floor-aligned at width 2^-k, so `Bits(k)`
    /// nests inside `Bits(k')` exactly when k >= k', and `Exact` (no
    /// widening) nests inside every `Bits`.
    pub fn coarser_or_equal(self, finer: RemPrecision) -> bool {
        match (self, finer) {
            (RemPrecision::Exact, _) => finer == RemPrecision::Exact,
            (RemPrecision::Bits(_), RemPrecision::Exact) => true,
            (RemPrecision::Bits(a), RemPrecision::Bits(b)) => a <= b,
        }
    }
}

/// The player's HELD-BUTTON trails `p_jump` / `p_dash` (plans/held-buttons.md).
/// They only record last frame's jump / dash button, to detect a press, and
/// each doubles the state count. `Unknown` at a non-exact level: the kernels
/// write both unknown at the boundary and fork both values inside the frame,
/// so one row covers both twins. Holding a button may then re-trigger a
/// press - an over-approximation the EXACT rung, which keeps them, refutes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum HeldPrecision {
    Unknown,
    Exact,
}

impl HeldPrecision {
    pub fn is_unknown(self) -> bool {
        self == HeldPrecision::Unknown
    }

    /// Unknown covers exact.
    pub fn coarser_or_equal(self, finer: HeldPrecision) -> bool {
        self == HeldPrecision::Unknown || finer == HeldPrecision::Exact
    }
}

/// The FLY FRUIT (plans/fly-fruit.md): `Unknown` at level 0 only, where
/// its `step` and `y` are unknown numbers, its `spd.y` / `rem.y` their whole
/// ranges and `fly` an unknown boolean (`widen::fork_fruit_inputs`,
/// `widen::widen_fly_fruit`), so the uncollected fruit is one row per frame.
/// The finer levels keep it exact.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FruitPrecision {
    Unknown,
    Exact,
}

impl FruitPrecision {
    pub fn is_unknown(self) -> bool {
        self == FruitPrecision::Unknown
    }

    /// Unknown covers exact.
    pub fn coarser_or_equal(self, finer: FruitPrecision) -> bool {
        self == FruitPrecision::Unknown || finer == FruitPrecision::Exact
    }
}

/// The FALL FLOORS and the objects' phases (plans/fall-floors.md). `Near`
/// (2026-09-30, room (7,0)) stores the countdowns - each floor's `delay`, the
/// balloon's respawn `timer`, the spring's - as the unknown number
/// (`widen::widen_floor_timers`), each floor's `state` as the interval [0, 2]
/// and its `collideable` unknown - EXCEPT where the player overlaps the floor
/// at the end of the frame (`runtime2::floor_player_window`), where they stay
/// exact - hidden, the cart's invariant there, owed by the widening
/// (`widen::widen_near_floors`) - and the objects' phases as their ranges
/// (`widen::phase_paths`). The finer levels keep them exact.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloorsPrecision {
    Near,
    Exact,
}

impl FloorsPrecision {
    /// Widened except where the player overlaps the floor (`Near`).
    pub fn is_near(self) -> bool {
        self == FloorsPrecision::Near
    }

    /// Near covers exact.
    pub fn coarser_or_equal(self, finer: FloorsPrecision) -> bool {
        self == FloorsPrecision::Near || finer == FloorsPrecision::Exact
    }
}

/// The moving PLATFORMS (plans/platforms-unknown.md): `Unknown` at the coarse
/// rungs, where every platform's `x` and `last` are the interval of its whole
/// path and its `rem.x` the whole remainder - its phase forgotten, so states
/// from different frames merge. The exact rungs keep the real positions.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PlatformsPrecision {
    Unknown,
    Exact,
}

impl PlatformsPrecision {
    pub fn is_unknown(self) -> bool {
        self == PlatformsPrecision::Unknown
    }

    /// Unknown covers exact.
    pub fn coarser_or_equal(self, finer: PlatformsPrecision) -> bool {
        self == PlatformsPrecision::Unknown || finer == PlatformsPrecision::Exact
    }
}

/// ONE LEVEL of the ladder. The search's levels are a list of these
/// (`parse_ladder`); the process-global level (`set_level`) names the level
/// whose kernels the engine dispatches to. The player's speed and position
/// are exact at every level.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Level {
    pub rem: RemPrecision,
    pub held: HeldPrecision,
    pub fruit: FruitPrecision,
    pub floors: FloorsPrecision,
    pub platforms: PlatformsPrecision,
}

impl Level {
    /// The level at rem rung `rem`, every object exact.
    pub fn for_rem(rem: RemPrecision) -> Level {
        Level { rem, ..Level::EXACT }
    }

    /// Exact in every coordinate: the top of every ladder.
    pub const EXACT: Level = Level {
        rem: RemPrecision::Exact,
        held: HeldPrecision::Exact,
        fruit: FruitPrecision::Exact,
        floors: FloorsPrecision::Exact,
        platforms: PlatformsPrecision::Exact,
    };

    /// The held buttons and the objects are widened only at a rem rung: the
    /// exact rung is what narrows them back (plans/held-buttons.md), the mark
    /// filter widens a finer row to a coarser key only at a rem rung, and
    /// their input forks and output widenings are keyed on the domain's
    /// flags, not on the trace mode (plans/fly-fruit.md, plans/fall-floors.md).
    pub fn grid_consistent(self) -> bool {
        let widens_objects = self.held.is_unknown() || self.fruit.is_unknown() || self.floors != FloorsPrecision::Exact || self.platforms.is_unknown();
        !widens_objects || matches!(self.rem, RemPrecision::Bits(_))
    }

    /// Does `self` widen at least as much as `finer` in EVERY coordinate?
    pub fn coarser_or_equal(self, finer: Level) -> bool {
        self.rem.coarser_or_equal(finer.rem)
            && self.held.coarser_or_equal(finer.held)
            && self.fruit.coarser_or_equal(finer.fruit)
            && self.floors.coarser_or_equal(finer.floors)
            && self.platforms.coarser_or_equal(finer.platforms)
    }

    /// One level from `r<k|x>sx[h][f][n][p]`: rem rung k (0..=15) or exact,
    /// the speed exact (`sx`, the only speed there is), then `h` for held
    /// buttons unknown, `f` for the fly fruit unknown, `n` for the fall floors
    /// and the objects' phases widened except where the player overlaps a
    /// floor, and `p` for the moving platforms unknown (each absent = exact).
    pub fn parse(spec: &str) -> Result<Level, String> {
        let s = spec.trim();
        let (r, mut rest) = s
            .strip_prefix('r')
            .and_then(|t| t.split_once('s'))
            .ok_or_else(|| format!("level {spec:?}: expected r<k|x>sx..."))?;
        let rem = match r {
            "x" => RemPrecision::Exact,
            k => RemPrecision::Bits(k.parse::<u8>().map_err(|e| format!("level {spec:?}: rem rung: {e}"))?),
        };
        if let RemPrecision::Bits(k) = rem {
            if k > 15 {
                return Err(format!("level {spec:?}: rem rung {k} > 15 (use x for exact)"));
            }
        }
        rest = rest
            .strip_prefix('x')
            .ok_or_else(|| format!("level {spec:?}: the speed is exact at every level (`sx`)"))?;
        let mut flag = |c: char| -> bool {
            match rest.strip_prefix(c) {
                Some(t) => {
                    rest = t;
                    true
                }
                None => false,
            }
        };
        let held = if flag('h') { HeldPrecision::Unknown } else { HeldPrecision::Exact };
        let fruit = if flag('f') { FruitPrecision::Unknown } else { FruitPrecision::Exact };
        let floors = if flag('n') { FloorsPrecision::Near } else { FloorsPrecision::Exact };
        let platforms = if flag('p') { PlatformsPrecision::Unknown } else { PlatformsPrecision::Exact };
        if !rest.is_empty() {
            return Err(format!("level {spec:?}: unexpected {rest:?} (flags are h, f, n, p in that order)"));
        }
        Ok(Level { rem, held, fruit, floors, platforms })
    }

    /// A ladder from a comma-separated list of levels, coarsest first,
    /// each level coarser-or-equal to the next in every coordinate and the
    /// last exact in every one (the ladder's soundness argument).
    pub fn parse_ladder(spec: &str) -> Result<Vec<Level>, String> {
        let levels: Vec<Level> = spec.split(',').map(Level::parse).collect::<Result<_, _>>()?;
        if levels.is_empty() {
            return Err("an empty ladder".into());
        }
        for l in &levels {
            if !l.grid_consistent() {
                return Err(format!("ladder: {l} - held buttons and objects are widened only at a rem rung"));
            }
        }
        for w in levels.windows(2) {
            if !w[0].coarser_or_equal(w[1]) {
                return Err(format!("ladder: {} is not coarser-or-equal to the next level {}", w[0], w[1]));
            }
        }
        if *levels.last().unwrap() != Level::EXACT {
            return Err("ladder: the last level must be rxsx (exact in every coordinate)".into());
        }
        Ok(levels)
    }

    /// The default ladder: rem Bits(0..=maxk) then exact.
    pub fn default_ladder(maxk: u8) -> Vec<Level> {
        (0u8..=maxk.min(15))
            .map(|k| Level::for_rem(RemPrecision::Bits(k)))
            .chain(std::iter::once(Level::EXACT))
            .collect()
    }
}

impl std::fmt::Display for Level {
    /// `Bits(k)` / `Exact` (the form the pinned marks gate was taken with),
    /// then a flag per widened object.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.rem)?;
        if self.held.is_unknown() {
            write!(f, "/H")?;
        }
        if self.fruit.is_unknown() {
            write!(f, "/F")?;
        }
        if self.floors.is_near() {
            write!(f, "/N")?;
        }
        if self.platforms.is_unknown() {
            write!(f, "/M")?;
        }
        Ok(())
    }
}

/// The process-global level (`set_level`): what the engine dispatches to.
/// Unset, it is level 0 (rem Bits(0), every object exact).
static LEVEL: std::sync::Mutex<Option<Level>> = std::sync::Mutex::new(None);

/// The level whose kernels the engine dispatches to, from here on.
pub fn set_level(l: Level) {
    *LEVEL.lock().unwrap() = Some(l);
}

/// Set only the rem precision of the process-global level.
pub fn set_rem_precision(p: RemPrecision) {
    let mut l = LEVEL.lock().unwrap();
    *l = Some(Level { rem: p, ..l.unwrap_or(Level::for_rem(RemPrecision::Bits(0))) });
}

pub fn current_level() -> Level {
    LEVEL.lock().unwrap().unwrap_or(Level::for_rem(RemPrecision::Bits(0)))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `coarser_or_equal` is the guard on every artifact one ladder level
    /// shares with another, so it must mean BUCKET CONTAINMENT and not just
    /// "a smaller number": rem's floor-aligned buckets at raw width
    /// 2^(16-k) nest exactly when the rung is coarser.
    #[test]
    fn coarser_or_equal_means_the_buckets_nest() {
        let bucket = |raw: i32, w: u8| -> (i32, i32) {
            let width = 1i32 << w;
            let low = raw.div_euclid(width) * width;
            (low, low + width - 1)
        };
        let probes = [-0x2_8000i32, -0x1_0000, -0x4CCC, -1, 0, 0x1234, 0x7FFF, 0x1_2345];
        let all = || (0..16u8).map(RemPrecision::Bits).chain(std::iter::once(RemPrecision::Exact));
        for a in all() {
            for b in all() {
                let (RemPrecision::Bits(ka), RemPrecision::Bits(kb)) = (a, b) else {
                    // Exact is a point, so it nests in everything and
                    // contains only itself.
                    assert_eq!(a.coarser_or_equal(b), b == RemPrecision::Exact || a != RemPrecision::Exact);
                    continue;
                };
                let nests = probes.iter().all(|&r| {
                    let (loa, hia) = bucket(r, 16 - ka);
                    let (lob, hib) = bucket(r, 16 - kb);
                    loa <= lob && hib <= hia
                });
                assert_eq!(a.coarser_or_equal(b), nests, "rem {:?} vs {:?}", a, b);
            }
        }
    }

    #[test]
    fn levels_parse_and_order() {
        let l = Level::parse_ladder("r0sx,r1sx,r2sx,rxsx").unwrap();
        assert_eq!(l.len(), 4);
        assert_eq!(l[2], Level::for_rem(RemPrecision::Bits(2)));
        assert_eq!(Level::parse("r0sx").unwrap(), Level::for_rem(RemPrecision::Bits(0)));
        // Speed buckets and position buckets are gone.
        assert!(Level::parse("r0s16").is_err());
        assert!(Level::parse("y2r0sx").is_err());
        // Held buttons unknown: `h`, at rem rungs only, coarser than exact.
        let h = Level::parse("r0sxh").unwrap();
        assert_eq!(h, Level { held: HeldPrecision::Unknown, ..Level::for_rem(RemPrecision::Bits(0)) });
        assert_eq!(format!("{h}"), "Bits(0)/H");
        assert!(!Level::parse("rxsxh").unwrap().grid_consistent());
        assert!(Level::parse_ladder("r0sxh,r1sxh,rxsx").is_ok());
        assert!(Level::parse_ladder("r0sx,r1sxh,rxsx").is_err());
        // The fly fruit unknown: `f` after `h`.
        let hf = Level::parse("r0sxhf").unwrap();
        assert!(hf.fruit.is_unknown() && hf.held.is_unknown() && hf.grid_consistent());
        assert_eq!(format!("{hf}"), "Bits(0)/H/F");
        assert!(Level::parse_ladder("r0sxhf,r0sxh,r1sxh,rxsx").is_ok());
        assert!(Level::parse_ladder("r0sxh,r0sxhf,rxsx").is_err());
        // The floors: near covers exact.
        let hn = Level::parse("r0sxhn").unwrap();
        assert!(hn.floors.is_near() && hn.grid_consistent());
        assert_eq!(format!("{hn}"), "Bits(0)/H/N");
        assert!(Level::parse_ladder("r0sxhn,r1sxhn,r1sxh,rxsx").is_ok());
        assert!(Level::parse_ladder("r0sxh,r0sxhn,rxsx").is_err());
        assert!(Level::parse("r0sxhb").is_err(), "floors unknown `b` is gone");
        assert!(Level::parse("r0sxht").is_err(), "timers only `t` is gone");
        // The platforms: `p`, last.
        let p = Level::parse("r0sxhnp").unwrap();
        assert!(p.platforms.is_unknown() && p.floors.is_near());
        assert_eq!(format!("{p}"), "Bits(0)/H/N/M");
        assert!(Level::parse("r0sxph").is_err(), "flags in order");
        assert!(Level::parse_ladder("r0sx,r1sx").is_err(), "must end exact");
        // The process-global level round-trips.
        set_level(hn);
        assert_eq!(current_level(), hn);
        set_rem_precision(RemPrecision::Exact);
        assert_eq!(current_level(), Level { rem: RemPrecision::Exact, ..hn });
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
