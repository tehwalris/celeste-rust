//! The LEVEL a forward runs at: which of the room's objects it widens, as a
//! value (`Level`) and as the process-global level the engine dispatches to.
//!
//! The player's remainder is widened at every boundary but tracked exactly
//! beside the rows (the arcs, `search::arcs`); speed and position are exact.
//! A level's flags widen the objects and held buttons: an over-approximation,
//! so its optimum is a lower bound that the concrete count-up (`arc_dp`)
//! confirms. The widenings themselves live in `trace::widen` and
//! `Rt2::widen_to`; this module only names them.

pub use celeste_engine::widening::Level;

/// The process-global level; unset means `Level::EXACT`.
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
    fn the_process_global_level_round_trips() {
        let hn = Level::parse("r0sxhn").unwrap();
        set_level(hn);
        assert_eq!(current_level(), hn);
    }
}

/// A SYNTHETIC win target for cheap end-to-end tests: `CELESTE_WIN_AT_XY=x,y`
/// makes "won" mean "the player is at whole-pixel (x, y)" instead of "the
/// player left the room", so the pipeline sees real wins at a short horizon;
/// `x0..x1,y0..y1` (inclusive) makes it "the player is in that rectangle"
/// (can a region be reached at all?). Returns (x_lo, x_hi, y_lo, y_hi).
///
/// A checkpoint made under a synthetic win is a different search: never
/// resume from it or compare it with a real one. Read once.
pub fn synthetic_win_rect() -> Option<(i16, i16, i16, i16)> {
    static TARGET: std::sync::OnceLock<Option<(i16, i16, i16, i16)>> = std::sync::OnceLock::new();
    *TARGET.get_or_init(|| {
        let raw = std::env::var("CELESTE_WIN_AT_XY").ok()?;
        let (x, y) = raw
            .split_once(',')
            .unwrap_or_else(|| panic!("CELESTE_WIN_AT_XY must be \"x,y\" or \"x0..x1,y0..y1\", got {:?}", raw));
        let parse = |s: &str, which: &str| -> (i16, i16) {
            let one = |t: &str| -> i16 {
                t.trim().parse().unwrap_or_else(|e| panic!("CELESTE_WIN_AT_XY {} coordinate {:?}: {}", which, t, e))
            };
            match s.split_once("..") {
                Some((lo, hi)) => {
                    let (lo, hi) = (one(lo), one(hi));
                    assert!(lo <= hi, "CELESTE_WIN_AT_XY {which}: {lo}..{hi} is empty");
                    (lo, hi)
                }
                None => (one(s), one(s)),
            }
        };
        let ((xl, xh), (yl, yh)) = (parse(x, "x"), parse(y, "y"));
        println!(
            "SYNTHETIC WIN: a lane counts as won at player x {xl}..={xh}, y {yl}..={yh}, NOT at the \
             room exit - this is a test configuration and is in the fingerprint"
        );
        Some((xl, xh, yl, yh))
    })
}
