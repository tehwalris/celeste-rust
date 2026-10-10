//! The LEVEL a forward runs at (`Level`, a value: the engines, trees and
//! searches carry it): which of the room's objects it widens. Its entries
//! are `celeste_engine::widening::TABLE`'s, interpreted by `trace::widen`
//! and `Rt2::widen_to`. And the synthetic win target.

pub use celeste_engine::widening::Level;

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
        let raw = crate::knobs::value("CELESTE_WIN_AT_XY")?;
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
