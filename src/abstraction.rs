//! The LEVEL a forward runs at (`Level`, a value: the engines, trees and
//! searches carry it): which of the room's objects it widens. Its entries
//! are `celeste_engine::widening::TABLE`'s, interpreted by `trace::widen`
//! and `Rt2::widen_to`. And the synthetic win target.

pub use celeste_engine::widening::Level;

/// A SYNTHETIC win target for cheap end-to-end tests (`--win-at`): `x,y`
/// makes "won" mean "the player is at whole-pixel (x, y)" instead of "the
/// player left the room", so the pipeline sees real wins at a short horizon;
/// `x0..x1,y0..y1` (inclusive) makes it "the player is in that rectangle"
/// (can a region be reached at all?). Returns (x_lo, x_hi, y_lo, y_hi).
///
/// A tree made under a synthetic win is a different search: its settings
/// record says so (`settings::Settings::check`).
pub fn synthetic_win_rect() -> Option<(i16, i16, i16, i16)> {
    let rect = crate::settings::get().win_at;
    if let Some((xl, xh, yl, yh)) = rect {
        static SAID: std::sync::Once = std::sync::Once::new();
        SAID.call_once(|| {
            println!(
                "SYNTHETIC WIN: a lane counts as won at player x {xl}..={xh}, y {yl}..={yh}, NOT at the \
                 room exit - this is a test configuration and is in the fingerprint"
            )
        });
    }
    rect
}
