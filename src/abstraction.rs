//! The LEVEL a forward runs at (`Level`, a value: the engines, trees and
//! searches carry it): which of the room's objects it widens. Its entries
//! are `celeste_engine::widening::TABLE`'s, interpreted by `trace::widen`
//! and `Rt2::widen_to`. (The synthetic win target is a setting, `--win-at`:
//! `frame::win_rect`.)

pub use celeste_engine::widening::Level;

