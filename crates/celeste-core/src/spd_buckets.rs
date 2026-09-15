//! THE SPEED BUCKETS of a bucketed level (2026-09-15).
//!
//! A level that buckets the player's speed keys each row on its bucket and
//! stores the tight speed actually merged into it (the speed hull). The
//! buckets are the cells of an EDGE TABLE per axis: the uniform grid of
//! width `2^w` raw units (the rung's grid: 1 px at level 0), plus the
//! constants the cart compares the speed against, so that every such
//! comparison is decided per bucket and the kernel of a bucket never
//! meets a condition it cannot decide; plus SINGLETON buckets - one raw
//! unit wide - at the values whose equality the cart tests (`spd ~= 0`)
//! and at the preimages of those under this frame's shifts (the facing
//! test reads `spd.x` after `appr` added or subtracted the accel). The
//! set is the cart's, not a room's: `celeste-minimal.lua`, the player's
//! update, spikes, the spring and the spawn.
//!
//! A finer rung's grid contains the coarser's, and the thresholds are the
//! same at every rung, so a finer level's buckets refine a coarser
//! level's (the ladder's nesting).

use crate::pico8_num::Pico8Num;
use std::collections::BTreeSet;
use std::sync::{Mutex, OnceLock};

/// A threshold and the comparison it serves. A bucket is closed at its
/// low end, so `spd > t` / `spd <= t` need the edge one raw unit ABOVE
/// `t` (then `t` itself is in the bucket below and the test is decided
/// on both sides); `spd >= t` / `spd < t` need it AT `t`; an equality
/// test needs `t` to be a bucket of its own (both edges).
#[derive(Clone, Copy)]
enum Cut {
    /// `spd > t`, `spd <= t`: edge at `t + 1`.
    Above,
    /// `spd >= t`, `spd < t`: edge at `t`.
    At,
    /// `spd == t`, `spd ~= t`: `{t}` a bucket of its own.
    Point,
}

/// The x-axis thresholds of `celeste-minimal.lua`'s player update.
const X_CUTS: &[(&str, Cut)] = &[
    // `spd.x ~= 0`, `sign(spd.x)` (`> 0`, `< 0`), the spikes' `xspd <= 0` / `>= 0`.
    ("0", Cut::Point),
    // The facing test `spd.x ~= 0` / `spd.x < 0` runs on `spd.x` AFTER
    // `appr` added or subtracted the accel (0.6 ground, 0.4 air, 0.05
    // ice, 0.15 decel): decided per bucket iff the preimages of 0 are
    // buckets of their own.
    ("0.6", Cut::Point), ("-0.6", Cut::Point), ("0.4", Cut::Point), ("-0.4", Cut::Point),
    ("0.15", Cut::Point), ("-0.15", Cut::Point), ("0.05", Cut::Point), ("-0.05", Cut::Point),
    // `abs(spd.x) > maxrun` (1) and `appr`'s `val > target` with target
    // `input * maxrun`: strict above 1. Below -1 the two disagree: `abs`
    // flips the test to `spd.x < -1` (edge AT -1) while `appr` keeps `val >
    // -1` (edge above it), so -1 is a point. With `Above` alone -1 shared
    // a bucket with (-1.5, -1), `abs(spd.x) > 1` was undecided there, and
    // only the 1 px grid's edge at -1 had hidden it: at `s20` a hull
    // reaching -1 merged both arms past its fork's arity and the kernel
    // declined it (room (1,0) f33, 2026-09-16).
    ("1", Cut::Above), ("-1", Cut::Point),
    // Mid-dash `appr(spd.x, dash_target.x, ..)`: `val > target`, target
    // in {0, +-1.5, +-2}; the wall jump writes +-2.
    ("1.5", Cut::Above), ("-1.5", Cut::Above), ("2", Cut::Above), ("-2", Cut::Above),
];
/// The y-axis thresholds.
const Y_CUTS: &[(&str, Cut)] = &[
    // `spd.y ~= 0`, `sign(spd.y)`, the spikes' `yspd >= 0` / `<= 0`, the spring's `>= 0`.
    ("0", Cut::Point),
    // The spawn's `spd.y + 0.5 > 0`: the preimage of 0 under +0.5, strict.
    ("-0.5", Cut::Above),
    // `abs(spd.y) <= 0.15` (gravity halving): `spd.y <= 0.15` and `spd.y >= -0.15`.
    ("0.15", Cut::Above), ("-0.15", Cut::At),
    // `appr(spd.y, maxfall, ..)`: `val > maxfall`, maxfall 2 or 0.4 (wall slide).
    ("2", Cut::Above), ("0.4", Cut::Above),
    // Mid-dash targets {0, +-1.5, +-2} (`.75 * 2` upward); the jump writes -2.
    ("1.5", Cut::Above), ("-1.5", Cut::Above), ("-2", Cut::Above),
];
/// The grid's extent: speeds are within ±16 px/frame (the boundary's
/// sanity range).
const EXTENT_PX: i32 = 16;

fn build(w: u8, axis: usize) -> Vec<i32> {
    let raw = |s: &str| -> i32 { s.parse::<Pico8Num>().expect("speed constant").as_raw_u32() as i32 };
    let mut es: BTreeSet<i32> = BTreeSet::new();
    let step = 1i32 << w;
    let mut v = -(EXTENT_PX << 16);
    while v <= EXTENT_PX << 16 {
        es.insert(v);
        v += step;
    }
    for (s, cut) in if axis == 0 { X_CUTS } else { Y_CUTS } {
        let t = raw(s);
        match cut {
            Cut::Above => {
                es.insert(t + 1);
            }
            Cut::At => {
                es.insert(t);
            }
            Cut::Point => {
                es.insert(t);
                es.insert(t + 1);
            }
        }
    }
    es.into_iter().collect()
}

/// The edge table of axis `axis` (0 = x, 1 = y) at grid width `2^w` raw:
/// sorted; bucket `j` is `[edges[j-1], edges[j] - 1]`, the two ends open.
pub fn edges(w: u8, axis: usize) -> &'static [i32] {
    static TABLES: OnceLock<Mutex<Vec<(u8, usize, &'static [i32])>>> = OnceLock::new();
    let tables = TABLES.get_or_init(|| Mutex::new(Vec::new()));
    let mut t = tables.lock().unwrap();
    if let Some((_, _, e)) = t.iter().find(|(ww, a, _)| *ww == w && *a == axis) {
        return e;
    }
    let e: &'static [i32] = Box::leak(build(w, axis).into_boxed_slice());
    t.push((w, axis, e));
    e
}

/// The index of the bucket holding raw speed `v` (0 = below every edge).
pub fn index(v: i32, w: u8, axis: usize) -> u16 {
    edges(w, axis).partition_point(|&e| e <= v) as u16
}

/// Bucket `i`'s range, inclusive.
pub fn range(i: u16, w: u8, axis: usize) -> (i32, i32) {
    let e = edges(w, axis);
    let lo = if i == 0 { i32::MIN } else { e[i as usize - 1] };
    let hi = if i as usize == e.len() { i32::MAX } else { e[i as usize] - 1 };
    (lo, hi)
}

/// The bucket holding raw speed `v`, inclusive.
pub fn bucket(v: i32, w: u8, axis: usize) -> (i32, i32) {
    range(index(v, w, axis), w, axis)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn buckets_tile_the_line_and_nest_across_rungs() {
        for axis in 0..2 {
            let e = edges(16, axis);
            assert!(e.windows(2).all(|w| w[0] < w[1]));
            // Every raw value is in exactly the bucket `index` names.
            for v in [-(20 << 16), -65536, -1, 0, 1, 3276, 3277, 9830, 65535, 65536, 98304, 5 << 16] {
                let (lo, hi) = bucket(v, 16, axis);
                assert!(lo <= v && v <= hi, "{v} in [{lo}, {hi}]");
            }
            // {0} is its own bucket.
            assert_eq!(bucket(0, 16, axis), (0, 0));
            // A finer rung's edges contain the coarser's.
            let fine = edges(15, axis);
            assert!(e.iter().all(|x| fine.binary_search(x).is_ok()));
        }
        // The facing preimages are points; the strict thresholds put
        // `t` in the bucket below (`spd > 1` is false at exactly 1).
        assert_eq!(bucket(-39321, 16, 0), (-39321, -39321));
        assert_eq!(bucket(65536, 16, 0).1, 65536);
        assert_eq!(bucket(65537, 16, 0).0, 65537);
        // y: `spd.y <= 0.15` decided on both sides of 0.15.
        assert_eq!(bucket(9830, 16, 1).1, 9830);
        // x: `abs(spd.x) > 1` and `spd.x > -1` are both decided at -1 on
        // every grid, not only where the grid has an edge there.
        for w in [16, 18, 20] {
            assert_eq!(bucket(-65536, w, 0), (-65536, -65536), "w {w}");
        }
    }
}
