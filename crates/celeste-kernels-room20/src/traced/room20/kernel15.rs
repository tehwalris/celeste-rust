// GENERATED from a TRACED frame (shape 15). Do not edit.
//
// One input shape, 4 output shapes, 29 distinct button
// assignments. See `trace::kernel` for what this interface is
// and why it is not the walk's.
#![allow(unused_variables, unused_mut, unused_imports, clippy::all)]
use celeste_engine::kernel::*;
use celeste_core::pico8_num::{Pico8Num as P8, Pico8NumInterval as IV};
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2::{Rt2, Col, AV};
use celeste_engine::slots::{build_block, resolve_path, SCell};
use std::sync::Arc;

pub struct G<'a> { pub cart: &'a CartData, pub cache: &'a CollisionCache }

/// The canonical shape this kernel was traced for.
pub const SHAPE: u64 = 5116667838449112239;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
    ("objects[1].hitbox.h", "num"),
    ("objects[1].hitbox.w", "num"),
    ("objects[1].hitbox.x", "num"),
    ("objects[1].hitbox.y", "num"),
    ("objects[2].hitbox.h", "num"),
    ("objects[2].hitbox.w", "num"),
    ("objects[2].hitbox.x", "num"),
    ("objects[2].hitbox.y", "num"),
    ("objects[3].hitbox.h", "num"),
    ("objects[3].hitbox.w", "num"),
    ("objects[3].hitbox.x", "num"),
    ("objects[3].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c360: P8,
    pub c361: P8,
    pub c362: P8,
    pub c363: P8,
    pub c370: P8,
    pub c371: P8,
    pub c372: P8,
    pub c373: P8,
    pub c382: P8,
    pub c383: P8,
    pub c384: P8,
    pub c385: P8,
    pub c392: P8,
    pub c393: P8,
    pub c394: P8,
    pub c395: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("minutes", "num"),
    ("objects[0].collideable", "bool"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].start", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "ival"),
    ("objects[1].collideable", "bool"),
    ("objects[1].delay", "num"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].state", "num"),
    ("objects[1].target.x", "num"),
    ("objects[1].target.y", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
    ("objects[2].collideable", "bool"),
    ("objects[2].flip.x", "bool"),
    ("objects[2].flip.y", "bool"),
    ("objects[2].hide_for", "num"),
    ("objects[2].hide_in", "num"),
    ("objects[2].rem.x", "num"),
    ("objects[2].rem.y", "num"),
    ("objects[2].solids", "bool"),
    ("objects[2].spd.x", "num"),
    ("objects[2].spd.y", "num"),
    ("objects[2].spr", "num"),
    ("objects[2].x", "num"),
    ("objects[2].y", "num"),
    ("objects[3].collideable", "bool"),
    ("objects[3].flip.x", "bool"),
    ("objects[3].flip.y", "bool"),
    ("objects[3].hide_for", "num"),
    ("objects[3].hide_in", "num"),
    ("objects[3].rem.x", "num"),
    ("objects[3].rem.y", "num"),
    ("objects[3].solids", "bool"),
    ("objects[3].spd.x", "num"),
    ("objects[3].spd.y", "num"),
    ("objects[3].spr", "num"),
    ("objects[3].x", "num"),
    ("objects[3].y", "num"),
    ("pause_player", "bool"),
    ("seconds", "num"),
    ("will_restart", "bool"),
];

/// Per-lane inputs, in `ROW_SLOTS` order.
pub struct RowsIn {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c238: u16,
    pub c358: u16,
    pub c359: u16,
    pub c364: ZN,
    pub c365: ZN,
    pub c248: u16,
    pub c366: ZN,
    pub c367: ZN,
    pub c250: ZN,
    pub c251: ZN,
    pub c253: ZN,
    pub c254: ZI,
    pub c257: u16,
    pub c258: ZN,
    pub c368: u16,
    pub c369: u16,
    pub c374: ZN,
    pub c375: ZN,
    pub c267: u16,
    pub c376: ZN,
    pub c377: ZN,
    pub c269: ZN,
    pub c270: ZN,
    pub c378: ZN,
    pub c379: ZN,
    pub c273: ZN,
    pub c274: ZN,
    pub c277: u16,
    pub c380: u16,
    pub c381: u16,
    pub c279: ZN,
    pub c280: ZN,
    pub c386: ZN,
    pub c387: ZN,
    pub c288: u16,
    pub c388: ZN,
    pub c389: ZN,
    pub c290: ZN,
    pub c292: ZN,
    pub c293: ZN,
    pub c296: u16,
    pub c390: u16,
    pub c391: u16,
    pub c298: ZN,
    pub c299: ZN,
    pub c396: ZN,
    pub c397: ZN,
    pub c307: u16,
    pub c398: ZN,
    pub c399: ZN,
    pub c309: ZN,
    pub c311: ZN,
    pub c312: ZN,
    pub c43: u16,
    pub c85: ZN,
    pub c38: u16,
}

/// Cell ids for `ROW_SLOTS` in the bound block.
pub struct RowSlots {
    pub c87: u32,
    pub c39: u32,
    pub c84: u32,
    pub c20: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c238: u32,
    pub c358: u32,
    pub c359: u32,
    pub c364: u32,
    pub c365: u32,
    pub c248: u32,
    pub c366: u32,
    pub c367: u32,
    pub c250: u32,
    pub c251: u32,
    pub c253: u32,
    pub c254: u32,
    pub c257: u32,
    pub c258: u32,
    pub c368: u32,
    pub c369: u32,
    pub c374: u32,
    pub c375: u32,
    pub c267: u32,
    pub c376: u32,
    pub c377: u32,
    pub c269: u32,
    pub c270: u32,
    pub c378: u32,
    pub c379: u32,
    pub c273: u32,
    pub c274: u32,
    pub c277: u32,
    pub c380: u32,
    pub c381: u32,
    pub c279: u32,
    pub c280: u32,
    pub c386: u32,
    pub c387: u32,
    pub c288: u32,
    pub c388: u32,
    pub c389: u32,
    pub c290: u32,
    pub c292: u32,
    pub c293: u32,
    pub c296: u32,
    pub c390: u32,
    pub c391: u32,
    pub c298: u32,
    pub c299: u32,
    pub c396: u32,
    pub c397: u32,
    pub c307: u32,
    pub c398: u32,
    pub c399: u32,
    pub c309: u32,
    pub c311: u32,
    pub c312: u32,
    pub c43: u32,
    pub c85: u32,
    pub c38: u32,
}

/// Bind to a block by PATH. `None` means this block does not
/// have a slot the kernel needs, or holds it at the wrong kind -
/// either way the block takes the interpreter path.
pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {
    let cell = |p: &str| resolve_path(b, p).ok();
    let u = Uni {
        c360: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c361: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c362: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c363: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c370: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c371: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c372: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c373: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c382: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c383: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c384: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c385: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c392: match &b.cols[cell("objects[3].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c393: match &b.cols[cell("objects[3].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c394: match &b.cols[cell("objects[3].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c395: match &b.cols[cell("objects[3].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c238: cell("objects[0].collideable")?,
        c358: cell("objects[0].flip.x")?,
        c359: cell("objects[0].flip.y")?,
        c364: cell("objects[0].rem.x")?,
        c365: cell("objects[0].rem.y")?,
        c248: cell("objects[0].solids")?,
        c366: cell("objects[0].spd.x")?,
        c367: cell("objects[0].spd.y")?,
        c250: cell("objects[0].spr")?,
        c251: cell("objects[0].start")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
        c257: cell("objects[1].collideable")?,
        c258: cell("objects[1].delay")?,
        c368: cell("objects[1].flip.x")?,
        c369: cell("objects[1].flip.y")?,
        c374: cell("objects[1].rem.x")?,
        c375: cell("objects[1].rem.y")?,
        c267: cell("objects[1].solids")?,
        c376: cell("objects[1].spd.x")?,
        c377: cell("objects[1].spd.y")?,
        c269: cell("objects[1].spr")?,
        c270: cell("objects[1].state")?,
        c378: cell("objects[1].target.x")?,
        c379: cell("objects[1].target.y")?,
        c273: cell("objects[1].x")?,
        c274: cell("objects[1].y")?,
        c277: cell("objects[2].collideable")?,
        c380: cell("objects[2].flip.x")?,
        c381: cell("objects[2].flip.y")?,
        c279: cell("objects[2].hide_for")?,
        c280: cell("objects[2].hide_in")?,
        c386: cell("objects[2].rem.x")?,
        c387: cell("objects[2].rem.y")?,
        c288: cell("objects[2].solids")?,
        c388: cell("objects[2].spd.x")?,
        c389: cell("objects[2].spd.y")?,
        c290: cell("objects[2].spr")?,
        c292: cell("objects[2].x")?,
        c293: cell("objects[2].y")?,
        c296: cell("objects[3].collideable")?,
        c390: cell("objects[3].flip.x")?,
        c391: cell("objects[3].flip.y")?,
        c298: cell("objects[3].hide_for")?,
        c299: cell("objects[3].hide_in")?,
        c396: cell("objects[3].rem.x")?,
        c397: cell("objects[3].rem.y")?,
        c307: cell("objects[3].solids")?,
        c398: cell("objects[3].spd.x")?,
        c399: cell("objects[3].spd.y")?,
        c309: cell("objects[3].spr")?,
        c311: cell("objects[3].x")?,
        c312: cell("objects[3].y")?,
        c43: cell("pause_player")?,
        c85: cell("seconds")?,
        c38: cell("will_restart")?,
    };
    Some((u, s))
}

/// Which slot stopped `bind`. `None` means it would have bound.
pub fn bind_why(b: &Rt2) -> Option<String> {
    for (p, kind) in UNI_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let ok = matches!(
            (*kind, col),
            ("num", Col::U(AV::Num(_))) | ("bool", Col::U(AV::Bool(_)))
        );
        if !ok {
            return Some(format!(
                "{}: block-uniform {} slot holds {:?}",
                p, kind, col
            ));
        }
    }
    for (p, kind) in ROW_SLOTS {
        let Ok(c) = resolve_path(b, p) else {
            return Some(format!("{}: no such slot", p));
        };
        let col = &b.cols[c as usize];
        let lanes = (0..b.width).map(|i| col.at(i));
        let ok = match *kind {
            "num" => lanes.clone().all(|v| matches!(v, AV::Num(_))),
            _ => lanes.clone().all(|v| matches!(v, AV::Bool(_))),
        };
        if !ok {
            let bad = lanes.enumerate().find(|(_, v)| match *kind {
                "num" => !matches!(v, AV::Num(_)),
                _ => !matches!(v, AV::Bool(_)),
            });
            return Some(format!(
                "{}: per-lane {} slot holds {:?} at lane {:?}",
                p, kind, bad.map(|(_, v)| v), bad.map(|(i, _)| i)
            ));
        }
    }
    // Every slot resolves and every lane is the right kind, so
    // what stopped `rows` is REPRESENTATION: a column whose
    // values are right but whose storage the gather does not
    // accept (a `Col::V` of numbers where it wants `Col::N`).
    // `collapse_uniform_cols` is the usual missing step.
    None
}

/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by
/// repeating the last row; padded lanes are ignored by width.
pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {
    let at = |i: usize| -> usize { (lo + i).min(b.width - 1) };
    Some(RowsIn {
        c87: match &b.cols[s.c87 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c39: match &b.cols[s.c39 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c84: match &b.cols[s.c84 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c20: match &b.cols[s.c20 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c41: match &b.cols[s.c41 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c42: match &b.cols[s.c42 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c88: match &b.cols[s.c88 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c86: match &b.cols[s.c86 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c238: match &b.cols[s.c238 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c358: match &b.cols[s.c358 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c359: match &b.cols[s.c359 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c364: match &b.cols[s.c364 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c365: match &b.cols[s.c365 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c366: match &b.cols[s.c366 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c367: match &b.cols[s.c367 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c250: match &b.cols[s.c250 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c251: match &b.cols[s.c251 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c254: match &b.cols[s.c254 as usize] {
            Col::I(v) => ZI {
                lo: ZN::from_array(core::array::from_fn(|i| v[at(i)].0)),
                hi: ZN::from_array(core::array::from_fn(|i| v[at(i)].1)),
            },
            Col::U(AV::Ival(lo, hi)) => ZI { lo: zn_splat(*lo), hi: zn_splat(*hi) },
            Col::N(v) => ZI {
                lo: ZN::from_array(core::array::from_fn(|i| v[at(i)])),
                hi: ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            },
            Col::U(AV::Num(n)) => ZI { lo: zn_splat(*n), hi: zn_splat(*n) },
            _ => return None,
        },
        c257: match &b.cols[s.c257 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c258: match &b.cols[s.c258 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c368: match &b.cols[s.c368 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c369: match &b.cols[s.c369 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c374: match &b.cols[s.c374 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c375: match &b.cols[s.c375 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c267: match &b.cols[s.c267 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c376: match &b.cols[s.c376 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c377: match &b.cols[s.c377 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c269: match &b.cols[s.c269 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c270: match &b.cols[s.c270 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c378: match &b.cols[s.c378 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c379: match &b.cols[s.c379 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c274: match &b.cols[s.c274 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c277: match &b.cols[s.c277 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c380: match &b.cols[s.c380 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c381: match &b.cols[s.c381 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c280: match &b.cols[s.c280 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c386: match &b.cols[s.c386 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c387: match &b.cols[s.c387 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c288: match &b.cols[s.c288 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c388: match &b.cols[s.c388 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c389: match &b.cols[s.c389 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c290: match &b.cols[s.c290 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c292: match &b.cols[s.c292 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c293: match &b.cols[s.c293 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c296: match &b.cols[s.c296 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c390: match &b.cols[s.c390 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c391: match &b.cols[s.c391 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c298: match &b.cols[s.c298 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c299: match &b.cols[s.c299 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c396: match &b.cols[s.c396 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c397: match &b.cols[s.c397 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c307: match &b.cols[s.c307 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c398: match &b.cols[s.c398 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c399: match &b.cols[s.c399 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c309: match &b.cols[s.c309 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c312: match &b.cols[s.c312 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c43: match &b.cols[s.c43 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
        c85: match &b.cols[s.c85 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c38: match &b.cols[s.c38 as usize] {
            Col::V(v) => {
                let mut m = 0u16;
                for i in 0..W {
                    match v[at(i)] {
                        AV::Bool(true) => m |= 1 << i,
                        AV::Bool(false) => {}
                        _ => return None,
                    }
                }
                m
            }
            Col::U(AV::Bool(t)) => if *t { 0xffff } else { 0 },
            _ => return None,
        },
    })
}

// ---------------- outcome 0 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_0: &[(u32, &str)] = &[
    (177, "balloon.tile"),
    (204, "big_chest.tile"),
    (196, "chest.if_not_fruit"),
    (198, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (190, "fake_wall.if_not_fruit"),
    (191, "fake_wall.tile"),
    (180, "fall_floor.tile"),
    (186, "fly_fruit.if_not_fruit"),
    (188, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (182, "fruit.if_not_fruit"),
    (184, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (193, "key.if_not_fruit"),
    (194, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (238, "objects[0].collideable"),
    (364, "objects[0].flip.x"),
    (365, "objects[0].flip.y"),
    (366, "objects[0].hitbox.h"),
    (367, "objects[0].hitbox.w"),
    (368, "objects[0].hitbox.x"),
    (369, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (370, "objects[0].rem.x"),
    (371, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (372, "objects[0].spd.x"),
    (373, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (374, "objects[1].flip.x"),
    (375, "objects[1].flip.y"),
    (259, "objects[1].hide_for"),
    (260, "objects[1].hide_in"),
    (376, "objects[1].hitbox.h"),
    (377, "objects[1].hitbox.w"),
    (378, "objects[1].hitbox.x"),
    (379, "objects[1].hitbox.y"),
    (380, "objects[1].rem.x"),
    (381, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (382, "objects[1].spd.x"),
    (383, "objects[1].spd.y"),
    (270, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (276, "objects[2].collideable"),
    (384, "objects[2].flip.x"),
    (385, "objects[2].flip.y"),
    (278, "objects[2].hide_for"),
    (279, "objects[2].hide_in"),
    (386, "objects[2].hitbox.h"),
    (387, "objects[2].hitbox.w"),
    (388, "objects[2].hitbox.x"),
    (389, "objects[2].hitbox.y"),
    (390, "objects[2].rem.x"),
    (391, "objects[2].rem.y"),
    (287, "objects[2].solids"),
    (392, "objects[2].spd.x"),
    (393, "objects[2].spd.y"),
    (289, "objects[2].spr"),
    (291, "objects[2].x"),
    (292, "objects[2].y"),
    (295, "objects[3].collideable"),
    (394, "objects[3].dash_accel.x"),
    (395, "objects[3].dash_accel.y"),
    (297, "objects[3].dash_effect_time"),
    (396, "objects[3].dash_target.x"),
    (397, "objects[3].dash_target.y"),
    (299, "objects[3].dash_time"),
    (300, "objects[3].djump"),
    (398, "objects[3].flip.x"),
    (399, "objects[3].flip.y"),
    (302, "objects[3].grace"),
    (400, "objects[3].hitbox.h"),
    (401, "objects[3].hitbox.w"),
    (402, "objects[3].hitbox.x"),
    (403, "objects[3].hitbox.y"),
    (309, "objects[3].p_dash"),
    (310, "objects[3].p_jump"),
    (404, "objects[3].rem.x"),
    (405, "objects[3].rem.y"),
    (312, "objects[3].solids"),
    (406, "objects[3].spd.x"),
    (407, "objects[3].spd.y"),
    (316, "objects[3].x"),
    (317, "objects[3].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_0: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_0: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154]),
    SCell::Obj(&[(7, 155), (5, 156), (6, 157)]),
    SCell::Obj(&[(5, 158), (8, 159), (6, 160)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 161), (2, 162)]),
    SCell::Arr(&[163, 164, 165, 166, 167, 168, 169, 170, 171, 172]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 182), (5, 183), (8, 184), (6, 185)]),
    SCell::Obj(&[(9, 186), (5, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(5, 200), (6, 201)]),
    SCell::Obj(&[(7, 202), (5, 203), (8, 204)]),
    SCell::Obj(&[(7, 205), (5, 206)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 236), (20, 237), (11, 238), (14, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (41, 246), (4, 247), (12, 248), (3, 249), (13, 250), (38, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (14, 258), (45, 259), (42, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
    SCell::Obj(&[(21, 274), (20, 275), (11, 276), (14, 277), (45, 278), (42, 279), (15, 280), (19, 281), (18, 282), (22, 283), (23, 284), (24, 285), (4, 286), (12, 287), (3, 288), (13, 289), (0, 290), (1, 291), (2, 292)]),
    SCell::Obj(&[(21, 293), (20, 294), (11, 295), (28, 296), (33, 297), (27, 298), (26, 299), (30, 300), (14, 301), (29, 302), (15, 303), (19, 304), (18, 305), (22, 306), (23, 307), (24, 308), (32, 309), (31, 310), (4, 311), (12, 312), (3, 313), (13, 314), (0, 315), (1, 316), (2, 317)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 364), (2, 365)]),
    SCell::Obj(&[(17, 366), (16, 367), (1, 368), (2, 369)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 370), (2, 371)]),
    SCell::Obj(&[(1, 372), (2, 373)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Obj(&[(17, 376), (16, 377), (1, 378), (2, 379)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 380), (2, 381)]),
    SCell::Obj(&[(1, 382), (2, 383)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 384), (2, 385)]),
    SCell::Obj(&[(17, 386), (16, 387), (1, 388), (2, 389)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 390), (2, 391)]),
    SCell::Obj(&[(1, 392), (2, 393)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 394), (2, 395)]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Obj(&[(17, 400), (16, 401), (1, 402), (2, 403)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 404), (2, 405)]),
    SCell::Obj(&[(1, 406), (2, 407)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_0: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_0: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 207),
    (152, 208),
    (153, 209),
    (154, 210),
    (155, 211),
    (156, 212),
    (157, 213),
    (158, 214),
    (160, 215),
    (163, 94),
    (164, 116),
    (165, 118),
    (166, 119),
    (167, 121),
    (168, 122),
    (169, 123),
    (170, 124),
    (171, 125),
    (172, 127),
    (173, 216),
    (175, 217),
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (183, 222),
    (185, 223),
    (187, 224),
    (189, 225),
    (192, 226),
    (195, 227),
    (197, 228),
    (199, 229),
    (200, 230),
    (201, 231),
    (202, 232),
    (203, 233),
    (205, 234),
    (206, 235),
    (236, 318),
    (237, 319),
    (239, 320),
    (240, 321),
    (241, 322),
    (242, 323),
    (243, 324),
    (244, 325),
    (245, 326),
    (247, 327),
    (249, 328),
    (252, 121),
    (255, 329),
    (256, 330),
    (258, 331),
    (261, 332),
    (262, 333),
    (263, 334),
    (264, 335),
    (265, 336),
    (266, 337),
    (267, 338),
    (269, 339),
    (271, 116),
    (274, 340),
    (275, 341),
    (277, 342),
    (280, 343),
    (281, 344),
    (282, 345),
    (283, 346),
    (284, 347),
    (285, 348),
    (286, 349),
    (288, 350),
    (290, 116),
    (293, 351),
    (294, 352),
    (296, 353),
    (298, 354),
    (301, 355),
    (303, 356),
    (304, 357),
    (305, 358),
    (306, 359),
    (307, 360),
    (308, 361),
    (311, 362),
    (313, 363),
    (315, 93),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c39: ZN,
    pub c317: ZN,
}

/// Outcome 0's per-assignment values and lane masks.
/// The cells of outcome 0 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared0`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut0 {
    pub c20: ZN,
    pub c41: ZB,
    pub c394: ZN,
    pub c395: ZN,
    pub c297: ZN,
    pub c396: ZN,
    pub c397: ZN,
    pub c299: ZN,
    pub c300: ZN,
    pub c398: ZB,
    pub c302: ZN,
    pub c309: ZB,
    pub c310: ZB,
    pub c406: ZN,
    pub c407: ZN,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 1 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_1: &[(u32, &str)] = &[
    (177, "balloon.tile"),
    (204, "big_chest.tile"),
    (196, "chest.if_not_fruit"),
    (198, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (190, "fake_wall.if_not_fruit"),
    (191, "fake_wall.tile"),
    (180, "fall_floor.tile"),
    (186, "fly_fruit.if_not_fruit"),
    (188, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (182, "fruit.if_not_fruit"),
    (184, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (193, "key.if_not_fruit"),
    (194, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (237, "objects[0].collideable"),
    (325, "objects[0].flip.x"),
    (326, "objects[0].flip.y"),
    (327, "objects[0].hitbox.h"),
    (328, "objects[0].hitbox.w"),
    (329, "objects[0].hitbox.x"),
    (330, "objects[0].hitbox.y"),
    (245, "objects[0].off"),
    (331, "objects[0].rem.x"),
    (332, "objects[0].rem.y"),
    (247, "objects[0].solids"),
    (333, "objects[0].spd.x"),
    (334, "objects[0].spd.y"),
    (249, "objects[0].spr"),
    (250, "objects[0].start"),
    (252, "objects[0].x"),
    (253, "objects[0].y"),
    (256, "objects[1].collideable"),
    (335, "objects[1].flip.x"),
    (336, "objects[1].flip.y"),
    (258, "objects[1].hide_for"),
    (259, "objects[1].hide_in"),
    (337, "objects[1].hitbox.h"),
    (338, "objects[1].hitbox.w"),
    (339, "objects[1].hitbox.x"),
    (340, "objects[1].hitbox.y"),
    (341, "objects[1].rem.x"),
    (342, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (343, "objects[1].spd.x"),
    (344, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (271, "objects[1].x"),
    (272, "objects[1].y"),
    (275, "objects[2].collideable"),
    (345, "objects[2].flip.x"),
    (346, "objects[2].flip.y"),
    (277, "objects[2].hide_for"),
    (278, "objects[2].hide_in"),
    (347, "objects[2].hitbox.h"),
    (348, "objects[2].hitbox.w"),
    (349, "objects[2].hitbox.x"),
    (350, "objects[2].hitbox.y"),
    (351, "objects[2].rem.x"),
    (352, "objects[2].rem.y"),
    (286, "objects[2].solids"),
    (353, "objects[2].spd.x"),
    (354, "objects[2].spd.y"),
    (288, "objects[2].spr"),
    (290, "objects[2].x"),
    (291, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_1: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_1: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154]),
    SCell::Obj(&[(7, 155), (5, 156), (6, 157)]),
    SCell::Obj(&[(5, 158), (8, 159), (6, 160)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 161), (2, 162)]),
    SCell::Arr(&[163, 164, 165, 166, 167, 168, 169, 170, 171, 172]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 182), (5, 183), (8, 184), (6, 185)]),
    SCell::Obj(&[(9, 186), (5, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(5, 200), (6, 201)]),
    SCell::Obj(&[(7, 202), (5, 203), (8, 204)]),
    SCell::Obj(&[(7, 205), (5, 206)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 235), (20, 236), (11, 237), (14, 238), (15, 239), (19, 240), (18, 241), (22, 242), (23, 243), (24, 244), (41, 245), (4, 246), (12, 247), (3, 248), (13, 249), (38, 250), (0, 251), (1, 252), (2, 253)]),
    SCell::Obj(&[(21, 254), (20, 255), (11, 256), (14, 257), (45, 258), (42, 259), (15, 260), (19, 261), (18, 262), (22, 263), (23, 264), (24, 265), (4, 266), (12, 267), (3, 268), (13, 269), (0, 270), (1, 271), (2, 272)]),
    SCell::Obj(&[(21, 273), (20, 274), (11, 275), (14, 276), (45, 277), (42, 278), (15, 279), (19, 280), (18, 281), (22, 282), (23, 283), (24, 284), (4, 285), (12, 286), (3, 287), (13, 288), (0, 289), (1, 290), (2, 291)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 325), (2, 326)]),
    SCell::Obj(&[(17, 327), (16, 328), (1, 329), (2, 330)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 331), (2, 332)]),
    SCell::Obj(&[(1, 333), (2, 334)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 335), (2, 336)]),
    SCell::Obj(&[(17, 337), (16, 338), (1, 339), (2, 340)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 341), (2, 342)]),
    SCell::Obj(&[(1, 343), (2, 344)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 345), (2, 346)]),
    SCell::Obj(&[(17, 347), (16, 348), (1, 349), (2, 350)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 351), (2, 352)]),
    SCell::Obj(&[(1, 353), (2, 354)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_1: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_1: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 207),
    (152, 208),
    (153, 209),
    (155, 210),
    (156, 211),
    (157, 212),
    (158, 213),
    (160, 214),
    (163, 94),
    (164, 116),
    (165, 118),
    (166, 119),
    (167, 121),
    (168, 122),
    (169, 123),
    (170, 124),
    (171, 125),
    (172, 127),
    (173, 215),
    (175, 216),
    (176, 217),
    (178, 218),
    (179, 219),
    (181, 220),
    (183, 221),
    (185, 222),
    (187, 223),
    (189, 224),
    (192, 225),
    (195, 226),
    (197, 227),
    (199, 228),
    (200, 229),
    (201, 230),
    (202, 231),
    (203, 232),
    (205, 233),
    (206, 234),
    (235, 292),
    (236, 293),
    (238, 294),
    (239, 295),
    (240, 296),
    (241, 297),
    (242, 298),
    (243, 299),
    (244, 300),
    (246, 301),
    (248, 302),
    (251, 121),
    (254, 303),
    (255, 304),
    (257, 305),
    (260, 306),
    (261, 307),
    (262, 308),
    (263, 309),
    (264, 310),
    (265, 311),
    (266, 312),
    (268, 313),
    (270, 116),
    (273, 314),
    (274, 315),
    (276, 316),
    (279, 317),
    (280, 318),
    (281, 319),
    (282, 320),
    (283, 321),
    (284, 322),
    (285, 323),
    (287, 324),
    (289, 116),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
}

/// Outcome 1's per-assignment values and lane masks.
/// The cells of outcome 1 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared1`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut1 {
    pub c20: ZN,
    pub c41: ZB,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
    (187, "balloon.tile"),
    (214, "big_chest.tile"),
    (206, "chest.if_not_fruit"),
    (208, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (200, "fake_wall.if_not_fruit"),
    (201, "fake_wall.tile"),
    (190, "fall_floor.tile"),
    (196, "fly_fruit.if_not_fruit"),
    (198, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (192, "fruit.if_not_fruit"),
    (194, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (203, "key.if_not_fruit"),
    (204, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (258, "objects[0].collideable"),
    (259, "objects[0].delay"),
    (679, "objects[0].flip.x"),
    (680, "objects[0].flip.y"),
    (681, "objects[0].hitbox.h"),
    (682, "objects[0].hitbox.w"),
    (683, "objects[0].hitbox.x"),
    (684, "objects[0].hitbox.y"),
    (685, "objects[0].rem.x"),
    (686, "objects[0].rem.y"),
    (268, "objects[0].solids"),
    (687, "objects[0].spd.x"),
    (688, "objects[0].spd.y"),
    (270, "objects[0].spr"),
    (271, "objects[0].state"),
    (689, "objects[0].target.x"),
    (690, "objects[0].target.y"),
    (169, "objects[0].type.tile"),
    (274, "objects[0].x"),
    (275, "objects[0].y"),
    (278, "objects[1].collideable"),
    (691, "objects[1].flip.x"),
    (692, "objects[1].flip.y"),
    (280, "objects[1].fly"),
    (693, "objects[1].hitbox.h"),
    (694, "objects[1].hitbox.w"),
    (695, "objects[1].hitbox.x"),
    (696, "objects[1].hitbox.y"),
    (697, "objects[1].rem.x"),
    (698, "objects[1].rem.y"),
    (288, "objects[1].solids"),
    (699, "objects[1].spd.x"),
    (700, "objects[1].spd.y"),
    (290, "objects[1].spr"),
    (291, "objects[1].start"),
    (292, "objects[1].step"),
    (294, "objects[1].x"),
    (295, "objects[1].y"),
    (298, "objects[2].collideable"),
    (701, "objects[2].flip.x"),
    (702, "objects[2].flip.y"),
    (703, "objects[2].hitbox.h"),
    (704, "objects[2].hitbox.w"),
    (705, "objects[2].hitbox.x"),
    (706, "objects[2].hitbox.y"),
    (707, "objects[2].rem.x"),
    (708, "objects[2].rem.y"),
    (307, "objects[2].solid"),
    (308, "objects[2].solids"),
    (709, "objects[2].spd.x"),
    (710, "objects[2].spd.y"),
    (310, "objects[2].spr"),
    (311, "objects[2].state"),
    (313, "objects[2].x"),
    (314, "objects[2].y"),
    (317, "objects[3].collideable"),
    (711, "objects[3].flip.x"),
    (712, "objects[3].flip.y"),
    (713, "objects[3].hitbox.h"),
    (714, "objects[3].hitbox.w"),
    (715, "objects[3].hitbox.x"),
    (716, "objects[3].hitbox.y"),
    (717, "objects[3].rem.x"),
    (718, "objects[3].rem.y"),
    (326, "objects[3].solid"),
    (327, "objects[3].solids"),
    (719, "objects[3].spd.x"),
    (720, "objects[3].spd.y"),
    (329, "objects[3].spr"),
    (330, "objects[3].state"),
    (332, "objects[3].x"),
    (333, "objects[3].y"),
    (336, "objects[4].collideable"),
    (721, "objects[4].flip.x"),
    (722, "objects[4].flip.y"),
    (723, "objects[4].hitbox.h"),
    (724, "objects[4].hitbox.w"),
    (725, "objects[4].hitbox.x"),
    (726, "objects[4].hitbox.y"),
    (727, "objects[4].rem.x"),
    (728, "objects[4].rem.y"),
    (345, "objects[4].solid"),
    (346, "objects[4].solids"),
    (729, "objects[4].spd.x"),
    (730, "objects[4].spd.y"),
    (348, "objects[4].spr"),
    (349, "objects[4].state"),
    (351, "objects[4].x"),
    (352, "objects[4].y"),
    (355, "objects[5].collideable"),
    (731, "objects[5].flip.x"),
    (732, "objects[5].flip.y"),
    (733, "objects[5].hitbox.h"),
    (734, "objects[5].hitbox.w"),
    (735, "objects[5].hitbox.x"),
    (736, "objects[5].hitbox.y"),
    (737, "objects[5].rem.x"),
    (738, "objects[5].rem.y"),
    (364, "objects[5].solid"),
    (365, "objects[5].solids"),
    (739, "objects[5].spd.x"),
    (740, "objects[5].spd.y"),
    (367, "objects[5].spr"),
    (368, "objects[5].state"),
    (370, "objects[5].x"),
    (371, "objects[5].y"),
    (374, "objects[6].collideable"),
    (741, "objects[6].flip.x"),
    (742, "objects[6].flip.y"),
    (743, "objects[6].hitbox.h"),
    (744, "objects[6].hitbox.w"),
    (745, "objects[6].hitbox.x"),
    (746, "objects[6].hitbox.y"),
    (747, "objects[6].rem.x"),
    (748, "objects[6].rem.y"),
    (383, "objects[6].solid"),
    (384, "objects[6].solids"),
    (749, "objects[6].spd.x"),
    (750, "objects[6].spd.y"),
    (386, "objects[6].spr"),
    (387, "objects[6].state"),
    (389, "objects[6].x"),
    (390, "objects[6].y"),
    (393, "objects[7].collideable"),
    (751, "objects[7].flip.x"),
    (752, "objects[7].flip.y"),
    (753, "objects[7].hitbox.h"),
    (754, "objects[7].hitbox.w"),
    (755, "objects[7].hitbox.x"),
    (756, "objects[7].hitbox.y"),
    (757, "objects[7].rem.x"),
    (758, "objects[7].rem.y"),
    (402, "objects[7].solid"),
    (403, "objects[7].solids"),
    (759, "objects[7].spd.x"),
    (760, "objects[7].spd.y"),
    (405, "objects[7].spr"),
    (406, "objects[7].state"),
    (408, "objects[7].x"),
    (409, "objects[7].y"),
    (412, "objects[8].collideable"),
    (761, "objects[8].flip.x"),
    (762, "objects[8].flip.y"),
    (763, "objects[8].hitbox.h"),
    (764, "objects[8].hitbox.w"),
    (765, "objects[8].hitbox.x"),
    (766, "objects[8].hitbox.y"),
    (767, "objects[8].rem.x"),
    (768, "objects[8].rem.y"),
    (421, "objects[8].solid"),
    (422, "objects[8].solids"),
    (769, "objects[8].spd.x"),
    (770, "objects[8].spd.y"),
    (424, "objects[8].spr"),
    (425, "objects[8].state"),
    (427, "objects[8].x"),
    (428, "objects[8].y"),
    (431, "objects[9].collideable"),
    (771, "objects[9].flip.x"),
    (772, "objects[9].flip.y"),
    (773, "objects[9].hitbox.h"),
    (774, "objects[9].hitbox.w"),
    (775, "objects[9].hitbox.x"),
    (776, "objects[9].hitbox.y"),
    (777, "objects[9].rem.x"),
    (778, "objects[9].rem.y"),
    (440, "objects[9].solid"),
    (441, "objects[9].solids"),
    (779, "objects[9].spd.x"),
    (780, "objects[9].spd.y"),
    (443, "objects[9].spr"),
    (444, "objects[9].state"),
    (446, "objects[9].x"),
    (447, "objects[9].y"),
    (450, "objects[10].collideable"),
    (781, "objects[10].flip.x"),
    (782, "objects[10].flip.y"),
    (783, "objects[10].hitbox.h"),
    (784, "objects[10].hitbox.w"),
    (785, "objects[10].hitbox.x"),
    (786, "objects[10].hitbox.y"),
    (787, "objects[10].rem.x"),
    (788, "objects[10].rem.y"),
    (459, "objects[10].solid"),
    (460, "objects[10].solids"),
    (789, "objects[10].spd.x"),
    (790, "objects[10].spd.y"),
    (462, "objects[10].spr"),
    (463, "objects[10].state"),
    (465, "objects[10].x"),
    (466, "objects[10].y"),
    (469, "objects[11].collideable"),
    (791, "objects[11].flip.x"),
    (792, "objects[11].flip.y"),
    (793, "objects[11].hitbox.h"),
    (794, "objects[11].hitbox.w"),
    (795, "objects[11].hitbox.x"),
    (796, "objects[11].hitbox.y"),
    (797, "objects[11].rem.x"),
    (798, "objects[11].rem.y"),
    (478, "objects[11].solid"),
    (479, "objects[11].solids"),
    (799, "objects[11].spd.x"),
    (800, "objects[11].spd.y"),
    (481, "objects[11].spr"),
    (482, "objects[11].state"),
    (484, "objects[11].x"),
    (485, "objects[11].y"),
    (488, "objects[12].collideable"),
    (801, "objects[12].flip.x"),
    (802, "objects[12].flip.y"),
    (803, "objects[12].hitbox.h"),
    (804, "objects[12].hitbox.w"),
    (805, "objects[12].hitbox.x"),
    (806, "objects[12].hitbox.y"),
    (807, "objects[12].rem.x"),
    (808, "objects[12].rem.y"),
    (497, "objects[12].solid"),
    (498, "objects[12].solids"),
    (809, "objects[12].spd.x"),
    (810, "objects[12].spd.y"),
    (500, "objects[12].spr"),
    (501, "objects[12].state"),
    (503, "objects[12].x"),
    (504, "objects[12].y"),
    (507, "objects[13].collideable"),
    (811, "objects[13].flip.x"),
    (812, "objects[13].flip.y"),
    (813, "objects[13].hitbox.h"),
    (814, "objects[13].hitbox.w"),
    (815, "objects[13].hitbox.x"),
    (816, "objects[13].hitbox.y"),
    (817, "objects[13].rem.x"),
    (818, "objects[13].rem.y"),
    (516, "objects[13].solid"),
    (517, "objects[13].solids"),
    (819, "objects[13].spd.x"),
    (820, "objects[13].spd.y"),
    (519, "objects[13].spr"),
    (520, "objects[13].state"),
    (522, "objects[13].x"),
    (523, "objects[13].y"),
    (43, "pause_player"),
    (171, "room.x"),
    (172, "room.y"),
    (85, "seconds"),
    (184, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_2: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_2: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164]),
    SCell::Obj(&[(7, 165), (5, 166), (6, 167)]),
    SCell::Obj(&[(5, 168), (8, 169), (6, 170)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 171), (2, 172)]),
    SCell::Arr(&[173, 174, 175, 176, 177, 178, 179, 180, 181, 182]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 183), (8, 184), (6, 185)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(5, 189), (8, 190), (6, 191)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 192), (5, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(9, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(9, 203), (8, 204), (6, 205)]),
    SCell::Obj(&[(9, 206), (5, 207), (8, 208), (6, 209)]),
    SCell::Obj(&[(5, 210), (6, 211)]),
    SCell::Obj(&[(7, 212), (5, 213), (8, 214)]),
    SCell::Obj(&[(7, 215), (5, 216)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 256), (20, 257), (11, 258), (35, 259), (14, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (4, 267), (12, 268), (3, 269), (13, 270), (25, 271), (34, 272), (0, 273), (1, 274), (2, 275)]),
    SCell::Obj(&[(21, 276), (20, 277), (11, 278), (14, 279), (39, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (38, 291), (40, 292), (0, 293), (1, 294), (2, 295)]),
    SCell::Obj(&[(21, 296), (20, 297), (11, 298), (14, 299), (15, 300), (19, 301), (18, 302), (22, 303), (23, 304), (24, 305), (4, 306), (43, 307), (12, 308), (3, 309), (13, 310), (25, 311), (0, 312), (1, 313), (2, 314)]),
    SCell::Obj(&[(21, 315), (20, 316), (11, 317), (14, 318), (15, 319), (19, 320), (18, 321), (22, 322), (23, 323), (24, 324), (4, 325), (43, 326), (12, 327), (3, 328), (13, 329), (25, 330), (0, 331), (1, 332), (2, 333)]),
    SCell::Obj(&[(21, 334), (20, 335), (11, 336), (14, 337), (15, 338), (19, 339), (18, 340), (22, 341), (23, 342), (24, 343), (4, 344), (43, 345), (12, 346), (3, 347), (13, 348), (25, 349), (0, 350), (1, 351), (2, 352)]),
    SCell::Obj(&[(21, 353), (20, 354), (11, 355), (14, 356), (15, 357), (19, 358), (18, 359), (22, 360), (23, 361), (24, 362), (4, 363), (43, 364), (12, 365), (3, 366), (13, 367), (25, 368), (0, 369), (1, 370), (2, 371)]),
    SCell::Obj(&[(21, 372), (20, 373), (11, 374), (14, 375), (15, 376), (19, 377), (18, 378), (22, 379), (23, 380), (24, 381), (4, 382), (43, 383), (12, 384), (3, 385), (13, 386), (25, 387), (0, 388), (1, 389), (2, 390)]),
    SCell::Obj(&[(21, 391), (20, 392), (11, 393), (14, 394), (15, 395), (19, 396), (18, 397), (22, 398), (23, 399), (24, 400), (4, 401), (43, 402), (12, 403), (3, 404), (13, 405), (25, 406), (0, 407), (1, 408), (2, 409)]),
    SCell::Obj(&[(21, 410), (20, 411), (11, 412), (14, 413), (15, 414), (19, 415), (18, 416), (22, 417), (23, 418), (24, 419), (4, 420), (43, 421), (12, 422), (3, 423), (13, 424), (25, 425), (0, 426), (1, 427), (2, 428)]),
    SCell::Obj(&[(21, 429), (20, 430), (11, 431), (14, 432), (15, 433), (19, 434), (18, 435), (22, 436), (23, 437), (24, 438), (4, 439), (43, 440), (12, 441), (3, 442), (13, 443), (25, 444), (0, 445), (1, 446), (2, 447)]),
    SCell::Obj(&[(21, 448), (20, 449), (11, 450), (14, 451), (15, 452), (19, 453), (18, 454), (22, 455), (23, 456), (24, 457), (4, 458), (43, 459), (12, 460), (3, 461), (13, 462), (25, 463), (0, 464), (1, 465), (2, 466)]),
    SCell::Obj(&[(21, 467), (20, 468), (11, 469), (14, 470), (15, 471), (19, 472), (18, 473), (22, 474), (23, 475), (24, 476), (4, 477), (43, 478), (12, 479), (3, 480), (13, 481), (25, 482), (0, 483), (1, 484), (2, 485)]),
    SCell::Obj(&[(21, 486), (20, 487), (11, 488), (14, 489), (15, 490), (19, 491), (18, 492), (22, 493), (23, 494), (24, 495), (4, 496), (43, 497), (12, 498), (3, 499), (13, 500), (25, 501), (0, 502), (1, 503), (2, 504)]),
    SCell::Obj(&[(21, 505), (20, 506), (11, 507), (14, 508), (15, 509), (19, 510), (18, 511), (22, 512), (23, 513), (24, 514), (4, 515), (43, 516), (12, 517), (3, 518), (13, 519), (25, 520), (0, 521), (1, 522), (2, 523)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[217]),
    SCell::Clo(25, &[217]),
    SCell::Obj(&[(1, 679), (2, 680)]),
    SCell::Obj(&[(17, 681), (16, 682), (1, 683), (2, 684)]),
    SCell::Clo(24, &[217]),
    SCell::Clo(23, &[217]),
    SCell::Clo(27, &[217]),
    SCell::Clo(28, &[217]),
    SCell::Clo(29, &[217]),
    SCell::Obj(&[(1, 685), (2, 686)]),
    SCell::Obj(&[(1, 687), (2, 688)]),
    SCell::Obj(&[(1, 689), (2, 690)]),
    SCell::Clo(26, &[218]),
    SCell::Clo(25, &[218]),
    SCell::Obj(&[(1, 691), (2, 692)]),
    SCell::Obj(&[(17, 693), (16, 694), (1, 695), (2, 696)]),
    SCell::Clo(24, &[218]),
    SCell::Clo(23, &[218]),
    SCell::Clo(27, &[218]),
    SCell::Clo(28, &[218]),
    SCell::Clo(29, &[218]),
    SCell::Obj(&[(1, 697), (2, 698)]),
    SCell::Obj(&[(1, 699), (2, 700)]),
    SCell::Clo(26, &[219]),
    SCell::Clo(25, &[219]),
    SCell::Obj(&[(1, 701), (2, 702)]),
    SCell::Obj(&[(17, 703), (16, 704), (1, 705), (2, 706)]),
    SCell::Clo(24, &[219]),
    SCell::Clo(23, &[219]),
    SCell::Clo(27, &[219]),
    SCell::Clo(28, &[219]),
    SCell::Clo(29, &[219]),
    SCell::Obj(&[(1, 707), (2, 708)]),
    SCell::Obj(&[(1, 709), (2, 710)]),
    SCell::Clo(26, &[220]),
    SCell::Clo(25, &[220]),
    SCell::Obj(&[(1, 711), (2, 712)]),
    SCell::Obj(&[(17, 713), (16, 714), (1, 715), (2, 716)]),
    SCell::Clo(24, &[220]),
    SCell::Clo(23, &[220]),
    SCell::Clo(27, &[220]),
    SCell::Clo(28, &[220]),
    SCell::Clo(29, &[220]),
    SCell::Obj(&[(1, 717), (2, 718)]),
    SCell::Obj(&[(1, 719), (2, 720)]),
    SCell::Clo(26, &[221]),
    SCell::Clo(25, &[221]),
    SCell::Obj(&[(1, 721), (2, 722)]),
    SCell::Obj(&[(17, 723), (16, 724), (1, 725), (2, 726)]),
    SCell::Clo(24, &[221]),
    SCell::Clo(23, &[221]),
    SCell::Clo(27, &[221]),
    SCell::Clo(28, &[221]),
    SCell::Clo(29, &[221]),
    SCell::Obj(&[(1, 727), (2, 728)]),
    SCell::Obj(&[(1, 729), (2, 730)]),
    SCell::Clo(26, &[222]),
    SCell::Clo(25, &[222]),
    SCell::Obj(&[(1, 731), (2, 732)]),
    SCell::Obj(&[(17, 733), (16, 734), (1, 735), (2, 736)]),
    SCell::Clo(24, &[222]),
    SCell::Clo(23, &[222]),
    SCell::Clo(27, &[222]),
    SCell::Clo(28, &[222]),
    SCell::Clo(29, &[222]),
    SCell::Obj(&[(1, 737), (2, 738)]),
    SCell::Obj(&[(1, 739), (2, 740)]),
    SCell::Clo(26, &[223]),
    SCell::Clo(25, &[223]),
    SCell::Obj(&[(1, 741), (2, 742)]),
    SCell::Obj(&[(17, 743), (16, 744), (1, 745), (2, 746)]),
    SCell::Clo(24, &[223]),
    SCell::Clo(23, &[223]),
    SCell::Clo(27, &[223]),
    SCell::Clo(28, &[223]),
    SCell::Clo(29, &[223]),
    SCell::Obj(&[(1, 747), (2, 748)]),
    SCell::Obj(&[(1, 749), (2, 750)]),
    SCell::Clo(26, &[224]),
    SCell::Clo(25, &[224]),
    SCell::Obj(&[(1, 751), (2, 752)]),
    SCell::Obj(&[(17, 753), (16, 754), (1, 755), (2, 756)]),
    SCell::Clo(24, &[224]),
    SCell::Clo(23, &[224]),
    SCell::Clo(27, &[224]),
    SCell::Clo(28, &[224]),
    SCell::Clo(29, &[224]),
    SCell::Obj(&[(1, 757), (2, 758)]),
    SCell::Obj(&[(1, 759), (2, 760)]),
    SCell::Clo(26, &[225]),
    SCell::Clo(25, &[225]),
    SCell::Obj(&[(1, 761), (2, 762)]),
    SCell::Obj(&[(17, 763), (16, 764), (1, 765), (2, 766)]),
    SCell::Clo(24, &[225]),
    SCell::Clo(23, &[225]),
    SCell::Clo(27, &[225]),
    SCell::Clo(28, &[225]),
    SCell::Clo(29, &[225]),
    SCell::Obj(&[(1, 767), (2, 768)]),
    SCell::Obj(&[(1, 769), (2, 770)]),
    SCell::Clo(26, &[226]),
    SCell::Clo(25, &[226]),
    SCell::Obj(&[(1, 771), (2, 772)]),
    SCell::Obj(&[(17, 773), (16, 774), (1, 775), (2, 776)]),
    SCell::Clo(24, &[226]),
    SCell::Clo(23, &[226]),
    SCell::Clo(27, &[226]),
    SCell::Clo(28, &[226]),
    SCell::Clo(29, &[226]),
    SCell::Obj(&[(1, 777), (2, 778)]),
    SCell::Obj(&[(1, 779), (2, 780)]),
    SCell::Clo(26, &[227]),
    SCell::Clo(25, &[227]),
    SCell::Obj(&[(1, 781), (2, 782)]),
    SCell::Obj(&[(17, 783), (16, 784), (1, 785), (2, 786)]),
    SCell::Clo(24, &[227]),
    SCell::Clo(23, &[227]),
    SCell::Clo(27, &[227]),
    SCell::Clo(28, &[227]),
    SCell::Clo(29, &[227]),
    SCell::Obj(&[(1, 787), (2, 788)]),
    SCell::Obj(&[(1, 789), (2, 790)]),
    SCell::Clo(26, &[228]),
    SCell::Clo(25, &[228]),
    SCell::Obj(&[(1, 791), (2, 792)]),
    SCell::Obj(&[(17, 793), (16, 794), (1, 795), (2, 796)]),
    SCell::Clo(24, &[228]),
    SCell::Clo(23, &[228]),
    SCell::Clo(27, &[228]),
    SCell::Clo(28, &[228]),
    SCell::Clo(29, &[228]),
    SCell::Obj(&[(1, 797), (2, 798)]),
    SCell::Obj(&[(1, 799), (2, 800)]),
    SCell::Clo(26, &[229]),
    SCell::Clo(25, &[229]),
    SCell::Obj(&[(1, 801), (2, 802)]),
    SCell::Obj(&[(17, 803), (16, 804), (1, 805), (2, 806)]),
    SCell::Clo(24, &[229]),
    SCell::Clo(23, &[229]),
    SCell::Clo(27, &[229]),
    SCell::Clo(28, &[229]),
    SCell::Clo(29, &[229]),
    SCell::Obj(&[(1, 807), (2, 808)]),
    SCell::Obj(&[(1, 809), (2, 810)]),
    SCell::Clo(26, &[230]),
    SCell::Clo(25, &[230]),
    SCell::Obj(&[(1, 811), (2, 812)]),
    SCell::Obj(&[(17, 813), (16, 814), (1, 815), (2, 816)]),
    SCell::Clo(24, &[230]),
    SCell::Clo(23, &[230]),
    SCell::Clo(27, &[230]),
    SCell::Clo(28, &[230]),
    SCell::Clo(29, &[230]),
    SCell::Obj(&[(1, 817), (2, 818)]),
    SCell::Obj(&[(1, 819), (2, 820)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_2: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_2: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 217),
    (152, 218),
    (153, 219),
    (154, 220),
    (155, 221),
    (156, 222),
    (157, 223),
    (158, 224),
    (159, 225),
    (160, 226),
    (161, 227),
    (162, 228),
    (163, 229),
    (164, 230),
    (165, 231),
    (166, 232),
    (167, 233),
    (168, 234),
    (170, 235),
    (173, 94),
    (174, 116),
    (175, 118),
    (176, 119),
    (177, 121),
    (178, 122),
    (179, 123),
    (180, 124),
    (181, 125),
    (182, 127),
    (183, 236),
    (185, 237),
    (186, 238),
    (188, 239),
    (189, 240),
    (191, 241),
    (193, 242),
    (195, 243),
    (197, 244),
    (199, 245),
    (202, 246),
    (205, 247),
    (207, 248),
    (209, 249),
    (210, 250),
    (211, 251),
    (212, 252),
    (213, 253),
    (215, 254),
    (216, 255),
    (256, 524),
    (257, 525),
    (260, 526),
    (261, 527),
    (262, 528),
    (263, 529),
    (264, 530),
    (265, 531),
    (266, 532),
    (267, 533),
    (269, 534),
    (272, 535),
    (273, 94),
    (276, 536),
    (277, 537),
    (279, 538),
    (281, 539),
    (282, 540),
    (283, 541),
    (284, 542),
    (285, 543),
    (286, 544),
    (287, 545),
    (289, 546),
    (293, 122),
    (296, 547),
    (297, 548),
    (299, 549),
    (300, 550),
    (301, 551),
    (302, 552),
    (303, 553),
    (304, 554),
    (305, 555),
    (306, 556),
    (309, 557),
    (312, 119),
    (315, 558),
    (316, 559),
    (318, 560),
    (319, 561),
    (320, 562),
    (321, 563),
    (322, 564),
    (323, 565),
    (324, 566),
    (325, 567),
    (328, 568),
    (331, 119),
    (334, 569),
    (335, 570),
    (337, 571),
    (338, 572),
    (339, 573),
    (340, 574),
    (341, 575),
    (342, 576),
    (343, 577),
    (344, 578),
    (347, 579),
    (350, 119),
    (353, 580),
    (354, 581),
    (356, 582),
    (357, 583),
    (358, 584),
    (359, 585),
    (360, 586),
    (361, 587),
    (362, 588),
    (363, 589),
    (366, 590),
    (369, 119),
    (372, 591),
    (373, 592),
    (375, 593),
    (376, 594),
    (377, 595),
    (378, 596),
    (379, 597),
    (380, 598),
    (381, 599),
    (382, 600),
    (385, 601),
    (388, 119),
    (391, 602),
    (392, 603),
    (394, 604),
    (395, 605),
    (396, 606),
    (397, 607),
    (398, 608),
    (399, 609),
    (400, 610),
    (401, 611),
    (404, 612),
    (407, 119),
    (410, 613),
    (411, 614),
    (413, 615),
    (414, 616),
    (415, 617),
    (416, 618),
    (417, 619),
    (418, 620),
    (419, 621),
    (420, 622),
    (423, 623),
    (426, 119),
    (429, 624),
    (430, 625),
    (432, 626),
    (433, 627),
    (434, 628),
    (435, 629),
    (436, 630),
    (437, 631),
    (438, 632),
    (439, 633),
    (442, 634),
    (445, 119),
    (448, 635),
    (449, 636),
    (451, 637),
    (452, 638),
    (453, 639),
    (454, 640),
    (455, 641),
    (456, 642),
    (457, 643),
    (458, 644),
    (461, 645),
    (464, 119),
    (467, 646),
    (468, 647),
    (470, 648),
    (471, 649),
    (472, 650),
    (473, 651),
    (474, 652),
    (475, 653),
    (476, 654),
    (477, 655),
    (480, 656),
    (483, 119),
    (486, 657),
    (487, 658),
    (489, 659),
    (490, 660),
    (491, 661),
    (492, 662),
    (493, 663),
    (494, 664),
    (495, 665),
    (496, 666),
    (499, 667),
    (502, 119),
    (505, 668),
    (506, 669),
    (508, 670),
    (509, 671),
    (510, 672),
    (511, 673),
    (512, 674),
    (513, 675),
    (514, 676),
    (515, 677),
    (518, 678),
    (521, 119),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c39: ZN,
    pub c38: ZB,
}

/// Outcome 2's per-assignment values and lane masks.
/// The cells of outcome 2 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared2`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut2 {
    pub c20: ZN,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 3 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_3: &[(u32, &str)] = &[
    (177, "balloon.tile"),
    (204, "big_chest.tile"),
    (196, "chest.if_not_fruit"),
    (198, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (190, "fake_wall.if_not_fruit"),
    (191, "fake_wall.tile"),
    (180, "fall_floor.tile"),
    (186, "fly_fruit.if_not_fruit"),
    (188, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (182, "fruit.if_not_fruit"),
    (184, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (193, "key.if_not_fruit"),
    (194, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (238, "objects[0].collideable"),
    (358, "objects[0].flip.x"),
    (359, "objects[0].flip.y"),
    (360, "objects[0].hitbox.h"),
    (361, "objects[0].hitbox.w"),
    (362, "objects[0].hitbox.x"),
    (363, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (364, "objects[0].rem.x"),
    (365, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (366, "objects[0].spd.x"),
    (367, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (368, "objects[1].flip.x"),
    (369, "objects[1].flip.y"),
    (370, "objects[1].hitbox.h"),
    (371, "objects[1].hitbox.w"),
    (372, "objects[1].hitbox.x"),
    (373, "objects[1].hitbox.y"),
    (374, "objects[1].rem.x"),
    (375, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (376, "objects[1].spd.x"),
    (377, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (270, "objects[1].state"),
    (378, "objects[1].target.x"),
    (379, "objects[1].target.y"),
    (159, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (380, "objects[2].flip.x"),
    (381, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (382, "objects[2].hitbox.h"),
    (383, "objects[2].hitbox.w"),
    (384, "objects[2].hitbox.x"),
    (385, "objects[2].hitbox.y"),
    (386, "objects[2].rem.x"),
    (387, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (388, "objects[2].spd.x"),
    (389, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (174, "objects[2].type.tile"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (390, "objects[3].flip.x"),
    (391, "objects[3].flip.y"),
    (298, "objects[3].hide_for"),
    (299, "objects[3].hide_in"),
    (392, "objects[3].hitbox.h"),
    (393, "objects[3].hitbox.w"),
    (394, "objects[3].hitbox.x"),
    (395, "objects[3].hitbox.y"),
    (396, "objects[3].rem.x"),
    (397, "objects[3].rem.y"),
    (307, "objects[3].solids"),
    (398, "objects[3].spd.x"),
    (399, "objects[3].spd.y"),
    (309, "objects[3].spr"),
    (311, "objects[3].x"),
    (312, "objects[3].y"),
    (43, "pause_player"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_3: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_3: &[SCell] = &[
    SCell::Bi(0),
    SCell::Bi(1),
    SCell::Bi(2),
    SCell::Bi(3),
    SCell::Bi(4),
    SCell::Bi(5),
    SCell::Bi(6),
    SCell::Bi(7),
    SCell::Bi(8),
    SCell::Bi(9),
    SCell::Bi(10),
    SCell::Bi(11),
    SCell::Val,
    SCell::Val,
    SCell::Bi(14),
    SCell::Bi(15),
    SCell::Bi(16),
    SCell::Bi(17),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(76, &[]),
    SCell::Clo(69, &[]),
    SCell::Arr(&[145, 146, 147, 148, 149, 150]),
    SCell::Arr(&[151, 152, 153, 154]),
    SCell::Obj(&[(7, 155), (5, 156), (6, 157)]),
    SCell::Obj(&[(5, 158), (8, 159), (6, 160)]),
    SCell::Clo(75, &[]),
    SCell::Clo(74, &[]),
    SCell::Clo(73, &[]),
    SCell::Clo(72, &[]),
    SCell::Clo(71, &[]),
    SCell::Clo(70, &[]),
    SCell::Clo(68, &[]),
    SCell::Clo(67, &[]),
    SCell::Clo(66, &[]),
    SCell::Clo(65, &[]),
    SCell::Clo(64, &[]),
    SCell::Clo(63, &[]),
    SCell::Clo(62, &[]),
    SCell::Obj(&[(1, 161), (2, 162)]),
    SCell::Arr(&[163, 164, 165, 166, 167, 168, 169, 170, 171, 172]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 173), (8, 174), (6, 175)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 182), (5, 183), (8, 184), (6, 185)]),
    SCell::Obj(&[(9, 186), (5, 187), (8, 188), (6, 189)]),
    SCell::Obj(&[(9, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (5, 197), (8, 198), (6, 199)]),
    SCell::Obj(&[(5, 200), (6, 201)]),
    SCell::Obj(&[(7, 202), (5, 203), (8, 204)]),
    SCell::Obj(&[(7, 205), (5, 206)]),
    SCell::Clo(22, &[]),
    SCell::Clo(21, &[]),
    SCell::Clo(20, &[]),
    SCell::Clo(19, &[]),
    SCell::Clo(18, &[]),
    SCell::Clo(16, &[]),
    SCell::Clo(14, &[]),
    SCell::Clo(11, &[]),
    SCell::Clo(10, &[]),
    SCell::Clo(9, &[]),
    SCell::Clo(8, &[]),
    SCell::Clo(7, &[]),
    SCell::Clo(6, &[]),
    SCell::Clo(5, &[]),
    SCell::Clo(3, &[]),
    SCell::Clo(2, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 236), (20, 237), (11, 238), (14, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (41, 246), (4, 247), (12, 248), (3, 249), (13, 250), (38, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (15, 260), (19, 261), (18, 262), (22, 263), (23, 264), (24, 265), (4, 266), (12, 267), (3, 268), (13, 269), (25, 270), (34, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (14, 297), (45, 298), (42, 299), (15, 300), (19, 301), (18, 302), (22, 303), (23, 304), (24, 305), (4, 306), (12, 307), (3, 308), (13, 309), (0, 310), (1, 311), (2, 312)]),
    SCell::Clo(54, &[]),
    SCell::Clo(56, &[]),
    SCell::Clo(55, &[]),
    SCell::Clo(53, &[]),
    SCell::Clo(52, &[]),
    SCell::Clo(51, &[]),
    SCell::Clo(50, &[]),
    SCell::Clo(48, &[]),
    SCell::Clo(47, &[]),
    SCell::Clo(46, &[]),
    SCell::Clo(45, &[]),
    SCell::Clo(43, &[]),
    SCell::Clo(42, &[]),
    SCell::Clo(41, &[]),
    SCell::Clo(40, &[]),
    SCell::Clo(39, &[]),
    SCell::Clo(38, &[]),
    SCell::Clo(37, &[]),
    SCell::Clo(36, &[]),
    SCell::Clo(35, &[]),
    SCell::Clo(34, &[]),
    SCell::Clo(32, &[]),
    SCell::Clo(33, &[]),
    SCell::Clo(30, &[]),
    SCell::Clo(31, &[]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(17, 360), (16, 361), (1, 362), (2, 363)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 364), (2, 365)]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Obj(&[(17, 370), (16, 371), (1, 372), (2, 373)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Obj(&[(1, 376), (2, 377)]),
    SCell::Obj(&[(1, 378), (2, 379)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 380), (2, 381)]),
    SCell::Obj(&[(17, 382), (16, 383), (1, 384), (2, 385)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 386), (2, 387)]),
    SCell::Obj(&[(1, 388), (2, 389)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 390), (2, 391)]),
    SCell::Obj(&[(17, 392), (16, 393), (1, 394), (2, 395)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
];

pub const OUT_GLOBALS_3: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_3: &[(u32, u32)] = &[
    (12, 89),
    (13, 90),
    (18, 91),
    (19, 92),
    (21, 93),
    (22, 94),
    (23, 95),
    (24, 96),
    (25, 97),
    (26, 98),
    (27, 99),
    (28, 100),
    (29, 101),
    (30, 102),
    (31, 103),
    (32, 104),
    (33, 105),
    (34, 106),
    (35, 107),
    (36, 108),
    (37, 109),
    (40, 110),
    (50, 111),
    (51, 112),
    (52, 113),
    (53, 114),
    (54, 115),
    (55, 116),
    (56, 117),
    (57, 118),
    (58, 119),
    (59, 120),
    (60, 121),
    (61, 122),
    (62, 123),
    (63, 124),
    (64, 125),
    (65, 126),
    (66, 127),
    (67, 128),
    (68, 129),
    (69, 130),
    (70, 131),
    (71, 132),
    (72, 133),
    (73, 134),
    (74, 135),
    (75, 136),
    (76, 137),
    (77, 138),
    (78, 139),
    (79, 140),
    (80, 141),
    (81, 142),
    (82, 143),
    (83, 144),
    (151, 207),
    (152, 208),
    (153, 209),
    (154, 210),
    (155, 211),
    (156, 212),
    (157, 213),
    (158, 214),
    (160, 215),
    (163, 94),
    (164, 116),
    (165, 118),
    (166, 119),
    (167, 121),
    (168, 122),
    (169, 123),
    (170, 124),
    (171, 125),
    (172, 127),
    (173, 216),
    (175, 217),
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (183, 222),
    (185, 223),
    (187, 224),
    (189, 225),
    (192, 226),
    (195, 227),
    (197, 228),
    (199, 229),
    (200, 230),
    (201, 231),
    (202, 232),
    (203, 233),
    (205, 234),
    (206, 235),
    (236, 313),
    (237, 314),
    (239, 315),
    (240, 316),
    (241, 317),
    (242, 318),
    (243, 319),
    (244, 320),
    (245, 321),
    (247, 322),
    (249, 323),
    (252, 121),
    (255, 324),
    (256, 325),
    (259, 326),
    (260, 327),
    (261, 328),
    (262, 329),
    (263, 330),
    (264, 331),
    (265, 332),
    (266, 333),
    (268, 334),
    (271, 335),
    (272, 94),
    (275, 336),
    (276, 337),
    (278, 338),
    (281, 339),
    (282, 340),
    (283, 341),
    (284, 342),
    (285, 343),
    (286, 344),
    (287, 345),
    (289, 346),
    (291, 116),
    (294, 347),
    (295, 348),
    (297, 349),
    (300, 350),
    (301, 351),
    (302, 352),
    (303, 353),
    (304, 354),
    (305, 355),
    (306, 356),
    (308, 357),
    (310, 116),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c39: ZN,
    pub c20: ZN,
    pub c258: ZN,
    pub c375: ZN,
    pub c377: ZN,
    pub c269: ZN,
    pub c270: ZN,
    pub c274: ZN,
}

/// Outcome 3's per-assignment values and lane masks.
/// The cells of outcome 3 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared3`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut3 {
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

/// An EMPTY accumulator with outcome 0's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append0`.
pub fn acc0(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_0, OUT_GLOBALS_0, OUT_PTRS_0, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[182] = Col::U(AV::Bool(true));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[364] = Col::U(AV::Bool(false));
    b.cols[365] = Col::U(AV::Bool(false));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[369] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Ival(P8::from_raw(0i32), P8::from_raw(2555904i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Ival(P8::from_raw(2981888i32), P8::from_raw(3309568i32)));
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[374] = Col::U(AV::Bool(false));
    b.cols[375] = Col::U(AV::Bool(false));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[376] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[380] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[381] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[276] = Col::U(AV::Bool(true));
    b.cols[384] = Col::U(AV::Bool(false));
    b.cols[385] = Col::U(AV::Bool(false));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[391] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[287] = Col::U(AV::Bool(true));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[289] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[291] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[295] = Col::U(AV::Bool(true));
    b.cols[394] = Col::N(Vec::new());
    b.cols[395] = Col::N(Vec::new());
    b.cols[297] = Col::N(Vec::new());
    b.cols[396] = Col::N(Vec::new());
    b.cols[397] = Col::N(Vec::new());
    b.cols[299] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[398] = Col::V(Vec::new());
    b.cols[399] = Col::U(AV::Bool(false));
    b.cols[302] = Col::N(Vec::new());
    b.cols[400] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[401] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[402] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[403] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[309] = Col::V(Vec::new());
    b.cols[310] = Col::V(Vec::new());
    b.cols[404] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[405] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[312] = Col::U(AV::Bool(true));
    b.cols[406] = Col::N(Vec::new());
    b.cols[407] = Col::N(Vec::new());
    b.cols[316] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[317] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_0 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_0: u64 = 7837082039621144106;
pub const KPART2_0: u64 = 10112752925507046138;

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let k0 = mix64(KPART1_0.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_0.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[394] { v.push(kv.c394.lane(i)); }
        if let Col::N(v) = &mut acc.cols[395] { v.push(kv.c395.lane(i)); }
        if let Col::N(v) = &mut acc.cols[297] { v.push(kv.c297.lane(i)); }
        if let Col::N(v) = &mut acc.cols[396] { v.push(kv.c396.lane(i)); }
        if let Col::N(v) = &mut acc.cols[397] { v.push(kv.c397.lane(i)); }
        if let Col::N(v) = &mut acc.cols[299] { v.push(kv.c299.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::V(v) = &mut acc.cols[398] {
            v.push(if kv.c398.known & (1 << i) != 0 {
                AV::Bool(kv.c398.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[302] { v.push(kv.c302.lane(i)); }
        if let Col::V(v) = &mut acc.cols[309] {
            v.push(if kv.c309.known & (1 << i) != 0 {
                AV::Bool(kv.c309.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[310] {
            v.push(if kv.c310.known & (1 << i) != 0 {
                AV::Bool(kv.c310.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[406] { v.push(kv.c406.lane(i)); }
        if let Col::N(v) = &mut acc.cols[407] { v.push(kv.c407.lane(i)); }
        if let Col::N(v) = &mut acc.cols[317] { v.push(sh.c317.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[182] = Col::U(AV::Bool(true));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[237] = Col::U(AV::Bool(true));
    b.cols[325] = Col::U(AV::Bool(false));
    b.cols[326] = Col::U(AV::Bool(false));
    b.cols[327] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[328] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Ival(P8::from_raw(0i32), P8::from_raw(2555904i32)));
    b.cols[331] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[247] = Col::U(AV::Bool(true));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::U(AV::Ival(P8::from_raw(2981888i32), P8::from_raw(3309568i32)));
    b.cols[256] = Col::U(AV::Bool(true));
    b.cols[335] = Col::U(AV::Bool(false));
    b.cols[336] = Col::U(AV::Bool(false));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[337] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Bool(true));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[275] = Col::U(AV::Bool(true));
    b.cols[345] = Col::U(AV::Bool(false));
    b.cols[346] = Col::U(AV::Bool(false));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[347] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[286] = Col::U(AV::Bool(true));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[291] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_1 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_1: u64 = 6644183290428053420;
pub const KPART2_1: u64 = 12120158949941374804;

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let k0 = mix64(KPART1_1.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_1.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[214] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[206] = Col::U(AV::Bool(true));
    b.cols[208] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[200] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[192] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[203] = Col::U(AV::Bool(true));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[258] = Col::U(AV::Bool(true));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[679] = Col::U(AV::Bool(false));
    b.cols[680] = Col::U(AV::Bool(false));
    b.cols[681] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[682] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[683] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[684] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[685] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[686] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(false));
    b.cols[687] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[688] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[689] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[690] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[169] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(8388608i32)));
    b.cols[278] = Col::U(AV::Bool(true));
    b.cols[691] = Col::U(AV::Bool(false));
    b.cols[692] = Col::U(AV::Bool(false));
    b.cols[280] = Col::U(AV::Bool(false));
    b.cols[693] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[694] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[695] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[696] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[697] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[698] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(false));
    b.cols[699] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[700] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[291] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(32768i32)));
    b.cols[294] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[295] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[298] = Col::U(AV::Bool(true));
    b.cols[701] = Col::U(AV::Bool(false));
    b.cols[702] = Col::U(AV::Bool(false));
    b.cols[703] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[704] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[705] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[706] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[707] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[708] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Bool(true));
    b.cols[308] = Col::U(AV::Bool(true));
    b.cols[709] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[710] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[317] = Col::U(AV::Bool(true));
    b.cols[711] = Col::U(AV::Bool(false));
    b.cols[712] = Col::U(AV::Bool(false));
    b.cols[713] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[714] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[715] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[716] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[717] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[718] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[326] = Col::U(AV::Bool(true));
    b.cols[327] = Col::U(AV::Bool(true));
    b.cols[719] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[720] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[336] = Col::U(AV::Bool(true));
    b.cols[721] = Col::U(AV::Bool(false));
    b.cols[722] = Col::U(AV::Bool(false));
    b.cols[723] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[724] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[725] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[726] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[727] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[728] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Bool(true));
    b.cols[346] = Col::U(AV::Bool(true));
    b.cols[729] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[730] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[355] = Col::U(AV::Bool(true));
    b.cols[731] = Col::U(AV::Bool(false));
    b.cols[732] = Col::U(AV::Bool(false));
    b.cols[733] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[734] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[735] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[736] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[737] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[738] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[364] = Col::U(AV::Bool(true));
    b.cols[365] = Col::U(AV::Bool(true));
    b.cols[739] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[740] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[374] = Col::U(AV::Bool(true));
    b.cols[741] = Col::U(AV::Bool(false));
    b.cols[742] = Col::U(AV::Bool(false));
    b.cols[743] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[744] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[745] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[746] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[747] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[748] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[383] = Col::U(AV::Bool(true));
    b.cols[384] = Col::U(AV::Bool(true));
    b.cols[749] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[750] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[393] = Col::U(AV::Bool(true));
    b.cols[751] = Col::U(AV::Bool(false));
    b.cols[752] = Col::U(AV::Bool(false));
    b.cols[753] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[754] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[755] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[756] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[757] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[758] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[402] = Col::U(AV::Bool(true));
    b.cols[403] = Col::U(AV::Bool(true));
    b.cols[759] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[760] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[405] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[406] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[408] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[409] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[412] = Col::U(AV::Bool(true));
    b.cols[761] = Col::U(AV::Bool(false));
    b.cols[762] = Col::U(AV::Bool(false));
    b.cols[763] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[764] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[765] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[766] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[767] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[768] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[421] = Col::U(AV::Bool(true));
    b.cols[422] = Col::U(AV::Bool(true));
    b.cols[769] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[770] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[424] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[425] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[427] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[428] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[431] = Col::U(AV::Bool(true));
    b.cols[771] = Col::U(AV::Bool(false));
    b.cols[772] = Col::U(AV::Bool(false));
    b.cols[773] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[774] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[775] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[776] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[777] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[778] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[440] = Col::U(AV::Bool(true));
    b.cols[441] = Col::U(AV::Bool(true));
    b.cols[779] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[780] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[443] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[444] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[446] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[447] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[450] = Col::U(AV::Bool(true));
    b.cols[781] = Col::U(AV::Bool(false));
    b.cols[782] = Col::U(AV::Bool(false));
    b.cols[783] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[784] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[785] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[786] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[787] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[788] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[459] = Col::U(AV::Bool(true));
    b.cols[460] = Col::U(AV::Bool(true));
    b.cols[789] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[790] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[462] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[463] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[465] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[466] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[469] = Col::U(AV::Bool(true));
    b.cols[791] = Col::U(AV::Bool(false));
    b.cols[792] = Col::U(AV::Bool(false));
    b.cols[793] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[794] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[795] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[796] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[797] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[798] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[478] = Col::U(AV::Bool(true));
    b.cols[479] = Col::U(AV::Bool(true));
    b.cols[799] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[800] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[481] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[482] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[484] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[485] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[488] = Col::U(AV::Bool(true));
    b.cols[801] = Col::U(AV::Bool(false));
    b.cols[802] = Col::U(AV::Bool(false));
    b.cols[803] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[804] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[805] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[806] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[807] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[808] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[497] = Col::U(AV::Bool(true));
    b.cols[498] = Col::U(AV::Bool(true));
    b.cols[809] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[810] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[500] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[501] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[503] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[504] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[507] = Col::U(AV::Bool(true));
    b.cols[811] = Col::U(AV::Bool(false));
    b.cols[812] = Col::U(AV::Bool(false));
    b.cols[813] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[814] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[815] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[816] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[817] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[818] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[516] = Col::U(AV::Bool(true));
    b.cols[517] = Col::U(AV::Bool(true));
    b.cols[819] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[820] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[519] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[520] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[522] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[523] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_2 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 695480966907961521;
pub const KPART2_2: u64 = 12646940967063900584;

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let k0 = mix64(KPART1_2.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_2.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[20] = Col::N(Vec::new());
    b.cols[182] = Col::U(AV::Bool(true));
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[358] = Col::U(AV::Bool(false));
    b.cols[359] = Col::U(AV::Bool(false));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[362] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::U(AV::Ival(P8::from_raw(0i32), P8::from_raw(2555904i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::U(AV::Ival(P8::from_raw(2981888i32), P8::from_raw(3309568i32)));
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::N(Vec::new());
    b.cols[368] = Col::U(AV::Bool(false));
    b.cols[369] = Col::U(AV::Bool(false));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[375] = Col::N(Vec::new());
    b.cols[267] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::N(Vec::new());
    b.cols[269] = Col::N(Vec::new());
    b.cols[270] = Col::N(Vec::new());
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::N(Vec::new());
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[380] = Col::U(AV::Bool(false));
    b.cols[381] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[385] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[390] = Col::U(AV::Bool(false));
    b.cols[391] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[395] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[396] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[397] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Bool(true));
    b.cols[398] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[399] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_3 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 15626649125756005725;
pub const KPART2_3: u64 = 18071780321433227545;

/// Append this assignment's lanes that TAKE outcome 3 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote, by the SOUND full boundary key (`mix64(KPART + kv.h)`),
/// so the dedup here is exactly the boundary's - computed from
/// values already in registers before materializing the row.
/// A duplicate caught here costs nothing; one caught at the
/// boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> u16 {
    // Returns the lanes actually WRITTEN, which is `take`
    // minus the ones another configuration already wrote.
    // A caller that needs to know which (assignment, lane)
    // produced row k cannot infer it from `take`.
    let mut wrote: u16 = 0;
    let take = take & ((1u32 << n) - 1) as u16;
    // The key columns come out of their registers ONCE.
    // `ZW::lane` is a store plus a load, so calling it
    // inside the loop would do that sixteen times for a
    // value that does not change.
    let (h1, h2) = (kv.h1.to_array(), kv.h2.to_array());
    for i in 0..n {
        if take & (1 << i) == 0 { continue; }
        let k0 = mix64(KPART1_3.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_3.wrapping_add(h2[i]));
        let key = if org.is_empty() { (k0, k1) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            let m = mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64);
            (mix64(k0 ^ m), mix64(k1 ^ m))
        };
        if !seen.insert(key) { continue; }
        // Option 1: a row already in the FROZEN frontier is a
        // cross-frame duplicate - skip materialization entirely.
        if skip(key) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[258] { v.push(sh.c258.lane(i)); }
        if let Col::N(v) = &mut acc.cols[375] { v.push(sh.c375.lane(i)); }
        if let Col::N(v) = &mut acc.cols[377] { v.push(sh.c377.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[270] { v.push(sh.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[274] { v.push(sh.c274.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 4;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        _ => panic!("outcome {} of 4", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        _ => panic!("outcome {} of 4", i),
    }
}

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32], skip: &'a dyn Fn((u64, u64)) -> bool }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org, self.skip);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org, self.skip);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org, self.skip);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org, self.skip);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
    skip: &dyn Fn((u64, u64)) -> bool,
) -> Option<u16> {
    let (u, s) = bind(b)?;
    let rin = rows(b, &s, lo)?;
    let g = G { cart: &b.cart, cache: &b.cache };
    // The row sets are the CALLER's and reset in O(1) here.
    // Slice-local because that is where the duplication is:
    // it comes from configurations agreeing on one lane, and
    // a lane lives in one slice.
    seen.iter_mut().for_each(|s| s.next_slice());
    // Engine-carried origin metadata for this slice's lanes
    // (empty = untracked); see `Rt2::origin`.
    let org: &[u32] = if b.origin.is_empty() { &[] } else { &b.origin[lo..lo + n] };
    let mut sink = Append { accs, seen, n, org, skip };
    Some(frame(&u, &rin, &g, &mut sink))
}

/// Where a frame's rows go. One call per (outcome, GROUP), not
/// per outcome per variant: variants that write identical
/// values are one call whose `take` is the union of their
/// lanes. `mask` is the group's REPRESENTATIVE assignment -
/// every member computes the same values, so any of them
/// identifies the row for a caller that wants to check it
/// against the graph.
pub trait Sink {
    fn o0(&mut self, mask: u8, take: u16, sh: &KShared0, v: &KOut0);
    fn o1(&mut self, mask: u8, take: u16, sh: &KShared1, v: &KOut1);
    fn o2(&mut self, mask: u8, take: u16, sh: &KShared2, v: &KOut2);
    fn o3(&mut self, mask: u8, take: u16, sh: &KShared3, v: &KOut3);
}

/// Run one frame over 16 lanes. Returns the lanes DECLINED -
/// live but not `ok` - which the never-deopt doctrine turns
/// into a stopped run.
#[inline(never)]
pub fn frame(u: &Uni, rin: &RowsIn, g: &G, sink: &mut dyn Sink) -> u16 {
    let mut declined: u16 = 0;
    let r_c20: ZN = rin.c20;
    let r_c38: ZB = ZB { val: rin.c38, known: ALL };
    let r_c39: ZN = rin.c39;
    let r_c41: ZB = ZB { val: rin.c41, known: ALL };
    let r_c42: ZB = ZB { val: rin.c42, known: ALL };
    let r_c43: ZB = ZB { val: rin.c43, known: ALL };
    let r_c84: ZN = rin.c84;
    let r_c85: ZN = rin.c85;
    let r_c86: ZN = rin.c86;
    let r_c87: ZN = rin.c87;
    let r_c88: ZN = rin.c88;
    let r_c238: ZB = ZB { val: rin.c238, known: ALL };
    let r_c248: ZB = ZB { val: rin.c248, known: ALL };
    let r_c250: ZN = rin.c250;
    let r_c251: ZN = rin.c251;
    let r_c253: ZN = rin.c253;
    let r_c254: ZI = rin.c254;
    let r_c257: ZB = ZB { val: rin.c257, known: ALL };
    let r_c258: ZN = rin.c258;
    let r_c267: ZB = ZB { val: rin.c267, known: ALL };
    let r_c269: ZN = rin.c269;
    let r_c270: ZN = rin.c270;
    let r_c273: ZN = rin.c273;
    let r_c274: ZN = rin.c274;
    let r_c277: ZB = ZB { val: rin.c277, known: ALL };
    let r_c279: ZN = rin.c279;
    let r_c280: ZN = rin.c280;
    let r_c288: ZB = ZB { val: rin.c288, known: ALL };
    let r_c290: ZN = rin.c290;
    let r_c292: ZN = rin.c292;
    let r_c293: ZN = rin.c293;
    let r_c296: ZB = ZB { val: rin.c296, known: ALL };
    let r_c298: ZN = rin.c298;
    let r_c299: ZN = rin.c299;
    let r_c307: ZB = ZB { val: rin.c307, known: ALL };
    let r_c309: ZN = rin.c309;
    let r_c311: ZN = rin.c311;
    let r_c312: ZN = rin.c312;
    let r_c358: ZB = ZB { val: rin.c358, known: ALL };
    let r_c359: ZB = ZB { val: rin.c359, known: ALL };
    let r_c364: ZN = rin.c364;
    let r_c365: ZN = rin.c365;
    let r_c366: ZN = rin.c366;
    let r_c367: ZN = rin.c367;
    let r_c368: ZB = ZB { val: rin.c368, known: ALL };
    let r_c369: ZB = ZB { val: rin.c369, known: ALL };
    let r_c374: ZN = rin.c374;
    let r_c375: ZN = rin.c375;
    let r_c376: ZN = rin.c376;
    let r_c377: ZN = rin.c377;
    let r_c378: ZN = rin.c378;
    let r_c379: ZN = rin.c379;
    let r_c380: ZB = ZB { val: rin.c380, known: ALL };
    let r_c381: ZB = ZB { val: rin.c381, known: ALL };
    let r_c386: ZN = rin.c386;
    let r_c387: ZN = rin.c387;
    let r_c388: ZN = rin.c388;
    let r_c389: ZN = rin.c389;
    let r_c390: ZB = ZB { val: rin.c390, known: ALL };
    let r_c391: ZB = ZB { val: rin.c391, known: ALL };
    let r_c396: ZN = rin.c396;
    let r_c397: ZN = rin.c397;
    let r_c398: ZN = rin.c398;
    let r_c399: ZN = rin.c399;
    let n109: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n111: ZB = zb_not(r_c41);
    let n112: ZB = zb_not(r_c42);
    let n113: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n114: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n115: ZB = zb_not(r_c358);
    let n116: ZB = zb_not(r_c359);
    let n117: bool = P8::from_raw(524288i32) == u.c360;
    let n118: bool = P8::from_raw(524288i32) == u.c361;
    let n119: bool = P8::from_raw(0i32) == u.c362;
    let n120: bool = P8::from_raw(0i32) == u.c363;
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c364);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c365);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c366);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c367);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c250);
    let n126: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c251);
    let n127: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c253);
    let n128: ZB = zb_not(r_c368);
    let n129: ZB = zb_not(r_c369);
    let n130: bool = P8::from_raw(524288i32) == u.c370;
    let n131: bool = P8::from_raw(524288i32) == u.c371;
    let n132: bool = P8::from_raw(0i32) == u.c372;
    let n133: bool = P8::from_raw(0i32) == u.c373;
    let n134: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c374);
    let n135: ZB = zb_not(r_c267);
    let n136: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c376);
    let n137: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n138: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c378);
    let n139: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c379);
    let n140: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c273);
    let n141: ZB = zb_not(r_c380);
    let n142: ZB = zb_not(r_c381);
    let n143: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c279);
    let n144: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n145: bool = P8::from_raw(524288i32) == u.c382;
    let n146: bool = P8::from_raw(524288i32) == u.c383;
    let n147: bool = P8::from_raw(0i32) == u.c384;
    let n148: bool = P8::from_raw(0i32) == u.c385;
    let n149: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c386);
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c387);
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c388);
    let n152: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c389);
    let n153: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c290);
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c292);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c293);
    let n156: ZB = zb_not(r_c390);
    let n157: ZB = zb_not(r_c391);
    let n158: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c298);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c299);
    let n160: bool = P8::from_raw(524288i32) == u.c392;
    let n161: bool = P8::from_raw(524288i32) == u.c393;
    let n162: bool = P8::from_raw(0i32) == u.c394;
    let n163: bool = P8::from_raw(0i32) == u.c395;
    let n164: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c396);
    let n165: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c397);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c398);
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c399);
    let n168: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c309);
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c311);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c312);
    let n171: ZB = zb_not(r_c43);
    let n172: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n173: ZB = zb_not(r_c38);
    let n177: (P8, P8) = { let r = IV::new((P8::from_raw(-65536i32), P8::from_raw(65536i32)).0, (P8::from_raw(-65536i32), P8::from_raw(65536i32)).1).scale_positive(P8::from_raw(163840i32)); (r.low, r.high) };
    let n178: (P8, P8) = si_add((P8::from_raw(3145728i32), P8::from_raw(3145728i32)), n177);
    let n184: ZB = zb_not(n137);
    let n185: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c270);
    let n186: ZB = zb_not(n185);
    let n187: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c270);
    let n188: ZN = zn_sub(r_c258, zn_splat(P8::from_raw(65536i32)));
    let n189: ZB = zn_lt(n188, zn_splat(P8::from_raw(0i32)));
    let n200: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c377);
    let n201: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n203: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n231: ZB = zb_not(n200);
    let n232: ZB = zb_and(n184, n201);
    let n233: ZB = zb_and(n186, n232);
    let n234: ZB = zb_and(n187, n233);
    let n235: ZB = zb_and(n189, n234);
    let n278: ZN = zn_add(r_c375, r_c377);
    let n279: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n278);
    let n280: ZN = zn_flr(n279);
    let n281: ZN = zn_add(r_c274, n280);
    let n282: ZN = zsel_n(n231, n281, r_c274);
    let n283: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n282);
    let n284: ZN = zn_div(n283, zn_splat(P8::from_raw(524288i32)));
    let n285: ZN = zn_flr(n284);
    let n286: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n285);
    let n287: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n283);
    let n288: ZN = zn_sub(n287, zn_splat(P8::from_raw(65536i32)));
    let n289: ZN = zn_div(n288, zn_splat(P8::from_raw(524288i32)));
    let n290: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n289);
    let n291: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n286);
    let n292: ZB = zn_le(n291, n290);
    let n293: ZB = zn_gt(n291, n290);
    let n294: ZB = zb_and(n235, n293);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n291);
    let n296: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n295);
    let n297: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n296);
    let n298: ZN = zn_rem(n288, zn_splat(P8::from_raw(524288i32)));
    let n299: ZB = zn_ge(n298, zn_splat(P8::from_raw(393216i32)));
    let n300: ZN = zn_mul(n291, zn_splat(P8::from_raw(524288i32)));
    let n301: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n300);
    let n302: ZB = zn_eq(n287, n301);
    let n303: ZB = zb_and(n184, n186);
    let n304: ZB = zb_and(n187, n303);
    let n305: ZB = zb_and(n189, n304);
    let n306: ZB = zb_and(n201, n305);
    let n307: ZB = zb_and(n292, n306);
    let n308: ZB = zb_or(n299, n302);
    let n309: ZB = zb_and(n297, n308);
    let n310: ZB = zb_not(n309);
    let n311: ZB = zb_and(n307, n309);
    let n312: ZB = zb_and(n307, n310);
    let n313: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n296);
    let n314: ZN = zn_rem(n283, zn_splat(P8::from_raw(524288i32)));
    let n315: ZB = zn_le(n314, zn_splat(P8::from_raw(131072i32)));
    let n316: ZB = zb_and(n313, n315);
    let n317: ZB = zb_not(n316);
    let n318: ZB = zb_and(n312, n316);
    let n319: ZB = zb_and(n312, n317);
    let n320: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n296);
    let n321: ZB = zb_not(n320);
    let n322: ZB = zb_and(n319, n320);
    let n323: ZB = zb_and(n319, n321);
    let n324: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n296);
    let n325: ZB = zb_not(n324);
    let n326: ZB = zb_and(n323, n324);
    let n327: ZB = zb_and(n323, n325);
    let n328: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n286);
    let n329: ZB = zn_le(n328, n290);
    let n330: ZB = zn_gt(n328, n290);
    let n331: ZB = zb_and(n327, n329);
    let n332: ZB = zb_and(n327, n330);
    let n333: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n328);
    let n334: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n333);
    let n335: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n334);
    let n336: ZN = zn_mul(n328, zn_splat(P8::from_raw(524288i32)));
    let n337: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n336);
    let n338: ZB = zn_eq(n287, n337);
    let n339: ZB = zb_or(n299, n338);
    let n340: ZB = zb_and(n335, n339);
    let n341: ZB = zb_not(n340);
    let n342: ZB = zb_and(n331, n340);
    let n343: ZB = zb_and(n331, n341);
    let n344: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n334);
    let n345: ZB = zb_and(n315, n344);
    let n346: ZB = zb_not(n345);
    let n347: ZB = zb_and(n343, n345);
    let n348: ZB = zb_and(n343, n346);
    let n349: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n334);
    let n350: ZB = zb_not(n349);
    let n351: ZB = zb_and(n348, n349);
    let n352: ZB = zb_and(n348, n350);
    let n353: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n334);
    let n354: ZB = zb_not(n353);
    let n355: ZB = zb_and(n352, n353);
    let n356: ZB = zb_and(n352, n354);
    let n357: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n286);
    let n358: ZB = zn_le(n357, n290);
    let n359: ZB = zn_gt(n357, n290);
    let n360: ZB = zb_and(n356, n358);
    let n361: ZB = zb_and(n356, n359);
    let n362: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n357);
    let n363: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n362);
    let n364: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n363);
    let n365: ZN = zn_mul(n357, zn_splat(P8::from_raw(524288i32)));
    let n366: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n365);
    let n367: ZB = zn_eq(n287, n366);
    let n368: ZB = zb_or(n299, n367);
    let n369: ZB = zb_and(n364, n368);
    let n370: ZB = zb_not(n369);
    let n371: ZB = zb_and(n360, n369);
    let n372: ZB = zb_and(n360, n370);
    let n373: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n363);
    let n374: ZB = zb_and(n315, n373);
    let n375: ZB = zb_not(n374);
    let n376: ZB = zb_and(n372, n374);
    let n377: ZB = zb_and(n372, n375);
    let n378: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n363);
    let n379: ZB = zb_not(n378);
    let n380: ZB = zb_and(n377, n378);
    let n381: ZB = zb_and(n377, n379);
    let n382: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n363);
    let n383: ZB = zb_not(n382);
    let n384: ZB = zb_and(n381, n382);
    let n385: ZB = zb_and(n381, n383);
    let n386: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n286);
    let n387: ZB = zn_gt(n386, n290);
    let n388: ZB = zb_or(n361, n385);
    let n389: ZB = zb_or(n359, n387);
    let n390: ZB = zb_or(n332, n388);
    let n391: ZB = zb_or(n330, n389);
    let n392: ZB = zb_or(n294, n390);
    let n393: ZB = zb_or(n293, n391);
    let n394: ZB = zn_le(n282, zn_splat(P8::from_raw(8388608i32)));
    let n395: ZB = zb_and(n392, n394);
    let n396: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n283);
    let n397: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n396, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n398: ZB = zb_not(n397);
    let n399: ZN = zsel_n(n397, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n400: ZN = zsel_n(n398, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n401: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n400);
    let n402: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n400);
    let n403: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n283);
    let n404: ZN = zsel_n(n398, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n405: ZB = zn_gt(n399, zn_splat(P8::from_raw(0i32)));
    let n406: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n403, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n407: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n403, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n408: ZN = zsel_n(n407, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n409: ZN = zsel_n(n406, zn_splat(P8::from_raw(-65536i32)), n408);
    let n410: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n409);
    let n411: ZB = zb_not(n410);
    let n412: ZN = zn_neg(n409);
    let n413: ZN = zn_mul(n412, zn_splat(P8::from_raw(131072i32)));
    let n414: ZN = zsel_n(n411, n413, zn_splat(P8::from_raw(0i32)));
    let n415: ZN = zsel_n(n411, zn_splat(P8::from_raw(-131072i32)), n404);
    let n416: ZN = zsel_n(n405, zn_splat(P8::from_raw(0i32)), n399);
    let n417: ZN = zsel_n(n405, zn_splat(P8::from_raw(0i32)), n414);
    let n418: ZN = zsel_n(n405, zn_splat(P8::from_raw(-131072i32)), n415);
    let n419: ZB = zn_lt(n282, zn_splat(P8::from_raw(-262144i32)));
    let n420: ZB = zn_ge(n282, zn_splat(P8::from_raw(-262144i32)));
    let n421: ZB = zb_and(n395, n419);
    let n426: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n401);
    let n427: ZN = zsel_n(n411, n413, n426);
    let n428: ZN = zsel_n(n405, n426, n427);
    let n429: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n402);
    let n430: ZN = zsel_n(n411, n413, n429);
    let n431: ZN = zsel_n(n405, n429, n430);
    let n433: ZB = zb_or(n322, n326);
    let n434: ZB = zb_or(n318, n433);
    let n435: ZB = zb_or(n311, n434);
    let n436: ZB = zb_or(n351, n355);
    let n437: ZB = zb_or(n347, n436);
    let n438: ZB = zb_or(n342, n437);
    let n439: ZB = zb_or(n380, n384);
    let n440: ZB = zb_or(n376, n439);
    let n441: ZB = zb_or(n371, n440);
    let n442: ZB = zb_or(n438, n441);
    let n443: ZB = zb_or(n435, n442);
    let n444: ZB = zn_gt(n282, zn_splat(P8::from_raw(8388608i32)));
    let n445: ZB = zb_and(n392, n444);
    let n446: ZB = zb_or(n443, n445);
    let n447: ZB = zb_or(n393, n443);
    let n448: ZB = zb_and(n419, n446);
    let n456: ZN = zsel_n(n421, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n457: ZB = zb_not(n421);
    let n458: ZB = zb_or(n421, n448);
    let n459: ZB = zsel_b(n421, n393, n447);
    let n463: ZB = zb_not(n187);
    let n464: ZB = zn_ge(n188, zn_splat(P8::from_raw(0i32)));
    let n465: ZN = zsel_n(n187, n188, r_c258);
    let n466: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n467: ZN = zsel_n(n203, n466, r_c20);
    let n468: ZI = zsel_i(n203, r_c254, zi_splat(n178.0, n178.1));
    let n469: ZB = zi_cmp(Cmp::Ge, n468, zi_splat(P8::from_raw(2981888i32), P8::from_raw(2981888i32)));
    let n470: ZB = zi_cmp(Cmp::Le, n468, zi_splat(P8::from_raw(3309568i32), P8::from_raw(3309568i32)));
    let n472: ZB = zn_gt(r_c258, zn_splat(P8::from_raw(0i32)));
    let n473: ZB = zb_and(n137, n201);
    let n474: ZN = zn_add(r_c377, zn_splat(P8::from_raw(32768i32)));
    let n475: ZB = zn_gt(n474, zn_splat(P8::from_raw(0i32)));
    let n476: ZB = zb_and(n184, n185);
    let n477: ZB = zb_and(n201, n476);
    let n478: ZB = zb_and(n472, n475);
    let n479: ZN = zsel_n(n478, n188, r_c258);
    let n480: ZN = zsel_n(n478, zn_splat(P8::from_raw(0i32)), n474);
    let n481: ZB = zn_gt(n480, zn_splat(P8::from_raw(0i32)));
    let n482: ZB = zb_and(n233, n463);
    let n483: ZB = zb_and(n234, n464);
    let n484: ZN = zsel_n(n187, zn_splat(P8::from_raw(393216i32)), r_c269);
    let n485: ZB = zb_or(n482, n483);
    let n486: ZN = zsel_n(n185, r_c269, n484);
    let n487: ZN = zsel_n(n137, r_c269, n486);
    let n488: ZN = zsel_n(n203, r_c269, n487);
    let n489: ZN = zn_sub(n279, zn_splat(P8::from_raw(32768i32)));
    let n490: ZN = zn_sub(n489, n280);
    let n491: ZN = zsel_n(n231, n490, r_c375);
    let n492: ZB = zn_lt(n282, zn_splat(P8::from_raw(7864320i32)));
    let n493: ZN = zsel_n(n492, zn_splat(P8::from_raw(196608i32)), r_c258);
    let n494: ZN = zsel_n(n492, zn_splat(P8::from_raw(65536i32)), r_c270);
    let n495: ZB = zn_gt(n282, zn_splat(P8::from_raw(6815744i32)));
    let n496: ZB = zb_and(n481, n495);
    let n497: ZN = zsel_n(n496, zn_splat(P8::from_raw(327680i32)), n479);
    let n498: ZN = zsel_n(n496, zn_splat(P8::from_raw(131072i32)), r_c270);
    let n499: ZN = zsel_n(n496, zn_splat(P8::from_raw(6815744i32)), n282);
    let n500: ZN = zsel_n(n496, zn_splat(P8::from_raw(0i32)), n480);
    let n501: ZN = zsel_n(n185, n497, n465);
    let n502: ZN = zsel_n(n185, n498, r_c270);
    let n503: ZN = zsel_n(n185, n499, n282);
    let n504: ZN = zsel_n(n185, n500, r_c377);
    let n505: ZB = zb_or(n477, n485);
    let n506: ZN = zsel_n(n137, n493, n501);
    let n507: ZN = zsel_n(n137, n494, n502);
    let n508: ZN = zsel_n(n137, n282, n503);
    let n509: ZN = zsel_n(n137, r_c377, n504);
    let n510: ZB = zb_or(n473, n505);
    let n511: ZN = zsel_n(n203, r_c258, n506);
    let n512: ZN = zsel_n(n203, r_c270, n507);
    let n513: ZN = zsel_n(n203, r_c274, n508);
    let n514: ZN = zsel_n(n203, r_c375, n491);
    let n515: ZN = zsel_n(n203, r_c377, n509);
    let n516: ZB = zb_or(n203, n510);
    let n519: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n520: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n521: ZW = zw_add(zw_splat(0u64), n519);
    let n522: ZW = zw_add(zw_splat(0u64), n520);
    let n523: ZW = zw_cellmix_n(317u64, n282, 1542469173u64);
    let n524: ZW = zw_cellmix_n(317u64, n282, 668265263u64);
    let n525: ZW = zw_add(n521, n523);
    let n526: ZW = zw_add(n522, n524);
    let n527: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n528: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n529: ZW = zw_add(n525, n527);
    let n530: ZW = zw_add(n526, n528);
    let n531: ZW = zw_cellmix_b(41u64, zb_splat(false), 1542469173u64);
    let n532: ZW = zw_cellmix_b(41u64, zb_splat(false), 668265263u64);
    let n533: ZW = zw_add(n529, n531);
    let n534: ZW = zw_add(n530, n532);
    let n535: ZW = zw_cellmix_n(297u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n536: ZW = zw_cellmix_n(297u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n537: ZW = zw_add(n533, n535);
    let n538: ZW = zw_add(n534, n536);
    let n539: ZW = zw_cellmix_n(299u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n540: ZW = zw_cellmix_n(299u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n541: ZW = zw_add(n537, n539);
    let n542: ZW = zw_add(n538, n540);
    let n543: ZW = zw_cellmix_n(300u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n544: ZW = zw_cellmix_n(300u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n545: ZW = zw_add(n541, n543);
    let n546: ZW = zw_add(n542, n544);
    let n547: ZW = zw_cellmix_n(302u64, n399, 1542469173u64);
    let n548: ZW = zw_cellmix_n(302u64, n399, 668265263u64);
    let n549: ZW = zw_add(n545, n547);
    let n550: ZW = zw_add(n546, n548);
    let n551: ZW = zw_cellmix_b(309u64, zb_splat(false), 1542469173u64);
    let n552: ZW = zw_cellmix_b(309u64, zb_splat(false), 668265263u64);
    let n553: ZW = zw_add(n549, n551);
    let n554: ZW = zw_add(n550, n552);
    let n555: ZW = zw_cellmix_b(310u64, zb_splat(false), 1542469173u64);
    let n556: ZW = zw_cellmix_b(310u64, zb_splat(false), 668265263u64);
    let n557: ZW = zw_add(n553, n555);
    let n558: ZW = zw_add(n554, n556);
    let n559: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n560: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n561: ZW = zw_add(n557, n559);
    let n562: ZW = zw_add(n558, n560);
    let n563: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n564: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n565: ZW = zw_add(n561, n563);
    let n566: ZW = zw_add(n562, n564);
    let n567: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n568: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n569: ZW = zw_add(n565, n567);
    let n570: ZW = zw_add(n566, n568);
    let n571: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n572: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n573: ZW = zw_add(n569, n571);
    let n574: ZW = zw_add(n570, n572);
    let n575: ZW = zw_cellmix_b(398u64, zb_splat(false), 1542469173u64);
    let n576: ZW = zw_cellmix_b(398u64, zb_splat(false), 668265263u64);
    let n577: ZW = zw_add(n573, n575);
    let n578: ZW = zw_add(n574, n576);
    let n579: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n580: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n581: ZW = zw_add(n577, n579);
    let n582: ZW = zw_add(n578, n580);
    let n583: ZW = zw_cellmix_n(407u64, n404, 1542469173u64);
    let n584: ZW = zw_cellmix_n(407u64, n404, 668265263u64);
    let n585: ZW = zw_add(n581, n583);
    let n586: ZW = zw_add(n582, n584);
    let n587: ZW = zw_cellmix_b(398u64, zb_splat(true), 1542469173u64);
    let n588: ZW = zw_cellmix_b(398u64, zb_splat(true), 668265263u64);
    let n589: ZW = zw_add(n573, n587);
    let n590: ZW = zw_add(n574, n588);
    let n591: ZW = zw_cellmix_n(406u64, n426, 1542469173u64);
    let n592: ZW = zw_cellmix_n(406u64, n426, 668265263u64);
    let n593: ZW = zw_add(n589, n591);
    let n594: ZW = zw_add(n590, n592);
    let n595: ZW = zw_add(n593, n583);
    let n596: ZW = zw_add(n594, n584);
    let n597: ZW = zw_cellmix_n(406u64, n429, 1542469173u64);
    let n598: ZW = zw_cellmix_n(406u64, n429, 668265263u64);
    let n599: ZW = zw_add(n577, n597);
    let n600: ZW = zw_add(n578, n598);
    let n601: ZW = zw_add(n599, n583);
    let n602: ZW = zw_add(n600, n584);
    let n603: ZW = zw_cellmix_n(302u64, n416, 1542469173u64);
    let n604: ZW = zw_cellmix_n(302u64, n416, 668265263u64);
    let n605: ZW = zw_add(n545, n603);
    let n606: ZW = zw_add(n546, n604);
    let n607: ZW = zw_add(n605, n551);
    let n608: ZW = zw_add(n606, n552);
    let n609: ZW = zw_cellmix_b(310u64, zb_splat(true), 1542469173u64);
    let n610: ZW = zw_cellmix_b(310u64, zb_splat(true), 668265263u64);
    let n611: ZW = zw_add(n607, n609);
    let n612: ZW = zw_add(n608, n610);
    let n613: ZW = zw_add(n611, n559);
    let n614: ZW = zw_add(n612, n560);
    let n615: ZW = zw_add(n613, n563);
    let n616: ZW = zw_add(n614, n564);
    let n617: ZW = zw_add(n615, n567);
    let n618: ZW = zw_add(n616, n568);
    let n619: ZW = zw_add(n617, n571);
    let n620: ZW = zw_add(n618, n572);
    let n621: ZW = zw_add(n619, n575);
    let n622: ZW = zw_add(n620, n576);
    let n623: ZW = zw_cellmix_n(406u64, n417, 1542469173u64);
    let n624: ZW = zw_cellmix_n(406u64, n417, 668265263u64);
    let n625: ZW = zw_add(n621, n623);
    let n626: ZW = zw_add(n622, n624);
    let n627: ZW = zw_cellmix_n(407u64, n418, 1542469173u64);
    let n628: ZW = zw_cellmix_n(407u64, n418, 668265263u64);
    let n629: ZW = zw_add(n625, n627);
    let n630: ZW = zw_add(n626, n628);
    let n631: ZW = zw_add(n619, n587);
    let n632: ZW = zw_add(n620, n588);
    let n633: ZW = zw_cellmix_n(406u64, n428, 1542469173u64);
    let n634: ZW = zw_cellmix_n(406u64, n428, 668265263u64);
    let n635: ZW = zw_add(n631, n633);
    let n636: ZW = zw_add(n632, n634);
    let n637: ZW = zw_add(n635, n627);
    let n638: ZW = zw_add(n636, n628);
    let n639: ZW = zw_cellmix_n(406u64, n431, 1542469173u64);
    let n640: ZW = zw_cellmix_n(406u64, n431, 668265263u64);
    let n641: ZW = zw_add(n621, n639);
    let n642: ZW = zw_add(n622, n640);
    let n643: ZW = zw_add(n641, n627);
    let n644: ZW = zw_add(n642, n628);
    let n645: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n646: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n647: ZW = zw_add(n525, n645);
    let n648: ZW = zw_add(n526, n646);
    let n649: ZW = zw_cellmix_b(41u64, zb_splat(true), 1542469173u64);
    let n650: ZW = zw_cellmix_b(41u64, zb_splat(true), 668265263u64);
    let n651: ZW = zw_add(n647, n649);
    let n652: ZW = zw_add(n648, n650);
    let n653: ZW = zw_cellmix_n(297u64, zn_splat(P8::from_raw(655360i32)), 1542469173u64);
    let n654: ZW = zw_cellmix_n(297u64, zn_splat(P8::from_raw(655360i32)), 668265263u64);
    let n655: ZW = zw_add(n651, n653);
    let n656: ZW = zw_add(n652, n654);
    let n657: ZW = zw_cellmix_n(299u64, zn_splat(P8::from_raw(262144i32)), 1542469173u64);
    let n658: ZW = zw_cellmix_n(299u64, zn_splat(P8::from_raw(262144i32)), 668265263u64);
    let n659: ZW = zw_add(n655, n657);
    let n660: ZW = zw_add(n656, n658);
    let n661: ZW = zw_cellmix_n(300u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n662: ZW = zw_cellmix_n(300u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n663: ZW = zw_add(n659, n661);
    let n664: ZW = zw_add(n660, n662);
    let n665: ZW = zw_add(n663, n547);
    let n666: ZW = zw_add(n664, n548);
    let n667: ZW = zw_cellmix_b(309u64, zb_splat(true), 1542469173u64);
    let n668: ZW = zw_cellmix_b(309u64, zb_splat(true), 668265263u64);
    let n669: ZW = zw_add(n665, n667);
    let n670: ZW = zw_add(n666, n668);
    let n671: ZW = zw_add(n669, n555);
    let n672: ZW = zw_add(n670, n556);
    let n673: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n674: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n675: ZW = zw_add(n671, n673);
    let n676: ZW = zw_add(n672, n674);
    let n677: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n678: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n679: ZW = zw_add(n675, n677);
    let n680: ZW = zw_add(n676, n678);
    let n681: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n682: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n683: ZW = zw_add(n679, n681);
    let n684: ZW = zw_add(n680, n682);
    let n685: ZW = zw_add(n683, n571);
    let n686: ZW = zw_add(n684, n572);
    let n687: ZW = zw_add(n685, n575);
    let n688: ZW = zw_add(n686, n576);
    let n689: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n690: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n691: ZW = zw_add(n687, n689);
    let n692: ZW = zw_add(n688, n690);
    let n693: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n694: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n695: ZW = zw_add(n691, n693);
    let n696: ZW = zw_add(n692, n694);
    let n697: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(-131072i32)), 1542469173u64);
    let n698: ZW = zw_cellmix_n(396u64, zn_splat(P8::from_raw(-131072i32)), 668265263u64);
    let n699: ZW = zw_add(n679, n697);
    let n700: ZW = zw_add(n680, n698);
    let n701: ZW = zw_add(n699, n571);
    let n702: ZW = zw_add(n700, n572);
    let n703: ZW = zw_add(n701, n587);
    let n704: ZW = zw_add(n702, n588);
    let n705: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n706: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n707: ZW = zw_add(n703, n705);
    let n708: ZW = zw_add(n704, n706);
    let n709: ZW = zw_add(n707, n693);
    let n710: ZW = zw_add(n708, n694);
    let n711: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n712: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n713: ZW = zw_add(n687, n711);
    let n714: ZW = zw_add(n688, n712);
    let n715: ZW = zw_add(n713, n693);
    let n716: ZW = zw_add(n714, n694);
    let n717: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n718: ZW = zw_cellmix_n(394u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n719: ZW = zw_add(n671, n717);
    let n720: ZW = zw_add(n672, n718);
    let n721: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n722: ZW = zw_cellmix_n(395u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n723: ZW = zw_add(n719, n721);
    let n724: ZW = zw_add(n720, n722);
    let n725: ZW = zw_add(n723, n567);
    let n726: ZW = zw_add(n724, n568);
    let n727: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(-98304i32)), 1542469173u64);
    let n728: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(-98304i32)), 668265263u64);
    let n729: ZW = zw_add(n725, n727);
    let n730: ZW = zw_add(n726, n728);
    let n731: ZW = zw_add(n729, n575);
    let n732: ZW = zw_add(n730, n576);
    let n733: ZW = zw_add(n731, n579);
    let n734: ZW = zw_add(n732, n580);
    let n735: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n736: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n737: ZW = zw_add(n733, n735);
    let n738: ZW = zw_add(n734, n736);
    let n739: ZW = zw_add(n719, n677);
    let n740: ZW = zw_add(n720, n678);
    let n741: ZW = zw_add(n739, n697);
    let n742: ZW = zw_add(n740, n698);
    let n743: ZW = zw_add(n741, n727);
    let n744: ZW = zw_add(n742, n728);
    let n745: ZW = zw_add(n743, n587);
    let n746: ZW = zw_add(n744, n588);
    let n747: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n748: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n749: ZW = zw_add(n745, n747);
    let n750: ZW = zw_add(n746, n748);
    let n751: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n752: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n753: ZW = zw_add(n749, n751);
    let n754: ZW = zw_add(n750, n752);
    let n755: ZW = zw_add(n739, n681);
    let n756: ZW = zw_add(n740, n682);
    let n757: ZW = zw_add(n755, n727);
    let n758: ZW = zw_add(n756, n728);
    let n759: ZW = zw_add(n757, n575);
    let n760: ZW = zw_add(n758, n576);
    let n761: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n762: ZW = zw_cellmix_n(406u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n763: ZW = zw_add(n759, n761);
    let n764: ZW = zw_add(n760, n762);
    let n765: ZW = zw_add(n763, n751);
    let n766: ZW = zw_add(n764, n752);
    let n767: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n768: ZW = zw_cellmix_n(397u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n769: ZW = zw_add(n725, n767);
    let n770: ZW = zw_add(n726, n768);
    let n771: ZW = zw_add(n769, n575);
    let n772: ZW = zw_add(n770, n576);
    let n773: ZW = zw_add(n771, n579);
    let n774: ZW = zw_add(n772, n580);
    let n775: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n776: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n777: ZW = zw_add(n773, n775);
    let n778: ZW = zw_add(n774, n776);
    let n779: ZW = zw_add(n741, n767);
    let n780: ZW = zw_add(n742, n768);
    let n781: ZW = zw_add(n779, n587);
    let n782: ZW = zw_add(n780, n588);
    let n783: ZW = zw_add(n781, n747);
    let n784: ZW = zw_add(n782, n748);
    let n785: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n786: ZW = zw_cellmix_n(407u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n787: ZW = zw_add(n783, n785);
    let n788: ZW = zw_add(n784, n786);
    let n789: ZW = zw_add(n755, n767);
    let n790: ZW = zw_add(n756, n768);
    let n791: ZW = zw_add(n789, n575);
    let n792: ZW = zw_add(n790, n576);
    let n793: ZW = zw_add(n791, n761);
    let n794: ZW = zw_add(n792, n762);
    let n795: ZW = zw_add(n793, n785);
    let n796: ZW = zw_add(n794, n786);
    let n797: ZW = zw_add(n663, n603);
    let n798: ZW = zw_add(n664, n604);
    let n799: ZW = zw_add(n797, n667);
    let n800: ZW = zw_add(n798, n668);
    let n801: ZW = zw_add(n799, n609);
    let n802: ZW = zw_add(n800, n610);
    let n803: ZW = zw_add(n801, n673);
    let n804: ZW = zw_add(n802, n674);
    let n805: ZW = zw_add(n803, n677);
    let n806: ZW = zw_add(n804, n678);
    let n807: ZW = zw_add(n805, n681);
    let n808: ZW = zw_add(n806, n682);
    let n809: ZW = zw_add(n807, n571);
    let n810: ZW = zw_add(n808, n572);
    let n811: ZW = zw_add(n809, n575);
    let n812: ZW = zw_add(n810, n576);
    let n813: ZW = zw_add(n811, n689);
    let n814: ZW = zw_add(n812, n690);
    let n815: ZW = zw_add(n813, n693);
    let n816: ZW = zw_add(n814, n694);
    let n817: ZW = zw_add(n805, n697);
    let n818: ZW = zw_add(n806, n698);
    let n819: ZW = zw_add(n817, n571);
    let n820: ZW = zw_add(n818, n572);
    let n821: ZW = zw_add(n819, n587);
    let n822: ZW = zw_add(n820, n588);
    let n823: ZW = zw_add(n821, n705);
    let n824: ZW = zw_add(n822, n706);
    let n825: ZW = zw_add(n823, n693);
    let n826: ZW = zw_add(n824, n694);
    let n827: ZW = zw_add(n811, n711);
    let n828: ZW = zw_add(n812, n712);
    let n829: ZW = zw_add(n827, n693);
    let n830: ZW = zw_add(n828, n694);
    let n831: ZW = zw_add(n801, n717);
    let n832: ZW = zw_add(n802, n718);
    let n833: ZW = zw_add(n831, n721);
    let n834: ZW = zw_add(n832, n722);
    let n835: ZW = zw_add(n833, n567);
    let n836: ZW = zw_add(n834, n568);
    let n837: ZW = zw_add(n835, n727);
    let n838: ZW = zw_add(n836, n728);
    let n839: ZW = zw_add(n837, n575);
    let n840: ZW = zw_add(n838, n576);
    let n841: ZW = zw_add(n839, n579);
    let n842: ZW = zw_add(n840, n580);
    let n843: ZW = zw_add(n841, n735);
    let n844: ZW = zw_add(n842, n736);
    let n845: ZW = zw_add(n831, n677);
    let n846: ZW = zw_add(n832, n678);
    let n847: ZW = zw_add(n845, n697);
    let n848: ZW = zw_add(n846, n698);
    let n849: ZW = zw_add(n847, n727);
    let n850: ZW = zw_add(n848, n728);
    let n851: ZW = zw_add(n849, n587);
    let n852: ZW = zw_add(n850, n588);
    let n853: ZW = zw_add(n851, n747);
    let n854: ZW = zw_add(n852, n748);
    let n855: ZW = zw_add(n853, n751);
    let n856: ZW = zw_add(n854, n752);
    let n857: ZW = zw_add(n845, n681);
    let n858: ZW = zw_add(n846, n682);
    let n859: ZW = zw_add(n857, n727);
    let n860: ZW = zw_add(n858, n728);
    let n861: ZW = zw_add(n859, n575);
    let n862: ZW = zw_add(n860, n576);
    let n863: ZW = zw_add(n861, n761);
    let n864: ZW = zw_add(n862, n762);
    let n865: ZW = zw_add(n863, n751);
    let n866: ZW = zw_add(n864, n752);
    let n867: ZW = zw_add(n835, n767);
    let n868: ZW = zw_add(n836, n768);
    let n869: ZW = zw_add(n867, n575);
    let n870: ZW = zw_add(n868, n576);
    let n871: ZW = zw_add(n869, n579);
    let n872: ZW = zw_add(n870, n580);
    let n873: ZW = zw_add(n871, n775);
    let n874: ZW = zw_add(n872, n776);
    let n875: ZW = zw_add(n847, n767);
    let n876: ZW = zw_add(n848, n768);
    let n877: ZW = zw_add(n875, n587);
    let n878: ZW = zw_add(n876, n588);
    let n879: ZW = zw_add(n877, n747);
    let n880: ZW = zw_add(n878, n748);
    let n881: ZW = zw_add(n879, n785);
    let n882: ZW = zw_add(n880, n786);
    let n883: ZW = zw_add(n857, n767);
    let n884: ZW = zw_add(n858, n768);
    let n885: ZW = zw_add(n883, n575);
    let n886: ZW = zw_add(n884, n576);
    let n887: ZW = zw_add(n885, n761);
    let n888: ZW = zw_add(n886, n762);
    let n889: ZW = zw_add(n887, n785);
    let n890: ZW = zw_add(n888, n786);
    let n891: ZW = zw_add(zw_splat(0u64), n527);
    let n892: ZW = zw_add(zw_splat(0u64), n528);
    let n893: ZW = zw_add(n891, n531);
    let n894: ZW = zw_add(n892, n532);
    let n895: ZW = zw_add(zw_splat(0u64), n645);
    let n896: ZW = zw_add(zw_splat(0u64), n646);
    let n897: ZW = zw_add(n895, n649);
    let n898: ZW = zw_add(n896, n650);
    let n899: ZW = zw_cellmix_b(38u64, n457, 1542469173u64);
    let n900: ZW = zw_cellmix_b(38u64, n457, 668265263u64);
    let n901: ZW = zw_add(zw_splat(0u64), n899);
    let n902: ZW = zw_add(zw_splat(0u64), n900);
    let n903: ZW = zw_cellmix_n(39u64, n456, 1542469173u64);
    let n904: ZW = zw_cellmix_n(39u64, n456, 668265263u64);
    let n905: ZW = zw_add(n901, n903);
    let n906: ZW = zw_add(n902, n904);
    let n907: ZW = zw_add(n905, n527);
    let n908: ZW = zw_add(n906, n528);
    let n909: ZW = zw_add(n905, n645);
    let n910: ZW = zw_add(n906, n646);
    let n911: ZW = zw_cellmix_n(20u64, n467, 1542469173u64);
    let n912: ZW = zw_cellmix_n(20u64, n467, 668265263u64);
    let n913: ZW = zw_add(zw_splat(0u64), n911);
    let n914: ZW = zw_add(zw_splat(0u64), n912);
    let n915: ZW = zw_add(n913, n519);
    let n916: ZW = zw_add(n914, n520);
    let n917: ZW = zw_cellmix_n(258u64, n511, 1542469173u64);
    let n918: ZW = zw_cellmix_n(258u64, n511, 668265263u64);
    let n919: ZW = zw_add(n915, n917);
    let n920: ZW = zw_add(n916, n918);
    let n921: ZW = zw_cellmix_n(269u64, n488, 1542469173u64);
    let n922: ZW = zw_cellmix_n(269u64, n488, 668265263u64);
    let n923: ZW = zw_add(n919, n921);
    let n924: ZW = zw_add(n920, n922);
    let n925: ZW = zw_cellmix_n(270u64, n512, 1542469173u64);
    let n926: ZW = zw_cellmix_n(270u64, n512, 668265263u64);
    let n927: ZW = zw_add(n923, n925);
    let n928: ZW = zw_add(n924, n926);
    let n929: ZW = zw_cellmix_n(274u64, n513, 1542469173u64);
    let n930: ZW = zw_cellmix_n(274u64, n513, 668265263u64);
    let n931: ZW = zw_add(n927, n929);
    let n932: ZW = zw_add(n928, n930);
    let n933: ZW = zw_cellmix_n(375u64, n514, 1542469173u64);
    let n934: ZW = zw_cellmix_n(375u64, n514, 668265263u64);
    let n935: ZW = zw_add(n931, n933);
    let n936: ZW = zw_add(n932, n934);
    let n937: ZW = zw_cellmix_n(377u64, n515, 1542469173u64);
    let n938: ZW = zw_cellmix_n(377u64, n515, 668265263u64);
    let n939: ZW = zw_add(n935, n937);
    let n940: ZW = zw_add(n936, n938);
    let ok_v0_b0: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v0_b0: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v0_b0: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v1_b1: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v1_b1: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v1_b1: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v2_b2: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v2_b2: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v2_b2: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v16_b3: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v16_b3: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v16_b3: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v17_b4: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v17_b4: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v17_b4: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v18_b5: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v18_b5: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v18_b5: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v32_b6: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v32_b6: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v32_b6: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v33_b7: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v33_b7: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v33_b7: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v34_b8: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v34_b8: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v34_b8: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v36_b9: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v36_b9: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v36_b9: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v37_b10: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v37_b10: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v37_b10: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v38_b11: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v38_b11: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v38_b11: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v40_b12: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v40_b12: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v40_b12: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v41_b13: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v41_b13: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v41_b13: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v42_b14: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v42_b14: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v42_b14: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v48_b15: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v48_b15: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v48_b15: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v49_b16: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v49_b16: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v49_b16: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v50_b17: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v50_b17: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v50_b17: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v52_b18: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v52_b18: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v52_b18: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v53_b19: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v53_b19: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v53_b19: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v54_b20: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v54_b20: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v54_b20: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v56_b21: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v56_b21: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v56_b21: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v57_b22: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v57_b22: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v57_b22: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v58_b23: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n393);
    let bd_v58_b23: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v58_b23: u16 = ALL & zb_holds(n392) & zb_holds(n394) & zb_holds(n420);
    let ok_v0_b24: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n447);
    let bd_v0_b24: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v0_b24: u16 = ALL & zb_holds(n420) & zb_holds(n446);
    let ok_v32_b25: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n447);
    let bd_v32_b25: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v32_b25: u16 = ALL & zb_holds(n420) & zb_holds(n446);
    let ok_v0_b26: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n459);
    let bd_v0_b26: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v0_b26: u16 = ALL & zb_holds(n458);
    let ok_v32_b27: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n459);
    let bd_v32_b27: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v32_b27: u16 = ALL & zb_holds(n458);
    let ok_v0_b28: u16 = ALL & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(n166) & zb_holds(r_c307) & zb_holds(n165) & zb_holds(n164) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(n156) & zb_holds(r_c296) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(n151) & zb_holds(r_c288) & zb_holds(n150) & zb_holds(n149) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(n141) & zb_holds(r_c277) & zb_holds(n140) & zb_holds(n139) & zb_holds(n138) & zb_holds(n136) & zb_holds(n135) & zb_holds(n134) & zb_holds(n129) & zb_holds(n128) & zb_holds(r_c257) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(r_c248) & zb_holds(n122) & zb_holds(n121) & zb_holds(n116) & zb_holds(n115) & zb_holds(r_c238) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n111) & zb_holds(n109) & zb_holds(n110) & zb_holds(n469) & zb_holds(n470);
    let bd_v0_b28: bool = !n163 || !n162 || !n161 || !n160 || !n148 || !n147 || !n146 || !n145 || !n133 || !n132 || !n131 || !n130 || !n120 || !n119 || !n118 || !n117;
    let live_v0_b28: u16 = ALL & zb_holds(n516);
    let sh0 = KShared0 {
        c39: r_c39,
        c317: n282,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
        c39: n456,
        c38: n457,
    };
    let sh3 = KShared3 {
        c39: r_c39,
        c20: n467,
        c258: n511,
        c375: n514,
        c377: n515,
        c269: n488,
        c270: n512,
        c274: n513,
    };
    let mut take_0_0: u16 = 0;
    let mut take_0_1: u16 = 0;
    let mut take_0_2: u16 = 0;
    let mut take_0_3: u16 = 0;
    let mut take_0_4: u16 = 0;
    let mut take_0_5: u16 = 0;
    let mut take_0_6: u16 = 0;
    let mut take_0_7: u16 = 0;
    let mut take_0_8: u16 = 0;
    let mut take_0_9: u16 = 0;
    let mut take_0_10: u16 = 0;
    let mut take_0_11: u16 = 0;
    let mut take_0_12: u16 = 0;
    let mut take_0_13: u16 = 0;
    let mut take_0_14: u16 = 0;
    let mut take_0_15: u16 = 0;
    let mut take_0_16: u16 = 0;
    let mut take_0_17: u16 = 0;
    let mut take_0_18: u16 = 0;
    let mut take_0_19: u16 = 0;
    let mut take_0_20: u16 = 0;
    let mut take_0_21: u16 = 0;
    let mut take_0_22: u16 = 0;
    let mut take_0_23: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 29 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 1] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: n404,
        h1: n585, h2: n586,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(true),
        c302: n399,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: n426,
        c407: n404,
        h1: n595, h2: n596,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: n429,
        c407: n404,
        h1: n601, h2: n602,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n417,
        c407: n418,
        h1: n629, h2: n630,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(true),
        c302: n416,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n428,
        c407: n418,
        h1: n637, h2: n638,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c394: zn_splat(P8::from_raw(0i32)),
        c395: zn_splat(P8::from_raw(0i32)),
        c297: zn_splat(P8::from_raw(0i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(0i32)),
        c300: zn_splat(P8::from_raw(65536i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n431,
        c407: n418,
        h1: n643, h2: n644,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(65536i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n695, h2: n696,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n709, h2: n710,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n715, h2: n716,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(98304i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(-327680i32)),
        h1: n737, h2: n738,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n753, h2: n754,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n765, h2: n766,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(98304i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(327680i32)),
        h1: n777, h2: n778,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n787, h2: n788,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n399,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n795, h2: n796,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(65536i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n815, h2: n816,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n825, h2: n826,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(98304i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(0i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n829, h2: n830,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(98304i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(-327680i32)),
        h1: n843, h2: n844,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n855, h2: n856,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(-98304i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n865, h2: n866,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(98304i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(0i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(327680i32)),
        h1: n873, h2: n874,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(-131072i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(true),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n881, h2: n882,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c394: zn_splat(P8::from_raw(69510i32)),
        c395: zn_splat(P8::from_raw(69510i32)),
        c297: zn_splat(P8::from_raw(655360i32)),
        c396: zn_splat(P8::from_raw(131072i32)),
        c397: zn_splat(P8::from_raw(131072i32)),
        c299: zn_splat(P8::from_raw(262144i32)),
        c300: zn_splat(P8::from_raw(0i32)),
        c398: zb_splat(false),
        c302: n416,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n889, h2: n890,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n893, h2: n894,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: n897, h2: n898,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n907, h2: n908,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: zn_splat(P8::from_raw(131072i32)),
        h1: n909, h2: n910,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n939, h2: n940,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
