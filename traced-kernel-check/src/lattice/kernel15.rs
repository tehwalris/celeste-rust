// GENERATED from a TRACED frame (shape 15). Do not edit.
//
// One input shape, 4 output shapes, 49 distinct button
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
    ("objects[0].off", "num"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].start", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
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
    pub c246: ZN,
    pub c364: ZN,
    pub c365: ZN,
    pub c248: u16,
    pub c366: ZN,
    pub c367: ZN,
    pub c250: ZN,
    pub c251: ZN,
    pub c253: ZN,
    pub c254: ZN,
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
    pub c246: u32,
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
        c246: cell("objects[0].off")?,
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
        c246: match &b.cols[s.c246 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
    pub c39: ZN,
    pub c20: ZN,
    pub c38: ZB,
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

/// Append this assignment's lanes that TAKE outcome 0 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet,
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
        if !seen.insert((h1[i], h2[i])) { continue; }
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

/// Append this assignment's lanes that TAKE outcome 1 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet,
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
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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

/// Append this assignment's lanes that TAKE outcome 2 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet,
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
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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

/// Append this assignment's lanes that TAKE outcome 3 and
/// that the kernel is willing to keep. A lane in `deopt` is
/// dropped here and belongs to the interpreter - the caller
/// has `kv.deopt` and must account for it.
/// SKIPS a row whose values another configuration already
/// wrote. The key is over the non-constant cells only - the
/// rest are one value for the whole accumulator and cannot
/// tell two rows apart - so it is a handful of mixes rather
/// than a hundred, computed from values already in
/// registers. A duplicate caught here costs nothing; one
/// caught at the boundary has already been written.
///
/// 128-bit like the boundary's own key, because a collision
/// DROPS a successor rather than merely costing time.
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
    n: usize, seen: &mut RowSet,
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
        if !seen.insert((h1[i], h2[i])) { continue; }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[258] { v.push(sh.c258.lane(i)); }
        if let Col::N(v) = &mut acc.cols[375] { v.push(sh.c375.lane(i)); }
        if let Col::N(v) = &mut acc.cols[377] { v.push(sh.c377.lane(i)); }
        if let Col::N(v) = &mut acc.cols[269] { v.push(sh.c269.lane(i)); }
        if let Col::N(v) = &mut acc.cols[270] { v.push(sh.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[274] { v.push(sh.c274.lane(i)); }
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

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0]);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1]);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2]);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3]);
    }
}

pub fn step(
    b: &Rt2, lo: usize, n: usize, accs: &mut [Rt2], seen: &mut [RowSet],
) -> Option<u16> {
    let (u, s) = bind(b)?;
    let rin = rows(b, &s, lo)?;
    let g = G { cart: &b.cart, cache: &b.cache };
    // The row sets are the CALLER's and reset in O(1) here.
    // Slice-local because that is where the duplication is:
    // it comes from configurations agreeing on one lane, and
    // a lane lives in one slice.
    seen.iter_mut().for_each(|s| s.next_slice());
    let mut sink = Append { accs, seen, n };
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
    let r_c246: ZN = rin.c246;
    let r_c248: ZB = ZB { val: rin.c248, known: ALL };
    let r_c250: ZN = rin.c250;
    let r_c251: ZN = rin.c251;
    let r_c253: ZN = rin.c253;
    let r_c254: ZN = rin.c254;
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
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n111: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n112: ZB = zb_not(r_c41);
    let n113: ZB = zb_not(r_c42);
    let n114: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n115: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n116: ZB = zb_not(r_c358);
    let n117: ZB = zb_not(r_c359);
    let n118: bool = P8::from_raw(524288i32) == u.c360;
    let n119: bool = P8::from_raw(524288i32) == u.c361;
    let n120: bool = P8::from_raw(0i32) == u.c362;
    let n121: bool = P8::from_raw(0i32) == u.c363;
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c364);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c365);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c366);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c367);
    let n126: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c250);
    let n127: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c251);
    let n128: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c253);
    let n129: ZB = zb_not(r_c368);
    let n130: ZB = zb_not(r_c369);
    let n131: bool = P8::from_raw(524288i32) == u.c370;
    let n132: bool = P8::from_raw(524288i32) == u.c371;
    let n133: bool = P8::from_raw(0i32) == u.c372;
    let n134: bool = P8::from_raw(0i32) == u.c373;
    let n135: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c374);
    let n136: ZB = zb_not(r_c267);
    let n137: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c376);
    let n138: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c270);
    let n139: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c378);
    let n140: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c379);
    let n141: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c273);
    let n142: ZB = zb_not(r_c380);
    let n143: ZB = zb_not(r_c381);
    let n144: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c279);
    let n145: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n146: bool = P8::from_raw(524288i32) == u.c382;
    let n147: bool = P8::from_raw(524288i32) == u.c383;
    let n148: bool = P8::from_raw(0i32) == u.c384;
    let n149: bool = P8::from_raw(0i32) == u.c385;
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c386);
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c387);
    let n152: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c388);
    let n153: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c389);
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c290);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c292);
    let n156: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c293);
    let n157: ZB = zb_not(r_c390);
    let n158: ZB = zb_not(r_c391);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c298);
    let n160: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c299);
    let n161: bool = P8::from_raw(524288i32) == u.c392;
    let n162: bool = P8::from_raw(524288i32) == u.c393;
    let n163: bool = P8::from_raw(0i32) == u.c394;
    let n164: bool = P8::from_raw(0i32) == u.c395;
    let n165: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c396);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c397);
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c398);
    let n168: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c399);
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c309);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c311);
    let n171: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c312);
    let n172: ZB = zb_not(r_c43);
    let n173: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n174: ZB = zb_not(r_c38);
    let n181: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c246);
    let n182: ZN = zn_div(n181, zn_splat(P8::from_raw(2621440i32)));
    let n183: ZN = zn_sin(n182);
    let n184: ZN = zn_mul(n183, zn_splat(P8::from_raw(163840i32)));
    let n185: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n184);
    let n186: ZB = zn_ge(n185, zn_splat(P8::from_raw(2981888i32)));
    let n187: ZB = zn_le(n185, zn_splat(P8::from_raw(3309568i32)));
    let n189: ZB = zb_not(n138);
    let n190: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c270);
    let n191: ZB = zb_not(n190);
    let n192: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c270);
    let n193: ZN = zn_sub(r_c258, zn_splat(P8::from_raw(65536i32)));
    let n194: ZB = zn_lt(n193, zn_splat(P8::from_raw(0i32)));
    let n206: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c377);
    let n207: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n209: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n237: ZB = zb_not(n206);
    let n238: ZB = zb_and(n189, n207);
    let n239: ZB = zb_and(n191, n238);
    let n240: ZB = zb_and(n192, n239);
    let n241: ZB = zb_and(n194, n240);
    let n284: ZN = zn_add(r_c375, r_c377);
    let n285: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n284);
    let n286: ZN = zn_flr(n285);
    let n287: ZN = zn_add(r_c274, n286);
    let n288: ZN = zsel_n(n237, n287, r_c274);
    let n289: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n288);
    let n290: ZN = zn_div(n289, zn_splat(P8::from_raw(524288i32)));
    let n291: ZN = zn_flr(n290);
    let n292: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n291);
    let n293: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n289);
    let n294: ZN = zn_sub(n293, zn_splat(P8::from_raw(65536i32)));
    let n295: ZN = zn_div(n294, zn_splat(P8::from_raw(524288i32)));
    let n296: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n295);
    let n297: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n292);
    let n298: ZB = zn_le(n297, n296);
    let n299: ZB = zn_gt(n297, n296);
    let n300: ZB = zb_and(n241, n298);
    let n301: ZB = zb_and(n241, n299);
    let n302: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n297);
    let n303: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n302);
    let n304: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n303);
    let n305: ZB = zb_not(n304);
    let n306: ZB = zb_and(n300, n304);
    let n307: ZB = zb_and(n300, n305);
    let n308: ZN = zn_rem(n294, zn_splat(P8::from_raw(524288i32)));
    let n309: ZB = zn_ge(n308, zn_splat(P8::from_raw(393216i32)));
    let n310: ZB = zn_lt(n308, zn_splat(P8::from_raw(393216i32)));
    let n311: ZB = zb_and(n306, n310);
    let n312: ZB = zb_and(n306, n309);
    let n313: ZN = zn_mul(n297, zn_splat(P8::from_raw(524288i32)));
    let n314: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n313);
    let n315: ZB = zn_eq(n293, n314);
    let n316: ZB = zb_or(n311, n312);
    let n317: ZB = zb_or(n309, n315);
    let n318: ZB = zb_or(n307, n316);
    let n319: ZB = zb_and(n304, n317);
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n318, n319);
    let n322: ZB = zb_and(n318, n320);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZB = zb_and(n320, n323);
    let n325: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n303);
    let n326: ZB = zb_not(n325);
    let n327: ZB = zb_and(n324, n325);
    let n328: ZB = zb_and(n324, n326);
    let n329: ZN = zn_rem(n289, zn_splat(P8::from_raw(524288i32)));
    let n330: ZB = zn_le(n329, zn_splat(P8::from_raw(131072i32)));
    let n331: ZB = zb_or(n327, n328);
    let n332: ZB = zb_and(n325, n330);
    let n333: ZB = zb_not(n332);
    let n334: ZB = zb_and(n331, n332);
    let n335: ZB = zb_and(n331, n333);
    let n336: ZB = zb_or(n334, n335);
    let n337: ZB = zb_and(n333, n336);
    let n338: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n303);
    let n339: ZB = zb_not(n338);
    let n340: ZB = zb_and(n337, n338);
    let n341: ZB = zb_and(n337, n339);
    let n342: ZB = zb_or(n340, n341);
    let n343: ZB = zb_and(n338, n342);
    let n344: ZB = zb_and(n339, n342);
    let n345: ZB = zb_or(n343, n344);
    let n346: ZB = zb_and(n339, n345);
    let n347: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n303);
    let n348: ZB = zb_not(n347);
    let n349: ZB = zb_and(n346, n347);
    let n350: ZB = zb_and(n346, n348);
    let n351: ZB = zb_or(n349, n350);
    let n352: ZB = zb_and(n347, n351);
    let n353: ZB = zb_and(n348, n351);
    let n354: ZB = zb_or(n352, n353);
    let n355: ZB = zb_and(n348, n354);
    let n356: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n292);
    let n357: ZB = zn_le(n356, n296);
    let n358: ZB = zn_gt(n356, n296);
    let n359: ZB = zb_and(n355, n357);
    let n360: ZB = zb_and(n355, n358);
    let n361: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n356);
    let n362: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n361);
    let n363: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n362);
    let n364: ZB = zb_not(n363);
    let n365: ZB = zb_and(n359, n363);
    let n366: ZB = zb_and(n359, n364);
    let n367: ZB = zb_and(n310, n365);
    let n368: ZB = zb_and(n309, n365);
    let n369: ZN = zn_mul(n356, zn_splat(P8::from_raw(524288i32)));
    let n370: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n369);
    let n371: ZB = zn_eq(n293, n370);
    let n372: ZB = zb_or(n367, n368);
    let n373: ZB = zb_or(n309, n371);
    let n374: ZB = zb_or(n366, n372);
    let n375: ZB = zb_and(n363, n373);
    let n376: ZB = zb_not(n375);
    let n377: ZB = zb_and(n374, n375);
    let n378: ZB = zb_and(n374, n376);
    let n379: ZB = zb_or(n377, n378);
    let n380: ZB = zb_and(n376, n379);
    let n381: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n362);
    let n382: ZB = zb_not(n381);
    let n383: ZB = zb_and(n380, n381);
    let n384: ZB = zb_and(n380, n382);
    let n385: ZB = zb_or(n383, n384);
    let n386: ZB = zb_and(n330, n381);
    let n387: ZB = zb_not(n386);
    let n388: ZB = zb_and(n385, n386);
    let n389: ZB = zb_and(n385, n387);
    let n390: ZB = zb_or(n388, n389);
    let n391: ZB = zb_and(n387, n390);
    let n392: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n362);
    let n393: ZB = zb_not(n392);
    let n394: ZB = zb_and(n391, n392);
    let n395: ZB = zb_and(n391, n393);
    let n396: ZB = zb_or(n394, n395);
    let n397: ZB = zb_and(n392, n396);
    let n398: ZB = zb_and(n393, n396);
    let n399: ZB = zb_or(n397, n398);
    let n400: ZB = zb_and(n393, n399);
    let n401: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n362);
    let n402: ZB = zb_not(n401);
    let n403: ZB = zb_and(n400, n401);
    let n404: ZB = zb_and(n400, n402);
    let n405: ZB = zb_or(n403, n404);
    let n406: ZB = zb_and(n401, n405);
    let n407: ZB = zb_and(n402, n405);
    let n408: ZB = zb_or(n406, n407);
    let n409: ZB = zb_and(n402, n408);
    let n410: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n292);
    let n411: ZB = zn_le(n410, n296);
    let n412: ZB = zn_gt(n410, n296);
    let n413: ZB = zb_and(n409, n411);
    let n414: ZB = zb_and(n409, n412);
    let n415: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n410);
    let n416: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n415);
    let n417: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n416);
    let n418: ZB = zb_not(n417);
    let n419: ZB = zb_and(n413, n417);
    let n420: ZB = zb_and(n413, n418);
    let n421: ZB = zb_and(n310, n419);
    let n422: ZB = zb_and(n309, n419);
    let n423: ZN = zn_mul(n410, zn_splat(P8::from_raw(524288i32)));
    let n424: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n423);
    let n425: ZB = zn_eq(n293, n424);
    let n426: ZB = zb_or(n421, n422);
    let n427: ZB = zb_or(n309, n425);
    let n428: ZB = zb_or(n420, n426);
    let n429: ZB = zb_and(n417, n427);
    let n430: ZB = zb_not(n429);
    let n431: ZB = zb_and(n428, n429);
    let n432: ZB = zb_and(n428, n430);
    let n433: ZB = zb_or(n431, n432);
    let n434: ZB = zb_and(n430, n433);
    let n435: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n416);
    let n436: ZB = zb_not(n435);
    let n437: ZB = zb_and(n434, n435);
    let n438: ZB = zb_and(n434, n436);
    let n439: ZB = zb_or(n437, n438);
    let n440: ZB = zb_and(n330, n435);
    let n441: ZB = zb_not(n440);
    let n442: ZB = zb_and(n439, n440);
    let n443: ZB = zb_and(n439, n441);
    let n444: ZB = zb_or(n442, n443);
    let n445: ZB = zb_and(n441, n444);
    let n446: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n416);
    let n447: ZB = zb_not(n446);
    let n448: ZB = zb_and(n445, n446);
    let n449: ZB = zb_and(n445, n447);
    let n450: ZB = zb_or(n448, n449);
    let n451: ZB = zb_and(n446, n450);
    let n452: ZB = zb_and(n447, n450);
    let n453: ZB = zb_or(n451, n452);
    let n454: ZB = zb_and(n447, n453);
    let n455: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n416);
    let n456: ZB = zb_not(n455);
    let n457: ZB = zb_and(n454, n455);
    let n458: ZB = zb_and(n454, n456);
    let n459: ZB = zb_or(n457, n458);
    let n460: ZB = zb_and(n455, n459);
    let n461: ZB = zb_and(n456, n459);
    let n462: ZB = zb_or(n460, n461);
    let n463: ZB = zb_and(n456, n462);
    let n464: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n292);
    let n465: ZB = zn_gt(n464, n296);
    let n466: ZB = zb_or(n414, n463);
    let n467: ZB = zb_or(n412, n465);
    let n468: ZB = zb_or(n360, n466);
    let n469: ZB = zb_or(n358, n467);
    let n470: ZB = zb_or(n301, n468);
    let n471: ZB = zb_or(n299, n469);
    let n472: ZB = zn_le(n288, zn_splat(P8::from_raw(8388608i32)));
    let n473: ZB = zb_and(n470, n472);
    let n474: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n289);
    let n475: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n474, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n476: ZB = zb_not(n475);
    let n477: ZB = zb_and(n473, n476);
    let n478: ZB = zb_and(n473, n475);
    let n479: ZB = zb_or(n477, n478);
    let n480: ZB = zb_and(n476, n479);
    let n481: ZB = zb_and(n475, n479);
    let n482: ZB = zb_or(n480, n481);
    let n483: ZB = zb_and(n475, n482);
    let n484: ZB = zb_and(n476, n482);
    let n485: ZN = zsel_n(n475, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n486: ZB = zb_or(n483, n484);
    let n487: ZB = zb_and(n476, n486);
    let n488: ZB = zb_and(n475, n486);
    let n489: ZN = zsel_n(n476, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n490: ZB = zb_or(n487, n488);
    let n491: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n489);
    let n492: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n489);
    let n493: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n289);
    let n494: ZB = zb_and(n476, n490);
    let n495: ZB = zb_and(n475, n490);
    let n496: ZN = zsel_n(n476, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n497: ZB = zb_or(n494, n495);
    let n498: ZB = zn_gt(n485, zn_splat(P8::from_raw(0i32)));
    let n499: ZB = zn_le(n485, zn_splat(P8::from_raw(0i32)));
    let n500: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n493, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n501: ZB = zb_not(n500);
    let n502: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n493, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n503: ZB = zb_not(n502);
    let n504: ZN = zsel_n(n502, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n505: ZN = zsel_n(n500, zn_splat(P8::from_raw(-65536i32)), n504);
    let n506: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n505);
    let n507: ZB = zb_not(n506);
    let n508: ZN = zn_neg(n505);
    let n509: ZN = zn_mul(n508, zn_splat(P8::from_raw(131072i32)));
    let n510: ZN = zsel_n(n507, n509, zn_splat(P8::from_raw(0i32)));
    let n511: ZN = zsel_n(n507, zn_splat(P8::from_raw(-131072i32)), n496);
    let n512: ZN = zsel_n(n498, zn_splat(P8::from_raw(0i32)), n485);
    let n513: ZN = zsel_n(n498, zn_splat(P8::from_raw(0i32)), n510);
    let n514: ZN = zsel_n(n498, zn_splat(P8::from_raw(-131072i32)), n511);
    let n515: ZB = zn_lt(n288, zn_splat(P8::from_raw(-262144i32)));
    let n516: ZB = zn_ge(n288, zn_splat(P8::from_raw(-262144i32)));
    let n517: ZB = zb_and(n497, n515);
    let n518: ZB = zb_and(n497, n516);
    let n519: ZB = zb_or(n517, n518);
    let n526: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n491);
    let n527: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n493, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n528: ZB = zb_not(n527);
    let n529: ZB = zb_and(n490, n528);
    let n530: ZB = zb_and(n490, n527);
    let n531: ZB = zb_or(n529, n530);
    let n532: ZB = zb_and(n528, n531);
    let n533: ZB = zb_and(n527, n531);
    let n534: ZB = zb_or(n532, n533);
    let n535: ZB = zb_and(n527, n534);
    let n536: ZB = zb_and(n528, n534);
    let n537: ZB = zb_or(n535, n536);
    let n538: ZB = zb_and(n527, n537);
    let n539: ZB = zb_and(n528, n537);
    let n540: ZB = zb_or(n538, n539);
    let n541: ZB = zb_and(n476, n540);
    let n542: ZB = zb_and(n475, n540);
    let n543: ZB = zb_or(n541, n542);
    let n544: ZN = zsel_n(n507, n509, n526);
    let n545: ZN = zsel_n(n498, n526, n544);
    let n546: ZB = zb_and(n515, n543);
    let n547: ZB = zb_and(n516, n543);
    let n548: ZB = zb_or(n546, n547);
    let n551: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n492);
    let n552: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n493, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n553: ZB = zb_not(n552);
    let n554: ZB = zb_and(n490, n553);
    let n555: ZB = zb_and(n490, n552);
    let n556: ZB = zb_or(n554, n555);
    let n557: ZB = zb_and(n553, n556);
    let n558: ZB = zb_and(n552, n556);
    let n559: ZB = zb_or(n557, n558);
    let n560: ZB = zb_and(n552, n559);
    let n561: ZB = zb_and(n553, n559);
    let n562: ZB = zb_or(n560, n561);
    let n563: ZB = zb_and(n552, n562);
    let n564: ZB = zb_and(n553, n562);
    let n565: ZB = zb_or(n563, n564);
    let n566: ZB = zb_and(n476, n565);
    let n567: ZB = zb_and(n475, n565);
    let n568: ZB = zb_or(n566, n567);
    let n569: ZN = zsel_n(n507, n509, n551);
    let n570: ZN = zsel_n(n498, n551, n569);
    let n571: ZB = zb_and(n515, n568);
    let n572: ZB = zb_and(n516, n568);
    let n573: ZB = zb_or(n571, n572);
    let n576: ZB = zb_and(n497, n498);
    let n577: ZB = zb_and(n497, n499);
    let n578: ZB = zb_and(n501, n577);
    let n579: ZB = zb_and(n500, n577);
    let n580: ZB = zb_or(n578, n579);
    let n581: ZB = zb_and(n501, n580);
    let n582: ZB = zb_and(n500, n580);
    let n583: ZB = zb_or(n581, n582);
    let n584: ZB = zb_and(n500, n583);
    let n585: ZB = zb_and(n501, n583);
    let n586: ZB = zb_and(n503, n585);
    let n587: ZB = zb_and(n502, n585);
    let n588: ZB = zb_or(n586, n587);
    let n589: ZB = zb_and(n503, n588);
    let n590: ZB = zb_and(n502, n588);
    let n591: ZB = zb_or(n589, n590);
    let n592: ZB = zb_and(n502, n591);
    let n593: ZB = zb_and(n503, n591);
    let n594: ZB = zb_or(n592, n593);
    let n595: ZB = zb_or(n584, n594);
    let n596: ZB = zb_and(n507, n595);
    let n597: ZB = zb_and(n506, n595);
    let n598: ZB = zb_or(n596, n597);
    let n599: ZB = zb_or(n576, n598);
    let n600: ZB = zb_and(n515, n599);
    let n601: ZB = zb_and(n516, n599);
    let n602: ZB = zb_or(n600, n601);
    let n605: ZB = zb_and(n498, n543);
    let n606: ZB = zb_and(n499, n543);
    let n607: ZB = zb_and(n501, n606);
    let n608: ZB = zb_and(n500, n606);
    let n609: ZB = zb_or(n607, n608);
    let n610: ZB = zb_and(n501, n609);
    let n611: ZB = zb_and(n500, n609);
    let n612: ZB = zb_or(n610, n611);
    let n613: ZB = zb_and(n500, n612);
    let n614: ZB = zb_and(n501, n612);
    let n615: ZB = zb_and(n503, n614);
    let n616: ZB = zb_and(n502, n614);
    let n617: ZB = zb_or(n615, n616);
    let n618: ZB = zb_and(n503, n617);
    let n619: ZB = zb_and(n502, n617);
    let n620: ZB = zb_or(n618, n619);
    let n621: ZB = zb_and(n502, n620);
    let n622: ZB = zb_and(n503, n620);
    let n623: ZB = zb_or(n621, n622);
    let n624: ZB = zb_or(n613, n623);
    let n625: ZB = zb_and(n507, n624);
    let n626: ZB = zb_and(n506, n624);
    let n627: ZB = zb_or(n625, n626);
    let n628: ZB = zb_or(n605, n627);
    let n629: ZB = zb_and(n515, n628);
    let n630: ZB = zb_and(n516, n628);
    let n631: ZB = zb_or(n629, n630);
    let n634: ZB = zb_and(n498, n568);
    let n635: ZB = zb_and(n499, n568);
    let n636: ZB = zb_and(n501, n635);
    let n637: ZB = zb_and(n500, n635);
    let n638: ZB = zb_or(n636, n637);
    let n639: ZB = zb_and(n501, n638);
    let n640: ZB = zb_and(n500, n638);
    let n641: ZB = zb_or(n639, n640);
    let n642: ZB = zb_and(n500, n641);
    let n643: ZB = zb_and(n501, n641);
    let n644: ZB = zb_and(n503, n643);
    let n645: ZB = zb_and(n502, n643);
    let n646: ZB = zb_or(n644, n645);
    let n647: ZB = zb_and(n503, n646);
    let n648: ZB = zb_and(n502, n646);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_and(n502, n649);
    let n651: ZB = zb_and(n503, n649);
    let n652: ZB = zb_or(n650, n651);
    let n653: ZB = zb_or(n642, n652);
    let n654: ZB = zb_and(n507, n653);
    let n655: ZB = zb_and(n506, n653);
    let n656: ZB = zb_or(n654, n655);
    let n657: ZB = zb_or(n634, n656);
    let n658: ZB = zb_and(n515, n657);
    let n659: ZB = zb_and(n516, n657);
    let n660: ZB = zb_or(n658, n659);
    let n664: ZB = zb_and(n319, n323);
    let n665: ZB = zb_and(n332, n336);
    let n666: ZB = zb_and(n338, n345);
    let n667: ZB = zb_and(n347, n354);
    let n668: ZB = zb_or(n666, n667);
    let n669: ZB = zb_or(n665, n668);
    let n670: ZB = zb_or(n664, n669);
    let n671: ZB = zb_and(n375, n379);
    let n672: ZB = zb_and(n386, n390);
    let n673: ZB = zb_and(n392, n399);
    let n674: ZB = zb_and(n401, n408);
    let n675: ZB = zb_or(n673, n674);
    let n676: ZB = zb_or(n672, n675);
    let n677: ZB = zb_or(n671, n676);
    let n678: ZB = zb_and(n429, n433);
    let n679: ZB = zb_and(n440, n444);
    let n680: ZB = zb_and(n446, n453);
    let n681: ZB = zb_and(n455, n462);
    let n682: ZB = zb_or(n680, n681);
    let n683: ZB = zb_or(n679, n682);
    let n684: ZB = zb_or(n678, n683);
    let n685: ZB = zb_or(n677, n684);
    let n686: ZB = zb_or(n670, n685);
    let n687: ZB = zn_gt(n288, zn_splat(P8::from_raw(8388608i32)));
    let n688: ZB = zb_and(n686, n687);
    let n689: ZB = zb_and(n472, n686);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZB = zb_and(n470, n687);
    let n692: ZB = zb_or(n690, n691);
    let n693: ZB = zb_or(n471, n690);
    let n694: ZB = zb_and(n476, n692);
    let n695: ZB = zb_and(n475, n692);
    let n696: ZB = zb_or(n694, n695);
    let n697: ZB = zb_and(n476, n696);
    let n698: ZB = zb_and(n475, n696);
    let n699: ZB = zb_or(n697, n698);
    let n700: ZB = zb_and(n475, n699);
    let n701: ZB = zb_and(n476, n699);
    let n702: ZB = zb_or(n700, n701);
    let n703: ZB = zb_and(n476, n702);
    let n704: ZB = zb_and(n475, n702);
    let n705: ZB = zb_or(n703, n704);
    let n706: ZB = zb_and(n476, n705);
    let n707: ZB = zb_and(n475, n705);
    let n708: ZB = zb_or(n706, n707);
    let n709: ZB = zb_and(n515, n708);
    let n710: ZB = zb_and(n516, n708);
    let n711: ZB = zb_or(n709, n710);
    let n716: ZB = zb_and(n528, n705);
    let n717: ZB = zb_and(n527, n705);
    let n718: ZB = zb_or(n716, n717);
    let n719: ZB = zb_and(n528, n718);
    let n720: ZB = zb_and(n527, n718);
    let n721: ZB = zb_or(n719, n720);
    let n722: ZB = zb_and(n527, n721);
    let n723: ZB = zb_and(n528, n721);
    let n724: ZB = zb_or(n722, n723);
    let n725: ZB = zb_and(n527, n724);
    let n726: ZB = zb_and(n528, n724);
    let n727: ZB = zb_or(n725, n726);
    let n728: ZB = zb_and(n476, n727);
    let n729: ZB = zb_and(n475, n727);
    let n730: ZB = zb_or(n728, n729);
    let n731: ZB = zb_and(n515, n730);
    let n732: ZB = zb_and(n516, n730);
    let n733: ZB = zb_or(n731, n732);
    let n736: ZB = zb_and(n553, n705);
    let n737: ZB = zb_and(n552, n705);
    let n738: ZB = zb_or(n736, n737);
    let n739: ZB = zb_and(n553, n738);
    let n740: ZB = zb_and(n552, n738);
    let n741: ZB = zb_or(n739, n740);
    let n742: ZB = zb_and(n552, n741);
    let n743: ZB = zb_and(n553, n741);
    let n744: ZB = zb_or(n742, n743);
    let n745: ZB = zb_and(n552, n744);
    let n746: ZB = zb_and(n553, n744);
    let n747: ZB = zb_or(n745, n746);
    let n748: ZB = zb_and(n476, n747);
    let n749: ZB = zb_and(n475, n747);
    let n750: ZB = zb_or(n748, n749);
    let n751: ZB = zb_and(n515, n750);
    let n752: ZB = zb_and(n516, n750);
    let n753: ZB = zb_or(n751, n752);
    let n756: ZB = zb_and(n498, n708);
    let n757: ZB = zb_and(n499, n708);
    let n758: ZB = zb_and(n501, n757);
    let n759: ZB = zb_and(n500, n757);
    let n760: ZB = zb_or(n758, n759);
    let n761: ZB = zb_and(n501, n760);
    let n762: ZB = zb_and(n500, n760);
    let n763: ZB = zb_or(n761, n762);
    let n764: ZB = zb_and(n500, n763);
    let n765: ZB = zb_and(n501, n763);
    let n766: ZB = zb_and(n503, n765);
    let n767: ZB = zb_and(n502, n765);
    let n768: ZB = zb_or(n766, n767);
    let n769: ZB = zb_and(n503, n768);
    let n770: ZB = zb_and(n502, n768);
    let n771: ZB = zb_or(n769, n770);
    let n772: ZB = zb_and(n502, n771);
    let n773: ZB = zb_and(n503, n771);
    let n774: ZB = zb_or(n772, n773);
    let n775: ZB = zb_or(n764, n774);
    let n776: ZB = zb_and(n507, n775);
    let n777: ZB = zb_and(n506, n775);
    let n778: ZB = zb_or(n776, n777);
    let n779: ZB = zb_or(n756, n778);
    let n780: ZB = zb_and(n515, n779);
    let n781: ZB = zb_and(n516, n779);
    let n782: ZB = zb_or(n780, n781);
    let n785: ZB = zb_and(n498, n730);
    let n786: ZB = zb_and(n499, n730);
    let n787: ZB = zb_and(n501, n786);
    let n788: ZB = zb_and(n500, n786);
    let n789: ZB = zb_or(n787, n788);
    let n790: ZB = zb_and(n501, n789);
    let n791: ZB = zb_and(n500, n789);
    let n792: ZB = zb_or(n790, n791);
    let n793: ZB = zb_and(n500, n792);
    let n794: ZB = zb_and(n501, n792);
    let n795: ZB = zb_and(n503, n794);
    let n796: ZB = zb_and(n502, n794);
    let n797: ZB = zb_or(n795, n796);
    let n798: ZB = zb_and(n503, n797);
    let n799: ZB = zb_and(n502, n797);
    let n800: ZB = zb_or(n798, n799);
    let n801: ZB = zb_and(n502, n800);
    let n802: ZB = zb_and(n503, n800);
    let n803: ZB = zb_or(n801, n802);
    let n804: ZB = zb_or(n793, n803);
    let n805: ZB = zb_and(n507, n804);
    let n806: ZB = zb_and(n506, n804);
    let n807: ZB = zb_or(n805, n806);
    let n808: ZB = zb_or(n785, n807);
    let n809: ZB = zb_and(n515, n808);
    let n810: ZB = zb_and(n516, n808);
    let n811: ZB = zb_or(n809, n810);
    let n814: ZB = zb_and(n498, n750);
    let n815: ZB = zb_and(n499, n750);
    let n816: ZB = zb_and(n501, n815);
    let n817: ZB = zb_and(n500, n815);
    let n818: ZB = zb_or(n816, n817);
    let n819: ZB = zb_and(n501, n818);
    let n820: ZB = zb_and(n500, n818);
    let n821: ZB = zb_or(n819, n820);
    let n822: ZB = zb_and(n500, n821);
    let n823: ZB = zb_and(n501, n821);
    let n824: ZB = zb_and(n503, n823);
    let n825: ZB = zb_and(n502, n823);
    let n826: ZB = zb_or(n824, n825);
    let n827: ZB = zb_and(n503, n826);
    let n828: ZB = zb_and(n502, n826);
    let n829: ZB = zb_or(n827, n828);
    let n830: ZB = zb_and(n502, n829);
    let n831: ZB = zb_and(n503, n829);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_or(n822, n832);
    let n834: ZB = zb_and(n507, n833);
    let n835: ZB = zb_and(n506, n833);
    let n836: ZB = zb_or(n834, n835);
    let n837: ZB = zb_or(n814, n836);
    let n838: ZB = zb_and(n515, n837);
    let n839: ZB = zb_and(n516, n837);
    let n840: ZB = zb_or(n838, n839);
    let n848: ZB = zb_and(n515, n519);
    let n849: ZB = zb_and(n515, n711);
    let n850: ZN = zsel_n(n848, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n851: ZB = zb_not(n848);
    let n852: ZB = zb_or(n848, n849);
    let n853: ZB = zsel_b(n848, n471, n693);
    let n856: ZB = zb_and(n515, n548);
    let n857: ZB = zb_and(n515, n733);
    let n858: ZN = zsel_n(n856, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n859: ZB = zb_not(n856);
    let n860: ZB = zb_or(n856, n857);
    let n861: ZB = zsel_b(n856, n471, n693);
    let n864: ZB = zb_and(n515, n573);
    let n865: ZB = zb_and(n515, n753);
    let n866: ZN = zsel_n(n864, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n867: ZB = zb_not(n864);
    let n868: ZB = zb_or(n864, n865);
    let n869: ZB = zsel_b(n864, n471, n693);
    let n872: ZB = zb_and(n515, n602);
    let n873: ZB = zb_and(n515, n782);
    let n874: ZN = zsel_n(n872, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n875: ZB = zb_not(n872);
    let n876: ZB = zb_or(n872, n873);
    let n877: ZB = zsel_b(n872, n471, n693);
    let n880: ZB = zb_and(n515, n631);
    let n881: ZB = zb_and(n515, n811);
    let n882: ZN = zsel_n(n880, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n883: ZB = zb_not(n880);
    let n884: ZB = zb_or(n880, n881);
    let n885: ZB = zsel_b(n880, n471, n693);
    let n888: ZB = zb_and(n515, n660);
    let n889: ZB = zb_and(n515, n840);
    let n890: ZN = zsel_n(n888, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n891: ZB = zb_not(n888);
    let n892: ZB = zb_or(n888, n889);
    let n893: ZB = zsel_b(n888, n471, n693);
    let n898: ZB = zb_not(n192);
    let n899: ZB = zn_ge(n193, zn_splat(P8::from_raw(0i32)));
    let n900: ZN = zsel_n(n192, n193, r_c258);
    let n901: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n902: ZN = zsel_n(n209, n901, r_c20);
    let n903: ZN = zsel_n(n209, r_c254, n185);
    let n904: ZB = zn_gt(n902, zn_splat(P8::from_raw(0i32)));
    let n905: ZB = zn_le(n902, zn_splat(P8::from_raw(0i32)));
    let n906: ZB = zn_ge(n903, zn_splat(P8::from_raw(2981888i32)));
    let n907: ZB = zn_le(n903, zn_splat(P8::from_raw(3309568i32)));
    let n909: ZB = zn_gt(r_c258, zn_splat(P8::from_raw(0i32)));
    let n910: ZB = zb_and(n138, n207);
    let n911: ZB = zb_and(n190, n238);
    let n912: ZN = zn_add(r_c377, zn_splat(P8::from_raw(32768i32)));
    let n913: ZB = zn_gt(n912, zn_splat(P8::from_raw(0i32)));
    let n914: ZB = zn_le(n912, zn_splat(P8::from_raw(0i32)));
    let n915: ZB = zb_and(n911, n913);
    let n916: ZB = zb_and(n911, n914);
    let n917: ZB = zb_or(n915, n916);
    let n918: ZB = zb_and(n909, n913);
    let n919: ZB = zb_not(n918);
    let n920: ZB = zb_and(n917, n918);
    let n921: ZB = zb_and(n917, n919);
    let n922: ZN = zsel_n(n918, n193, r_c258);
    let n923: ZN = zsel_n(n918, zn_splat(P8::from_raw(0i32)), n912);
    let n924: ZB = zb_or(n920, n921);
    let n925: ZB = zn_gt(n923, zn_splat(P8::from_raw(0i32)));
    let n926: ZB = zn_le(n923, zn_splat(P8::from_raw(0i32)));
    let n927: ZB = zb_and(n924, n925);
    let n928: ZB = zb_and(n924, n926);
    let n929: ZB = zb_or(n927, n928);
    let n930: ZB = zb_and(n239, n898);
    let n931: ZB = zb_and(n240, n899);
    let n932: ZN = zsel_n(n192, zn_splat(P8::from_raw(393216i32)), r_c269);
    let n933: ZB = zb_or(n930, n931);
    let n934: ZN = zsel_n(n190, r_c269, n932);
    let n935: ZN = zsel_n(n138, r_c269, n934);
    let n936: ZN = zsel_n(n209, r_c269, n935);
    let n937: ZN = zn_sub(n285, zn_splat(P8::from_raw(32768i32)));
    let n938: ZN = zn_sub(n937, n286);
    let n939: ZN = zsel_n(n237, n938, r_c375);
    let n940: ZB = zn_lt(n288, zn_splat(P8::from_raw(7864320i32)));
    let n941: ZB = zn_ge(n288, zn_splat(P8::from_raw(7864320i32)));
    let n942: ZB = zb_and(n910, n940);
    let n943: ZB = zb_and(n910, n941);
    let n944: ZN = zsel_n(n940, zn_splat(P8::from_raw(196608i32)), r_c258);
    let n945: ZN = zsel_n(n940, zn_splat(P8::from_raw(65536i32)), r_c270);
    let n946: ZB = zb_or(n942, n943);
    let n947: ZB = zn_gt(n288, zn_splat(P8::from_raw(6815744i32)));
    let n948: ZB = zb_and(n925, n947);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zb_and(n929, n948);
    let n951: ZB = zb_and(n929, n949);
    let n952: ZN = zsel_n(n948, zn_splat(P8::from_raw(327680i32)), n922);
    let n953: ZN = zsel_n(n948, zn_splat(P8::from_raw(131072i32)), r_c270);
    let n954: ZN = zsel_n(n948, zn_splat(P8::from_raw(6815744i32)), n288);
    let n955: ZN = zsel_n(n948, zn_splat(P8::from_raw(0i32)), n923);
    let n956: ZB = zb_or(n950, n951);
    let n957: ZN = zsel_n(n190, n952, n900);
    let n958: ZN = zsel_n(n190, n953, r_c270);
    let n959: ZN = zsel_n(n190, n954, n288);
    let n960: ZN = zsel_n(n190, n955, r_c377);
    let n961: ZB = zb_or(n933, n956);
    let n962: ZN = zsel_n(n138, n944, n957);
    let n963: ZN = zsel_n(n138, n945, n958);
    let n964: ZN = zsel_n(n138, n288, n959);
    let n965: ZN = zsel_n(n138, r_c377, n960);
    let n966: ZB = zb_or(n946, n961);
    let n967: ZN = zsel_n(n209, r_c258, n962);
    let n968: ZN = zsel_n(n209, r_c270, n963);
    let n969: ZN = zsel_n(n209, r_c274, n964);
    let n970: ZN = zsel_n(n209, r_c375, n939);
    let n971: ZN = zsel_n(n209, r_c377, n965);
    let n972: ZB = zb_or(n209, n966);
    let n973: ZB = zb_and(n904, n972);
    let n974: ZB = zb_and(n905, n972);
    let n975: ZB = zb_or(n973, n974);
    let n979: ZW = zw_bits_n(r_c39);
    let n980: ZW = zw_mix1(zw_splat(11400714819323198485u64), n979, 39u64);
    let n981: ZW = zw_mix2(zw_splat(11562461410679940143u64), n979, 39u64);
    let n982: ZW = zw_bits_n(n288);
    let n983: ZW = zw_mix1(n980, n982, 317u64);
    let n984: ZW = zw_mix2(n981, n982, 317u64);
    let n985: ZW = zw_bits_n(r_c20);
    let n986: ZW = zw_mix1(n983, n985, 20u64);
    let n987: ZW = zw_mix2(n984, n985, 20u64);
    let n988: u64 = false as u64;
    let n989: ZW = zw_mix1(n986, zw_splat(n988), 41u64);
    let n990: ZW = zw_mix2(n987, zw_splat(n988), 41u64);
    let n991: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n992: ZW = zw_mix1(n989, zw_splat(n991), 297u64);
    let n993: ZW = zw_mix2(n990, zw_splat(n991), 297u64);
    let n994: ZW = zw_mix1(n992, zw_splat(n991), 299u64);
    let n995: ZW = zw_mix2(n993, zw_splat(n991), 299u64);
    let n996: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n997: ZW = zw_mix1(n994, zw_splat(n996), 300u64);
    let n998: ZW = zw_mix2(n995, zw_splat(n996), 300u64);
    let n999: ZW = zw_bits_n(n485);
    let n1000: ZW = zw_mix1(n997, n999, 302u64);
    let n1001: ZW = zw_mix2(n998, n999, 302u64);
    let n1002: ZW = zw_mix1(n1000, zw_splat(n988), 309u64);
    let n1003: ZW = zw_mix2(n1001, zw_splat(n988), 309u64);
    let n1004: ZW = zw_mix1(n1002, zw_splat(n988), 310u64);
    let n1005: ZW = zw_mix2(n1003, zw_splat(n988), 310u64);
    let n1006: ZW = zw_mix1(n1004, zw_splat(n991), 394u64);
    let n1007: ZW = zw_mix2(n1005, zw_splat(n991), 394u64);
    let n1008: ZW = zw_mix1(n1006, zw_splat(n991), 395u64);
    let n1009: ZW = zw_mix2(n1007, zw_splat(n991), 395u64);
    let n1010: ZW = zw_mix1(n1008, zw_splat(n991), 396u64);
    let n1011: ZW = zw_mix2(n1009, zw_splat(n991), 396u64);
    let n1012: ZW = zw_mix1(n1010, zw_splat(n991), 397u64);
    let n1013: ZW = zw_mix2(n1011, zw_splat(n991), 397u64);
    let n1014: ZW = zw_mix1(n1012, zw_splat(n988), 398u64);
    let n1015: ZW = zw_mix2(n1013, zw_splat(n988), 398u64);
    let n1016: ZW = zw_mix1(n1014, zw_splat(n991), 406u64);
    let n1017: ZW = zw_mix2(n1015, zw_splat(n991), 406u64);
    let n1018: ZW = zw_bits_n(n496);
    let n1019: ZW = zw_mix1(n1016, n1018, 407u64);
    let n1020: ZW = zw_mix2(n1017, n1018, 407u64);
    let n1021: u64 = true as u64;
    let n1022: ZW = zw_mix1(n1012, zw_splat(n1021), 398u64);
    let n1023: ZW = zw_mix2(n1013, zw_splat(n1021), 398u64);
    let n1024: ZW = zw_bits_n(n526);
    let n1025: ZW = zw_mix1(n1022, n1024, 406u64);
    let n1026: ZW = zw_mix2(n1023, n1024, 406u64);
    let n1027: ZW = zw_mix1(n1025, n1018, 407u64);
    let n1028: ZW = zw_mix2(n1026, n1018, 407u64);
    let n1029: ZW = zw_bits_n(n551);
    let n1030: ZW = zw_mix1(n1014, n1029, 406u64);
    let n1031: ZW = zw_mix2(n1015, n1029, 406u64);
    let n1032: ZW = zw_mix1(n1030, n1018, 407u64);
    let n1033: ZW = zw_mix2(n1031, n1018, 407u64);
    let n1034: ZW = zw_bits_n(n512);
    let n1035: ZW = zw_mix1(n997, n1034, 302u64);
    let n1036: ZW = zw_mix2(n998, n1034, 302u64);
    let n1037: ZW = zw_mix1(n1035, zw_splat(n988), 309u64);
    let n1038: ZW = zw_mix2(n1036, zw_splat(n988), 309u64);
    let n1039: ZW = zw_mix1(n1037, zw_splat(n1021), 310u64);
    let n1040: ZW = zw_mix2(n1038, zw_splat(n1021), 310u64);
    let n1041: ZW = zw_mix1(n1039, zw_splat(n991), 394u64);
    let n1042: ZW = zw_mix2(n1040, zw_splat(n991), 394u64);
    let n1043: ZW = zw_mix1(n1041, zw_splat(n991), 395u64);
    let n1044: ZW = zw_mix2(n1042, zw_splat(n991), 395u64);
    let n1045: ZW = zw_mix1(n1043, zw_splat(n991), 396u64);
    let n1046: ZW = zw_mix2(n1044, zw_splat(n991), 396u64);
    let n1047: ZW = zw_mix1(n1045, zw_splat(n991), 397u64);
    let n1048: ZW = zw_mix2(n1046, zw_splat(n991), 397u64);
    let n1049: ZW = zw_mix1(n1047, zw_splat(n988), 398u64);
    let n1050: ZW = zw_mix2(n1048, zw_splat(n988), 398u64);
    let n1051: ZW = zw_bits_n(n513);
    let n1052: ZW = zw_mix1(n1049, n1051, 406u64);
    let n1053: ZW = zw_mix2(n1050, n1051, 406u64);
    let n1054: ZW = zw_bits_n(n514);
    let n1055: ZW = zw_mix1(n1052, n1054, 407u64);
    let n1056: ZW = zw_mix2(n1053, n1054, 407u64);
    let n1057: ZW = zw_mix1(n1047, zw_splat(n1021), 398u64);
    let n1058: ZW = zw_mix2(n1048, zw_splat(n1021), 398u64);
    let n1059: ZW = zw_bits_n(n545);
    let n1060: ZW = zw_mix1(n1057, n1059, 406u64);
    let n1061: ZW = zw_mix2(n1058, n1059, 406u64);
    let n1062: ZW = zw_mix1(n1060, n1054, 407u64);
    let n1063: ZW = zw_mix2(n1061, n1054, 407u64);
    let n1064: ZW = zw_bits_n(n570);
    let n1065: ZW = zw_mix1(n1049, n1064, 406u64);
    let n1066: ZW = zw_mix2(n1050, n1064, 406u64);
    let n1067: ZW = zw_mix1(n1065, n1054, 407u64);
    let n1068: ZW = zw_mix2(n1066, n1054, 407u64);
    let n1069: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n1070: ZW = zw_mix1(n983, zw_splat(n1069), 20u64);
    let n1071: ZW = zw_mix2(n984, zw_splat(n1069), 20u64);
    let n1072: ZW = zw_mix1(n1070, zw_splat(n1021), 41u64);
    let n1073: ZW = zw_mix2(n1071, zw_splat(n1021), 41u64);
    let n1074: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n1075: ZW = zw_mix1(n1072, zw_splat(n1074), 297u64);
    let n1076: ZW = zw_mix2(n1073, zw_splat(n1074), 297u64);
    let n1077: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n1078: ZW = zw_mix1(n1075, zw_splat(n1077), 299u64);
    let n1079: ZW = zw_mix2(n1076, zw_splat(n1077), 299u64);
    let n1080: ZW = zw_mix1(n1078, zw_splat(n991), 300u64);
    let n1081: ZW = zw_mix2(n1079, zw_splat(n991), 300u64);
    let n1082: ZW = zw_mix1(n1080, n999, 302u64);
    let n1083: ZW = zw_mix2(n1081, n999, 302u64);
    let n1084: ZW = zw_mix1(n1082, zw_splat(n1021), 309u64);
    let n1085: ZW = zw_mix2(n1083, zw_splat(n1021), 309u64);
    let n1086: ZW = zw_mix1(n1084, zw_splat(n988), 310u64);
    let n1087: ZW = zw_mix2(n1085, zw_splat(n988), 310u64);
    let n1088: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n1089: ZW = zw_mix1(n1086, zw_splat(n1088), 394u64);
    let n1090: ZW = zw_mix2(n1087, zw_splat(n1088), 394u64);
    let n1091: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n1092: ZW = zw_mix1(n1089, zw_splat(n1091), 395u64);
    let n1093: ZW = zw_mix2(n1090, zw_splat(n1091), 395u64);
    let n1094: ZW = zw_mix1(n1092, zw_splat(n1069), 396u64);
    let n1095: ZW = zw_mix2(n1093, zw_splat(n1069), 396u64);
    let n1096: ZW = zw_mix1(n1094, zw_splat(n991), 397u64);
    let n1097: ZW = zw_mix2(n1095, zw_splat(n991), 397u64);
    let n1098: ZW = zw_mix1(n1096, zw_splat(n988), 398u64);
    let n1099: ZW = zw_mix2(n1097, zw_splat(n988), 398u64);
    let n1100: ZW = zw_mix1(n1098, zw_splat(n996), 406u64);
    let n1101: ZW = zw_mix2(n1099, zw_splat(n996), 406u64);
    let n1102: ZW = zw_mix1(n1100, zw_splat(n991), 407u64);
    let n1103: ZW = zw_mix2(n1101, zw_splat(n991), 407u64);
    let n1104: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n1105: ZW = zw_mix1(n1092, zw_splat(n1104), 396u64);
    let n1106: ZW = zw_mix2(n1093, zw_splat(n1104), 396u64);
    let n1107: ZW = zw_mix1(n1105, zw_splat(n991), 397u64);
    let n1108: ZW = zw_mix2(n1106, zw_splat(n991), 397u64);
    let n1109: ZW = zw_mix1(n1107, zw_splat(n1021), 398u64);
    let n1110: ZW = zw_mix2(n1108, zw_splat(n1021), 398u64);
    let n1111: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n1112: ZW = zw_mix1(n1109, zw_splat(n1111), 406u64);
    let n1113: ZW = zw_mix2(n1110, zw_splat(n1111), 406u64);
    let n1114: ZW = zw_mix1(n1112, zw_splat(n991), 407u64);
    let n1115: ZW = zw_mix2(n1113, zw_splat(n991), 407u64);
    let n1116: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n1117: ZW = zw_mix1(n1098, zw_splat(n1116), 406u64);
    let n1118: ZW = zw_mix2(n1099, zw_splat(n1116), 406u64);
    let n1119: ZW = zw_mix1(n1117, zw_splat(n991), 407u64);
    let n1120: ZW = zw_mix2(n1118, zw_splat(n991), 407u64);
    let n1121: ZW = zw_mix1(n1086, zw_splat(n1091), 394u64);
    let n1122: ZW = zw_mix2(n1087, zw_splat(n1091), 394u64);
    let n1123: ZW = zw_mix1(n1121, zw_splat(n1088), 395u64);
    let n1124: ZW = zw_mix2(n1122, zw_splat(n1088), 395u64);
    let n1125: ZW = zw_mix1(n1123, zw_splat(n991), 396u64);
    let n1126: ZW = zw_mix2(n1124, zw_splat(n991), 396u64);
    let n1127: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n1128: ZW = zw_mix1(n1125, zw_splat(n1127), 397u64);
    let n1129: ZW = zw_mix2(n1126, zw_splat(n1127), 397u64);
    let n1130: ZW = zw_mix1(n1128, zw_splat(n988), 398u64);
    let n1131: ZW = zw_mix2(n1129, zw_splat(n988), 398u64);
    let n1132: ZW = zw_mix1(n1130, zw_splat(n991), 406u64);
    let n1133: ZW = zw_mix2(n1131, zw_splat(n991), 406u64);
    let n1134: ZW = zw_mix1(n1132, zw_splat(n1111), 407u64);
    let n1135: ZW = zw_mix2(n1133, zw_splat(n1111), 407u64);
    let n1136: ZW = zw_mix1(n1121, zw_splat(n1091), 395u64);
    let n1137: ZW = zw_mix2(n1122, zw_splat(n1091), 395u64);
    let n1138: ZW = zw_mix1(n1136, zw_splat(n1104), 396u64);
    let n1139: ZW = zw_mix2(n1137, zw_splat(n1104), 396u64);
    let n1140: ZW = zw_mix1(n1138, zw_splat(n1127), 397u64);
    let n1141: ZW = zw_mix2(n1139, zw_splat(n1127), 397u64);
    let n1142: ZW = zw_mix1(n1140, zw_splat(n1021), 398u64);
    let n1143: ZW = zw_mix2(n1141, zw_splat(n1021), 398u64);
    let n1144: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1145: ZW = zw_mix1(n1142, zw_splat(n1144), 406u64);
    let n1146: ZW = zw_mix2(n1143, zw_splat(n1144), 406u64);
    let n1147: ZW = zw_mix1(n1145, zw_splat(n1144), 407u64);
    let n1148: ZW = zw_mix2(n1146, zw_splat(n1144), 407u64);
    let n1149: ZW = zw_mix1(n1136, zw_splat(n1069), 396u64);
    let n1150: ZW = zw_mix2(n1137, zw_splat(n1069), 396u64);
    let n1151: ZW = zw_mix1(n1149, zw_splat(n1127), 397u64);
    let n1152: ZW = zw_mix2(n1150, zw_splat(n1127), 397u64);
    let n1153: ZW = zw_mix1(n1151, zw_splat(n988), 398u64);
    let n1154: ZW = zw_mix2(n1152, zw_splat(n988), 398u64);
    let n1155: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1156: ZW = zw_mix1(n1153, zw_splat(n1155), 406u64);
    let n1157: ZW = zw_mix2(n1154, zw_splat(n1155), 406u64);
    let n1158: ZW = zw_mix1(n1156, zw_splat(n1144), 407u64);
    let n1159: ZW = zw_mix2(n1157, zw_splat(n1144), 407u64);
    let n1160: ZW = zw_mix1(n1125, zw_splat(n1069), 397u64);
    let n1161: ZW = zw_mix2(n1126, zw_splat(n1069), 397u64);
    let n1162: ZW = zw_mix1(n1160, zw_splat(n988), 398u64);
    let n1163: ZW = zw_mix2(n1161, zw_splat(n988), 398u64);
    let n1164: ZW = zw_mix1(n1162, zw_splat(n991), 406u64);
    let n1165: ZW = zw_mix2(n1163, zw_splat(n991), 406u64);
    let n1166: ZW = zw_mix1(n1164, zw_splat(n1116), 407u64);
    let n1167: ZW = zw_mix2(n1165, zw_splat(n1116), 407u64);
    let n1168: ZW = zw_mix1(n1138, zw_splat(n1069), 397u64);
    let n1169: ZW = zw_mix2(n1139, zw_splat(n1069), 397u64);
    let n1170: ZW = zw_mix1(n1168, zw_splat(n1021), 398u64);
    let n1171: ZW = zw_mix2(n1169, zw_splat(n1021), 398u64);
    let n1172: ZW = zw_mix1(n1170, zw_splat(n1144), 406u64);
    let n1173: ZW = zw_mix2(n1171, zw_splat(n1144), 406u64);
    let n1174: ZW = zw_mix1(n1172, zw_splat(n1155), 407u64);
    let n1175: ZW = zw_mix2(n1173, zw_splat(n1155), 407u64);
    let n1176: ZW = zw_mix1(n1149, zw_splat(n1069), 397u64);
    let n1177: ZW = zw_mix2(n1150, zw_splat(n1069), 397u64);
    let n1178: ZW = zw_mix1(n1176, zw_splat(n988), 398u64);
    let n1179: ZW = zw_mix2(n1177, zw_splat(n988), 398u64);
    let n1180: ZW = zw_mix1(n1178, zw_splat(n1155), 406u64);
    let n1181: ZW = zw_mix2(n1179, zw_splat(n1155), 406u64);
    let n1182: ZW = zw_mix1(n1180, zw_splat(n1155), 407u64);
    let n1183: ZW = zw_mix2(n1181, zw_splat(n1155), 407u64);
    let n1184: ZW = zw_mix1(n1080, n1034, 302u64);
    let n1185: ZW = zw_mix2(n1081, n1034, 302u64);
    let n1186: ZW = zw_mix1(n1184, zw_splat(n1021), 309u64);
    let n1187: ZW = zw_mix2(n1185, zw_splat(n1021), 309u64);
    let n1188: ZW = zw_mix1(n1186, zw_splat(n1021), 310u64);
    let n1189: ZW = zw_mix2(n1187, zw_splat(n1021), 310u64);
    let n1190: ZW = zw_mix1(n1188, zw_splat(n1088), 394u64);
    let n1191: ZW = zw_mix2(n1189, zw_splat(n1088), 394u64);
    let n1192: ZW = zw_mix1(n1190, zw_splat(n1091), 395u64);
    let n1193: ZW = zw_mix2(n1191, zw_splat(n1091), 395u64);
    let n1194: ZW = zw_mix1(n1192, zw_splat(n1069), 396u64);
    let n1195: ZW = zw_mix2(n1193, zw_splat(n1069), 396u64);
    let n1196: ZW = zw_mix1(n1194, zw_splat(n991), 397u64);
    let n1197: ZW = zw_mix2(n1195, zw_splat(n991), 397u64);
    let n1198: ZW = zw_mix1(n1196, zw_splat(n988), 398u64);
    let n1199: ZW = zw_mix2(n1197, zw_splat(n988), 398u64);
    let n1200: ZW = zw_mix1(n1198, zw_splat(n996), 406u64);
    let n1201: ZW = zw_mix2(n1199, zw_splat(n996), 406u64);
    let n1202: ZW = zw_mix1(n1200, zw_splat(n991), 407u64);
    let n1203: ZW = zw_mix2(n1201, zw_splat(n991), 407u64);
    let n1204: ZW = zw_mix1(n1192, zw_splat(n1104), 396u64);
    let n1205: ZW = zw_mix2(n1193, zw_splat(n1104), 396u64);
    let n1206: ZW = zw_mix1(n1204, zw_splat(n991), 397u64);
    let n1207: ZW = zw_mix2(n1205, zw_splat(n991), 397u64);
    let n1208: ZW = zw_mix1(n1206, zw_splat(n1021), 398u64);
    let n1209: ZW = zw_mix2(n1207, zw_splat(n1021), 398u64);
    let n1210: ZW = zw_mix1(n1208, zw_splat(n1111), 406u64);
    let n1211: ZW = zw_mix2(n1209, zw_splat(n1111), 406u64);
    let n1212: ZW = zw_mix1(n1210, zw_splat(n991), 407u64);
    let n1213: ZW = zw_mix2(n1211, zw_splat(n991), 407u64);
    let n1214: ZW = zw_mix1(n1198, zw_splat(n1116), 406u64);
    let n1215: ZW = zw_mix2(n1199, zw_splat(n1116), 406u64);
    let n1216: ZW = zw_mix1(n1214, zw_splat(n991), 407u64);
    let n1217: ZW = zw_mix2(n1215, zw_splat(n991), 407u64);
    let n1218: ZW = zw_mix1(n1188, zw_splat(n1091), 394u64);
    let n1219: ZW = zw_mix2(n1189, zw_splat(n1091), 394u64);
    let n1220: ZW = zw_mix1(n1218, zw_splat(n1088), 395u64);
    let n1221: ZW = zw_mix2(n1219, zw_splat(n1088), 395u64);
    let n1222: ZW = zw_mix1(n1220, zw_splat(n991), 396u64);
    let n1223: ZW = zw_mix2(n1221, zw_splat(n991), 396u64);
    let n1224: ZW = zw_mix1(n1222, zw_splat(n1127), 397u64);
    let n1225: ZW = zw_mix2(n1223, zw_splat(n1127), 397u64);
    let n1226: ZW = zw_mix1(n1224, zw_splat(n988), 398u64);
    let n1227: ZW = zw_mix2(n1225, zw_splat(n988), 398u64);
    let n1228: ZW = zw_mix1(n1226, zw_splat(n991), 406u64);
    let n1229: ZW = zw_mix2(n1227, zw_splat(n991), 406u64);
    let n1230: ZW = zw_mix1(n1228, zw_splat(n1111), 407u64);
    let n1231: ZW = zw_mix2(n1229, zw_splat(n1111), 407u64);
    let n1232: ZW = zw_mix1(n1218, zw_splat(n1091), 395u64);
    let n1233: ZW = zw_mix2(n1219, zw_splat(n1091), 395u64);
    let n1234: ZW = zw_mix1(n1232, zw_splat(n1104), 396u64);
    let n1235: ZW = zw_mix2(n1233, zw_splat(n1104), 396u64);
    let n1236: ZW = zw_mix1(n1234, zw_splat(n1127), 397u64);
    let n1237: ZW = zw_mix2(n1235, zw_splat(n1127), 397u64);
    let n1238: ZW = zw_mix1(n1236, zw_splat(n1021), 398u64);
    let n1239: ZW = zw_mix2(n1237, zw_splat(n1021), 398u64);
    let n1240: ZW = zw_mix1(n1238, zw_splat(n1144), 406u64);
    let n1241: ZW = zw_mix2(n1239, zw_splat(n1144), 406u64);
    let n1242: ZW = zw_mix1(n1240, zw_splat(n1144), 407u64);
    let n1243: ZW = zw_mix2(n1241, zw_splat(n1144), 407u64);
    let n1244: ZW = zw_mix1(n1232, zw_splat(n1069), 396u64);
    let n1245: ZW = zw_mix2(n1233, zw_splat(n1069), 396u64);
    let n1246: ZW = zw_mix1(n1244, zw_splat(n1127), 397u64);
    let n1247: ZW = zw_mix2(n1245, zw_splat(n1127), 397u64);
    let n1248: ZW = zw_mix1(n1246, zw_splat(n988), 398u64);
    let n1249: ZW = zw_mix2(n1247, zw_splat(n988), 398u64);
    let n1250: ZW = zw_mix1(n1248, zw_splat(n1155), 406u64);
    let n1251: ZW = zw_mix2(n1249, zw_splat(n1155), 406u64);
    let n1252: ZW = zw_mix1(n1250, zw_splat(n1144), 407u64);
    let n1253: ZW = zw_mix2(n1251, zw_splat(n1144), 407u64);
    let n1254: ZW = zw_mix1(n1222, zw_splat(n1069), 397u64);
    let n1255: ZW = zw_mix2(n1223, zw_splat(n1069), 397u64);
    let n1256: ZW = zw_mix1(n1254, zw_splat(n988), 398u64);
    let n1257: ZW = zw_mix2(n1255, zw_splat(n988), 398u64);
    let n1258: ZW = zw_mix1(n1256, zw_splat(n991), 406u64);
    let n1259: ZW = zw_mix2(n1257, zw_splat(n991), 406u64);
    let n1260: ZW = zw_mix1(n1258, zw_splat(n1116), 407u64);
    let n1261: ZW = zw_mix2(n1259, zw_splat(n1116), 407u64);
    let n1262: ZW = zw_mix1(n1234, zw_splat(n1069), 397u64);
    let n1263: ZW = zw_mix2(n1235, zw_splat(n1069), 397u64);
    let n1264: ZW = zw_mix1(n1262, zw_splat(n1021), 398u64);
    let n1265: ZW = zw_mix2(n1263, zw_splat(n1021), 398u64);
    let n1266: ZW = zw_mix1(n1264, zw_splat(n1144), 406u64);
    let n1267: ZW = zw_mix2(n1265, zw_splat(n1144), 406u64);
    let n1268: ZW = zw_mix1(n1266, zw_splat(n1155), 407u64);
    let n1269: ZW = zw_mix2(n1267, zw_splat(n1155), 407u64);
    let n1270: ZW = zw_mix1(n1244, zw_splat(n1069), 397u64);
    let n1271: ZW = zw_mix2(n1245, zw_splat(n1069), 397u64);
    let n1272: ZW = zw_mix1(n1270, zw_splat(n988), 398u64);
    let n1273: ZW = zw_mix2(n1271, zw_splat(n988), 398u64);
    let n1274: ZW = zw_mix1(n1272, zw_splat(n1155), 406u64);
    let n1275: ZW = zw_mix2(n1273, zw_splat(n1155), 406u64);
    let n1276: ZW = zw_mix1(n1274, zw_splat(n1155), 407u64);
    let n1277: ZW = zw_mix2(n1275, zw_splat(n1155), 407u64);
    let n1278: ZW = zw_mix1(zw_splat(11400714819323198485u64), n985, 20u64);
    let n1279: ZW = zw_mix2(zw_splat(11562461410679940143u64), n985, 20u64);
    let n1280: ZW = zw_mix1(n1278, zw_splat(n988), 41u64);
    let n1281: ZW = zw_mix2(n1279, zw_splat(n988), 41u64);
    let n1282: u64 = mix64(11400714819323198485u64 ^ mix64(n1069 ^ 20u64));
    let n1283: u64 = 11562461410679940143u64.wrapping_add(mix64(n1069.wrapping_mul((20u64 << 1) | 1)));
    let n1284: u64 = mix64(n1282 ^ mix64(n1021 ^ 41u64));
    let n1285: u64 = n1283.wrapping_add(mix64(n1021.wrapping_mul((41u64 << 1) | 1)));
    let n1286: ZW = zw_bits_b(n851);
    let n1287: ZW = zw_mix1(n1278, n1286, 38u64);
    let n1288: ZW = zw_mix2(n1279, n1286, 38u64);
    let n1289: ZW = zw_bits_n(n850);
    let n1290: ZW = zw_mix1(n1287, n1289, 39u64);
    let n1291: ZW = zw_mix2(n1288, n1289, 39u64);
    let n1292: ZW = zw_bits_b(n859);
    let n1293: ZW = zw_mix1(n1278, n1292, 38u64);
    let n1294: ZW = zw_mix2(n1279, n1292, 38u64);
    let n1295: ZW = zw_bits_n(n858);
    let n1296: ZW = zw_mix1(n1293, n1295, 39u64);
    let n1297: ZW = zw_mix2(n1294, n1295, 39u64);
    let n1298: ZW = zw_bits_b(n867);
    let n1299: ZW = zw_mix1(n1278, n1298, 38u64);
    let n1300: ZW = zw_mix2(n1279, n1298, 38u64);
    let n1301: ZW = zw_bits_n(n866);
    let n1302: ZW = zw_mix1(n1299, n1301, 39u64);
    let n1303: ZW = zw_mix2(n1300, n1301, 39u64);
    let n1304: ZW = zw_bits_b(n875);
    let n1305: ZW = zw_mix1(n1278, n1304, 38u64);
    let n1306: ZW = zw_mix2(n1279, n1304, 38u64);
    let n1307: ZW = zw_bits_n(n874);
    let n1308: ZW = zw_mix1(n1305, n1307, 39u64);
    let n1309: ZW = zw_mix2(n1306, n1307, 39u64);
    let n1310: ZW = zw_bits_b(n883);
    let n1311: ZW = zw_mix1(n1278, n1310, 38u64);
    let n1312: ZW = zw_mix2(n1279, n1310, 38u64);
    let n1313: ZW = zw_bits_n(n882);
    let n1314: ZW = zw_mix1(n1311, n1313, 39u64);
    let n1315: ZW = zw_mix2(n1312, n1313, 39u64);
    let n1316: ZW = zw_bits_b(n891);
    let n1317: ZW = zw_mix1(n1278, n1316, 38u64);
    let n1318: ZW = zw_mix2(n1279, n1316, 38u64);
    let n1319: ZW = zw_bits_n(n890);
    let n1320: ZW = zw_mix1(n1317, n1319, 39u64);
    let n1321: ZW = zw_mix2(n1318, n1319, 39u64);
    let n1322: ZW = zw_mix1(zw_splat(n1282), n1286, 38u64);
    let n1323: ZW = zw_mix2(zw_splat(n1283), n1286, 38u64);
    let n1324: ZW = zw_mix1(n1322, n1289, 39u64);
    let n1325: ZW = zw_mix2(n1323, n1289, 39u64);
    let n1326: ZW = zw_mix1(zw_splat(n1282), n1292, 38u64);
    let n1327: ZW = zw_mix2(zw_splat(n1283), n1292, 38u64);
    let n1328: ZW = zw_mix1(n1326, n1295, 39u64);
    let n1329: ZW = zw_mix2(n1327, n1295, 39u64);
    let n1330: ZW = zw_mix1(zw_splat(n1282), n1298, 38u64);
    let n1331: ZW = zw_mix2(zw_splat(n1283), n1298, 38u64);
    let n1332: ZW = zw_mix1(n1330, n1301, 39u64);
    let n1333: ZW = zw_mix2(n1331, n1301, 39u64);
    let n1334: ZW = zw_mix1(zw_splat(n1282), n1304, 38u64);
    let n1335: ZW = zw_mix2(zw_splat(n1283), n1304, 38u64);
    let n1336: ZW = zw_mix1(n1334, n1307, 39u64);
    let n1337: ZW = zw_mix2(n1335, n1307, 39u64);
    let n1338: ZW = zw_mix1(zw_splat(n1282), n1310, 38u64);
    let n1339: ZW = zw_mix2(zw_splat(n1283), n1310, 38u64);
    let n1340: ZW = zw_mix1(n1338, n1313, 39u64);
    let n1341: ZW = zw_mix2(n1339, n1313, 39u64);
    let n1342: ZW = zw_mix1(zw_splat(n1282), n1316, 38u64);
    let n1343: ZW = zw_mix2(zw_splat(n1283), n1316, 38u64);
    let n1344: ZW = zw_mix1(n1342, n1319, 39u64);
    let n1345: ZW = zw_mix2(n1343, n1319, 39u64);
    let n1346: ZW = zw_bits_n(n902);
    let n1347: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1346, 20u64);
    let n1348: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1346, 20u64);
    let n1349: ZW = zw_mix1(n1347, n979, 39u64);
    let n1350: ZW = zw_mix2(n1348, n979, 39u64);
    let n1351: ZW = zw_bits_n(n967);
    let n1352: ZW = zw_mix1(n1349, n1351, 258u64);
    let n1353: ZW = zw_mix2(n1350, n1351, 258u64);
    let n1354: ZW = zw_bits_n(n936);
    let n1355: ZW = zw_mix1(n1352, n1354, 269u64);
    let n1356: ZW = zw_mix2(n1353, n1354, 269u64);
    let n1357: ZW = zw_bits_n(n968);
    let n1358: ZW = zw_mix1(n1355, n1357, 270u64);
    let n1359: ZW = zw_mix2(n1356, n1357, 270u64);
    let n1360: ZW = zw_bits_n(n969);
    let n1361: ZW = zw_mix1(n1358, n1360, 274u64);
    let n1362: ZW = zw_mix2(n1359, n1360, 274u64);
    let n1363: ZW = zw_bits_n(n970);
    let n1364: ZW = zw_mix1(n1361, n1363, 375u64);
    let n1365: ZW = zw_mix2(n1362, n1363, 375u64);
    let n1366: ZW = zw_bits_n(n971);
    let n1367: ZW = zw_mix1(n1364, n1366, 377u64);
    let n1368: ZW = zw_mix2(n1365, n1366, 377u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v0_b0: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v0_b0: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n519);
    let ok_v1_b1: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v1_b1: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v1_b1: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n548);
    let ok_v2_b2: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v2_b2: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v2_b2: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n573);
    let ok_v16_b3: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v16_b3: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v16_b3: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n602);
    let ok_v17_b4: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v17_b4: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v17_b4: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n631);
    let ok_v18_b5: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v18_b5: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v18_b5: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n660);
    let ok_v32_b6: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v32_b6: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v32_b6: u16 = ALL & zb_holds(n516) & zb_holds(n519);
    let ok_v33_b7: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v33_b7: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v33_b7: u16 = ALL & zb_holds(n516) & zb_holds(n548);
    let ok_v34_b8: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v34_b8: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v34_b8: u16 = ALL & zb_holds(n516) & zb_holds(n573);
    let ok_v36_b9: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v36_b9: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v36_b9: u16 = ALL & zb_holds(n516) & zb_holds(n519);
    let ok_v37_b10: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v37_b10: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v37_b10: u16 = ALL & zb_holds(n516) & zb_holds(n548);
    let ok_v38_b11: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v38_b11: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v38_b11: u16 = ALL & zb_holds(n516) & zb_holds(n573);
    let ok_v40_b12: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v40_b12: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v40_b12: u16 = ALL & zb_holds(n516) & zb_holds(n519);
    let ok_v41_b13: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v41_b13: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v41_b13: u16 = ALL & zb_holds(n516) & zb_holds(n548);
    let ok_v42_b14: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v42_b14: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v42_b14: u16 = ALL & zb_holds(n516) & zb_holds(n573);
    let ok_v48_b15: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v48_b15: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v48_b15: u16 = ALL & zb_holds(n516) & zb_holds(n602);
    let ok_v49_b16: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v49_b16: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v49_b16: u16 = ALL & zb_holds(n516) & zb_holds(n631);
    let ok_v50_b17: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v50_b17: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v50_b17: u16 = ALL & zb_holds(n516) & zb_holds(n660);
    let ok_v52_b18: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v52_b18: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v52_b18: u16 = ALL & zb_holds(n516) & zb_holds(n602);
    let ok_v53_b19: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v53_b19: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v53_b19: u16 = ALL & zb_holds(n516) & zb_holds(n631);
    let ok_v54_b20: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v54_b20: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v54_b20: u16 = ALL & zb_holds(n516) & zb_holds(n660);
    let ok_v56_b21: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v56_b21: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v56_b21: u16 = ALL & zb_holds(n516) & zb_holds(n602);
    let ok_v57_b22: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v57_b22: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v57_b22: u16 = ALL & zb_holds(n516) & zb_holds(n631);
    let ok_v58_b23: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n471);
    let bd_v58_b23: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v58_b23: u16 = ALL & zb_holds(n516) & zb_holds(n660);
    let ok_v0_b24: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v0_b24: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v0_b24: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n711);
    let ok_v1_b25: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v1_b25: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v1_b25: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n733);
    let ok_v2_b26: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v2_b26: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v2_b26: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n753);
    let ok_v16_b27: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v16_b27: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v16_b27: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n782);
    let ok_v17_b28: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v17_b28: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v17_b28: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n811);
    let ok_v18_b29: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v18_b29: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v18_b29: u16 = ALL & zb_holds(n207) & zb_holds(n516) & zb_holds(n840);
    let ok_v32_b30: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v32_b30: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v32_b30: u16 = ALL & zb_holds(n516) & zb_holds(n711);
    let ok_v33_b31: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v33_b31: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v33_b31: u16 = ALL & zb_holds(n516) & zb_holds(n733);
    let ok_v34_b32: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v34_b32: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v34_b32: u16 = ALL & zb_holds(n516) & zb_holds(n753);
    let ok_v48_b33: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v48_b33: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v48_b33: u16 = ALL & zb_holds(n516) & zb_holds(n782);
    let ok_v49_b34: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v49_b34: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v49_b34: u16 = ALL & zb_holds(n516) & zb_holds(n811);
    let ok_v50_b35: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n186) & zb_holds(n187) & zb_holds(n693);
    let bd_v50_b35: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v50_b35: u16 = ALL & zb_holds(n516) & zb_holds(n840);
    let ok_v0_b36: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n853);
    let bd_v0_b36: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v0_b36: u16 = ALL & zb_holds(n207) & zb_holds(n852);
    let ok_v1_b37: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n861);
    let bd_v1_b37: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v1_b37: u16 = ALL & zb_holds(n207) & zb_holds(n860);
    let ok_v2_b38: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n869);
    let bd_v2_b38: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v2_b38: u16 = ALL & zb_holds(n207) & zb_holds(n868);
    let ok_v16_b39: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n877);
    let bd_v16_b39: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v16_b39: u16 = ALL & zb_holds(n207) & zb_holds(n876);
    let ok_v17_b40: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n885);
    let bd_v17_b40: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v17_b40: u16 = ALL & zb_holds(n207) & zb_holds(n884);
    let ok_v18_b41: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n893);
    let bd_v18_b41: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v18_b41: u16 = ALL & zb_holds(n207) & zb_holds(n892);
    let ok_v32_b42: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n853);
    let bd_v32_b42: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v32_b42: u16 = ALL & zb_holds(n852);
    let ok_v33_b43: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n861);
    let bd_v33_b43: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v33_b43: u16 = ALL & zb_holds(n860);
    let ok_v34_b44: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n869);
    let bd_v34_b44: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v34_b44: u16 = ALL & zb_holds(n868);
    let ok_v48_b45: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n877);
    let bd_v48_b45: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v48_b45: u16 = ALL & zb_holds(n876);
    let ok_v49_b46: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n885);
    let bd_v49_b46: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v49_b46: u16 = ALL & zb_holds(n884);
    let ok_v50_b47: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n893);
    let bd_v50_b47: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v50_b47: u16 = ALL & zb_holds(n892);
    let ok_v0_b48: u16 = ALL & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n171) & zb_holds(n170) & zb_holds(n169) & zb_holds(n168) & zb_holds(n167) & zb_holds(r_c307) & zb_holds(n166) & zb_holds(n165) & zb_holds(n160) & zb_holds(n159) & zb_holds(n158) & zb_holds(n157) & zb_holds(r_c296) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(n152) & zb_holds(r_c288) & zb_holds(n151) & zb_holds(n150) & zb_holds(n145) & zb_holds(n144) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c277) & zb_holds(n141) & zb_holds(n140) & zb_holds(n139) & zb_holds(n137) & zb_holds(n136) & zb_holds(n135) & zb_holds(n130) & zb_holds(n129) & zb_holds(r_c257) & zb_holds(n128) & zb_holds(n127) & zb_holds(n126) & zb_holds(n125) & zb_holds(n124) & zb_holds(r_c248) & zb_holds(n123) & zb_holds(n122) & zb_holds(n117) & zb_holds(n116) & zb_holds(r_c238) & zb_holds(n115) & zb_holds(n114) & zb_holds(n113) & zb_holds(n112) & zb_holds(n110) & zb_holds(n111) & zb_holds(n906) & zb_holds(n907);
    let bd_v0_b48: bool = !n164 || !n163 || !n162 || !n161 || !n149 || !n148 || !n147 || !n146 || !n134 || !n133 || !n132 || !n131 || !n121 || !n120 || !n119 || !n118;
    let live_v0_b48: u16 = ALL & zb_holds(n975);
    let sh0 = KShared0 {
        c39: r_c39,
        c317: n288,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
    };
    let sh3 = KShared3 {
        c39: r_c39,
        c20: n902,
        c258: n967,
        c375: n970,
        c377: n971,
        c269: n936,
        c270: n968,
        c274: n969,
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
    let mut take_2_2: u16 = 0;
    let mut take_2_3: u16 = 0;
    let mut take_2_4: u16 = 0;
    let mut take_2_5: u16 = 0;
    let mut take_2_6: u16 = 0;
    let mut take_2_7: u16 = 0;
    let mut take_2_8: u16 = 0;
    let mut take_2_9: u16 = 0;
    let mut take_2_10: u16 = 0;
    let mut take_2_11: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 49 distinct button assignments; per outcome they fall
    // into [24, 2, 12, 1] groups that write identical values.
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
        c302: n485,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: n496,
        h1: n1019, h2: n1020,
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
        c302: n485,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: n526,
        c407: n496,
        h1: n1027, h2: n1028,
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
        c302: n485,
        c309: zb_splat(false),
        c310: zb_splat(false),
        c406: n551,
        c407: n496,
        h1: n1032, h2: n1033,
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
        c302: n512,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n513,
        c407: n514,
        h1: n1055, h2: n1056,
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
        c302: n512,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n545,
        c407: n514,
        h1: n1062, h2: n1063,
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
        c302: n512,
        c309: zb_splat(false),
        c310: zb_splat(true),
        c406: n570,
        c407: n514,
        h1: n1067, h2: n1068,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(65536i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1102, h2: n1103,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1114, h2: n1115,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1119, h2: n1120,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(-327680i32)),
        h1: n1134, h2: n1135,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n1147, h2: n1148,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n1158, h2: n1159,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(327680i32)),
        h1: n1166, h2: n1167,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n1174, h2: n1175,
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
        c302: n485,
        c309: zb_splat(true),
        c310: zb_splat(false),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n1182, h2: n1183,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(65536i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1202, h2: n1203,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1212, h2: n1213,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(327680i32)),
        c407: zn_splat(P8::from_raw(0i32)),
        h1: n1216, h2: n1217,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(-327680i32)),
        h1: n1230, h2: n1231,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n1242, h2: n1243,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(-231700i32)),
        h1: n1252, h2: n1253,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(0i32)),
        c407: zn_splat(P8::from_raw(327680i32)),
        h1: n1260, h2: n1261,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(-231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n1268, h2: n1269,
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
        c302: n512,
        c309: zb_splat(true),
        c310: zb_splat(true),
        c406: zn_splat(P8::from_raw(231700i32)),
        c407: zn_splat(P8::from_raw(231700i32)),
        h1: n1276, h2: n1277,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_0 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_0 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_0 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_0 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_0 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n1280, h2: n1281,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_1 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_1 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_1 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    declined |= live_v48_b33 & (if bd_v48_b33 { ALL } else { !ok_v48_b33 });
    take_1_1 |= live_v48_b33 & ok_v48_b33 & (if bd_v48_b33 { 0 } else { ALL });
    declined |= live_v49_b34 & (if bd_v49_b34 { ALL } else { !ok_v49_b34 });
    take_1_1 |= live_v49_b34 & ok_v49_b34 & (if bd_v49_b34 { 0 } else { ALL });
    declined |= live_v50_b35 & (if bd_v50_b35 { ALL } else { !ok_v50_b35 });
    take_1_1 |= live_v50_b35 & ok_v50_b35 & (if bd_v50_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: zw_splat(n1284), h2: zw_splat(n1285),
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n850,
        c20: r_c20,
        c38: n851,
        h1: n1290, h2: n1291,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n858,
        c20: r_c20,
        c38: n859,
        h1: n1296, h2: n1297,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n866,
        c20: r_c20,
        c38: n867,
        h1: n1302, h2: n1303,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n874,
        c20: r_c20,
        c38: n875,
        h1: n1308, h2: n1309,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n882,
        c20: r_c20,
        c38: n883,
        h1: n1314, h2: n1315,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n890,
        c20: r_c20,
        c38: n891,
        h1: n1320, h2: n1321,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n850,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n851,
        h1: n1324, h2: n1325,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n858,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n859,
        h1: n1328, h2: n1329,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n866,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n867,
        h1: n1332, h2: n1333,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n874,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n875,
        h1: n1336, h2: n1337,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n882,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n883,
        h1: n1340, h2: n1341,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n890,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n891,
        h1: n1344, h2: n1345,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n1367, h2: n1368,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
