// GENERATED from a TRACED frame (shape 13). Do not edit.
//
// One input shape, 6 output shapes, 56 distinct button
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
pub const SHAPE: u64 = 8632593074248271721;

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
    pub c368: P8,
    pub c369: P8,
    pub c370: P8,
    pub c371: P8,
    pub c378: P8,
    pub c379: P8,
    pub c380: P8,
    pub c381: P8,
    pub c388: P8,
    pub c389: P8,
    pub c390: P8,
    pub c391: P8,
    pub c402: P8,
    pub c403: P8,
    pub c404: P8,
    pub c405: P8,
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
    ("objects[1].hide_for", "num"),
    ("objects[1].hide_in", "num"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
    ("objects[2].collideable", "bool"),
    ("objects[2].delay", "num"),
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
    ("objects[3].dash_accel.x", "num"),
    ("objects[3].dash_accel.y", "num"),
    ("objects[3].dash_effect_time", "num"),
    ("objects[3].dash_target.x", "num"),
    ("objects[3].dash_target.y", "num"),
    ("objects[3].dash_time", "num"),
    ("objects[3].djump", "num"),
    ("objects[3].flip.x", "bool"),
    ("objects[3].flip.y", "bool"),
    ("objects[3].grace", "num"),
    ("objects[3].p_dash", "bool"),
    ("objects[3].p_jump", "bool"),
    ("objects[3].rem.x", "num"),
    ("objects[3].rem.y", "num"),
    ("objects[3].solids", "bool"),
    ("objects[3].spd.x", "num"),
    ("objects[3].spd.y", "num"),
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
    pub c366: u16,
    pub c367: u16,
    pub c246: ZN,
    pub c372: ZN,
    pub c373: ZN,
    pub c248: u16,
    pub c374: ZN,
    pub c375: ZN,
    pub c250: ZN,
    pub c251: ZN,
    pub c253: ZN,
    pub c254: ZN,
    pub c257: u16,
    pub c258: ZN,
    pub c376: u16,
    pub c377: u16,
    pub c260: ZN,
    pub c261: ZN,
    pub c382: ZN,
    pub c383: ZN,
    pub c269: u16,
    pub c384: ZN,
    pub c385: ZN,
    pub c271: ZN,
    pub c273: ZN,
    pub c274: ZN,
    pub c277: u16,
    pub c278: ZN,
    pub c386: u16,
    pub c387: u16,
    pub c280: ZN,
    pub c281: ZN,
    pub c392: ZN,
    pub c393: ZN,
    pub c289: u16,
    pub c394: ZN,
    pub c395: ZN,
    pub c291: ZN,
    pub c293: ZN,
    pub c294: ZN,
    pub c297: u16,
    pub c396: ZN,
    pub c397: ZN,
    pub c299: ZN,
    pub c398: ZN,
    pub c399: ZN,
    pub c301: ZN,
    pub c302: ZN,
    pub c400: u16,
    pub c401: u16,
    pub c304: ZN,
    pub c311: u16,
    pub c312: u16,
    pub c406: ZN,
    pub c407: ZN,
    pub c314: u16,
    pub c408: ZN,
    pub c409: ZN,
    pub c318: ZN,
    pub c319: ZN,
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
    pub c366: u32,
    pub c367: u32,
    pub c246: u32,
    pub c372: u32,
    pub c373: u32,
    pub c248: u32,
    pub c374: u32,
    pub c375: u32,
    pub c250: u32,
    pub c251: u32,
    pub c253: u32,
    pub c254: u32,
    pub c257: u32,
    pub c258: u32,
    pub c376: u32,
    pub c377: u32,
    pub c260: u32,
    pub c261: u32,
    pub c382: u32,
    pub c383: u32,
    pub c269: u32,
    pub c384: u32,
    pub c385: u32,
    pub c271: u32,
    pub c273: u32,
    pub c274: u32,
    pub c277: u32,
    pub c278: u32,
    pub c386: u32,
    pub c387: u32,
    pub c280: u32,
    pub c281: u32,
    pub c392: u32,
    pub c393: u32,
    pub c289: u32,
    pub c394: u32,
    pub c395: u32,
    pub c291: u32,
    pub c293: u32,
    pub c294: u32,
    pub c297: u32,
    pub c396: u32,
    pub c397: u32,
    pub c299: u32,
    pub c398: u32,
    pub c399: u32,
    pub c301: u32,
    pub c302: u32,
    pub c400: u32,
    pub c401: u32,
    pub c304: u32,
    pub c311: u32,
    pub c312: u32,
    pub c406: u32,
    pub c407: u32,
    pub c314: u32,
    pub c408: u32,
    pub c409: u32,
    pub c318: u32,
    pub c319: u32,
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
        c368: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c369: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c370: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c371: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c378: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c379: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c380: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c381: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c388: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c389: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c390: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c391: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c402: match &b.cols[cell("objects[3].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c403: match &b.cols[cell("objects[3].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c404: match &b.cols[cell("objects[3].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c405: match &b.cols[cell("objects[3].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c366: cell("objects[0].flip.x")?,
        c367: cell("objects[0].flip.y")?,
        c246: cell("objects[0].off")?,
        c372: cell("objects[0].rem.x")?,
        c373: cell("objects[0].rem.y")?,
        c248: cell("objects[0].solids")?,
        c374: cell("objects[0].spd.x")?,
        c375: cell("objects[0].spd.y")?,
        c250: cell("objects[0].spr")?,
        c251: cell("objects[0].start")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
        c257: cell("objects[1].collideable")?,
        c258: cell("objects[1].delay")?,
        c376: cell("objects[1].flip.x")?,
        c377: cell("objects[1].flip.y")?,
        c260: cell("objects[1].hide_for")?,
        c261: cell("objects[1].hide_in")?,
        c382: cell("objects[1].rem.x")?,
        c383: cell("objects[1].rem.y")?,
        c269: cell("objects[1].solids")?,
        c384: cell("objects[1].spd.x")?,
        c385: cell("objects[1].spd.y")?,
        c271: cell("objects[1].spr")?,
        c273: cell("objects[1].x")?,
        c274: cell("objects[1].y")?,
        c277: cell("objects[2].collideable")?,
        c278: cell("objects[2].delay")?,
        c386: cell("objects[2].flip.x")?,
        c387: cell("objects[2].flip.y")?,
        c280: cell("objects[2].hide_for")?,
        c281: cell("objects[2].hide_in")?,
        c392: cell("objects[2].rem.x")?,
        c393: cell("objects[2].rem.y")?,
        c289: cell("objects[2].solids")?,
        c394: cell("objects[2].spd.x")?,
        c395: cell("objects[2].spd.y")?,
        c291: cell("objects[2].spr")?,
        c293: cell("objects[2].x")?,
        c294: cell("objects[2].y")?,
        c297: cell("objects[3].collideable")?,
        c396: cell("objects[3].dash_accel.x")?,
        c397: cell("objects[3].dash_accel.y")?,
        c299: cell("objects[3].dash_effect_time")?,
        c398: cell("objects[3].dash_target.x")?,
        c399: cell("objects[3].dash_target.y")?,
        c301: cell("objects[3].dash_time")?,
        c302: cell("objects[3].djump")?,
        c400: cell("objects[3].flip.x")?,
        c401: cell("objects[3].flip.y")?,
        c304: cell("objects[3].grace")?,
        c311: cell("objects[3].p_dash")?,
        c312: cell("objects[3].p_jump")?,
        c406: cell("objects[3].rem.x")?,
        c407: cell("objects[3].rem.y")?,
        c314: cell("objects[3].solids")?,
        c408: cell("objects[3].spd.x")?,
        c409: cell("objects[3].spd.y")?,
        c318: cell("objects[3].x")?,
        c319: cell("objects[3].y")?,
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
        c366: match &b.cols[s.c366 as usize] {
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
        c367: match &b.cols[s.c367 as usize] {
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
        c372: match &b.cols[s.c372 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c373: match &b.cols[s.c373 as usize] {
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
        c376: match &b.cols[s.c376 as usize] {
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
        c377: match &b.cols[s.c377 as usize] {
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
        c260: match &b.cols[s.c260 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c261: match &b.cols[s.c261 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c382: match &b.cols[s.c382 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c383: match &b.cols[s.c383 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c269: match &b.cols[s.c269 as usize] {
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
        c384: match &b.cols[s.c384 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c385: match &b.cols[s.c385 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
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
        c278: match &b.cols[s.c278 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c386: match &b.cols[s.c386 as usize] {
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
        c387: match &b.cols[s.c387 as usize] {
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
        c280: match &b.cols[s.c280 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c281: match &b.cols[s.c281 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c392: match &b.cols[s.c392 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c393: match &b.cols[s.c393 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c289: match &b.cols[s.c289 as usize] {
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
        c394: match &b.cols[s.c394 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c395: match &b.cols[s.c395 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c291: match &b.cols[s.c291 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c293: match &b.cols[s.c293 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c294: match &b.cols[s.c294 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c297: match &b.cols[s.c297 as usize] {
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
        c299: match &b.cols[s.c299 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c301: match &b.cols[s.c301 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c302: match &b.cols[s.c302 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c400: match &b.cols[s.c400 as usize] {
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
        c401: match &b.cols[s.c401 as usize] {
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
        c304: match &b.cols[s.c304 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
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
        c312: match &b.cols[s.c312 as usize] {
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
        c406: match &b.cols[s.c406 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c407: match &b.cols[s.c407 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c314: match &b.cols[s.c314 as usize] {
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
        c408: match &b.cols[s.c408 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c409: match &b.cols[s.c409 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c318: match &b.cols[s.c318 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c319: match &b.cols[s.c319 as usize] {
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
    (180, "balloon.tile"),
    (207, "big_chest.tile"),
    (199, "chest.if_not_fruit"),
    (201, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (193, "fake_wall.if_not_fruit"),
    (194, "fake_wall.tile"),
    (183, "fall_floor.tile"),
    (189, "fly_fruit.if_not_fruit"),
    (191, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (185, "fruit.if_not_fruit"),
    (187, "fruit.tile"),
    (175, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (196, "key.if_not_fruit"),
    (197, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (240, "objects[0].collideable"),
    (241, "objects[0].delay"),
    (338, "objects[0].flip.x"),
    (339, "objects[0].flip.y"),
    (243, "objects[0].hide_for"),
    (244, "objects[0].hide_in"),
    (340, "objects[0].hitbox.h"),
    (341, "objects[0].hitbox.w"),
    (342, "objects[0].hitbox.x"),
    (343, "objects[0].hitbox.y"),
    (344, "objects[0].rem.x"),
    (345, "objects[0].rem.y"),
    (252, "objects[0].solids"),
    (346, "objects[0].spd.x"),
    (347, "objects[0].spd.y"),
    (254, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (256, "objects[0].x"),
    (257, "objects[0].y"),
    (260, "objects[1].collideable"),
    (261, "objects[1].delay"),
    (348, "objects[1].flip.x"),
    (349, "objects[1].flip.y"),
    (263, "objects[1].hide_for"),
    (264, "objects[1].hide_in"),
    (350, "objects[1].hitbox.h"),
    (351, "objects[1].hitbox.w"),
    (352, "objects[1].hitbox.x"),
    (353, "objects[1].hitbox.y"),
    (354, "objects[1].rem.x"),
    (355, "objects[1].rem.y"),
    (272, "objects[1].solids"),
    (356, "objects[1].spd.x"),
    (357, "objects[1].spd.y"),
    (274, "objects[1].spr"),
    (276, "objects[1].x"),
    (277, "objects[1].y"),
    (280, "objects[2].collideable"),
    (358, "objects[2].dash_accel.x"),
    (359, "objects[2].dash_accel.y"),
    (282, "objects[2].dash_effect_time"),
    (360, "objects[2].dash_target.x"),
    (361, "objects[2].dash_target.y"),
    (284, "objects[2].dash_time"),
    (285, "objects[2].djump"),
    (362, "objects[2].flip.x"),
    (363, "objects[2].flip.y"),
    (287, "objects[2].grace"),
    (364, "objects[2].hitbox.h"),
    (365, "objects[2].hitbox.w"),
    (366, "objects[2].hitbox.x"),
    (367, "objects[2].hitbox.y"),
    (294, "objects[2].p_dash"),
    (295, "objects[2].p_jump"),
    (368, "objects[2].rem.x"),
    (369, "objects[2].rem.y"),
    (297, "objects[2].solids"),
    (370, "objects[2].spd.x"),
    (371, "objects[2].spd.y"),
    (301, "objects[2].x"),
    (302, "objects[2].y"),
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
    SCell::Arr(&[173, 174, 175]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Obj(&[(5, 182), (8, 183), (6, 184)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (5, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(5, 203), (6, 204)]),
    SCell::Obj(&[(7, 205), (5, 206), (8, 207)]),
    SCell::Obj(&[(7, 208), (5, 209)]),
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
    SCell::Obj(&[(21, 238), (20, 239), (11, 240), (35, 241), (14, 242), (45, 243), (42, 244), (15, 245), (19, 246), (18, 247), (22, 248), (23, 249), (24, 250), (4, 251), (12, 252), (3, 253), (13, 254), (0, 255), (1, 256), (2, 257)]),
    SCell::Obj(&[(21, 258), (20, 259), (11, 260), (35, 261), (14, 262), (45, 263), (42, 264), (15, 265), (19, 266), (18, 267), (22, 268), (23, 269), (24, 270), (4, 271), (12, 272), (3, 273), (13, 274), (0, 275), (1, 276), (2, 277)]),
    SCell::Obj(&[(21, 278), (20, 279), (11, 280), (28, 281), (33, 282), (27, 283), (26, 284), (30, 285), (14, 286), (29, 287), (15, 288), (19, 289), (18, 290), (22, 291), (23, 292), (24, 293), (32, 294), (31, 295), (4, 296), (12, 297), (3, 298), (13, 299), (0, 300), (1, 301), (2, 302)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 338), (2, 339)]),
    SCell::Obj(&[(17, 340), (16, 341), (1, 342), (2, 343)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 344), (2, 345)]),
    SCell::Obj(&[(1, 346), (2, 347)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 348), (2, 349)]),
    SCell::Obj(&[(17, 350), (16, 351), (1, 352), (2, 353)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 354), (2, 355)]),
    SCell::Obj(&[(1, 356), (2, 357)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(1, 360), (2, 361)]),
    SCell::Obj(&[(1, 362), (2, 363)]),
    SCell::Obj(&[(17, 364), (16, 365), (1, 366), (2, 367)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Obj(&[(1, 370), (2, 371)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 210),
    (152, 211),
    (153, 212),
    (155, 213),
    (156, 214),
    (157, 215),
    (158, 216),
    (160, 217),
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
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (182, 222),
    (184, 223),
    (186, 224),
    (188, 225),
    (190, 226),
    (192, 227),
    (195, 228),
    (198, 229),
    (200, 230),
    (202, 231),
    (203, 232),
    (204, 233),
    (205, 234),
    (206, 235),
    (208, 236),
    (209, 237),
    (238, 303),
    (239, 304),
    (242, 305),
    (245, 306),
    (246, 307),
    (247, 308),
    (248, 309),
    (249, 310),
    (250, 311),
    (251, 312),
    (253, 313),
    (255, 116),
    (258, 314),
    (259, 315),
    (262, 316),
    (265, 317),
    (266, 318),
    (267, 319),
    (268, 320),
    (269, 321),
    (270, 322),
    (271, 323),
    (273, 324),
    (275, 116),
    (278, 325),
    (279, 326),
    (281, 327),
    (283, 328),
    (286, 329),
    (288, 330),
    (289, 331),
    (290, 332),
    (291, 333),
    (292, 334),
    (293, 335),
    (296, 336),
    (298, 337),
    (300, 93),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c241: ZN,
    pub c254: ZN,
    pub c261: ZN,
    pub c274: ZN,
    pub c368: ZN,
    pub c369: ZN,
    pub c302: ZN,
    pub c85: ZN,
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
    pub c358: ZN,
    pub c359: ZN,
    pub c282: ZN,
    pub c360: ZN,
    pub c361: ZN,
    pub c284: ZN,
    pub c285: ZN,
    pub c362: ZB,
    pub c287: ZN,
    pub c294: ZB,
    pub c295: ZB,
    pub c370: ZN,
    pub c371: ZN,
    pub c301: ZN,
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
    (180, "balloon.tile"),
    (207, "big_chest.tile"),
    (199, "chest.if_not_fruit"),
    (201, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (193, "fake_wall.if_not_fruit"),
    (194, "fake_wall.tile"),
    (183, "fall_floor.tile"),
    (189, "fly_fruit.if_not_fruit"),
    (191, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (185, "fruit.if_not_fruit"),
    (187, "fruit.tile"),
    (175, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (196, "key.if_not_fruit"),
    (197, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (239, "objects[0].collideable"),
    (240, "objects[0].delay"),
    (299, "objects[0].flip.x"),
    (300, "objects[0].flip.y"),
    (242, "objects[0].hide_for"),
    (243, "objects[0].hide_in"),
    (301, "objects[0].hitbox.h"),
    (302, "objects[0].hitbox.w"),
    (303, "objects[0].hitbox.x"),
    (304, "objects[0].hitbox.y"),
    (305, "objects[0].rem.x"),
    (306, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (307, "objects[0].spd.x"),
    (308, "objects[0].spd.y"),
    (253, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (260, "objects[1].delay"),
    (309, "objects[1].flip.x"),
    (310, "objects[1].flip.y"),
    (262, "objects[1].hide_for"),
    (263, "objects[1].hide_in"),
    (311, "objects[1].hitbox.h"),
    (312, "objects[1].hitbox.w"),
    (313, "objects[1].hitbox.x"),
    (314, "objects[1].hitbox.y"),
    (315, "objects[1].rem.x"),
    (316, "objects[1].rem.y"),
    (271, "objects[1].solids"),
    (317, "objects[1].spd.x"),
    (318, "objects[1].spd.y"),
    (273, "objects[1].spr"),
    (275, "objects[1].x"),
    (276, "objects[1].y"),
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
    SCell::Arr(&[173, 174, 175]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Obj(&[(5, 182), (8, 183), (6, 184)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (5, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(5, 203), (6, 204)]),
    SCell::Obj(&[(7, 205), (5, 206), (8, 207)]),
    SCell::Obj(&[(7, 208), (5, 209)]),
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
    SCell::Obj(&[(21, 237), (20, 238), (11, 239), (35, 240), (14, 241), (45, 242), (42, 243), (15, 244), (19, 245), (18, 246), (22, 247), (23, 248), (24, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (35, 260), (14, 261), (45, 262), (42, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (0, 274), (1, 275), (2, 276)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 299), (2, 300)]),
    SCell::Obj(&[(17, 301), (16, 302), (1, 303), (2, 304)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 305), (2, 306)]),
    SCell::Obj(&[(1, 307), (2, 308)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 309), (2, 310)]),
    SCell::Obj(&[(17, 311), (16, 312), (1, 313), (2, 314)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 315), (2, 316)]),
    SCell::Obj(&[(1, 317), (2, 318)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 210),
    (152, 211),
    (155, 212),
    (156, 213),
    (157, 214),
    (158, 215),
    (160, 216),
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
    (176, 217),
    (178, 218),
    (179, 219),
    (181, 220),
    (182, 221),
    (184, 222),
    (186, 223),
    (188, 224),
    (190, 225),
    (192, 226),
    (195, 227),
    (198, 228),
    (200, 229),
    (202, 230),
    (203, 231),
    (204, 232),
    (205, 233),
    (206, 234),
    (208, 235),
    (209, 236),
    (237, 277),
    (238, 278),
    (241, 279),
    (244, 280),
    (245, 281),
    (246, 282),
    (247, 283),
    (248, 284),
    (249, 285),
    (250, 286),
    (252, 287),
    (254, 116),
    (257, 288),
    (258, 289),
    (261, 290),
    (264, 291),
    (265, 292),
    (266, 293),
    (267, 294),
    (268, 295),
    (269, 296),
    (270, 297),
    (272, 298),
    (274, 116),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c240: ZN,
    pub c253: ZN,
    pub c260: ZN,
    pub c273: ZN,
    pub c85: ZN,
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
    (327, "objects[0].flip.x"),
    (328, "objects[0].flip.y"),
    (329, "objects[0].hitbox.h"),
    (330, "objects[0].hitbox.w"),
    (331, "objects[0].hitbox.x"),
    (332, "objects[0].hitbox.y"),
    (245, "objects[0].off"),
    (333, "objects[0].rem.x"),
    (334, "objects[0].rem.y"),
    (247, "objects[0].solids"),
    (335, "objects[0].spd.x"),
    (336, "objects[0].spd.y"),
    (249, "objects[0].spr"),
    (250, "objects[0].start"),
    (252, "objects[0].x"),
    (253, "objects[0].y"),
    (256, "objects[1].collideable"),
    (257, "objects[1].delay"),
    (337, "objects[1].flip.x"),
    (338, "objects[1].flip.y"),
    (259, "objects[1].hide_for"),
    (260, "objects[1].hide_in"),
    (339, "objects[1].hitbox.h"),
    (340, "objects[1].hitbox.w"),
    (341, "objects[1].hitbox.x"),
    (342, "objects[1].hitbox.y"),
    (343, "objects[1].rem.x"),
    (344, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (345, "objects[1].spd.x"),
    (346, "objects[1].spd.y"),
    (270, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (276, "objects[2].collideable"),
    (277, "objects[2].delay"),
    (347, "objects[2].flip.x"),
    (348, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (349, "objects[2].hitbox.h"),
    (350, "objects[2].hitbox.w"),
    (351, "objects[2].hitbox.x"),
    (352, "objects[2].hitbox.y"),
    (353, "objects[2].rem.x"),
    (354, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (355, "objects[2].spd.x"),
    (356, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
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
    SCell::Obj(&[(21, 254), (20, 255), (11, 256), (35, 257), (14, 258), (45, 259), (42, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
    SCell::Obj(&[(21, 274), (20, 275), (11, 276), (35, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 327), (2, 328)]),
    SCell::Obj(&[(17, 329), (16, 330), (1, 331), (2, 332)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 333), (2, 334)]),
    SCell::Obj(&[(1, 335), (2, 336)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 337), (2, 338)]),
    SCell::Obj(&[(17, 339), (16, 340), (1, 341), (2, 342)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 343), (2, 344)]),
    SCell::Obj(&[(1, 345), (2, 346)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 347), (2, 348)]),
    SCell::Obj(&[(17, 349), (16, 350), (1, 351), (2, 352)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 353), (2, 354)]),
    SCell::Obj(&[(1, 355), (2, 356)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (235, 294),
    (236, 295),
    (238, 296),
    (239, 297),
    (240, 298),
    (241, 299),
    (242, 300),
    (243, 301),
    (244, 302),
    (246, 303),
    (248, 304),
    (251, 121),
    (254, 305),
    (255, 306),
    (258, 307),
    (261, 308),
    (262, 309),
    (263, 310),
    (264, 311),
    (265, 312),
    (266, 313),
    (267, 314),
    (269, 315),
    (271, 116),
    (274, 316),
    (275, 317),
    (278, 318),
    (281, 319),
    (282, 320),
    (283, 321),
    (284, 322),
    (285, 323),
    (286, 324),
    (287, 325),
    (289, 326),
    (291, 116),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c257: ZN,
    pub c270: ZN,
    pub c277: ZN,
    pub c290: ZN,
    pub c85: ZN,
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

// ---------------- outcome 3 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_3: &[(u32, &str)] = &[
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

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
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

// ---------------- outcome 4 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_4: &[(u32, &str)] = &[
    (190, "balloon.tile"),
    (217, "big_chest.tile"),
    (209, "chest.if_not_fruit"),
    (211, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (203, "fake_wall.if_not_fruit"),
    (204, "fake_wall.tile"),
    (193, "fall_floor.tile"),
    (199, "fly_fruit.if_not_fruit"),
    (201, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (195, "fruit.if_not_fruit"),
    (197, "fruit.tile"),
    (185, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (206, "key.if_not_fruit"),
    (207, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (261, "objects[0].collideable"),
    (262, "objects[0].delay"),
    (682, "objects[0].flip.x"),
    (683, "objects[0].flip.y"),
    (684, "objects[0].hitbox.h"),
    (685, "objects[0].hitbox.w"),
    (686, "objects[0].hitbox.x"),
    (687, "objects[0].hitbox.y"),
    (688, "objects[0].rem.x"),
    (689, "objects[0].rem.y"),
    (271, "objects[0].solids"),
    (690, "objects[0].spd.x"),
    (691, "objects[0].spd.y"),
    (273, "objects[0].spr"),
    (274, "objects[0].state"),
    (692, "objects[0].target.x"),
    (693, "objects[0].target.y"),
    (169, "objects[0].type.tile"),
    (277, "objects[0].x"),
    (278, "objects[0].y"),
    (281, "objects[1].collideable"),
    (694, "objects[1].flip.x"),
    (695, "objects[1].flip.y"),
    (283, "objects[1].fly"),
    (696, "objects[1].hitbox.h"),
    (697, "objects[1].hitbox.w"),
    (698, "objects[1].hitbox.x"),
    (699, "objects[1].hitbox.y"),
    (700, "objects[1].rem.x"),
    (701, "objects[1].rem.y"),
    (291, "objects[1].solids"),
    (702, "objects[1].spd.x"),
    (703, "objects[1].spd.y"),
    (293, "objects[1].spr"),
    (294, "objects[1].start"),
    (295, "objects[1].step"),
    (297, "objects[1].x"),
    (298, "objects[1].y"),
    (301, "objects[2].collideable"),
    (704, "objects[2].flip.x"),
    (705, "objects[2].flip.y"),
    (706, "objects[2].hitbox.h"),
    (707, "objects[2].hitbox.w"),
    (708, "objects[2].hitbox.x"),
    (709, "objects[2].hitbox.y"),
    (710, "objects[2].rem.x"),
    (711, "objects[2].rem.y"),
    (310, "objects[2].solid"),
    (311, "objects[2].solids"),
    (712, "objects[2].spd.x"),
    (713, "objects[2].spd.y"),
    (313, "objects[2].spr"),
    (314, "objects[2].state"),
    (316, "objects[2].x"),
    (317, "objects[2].y"),
    (320, "objects[3].collideable"),
    (714, "objects[3].flip.x"),
    (715, "objects[3].flip.y"),
    (716, "objects[3].hitbox.h"),
    (717, "objects[3].hitbox.w"),
    (718, "objects[3].hitbox.x"),
    (719, "objects[3].hitbox.y"),
    (720, "objects[3].rem.x"),
    (721, "objects[3].rem.y"),
    (329, "objects[3].solid"),
    (330, "objects[3].solids"),
    (722, "objects[3].spd.x"),
    (723, "objects[3].spd.y"),
    (332, "objects[3].spr"),
    (333, "objects[3].state"),
    (335, "objects[3].x"),
    (336, "objects[3].y"),
    (339, "objects[4].collideable"),
    (724, "objects[4].flip.x"),
    (725, "objects[4].flip.y"),
    (726, "objects[4].hitbox.h"),
    (727, "objects[4].hitbox.w"),
    (728, "objects[4].hitbox.x"),
    (729, "objects[4].hitbox.y"),
    (730, "objects[4].rem.x"),
    (731, "objects[4].rem.y"),
    (348, "objects[4].solid"),
    (349, "objects[4].solids"),
    (732, "objects[4].spd.x"),
    (733, "objects[4].spd.y"),
    (351, "objects[4].spr"),
    (352, "objects[4].state"),
    (354, "objects[4].x"),
    (355, "objects[4].y"),
    (358, "objects[5].collideable"),
    (734, "objects[5].flip.x"),
    (735, "objects[5].flip.y"),
    (736, "objects[5].hitbox.h"),
    (737, "objects[5].hitbox.w"),
    (738, "objects[5].hitbox.x"),
    (739, "objects[5].hitbox.y"),
    (740, "objects[5].rem.x"),
    (741, "objects[5].rem.y"),
    (367, "objects[5].solid"),
    (368, "objects[5].solids"),
    (742, "objects[5].spd.x"),
    (743, "objects[5].spd.y"),
    (370, "objects[5].spr"),
    (371, "objects[5].state"),
    (373, "objects[5].x"),
    (374, "objects[5].y"),
    (377, "objects[6].collideable"),
    (744, "objects[6].flip.x"),
    (745, "objects[6].flip.y"),
    (746, "objects[6].hitbox.h"),
    (747, "objects[6].hitbox.w"),
    (748, "objects[6].hitbox.x"),
    (749, "objects[6].hitbox.y"),
    (750, "objects[6].rem.x"),
    (751, "objects[6].rem.y"),
    (386, "objects[6].solid"),
    (387, "objects[6].solids"),
    (752, "objects[6].spd.x"),
    (753, "objects[6].spd.y"),
    (389, "objects[6].spr"),
    (390, "objects[6].state"),
    (392, "objects[6].x"),
    (393, "objects[6].y"),
    (396, "objects[7].collideable"),
    (754, "objects[7].flip.x"),
    (755, "objects[7].flip.y"),
    (756, "objects[7].hitbox.h"),
    (757, "objects[7].hitbox.w"),
    (758, "objects[7].hitbox.x"),
    (759, "objects[7].hitbox.y"),
    (760, "objects[7].rem.x"),
    (761, "objects[7].rem.y"),
    (405, "objects[7].solid"),
    (406, "objects[7].solids"),
    (762, "objects[7].spd.x"),
    (763, "objects[7].spd.y"),
    (408, "objects[7].spr"),
    (409, "objects[7].state"),
    (411, "objects[7].x"),
    (412, "objects[7].y"),
    (415, "objects[8].collideable"),
    (764, "objects[8].flip.x"),
    (765, "objects[8].flip.y"),
    (766, "objects[8].hitbox.h"),
    (767, "objects[8].hitbox.w"),
    (768, "objects[8].hitbox.x"),
    (769, "objects[8].hitbox.y"),
    (770, "objects[8].rem.x"),
    (771, "objects[8].rem.y"),
    (424, "objects[8].solid"),
    (425, "objects[8].solids"),
    (772, "objects[8].spd.x"),
    (773, "objects[8].spd.y"),
    (427, "objects[8].spr"),
    (428, "objects[8].state"),
    (430, "objects[8].x"),
    (431, "objects[8].y"),
    (434, "objects[9].collideable"),
    (774, "objects[9].flip.x"),
    (775, "objects[9].flip.y"),
    (776, "objects[9].hitbox.h"),
    (777, "objects[9].hitbox.w"),
    (778, "objects[9].hitbox.x"),
    (779, "objects[9].hitbox.y"),
    (780, "objects[9].rem.x"),
    (781, "objects[9].rem.y"),
    (443, "objects[9].solid"),
    (444, "objects[9].solids"),
    (782, "objects[9].spd.x"),
    (783, "objects[9].spd.y"),
    (446, "objects[9].spr"),
    (447, "objects[9].state"),
    (449, "objects[9].x"),
    (450, "objects[9].y"),
    (453, "objects[10].collideable"),
    (784, "objects[10].flip.x"),
    (785, "objects[10].flip.y"),
    (786, "objects[10].hitbox.h"),
    (787, "objects[10].hitbox.w"),
    (788, "objects[10].hitbox.x"),
    (789, "objects[10].hitbox.y"),
    (790, "objects[10].rem.x"),
    (791, "objects[10].rem.y"),
    (462, "objects[10].solid"),
    (463, "objects[10].solids"),
    (792, "objects[10].spd.x"),
    (793, "objects[10].spd.y"),
    (465, "objects[10].spr"),
    (466, "objects[10].state"),
    (468, "objects[10].x"),
    (469, "objects[10].y"),
    (472, "objects[11].collideable"),
    (794, "objects[11].flip.x"),
    (795, "objects[11].flip.y"),
    (796, "objects[11].hitbox.h"),
    (797, "objects[11].hitbox.w"),
    (798, "objects[11].hitbox.x"),
    (799, "objects[11].hitbox.y"),
    (800, "objects[11].rem.x"),
    (801, "objects[11].rem.y"),
    (481, "objects[11].solid"),
    (482, "objects[11].solids"),
    (802, "objects[11].spd.x"),
    (803, "objects[11].spd.y"),
    (484, "objects[11].spr"),
    (485, "objects[11].state"),
    (487, "objects[11].x"),
    (488, "objects[11].y"),
    (491, "objects[12].collideable"),
    (804, "objects[12].flip.x"),
    (805, "objects[12].flip.y"),
    (806, "objects[12].hitbox.h"),
    (807, "objects[12].hitbox.w"),
    (808, "objects[12].hitbox.x"),
    (809, "objects[12].hitbox.y"),
    (810, "objects[12].rem.x"),
    (811, "objects[12].rem.y"),
    (500, "objects[12].solid"),
    (501, "objects[12].solids"),
    (812, "objects[12].spd.x"),
    (813, "objects[12].spd.y"),
    (503, "objects[12].spr"),
    (504, "objects[12].state"),
    (506, "objects[12].x"),
    (507, "objects[12].y"),
    (510, "objects[13].collideable"),
    (814, "objects[13].flip.x"),
    (815, "objects[13].flip.y"),
    (816, "objects[13].hitbox.h"),
    (817, "objects[13].hitbox.w"),
    (818, "objects[13].hitbox.x"),
    (819, "objects[13].hitbox.y"),
    (820, "objects[13].rem.x"),
    (821, "objects[13].rem.y"),
    (519, "objects[13].solid"),
    (520, "objects[13].solids"),
    (822, "objects[13].spd.x"),
    (823, "objects[13].spd.y"),
    (522, "objects[13].spr"),
    (523, "objects[13].state"),
    (525, "objects[13].x"),
    (526, "objects[13].y"),
    (43, "pause_player"),
    (171, "room.x"),
    (172, "room.y"),
    (85, "seconds"),
    (187, "spring.tile"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_4: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_4: &[SCell] = &[
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
    SCell::Arr(&[183, 184, 185]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 186), (8, 187), (6, 188)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(5, 192), (8, 193), (6, 194)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(9, 203), (8, 204), (6, 205)]),
    SCell::Obj(&[(9, 206), (8, 207), (6, 208)]),
    SCell::Obj(&[(9, 209), (5, 210), (8, 211), (6, 212)]),
    SCell::Obj(&[(5, 213), (6, 214)]),
    SCell::Obj(&[(7, 215), (5, 216), (8, 217)]),
    SCell::Obj(&[(7, 218), (5, 219)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 259), (20, 260), (11, 261), (35, 262), (14, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (25, 274), (34, 275), (0, 276), (1, 277), (2, 278)]),
    SCell::Obj(&[(21, 279), (20, 280), (11, 281), (14, 282), (39, 283), (15, 284), (19, 285), (18, 286), (22, 287), (23, 288), (24, 289), (4, 290), (12, 291), (3, 292), (13, 293), (38, 294), (40, 295), (0, 296), (1, 297), (2, 298)]),
    SCell::Obj(&[(21, 299), (20, 300), (11, 301), (14, 302), (15, 303), (19, 304), (18, 305), (22, 306), (23, 307), (24, 308), (4, 309), (43, 310), (12, 311), (3, 312), (13, 313), (25, 314), (0, 315), (1, 316), (2, 317)]),
    SCell::Obj(&[(21, 318), (20, 319), (11, 320), (14, 321), (15, 322), (19, 323), (18, 324), (22, 325), (23, 326), (24, 327), (4, 328), (43, 329), (12, 330), (3, 331), (13, 332), (25, 333), (0, 334), (1, 335), (2, 336)]),
    SCell::Obj(&[(21, 337), (20, 338), (11, 339), (14, 340), (15, 341), (19, 342), (18, 343), (22, 344), (23, 345), (24, 346), (4, 347), (43, 348), (12, 349), (3, 350), (13, 351), (25, 352), (0, 353), (1, 354), (2, 355)]),
    SCell::Obj(&[(21, 356), (20, 357), (11, 358), (14, 359), (15, 360), (19, 361), (18, 362), (22, 363), (23, 364), (24, 365), (4, 366), (43, 367), (12, 368), (3, 369), (13, 370), (25, 371), (0, 372), (1, 373), (2, 374)]),
    SCell::Obj(&[(21, 375), (20, 376), (11, 377), (14, 378), (15, 379), (19, 380), (18, 381), (22, 382), (23, 383), (24, 384), (4, 385), (43, 386), (12, 387), (3, 388), (13, 389), (25, 390), (0, 391), (1, 392), (2, 393)]),
    SCell::Obj(&[(21, 394), (20, 395), (11, 396), (14, 397), (15, 398), (19, 399), (18, 400), (22, 401), (23, 402), (24, 403), (4, 404), (43, 405), (12, 406), (3, 407), (13, 408), (25, 409), (0, 410), (1, 411), (2, 412)]),
    SCell::Obj(&[(21, 413), (20, 414), (11, 415), (14, 416), (15, 417), (19, 418), (18, 419), (22, 420), (23, 421), (24, 422), (4, 423), (43, 424), (12, 425), (3, 426), (13, 427), (25, 428), (0, 429), (1, 430), (2, 431)]),
    SCell::Obj(&[(21, 432), (20, 433), (11, 434), (14, 435), (15, 436), (19, 437), (18, 438), (22, 439), (23, 440), (24, 441), (4, 442), (43, 443), (12, 444), (3, 445), (13, 446), (25, 447), (0, 448), (1, 449), (2, 450)]),
    SCell::Obj(&[(21, 451), (20, 452), (11, 453), (14, 454), (15, 455), (19, 456), (18, 457), (22, 458), (23, 459), (24, 460), (4, 461), (43, 462), (12, 463), (3, 464), (13, 465), (25, 466), (0, 467), (1, 468), (2, 469)]),
    SCell::Obj(&[(21, 470), (20, 471), (11, 472), (14, 473), (15, 474), (19, 475), (18, 476), (22, 477), (23, 478), (24, 479), (4, 480), (43, 481), (12, 482), (3, 483), (13, 484), (25, 485), (0, 486), (1, 487), (2, 488)]),
    SCell::Obj(&[(21, 489), (20, 490), (11, 491), (14, 492), (15, 493), (19, 494), (18, 495), (22, 496), (23, 497), (24, 498), (4, 499), (43, 500), (12, 501), (3, 502), (13, 503), (25, 504), (0, 505), (1, 506), (2, 507)]),
    SCell::Obj(&[(21, 508), (20, 509), (11, 510), (14, 511), (15, 512), (19, 513), (18, 514), (22, 515), (23, 516), (24, 517), (4, 518), (43, 519), (12, 520), (3, 521), (13, 522), (25, 523), (0, 524), (1, 525), (2, 526)]),
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
    SCell::Clo(26, &[220]),
    SCell::Clo(25, &[220]),
    SCell::Obj(&[(1, 682), (2, 683)]),
    SCell::Obj(&[(17, 684), (16, 685), (1, 686), (2, 687)]),
    SCell::Clo(24, &[220]),
    SCell::Clo(23, &[220]),
    SCell::Clo(27, &[220]),
    SCell::Clo(28, &[220]),
    SCell::Clo(29, &[220]),
    SCell::Obj(&[(1, 688), (2, 689)]),
    SCell::Obj(&[(1, 690), (2, 691)]),
    SCell::Obj(&[(1, 692), (2, 693)]),
    SCell::Clo(26, &[221]),
    SCell::Clo(25, &[221]),
    SCell::Obj(&[(1, 694), (2, 695)]),
    SCell::Obj(&[(17, 696), (16, 697), (1, 698), (2, 699)]),
    SCell::Clo(24, &[221]),
    SCell::Clo(23, &[221]),
    SCell::Clo(27, &[221]),
    SCell::Clo(28, &[221]),
    SCell::Clo(29, &[221]),
    SCell::Obj(&[(1, 700), (2, 701)]),
    SCell::Obj(&[(1, 702), (2, 703)]),
    SCell::Clo(26, &[222]),
    SCell::Clo(25, &[222]),
    SCell::Obj(&[(1, 704), (2, 705)]),
    SCell::Obj(&[(17, 706), (16, 707), (1, 708), (2, 709)]),
    SCell::Clo(24, &[222]),
    SCell::Clo(23, &[222]),
    SCell::Clo(27, &[222]),
    SCell::Clo(28, &[222]),
    SCell::Clo(29, &[222]),
    SCell::Obj(&[(1, 710), (2, 711)]),
    SCell::Obj(&[(1, 712), (2, 713)]),
    SCell::Clo(26, &[223]),
    SCell::Clo(25, &[223]),
    SCell::Obj(&[(1, 714), (2, 715)]),
    SCell::Obj(&[(17, 716), (16, 717), (1, 718), (2, 719)]),
    SCell::Clo(24, &[223]),
    SCell::Clo(23, &[223]),
    SCell::Clo(27, &[223]),
    SCell::Clo(28, &[223]),
    SCell::Clo(29, &[223]),
    SCell::Obj(&[(1, 720), (2, 721)]),
    SCell::Obj(&[(1, 722), (2, 723)]),
    SCell::Clo(26, &[224]),
    SCell::Clo(25, &[224]),
    SCell::Obj(&[(1, 724), (2, 725)]),
    SCell::Obj(&[(17, 726), (16, 727), (1, 728), (2, 729)]),
    SCell::Clo(24, &[224]),
    SCell::Clo(23, &[224]),
    SCell::Clo(27, &[224]),
    SCell::Clo(28, &[224]),
    SCell::Clo(29, &[224]),
    SCell::Obj(&[(1, 730), (2, 731)]),
    SCell::Obj(&[(1, 732), (2, 733)]),
    SCell::Clo(26, &[225]),
    SCell::Clo(25, &[225]),
    SCell::Obj(&[(1, 734), (2, 735)]),
    SCell::Obj(&[(17, 736), (16, 737), (1, 738), (2, 739)]),
    SCell::Clo(24, &[225]),
    SCell::Clo(23, &[225]),
    SCell::Clo(27, &[225]),
    SCell::Clo(28, &[225]),
    SCell::Clo(29, &[225]),
    SCell::Obj(&[(1, 740), (2, 741)]),
    SCell::Obj(&[(1, 742), (2, 743)]),
    SCell::Clo(26, &[226]),
    SCell::Clo(25, &[226]),
    SCell::Obj(&[(1, 744), (2, 745)]),
    SCell::Obj(&[(17, 746), (16, 747), (1, 748), (2, 749)]),
    SCell::Clo(24, &[226]),
    SCell::Clo(23, &[226]),
    SCell::Clo(27, &[226]),
    SCell::Clo(28, &[226]),
    SCell::Clo(29, &[226]),
    SCell::Obj(&[(1, 750), (2, 751)]),
    SCell::Obj(&[(1, 752), (2, 753)]),
    SCell::Clo(26, &[227]),
    SCell::Clo(25, &[227]),
    SCell::Obj(&[(1, 754), (2, 755)]),
    SCell::Obj(&[(17, 756), (16, 757), (1, 758), (2, 759)]),
    SCell::Clo(24, &[227]),
    SCell::Clo(23, &[227]),
    SCell::Clo(27, &[227]),
    SCell::Clo(28, &[227]),
    SCell::Clo(29, &[227]),
    SCell::Obj(&[(1, 760), (2, 761)]),
    SCell::Obj(&[(1, 762), (2, 763)]),
    SCell::Clo(26, &[228]),
    SCell::Clo(25, &[228]),
    SCell::Obj(&[(1, 764), (2, 765)]),
    SCell::Obj(&[(17, 766), (16, 767), (1, 768), (2, 769)]),
    SCell::Clo(24, &[228]),
    SCell::Clo(23, &[228]),
    SCell::Clo(27, &[228]),
    SCell::Clo(28, &[228]),
    SCell::Clo(29, &[228]),
    SCell::Obj(&[(1, 770), (2, 771)]),
    SCell::Obj(&[(1, 772), (2, 773)]),
    SCell::Clo(26, &[229]),
    SCell::Clo(25, &[229]),
    SCell::Obj(&[(1, 774), (2, 775)]),
    SCell::Obj(&[(17, 776), (16, 777), (1, 778), (2, 779)]),
    SCell::Clo(24, &[229]),
    SCell::Clo(23, &[229]),
    SCell::Clo(27, &[229]),
    SCell::Clo(28, &[229]),
    SCell::Clo(29, &[229]),
    SCell::Obj(&[(1, 780), (2, 781)]),
    SCell::Obj(&[(1, 782), (2, 783)]),
    SCell::Clo(26, &[230]),
    SCell::Clo(25, &[230]),
    SCell::Obj(&[(1, 784), (2, 785)]),
    SCell::Obj(&[(17, 786), (16, 787), (1, 788), (2, 789)]),
    SCell::Clo(24, &[230]),
    SCell::Clo(23, &[230]),
    SCell::Clo(27, &[230]),
    SCell::Clo(28, &[230]),
    SCell::Clo(29, &[230]),
    SCell::Obj(&[(1, 790), (2, 791)]),
    SCell::Obj(&[(1, 792), (2, 793)]),
    SCell::Clo(26, &[231]),
    SCell::Clo(25, &[231]),
    SCell::Obj(&[(1, 794), (2, 795)]),
    SCell::Obj(&[(17, 796), (16, 797), (1, 798), (2, 799)]),
    SCell::Clo(24, &[231]),
    SCell::Clo(23, &[231]),
    SCell::Clo(27, &[231]),
    SCell::Clo(28, &[231]),
    SCell::Clo(29, &[231]),
    SCell::Obj(&[(1, 800), (2, 801)]),
    SCell::Obj(&[(1, 802), (2, 803)]),
    SCell::Clo(26, &[232]),
    SCell::Clo(25, &[232]),
    SCell::Obj(&[(1, 804), (2, 805)]),
    SCell::Obj(&[(17, 806), (16, 807), (1, 808), (2, 809)]),
    SCell::Clo(24, &[232]),
    SCell::Clo(23, &[232]),
    SCell::Clo(27, &[232]),
    SCell::Clo(28, &[232]),
    SCell::Clo(29, &[232]),
    SCell::Obj(&[(1, 810), (2, 811)]),
    SCell::Obj(&[(1, 812), (2, 813)]),
    SCell::Clo(26, &[233]),
    SCell::Clo(25, &[233]),
    SCell::Obj(&[(1, 814), (2, 815)]),
    SCell::Obj(&[(17, 816), (16, 817), (1, 818), (2, 819)]),
    SCell::Clo(24, &[233]),
    SCell::Clo(23, &[233]),
    SCell::Clo(27, &[233]),
    SCell::Clo(28, &[233]),
    SCell::Clo(29, &[233]),
    SCell::Obj(&[(1, 820), (2, 821)]),
    SCell::Obj(&[(1, 822), (2, 823)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_4: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_4: &[(u32, u32)] = &[
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
    (151, 220),
    (152, 221),
    (153, 222),
    (154, 223),
    (155, 224),
    (156, 225),
    (157, 226),
    (158, 227),
    (159, 228),
    (160, 229),
    (161, 230),
    (162, 231),
    (163, 232),
    (164, 233),
    (165, 234),
    (166, 235),
    (167, 236),
    (168, 237),
    (170, 238),
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
    (186, 239),
    (188, 240),
    (189, 241),
    (191, 242),
    (192, 243),
    (194, 244),
    (196, 245),
    (198, 246),
    (200, 247),
    (202, 248),
    (205, 249),
    (208, 250),
    (210, 251),
    (212, 252),
    (213, 253),
    (214, 254),
    (215, 255),
    (216, 256),
    (218, 257),
    (219, 258),
    (259, 527),
    (260, 528),
    (263, 529),
    (264, 530),
    (265, 531),
    (266, 532),
    (267, 533),
    (268, 534),
    (269, 535),
    (270, 536),
    (272, 537),
    (275, 538),
    (276, 94),
    (279, 539),
    (280, 540),
    (282, 541),
    (284, 542),
    (285, 543),
    (286, 544),
    (287, 545),
    (288, 546),
    (289, 547),
    (290, 548),
    (292, 549),
    (296, 122),
    (299, 550),
    (300, 551),
    (302, 552),
    (303, 553),
    (304, 554),
    (305, 555),
    (306, 556),
    (307, 557),
    (308, 558),
    (309, 559),
    (312, 560),
    (315, 119),
    (318, 561),
    (319, 562),
    (321, 563),
    (322, 564),
    (323, 565),
    (324, 566),
    (325, 567),
    (326, 568),
    (327, 569),
    (328, 570),
    (331, 571),
    (334, 119),
    (337, 572),
    (338, 573),
    (340, 574),
    (341, 575),
    (342, 576),
    (343, 577),
    (344, 578),
    (345, 579),
    (346, 580),
    (347, 581),
    (350, 582),
    (353, 119),
    (356, 583),
    (357, 584),
    (359, 585),
    (360, 586),
    (361, 587),
    (362, 588),
    (363, 589),
    (364, 590),
    (365, 591),
    (366, 592),
    (369, 593),
    (372, 119),
    (375, 594),
    (376, 595),
    (378, 596),
    (379, 597),
    (380, 598),
    (381, 599),
    (382, 600),
    (383, 601),
    (384, 602),
    (385, 603),
    (388, 604),
    (391, 119),
    (394, 605),
    (395, 606),
    (397, 607),
    (398, 608),
    (399, 609),
    (400, 610),
    (401, 611),
    (402, 612),
    (403, 613),
    (404, 614),
    (407, 615),
    (410, 119),
    (413, 616),
    (414, 617),
    (416, 618),
    (417, 619),
    (418, 620),
    (419, 621),
    (420, 622),
    (421, 623),
    (422, 624),
    (423, 625),
    (426, 626),
    (429, 119),
    (432, 627),
    (433, 628),
    (435, 629),
    (436, 630),
    (437, 631),
    (438, 632),
    (439, 633),
    (440, 634),
    (441, 635),
    (442, 636),
    (445, 637),
    (448, 119),
    (451, 638),
    (452, 639),
    (454, 640),
    (455, 641),
    (456, 642),
    (457, 643),
    (458, 644),
    (459, 645),
    (460, 646),
    (461, 647),
    (464, 648),
    (467, 119),
    (470, 649),
    (471, 650),
    (473, 651),
    (474, 652),
    (475, 653),
    (476, 654),
    (477, 655),
    (478, 656),
    (479, 657),
    (480, 658),
    (483, 659),
    (486, 119),
    (489, 660),
    (490, 661),
    (492, 662),
    (493, 663),
    (494, 664),
    (495, 665),
    (496, 666),
    (497, 667),
    (498, 668),
    (499, 669),
    (502, 670),
    (505, 119),
    (508, 671),
    (509, 672),
    (511, 673),
    (512, 674),
    (513, 675),
    (514, 676),
    (515, 677),
    (516, 678),
    (517, 679),
    (518, 680),
    (521, 681),
    (524, 119),
];

/// Outcome 4's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared4 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
}

/// Outcome 4's per-assignment values and lane masks.
/// The cells of outcome 4 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared4`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut4 {
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

// ---------------- outcome 5 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_5: &[(u32, &str)] = &[
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
    (366, "objects[0].flip.x"),
    (367, "objects[0].flip.y"),
    (368, "objects[0].hitbox.h"),
    (369, "objects[0].hitbox.w"),
    (370, "objects[0].hitbox.x"),
    (371, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (372, "objects[0].rem.x"),
    (373, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (374, "objects[0].spd.x"),
    (375, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (376, "objects[1].flip.x"),
    (377, "objects[1].flip.y"),
    (260, "objects[1].hide_for"),
    (261, "objects[1].hide_in"),
    (378, "objects[1].hitbox.h"),
    (379, "objects[1].hitbox.w"),
    (380, "objects[1].hitbox.x"),
    (381, "objects[1].hitbox.y"),
    (382, "objects[1].rem.x"),
    (383, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (384, "objects[1].spd.x"),
    (385, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (278, "objects[2].delay"),
    (386, "objects[2].flip.x"),
    (387, "objects[2].flip.y"),
    (280, "objects[2].hide_for"),
    (281, "objects[2].hide_in"),
    (388, "objects[2].hitbox.h"),
    (389, "objects[2].hitbox.w"),
    (390, "objects[2].hitbox.x"),
    (391, "objects[2].hitbox.y"),
    (392, "objects[2].rem.x"),
    (393, "objects[2].rem.y"),
    (289, "objects[2].solids"),
    (394, "objects[2].spd.x"),
    (395, "objects[2].spd.y"),
    (291, "objects[2].spr"),
    (293, "objects[2].x"),
    (294, "objects[2].y"),
    (297, "objects[3].collideable"),
    (396, "objects[3].dash_accel.x"),
    (397, "objects[3].dash_accel.y"),
    (299, "objects[3].dash_effect_time"),
    (398, "objects[3].dash_target.x"),
    (399, "objects[3].dash_target.y"),
    (301, "objects[3].dash_time"),
    (302, "objects[3].djump"),
    (400, "objects[3].flip.x"),
    (401, "objects[3].flip.y"),
    (304, "objects[3].grace"),
    (402, "objects[3].hitbox.h"),
    (403, "objects[3].hitbox.w"),
    (404, "objects[3].hitbox.x"),
    (405, "objects[3].hitbox.y"),
    (311, "objects[3].p_dash"),
    (312, "objects[3].p_jump"),
    (406, "objects[3].rem.x"),
    (407, "objects[3].rem.y"),
    (314, "objects[3].solids"),
    (408, "objects[3].spd.x"),
    (409, "objects[3].spd.y"),
    (318, "objects[3].x"),
    (319, "objects[3].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_5: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_5: &[SCell] = &[
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
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (45, 260), (42, 261), (15, 262), (19, 263), (18, 264), (22, 265), (23, 266), (24, 267), (4, 268), (12, 269), (3, 270), (13, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (35, 278), (14, 279), (45, 280), (42, 281), (15, 282), (19, 283), (18, 284), (22, 285), (23, 286), (24, 287), (4, 288), (12, 289), (3, 290), (13, 291), (0, 292), (1, 293), (2, 294)]),
    SCell::Obj(&[(21, 295), (20, 296), (11, 297), (28, 298), (33, 299), (27, 300), (26, 301), (30, 302), (14, 303), (29, 304), (15, 305), (19, 306), (18, 307), (22, 308), (23, 309), (24, 310), (32, 311), (31, 312), (4, 313), (12, 314), (3, 315), (13, 316), (0, 317), (1, 318), (2, 319)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Obj(&[(17, 368), (16, 369), (1, 370), (2, 371)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 372), (2, 373)]),
    SCell::Obj(&[(1, 374), (2, 375)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 376), (2, 377)]),
    SCell::Obj(&[(17, 378), (16, 379), (1, 380), (2, 381)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 382), (2, 383)]),
    SCell::Obj(&[(1, 384), (2, 385)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 386), (2, 387)]),
    SCell::Obj(&[(17, 388), (16, 389), (1, 390), (2, 391)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 392), (2, 393)]),
    SCell::Obj(&[(1, 394), (2, 395)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 396), (2, 397)]),
    SCell::Obj(&[(1, 398), (2, 399)]),
    SCell::Obj(&[(1, 400), (2, 401)]),
    SCell::Obj(&[(17, 402), (16, 403), (1, 404), (2, 405)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 406), (2, 407)]),
    SCell::Obj(&[(1, 408), (2, 409)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_5: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_5: &[(u32, u32)] = &[
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
    (236, 320),
    (237, 321),
    (239, 322),
    (240, 323),
    (241, 324),
    (242, 325),
    (243, 326),
    (244, 327),
    (245, 328),
    (247, 329),
    (249, 330),
    (252, 121),
    (255, 331),
    (256, 332),
    (259, 333),
    (262, 334),
    (263, 335),
    (264, 336),
    (265, 337),
    (266, 338),
    (267, 339),
    (268, 340),
    (270, 341),
    (272, 116),
    (275, 342),
    (276, 343),
    (279, 344),
    (282, 345),
    (283, 346),
    (284, 347),
    (285, 348),
    (286, 349),
    (287, 350),
    (288, 351),
    (290, 352),
    (292, 116),
    (295, 353),
    (296, 354),
    (298, 355),
    (300, 356),
    (303, 357),
    (305, 358),
    (306, 359),
    (307, 360),
    (308, 361),
    (309, 362),
    (310, 363),
    (313, 364),
    (315, 365),
    (317, 93),
];

/// Outcome 5's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared5 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c246: ZN,
    pub c254: ZN,
    pub c258: ZN,
    pub c271: ZN,
    pub c278: ZN,
    pub c291: ZN,
    pub c406: ZN,
    pub c407: ZN,
    pub c319: ZN,
    pub c85: ZN,
}

/// Outcome 5's per-assignment values and lane masks.
/// The cells of outcome 5 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared5`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut5 {
    pub c20: ZN,
    pub c41: ZB,
    pub c396: ZN,
    pub c397: ZN,
    pub c299: ZN,
    pub c398: ZN,
    pub c399: ZN,
    pub c301: ZN,
    pub c302: ZN,
    pub c400: ZB,
    pub c304: ZN,
    pub c311: ZB,
    pub c312: ZB,
    pub c408: ZN,
    pub c409: ZN,
    pub c318: ZN,
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
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[175] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[240] = Col::U(AV::Bool(true));
    b.cols[241] = Col::N(Vec::new());
    b.cols[338] = Col::U(AV::Bool(false));
    b.cols[339] = Col::U(AV::Bool(false));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[252] = Col::U(AV::Bool(true));
    b.cols[346] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[347] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[257] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[260] = Col::U(AV::Bool(true));
    b.cols[261] = Col::N(Vec::new());
    b.cols[348] = Col::U(AV::Bool(false));
    b.cols[349] = Col::U(AV::Bool(false));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Bool(true));
    b.cols[356] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[357] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[274] = Col::N(Vec::new());
    b.cols[276] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[280] = Col::U(AV::Bool(true));
    b.cols[358] = Col::N(Vec::new());
    b.cols[359] = Col::N(Vec::new());
    b.cols[282] = Col::N(Vec::new());
    b.cols[360] = Col::N(Vec::new());
    b.cols[361] = Col::N(Vec::new());
    b.cols[284] = Col::N(Vec::new());
    b.cols[285] = Col::N(Vec::new());
    b.cols[362] = Col::V(Vec::new());
    b.cols[363] = Col::U(AV::Bool(false));
    b.cols[287] = Col::N(Vec::new());
    b.cols[364] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[294] = Col::V(Vec::new());
    b.cols[295] = Col::V(Vec::new());
    b.cols[368] = Col::N(Vec::new());
    b.cols[369] = Col::N(Vec::new());
    b.cols[297] = Col::U(AV::Bool(true));
    b.cols[370] = Col::N(Vec::new());
    b.cols[371] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[302] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
pub const KPART1_0: u64 = 10388043936962107023;
pub const KPART2_0: u64 = 15934586863823318640;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[241] { v.push(sh.c241.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::N(v) = &mut acc.cols[261] { v.push(sh.c261.lane(i)); }
        if let Col::N(v) = &mut acc.cols[274] { v.push(sh.c274.lane(i)); }
        if let Col::N(v) = &mut acc.cols[358] { v.push(kv.c358.lane(i)); }
        if let Col::N(v) = &mut acc.cols[359] { v.push(kv.c359.lane(i)); }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[360] { v.push(kv.c360.lane(i)); }
        if let Col::N(v) = &mut acc.cols[361] { v.push(kv.c361.lane(i)); }
        if let Col::N(v) = &mut acc.cols[284] { v.push(kv.c284.lane(i)); }
        if let Col::N(v) = &mut acc.cols[285] { v.push(kv.c285.lane(i)); }
        if let Col::V(v) = &mut acc.cols[362] {
            v.push(if kv.c362.known & (1 << i) != 0 {
                AV::Bool(kv.c362.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[287] { v.push(kv.c287.lane(i)); }
        if let Col::V(v) = &mut acc.cols[294] {
            v.push(if kv.c294.known & (1 << i) != 0 {
                AV::Bool(kv.c294.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[295] {
            v.push(if kv.c295.known & (1 << i) != 0 {
                AV::Bool(kv.c295.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[368] { v.push(sh.c368.lane(i)); }
        if let Col::N(v) = &mut acc.cols[369] { v.push(sh.c369.lane(i)); }
        if let Col::N(v) = &mut acc.cols[370] { v.push(kv.c370.lane(i)); }
        if let Col::N(v) = &mut acc.cols[371] { v.push(kv.c371.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::N(v) = &mut acc.cols[302] { v.push(sh.c302.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 1's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append1`.
pub fn acc1(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_1, OUT_GLOBALS_1, OUT_PTRS_1, 0, cart, cache);
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[175] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[239] = Col::U(AV::Bool(true));
    b.cols[240] = Col::N(Vec::new());
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[300] = Col::U(AV::Bool(false));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[308] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[253] = Col::N(Vec::new());
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[260] = Col::N(Vec::new());
    b.cols[309] = Col::U(AV::Bool(false));
    b.cols[310] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(true));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[318] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::N(Vec::new());
    b.cols[275] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
pub const KPART1_1: u64 = 5922312683678211919;
pub const KPART2_1: u64 = 1099119964388075113;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[240] { v.push(sh.c240.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[260] { v.push(sh.c260.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
    b.cols[237] = Col::U(AV::Bool(true));
    b.cols[327] = Col::U(AV::Bool(false));
    b.cols[328] = Col::U(AV::Bool(false));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[331] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::N(Vec::new());
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[247] = Col::U(AV::Bool(true));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[336] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::N(Vec::new());
    b.cols[256] = Col::U(AV::Bool(true));
    b.cols[257] = Col::N(Vec::new());
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[338] = Col::U(AV::Bool(false));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[346] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::N(Vec::new());
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[276] = Col::U(AV::Bool(true));
    b.cols[277] = Col::N(Vec::new());
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[348] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[356] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::N(Vec::new());
    b.cols[292] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
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
pub const KPART1_2: u64 = 2188804919208195361;
pub const KPART2_2: u64 = 18438466697642690530;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[245] { v.push(sh.c245.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[257] { v.push(sh.c257.lane(i)); }
        if let Col::N(v) = &mut acc.cols[270] { v.push(sh.c270.lane(i)); }
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[290] { v.push(sh.c290.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[214] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[206] = Col::U(AV::Bool(true));
    b.cols[208] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[200] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[190] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
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
    b.cols[85] = Col::N(Vec::new());
    b.cols[184] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
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
pub const KPART1_3: u64 = 7240363014696139081;
pub const KPART2_3: u64 = 97385359111614732;

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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 4's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append4`.
pub fn acc4(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_4, OUT_GLOBALS_4, OUT_PTRS_4, 0, cart, cache);
    b.cols[190] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[217] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[209] = Col::U(AV::Bool(true));
    b.cols[211] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[203] = Col::U(AV::Bool(true));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[206] = Col::U(AV::Bool(true));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[261] = Col::U(AV::Bool(true));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[682] = Col::U(AV::Bool(false));
    b.cols[683] = Col::U(AV::Bool(false));
    b.cols[684] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[685] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[686] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[687] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[688] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[689] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(false));
    b.cols[690] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[691] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[692] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[693] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[169] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(8388608i32)));
    b.cols[281] = Col::U(AV::Bool(true));
    b.cols[694] = Col::U(AV::Bool(false));
    b.cols[695] = Col::U(AV::Bool(false));
    b.cols[283] = Col::U(AV::Bool(false));
    b.cols[696] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[697] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[698] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[699] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[700] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[701] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[291] = Col::U(AV::Bool(false));
    b.cols[702] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[703] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[294] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[295] = Col::U(AV::Num(P8::from_raw(32768i32)));
    b.cols[297] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[301] = Col::U(AV::Bool(true));
    b.cols[704] = Col::U(AV::Bool(false));
    b.cols[705] = Col::U(AV::Bool(false));
    b.cols[706] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[707] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[708] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[709] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[710] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[711] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[310] = Col::U(AV::Bool(true));
    b.cols[311] = Col::U(AV::Bool(true));
    b.cols[712] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[713] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[320] = Col::U(AV::Bool(true));
    b.cols[714] = Col::U(AV::Bool(false));
    b.cols[715] = Col::U(AV::Bool(false));
    b.cols[716] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[717] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[718] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[719] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[720] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[721] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[329] = Col::U(AV::Bool(true));
    b.cols[330] = Col::U(AV::Bool(true));
    b.cols[722] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[723] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[336] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[339] = Col::U(AV::Bool(true));
    b.cols[724] = Col::U(AV::Bool(false));
    b.cols[725] = Col::U(AV::Bool(false));
    b.cols[726] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[727] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[728] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[729] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[730] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[731] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Bool(true));
    b.cols[349] = Col::U(AV::Bool(true));
    b.cols[732] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[733] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[358] = Col::U(AV::Bool(true));
    b.cols[734] = Col::U(AV::Bool(false));
    b.cols[735] = Col::U(AV::Bool(false));
    b.cols[736] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[737] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[738] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[739] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[740] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[741] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Bool(true));
    b.cols[368] = Col::U(AV::Bool(true));
    b.cols[742] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[743] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[377] = Col::U(AV::Bool(true));
    b.cols[744] = Col::U(AV::Bool(false));
    b.cols[745] = Col::U(AV::Bool(false));
    b.cols[746] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[747] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[748] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[749] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[750] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[751] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Bool(true));
    b.cols[387] = Col::U(AV::Bool(true));
    b.cols[752] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[753] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[396] = Col::U(AV::Bool(true));
    b.cols[754] = Col::U(AV::Bool(false));
    b.cols[755] = Col::U(AV::Bool(false));
    b.cols[756] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[757] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[758] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[759] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[760] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[761] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[405] = Col::U(AV::Bool(true));
    b.cols[406] = Col::U(AV::Bool(true));
    b.cols[762] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[763] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[408] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[409] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[411] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[412] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[415] = Col::U(AV::Bool(true));
    b.cols[764] = Col::U(AV::Bool(false));
    b.cols[765] = Col::U(AV::Bool(false));
    b.cols[766] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[767] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[768] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[769] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[770] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[771] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[424] = Col::U(AV::Bool(true));
    b.cols[425] = Col::U(AV::Bool(true));
    b.cols[772] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[773] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[427] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[428] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[430] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[431] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[434] = Col::U(AV::Bool(true));
    b.cols[774] = Col::U(AV::Bool(false));
    b.cols[775] = Col::U(AV::Bool(false));
    b.cols[776] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[777] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[778] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[779] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[780] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[781] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[443] = Col::U(AV::Bool(true));
    b.cols[444] = Col::U(AV::Bool(true));
    b.cols[782] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[783] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[446] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[447] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[449] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[450] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[453] = Col::U(AV::Bool(true));
    b.cols[784] = Col::U(AV::Bool(false));
    b.cols[785] = Col::U(AV::Bool(false));
    b.cols[786] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[787] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[788] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[789] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[790] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[791] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[462] = Col::U(AV::Bool(true));
    b.cols[463] = Col::U(AV::Bool(true));
    b.cols[792] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[793] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[465] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[466] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[468] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[469] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[472] = Col::U(AV::Bool(true));
    b.cols[794] = Col::U(AV::Bool(false));
    b.cols[795] = Col::U(AV::Bool(false));
    b.cols[796] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[797] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[798] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[799] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[800] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[801] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[481] = Col::U(AV::Bool(true));
    b.cols[482] = Col::U(AV::Bool(true));
    b.cols[802] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[803] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[484] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[485] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[487] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[488] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[491] = Col::U(AV::Bool(true));
    b.cols[804] = Col::U(AV::Bool(false));
    b.cols[805] = Col::U(AV::Bool(false));
    b.cols[806] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[807] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[808] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[809] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[810] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[811] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[500] = Col::U(AV::Bool(true));
    b.cols[501] = Col::U(AV::Bool(true));
    b.cols[812] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[813] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[503] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[504] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[506] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[507] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[510] = Col::U(AV::Bool(true));
    b.cols[814] = Col::U(AV::Bool(false));
    b.cols[815] = Col::U(AV::Bool(false));
    b.cols[816] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[817] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[818] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[819] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[820] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[821] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[519] = Col::U(AV::Bool(true));
    b.cols[520] = Col::U(AV::Bool(true));
    b.cols[822] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[823] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[522] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[523] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[525] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[526] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
    for (cell, _) in OUT_UBOOL_4 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_4: u64 = 2180868251631278096;
pub const KPART2_4: u64 = 15589330933981481702;

/// Append this assignment's lanes that TAKE outcome 4 and
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
pub fn append4(
    acc: &mut Rt2, sh: &KShared4, kv: &KOut4, take: u16,
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
        let k0 = mix64(KPART1_4.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_4.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if sh.c38.known & (1 << i) != 0 {
                AV::Bool(sh.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

/// An EMPTY accumulator with outcome 5's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append5`.
pub fn acc5(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_5, OUT_GLOBALS_5, OUT_PTRS_5, 0, cart, cache);
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[198] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[190] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[186] = Col::U(AV::Bool(true));
    b.cols[188] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
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
    b.cols[86] = Col::N(Vec::new());
    b.cols[238] = Col::U(AV::Bool(true));
    b.cols[366] = Col::U(AV::Bool(false));
    b.cols[367] = Col::U(AV::Bool(false));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[369] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::N(Vec::new());
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[375] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::N(Vec::new());
    b.cols[376] = Col::U(AV::Bool(false));
    b.cols[377] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[380] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[381] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[385] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::N(Vec::new());
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[278] = Col::N(Vec::new());
    b.cols[386] = Col::U(AV::Bool(false));
    b.cols[387] = Col::U(AV::Bool(false));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[281] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[391] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[289] = Col::U(AV::Bool(true));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[395] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[291] = Col::N(Vec::new());
    b.cols[293] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[294] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[297] = Col::U(AV::Bool(true));
    b.cols[396] = Col::N(Vec::new());
    b.cols[397] = Col::N(Vec::new());
    b.cols[299] = Col::N(Vec::new());
    b.cols[398] = Col::N(Vec::new());
    b.cols[399] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[302] = Col::N(Vec::new());
    b.cols[400] = Col::V(Vec::new());
    b.cols[401] = Col::U(AV::Bool(false));
    b.cols[304] = Col::N(Vec::new());
    b.cols[402] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[403] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[404] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[405] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[311] = Col::V(Vec::new());
    b.cols[312] = Col::V(Vec::new());
    b.cols[406] = Col::N(Vec::new());
    b.cols[407] = Col::N(Vec::new());
    b.cols[314] = Col::U(AV::Bool(true));
    b.cols[408] = Col::N(Vec::new());
    b.cols[409] = Col::N(Vec::new());
    b.cols[318] = Col::N(Vec::new());
    b.cols[319] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_5 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_5: u64 = 4593970326063826993;
pub const KPART2_5: u64 = 18380590959327347781;

/// Append this assignment's lanes that TAKE outcome 5 and
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
pub fn append5(
    acc: &mut Rt2, sh: &KShared5, kv: &KOut5, take: u16,
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
        let k0 = mix64(KPART1_5.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_5.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::N(v) = &mut acc.cols[258] { v.push(sh.c258.lane(i)); }
        if let Col::N(v) = &mut acc.cols[271] { v.push(sh.c271.lane(i)); }
        if let Col::N(v) = &mut acc.cols[278] { v.push(sh.c278.lane(i)); }
        if let Col::N(v) = &mut acc.cols[291] { v.push(sh.c291.lane(i)); }
        if let Col::N(v) = &mut acc.cols[396] { v.push(kv.c396.lane(i)); }
        if let Col::N(v) = &mut acc.cols[397] { v.push(kv.c397.lane(i)); }
        if let Col::N(v) = &mut acc.cols[299] { v.push(kv.c299.lane(i)); }
        if let Col::N(v) = &mut acc.cols[398] { v.push(kv.c398.lane(i)); }
        if let Col::N(v) = &mut acc.cols[399] { v.push(kv.c399.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::N(v) = &mut acc.cols[302] { v.push(kv.c302.lane(i)); }
        if let Col::V(v) = &mut acc.cols[400] {
            v.push(if kv.c400.known & (1 << i) != 0 {
                AV::Bool(kv.c400.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[304] { v.push(kv.c304.lane(i)); }
        if let Col::V(v) = &mut acc.cols[311] {
            v.push(if kv.c311.known & (1 << i) != 0 {
                AV::Bool(kv.c311.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[312] {
            v.push(if kv.c312.known & (1 << i) != 0 {
                AV::Bool(kv.c312.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[406] { v.push(sh.c406.lane(i)); }
        if let Col::N(v) = &mut acc.cols[407] { v.push(sh.c407.lane(i)); }
        if let Col::N(v) = &mut acc.cols[408] { v.push(kv.c408.lane(i)); }
        if let Col::N(v) = &mut acc.cols[409] { v.push(kv.c409.lane(i)); }
        if let Col::N(v) = &mut acc.cols[318] { v.push(kv.c318.lane(i)); }
        if let Col::N(v) = &mut acc.cols[319] { v.push(sh.c319.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
    }
    wrote
}

pub const OUTCOMES: usize = 6;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        4 => acc4(cart, cache),
        5 => acc5(cart, cache),
        _ => panic!("outcome {} of 6", i),
    }
}

pub fn out_slots(i: usize) -> &'static [(u32, &'static str)] {
    match i {
        0 => OUT_SLOTS_0,
        1 => OUT_SLOTS_1,
        2 => OUT_SLOTS_2,
        3 => OUT_SLOTS_3,
        4 => OUT_SLOTS_4,
        5 => OUT_SLOTS_5,
        _ => panic!("outcome {} of 6", i),
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
    fn o4(&mut self, _mask: u8, take: u16, sh: &KShared4, v: &KOut4) {
        append4(&mut self.accs[4], sh, v, take, self.n, &mut self.seen[4], self.org, self.skip);
    }
    fn o5(&mut self, _mask: u8, take: u16, sh: &KShared5, v: &KOut5) {
        append5(&mut self.accs[5], sh, v, take, self.n, &mut self.seen[5], self.org, self.skip);
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
    fn o4(&mut self, mask: u8, take: u16, sh: &KShared4, v: &KOut4);
    fn o5(&mut self, mask: u8, take: u16, sh: &KShared5, v: &KOut5);
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
    let r_c260: ZN = rin.c260;
    let r_c261: ZN = rin.c261;
    let r_c269: ZB = ZB { val: rin.c269, known: ALL };
    let r_c271: ZN = rin.c271;
    let r_c273: ZN = rin.c273;
    let r_c274: ZN = rin.c274;
    let r_c277: ZB = ZB { val: rin.c277, known: ALL };
    let r_c278: ZN = rin.c278;
    let r_c280: ZN = rin.c280;
    let r_c281: ZN = rin.c281;
    let r_c289: ZB = ZB { val: rin.c289, known: ALL };
    let r_c291: ZN = rin.c291;
    let r_c293: ZN = rin.c293;
    let r_c294: ZN = rin.c294;
    let r_c297: ZB = ZB { val: rin.c297, known: ALL };
    let r_c299: ZN = rin.c299;
    let r_c301: ZN = rin.c301;
    let r_c302: ZN = rin.c302;
    let r_c304: ZN = rin.c304;
    let r_c311: ZB = ZB { val: rin.c311, known: ALL };
    let r_c312: ZB = ZB { val: rin.c312, known: ALL };
    let r_c314: ZB = ZB { val: rin.c314, known: ALL };
    let r_c318: ZN = rin.c318;
    let r_c319: ZN = rin.c319;
    let r_c366: ZB = ZB { val: rin.c366, known: ALL };
    let r_c367: ZB = ZB { val: rin.c367, known: ALL };
    let r_c372: ZN = rin.c372;
    let r_c373: ZN = rin.c373;
    let r_c374: ZN = rin.c374;
    let r_c375: ZN = rin.c375;
    let r_c376: ZB = ZB { val: rin.c376, known: ALL };
    let r_c377: ZB = ZB { val: rin.c377, known: ALL };
    let r_c382: ZN = rin.c382;
    let r_c383: ZN = rin.c383;
    let r_c384: ZN = rin.c384;
    let r_c385: ZN = rin.c385;
    let r_c386: ZB = ZB { val: rin.c386, known: ALL };
    let r_c387: ZB = ZB { val: rin.c387, known: ALL };
    let r_c392: ZN = rin.c392;
    let r_c393: ZN = rin.c393;
    let r_c394: ZN = rin.c394;
    let r_c395: ZN = rin.c395;
    let r_c396: ZN = rin.c396;
    let r_c397: ZN = rin.c397;
    let r_c398: ZN = rin.c398;
    let r_c399: ZN = rin.c399;
    let r_c400: ZB = ZB { val: rin.c400, known: ALL };
    let r_c401: ZB = ZB { val: rin.c401, known: ALL };
    let r_c406: ZN = rin.c406;
    let r_c407: ZN = rin.c407;
    let r_c408: ZN = rin.c408;
    let r_c409: ZN = rin.c409;
    let n111: ZB = zb_not(r_c42);
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n113: ZB = zb_not(r_c366);
    let n114: ZB = zb_not(r_c367);
    let n115: bool = P8::from_raw(524288i32) == u.c368;
    let n116: bool = P8::from_raw(524288i32) == u.c369;
    let n117: bool = P8::from_raw(0i32) == u.c370;
    let n118: bool = P8::from_raw(0i32) == u.c371;
    let n119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c372);
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c373);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c374);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c375);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c250);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c251);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c253);
    let n126: ZB = zb_not(r_c376);
    let n127: ZB = zb_not(r_c377);
    let n128: bool = P8::from_raw(0i32) == u.c380;
    let n129: bool = P8::from_raw(0i32) == u.c381;
    let n130: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c383);
    let n131: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c385);
    let n132: ZB = zb_not(r_c386);
    let n133: ZB = zb_not(r_c387);
    let n134: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n135: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c281);
    let n136: bool = P8::from_raw(524288i32) == u.c388;
    let n137: bool = P8::from_raw(524288i32) == u.c389;
    let n138: bool = P8::from_raw(0i32) == u.c390;
    let n139: bool = P8::from_raw(0i32) == u.c391;
    let n140: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c392);
    let n141: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c393);
    let n142: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c394);
    let n143: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c395);
    let n144: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c291);
    let n145: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c294);
    let n149: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n150: ZN = zn_rem(n149, zn_splat(P8::from_raw(1966080i32)));
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n150);
    let n152: ZN = zn_sub(r_c258, zn_splat(P8::from_raw(65536i32)));
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c260);
    let n168: bool = P8::from_raw(524288i32) == u.c378;
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c382);
    let n170: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c384);
    let n171: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n172: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n195: ZB = zb_not(r_c43);
    let n196: ZN = zn_add(zn_splat(P8::from_raw(0i32)), r_c254);
    let n197: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n196);
    let n198: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n196);
    let n199: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n198);
    let n203: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c261);
    let n204: bool = P8::from_raw(524288i32) == u.c379;
    let n205: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c273);
    let n206: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c274);
    let n220: ZB = zn_gt(r_c258, zn_splat(P8::from_raw(0i32)));
    let n221: ZB = zn_le(n152, zn_splat(P8::from_raw(0i32)));
    let n222: ZN = zsel_n(n220, n152, r_c258);
    let n226: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c271);
    let n227: ZB = zb_not(n226);
    let n228: ZN = zsel_n(n221, zn_splat(P8::from_raw(1179648i32)), r_c271);
    let n229: ZN = zsel_n(n220, n228, r_c271);
    let n231: ZB = zb_not(r_c311);
    let n232: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c293);
    let n233: ZB = zb_not(n144);
    let n234: ZB = zn_gt(r_c278, zn_splat(P8::from_raw(0i32)));
    let n235: ZN = zn_sub(r_c278, zn_splat(P8::from_raw(65536i32)));
    let n236: ZB = zn_le(n235, zn_splat(P8::from_raw(0i32)));
    let n237: ZN = zsel_n(n236, zn_splat(P8::from_raw(1179648i32)), r_c291);
    let n238: ZN = zsel_n(n234, n235, r_c278);
    let n239: ZN = zsel_n(n234, n237, r_c291);
    let n256: ZB = zb_not(r_c401);
    let n257: bool = P8::from_raw(327680i32) == u.c402;
    let n258: bool = P8::from_raw(393216i32) == u.c403;
    let n259: bool = P8::from_raw(65536i32) == u.c404;
    let n260: bool = P8::from_raw(196608i32) == u.c405;
    let n261: ZB = zb_not(r_c38);
    let n262: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n263: ZN = zn_rem(n262, zn_splat(P8::from_raw(3932160i32)));
    let n264: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n263);
    let n265: ZN = zsel_n(n264, n171, r_c86);
    let n266: ZN = zsel_n(n151, n265, r_c86);
    let n267: ZN = zsel_n(n151, n263, r_c85);
    let n268: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c318);
    let n269: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n268);
    let n270: ZB = zn_gt(n269, zn_splat(P8::from_raw(524288i32)));
    let n271: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c319);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n271);
    let n273: ZB = zn_gt(n272, n197);
    let n274: ZB = zb_and(n270, n273);
    let n275: ZB = zn_lt(n268, zn_splat(P8::from_raw(1048576i32)));
    let n276: ZB = zb_and(n274, n275);
    let n277: ZB = zn_lt(n271, n199);
    let n278: ZB = zb_and(n276, n277);
    let n279: ZB = zn_gt(n269, zn_splat(P8::from_raw(6815744i32)));
    let n280: ZB = zn_gt(n272, zn_splat(P8::from_raw(7340032i32)));
    let n281: ZB = zn_lt(n268, zn_splat(P8::from_raw(7340032i32)));
    let n282: ZB = zn_lt(n271, zn_splat(P8::from_raw(7864320i32)));
    let n283: ZB = zn_ge(r_c409, zn_splat(P8::from_raw(0i32)));
    let n284: ZN = zn_mul(r_c408, zn_splat(P8::from_raw(13107i32)));
    let n285: ZN = zsel_n(n283, zn_splat(P8::from_raw(7077888i32)), r_c319);
    let n286: ZN = zsel_n(n283, n284, r_c408);
    let n287: ZN = zsel_n(n283, zn_splat(P8::from_raw(-196608i32)), r_c409);
    let n288: ZB = zb_not(r_c312);
    let n289: ZB = zn_gt(r_c304, zn_splat(P8::from_raw(0i32)));
    let n290: ZN = zn_sub(r_c304, zn_splat(P8::from_raw(65536i32)));
    let n291: ZN = zsel_n(n289, n290, r_c304);
    let n292: ZN = zn_sub(r_c299, zn_splat(P8::from_raw(65536i32)));
    let n293: ZB = zn_gt(r_c301, zn_splat(P8::from_raw(0i32)));
    let n294: ZN = zn_sub(r_c301, zn_splat(P8::from_raw(65536i32)));
    let n295: ZN = zsel_n(n293, n294, r_c301);
    let n296: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n297: ZB = zn_gt(n269, zn_splat(P8::from_raw(2621440i32)));
    let n298: ZB = zb_and(n280, n297);
    let n299: ZB = zn_lt(n268, zn_splat(P8::from_raw(3145728i32)));
    let n300: ZB = zb_and(n298, n299);
    let n301: ZB = zb_and(n282, n300);
    let n302: ZN = zsel_n(n283, zn_splat(P8::from_raw(655360i32)), r_c258);
    let n303: ZN = zsel_n(n283, zn_splat(P8::from_raw(1245184i32)), r_c271);
    let n304: ZN = zsel_n(n301, n302, r_c258);
    let n305: ZN = zsel_n(n301, n303, r_c271);
    let n306: ZN = zsel_n(n301, n285, r_c319);
    let n307: ZN = zsel_n(n301, n286, r_c408);
    let n308: ZN = zsel_n(n301, n287, r_c409);
    let n309: ZN = zsel_n(n227, n222, n304);
    let n310: ZN = zsel_n(n227, n229, n305);
    let n311: ZN = zsel_n(n227, r_c319, n306);
    let n312: ZN = zsel_n(n227, r_c408, n307);
    let n313: ZN = zsel_n(n227, r_c409, n308);
    let n314: ZB = zb_and(n172, n270);
    let n315: ZB = zb_and(n273, n314);
    let n316: ZB = zb_and(n275, n315);
    let n317: ZB = zb_and(n277, n316);
    let n318: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n311);
    let n319: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n318);
    let n320: ZB = zn_gt(n319, zn_splat(P8::from_raw(7340032i32)));
    let n321: ZB = zb_and(n279, n320);
    let n322: ZB = zb_and(n281, n321);
    let n323: ZB = zn_lt(n318, zn_splat(P8::from_raw(7864320i32)));
    let n324: ZB = zb_and(n322, n323);
    let n325: ZB = zn_ge(n313, zn_splat(P8::from_raw(0i32)));
    let n326: ZN = zn_mul(n312, zn_splat(P8::from_raw(13107i32)));
    let n327: ZN = zsel_n(n325, zn_splat(P8::from_raw(7077888i32)), n311);
    let n328: ZN = zsel_n(n325, zn_splat(P8::from_raw(655360i32)), r_c278);
    let n329: ZN = zsel_n(n325, zn_splat(P8::from_raw(1245184i32)), r_c291);
    let n330: ZN = zsel_n(n325, n326, n312);
    let n331: ZN = zsel_n(n325, zn_splat(P8::from_raw(-196608i32)), n313);
    let n332: ZN = zsel_n(n324, n328, r_c278);
    let n333: ZN = zsel_n(n324, n329, r_c291);
    let n334: ZN = zsel_n(n324, n327, n311);
    let n335: ZN = zsel_n(n324, n330, n312);
    let n336: ZN = zsel_n(n324, n331, n313);
    let n337: ZN = zsel_n(n233, n238, n332);
    let n338: ZN = zsel_n(n233, n239, n333);
    let n339: ZN = zsel_n(n233, n311, n334);
    let n340: ZN = zsel_n(n233, n312, n335);
    let n341: ZN = zsel_n(n233, n313, n336);
    let n342: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n340);
    let n343: ZB = zb_not(n342);
    let n344: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n341);
    let n345: ZB = zb_not(n344);
    let n346: ZB = zb_or(n343, n345);
    let n347: ZB = zb_not(n346);
    let n348: ZN = zn_add(r_c406, n340);
    let n349: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n348);
    let n350: ZN = zn_flr(n349);
    let n351: ZN = zn_sub(n349, zn_splat(P8::from_raw(32768i32)));
    let n352: ZN = zn_sub(n351, n350);
    let n353: ZB = zn_gt(n350, zn_splat(P8::from_raw(0i32)));
    let n354: ZB = zn_lt(n350, zn_splat(P8::from_raw(0i32)));
    let n355: ZN = zsel_n(n354, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n356: ZN = zsel_n(n353, zn_splat(P8::from_raw(65536i32)), n355);
    let n357: ZN = zn_abs(n350);
    let n358: ZN = zn_add(n268, n356);
    let n359: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n339);
    let n360: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n359);
    let n361: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n362: ZN = zn_add(r_c318, n356);
    let n363: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n357);
    let n364: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n362);
    let n365: ZN = zn_add(n356, n364);
    let n366: ZB = zn_tile_flag_at(g.cache, g.cart, n365, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n367: ZN = zn_add(n356, n362);
    let n368: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n357);
    let n369: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n367);
    let n370: ZN = zn_add(n356, n369);
    let n371: ZB = zn_tile_flag_at(g.cache, g.cart, n370, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n372: ZN = zn_add(n356, n367);
    let n373: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n357);
    let n374: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n372);
    let n375: ZN = zn_add(n356, n374);
    let n376: ZB = zn_tile_flag_at(g.cache, g.cart, n375, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n377: ZN = zn_add(n356, n372);
    let n378: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n357);
    let n379: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n377);
    let n380: ZN = zn_add(n356, n379);
    let n381: ZB = zn_tile_flag_at(g.cache, g.cart, n380, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n382: ZN = zn_add(n356, n377);
    let n383: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n357);
    let n384: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n382);
    let n385: ZN = zn_add(n356, n384);
    let n386: ZB = zn_tile_flag_at(g.cache, g.cart, n385, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n387: ZN = zn_add(n356, n382);
    let n388: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n357);
    let n389: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n387);
    let n390: ZN = zn_add(n356, n389);
    let n391: ZB = zn_tile_flag_at(g.cache, g.cart, n390, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n392: ZN = zn_add(n356, n387);
    let n393: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n357);
    let n394: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n392);
    let n395: ZN = zn_add(n356, n394);
    let n396: ZB = zn_tile_flag_at(g.cache, g.cart, n395, n360, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n397: ZN = zn_add(n356, n392);
    let n398: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n357);
    let n399: ZN = zsel_n(n396, n392, n397);
    let n400: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n352);
    let n401: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n340);
    let n402: ZB = zb_or(n396, n398);
    let n403: ZN = zsel_n(n393, n392, n399);
    let n404: ZN = zsel_n(n393, n352, n400);
    let n405: ZN = zsel_n(n393, n340, n401);
    let n406: ZB = zb_or(n393, n402);
    let n407: ZN = zsel_n(n391, n387, n403);
    let n408: ZN = zsel_n(n391, zn_splat(P8::from_raw(0i32)), n404);
    let n409: ZN = zsel_n(n391, zn_splat(P8::from_raw(0i32)), n405);
    let n410: ZB = zb_or(n391, n406);
    let n411: ZN = zsel_n(n388, n387, n407);
    let n412: ZN = zsel_n(n388, n352, n408);
    let n413: ZN = zsel_n(n388, n340, n409);
    let n414: ZB = zb_or(n388, n410);
    let n415: ZN = zsel_n(n386, n382, n411);
    let n416: ZN = zsel_n(n386, zn_splat(P8::from_raw(0i32)), n412);
    let n417: ZN = zsel_n(n386, zn_splat(P8::from_raw(0i32)), n413);
    let n418: ZB = zb_or(n386, n414);
    let n419: ZN = zsel_n(n383, n382, n415);
    let n420: ZN = zsel_n(n383, n352, n416);
    let n421: ZN = zsel_n(n383, n340, n417);
    let n422: ZB = zb_or(n383, n418);
    let n423: ZN = zsel_n(n381, n377, n419);
    let n424: ZN = zsel_n(n381, zn_splat(P8::from_raw(0i32)), n420);
    let n425: ZN = zsel_n(n381, zn_splat(P8::from_raw(0i32)), n421);
    let n426: ZB = zb_or(n381, n422);
    let n427: ZN = zsel_n(n378, n377, n423);
    let n428: ZN = zsel_n(n378, n352, n424);
    let n429: ZN = zsel_n(n378, n340, n425);
    let n430: ZB = zb_or(n378, n426);
    let n431: ZN = zsel_n(n376, n372, n427);
    let n432: ZN = zsel_n(n376, zn_splat(P8::from_raw(0i32)), n428);
    let n433: ZN = zsel_n(n376, zn_splat(P8::from_raw(0i32)), n429);
    let n434: ZB = zb_or(n376, n430);
    let n435: ZN = zsel_n(n373, n372, n431);
    let n436: ZN = zsel_n(n373, n352, n432);
    let n437: ZN = zsel_n(n373, n340, n433);
    let n438: ZB = zb_or(n373, n434);
    let n439: ZN = zsel_n(n371, n367, n435);
    let n440: ZN = zsel_n(n371, zn_splat(P8::from_raw(0i32)), n436);
    let n441: ZN = zsel_n(n371, zn_splat(P8::from_raw(0i32)), n437);
    let n442: ZB = zb_or(n371, n438);
    let n443: ZN = zsel_n(n368, n367, n439);
    let n444: ZN = zsel_n(n368, n352, n440);
    let n445: ZN = zsel_n(n368, n340, n441);
    let n446: ZB = zb_or(n368, n442);
    let n447: ZN = zsel_n(n366, n362, n443);
    let n448: ZN = zsel_n(n366, zn_splat(P8::from_raw(0i32)), n444);
    let n449: ZN = zsel_n(n366, zn_splat(P8::from_raw(0i32)), n445);
    let n450: ZB = zb_or(n366, n446);
    let n451: ZN = zsel_n(n363, n362, n447);
    let n452: ZN = zsel_n(n363, n352, n448);
    let n453: ZN = zsel_n(n363, n340, n449);
    let n454: ZB = zb_or(n363, n450);
    let n455: ZN = zsel_n(n361, r_c318, n451);
    let n456: ZN = zsel_n(n361, zn_splat(P8::from_raw(0i32)), n452);
    let n457: ZN = zsel_n(n361, zn_splat(P8::from_raw(0i32)), n453);
    let n458: ZB = zb_or(n361, n454);
    let n459: ZN = zn_add(r_c407, n341);
    let n460: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n459);
    let n461: ZN = zn_flr(n460);
    let n462: ZN = zn_sub(n460, zn_splat(P8::from_raw(32768i32)));
    let n463: ZN = zn_sub(n462, n461);
    let n464: ZB = zn_gt(n461, zn_splat(P8::from_raw(0i32)));
    let n465: ZB = zn_lt(n461, zn_splat(P8::from_raw(0i32)));
    let n466: ZN = zsel_n(n465, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n467: ZN = zsel_n(n464, zn_splat(P8::from_raw(65536i32)), n466);
    let n468: ZN = zn_abs(n461);
    let n469: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n455);
    let n470: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n469);
    let n471: ZN = zn_add(n359, n467);
    let n472: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n471, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n473: ZN = zn_add(n339, n467);
    let n474: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n468);
    let n475: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n473);
    let n476: ZN = zn_add(n467, n475);
    let n477: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n476, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n478: ZN = zn_add(n467, n473);
    let n479: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n468);
    let n480: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n478);
    let n481: ZN = zn_add(n467, n480);
    let n482: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n481, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n483: ZN = zn_add(n467, n478);
    let n484: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n468);
    let n485: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n483);
    let n486: ZN = zn_add(n467, n485);
    let n487: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n486, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n488: ZN = zn_add(n467, n483);
    let n489: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n468);
    let n490: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n488);
    let n491: ZN = zn_add(n467, n490);
    let n492: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n491, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n493: ZN = zn_add(n467, n488);
    let n494: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n468);
    let n495: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n493);
    let n496: ZN = zn_add(n467, n495);
    let n497: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n496, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n498: ZN = zn_add(n467, n493);
    let n499: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n468);
    let n500: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n498);
    let n501: ZN = zn_add(n467, n500);
    let n502: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n501, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n503: ZN = zn_add(n467, n498);
    let n504: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n468);
    let n505: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n503);
    let n506: ZN = zn_add(n467, n505);
    let n507: ZB = zn_tile_flag_at(g.cache, g.cart, n470, n506, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n508: ZN = zn_add(n467, n503);
    let n509: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n468);
    let n510: ZB = zb_and(n458, n509);
    let n511: ZN = zsel_n(n507, n503, n508);
    let n512: ZN = zsel_n(n507, zn_splat(P8::from_raw(0i32)), n463);
    let n513: ZN = zsel_n(n507, zn_splat(P8::from_raw(0i32)), n341);
    let n514: ZB = zsel_b(n507, n458, n510);
    let n515: ZN = zsel_n(n504, n503, n511);
    let n516: ZN = zsel_n(n504, n463, n512);
    let n517: ZN = zsel_n(n504, n341, n513);
    let n518: ZB = zsel_b(n504, n458, n514);
    let n519: ZN = zsel_n(n502, n498, n515);
    let n520: ZN = zsel_n(n502, zn_splat(P8::from_raw(0i32)), n516);
    let n521: ZN = zsel_n(n502, zn_splat(P8::from_raw(0i32)), n517);
    let n522: ZB = zsel_b(n502, n458, n518);
    let n523: ZN = zsel_n(n499, n498, n519);
    let n524: ZN = zsel_n(n499, n463, n520);
    let n525: ZN = zsel_n(n499, n341, n521);
    let n526: ZB = zsel_b(n499, n458, n522);
    let n527: ZN = zsel_n(n497, n493, n523);
    let n528: ZN = zsel_n(n497, zn_splat(P8::from_raw(0i32)), n524);
    let n529: ZN = zsel_n(n497, zn_splat(P8::from_raw(0i32)), n525);
    let n530: ZB = zsel_b(n497, n458, n526);
    let n531: ZN = zsel_n(n494, n493, n527);
    let n532: ZN = zsel_n(n494, n463, n528);
    let n533: ZN = zsel_n(n494, n341, n529);
    let n534: ZB = zsel_b(n494, n458, n530);
    let n535: ZN = zsel_n(n492, n488, n531);
    let n536: ZN = zsel_n(n492, zn_splat(P8::from_raw(0i32)), n532);
    let n537: ZN = zsel_n(n492, zn_splat(P8::from_raw(0i32)), n533);
    let n538: ZB = zsel_b(n492, n458, n534);
    let n539: ZN = zsel_n(n489, n488, n535);
    let n540: ZN = zsel_n(n489, n463, n536);
    let n541: ZN = zsel_n(n489, n341, n537);
    let n542: ZB = zsel_b(n489, n458, n538);
    let n543: ZN = zsel_n(n487, n483, n539);
    let n544: ZN = zsel_n(n487, zn_splat(P8::from_raw(0i32)), n540);
    let n545: ZN = zsel_n(n487, zn_splat(P8::from_raw(0i32)), n541);
    let n546: ZB = zsel_b(n487, n458, n542);
    let n547: ZN = zsel_n(n484, n483, n543);
    let n548: ZN = zsel_n(n484, n463, n544);
    let n549: ZN = zsel_n(n484, n341, n545);
    let n550: ZB = zsel_b(n484, n458, n546);
    let n551: ZN = zsel_n(n482, n478, n547);
    let n552: ZN = zsel_n(n482, zn_splat(P8::from_raw(0i32)), n548);
    let n553: ZN = zsel_n(n482, zn_splat(P8::from_raw(0i32)), n549);
    let n554: ZB = zsel_b(n482, n458, n550);
    let n555: ZN = zsel_n(n479, n478, n551);
    let n556: ZN = zsel_n(n479, n463, n552);
    let n557: ZN = zsel_n(n479, n341, n553);
    let n558: ZB = zsel_b(n479, n458, n554);
    let n559: ZN = zsel_n(n477, n473, n555);
    let n560: ZN = zsel_n(n477, zn_splat(P8::from_raw(0i32)), n556);
    let n561: ZN = zsel_n(n477, zn_splat(P8::from_raw(0i32)), n557);
    let n562: ZB = zsel_b(n477, n458, n558);
    let n563: ZN = zsel_n(n474, n473, n559);
    let n564: ZN = zsel_n(n474, n463, n560);
    let n565: ZN = zsel_n(n474, n341, n561);
    let n566: ZB = zsel_b(n474, n458, n562);
    let n567: ZN = zsel_n(n472, n339, n563);
    let n568: ZN = zsel_n(n472, zn_splat(P8::from_raw(0i32)), n564);
    let n569: ZN = zsel_n(n472, zn_splat(P8::from_raw(0i32)), n565);
    let n570: ZB = zsel_b(n472, n458, n566);
    let n571: ZN = zsel_n(n346, n455, r_c318);
    let n572: ZN = zsel_n(n346, n567, n339);
    let n573: ZN = zsel_n(n346, n456, r_c406);
    let n574: ZN = zsel_n(n346, n568, r_c407);
    let n575: ZN = zsel_n(n346, n457, n340);
    let n576: ZN = zsel_n(n346, n569, n341);
    let n577: ZB = zb_or(n347, n570);
    let n578: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n571);
    let n579: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n572);
    let n580: ZN = zn_div(n578, zn_splat(P8::from_raw(524288i32)));
    let n581: ZN = zn_flr(n580);
    let n582: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n581);
    let n583: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n578);
    let n584: ZN = zn_sub(n583, zn_splat(P8::from_raw(65536i32)));
    let n585: ZN = zn_div(n584, zn_splat(P8::from_raw(524288i32)));
    let n586: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n585);
    let n587: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n582);
    let n588: ZB = zn_le(n587, n586);
    let n589: ZB = zn_gt(n587, n586);
    let n590: ZB = zb_and(n317, n588);
    let n591: ZB = zb_and(n317, n589);
    let n592: ZN = zn_div(n579, zn_splat(P8::from_raw(524288i32)));
    let n593: ZN = zn_flr(n592);
    let n594: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n593);
    let n595: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n579);
    let n596: ZN = zn_sub(n595, zn_splat(P8::from_raw(65536i32)));
    let n597: ZN = zn_div(n596, zn_splat(P8::from_raw(524288i32)));
    let n598: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n597);
    let n599: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n594);
    let n600: ZB = zn_le(n599, n598);
    let n601: ZB = zn_gt(n599, n598);
    let n602: ZB = zb_and(n590, n600);
    let n603: ZB = zb_and(n590, n601);
    let n604: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n587);
    let n605: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n599);
    let n606: ZN = zn_mget(g.cart, n604, n605);
    let n607: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n606);
    let n608: ZN = zn_rem(n596, zn_splat(P8::from_raw(524288i32)));
    let n609: ZB = zn_ge(n608, zn_splat(P8::from_raw(393216i32)));
    let n610: ZN = zn_mul(n599, zn_splat(P8::from_raw(524288i32)));
    let n611: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n610);
    let n612: ZB = zn_eq(n595, n611);
    let n613: ZB = zb_or(n609, n612);
    let n614: ZB = zb_and(n607, n613);
    let n615: ZB = zn_ge(n576, zn_splat(P8::from_raw(0i32)));
    let n616: ZB = zb_and(n614, n615);
    let n617: ZB = zb_not(n616);
    let n618: ZB = zb_and(n602, n617);
    let n619: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n606);
    let n620: ZN = zn_rem(n579, zn_splat(P8::from_raw(524288i32)));
    let n621: ZB = zn_le(n620, zn_splat(P8::from_raw(131072i32)));
    let n622: ZB = zb_and(n619, n621);
    let n623: ZB = zn_le(n576, zn_splat(P8::from_raw(0i32)));
    let n624: ZB = zb_and(n622, n623);
    let n625: ZB = zb_not(n624);
    let n626: ZB = zb_and(n618, n625);
    let n627: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n606);
    let n628: ZN = zn_rem(n578, zn_splat(P8::from_raw(524288i32)));
    let n629: ZB = zn_le(n628, zn_splat(P8::from_raw(131072i32)));
    let n630: ZB = zb_and(n627, n629);
    let n631: ZB = zn_le(n575, zn_splat(P8::from_raw(0i32)));
    let n632: ZB = zb_and(n630, n631);
    let n633: ZB = zb_not(n632);
    let n634: ZB = zb_and(n626, n633);
    let n635: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n606);
    let n636: ZN = zn_rem(n584, zn_splat(P8::from_raw(524288i32)));
    let n637: ZB = zn_ge(n636, zn_splat(P8::from_raw(393216i32)));
    let n638: ZN = zn_mul(n587, zn_splat(P8::from_raw(524288i32)));
    let n639: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n638);
    let n640: ZB = zn_eq(n583, n639);
    let n641: ZB = zb_or(n637, n640);
    let n642: ZB = zb_and(n635, n641);
    let n643: ZB = zn_ge(n575, zn_splat(P8::from_raw(0i32)));
    let n644: ZB = zb_and(n642, n643);
    let n645: ZB = zb_not(n644);
    let n646: ZB = zb_and(n634, n645);
    let n647: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n594);
    let n648: ZB = zn_le(n647, n598);
    let n649: ZB = zn_gt(n647, n598);
    let n650: ZB = zb_and(n646, n648);
    let n651: ZB = zb_and(n646, n649);
    let n652: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n647);
    let n653: ZN = zn_mget(g.cart, n604, n652);
    let n654: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n653);
    let n655: ZN = zn_mul(n647, zn_splat(P8::from_raw(524288i32)));
    let n656: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n655);
    let n657: ZB = zn_eq(n595, n656);
    let n658: ZB = zb_or(n609, n657);
    let n659: ZB = zb_and(n654, n658);
    let n660: ZB = zb_and(n615, n659);
    let n661: ZB = zb_not(n660);
    let n662: ZB = zb_and(n650, n661);
    let n663: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n653);
    let n664: ZB = zb_and(n621, n663);
    let n665: ZB = zb_and(n623, n664);
    let n666: ZB = zb_not(n665);
    let n667: ZB = zb_and(n662, n666);
    let n668: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n653);
    let n669: ZB = zb_and(n629, n668);
    let n670: ZB = zb_and(n631, n669);
    let n671: ZB = zb_not(n670);
    let n672: ZB = zb_and(n667, n671);
    let n673: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n653);
    let n674: ZB = zb_and(n641, n673);
    let n675: ZB = zb_and(n643, n674);
    let n676: ZB = zb_not(n675);
    let n677: ZB = zb_and(n672, n676);
    let n678: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n594);
    let n679: ZB = zn_le(n678, n598);
    let n680: ZB = zn_gt(n678, n598);
    let n681: ZB = zb_and(n677, n679);
    let n682: ZB = zb_and(n677, n680);
    let n683: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n678);
    let n684: ZN = zn_mget(g.cart, n604, n683);
    let n685: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n684);
    let n686: ZN = zn_mul(n678, zn_splat(P8::from_raw(524288i32)));
    let n687: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n686);
    let n688: ZB = zn_eq(n595, n687);
    let n689: ZB = zb_or(n609, n688);
    let n690: ZB = zb_and(n685, n689);
    let n691: ZB = zb_and(n615, n690);
    let n692: ZB = zb_not(n691);
    let n693: ZB = zb_and(n681, n692);
    let n694: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n684);
    let n695: ZB = zb_and(n621, n694);
    let n696: ZB = zb_and(n623, n695);
    let n697: ZB = zb_not(n696);
    let n698: ZB = zb_and(n693, n697);
    let n699: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n684);
    let n700: ZB = zb_and(n629, n699);
    let n701: ZB = zb_and(n631, n700);
    let n702: ZB = zb_not(n701);
    let n703: ZB = zb_and(n698, n702);
    let n704: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n684);
    let n705: ZB = zb_and(n641, n704);
    let n706: ZB = zb_and(n643, n705);
    let n707: ZB = zb_not(n706);
    let n708: ZB = zb_and(n703, n707);
    let n709: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n594);
    let n710: ZB = zn_gt(n709, n598);
    let n711: ZB = zb_and(n577, n710);
    let n712: ZB = zb_or(n682, n708);
    let n713: ZB = zsel_b(n680, n577, n711);
    let n714: ZB = zb_or(n651, n712);
    let n715: ZB = zsel_b(n649, n577, n713);
    let n716: ZB = zb_or(n603, n714);
    let n717: ZB = zsel_b(n601, n577, n715);
    let n718: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n582);
    let n719: ZB = zn_le(n718, n586);
    let n720: ZB = zn_gt(n718, n586);
    let n721: ZB = zb_and(n716, n719);
    let n722: ZB = zb_and(n716, n720);
    let n723: ZB = zb_and(n601, n721);
    let n724: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n718);
    let n725: ZN = zn_mget(g.cart, n724, n605);
    let n726: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n725);
    let n727: ZB = zb_and(n600, n716);
    let n728: ZB = zb_and(n719, n727);
    let n729: ZB = zb_and(n613, n726);
    let n730: ZB = zb_and(n615, n729);
    let n731: ZB = zb_not(n730);
    let n732: ZB = zb_and(n728, n731);
    let n733: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n725);
    let n734: ZB = zb_and(n621, n733);
    let n735: ZB = zb_and(n623, n734);
    let n736: ZB = zb_not(n735);
    let n737: ZB = zb_and(n732, n736);
    let n738: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n725);
    let n739: ZB = zb_and(n629, n738);
    let n740: ZB = zb_and(n631, n739);
    let n741: ZB = zb_not(n740);
    let n742: ZB = zb_and(n737, n741);
    let n743: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n725);
    let n744: ZN = zn_mul(n718, zn_splat(P8::from_raw(524288i32)));
    let n745: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n744);
    let n746: ZB = zn_eq(n583, n745);
    let n747: ZB = zb_or(n637, n746);
    let n748: ZB = zb_and(n743, n747);
    let n749: ZB = zb_and(n643, n748);
    let n750: ZB = zb_not(n749);
    let n751: ZB = zb_and(n742, n750);
    let n752: ZB = zb_and(n649, n751);
    let n753: ZN = zn_mget(g.cart, n724, n652);
    let n754: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n753);
    let n755: ZB = zb_and(n648, n742);
    let n756: ZB = zb_and(n750, n755);
    let n757: ZB = zb_and(n658, n754);
    let n758: ZB = zb_and(n615, n757);
    let n759: ZB = zb_not(n758);
    let n760: ZB = zb_and(n756, n759);
    let n761: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n753);
    let n762: ZB = zb_and(n621, n761);
    let n763: ZB = zb_and(n623, n762);
    let n764: ZB = zb_not(n763);
    let n765: ZB = zb_and(n760, n764);
    let n766: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n753);
    let n767: ZB = zb_and(n629, n766);
    let n768: ZB = zb_and(n631, n767);
    let n769: ZB = zb_not(n768);
    let n770: ZB = zb_and(n765, n769);
    let n771: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n753);
    let n772: ZB = zb_and(n747, n771);
    let n773: ZB = zb_and(n643, n772);
    let n774: ZB = zb_not(n773);
    let n775: ZB = zb_and(n770, n774);
    let n776: ZB = zb_and(n680, n775);
    let n777: ZN = zn_mget(g.cart, n724, n683);
    let n778: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n777);
    let n779: ZB = zb_and(n679, n770);
    let n780: ZB = zb_and(n774, n779);
    let n781: ZB = zb_and(n689, n778);
    let n782: ZB = zb_and(n615, n781);
    let n783: ZB = zb_not(n782);
    let n784: ZB = zb_and(n780, n783);
    let n785: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n777);
    let n786: ZB = zb_and(n621, n785);
    let n787: ZB = zb_and(n623, n786);
    let n788: ZB = zb_not(n787);
    let n789: ZB = zb_and(n784, n788);
    let n790: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n777);
    let n791: ZB = zb_and(n629, n790);
    let n792: ZB = zb_and(n631, n791);
    let n793: ZB = zb_not(n792);
    let n794: ZB = zb_and(n789, n793);
    let n795: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n777);
    let n796: ZB = zb_and(n747, n795);
    let n797: ZB = zb_and(n643, n796);
    let n798: ZB = zb_not(n797);
    let n799: ZB = zb_and(n794, n798);
    let n800: ZB = zb_and(n710, n717);
    let n801: ZB = zb_or(n776, n799);
    let n802: ZB = zsel_b(n680, n717, n800);
    let n803: ZB = zb_or(n752, n801);
    let n804: ZB = zsel_b(n649, n717, n802);
    let n805: ZB = zb_or(n723, n803);
    let n806: ZB = zsel_b(n601, n717, n804);
    let n807: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n582);
    let n808: ZB = zn_le(n807, n586);
    let n809: ZB = zn_gt(n807, n586);
    let n810: ZB = zb_and(n805, n808);
    let n811: ZB = zb_and(n805, n809);
    let n812: ZB = zb_and(n601, n810);
    let n813: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n807);
    let n814: ZN = zn_mget(g.cart, n813, n605);
    let n815: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n814);
    let n816: ZB = zb_and(n600, n805);
    let n817: ZB = zb_and(n808, n816);
    let n818: ZB = zb_and(n613, n815);
    let n819: ZB = zb_and(n615, n818);
    let n820: ZB = zb_not(n819);
    let n821: ZB = zb_and(n817, n820);
    let n822: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n814);
    let n823: ZB = zb_and(n621, n822);
    let n824: ZB = zb_and(n623, n823);
    let n825: ZB = zb_not(n824);
    let n826: ZB = zb_and(n821, n825);
    let n827: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n814);
    let n828: ZB = zb_and(n629, n827);
    let n829: ZB = zb_and(n631, n828);
    let n830: ZB = zb_not(n829);
    let n831: ZB = zb_and(n826, n830);
    let n832: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n814);
    let n833: ZN = zn_mul(n807, zn_splat(P8::from_raw(524288i32)));
    let n834: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n833);
    let n835: ZB = zn_eq(n583, n834);
    let n836: ZB = zb_or(n637, n835);
    let n837: ZB = zb_and(n832, n836);
    let n838: ZB = zb_and(n643, n837);
    let n839: ZB = zb_not(n838);
    let n840: ZB = zb_and(n831, n839);
    let n841: ZB = zb_and(n649, n840);
    let n842: ZN = zn_mget(g.cart, n813, n652);
    let n843: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n842);
    let n844: ZB = zb_and(n648, n831);
    let n845: ZB = zb_and(n839, n844);
    let n846: ZB = zb_and(n658, n843);
    let n847: ZB = zb_and(n615, n846);
    let n848: ZB = zb_not(n847);
    let n849: ZB = zb_and(n845, n848);
    let n850: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n842);
    let n851: ZB = zb_and(n621, n850);
    let n852: ZB = zb_and(n623, n851);
    let n853: ZB = zb_not(n852);
    let n854: ZB = zb_and(n849, n853);
    let n855: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n842);
    let n856: ZB = zb_and(n629, n855);
    let n857: ZB = zb_and(n631, n856);
    let n858: ZB = zb_not(n857);
    let n859: ZB = zb_and(n854, n858);
    let n860: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n842);
    let n861: ZB = zb_and(n836, n860);
    let n862: ZB = zb_and(n643, n861);
    let n863: ZB = zb_not(n862);
    let n864: ZB = zb_and(n859, n863);
    let n865: ZB = zb_and(n680, n864);
    let n866: ZN = zn_mget(g.cart, n813, n683);
    let n867: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n866);
    let n868: ZB = zb_and(n679, n859);
    let n869: ZB = zb_and(n863, n868);
    let n870: ZB = zb_and(n689, n867);
    let n871: ZB = zb_and(n615, n870);
    let n872: ZB = zb_not(n871);
    let n873: ZB = zb_and(n869, n872);
    let n874: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n866);
    let n875: ZB = zb_and(n621, n874);
    let n876: ZB = zb_and(n623, n875);
    let n877: ZB = zb_not(n876);
    let n878: ZB = zb_and(n873, n877);
    let n879: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n866);
    let n880: ZB = zb_and(n629, n879);
    let n881: ZB = zb_and(n631, n880);
    let n882: ZB = zb_not(n881);
    let n883: ZB = zb_and(n878, n882);
    let n884: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n866);
    let n885: ZB = zb_and(n836, n884);
    let n886: ZB = zb_and(n643, n885);
    let n887: ZB = zb_not(n886);
    let n888: ZB = zb_and(n883, n887);
    let n889: ZB = zb_and(n710, n806);
    let n890: ZB = zb_or(n865, n888);
    let n891: ZB = zsel_b(n680, n806, n889);
    let n892: ZB = zb_or(n841, n890);
    let n893: ZB = zsel_b(n649, n806, n891);
    let n894: ZB = zb_or(n812, n892);
    let n895: ZB = zsel_b(n601, n806, n893);
    let n896: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n582);
    let n897: ZB = zn_gt(n896, n586);
    let n898: ZB = zb_and(n895, n897);
    let n899: ZB = zb_or(n811, n894);
    let n900: ZB = zsel_b(n809, n806, n898);
    let n901: ZB = zb_or(n722, n899);
    let n902: ZB = zsel_b(n720, n717, n900);
    let n903: ZB = zb_or(n591, n901);
    let n904: ZB = zsel_b(n589, n577, n902);
    let n905: ZB = zn_le(n572, zn_splat(P8::from_raw(8388608i32)));
    let n906: ZB = zb_and(n903, n905);
    let n907: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n578);
    let n908: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n579);
    let n909: ZB = zn_tile_flag_at(g.cache, g.cart, n907, n908, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n910: ZB = zb_not(n909);
    let n911: ZN = zsel_n(n909, zn_splat(P8::from_raw(393216i32)), n291);
    let n912: ZB = zn_gt(n575, r_c398);
    let n913: ZN = zn_sub(n575, r_c396);
    let n914: ZN = zn_max(r_c398, n913);
    let n915: ZN = zn_add(r_c396, n575);
    let n916: ZN = zn_min(r_c398, n915);
    let n917: ZN = zsel_n(n912, n914, n916);
    let n918: ZB = zn_gt(n576, r_c399);
    let n919: ZN = zn_sub(n576, r_c397);
    let n920: ZN = zn_max(r_c399, n919);
    let n921: ZN = zn_add(r_c397, n576);
    let n922: ZN = zn_min(r_c399, n921);
    let n923: ZN = zsel_n(n918, n920, n922);
    let n924: ZN = zsel_n(n910, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n925: ZN = zn_abs(n575);
    let n926: ZB = zn_gt(n925, zn_splat(P8::from_raw(65536i32)));
    let n927: ZB = zn_gt(n575, zn_splat(P8::from_raw(0i32)));
    let n928: ZB = zn_lt(n575, zn_splat(P8::from_raw(0i32)));
    let n929: ZB = zn_gt(n575, zn_splat(P8::from_raw(65536i32)));
    let n930: ZN = zn_sub(n575, zn_splat(P8::from_raw(9830i32)));
    let n931: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n930);
    let n932: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n575);
    let n933: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n932);
    let n934: ZB = zn_gt(n575, zn_splat(P8::from_raw(-65536i32)));
    let n935: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n930);
    let n936: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n932);
    let n937: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n930);
    let n938: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n932);
    let n939: ZN = zsel_n(n934, n935, n936);
    let n940: ZN = zsel_n(n927, n937, n938);
    let n941: ZN = zsel_n(n929, n931, n933);
    let n942: ZN = zsel_n(n928, n939, n940);
    let n943: ZN = zsel_n(n927, n941, n942);
    let n944: ZN = zn_sub(n575, n924);
    let n945: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n944);
    let n946: ZN = zn_add(n575, n924);
    let n947: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n946);
    let n948: ZN = zsel_n(n927, n945, n947);
    let n949: ZN = zsel_n(n926, n943, n948);
    let n950: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n949);
    let n951: ZB = zb_not(n950);
    let n952: ZB = zn_lt(n949, zn_splat(P8::from_raw(0i32)));
    let n953: ZB = zsel_b(n951, n952, r_c400);
    let n954: ZN = zn_abs(n576);
    let n955: ZB = zn_le(n954, zn_splat(P8::from_raw(9830i32)));
    let n956: ZN = zsel_n(n955, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n957: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n579);
    let n958: ZB = zn_gt(n576, zn_splat(P8::from_raw(131072i32)));
    let n959: ZN = zn_sub(n576, n956);
    let n960: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n959);
    let n961: ZN = zn_add(n576, n956);
    let n962: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n961);
    let n963: ZN = zsel_n(n958, n960, n962);
    let n964: ZN = zsel_n(n910, n963, n576);
    let n965: ZB = zn_gt(n911, zn_splat(P8::from_raw(0i32)));
    let n966: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n578);
    let n967: ZB = zn_tile_flag_at(g.cache, g.cart, n966, n957, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n968: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n578);
    let n969: ZB = zn_tile_flag_at(g.cache, g.cart, n968, n957, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n970: ZN = zsel_n(n969, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n971: ZN = zsel_n(n967, zn_splat(P8::from_raw(-65536i32)), n970);
    let n972: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n971);
    let n973: ZB = zb_not(n972);
    let n974: ZN = zn_neg(n971);
    let n975: ZN = zn_mul(n974, zn_splat(P8::from_raw(131072i32)));
    let n976: ZN = zsel_n(n973, n975, n949);
    let n977: ZN = zsel_n(n973, zn_splat(P8::from_raw(-131072i32)), n964);
    let n978: ZN = zsel_n(n965, zn_splat(P8::from_raw(0i32)), n911);
    let n979: ZN = zsel_n(n965, n949, n976);
    let n980: ZN = zsel_n(n965, zn_splat(P8::from_raw(-131072i32)), n977);
    let n981: ZN = zsel_n(n953, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n982: ZB = zn_gt(n981, zn_splat(P8::from_raw(0i32)));
    let n983: ZB = zn_lt(n981, zn_splat(P8::from_raw(0i32)));
    let n984: ZN = zsel_n(n983, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n985: ZN = zsel_n(n982, zn_splat(P8::from_raw(131072i32)), n984);
    let n986: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n981);
    let n987: ZB = zb_not(n986);
    let n988: ZN = zsel_n(n987, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n989: ZB = zsel_b(n293, r_c400, n953);
    let n990: ZN = zsel_n(n293, n917, n949);
    let n991: ZN = zsel_n(n293, n923, n964);
    let n992: ZB = zn_lt(n572, zn_splat(P8::from_raw(-262144i32)));
    let n993: ZB = zn_ge(n572, zn_splat(P8::from_raw(-262144i32)));
    let n994: ZB = zb_and(n906, n992);
    let n996: ZB = zn_lt(n571, zn_splat(P8::from_raw(-65536i32)));
    let n997: ZB = zn_gt(n571, zn_splat(P8::from_raw(7929856i32)));
    let n1000: ZB = zb_or(n996, n997);
    let n1001: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n571);
    let n1002: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1001);
    let n1003: ZN = zsel_n(n1000, n1002, n571);
    let n1004: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n990);
    let n1005: ZN = zsel_n(n296, n571, n1003);
    let n1006: ZN = zsel_n(n296, n990, n1004);
    let n1019: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n944);
    let n1020: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n946);
    let n1021: ZN = zsel_n(n934, n1019, n1020);
    let n1022: ZN = zsel_n(n926, n943, n1021);
    let n1023: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1022);
    let n1024: ZB = zb_not(n1023);
    let n1025: ZB = zn_lt(n1022, zn_splat(P8::from_raw(0i32)));
    let n1026: ZB = zsel_b(n1024, n1025, r_c400);
    let n1027: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n578);
    let n1028: ZB = zn_tile_flag_at(g.cache, g.cart, n1027, n957, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1029: ZN = zsel_n(n1028, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1030: ZB = zn_gt(n576, n1029);
    let n1031: ZN = zn_max(n959, n1029);
    let n1032: ZN = zn_min(n961, n1029);
    let n1033: ZN = zsel_n(n1030, n1031, n1032);
    let n1034: ZN = zsel_n(n910, n1033, n576);
    let n1035: ZN = zsel_n(n973, n975, n1022);
    let n1036: ZN = zsel_n(n973, zn_splat(P8::from_raw(-131072i32)), n1034);
    let n1037: ZN = zsel_n(n965, n1022, n1035);
    let n1038: ZN = zsel_n(n965, zn_splat(P8::from_raw(-131072i32)), n1036);
    let n1039: ZB = zsel_b(n293, r_c400, n1026);
    let n1040: ZN = zsel_n(n293, n917, n1022);
    let n1041: ZN = zsel_n(n293, n923, n1034);
    let n1042: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1040);
    let n1043: ZN = zsel_n(n296, n1040, n1042);
    let n1044: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n944);
    let n1045: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n946);
    let n1046: ZN = zsel_n(n929, n1044, n1045);
    let n1047: ZN = zsel_n(n926, n943, n1046);
    let n1048: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1047);
    let n1049: ZB = zb_not(n1048);
    let n1050: ZB = zn_lt(n1047, zn_splat(P8::from_raw(0i32)));
    let n1051: ZB = zsel_b(n1049, n1050, r_c400);
    let n1052: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n578);
    let n1053: ZB = zn_tile_flag_at(g.cache, g.cart, n1052, n957, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1054: ZN = zsel_n(n1053, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1055: ZB = zn_gt(n576, n1054);
    let n1056: ZN = zn_max(n959, n1054);
    let n1057: ZN = zn_min(n961, n1054);
    let n1058: ZN = zsel_n(n1055, n1056, n1057);
    let n1059: ZN = zsel_n(n910, n1058, n576);
    let n1060: ZN = zsel_n(n973, n975, n1047);
    let n1061: ZN = zsel_n(n973, zn_splat(P8::from_raw(-131072i32)), n1059);
    let n1062: ZN = zsel_n(n965, n1047, n1060);
    let n1063: ZN = zsel_n(n965, zn_splat(P8::from_raw(-131072i32)), n1061);
    let n1064: ZB = zsel_b(n293, r_c400, n1051);
    let n1065: ZN = zsel_n(n293, n917, n1047);
    let n1066: ZN = zsel_n(n293, n923, n1059);
    let n1067: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1065);
    let n1068: ZN = zsel_n(n296, n1065, n1067);
    let n1069: ZN = zsel_n(n288, n978, n911);
    let n1070: ZN = zsel_n(n288, n979, n949);
    let n1071: ZN = zsel_n(n288, n980, n964);
    let n1072: ZN = zsel_n(n293, n911, n1069);
    let n1073: ZN = zsel_n(n293, n917, n1070);
    let n1074: ZN = zsel_n(n293, n923, n1071);
    let n1077: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1073);
    let n1078: ZN = zsel_n(n296, n1073, n1077);
    let n1079: ZN = zsel_n(n288, n1037, n1022);
    let n1080: ZN = zsel_n(n288, n1038, n1034);
    let n1081: ZN = zsel_n(n293, n917, n1079);
    let n1082: ZN = zsel_n(n293, n923, n1080);
    let n1083: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1081);
    let n1084: ZN = zsel_n(n296, n1081, n1083);
    let n1085: ZN = zsel_n(n288, n1062, n1047);
    let n1086: ZN = zsel_n(n288, n1063, n1059);
    let n1087: ZN = zsel_n(n293, n917, n1085);
    let n1088: ZN = zsel_n(n293, n923, n1086);
    let n1089: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1087);
    let n1090: ZN = zsel_n(n296, n1087, n1089);
    let n1091: ZN = zsel_n(n231, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1092: ZB = zb_or(r_c41, n231);
    let n1093: ZN = zsel_n(n231, zn_splat(P8::from_raw(655360i32)), n292);
    let n1094: ZN = zsel_n(n231, zn_splat(P8::from_raw(262144i32)), r_c301);
    let n1095: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1096: ZN = zsel_n(n231, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n1097: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), r_c399);
    let n1098: ZN = zsel_n(n293, r_c20, n1091);
    let n1099: ZB = zsel_b(n293, r_c41, n1092);
    let n1100: ZN = zsel_n(n293, n292, n1093);
    let n1101: ZN = zsel_n(n293, n294, n1094);
    let n1102: ZN = zsel_n(n293, zn_splat(P8::from_raw(65536i32)), n1095);
    let n1103: ZN = zsel_n(n293, r_c396, n1096);
    let n1104: ZN = zsel_n(n293, r_c399, n1097);
    let n1105: ZB = zn_gt(n1098, zn_splat(P8::from_raw(0i32)));
    let n1106: ZN = zsel_n(n231, n988, r_c397);
    let n1107: ZN = zsel_n(n231, n985, r_c398);
    let n1108: ZN = zsel_n(n231, n981, n949);
    let n1109: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n964);
    let n1110: ZN = zsel_n(n293, r_c397, n1106);
    let n1111: ZN = zsel_n(n293, r_c398, n1107);
    let n1112: ZN = zsel_n(n293, n917, n1108);
    let n1113: ZN = zsel_n(n293, n923, n1109);
    let n1114: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1112);
    let n1115: ZN = zsel_n(n1105, n571, n1003);
    let n1116: ZN = zsel_n(n1105, n1112, n1114);
    let n1117: ZN = zsel_n(n231, zn_splat(P8::from_raw(69510i32)), r_c397);
    let n1118: ZN = zsel_n(n231, zn_splat(P8::from_raw(-131072i32)), r_c398);
    let n1119: ZN = zsel_n(n231, zn_splat(P8::from_raw(-327680i32)), n1022);
    let n1120: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1034);
    let n1121: ZN = zsel_n(n293, r_c397, n1117);
    let n1122: ZN = zsel_n(n293, r_c398, n1118);
    let n1123: ZN = zsel_n(n293, n917, n1119);
    let n1124: ZN = zsel_n(n293, n923, n1120);
    let n1125: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1123);
    let n1126: ZN = zsel_n(n1105, n1123, n1125);
    let n1127: ZN = zsel_n(n231, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n1128: ZN = zsel_n(n231, zn_splat(P8::from_raw(327680i32)), n1047);
    let n1129: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1059);
    let n1130: ZN = zsel_n(n293, r_c398, n1127);
    let n1131: ZN = zsel_n(n293, n917, n1128);
    let n1132: ZN = zsel_n(n293, n923, n1129);
    let n1133: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1131);
    let n1134: ZN = zsel_n(n1105, n1131, n1133);
    let n1136: ZN = zsel_n(n231, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n1137: ZN = zsel_n(n231, zn_splat(P8::from_raw(-98304i32)), r_c399);
    let n1138: ZN = zsel_n(n293, r_c396, n1136);
    let n1139: ZN = zsel_n(n293, r_c399, n1137);
    let n1140: ZN = zsel_n(n231, zn_splat(P8::from_raw(98304i32)), r_c397);
    let n1141: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), r_c398);
    let n1142: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n949);
    let n1143: ZN = zsel_n(n231, zn_splat(P8::from_raw(-327680i32)), n964);
    let n1144: ZN = zsel_n(n293, r_c397, n1140);
    let n1145: ZN = zsel_n(n293, r_c398, n1141);
    let n1146: ZN = zsel_n(n293, n917, n1142);
    let n1147: ZN = zsel_n(n293, n923, n1143);
    let n1148: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1146);
    let n1149: ZN = zsel_n(n1105, n1146, n1148);
    let n1150: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1022);
    let n1151: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1034);
    let n1152: ZN = zsel_n(n293, n917, n1150);
    let n1153: ZN = zsel_n(n293, n923, n1151);
    let n1154: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1152);
    let n1155: ZN = zsel_n(n1105, n1152, n1154);
    let n1156: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1047);
    let n1157: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1059);
    let n1158: ZN = zsel_n(n293, n917, n1156);
    let n1159: ZN = zsel_n(n293, n923, n1157);
    let n1160: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1158);
    let n1161: ZN = zsel_n(n1105, n1158, n1160);
    let n1162: ZN = zsel_n(n231, zn_splat(P8::from_raw(131072i32)), r_c399);
    let n1163: ZN = zsel_n(n293, r_c399, n1162);
    let n1164: ZN = zsel_n(n231, zn_splat(P8::from_raw(327680i32)), n964);
    let n1165: ZN = zsel_n(n293, n923, n1164);
    let n1166: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1034);
    let n1167: ZN = zsel_n(n293, n923, n1166);
    let n1168: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1059);
    let n1169: ZN = zsel_n(n293, n923, n1168);
    let n1170: ZN = zsel_n(n231, n981, n1070);
    let n1171: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1071);
    let n1172: ZN = zsel_n(n293, n917, n1170);
    let n1173: ZN = zsel_n(n293, n923, n1171);
    let n1174: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1172);
    let n1175: ZN = zsel_n(n1105, n1172, n1174);
    let n1176: ZN = zsel_n(n231, zn_splat(P8::from_raw(-327680i32)), n1079);
    let n1177: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1080);
    let n1178: ZN = zsel_n(n293, n917, n1176);
    let n1179: ZN = zsel_n(n293, n923, n1177);
    let n1180: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1178);
    let n1181: ZN = zsel_n(n1105, n1178, n1180);
    let n1182: ZN = zsel_n(n231, zn_splat(P8::from_raw(327680i32)), n1085);
    let n1183: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1086);
    let n1184: ZN = zsel_n(n293, n917, n1182);
    let n1185: ZN = zsel_n(n293, n923, n1183);
    let n1186: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1184);
    let n1187: ZN = zsel_n(n1105, n1184, n1186);
    let n1188: ZN = zsel_n(n231, zn_splat(P8::from_raw(0i32)), n1070);
    let n1189: ZN = zsel_n(n231, zn_splat(P8::from_raw(-327680i32)), n1071);
    let n1190: ZN = zsel_n(n293, n917, n1188);
    let n1191: ZN = zsel_n(n293, n923, n1189);
    let n1192: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1190);
    let n1193: ZN = zsel_n(n1105, n1190, n1192);
    let n1194: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1079);
    let n1195: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1080);
    let n1196: ZN = zsel_n(n293, n917, n1194);
    let n1197: ZN = zsel_n(n293, n923, n1195);
    let n1198: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1196);
    let n1199: ZN = zsel_n(n1105, n1196, n1198);
    let n1200: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1085);
    let n1201: ZN = zsel_n(n231, zn_splat(P8::from_raw(-231700i32)), n1086);
    let n1202: ZN = zsel_n(n293, n917, n1200);
    let n1203: ZN = zsel_n(n293, n923, n1201);
    let n1204: ZN = zsel_n(n1000, zn_splat(P8::from_raw(0i32)), n1202);
    let n1205: ZN = zsel_n(n1105, n1202, n1204);
    let n1206: ZN = zsel_n(n231, zn_splat(P8::from_raw(327680i32)), n1071);
    let n1207: ZN = zsel_n(n293, n923, n1206);
    let n1208: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1080);
    let n1209: ZN = zsel_n(n293, n923, n1208);
    let n1210: ZN = zsel_n(n231, zn_splat(P8::from_raw(231700i32)), n1086);
    let n1211: ZN = zsel_n(n293, n923, n1210);
    let n1212: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1213: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1212);
    let n1214: ZB = zb_and(n602, n616);
    let n1215: ZB = zb_and(n618, n624);
    let n1216: ZB = zb_and(n626, n632);
    let n1217: ZB = zb_and(n634, n644);
    let n1218: ZB = zb_or(n1216, n1217);
    let n1219: ZB = zb_or(n1215, n1218);
    let n1220: ZB = zb_or(n1214, n1219);
    let n1221: ZB = zb_and(n650, n660);
    let n1222: ZB = zb_and(n662, n665);
    let n1223: ZB = zb_and(n667, n670);
    let n1224: ZB = zb_and(n672, n675);
    let n1225: ZB = zb_or(n1223, n1224);
    let n1226: ZB = zb_or(n1222, n1225);
    let n1227: ZB = zb_or(n1221, n1226);
    let n1228: ZB = zb_and(n681, n691);
    let n1229: ZB = zb_and(n693, n696);
    let n1230: ZB = zb_and(n698, n701);
    let n1231: ZB = zb_and(n703, n706);
    let n1232: ZB = zb_or(n1230, n1231);
    let n1233: ZB = zb_or(n1229, n1232);
    let n1234: ZB = zb_or(n1228, n1233);
    let n1235: ZB = zb_or(n1227, n1234);
    let n1236: ZB = zb_or(n1220, n1235);
    let n1237: ZB = zb_and(n728, n730);
    let n1238: ZB = zb_and(n732, n735);
    let n1239: ZB = zb_and(n737, n740);
    let n1240: ZB = zb_and(n742, n749);
    let n1241: ZB = zb_or(n1239, n1240);
    let n1242: ZB = zb_or(n1238, n1241);
    let n1243: ZB = zb_or(n1237, n1242);
    let n1244: ZB = zb_and(n756, n758);
    let n1245: ZB = zb_and(n760, n763);
    let n1246: ZB = zb_and(n765, n768);
    let n1247: ZB = zb_and(n770, n773);
    let n1248: ZB = zb_or(n1246, n1247);
    let n1249: ZB = zb_or(n1245, n1248);
    let n1250: ZB = zb_or(n1244, n1249);
    let n1251: ZB = zb_and(n780, n782);
    let n1252: ZB = zb_and(n784, n787);
    let n1253: ZB = zb_and(n789, n792);
    let n1254: ZB = zb_and(n794, n797);
    let n1255: ZB = zb_or(n1253, n1254);
    let n1256: ZB = zb_or(n1252, n1255);
    let n1257: ZB = zb_or(n1251, n1256);
    let n1258: ZB = zb_or(n1250, n1257);
    let n1259: ZB = zb_or(n1243, n1258);
    let n1260: ZB = zb_and(n817, n819);
    let n1261: ZB = zb_and(n821, n824);
    let n1262: ZB = zb_and(n826, n829);
    let n1263: ZB = zb_and(n831, n838);
    let n1264: ZB = zb_or(n1262, n1263);
    let n1265: ZB = zb_or(n1261, n1264);
    let n1266: ZB = zb_or(n1260, n1265);
    let n1267: ZB = zb_and(n845, n847);
    let n1268: ZB = zb_and(n849, n852);
    let n1269: ZB = zb_and(n854, n857);
    let n1270: ZB = zb_and(n859, n862);
    let n1271: ZB = zb_or(n1269, n1270);
    let n1272: ZB = zb_or(n1268, n1271);
    let n1273: ZB = zb_or(n1267, n1272);
    let n1274: ZB = zb_and(n869, n871);
    let n1275: ZB = zb_and(n873, n876);
    let n1276: ZB = zb_and(n878, n881);
    let n1277: ZB = zb_and(n883, n886);
    let n1278: ZB = zb_or(n1276, n1277);
    let n1279: ZB = zb_or(n1275, n1278);
    let n1280: ZB = zb_or(n1274, n1279);
    let n1281: ZB = zb_or(n1273, n1280);
    let n1282: ZB = zb_or(n1266, n1281);
    let n1283: ZB = zb_or(n1259, n1282);
    let n1284: ZB = zsel_b(n1259, n717, n806);
    let n1285: ZB = zb_or(n1236, n1283);
    let n1286: ZB = zsel_b(n1236, n577, n1284);
    let n1287: ZB = zn_gt(n572, zn_splat(P8::from_raw(8388608i32)));
    let n1288: ZB = zb_and(n903, n1287);
    let n1289: ZB = zb_or(n1285, n1288);
    let n1290: ZB = zsel_b(n1285, n1286, n904);
    let n1291: ZB = zb_and(n992, n1289);
    let n1293: ZN = zsel_n(n1287, n1213, n1212);
    let n1294: ZN = zsel_n(n1285, n1293, n1212);
    let n1298: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c246);
    let n1299: ZN = zn_div(n1298, zn_splat(P8::from_raw(2621440i32)));
    let n1300: ZN = zn_sin(n1299);
    let n1302: ZN = zn_mul(n1300, zn_splat(P8::from_raw(163840i32)));
    let n1303: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n1302);
    let n1304: ZB = zb_not(n278);
    let n1305: ZN = zsel_n(n283, zn_splat(P8::from_raw(65536i32)), r_c302);
    let n1306: ZN = zsel_n(n301, n1305, r_c302);
    let n1307: ZN = zsel_n(n227, r_c302, n1306);
    let n1308: ZB = zb_and(n172, n1304);
    let n1309: ZN = zsel_n(n325, zn_splat(P8::from_raw(65536i32)), n1307);
    let n1310: ZN = zsel_n(n324, n1309, n1307);
    let n1311: ZN = zsel_n(n233, n1307, n1310);
    let n1312: ZB = zb_and(n588, n1308);
    let n1313: ZB = zb_and(n589, n1308);
    let n1314: ZB = zb_and(n601, n1312);
    let n1315: ZB = zb_and(n588, n600);
    let n1316: ZB = zb_and(n1308, n1315);
    let n1317: ZB = zb_and(n616, n1316);
    let n1318: ZB = zb_and(n617, n1316);
    let n1319: ZB = zb_and(n624, n1318);
    let n1320: ZB = zb_and(n625, n1318);
    let n1321: ZB = zb_and(n632, n1320);
    let n1322: ZB = zb_and(n633, n1320);
    let n1323: ZB = zb_and(n644, n1322);
    let n1324: ZB = zb_and(n645, n1322);
    let n1325: ZB = zb_or(n1321, n1323);
    let n1326: ZB = zb_or(n1319, n1325);
    let n1327: ZB = zb_or(n1317, n1326);
    let n1328: ZB = zb_and(n649, n1324);
    let n1329: ZB = zb_and(n645, n648);
    let n1330: ZB = zb_and(n1322, n1329);
    let n1331: ZB = zb_and(n660, n1330);
    let n1332: ZB = zb_and(n661, n1330);
    let n1333: ZB = zb_and(n665, n1332);
    let n1334: ZB = zb_and(n666, n1332);
    let n1335: ZB = zb_and(n670, n1334);
    let n1336: ZB = zb_and(n671, n1334);
    let n1337: ZB = zb_and(n675, n1336);
    let n1338: ZB = zb_and(n676, n1336);
    let n1339: ZB = zb_or(n1335, n1337);
    let n1340: ZB = zb_or(n1333, n1339);
    let n1341: ZB = zb_or(n1331, n1340);
    let n1342: ZB = zb_and(n680, n1338);
    let n1343: ZB = zb_and(n676, n679);
    let n1344: ZB = zb_and(n1336, n1343);
    let n1345: ZB = zb_and(n691, n1344);
    let n1346: ZB = zb_and(n692, n1344);
    let n1347: ZB = zb_and(n696, n1346);
    let n1348: ZB = zb_and(n697, n1346);
    let n1349: ZB = zb_and(n701, n1348);
    let n1350: ZB = zb_and(n702, n1348);
    let n1351: ZB = zb_and(n706, n1350);
    let n1352: ZB = zb_and(n707, n1350);
    let n1353: ZB = zb_or(n1349, n1351);
    let n1354: ZB = zb_or(n1347, n1353);
    let n1355: ZB = zb_or(n1345, n1354);
    let n1356: ZB = zb_or(n1342, n1352);
    let n1357: ZB = zb_or(n1341, n1355);
    let n1358: ZB = zb_or(n1328, n1356);
    let n1359: ZB = zb_or(n1327, n1357);
    let n1360: ZB = zb_or(n1314, n1358);
    let n1361: ZB = zb_and(n719, n1360);
    let n1362: ZB = zb_and(n720, n1360);
    let n1363: ZB = zb_and(n601, n1361);
    let n1364: ZB = zb_and(n600, n719);
    let n1365: ZB = zb_and(n1360, n1364);
    let n1366: ZB = zb_and(n730, n1365);
    let n1367: ZB = zb_and(n731, n1365);
    let n1368: ZB = zb_and(n735, n1367);
    let n1369: ZB = zb_and(n736, n1367);
    let n1370: ZB = zb_and(n740, n1369);
    let n1371: ZB = zb_and(n741, n1369);
    let n1372: ZB = zb_and(n749, n1371);
    let n1373: ZB = zb_and(n750, n1371);
    let n1374: ZB = zb_or(n1370, n1372);
    let n1375: ZB = zb_or(n1368, n1374);
    let n1376: ZB = zb_or(n1366, n1375);
    let n1377: ZB = zb_and(n649, n1373);
    let n1378: ZB = zb_and(n648, n750);
    let n1379: ZB = zb_and(n1371, n1378);
    let n1380: ZB = zb_and(n758, n1379);
    let n1381: ZB = zb_and(n759, n1379);
    let n1382: ZB = zb_and(n763, n1381);
    let n1383: ZB = zb_and(n764, n1381);
    let n1384: ZB = zb_and(n768, n1383);
    let n1385: ZB = zb_and(n769, n1383);
    let n1386: ZB = zb_and(n773, n1385);
    let n1387: ZB = zb_and(n774, n1385);
    let n1388: ZB = zb_or(n1384, n1386);
    let n1389: ZB = zb_or(n1382, n1388);
    let n1390: ZB = zb_or(n1380, n1389);
    let n1391: ZB = zb_and(n680, n1387);
    let n1392: ZB = zb_and(n679, n774);
    let n1393: ZB = zb_and(n1385, n1392);
    let n1394: ZB = zb_and(n782, n1393);
    let n1395: ZB = zb_and(n783, n1393);
    let n1396: ZB = zb_and(n787, n1395);
    let n1397: ZB = zb_and(n788, n1395);
    let n1398: ZB = zb_and(n792, n1397);
    let n1399: ZB = zb_and(n793, n1397);
    let n1400: ZB = zb_and(n797, n1399);
    let n1401: ZB = zb_and(n798, n1399);
    let n1402: ZB = zb_or(n1398, n1400);
    let n1403: ZB = zb_or(n1396, n1402);
    let n1404: ZB = zb_or(n1394, n1403);
    let n1405: ZB = zb_or(n1391, n1401);
    let n1406: ZB = zb_or(n1390, n1404);
    let n1407: ZB = zb_or(n1377, n1405);
    let n1408: ZB = zb_or(n1376, n1406);
    let n1409: ZB = zb_or(n1363, n1407);
    let n1410: ZB = zb_and(n808, n1409);
    let n1411: ZB = zb_and(n809, n1409);
    let n1412: ZB = zb_and(n601, n1410);
    let n1413: ZB = zb_and(n600, n808);
    let n1414: ZB = zb_and(n1409, n1413);
    let n1415: ZB = zb_and(n819, n1414);
    let n1416: ZB = zb_and(n820, n1414);
    let n1417: ZB = zb_and(n824, n1416);
    let n1418: ZB = zb_and(n825, n1416);
    let n1419: ZB = zb_and(n829, n1418);
    let n1420: ZB = zb_and(n830, n1418);
    let n1421: ZB = zb_and(n838, n1420);
    let n1422: ZB = zb_and(n839, n1420);
    let n1423: ZB = zb_or(n1419, n1421);
    let n1424: ZB = zb_or(n1417, n1423);
    let n1425: ZB = zb_or(n1415, n1424);
    let n1426: ZB = zb_and(n649, n1422);
    let n1427: ZB = zb_and(n648, n839);
    let n1428: ZB = zb_and(n1420, n1427);
    let n1429: ZB = zb_and(n847, n1428);
    let n1430: ZB = zb_and(n848, n1428);
    let n1431: ZB = zb_and(n852, n1430);
    let n1432: ZB = zb_and(n853, n1430);
    let n1433: ZB = zb_and(n857, n1432);
    let n1434: ZB = zb_and(n858, n1432);
    let n1435: ZB = zb_and(n862, n1434);
    let n1436: ZB = zb_and(n863, n1434);
    let n1437: ZB = zb_or(n1433, n1435);
    let n1438: ZB = zb_or(n1431, n1437);
    let n1439: ZB = zb_or(n1429, n1438);
    let n1440: ZB = zb_and(n680, n1436);
    let n1441: ZB = zb_and(n679, n863);
    let n1442: ZB = zb_and(n1434, n1441);
    let n1443: ZB = zb_and(n871, n1442);
    let n1444: ZB = zb_and(n872, n1442);
    let n1445: ZB = zb_and(n876, n1444);
    let n1446: ZB = zb_and(n877, n1444);
    let n1447: ZB = zb_and(n881, n1446);
    let n1448: ZB = zb_and(n882, n1446);
    let n1449: ZB = zb_and(n886, n1448);
    let n1450: ZB = zb_and(n887, n1448);
    let n1451: ZB = zb_or(n1447, n1449);
    let n1452: ZB = zb_or(n1445, n1451);
    let n1453: ZB = zb_or(n1443, n1452);
    let n1454: ZB = zb_or(n1440, n1450);
    let n1455: ZB = zb_or(n1439, n1453);
    let n1456: ZB = zb_or(n1426, n1454);
    let n1457: ZB = zb_or(n1425, n1455);
    let n1458: ZB = zb_or(n1412, n1456);
    let n1459: ZB = zb_or(n1408, n1457);
    let n1460: ZB = zsel_b(n1408, n717, n806);
    let n1461: ZB = zb_or(n1411, n1458);
    let n1462: ZB = zb_or(n1359, n1459);
    let n1463: ZB = zsel_b(n1359, n577, n1460);
    let n1464: ZB = zb_or(n1362, n1461);
    let n1465: ZB = zb_or(n1313, n1464);
    let n1466: ZB = zb_and(n1287, n1465);
    let n1467: ZB = zb_or(n1462, n1466);
    let n1468: ZB = zsel_b(n1462, n1463, n904);
    let n1469: ZB = zn_lt(n1311, zn_splat(P8::from_raw(65536i32)));
    let n1470: ZN = zsel_n(n1469, zn_splat(P8::from_raw(65536i32)), n1311);
    let n1471: ZN = zsel_n(n909, n1470, n1311);
    let n1472: ZB = zn_gt(n1471, zn_splat(P8::from_raw(0i32)));
    let n1473: ZB = zb_and(n992, n1467);
    let n1475: ZN = zsel_n(n1462, n1293, n1212);
    let n1477: ZB = zb_and(n231, n1472);
    let n1478: ZN = zsel_n(n1477, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1479: ZB = zb_or(r_c41, n1477);
    let n1480: ZN = zsel_n(n293, r_c20, n1478);
    let n1481: ZB = zsel_b(n293, r_c41, n1479);
    let n1484: ZB = zb_and(n905, n1465);
    let n1485: ZB = zb_and(n992, n1484);
    let n1486: ZB = zb_and(n993, n1484);
    let n1487: ZB = zb_not(n1485);
    let n1488: ZB = zb_or(n1473, n1485);
    let n1489: ZB = zsel_b(n1485, n904, n1468);
    let n1490: ZN = zsel_n(n1485, r_c87, n1475);
    let n1491: ZN = zsel_n(n1485, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1493: ZB = zb_not(n994);
    let n1494: ZB = zb_or(n994, n1291);
    let n1495: ZB = zsel_b(n994, n904, n1290);
    let n1496: ZN = zsel_n(n994, r_c87, n1294);
    let n1497: ZN = zsel_n(n994, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n1499: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n1500: ZN = zsel_n(n296, r_c246, n1298);
    let n1501: ZN = zsel_n(n296, r_c254, n1303);
    let n1502: ZB = zb_and(r_c311, n296);
    let n1503: ZB = zb_and(r_c312, n296);
    let n1504: ZN = zn_sub(n1471, zn_splat(P8::from_raw(65536i32)));
    let n1505: ZN = zsel_n(n296, n1499, r_c20);
    let n1506: ZN = zsel_n(n296, r_c258, n309);
    let n1507: ZN = zsel_n(n296, r_c271, n310);
    let n1508: ZN = zsel_n(n296, r_c278, n337);
    let n1509: ZN = zsel_n(n296, r_c291, n338);
    let n1510: ZN = zsel_n(n296, r_c299, n292);
    let n1511: ZN = zsel_n(n296, r_c301, n295);
    let n1512: ZN = zsel_n(n296, r_c302, n1471);
    let n1513: ZN = zsel_n(n296, r_c304, n911);
    let n1514: ZN = zsel_n(n296, r_c318, n571);
    let n1515: ZN = zsel_n(n296, r_c319, n572);
    let n1516: ZB = zsel_b(n296, r_c400, n989);
    let n1517: ZN = zsel_n(n296, r_c406, n573);
    let n1518: ZN = zsel_n(n296, r_c407, n574);
    let n1519: ZN = zsel_n(n296, r_c408, n990);
    let n1520: ZN = zsel_n(n296, r_c409, n991);
    let n1521: ZB = zb_or(n296, n1486);
    let n1522: ZB = zb_or(n296, n904);
    let n1523: ZB = zn_gt(n1505, zn_splat(P8::from_raw(0i32)));
    let n1524: ZB = zn_lt(n1514, zn_splat(P8::from_raw(-65536i32)));
    let n1525: ZB = zn_gt(n1514, zn_splat(P8::from_raw(7929856i32)));
    let n1526: ZB = zb_or(n1524, n1525);
    let n1527: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1514);
    let n1528: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1527);
    let n1529: ZN = zsel_n(n1526, n1528, n1514);
    let n1530: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1519);
    let n1531: ZN = zsel_n(n1523, n1514, n1529);
    let n1532: ZN = zsel_n(n1523, n1519, n1530);
    let n1534: ZB = zsel_b(n296, r_c400, n1039);
    let n1535: ZN = zsel_n(n296, r_c408, n1040);
    let n1536: ZN = zsel_n(n296, r_c409, n1041);
    let n1537: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1535);
    let n1538: ZN = zsel_n(n1523, n1535, n1537);
    let n1539: ZB = zsel_b(n296, r_c400, n1064);
    let n1540: ZN = zsel_n(n296, r_c408, n1065);
    let n1541: ZN = zsel_n(n296, r_c409, n1066);
    let n1542: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1540);
    let n1543: ZN = zsel_n(n1523, n1540, n1542);
    let n1544: ZB = zb_or(r_c312, n172);
    let n1545: ZN = zsel_n(n296, r_c304, n1072);
    let n1546: ZN = zsel_n(n296, r_c408, n1073);
    let n1547: ZN = zsel_n(n296, r_c409, n1074);
    let n1548: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1546);
    let n1549: ZN = zsel_n(n1523, n1546, n1548);
    let n1550: ZN = zsel_n(n296, r_c408, n1081);
    let n1551: ZN = zsel_n(n296, r_c409, n1082);
    let n1552: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1550);
    let n1553: ZN = zsel_n(n1523, n1550, n1552);
    let n1554: ZN = zsel_n(n296, r_c408, n1087);
    let n1555: ZN = zsel_n(n296, r_c409, n1088);
    let n1556: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1554);
    let n1557: ZN = zsel_n(n1523, n1554, n1556);
    let n1558: ZB = zb_or(r_c311, n172);
    let n1559: ZN = zsel_n(n1477, zn_splat(P8::from_raw(655360i32)), n292);
    let n1560: ZN = zsel_n(n1477, zn_splat(P8::from_raw(262144i32)), r_c301);
    let n1561: ZN = zsel_n(n1477, n1504, n1471);
    let n1562: ZN = zsel_n(n1477, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n1563: ZN = zsel_n(n1477, n988, r_c397);
    let n1564: ZN = zsel_n(n1477, n985, r_c398);
    let n1565: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), r_c399);
    let n1566: ZN = zsel_n(n1477, n981, n949);
    let n1567: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n964);
    let n1568: ZN = zsel_n(n293, n292, n1559);
    let n1569: ZN = zsel_n(n293, n294, n1560);
    let n1570: ZN = zsel_n(n293, n1471, n1561);
    let n1571: ZN = zsel_n(n293, r_c396, n1562);
    let n1572: ZN = zsel_n(n293, r_c397, n1563);
    let n1573: ZN = zsel_n(n293, r_c398, n1564);
    let n1574: ZN = zsel_n(n293, r_c399, n1565);
    let n1575: ZN = zsel_n(n293, n917, n1566);
    let n1576: ZN = zsel_n(n293, n923, n1567);
    let n1577: ZN = zsel_n(n296, n1499, n1480);
    let n1578: ZB = zsel_b(n296, r_c41, n1481);
    let n1579: ZN = zsel_n(n296, r_c299, n1568);
    let n1580: ZN = zsel_n(n296, r_c301, n1569);
    let n1581: ZN = zsel_n(n296, r_c302, n1570);
    let n1582: ZN = zsel_n(n296, r_c396, n1571);
    let n1583: ZN = zsel_n(n296, r_c397, n1572);
    let n1584: ZN = zsel_n(n296, r_c398, n1573);
    let n1585: ZN = zsel_n(n296, r_c399, n1574);
    let n1586: ZN = zsel_n(n296, r_c408, n1575);
    let n1587: ZN = zsel_n(n296, r_c409, n1576);
    let n1588: ZB = zn_gt(n1577, zn_splat(P8::from_raw(0i32)));
    let n1589: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1586);
    let n1590: ZN = zsel_n(n1588, n1514, n1529);
    let n1591: ZN = zsel_n(n1588, n1586, n1589);
    let n1592: ZN = zsel_n(n1477, zn_splat(P8::from_raw(69510i32)), r_c397);
    let n1593: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-131072i32)), r_c398);
    let n1594: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-327680i32)), n1022);
    let n1595: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1034);
    let n1596: ZN = zsel_n(n293, r_c397, n1592);
    let n1597: ZN = zsel_n(n293, r_c398, n1593);
    let n1598: ZN = zsel_n(n293, n917, n1594);
    let n1599: ZN = zsel_n(n293, n923, n1595);
    let n1600: ZN = zsel_n(n296, r_c397, n1596);
    let n1601: ZN = zsel_n(n296, r_c398, n1597);
    let n1602: ZN = zsel_n(n296, r_c408, n1598);
    let n1603: ZN = zsel_n(n296, r_c409, n1599);
    let n1604: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1602);
    let n1605: ZN = zsel_n(n1588, n1602, n1604);
    let n1606: ZN = zsel_n(n1477, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n1607: ZN = zsel_n(n1477, zn_splat(P8::from_raw(327680i32)), n1047);
    let n1608: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1059);
    let n1609: ZN = zsel_n(n293, r_c398, n1606);
    let n1610: ZN = zsel_n(n293, n917, n1607);
    let n1611: ZN = zsel_n(n293, n923, n1608);
    let n1612: ZN = zsel_n(n296, r_c398, n1609);
    let n1613: ZN = zsel_n(n296, r_c408, n1610);
    let n1614: ZN = zsel_n(n296, r_c409, n1611);
    let n1615: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1613);
    let n1616: ZN = zsel_n(n1588, n1613, n1615);
    let n1617: ZN = zsel_n(n1477, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n1618: ZN = zsel_n(n1477, zn_splat(P8::from_raw(98304i32)), r_c397);
    let n1619: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), r_c398);
    let n1620: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-98304i32)), r_c399);
    let n1621: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n949);
    let n1622: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-327680i32)), n964);
    let n1623: ZN = zsel_n(n293, r_c396, n1617);
    let n1624: ZN = zsel_n(n293, r_c397, n1618);
    let n1625: ZN = zsel_n(n293, r_c398, n1619);
    let n1626: ZN = zsel_n(n293, r_c399, n1620);
    let n1627: ZN = zsel_n(n293, n917, n1621);
    let n1628: ZN = zsel_n(n293, n923, n1622);
    let n1629: ZN = zsel_n(n296, r_c396, n1623);
    let n1630: ZN = zsel_n(n296, r_c397, n1624);
    let n1631: ZN = zsel_n(n296, r_c398, n1625);
    let n1632: ZN = zsel_n(n296, r_c399, n1626);
    let n1633: ZN = zsel_n(n296, r_c408, n1627);
    let n1634: ZN = zsel_n(n296, r_c409, n1628);
    let n1635: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1633);
    let n1636: ZN = zsel_n(n1588, n1633, n1635);
    let n1637: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1022);
    let n1638: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1034);
    let n1639: ZN = zsel_n(n293, n917, n1637);
    let n1640: ZN = zsel_n(n293, n923, n1638);
    let n1641: ZN = zsel_n(n296, r_c408, n1639);
    let n1642: ZN = zsel_n(n296, r_c409, n1640);
    let n1643: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1641);
    let n1644: ZN = zsel_n(n1588, n1641, n1643);
    let n1645: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1047);
    let n1646: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1059);
    let n1647: ZN = zsel_n(n293, n917, n1645);
    let n1648: ZN = zsel_n(n293, n923, n1646);
    let n1649: ZN = zsel_n(n296, r_c408, n1647);
    let n1650: ZN = zsel_n(n296, r_c409, n1648);
    let n1651: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1649);
    let n1652: ZN = zsel_n(n1588, n1649, n1651);
    let n1653: ZN = zsel_n(n1477, zn_splat(P8::from_raw(131072i32)), r_c399);
    let n1654: ZN = zsel_n(n1477, zn_splat(P8::from_raw(327680i32)), n964);
    let n1655: ZN = zsel_n(n293, r_c399, n1653);
    let n1656: ZN = zsel_n(n293, n923, n1654);
    let n1657: ZN = zsel_n(n296, r_c399, n1655);
    let n1658: ZN = zsel_n(n296, r_c409, n1656);
    let n1659: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1034);
    let n1660: ZN = zsel_n(n293, n923, n1659);
    let n1661: ZN = zsel_n(n296, r_c409, n1660);
    let n1662: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1059);
    let n1663: ZN = zsel_n(n293, n923, n1662);
    let n1664: ZN = zsel_n(n296, r_c409, n1663);
    let n1665: ZN = zsel_n(n1477, n981, n1070);
    let n1666: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1071);
    let n1667: ZN = zsel_n(n293, n917, n1665);
    let n1668: ZN = zsel_n(n293, n923, n1666);
    let n1669: ZN = zsel_n(n296, r_c408, n1667);
    let n1670: ZN = zsel_n(n296, r_c409, n1668);
    let n1671: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1669);
    let n1672: ZN = zsel_n(n1588, n1669, n1671);
    let n1673: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-327680i32)), n1079);
    let n1674: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1080);
    let n1675: ZN = zsel_n(n293, n917, n1673);
    let n1676: ZN = zsel_n(n293, n923, n1674);
    let n1677: ZN = zsel_n(n296, r_c408, n1675);
    let n1678: ZN = zsel_n(n296, r_c409, n1676);
    let n1679: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1677);
    let n1680: ZN = zsel_n(n1588, n1677, n1679);
    let n1681: ZN = zsel_n(n1477, zn_splat(P8::from_raw(327680i32)), n1085);
    let n1682: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1086);
    let n1683: ZN = zsel_n(n293, n917, n1681);
    let n1684: ZN = zsel_n(n293, n923, n1682);
    let n1685: ZN = zsel_n(n296, r_c408, n1683);
    let n1686: ZN = zsel_n(n296, r_c409, n1684);
    let n1687: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1685);
    let n1688: ZN = zsel_n(n1588, n1685, n1687);
    let n1689: ZN = zsel_n(n1477, zn_splat(P8::from_raw(0i32)), n1070);
    let n1690: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-327680i32)), n1071);
    let n1691: ZN = zsel_n(n293, n917, n1689);
    let n1692: ZN = zsel_n(n293, n923, n1690);
    let n1693: ZN = zsel_n(n296, r_c408, n1691);
    let n1694: ZN = zsel_n(n296, r_c409, n1692);
    let n1695: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1693);
    let n1696: ZN = zsel_n(n1588, n1693, n1695);
    let n1697: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1079);
    let n1698: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1080);
    let n1699: ZN = zsel_n(n293, n917, n1697);
    let n1700: ZN = zsel_n(n293, n923, n1698);
    let n1701: ZN = zsel_n(n296, r_c408, n1699);
    let n1702: ZN = zsel_n(n296, r_c409, n1700);
    let n1703: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1701);
    let n1704: ZN = zsel_n(n1588, n1701, n1703);
    let n1705: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1085);
    let n1706: ZN = zsel_n(n1477, zn_splat(P8::from_raw(-231700i32)), n1086);
    let n1707: ZN = zsel_n(n293, n917, n1705);
    let n1708: ZN = zsel_n(n293, n923, n1706);
    let n1709: ZN = zsel_n(n296, r_c408, n1707);
    let n1710: ZN = zsel_n(n296, r_c409, n1708);
    let n1711: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1709);
    let n1712: ZN = zsel_n(n1588, n1709, n1711);
    let n1713: ZN = zsel_n(n1477, zn_splat(P8::from_raw(327680i32)), n1071);
    let n1714: ZN = zsel_n(n293, n923, n1713);
    let n1715: ZN = zsel_n(n296, r_c409, n1714);
    let n1716: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1080);
    let n1717: ZN = zsel_n(n293, n923, n1716);
    let n1718: ZN = zsel_n(n296, r_c409, n1717);
    let n1719: ZN = zsel_n(n1477, zn_splat(P8::from_raw(231700i32)), n1086);
    let n1720: ZN = zsel_n(n293, n923, n1719);
    let n1721: ZN = zsel_n(n296, r_c409, n1720);
    let n1723: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n1724: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n1725: ZW = zw_add(zw_splat(0u64), n1723);
    let n1726: ZW = zw_add(zw_splat(0u64), n1724);
    let n1727: ZW = zw_cellmix_n(84u64, n150, 1542469173u64);
    let n1728: ZW = zw_cellmix_n(84u64, n150, 668265263u64);
    let n1729: ZW = zw_add(n1725, n1727);
    let n1730: ZW = zw_add(n1726, n1728);
    let n1731: ZW = zw_cellmix_n(85u64, n267, 1542469173u64);
    let n1732: ZW = zw_cellmix_n(85u64, n267, 668265263u64);
    let n1733: ZW = zw_add(n1729, n1731);
    let n1734: ZW = zw_add(n1730, n1732);
    let n1735: ZW = zw_cellmix_n(86u64, n266, 1542469173u64);
    let n1736: ZW = zw_cellmix_n(86u64, n266, 668265263u64);
    let n1737: ZW = zw_add(n1733, n1735);
    let n1738: ZW = zw_add(n1734, n1736);
    let n1739: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n1740: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n1741: ZW = zw_add(n1737, n1739);
    let n1742: ZW = zw_add(n1738, n1740);
    let n1743: ZW = zw_cellmix_n(241u64, n309, 1542469173u64);
    let n1744: ZW = zw_cellmix_n(241u64, n309, 668265263u64);
    let n1745: ZW = zw_add(n1741, n1743);
    let n1746: ZW = zw_add(n1742, n1744);
    let n1747: ZW = zw_cellmix_n(254u64, n310, 1542469173u64);
    let n1748: ZW = zw_cellmix_n(254u64, n310, 668265263u64);
    let n1749: ZW = zw_add(n1745, n1747);
    let n1750: ZW = zw_add(n1746, n1748);
    let n1751: ZW = zw_cellmix_n(261u64, n337, 1542469173u64);
    let n1752: ZW = zw_cellmix_n(261u64, n337, 668265263u64);
    let n1753: ZW = zw_add(n1749, n1751);
    let n1754: ZW = zw_add(n1750, n1752);
    let n1755: ZW = zw_cellmix_n(274u64, n338, 1542469173u64);
    let n1756: ZW = zw_cellmix_n(274u64, n338, 668265263u64);
    let n1757: ZW = zw_add(n1753, n1755);
    let n1758: ZW = zw_add(n1754, n1756);
    let n1759: ZW = zw_cellmix_n(302u64, n572, 1542469173u64);
    let n1760: ZW = zw_cellmix_n(302u64, n572, 668265263u64);
    let n1761: ZW = zw_add(n1757, n1759);
    let n1762: ZW = zw_add(n1758, n1760);
    let n1763: ZW = zw_cellmix_n(368u64, n573, 1542469173u64);
    let n1764: ZW = zw_cellmix_n(368u64, n573, 668265263u64);
    let n1765: ZW = zw_add(n1761, n1763);
    let n1766: ZW = zw_add(n1762, n1764);
    let n1767: ZW = zw_cellmix_n(369u64, n574, 1542469173u64);
    let n1768: ZW = zw_cellmix_n(369u64, n574, 668265263u64);
    let n1769: ZW = zw_add(n1765, n1767);
    let n1770: ZW = zw_add(n1766, n1768);
    let n1771: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n1772: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n1773: ZW = zw_add(n1769, n1771);
    let n1774: ZW = zw_add(n1770, n1772);
    let n1775: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n1776: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n1777: ZW = zw_add(n1773, n1775);
    let n1778: ZW = zw_add(n1774, n1776);
    let n1779: ZW = zw_cellmix_n(282u64, n292, 1542469173u64);
    let n1780: ZW = zw_cellmix_n(282u64, n292, 668265263u64);
    let n1781: ZW = zw_add(n1777, n1779);
    let n1782: ZW = zw_add(n1778, n1780);
    let n1783: ZW = zw_cellmix_n(284u64, n295, 1542469173u64);
    let n1784: ZW = zw_cellmix_n(284u64, n295, 668265263u64);
    let n1785: ZW = zw_add(n1781, n1783);
    let n1786: ZW = zw_add(n1782, n1784);
    let n1787: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n1788: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n1789: ZW = zw_add(n1785, n1787);
    let n1790: ZW = zw_add(n1786, n1788);
    let n1791: ZW = zw_cellmix_n(287u64, n911, 1542469173u64);
    let n1792: ZW = zw_cellmix_n(287u64, n911, 668265263u64);
    let n1793: ZW = zw_add(n1789, n1791);
    let n1794: ZW = zw_add(n1790, n1792);
    let n1795: ZW = zw_cellmix_b(294u64, zb_splat(false), 1542469173u64);
    let n1796: ZW = zw_cellmix_b(294u64, zb_splat(false), 668265263u64);
    let n1797: ZW = zw_add(n1793, n1795);
    let n1798: ZW = zw_add(n1794, n1796);
    let n1799: ZW = zw_cellmix_b(295u64, zb_splat(false), 1542469173u64);
    let n1800: ZW = zw_cellmix_b(295u64, zb_splat(false), 668265263u64);
    let n1801: ZW = zw_add(n1797, n1799);
    let n1802: ZW = zw_add(n1798, n1800);
    let n1803: ZW = zw_cellmix_n(301u64, n1005, 1542469173u64);
    let n1804: ZW = zw_cellmix_n(301u64, n1005, 668265263u64);
    let n1805: ZW = zw_add(n1801, n1803);
    let n1806: ZW = zw_add(n1802, n1804);
    let n1807: ZW = zw_cellmix_n(358u64, r_c396, 1542469173u64);
    let n1808: ZW = zw_cellmix_n(358u64, r_c396, 668265263u64);
    let n1809: ZW = zw_add(n1805, n1807);
    let n1810: ZW = zw_add(n1806, n1808);
    let n1811: ZW = zw_cellmix_n(359u64, r_c397, 1542469173u64);
    let n1812: ZW = zw_cellmix_n(359u64, r_c397, 668265263u64);
    let n1813: ZW = zw_add(n1809, n1811);
    let n1814: ZW = zw_add(n1810, n1812);
    let n1815: ZW = zw_cellmix_n(360u64, r_c398, 1542469173u64);
    let n1816: ZW = zw_cellmix_n(360u64, r_c398, 668265263u64);
    let n1817: ZW = zw_add(n1813, n1815);
    let n1818: ZW = zw_add(n1814, n1816);
    let n1819: ZW = zw_cellmix_n(361u64, r_c399, 1542469173u64);
    let n1820: ZW = zw_cellmix_n(361u64, r_c399, 668265263u64);
    let n1821: ZW = zw_add(n1817, n1819);
    let n1822: ZW = zw_add(n1818, n1820);
    let n1823: ZW = zw_cellmix_b(362u64, n989, 1542469173u64);
    let n1824: ZW = zw_cellmix_b(362u64, n989, 668265263u64);
    let n1825: ZW = zw_add(n1821, n1823);
    let n1826: ZW = zw_add(n1822, n1824);
    let n1827: ZW = zw_cellmix_n(370u64, n1006, 1542469173u64);
    let n1828: ZW = zw_cellmix_n(370u64, n1006, 668265263u64);
    let n1829: ZW = zw_add(n1825, n1827);
    let n1830: ZW = zw_add(n1826, n1828);
    let n1831: ZW = zw_cellmix_n(371u64, n991, 1542469173u64);
    let n1832: ZW = zw_cellmix_n(371u64, n991, 668265263u64);
    let n1833: ZW = zw_add(n1829, n1831);
    let n1834: ZW = zw_add(n1830, n1832);
    let n1835: ZW = zw_cellmix_b(362u64, n1039, 1542469173u64);
    let n1836: ZW = zw_cellmix_b(362u64, n1039, 668265263u64);
    let n1837: ZW = zw_add(n1821, n1835);
    let n1838: ZW = zw_add(n1822, n1836);
    let n1839: ZW = zw_cellmix_n(370u64, n1043, 1542469173u64);
    let n1840: ZW = zw_cellmix_n(370u64, n1043, 668265263u64);
    let n1841: ZW = zw_add(n1837, n1839);
    let n1842: ZW = zw_add(n1838, n1840);
    let n1843: ZW = zw_cellmix_n(371u64, n1041, 1542469173u64);
    let n1844: ZW = zw_cellmix_n(371u64, n1041, 668265263u64);
    let n1845: ZW = zw_add(n1841, n1843);
    let n1846: ZW = zw_add(n1842, n1844);
    let n1847: ZW = zw_cellmix_b(362u64, n1064, 1542469173u64);
    let n1848: ZW = zw_cellmix_b(362u64, n1064, 668265263u64);
    let n1849: ZW = zw_add(n1821, n1847);
    let n1850: ZW = zw_add(n1822, n1848);
    let n1851: ZW = zw_cellmix_n(370u64, n1068, 1542469173u64);
    let n1852: ZW = zw_cellmix_n(370u64, n1068, 668265263u64);
    let n1853: ZW = zw_add(n1849, n1851);
    let n1854: ZW = zw_add(n1850, n1852);
    let n1855: ZW = zw_cellmix_n(371u64, n1066, 1542469173u64);
    let n1856: ZW = zw_cellmix_n(371u64, n1066, 668265263u64);
    let n1857: ZW = zw_add(n1853, n1855);
    let n1858: ZW = zw_add(n1854, n1856);
    let n1859: ZW = zw_cellmix_n(287u64, n1072, 1542469173u64);
    let n1860: ZW = zw_cellmix_n(287u64, n1072, 668265263u64);
    let n1861: ZW = zw_add(n1789, n1859);
    let n1862: ZW = zw_add(n1790, n1860);
    let n1863: ZW = zw_add(n1861, n1795);
    let n1864: ZW = zw_add(n1862, n1796);
    let n1865: ZW = zw_cellmix_b(295u64, zb_splat(true), 1542469173u64);
    let n1866: ZW = zw_cellmix_b(295u64, zb_splat(true), 668265263u64);
    let n1867: ZW = zw_add(n1863, n1865);
    let n1868: ZW = zw_add(n1864, n1866);
    let n1869: ZW = zw_add(n1867, n1803);
    let n1870: ZW = zw_add(n1868, n1804);
    let n1871: ZW = zw_add(n1869, n1807);
    let n1872: ZW = zw_add(n1870, n1808);
    let n1873: ZW = zw_add(n1871, n1811);
    let n1874: ZW = zw_add(n1872, n1812);
    let n1875: ZW = zw_add(n1873, n1815);
    let n1876: ZW = zw_add(n1874, n1816);
    let n1877: ZW = zw_add(n1875, n1819);
    let n1878: ZW = zw_add(n1876, n1820);
    let n1879: ZW = zw_add(n1877, n1823);
    let n1880: ZW = zw_add(n1878, n1824);
    let n1881: ZW = zw_cellmix_n(370u64, n1078, 1542469173u64);
    let n1882: ZW = zw_cellmix_n(370u64, n1078, 668265263u64);
    let n1883: ZW = zw_add(n1879, n1881);
    let n1884: ZW = zw_add(n1880, n1882);
    let n1885: ZW = zw_cellmix_n(371u64, n1074, 1542469173u64);
    let n1886: ZW = zw_cellmix_n(371u64, n1074, 668265263u64);
    let n1887: ZW = zw_add(n1883, n1885);
    let n1888: ZW = zw_add(n1884, n1886);
    let n1889: ZW = zw_add(n1877, n1835);
    let n1890: ZW = zw_add(n1878, n1836);
    let n1891: ZW = zw_cellmix_n(370u64, n1084, 1542469173u64);
    let n1892: ZW = zw_cellmix_n(370u64, n1084, 668265263u64);
    let n1893: ZW = zw_add(n1889, n1891);
    let n1894: ZW = zw_add(n1890, n1892);
    let n1895: ZW = zw_cellmix_n(371u64, n1082, 1542469173u64);
    let n1896: ZW = zw_cellmix_n(371u64, n1082, 668265263u64);
    let n1897: ZW = zw_add(n1893, n1895);
    let n1898: ZW = zw_add(n1894, n1896);
    let n1899: ZW = zw_add(n1877, n1847);
    let n1900: ZW = zw_add(n1878, n1848);
    let n1901: ZW = zw_cellmix_n(370u64, n1090, 1542469173u64);
    let n1902: ZW = zw_cellmix_n(370u64, n1090, 668265263u64);
    let n1903: ZW = zw_add(n1899, n1901);
    let n1904: ZW = zw_add(n1900, n1902);
    let n1905: ZW = zw_cellmix_n(371u64, n1088, 1542469173u64);
    let n1906: ZW = zw_cellmix_n(371u64, n1088, 668265263u64);
    let n1907: ZW = zw_add(n1903, n1905);
    let n1908: ZW = zw_add(n1904, n1906);
    let n1909: ZW = zw_cellmix_n(20u64, n1098, 1542469173u64);
    let n1910: ZW = zw_cellmix_n(20u64, n1098, 668265263u64);
    let n1911: ZW = zw_add(n1769, n1909);
    let n1912: ZW = zw_add(n1770, n1910);
    let n1913: ZW = zw_cellmix_b(41u64, n1099, 1542469173u64);
    let n1914: ZW = zw_cellmix_b(41u64, n1099, 668265263u64);
    let n1915: ZW = zw_add(n1911, n1913);
    let n1916: ZW = zw_add(n1912, n1914);
    let n1917: ZW = zw_cellmix_n(282u64, n1100, 1542469173u64);
    let n1918: ZW = zw_cellmix_n(282u64, n1100, 668265263u64);
    let n1919: ZW = zw_add(n1915, n1917);
    let n1920: ZW = zw_add(n1916, n1918);
    let n1921: ZW = zw_cellmix_n(284u64, n1101, 1542469173u64);
    let n1922: ZW = zw_cellmix_n(284u64, n1101, 668265263u64);
    let n1923: ZW = zw_add(n1919, n1921);
    let n1924: ZW = zw_add(n1920, n1922);
    let n1925: ZW = zw_cellmix_n(285u64, n1102, 1542469173u64);
    let n1926: ZW = zw_cellmix_n(285u64, n1102, 668265263u64);
    let n1927: ZW = zw_add(n1923, n1925);
    let n1928: ZW = zw_add(n1924, n1926);
    let n1929: ZW = zw_add(n1927, n1791);
    let n1930: ZW = zw_add(n1928, n1792);
    let n1931: ZW = zw_cellmix_b(294u64, zb_splat(true), 1542469173u64);
    let n1932: ZW = zw_cellmix_b(294u64, zb_splat(true), 668265263u64);
    let n1933: ZW = zw_add(n1929, n1931);
    let n1934: ZW = zw_add(n1930, n1932);
    let n1935: ZW = zw_add(n1933, n1799);
    let n1936: ZW = zw_add(n1934, n1800);
    let n1937: ZW = zw_cellmix_n(301u64, n1115, 1542469173u64);
    let n1938: ZW = zw_cellmix_n(301u64, n1115, 668265263u64);
    let n1939: ZW = zw_add(n1935, n1937);
    let n1940: ZW = zw_add(n1936, n1938);
    let n1941: ZW = zw_cellmix_n(358u64, n1103, 1542469173u64);
    let n1942: ZW = zw_cellmix_n(358u64, n1103, 668265263u64);
    let n1943: ZW = zw_add(n1939, n1941);
    let n1944: ZW = zw_add(n1940, n1942);
    let n1945: ZW = zw_cellmix_n(359u64, n1110, 1542469173u64);
    let n1946: ZW = zw_cellmix_n(359u64, n1110, 668265263u64);
    let n1947: ZW = zw_add(n1943, n1945);
    let n1948: ZW = zw_add(n1944, n1946);
    let n1949: ZW = zw_cellmix_n(360u64, n1111, 1542469173u64);
    let n1950: ZW = zw_cellmix_n(360u64, n1111, 668265263u64);
    let n1951: ZW = zw_add(n1947, n1949);
    let n1952: ZW = zw_add(n1948, n1950);
    let n1953: ZW = zw_cellmix_n(361u64, n1104, 1542469173u64);
    let n1954: ZW = zw_cellmix_n(361u64, n1104, 668265263u64);
    let n1955: ZW = zw_add(n1951, n1953);
    let n1956: ZW = zw_add(n1952, n1954);
    let n1957: ZW = zw_add(n1955, n1823);
    let n1958: ZW = zw_add(n1956, n1824);
    let n1959: ZW = zw_cellmix_n(370u64, n1116, 1542469173u64);
    let n1960: ZW = zw_cellmix_n(370u64, n1116, 668265263u64);
    let n1961: ZW = zw_add(n1957, n1959);
    let n1962: ZW = zw_add(n1958, n1960);
    let n1963: ZW = zw_cellmix_n(371u64, n1113, 1542469173u64);
    let n1964: ZW = zw_cellmix_n(371u64, n1113, 668265263u64);
    let n1965: ZW = zw_add(n1961, n1963);
    let n1966: ZW = zw_add(n1962, n1964);
    let n1967: ZW = zw_cellmix_n(359u64, n1121, 1542469173u64);
    let n1968: ZW = zw_cellmix_n(359u64, n1121, 668265263u64);
    let n1969: ZW = zw_add(n1943, n1967);
    let n1970: ZW = zw_add(n1944, n1968);
    let n1971: ZW = zw_cellmix_n(360u64, n1122, 1542469173u64);
    let n1972: ZW = zw_cellmix_n(360u64, n1122, 668265263u64);
    let n1973: ZW = zw_add(n1969, n1971);
    let n1974: ZW = zw_add(n1970, n1972);
    let n1975: ZW = zw_add(n1973, n1953);
    let n1976: ZW = zw_add(n1974, n1954);
    let n1977: ZW = zw_add(n1975, n1835);
    let n1978: ZW = zw_add(n1976, n1836);
    let n1979: ZW = zw_cellmix_n(370u64, n1126, 1542469173u64);
    let n1980: ZW = zw_cellmix_n(370u64, n1126, 668265263u64);
    let n1981: ZW = zw_add(n1977, n1979);
    let n1982: ZW = zw_add(n1978, n1980);
    let n1983: ZW = zw_cellmix_n(371u64, n1124, 1542469173u64);
    let n1984: ZW = zw_cellmix_n(371u64, n1124, 668265263u64);
    let n1985: ZW = zw_add(n1981, n1983);
    let n1986: ZW = zw_add(n1982, n1984);
    let n1987: ZW = zw_cellmix_n(360u64, n1130, 1542469173u64);
    let n1988: ZW = zw_cellmix_n(360u64, n1130, 668265263u64);
    let n1989: ZW = zw_add(n1969, n1987);
    let n1990: ZW = zw_add(n1970, n1988);
    let n1991: ZW = zw_add(n1989, n1953);
    let n1992: ZW = zw_add(n1990, n1954);
    let n1993: ZW = zw_add(n1991, n1847);
    let n1994: ZW = zw_add(n1992, n1848);
    let n1995: ZW = zw_cellmix_n(370u64, n1134, 1542469173u64);
    let n1996: ZW = zw_cellmix_n(370u64, n1134, 668265263u64);
    let n1997: ZW = zw_add(n1993, n1995);
    let n1998: ZW = zw_add(n1994, n1996);
    let n1999: ZW = zw_cellmix_n(371u64, n1132, 1542469173u64);
    let n2000: ZW = zw_cellmix_n(371u64, n1132, 668265263u64);
    let n2001: ZW = zw_add(n1997, n1999);
    let n2002: ZW = zw_add(n1998, n2000);
    let n2003: ZW = zw_cellmix_n(358u64, n1138, 1542469173u64);
    let n2004: ZW = zw_cellmix_n(358u64, n1138, 668265263u64);
    let n2005: ZW = zw_add(n1939, n2003);
    let n2006: ZW = zw_add(n1940, n2004);
    let n2007: ZW = zw_cellmix_n(359u64, n1144, 1542469173u64);
    let n2008: ZW = zw_cellmix_n(359u64, n1144, 668265263u64);
    let n2009: ZW = zw_add(n2005, n2007);
    let n2010: ZW = zw_add(n2006, n2008);
    let n2011: ZW = zw_cellmix_n(360u64, n1145, 1542469173u64);
    let n2012: ZW = zw_cellmix_n(360u64, n1145, 668265263u64);
    let n2013: ZW = zw_add(n2009, n2011);
    let n2014: ZW = zw_add(n2010, n2012);
    let n2015: ZW = zw_cellmix_n(361u64, n1139, 1542469173u64);
    let n2016: ZW = zw_cellmix_n(361u64, n1139, 668265263u64);
    let n2017: ZW = zw_add(n2013, n2015);
    let n2018: ZW = zw_add(n2014, n2016);
    let n2019: ZW = zw_add(n2017, n1823);
    let n2020: ZW = zw_add(n2018, n1824);
    let n2021: ZW = zw_cellmix_n(370u64, n1149, 1542469173u64);
    let n2022: ZW = zw_cellmix_n(370u64, n1149, 668265263u64);
    let n2023: ZW = zw_add(n2019, n2021);
    let n2024: ZW = zw_add(n2020, n2022);
    let n2025: ZW = zw_cellmix_n(371u64, n1147, 1542469173u64);
    let n2026: ZW = zw_cellmix_n(371u64, n1147, 668265263u64);
    let n2027: ZW = zw_add(n2023, n2025);
    let n2028: ZW = zw_add(n2024, n2026);
    let n2029: ZW = zw_add(n2005, n1967);
    let n2030: ZW = zw_add(n2006, n1968);
    let n2031: ZW = zw_add(n2029, n1971);
    let n2032: ZW = zw_add(n2030, n1972);
    let n2033: ZW = zw_add(n2031, n2015);
    let n2034: ZW = zw_add(n2032, n2016);
    let n2035: ZW = zw_add(n2033, n1835);
    let n2036: ZW = zw_add(n2034, n1836);
    let n2037: ZW = zw_cellmix_n(370u64, n1155, 1542469173u64);
    let n2038: ZW = zw_cellmix_n(370u64, n1155, 668265263u64);
    let n2039: ZW = zw_add(n2035, n2037);
    let n2040: ZW = zw_add(n2036, n2038);
    let n2041: ZW = zw_cellmix_n(371u64, n1153, 1542469173u64);
    let n2042: ZW = zw_cellmix_n(371u64, n1153, 668265263u64);
    let n2043: ZW = zw_add(n2039, n2041);
    let n2044: ZW = zw_add(n2040, n2042);
    let n2045: ZW = zw_add(n2029, n1987);
    let n2046: ZW = zw_add(n2030, n1988);
    let n2047: ZW = zw_add(n2045, n2015);
    let n2048: ZW = zw_add(n2046, n2016);
    let n2049: ZW = zw_add(n2047, n1847);
    let n2050: ZW = zw_add(n2048, n1848);
    let n2051: ZW = zw_cellmix_n(370u64, n1161, 1542469173u64);
    let n2052: ZW = zw_cellmix_n(370u64, n1161, 668265263u64);
    let n2053: ZW = zw_add(n2049, n2051);
    let n2054: ZW = zw_add(n2050, n2052);
    let n2055: ZW = zw_cellmix_n(371u64, n1159, 1542469173u64);
    let n2056: ZW = zw_cellmix_n(371u64, n1159, 668265263u64);
    let n2057: ZW = zw_add(n2053, n2055);
    let n2058: ZW = zw_add(n2054, n2056);
    let n2059: ZW = zw_cellmix_n(361u64, n1163, 1542469173u64);
    let n2060: ZW = zw_cellmix_n(361u64, n1163, 668265263u64);
    let n2061: ZW = zw_add(n2013, n2059);
    let n2062: ZW = zw_add(n2014, n2060);
    let n2063: ZW = zw_add(n2061, n1823);
    let n2064: ZW = zw_add(n2062, n1824);
    let n2065: ZW = zw_add(n2063, n2021);
    let n2066: ZW = zw_add(n2064, n2022);
    let n2067: ZW = zw_cellmix_n(371u64, n1165, 1542469173u64);
    let n2068: ZW = zw_cellmix_n(371u64, n1165, 668265263u64);
    let n2069: ZW = zw_add(n2065, n2067);
    let n2070: ZW = zw_add(n2066, n2068);
    let n2071: ZW = zw_add(n2031, n2059);
    let n2072: ZW = zw_add(n2032, n2060);
    let n2073: ZW = zw_add(n2071, n1835);
    let n2074: ZW = zw_add(n2072, n1836);
    let n2075: ZW = zw_add(n2073, n2037);
    let n2076: ZW = zw_add(n2074, n2038);
    let n2077: ZW = zw_cellmix_n(371u64, n1167, 1542469173u64);
    let n2078: ZW = zw_cellmix_n(371u64, n1167, 668265263u64);
    let n2079: ZW = zw_add(n2075, n2077);
    let n2080: ZW = zw_add(n2076, n2078);
    let n2081: ZW = zw_add(n2045, n2059);
    let n2082: ZW = zw_add(n2046, n2060);
    let n2083: ZW = zw_add(n2081, n1847);
    let n2084: ZW = zw_add(n2082, n1848);
    let n2085: ZW = zw_add(n2083, n2051);
    let n2086: ZW = zw_add(n2084, n2052);
    let n2087: ZW = zw_cellmix_n(371u64, n1169, 1542469173u64);
    let n2088: ZW = zw_cellmix_n(371u64, n1169, 668265263u64);
    let n2089: ZW = zw_add(n2085, n2087);
    let n2090: ZW = zw_add(n2086, n2088);
    let n2091: ZW = zw_add(n1927, n1859);
    let n2092: ZW = zw_add(n1928, n1860);
    let n2093: ZW = zw_add(n2091, n1931);
    let n2094: ZW = zw_add(n2092, n1932);
    let n2095: ZW = zw_add(n2093, n1865);
    let n2096: ZW = zw_add(n2094, n1866);
    let n2097: ZW = zw_add(n2095, n1937);
    let n2098: ZW = zw_add(n2096, n1938);
    let n2099: ZW = zw_add(n2097, n1941);
    let n2100: ZW = zw_add(n2098, n1942);
    let n2101: ZW = zw_add(n2099, n1945);
    let n2102: ZW = zw_add(n2100, n1946);
    let n2103: ZW = zw_add(n2101, n1949);
    let n2104: ZW = zw_add(n2102, n1950);
    let n2105: ZW = zw_add(n2103, n1953);
    let n2106: ZW = zw_add(n2104, n1954);
    let n2107: ZW = zw_add(n2105, n1823);
    let n2108: ZW = zw_add(n2106, n1824);
    let n2109: ZW = zw_cellmix_n(370u64, n1175, 1542469173u64);
    let n2110: ZW = zw_cellmix_n(370u64, n1175, 668265263u64);
    let n2111: ZW = zw_add(n2107, n2109);
    let n2112: ZW = zw_add(n2108, n2110);
    let n2113: ZW = zw_cellmix_n(371u64, n1173, 1542469173u64);
    let n2114: ZW = zw_cellmix_n(371u64, n1173, 668265263u64);
    let n2115: ZW = zw_add(n2111, n2113);
    let n2116: ZW = zw_add(n2112, n2114);
    let n2117: ZW = zw_add(n2099, n1967);
    let n2118: ZW = zw_add(n2100, n1968);
    let n2119: ZW = zw_add(n2117, n1971);
    let n2120: ZW = zw_add(n2118, n1972);
    let n2121: ZW = zw_add(n2119, n1953);
    let n2122: ZW = zw_add(n2120, n1954);
    let n2123: ZW = zw_add(n2121, n1835);
    let n2124: ZW = zw_add(n2122, n1836);
    let n2125: ZW = zw_cellmix_n(370u64, n1181, 1542469173u64);
    let n2126: ZW = zw_cellmix_n(370u64, n1181, 668265263u64);
    let n2127: ZW = zw_add(n2123, n2125);
    let n2128: ZW = zw_add(n2124, n2126);
    let n2129: ZW = zw_cellmix_n(371u64, n1179, 1542469173u64);
    let n2130: ZW = zw_cellmix_n(371u64, n1179, 668265263u64);
    let n2131: ZW = zw_add(n2127, n2129);
    let n2132: ZW = zw_add(n2128, n2130);
    let n2133: ZW = zw_add(n2117, n1987);
    let n2134: ZW = zw_add(n2118, n1988);
    let n2135: ZW = zw_add(n2133, n1953);
    let n2136: ZW = zw_add(n2134, n1954);
    let n2137: ZW = zw_add(n2135, n1847);
    let n2138: ZW = zw_add(n2136, n1848);
    let n2139: ZW = zw_cellmix_n(370u64, n1187, 1542469173u64);
    let n2140: ZW = zw_cellmix_n(370u64, n1187, 668265263u64);
    let n2141: ZW = zw_add(n2137, n2139);
    let n2142: ZW = zw_add(n2138, n2140);
    let n2143: ZW = zw_cellmix_n(371u64, n1185, 1542469173u64);
    let n2144: ZW = zw_cellmix_n(371u64, n1185, 668265263u64);
    let n2145: ZW = zw_add(n2141, n2143);
    let n2146: ZW = zw_add(n2142, n2144);
    let n2147: ZW = zw_add(n2097, n2003);
    let n2148: ZW = zw_add(n2098, n2004);
    let n2149: ZW = zw_add(n2147, n2007);
    let n2150: ZW = zw_add(n2148, n2008);
    let n2151: ZW = zw_add(n2149, n2011);
    let n2152: ZW = zw_add(n2150, n2012);
    let n2153: ZW = zw_add(n2151, n2015);
    let n2154: ZW = zw_add(n2152, n2016);
    let n2155: ZW = zw_add(n2153, n1823);
    let n2156: ZW = zw_add(n2154, n1824);
    let n2157: ZW = zw_cellmix_n(370u64, n1193, 1542469173u64);
    let n2158: ZW = zw_cellmix_n(370u64, n1193, 668265263u64);
    let n2159: ZW = zw_add(n2155, n2157);
    let n2160: ZW = zw_add(n2156, n2158);
    let n2161: ZW = zw_cellmix_n(371u64, n1191, 1542469173u64);
    let n2162: ZW = zw_cellmix_n(371u64, n1191, 668265263u64);
    let n2163: ZW = zw_add(n2159, n2161);
    let n2164: ZW = zw_add(n2160, n2162);
    let n2165: ZW = zw_add(n2147, n1967);
    let n2166: ZW = zw_add(n2148, n1968);
    let n2167: ZW = zw_add(n2165, n1971);
    let n2168: ZW = zw_add(n2166, n1972);
    let n2169: ZW = zw_add(n2167, n2015);
    let n2170: ZW = zw_add(n2168, n2016);
    let n2171: ZW = zw_add(n2169, n1835);
    let n2172: ZW = zw_add(n2170, n1836);
    let n2173: ZW = zw_cellmix_n(370u64, n1199, 1542469173u64);
    let n2174: ZW = zw_cellmix_n(370u64, n1199, 668265263u64);
    let n2175: ZW = zw_add(n2171, n2173);
    let n2176: ZW = zw_add(n2172, n2174);
    let n2177: ZW = zw_cellmix_n(371u64, n1197, 1542469173u64);
    let n2178: ZW = zw_cellmix_n(371u64, n1197, 668265263u64);
    let n2179: ZW = zw_add(n2175, n2177);
    let n2180: ZW = zw_add(n2176, n2178);
    let n2181: ZW = zw_add(n2165, n1987);
    let n2182: ZW = zw_add(n2166, n1988);
    let n2183: ZW = zw_add(n2181, n2015);
    let n2184: ZW = zw_add(n2182, n2016);
    let n2185: ZW = zw_add(n2183, n1847);
    let n2186: ZW = zw_add(n2184, n1848);
    let n2187: ZW = zw_cellmix_n(370u64, n1205, 1542469173u64);
    let n2188: ZW = zw_cellmix_n(370u64, n1205, 668265263u64);
    let n2189: ZW = zw_add(n2185, n2187);
    let n2190: ZW = zw_add(n2186, n2188);
    let n2191: ZW = zw_cellmix_n(371u64, n1203, 1542469173u64);
    let n2192: ZW = zw_cellmix_n(371u64, n1203, 668265263u64);
    let n2193: ZW = zw_add(n2189, n2191);
    let n2194: ZW = zw_add(n2190, n2192);
    let n2195: ZW = zw_add(n2151, n2059);
    let n2196: ZW = zw_add(n2152, n2060);
    let n2197: ZW = zw_add(n2195, n1823);
    let n2198: ZW = zw_add(n2196, n1824);
    let n2199: ZW = zw_add(n2197, n2157);
    let n2200: ZW = zw_add(n2198, n2158);
    let n2201: ZW = zw_cellmix_n(371u64, n1207, 1542469173u64);
    let n2202: ZW = zw_cellmix_n(371u64, n1207, 668265263u64);
    let n2203: ZW = zw_add(n2199, n2201);
    let n2204: ZW = zw_add(n2200, n2202);
    let n2205: ZW = zw_add(n2167, n2059);
    let n2206: ZW = zw_add(n2168, n2060);
    let n2207: ZW = zw_add(n2205, n1835);
    let n2208: ZW = zw_add(n2206, n1836);
    let n2209: ZW = zw_add(n2207, n2173);
    let n2210: ZW = zw_add(n2208, n2174);
    let n2211: ZW = zw_cellmix_n(371u64, n1209, 1542469173u64);
    let n2212: ZW = zw_cellmix_n(371u64, n1209, 668265263u64);
    let n2213: ZW = zw_add(n2209, n2211);
    let n2214: ZW = zw_add(n2210, n2212);
    let n2215: ZW = zw_add(n2181, n2059);
    let n2216: ZW = zw_add(n2182, n2060);
    let n2217: ZW = zw_add(n2215, n1847);
    let n2218: ZW = zw_add(n2216, n1848);
    let n2219: ZW = zw_add(n2217, n2187);
    let n2220: ZW = zw_add(n2218, n2188);
    let n2221: ZW = zw_cellmix_n(371u64, n1211, 1542469173u64);
    let n2222: ZW = zw_cellmix_n(371u64, n1211, 668265263u64);
    let n2223: ZW = zw_add(n2219, n2221);
    let n2224: ZW = zw_add(n2220, n2222);
    let n2225: ZW = zw_add(zw_splat(0u64), n1727);
    let n2226: ZW = zw_add(zw_splat(0u64), n1728);
    let n2227: ZW = zw_add(n2225, n1731);
    let n2228: ZW = zw_add(n2226, n1732);
    let n2229: ZW = zw_add(n2227, n1735);
    let n2230: ZW = zw_add(n2228, n1736);
    let n2231: ZW = zw_cellmix_n(87u64, n1294, 1542469173u64);
    let n2232: ZW = zw_cellmix_n(87u64, n1294, 668265263u64);
    let n2233: ZW = zw_add(n2229, n2231);
    let n2234: ZW = zw_add(n2230, n2232);
    let n2235: ZW = zw_cellmix_n(240u64, n309, 1542469173u64);
    let n2236: ZW = zw_cellmix_n(240u64, n309, 668265263u64);
    let n2237: ZW = zw_add(n2233, n2235);
    let n2238: ZW = zw_add(n2234, n2236);
    let n2239: ZW = zw_cellmix_n(253u64, n310, 1542469173u64);
    let n2240: ZW = zw_cellmix_n(253u64, n310, 668265263u64);
    let n2241: ZW = zw_add(n2237, n2239);
    let n2242: ZW = zw_add(n2238, n2240);
    let n2243: ZW = zw_cellmix_n(260u64, n337, 1542469173u64);
    let n2244: ZW = zw_cellmix_n(260u64, n337, 668265263u64);
    let n2245: ZW = zw_add(n2241, n2243);
    let n2246: ZW = zw_add(n2242, n2244);
    let n2247: ZW = zw_cellmix_n(273u64, n338, 1542469173u64);
    let n2248: ZW = zw_cellmix_n(273u64, n338, 668265263u64);
    let n2249: ZW = zw_add(n2245, n2247);
    let n2250: ZW = zw_add(n2246, n2248);
    let n2251: ZW = zw_add(n2249, n1771);
    let n2252: ZW = zw_add(n2250, n1772);
    let n2253: ZW = zw_add(n2251, n1775);
    let n2254: ZW = zw_add(n2252, n1776);
    let n2255: ZW = zw_add(n2249, n1909);
    let n2256: ZW = zw_add(n2250, n1910);
    let n2257: ZW = zw_add(n2255, n1913);
    let n2258: ZW = zw_add(n2256, n1914);
    let n2259: ZW = zw_cellmix_n(87u64, n1475, 1542469173u64);
    let n2260: ZW = zw_cellmix_n(87u64, n1475, 668265263u64);
    let n2261: ZW = zw_add(n2229, n2259);
    let n2262: ZW = zw_add(n2230, n2260);
    let n2263: ZW = zw_cellmix_n(245u64, n1298, 1542469173u64);
    let n2264: ZW = zw_cellmix_n(245u64, n1298, 668265263u64);
    let n2265: ZW = zw_add(n2261, n2263);
    let n2266: ZW = zw_add(n2262, n2264);
    let n2267: ZW = zw_cellmix_n(253u64, n1303, 1542469173u64);
    let n2268: ZW = zw_cellmix_n(253u64, n1303, 668265263u64);
    let n2269: ZW = zw_add(n2265, n2267);
    let n2270: ZW = zw_add(n2266, n2268);
    let n2271: ZW = zw_cellmix_n(257u64, n309, 1542469173u64);
    let n2272: ZW = zw_cellmix_n(257u64, n309, 668265263u64);
    let n2273: ZW = zw_add(n2269, n2271);
    let n2274: ZW = zw_add(n2270, n2272);
    let n2275: ZW = zw_cellmix_n(270u64, n310, 1542469173u64);
    let n2276: ZW = zw_cellmix_n(270u64, n310, 668265263u64);
    let n2277: ZW = zw_add(n2273, n2275);
    let n2278: ZW = zw_add(n2274, n2276);
    let n2279: ZW = zw_cellmix_n(277u64, n337, 1542469173u64);
    let n2280: ZW = zw_cellmix_n(277u64, n337, 668265263u64);
    let n2281: ZW = zw_add(n2277, n2279);
    let n2282: ZW = zw_add(n2278, n2280);
    let n2283: ZW = zw_cellmix_n(290u64, n338, 1542469173u64);
    let n2284: ZW = zw_cellmix_n(290u64, n338, 668265263u64);
    let n2285: ZW = zw_add(n2281, n2283);
    let n2286: ZW = zw_add(n2282, n2284);
    let n2287: ZW = zw_add(n2285, n1771);
    let n2288: ZW = zw_add(n2286, n1772);
    let n2289: ZW = zw_add(n2287, n1775);
    let n2290: ZW = zw_add(n2288, n1776);
    let n2291: ZW = zw_cellmix_n(20u64, n1480, 1542469173u64);
    let n2292: ZW = zw_cellmix_n(20u64, n1480, 668265263u64);
    let n2293: ZW = zw_add(n2285, n2291);
    let n2294: ZW = zw_add(n2286, n2292);
    let n2295: ZW = zw_cellmix_b(41u64, n1481, 1542469173u64);
    let n2296: ZW = zw_cellmix_b(41u64, n1481, 668265263u64);
    let n2297: ZW = zw_add(n2293, n2295);
    let n2298: ZW = zw_add(n2294, n2296);
    let n2299: ZW = zw_cellmix_b(38u64, n1487, 1542469173u64);
    let n2300: ZW = zw_cellmix_b(38u64, n1487, 668265263u64);
    let n2301: ZW = zw_add(zw_splat(0u64), n2299);
    let n2302: ZW = zw_add(zw_splat(0u64), n2300);
    let n2303: ZW = zw_cellmix_n(39u64, n1491, 1542469173u64);
    let n2304: ZW = zw_cellmix_n(39u64, n1491, 668265263u64);
    let n2305: ZW = zw_add(n2301, n2303);
    let n2306: ZW = zw_add(n2302, n2304);
    let n2307: ZW = zw_add(n2305, n1727);
    let n2308: ZW = zw_add(n2306, n1728);
    let n2309: ZW = zw_add(n2307, n1731);
    let n2310: ZW = zw_add(n2308, n1732);
    let n2311: ZW = zw_add(n2309, n1735);
    let n2312: ZW = zw_add(n2310, n1736);
    let n2313: ZW = zw_cellmix_n(87u64, n1490, 1542469173u64);
    let n2314: ZW = zw_cellmix_n(87u64, n1490, 668265263u64);
    let n2315: ZW = zw_add(n2311, n2313);
    let n2316: ZW = zw_add(n2312, n2314);
    let n2317: ZW = zw_add(n2315, n1771);
    let n2318: ZW = zw_add(n2316, n1772);
    let n2319: ZW = zw_add(n2315, n2291);
    let n2320: ZW = zw_add(n2316, n2292);
    let n2321: ZW = zw_cellmix_b(38u64, n1493, 1542469173u64);
    let n2322: ZW = zw_cellmix_b(38u64, n1493, 668265263u64);
    let n2323: ZW = zw_add(zw_splat(0u64), n2321);
    let n2324: ZW = zw_add(zw_splat(0u64), n2322);
    let n2325: ZW = zw_cellmix_n(39u64, n1497, 1542469173u64);
    let n2326: ZW = zw_cellmix_n(39u64, n1497, 668265263u64);
    let n2327: ZW = zw_add(n2323, n2325);
    let n2328: ZW = zw_add(n2324, n2326);
    let n2329: ZW = zw_add(n2327, n1727);
    let n2330: ZW = zw_add(n2328, n1728);
    let n2331: ZW = zw_add(n2329, n1731);
    let n2332: ZW = zw_add(n2330, n1732);
    let n2333: ZW = zw_add(n2331, n1735);
    let n2334: ZW = zw_add(n2332, n1736);
    let n2335: ZW = zw_cellmix_n(87u64, n1496, 1542469173u64);
    let n2336: ZW = zw_cellmix_n(87u64, n1496, 668265263u64);
    let n2337: ZW = zw_add(n2333, n2335);
    let n2338: ZW = zw_add(n2334, n2336);
    let n2339: ZW = zw_add(n2337, n1771);
    let n2340: ZW = zw_add(n2338, n1772);
    let n2341: ZW = zw_add(n2337, n1909);
    let n2342: ZW = zw_add(n2338, n1910);
    let n2343: ZW = zw_cellmix_n(246u64, n1500, 1542469173u64);
    let n2344: ZW = zw_cellmix_n(246u64, n1500, 668265263u64);
    let n2345: ZW = zw_add(n1741, n2343);
    let n2346: ZW = zw_add(n1742, n2344);
    let n2347: ZW = zw_cellmix_n(254u64, n1501, 1542469173u64);
    let n2348: ZW = zw_cellmix_n(254u64, n1501, 668265263u64);
    let n2349: ZW = zw_add(n2345, n2347);
    let n2350: ZW = zw_add(n2346, n2348);
    let n2351: ZW = zw_cellmix_n(258u64, n1506, 1542469173u64);
    let n2352: ZW = zw_cellmix_n(258u64, n1506, 668265263u64);
    let n2353: ZW = zw_add(n2349, n2351);
    let n2354: ZW = zw_add(n2350, n2352);
    let n2355: ZW = zw_cellmix_n(271u64, n1507, 1542469173u64);
    let n2356: ZW = zw_cellmix_n(271u64, n1507, 668265263u64);
    let n2357: ZW = zw_add(n2353, n2355);
    let n2358: ZW = zw_add(n2354, n2356);
    let n2359: ZW = zw_cellmix_n(278u64, n1508, 1542469173u64);
    let n2360: ZW = zw_cellmix_n(278u64, n1508, 668265263u64);
    let n2361: ZW = zw_add(n2357, n2359);
    let n2362: ZW = zw_add(n2358, n2360);
    let n2363: ZW = zw_cellmix_n(291u64, n1509, 1542469173u64);
    let n2364: ZW = zw_cellmix_n(291u64, n1509, 668265263u64);
    let n2365: ZW = zw_add(n2361, n2363);
    let n2366: ZW = zw_add(n2362, n2364);
    let n2367: ZW = zw_cellmix_n(319u64, n1515, 1542469173u64);
    let n2368: ZW = zw_cellmix_n(319u64, n1515, 668265263u64);
    let n2369: ZW = zw_add(n2365, n2367);
    let n2370: ZW = zw_add(n2366, n2368);
    let n2371: ZW = zw_cellmix_n(406u64, n1517, 1542469173u64);
    let n2372: ZW = zw_cellmix_n(406u64, n1517, 668265263u64);
    let n2373: ZW = zw_add(n2369, n2371);
    let n2374: ZW = zw_add(n2370, n2372);
    let n2375: ZW = zw_cellmix_n(407u64, n1518, 1542469173u64);
    let n2376: ZW = zw_cellmix_n(407u64, n1518, 668265263u64);
    let n2377: ZW = zw_add(n2373, n2375);
    let n2378: ZW = zw_add(n2374, n2376);
    let n2379: ZW = zw_cellmix_n(20u64, n1505, 1542469173u64);
    let n2380: ZW = zw_cellmix_n(20u64, n1505, 668265263u64);
    let n2381: ZW = zw_add(n2377, n2379);
    let n2382: ZW = zw_add(n2378, n2380);
    let n2383: ZW = zw_add(n2381, n1775);
    let n2384: ZW = zw_add(n2382, n1776);
    let n2385: ZW = zw_cellmix_n(299u64, n1510, 1542469173u64);
    let n2386: ZW = zw_cellmix_n(299u64, n1510, 668265263u64);
    let n2387: ZW = zw_add(n2383, n2385);
    let n2388: ZW = zw_add(n2384, n2386);
    let n2389: ZW = zw_cellmix_n(301u64, n1511, 1542469173u64);
    let n2390: ZW = zw_cellmix_n(301u64, n1511, 668265263u64);
    let n2391: ZW = zw_add(n2387, n2389);
    let n2392: ZW = zw_add(n2388, n2390);
    let n2393: ZW = zw_cellmix_n(302u64, n1512, 1542469173u64);
    let n2394: ZW = zw_cellmix_n(302u64, n1512, 668265263u64);
    let n2395: ZW = zw_add(n2391, n2393);
    let n2396: ZW = zw_add(n2392, n2394);
    let n2397: ZW = zw_cellmix_n(304u64, n1513, 1542469173u64);
    let n2398: ZW = zw_cellmix_n(304u64, n1513, 668265263u64);
    let n2399: ZW = zw_add(n2395, n2397);
    let n2400: ZW = zw_add(n2396, n2398);
    let n2401: ZW = zw_cellmix_b(311u64, n1502, 1542469173u64);
    let n2402: ZW = zw_cellmix_b(311u64, n1502, 668265263u64);
    let n2403: ZW = zw_add(n2399, n2401);
    let n2404: ZW = zw_add(n2400, n2402);
    let n2405: ZW = zw_cellmix_b(312u64, n1503, 1542469173u64);
    let n2406: ZW = zw_cellmix_b(312u64, n1503, 668265263u64);
    let n2407: ZW = zw_add(n2403, n2405);
    let n2408: ZW = zw_add(n2404, n2406);
    let n2409: ZW = zw_cellmix_n(318u64, n1531, 1542469173u64);
    let n2410: ZW = zw_cellmix_n(318u64, n1531, 668265263u64);
    let n2411: ZW = zw_add(n2407, n2409);
    let n2412: ZW = zw_add(n2408, n2410);
    let n2413: ZW = zw_cellmix_n(396u64, r_c396, 1542469173u64);
    let n2414: ZW = zw_cellmix_n(396u64, r_c396, 668265263u64);
    let n2415: ZW = zw_add(n2411, n2413);
    let n2416: ZW = zw_add(n2412, n2414);
    let n2417: ZW = zw_cellmix_n(397u64, r_c397, 1542469173u64);
    let n2418: ZW = zw_cellmix_n(397u64, r_c397, 668265263u64);
    let n2419: ZW = zw_add(n2415, n2417);
    let n2420: ZW = zw_add(n2416, n2418);
    let n2421: ZW = zw_cellmix_n(398u64, r_c398, 1542469173u64);
    let n2422: ZW = zw_cellmix_n(398u64, r_c398, 668265263u64);
    let n2423: ZW = zw_add(n2419, n2421);
    let n2424: ZW = zw_add(n2420, n2422);
    let n2425: ZW = zw_cellmix_n(399u64, r_c399, 1542469173u64);
    let n2426: ZW = zw_cellmix_n(399u64, r_c399, 668265263u64);
    let n2427: ZW = zw_add(n2423, n2425);
    let n2428: ZW = zw_add(n2424, n2426);
    let n2429: ZW = zw_cellmix_b(400u64, n1516, 1542469173u64);
    let n2430: ZW = zw_cellmix_b(400u64, n1516, 668265263u64);
    let n2431: ZW = zw_add(n2427, n2429);
    let n2432: ZW = zw_add(n2428, n2430);
    let n2433: ZW = zw_cellmix_n(408u64, n1532, 1542469173u64);
    let n2434: ZW = zw_cellmix_n(408u64, n1532, 668265263u64);
    let n2435: ZW = zw_add(n2431, n2433);
    let n2436: ZW = zw_add(n2432, n2434);
    let n2437: ZW = zw_cellmix_n(409u64, n1520, 1542469173u64);
    let n2438: ZW = zw_cellmix_n(409u64, n1520, 668265263u64);
    let n2439: ZW = zw_add(n2435, n2437);
    let n2440: ZW = zw_add(n2436, n2438);
    let n2441: ZW = zw_cellmix_b(400u64, n1534, 1542469173u64);
    let n2442: ZW = zw_cellmix_b(400u64, n1534, 668265263u64);
    let n2443: ZW = zw_add(n2427, n2441);
    let n2444: ZW = zw_add(n2428, n2442);
    let n2445: ZW = zw_cellmix_n(408u64, n1538, 1542469173u64);
    let n2446: ZW = zw_cellmix_n(408u64, n1538, 668265263u64);
    let n2447: ZW = zw_add(n2443, n2445);
    let n2448: ZW = zw_add(n2444, n2446);
    let n2449: ZW = zw_cellmix_n(409u64, n1536, 1542469173u64);
    let n2450: ZW = zw_cellmix_n(409u64, n1536, 668265263u64);
    let n2451: ZW = zw_add(n2447, n2449);
    let n2452: ZW = zw_add(n2448, n2450);
    let n2453: ZW = zw_cellmix_b(400u64, n1539, 1542469173u64);
    let n2454: ZW = zw_cellmix_b(400u64, n1539, 668265263u64);
    let n2455: ZW = zw_add(n2427, n2453);
    let n2456: ZW = zw_add(n2428, n2454);
    let n2457: ZW = zw_cellmix_n(408u64, n1543, 1542469173u64);
    let n2458: ZW = zw_cellmix_n(408u64, n1543, 668265263u64);
    let n2459: ZW = zw_add(n2455, n2457);
    let n2460: ZW = zw_add(n2456, n2458);
    let n2461: ZW = zw_cellmix_n(409u64, n1541, 1542469173u64);
    let n2462: ZW = zw_cellmix_n(409u64, n1541, 668265263u64);
    let n2463: ZW = zw_add(n2459, n2461);
    let n2464: ZW = zw_add(n2460, n2462);
    let n2465: ZW = zw_cellmix_n(304u64, n1545, 1542469173u64);
    let n2466: ZW = zw_cellmix_n(304u64, n1545, 668265263u64);
    let n2467: ZW = zw_add(n2395, n2465);
    let n2468: ZW = zw_add(n2396, n2466);
    let n2469: ZW = zw_add(n2467, n2401);
    let n2470: ZW = zw_add(n2468, n2402);
    let n2471: ZW = zw_cellmix_b(312u64, n1544, 1542469173u64);
    let n2472: ZW = zw_cellmix_b(312u64, n1544, 668265263u64);
    let n2473: ZW = zw_add(n2469, n2471);
    let n2474: ZW = zw_add(n2470, n2472);
    let n2475: ZW = zw_add(n2473, n2409);
    let n2476: ZW = zw_add(n2474, n2410);
    let n2477: ZW = zw_add(n2475, n2413);
    let n2478: ZW = zw_add(n2476, n2414);
    let n2479: ZW = zw_add(n2477, n2417);
    let n2480: ZW = zw_add(n2478, n2418);
    let n2481: ZW = zw_add(n2479, n2421);
    let n2482: ZW = zw_add(n2480, n2422);
    let n2483: ZW = zw_add(n2481, n2425);
    let n2484: ZW = zw_add(n2482, n2426);
    let n2485: ZW = zw_add(n2483, n2429);
    let n2486: ZW = zw_add(n2484, n2430);
    let n2487: ZW = zw_cellmix_n(408u64, n1549, 1542469173u64);
    let n2488: ZW = zw_cellmix_n(408u64, n1549, 668265263u64);
    let n2489: ZW = zw_add(n2485, n2487);
    let n2490: ZW = zw_add(n2486, n2488);
    let n2491: ZW = zw_cellmix_n(409u64, n1547, 1542469173u64);
    let n2492: ZW = zw_cellmix_n(409u64, n1547, 668265263u64);
    let n2493: ZW = zw_add(n2489, n2491);
    let n2494: ZW = zw_add(n2490, n2492);
    let n2495: ZW = zw_add(n2483, n2441);
    let n2496: ZW = zw_add(n2484, n2442);
    let n2497: ZW = zw_cellmix_n(408u64, n1553, 1542469173u64);
    let n2498: ZW = zw_cellmix_n(408u64, n1553, 668265263u64);
    let n2499: ZW = zw_add(n2495, n2497);
    let n2500: ZW = zw_add(n2496, n2498);
    let n2501: ZW = zw_cellmix_n(409u64, n1551, 1542469173u64);
    let n2502: ZW = zw_cellmix_n(409u64, n1551, 668265263u64);
    let n2503: ZW = zw_add(n2499, n2501);
    let n2504: ZW = zw_add(n2500, n2502);
    let n2505: ZW = zw_add(n2483, n2453);
    let n2506: ZW = zw_add(n2484, n2454);
    let n2507: ZW = zw_cellmix_n(408u64, n1557, 1542469173u64);
    let n2508: ZW = zw_cellmix_n(408u64, n1557, 668265263u64);
    let n2509: ZW = zw_add(n2505, n2507);
    let n2510: ZW = zw_add(n2506, n2508);
    let n2511: ZW = zw_cellmix_n(409u64, n1555, 1542469173u64);
    let n2512: ZW = zw_cellmix_n(409u64, n1555, 668265263u64);
    let n2513: ZW = zw_add(n2509, n2511);
    let n2514: ZW = zw_add(n2510, n2512);
    let n2515: ZW = zw_cellmix_n(20u64, n1577, 1542469173u64);
    let n2516: ZW = zw_cellmix_n(20u64, n1577, 668265263u64);
    let n2517: ZW = zw_add(n2377, n2515);
    let n2518: ZW = zw_add(n2378, n2516);
    let n2519: ZW = zw_cellmix_b(41u64, n1578, 1542469173u64);
    let n2520: ZW = zw_cellmix_b(41u64, n1578, 668265263u64);
    let n2521: ZW = zw_add(n2517, n2519);
    let n2522: ZW = zw_add(n2518, n2520);
    let n2523: ZW = zw_cellmix_n(299u64, n1579, 1542469173u64);
    let n2524: ZW = zw_cellmix_n(299u64, n1579, 668265263u64);
    let n2525: ZW = zw_add(n2521, n2523);
    let n2526: ZW = zw_add(n2522, n2524);
    let n2527: ZW = zw_cellmix_n(301u64, n1580, 1542469173u64);
    let n2528: ZW = zw_cellmix_n(301u64, n1580, 668265263u64);
    let n2529: ZW = zw_add(n2525, n2527);
    let n2530: ZW = zw_add(n2526, n2528);
    let n2531: ZW = zw_cellmix_n(302u64, n1581, 1542469173u64);
    let n2532: ZW = zw_cellmix_n(302u64, n1581, 668265263u64);
    let n2533: ZW = zw_add(n2529, n2531);
    let n2534: ZW = zw_add(n2530, n2532);
    let n2535: ZW = zw_add(n2533, n2397);
    let n2536: ZW = zw_add(n2534, n2398);
    let n2537: ZW = zw_cellmix_b(311u64, n1558, 1542469173u64);
    let n2538: ZW = zw_cellmix_b(311u64, n1558, 668265263u64);
    let n2539: ZW = zw_add(n2535, n2537);
    let n2540: ZW = zw_add(n2536, n2538);
    let n2541: ZW = zw_add(n2539, n2405);
    let n2542: ZW = zw_add(n2540, n2406);
    let n2543: ZW = zw_cellmix_n(318u64, n1590, 1542469173u64);
    let n2544: ZW = zw_cellmix_n(318u64, n1590, 668265263u64);
    let n2545: ZW = zw_add(n2541, n2543);
    let n2546: ZW = zw_add(n2542, n2544);
    let n2547: ZW = zw_cellmix_n(396u64, n1582, 1542469173u64);
    let n2548: ZW = zw_cellmix_n(396u64, n1582, 668265263u64);
    let n2549: ZW = zw_add(n2545, n2547);
    let n2550: ZW = zw_add(n2546, n2548);
    let n2551: ZW = zw_cellmix_n(397u64, n1583, 1542469173u64);
    let n2552: ZW = zw_cellmix_n(397u64, n1583, 668265263u64);
    let n2553: ZW = zw_add(n2549, n2551);
    let n2554: ZW = zw_add(n2550, n2552);
    let n2555: ZW = zw_cellmix_n(398u64, n1584, 1542469173u64);
    let n2556: ZW = zw_cellmix_n(398u64, n1584, 668265263u64);
    let n2557: ZW = zw_add(n2553, n2555);
    let n2558: ZW = zw_add(n2554, n2556);
    let n2559: ZW = zw_cellmix_n(399u64, n1585, 1542469173u64);
    let n2560: ZW = zw_cellmix_n(399u64, n1585, 668265263u64);
    let n2561: ZW = zw_add(n2557, n2559);
    let n2562: ZW = zw_add(n2558, n2560);
    let n2563: ZW = zw_add(n2561, n2429);
    let n2564: ZW = zw_add(n2562, n2430);
    let n2565: ZW = zw_cellmix_n(408u64, n1591, 1542469173u64);
    let n2566: ZW = zw_cellmix_n(408u64, n1591, 668265263u64);
    let n2567: ZW = zw_add(n2563, n2565);
    let n2568: ZW = zw_add(n2564, n2566);
    let n2569: ZW = zw_cellmix_n(409u64, n1587, 1542469173u64);
    let n2570: ZW = zw_cellmix_n(409u64, n1587, 668265263u64);
    let n2571: ZW = zw_add(n2567, n2569);
    let n2572: ZW = zw_add(n2568, n2570);
    let n2573: ZW = zw_cellmix_n(397u64, n1600, 1542469173u64);
    let n2574: ZW = zw_cellmix_n(397u64, n1600, 668265263u64);
    let n2575: ZW = zw_add(n2549, n2573);
    let n2576: ZW = zw_add(n2550, n2574);
    let n2577: ZW = zw_cellmix_n(398u64, n1601, 1542469173u64);
    let n2578: ZW = zw_cellmix_n(398u64, n1601, 668265263u64);
    let n2579: ZW = zw_add(n2575, n2577);
    let n2580: ZW = zw_add(n2576, n2578);
    let n2581: ZW = zw_add(n2579, n2559);
    let n2582: ZW = zw_add(n2580, n2560);
    let n2583: ZW = zw_add(n2581, n2441);
    let n2584: ZW = zw_add(n2582, n2442);
    let n2585: ZW = zw_cellmix_n(408u64, n1605, 1542469173u64);
    let n2586: ZW = zw_cellmix_n(408u64, n1605, 668265263u64);
    let n2587: ZW = zw_add(n2583, n2585);
    let n2588: ZW = zw_add(n2584, n2586);
    let n2589: ZW = zw_cellmix_n(409u64, n1603, 1542469173u64);
    let n2590: ZW = zw_cellmix_n(409u64, n1603, 668265263u64);
    let n2591: ZW = zw_add(n2587, n2589);
    let n2592: ZW = zw_add(n2588, n2590);
    let n2593: ZW = zw_cellmix_n(398u64, n1612, 1542469173u64);
    let n2594: ZW = zw_cellmix_n(398u64, n1612, 668265263u64);
    let n2595: ZW = zw_add(n2575, n2593);
    let n2596: ZW = zw_add(n2576, n2594);
    let n2597: ZW = zw_add(n2595, n2559);
    let n2598: ZW = zw_add(n2596, n2560);
    let n2599: ZW = zw_add(n2597, n2453);
    let n2600: ZW = zw_add(n2598, n2454);
    let n2601: ZW = zw_cellmix_n(408u64, n1616, 1542469173u64);
    let n2602: ZW = zw_cellmix_n(408u64, n1616, 668265263u64);
    let n2603: ZW = zw_add(n2599, n2601);
    let n2604: ZW = zw_add(n2600, n2602);
    let n2605: ZW = zw_cellmix_n(409u64, n1614, 1542469173u64);
    let n2606: ZW = zw_cellmix_n(409u64, n1614, 668265263u64);
    let n2607: ZW = zw_add(n2603, n2605);
    let n2608: ZW = zw_add(n2604, n2606);
    let n2609: ZW = zw_cellmix_n(396u64, n1629, 1542469173u64);
    let n2610: ZW = zw_cellmix_n(396u64, n1629, 668265263u64);
    let n2611: ZW = zw_add(n2545, n2609);
    let n2612: ZW = zw_add(n2546, n2610);
    let n2613: ZW = zw_cellmix_n(397u64, n1630, 1542469173u64);
    let n2614: ZW = zw_cellmix_n(397u64, n1630, 668265263u64);
    let n2615: ZW = zw_add(n2611, n2613);
    let n2616: ZW = zw_add(n2612, n2614);
    let n2617: ZW = zw_cellmix_n(398u64, n1631, 1542469173u64);
    let n2618: ZW = zw_cellmix_n(398u64, n1631, 668265263u64);
    let n2619: ZW = zw_add(n2615, n2617);
    let n2620: ZW = zw_add(n2616, n2618);
    let n2621: ZW = zw_cellmix_n(399u64, n1632, 1542469173u64);
    let n2622: ZW = zw_cellmix_n(399u64, n1632, 668265263u64);
    let n2623: ZW = zw_add(n2619, n2621);
    let n2624: ZW = zw_add(n2620, n2622);
    let n2625: ZW = zw_add(n2623, n2429);
    let n2626: ZW = zw_add(n2624, n2430);
    let n2627: ZW = zw_cellmix_n(408u64, n1636, 1542469173u64);
    let n2628: ZW = zw_cellmix_n(408u64, n1636, 668265263u64);
    let n2629: ZW = zw_add(n2625, n2627);
    let n2630: ZW = zw_add(n2626, n2628);
    let n2631: ZW = zw_cellmix_n(409u64, n1634, 1542469173u64);
    let n2632: ZW = zw_cellmix_n(409u64, n1634, 668265263u64);
    let n2633: ZW = zw_add(n2629, n2631);
    let n2634: ZW = zw_add(n2630, n2632);
    let n2635: ZW = zw_add(n2611, n2573);
    let n2636: ZW = zw_add(n2612, n2574);
    let n2637: ZW = zw_add(n2635, n2577);
    let n2638: ZW = zw_add(n2636, n2578);
    let n2639: ZW = zw_add(n2637, n2621);
    let n2640: ZW = zw_add(n2638, n2622);
    let n2641: ZW = zw_add(n2639, n2441);
    let n2642: ZW = zw_add(n2640, n2442);
    let n2643: ZW = zw_cellmix_n(408u64, n1644, 1542469173u64);
    let n2644: ZW = zw_cellmix_n(408u64, n1644, 668265263u64);
    let n2645: ZW = zw_add(n2641, n2643);
    let n2646: ZW = zw_add(n2642, n2644);
    let n2647: ZW = zw_cellmix_n(409u64, n1642, 1542469173u64);
    let n2648: ZW = zw_cellmix_n(409u64, n1642, 668265263u64);
    let n2649: ZW = zw_add(n2645, n2647);
    let n2650: ZW = zw_add(n2646, n2648);
    let n2651: ZW = zw_add(n2635, n2593);
    let n2652: ZW = zw_add(n2636, n2594);
    let n2653: ZW = zw_add(n2651, n2621);
    let n2654: ZW = zw_add(n2652, n2622);
    let n2655: ZW = zw_add(n2653, n2453);
    let n2656: ZW = zw_add(n2654, n2454);
    let n2657: ZW = zw_cellmix_n(408u64, n1652, 1542469173u64);
    let n2658: ZW = zw_cellmix_n(408u64, n1652, 668265263u64);
    let n2659: ZW = zw_add(n2655, n2657);
    let n2660: ZW = zw_add(n2656, n2658);
    let n2661: ZW = zw_cellmix_n(409u64, n1650, 1542469173u64);
    let n2662: ZW = zw_cellmix_n(409u64, n1650, 668265263u64);
    let n2663: ZW = zw_add(n2659, n2661);
    let n2664: ZW = zw_add(n2660, n2662);
    let n2665: ZW = zw_cellmix_n(399u64, n1657, 1542469173u64);
    let n2666: ZW = zw_cellmix_n(399u64, n1657, 668265263u64);
    let n2667: ZW = zw_add(n2619, n2665);
    let n2668: ZW = zw_add(n2620, n2666);
    let n2669: ZW = zw_add(n2667, n2429);
    let n2670: ZW = zw_add(n2668, n2430);
    let n2671: ZW = zw_add(n2669, n2627);
    let n2672: ZW = zw_add(n2670, n2628);
    let n2673: ZW = zw_cellmix_n(409u64, n1658, 1542469173u64);
    let n2674: ZW = zw_cellmix_n(409u64, n1658, 668265263u64);
    let n2675: ZW = zw_add(n2671, n2673);
    let n2676: ZW = zw_add(n2672, n2674);
    let n2677: ZW = zw_add(n2637, n2665);
    let n2678: ZW = zw_add(n2638, n2666);
    let n2679: ZW = zw_add(n2677, n2441);
    let n2680: ZW = zw_add(n2678, n2442);
    let n2681: ZW = zw_add(n2679, n2643);
    let n2682: ZW = zw_add(n2680, n2644);
    let n2683: ZW = zw_cellmix_n(409u64, n1661, 1542469173u64);
    let n2684: ZW = zw_cellmix_n(409u64, n1661, 668265263u64);
    let n2685: ZW = zw_add(n2681, n2683);
    let n2686: ZW = zw_add(n2682, n2684);
    let n2687: ZW = zw_add(n2651, n2665);
    let n2688: ZW = zw_add(n2652, n2666);
    let n2689: ZW = zw_add(n2687, n2453);
    let n2690: ZW = zw_add(n2688, n2454);
    let n2691: ZW = zw_add(n2689, n2657);
    let n2692: ZW = zw_add(n2690, n2658);
    let n2693: ZW = zw_cellmix_n(409u64, n1664, 1542469173u64);
    let n2694: ZW = zw_cellmix_n(409u64, n1664, 668265263u64);
    let n2695: ZW = zw_add(n2691, n2693);
    let n2696: ZW = zw_add(n2692, n2694);
    let n2697: ZW = zw_add(n2533, n2465);
    let n2698: ZW = zw_add(n2534, n2466);
    let n2699: ZW = zw_add(n2697, n2537);
    let n2700: ZW = zw_add(n2698, n2538);
    let n2701: ZW = zw_add(n2699, n2471);
    let n2702: ZW = zw_add(n2700, n2472);
    let n2703: ZW = zw_add(n2701, n2543);
    let n2704: ZW = zw_add(n2702, n2544);
    let n2705: ZW = zw_add(n2703, n2547);
    let n2706: ZW = zw_add(n2704, n2548);
    let n2707: ZW = zw_add(n2705, n2551);
    let n2708: ZW = zw_add(n2706, n2552);
    let n2709: ZW = zw_add(n2707, n2555);
    let n2710: ZW = zw_add(n2708, n2556);
    let n2711: ZW = zw_add(n2709, n2559);
    let n2712: ZW = zw_add(n2710, n2560);
    let n2713: ZW = zw_add(n2711, n2429);
    let n2714: ZW = zw_add(n2712, n2430);
    let n2715: ZW = zw_cellmix_n(408u64, n1672, 1542469173u64);
    let n2716: ZW = zw_cellmix_n(408u64, n1672, 668265263u64);
    let n2717: ZW = zw_add(n2713, n2715);
    let n2718: ZW = zw_add(n2714, n2716);
    let n2719: ZW = zw_cellmix_n(409u64, n1670, 1542469173u64);
    let n2720: ZW = zw_cellmix_n(409u64, n1670, 668265263u64);
    let n2721: ZW = zw_add(n2717, n2719);
    let n2722: ZW = zw_add(n2718, n2720);
    let n2723: ZW = zw_add(n2705, n2573);
    let n2724: ZW = zw_add(n2706, n2574);
    let n2725: ZW = zw_add(n2723, n2577);
    let n2726: ZW = zw_add(n2724, n2578);
    let n2727: ZW = zw_add(n2725, n2559);
    let n2728: ZW = zw_add(n2726, n2560);
    let n2729: ZW = zw_add(n2727, n2441);
    let n2730: ZW = zw_add(n2728, n2442);
    let n2731: ZW = zw_cellmix_n(408u64, n1680, 1542469173u64);
    let n2732: ZW = zw_cellmix_n(408u64, n1680, 668265263u64);
    let n2733: ZW = zw_add(n2729, n2731);
    let n2734: ZW = zw_add(n2730, n2732);
    let n2735: ZW = zw_cellmix_n(409u64, n1678, 1542469173u64);
    let n2736: ZW = zw_cellmix_n(409u64, n1678, 668265263u64);
    let n2737: ZW = zw_add(n2733, n2735);
    let n2738: ZW = zw_add(n2734, n2736);
    let n2739: ZW = zw_add(n2723, n2593);
    let n2740: ZW = zw_add(n2724, n2594);
    let n2741: ZW = zw_add(n2739, n2559);
    let n2742: ZW = zw_add(n2740, n2560);
    let n2743: ZW = zw_add(n2741, n2453);
    let n2744: ZW = zw_add(n2742, n2454);
    let n2745: ZW = zw_cellmix_n(408u64, n1688, 1542469173u64);
    let n2746: ZW = zw_cellmix_n(408u64, n1688, 668265263u64);
    let n2747: ZW = zw_add(n2743, n2745);
    let n2748: ZW = zw_add(n2744, n2746);
    let n2749: ZW = zw_cellmix_n(409u64, n1686, 1542469173u64);
    let n2750: ZW = zw_cellmix_n(409u64, n1686, 668265263u64);
    let n2751: ZW = zw_add(n2747, n2749);
    let n2752: ZW = zw_add(n2748, n2750);
    let n2753: ZW = zw_add(n2703, n2609);
    let n2754: ZW = zw_add(n2704, n2610);
    let n2755: ZW = zw_add(n2753, n2613);
    let n2756: ZW = zw_add(n2754, n2614);
    let n2757: ZW = zw_add(n2755, n2617);
    let n2758: ZW = zw_add(n2756, n2618);
    let n2759: ZW = zw_add(n2757, n2621);
    let n2760: ZW = zw_add(n2758, n2622);
    let n2761: ZW = zw_add(n2759, n2429);
    let n2762: ZW = zw_add(n2760, n2430);
    let n2763: ZW = zw_cellmix_n(408u64, n1696, 1542469173u64);
    let n2764: ZW = zw_cellmix_n(408u64, n1696, 668265263u64);
    let n2765: ZW = zw_add(n2761, n2763);
    let n2766: ZW = zw_add(n2762, n2764);
    let n2767: ZW = zw_cellmix_n(409u64, n1694, 1542469173u64);
    let n2768: ZW = zw_cellmix_n(409u64, n1694, 668265263u64);
    let n2769: ZW = zw_add(n2765, n2767);
    let n2770: ZW = zw_add(n2766, n2768);
    let n2771: ZW = zw_add(n2753, n2573);
    let n2772: ZW = zw_add(n2754, n2574);
    let n2773: ZW = zw_add(n2771, n2577);
    let n2774: ZW = zw_add(n2772, n2578);
    let n2775: ZW = zw_add(n2773, n2621);
    let n2776: ZW = zw_add(n2774, n2622);
    let n2777: ZW = zw_add(n2775, n2441);
    let n2778: ZW = zw_add(n2776, n2442);
    let n2779: ZW = zw_cellmix_n(408u64, n1704, 1542469173u64);
    let n2780: ZW = zw_cellmix_n(408u64, n1704, 668265263u64);
    let n2781: ZW = zw_add(n2777, n2779);
    let n2782: ZW = zw_add(n2778, n2780);
    let n2783: ZW = zw_cellmix_n(409u64, n1702, 1542469173u64);
    let n2784: ZW = zw_cellmix_n(409u64, n1702, 668265263u64);
    let n2785: ZW = zw_add(n2781, n2783);
    let n2786: ZW = zw_add(n2782, n2784);
    let n2787: ZW = zw_add(n2771, n2593);
    let n2788: ZW = zw_add(n2772, n2594);
    let n2789: ZW = zw_add(n2787, n2621);
    let n2790: ZW = zw_add(n2788, n2622);
    let n2791: ZW = zw_add(n2789, n2453);
    let n2792: ZW = zw_add(n2790, n2454);
    let n2793: ZW = zw_cellmix_n(408u64, n1712, 1542469173u64);
    let n2794: ZW = zw_cellmix_n(408u64, n1712, 668265263u64);
    let n2795: ZW = zw_add(n2791, n2793);
    let n2796: ZW = zw_add(n2792, n2794);
    let n2797: ZW = zw_cellmix_n(409u64, n1710, 1542469173u64);
    let n2798: ZW = zw_cellmix_n(409u64, n1710, 668265263u64);
    let n2799: ZW = zw_add(n2795, n2797);
    let n2800: ZW = zw_add(n2796, n2798);
    let n2801: ZW = zw_add(n2757, n2665);
    let n2802: ZW = zw_add(n2758, n2666);
    let n2803: ZW = zw_add(n2801, n2429);
    let n2804: ZW = zw_add(n2802, n2430);
    let n2805: ZW = zw_add(n2803, n2763);
    let n2806: ZW = zw_add(n2804, n2764);
    let n2807: ZW = zw_cellmix_n(409u64, n1715, 1542469173u64);
    let n2808: ZW = zw_cellmix_n(409u64, n1715, 668265263u64);
    let n2809: ZW = zw_add(n2805, n2807);
    let n2810: ZW = zw_add(n2806, n2808);
    let n2811: ZW = zw_add(n2773, n2665);
    let n2812: ZW = zw_add(n2774, n2666);
    let n2813: ZW = zw_add(n2811, n2441);
    let n2814: ZW = zw_add(n2812, n2442);
    let n2815: ZW = zw_add(n2813, n2779);
    let n2816: ZW = zw_add(n2814, n2780);
    let n2817: ZW = zw_cellmix_n(409u64, n1718, 1542469173u64);
    let n2818: ZW = zw_cellmix_n(409u64, n1718, 668265263u64);
    let n2819: ZW = zw_add(n2815, n2817);
    let n2820: ZW = zw_add(n2816, n2818);
    let n2821: ZW = zw_add(n2787, n2665);
    let n2822: ZW = zw_add(n2788, n2666);
    let n2823: ZW = zw_add(n2821, n2453);
    let n2824: ZW = zw_add(n2822, n2454);
    let n2825: ZW = zw_add(n2823, n2793);
    let n2826: ZW = zw_add(n2824, n2794);
    let n2827: ZW = zw_cellmix_n(409u64, n1721, 1542469173u64);
    let n2828: ZW = zw_cellmix_n(409u64, n1721, 668265263u64);
    let n2829: ZW = zw_add(n2825, n2827);
    let n2830: ZW = zw_add(n2826, n2828);
    let ok_v0_b0: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b0: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b0: u16 = ALL & zb_holds(n993) & zb_holds(n172) & zb_holds(n903) & zb_holds(n905);
    let ok_v1_b1: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b1: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b1: u16 = ALL & zb_holds(n993) & zb_holds(n172) & zb_holds(n903) & zb_holds(n905);
    let ok_v2_b2: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b2: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b2: u16 = ALL & zb_holds(n993) & zb_holds(n172) & zb_holds(n903) & zb_holds(n905);
    let ok_v16_b3: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b3: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b3: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n172) & zb_holds(n993);
    let ok_v17_b4: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b4: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b4: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n172) & zb_holds(n993);
    let ok_v18_b5: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b5: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b5: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n172) & zb_holds(n993);
    let ok_v32_b6: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b6: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b6: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v33_b7: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b7: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b7: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v34_b8: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b8: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b8: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v36_b9: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b9: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b9: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v37_b10: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b10: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b10: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v38_b11: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b11: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b11: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v40_b12: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b12: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b12: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v41_b13: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b13: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b13: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v42_b14: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b14: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b14: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v48_b15: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b15: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b15: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v49_b16: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b16: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b16: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v50_b17: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b17: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b17: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v52_b18: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b18: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b18: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v53_b19: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b19: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b19: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v54_b20: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b20: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b20: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v56_b21: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b21: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b21: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v57_b22: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b22: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b22: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v58_b23: u16 = ALL & zb_holds(n904) & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b23: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b23: u16 = ALL & zb_holds(n903) & zb_holds(n905) & zb_holds(n993);
    let ok_v0_b24: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1290);
    let bd_v0_b24: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b24: u16 = ALL & zb_holds(n993) & zb_holds(n1289);
    let ok_v32_b25: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1290);
    let bd_v32_b25: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b25: u16 = ALL & zb_holds(n993) & zb_holds(n1289);
    let ok_v0_b26: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1468);
    let bd_v0_b26: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b26: u16 = ALL & zb_holds(n993) & zb_holds(n1467);
    let ok_v32_b27: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1468);
    let bd_v32_b27: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b27: u16 = ALL & zb_holds(n993) & zb_holds(n1467);
    let ok_v0_b28: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1489);
    let bd_v0_b28: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b28: u16 = ALL & zb_holds(n1488);
    let ok_v32_b29: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1489);
    let bd_v32_b29: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b29: u16 = ALL & zb_holds(n1488);
    let ok_v0_b30: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1495);
    let bd_v0_b30: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b30: u16 = ALL & zb_holds(n1494);
    let ok_v32_b31: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1495);
    let bd_v32_b31: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b31: u16 = ALL & zb_holds(n1494);
    let ok_v0_b32: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v0_b32: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b32: u16 = ALL & zb_holds(n1521);
    let ok_v1_b33: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v1_b33: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b33: u16 = ALL & zb_holds(n1521);
    let ok_v2_b34: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v2_b34: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b34: u16 = ALL & zb_holds(n1521);
    let ok_v16_b35: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v16_b35: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b35: u16 = ALL & zb_holds(n1521);
    let ok_v17_b36: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v17_b36: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b36: u16 = ALL & zb_holds(n1521);
    let ok_v18_b37: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v18_b37: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b37: u16 = ALL & zb_holds(n1521);
    let ok_v32_b38: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v32_b38: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b38: u16 = ALL & zb_holds(n1521);
    let ok_v33_b39: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v33_b39: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b39: u16 = ALL & zb_holds(n1521);
    let ok_v34_b40: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v34_b40: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b40: u16 = ALL & zb_holds(n1521);
    let ok_v36_b41: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v36_b41: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b41: u16 = ALL & zb_holds(n1521);
    let ok_v37_b42: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v37_b42: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b42: u16 = ALL & zb_holds(n1521);
    let ok_v38_b43: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v38_b43: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b43: u16 = ALL & zb_holds(n1521);
    let ok_v40_b44: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v40_b44: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b44: u16 = ALL & zb_holds(n1521);
    let ok_v41_b45: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v41_b45: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b45: u16 = ALL & zb_holds(n1521);
    let ok_v42_b46: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v42_b46: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b46: u16 = ALL & zb_holds(n1521);
    let ok_v48_b47: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v48_b47: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b47: u16 = ALL & zb_holds(n1521);
    let ok_v49_b48: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v49_b48: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b48: u16 = ALL & zb_holds(n1521);
    let ok_v50_b49: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v50_b49: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b49: u16 = ALL & zb_holds(n1521);
    let ok_v52_b50: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v52_b50: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b50: u16 = ALL & zb_holds(n1521);
    let ok_v53_b51: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v53_b51: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b51: u16 = ALL & zb_holds(n1521);
    let ok_v54_b52: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v54_b52: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b52: u16 = ALL & zb_holds(n1521);
    let ok_v56_b53: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v56_b53: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b53: u16 = ALL & zb_holds(n1521);
    let ok_v57_b54: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v57_b54: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b54: u16 = ALL & zb_holds(n1521);
    let ok_v58_b55: u16 = ALL & zb_holds(n261) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n256) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n232) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1522);
    let bd_v58_b55: bool = !n260 || !n259 || !n258 || !n257 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b55: u16 = ALL & zb_holds(n1521);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n150,
        c86: n266,
        c241: n309,
        c254: n310,
        c261: n337,
        c274: n338,
        c368: n573,
        c369: n574,
        c302: n572,
        c85: n267,
    };
    let sh1 = KShared1 {
        c87: n1294,
        c84: n150,
        c86: n266,
        c240: n309,
        c253: n310,
        c260: n337,
        c273: n338,
        c85: n267,
    };
    let sh2 = KShared2 {
        c87: n1475,
        c84: n150,
        c86: n266,
        c245: n1298,
        c253: n1303,
        c257: n309,
        c270: n310,
        c277: n337,
        c290: n338,
        c85: n267,
    };
    let sh3 = KShared3 {
        c87: n1490,
        c39: n1491,
        c84: n150,
        c86: n266,
        c85: n267,
        c38: n1487,
    };
    let sh4 = KShared4 {
        c87: n1496,
        c39: n1497,
        c84: n150,
        c86: n266,
        c85: n267,
        c38: n1493,
    };
    let sh5 = KShared5 {
        c87: r_c87,
        c39: r_c39,
        c84: n150,
        c86: n266,
        c246: n1500,
        c254: n1501,
        c258: n1506,
        c271: n1507,
        c278: n1508,
        c291: n1509,
        c406: n1517,
        c407: n1518,
        c319: n1515,
        c85: n267,
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
    let mut take_3_1: u16 = 0;
    let mut take_4_0: u16 = 0;
    let mut take_4_1: u16 = 0;
    let mut take_5_0: u16 = 0;
    let mut take_5_1: u16 = 0;
    let mut take_5_2: u16 = 0;
    let mut take_5_3: u16 = 0;
    let mut take_5_4: u16 = 0;
    let mut take_5_5: u16 = 0;
    let mut take_5_6: u16 = 0;
    let mut take_5_7: u16 = 0;
    let mut take_5_8: u16 = 0;
    let mut take_5_9: u16 = 0;
    let mut take_5_10: u16 = 0;
    let mut take_5_11: u16 = 0;
    let mut take_5_12: u16 = 0;
    let mut take_5_13: u16 = 0;
    let mut take_5_14: u16 = 0;
    let mut take_5_15: u16 = 0;
    let mut take_5_16: u16 = 0;
    let mut take_5_17: u16 = 0;
    let mut take_5_18: u16 = 0;
    let mut take_5_19: u16 = 0;
    let mut take_5_20: u16 = 0;
    let mut take_5_21: u16 = 0;
    let mut take_5_22: u16 = 0;
    let mut take_5_23: u16 = 0;
    // 56 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n989,
        c287: n911,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1006,
        c371: n991,
        c301: n1005,
        h1: n1833, h2: n1834,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1039,
        c287: n911,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1043,
        c371: n1041,
        c301: n1005,
        h1: n1845, h2: n1846,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1064,
        c287: n911,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1068,
        c371: n1066,
        c301: n1005,
        h1: n1857, h2: n1858,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n989,
        c287: n1072,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1078,
        c371: n1074,
        c301: n1005,
        h1: n1887, h2: n1888,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1039,
        c287: n1072,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1084,
        c371: n1082,
        c301: n1005,
        h1: n1897, h2: n1898,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n292,
        c360: r_c398,
        c361: r_c399,
        c284: n295,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1064,
        c287: n1072,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1090,
        c371: n1088,
        c301: n1005,
        h1: n1907, h2: n1908,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1110,
        c282: n1100,
        c360: n1111,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1116,
        c371: n1113,
        c301: n1115,
        h1: n1965, h2: n1966,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1126,
        c371: n1124,
        c301: n1115,
        h1: n1985, h2: n1986,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1134,
        c371: n1132,
        c301: n1115,
        h1: n2001, h2: n2002,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1144,
        c282: n1100,
        c360: n1145,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1149,
        c371: n1147,
        c301: n1115,
        h1: n2027, h2: n2028,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1155,
        c371: n1153,
        c301: n1115,
        h1: n2043, h2: n2044,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1161,
        c371: n1159,
        c301: n1115,
        h1: n2057, h2: n2058,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1144,
        c282: n1100,
        c360: n1145,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1149,
        c371: n1165,
        c301: n1115,
        h1: n2069, h2: n2070,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1155,
        c371: n1167,
        c301: n1115,
        h1: n2079, h2: n2080,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n911,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1161,
        c371: n1169,
        c301: n1115,
        h1: n2089, h2: n2090,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1110,
        c282: n1100,
        c360: n1111,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1175,
        c371: n1173,
        c301: n1115,
        h1: n2115, h2: n2116,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1181,
        c371: n1179,
        c301: n1115,
        h1: n2131, h2: n2132,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1103,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1104,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1187,
        c371: n1185,
        c301: n1115,
        h1: n2145, h2: n2146,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1144,
        c282: n1100,
        c360: n1145,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1193,
        c371: n1191,
        c301: n1115,
        h1: n2163, h2: n2164,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1199,
        c371: n1197,
        c301: n1115,
        h1: n2179, h2: n2180,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1139,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1205,
        c371: n1203,
        c301: n1115,
        h1: n2193, h2: n2194,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1144,
        c282: n1100,
        c360: n1145,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n989,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1193,
        c371: n1207,
        c301: n1115,
        h1: n2203, h2: n2204,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1122,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n1039,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1199,
        c371: n1209,
        c301: n1115,
        h1: n2213, h2: n2214,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1098,
        c41: n1099,
        c358: n1138,
        c359: n1121,
        c282: n1100,
        c360: n1130,
        c361: n1163,
        c284: n1101,
        c285: n1102,
        c362: n1064,
        c287: n1072,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1205,
        c371: n1211,
        c301: n1115,
        h1: n2223, h2: n2224,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        h1: n2253, h2: n2254,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1098,
        c41: n1099,
        h1: n2257, h2: n2258,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n2289, h2: n2290,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1480,
        c41: n1481,
        h1: n2297, h2: n2298,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        h1: n2317, h2: n2318,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v32_b29 & (if bd_v32_b29 { ALL } else { !ok_v32_b29 });
    take_3_1 |= live_v32_b29 & ok_v32_b29 & (if bd_v32_b29 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1480,
        h1: n2319, h2: n2320,
    };
    // body 29: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined |= live_v0_b30 & (if bd_v0_b30 { ALL } else { !ok_v0_b30 });
    take_4_0 |= live_v0_b30 & ok_v0_b30 & (if bd_v0_b30 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        h1: n2339, h2: n2340,
    };
    // body 30: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v32_b31 & (if bd_v32_b31 { ALL } else { !ok_v32_b31 });
    take_4_1 |= live_v32_b31 & ok_v32_b31 & (if bd_v32_b31 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1098,
        h1: n2341, h2: n2342,
    };
    // body 31: buttons 0x20, forks 0x0
    sink.o4(32, take_4_1, &sh4, &o4);
    declined |= live_v0_b32 & (if bd_v0_b32 { ALL } else { !ok_v0_b32 });
    take_5_0 |= live_v0_b32 & ok_v0_b32 & (if bd_v0_b32 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1516,
        c304: n1513,
        c311: n1502,
        c312: n1503,
        c408: n1532,
        c409: n1520,
        c318: n1531,
        h1: n2439, h2: n2440,
    };
    // body 32: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v1_b33 & (if bd_v1_b33 { ALL } else { !ok_v1_b33 });
    take_5_1 |= live_v1_b33 & ok_v1_b33 & (if bd_v1_b33 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1534,
        c304: n1513,
        c311: n1502,
        c312: n1503,
        c408: n1538,
        c409: n1536,
        c318: n1531,
        h1: n2451, h2: n2452,
    };
    // body 33: buttons 0x01, forks 0x0
    sink.o5(1, take_5_1, &sh5, &o5);
    declined |= live_v2_b34 & (if bd_v2_b34 { ALL } else { !ok_v2_b34 });
    take_5_2 |= live_v2_b34 & ok_v2_b34 & (if bd_v2_b34 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1539,
        c304: n1513,
        c311: n1502,
        c312: n1503,
        c408: n1543,
        c409: n1541,
        c318: n1531,
        h1: n2463, h2: n2464,
    };
    // body 34: buttons 0x02, forks 0x0
    sink.o5(2, take_5_2, &sh5, &o5);
    declined |= live_v16_b35 & (if bd_v16_b35 { ALL } else { !ok_v16_b35 });
    take_5_3 |= live_v16_b35 & ok_v16_b35 & (if bd_v16_b35 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1516,
        c304: n1545,
        c311: n1502,
        c312: n1544,
        c408: n1549,
        c409: n1547,
        c318: n1531,
        h1: n2493, h2: n2494,
    };
    // body 35: buttons 0x10, forks 0x0
    sink.o5(16, take_5_3, &sh5, &o5);
    declined |= live_v17_b36 & (if bd_v17_b36 { ALL } else { !ok_v17_b36 });
    take_5_4 |= live_v17_b36 & ok_v17_b36 & (if bd_v17_b36 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1534,
        c304: n1545,
        c311: n1502,
        c312: n1544,
        c408: n1553,
        c409: n1551,
        c318: n1531,
        h1: n2503, h2: n2504,
    };
    // body 36: buttons 0x11, forks 0x0
    sink.o5(17, take_5_4, &sh5, &o5);
    declined |= live_v18_b37 & (if bd_v18_b37 { ALL } else { !ok_v18_b37 });
    take_5_5 |= live_v18_b37 & ok_v18_b37 & (if bd_v18_b37 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1505,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n1510,
        c398: r_c398,
        c399: r_c399,
        c301: n1511,
        c302: n1512,
        c400: n1539,
        c304: n1545,
        c311: n1502,
        c312: n1544,
        c408: n1557,
        c409: n1555,
        c318: n1531,
        h1: n2513, h2: n2514,
    };
    // body 37: buttons 0x12, forks 0x0
    sink.o5(18, take_5_5, &sh5, &o5);
    declined |= live_v32_b38 & (if bd_v32_b38 { ALL } else { !ok_v32_b38 });
    take_5_6 |= live_v32_b38 & ok_v32_b38 & (if bd_v32_b38 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1583,
        c299: n1579,
        c398: n1584,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1591,
        c409: n1587,
        c318: n1590,
        h1: n2571, h2: n2572,
    };
    // body 38: buttons 0x20, forks 0x0
    sink.o5(32, take_5_6, &sh5, &o5);
    declined |= live_v33_b39 & (if bd_v33_b39 { ALL } else { !ok_v33_b39 });
    take_5_7 |= live_v33_b39 & ok_v33_b39 & (if bd_v33_b39 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1605,
        c409: n1603,
        c318: n1590,
        h1: n2591, h2: n2592,
    };
    // body 39: buttons 0x21, forks 0x0
    sink.o5(33, take_5_7, &sh5, &o5);
    declined |= live_v34_b40 & (if bd_v34_b40 { ALL } else { !ok_v34_b40 });
    take_5_8 |= live_v34_b40 & ok_v34_b40 & (if bd_v34_b40 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1616,
        c409: n1614,
        c318: n1590,
        h1: n2607, h2: n2608,
    };
    // body 40: buttons 0x22, forks 0x0
    sink.o5(34, take_5_8, &sh5, &o5);
    declined |= live_v36_b41 & (if bd_v36_b41 { ALL } else { !ok_v36_b41 });
    take_5_9 |= live_v36_b41 & ok_v36_b41 & (if bd_v36_b41 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1630,
        c299: n1579,
        c398: n1631,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1636,
        c409: n1634,
        c318: n1590,
        h1: n2633, h2: n2634,
    };
    // body 41: buttons 0x24, forks 0x0
    sink.o5(36, take_5_9, &sh5, &o5);
    declined |= live_v37_b42 & (if bd_v37_b42 { ALL } else { !ok_v37_b42 });
    take_5_10 |= live_v37_b42 & ok_v37_b42 & (if bd_v37_b42 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1644,
        c409: n1642,
        c318: n1590,
        h1: n2649, h2: n2650,
    };
    // body 42: buttons 0x25, forks 0x0
    sink.o5(37, take_5_10, &sh5, &o5);
    declined |= live_v38_b43 & (if bd_v38_b43 { ALL } else { !ok_v38_b43 });
    take_5_11 |= live_v38_b43 & ok_v38_b43 & (if bd_v38_b43 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1652,
        c409: n1650,
        c318: n1590,
        h1: n2663, h2: n2664,
    };
    // body 43: buttons 0x26, forks 0x0
    sink.o5(38, take_5_11, &sh5, &o5);
    declined |= live_v40_b44 & (if bd_v40_b44 { ALL } else { !ok_v40_b44 });
    take_5_12 |= live_v40_b44 & ok_v40_b44 & (if bd_v40_b44 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1630,
        c299: n1579,
        c398: n1631,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1636,
        c409: n1658,
        c318: n1590,
        h1: n2675, h2: n2676,
    };
    // body 44: buttons 0x28, forks 0x0
    sink.o5(40, take_5_12, &sh5, &o5);
    declined |= live_v41_b45 & (if bd_v41_b45 { ALL } else { !ok_v41_b45 });
    take_5_13 |= live_v41_b45 & ok_v41_b45 & (if bd_v41_b45 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1644,
        c409: n1661,
        c318: n1590,
        h1: n2685, h2: n2686,
    };
    // body 45: buttons 0x29, forks 0x0
    sink.o5(41, take_5_13, &sh5, &o5);
    declined |= live_v42_b46 & (if bd_v42_b46 { ALL } else { !ok_v42_b46 });
    take_5_14 |= live_v42_b46 & ok_v42_b46 & (if bd_v42_b46 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1513,
        c311: n1558,
        c312: n1503,
        c408: n1652,
        c409: n1664,
        c318: n1590,
        h1: n2695, h2: n2696,
    };
    // body 46: buttons 0x2a, forks 0x0
    sink.o5(42, take_5_14, &sh5, &o5);
    declined |= live_v48_b47 & (if bd_v48_b47 { ALL } else { !ok_v48_b47 });
    take_5_15 |= live_v48_b47 & ok_v48_b47 & (if bd_v48_b47 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1583,
        c299: n1579,
        c398: n1584,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1672,
        c409: n1670,
        c318: n1590,
        h1: n2721, h2: n2722,
    };
    // body 47: buttons 0x30, forks 0x0
    sink.o5(48, take_5_15, &sh5, &o5);
    declined |= live_v49_b48 & (if bd_v49_b48 { ALL } else { !ok_v49_b48 });
    take_5_16 |= live_v49_b48 & ok_v49_b48 & (if bd_v49_b48 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1680,
        c409: n1678,
        c318: n1590,
        h1: n2737, h2: n2738,
    };
    // body 48: buttons 0x31, forks 0x0
    sink.o5(49, take_5_16, &sh5, &o5);
    declined |= live_v50_b49 & (if bd_v50_b49 { ALL } else { !ok_v50_b49 });
    take_5_17 |= live_v50_b49 & ok_v50_b49 & (if bd_v50_b49 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1582,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1585,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1688,
        c409: n1686,
        c318: n1590,
        h1: n2751, h2: n2752,
    };
    // body 49: buttons 0x32, forks 0x0
    sink.o5(50, take_5_17, &sh5, &o5);
    declined |= live_v52_b50 & (if bd_v52_b50 { ALL } else { !ok_v52_b50 });
    take_5_18 |= live_v52_b50 & ok_v52_b50 & (if bd_v52_b50 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1630,
        c299: n1579,
        c398: n1631,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1696,
        c409: n1694,
        c318: n1590,
        h1: n2769, h2: n2770,
    };
    // body 50: buttons 0x34, forks 0x0
    sink.o5(52, take_5_18, &sh5, &o5);
    declined |= live_v53_b51 & (if bd_v53_b51 { ALL } else { !ok_v53_b51 });
    take_5_19 |= live_v53_b51 & ok_v53_b51 & (if bd_v53_b51 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1704,
        c409: n1702,
        c318: n1590,
        h1: n2785, h2: n2786,
    };
    // body 51: buttons 0x35, forks 0x0
    sink.o5(53, take_5_19, &sh5, &o5);
    declined |= live_v54_b52 & (if bd_v54_b52 { ALL } else { !ok_v54_b52 });
    take_5_20 |= live_v54_b52 & ok_v54_b52 & (if bd_v54_b52 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1632,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1712,
        c409: n1710,
        c318: n1590,
        h1: n2799, h2: n2800,
    };
    // body 52: buttons 0x36, forks 0x0
    sink.o5(54, take_5_20, &sh5, &o5);
    declined |= live_v56_b53 & (if bd_v56_b53 { ALL } else { !ok_v56_b53 });
    take_5_21 |= live_v56_b53 & ok_v56_b53 & (if bd_v56_b53 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1630,
        c299: n1579,
        c398: n1631,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1516,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1696,
        c409: n1715,
        c318: n1590,
        h1: n2809, h2: n2810,
    };
    // body 53: buttons 0x38, forks 0x0
    sink.o5(56, take_5_21, &sh5, &o5);
    declined |= live_v57_b54 & (if bd_v57_b54 { ALL } else { !ok_v57_b54 });
    take_5_22 |= live_v57_b54 & ok_v57_b54 & (if bd_v57_b54 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1601,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1534,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1704,
        c409: n1718,
        c318: n1590,
        h1: n2819, h2: n2820,
    };
    // body 54: buttons 0x39, forks 0x0
    sink.o5(57, take_5_22, &sh5, &o5);
    declined |= live_v58_b55 & (if bd_v58_b55 { ALL } else { !ok_v58_b55 });
    take_5_23 |= live_v58_b55 & ok_v58_b55 & (if bd_v58_b55 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1577,
        c41: n1578,
        c396: n1629,
        c397: n1600,
        c299: n1579,
        c398: n1612,
        c399: n1657,
        c301: n1580,
        c302: n1581,
        c400: n1539,
        c304: n1545,
        c311: n1558,
        c312: n1544,
        c408: n1712,
        c409: n1721,
        c318: n1590,
        h1: n2829, h2: n2830,
    };
    // body 55: buttons 0x3a, forks 0x0
    sink.o5(58, take_5_23, &sh5, &o5);
    declined
}
