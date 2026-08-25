// GENERATED from a TRACED frame (shape 13). Do not edit.
//
// One input shape, 6 output shapes, 104 distinct button
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
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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
    pub c87: ZN,
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
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
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
    pub c87: ZN,
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append0(
    acc: &mut Rt2, sh: &KShared0, kv: &KOut0, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append1(
    acc: &mut Rt2, sh: &KShared1, kv: &KOut1, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append2(
    acc: &mut Rt2, sh: &KShared2, kv: &KOut2, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
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

/// Append this assignment's lanes that TAKE outcome 4 and
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append4(
    acc: &mut Rt2, sh: &KShared4, kv: &KOut4, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
        if let Col::N(v) = &mut acc.cols[87] { v.push(kv.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if let Col::V(v) = &mut acc.cols[38] {
            v.push(if kv.c38.known & (1 << i) != 0 {
                AV::Bool(kv.c38.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
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

/// Append this assignment's lanes that TAKE outcome 5 and
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
///
/// `org` is the engine-carried origin metadata of this
/// slice's input lanes (empty = untracked): each written
/// row records its input lane's origin, and the origin is
/// mixed into the dedup key so two rows from different
/// origins never collapse (`Rt2::origin`).
pub fn append5(
    acc: &mut Rt2, sh: &KShared5, kv: &KOut5, take: u16,
    n: usize, seen: &mut RowSet, org: &[u32],
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
        let key = if org.is_empty() { (h1[i], h2[i]) } else {
            // mix64 is a bijection: same row, different
            // origins can never collide.
            (mix64(h1[i] ^ mix64(0x517c_c1b7_2722_0a95 ^ org[i] as u64)), h2[i])
        };
        if !seen.insert(key) { continue; }
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

struct Append<'a> { accs: &'a mut [Rt2], seen: &'a mut [RowSet], n: usize, org: &'a [u32] }

impl<'a> Sink for Append<'a> {
    fn o0(&mut self, _mask: u8, take: u16, sh: &KShared0, v: &KOut0) {
        append0(&mut self.accs[0], sh, v, take, self.n, &mut self.seen[0], self.org);
    }
    fn o1(&mut self, _mask: u8, take: u16, sh: &KShared1, v: &KOut1) {
        append1(&mut self.accs[1], sh, v, take, self.n, &mut self.seen[1], self.org);
    }
    fn o2(&mut self, _mask: u8, take: u16, sh: &KShared2, v: &KOut2) {
        append2(&mut self.accs[2], sh, v, take, self.n, &mut self.seen[2], self.org);
    }
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org);
    }
    fn o4(&mut self, _mask: u8, take: u16, sh: &KShared4, v: &KOut4) {
        append4(&mut self.accs[4], sh, v, take, self.n, &mut self.seen[4], self.org);
    }
    fn o5(&mut self, _mask: u8, take: u16, sh: &KShared5, v: &KOut5) {
        append5(&mut self.accs[5], sh, v, take, self.n, &mut self.seen[5], self.org);
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
    // Engine-carried origin metadata for this slice's lanes
    // (empty = untracked); see `Rt2::origin`.
    let org: &[u32] = if b.origin.is_empty() { &[] } else { &b.origin[lo..lo + n] };
    let mut sink = Append { accs, seen, n, org };
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
    let n221: ZB = zn_le(r_c258, zn_splat(P8::from_raw(0i32)));
    let n222: ZB = zn_le(n152, zn_splat(P8::from_raw(0i32)));
    let n223: ZB = zn_gt(n152, zn_splat(P8::from_raw(0i32)));
    let n224: ZN = zsel_n(n220, n152, r_c258);
    let n228: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c271);
    let n229: ZB = zb_not(n228);
    let n230: ZN = zsel_n(n222, zn_splat(P8::from_raw(1179648i32)), r_c271);
    let n231: ZN = zsel_n(n220, n230, r_c271);
    let n233: ZB = zb_not(r_c311);
    let n234: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c293);
    let n235: ZB = zb_not(n144);
    let n236: ZB = zn_gt(r_c278, zn_splat(P8::from_raw(0i32)));
    let n237: ZB = zn_le(r_c278, zn_splat(P8::from_raw(0i32)));
    let n238: ZN = zn_sub(r_c278, zn_splat(P8::from_raw(65536i32)));
    let n239: ZB = zn_le(n238, zn_splat(P8::from_raw(0i32)));
    let n240: ZB = zn_gt(n238, zn_splat(P8::from_raw(0i32)));
    let n241: ZN = zsel_n(n239, zn_splat(P8::from_raw(1179648i32)), r_c291);
    let n242: ZN = zsel_n(n236, n238, r_c278);
    let n243: ZN = zsel_n(n236, n241, r_c291);
    let n260: ZB = zb_not(r_c401);
    let n261: bool = P8::from_raw(327680i32) == u.c402;
    let n262: bool = P8::from_raw(393216i32) == u.c403;
    let n263: bool = P8::from_raw(65536i32) == u.c404;
    let n264: bool = P8::from_raw(196608i32) == u.c405;
    let n265: ZB = zb_not(r_c38);
    let n266: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n267: ZN = zn_rem(n266, zn_splat(P8::from_raw(3932160i32)));
    let n268: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n267);
    let n269: ZN = zsel_n(n268, n171, r_c86);
    let n270: ZN = zsel_n(n151, n269, r_c86);
    let n271: ZN = zsel_n(n151, n267, r_c85);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c318);
    let n273: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n272);
    let n274: ZB = zn_gt(n273, zn_splat(P8::from_raw(524288i32)));
    let n275: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c319);
    let n276: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n275);
    let n277: ZB = zn_gt(n276, n197);
    let n278: ZB = zb_and(n274, n277);
    let n279: ZB = zn_lt(n272, zn_splat(P8::from_raw(1048576i32)));
    let n280: ZB = zb_and(n278, n279);
    let n281: ZB = zn_lt(n275, n199);
    let n282: ZB = zb_and(n280, n281);
    let n283: ZB = zb_and(n172, n282);
    let n284: ZB = zn_gt(n273, zn_splat(P8::from_raw(6815744i32)));
    let n285: ZB = zn_le(n273, zn_splat(P8::from_raw(6815744i32)));
    let n286: ZB = zn_gt(n276, zn_splat(P8::from_raw(7340032i32)));
    let n287: ZB = zn_lt(n272, zn_splat(P8::from_raw(7340032i32)));
    let n288: ZB = zn_lt(n275, zn_splat(P8::from_raw(7864320i32)));
    let n289: ZB = zn_ge(r_c409, zn_splat(P8::from_raw(0i32)));
    let n290: ZB = zn_lt(r_c409, zn_splat(P8::from_raw(0i32)));
    let n291: ZN = zn_mul(r_c408, zn_splat(P8::from_raw(13107i32)));
    let n292: ZN = zsel_n(n289, zn_splat(P8::from_raw(7077888i32)), r_c319);
    let n293: ZN = zsel_n(n289, n291, r_c408);
    let n294: ZN = zsel_n(n289, zn_splat(P8::from_raw(-196608i32)), r_c409);
    let n295: ZB = zb_not(r_c312);
    let n296: ZB = zn_gt(r_c304, zn_splat(P8::from_raw(0i32)));
    let n297: ZB = zn_le(r_c304, zn_splat(P8::from_raw(0i32)));
    let n298: ZN = zn_sub(r_c304, zn_splat(P8::from_raw(65536i32)));
    let n299: ZN = zsel_n(n296, n298, r_c304);
    let n300: ZN = zn_sub(r_c299, zn_splat(P8::from_raw(65536i32)));
    let n301: ZB = zn_gt(r_c301, zn_splat(P8::from_raw(0i32)));
    let n302: ZB = zn_le(r_c301, zn_splat(P8::from_raw(0i32)));
    let n303: ZN = zn_sub(r_c301, zn_splat(P8::from_raw(65536i32)));
    let n304: ZN = zsel_n(n301, n303, r_c301);
    let n305: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n306: ZB = zb_and(n228, n283);
    let n307: ZB = zb_and(n229, n283);
    let n308: ZB = zn_gt(n273, zn_splat(P8::from_raw(2621440i32)));
    let n309: ZB = zn_le(n273, zn_splat(P8::from_raw(2621440i32)));
    let n310: ZB = zb_and(n306, n308);
    let n311: ZB = zb_and(n306, n309);
    let n312: ZB = zb_or(n310, n311);
    let n313: ZB = zb_and(n286, n308);
    let n314: ZB = zb_not(n313);
    let n315: ZB = zb_and(n312, n313);
    let n316: ZB = zb_and(n312, n314);
    let n317: ZB = zn_lt(n272, zn_splat(P8::from_raw(3145728i32)));
    let n318: ZB = zb_or(n315, n316);
    let n319: ZB = zb_and(n313, n317);
    let n320: ZB = zb_not(n319);
    let n321: ZB = zb_and(n318, n319);
    let n322: ZB = zb_and(n318, n320);
    let n323: ZB = zb_or(n321, n322);
    let n324: ZB = zb_and(n288, n319);
    let n325: ZB = zb_not(n324);
    let n326: ZB = zb_and(n323, n324);
    let n327: ZB = zb_and(n323, n325);
    let n328: ZB = zb_and(n289, n326);
    let n329: ZB = zb_and(n290, n326);
    let n330: ZN = zsel_n(n289, zn_splat(P8::from_raw(655360i32)), r_c258);
    let n331: ZN = zsel_n(n289, zn_splat(P8::from_raw(1245184i32)), r_c271);
    let n332: ZB = zb_or(n328, n329);
    let n333: ZB = zb_and(n220, n307);
    let n334: ZB = zb_and(n221, n307);
    let n335: ZB = zb_and(n222, n333);
    let n336: ZB = zb_and(n223, n333);
    let n337: ZB = zb_or(n335, n336);
    let n338: ZB = zb_or(n334, n337);
    let n339: ZN = zsel_n(n324, n330, r_c258);
    let n340: ZN = zsel_n(n324, n331, r_c271);
    let n341: ZN = zsel_n(n324, n292, r_c319);
    let n342: ZN = zsel_n(n324, n293, r_c408);
    let n343: ZN = zsel_n(n324, n294, r_c409);
    let n344: ZB = zb_or(n327, n332);
    let n345: ZN = zsel_n(n229, n224, n339);
    let n346: ZN = zsel_n(n229, n231, n340);
    let n347: ZN = zsel_n(n229, r_c319, n341);
    let n348: ZN = zsel_n(n229, r_c408, n342);
    let n349: ZN = zsel_n(n229, r_c409, n343);
    let n350: ZB = zb_or(n338, n344);
    let n351: ZB = zb_and(n144, n350);
    let n352: ZB = zb_and(n235, n350);
    let n353: ZB = zb_and(n284, n351);
    let n354: ZB = zb_and(n285, n351);
    let n355: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n347);
    let n356: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n355);
    let n357: ZB = zn_gt(n356, zn_splat(P8::from_raw(7340032i32)));
    let n358: ZB = zb_or(n353, n354);
    let n359: ZB = zb_and(n284, n357);
    let n360: ZB = zb_not(n359);
    let n361: ZB = zb_and(n358, n359);
    let n362: ZB = zb_and(n358, n360);
    let n363: ZB = zb_or(n361, n362);
    let n364: ZB = zb_and(n287, n359);
    let n365: ZB = zb_not(n364);
    let n366: ZB = zb_and(n363, n364);
    let n367: ZB = zb_and(n363, n365);
    let n368: ZB = zn_lt(n355, zn_splat(P8::from_raw(7864320i32)));
    let n369: ZB = zb_or(n366, n367);
    let n370: ZB = zb_and(n364, n368);
    let n371: ZB = zb_not(n370);
    let n372: ZB = zb_and(n369, n370);
    let n373: ZB = zb_and(n369, n371);
    let n374: ZB = zn_ge(n349, zn_splat(P8::from_raw(0i32)));
    let n375: ZB = zn_lt(n349, zn_splat(P8::from_raw(0i32)));
    let n376: ZB = zb_and(n372, n374);
    let n377: ZB = zb_and(n372, n375);
    let n378: ZN = zn_mul(n348, zn_splat(P8::from_raw(13107i32)));
    let n379: ZN = zsel_n(n374, zn_splat(P8::from_raw(7077888i32)), n347);
    let n380: ZN = zsel_n(n374, zn_splat(P8::from_raw(655360i32)), r_c278);
    let n381: ZN = zsel_n(n374, zn_splat(P8::from_raw(1245184i32)), r_c291);
    let n382: ZN = zsel_n(n374, n378, n348);
    let n383: ZN = zsel_n(n374, zn_splat(P8::from_raw(-196608i32)), n349);
    let n384: ZB = zb_or(n376, n377);
    let n385: ZB = zb_and(n236, n352);
    let n386: ZB = zb_and(n237, n352);
    let n387: ZB = zb_and(n239, n385);
    let n388: ZB = zb_and(n240, n385);
    let n389: ZB = zb_or(n387, n388);
    let n390: ZB = zb_or(n386, n389);
    let n391: ZN = zsel_n(n370, n380, r_c278);
    let n392: ZN = zsel_n(n370, n381, r_c291);
    let n393: ZN = zsel_n(n370, n379, n347);
    let n394: ZN = zsel_n(n370, n382, n348);
    let n395: ZN = zsel_n(n370, n383, n349);
    let n396: ZB = zb_or(n373, n384);
    let n397: ZN = zsel_n(n235, n242, n391);
    let n398: ZN = zsel_n(n235, n243, n392);
    let n399: ZN = zsel_n(n235, n347, n393);
    let n400: ZN = zsel_n(n235, n348, n394);
    let n401: ZN = zsel_n(n235, n349, n395);
    let n402: ZB = zb_or(n390, n396);
    let n403: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n400);
    let n404: ZB = zb_not(n403);
    let n405: ZB = zb_and(n402, n403);
    let n406: ZB = zb_and(n402, n404);
    let n407: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n401);
    let n408: ZB = zb_not(n407);
    let n409: ZB = zb_or(n405, n406);
    let n410: ZB = zb_or(n404, n408);
    let n411: ZB = zb_not(n410);
    let n412: ZB = zb_and(n409, n410);
    let n413: ZB = zb_and(n409, n411);
    let n414: ZN = zn_add(r_c406, n400);
    let n415: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n414);
    let n416: ZN = zn_flr(n415);
    let n417: ZN = zn_sub(n415, zn_splat(P8::from_raw(32768i32)));
    let n418: ZN = zn_sub(n417, n416);
    let n419: ZB = zn_gt(n416, zn_splat(P8::from_raw(0i32)));
    let n420: ZB = zn_le(n416, zn_splat(P8::from_raw(0i32)));
    let n421: ZB = zb_and(n412, n419);
    let n422: ZB = zb_and(n412, n420);
    let n423: ZB = zn_lt(n416, zn_splat(P8::from_raw(0i32)));
    let n424: ZB = zn_ge(n416, zn_splat(P8::from_raw(0i32)));
    let n425: ZB = zb_and(n422, n423);
    let n426: ZB = zb_and(n422, n424);
    let n427: ZN = zsel_n(n423, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n428: ZB = zb_or(n425, n426);
    let n429: ZN = zsel_n(n419, zn_splat(P8::from_raw(65536i32)), n427);
    let n430: ZB = zb_or(n421, n428);
    let n431: ZN = zn_abs(n416);
    let n432: ZN = zn_add(n272, n429);
    let n433: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n399);
    let n434: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n433);
    let n435: ZB = zn_tile_flag_at(g.cache, g.cart, n432, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n436: ZB = zb_not(n435);
    let n437: ZB = zb_and(n430, n436);
    let n438: ZB = zb_and(n430, n435);
    let n439: ZB = zb_or(n437, n438);
    let n440: ZB = zb_and(n436, n439);
    let n441: ZB = zb_and(n435, n439);
    let n442: ZB = zb_or(n440, n441);
    let n443: ZB = zb_and(n436, n442);
    let n444: ZB = zb_and(n435, n442);
    let n445: ZN = zn_add(r_c318, n429);
    let n446: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n431);
    let n447: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n431);
    let n448: ZB = zb_and(n443, n446);
    let n449: ZB = zb_and(n443, n447);
    let n450: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n445);
    let n451: ZN = zn_add(n429, n450);
    let n452: ZB = zn_tile_flag_at(g.cache, g.cart, n451, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n453: ZB = zb_not(n452);
    let n454: ZB = zb_and(n448, n453);
    let n455: ZB = zb_and(n448, n452);
    let n456: ZB = zb_or(n454, n455);
    let n457: ZB = zb_and(n453, n456);
    let n458: ZB = zb_and(n452, n456);
    let n459: ZB = zb_or(n457, n458);
    let n460: ZB = zb_and(n453, n459);
    let n461: ZB = zb_and(n452, n459);
    let n462: ZN = zn_add(n429, n445);
    let n463: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n431);
    let n464: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n431);
    let n465: ZB = zb_and(n460, n463);
    let n466: ZB = zb_and(n460, n464);
    let n467: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n462);
    let n468: ZN = zn_add(n429, n467);
    let n469: ZB = zn_tile_flag_at(g.cache, g.cart, n468, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n470: ZB = zb_not(n469);
    let n471: ZB = zb_and(n465, n470);
    let n472: ZB = zb_and(n465, n469);
    let n473: ZB = zb_or(n471, n472);
    let n474: ZB = zb_and(n470, n473);
    let n475: ZB = zb_and(n469, n473);
    let n476: ZB = zb_or(n474, n475);
    let n477: ZB = zb_and(n470, n476);
    let n478: ZB = zb_and(n469, n476);
    let n479: ZN = zn_add(n429, n462);
    let n480: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n431);
    let n481: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n431);
    let n482: ZB = zb_and(n477, n480);
    let n483: ZB = zb_and(n477, n481);
    let n484: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n479);
    let n485: ZN = zn_add(n429, n484);
    let n486: ZB = zn_tile_flag_at(g.cache, g.cart, n485, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n487: ZB = zb_not(n486);
    let n488: ZB = zb_and(n482, n487);
    let n489: ZB = zb_and(n482, n486);
    let n490: ZB = zb_or(n488, n489);
    let n491: ZB = zb_and(n487, n490);
    let n492: ZB = zb_and(n486, n490);
    let n493: ZB = zb_or(n491, n492);
    let n494: ZB = zb_and(n487, n493);
    let n495: ZB = zb_and(n486, n493);
    let n496: ZN = zn_add(n429, n479);
    let n497: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n431);
    let n498: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n431);
    let n499: ZB = zb_and(n494, n497);
    let n500: ZB = zb_and(n494, n498);
    let n501: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n496);
    let n502: ZN = zn_add(n429, n501);
    let n503: ZB = zn_tile_flag_at(g.cache, g.cart, n502, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n504: ZB = zb_not(n503);
    let n505: ZB = zb_and(n499, n504);
    let n506: ZB = zb_and(n499, n503);
    let n507: ZB = zb_or(n505, n506);
    let n508: ZB = zb_and(n504, n507);
    let n509: ZB = zb_and(n503, n507);
    let n510: ZB = zb_or(n508, n509);
    let n511: ZB = zb_and(n504, n510);
    let n512: ZB = zb_and(n503, n510);
    let n513: ZN = zn_add(n429, n496);
    let n514: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n431);
    let n515: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n431);
    let n516: ZB = zb_and(n511, n514);
    let n517: ZB = zb_and(n511, n515);
    let n518: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n513);
    let n519: ZN = zn_add(n429, n518);
    let n520: ZB = zn_tile_flag_at(g.cache, g.cart, n519, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n521: ZB = zb_not(n520);
    let n522: ZB = zb_and(n516, n521);
    let n523: ZB = zb_and(n516, n520);
    let n524: ZB = zb_or(n522, n523);
    let n525: ZB = zb_and(n521, n524);
    let n526: ZB = zb_and(n520, n524);
    let n527: ZB = zb_or(n525, n526);
    let n528: ZB = zb_and(n521, n527);
    let n529: ZB = zb_and(n520, n527);
    let n530: ZN = zn_add(n429, n513);
    let n531: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n431);
    let n532: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n431);
    let n533: ZB = zb_and(n528, n531);
    let n534: ZB = zb_and(n528, n532);
    let n535: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n530);
    let n536: ZN = zn_add(n429, n535);
    let n537: ZB = zn_tile_flag_at(g.cache, g.cart, n536, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n538: ZB = zb_not(n537);
    let n539: ZB = zb_and(n533, n538);
    let n540: ZB = zb_and(n533, n537);
    let n541: ZB = zb_or(n539, n540);
    let n542: ZB = zb_and(n538, n541);
    let n543: ZB = zb_and(n537, n541);
    let n544: ZB = zb_or(n542, n543);
    let n545: ZB = zb_and(n538, n544);
    let n546: ZB = zb_and(n537, n544);
    let n547: ZN = zn_add(n429, n530);
    let n548: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n431);
    let n549: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n431);
    let n550: ZB = zb_and(n545, n548);
    let n551: ZB = zb_and(n545, n549);
    let n552: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n547);
    let n553: ZN = zn_add(n429, n552);
    let n554: ZB = zn_tile_flag_at(g.cache, g.cart, n553, n434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n550, n555);
    let n557: ZB = zb_and(n550, n554);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n555, n558);
    let n560: ZB = zb_and(n554, n558);
    let n561: ZB = zb_or(n559, n560);
    let n562: ZB = zb_and(n555, n561);
    let n563: ZB = zb_and(n554, n561);
    let n564: ZN = zn_add(n429, n547);
    let n565: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n431);
    let n566: ZN = zsel_n(n554, n547, n564);
    let n567: ZN = zsel_n(n554, zn_splat(P8::from_raw(0i32)), n418);
    let n568: ZN = zsel_n(n554, zn_splat(P8::from_raw(0i32)), n400);
    let n569: ZB = zb_or(n562, n563);
    let n570: ZB = zb_or(n554, n565);
    let n571: ZN = zsel_n(n549, n547, n566);
    let n572: ZN = zsel_n(n549, n418, n567);
    let n573: ZN = zsel_n(n549, n400, n568);
    let n574: ZB = zb_or(n551, n569);
    let n575: ZB = zb_or(n549, n570);
    let n576: ZN = zsel_n(n537, n530, n571);
    let n577: ZN = zsel_n(n537, zn_splat(P8::from_raw(0i32)), n572);
    let n578: ZN = zsel_n(n537, zn_splat(P8::from_raw(0i32)), n573);
    let n579: ZB = zb_or(n546, n574);
    let n580: ZB = zb_or(n537, n575);
    let n581: ZN = zsel_n(n532, n530, n576);
    let n582: ZN = zsel_n(n532, n418, n577);
    let n583: ZN = zsel_n(n532, n400, n578);
    let n584: ZB = zb_or(n534, n579);
    let n585: ZB = zb_or(n532, n580);
    let n586: ZN = zsel_n(n520, n513, n581);
    let n587: ZN = zsel_n(n520, zn_splat(P8::from_raw(0i32)), n582);
    let n588: ZN = zsel_n(n520, zn_splat(P8::from_raw(0i32)), n583);
    let n589: ZB = zb_or(n529, n584);
    let n590: ZB = zb_or(n520, n585);
    let n591: ZN = zsel_n(n515, n513, n586);
    let n592: ZN = zsel_n(n515, n418, n587);
    let n593: ZN = zsel_n(n515, n400, n588);
    let n594: ZB = zb_or(n517, n589);
    let n595: ZB = zb_or(n515, n590);
    let n596: ZN = zsel_n(n503, n496, n591);
    let n597: ZN = zsel_n(n503, zn_splat(P8::from_raw(0i32)), n592);
    let n598: ZN = zsel_n(n503, zn_splat(P8::from_raw(0i32)), n593);
    let n599: ZB = zb_or(n512, n594);
    let n600: ZB = zb_or(n503, n595);
    let n601: ZN = zsel_n(n498, n496, n596);
    let n602: ZN = zsel_n(n498, n418, n597);
    let n603: ZN = zsel_n(n498, n400, n598);
    let n604: ZB = zb_or(n500, n599);
    let n605: ZB = zb_or(n498, n600);
    let n606: ZN = zsel_n(n486, n479, n601);
    let n607: ZN = zsel_n(n486, zn_splat(P8::from_raw(0i32)), n602);
    let n608: ZN = zsel_n(n486, zn_splat(P8::from_raw(0i32)), n603);
    let n609: ZB = zb_or(n495, n604);
    let n610: ZB = zb_or(n486, n605);
    let n611: ZN = zsel_n(n481, n479, n606);
    let n612: ZN = zsel_n(n481, n418, n607);
    let n613: ZN = zsel_n(n481, n400, n608);
    let n614: ZB = zb_or(n483, n609);
    let n615: ZB = zb_or(n481, n610);
    let n616: ZN = zsel_n(n469, n462, n611);
    let n617: ZN = zsel_n(n469, zn_splat(P8::from_raw(0i32)), n612);
    let n618: ZN = zsel_n(n469, zn_splat(P8::from_raw(0i32)), n613);
    let n619: ZB = zb_or(n478, n614);
    let n620: ZB = zb_or(n469, n615);
    let n621: ZN = zsel_n(n464, n462, n616);
    let n622: ZN = zsel_n(n464, n418, n617);
    let n623: ZN = zsel_n(n464, n400, n618);
    let n624: ZB = zb_or(n466, n619);
    let n625: ZB = zb_or(n464, n620);
    let n626: ZN = zsel_n(n452, n445, n621);
    let n627: ZN = zsel_n(n452, zn_splat(P8::from_raw(0i32)), n622);
    let n628: ZN = zsel_n(n452, zn_splat(P8::from_raw(0i32)), n623);
    let n629: ZB = zb_or(n461, n624);
    let n630: ZB = zb_or(n452, n625);
    let n631: ZN = zsel_n(n447, n445, n626);
    let n632: ZN = zsel_n(n447, n418, n627);
    let n633: ZN = zsel_n(n447, n400, n628);
    let n634: ZB = zb_or(n449, n629);
    let n635: ZB = zb_or(n447, n630);
    let n636: ZN = zsel_n(n435, r_c318, n631);
    let n637: ZN = zsel_n(n435, zn_splat(P8::from_raw(0i32)), n632);
    let n638: ZN = zsel_n(n435, zn_splat(P8::from_raw(0i32)), n633);
    let n639: ZB = zb_or(n444, n634);
    let n640: ZB = zb_or(n435, n635);
    let n641: ZN = zn_add(r_c407, n401);
    let n642: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n641);
    let n643: ZN = zn_flr(n642);
    let n644: ZN = zn_sub(n642, zn_splat(P8::from_raw(32768i32)));
    let n645: ZN = zn_sub(n644, n643);
    let n646: ZB = zn_gt(n643, zn_splat(P8::from_raw(0i32)));
    let n647: ZB = zn_le(n643, zn_splat(P8::from_raw(0i32)));
    let n648: ZB = zb_and(n639, n646);
    let n649: ZB = zb_and(n639, n647);
    let n650: ZB = zn_lt(n643, zn_splat(P8::from_raw(0i32)));
    let n651: ZB = zn_ge(n643, zn_splat(P8::from_raw(0i32)));
    let n652: ZB = zb_and(n649, n650);
    let n653: ZB = zb_and(n649, n651);
    let n654: ZN = zsel_n(n650, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n655: ZB = zb_or(n652, n653);
    let n656: ZN = zsel_n(n646, zn_splat(P8::from_raw(65536i32)), n654);
    let n657: ZB = zb_or(n648, n655);
    let n658: ZN = zn_abs(n643);
    let n659: ZB = zn_gt(n656, zn_splat(P8::from_raw(0i32)));
    let n660: ZB = zn_le(n656, zn_splat(P8::from_raw(0i32)));
    let n661: ZB = zb_and(n657, n659);
    let n662: ZB = zb_and(n657, n660);
    let n663: ZB = zb_or(n661, n662);
    let n664: ZB = zb_and(n659, n663);
    let n665: ZB = zb_and(n660, n663);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n636);
    let n668: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n667);
    let n669: ZN = zn_add(n433, n656);
    let n670: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n669, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n671: ZB = zb_not(n670);
    let n672: ZB = zb_and(n666, n671);
    let n673: ZB = zb_and(n666, n670);
    let n674: ZB = zb_or(n672, n673);
    let n675: ZB = zb_and(n671, n674);
    let n676: ZB = zb_and(n670, n674);
    let n677: ZB = zb_or(n675, n676);
    let n678: ZB = zb_and(n671, n677);
    let n679: ZB = zb_and(n670, n677);
    let n680: ZN = zn_add(n399, n656);
    let n681: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n658);
    let n682: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n658);
    let n683: ZB = zb_and(n678, n681);
    let n684: ZB = zb_and(n678, n682);
    let n685: ZB = zb_and(n659, n683);
    let n686: ZB = zb_and(n660, n683);
    let n687: ZB = zb_or(n685, n686);
    let n688: ZB = zb_and(n659, n687);
    let n689: ZB = zb_and(n660, n687);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n680);
    let n692: ZN = zn_add(n656, n691);
    let n693: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n692, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n694: ZB = zb_not(n693);
    let n695: ZB = zb_and(n690, n694);
    let n696: ZB = zb_and(n690, n693);
    let n697: ZB = zb_or(n695, n696);
    let n698: ZB = zb_and(n694, n697);
    let n699: ZB = zb_and(n693, n697);
    let n700: ZB = zb_or(n698, n699);
    let n701: ZB = zb_and(n694, n700);
    let n702: ZB = zb_and(n693, n700);
    let n703: ZN = zn_add(n656, n680);
    let n704: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n658);
    let n705: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n658);
    let n706: ZB = zb_and(n701, n704);
    let n707: ZB = zb_and(n701, n705);
    let n708: ZB = zb_and(n659, n706);
    let n709: ZB = zb_and(n660, n706);
    let n710: ZB = zb_or(n708, n709);
    let n711: ZB = zb_and(n659, n710);
    let n712: ZB = zb_and(n660, n710);
    let n713: ZB = zb_or(n711, n712);
    let n714: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n703);
    let n715: ZN = zn_add(n656, n714);
    let n716: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n715, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n717: ZB = zb_not(n716);
    let n718: ZB = zb_and(n713, n717);
    let n719: ZB = zb_and(n713, n716);
    let n720: ZB = zb_or(n718, n719);
    let n721: ZB = zb_and(n717, n720);
    let n722: ZB = zb_and(n716, n720);
    let n723: ZB = zb_or(n721, n722);
    let n724: ZB = zb_and(n717, n723);
    let n725: ZB = zb_and(n716, n723);
    let n726: ZN = zn_add(n656, n703);
    let n727: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n658);
    let n728: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n658);
    let n729: ZB = zb_and(n724, n727);
    let n730: ZB = zb_and(n724, n728);
    let n731: ZB = zb_and(n659, n729);
    let n732: ZB = zb_and(n660, n729);
    let n733: ZB = zb_or(n731, n732);
    let n734: ZB = zb_and(n659, n733);
    let n735: ZB = zb_and(n660, n733);
    let n736: ZB = zb_or(n734, n735);
    let n737: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n726);
    let n738: ZN = zn_add(n656, n737);
    let n739: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n738, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n740: ZB = zb_not(n739);
    let n741: ZB = zb_and(n736, n740);
    let n742: ZB = zb_and(n736, n739);
    let n743: ZB = zb_or(n741, n742);
    let n744: ZB = zb_and(n740, n743);
    let n745: ZB = zb_and(n739, n743);
    let n746: ZB = zb_or(n744, n745);
    let n747: ZB = zb_and(n740, n746);
    let n748: ZB = zb_and(n739, n746);
    let n749: ZN = zn_add(n656, n726);
    let n750: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n658);
    let n751: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n658);
    let n752: ZB = zb_and(n747, n750);
    let n753: ZB = zb_and(n747, n751);
    let n754: ZB = zb_and(n659, n752);
    let n755: ZB = zb_and(n660, n752);
    let n756: ZB = zb_or(n754, n755);
    let n757: ZB = zb_and(n659, n756);
    let n758: ZB = zb_and(n660, n756);
    let n759: ZB = zb_or(n757, n758);
    let n760: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n749);
    let n761: ZN = zn_add(n656, n760);
    let n762: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n761, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n763: ZB = zb_not(n762);
    let n764: ZB = zb_and(n759, n763);
    let n765: ZB = zb_and(n759, n762);
    let n766: ZB = zb_or(n764, n765);
    let n767: ZB = zb_and(n763, n766);
    let n768: ZB = zb_and(n762, n766);
    let n769: ZB = zb_or(n767, n768);
    let n770: ZB = zb_and(n763, n769);
    let n771: ZB = zb_and(n762, n769);
    let n772: ZN = zn_add(n656, n749);
    let n773: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n658);
    let n774: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n658);
    let n775: ZB = zb_and(n770, n773);
    let n776: ZB = zb_and(n770, n774);
    let n777: ZB = zb_and(n659, n775);
    let n778: ZB = zb_and(n660, n775);
    let n779: ZB = zb_or(n777, n778);
    let n780: ZB = zb_and(n659, n779);
    let n781: ZB = zb_and(n660, n779);
    let n782: ZB = zb_or(n780, n781);
    let n783: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n772);
    let n784: ZN = zn_add(n656, n783);
    let n785: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n784, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n782, n786);
    let n788: ZB = zb_and(n782, n785);
    let n789: ZB = zb_or(n787, n788);
    let n790: ZB = zb_and(n786, n789);
    let n791: ZB = zb_and(n785, n789);
    let n792: ZB = zb_or(n790, n791);
    let n793: ZB = zb_and(n786, n792);
    let n794: ZB = zb_and(n785, n792);
    let n795: ZN = zn_add(n656, n772);
    let n796: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n658);
    let n797: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n658);
    let n798: ZB = zb_and(n793, n796);
    let n799: ZB = zb_and(n793, n797);
    let n800: ZB = zb_and(n659, n798);
    let n801: ZB = zb_and(n660, n798);
    let n802: ZB = zb_or(n800, n801);
    let n803: ZB = zb_and(n659, n802);
    let n804: ZB = zb_and(n660, n802);
    let n805: ZB = zb_or(n803, n804);
    let n806: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n795);
    let n807: ZN = zn_add(n656, n806);
    let n808: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n807, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n809: ZB = zb_not(n808);
    let n810: ZB = zb_and(n805, n809);
    let n811: ZB = zb_and(n805, n808);
    let n812: ZB = zb_or(n810, n811);
    let n813: ZB = zb_and(n809, n812);
    let n814: ZB = zb_and(n808, n812);
    let n815: ZB = zb_or(n813, n814);
    let n816: ZB = zb_and(n809, n815);
    let n817: ZB = zb_and(n808, n815);
    let n818: ZN = zn_add(n656, n795);
    let n819: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n658);
    let n820: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n658);
    let n821: ZB = zb_and(n816, n819);
    let n822: ZB = zb_and(n816, n820);
    let n823: ZB = zb_and(n659, n821);
    let n824: ZB = zb_and(n660, n821);
    let n825: ZB = zb_or(n823, n824);
    let n826: ZB = zb_and(n659, n825);
    let n827: ZB = zb_and(n660, n825);
    let n828: ZB = zb_or(n826, n827);
    let n829: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n818);
    let n830: ZN = zn_add(n656, n829);
    let n831: ZB = zn_tile_flag_at(g.cache, g.cart, n668, n830, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n832: ZB = zb_not(n831);
    let n833: ZB = zb_and(n828, n832);
    let n834: ZB = zb_and(n828, n831);
    let n835: ZB = zb_or(n833, n834);
    let n836: ZB = zb_and(n832, n835);
    let n837: ZB = zb_and(n831, n835);
    let n838: ZB = zb_or(n836, n837);
    let n839: ZB = zb_and(n832, n838);
    let n840: ZB = zb_and(n831, n838);
    let n841: ZN = zn_add(n656, n818);
    let n842: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n658);
    let n843: ZB = zb_and(n640, n842);
    let n844: ZN = zsel_n(n831, n818, n841);
    let n845: ZN = zsel_n(n831, zn_splat(P8::from_raw(0i32)), n645);
    let n846: ZN = zsel_n(n831, zn_splat(P8::from_raw(0i32)), n401);
    let n847: ZB = zb_or(n839, n840);
    let n848: ZB = zsel_b(n831, n640, n843);
    let n849: ZN = zsel_n(n820, n818, n844);
    let n850: ZN = zsel_n(n820, n645, n845);
    let n851: ZN = zsel_n(n820, n401, n846);
    let n852: ZB = zb_or(n822, n847);
    let n853: ZB = zsel_b(n820, n640, n848);
    let n854: ZN = zsel_n(n808, n795, n849);
    let n855: ZN = zsel_n(n808, zn_splat(P8::from_raw(0i32)), n850);
    let n856: ZN = zsel_n(n808, zn_splat(P8::from_raw(0i32)), n851);
    let n857: ZB = zb_or(n817, n852);
    let n858: ZB = zsel_b(n808, n640, n853);
    let n859: ZN = zsel_n(n797, n795, n854);
    let n860: ZN = zsel_n(n797, n645, n855);
    let n861: ZN = zsel_n(n797, n401, n856);
    let n862: ZB = zb_or(n799, n857);
    let n863: ZB = zsel_b(n797, n640, n858);
    let n864: ZN = zsel_n(n785, n772, n859);
    let n865: ZN = zsel_n(n785, zn_splat(P8::from_raw(0i32)), n860);
    let n866: ZN = zsel_n(n785, zn_splat(P8::from_raw(0i32)), n861);
    let n867: ZB = zb_or(n794, n862);
    let n868: ZB = zsel_b(n785, n640, n863);
    let n869: ZN = zsel_n(n774, n772, n864);
    let n870: ZN = zsel_n(n774, n645, n865);
    let n871: ZN = zsel_n(n774, n401, n866);
    let n872: ZB = zb_or(n776, n867);
    let n873: ZB = zsel_b(n774, n640, n868);
    let n874: ZN = zsel_n(n762, n749, n869);
    let n875: ZN = zsel_n(n762, zn_splat(P8::from_raw(0i32)), n870);
    let n876: ZN = zsel_n(n762, zn_splat(P8::from_raw(0i32)), n871);
    let n877: ZB = zb_or(n771, n872);
    let n878: ZB = zsel_b(n762, n640, n873);
    let n879: ZN = zsel_n(n751, n749, n874);
    let n880: ZN = zsel_n(n751, n645, n875);
    let n881: ZN = zsel_n(n751, n401, n876);
    let n882: ZB = zb_or(n753, n877);
    let n883: ZB = zsel_b(n751, n640, n878);
    let n884: ZN = zsel_n(n739, n726, n879);
    let n885: ZN = zsel_n(n739, zn_splat(P8::from_raw(0i32)), n880);
    let n886: ZN = zsel_n(n739, zn_splat(P8::from_raw(0i32)), n881);
    let n887: ZB = zb_or(n748, n882);
    let n888: ZB = zsel_b(n739, n640, n883);
    let n889: ZN = zsel_n(n728, n726, n884);
    let n890: ZN = zsel_n(n728, n645, n885);
    let n891: ZN = zsel_n(n728, n401, n886);
    let n892: ZB = zb_or(n730, n887);
    let n893: ZB = zsel_b(n728, n640, n888);
    let n894: ZN = zsel_n(n716, n703, n889);
    let n895: ZN = zsel_n(n716, zn_splat(P8::from_raw(0i32)), n890);
    let n896: ZN = zsel_n(n716, zn_splat(P8::from_raw(0i32)), n891);
    let n897: ZB = zb_or(n725, n892);
    let n898: ZB = zsel_b(n716, n640, n893);
    let n899: ZN = zsel_n(n705, n703, n894);
    let n900: ZN = zsel_n(n705, n645, n895);
    let n901: ZN = zsel_n(n705, n401, n896);
    let n902: ZB = zb_or(n707, n897);
    let n903: ZB = zsel_b(n705, n640, n898);
    let n904: ZN = zsel_n(n693, n680, n899);
    let n905: ZN = zsel_n(n693, zn_splat(P8::from_raw(0i32)), n900);
    let n906: ZN = zsel_n(n693, zn_splat(P8::from_raw(0i32)), n901);
    let n907: ZB = zb_or(n702, n902);
    let n908: ZB = zsel_b(n693, n640, n903);
    let n909: ZN = zsel_n(n682, n680, n904);
    let n910: ZN = zsel_n(n682, n645, n905);
    let n911: ZN = zsel_n(n682, n401, n906);
    let n912: ZB = zb_or(n684, n907);
    let n913: ZB = zsel_b(n682, n640, n908);
    let n914: ZN = zsel_n(n670, n399, n909);
    let n915: ZN = zsel_n(n670, zn_splat(P8::from_raw(0i32)), n910);
    let n916: ZN = zsel_n(n670, zn_splat(P8::from_raw(0i32)), n911);
    let n917: ZB = zb_or(n679, n912);
    let n918: ZB = zsel_b(n670, n640, n913);
    let n919: ZN = zsel_n(n410, n636, r_c318);
    let n920: ZN = zsel_n(n410, n914, n399);
    let n921: ZN = zsel_n(n410, n637, r_c406);
    let n922: ZN = zsel_n(n410, n915, r_c407);
    let n923: ZN = zsel_n(n410, n638, n400);
    let n924: ZN = zsel_n(n410, n916, n401);
    let n925: ZB = zb_or(n413, n917);
    let n926: ZB = zb_or(n411, n918);
    let n927: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n919);
    let n928: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n920);
    let n929: ZN = zn_div(n927, zn_splat(P8::from_raw(524288i32)));
    let n930: ZN = zn_flr(n929);
    let n931: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n930);
    let n932: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n927);
    let n933: ZN = zn_sub(n932, zn_splat(P8::from_raw(65536i32)));
    let n934: ZN = zn_div(n933, zn_splat(P8::from_raw(524288i32)));
    let n935: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n934);
    let n936: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n931);
    let n937: ZB = zn_le(n936, n935);
    let n938: ZB = zn_gt(n936, n935);
    let n939: ZB = zb_and(n925, n937);
    let n940: ZB = zb_and(n925, n938);
    let n941: ZN = zn_div(n928, zn_splat(P8::from_raw(524288i32)));
    let n942: ZN = zn_flr(n941);
    let n943: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n942);
    let n944: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n928);
    let n945: ZN = zn_sub(n944, zn_splat(P8::from_raw(65536i32)));
    let n946: ZN = zn_div(n945, zn_splat(P8::from_raw(524288i32)));
    let n947: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n946);
    let n948: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n943);
    let n949: ZB = zn_le(n948, n947);
    let n950: ZB = zn_gt(n948, n947);
    let n951: ZB = zb_and(n939, n949);
    let n952: ZB = zb_and(n939, n950);
    let n953: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n936);
    let n954: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n948);
    let n955: ZN = zn_mget(g.cart, n953, n954);
    let n956: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n955);
    let n957: ZB = zb_not(n956);
    let n958: ZB = zb_and(n951, n956);
    let n959: ZB = zb_and(n951, n957);
    let n960: ZN = zn_rem(n945, zn_splat(P8::from_raw(524288i32)));
    let n961: ZB = zn_ge(n960, zn_splat(P8::from_raw(393216i32)));
    let n962: ZB = zn_lt(n960, zn_splat(P8::from_raw(393216i32)));
    let n963: ZB = zb_and(n958, n962);
    let n964: ZB = zb_and(n958, n961);
    let n965: ZN = zn_mul(n948, zn_splat(P8::from_raw(524288i32)));
    let n966: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n965);
    let n967: ZB = zn_eq(n944, n966);
    let n968: ZB = zb_or(n963, n964);
    let n969: ZB = zb_or(n961, n967);
    let n970: ZB = zb_or(n959, n968);
    let n971: ZB = zb_and(n956, n969);
    let n972: ZB = zb_not(n971);
    let n973: ZB = zb_and(n970, n971);
    let n974: ZB = zb_and(n970, n972);
    let n975: ZB = zn_ge(n924, zn_splat(P8::from_raw(0i32)));
    let n976: ZB = zb_or(n973, n974);
    let n977: ZB = zb_and(n971, n975);
    let n978: ZB = zb_not(n977);
    let n979: ZB = zb_and(n976, n978);
    let n980: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n955);
    let n981: ZB = zb_not(n980);
    let n982: ZB = zb_and(n979, n980);
    let n983: ZB = zb_and(n979, n981);
    let n984: ZN = zn_rem(n928, zn_splat(P8::from_raw(524288i32)));
    let n985: ZB = zn_le(n984, zn_splat(P8::from_raw(131072i32)));
    let n986: ZB = zb_or(n982, n983);
    let n987: ZB = zb_and(n980, n985);
    let n988: ZB = zb_not(n987);
    let n989: ZB = zb_and(n986, n987);
    let n990: ZB = zb_and(n986, n988);
    let n991: ZB = zn_le(n924, zn_splat(P8::from_raw(0i32)));
    let n992: ZB = zb_or(n989, n990);
    let n993: ZB = zb_and(n987, n991);
    let n994: ZB = zb_not(n993);
    let n995: ZB = zb_and(n992, n994);
    let n996: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n955);
    let n997: ZB = zb_not(n996);
    let n998: ZB = zb_and(n995, n996);
    let n999: ZB = zb_and(n995, n997);
    let n1000: ZN = zn_rem(n927, zn_splat(P8::from_raw(524288i32)));
    let n1001: ZB = zn_le(n1000, zn_splat(P8::from_raw(131072i32)));
    let n1002: ZB = zb_or(n998, n999);
    let n1003: ZB = zb_and(n996, n1001);
    let n1004: ZB = zb_not(n1003);
    let n1005: ZB = zb_and(n1002, n1003);
    let n1006: ZB = zb_and(n1002, n1004);
    let n1007: ZB = zn_le(n923, zn_splat(P8::from_raw(0i32)));
    let n1008: ZB = zb_or(n1005, n1006);
    let n1009: ZB = zb_and(n1003, n1007);
    let n1010: ZB = zb_not(n1009);
    let n1011: ZB = zb_and(n1008, n1010);
    let n1012: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n955);
    let n1013: ZB = zb_not(n1012);
    let n1014: ZB = zb_and(n1011, n1012);
    let n1015: ZB = zb_and(n1011, n1013);
    let n1016: ZN = zn_rem(n933, zn_splat(P8::from_raw(524288i32)));
    let n1017: ZB = zn_ge(n1016, zn_splat(P8::from_raw(393216i32)));
    let n1018: ZB = zn_lt(n1016, zn_splat(P8::from_raw(393216i32)));
    let n1019: ZB = zb_and(n1014, n1018);
    let n1020: ZB = zb_and(n1014, n1017);
    let n1021: ZN = zn_mul(n936, zn_splat(P8::from_raw(524288i32)));
    let n1022: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1021);
    let n1023: ZB = zn_eq(n932, n1022);
    let n1024: ZB = zb_or(n1019, n1020);
    let n1025: ZB = zb_or(n1017, n1023);
    let n1026: ZB = zb_or(n1015, n1024);
    let n1027: ZB = zb_and(n1012, n1025);
    let n1028: ZB = zb_not(n1027);
    let n1029: ZB = zb_and(n1026, n1027);
    let n1030: ZB = zb_and(n1026, n1028);
    let n1031: ZB = zn_ge(n923, zn_splat(P8::from_raw(0i32)));
    let n1032: ZB = zb_or(n1029, n1030);
    let n1033: ZB = zb_and(n1027, n1031);
    let n1034: ZB = zb_not(n1033);
    let n1035: ZB = zb_and(n1032, n1034);
    let n1036: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n943);
    let n1037: ZB = zn_le(n1036, n947);
    let n1038: ZB = zn_gt(n1036, n947);
    let n1039: ZB = zb_and(n1035, n1037);
    let n1040: ZB = zb_and(n1035, n1038);
    let n1041: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1036);
    let n1042: ZN = zn_mget(g.cart, n953, n1041);
    let n1043: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1042);
    let n1044: ZB = zb_not(n1043);
    let n1045: ZB = zb_and(n1039, n1043);
    let n1046: ZB = zb_and(n1039, n1044);
    let n1047: ZB = zb_and(n962, n1045);
    let n1048: ZB = zb_and(n961, n1045);
    let n1049: ZN = zn_mul(n1036, zn_splat(P8::from_raw(524288i32)));
    let n1050: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1049);
    let n1051: ZB = zn_eq(n944, n1050);
    let n1052: ZB = zb_or(n1047, n1048);
    let n1053: ZB = zb_or(n961, n1051);
    let n1054: ZB = zb_or(n1046, n1052);
    let n1055: ZB = zb_and(n1043, n1053);
    let n1056: ZB = zb_not(n1055);
    let n1057: ZB = zb_and(n1054, n1055);
    let n1058: ZB = zb_and(n1054, n1056);
    let n1059: ZB = zb_or(n1057, n1058);
    let n1060: ZB = zb_and(n975, n1055);
    let n1061: ZB = zb_not(n1060);
    let n1062: ZB = zb_and(n1059, n1061);
    let n1063: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1042);
    let n1064: ZB = zb_not(n1063);
    let n1065: ZB = zb_and(n1062, n1063);
    let n1066: ZB = zb_and(n1062, n1064);
    let n1067: ZB = zb_or(n1065, n1066);
    let n1068: ZB = zb_and(n985, n1063);
    let n1069: ZB = zb_not(n1068);
    let n1070: ZB = zb_and(n1067, n1068);
    let n1071: ZB = zb_and(n1067, n1069);
    let n1072: ZB = zb_or(n1070, n1071);
    let n1073: ZB = zb_and(n991, n1068);
    let n1074: ZB = zb_not(n1073);
    let n1075: ZB = zb_and(n1072, n1074);
    let n1076: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1042);
    let n1077: ZB = zb_not(n1076);
    let n1078: ZB = zb_and(n1075, n1076);
    let n1079: ZB = zb_and(n1075, n1077);
    let n1080: ZB = zb_or(n1078, n1079);
    let n1081: ZB = zb_and(n1001, n1076);
    let n1082: ZB = zb_not(n1081);
    let n1083: ZB = zb_and(n1080, n1081);
    let n1084: ZB = zb_and(n1080, n1082);
    let n1085: ZB = zb_or(n1083, n1084);
    let n1086: ZB = zb_and(n1007, n1081);
    let n1087: ZB = zb_not(n1086);
    let n1088: ZB = zb_and(n1085, n1087);
    let n1089: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1042);
    let n1090: ZB = zb_not(n1089);
    let n1091: ZB = zb_and(n1088, n1089);
    let n1092: ZB = zb_and(n1088, n1090);
    let n1093: ZB = zb_and(n1018, n1091);
    let n1094: ZB = zb_and(n1017, n1091);
    let n1095: ZB = zb_or(n1093, n1094);
    let n1096: ZB = zb_or(n1092, n1095);
    let n1097: ZB = zb_and(n1025, n1089);
    let n1098: ZB = zb_not(n1097);
    let n1099: ZB = zb_and(n1096, n1097);
    let n1100: ZB = zb_and(n1096, n1098);
    let n1101: ZB = zb_or(n1099, n1100);
    let n1102: ZB = zb_and(n1031, n1097);
    let n1103: ZB = zb_not(n1102);
    let n1104: ZB = zb_and(n1101, n1103);
    let n1105: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n943);
    let n1106: ZB = zn_le(n1105, n947);
    let n1107: ZB = zn_gt(n1105, n947);
    let n1108: ZB = zb_and(n1104, n1106);
    let n1109: ZB = zb_and(n1104, n1107);
    let n1110: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1105);
    let n1111: ZN = zn_mget(g.cart, n953, n1110);
    let n1112: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1111);
    let n1113: ZB = zb_not(n1112);
    let n1114: ZB = zb_and(n1108, n1112);
    let n1115: ZB = zb_and(n1108, n1113);
    let n1116: ZB = zb_and(n962, n1114);
    let n1117: ZB = zb_and(n961, n1114);
    let n1118: ZN = zn_mul(n1105, zn_splat(P8::from_raw(524288i32)));
    let n1119: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1118);
    let n1120: ZB = zn_eq(n944, n1119);
    let n1121: ZB = zb_or(n1116, n1117);
    let n1122: ZB = zb_or(n961, n1120);
    let n1123: ZB = zb_or(n1115, n1121);
    let n1124: ZB = zb_and(n1112, n1122);
    let n1125: ZB = zb_not(n1124);
    let n1126: ZB = zb_and(n1123, n1124);
    let n1127: ZB = zb_and(n1123, n1125);
    let n1128: ZB = zb_or(n1126, n1127);
    let n1129: ZB = zb_and(n975, n1124);
    let n1130: ZB = zb_not(n1129);
    let n1131: ZB = zb_and(n1128, n1130);
    let n1132: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1111);
    let n1133: ZB = zb_not(n1132);
    let n1134: ZB = zb_and(n1131, n1132);
    let n1135: ZB = zb_and(n1131, n1133);
    let n1136: ZB = zb_or(n1134, n1135);
    let n1137: ZB = zb_and(n985, n1132);
    let n1138: ZB = zb_not(n1137);
    let n1139: ZB = zb_and(n1136, n1137);
    let n1140: ZB = zb_and(n1136, n1138);
    let n1141: ZB = zb_or(n1139, n1140);
    let n1142: ZB = zb_and(n991, n1137);
    let n1143: ZB = zb_not(n1142);
    let n1144: ZB = zb_and(n1141, n1143);
    let n1145: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1111);
    let n1146: ZB = zb_not(n1145);
    let n1147: ZB = zb_and(n1144, n1145);
    let n1148: ZB = zb_and(n1144, n1146);
    let n1149: ZB = zb_or(n1147, n1148);
    let n1150: ZB = zb_and(n1001, n1145);
    let n1151: ZB = zb_not(n1150);
    let n1152: ZB = zb_and(n1149, n1150);
    let n1153: ZB = zb_and(n1149, n1151);
    let n1154: ZB = zb_or(n1152, n1153);
    let n1155: ZB = zb_and(n1007, n1150);
    let n1156: ZB = zb_not(n1155);
    let n1157: ZB = zb_and(n1154, n1156);
    let n1158: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1111);
    let n1159: ZB = zb_not(n1158);
    let n1160: ZB = zb_and(n1157, n1158);
    let n1161: ZB = zb_and(n1157, n1159);
    let n1162: ZB = zb_and(n1018, n1160);
    let n1163: ZB = zb_and(n1017, n1160);
    let n1164: ZB = zb_or(n1162, n1163);
    let n1165: ZB = zb_or(n1161, n1164);
    let n1166: ZB = zb_and(n1025, n1158);
    let n1167: ZB = zb_not(n1166);
    let n1168: ZB = zb_and(n1165, n1166);
    let n1169: ZB = zb_and(n1165, n1167);
    let n1170: ZB = zb_or(n1168, n1169);
    let n1171: ZB = zb_and(n1031, n1166);
    let n1172: ZB = zb_not(n1171);
    let n1173: ZB = zb_and(n1170, n1172);
    let n1174: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n943);
    let n1175: ZB = zn_gt(n1174, n947);
    let n1176: ZB = zb_and(n926, n1175);
    let n1177: ZB = zb_or(n1109, n1173);
    let n1178: ZB = zsel_b(n1107, n926, n1176);
    let n1179: ZB = zb_or(n1040, n1177);
    let n1180: ZB = zsel_b(n1038, n926, n1178);
    let n1181: ZB = zb_or(n952, n1179);
    let n1182: ZB = zsel_b(n950, n926, n1180);
    let n1183: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n931);
    let n1184: ZB = zn_le(n1183, n935);
    let n1185: ZB = zn_gt(n1183, n935);
    let n1186: ZB = zb_and(n1181, n1184);
    let n1187: ZB = zb_and(n1181, n1185);
    let n1188: ZB = zb_and(n949, n1186);
    let n1189: ZB = zb_and(n950, n1186);
    let n1190: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1183);
    let n1191: ZN = zn_mget(g.cart, n1190, n954);
    let n1192: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1191);
    let n1193: ZB = zb_not(n1192);
    let n1194: ZB = zb_and(n1188, n1192);
    let n1195: ZB = zb_and(n1188, n1193);
    let n1196: ZB = zb_and(n962, n1194);
    let n1197: ZB = zb_and(n961, n1194);
    let n1198: ZB = zb_or(n1196, n1197);
    let n1199: ZB = zb_or(n1195, n1198);
    let n1200: ZB = zb_and(n969, n1192);
    let n1201: ZB = zb_not(n1200);
    let n1202: ZB = zb_and(n1199, n1200);
    let n1203: ZB = zb_and(n1199, n1201);
    let n1204: ZB = zb_or(n1202, n1203);
    let n1205: ZB = zb_and(n975, n1200);
    let n1206: ZB = zb_not(n1205);
    let n1207: ZB = zb_and(n1204, n1206);
    let n1208: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1191);
    let n1209: ZB = zb_not(n1208);
    let n1210: ZB = zb_and(n1207, n1208);
    let n1211: ZB = zb_and(n1207, n1209);
    let n1212: ZB = zb_or(n1210, n1211);
    let n1213: ZB = zb_and(n985, n1208);
    let n1214: ZB = zb_not(n1213);
    let n1215: ZB = zb_and(n1212, n1213);
    let n1216: ZB = zb_and(n1212, n1214);
    let n1217: ZB = zb_or(n1215, n1216);
    let n1218: ZB = zb_and(n991, n1213);
    let n1219: ZB = zb_not(n1218);
    let n1220: ZB = zb_and(n1217, n1219);
    let n1221: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1191);
    let n1222: ZB = zb_not(n1221);
    let n1223: ZB = zb_and(n1220, n1221);
    let n1224: ZB = zb_and(n1220, n1222);
    let n1225: ZB = zb_or(n1223, n1224);
    let n1226: ZB = zb_and(n1001, n1221);
    let n1227: ZB = zb_not(n1226);
    let n1228: ZB = zb_and(n1225, n1226);
    let n1229: ZB = zb_and(n1225, n1227);
    let n1230: ZB = zb_or(n1228, n1229);
    let n1231: ZB = zb_and(n1007, n1226);
    let n1232: ZB = zb_not(n1231);
    let n1233: ZB = zb_and(n1230, n1232);
    let n1234: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1191);
    let n1235: ZB = zb_not(n1234);
    let n1236: ZB = zb_and(n1233, n1234);
    let n1237: ZB = zb_and(n1233, n1235);
    let n1238: ZB = zb_and(n1018, n1236);
    let n1239: ZB = zb_and(n1017, n1236);
    let n1240: ZN = zn_mul(n1183, zn_splat(P8::from_raw(524288i32)));
    let n1241: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1240);
    let n1242: ZB = zn_eq(n932, n1241);
    let n1243: ZB = zb_or(n1238, n1239);
    let n1244: ZB = zb_or(n1017, n1242);
    let n1245: ZB = zb_or(n1237, n1243);
    let n1246: ZB = zb_and(n1234, n1244);
    let n1247: ZB = zb_not(n1246);
    let n1248: ZB = zb_and(n1245, n1246);
    let n1249: ZB = zb_and(n1245, n1247);
    let n1250: ZB = zb_or(n1248, n1249);
    let n1251: ZB = zb_and(n1031, n1246);
    let n1252: ZB = zb_not(n1251);
    let n1253: ZB = zb_and(n1250, n1252);
    let n1254: ZB = zb_and(n1037, n1253);
    let n1255: ZB = zb_and(n1038, n1253);
    let n1256: ZN = zn_mget(g.cart, n1190, n1041);
    let n1257: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1256);
    let n1258: ZB = zb_not(n1257);
    let n1259: ZB = zb_and(n1254, n1257);
    let n1260: ZB = zb_and(n1254, n1258);
    let n1261: ZB = zb_and(n962, n1259);
    let n1262: ZB = zb_and(n961, n1259);
    let n1263: ZB = zb_or(n1261, n1262);
    let n1264: ZB = zb_or(n1260, n1263);
    let n1265: ZB = zb_and(n1053, n1257);
    let n1266: ZB = zb_not(n1265);
    let n1267: ZB = zb_and(n1264, n1265);
    let n1268: ZB = zb_and(n1264, n1266);
    let n1269: ZB = zb_or(n1267, n1268);
    let n1270: ZB = zb_and(n975, n1265);
    let n1271: ZB = zb_not(n1270);
    let n1272: ZB = zb_and(n1269, n1271);
    let n1273: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1256);
    let n1274: ZB = zb_not(n1273);
    let n1275: ZB = zb_and(n1272, n1273);
    let n1276: ZB = zb_and(n1272, n1274);
    let n1277: ZB = zb_or(n1275, n1276);
    let n1278: ZB = zb_and(n985, n1273);
    let n1279: ZB = zb_not(n1278);
    let n1280: ZB = zb_and(n1277, n1278);
    let n1281: ZB = zb_and(n1277, n1279);
    let n1282: ZB = zb_or(n1280, n1281);
    let n1283: ZB = zb_and(n991, n1278);
    let n1284: ZB = zb_not(n1283);
    let n1285: ZB = zb_and(n1282, n1284);
    let n1286: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1256);
    let n1287: ZB = zb_not(n1286);
    let n1288: ZB = zb_and(n1285, n1286);
    let n1289: ZB = zb_and(n1285, n1287);
    let n1290: ZB = zb_or(n1288, n1289);
    let n1291: ZB = zb_and(n1001, n1286);
    let n1292: ZB = zb_not(n1291);
    let n1293: ZB = zb_and(n1290, n1291);
    let n1294: ZB = zb_and(n1290, n1292);
    let n1295: ZB = zb_or(n1293, n1294);
    let n1296: ZB = zb_and(n1007, n1291);
    let n1297: ZB = zb_not(n1296);
    let n1298: ZB = zb_and(n1295, n1297);
    let n1299: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1256);
    let n1300: ZB = zb_not(n1299);
    let n1301: ZB = zb_and(n1298, n1299);
    let n1302: ZB = zb_and(n1298, n1300);
    let n1303: ZB = zb_and(n1018, n1301);
    let n1304: ZB = zb_and(n1017, n1301);
    let n1305: ZB = zb_or(n1303, n1304);
    let n1306: ZB = zb_or(n1302, n1305);
    let n1307: ZB = zb_and(n1244, n1299);
    let n1308: ZB = zb_not(n1307);
    let n1309: ZB = zb_and(n1306, n1307);
    let n1310: ZB = zb_and(n1306, n1308);
    let n1311: ZB = zb_or(n1309, n1310);
    let n1312: ZB = zb_and(n1031, n1307);
    let n1313: ZB = zb_not(n1312);
    let n1314: ZB = zb_and(n1311, n1313);
    let n1315: ZB = zb_and(n1106, n1314);
    let n1316: ZB = zb_and(n1107, n1314);
    let n1317: ZN = zn_mget(g.cart, n1190, n1110);
    let n1318: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1317);
    let n1319: ZB = zb_not(n1318);
    let n1320: ZB = zb_and(n1315, n1318);
    let n1321: ZB = zb_and(n1315, n1319);
    let n1322: ZB = zb_and(n962, n1320);
    let n1323: ZB = zb_and(n961, n1320);
    let n1324: ZB = zb_or(n1322, n1323);
    let n1325: ZB = zb_or(n1321, n1324);
    let n1326: ZB = zb_and(n1122, n1318);
    let n1327: ZB = zb_not(n1326);
    let n1328: ZB = zb_and(n1325, n1326);
    let n1329: ZB = zb_and(n1325, n1327);
    let n1330: ZB = zb_or(n1328, n1329);
    let n1331: ZB = zb_and(n975, n1326);
    let n1332: ZB = zb_not(n1331);
    let n1333: ZB = zb_and(n1330, n1332);
    let n1334: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1317);
    let n1335: ZB = zb_not(n1334);
    let n1336: ZB = zb_and(n1333, n1334);
    let n1337: ZB = zb_and(n1333, n1335);
    let n1338: ZB = zb_or(n1336, n1337);
    let n1339: ZB = zb_and(n985, n1334);
    let n1340: ZB = zb_not(n1339);
    let n1341: ZB = zb_and(n1338, n1339);
    let n1342: ZB = zb_and(n1338, n1340);
    let n1343: ZB = zb_or(n1341, n1342);
    let n1344: ZB = zb_and(n991, n1339);
    let n1345: ZB = zb_not(n1344);
    let n1346: ZB = zb_and(n1343, n1345);
    let n1347: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1317);
    let n1348: ZB = zb_not(n1347);
    let n1349: ZB = zb_and(n1346, n1347);
    let n1350: ZB = zb_and(n1346, n1348);
    let n1351: ZB = zb_or(n1349, n1350);
    let n1352: ZB = zb_and(n1001, n1347);
    let n1353: ZB = zb_not(n1352);
    let n1354: ZB = zb_and(n1351, n1352);
    let n1355: ZB = zb_and(n1351, n1353);
    let n1356: ZB = zb_or(n1354, n1355);
    let n1357: ZB = zb_and(n1007, n1352);
    let n1358: ZB = zb_not(n1357);
    let n1359: ZB = zb_and(n1356, n1358);
    let n1360: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1317);
    let n1361: ZB = zb_not(n1360);
    let n1362: ZB = zb_and(n1359, n1360);
    let n1363: ZB = zb_and(n1359, n1361);
    let n1364: ZB = zb_and(n1018, n1362);
    let n1365: ZB = zb_and(n1017, n1362);
    let n1366: ZB = zb_or(n1364, n1365);
    let n1367: ZB = zb_or(n1363, n1366);
    let n1368: ZB = zb_and(n1244, n1360);
    let n1369: ZB = zb_not(n1368);
    let n1370: ZB = zb_and(n1367, n1368);
    let n1371: ZB = zb_and(n1367, n1369);
    let n1372: ZB = zb_or(n1370, n1371);
    let n1373: ZB = zb_and(n1031, n1368);
    let n1374: ZB = zb_not(n1373);
    let n1375: ZB = zb_and(n1372, n1374);
    let n1376: ZB = zb_and(n1175, n1182);
    let n1377: ZB = zb_or(n1316, n1375);
    let n1378: ZB = zsel_b(n1107, n1182, n1376);
    let n1379: ZB = zb_or(n1255, n1377);
    let n1380: ZB = zsel_b(n1038, n1182, n1378);
    let n1381: ZB = zb_or(n1189, n1379);
    let n1382: ZB = zsel_b(n950, n1182, n1380);
    let n1383: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n931);
    let n1384: ZB = zn_le(n1383, n935);
    let n1385: ZB = zn_gt(n1383, n935);
    let n1386: ZB = zb_and(n1381, n1384);
    let n1387: ZB = zb_and(n1381, n1385);
    let n1388: ZB = zb_and(n949, n1386);
    let n1389: ZB = zb_and(n950, n1386);
    let n1390: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1383);
    let n1391: ZN = zn_mget(g.cart, n1390, n954);
    let n1392: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1391);
    let n1393: ZB = zb_not(n1392);
    let n1394: ZB = zb_and(n1388, n1392);
    let n1395: ZB = zb_and(n1388, n1393);
    let n1396: ZB = zb_and(n962, n1394);
    let n1397: ZB = zb_and(n961, n1394);
    let n1398: ZB = zb_or(n1396, n1397);
    let n1399: ZB = zb_or(n1395, n1398);
    let n1400: ZB = zb_and(n969, n1392);
    let n1401: ZB = zb_not(n1400);
    let n1402: ZB = zb_and(n1399, n1400);
    let n1403: ZB = zb_and(n1399, n1401);
    let n1404: ZB = zb_or(n1402, n1403);
    let n1405: ZB = zb_and(n975, n1400);
    let n1406: ZB = zb_not(n1405);
    let n1407: ZB = zb_and(n1404, n1406);
    let n1408: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1391);
    let n1409: ZB = zb_not(n1408);
    let n1410: ZB = zb_and(n1407, n1408);
    let n1411: ZB = zb_and(n1407, n1409);
    let n1412: ZB = zb_or(n1410, n1411);
    let n1413: ZB = zb_and(n985, n1408);
    let n1414: ZB = zb_not(n1413);
    let n1415: ZB = zb_and(n1412, n1413);
    let n1416: ZB = zb_and(n1412, n1414);
    let n1417: ZB = zb_or(n1415, n1416);
    let n1418: ZB = zb_and(n991, n1413);
    let n1419: ZB = zb_not(n1418);
    let n1420: ZB = zb_and(n1417, n1419);
    let n1421: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1391);
    let n1422: ZB = zb_not(n1421);
    let n1423: ZB = zb_and(n1420, n1421);
    let n1424: ZB = zb_and(n1420, n1422);
    let n1425: ZB = zb_or(n1423, n1424);
    let n1426: ZB = zb_and(n1001, n1421);
    let n1427: ZB = zb_not(n1426);
    let n1428: ZB = zb_and(n1425, n1426);
    let n1429: ZB = zb_and(n1425, n1427);
    let n1430: ZB = zb_or(n1428, n1429);
    let n1431: ZB = zb_and(n1007, n1426);
    let n1432: ZB = zb_not(n1431);
    let n1433: ZB = zb_and(n1430, n1432);
    let n1434: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1391);
    let n1435: ZB = zb_not(n1434);
    let n1436: ZB = zb_and(n1433, n1434);
    let n1437: ZB = zb_and(n1433, n1435);
    let n1438: ZB = zb_and(n1018, n1436);
    let n1439: ZB = zb_and(n1017, n1436);
    let n1440: ZN = zn_mul(n1383, zn_splat(P8::from_raw(524288i32)));
    let n1441: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1440);
    let n1442: ZB = zn_eq(n932, n1441);
    let n1443: ZB = zb_or(n1438, n1439);
    let n1444: ZB = zb_or(n1017, n1442);
    let n1445: ZB = zb_or(n1437, n1443);
    let n1446: ZB = zb_and(n1434, n1444);
    let n1447: ZB = zb_not(n1446);
    let n1448: ZB = zb_and(n1445, n1446);
    let n1449: ZB = zb_and(n1445, n1447);
    let n1450: ZB = zb_or(n1448, n1449);
    let n1451: ZB = zb_and(n1031, n1446);
    let n1452: ZB = zb_not(n1451);
    let n1453: ZB = zb_and(n1450, n1452);
    let n1454: ZB = zb_and(n1037, n1453);
    let n1455: ZB = zb_and(n1038, n1453);
    let n1456: ZN = zn_mget(g.cart, n1390, n1041);
    let n1457: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1456);
    let n1458: ZB = zb_not(n1457);
    let n1459: ZB = zb_and(n1454, n1457);
    let n1460: ZB = zb_and(n1454, n1458);
    let n1461: ZB = zb_and(n962, n1459);
    let n1462: ZB = zb_and(n961, n1459);
    let n1463: ZB = zb_or(n1461, n1462);
    let n1464: ZB = zb_or(n1460, n1463);
    let n1465: ZB = zb_and(n1053, n1457);
    let n1466: ZB = zb_not(n1465);
    let n1467: ZB = zb_and(n1464, n1465);
    let n1468: ZB = zb_and(n1464, n1466);
    let n1469: ZB = zb_or(n1467, n1468);
    let n1470: ZB = zb_and(n975, n1465);
    let n1471: ZB = zb_not(n1470);
    let n1472: ZB = zb_and(n1469, n1471);
    let n1473: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1456);
    let n1474: ZB = zb_not(n1473);
    let n1475: ZB = zb_and(n1472, n1473);
    let n1476: ZB = zb_and(n1472, n1474);
    let n1477: ZB = zb_or(n1475, n1476);
    let n1478: ZB = zb_and(n985, n1473);
    let n1479: ZB = zb_not(n1478);
    let n1480: ZB = zb_and(n1477, n1478);
    let n1481: ZB = zb_and(n1477, n1479);
    let n1482: ZB = zb_or(n1480, n1481);
    let n1483: ZB = zb_and(n991, n1478);
    let n1484: ZB = zb_not(n1483);
    let n1485: ZB = zb_and(n1482, n1484);
    let n1486: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1456);
    let n1487: ZB = zb_not(n1486);
    let n1488: ZB = zb_and(n1485, n1486);
    let n1489: ZB = zb_and(n1485, n1487);
    let n1490: ZB = zb_or(n1488, n1489);
    let n1491: ZB = zb_and(n1001, n1486);
    let n1492: ZB = zb_not(n1491);
    let n1493: ZB = zb_and(n1490, n1491);
    let n1494: ZB = zb_and(n1490, n1492);
    let n1495: ZB = zb_or(n1493, n1494);
    let n1496: ZB = zb_and(n1007, n1491);
    let n1497: ZB = zb_not(n1496);
    let n1498: ZB = zb_and(n1495, n1497);
    let n1499: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1456);
    let n1500: ZB = zb_not(n1499);
    let n1501: ZB = zb_and(n1498, n1499);
    let n1502: ZB = zb_and(n1498, n1500);
    let n1503: ZB = zb_and(n1018, n1501);
    let n1504: ZB = zb_and(n1017, n1501);
    let n1505: ZB = zb_or(n1503, n1504);
    let n1506: ZB = zb_or(n1502, n1505);
    let n1507: ZB = zb_and(n1444, n1499);
    let n1508: ZB = zb_not(n1507);
    let n1509: ZB = zb_and(n1506, n1507);
    let n1510: ZB = zb_and(n1506, n1508);
    let n1511: ZB = zb_or(n1509, n1510);
    let n1512: ZB = zb_and(n1031, n1507);
    let n1513: ZB = zb_not(n1512);
    let n1514: ZB = zb_and(n1511, n1513);
    let n1515: ZB = zb_and(n1106, n1514);
    let n1516: ZB = zb_and(n1107, n1514);
    let n1517: ZN = zn_mget(g.cart, n1390, n1110);
    let n1518: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1517);
    let n1519: ZB = zb_not(n1518);
    let n1520: ZB = zb_and(n1515, n1518);
    let n1521: ZB = zb_and(n1515, n1519);
    let n1522: ZB = zb_and(n962, n1520);
    let n1523: ZB = zb_and(n961, n1520);
    let n1524: ZB = zb_or(n1522, n1523);
    let n1525: ZB = zb_or(n1521, n1524);
    let n1526: ZB = zb_and(n1122, n1518);
    let n1527: ZB = zb_not(n1526);
    let n1528: ZB = zb_and(n1525, n1526);
    let n1529: ZB = zb_and(n1525, n1527);
    let n1530: ZB = zb_or(n1528, n1529);
    let n1531: ZB = zb_and(n975, n1526);
    let n1532: ZB = zb_not(n1531);
    let n1533: ZB = zb_and(n1530, n1532);
    let n1534: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1517);
    let n1535: ZB = zb_not(n1534);
    let n1536: ZB = zb_and(n1533, n1534);
    let n1537: ZB = zb_and(n1533, n1535);
    let n1538: ZB = zb_or(n1536, n1537);
    let n1539: ZB = zb_and(n985, n1534);
    let n1540: ZB = zb_not(n1539);
    let n1541: ZB = zb_and(n1538, n1539);
    let n1542: ZB = zb_and(n1538, n1540);
    let n1543: ZB = zb_or(n1541, n1542);
    let n1544: ZB = zb_and(n991, n1539);
    let n1545: ZB = zb_not(n1544);
    let n1546: ZB = zb_and(n1543, n1545);
    let n1547: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1517);
    let n1548: ZB = zb_not(n1547);
    let n1549: ZB = zb_and(n1546, n1547);
    let n1550: ZB = zb_and(n1546, n1548);
    let n1551: ZB = zb_or(n1549, n1550);
    let n1552: ZB = zb_and(n1001, n1547);
    let n1553: ZB = zb_not(n1552);
    let n1554: ZB = zb_and(n1551, n1552);
    let n1555: ZB = zb_and(n1551, n1553);
    let n1556: ZB = zb_or(n1554, n1555);
    let n1557: ZB = zb_and(n1007, n1552);
    let n1558: ZB = zb_not(n1557);
    let n1559: ZB = zb_and(n1556, n1558);
    let n1560: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1517);
    let n1561: ZB = zb_not(n1560);
    let n1562: ZB = zb_and(n1559, n1560);
    let n1563: ZB = zb_and(n1559, n1561);
    let n1564: ZB = zb_and(n1018, n1562);
    let n1565: ZB = zb_and(n1017, n1562);
    let n1566: ZB = zb_or(n1564, n1565);
    let n1567: ZB = zb_or(n1563, n1566);
    let n1568: ZB = zb_and(n1444, n1560);
    let n1569: ZB = zb_not(n1568);
    let n1570: ZB = zb_and(n1567, n1568);
    let n1571: ZB = zb_and(n1567, n1569);
    let n1572: ZB = zb_or(n1570, n1571);
    let n1573: ZB = zb_and(n1031, n1568);
    let n1574: ZB = zb_not(n1573);
    let n1575: ZB = zb_and(n1572, n1574);
    let n1576: ZB = zb_and(n1175, n1382);
    let n1577: ZB = zb_or(n1516, n1575);
    let n1578: ZB = zsel_b(n1107, n1382, n1576);
    let n1579: ZB = zb_or(n1455, n1577);
    let n1580: ZB = zsel_b(n1038, n1382, n1578);
    let n1581: ZB = zb_or(n1389, n1579);
    let n1582: ZB = zsel_b(n950, n1382, n1580);
    let n1583: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n931);
    let n1584: ZB = zn_gt(n1583, n935);
    let n1585: ZB = zb_and(n1582, n1584);
    let n1586: ZB = zb_or(n1387, n1581);
    let n1587: ZB = zsel_b(n1385, n1382, n1585);
    let n1588: ZB = zb_or(n1187, n1586);
    let n1589: ZB = zsel_b(n1185, n1182, n1587);
    let n1590: ZB = zb_or(n940, n1588);
    let n1591: ZB = zsel_b(n938, n926, n1589);
    let n1592: ZB = zn_le(n920, zn_splat(P8::from_raw(8388608i32)));
    let n1593: ZB = zb_and(n1590, n1592);
    let n1594: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n927);
    let n1595: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n928);
    let n1596: ZB = zn_tile_flag_at(g.cache, g.cart, n1594, n1595, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1597: ZB = zb_not(n1596);
    let n1598: ZB = zb_and(n1593, n1597);
    let n1599: ZB = zb_and(n1593, n1596);
    let n1600: ZB = zb_or(n1598, n1599);
    let n1601: ZB = zb_and(n1597, n1600);
    let n1602: ZB = zb_and(n1596, n1600);
    let n1603: ZB = zb_or(n1601, n1602);
    let n1604: ZB = zb_and(n1596, n1603);
    let n1605: ZB = zb_and(n1597, n1603);
    let n1606: ZB = zb_and(n296, n1605);
    let n1607: ZB = zb_and(n297, n1605);
    let n1608: ZB = zb_or(n1606, n1607);
    let n1609: ZN = zsel_n(n1596, zn_splat(P8::from_raw(393216i32)), n299);
    let n1610: ZB = zb_or(n1604, n1608);
    let n1611: ZB = zb_and(n301, n1610);
    let n1612: ZB = zb_and(n302, n1610);
    let n1613: ZB = zn_gt(n923, r_c398);
    let n1614: ZB = zn_le(n923, r_c398);
    let n1615: ZB = zb_and(n1611, n1613);
    let n1616: ZB = zb_and(n1611, n1614);
    let n1617: ZN = zn_sub(n923, r_c396);
    let n1618: ZN = zn_max(r_c398, n1617);
    let n1619: ZN = zn_add(r_c396, n923);
    let n1620: ZN = zn_min(r_c398, n1619);
    let n1621: ZN = zsel_n(n1613, n1618, n1620);
    let n1622: ZB = zb_or(n1615, n1616);
    let n1623: ZB = zn_gt(n924, r_c399);
    let n1624: ZB = zn_le(n924, r_c399);
    let n1625: ZB = zb_and(n1622, n1623);
    let n1626: ZB = zb_and(n1622, n1624);
    let n1627: ZN = zn_sub(n924, r_c397);
    let n1628: ZN = zn_max(r_c399, n1627);
    let n1629: ZN = zn_add(r_c397, n924);
    let n1630: ZN = zn_min(r_c399, n1629);
    let n1631: ZN = zsel_n(n1623, n1628, n1630);
    let n1632: ZB = zb_or(n1625, n1626);
    let n1633: ZB = zb_and(n1597, n1612);
    let n1634: ZB = zb_and(n1596, n1612);
    let n1635: ZN = zsel_n(n1597, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1636: ZB = zb_or(n1633, n1634);
    let n1637: ZN = zn_abs(n923);
    let n1638: ZB = zn_gt(n1637, zn_splat(P8::from_raw(65536i32)));
    let n1639: ZB = zn_le(n1637, zn_splat(P8::from_raw(65536i32)));
    let n1640: ZB = zb_and(n1636, n1638);
    let n1641: ZB = zb_and(n1636, n1639);
    let n1642: ZB = zn_gt(n923, zn_splat(P8::from_raw(0i32)));
    let n1643: ZB = zb_and(n1640, n1642);
    let n1644: ZB = zb_and(n1007, n1640);
    let n1645: ZB = zn_lt(n923, zn_splat(P8::from_raw(0i32)));
    let n1646: ZB = zb_and(n1644, n1645);
    let n1647: ZB = zb_and(n1031, n1644);
    let n1648: ZB = zn_gt(n923, zn_splat(P8::from_raw(65536i32)));
    let n1649: ZB = zn_le(n923, zn_splat(P8::from_raw(65536i32)));
    let n1650: ZB = zb_and(n1643, n1648);
    let n1651: ZB = zb_and(n1643, n1649);
    let n1652: ZN = zn_sub(n923, zn_splat(P8::from_raw(9830i32)));
    let n1653: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1652);
    let n1654: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n923);
    let n1655: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1654);
    let n1656: ZB = zn_gt(n923, zn_splat(P8::from_raw(-65536i32)));
    let n1657: ZB = zn_le(n923, zn_splat(P8::from_raw(-65536i32)));
    let n1658: ZB = zb_and(n1646, n1656);
    let n1659: ZB = zb_and(n1646, n1657);
    let n1660: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1652);
    let n1661: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1654);
    let n1662: ZB = zb_and(n1007, n1647);
    let n1663: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1652);
    let n1664: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1654);
    let n1665: ZN = zsel_n(n1656, n1660, n1661);
    let n1666: ZB = zb_or(n1658, n1659);
    let n1667: ZN = zsel_n(n1642, n1663, n1664);
    let n1668: ZN = zsel_n(n1648, n1653, n1655);
    let n1669: ZB = zb_or(n1650, n1651);
    let n1670: ZN = zsel_n(n1645, n1665, n1667);
    let n1671: ZB = zb_or(n1662, n1666);
    let n1672: ZN = zsel_n(n1642, n1668, n1670);
    let n1673: ZB = zb_or(n1669, n1671);
    let n1674: ZB = zb_and(n1641, n1642);
    let n1675: ZB = zb_and(n1007, n1641);
    let n1676: ZN = zn_sub(n923, n1635);
    let n1677: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1676);
    let n1678: ZN = zn_add(n923, n1635);
    let n1679: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1678);
    let n1680: ZN = zsel_n(n1642, n1677, n1679);
    let n1681: ZB = zb_or(n1674, n1675);
    let n1682: ZN = zsel_n(n1638, n1672, n1680);
    let n1683: ZB = zb_or(n1673, n1681);
    let n1684: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1682);
    let n1685: ZB = zb_not(n1684);
    let n1686: ZB = zb_and(n1683, n1685);
    let n1687: ZB = zb_and(n1683, n1684);
    let n1688: ZB = zn_lt(n1682, zn_splat(P8::from_raw(0i32)));
    let n1689: ZB = zsel_b(n1685, n1688, r_c400);
    let n1690: ZB = zb_or(n1686, n1687);
    let n1691: ZN = zn_abs(n924);
    let n1692: ZB = zn_le(n1691, zn_splat(P8::from_raw(9830i32)));
    let n1693: ZB = zn_gt(n1691, zn_splat(P8::from_raw(9830i32)));
    let n1694: ZB = zb_and(n1690, n1692);
    let n1695: ZB = zb_and(n1690, n1693);
    let n1696: ZN = zsel_n(n1692, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1697: ZB = zb_or(n1694, n1695);
    let n1698: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n928);
    let n1699: ZB = zb_and(n1597, n1697);
    let n1700: ZB = zb_and(n1596, n1697);
    let n1701: ZB = zn_gt(n924, zn_splat(P8::from_raw(131072i32)));
    let n1702: ZB = zn_le(n924, zn_splat(P8::from_raw(131072i32)));
    let n1703: ZB = zb_and(n1699, n1701);
    let n1704: ZB = zb_and(n1699, n1702);
    let n1705: ZN = zn_sub(n924, n1696);
    let n1706: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n1705);
    let n1707: ZN = zn_add(n924, n1696);
    let n1708: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1707);
    let n1709: ZN = zsel_n(n1701, n1706, n1708);
    let n1710: ZB = zb_or(n1703, n1704);
    let n1711: ZN = zsel_n(n1597, n1709, n924);
    let n1712: ZB = zb_or(n1700, n1710);
    let n1713: ZB = zn_gt(n1609, zn_splat(P8::from_raw(0i32)));
    let n1714: ZB = zn_le(n1609, zn_splat(P8::from_raw(0i32)));
    let n1715: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n927);
    let n1716: ZB = zn_tile_flag_at(g.cache, g.cart, n1715, n1698, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1717: ZB = zb_not(n1716);
    let n1718: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n927);
    let n1719: ZB = zn_tile_flag_at(g.cache, g.cart, n1718, n1698, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1720: ZB = zb_not(n1719);
    let n1721: ZN = zsel_n(n1719, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1722: ZN = zsel_n(n1716, zn_splat(P8::from_raw(-65536i32)), n1721);
    let n1723: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1722);
    let n1724: ZB = zb_not(n1723);
    let n1725: ZN = zn_neg(n1722);
    let n1726: ZN = zn_mul(n1725, zn_splat(P8::from_raw(131072i32)));
    let n1727: ZN = zsel_n(n1724, n1726, n1682);
    let n1728: ZN = zsel_n(n1724, zn_splat(P8::from_raw(-131072i32)), n1711);
    let n1729: ZN = zsel_n(n1713, zn_splat(P8::from_raw(0i32)), n1609);
    let n1730: ZN = zsel_n(n1713, n1682, n1727);
    let n1731: ZN = zsel_n(n1713, zn_splat(P8::from_raw(-131072i32)), n1728);
    let n1732: ZB = zb_not(n1689);
    let n1733: ZN = zsel_n(n1689, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1734: ZB = zn_gt(n1733, zn_splat(P8::from_raw(0i32)));
    let n1735: ZB = zn_le(n1733, zn_splat(P8::from_raw(0i32)));
    let n1736: ZB = zn_lt(n1733, zn_splat(P8::from_raw(0i32)));
    let n1737: ZB = zn_ge(n1733, zn_splat(P8::from_raw(0i32)));
    let n1738: ZN = zsel_n(n1736, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1739: ZN = zsel_n(n1734, zn_splat(P8::from_raw(131072i32)), n1738);
    let n1740: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1733);
    let n1741: ZB = zb_not(n1740);
    let n1742: ZN = zsel_n(n1741, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1743: ZB = zsel_b(n301, r_c400, n1689);
    let n1744: ZN = zsel_n(n301, n1621, n1682);
    let n1745: ZN = zsel_n(n301, n1631, n1711);
    let n1746: ZB = zb_or(n1632, n1712);
    let n1747: ZB = zn_lt(n920, zn_splat(P8::from_raw(-262144i32)));
    let n1748: ZB = zn_ge(n920, zn_splat(P8::from_raw(-262144i32)));
    let n1749: ZB = zb_and(n1746, n1747);
    let n1750: ZB = zb_and(n1746, n1748);
    let n1751: ZB = zb_or(n1749, n1750);
    let n1752: ZB = zb_and(n1748, n1751);
    let n1753: ZB = zb_and(n172, n1752);
    let n1754: ZB = zn_lt(n919, zn_splat(P8::from_raw(-65536i32)));
    let n1755: ZB = zn_ge(n919, zn_splat(P8::from_raw(-65536i32)));
    let n1756: ZB = zb_and(n1753, n1755);
    let n1757: ZB = zb_and(n1753, n1754);
    let n1758: ZB = zn_gt(n919, zn_splat(P8::from_raw(7929856i32)));
    let n1759: ZB = zb_or(n1756, n1757);
    let n1760: ZB = zb_or(n1754, n1758);
    let n1761: ZB = zb_not(n1760);
    let n1762: ZB = zb_and(n1759, n1760);
    let n1763: ZB = zb_and(n1759, n1761);
    let n1764: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n919);
    let n1765: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1764);
    let n1766: ZN = zsel_n(n1760, n1765, n919);
    let n1767: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1744);
    let n1768: ZB = zb_or(n1762, n1763);
    let n1769: ZN = zsel_n(n305, n919, n1766);
    let n1770: ZN = zsel_n(n305, n1744, n1767);
    let n1783: ZB = zb_and(n1641, n1656);
    let n1784: ZB = zb_and(n1641, n1657);
    let n1785: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1676);
    let n1786: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1678);
    let n1787: ZN = zsel_n(n1656, n1785, n1786);
    let n1788: ZB = zb_or(n1783, n1784);
    let n1789: ZN = zsel_n(n1638, n1672, n1787);
    let n1790: ZB = zb_or(n1673, n1788);
    let n1791: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1789);
    let n1792: ZB = zb_not(n1791);
    let n1793: ZB = zb_and(n1790, n1792);
    let n1794: ZB = zb_and(n1790, n1791);
    let n1795: ZB = zn_lt(n1789, zn_splat(P8::from_raw(0i32)));
    let n1796: ZB = zsel_b(n1792, n1795, r_c400);
    let n1797: ZB = zb_or(n1793, n1794);
    let n1798: ZB = zb_and(n1692, n1797);
    let n1799: ZB = zb_and(n1693, n1797);
    let n1800: ZB = zb_or(n1798, n1799);
    let n1801: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n927);
    let n1802: ZB = zn_tile_flag_at(g.cache, g.cart, n1801, n1698, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1803: ZB = zb_not(n1802);
    let n1804: ZB = zb_and(n1800, n1803);
    let n1805: ZB = zb_and(n1800, n1802);
    let n1806: ZB = zb_or(n1804, n1805);
    let n1807: ZB = zb_and(n1803, n1806);
    let n1808: ZB = zb_and(n1802, n1806);
    let n1809: ZB = zb_or(n1807, n1808);
    let n1810: ZB = zb_and(n1802, n1809);
    let n1811: ZB = zb_and(n1803, n1809);
    let n1812: ZB = zb_or(n1810, n1811);
    let n1813: ZB = zb_and(n1802, n1812);
    let n1814: ZB = zb_and(n1803, n1812);
    let n1815: ZN = zsel_n(n1802, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1816: ZB = zb_or(n1813, n1814);
    let n1817: ZB = zb_and(n1597, n1816);
    let n1818: ZB = zb_and(n1596, n1816);
    let n1819: ZB = zn_gt(n924, n1815);
    let n1820: ZB = zn_le(n924, n1815);
    let n1821: ZB = zb_and(n1817, n1819);
    let n1822: ZB = zb_and(n1817, n1820);
    let n1823: ZN = zn_max(n1705, n1815);
    let n1824: ZN = zn_min(n1707, n1815);
    let n1825: ZN = zsel_n(n1819, n1823, n1824);
    let n1826: ZB = zb_or(n1821, n1822);
    let n1827: ZN = zsel_n(n1597, n1825, n924);
    let n1828: ZB = zb_or(n1818, n1826);
    let n1829: ZN = zsel_n(n1724, n1726, n1789);
    let n1830: ZN = zsel_n(n1724, zn_splat(P8::from_raw(-131072i32)), n1827);
    let n1831: ZN = zsel_n(n1713, n1789, n1829);
    let n1832: ZN = zsel_n(n1713, zn_splat(P8::from_raw(-131072i32)), n1830);
    let n1833: ZB = zsel_b(n301, r_c400, n1796);
    let n1834: ZN = zsel_n(n301, n1621, n1789);
    let n1835: ZN = zsel_n(n301, n1631, n1827);
    let n1836: ZB = zb_or(n1632, n1828);
    let n1837: ZB = zb_and(n1747, n1836);
    let n1838: ZB = zb_and(n1748, n1836);
    let n1839: ZB = zb_or(n1837, n1838);
    let n1840: ZB = zb_and(n1748, n1839);
    let n1841: ZB = zb_and(n172, n1840);
    let n1842: ZB = zb_and(n1755, n1841);
    let n1843: ZB = zb_and(n1754, n1841);
    let n1844: ZB = zb_or(n1842, n1843);
    let n1845: ZB = zb_and(n1760, n1844);
    let n1846: ZB = zb_and(n1761, n1844);
    let n1847: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1834);
    let n1848: ZB = zb_or(n1845, n1846);
    let n1849: ZN = zsel_n(n305, n1834, n1847);
    let n1850: ZB = zb_and(n1641, n1648);
    let n1851: ZB = zb_and(n1641, n1649);
    let n1852: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1676);
    let n1853: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1678);
    let n1854: ZN = zsel_n(n1648, n1852, n1853);
    let n1855: ZB = zb_or(n1850, n1851);
    let n1856: ZN = zsel_n(n1638, n1672, n1854);
    let n1857: ZB = zb_or(n1673, n1855);
    let n1858: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1856);
    let n1859: ZB = zb_not(n1858);
    let n1860: ZB = zb_and(n1857, n1859);
    let n1861: ZB = zb_and(n1857, n1858);
    let n1862: ZB = zn_lt(n1856, zn_splat(P8::from_raw(0i32)));
    let n1863: ZB = zsel_b(n1859, n1862, r_c400);
    let n1864: ZB = zb_or(n1860, n1861);
    let n1865: ZB = zb_and(n1692, n1864);
    let n1866: ZB = zb_and(n1693, n1864);
    let n1867: ZB = zb_or(n1865, n1866);
    let n1868: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n927);
    let n1869: ZB = zn_tile_flag_at(g.cache, g.cart, n1868, n1698, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1870: ZB = zb_not(n1869);
    let n1871: ZB = zb_and(n1867, n1870);
    let n1872: ZB = zb_and(n1867, n1869);
    let n1873: ZB = zb_or(n1871, n1872);
    let n1874: ZB = zb_and(n1870, n1873);
    let n1875: ZB = zb_and(n1869, n1873);
    let n1876: ZB = zb_or(n1874, n1875);
    let n1877: ZB = zb_and(n1869, n1876);
    let n1878: ZB = zb_and(n1870, n1876);
    let n1879: ZB = zb_or(n1877, n1878);
    let n1880: ZB = zb_and(n1869, n1879);
    let n1881: ZB = zb_and(n1870, n1879);
    let n1882: ZN = zsel_n(n1869, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1883: ZB = zb_or(n1880, n1881);
    let n1884: ZB = zb_and(n1597, n1883);
    let n1885: ZB = zb_and(n1596, n1883);
    let n1886: ZB = zn_gt(n924, n1882);
    let n1887: ZB = zn_le(n924, n1882);
    let n1888: ZB = zb_and(n1884, n1886);
    let n1889: ZB = zb_and(n1884, n1887);
    let n1890: ZN = zn_max(n1705, n1882);
    let n1891: ZN = zn_min(n1707, n1882);
    let n1892: ZN = zsel_n(n1886, n1890, n1891);
    let n1893: ZB = zb_or(n1888, n1889);
    let n1894: ZN = zsel_n(n1597, n1892, n924);
    let n1895: ZB = zb_or(n1885, n1893);
    let n1896: ZN = zsel_n(n1724, n1726, n1856);
    let n1897: ZN = zsel_n(n1724, zn_splat(P8::from_raw(-131072i32)), n1894);
    let n1898: ZN = zsel_n(n1713, n1856, n1896);
    let n1899: ZN = zsel_n(n1713, zn_splat(P8::from_raw(-131072i32)), n1897);
    let n1900: ZB = zsel_b(n301, r_c400, n1863);
    let n1901: ZN = zsel_n(n301, n1621, n1856);
    let n1902: ZN = zsel_n(n301, n1631, n1894);
    let n1903: ZB = zb_or(n1632, n1895);
    let n1904: ZB = zb_and(n1747, n1903);
    let n1905: ZB = zb_and(n1748, n1903);
    let n1906: ZB = zb_or(n1904, n1905);
    let n1907: ZB = zb_and(n1748, n1906);
    let n1908: ZB = zb_and(n172, n1907);
    let n1909: ZB = zb_and(n1755, n1908);
    let n1910: ZB = zb_and(n1754, n1908);
    let n1911: ZB = zb_or(n1909, n1910);
    let n1912: ZB = zb_and(n1760, n1911);
    let n1913: ZB = zb_and(n1761, n1911);
    let n1914: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1901);
    let n1915: ZB = zb_or(n1912, n1913);
    let n1916: ZN = zsel_n(n305, n1901, n1914);
    let n1917: ZB = zb_and(n295, n1712);
    let n1918: ZB = zb_and(r_c312, n1712);
    let n1919: ZB = zb_and(n1713, n1917);
    let n1920: ZB = zb_and(n1714, n1917);
    let n1921: ZB = zb_and(n1717, n1920);
    let n1922: ZB = zb_and(n1716, n1920);
    let n1923: ZB = zb_or(n1921, n1922);
    let n1924: ZB = zb_and(n1717, n1923);
    let n1925: ZB = zb_and(n1716, n1923);
    let n1926: ZB = zb_or(n1924, n1925);
    let n1927: ZB = zb_and(n1716, n1926);
    let n1928: ZB = zb_and(n1717, n1926);
    let n1929: ZB = zb_and(n1720, n1928);
    let n1930: ZB = zb_and(n1719, n1928);
    let n1931: ZB = zb_or(n1929, n1930);
    let n1932: ZB = zb_and(n1720, n1931);
    let n1933: ZB = zb_and(n1719, n1931);
    let n1934: ZB = zb_or(n1932, n1933);
    let n1935: ZB = zb_and(n1719, n1934);
    let n1936: ZB = zb_and(n1720, n1934);
    let n1937: ZB = zb_or(n1935, n1936);
    let n1938: ZB = zb_or(n1927, n1937);
    let n1939: ZB = zb_and(n1724, n1938);
    let n1940: ZB = zb_and(n1723, n1938);
    let n1941: ZB = zb_or(n1939, n1940);
    let n1942: ZB = zb_or(n1919, n1941);
    let n1943: ZN = zsel_n(n295, n1729, n1609);
    let n1944: ZN = zsel_n(n295, n1730, n1682);
    let n1945: ZN = zsel_n(n295, n1731, n1711);
    let n1946: ZB = zb_or(n1918, n1942);
    let n1947: ZN = zsel_n(n301, n1609, n1943);
    let n1948: ZN = zsel_n(n301, n1621, n1944);
    let n1949: ZN = zsel_n(n301, n1631, n1945);
    let n1950: ZB = zb_or(n1632, n1946);
    let n1951: ZB = zb_and(n1747, n1950);
    let n1952: ZB = zb_and(n1748, n1950);
    let n1953: ZB = zb_or(n1951, n1952);
    let n1954: ZB = zb_and(n1748, n1953);
    let n1955: ZB = zb_and(n172, n1954);
    let n1956: ZB = zb_and(n1755, n1955);
    let n1957: ZB = zb_and(n1754, n1955);
    let n1958: ZB = zb_or(n1956, n1957);
    let n1959: ZB = zb_and(n1760, n1958);
    let n1960: ZB = zb_and(n1761, n1958);
    let n1961: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1948);
    let n1962: ZB = zb_or(n1959, n1960);
    let n1963: ZN = zsel_n(n305, n1948, n1961);
    let n1964: ZB = zb_and(n295, n1828);
    let n1965: ZB = zb_and(r_c312, n1828);
    let n1966: ZB = zb_and(n1713, n1964);
    let n1967: ZB = zb_and(n1714, n1964);
    let n1968: ZB = zb_and(n1717, n1967);
    let n1969: ZB = zb_and(n1716, n1967);
    let n1970: ZB = zb_or(n1968, n1969);
    let n1971: ZB = zb_and(n1717, n1970);
    let n1972: ZB = zb_and(n1716, n1970);
    let n1973: ZB = zb_or(n1971, n1972);
    let n1974: ZB = zb_and(n1716, n1973);
    let n1975: ZB = zb_and(n1717, n1973);
    let n1976: ZB = zb_and(n1720, n1975);
    let n1977: ZB = zb_and(n1719, n1975);
    let n1978: ZB = zb_or(n1976, n1977);
    let n1979: ZB = zb_and(n1720, n1978);
    let n1980: ZB = zb_and(n1719, n1978);
    let n1981: ZB = zb_or(n1979, n1980);
    let n1982: ZB = zb_and(n1719, n1981);
    let n1983: ZB = zb_and(n1720, n1981);
    let n1984: ZB = zb_or(n1982, n1983);
    let n1985: ZB = zb_or(n1974, n1984);
    let n1986: ZB = zb_and(n1724, n1985);
    let n1987: ZB = zb_and(n1723, n1985);
    let n1988: ZB = zb_or(n1986, n1987);
    let n1989: ZB = zb_or(n1966, n1988);
    let n1990: ZN = zsel_n(n295, n1831, n1789);
    let n1991: ZN = zsel_n(n295, n1832, n1827);
    let n1992: ZB = zb_or(n1965, n1989);
    let n1993: ZN = zsel_n(n301, n1621, n1990);
    let n1994: ZN = zsel_n(n301, n1631, n1991);
    let n1995: ZB = zb_or(n1632, n1992);
    let n1996: ZB = zb_and(n1747, n1995);
    let n1997: ZB = zb_and(n1748, n1995);
    let n1998: ZB = zb_or(n1996, n1997);
    let n1999: ZB = zb_and(n1748, n1998);
    let n2000: ZB = zb_and(n172, n1999);
    let n2001: ZB = zb_and(n1755, n2000);
    let n2002: ZB = zb_and(n1754, n2000);
    let n2003: ZB = zb_or(n2001, n2002);
    let n2004: ZB = zb_and(n1760, n2003);
    let n2005: ZB = zb_and(n1761, n2003);
    let n2006: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1993);
    let n2007: ZB = zb_or(n2004, n2005);
    let n2008: ZN = zsel_n(n305, n1993, n2006);
    let n2009: ZB = zb_and(n295, n1895);
    let n2010: ZB = zb_and(r_c312, n1895);
    let n2011: ZB = zb_and(n1713, n2009);
    let n2012: ZB = zb_and(n1714, n2009);
    let n2013: ZB = zb_and(n1717, n2012);
    let n2014: ZB = zb_and(n1716, n2012);
    let n2015: ZB = zb_or(n2013, n2014);
    let n2016: ZB = zb_and(n1717, n2015);
    let n2017: ZB = zb_and(n1716, n2015);
    let n2018: ZB = zb_or(n2016, n2017);
    let n2019: ZB = zb_and(n1716, n2018);
    let n2020: ZB = zb_and(n1717, n2018);
    let n2021: ZB = zb_and(n1720, n2020);
    let n2022: ZB = zb_and(n1719, n2020);
    let n2023: ZB = zb_or(n2021, n2022);
    let n2024: ZB = zb_and(n1720, n2023);
    let n2025: ZB = zb_and(n1719, n2023);
    let n2026: ZB = zb_or(n2024, n2025);
    let n2027: ZB = zb_and(n1719, n2026);
    let n2028: ZB = zb_and(n1720, n2026);
    let n2029: ZB = zb_or(n2027, n2028);
    let n2030: ZB = zb_or(n2019, n2029);
    let n2031: ZB = zb_and(n1724, n2030);
    let n2032: ZB = zb_and(n1723, n2030);
    let n2033: ZB = zb_or(n2031, n2032);
    let n2034: ZB = zb_or(n2011, n2033);
    let n2035: ZN = zsel_n(n295, n1898, n1856);
    let n2036: ZN = zsel_n(n295, n1899, n1894);
    let n2037: ZB = zb_or(n2010, n2034);
    let n2038: ZN = zsel_n(n301, n1621, n2035);
    let n2039: ZN = zsel_n(n301, n1631, n2036);
    let n2040: ZB = zb_or(n1632, n2037);
    let n2041: ZB = zb_and(n1747, n2040);
    let n2042: ZB = zb_and(n1748, n2040);
    let n2043: ZB = zb_or(n2041, n2042);
    let n2044: ZB = zb_and(n1748, n2043);
    let n2045: ZB = zb_and(n172, n2044);
    let n2046: ZB = zb_and(n1755, n2045);
    let n2047: ZB = zb_and(n1754, n2045);
    let n2048: ZB = zb_or(n2046, n2047);
    let n2049: ZB = zb_and(n1760, n2048);
    let n2050: ZB = zb_and(n1761, n2048);
    let n2051: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2038);
    let n2052: ZB = zb_or(n2049, n2050);
    let n2053: ZN = zsel_n(n305, n2038, n2051);
    let n2054: ZN = zsel_n(n233, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2055: ZB = zb_or(r_c41, n233);
    let n2056: ZN = zsel_n(n233, zn_splat(P8::from_raw(655360i32)), n300);
    let n2057: ZN = zsel_n(n233, zn_splat(P8::from_raw(262144i32)), r_c301);
    let n2058: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n2059: ZN = zsel_n(n233, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n2060: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), r_c399);
    let n2061: ZN = zsel_n(n301, r_c20, n2054);
    let n2062: ZB = zsel_b(n301, r_c41, n2055);
    let n2063: ZN = zsel_n(n301, n300, n2056);
    let n2064: ZN = zsel_n(n301, n303, n2057);
    let n2065: ZN = zsel_n(n301, zn_splat(P8::from_raw(65536i32)), n2058);
    let n2066: ZN = zsel_n(n301, r_c396, n2059);
    let n2067: ZN = zsel_n(n301, r_c399, n2060);
    let n2068: ZB = zn_gt(n2061, zn_splat(P8::from_raw(0i32)));
    let n2069: ZB = zn_le(n2061, zn_splat(P8::from_raw(0i32)));
    let n2070: ZB = zb_and(n233, n1712);
    let n2071: ZB = zb_and(r_c311, n1712);
    let n2072: ZB = zb_and(n1689, n2070);
    let n2073: ZB = zb_and(n1732, n2070);
    let n2074: ZB = zb_or(n2072, n2073);
    let n2075: ZB = zb_and(n1734, n2074);
    let n2076: ZB = zb_and(n1735, n2074);
    let n2077: ZB = zb_and(n1736, n2076);
    let n2078: ZB = zb_and(n1737, n2076);
    let n2079: ZB = zb_or(n2077, n2078);
    let n2080: ZB = zb_or(n2075, n2079);
    let n2081: ZB = zb_and(n1741, n2080);
    let n2082: ZB = zb_and(n1740, n2080);
    let n2083: ZB = zb_or(n2081, n2082);
    let n2084: ZN = zsel_n(n233, n1742, r_c397);
    let n2085: ZN = zsel_n(n233, n1739, r_c398);
    let n2086: ZN = zsel_n(n233, n1733, n1682);
    let n2087: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1711);
    let n2088: ZB = zb_or(n2071, n2083);
    let n2089: ZN = zsel_n(n301, r_c397, n2084);
    let n2090: ZN = zsel_n(n301, r_c398, n2085);
    let n2091: ZN = zsel_n(n301, n1621, n2086);
    let n2092: ZN = zsel_n(n301, n1631, n2087);
    let n2093: ZB = zb_or(n1632, n2088);
    let n2094: ZB = zb_and(n1747, n2093);
    let n2095: ZB = zb_and(n1748, n2093);
    let n2096: ZB = zb_or(n2094, n2095);
    let n2097: ZB = zb_and(n1748, n2096);
    let n2098: ZB = zb_and(n2068, n2097);
    let n2099: ZB = zb_and(n2069, n2097);
    let n2100: ZB = zb_and(n1755, n2099);
    let n2101: ZB = zb_and(n1754, n2099);
    let n2102: ZB = zb_or(n2100, n2101);
    let n2103: ZB = zb_and(n1760, n2102);
    let n2104: ZB = zb_and(n1761, n2102);
    let n2105: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2091);
    let n2106: ZB = zb_or(n2103, n2104);
    let n2107: ZN = zsel_n(n2068, n919, n1766);
    let n2108: ZN = zsel_n(n2068, n2091, n2105);
    let n2109: ZB = zb_or(n2098, n2106);
    let n2110: ZB = zb_and(n233, n1828);
    let n2111: ZB = zb_and(r_c311, n1828);
    let n2112: ZN = zsel_n(n233, zn_splat(P8::from_raw(69510i32)), r_c397);
    let n2113: ZN = zsel_n(n233, zn_splat(P8::from_raw(-131072i32)), r_c398);
    let n2114: ZN = zsel_n(n233, zn_splat(P8::from_raw(-327680i32)), n1789);
    let n2115: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1827);
    let n2116: ZB = zb_or(n2110, n2111);
    let n2117: ZN = zsel_n(n301, r_c397, n2112);
    let n2118: ZN = zsel_n(n301, r_c398, n2113);
    let n2119: ZN = zsel_n(n301, n1621, n2114);
    let n2120: ZN = zsel_n(n301, n1631, n2115);
    let n2121: ZB = zb_or(n1632, n2116);
    let n2122: ZB = zb_and(n1747, n2121);
    let n2123: ZB = zb_and(n1748, n2121);
    let n2124: ZB = zb_or(n2122, n2123);
    let n2125: ZB = zb_and(n1748, n2124);
    let n2126: ZB = zb_and(n2068, n2125);
    let n2127: ZB = zb_and(n2069, n2125);
    let n2128: ZB = zb_and(n1755, n2127);
    let n2129: ZB = zb_and(n1754, n2127);
    let n2130: ZB = zb_or(n2128, n2129);
    let n2131: ZB = zb_and(n1760, n2130);
    let n2132: ZB = zb_and(n1761, n2130);
    let n2133: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2119);
    let n2134: ZB = zb_or(n2131, n2132);
    let n2135: ZN = zsel_n(n2068, n2119, n2133);
    let n2136: ZB = zb_or(n2126, n2134);
    let n2137: ZB = zb_and(n233, n1895);
    let n2138: ZB = zb_and(r_c311, n1895);
    let n2139: ZN = zsel_n(n233, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n2140: ZN = zsel_n(n233, zn_splat(P8::from_raw(327680i32)), n1856);
    let n2141: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1894);
    let n2142: ZB = zb_or(n2137, n2138);
    let n2143: ZN = zsel_n(n301, r_c398, n2139);
    let n2144: ZN = zsel_n(n301, n1621, n2140);
    let n2145: ZN = zsel_n(n301, n1631, n2141);
    let n2146: ZB = zb_or(n1632, n2142);
    let n2147: ZB = zb_and(n1747, n2146);
    let n2148: ZB = zb_and(n1748, n2146);
    let n2149: ZB = zb_or(n2147, n2148);
    let n2150: ZB = zb_and(n1748, n2149);
    let n2151: ZB = zb_and(n2068, n2150);
    let n2152: ZB = zb_and(n2069, n2150);
    let n2153: ZB = zb_and(n1755, n2152);
    let n2154: ZB = zb_and(n1754, n2152);
    let n2155: ZB = zb_or(n2153, n2154);
    let n2156: ZB = zb_and(n1760, n2155);
    let n2157: ZB = zb_and(n1761, n2155);
    let n2158: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2144);
    let n2159: ZB = zb_or(n2156, n2157);
    let n2160: ZN = zsel_n(n2068, n2144, n2158);
    let n2161: ZB = zb_or(n2151, n2159);
    let n2163: ZN = zsel_n(n233, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n2164: ZN = zsel_n(n233, zn_splat(P8::from_raw(-98304i32)), r_c399);
    let n2165: ZN = zsel_n(n301, r_c396, n2163);
    let n2166: ZN = zsel_n(n301, r_c399, n2164);
    let n2167: ZN = zsel_n(n233, zn_splat(P8::from_raw(98304i32)), r_c397);
    let n2168: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), r_c398);
    let n2169: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1682);
    let n2170: ZN = zsel_n(n233, zn_splat(P8::from_raw(-327680i32)), n1711);
    let n2171: ZB = zb_or(n2070, n2071);
    let n2172: ZN = zsel_n(n301, r_c397, n2167);
    let n2173: ZN = zsel_n(n301, r_c398, n2168);
    let n2174: ZN = zsel_n(n301, n1621, n2169);
    let n2175: ZN = zsel_n(n301, n1631, n2170);
    let n2176: ZB = zb_or(n1632, n2171);
    let n2177: ZB = zb_and(n1747, n2176);
    let n2178: ZB = zb_and(n1748, n2176);
    let n2179: ZB = zb_or(n2177, n2178);
    let n2180: ZB = zb_and(n1748, n2179);
    let n2181: ZB = zb_and(n2068, n2180);
    let n2182: ZB = zb_and(n2069, n2180);
    let n2183: ZB = zb_and(n1755, n2182);
    let n2184: ZB = zb_and(n1754, n2182);
    let n2185: ZB = zb_or(n2183, n2184);
    let n2186: ZB = zb_and(n1760, n2185);
    let n2187: ZB = zb_and(n1761, n2185);
    let n2188: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2174);
    let n2189: ZB = zb_or(n2186, n2187);
    let n2190: ZN = zsel_n(n2068, n2174, n2188);
    let n2191: ZB = zb_or(n2181, n2189);
    let n2192: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n1789);
    let n2193: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n1827);
    let n2194: ZN = zsel_n(n301, n1621, n2192);
    let n2195: ZN = zsel_n(n301, n1631, n2193);
    let n2196: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2194);
    let n2197: ZN = zsel_n(n2068, n2194, n2196);
    let n2198: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n1856);
    let n2199: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n1894);
    let n2200: ZN = zsel_n(n301, n1621, n2198);
    let n2201: ZN = zsel_n(n301, n1631, n2199);
    let n2202: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2200);
    let n2203: ZN = zsel_n(n2068, n2200, n2202);
    let n2204: ZN = zsel_n(n233, zn_splat(P8::from_raw(131072i32)), r_c399);
    let n2205: ZN = zsel_n(n301, r_c399, n2204);
    let n2206: ZN = zsel_n(n233, zn_splat(P8::from_raw(327680i32)), n1711);
    let n2207: ZN = zsel_n(n301, n1631, n2206);
    let n2208: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n1827);
    let n2209: ZN = zsel_n(n301, n1631, n2208);
    let n2210: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n1894);
    let n2211: ZN = zsel_n(n301, n1631, n2210);
    let n2212: ZB = zb_and(n233, n1946);
    let n2213: ZB = zb_and(r_c311, n1946);
    let n2214: ZB = zb_and(n1689, n2212);
    let n2215: ZB = zb_and(n1732, n2212);
    let n2216: ZB = zb_or(n2214, n2215);
    let n2217: ZB = zb_and(n1734, n2216);
    let n2218: ZB = zb_and(n1735, n2216);
    let n2219: ZB = zb_and(n1736, n2218);
    let n2220: ZB = zb_and(n1737, n2218);
    let n2221: ZB = zb_or(n2219, n2220);
    let n2222: ZB = zb_or(n2217, n2221);
    let n2223: ZB = zb_and(n1741, n2222);
    let n2224: ZB = zb_and(n1740, n2222);
    let n2225: ZB = zb_or(n2223, n2224);
    let n2226: ZN = zsel_n(n233, n1733, n1944);
    let n2227: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1945);
    let n2228: ZB = zb_or(n2213, n2225);
    let n2229: ZN = zsel_n(n301, n1621, n2226);
    let n2230: ZN = zsel_n(n301, n1631, n2227);
    let n2231: ZB = zb_or(n1632, n2228);
    let n2232: ZB = zb_and(n1747, n2231);
    let n2233: ZB = zb_and(n1748, n2231);
    let n2234: ZB = zb_or(n2232, n2233);
    let n2235: ZB = zb_and(n1748, n2234);
    let n2236: ZB = zb_and(n2068, n2235);
    let n2237: ZB = zb_and(n2069, n2235);
    let n2238: ZB = zb_and(n1755, n2237);
    let n2239: ZB = zb_and(n1754, n2237);
    let n2240: ZB = zb_or(n2238, n2239);
    let n2241: ZB = zb_and(n1760, n2240);
    let n2242: ZB = zb_and(n1761, n2240);
    let n2243: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2229);
    let n2244: ZB = zb_or(n2241, n2242);
    let n2245: ZN = zsel_n(n2068, n2229, n2243);
    let n2246: ZB = zb_or(n2236, n2244);
    let n2247: ZB = zb_and(n233, n1992);
    let n2248: ZB = zb_and(r_c311, n1992);
    let n2249: ZN = zsel_n(n233, zn_splat(P8::from_raw(-327680i32)), n1990);
    let n2250: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1991);
    let n2251: ZB = zb_or(n2247, n2248);
    let n2252: ZN = zsel_n(n301, n1621, n2249);
    let n2253: ZN = zsel_n(n301, n1631, n2250);
    let n2254: ZB = zb_or(n1632, n2251);
    let n2255: ZB = zb_and(n1747, n2254);
    let n2256: ZB = zb_and(n1748, n2254);
    let n2257: ZB = zb_or(n2255, n2256);
    let n2258: ZB = zb_and(n1748, n2257);
    let n2259: ZB = zb_and(n2068, n2258);
    let n2260: ZB = zb_and(n2069, n2258);
    let n2261: ZB = zb_and(n1755, n2260);
    let n2262: ZB = zb_and(n1754, n2260);
    let n2263: ZB = zb_or(n2261, n2262);
    let n2264: ZB = zb_and(n1760, n2263);
    let n2265: ZB = zb_and(n1761, n2263);
    let n2266: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2252);
    let n2267: ZB = zb_or(n2264, n2265);
    let n2268: ZN = zsel_n(n2068, n2252, n2266);
    let n2269: ZB = zb_or(n2259, n2267);
    let n2270: ZB = zb_and(n233, n2037);
    let n2271: ZB = zb_and(r_c311, n2037);
    let n2272: ZN = zsel_n(n233, zn_splat(P8::from_raw(327680i32)), n2035);
    let n2273: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n2036);
    let n2274: ZB = zb_or(n2270, n2271);
    let n2275: ZN = zsel_n(n301, n1621, n2272);
    let n2276: ZN = zsel_n(n301, n1631, n2273);
    let n2277: ZB = zb_or(n1632, n2274);
    let n2278: ZB = zb_and(n1747, n2277);
    let n2279: ZB = zb_and(n1748, n2277);
    let n2280: ZB = zb_or(n2278, n2279);
    let n2281: ZB = zb_and(n1748, n2280);
    let n2282: ZB = zb_and(n2068, n2281);
    let n2283: ZB = zb_and(n2069, n2281);
    let n2284: ZB = zb_and(n1755, n2283);
    let n2285: ZB = zb_and(n1754, n2283);
    let n2286: ZB = zb_or(n2284, n2285);
    let n2287: ZB = zb_and(n1760, n2286);
    let n2288: ZB = zb_and(n1761, n2286);
    let n2289: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2275);
    let n2290: ZB = zb_or(n2287, n2288);
    let n2291: ZN = zsel_n(n2068, n2275, n2289);
    let n2292: ZB = zb_or(n2282, n2290);
    let n2293: ZN = zsel_n(n233, zn_splat(P8::from_raw(0i32)), n1944);
    let n2294: ZN = zsel_n(n233, zn_splat(P8::from_raw(-327680i32)), n1945);
    let n2295: ZB = zb_or(n2212, n2213);
    let n2296: ZN = zsel_n(n301, n1621, n2293);
    let n2297: ZN = zsel_n(n301, n1631, n2294);
    let n2298: ZB = zb_or(n1632, n2295);
    let n2299: ZB = zb_and(n1747, n2298);
    let n2300: ZB = zb_and(n1748, n2298);
    let n2301: ZB = zb_or(n2299, n2300);
    let n2302: ZB = zb_and(n1748, n2301);
    let n2303: ZB = zb_and(n2068, n2302);
    let n2304: ZB = zb_and(n2069, n2302);
    let n2305: ZB = zb_and(n1755, n2304);
    let n2306: ZB = zb_and(n1754, n2304);
    let n2307: ZB = zb_or(n2305, n2306);
    let n2308: ZB = zb_and(n1760, n2307);
    let n2309: ZB = zb_and(n1761, n2307);
    let n2310: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2296);
    let n2311: ZB = zb_or(n2308, n2309);
    let n2312: ZN = zsel_n(n2068, n2296, n2310);
    let n2313: ZB = zb_or(n2303, n2311);
    let n2314: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n1990);
    let n2315: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n1991);
    let n2316: ZN = zsel_n(n301, n1621, n2314);
    let n2317: ZN = zsel_n(n301, n1631, n2315);
    let n2318: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2316);
    let n2319: ZN = zsel_n(n2068, n2316, n2318);
    let n2320: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n2035);
    let n2321: ZN = zsel_n(n233, zn_splat(P8::from_raw(-231700i32)), n2036);
    let n2322: ZN = zsel_n(n301, n1621, n2320);
    let n2323: ZN = zsel_n(n301, n1631, n2321);
    let n2324: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n2322);
    let n2325: ZN = zsel_n(n2068, n2322, n2324);
    let n2326: ZN = zsel_n(n233, zn_splat(P8::from_raw(327680i32)), n1945);
    let n2327: ZN = zsel_n(n301, n1631, n2326);
    let n2328: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n1991);
    let n2329: ZN = zsel_n(n301, n1631, n2328);
    let n2330: ZN = zsel_n(n233, zn_splat(P8::from_raw(231700i32)), n2036);
    let n2331: ZN = zsel_n(n301, n1631, n2330);
    let n2332: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n2333: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n2332);
    let n2334: ZB = zb_and(n976, n977);
    let n2335: ZB = zb_and(n992, n993);
    let n2336: ZB = zb_and(n1008, n1009);
    let n2337: ZB = zb_and(n1032, n1033);
    let n2338: ZB = zb_or(n2336, n2337);
    let n2339: ZB = zb_or(n2335, n2338);
    let n2340: ZB = zb_or(n2334, n2339);
    let n2341: ZB = zb_and(n1059, n1060);
    let n2342: ZB = zb_and(n1072, n1073);
    let n2343: ZB = zb_and(n1085, n1086);
    let n2344: ZB = zb_and(n1101, n1102);
    let n2345: ZB = zb_or(n2343, n2344);
    let n2346: ZB = zb_or(n2342, n2345);
    let n2347: ZB = zb_or(n2341, n2346);
    let n2348: ZB = zb_and(n1128, n1129);
    let n2349: ZB = zb_and(n1141, n1142);
    let n2350: ZB = zb_and(n1154, n1155);
    let n2351: ZB = zb_and(n1170, n1171);
    let n2352: ZB = zb_or(n2350, n2351);
    let n2353: ZB = zb_or(n2349, n2352);
    let n2354: ZB = zb_or(n2348, n2353);
    let n2355: ZB = zb_or(n2347, n2354);
    let n2356: ZB = zb_or(n2340, n2355);
    let n2357: ZB = zb_and(n1204, n1205);
    let n2358: ZB = zb_and(n1217, n1218);
    let n2359: ZB = zb_and(n1230, n1231);
    let n2360: ZB = zb_and(n1250, n1251);
    let n2361: ZB = zb_or(n2359, n2360);
    let n2362: ZB = zb_or(n2358, n2361);
    let n2363: ZB = zb_or(n2357, n2362);
    let n2364: ZB = zb_and(n1269, n1270);
    let n2365: ZB = zb_and(n1282, n1283);
    let n2366: ZB = zb_and(n1295, n1296);
    let n2367: ZB = zb_and(n1311, n1312);
    let n2368: ZB = zb_or(n2366, n2367);
    let n2369: ZB = zb_or(n2365, n2368);
    let n2370: ZB = zb_or(n2364, n2369);
    let n2371: ZB = zb_and(n1330, n1331);
    let n2372: ZB = zb_and(n1343, n1344);
    let n2373: ZB = zb_and(n1356, n1357);
    let n2374: ZB = zb_and(n1372, n1373);
    let n2375: ZB = zb_or(n2373, n2374);
    let n2376: ZB = zb_or(n2372, n2375);
    let n2377: ZB = zb_or(n2371, n2376);
    let n2378: ZB = zb_or(n2370, n2377);
    let n2379: ZB = zb_or(n2363, n2378);
    let n2380: ZB = zb_and(n1404, n1405);
    let n2381: ZB = zb_and(n1417, n1418);
    let n2382: ZB = zb_and(n1430, n1431);
    let n2383: ZB = zb_and(n1450, n1451);
    let n2384: ZB = zb_or(n2382, n2383);
    let n2385: ZB = zb_or(n2381, n2384);
    let n2386: ZB = zb_or(n2380, n2385);
    let n2387: ZB = zb_and(n1469, n1470);
    let n2388: ZB = zb_and(n1482, n1483);
    let n2389: ZB = zb_and(n1495, n1496);
    let n2390: ZB = zb_and(n1511, n1512);
    let n2391: ZB = zb_or(n2389, n2390);
    let n2392: ZB = zb_or(n2388, n2391);
    let n2393: ZB = zb_or(n2387, n2392);
    let n2394: ZB = zb_and(n1530, n1531);
    let n2395: ZB = zb_and(n1543, n1544);
    let n2396: ZB = zb_and(n1556, n1557);
    let n2397: ZB = zb_and(n1572, n1573);
    let n2398: ZB = zb_or(n2396, n2397);
    let n2399: ZB = zb_or(n2395, n2398);
    let n2400: ZB = zb_or(n2394, n2399);
    let n2401: ZB = zb_or(n2393, n2400);
    let n2402: ZB = zb_or(n2386, n2401);
    let n2403: ZB = zb_or(n2379, n2402);
    let n2404: ZB = zsel_b(n2379, n1182, n1382);
    let n2405: ZB = zb_or(n2356, n2403);
    let n2406: ZB = zsel_b(n2356, n926, n2404);
    let n2407: ZB = zn_gt(n920, zn_splat(P8::from_raw(8388608i32)));
    let n2408: ZB = zb_and(n2405, n2407);
    let n2409: ZB = zb_and(n1592, n2405);
    let n2410: ZB = zb_or(n2408, n2409);
    let n2411: ZB = zb_and(n1590, n2407);
    let n2412: ZB = zb_or(n2410, n2411);
    let n2413: ZB = zsel_b(n2410, n2406, n1591);
    let n2414: ZB = zb_and(n1597, n2412);
    let n2415: ZB = zb_and(n1596, n2412);
    let n2416: ZB = zb_or(n2414, n2415);
    let n2417: ZB = zb_and(n1597, n2416);
    let n2418: ZB = zb_and(n1596, n2416);
    let n2419: ZB = zb_or(n2417, n2418);
    let n2420: ZB = zb_and(n1596, n2419);
    let n2421: ZB = zb_and(n1597, n2419);
    let n2422: ZB = zb_and(n296, n2421);
    let n2423: ZB = zb_and(n297, n2421);
    let n2424: ZB = zb_or(n2422, n2423);
    let n2425: ZB = zb_or(n2420, n2424);
    let n2426: ZB = zb_and(n301, n2425);
    let n2427: ZB = zb_and(n302, n2425);
    let n2428: ZB = zb_and(n1613, n2426);
    let n2429: ZB = zb_and(n1614, n2426);
    let n2430: ZB = zb_or(n2428, n2429);
    let n2431: ZB = zb_and(n1623, n2430);
    let n2432: ZB = zb_and(n1624, n2430);
    let n2433: ZB = zb_or(n2431, n2432);
    let n2434: ZB = zb_and(n1597, n2427);
    let n2435: ZB = zb_and(n1596, n2427);
    let n2436: ZB = zb_or(n2434, n2435);
    let n2437: ZB = zb_and(n1638, n2436);
    let n2438: ZB = zb_and(n1639, n2436);
    let n2439: ZB = zb_and(n1642, n2437);
    let n2440: ZB = zb_and(n1007, n2437);
    let n2441: ZB = zb_and(n1645, n2440);
    let n2442: ZB = zb_and(n1031, n2440);
    let n2443: ZB = zb_and(n1648, n2439);
    let n2444: ZB = zb_and(n1649, n2439);
    let n2445: ZB = zb_and(n1656, n2441);
    let n2446: ZB = zb_and(n1657, n2441);
    let n2447: ZB = zb_and(n1007, n2442);
    let n2448: ZB = zb_or(n2445, n2446);
    let n2449: ZB = zb_or(n2443, n2444);
    let n2450: ZB = zb_or(n2447, n2448);
    let n2451: ZB = zb_or(n2449, n2450);
    let n2452: ZB = zb_and(n1642, n2438);
    let n2453: ZB = zb_and(n1007, n2438);
    let n2454: ZB = zb_or(n2452, n2453);
    let n2455: ZB = zb_or(n2451, n2454);
    let n2456: ZB = zb_and(n1685, n2455);
    let n2457: ZB = zb_and(n1684, n2455);
    let n2458: ZB = zb_or(n2456, n2457);
    let n2459: ZB = zb_and(n1692, n2458);
    let n2460: ZB = zb_and(n1693, n2458);
    let n2461: ZB = zb_or(n2459, n2460);
    let n2462: ZB = zb_and(n1597, n2461);
    let n2463: ZB = zb_and(n1596, n2461);
    let n2464: ZB = zb_and(n1701, n2462);
    let n2465: ZB = zb_and(n1702, n2462);
    let n2466: ZB = zb_or(n2464, n2465);
    let n2467: ZB = zb_or(n2463, n2466);
    let n2468: ZB = zb_or(n2433, n2467);
    let n2469: ZB = zb_and(n1747, n2468);
    let n2470: ZB = zb_and(n1748, n2468);
    let n2471: ZB = zb_or(n2469, n2470);
    let n2474: ZN = zsel_n(n2407, n2333, n2332);
    let n2475: ZN = zsel_n(n2410, n2474, n2332);
    let n2477: ZB = zb_and(n1656, n2438);
    let n2478: ZB = zb_and(n1657, n2438);
    let n2479: ZB = zb_or(n2477, n2478);
    let n2480: ZB = zb_or(n2451, n2479);
    let n2481: ZB = zb_and(n1792, n2480);
    let n2482: ZB = zb_and(n1791, n2480);
    let n2483: ZB = zb_or(n2481, n2482);
    let n2484: ZB = zb_and(n1692, n2483);
    let n2485: ZB = zb_and(n1693, n2483);
    let n2486: ZB = zb_or(n2484, n2485);
    let n2487: ZB = zb_and(n1803, n2486);
    let n2488: ZB = zb_and(n1802, n2486);
    let n2489: ZB = zb_or(n2487, n2488);
    let n2490: ZB = zb_and(n1803, n2489);
    let n2491: ZB = zb_and(n1802, n2489);
    let n2492: ZB = zb_or(n2490, n2491);
    let n2493: ZB = zb_and(n1802, n2492);
    let n2494: ZB = zb_and(n1803, n2492);
    let n2495: ZB = zb_or(n2493, n2494);
    let n2496: ZB = zb_and(n1802, n2495);
    let n2497: ZB = zb_and(n1803, n2495);
    let n2498: ZB = zb_or(n2496, n2497);
    let n2499: ZB = zb_and(n1597, n2498);
    let n2500: ZB = zb_and(n1596, n2498);
    let n2501: ZB = zb_and(n1819, n2499);
    let n2502: ZB = zb_and(n1820, n2499);
    let n2503: ZB = zb_or(n2501, n2502);
    let n2504: ZB = zb_or(n2500, n2503);
    let n2505: ZB = zb_or(n2433, n2504);
    let n2506: ZB = zb_and(n1747, n2505);
    let n2507: ZB = zb_and(n1748, n2505);
    let n2508: ZB = zb_or(n2506, n2507);
    let n2511: ZB = zb_and(n1648, n2438);
    let n2512: ZB = zb_and(n1649, n2438);
    let n2513: ZB = zb_or(n2511, n2512);
    let n2514: ZB = zb_or(n2451, n2513);
    let n2515: ZB = zb_and(n1859, n2514);
    let n2516: ZB = zb_and(n1858, n2514);
    let n2517: ZB = zb_or(n2515, n2516);
    let n2518: ZB = zb_and(n1692, n2517);
    let n2519: ZB = zb_and(n1693, n2517);
    let n2520: ZB = zb_or(n2518, n2519);
    let n2521: ZB = zb_and(n1870, n2520);
    let n2522: ZB = zb_and(n1869, n2520);
    let n2523: ZB = zb_or(n2521, n2522);
    let n2524: ZB = zb_and(n1870, n2523);
    let n2525: ZB = zb_and(n1869, n2523);
    let n2526: ZB = zb_or(n2524, n2525);
    let n2527: ZB = zb_and(n1869, n2526);
    let n2528: ZB = zb_and(n1870, n2526);
    let n2529: ZB = zb_or(n2527, n2528);
    let n2530: ZB = zb_and(n1869, n2529);
    let n2531: ZB = zb_and(n1870, n2529);
    let n2532: ZB = zb_or(n2530, n2531);
    let n2533: ZB = zb_and(n1597, n2532);
    let n2534: ZB = zb_and(n1596, n2532);
    let n2535: ZB = zb_and(n1886, n2533);
    let n2536: ZB = zb_and(n1887, n2533);
    let n2537: ZB = zb_or(n2535, n2536);
    let n2538: ZB = zb_or(n2534, n2537);
    let n2539: ZB = zb_or(n2433, n2538);
    let n2540: ZB = zb_and(n1747, n2539);
    let n2541: ZB = zb_and(n1748, n2539);
    let n2542: ZB = zb_or(n2540, n2541);
    let n2545: ZB = zb_and(n295, n2467);
    let n2546: ZB = zb_and(r_c312, n2467);
    let n2547: ZB = zb_and(n1713, n2545);
    let n2548: ZB = zb_and(n1714, n2545);
    let n2549: ZB = zb_and(n1717, n2548);
    let n2550: ZB = zb_and(n1716, n2548);
    let n2551: ZB = zb_or(n2549, n2550);
    let n2552: ZB = zb_and(n1717, n2551);
    let n2553: ZB = zb_and(n1716, n2551);
    let n2554: ZB = zb_or(n2552, n2553);
    let n2555: ZB = zb_and(n1716, n2554);
    let n2556: ZB = zb_and(n1717, n2554);
    let n2557: ZB = zb_and(n1720, n2556);
    let n2558: ZB = zb_and(n1719, n2556);
    let n2559: ZB = zb_or(n2557, n2558);
    let n2560: ZB = zb_and(n1720, n2559);
    let n2561: ZB = zb_and(n1719, n2559);
    let n2562: ZB = zb_or(n2560, n2561);
    let n2563: ZB = zb_and(n1719, n2562);
    let n2564: ZB = zb_and(n1720, n2562);
    let n2565: ZB = zb_or(n2563, n2564);
    let n2566: ZB = zb_or(n2555, n2565);
    let n2567: ZB = zb_and(n1724, n2566);
    let n2568: ZB = zb_and(n1723, n2566);
    let n2569: ZB = zb_or(n2567, n2568);
    let n2570: ZB = zb_or(n2547, n2569);
    let n2571: ZB = zb_or(n2546, n2570);
    let n2572: ZB = zb_or(n2433, n2571);
    let n2573: ZB = zb_and(n1747, n2572);
    let n2574: ZB = zb_and(n1748, n2572);
    let n2575: ZB = zb_or(n2573, n2574);
    let n2578: ZB = zb_and(n295, n2504);
    let n2579: ZB = zb_and(r_c312, n2504);
    let n2580: ZB = zb_and(n1713, n2578);
    let n2581: ZB = zb_and(n1714, n2578);
    let n2582: ZB = zb_and(n1717, n2581);
    let n2583: ZB = zb_and(n1716, n2581);
    let n2584: ZB = zb_or(n2582, n2583);
    let n2585: ZB = zb_and(n1717, n2584);
    let n2586: ZB = zb_and(n1716, n2584);
    let n2587: ZB = zb_or(n2585, n2586);
    let n2588: ZB = zb_and(n1716, n2587);
    let n2589: ZB = zb_and(n1717, n2587);
    let n2590: ZB = zb_and(n1720, n2589);
    let n2591: ZB = zb_and(n1719, n2589);
    let n2592: ZB = zb_or(n2590, n2591);
    let n2593: ZB = zb_and(n1720, n2592);
    let n2594: ZB = zb_and(n1719, n2592);
    let n2595: ZB = zb_or(n2593, n2594);
    let n2596: ZB = zb_and(n1719, n2595);
    let n2597: ZB = zb_and(n1720, n2595);
    let n2598: ZB = zb_or(n2596, n2597);
    let n2599: ZB = zb_or(n2588, n2598);
    let n2600: ZB = zb_and(n1724, n2599);
    let n2601: ZB = zb_and(n1723, n2599);
    let n2602: ZB = zb_or(n2600, n2601);
    let n2603: ZB = zb_or(n2580, n2602);
    let n2604: ZB = zb_or(n2579, n2603);
    let n2605: ZB = zb_or(n2433, n2604);
    let n2606: ZB = zb_and(n1747, n2605);
    let n2607: ZB = zb_and(n1748, n2605);
    let n2608: ZB = zb_or(n2606, n2607);
    let n2611: ZB = zb_and(n295, n2538);
    let n2612: ZB = zb_and(r_c312, n2538);
    let n2613: ZB = zb_and(n1713, n2611);
    let n2614: ZB = zb_and(n1714, n2611);
    let n2615: ZB = zb_and(n1717, n2614);
    let n2616: ZB = zb_and(n1716, n2614);
    let n2617: ZB = zb_or(n2615, n2616);
    let n2618: ZB = zb_and(n1717, n2617);
    let n2619: ZB = zb_and(n1716, n2617);
    let n2620: ZB = zb_or(n2618, n2619);
    let n2621: ZB = zb_and(n1716, n2620);
    let n2622: ZB = zb_and(n1717, n2620);
    let n2623: ZB = zb_and(n1720, n2622);
    let n2624: ZB = zb_and(n1719, n2622);
    let n2625: ZB = zb_or(n2623, n2624);
    let n2626: ZB = zb_and(n1720, n2625);
    let n2627: ZB = zb_and(n1719, n2625);
    let n2628: ZB = zb_or(n2626, n2627);
    let n2629: ZB = zb_and(n1719, n2628);
    let n2630: ZB = zb_and(n1720, n2628);
    let n2631: ZB = zb_or(n2629, n2630);
    let n2632: ZB = zb_or(n2621, n2631);
    let n2633: ZB = zb_and(n1724, n2632);
    let n2634: ZB = zb_and(n1723, n2632);
    let n2635: ZB = zb_or(n2633, n2634);
    let n2636: ZB = zb_or(n2613, n2635);
    let n2637: ZB = zb_or(n2612, n2636);
    let n2638: ZB = zb_or(n2433, n2637);
    let n2639: ZB = zb_and(n1747, n2638);
    let n2640: ZB = zb_and(n1748, n2638);
    let n2641: ZB = zb_or(n2639, n2640);
    let n2644: ZB = zb_and(n233, n2467);
    let n2645: ZB = zb_and(r_c311, n2467);
    let n2646: ZB = zb_and(n1689, n2644);
    let n2647: ZB = zb_and(n1732, n2644);
    let n2648: ZB = zb_or(n2646, n2647);
    let n2649: ZB = zb_and(n1734, n2648);
    let n2650: ZB = zb_and(n1735, n2648);
    let n2651: ZB = zb_and(n1736, n2650);
    let n2652: ZB = zb_and(n1737, n2650);
    let n2653: ZB = zb_or(n2651, n2652);
    let n2654: ZB = zb_or(n2649, n2653);
    let n2655: ZB = zb_and(n1741, n2654);
    let n2656: ZB = zb_and(n1740, n2654);
    let n2657: ZB = zb_or(n2655, n2656);
    let n2658: ZB = zb_or(n2645, n2657);
    let n2659: ZB = zb_or(n2433, n2658);
    let n2660: ZB = zb_and(n1747, n2659);
    let n2661: ZB = zb_and(n1748, n2659);
    let n2662: ZB = zb_or(n2660, n2661);
    let n2663: ZB = zb_and(n1748, n2662);
    let n2664: ZB = zb_and(n2068, n2663);
    let n2665: ZB = zb_and(n2069, n2663);
    let n2666: ZB = zb_or(n2664, n2665);
    let n2667: ZB = zb_and(n233, n2504);
    let n2668: ZB = zb_and(r_c311, n2504);
    let n2669: ZB = zb_or(n2667, n2668);
    let n2670: ZB = zb_or(n2433, n2669);
    let n2671: ZB = zb_and(n1747, n2670);
    let n2672: ZB = zb_and(n1748, n2670);
    let n2673: ZB = zb_or(n2671, n2672);
    let n2674: ZB = zb_and(n1748, n2673);
    let n2675: ZB = zb_and(n2068, n2674);
    let n2676: ZB = zb_and(n2069, n2674);
    let n2677: ZB = zb_or(n2675, n2676);
    let n2678: ZB = zb_and(n233, n2538);
    let n2679: ZB = zb_and(r_c311, n2538);
    let n2680: ZB = zb_or(n2678, n2679);
    let n2681: ZB = zb_or(n2433, n2680);
    let n2682: ZB = zb_and(n1747, n2681);
    let n2683: ZB = zb_and(n1748, n2681);
    let n2684: ZB = zb_or(n2682, n2683);
    let n2685: ZB = zb_and(n1748, n2684);
    let n2686: ZB = zb_and(n2068, n2685);
    let n2687: ZB = zb_and(n2069, n2685);
    let n2688: ZB = zb_or(n2686, n2687);
    let n2689: ZB = zb_or(n2644, n2645);
    let n2690: ZB = zb_or(n2433, n2689);
    let n2691: ZB = zb_and(n1747, n2690);
    let n2692: ZB = zb_and(n1748, n2690);
    let n2693: ZB = zb_or(n2691, n2692);
    let n2694: ZB = zb_and(n1748, n2693);
    let n2695: ZB = zb_and(n2068, n2694);
    let n2696: ZB = zb_and(n2069, n2694);
    let n2697: ZB = zb_or(n2695, n2696);
    let n2698: ZB = zb_and(n233, n2571);
    let n2699: ZB = zb_and(r_c311, n2571);
    let n2700: ZB = zb_and(n1689, n2698);
    let n2701: ZB = zb_and(n1732, n2698);
    let n2702: ZB = zb_or(n2700, n2701);
    let n2703: ZB = zb_and(n1734, n2702);
    let n2704: ZB = zb_and(n1735, n2702);
    let n2705: ZB = zb_and(n1736, n2704);
    let n2706: ZB = zb_and(n1737, n2704);
    let n2707: ZB = zb_or(n2705, n2706);
    let n2708: ZB = zb_or(n2703, n2707);
    let n2709: ZB = zb_and(n1741, n2708);
    let n2710: ZB = zb_and(n1740, n2708);
    let n2711: ZB = zb_or(n2709, n2710);
    let n2712: ZB = zb_or(n2699, n2711);
    let n2713: ZB = zb_or(n2433, n2712);
    let n2714: ZB = zb_and(n1747, n2713);
    let n2715: ZB = zb_and(n1748, n2713);
    let n2716: ZB = zb_or(n2714, n2715);
    let n2717: ZB = zb_and(n1748, n2716);
    let n2718: ZB = zb_and(n2068, n2717);
    let n2719: ZB = zb_and(n2069, n2717);
    let n2720: ZB = zb_or(n2718, n2719);
    let n2721: ZB = zb_and(n233, n2604);
    let n2722: ZB = zb_and(r_c311, n2604);
    let n2723: ZB = zb_or(n2721, n2722);
    let n2724: ZB = zb_or(n2433, n2723);
    let n2725: ZB = zb_and(n1747, n2724);
    let n2726: ZB = zb_and(n1748, n2724);
    let n2727: ZB = zb_or(n2725, n2726);
    let n2728: ZB = zb_and(n1748, n2727);
    let n2729: ZB = zb_and(n2068, n2728);
    let n2730: ZB = zb_and(n2069, n2728);
    let n2731: ZB = zb_or(n2729, n2730);
    let n2732: ZB = zb_and(n233, n2637);
    let n2733: ZB = zb_and(r_c311, n2637);
    let n2734: ZB = zb_or(n2732, n2733);
    let n2735: ZB = zb_or(n2433, n2734);
    let n2736: ZB = zb_and(n1747, n2735);
    let n2737: ZB = zb_and(n1748, n2735);
    let n2738: ZB = zb_or(n2736, n2737);
    let n2739: ZB = zb_and(n1748, n2738);
    let n2740: ZB = zb_and(n2068, n2739);
    let n2741: ZB = zb_and(n2069, n2739);
    let n2742: ZB = zb_or(n2740, n2741);
    let n2743: ZB = zb_or(n2698, n2699);
    let n2744: ZB = zb_or(n2433, n2743);
    let n2745: ZB = zb_and(n1747, n2744);
    let n2746: ZB = zb_and(n1748, n2744);
    let n2747: ZB = zb_or(n2745, n2746);
    let n2748: ZB = zb_and(n1748, n2747);
    let n2749: ZB = zb_and(n2068, n2748);
    let n2750: ZB = zb_and(n2069, n2748);
    let n2751: ZB = zb_or(n2749, n2750);
    let n2754: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c246);
    let n2755: ZN = zn_div(n2754, zn_splat(P8::from_raw(2621440i32)));
    let n2756: ZN = zn_sin(n2755);
    let n2758: ZN = zn_mul(n2756, zn_splat(P8::from_raw(163840i32)));
    let n2759: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n2758);
    let n2760: ZB = zb_not(n282);
    let n2761: ZB = zb_and(n172, n2760);
    let n2762: ZN = zsel_n(n289, zn_splat(P8::from_raw(65536i32)), r_c302);
    let n2763: ZB = zb_and(n228, n2761);
    let n2764: ZB = zb_and(n229, n2761);
    let n2765: ZB = zb_and(n308, n2763);
    let n2766: ZB = zb_and(n309, n2763);
    let n2767: ZB = zb_or(n2765, n2766);
    let n2768: ZB = zb_and(n313, n2767);
    let n2769: ZB = zb_and(n314, n2767);
    let n2770: ZB = zb_or(n2768, n2769);
    let n2771: ZB = zb_and(n319, n2770);
    let n2772: ZB = zb_and(n320, n2770);
    let n2773: ZB = zb_or(n2771, n2772);
    let n2774: ZB = zb_and(n324, n2773);
    let n2775: ZB = zb_and(n325, n2773);
    let n2776: ZB = zb_and(n289, n2774);
    let n2777: ZB = zb_and(n290, n2774);
    let n2778: ZB = zb_or(n2776, n2777);
    let n2779: ZB = zb_and(n220, n2764);
    let n2780: ZB = zb_and(n221, n2764);
    let n2781: ZB = zb_and(n222, n2779);
    let n2782: ZB = zb_and(n223, n2779);
    let n2783: ZB = zb_or(n2781, n2782);
    let n2784: ZB = zb_or(n2780, n2783);
    let n2785: ZN = zsel_n(n324, n2762, r_c302);
    let n2786: ZB = zb_or(n2775, n2778);
    let n2787: ZN = zsel_n(n229, r_c302, n2785);
    let n2788: ZB = zb_or(n2784, n2786);
    let n2789: ZB = zb_and(n144, n2788);
    let n2790: ZB = zb_and(n235, n2788);
    let n2791: ZB = zb_and(n284, n2789);
    let n2792: ZB = zb_and(n285, n2789);
    let n2793: ZB = zb_or(n2791, n2792);
    let n2794: ZB = zb_and(n359, n2793);
    let n2795: ZB = zb_and(n360, n2793);
    let n2796: ZB = zb_or(n2794, n2795);
    let n2797: ZB = zb_and(n364, n2796);
    let n2798: ZB = zb_and(n365, n2796);
    let n2799: ZB = zb_or(n2797, n2798);
    let n2800: ZB = zb_and(n370, n2799);
    let n2801: ZB = zb_and(n371, n2799);
    let n2802: ZB = zb_and(n374, n2800);
    let n2803: ZB = zb_and(n375, n2800);
    let n2804: ZN = zsel_n(n374, zn_splat(P8::from_raw(65536i32)), n2787);
    let n2805: ZB = zb_or(n2802, n2803);
    let n2806: ZB = zb_and(n236, n2790);
    let n2807: ZB = zb_and(n237, n2790);
    let n2808: ZB = zb_and(n239, n2806);
    let n2809: ZB = zb_and(n240, n2806);
    let n2810: ZB = zb_or(n2808, n2809);
    let n2811: ZB = zb_or(n2807, n2810);
    let n2812: ZN = zsel_n(n370, n2804, n2787);
    let n2813: ZB = zb_or(n2801, n2805);
    let n2814: ZN = zsel_n(n235, n2787, n2812);
    let n2815: ZB = zb_or(n2811, n2813);
    let n2816: ZB = zb_and(n403, n2815);
    let n2817: ZB = zb_and(n404, n2815);
    let n2818: ZB = zb_or(n2816, n2817);
    let n2819: ZB = zb_and(n410, n2818);
    let n2820: ZB = zb_and(n411, n2818);
    let n2821: ZB = zb_and(n419, n2819);
    let n2822: ZB = zb_and(n420, n2819);
    let n2823: ZB = zb_and(n423, n2822);
    let n2824: ZB = zb_and(n424, n2822);
    let n2825: ZB = zb_or(n2823, n2824);
    let n2826: ZB = zb_or(n2821, n2825);
    let n2827: ZB = zb_and(n436, n2826);
    let n2828: ZB = zb_and(n435, n2826);
    let n2829: ZB = zb_or(n2827, n2828);
    let n2830: ZB = zb_and(n436, n2829);
    let n2831: ZB = zb_and(n435, n2829);
    let n2832: ZB = zb_or(n2830, n2831);
    let n2833: ZB = zb_and(n436, n2832);
    let n2834: ZB = zb_and(n435, n2832);
    let n2835: ZB = zb_and(n446, n2833);
    let n2836: ZB = zb_and(n447, n2833);
    let n2837: ZB = zb_and(n453, n2835);
    let n2838: ZB = zb_and(n452, n2835);
    let n2839: ZB = zb_or(n2837, n2838);
    let n2840: ZB = zb_and(n453, n2839);
    let n2841: ZB = zb_and(n452, n2839);
    let n2842: ZB = zb_or(n2840, n2841);
    let n2843: ZB = zb_and(n453, n2842);
    let n2844: ZB = zb_and(n452, n2842);
    let n2845: ZB = zb_and(n463, n2843);
    let n2846: ZB = zb_and(n464, n2843);
    let n2847: ZB = zb_and(n470, n2845);
    let n2848: ZB = zb_and(n469, n2845);
    let n2849: ZB = zb_or(n2847, n2848);
    let n2850: ZB = zb_and(n470, n2849);
    let n2851: ZB = zb_and(n469, n2849);
    let n2852: ZB = zb_or(n2850, n2851);
    let n2853: ZB = zb_and(n470, n2852);
    let n2854: ZB = zb_and(n469, n2852);
    let n2855: ZB = zb_and(n480, n2853);
    let n2856: ZB = zb_and(n481, n2853);
    let n2857: ZB = zb_and(n487, n2855);
    let n2858: ZB = zb_and(n486, n2855);
    let n2859: ZB = zb_or(n2857, n2858);
    let n2860: ZB = zb_and(n487, n2859);
    let n2861: ZB = zb_and(n486, n2859);
    let n2862: ZB = zb_or(n2860, n2861);
    let n2863: ZB = zb_and(n487, n2862);
    let n2864: ZB = zb_and(n486, n2862);
    let n2865: ZB = zb_and(n497, n2863);
    let n2866: ZB = zb_and(n498, n2863);
    let n2867: ZB = zb_and(n504, n2865);
    let n2868: ZB = zb_and(n503, n2865);
    let n2869: ZB = zb_or(n2867, n2868);
    let n2870: ZB = zb_and(n504, n2869);
    let n2871: ZB = zb_and(n503, n2869);
    let n2872: ZB = zb_or(n2870, n2871);
    let n2873: ZB = zb_and(n504, n2872);
    let n2874: ZB = zb_and(n503, n2872);
    let n2875: ZB = zb_and(n514, n2873);
    let n2876: ZB = zb_and(n515, n2873);
    let n2877: ZB = zb_and(n521, n2875);
    let n2878: ZB = zb_and(n520, n2875);
    let n2879: ZB = zb_or(n2877, n2878);
    let n2880: ZB = zb_and(n521, n2879);
    let n2881: ZB = zb_and(n520, n2879);
    let n2882: ZB = zb_or(n2880, n2881);
    let n2883: ZB = zb_and(n521, n2882);
    let n2884: ZB = zb_and(n520, n2882);
    let n2885: ZB = zb_and(n531, n2883);
    let n2886: ZB = zb_and(n532, n2883);
    let n2887: ZB = zb_and(n538, n2885);
    let n2888: ZB = zb_and(n537, n2885);
    let n2889: ZB = zb_or(n2887, n2888);
    let n2890: ZB = zb_and(n538, n2889);
    let n2891: ZB = zb_and(n537, n2889);
    let n2892: ZB = zb_or(n2890, n2891);
    let n2893: ZB = zb_and(n538, n2892);
    let n2894: ZB = zb_and(n537, n2892);
    let n2895: ZB = zb_and(n548, n2893);
    let n2896: ZB = zb_and(n549, n2893);
    let n2897: ZB = zb_and(n555, n2895);
    let n2898: ZB = zb_and(n554, n2895);
    let n2899: ZB = zb_or(n2897, n2898);
    let n2900: ZB = zb_and(n555, n2899);
    let n2901: ZB = zb_and(n554, n2899);
    let n2902: ZB = zb_or(n2900, n2901);
    let n2903: ZB = zb_and(n555, n2902);
    let n2904: ZB = zb_and(n554, n2902);
    let n2905: ZB = zb_or(n2903, n2904);
    let n2906: ZB = zb_or(n2896, n2905);
    let n2907: ZB = zb_or(n2894, n2906);
    let n2908: ZB = zb_or(n2886, n2907);
    let n2909: ZB = zb_or(n2884, n2908);
    let n2910: ZB = zb_or(n2876, n2909);
    let n2911: ZB = zb_or(n2874, n2910);
    let n2912: ZB = zb_or(n2866, n2911);
    let n2913: ZB = zb_or(n2864, n2912);
    let n2914: ZB = zb_or(n2856, n2913);
    let n2915: ZB = zb_or(n2854, n2914);
    let n2916: ZB = zb_or(n2846, n2915);
    let n2917: ZB = zb_or(n2844, n2916);
    let n2918: ZB = zb_or(n2836, n2917);
    let n2919: ZB = zb_or(n2834, n2918);
    let n2920: ZB = zb_and(n646, n2919);
    let n2921: ZB = zb_and(n647, n2919);
    let n2922: ZB = zb_and(n650, n2921);
    let n2923: ZB = zb_and(n651, n2921);
    let n2924: ZB = zb_or(n2922, n2923);
    let n2925: ZB = zb_or(n2920, n2924);
    let n2926: ZB = zb_and(n659, n2925);
    let n2927: ZB = zb_and(n660, n2925);
    let n2928: ZB = zb_or(n2926, n2927);
    let n2929: ZB = zb_and(n659, n2928);
    let n2930: ZB = zb_and(n660, n2928);
    let n2931: ZB = zb_or(n2929, n2930);
    let n2932: ZB = zb_and(n671, n2931);
    let n2933: ZB = zb_and(n670, n2931);
    let n2934: ZB = zb_or(n2932, n2933);
    let n2935: ZB = zb_and(n671, n2934);
    let n2936: ZB = zb_and(n670, n2934);
    let n2937: ZB = zb_or(n2935, n2936);
    let n2938: ZB = zb_and(n671, n2937);
    let n2939: ZB = zb_and(n670, n2937);
    let n2940: ZB = zb_and(n681, n2938);
    let n2941: ZB = zb_and(n682, n2938);
    let n2942: ZB = zb_and(n659, n2940);
    let n2943: ZB = zb_and(n660, n2940);
    let n2944: ZB = zb_or(n2942, n2943);
    let n2945: ZB = zb_and(n659, n2944);
    let n2946: ZB = zb_and(n660, n2944);
    let n2947: ZB = zb_or(n2945, n2946);
    let n2948: ZB = zb_and(n694, n2947);
    let n2949: ZB = zb_and(n693, n2947);
    let n2950: ZB = zb_or(n2948, n2949);
    let n2951: ZB = zb_and(n694, n2950);
    let n2952: ZB = zb_and(n693, n2950);
    let n2953: ZB = zb_or(n2951, n2952);
    let n2954: ZB = zb_and(n694, n2953);
    let n2955: ZB = zb_and(n693, n2953);
    let n2956: ZB = zb_and(n704, n2954);
    let n2957: ZB = zb_and(n705, n2954);
    let n2958: ZB = zb_and(n659, n2956);
    let n2959: ZB = zb_and(n660, n2956);
    let n2960: ZB = zb_or(n2958, n2959);
    let n2961: ZB = zb_and(n659, n2960);
    let n2962: ZB = zb_and(n660, n2960);
    let n2963: ZB = zb_or(n2961, n2962);
    let n2964: ZB = zb_and(n717, n2963);
    let n2965: ZB = zb_and(n716, n2963);
    let n2966: ZB = zb_or(n2964, n2965);
    let n2967: ZB = zb_and(n717, n2966);
    let n2968: ZB = zb_and(n716, n2966);
    let n2969: ZB = zb_or(n2967, n2968);
    let n2970: ZB = zb_and(n717, n2969);
    let n2971: ZB = zb_and(n716, n2969);
    let n2972: ZB = zb_and(n727, n2970);
    let n2973: ZB = zb_and(n728, n2970);
    let n2974: ZB = zb_and(n659, n2972);
    let n2975: ZB = zb_and(n660, n2972);
    let n2976: ZB = zb_or(n2974, n2975);
    let n2977: ZB = zb_and(n659, n2976);
    let n2978: ZB = zb_and(n660, n2976);
    let n2979: ZB = zb_or(n2977, n2978);
    let n2980: ZB = zb_and(n740, n2979);
    let n2981: ZB = zb_and(n739, n2979);
    let n2982: ZB = zb_or(n2980, n2981);
    let n2983: ZB = zb_and(n740, n2982);
    let n2984: ZB = zb_and(n739, n2982);
    let n2985: ZB = zb_or(n2983, n2984);
    let n2986: ZB = zb_and(n740, n2985);
    let n2987: ZB = zb_and(n739, n2985);
    let n2988: ZB = zb_and(n750, n2986);
    let n2989: ZB = zb_and(n751, n2986);
    let n2990: ZB = zb_and(n659, n2988);
    let n2991: ZB = zb_and(n660, n2988);
    let n2992: ZB = zb_or(n2990, n2991);
    let n2993: ZB = zb_and(n659, n2992);
    let n2994: ZB = zb_and(n660, n2992);
    let n2995: ZB = zb_or(n2993, n2994);
    let n2996: ZB = zb_and(n763, n2995);
    let n2997: ZB = zb_and(n762, n2995);
    let n2998: ZB = zb_or(n2996, n2997);
    let n2999: ZB = zb_and(n763, n2998);
    let n3000: ZB = zb_and(n762, n2998);
    let n3001: ZB = zb_or(n2999, n3000);
    let n3002: ZB = zb_and(n763, n3001);
    let n3003: ZB = zb_and(n762, n3001);
    let n3004: ZB = zb_and(n773, n3002);
    let n3005: ZB = zb_and(n774, n3002);
    let n3006: ZB = zb_and(n659, n3004);
    let n3007: ZB = zb_and(n660, n3004);
    let n3008: ZB = zb_or(n3006, n3007);
    let n3009: ZB = zb_and(n659, n3008);
    let n3010: ZB = zb_and(n660, n3008);
    let n3011: ZB = zb_or(n3009, n3010);
    let n3012: ZB = zb_and(n786, n3011);
    let n3013: ZB = zb_and(n785, n3011);
    let n3014: ZB = zb_or(n3012, n3013);
    let n3015: ZB = zb_and(n786, n3014);
    let n3016: ZB = zb_and(n785, n3014);
    let n3017: ZB = zb_or(n3015, n3016);
    let n3018: ZB = zb_and(n786, n3017);
    let n3019: ZB = zb_and(n785, n3017);
    let n3020: ZB = zb_and(n796, n3018);
    let n3021: ZB = zb_and(n797, n3018);
    let n3022: ZB = zb_and(n659, n3020);
    let n3023: ZB = zb_and(n660, n3020);
    let n3024: ZB = zb_or(n3022, n3023);
    let n3025: ZB = zb_and(n659, n3024);
    let n3026: ZB = zb_and(n660, n3024);
    let n3027: ZB = zb_or(n3025, n3026);
    let n3028: ZB = zb_and(n809, n3027);
    let n3029: ZB = zb_and(n808, n3027);
    let n3030: ZB = zb_or(n3028, n3029);
    let n3031: ZB = zb_and(n809, n3030);
    let n3032: ZB = zb_and(n808, n3030);
    let n3033: ZB = zb_or(n3031, n3032);
    let n3034: ZB = zb_and(n809, n3033);
    let n3035: ZB = zb_and(n808, n3033);
    let n3036: ZB = zb_and(n819, n3034);
    let n3037: ZB = zb_and(n820, n3034);
    let n3038: ZB = zb_and(n659, n3036);
    let n3039: ZB = zb_and(n660, n3036);
    let n3040: ZB = zb_or(n3038, n3039);
    let n3041: ZB = zb_and(n659, n3040);
    let n3042: ZB = zb_and(n660, n3040);
    let n3043: ZB = zb_or(n3041, n3042);
    let n3044: ZB = zb_and(n832, n3043);
    let n3045: ZB = zb_and(n831, n3043);
    let n3046: ZB = zb_or(n3044, n3045);
    let n3047: ZB = zb_and(n832, n3046);
    let n3048: ZB = zb_and(n831, n3046);
    let n3049: ZB = zb_or(n3047, n3048);
    let n3050: ZB = zb_and(n832, n3049);
    let n3051: ZB = zb_and(n831, n3049);
    let n3052: ZB = zb_or(n3050, n3051);
    let n3053: ZB = zb_or(n3037, n3052);
    let n3054: ZB = zb_or(n3035, n3053);
    let n3055: ZB = zb_or(n3021, n3054);
    let n3056: ZB = zb_or(n3019, n3055);
    let n3057: ZB = zb_or(n3005, n3056);
    let n3058: ZB = zb_or(n3003, n3057);
    let n3059: ZB = zb_or(n2989, n3058);
    let n3060: ZB = zb_or(n2987, n3059);
    let n3061: ZB = zb_or(n2973, n3060);
    let n3062: ZB = zb_or(n2971, n3061);
    let n3063: ZB = zb_or(n2957, n3062);
    let n3064: ZB = zb_or(n2955, n3063);
    let n3065: ZB = zb_or(n2941, n3064);
    let n3066: ZB = zb_or(n2939, n3065);
    let n3067: ZB = zb_or(n2820, n3066);
    let n3068: ZB = zb_and(n937, n3067);
    let n3069: ZB = zb_and(n938, n3067);
    let n3070: ZB = zb_and(n949, n3068);
    let n3071: ZB = zb_and(n950, n3068);
    let n3072: ZB = zb_and(n956, n3070);
    let n3073: ZB = zb_and(n957, n3070);
    let n3074: ZB = zb_and(n962, n3072);
    let n3075: ZB = zb_and(n961, n3072);
    let n3076: ZB = zb_or(n3074, n3075);
    let n3077: ZB = zb_or(n3073, n3076);
    let n3078: ZB = zb_and(n971, n3077);
    let n3079: ZB = zb_and(n972, n3077);
    let n3080: ZB = zb_or(n3078, n3079);
    let n3081: ZB = zb_and(n977, n3080);
    let n3082: ZB = zb_and(n978, n3080);
    let n3083: ZB = zb_and(n980, n3082);
    let n3084: ZB = zb_and(n981, n3082);
    let n3085: ZB = zb_or(n3083, n3084);
    let n3086: ZB = zb_and(n987, n3085);
    let n3087: ZB = zb_and(n988, n3085);
    let n3088: ZB = zb_or(n3086, n3087);
    let n3089: ZB = zb_and(n993, n3088);
    let n3090: ZB = zb_and(n994, n3088);
    let n3091: ZB = zb_and(n996, n3090);
    let n3092: ZB = zb_and(n997, n3090);
    let n3093: ZB = zb_or(n3091, n3092);
    let n3094: ZB = zb_and(n1003, n3093);
    let n3095: ZB = zb_and(n1004, n3093);
    let n3096: ZB = zb_or(n3094, n3095);
    let n3097: ZB = zb_and(n1009, n3096);
    let n3098: ZB = zb_and(n1010, n3096);
    let n3099: ZB = zb_and(n1012, n3098);
    let n3100: ZB = zb_and(n1013, n3098);
    let n3101: ZB = zb_and(n1018, n3099);
    let n3102: ZB = zb_and(n1017, n3099);
    let n3103: ZB = zb_or(n3101, n3102);
    let n3104: ZB = zb_or(n3100, n3103);
    let n3105: ZB = zb_and(n1027, n3104);
    let n3106: ZB = zb_and(n1028, n3104);
    let n3107: ZB = zb_or(n3105, n3106);
    let n3108: ZB = zb_and(n1033, n3107);
    let n3109: ZB = zb_and(n1034, n3107);
    let n3110: ZB = zb_or(n3097, n3108);
    let n3111: ZB = zb_or(n3089, n3110);
    let n3112: ZB = zb_or(n3081, n3111);
    let n3113: ZB = zb_and(n1037, n3109);
    let n3114: ZB = zb_and(n1038, n3109);
    let n3115: ZB = zb_and(n1043, n3113);
    let n3116: ZB = zb_and(n1044, n3113);
    let n3117: ZB = zb_and(n962, n3115);
    let n3118: ZB = zb_and(n961, n3115);
    let n3119: ZB = zb_or(n3117, n3118);
    let n3120: ZB = zb_or(n3116, n3119);
    let n3121: ZB = zb_and(n1055, n3120);
    let n3122: ZB = zb_and(n1056, n3120);
    let n3123: ZB = zb_or(n3121, n3122);
    let n3124: ZB = zb_and(n1060, n3123);
    let n3125: ZB = zb_and(n1061, n3123);
    let n3126: ZB = zb_and(n1063, n3125);
    let n3127: ZB = zb_and(n1064, n3125);
    let n3128: ZB = zb_or(n3126, n3127);
    let n3129: ZB = zb_and(n1068, n3128);
    let n3130: ZB = zb_and(n1069, n3128);
    let n3131: ZB = zb_or(n3129, n3130);
    let n3132: ZB = zb_and(n1073, n3131);
    let n3133: ZB = zb_and(n1074, n3131);
    let n3134: ZB = zb_and(n1076, n3133);
    let n3135: ZB = zb_and(n1077, n3133);
    let n3136: ZB = zb_or(n3134, n3135);
    let n3137: ZB = zb_and(n1081, n3136);
    let n3138: ZB = zb_and(n1082, n3136);
    let n3139: ZB = zb_or(n3137, n3138);
    let n3140: ZB = zb_and(n1086, n3139);
    let n3141: ZB = zb_and(n1087, n3139);
    let n3142: ZB = zb_and(n1089, n3141);
    let n3143: ZB = zb_and(n1090, n3141);
    let n3144: ZB = zb_and(n1018, n3142);
    let n3145: ZB = zb_and(n1017, n3142);
    let n3146: ZB = zb_or(n3144, n3145);
    let n3147: ZB = zb_or(n3143, n3146);
    let n3148: ZB = zb_and(n1097, n3147);
    let n3149: ZB = zb_and(n1098, n3147);
    let n3150: ZB = zb_or(n3148, n3149);
    let n3151: ZB = zb_and(n1102, n3150);
    let n3152: ZB = zb_and(n1103, n3150);
    let n3153: ZB = zb_or(n3140, n3151);
    let n3154: ZB = zb_or(n3132, n3153);
    let n3155: ZB = zb_or(n3124, n3154);
    let n3156: ZB = zb_and(n1106, n3152);
    let n3157: ZB = zb_and(n1107, n3152);
    let n3158: ZB = zb_and(n1112, n3156);
    let n3159: ZB = zb_and(n1113, n3156);
    let n3160: ZB = zb_and(n962, n3158);
    let n3161: ZB = zb_and(n961, n3158);
    let n3162: ZB = zb_or(n3160, n3161);
    let n3163: ZB = zb_or(n3159, n3162);
    let n3164: ZB = zb_and(n1124, n3163);
    let n3165: ZB = zb_and(n1125, n3163);
    let n3166: ZB = zb_or(n3164, n3165);
    let n3167: ZB = zb_and(n1129, n3166);
    let n3168: ZB = zb_and(n1130, n3166);
    let n3169: ZB = zb_and(n1132, n3168);
    let n3170: ZB = zb_and(n1133, n3168);
    let n3171: ZB = zb_or(n3169, n3170);
    let n3172: ZB = zb_and(n1137, n3171);
    let n3173: ZB = zb_and(n1138, n3171);
    let n3174: ZB = zb_or(n3172, n3173);
    let n3175: ZB = zb_and(n1142, n3174);
    let n3176: ZB = zb_and(n1143, n3174);
    let n3177: ZB = zb_and(n1145, n3176);
    let n3178: ZB = zb_and(n1146, n3176);
    let n3179: ZB = zb_or(n3177, n3178);
    let n3180: ZB = zb_and(n1150, n3179);
    let n3181: ZB = zb_and(n1151, n3179);
    let n3182: ZB = zb_or(n3180, n3181);
    let n3183: ZB = zb_and(n1155, n3182);
    let n3184: ZB = zb_and(n1156, n3182);
    let n3185: ZB = zb_and(n1158, n3184);
    let n3186: ZB = zb_and(n1159, n3184);
    let n3187: ZB = zb_and(n1018, n3185);
    let n3188: ZB = zb_and(n1017, n3185);
    let n3189: ZB = zb_or(n3187, n3188);
    let n3190: ZB = zb_or(n3186, n3189);
    let n3191: ZB = zb_and(n1166, n3190);
    let n3192: ZB = zb_and(n1167, n3190);
    let n3193: ZB = zb_or(n3191, n3192);
    let n3194: ZB = zb_and(n1171, n3193);
    let n3195: ZB = zb_and(n1172, n3193);
    let n3196: ZB = zb_or(n3183, n3194);
    let n3197: ZB = zb_or(n3175, n3196);
    let n3198: ZB = zb_or(n3167, n3197);
    let n3199: ZB = zb_or(n3157, n3195);
    let n3200: ZB = zb_or(n3155, n3198);
    let n3201: ZB = zb_or(n3114, n3199);
    let n3202: ZB = zb_or(n3112, n3200);
    let n3203: ZB = zb_or(n3071, n3201);
    let n3204: ZB = zb_and(n1184, n3203);
    let n3205: ZB = zb_and(n1185, n3203);
    let n3206: ZB = zb_and(n949, n3204);
    let n3207: ZB = zb_and(n950, n3204);
    let n3208: ZB = zb_and(n1192, n3206);
    let n3209: ZB = zb_and(n1193, n3206);
    let n3210: ZB = zb_and(n962, n3208);
    let n3211: ZB = zb_and(n961, n3208);
    let n3212: ZB = zb_or(n3210, n3211);
    let n3213: ZB = zb_or(n3209, n3212);
    let n3214: ZB = zb_and(n1200, n3213);
    let n3215: ZB = zb_and(n1201, n3213);
    let n3216: ZB = zb_or(n3214, n3215);
    let n3217: ZB = zb_and(n1205, n3216);
    let n3218: ZB = zb_and(n1206, n3216);
    let n3219: ZB = zb_and(n1208, n3218);
    let n3220: ZB = zb_and(n1209, n3218);
    let n3221: ZB = zb_or(n3219, n3220);
    let n3222: ZB = zb_and(n1213, n3221);
    let n3223: ZB = zb_and(n1214, n3221);
    let n3224: ZB = zb_or(n3222, n3223);
    let n3225: ZB = zb_and(n1218, n3224);
    let n3226: ZB = zb_and(n1219, n3224);
    let n3227: ZB = zb_and(n1221, n3226);
    let n3228: ZB = zb_and(n1222, n3226);
    let n3229: ZB = zb_or(n3227, n3228);
    let n3230: ZB = zb_and(n1226, n3229);
    let n3231: ZB = zb_and(n1227, n3229);
    let n3232: ZB = zb_or(n3230, n3231);
    let n3233: ZB = zb_and(n1231, n3232);
    let n3234: ZB = zb_and(n1232, n3232);
    let n3235: ZB = zb_and(n1234, n3234);
    let n3236: ZB = zb_and(n1235, n3234);
    let n3237: ZB = zb_and(n1018, n3235);
    let n3238: ZB = zb_and(n1017, n3235);
    let n3239: ZB = zb_or(n3237, n3238);
    let n3240: ZB = zb_or(n3236, n3239);
    let n3241: ZB = zb_and(n1246, n3240);
    let n3242: ZB = zb_and(n1247, n3240);
    let n3243: ZB = zb_or(n3241, n3242);
    let n3244: ZB = zb_and(n1251, n3243);
    let n3245: ZB = zb_and(n1252, n3243);
    let n3246: ZB = zb_or(n3233, n3244);
    let n3247: ZB = zb_or(n3225, n3246);
    let n3248: ZB = zb_or(n3217, n3247);
    let n3249: ZB = zb_and(n1037, n3245);
    let n3250: ZB = zb_and(n1038, n3245);
    let n3251: ZB = zb_and(n1257, n3249);
    let n3252: ZB = zb_and(n1258, n3249);
    let n3253: ZB = zb_and(n962, n3251);
    let n3254: ZB = zb_and(n961, n3251);
    let n3255: ZB = zb_or(n3253, n3254);
    let n3256: ZB = zb_or(n3252, n3255);
    let n3257: ZB = zb_and(n1265, n3256);
    let n3258: ZB = zb_and(n1266, n3256);
    let n3259: ZB = zb_or(n3257, n3258);
    let n3260: ZB = zb_and(n1270, n3259);
    let n3261: ZB = zb_and(n1271, n3259);
    let n3262: ZB = zb_and(n1273, n3261);
    let n3263: ZB = zb_and(n1274, n3261);
    let n3264: ZB = zb_or(n3262, n3263);
    let n3265: ZB = zb_and(n1278, n3264);
    let n3266: ZB = zb_and(n1279, n3264);
    let n3267: ZB = zb_or(n3265, n3266);
    let n3268: ZB = zb_and(n1283, n3267);
    let n3269: ZB = zb_and(n1284, n3267);
    let n3270: ZB = zb_and(n1286, n3269);
    let n3271: ZB = zb_and(n1287, n3269);
    let n3272: ZB = zb_or(n3270, n3271);
    let n3273: ZB = zb_and(n1291, n3272);
    let n3274: ZB = zb_and(n1292, n3272);
    let n3275: ZB = zb_or(n3273, n3274);
    let n3276: ZB = zb_and(n1296, n3275);
    let n3277: ZB = zb_and(n1297, n3275);
    let n3278: ZB = zb_and(n1299, n3277);
    let n3279: ZB = zb_and(n1300, n3277);
    let n3280: ZB = zb_and(n1018, n3278);
    let n3281: ZB = zb_and(n1017, n3278);
    let n3282: ZB = zb_or(n3280, n3281);
    let n3283: ZB = zb_or(n3279, n3282);
    let n3284: ZB = zb_and(n1307, n3283);
    let n3285: ZB = zb_and(n1308, n3283);
    let n3286: ZB = zb_or(n3284, n3285);
    let n3287: ZB = zb_and(n1312, n3286);
    let n3288: ZB = zb_and(n1313, n3286);
    let n3289: ZB = zb_or(n3276, n3287);
    let n3290: ZB = zb_or(n3268, n3289);
    let n3291: ZB = zb_or(n3260, n3290);
    let n3292: ZB = zb_and(n1106, n3288);
    let n3293: ZB = zb_and(n1107, n3288);
    let n3294: ZB = zb_and(n1318, n3292);
    let n3295: ZB = zb_and(n1319, n3292);
    let n3296: ZB = zb_and(n962, n3294);
    let n3297: ZB = zb_and(n961, n3294);
    let n3298: ZB = zb_or(n3296, n3297);
    let n3299: ZB = zb_or(n3295, n3298);
    let n3300: ZB = zb_and(n1326, n3299);
    let n3301: ZB = zb_and(n1327, n3299);
    let n3302: ZB = zb_or(n3300, n3301);
    let n3303: ZB = zb_and(n1331, n3302);
    let n3304: ZB = zb_and(n1332, n3302);
    let n3305: ZB = zb_and(n1334, n3304);
    let n3306: ZB = zb_and(n1335, n3304);
    let n3307: ZB = zb_or(n3305, n3306);
    let n3308: ZB = zb_and(n1339, n3307);
    let n3309: ZB = zb_and(n1340, n3307);
    let n3310: ZB = zb_or(n3308, n3309);
    let n3311: ZB = zb_and(n1344, n3310);
    let n3312: ZB = zb_and(n1345, n3310);
    let n3313: ZB = zb_and(n1347, n3312);
    let n3314: ZB = zb_and(n1348, n3312);
    let n3315: ZB = zb_or(n3313, n3314);
    let n3316: ZB = zb_and(n1352, n3315);
    let n3317: ZB = zb_and(n1353, n3315);
    let n3318: ZB = zb_or(n3316, n3317);
    let n3319: ZB = zb_and(n1357, n3318);
    let n3320: ZB = zb_and(n1358, n3318);
    let n3321: ZB = zb_and(n1360, n3320);
    let n3322: ZB = zb_and(n1361, n3320);
    let n3323: ZB = zb_and(n1018, n3321);
    let n3324: ZB = zb_and(n1017, n3321);
    let n3325: ZB = zb_or(n3323, n3324);
    let n3326: ZB = zb_or(n3322, n3325);
    let n3327: ZB = zb_and(n1368, n3326);
    let n3328: ZB = zb_and(n1369, n3326);
    let n3329: ZB = zb_or(n3327, n3328);
    let n3330: ZB = zb_and(n1373, n3329);
    let n3331: ZB = zb_and(n1374, n3329);
    let n3332: ZB = zb_or(n3319, n3330);
    let n3333: ZB = zb_or(n3311, n3332);
    let n3334: ZB = zb_or(n3303, n3333);
    let n3335: ZB = zb_or(n3293, n3331);
    let n3336: ZB = zb_or(n3291, n3334);
    let n3337: ZB = zb_or(n3250, n3335);
    let n3338: ZB = zb_or(n3248, n3336);
    let n3339: ZB = zb_or(n3207, n3337);
    let n3340: ZB = zb_and(n1384, n3339);
    let n3341: ZB = zb_and(n1385, n3339);
    let n3342: ZB = zb_and(n949, n3340);
    let n3343: ZB = zb_and(n950, n3340);
    let n3344: ZB = zb_and(n1392, n3342);
    let n3345: ZB = zb_and(n1393, n3342);
    let n3346: ZB = zb_and(n962, n3344);
    let n3347: ZB = zb_and(n961, n3344);
    let n3348: ZB = zb_or(n3346, n3347);
    let n3349: ZB = zb_or(n3345, n3348);
    let n3350: ZB = zb_and(n1400, n3349);
    let n3351: ZB = zb_and(n1401, n3349);
    let n3352: ZB = zb_or(n3350, n3351);
    let n3353: ZB = zb_and(n1405, n3352);
    let n3354: ZB = zb_and(n1406, n3352);
    let n3355: ZB = zb_and(n1408, n3354);
    let n3356: ZB = zb_and(n1409, n3354);
    let n3357: ZB = zb_or(n3355, n3356);
    let n3358: ZB = zb_and(n1413, n3357);
    let n3359: ZB = zb_and(n1414, n3357);
    let n3360: ZB = zb_or(n3358, n3359);
    let n3361: ZB = zb_and(n1418, n3360);
    let n3362: ZB = zb_and(n1419, n3360);
    let n3363: ZB = zb_and(n1421, n3362);
    let n3364: ZB = zb_and(n1422, n3362);
    let n3365: ZB = zb_or(n3363, n3364);
    let n3366: ZB = zb_and(n1426, n3365);
    let n3367: ZB = zb_and(n1427, n3365);
    let n3368: ZB = zb_or(n3366, n3367);
    let n3369: ZB = zb_and(n1431, n3368);
    let n3370: ZB = zb_and(n1432, n3368);
    let n3371: ZB = zb_and(n1434, n3370);
    let n3372: ZB = zb_and(n1435, n3370);
    let n3373: ZB = zb_and(n1018, n3371);
    let n3374: ZB = zb_and(n1017, n3371);
    let n3375: ZB = zb_or(n3373, n3374);
    let n3376: ZB = zb_or(n3372, n3375);
    let n3377: ZB = zb_and(n1446, n3376);
    let n3378: ZB = zb_and(n1447, n3376);
    let n3379: ZB = zb_or(n3377, n3378);
    let n3380: ZB = zb_and(n1451, n3379);
    let n3381: ZB = zb_and(n1452, n3379);
    let n3382: ZB = zb_or(n3369, n3380);
    let n3383: ZB = zb_or(n3361, n3382);
    let n3384: ZB = zb_or(n3353, n3383);
    let n3385: ZB = zb_and(n1037, n3381);
    let n3386: ZB = zb_and(n1038, n3381);
    let n3387: ZB = zb_and(n1457, n3385);
    let n3388: ZB = zb_and(n1458, n3385);
    let n3389: ZB = zb_and(n962, n3387);
    let n3390: ZB = zb_and(n961, n3387);
    let n3391: ZB = zb_or(n3389, n3390);
    let n3392: ZB = zb_or(n3388, n3391);
    let n3393: ZB = zb_and(n1465, n3392);
    let n3394: ZB = zb_and(n1466, n3392);
    let n3395: ZB = zb_or(n3393, n3394);
    let n3396: ZB = zb_and(n1470, n3395);
    let n3397: ZB = zb_and(n1471, n3395);
    let n3398: ZB = zb_and(n1473, n3397);
    let n3399: ZB = zb_and(n1474, n3397);
    let n3400: ZB = zb_or(n3398, n3399);
    let n3401: ZB = zb_and(n1478, n3400);
    let n3402: ZB = zb_and(n1479, n3400);
    let n3403: ZB = zb_or(n3401, n3402);
    let n3404: ZB = zb_and(n1483, n3403);
    let n3405: ZB = zb_and(n1484, n3403);
    let n3406: ZB = zb_and(n1486, n3405);
    let n3407: ZB = zb_and(n1487, n3405);
    let n3408: ZB = zb_or(n3406, n3407);
    let n3409: ZB = zb_and(n1491, n3408);
    let n3410: ZB = zb_and(n1492, n3408);
    let n3411: ZB = zb_or(n3409, n3410);
    let n3412: ZB = zb_and(n1496, n3411);
    let n3413: ZB = zb_and(n1497, n3411);
    let n3414: ZB = zb_and(n1499, n3413);
    let n3415: ZB = zb_and(n1500, n3413);
    let n3416: ZB = zb_and(n1018, n3414);
    let n3417: ZB = zb_and(n1017, n3414);
    let n3418: ZB = zb_or(n3416, n3417);
    let n3419: ZB = zb_or(n3415, n3418);
    let n3420: ZB = zb_and(n1507, n3419);
    let n3421: ZB = zb_and(n1508, n3419);
    let n3422: ZB = zb_or(n3420, n3421);
    let n3423: ZB = zb_and(n1512, n3422);
    let n3424: ZB = zb_and(n1513, n3422);
    let n3425: ZB = zb_or(n3412, n3423);
    let n3426: ZB = zb_or(n3404, n3425);
    let n3427: ZB = zb_or(n3396, n3426);
    let n3428: ZB = zb_and(n1106, n3424);
    let n3429: ZB = zb_and(n1107, n3424);
    let n3430: ZB = zb_and(n1518, n3428);
    let n3431: ZB = zb_and(n1519, n3428);
    let n3432: ZB = zb_and(n962, n3430);
    let n3433: ZB = zb_and(n961, n3430);
    let n3434: ZB = zb_or(n3432, n3433);
    let n3435: ZB = zb_or(n3431, n3434);
    let n3436: ZB = zb_and(n1526, n3435);
    let n3437: ZB = zb_and(n1527, n3435);
    let n3438: ZB = zb_or(n3436, n3437);
    let n3439: ZB = zb_and(n1531, n3438);
    let n3440: ZB = zb_and(n1532, n3438);
    let n3441: ZB = zb_and(n1534, n3440);
    let n3442: ZB = zb_and(n1535, n3440);
    let n3443: ZB = zb_or(n3441, n3442);
    let n3444: ZB = zb_and(n1539, n3443);
    let n3445: ZB = zb_and(n1540, n3443);
    let n3446: ZB = zb_or(n3444, n3445);
    let n3447: ZB = zb_and(n1544, n3446);
    let n3448: ZB = zb_and(n1545, n3446);
    let n3449: ZB = zb_and(n1547, n3448);
    let n3450: ZB = zb_and(n1548, n3448);
    let n3451: ZB = zb_or(n3449, n3450);
    let n3452: ZB = zb_and(n1552, n3451);
    let n3453: ZB = zb_and(n1553, n3451);
    let n3454: ZB = zb_or(n3452, n3453);
    let n3455: ZB = zb_and(n1557, n3454);
    let n3456: ZB = zb_and(n1558, n3454);
    let n3457: ZB = zb_and(n1560, n3456);
    let n3458: ZB = zb_and(n1561, n3456);
    let n3459: ZB = zb_and(n1018, n3457);
    let n3460: ZB = zb_and(n1017, n3457);
    let n3461: ZB = zb_or(n3459, n3460);
    let n3462: ZB = zb_or(n3458, n3461);
    let n3463: ZB = zb_and(n1568, n3462);
    let n3464: ZB = zb_and(n1569, n3462);
    let n3465: ZB = zb_or(n3463, n3464);
    let n3466: ZB = zb_and(n1573, n3465);
    let n3467: ZB = zb_and(n1574, n3465);
    let n3468: ZB = zb_or(n3455, n3466);
    let n3469: ZB = zb_or(n3447, n3468);
    let n3470: ZB = zb_or(n3439, n3469);
    let n3471: ZB = zb_or(n3429, n3467);
    let n3472: ZB = zb_or(n3427, n3470);
    let n3473: ZB = zb_or(n3386, n3471);
    let n3474: ZB = zb_or(n3384, n3472);
    let n3475: ZB = zb_or(n3343, n3473);
    let n3476: ZB = zb_or(n3338, n3474);
    let n3477: ZB = zsel_b(n3338, n1182, n1382);
    let n3478: ZB = zb_or(n3341, n3475);
    let n3479: ZB = zb_or(n3202, n3476);
    let n3480: ZB = zsel_b(n3202, n926, n3477);
    let n3481: ZB = zb_or(n3205, n3478);
    let n3482: ZB = zb_or(n3069, n3481);
    let n3483: ZB = zb_and(n2407, n3479);
    let n3484: ZB = zb_and(n1592, n3479);
    let n3485: ZB = zb_or(n3483, n3484);
    let n3486: ZB = zb_and(n2407, n3482);
    let n3487: ZB = zb_or(n3485, n3486);
    let n3488: ZB = zsel_b(n3485, n3480, n1591);
    let n3489: ZB = zb_and(n1597, n3487);
    let n3490: ZB = zb_and(n1596, n3487);
    let n3491: ZB = zb_or(n3489, n3490);
    let n3492: ZB = zb_and(n1597, n3491);
    let n3493: ZB = zb_and(n1596, n3491);
    let n3494: ZB = zb_or(n3492, n3493);
    let n3495: ZB = zn_lt(n2814, zn_splat(P8::from_raw(65536i32)));
    let n3496: ZB = zn_ge(n2814, zn_splat(P8::from_raw(65536i32)));
    let n3497: ZN = zsel_n(n3495, zn_splat(P8::from_raw(65536i32)), n2814);
    let n3498: ZN = zsel_n(n1596, n3497, n2814);
    let n3499: ZB = zb_and(n1596, n3494);
    let n3500: ZB = zb_and(n1597, n3494);
    let n3501: ZB = zb_and(n3495, n3499);
    let n3502: ZB = zb_and(n3496, n3499);
    let n3503: ZB = zb_or(n3501, n3502);
    let n3504: ZB = zb_and(n296, n3500);
    let n3505: ZB = zb_and(n297, n3500);
    let n3506: ZB = zb_or(n3504, n3505);
    let n3507: ZB = zb_or(n3503, n3506);
    let n3508: ZB = zn_gt(n3498, zn_splat(P8::from_raw(0i32)));
    let n3509: ZB = zn_le(n3498, zn_splat(P8::from_raw(0i32)));
    let n3510: ZB = zb_and(n301, n3507);
    let n3511: ZB = zb_and(n302, n3507);
    let n3512: ZB = zb_and(n1613, n3510);
    let n3513: ZB = zb_and(n1614, n3510);
    let n3514: ZB = zb_or(n3512, n3513);
    let n3515: ZB = zb_and(n1623, n3514);
    let n3516: ZB = zb_and(n1624, n3514);
    let n3517: ZB = zb_or(n3515, n3516);
    let n3518: ZB = zb_and(n1597, n3511);
    let n3519: ZB = zb_and(n1596, n3511);
    let n3520: ZB = zb_or(n3518, n3519);
    let n3521: ZB = zb_and(n1638, n3520);
    let n3522: ZB = zb_and(n1639, n3520);
    let n3523: ZB = zb_and(n1642, n3521);
    let n3524: ZB = zb_and(n1007, n3521);
    let n3525: ZB = zb_and(n1645, n3524);
    let n3526: ZB = zb_and(n1031, n3524);
    let n3527: ZB = zb_and(n1648, n3523);
    let n3528: ZB = zb_and(n1649, n3523);
    let n3529: ZB = zb_and(n1656, n3525);
    let n3530: ZB = zb_and(n1657, n3525);
    let n3531: ZB = zb_and(n1007, n3526);
    let n3532: ZB = zb_or(n3529, n3530);
    let n3533: ZB = zb_or(n3527, n3528);
    let n3534: ZB = zb_or(n3531, n3532);
    let n3535: ZB = zb_or(n3533, n3534);
    let n3536: ZB = zb_and(n1642, n3522);
    let n3537: ZB = zb_and(n1007, n3522);
    let n3538: ZB = zb_or(n3536, n3537);
    let n3539: ZB = zb_or(n3535, n3538);
    let n3540: ZB = zb_and(n1685, n3539);
    let n3541: ZB = zb_and(n1684, n3539);
    let n3542: ZB = zb_or(n3540, n3541);
    let n3543: ZB = zb_and(n1692, n3542);
    let n3544: ZB = zb_and(n1693, n3542);
    let n3545: ZB = zb_or(n3543, n3544);
    let n3546: ZB = zb_and(n1597, n3545);
    let n3547: ZB = zb_and(n1596, n3545);
    let n3548: ZB = zb_and(n1701, n3546);
    let n3549: ZB = zb_and(n1702, n3546);
    let n3550: ZB = zb_or(n3548, n3549);
    let n3551: ZB = zb_or(n3547, n3550);
    let n3552: ZB = zb_and(n3508, n3551);
    let n3553: ZB = zb_and(n3509, n3551);
    let n3554: ZB = zb_or(n3552, n3553);
    let n3555: ZB = zb_or(n3517, n3554);
    let n3556: ZB = zb_and(n1747, n3555);
    let n3557: ZB = zb_and(n1748, n3555);
    let n3558: ZB = zb_or(n3556, n3557);
    let n3561: ZN = zsel_n(n3485, n2474, n2332);
    let n3563: ZB = zb_and(n1656, n3522);
    let n3564: ZB = zb_and(n1657, n3522);
    let n3565: ZB = zb_or(n3563, n3564);
    let n3566: ZB = zb_or(n3535, n3565);
    let n3567: ZB = zb_and(n1792, n3566);
    let n3568: ZB = zb_and(n1791, n3566);
    let n3569: ZB = zb_or(n3567, n3568);
    let n3570: ZB = zb_and(n1692, n3569);
    let n3571: ZB = zb_and(n1693, n3569);
    let n3572: ZB = zb_or(n3570, n3571);
    let n3573: ZB = zb_and(n1803, n3572);
    let n3574: ZB = zb_and(n1802, n3572);
    let n3575: ZB = zb_or(n3573, n3574);
    let n3576: ZB = zb_and(n1803, n3575);
    let n3577: ZB = zb_and(n1802, n3575);
    let n3578: ZB = zb_or(n3576, n3577);
    let n3579: ZB = zb_and(n1802, n3578);
    let n3580: ZB = zb_and(n1803, n3578);
    let n3581: ZB = zb_or(n3579, n3580);
    let n3582: ZB = zb_and(n1802, n3581);
    let n3583: ZB = zb_and(n1803, n3581);
    let n3584: ZB = zb_or(n3582, n3583);
    let n3585: ZB = zb_and(n1597, n3584);
    let n3586: ZB = zb_and(n1596, n3584);
    let n3587: ZB = zb_and(n1819, n3585);
    let n3588: ZB = zb_and(n1820, n3585);
    let n3589: ZB = zb_or(n3587, n3588);
    let n3590: ZB = zb_or(n3586, n3589);
    let n3591: ZB = zb_and(n3508, n3590);
    let n3592: ZB = zb_and(n3509, n3590);
    let n3593: ZB = zb_or(n3591, n3592);
    let n3594: ZB = zb_or(n3517, n3593);
    let n3595: ZB = zb_and(n1747, n3594);
    let n3596: ZB = zb_and(n1748, n3594);
    let n3597: ZB = zb_or(n3595, n3596);
    let n3600: ZB = zb_and(n1648, n3522);
    let n3601: ZB = zb_and(n1649, n3522);
    let n3602: ZB = zb_or(n3600, n3601);
    let n3603: ZB = zb_or(n3535, n3602);
    let n3604: ZB = zb_and(n1859, n3603);
    let n3605: ZB = zb_and(n1858, n3603);
    let n3606: ZB = zb_or(n3604, n3605);
    let n3607: ZB = zb_and(n1692, n3606);
    let n3608: ZB = zb_and(n1693, n3606);
    let n3609: ZB = zb_or(n3607, n3608);
    let n3610: ZB = zb_and(n1870, n3609);
    let n3611: ZB = zb_and(n1869, n3609);
    let n3612: ZB = zb_or(n3610, n3611);
    let n3613: ZB = zb_and(n1870, n3612);
    let n3614: ZB = zb_and(n1869, n3612);
    let n3615: ZB = zb_or(n3613, n3614);
    let n3616: ZB = zb_and(n1869, n3615);
    let n3617: ZB = zb_and(n1870, n3615);
    let n3618: ZB = zb_or(n3616, n3617);
    let n3619: ZB = zb_and(n1869, n3618);
    let n3620: ZB = zb_and(n1870, n3618);
    let n3621: ZB = zb_or(n3619, n3620);
    let n3622: ZB = zb_and(n1597, n3621);
    let n3623: ZB = zb_and(n1596, n3621);
    let n3624: ZB = zb_and(n1886, n3622);
    let n3625: ZB = zb_and(n1887, n3622);
    let n3626: ZB = zb_or(n3624, n3625);
    let n3627: ZB = zb_or(n3623, n3626);
    let n3628: ZB = zb_and(n3508, n3627);
    let n3629: ZB = zb_and(n3509, n3627);
    let n3630: ZB = zb_or(n3628, n3629);
    let n3631: ZB = zb_or(n3517, n3630);
    let n3632: ZB = zb_and(n1747, n3631);
    let n3633: ZB = zb_and(n1748, n3631);
    let n3634: ZB = zb_or(n3632, n3633);
    let n3637: ZB = zb_and(n295, n3551);
    let n3638: ZB = zb_and(r_c312, n3551);
    let n3639: ZB = zb_and(n1713, n3637);
    let n3640: ZB = zb_and(n1714, n3637);
    let n3641: ZB = zb_and(n1717, n3640);
    let n3642: ZB = zb_and(n1716, n3640);
    let n3643: ZB = zb_or(n3641, n3642);
    let n3644: ZB = zb_and(n1717, n3643);
    let n3645: ZB = zb_and(n1716, n3643);
    let n3646: ZB = zb_or(n3644, n3645);
    let n3647: ZB = zb_and(n1716, n3646);
    let n3648: ZB = zb_and(n1717, n3646);
    let n3649: ZB = zb_and(n1720, n3648);
    let n3650: ZB = zb_and(n1719, n3648);
    let n3651: ZB = zb_or(n3649, n3650);
    let n3652: ZB = zb_and(n1720, n3651);
    let n3653: ZB = zb_and(n1719, n3651);
    let n3654: ZB = zb_or(n3652, n3653);
    let n3655: ZB = zb_and(n1719, n3654);
    let n3656: ZB = zb_and(n1720, n3654);
    let n3657: ZB = zb_or(n3655, n3656);
    let n3658: ZB = zb_or(n3647, n3657);
    let n3659: ZB = zb_and(n1724, n3658);
    let n3660: ZB = zb_and(n1723, n3658);
    let n3661: ZB = zb_or(n3659, n3660);
    let n3662: ZB = zb_or(n3639, n3661);
    let n3663: ZB = zb_or(n3638, n3662);
    let n3664: ZB = zb_and(n3508, n3663);
    let n3665: ZB = zb_and(n3509, n3663);
    let n3666: ZB = zb_or(n3664, n3665);
    let n3667: ZB = zb_or(n3517, n3666);
    let n3668: ZB = zb_and(n1747, n3667);
    let n3669: ZB = zb_and(n1748, n3667);
    let n3670: ZB = zb_or(n3668, n3669);
    let n3673: ZB = zb_and(n295, n3590);
    let n3674: ZB = zb_and(r_c312, n3590);
    let n3675: ZB = zb_and(n1713, n3673);
    let n3676: ZB = zb_and(n1714, n3673);
    let n3677: ZB = zb_and(n1717, n3676);
    let n3678: ZB = zb_and(n1716, n3676);
    let n3679: ZB = zb_or(n3677, n3678);
    let n3680: ZB = zb_and(n1717, n3679);
    let n3681: ZB = zb_and(n1716, n3679);
    let n3682: ZB = zb_or(n3680, n3681);
    let n3683: ZB = zb_and(n1716, n3682);
    let n3684: ZB = zb_and(n1717, n3682);
    let n3685: ZB = zb_and(n1720, n3684);
    let n3686: ZB = zb_and(n1719, n3684);
    let n3687: ZB = zb_or(n3685, n3686);
    let n3688: ZB = zb_and(n1720, n3687);
    let n3689: ZB = zb_and(n1719, n3687);
    let n3690: ZB = zb_or(n3688, n3689);
    let n3691: ZB = zb_and(n1719, n3690);
    let n3692: ZB = zb_and(n1720, n3690);
    let n3693: ZB = zb_or(n3691, n3692);
    let n3694: ZB = zb_or(n3683, n3693);
    let n3695: ZB = zb_and(n1724, n3694);
    let n3696: ZB = zb_and(n1723, n3694);
    let n3697: ZB = zb_or(n3695, n3696);
    let n3698: ZB = zb_or(n3675, n3697);
    let n3699: ZB = zb_or(n3674, n3698);
    let n3700: ZB = zb_and(n3508, n3699);
    let n3701: ZB = zb_and(n3509, n3699);
    let n3702: ZB = zb_or(n3700, n3701);
    let n3703: ZB = zb_or(n3517, n3702);
    let n3704: ZB = zb_and(n1747, n3703);
    let n3705: ZB = zb_and(n1748, n3703);
    let n3706: ZB = zb_or(n3704, n3705);
    let n3709: ZB = zb_and(n295, n3627);
    let n3710: ZB = zb_and(r_c312, n3627);
    let n3711: ZB = zb_and(n1713, n3709);
    let n3712: ZB = zb_and(n1714, n3709);
    let n3713: ZB = zb_and(n1717, n3712);
    let n3714: ZB = zb_and(n1716, n3712);
    let n3715: ZB = zb_or(n3713, n3714);
    let n3716: ZB = zb_and(n1717, n3715);
    let n3717: ZB = zb_and(n1716, n3715);
    let n3718: ZB = zb_or(n3716, n3717);
    let n3719: ZB = zb_and(n1716, n3718);
    let n3720: ZB = zb_and(n1717, n3718);
    let n3721: ZB = zb_and(n1720, n3720);
    let n3722: ZB = zb_and(n1719, n3720);
    let n3723: ZB = zb_or(n3721, n3722);
    let n3724: ZB = zb_and(n1720, n3723);
    let n3725: ZB = zb_and(n1719, n3723);
    let n3726: ZB = zb_or(n3724, n3725);
    let n3727: ZB = zb_and(n1719, n3726);
    let n3728: ZB = zb_and(n1720, n3726);
    let n3729: ZB = zb_or(n3727, n3728);
    let n3730: ZB = zb_or(n3719, n3729);
    let n3731: ZB = zb_and(n1724, n3730);
    let n3732: ZB = zb_and(n1723, n3730);
    let n3733: ZB = zb_or(n3731, n3732);
    let n3734: ZB = zb_or(n3711, n3733);
    let n3735: ZB = zb_or(n3710, n3734);
    let n3736: ZB = zb_and(n3508, n3735);
    let n3737: ZB = zb_and(n3509, n3735);
    let n3738: ZB = zb_or(n3736, n3737);
    let n3739: ZB = zb_or(n3517, n3738);
    let n3740: ZB = zb_and(n1747, n3739);
    let n3741: ZB = zb_and(n1748, n3739);
    let n3742: ZB = zb_or(n3740, n3741);
    let n3745: ZB = zb_and(n233, n3508);
    let n3746: ZB = zb_not(n3745);
    let n3747: ZN = zsel_n(n3745, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n3748: ZB = zb_or(r_c41, n3745);
    let n3749: ZN = zsel_n(n301, r_c20, n3747);
    let n3750: ZB = zsel_b(n301, r_c41, n3748);
    let n3751: ZB = zb_and(n3554, n3745);
    let n3752: ZB = zb_and(n3554, n3746);
    let n3753: ZB = zb_and(n1689, n3751);
    let n3754: ZB = zb_and(n1732, n3751);
    let n3755: ZB = zb_or(n3753, n3754);
    let n3756: ZB = zb_and(n1734, n3755);
    let n3757: ZB = zb_and(n1735, n3755);
    let n3758: ZB = zb_and(n1736, n3757);
    let n3759: ZB = zb_and(n1737, n3757);
    let n3760: ZB = zb_or(n3758, n3759);
    let n3761: ZB = zb_or(n3756, n3760);
    let n3762: ZB = zb_and(n1741, n3761);
    let n3763: ZB = zb_and(n1740, n3761);
    let n3764: ZB = zb_or(n3762, n3763);
    let n3765: ZB = zb_or(n3752, n3764);
    let n3766: ZB = zb_or(n3517, n3765);
    let n3767: ZB = zb_and(n1747, n3766);
    let n3768: ZB = zb_and(n1748, n3766);
    let n3769: ZB = zb_or(n3767, n3768);
    let n3770: ZB = zb_and(n1748, n3769);
    let n3771: ZB = zn_gt(n3749, zn_splat(P8::from_raw(0i32)));
    let n3772: ZB = zn_le(n3749, zn_splat(P8::from_raw(0i32)));
    let n3773: ZB = zb_and(n3770, n3771);
    let n3774: ZB = zb_and(n3770, n3772);
    let n3775: ZB = zb_or(n3773, n3774);
    let n3776: ZB = zb_and(n3593, n3745);
    let n3777: ZB = zb_and(n3593, n3746);
    let n3778: ZB = zb_or(n3776, n3777);
    let n3779: ZB = zb_or(n3517, n3778);
    let n3780: ZB = zb_and(n1747, n3779);
    let n3781: ZB = zb_and(n1748, n3779);
    let n3782: ZB = zb_or(n3780, n3781);
    let n3783: ZB = zb_and(n1748, n3782);
    let n3784: ZB = zb_and(n3771, n3783);
    let n3785: ZB = zb_and(n3772, n3783);
    let n3786: ZB = zb_or(n3784, n3785);
    let n3787: ZB = zb_and(n3630, n3745);
    let n3788: ZB = zb_and(n3630, n3746);
    let n3789: ZB = zb_or(n3787, n3788);
    let n3790: ZB = zb_or(n3517, n3789);
    let n3791: ZB = zb_and(n1747, n3790);
    let n3792: ZB = zb_and(n1748, n3790);
    let n3793: ZB = zb_or(n3791, n3792);
    let n3794: ZB = zb_and(n1748, n3793);
    let n3795: ZB = zb_and(n3771, n3794);
    let n3796: ZB = zb_and(n3772, n3794);
    let n3797: ZB = zb_or(n3795, n3796);
    let n3798: ZB = zb_or(n3751, n3752);
    let n3799: ZB = zb_or(n3517, n3798);
    let n3800: ZB = zb_and(n1747, n3799);
    let n3801: ZB = zb_and(n1748, n3799);
    let n3802: ZB = zb_or(n3800, n3801);
    let n3803: ZB = zb_and(n1748, n3802);
    let n3804: ZB = zb_and(n3771, n3803);
    let n3805: ZB = zb_and(n3772, n3803);
    let n3806: ZB = zb_or(n3804, n3805);
    let n3807: ZB = zb_and(n3666, n3745);
    let n3808: ZB = zb_and(n3666, n3746);
    let n3809: ZB = zb_and(n1689, n3807);
    let n3810: ZB = zb_and(n1732, n3807);
    let n3811: ZB = zb_or(n3809, n3810);
    let n3812: ZB = zb_and(n1734, n3811);
    let n3813: ZB = zb_and(n1735, n3811);
    let n3814: ZB = zb_and(n1736, n3813);
    let n3815: ZB = zb_and(n1737, n3813);
    let n3816: ZB = zb_or(n3814, n3815);
    let n3817: ZB = zb_or(n3812, n3816);
    let n3818: ZB = zb_and(n1741, n3817);
    let n3819: ZB = zb_and(n1740, n3817);
    let n3820: ZB = zb_or(n3818, n3819);
    let n3821: ZB = zb_or(n3808, n3820);
    let n3822: ZB = zb_or(n3517, n3821);
    let n3823: ZB = zb_and(n1747, n3822);
    let n3824: ZB = zb_and(n1748, n3822);
    let n3825: ZB = zb_or(n3823, n3824);
    let n3826: ZB = zb_and(n1748, n3825);
    let n3827: ZB = zb_and(n3771, n3826);
    let n3828: ZB = zb_and(n3772, n3826);
    let n3829: ZB = zb_or(n3827, n3828);
    let n3830: ZB = zb_and(n3702, n3745);
    let n3831: ZB = zb_and(n3702, n3746);
    let n3832: ZB = zb_or(n3830, n3831);
    let n3833: ZB = zb_or(n3517, n3832);
    let n3834: ZB = zb_and(n1747, n3833);
    let n3835: ZB = zb_and(n1748, n3833);
    let n3836: ZB = zb_or(n3834, n3835);
    let n3837: ZB = zb_and(n1748, n3836);
    let n3838: ZB = zb_and(n3771, n3837);
    let n3839: ZB = zb_and(n3772, n3837);
    let n3840: ZB = zb_or(n3838, n3839);
    let n3841: ZB = zb_and(n3738, n3745);
    let n3842: ZB = zb_and(n3738, n3746);
    let n3843: ZB = zb_or(n3841, n3842);
    let n3844: ZB = zb_or(n3517, n3843);
    let n3845: ZB = zb_and(n1747, n3844);
    let n3846: ZB = zb_and(n1748, n3844);
    let n3847: ZB = zb_or(n3845, n3846);
    let n3848: ZB = zb_and(n1748, n3847);
    let n3849: ZB = zb_and(n3771, n3848);
    let n3850: ZB = zb_and(n3772, n3848);
    let n3851: ZB = zb_or(n3849, n3850);
    let n3852: ZB = zb_or(n3807, n3808);
    let n3853: ZB = zb_or(n3517, n3852);
    let n3854: ZB = zb_and(n1747, n3853);
    let n3855: ZB = zb_and(n1748, n3853);
    let n3856: ZB = zb_or(n3854, n3855);
    let n3857: ZB = zb_and(n1748, n3856);
    let n3858: ZB = zb_and(n3771, n3857);
    let n3859: ZB = zb_and(n3772, n3857);
    let n3860: ZB = zb_or(n3858, n3859);
    let n3863: ZB = zb_and(n1592, n3482);
    let n3864: ZB = zb_and(n1597, n3863);
    let n3865: ZB = zb_and(n1596, n3863);
    let n3866: ZB = zb_or(n3864, n3865);
    let n3867: ZB = zb_and(n1597, n3866);
    let n3868: ZB = zb_and(n1596, n3866);
    let n3869: ZB = zb_or(n3867, n3868);
    let n3870: ZB = zb_and(n1596, n3869);
    let n3871: ZB = zb_and(n1597, n3869);
    let n3872: ZB = zb_and(n3495, n3870);
    let n3873: ZB = zb_and(n3496, n3870);
    let n3874: ZB = zb_or(n3872, n3873);
    let n3875: ZB = zb_and(n296, n3871);
    let n3876: ZB = zb_and(n297, n3871);
    let n3877: ZB = zb_or(n3875, n3876);
    let n3878: ZB = zb_or(n3874, n3877);
    let n3879: ZB = zb_and(n301, n3878);
    let n3880: ZB = zb_and(n302, n3878);
    let n3881: ZB = zb_and(n1613, n3879);
    let n3882: ZB = zb_and(n1614, n3879);
    let n3883: ZB = zb_or(n3881, n3882);
    let n3884: ZB = zb_and(n1623, n3883);
    let n3885: ZB = zb_and(n1624, n3883);
    let n3886: ZB = zb_or(n3884, n3885);
    let n3887: ZB = zb_and(n1597, n3880);
    let n3888: ZB = zb_and(n1596, n3880);
    let n3889: ZB = zb_or(n3887, n3888);
    let n3890: ZB = zb_and(n1638, n3889);
    let n3891: ZB = zb_and(n1639, n3889);
    let n3892: ZB = zb_and(n1642, n3890);
    let n3893: ZB = zb_and(n1007, n3890);
    let n3894: ZB = zb_and(n1645, n3893);
    let n3895: ZB = zb_and(n1031, n3893);
    let n3896: ZB = zb_and(n1648, n3892);
    let n3897: ZB = zb_and(n1649, n3892);
    let n3898: ZB = zb_and(n1656, n3894);
    let n3899: ZB = zb_and(n1657, n3894);
    let n3900: ZB = zb_and(n1007, n3895);
    let n3901: ZB = zb_or(n3898, n3899);
    let n3902: ZB = zb_or(n3896, n3897);
    let n3903: ZB = zb_or(n3900, n3901);
    let n3904: ZB = zb_or(n3902, n3903);
    let n3905: ZB = zb_and(n1642, n3891);
    let n3906: ZB = zb_and(n1007, n3891);
    let n3907: ZB = zb_or(n3905, n3906);
    let n3908: ZB = zb_or(n3904, n3907);
    let n3909: ZB = zb_and(n1685, n3908);
    let n3910: ZB = zb_and(n1684, n3908);
    let n3911: ZB = zb_or(n3909, n3910);
    let n3912: ZB = zb_and(n1692, n3911);
    let n3913: ZB = zb_and(n1693, n3911);
    let n3914: ZB = zb_or(n3912, n3913);
    let n3915: ZB = zb_and(n1597, n3914);
    let n3916: ZB = zb_and(n1596, n3914);
    let n3917: ZB = zb_and(n1701, n3915);
    let n3918: ZB = zb_and(n1702, n3915);
    let n3919: ZB = zb_or(n3917, n3918);
    let n3920: ZB = zb_or(n3916, n3919);
    let n3921: ZB = zb_and(n3508, n3920);
    let n3922: ZB = zb_and(n3509, n3920);
    let n3923: ZB = zb_or(n3921, n3922);
    let n3924: ZB = zb_or(n3886, n3923);
    let n3925: ZB = zb_and(n1747, n3924);
    let n3926: ZB = zb_and(n1748, n3924);
    let n3927: ZB = zb_or(n3925, n3926);
    let n3928: ZB = zb_and(n1747, n3927);
    let n3929: ZB = zb_and(n1747, n3558);
    let n3930: ZB = zb_not(n3928);
    let n3931: ZB = zb_or(n3928, n3929);
    let n3932: ZB = zsel_b(n3928, n1591, n3488);
    let n3934: ZN = zsel_n(n3928, r_c87, n3561);
    let n3935: ZN = zsel_n(n3928, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n3937: ZB = zb_and(n1656, n3891);
    let n3938: ZB = zb_and(n1657, n3891);
    let n3939: ZB = zb_or(n3937, n3938);
    let n3940: ZB = zb_or(n3904, n3939);
    let n3941: ZB = zb_and(n1792, n3940);
    let n3942: ZB = zb_and(n1791, n3940);
    let n3943: ZB = zb_or(n3941, n3942);
    let n3944: ZB = zb_and(n1692, n3943);
    let n3945: ZB = zb_and(n1693, n3943);
    let n3946: ZB = zb_or(n3944, n3945);
    let n3947: ZB = zb_and(n1803, n3946);
    let n3948: ZB = zb_and(n1802, n3946);
    let n3949: ZB = zb_or(n3947, n3948);
    let n3950: ZB = zb_and(n1803, n3949);
    let n3951: ZB = zb_and(n1802, n3949);
    let n3952: ZB = zb_or(n3950, n3951);
    let n3953: ZB = zb_and(n1802, n3952);
    let n3954: ZB = zb_and(n1803, n3952);
    let n3955: ZB = zb_or(n3953, n3954);
    let n3956: ZB = zb_and(n1802, n3955);
    let n3957: ZB = zb_and(n1803, n3955);
    let n3958: ZB = zb_or(n3956, n3957);
    let n3959: ZB = zb_and(n1597, n3958);
    let n3960: ZB = zb_and(n1596, n3958);
    let n3961: ZB = zb_and(n1819, n3959);
    let n3962: ZB = zb_and(n1820, n3959);
    let n3963: ZB = zb_or(n3961, n3962);
    let n3964: ZB = zb_or(n3960, n3963);
    let n3965: ZB = zb_and(n3508, n3964);
    let n3966: ZB = zb_and(n3509, n3964);
    let n3967: ZB = zb_or(n3965, n3966);
    let n3968: ZB = zb_or(n3886, n3967);
    let n3969: ZB = zb_and(n1747, n3968);
    let n3970: ZB = zb_and(n1748, n3968);
    let n3971: ZB = zb_or(n3969, n3970);
    let n3972: ZB = zb_and(n1747, n3971);
    let n3973: ZB = zb_and(n1747, n3597);
    let n3974: ZB = zb_not(n3972);
    let n3975: ZB = zb_or(n3972, n3973);
    let n3976: ZB = zsel_b(n3972, n1591, n3488);
    let n3978: ZN = zsel_n(n3972, r_c87, n3561);
    let n3979: ZN = zsel_n(n3972, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n3981: ZB = zb_and(n1648, n3891);
    let n3982: ZB = zb_and(n1649, n3891);
    let n3983: ZB = zb_or(n3981, n3982);
    let n3984: ZB = zb_or(n3904, n3983);
    let n3985: ZB = zb_and(n1859, n3984);
    let n3986: ZB = zb_and(n1858, n3984);
    let n3987: ZB = zb_or(n3985, n3986);
    let n3988: ZB = zb_and(n1692, n3987);
    let n3989: ZB = zb_and(n1693, n3987);
    let n3990: ZB = zb_or(n3988, n3989);
    let n3991: ZB = zb_and(n1870, n3990);
    let n3992: ZB = zb_and(n1869, n3990);
    let n3993: ZB = zb_or(n3991, n3992);
    let n3994: ZB = zb_and(n1870, n3993);
    let n3995: ZB = zb_and(n1869, n3993);
    let n3996: ZB = zb_or(n3994, n3995);
    let n3997: ZB = zb_and(n1869, n3996);
    let n3998: ZB = zb_and(n1870, n3996);
    let n3999: ZB = zb_or(n3997, n3998);
    let n4000: ZB = zb_and(n1869, n3999);
    let n4001: ZB = zb_and(n1870, n3999);
    let n4002: ZB = zb_or(n4000, n4001);
    let n4003: ZB = zb_and(n1597, n4002);
    let n4004: ZB = zb_and(n1596, n4002);
    let n4005: ZB = zb_and(n1886, n4003);
    let n4006: ZB = zb_and(n1887, n4003);
    let n4007: ZB = zb_or(n4005, n4006);
    let n4008: ZB = zb_or(n4004, n4007);
    let n4009: ZB = zb_and(n3508, n4008);
    let n4010: ZB = zb_and(n3509, n4008);
    let n4011: ZB = zb_or(n4009, n4010);
    let n4012: ZB = zb_or(n3886, n4011);
    let n4013: ZB = zb_and(n1747, n4012);
    let n4014: ZB = zb_and(n1748, n4012);
    let n4015: ZB = zb_or(n4013, n4014);
    let n4016: ZB = zb_and(n1747, n4015);
    let n4017: ZB = zb_and(n1747, n3634);
    let n4018: ZB = zb_not(n4016);
    let n4019: ZB = zb_or(n4016, n4017);
    let n4020: ZB = zsel_b(n4016, n1591, n3488);
    let n4022: ZN = zsel_n(n4016, r_c87, n3561);
    let n4023: ZN = zsel_n(n4016, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4025: ZB = zb_and(n295, n3920);
    let n4026: ZB = zb_and(r_c312, n3920);
    let n4027: ZB = zb_and(n1713, n4025);
    let n4028: ZB = zb_and(n1714, n4025);
    let n4029: ZB = zb_and(n1717, n4028);
    let n4030: ZB = zb_and(n1716, n4028);
    let n4031: ZB = zb_or(n4029, n4030);
    let n4032: ZB = zb_and(n1717, n4031);
    let n4033: ZB = zb_and(n1716, n4031);
    let n4034: ZB = zb_or(n4032, n4033);
    let n4035: ZB = zb_and(n1716, n4034);
    let n4036: ZB = zb_and(n1717, n4034);
    let n4037: ZB = zb_and(n1720, n4036);
    let n4038: ZB = zb_and(n1719, n4036);
    let n4039: ZB = zb_or(n4037, n4038);
    let n4040: ZB = zb_and(n1720, n4039);
    let n4041: ZB = zb_and(n1719, n4039);
    let n4042: ZB = zb_or(n4040, n4041);
    let n4043: ZB = zb_and(n1719, n4042);
    let n4044: ZB = zb_and(n1720, n4042);
    let n4045: ZB = zb_or(n4043, n4044);
    let n4046: ZB = zb_or(n4035, n4045);
    let n4047: ZB = zb_and(n1724, n4046);
    let n4048: ZB = zb_and(n1723, n4046);
    let n4049: ZB = zb_or(n4047, n4048);
    let n4050: ZB = zb_or(n4027, n4049);
    let n4051: ZB = zb_or(n4026, n4050);
    let n4052: ZB = zb_and(n3508, n4051);
    let n4053: ZB = zb_and(n3509, n4051);
    let n4054: ZB = zb_or(n4052, n4053);
    let n4055: ZB = zb_or(n3886, n4054);
    let n4056: ZB = zb_and(n1747, n4055);
    let n4057: ZB = zb_and(n1748, n4055);
    let n4058: ZB = zb_or(n4056, n4057);
    let n4059: ZB = zb_and(n1747, n4058);
    let n4060: ZB = zb_and(n1747, n3670);
    let n4061: ZB = zb_not(n4059);
    let n4062: ZB = zb_or(n4059, n4060);
    let n4063: ZB = zsel_b(n4059, n1591, n3488);
    let n4065: ZN = zsel_n(n4059, r_c87, n3561);
    let n4066: ZN = zsel_n(n4059, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4068: ZB = zb_and(n295, n3964);
    let n4069: ZB = zb_and(r_c312, n3964);
    let n4070: ZB = zb_and(n1713, n4068);
    let n4071: ZB = zb_and(n1714, n4068);
    let n4072: ZB = zb_and(n1717, n4071);
    let n4073: ZB = zb_and(n1716, n4071);
    let n4074: ZB = zb_or(n4072, n4073);
    let n4075: ZB = zb_and(n1717, n4074);
    let n4076: ZB = zb_and(n1716, n4074);
    let n4077: ZB = zb_or(n4075, n4076);
    let n4078: ZB = zb_and(n1716, n4077);
    let n4079: ZB = zb_and(n1717, n4077);
    let n4080: ZB = zb_and(n1720, n4079);
    let n4081: ZB = zb_and(n1719, n4079);
    let n4082: ZB = zb_or(n4080, n4081);
    let n4083: ZB = zb_and(n1720, n4082);
    let n4084: ZB = zb_and(n1719, n4082);
    let n4085: ZB = zb_or(n4083, n4084);
    let n4086: ZB = zb_and(n1719, n4085);
    let n4087: ZB = zb_and(n1720, n4085);
    let n4088: ZB = zb_or(n4086, n4087);
    let n4089: ZB = zb_or(n4078, n4088);
    let n4090: ZB = zb_and(n1724, n4089);
    let n4091: ZB = zb_and(n1723, n4089);
    let n4092: ZB = zb_or(n4090, n4091);
    let n4093: ZB = zb_or(n4070, n4092);
    let n4094: ZB = zb_or(n4069, n4093);
    let n4095: ZB = zb_and(n3508, n4094);
    let n4096: ZB = zb_and(n3509, n4094);
    let n4097: ZB = zb_or(n4095, n4096);
    let n4098: ZB = zb_or(n3886, n4097);
    let n4099: ZB = zb_and(n1747, n4098);
    let n4100: ZB = zb_and(n1748, n4098);
    let n4101: ZB = zb_or(n4099, n4100);
    let n4102: ZB = zb_and(n1747, n4101);
    let n4103: ZB = zb_and(n1747, n3706);
    let n4104: ZB = zb_not(n4102);
    let n4105: ZB = zb_or(n4102, n4103);
    let n4106: ZB = zsel_b(n4102, n1591, n3488);
    let n4108: ZN = zsel_n(n4102, r_c87, n3561);
    let n4109: ZN = zsel_n(n4102, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4111: ZB = zb_and(n295, n4008);
    let n4112: ZB = zb_and(r_c312, n4008);
    let n4113: ZB = zb_and(n1713, n4111);
    let n4114: ZB = zb_and(n1714, n4111);
    let n4115: ZB = zb_and(n1717, n4114);
    let n4116: ZB = zb_and(n1716, n4114);
    let n4117: ZB = zb_or(n4115, n4116);
    let n4118: ZB = zb_and(n1717, n4117);
    let n4119: ZB = zb_and(n1716, n4117);
    let n4120: ZB = zb_or(n4118, n4119);
    let n4121: ZB = zb_and(n1716, n4120);
    let n4122: ZB = zb_and(n1717, n4120);
    let n4123: ZB = zb_and(n1720, n4122);
    let n4124: ZB = zb_and(n1719, n4122);
    let n4125: ZB = zb_or(n4123, n4124);
    let n4126: ZB = zb_and(n1720, n4125);
    let n4127: ZB = zb_and(n1719, n4125);
    let n4128: ZB = zb_or(n4126, n4127);
    let n4129: ZB = zb_and(n1719, n4128);
    let n4130: ZB = zb_and(n1720, n4128);
    let n4131: ZB = zb_or(n4129, n4130);
    let n4132: ZB = zb_or(n4121, n4131);
    let n4133: ZB = zb_and(n1724, n4132);
    let n4134: ZB = zb_and(n1723, n4132);
    let n4135: ZB = zb_or(n4133, n4134);
    let n4136: ZB = zb_or(n4113, n4135);
    let n4137: ZB = zb_or(n4112, n4136);
    let n4138: ZB = zb_and(n3508, n4137);
    let n4139: ZB = zb_and(n3509, n4137);
    let n4140: ZB = zb_or(n4138, n4139);
    let n4141: ZB = zb_or(n3886, n4140);
    let n4142: ZB = zb_and(n1747, n4141);
    let n4143: ZB = zb_and(n1748, n4141);
    let n4144: ZB = zb_or(n4142, n4143);
    let n4145: ZB = zb_and(n1747, n4144);
    let n4146: ZB = zb_and(n1747, n3742);
    let n4147: ZB = zb_not(n4145);
    let n4148: ZB = zb_or(n4145, n4146);
    let n4149: ZB = zsel_b(n4145, n1591, n3488);
    let n4151: ZN = zsel_n(n4145, r_c87, n3561);
    let n4152: ZN = zsel_n(n4145, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4154: ZB = zb_and(n3745, n3923);
    let n4155: ZB = zb_and(n3746, n3923);
    let n4156: ZB = zb_and(n1689, n4154);
    let n4157: ZB = zb_and(n1732, n4154);
    let n4158: ZB = zb_or(n4156, n4157);
    let n4159: ZB = zb_and(n1734, n4158);
    let n4160: ZB = zb_and(n1735, n4158);
    let n4161: ZB = zb_and(n1736, n4160);
    let n4162: ZB = zb_and(n1737, n4160);
    let n4163: ZB = zb_or(n4161, n4162);
    let n4164: ZB = zb_or(n4159, n4163);
    let n4165: ZB = zb_and(n1741, n4164);
    let n4166: ZB = zb_and(n1740, n4164);
    let n4167: ZB = zb_or(n4165, n4166);
    let n4168: ZB = zb_or(n4155, n4167);
    let n4169: ZB = zb_or(n3886, n4168);
    let n4170: ZB = zb_and(n1747, n4169);
    let n4171: ZB = zb_and(n1748, n4169);
    let n4172: ZB = zb_or(n4170, n4171);
    let n4173: ZB = zb_and(n1747, n4172);
    let n4174: ZB = zb_and(n1747, n3769);
    let n4175: ZB = zb_not(n4173);
    let n4176: ZB = zb_or(n4173, n4174);
    let n4177: ZB = zsel_b(n4173, n1591, n3488);
    let n4178: ZB = zb_and(n3771, n4176);
    let n4179: ZB = zb_and(n3772, n4176);
    let n4180: ZB = zb_or(n4178, n4179);
    let n4181: ZN = zsel_n(n4173, r_c87, n3561);
    let n4182: ZN = zsel_n(n4173, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4184: ZB = zb_and(n3745, n3967);
    let n4185: ZB = zb_and(n3746, n3967);
    let n4186: ZB = zb_or(n4184, n4185);
    let n4187: ZB = zb_or(n3886, n4186);
    let n4188: ZB = zb_and(n1747, n4187);
    let n4189: ZB = zb_and(n1748, n4187);
    let n4190: ZB = zb_or(n4188, n4189);
    let n4191: ZB = zb_and(n1747, n4190);
    let n4192: ZB = zb_and(n1747, n3782);
    let n4193: ZB = zb_not(n4191);
    let n4194: ZB = zb_or(n4191, n4192);
    let n4195: ZB = zsel_b(n4191, n1591, n3488);
    let n4196: ZB = zb_and(n3771, n4194);
    let n4197: ZB = zb_and(n3772, n4194);
    let n4198: ZB = zb_or(n4196, n4197);
    let n4199: ZN = zsel_n(n4191, r_c87, n3561);
    let n4200: ZN = zsel_n(n4191, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4202: ZB = zb_and(n3745, n4011);
    let n4203: ZB = zb_and(n3746, n4011);
    let n4204: ZB = zb_or(n4202, n4203);
    let n4205: ZB = zb_or(n3886, n4204);
    let n4206: ZB = zb_and(n1747, n4205);
    let n4207: ZB = zb_and(n1748, n4205);
    let n4208: ZB = zb_or(n4206, n4207);
    let n4209: ZB = zb_and(n1747, n4208);
    let n4210: ZB = zb_and(n1747, n3793);
    let n4211: ZB = zb_not(n4209);
    let n4212: ZB = zb_or(n4209, n4210);
    let n4213: ZB = zsel_b(n4209, n1591, n3488);
    let n4214: ZB = zb_and(n3771, n4212);
    let n4215: ZB = zb_and(n3772, n4212);
    let n4216: ZB = zb_or(n4214, n4215);
    let n4217: ZN = zsel_n(n4209, r_c87, n3561);
    let n4218: ZN = zsel_n(n4209, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4220: ZB = zb_or(n4154, n4155);
    let n4221: ZB = zb_or(n3886, n4220);
    let n4222: ZB = zb_and(n1747, n4221);
    let n4223: ZB = zb_and(n1748, n4221);
    let n4224: ZB = zb_or(n4222, n4223);
    let n4225: ZB = zb_and(n1747, n4224);
    let n4226: ZB = zb_and(n1747, n3802);
    let n4227: ZB = zb_not(n4225);
    let n4228: ZB = zb_or(n4225, n4226);
    let n4229: ZB = zsel_b(n4225, n1591, n3488);
    let n4230: ZB = zb_and(n3771, n4228);
    let n4231: ZB = zb_and(n3772, n4228);
    let n4232: ZB = zb_or(n4230, n4231);
    let n4233: ZN = zsel_n(n4225, r_c87, n3561);
    let n4234: ZN = zsel_n(n4225, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4236: ZB = zb_and(n3745, n4054);
    let n4237: ZB = zb_and(n3746, n4054);
    let n4238: ZB = zb_and(n1689, n4236);
    let n4239: ZB = zb_and(n1732, n4236);
    let n4240: ZB = zb_or(n4238, n4239);
    let n4241: ZB = zb_and(n1734, n4240);
    let n4242: ZB = zb_and(n1735, n4240);
    let n4243: ZB = zb_and(n1736, n4242);
    let n4244: ZB = zb_and(n1737, n4242);
    let n4245: ZB = zb_or(n4243, n4244);
    let n4246: ZB = zb_or(n4241, n4245);
    let n4247: ZB = zb_and(n1741, n4246);
    let n4248: ZB = zb_and(n1740, n4246);
    let n4249: ZB = zb_or(n4247, n4248);
    let n4250: ZB = zb_or(n4237, n4249);
    let n4251: ZB = zb_or(n3886, n4250);
    let n4252: ZB = zb_and(n1747, n4251);
    let n4253: ZB = zb_and(n1748, n4251);
    let n4254: ZB = zb_or(n4252, n4253);
    let n4255: ZB = zb_and(n1747, n4254);
    let n4256: ZB = zb_and(n1747, n3825);
    let n4257: ZB = zb_not(n4255);
    let n4258: ZB = zb_or(n4255, n4256);
    let n4259: ZB = zsel_b(n4255, n1591, n3488);
    let n4260: ZB = zb_and(n3771, n4258);
    let n4261: ZB = zb_and(n3772, n4258);
    let n4262: ZB = zb_or(n4260, n4261);
    let n4263: ZN = zsel_n(n4255, r_c87, n3561);
    let n4264: ZN = zsel_n(n4255, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4266: ZB = zb_and(n3745, n4097);
    let n4267: ZB = zb_and(n3746, n4097);
    let n4268: ZB = zb_or(n4266, n4267);
    let n4269: ZB = zb_or(n3886, n4268);
    let n4270: ZB = zb_and(n1747, n4269);
    let n4271: ZB = zb_and(n1748, n4269);
    let n4272: ZB = zb_or(n4270, n4271);
    let n4273: ZB = zb_and(n1747, n4272);
    let n4274: ZB = zb_and(n1747, n3836);
    let n4275: ZB = zb_not(n4273);
    let n4276: ZB = zb_or(n4273, n4274);
    let n4277: ZB = zsel_b(n4273, n1591, n3488);
    let n4278: ZB = zb_and(n3771, n4276);
    let n4279: ZB = zb_and(n3772, n4276);
    let n4280: ZB = zb_or(n4278, n4279);
    let n4281: ZN = zsel_n(n4273, r_c87, n3561);
    let n4282: ZN = zsel_n(n4273, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4284: ZB = zb_and(n3745, n4140);
    let n4285: ZB = zb_and(n3746, n4140);
    let n4286: ZB = zb_or(n4284, n4285);
    let n4287: ZB = zb_or(n3886, n4286);
    let n4288: ZB = zb_and(n1747, n4287);
    let n4289: ZB = zb_and(n1748, n4287);
    let n4290: ZB = zb_or(n4288, n4289);
    let n4291: ZB = zb_and(n1747, n4290);
    let n4292: ZB = zb_and(n1747, n3847);
    let n4293: ZB = zb_not(n4291);
    let n4294: ZB = zb_or(n4291, n4292);
    let n4295: ZB = zsel_b(n4291, n1591, n3488);
    let n4296: ZB = zb_and(n3771, n4294);
    let n4297: ZB = zb_and(n3772, n4294);
    let n4298: ZB = zb_or(n4296, n4297);
    let n4299: ZN = zsel_n(n4291, r_c87, n3561);
    let n4300: ZN = zsel_n(n4291, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4302: ZB = zb_or(n4236, n4237);
    let n4303: ZB = zb_or(n3886, n4302);
    let n4304: ZB = zb_and(n1747, n4303);
    let n4305: ZB = zb_and(n1748, n4303);
    let n4306: ZB = zb_or(n4304, n4305);
    let n4307: ZB = zb_and(n1747, n4306);
    let n4308: ZB = zb_and(n1747, n3856);
    let n4309: ZB = zb_not(n4307);
    let n4310: ZB = zb_or(n4307, n4308);
    let n4311: ZB = zsel_b(n4307, n1591, n3488);
    let n4312: ZB = zb_and(n3771, n4310);
    let n4313: ZB = zb_and(n3772, n4310);
    let n4314: ZB = zb_or(n4312, n4313);
    let n4315: ZN = zsel_n(n4307, r_c87, n3561);
    let n4316: ZN = zsel_n(n4307, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4318: ZB = zb_and(n1747, n1751);
    let n4319: ZB = zb_and(n1747, n2471);
    let n4320: ZB = zb_not(n4318);
    let n4321: ZB = zb_or(n4318, n4319);
    let n4322: ZB = zsel_b(n4318, n1591, n2413);
    let n4324: ZN = zsel_n(n4318, r_c87, n2475);
    let n4325: ZN = zsel_n(n4318, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4327: ZB = zb_and(n1747, n1839);
    let n4328: ZB = zb_and(n1747, n2508);
    let n4329: ZB = zb_not(n4327);
    let n4330: ZB = zb_or(n4327, n4328);
    let n4331: ZB = zsel_b(n4327, n1591, n2413);
    let n4333: ZN = zsel_n(n4327, r_c87, n2475);
    let n4334: ZN = zsel_n(n4327, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4336: ZB = zb_and(n1747, n1906);
    let n4337: ZB = zb_and(n1747, n2542);
    let n4338: ZB = zb_not(n4336);
    let n4339: ZB = zb_or(n4336, n4337);
    let n4340: ZB = zsel_b(n4336, n1591, n2413);
    let n4342: ZN = zsel_n(n4336, r_c87, n2475);
    let n4343: ZN = zsel_n(n4336, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4345: ZB = zb_and(n1747, n1953);
    let n4346: ZB = zb_and(n1747, n2575);
    let n4347: ZB = zb_not(n4345);
    let n4348: ZB = zb_or(n4345, n4346);
    let n4349: ZB = zsel_b(n4345, n1591, n2413);
    let n4351: ZN = zsel_n(n4345, r_c87, n2475);
    let n4352: ZN = zsel_n(n4345, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4354: ZB = zb_and(n1747, n1998);
    let n4355: ZB = zb_and(n1747, n2608);
    let n4356: ZB = zb_not(n4354);
    let n4357: ZB = zb_or(n4354, n4355);
    let n4358: ZB = zsel_b(n4354, n1591, n2413);
    let n4360: ZN = zsel_n(n4354, r_c87, n2475);
    let n4361: ZN = zsel_n(n4354, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4363: ZB = zb_and(n1747, n2043);
    let n4364: ZB = zb_and(n1747, n2641);
    let n4365: ZB = zb_not(n4363);
    let n4366: ZB = zb_or(n4363, n4364);
    let n4367: ZB = zsel_b(n4363, n1591, n2413);
    let n4369: ZN = zsel_n(n4363, r_c87, n2475);
    let n4370: ZN = zsel_n(n4363, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4372: ZB = zb_and(n1747, n2096);
    let n4373: ZB = zb_and(n1747, n2662);
    let n4374: ZB = zb_not(n4372);
    let n4375: ZB = zb_or(n4372, n4373);
    let n4376: ZB = zsel_b(n4372, n1591, n2413);
    let n4377: ZB = zb_and(n2068, n4375);
    let n4378: ZB = zb_and(n2069, n4375);
    let n4379: ZB = zb_or(n4377, n4378);
    let n4380: ZN = zsel_n(n4372, r_c87, n2475);
    let n4381: ZN = zsel_n(n4372, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4383: ZB = zb_and(n1747, n2124);
    let n4384: ZB = zb_and(n1747, n2673);
    let n4385: ZB = zb_not(n4383);
    let n4386: ZB = zb_or(n4383, n4384);
    let n4387: ZB = zsel_b(n4383, n1591, n2413);
    let n4388: ZB = zb_and(n2068, n4386);
    let n4389: ZB = zb_and(n2069, n4386);
    let n4390: ZB = zb_or(n4388, n4389);
    let n4391: ZN = zsel_n(n4383, r_c87, n2475);
    let n4392: ZN = zsel_n(n4383, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4394: ZB = zb_and(n1747, n2149);
    let n4395: ZB = zb_and(n1747, n2684);
    let n4396: ZB = zb_not(n4394);
    let n4397: ZB = zb_or(n4394, n4395);
    let n4398: ZB = zsel_b(n4394, n1591, n2413);
    let n4399: ZB = zb_and(n2068, n4397);
    let n4400: ZB = zb_and(n2069, n4397);
    let n4401: ZB = zb_or(n4399, n4400);
    let n4402: ZN = zsel_n(n4394, r_c87, n2475);
    let n4403: ZN = zsel_n(n4394, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4405: ZB = zb_and(n1747, n2179);
    let n4406: ZB = zb_and(n1747, n2693);
    let n4407: ZB = zb_not(n4405);
    let n4408: ZB = zb_or(n4405, n4406);
    let n4409: ZB = zsel_b(n4405, n1591, n2413);
    let n4410: ZB = zb_and(n2068, n4408);
    let n4411: ZB = zb_and(n2069, n4408);
    let n4412: ZB = zb_or(n4410, n4411);
    let n4413: ZN = zsel_n(n4405, r_c87, n2475);
    let n4414: ZN = zsel_n(n4405, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4416: ZB = zb_and(n1747, n2234);
    let n4417: ZB = zb_and(n1747, n2716);
    let n4418: ZB = zb_not(n4416);
    let n4419: ZB = zb_or(n4416, n4417);
    let n4420: ZB = zsel_b(n4416, n1591, n2413);
    let n4421: ZB = zb_and(n2068, n4419);
    let n4422: ZB = zb_and(n2069, n4419);
    let n4423: ZB = zb_or(n4421, n4422);
    let n4424: ZN = zsel_n(n4416, r_c87, n2475);
    let n4425: ZN = zsel_n(n4416, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4427: ZB = zb_and(n1747, n2257);
    let n4428: ZB = zb_and(n1747, n2727);
    let n4429: ZB = zb_not(n4427);
    let n4430: ZB = zb_or(n4427, n4428);
    let n4431: ZB = zsel_b(n4427, n1591, n2413);
    let n4432: ZB = zb_and(n2068, n4430);
    let n4433: ZB = zb_and(n2069, n4430);
    let n4434: ZB = zb_or(n4432, n4433);
    let n4435: ZN = zsel_n(n4427, r_c87, n2475);
    let n4436: ZN = zsel_n(n4427, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4438: ZB = zb_and(n1747, n2280);
    let n4439: ZB = zb_and(n1747, n2738);
    let n4440: ZB = zb_not(n4438);
    let n4441: ZB = zb_or(n4438, n4439);
    let n4442: ZB = zsel_b(n4438, n1591, n2413);
    let n4443: ZB = zb_and(n2068, n4441);
    let n4444: ZB = zb_and(n2069, n4441);
    let n4445: ZB = zb_or(n4443, n4444);
    let n4446: ZN = zsel_n(n4438, r_c87, n2475);
    let n4447: ZN = zsel_n(n4438, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4449: ZB = zb_and(n1747, n2301);
    let n4450: ZB = zb_and(n1747, n2747);
    let n4451: ZB = zb_not(n4449);
    let n4452: ZB = zb_or(n4449, n4450);
    let n4453: ZB = zsel_b(n4449, n1591, n2413);
    let n4454: ZB = zb_and(n2068, n4452);
    let n4455: ZB = zb_and(n2069, n4452);
    let n4456: ZB = zb_or(n4454, n4455);
    let n4457: ZN = zsel_n(n4449, r_c87, n2475);
    let n4458: ZN = zsel_n(n4449, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n4460: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n4461: ZN = zsel_n(n305, r_c246, n2754);
    let n4462: ZN = zsel_n(n305, r_c254, n2759);
    let n4463: ZB = zb_and(r_c311, n305);
    let n4464: ZB = zb_and(r_c312, n305);
    let n4465: ZN = zn_sub(n3498, zn_splat(P8::from_raw(65536i32)));
    let n4466: ZB = zb_and(n1748, n3927);
    let n4467: ZN = zsel_n(n305, n4460, r_c20);
    let n4468: ZN = zsel_n(n305, r_c258, n345);
    let n4469: ZN = zsel_n(n305, r_c271, n346);
    let n4470: ZN = zsel_n(n305, r_c278, n397);
    let n4471: ZN = zsel_n(n305, r_c291, n398);
    let n4472: ZN = zsel_n(n305, r_c299, n300);
    let n4473: ZN = zsel_n(n305, r_c301, n304);
    let n4474: ZN = zsel_n(n305, r_c302, n3498);
    let n4475: ZN = zsel_n(n305, r_c304, n1609);
    let n4476: ZN = zsel_n(n305, r_c318, n919);
    let n4477: ZN = zsel_n(n305, r_c319, n920);
    let n4478: ZB = zsel_b(n305, r_c400, n1743);
    let n4479: ZN = zsel_n(n305, r_c406, n921);
    let n4480: ZN = zsel_n(n305, r_c407, n922);
    let n4481: ZN = zsel_n(n305, r_c408, n1744);
    let n4482: ZN = zsel_n(n305, r_c409, n1745);
    let n4483: ZB = zb_or(n305, n4466);
    let n4484: ZB = zb_or(n305, n1591);
    let n4485: ZB = zn_gt(n4467, zn_splat(P8::from_raw(0i32)));
    let n4486: ZB = zn_le(n4467, zn_splat(P8::from_raw(0i32)));
    let n4487: ZB = zb_and(n4483, n4485);
    let n4488: ZB = zb_and(n4483, n4486);
    let n4489: ZB = zn_lt(n4476, zn_splat(P8::from_raw(-65536i32)));
    let n4490: ZB = zn_ge(n4476, zn_splat(P8::from_raw(-65536i32)));
    let n4491: ZB = zb_and(n4488, n4490);
    let n4492: ZB = zb_and(n4488, n4489);
    let n4493: ZB = zn_gt(n4476, zn_splat(P8::from_raw(7929856i32)));
    let n4494: ZB = zb_or(n4491, n4492);
    let n4495: ZB = zb_or(n4489, n4493);
    let n4496: ZB = zb_not(n4495);
    let n4497: ZB = zb_and(n4494, n4495);
    let n4498: ZB = zb_and(n4494, n4496);
    let n4499: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n4476);
    let n4500: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4499);
    let n4501: ZN = zsel_n(n4495, n4500, n4476);
    let n4502: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4481);
    let n4503: ZB = zb_or(n4497, n4498);
    let n4504: ZN = zsel_n(n4485, n4476, n4501);
    let n4505: ZN = zsel_n(n4485, n4481, n4502);
    let n4506: ZB = zb_or(n4487, n4503);
    let n4508: ZB = zb_and(n1748, n3971);
    let n4509: ZB = zsel_b(n305, r_c400, n1833);
    let n4510: ZN = zsel_n(n305, r_c408, n1834);
    let n4511: ZN = zsel_n(n305, r_c409, n1835);
    let n4512: ZB = zb_or(n305, n4508);
    let n4513: ZB = zb_and(n4485, n4512);
    let n4514: ZB = zb_and(n4486, n4512);
    let n4515: ZB = zb_and(n4490, n4514);
    let n4516: ZB = zb_and(n4489, n4514);
    let n4517: ZB = zb_or(n4515, n4516);
    let n4518: ZB = zb_and(n4495, n4517);
    let n4519: ZB = zb_and(n4496, n4517);
    let n4520: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4510);
    let n4521: ZB = zb_or(n4518, n4519);
    let n4522: ZN = zsel_n(n4485, n4510, n4520);
    let n4523: ZB = zb_or(n4513, n4521);
    let n4524: ZB = zb_and(n1748, n4015);
    let n4525: ZB = zsel_b(n305, r_c400, n1900);
    let n4526: ZN = zsel_n(n305, r_c408, n1901);
    let n4527: ZN = zsel_n(n305, r_c409, n1902);
    let n4528: ZB = zb_or(n305, n4524);
    let n4529: ZB = zb_and(n4485, n4528);
    let n4530: ZB = zb_and(n4486, n4528);
    let n4531: ZB = zb_and(n4490, n4530);
    let n4532: ZB = zb_and(n4489, n4530);
    let n4533: ZB = zb_or(n4531, n4532);
    let n4534: ZB = zb_and(n4495, n4533);
    let n4535: ZB = zb_and(n4496, n4533);
    let n4536: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4526);
    let n4537: ZB = zb_or(n4534, n4535);
    let n4538: ZN = zsel_n(n4485, n4526, n4536);
    let n4539: ZB = zb_or(n4529, n4537);
    let n4540: ZB = zb_or(r_c312, n172);
    let n4541: ZB = zb_and(n1748, n4058);
    let n4542: ZN = zsel_n(n305, r_c304, n1947);
    let n4543: ZN = zsel_n(n305, r_c408, n1948);
    let n4544: ZN = zsel_n(n305, r_c409, n1949);
    let n4545: ZB = zb_or(n305, n4541);
    let n4546: ZB = zb_and(n4485, n4545);
    let n4547: ZB = zb_and(n4486, n4545);
    let n4548: ZB = zb_and(n4490, n4547);
    let n4549: ZB = zb_and(n4489, n4547);
    let n4550: ZB = zb_or(n4548, n4549);
    let n4551: ZB = zb_and(n4495, n4550);
    let n4552: ZB = zb_and(n4496, n4550);
    let n4553: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4543);
    let n4554: ZB = zb_or(n4551, n4552);
    let n4555: ZN = zsel_n(n4485, n4543, n4553);
    let n4556: ZB = zb_or(n4546, n4554);
    let n4557: ZB = zb_and(n1748, n4101);
    let n4558: ZN = zsel_n(n305, r_c408, n1993);
    let n4559: ZN = zsel_n(n305, r_c409, n1994);
    let n4560: ZB = zb_or(n305, n4557);
    let n4561: ZB = zb_and(n4485, n4560);
    let n4562: ZB = zb_and(n4486, n4560);
    let n4563: ZB = zb_and(n4490, n4562);
    let n4564: ZB = zb_and(n4489, n4562);
    let n4565: ZB = zb_or(n4563, n4564);
    let n4566: ZB = zb_and(n4495, n4565);
    let n4567: ZB = zb_and(n4496, n4565);
    let n4568: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4558);
    let n4569: ZB = zb_or(n4566, n4567);
    let n4570: ZN = zsel_n(n4485, n4558, n4568);
    let n4571: ZB = zb_or(n4561, n4569);
    let n4572: ZB = zb_and(n1748, n4144);
    let n4573: ZN = zsel_n(n305, r_c408, n2038);
    let n4574: ZN = zsel_n(n305, r_c409, n2039);
    let n4575: ZB = zb_or(n305, n4572);
    let n4576: ZB = zb_and(n4485, n4575);
    let n4577: ZB = zb_and(n4486, n4575);
    let n4578: ZB = zb_and(n4490, n4577);
    let n4579: ZB = zb_and(n4489, n4577);
    let n4580: ZB = zb_or(n4578, n4579);
    let n4581: ZB = zb_and(n4495, n4580);
    let n4582: ZB = zb_and(n4496, n4580);
    let n4583: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4573);
    let n4584: ZB = zb_or(n4581, n4582);
    let n4585: ZN = zsel_n(n4485, n4573, n4583);
    let n4586: ZB = zb_or(n4576, n4584);
    let n4587: ZB = zb_or(r_c311, n172);
    let n4588: ZN = zsel_n(n3745, zn_splat(P8::from_raw(655360i32)), n300);
    let n4589: ZN = zsel_n(n3745, zn_splat(P8::from_raw(262144i32)), r_c301);
    let n4590: ZN = zsel_n(n3745, n4465, n3498);
    let n4591: ZN = zsel_n(n3745, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n4592: ZN = zsel_n(n3745, n1742, r_c397);
    let n4593: ZN = zsel_n(n3745, n1739, r_c398);
    let n4594: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), r_c399);
    let n4595: ZN = zsel_n(n3745, n1733, n1682);
    let n4596: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1711);
    let n4597: ZN = zsel_n(n301, n300, n4588);
    let n4598: ZN = zsel_n(n301, n303, n4589);
    let n4599: ZN = zsel_n(n301, n3498, n4590);
    let n4600: ZN = zsel_n(n301, r_c396, n4591);
    let n4601: ZN = zsel_n(n301, r_c397, n4592);
    let n4602: ZN = zsel_n(n301, r_c398, n4593);
    let n4603: ZN = zsel_n(n301, r_c399, n4594);
    let n4604: ZN = zsel_n(n301, n1621, n4595);
    let n4605: ZN = zsel_n(n301, n1631, n4596);
    let n4606: ZB = zb_and(n1748, n4172);
    let n4607: ZN = zsel_n(n305, n4460, n3749);
    let n4608: ZB = zsel_b(n305, r_c41, n3750);
    let n4609: ZN = zsel_n(n305, r_c299, n4597);
    let n4610: ZN = zsel_n(n305, r_c301, n4598);
    let n4611: ZN = zsel_n(n305, r_c302, n4599);
    let n4612: ZN = zsel_n(n305, r_c396, n4600);
    let n4613: ZN = zsel_n(n305, r_c397, n4601);
    let n4614: ZN = zsel_n(n305, r_c398, n4602);
    let n4615: ZN = zsel_n(n305, r_c399, n4603);
    let n4616: ZN = zsel_n(n305, r_c408, n4604);
    let n4617: ZN = zsel_n(n305, r_c409, n4605);
    let n4618: ZB = zb_or(n305, n4606);
    let n4619: ZB = zn_gt(n4607, zn_splat(P8::from_raw(0i32)));
    let n4620: ZB = zn_le(n4607, zn_splat(P8::from_raw(0i32)));
    let n4621: ZB = zb_and(n4618, n4619);
    let n4622: ZB = zb_and(n4618, n4620);
    let n4623: ZB = zb_and(n4490, n4622);
    let n4624: ZB = zb_and(n4489, n4622);
    let n4625: ZB = zb_or(n4623, n4624);
    let n4626: ZB = zb_and(n4495, n4625);
    let n4627: ZB = zb_and(n4496, n4625);
    let n4628: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4616);
    let n4629: ZB = zb_or(n4626, n4627);
    let n4630: ZN = zsel_n(n4619, n4476, n4501);
    let n4631: ZN = zsel_n(n4619, n4616, n4628);
    let n4632: ZB = zb_or(n4621, n4629);
    let n4633: ZN = zsel_n(n3745, zn_splat(P8::from_raw(69510i32)), r_c397);
    let n4634: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-131072i32)), r_c398);
    let n4635: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-327680i32)), n1789);
    let n4636: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1827);
    let n4637: ZN = zsel_n(n301, r_c397, n4633);
    let n4638: ZN = zsel_n(n301, r_c398, n4634);
    let n4639: ZN = zsel_n(n301, n1621, n4635);
    let n4640: ZN = zsel_n(n301, n1631, n4636);
    let n4641: ZB = zb_and(n1748, n4190);
    let n4642: ZN = zsel_n(n305, r_c397, n4637);
    let n4643: ZN = zsel_n(n305, r_c398, n4638);
    let n4644: ZN = zsel_n(n305, r_c408, n4639);
    let n4645: ZN = zsel_n(n305, r_c409, n4640);
    let n4646: ZB = zb_or(n305, n4641);
    let n4647: ZB = zb_and(n4619, n4646);
    let n4648: ZB = zb_and(n4620, n4646);
    let n4649: ZB = zb_and(n4490, n4648);
    let n4650: ZB = zb_and(n4489, n4648);
    let n4651: ZB = zb_or(n4649, n4650);
    let n4652: ZB = zb_and(n4495, n4651);
    let n4653: ZB = zb_and(n4496, n4651);
    let n4654: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4644);
    let n4655: ZB = zb_or(n4652, n4653);
    let n4656: ZN = zsel_n(n4619, n4644, n4654);
    let n4657: ZB = zb_or(n4647, n4655);
    let n4658: ZN = zsel_n(n3745, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n4659: ZN = zsel_n(n3745, zn_splat(P8::from_raw(327680i32)), n1856);
    let n4660: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1894);
    let n4661: ZN = zsel_n(n301, r_c398, n4658);
    let n4662: ZN = zsel_n(n301, n1621, n4659);
    let n4663: ZN = zsel_n(n301, n1631, n4660);
    let n4664: ZB = zb_and(n1748, n4208);
    let n4665: ZN = zsel_n(n305, r_c398, n4661);
    let n4666: ZN = zsel_n(n305, r_c408, n4662);
    let n4667: ZN = zsel_n(n305, r_c409, n4663);
    let n4668: ZB = zb_or(n305, n4664);
    let n4669: ZB = zb_and(n4619, n4668);
    let n4670: ZB = zb_and(n4620, n4668);
    let n4671: ZB = zb_and(n4490, n4670);
    let n4672: ZB = zb_and(n4489, n4670);
    let n4673: ZB = zb_or(n4671, n4672);
    let n4674: ZB = zb_and(n4495, n4673);
    let n4675: ZB = zb_and(n4496, n4673);
    let n4676: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4666);
    let n4677: ZB = zb_or(n4674, n4675);
    let n4678: ZN = zsel_n(n4619, n4666, n4676);
    let n4679: ZB = zb_or(n4669, n4677);
    let n4680: ZN = zsel_n(n3745, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n4681: ZN = zsel_n(n3745, zn_splat(P8::from_raw(98304i32)), r_c397);
    let n4682: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), r_c398);
    let n4683: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-98304i32)), r_c399);
    let n4684: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1682);
    let n4685: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-327680i32)), n1711);
    let n4686: ZN = zsel_n(n301, r_c396, n4680);
    let n4687: ZN = zsel_n(n301, r_c397, n4681);
    let n4688: ZN = zsel_n(n301, r_c398, n4682);
    let n4689: ZN = zsel_n(n301, r_c399, n4683);
    let n4690: ZN = zsel_n(n301, n1621, n4684);
    let n4691: ZN = zsel_n(n301, n1631, n4685);
    let n4692: ZB = zb_and(n1748, n4224);
    let n4693: ZN = zsel_n(n305, r_c396, n4686);
    let n4694: ZN = zsel_n(n305, r_c397, n4687);
    let n4695: ZN = zsel_n(n305, r_c398, n4688);
    let n4696: ZN = zsel_n(n305, r_c399, n4689);
    let n4697: ZN = zsel_n(n305, r_c408, n4690);
    let n4698: ZN = zsel_n(n305, r_c409, n4691);
    let n4699: ZB = zb_or(n305, n4692);
    let n4700: ZB = zb_and(n4619, n4699);
    let n4701: ZB = zb_and(n4620, n4699);
    let n4702: ZB = zb_and(n4490, n4701);
    let n4703: ZB = zb_and(n4489, n4701);
    let n4704: ZB = zb_or(n4702, n4703);
    let n4705: ZB = zb_and(n4495, n4704);
    let n4706: ZB = zb_and(n4496, n4704);
    let n4707: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4697);
    let n4708: ZB = zb_or(n4705, n4706);
    let n4709: ZN = zsel_n(n4619, n4697, n4707);
    let n4710: ZB = zb_or(n4700, n4708);
    let n4711: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n1789);
    let n4712: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n1827);
    let n4713: ZN = zsel_n(n301, n1621, n4711);
    let n4714: ZN = zsel_n(n301, n1631, n4712);
    let n4715: ZN = zsel_n(n305, r_c408, n4713);
    let n4716: ZN = zsel_n(n305, r_c409, n4714);
    let n4717: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4715);
    let n4718: ZN = zsel_n(n4619, n4715, n4717);
    let n4719: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n1856);
    let n4720: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n1894);
    let n4721: ZN = zsel_n(n301, n1621, n4719);
    let n4722: ZN = zsel_n(n301, n1631, n4720);
    let n4723: ZN = zsel_n(n305, r_c408, n4721);
    let n4724: ZN = zsel_n(n305, r_c409, n4722);
    let n4725: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4723);
    let n4726: ZN = zsel_n(n4619, n4723, n4725);
    let n4727: ZN = zsel_n(n3745, zn_splat(P8::from_raw(131072i32)), r_c399);
    let n4728: ZN = zsel_n(n3745, zn_splat(P8::from_raw(327680i32)), n1711);
    let n4729: ZN = zsel_n(n301, r_c399, n4727);
    let n4730: ZN = zsel_n(n301, n1631, n4728);
    let n4731: ZN = zsel_n(n305, r_c399, n4729);
    let n4732: ZN = zsel_n(n305, r_c409, n4730);
    let n4733: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n1827);
    let n4734: ZN = zsel_n(n301, n1631, n4733);
    let n4735: ZN = zsel_n(n305, r_c409, n4734);
    let n4736: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n1894);
    let n4737: ZN = zsel_n(n301, n1631, n4736);
    let n4738: ZN = zsel_n(n305, r_c409, n4737);
    let n4739: ZN = zsel_n(n3745, n1733, n1944);
    let n4740: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1945);
    let n4741: ZN = zsel_n(n301, n1621, n4739);
    let n4742: ZN = zsel_n(n301, n1631, n4740);
    let n4743: ZB = zb_and(n1748, n4254);
    let n4744: ZN = zsel_n(n305, r_c408, n4741);
    let n4745: ZN = zsel_n(n305, r_c409, n4742);
    let n4746: ZB = zb_or(n305, n4743);
    let n4747: ZB = zb_and(n4619, n4746);
    let n4748: ZB = zb_and(n4620, n4746);
    let n4749: ZB = zb_and(n4490, n4748);
    let n4750: ZB = zb_and(n4489, n4748);
    let n4751: ZB = zb_or(n4749, n4750);
    let n4752: ZB = zb_and(n4495, n4751);
    let n4753: ZB = zb_and(n4496, n4751);
    let n4754: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4744);
    let n4755: ZB = zb_or(n4752, n4753);
    let n4756: ZN = zsel_n(n4619, n4744, n4754);
    let n4757: ZB = zb_or(n4747, n4755);
    let n4758: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-327680i32)), n1990);
    let n4759: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1991);
    let n4760: ZN = zsel_n(n301, n1621, n4758);
    let n4761: ZN = zsel_n(n301, n1631, n4759);
    let n4762: ZB = zb_and(n1748, n4272);
    let n4763: ZN = zsel_n(n305, r_c408, n4760);
    let n4764: ZN = zsel_n(n305, r_c409, n4761);
    let n4765: ZB = zb_or(n305, n4762);
    let n4766: ZB = zb_and(n4619, n4765);
    let n4767: ZB = zb_and(n4620, n4765);
    let n4768: ZB = zb_and(n4490, n4767);
    let n4769: ZB = zb_and(n4489, n4767);
    let n4770: ZB = zb_or(n4768, n4769);
    let n4771: ZB = zb_and(n4495, n4770);
    let n4772: ZB = zb_and(n4496, n4770);
    let n4773: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4763);
    let n4774: ZB = zb_or(n4771, n4772);
    let n4775: ZN = zsel_n(n4619, n4763, n4773);
    let n4776: ZB = zb_or(n4766, n4774);
    let n4777: ZN = zsel_n(n3745, zn_splat(P8::from_raw(327680i32)), n2035);
    let n4778: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n2036);
    let n4779: ZN = zsel_n(n301, n1621, n4777);
    let n4780: ZN = zsel_n(n301, n1631, n4778);
    let n4781: ZB = zb_and(n1748, n4290);
    let n4782: ZN = zsel_n(n305, r_c408, n4779);
    let n4783: ZN = zsel_n(n305, r_c409, n4780);
    let n4784: ZB = zb_or(n305, n4781);
    let n4785: ZB = zb_and(n4619, n4784);
    let n4786: ZB = zb_and(n4620, n4784);
    let n4787: ZB = zb_and(n4490, n4786);
    let n4788: ZB = zb_and(n4489, n4786);
    let n4789: ZB = zb_or(n4787, n4788);
    let n4790: ZB = zb_and(n4495, n4789);
    let n4791: ZB = zb_and(n4496, n4789);
    let n4792: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4782);
    let n4793: ZB = zb_or(n4790, n4791);
    let n4794: ZN = zsel_n(n4619, n4782, n4792);
    let n4795: ZB = zb_or(n4785, n4793);
    let n4796: ZN = zsel_n(n3745, zn_splat(P8::from_raw(0i32)), n1944);
    let n4797: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-327680i32)), n1945);
    let n4798: ZN = zsel_n(n301, n1621, n4796);
    let n4799: ZN = zsel_n(n301, n1631, n4797);
    let n4800: ZB = zb_and(n1748, n4306);
    let n4801: ZN = zsel_n(n305, r_c408, n4798);
    let n4802: ZN = zsel_n(n305, r_c409, n4799);
    let n4803: ZB = zb_or(n305, n4800);
    let n4804: ZB = zb_and(n4619, n4803);
    let n4805: ZB = zb_and(n4620, n4803);
    let n4806: ZB = zb_and(n4490, n4805);
    let n4807: ZB = zb_and(n4489, n4805);
    let n4808: ZB = zb_or(n4806, n4807);
    let n4809: ZB = zb_and(n4495, n4808);
    let n4810: ZB = zb_and(n4496, n4808);
    let n4811: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4801);
    let n4812: ZB = zb_or(n4809, n4810);
    let n4813: ZN = zsel_n(n4619, n4801, n4811);
    let n4814: ZB = zb_or(n4804, n4812);
    let n4815: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n1990);
    let n4816: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n1991);
    let n4817: ZN = zsel_n(n301, n1621, n4815);
    let n4818: ZN = zsel_n(n301, n1631, n4816);
    let n4819: ZN = zsel_n(n305, r_c408, n4817);
    let n4820: ZN = zsel_n(n305, r_c409, n4818);
    let n4821: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4819);
    let n4822: ZN = zsel_n(n4619, n4819, n4821);
    let n4823: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n2035);
    let n4824: ZN = zsel_n(n3745, zn_splat(P8::from_raw(-231700i32)), n2036);
    let n4825: ZN = zsel_n(n301, n1621, n4823);
    let n4826: ZN = zsel_n(n301, n1631, n4824);
    let n4827: ZN = zsel_n(n305, r_c408, n4825);
    let n4828: ZN = zsel_n(n305, r_c409, n4826);
    let n4829: ZN = zsel_n(n4495, zn_splat(P8::from_raw(0i32)), n4827);
    let n4830: ZN = zsel_n(n4619, n4827, n4829);
    let n4831: ZN = zsel_n(n3745, zn_splat(P8::from_raw(327680i32)), n1945);
    let n4832: ZN = zsel_n(n301, n1631, n4831);
    let n4833: ZN = zsel_n(n305, r_c409, n4832);
    let n4834: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n1991);
    let n4835: ZN = zsel_n(n301, n1631, n4834);
    let n4836: ZN = zsel_n(n305, r_c409, n4835);
    let n4837: ZN = zsel_n(n3745, zn_splat(P8::from_raw(231700i32)), n2036);
    let n4838: ZN = zsel_n(n301, n1631, n4837);
    let n4839: ZN = zsel_n(n305, r_c409, n4838);
    let n4842: ZW = zw_bits_n(r_c39);
    let n4843: ZW = zw_mix1(zw_splat(11400714819323198485u64), n4842, 39u64);
    let n4844: ZW = zw_mix2(zw_splat(11562461410679940143u64), n4842, 39u64);
    let n4845: ZW = zw_bits_n(n150);
    let n4846: ZW = zw_mix1(n4843, n4845, 84u64);
    let n4847: ZW = zw_mix2(n4844, n4845, 84u64);
    let n4848: ZW = zw_bits_n(n271);
    let n4849: ZW = zw_mix1(n4846, n4848, 85u64);
    let n4850: ZW = zw_mix2(n4847, n4848, 85u64);
    let n4851: ZW = zw_bits_n(n270);
    let n4852: ZW = zw_mix1(n4849, n4851, 86u64);
    let n4853: ZW = zw_mix2(n4850, n4851, 86u64);
    let n4854: ZW = zw_bits_n(r_c87);
    let n4855: ZW = zw_mix1(n4852, n4854, 87u64);
    let n4856: ZW = zw_mix2(n4853, n4854, 87u64);
    let n4857: ZW = zw_bits_n(n345);
    let n4858: ZW = zw_mix1(n4855, n4857, 241u64);
    let n4859: ZW = zw_mix2(n4856, n4857, 241u64);
    let n4860: ZW = zw_bits_n(n346);
    let n4861: ZW = zw_mix1(n4858, n4860, 254u64);
    let n4862: ZW = zw_mix2(n4859, n4860, 254u64);
    let n4863: ZW = zw_bits_n(n397);
    let n4864: ZW = zw_mix1(n4861, n4863, 261u64);
    let n4865: ZW = zw_mix2(n4862, n4863, 261u64);
    let n4866: ZW = zw_bits_n(n398);
    let n4867: ZW = zw_mix1(n4864, n4866, 274u64);
    let n4868: ZW = zw_mix2(n4865, n4866, 274u64);
    let n4869: ZW = zw_bits_n(n920);
    let n4870: ZW = zw_mix1(n4867, n4869, 302u64);
    let n4871: ZW = zw_mix2(n4868, n4869, 302u64);
    let n4872: ZW = zw_bits_n(n921);
    let n4873: ZW = zw_mix1(n4870, n4872, 368u64);
    let n4874: ZW = zw_mix2(n4871, n4872, 368u64);
    let n4875: ZW = zw_bits_n(n922);
    let n4876: ZW = zw_mix1(n4873, n4875, 369u64);
    let n4877: ZW = zw_mix2(n4874, n4875, 369u64);
    let n4878: ZW = zw_bits_n(r_c20);
    let n4879: ZW = zw_mix1(n4876, n4878, 20u64);
    let n4880: ZW = zw_mix2(n4877, n4878, 20u64);
    let n4881: ZW = zw_bits_b(r_c41);
    let n4882: ZW = zw_mix1(n4879, n4881, 41u64);
    let n4883: ZW = zw_mix2(n4880, n4881, 41u64);
    let n4884: ZW = zw_bits_n(n300);
    let n4885: ZW = zw_mix1(n4882, n4884, 282u64);
    let n4886: ZW = zw_mix2(n4883, n4884, 282u64);
    let n4887: ZW = zw_bits_n(n304);
    let n4888: ZW = zw_mix1(n4885, n4887, 284u64);
    let n4889: ZW = zw_mix2(n4886, n4887, 284u64);
    let n4890: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n4891: ZW = zw_mix1(n4888, zw_splat(n4890), 285u64);
    let n4892: ZW = zw_mix2(n4889, zw_splat(n4890), 285u64);
    let n4893: ZW = zw_bits_n(n1609);
    let n4894: ZW = zw_mix1(n4891, n4893, 287u64);
    let n4895: ZW = zw_mix2(n4892, n4893, 287u64);
    let n4896: u64 = false as u64;
    let n4897: ZW = zw_mix1(n4894, zw_splat(n4896), 294u64);
    let n4898: ZW = zw_mix2(n4895, zw_splat(n4896), 294u64);
    let n4899: ZW = zw_mix1(n4897, zw_splat(n4896), 295u64);
    let n4900: ZW = zw_mix2(n4898, zw_splat(n4896), 295u64);
    let n4901: ZW = zw_bits_n(n1769);
    let n4902: ZW = zw_mix1(n4899, n4901, 301u64);
    let n4903: ZW = zw_mix2(n4900, n4901, 301u64);
    let n4904: ZW = zw_bits_n(r_c396);
    let n4905: ZW = zw_mix1(n4902, n4904, 358u64);
    let n4906: ZW = zw_mix2(n4903, n4904, 358u64);
    let n4907: ZW = zw_bits_n(r_c397);
    let n4908: ZW = zw_mix1(n4905, n4907, 359u64);
    let n4909: ZW = zw_mix2(n4906, n4907, 359u64);
    let n4910: ZW = zw_bits_n(r_c398);
    let n4911: ZW = zw_mix1(n4908, n4910, 360u64);
    let n4912: ZW = zw_mix2(n4909, n4910, 360u64);
    let n4913: ZW = zw_bits_n(r_c399);
    let n4914: ZW = zw_mix1(n4911, n4913, 361u64);
    let n4915: ZW = zw_mix2(n4912, n4913, 361u64);
    let n4916: ZW = zw_bits_b(n1743);
    let n4917: ZW = zw_mix1(n4914, n4916, 362u64);
    let n4918: ZW = zw_mix2(n4915, n4916, 362u64);
    let n4919: ZW = zw_bits_n(n1770);
    let n4920: ZW = zw_mix1(n4917, n4919, 370u64);
    let n4921: ZW = zw_mix2(n4918, n4919, 370u64);
    let n4922: ZW = zw_bits_n(n1745);
    let n4923: ZW = zw_mix1(n4920, n4922, 371u64);
    let n4924: ZW = zw_mix2(n4921, n4922, 371u64);
    let n4925: ZW = zw_bits_b(n1833);
    let n4926: ZW = zw_mix1(n4914, n4925, 362u64);
    let n4927: ZW = zw_mix2(n4915, n4925, 362u64);
    let n4928: ZW = zw_bits_n(n1849);
    let n4929: ZW = zw_mix1(n4926, n4928, 370u64);
    let n4930: ZW = zw_mix2(n4927, n4928, 370u64);
    let n4931: ZW = zw_bits_n(n1835);
    let n4932: ZW = zw_mix1(n4929, n4931, 371u64);
    let n4933: ZW = zw_mix2(n4930, n4931, 371u64);
    let n4934: ZW = zw_bits_b(n1900);
    let n4935: ZW = zw_mix1(n4914, n4934, 362u64);
    let n4936: ZW = zw_mix2(n4915, n4934, 362u64);
    let n4937: ZW = zw_bits_n(n1916);
    let n4938: ZW = zw_mix1(n4935, n4937, 370u64);
    let n4939: ZW = zw_mix2(n4936, n4937, 370u64);
    let n4940: ZW = zw_bits_n(n1902);
    let n4941: ZW = zw_mix1(n4938, n4940, 371u64);
    let n4942: ZW = zw_mix2(n4939, n4940, 371u64);
    let n4943: ZW = zw_bits_n(n1947);
    let n4944: ZW = zw_mix1(n4891, n4943, 287u64);
    let n4945: ZW = zw_mix2(n4892, n4943, 287u64);
    let n4946: ZW = zw_mix1(n4944, zw_splat(n4896), 294u64);
    let n4947: ZW = zw_mix2(n4945, zw_splat(n4896), 294u64);
    let n4948: u64 = true as u64;
    let n4949: ZW = zw_mix1(n4946, zw_splat(n4948), 295u64);
    let n4950: ZW = zw_mix2(n4947, zw_splat(n4948), 295u64);
    let n4951: ZW = zw_mix1(n4949, n4901, 301u64);
    let n4952: ZW = zw_mix2(n4950, n4901, 301u64);
    let n4953: ZW = zw_mix1(n4951, n4904, 358u64);
    let n4954: ZW = zw_mix2(n4952, n4904, 358u64);
    let n4955: ZW = zw_mix1(n4953, n4907, 359u64);
    let n4956: ZW = zw_mix2(n4954, n4907, 359u64);
    let n4957: ZW = zw_mix1(n4955, n4910, 360u64);
    let n4958: ZW = zw_mix2(n4956, n4910, 360u64);
    let n4959: ZW = zw_mix1(n4957, n4913, 361u64);
    let n4960: ZW = zw_mix2(n4958, n4913, 361u64);
    let n4961: ZW = zw_mix1(n4959, n4916, 362u64);
    let n4962: ZW = zw_mix2(n4960, n4916, 362u64);
    let n4963: ZW = zw_bits_n(n1963);
    let n4964: ZW = zw_mix1(n4961, n4963, 370u64);
    let n4965: ZW = zw_mix2(n4962, n4963, 370u64);
    let n4966: ZW = zw_bits_n(n1949);
    let n4967: ZW = zw_mix1(n4964, n4966, 371u64);
    let n4968: ZW = zw_mix2(n4965, n4966, 371u64);
    let n4969: ZW = zw_mix1(n4959, n4925, 362u64);
    let n4970: ZW = zw_mix2(n4960, n4925, 362u64);
    let n4971: ZW = zw_bits_n(n2008);
    let n4972: ZW = zw_mix1(n4969, n4971, 370u64);
    let n4973: ZW = zw_mix2(n4970, n4971, 370u64);
    let n4974: ZW = zw_bits_n(n1994);
    let n4975: ZW = zw_mix1(n4972, n4974, 371u64);
    let n4976: ZW = zw_mix2(n4973, n4974, 371u64);
    let n4977: ZW = zw_mix1(n4959, n4934, 362u64);
    let n4978: ZW = zw_mix2(n4960, n4934, 362u64);
    let n4979: ZW = zw_bits_n(n2053);
    let n4980: ZW = zw_mix1(n4977, n4979, 370u64);
    let n4981: ZW = zw_mix2(n4978, n4979, 370u64);
    let n4982: ZW = zw_bits_n(n2039);
    let n4983: ZW = zw_mix1(n4980, n4982, 371u64);
    let n4984: ZW = zw_mix2(n4981, n4982, 371u64);
    let n4985: ZW = zw_bits_n(n2061);
    let n4986: ZW = zw_mix1(n4876, n4985, 20u64);
    let n4987: ZW = zw_mix2(n4877, n4985, 20u64);
    let n4988: ZW = zw_bits_b(n2062);
    let n4989: ZW = zw_mix1(n4986, n4988, 41u64);
    let n4990: ZW = zw_mix2(n4987, n4988, 41u64);
    let n4991: ZW = zw_bits_n(n2063);
    let n4992: ZW = zw_mix1(n4989, n4991, 282u64);
    let n4993: ZW = zw_mix2(n4990, n4991, 282u64);
    let n4994: ZW = zw_bits_n(n2064);
    let n4995: ZW = zw_mix1(n4992, n4994, 284u64);
    let n4996: ZW = zw_mix2(n4993, n4994, 284u64);
    let n4997: ZW = zw_bits_n(n2065);
    let n4998: ZW = zw_mix1(n4995, n4997, 285u64);
    let n4999: ZW = zw_mix2(n4996, n4997, 285u64);
    let n5000: ZW = zw_mix1(n4998, n4893, 287u64);
    let n5001: ZW = zw_mix2(n4999, n4893, 287u64);
    let n5002: ZW = zw_mix1(n5000, zw_splat(n4948), 294u64);
    let n5003: ZW = zw_mix2(n5001, zw_splat(n4948), 294u64);
    let n5004: ZW = zw_mix1(n5002, zw_splat(n4896), 295u64);
    let n5005: ZW = zw_mix2(n5003, zw_splat(n4896), 295u64);
    let n5006: ZW = zw_bits_n(n2107);
    let n5007: ZW = zw_mix1(n5004, n5006, 301u64);
    let n5008: ZW = zw_mix2(n5005, n5006, 301u64);
    let n5009: ZW = zw_bits_n(n2066);
    let n5010: ZW = zw_mix1(n5007, n5009, 358u64);
    let n5011: ZW = zw_mix2(n5008, n5009, 358u64);
    let n5012: ZW = zw_bits_n(n2089);
    let n5013: ZW = zw_mix1(n5010, n5012, 359u64);
    let n5014: ZW = zw_mix2(n5011, n5012, 359u64);
    let n5015: ZW = zw_bits_n(n2090);
    let n5016: ZW = zw_mix1(n5013, n5015, 360u64);
    let n5017: ZW = zw_mix2(n5014, n5015, 360u64);
    let n5018: ZW = zw_bits_n(n2067);
    let n5019: ZW = zw_mix1(n5016, n5018, 361u64);
    let n5020: ZW = zw_mix2(n5017, n5018, 361u64);
    let n5021: ZW = zw_mix1(n5019, n4916, 362u64);
    let n5022: ZW = zw_mix2(n5020, n4916, 362u64);
    let n5023: ZW = zw_bits_n(n2108);
    let n5024: ZW = zw_mix1(n5021, n5023, 370u64);
    let n5025: ZW = zw_mix2(n5022, n5023, 370u64);
    let n5026: ZW = zw_bits_n(n2092);
    let n5027: ZW = zw_mix1(n5024, n5026, 371u64);
    let n5028: ZW = zw_mix2(n5025, n5026, 371u64);
    let n5029: ZW = zw_bits_n(n2117);
    let n5030: ZW = zw_mix1(n5010, n5029, 359u64);
    let n5031: ZW = zw_mix2(n5011, n5029, 359u64);
    let n5032: ZW = zw_bits_n(n2118);
    let n5033: ZW = zw_mix1(n5030, n5032, 360u64);
    let n5034: ZW = zw_mix2(n5031, n5032, 360u64);
    let n5035: ZW = zw_mix1(n5033, n5018, 361u64);
    let n5036: ZW = zw_mix2(n5034, n5018, 361u64);
    let n5037: ZW = zw_mix1(n5035, n4925, 362u64);
    let n5038: ZW = zw_mix2(n5036, n4925, 362u64);
    let n5039: ZW = zw_bits_n(n2135);
    let n5040: ZW = zw_mix1(n5037, n5039, 370u64);
    let n5041: ZW = zw_mix2(n5038, n5039, 370u64);
    let n5042: ZW = zw_bits_n(n2120);
    let n5043: ZW = zw_mix1(n5040, n5042, 371u64);
    let n5044: ZW = zw_mix2(n5041, n5042, 371u64);
    let n5045: ZW = zw_bits_n(n2143);
    let n5046: ZW = zw_mix1(n5030, n5045, 360u64);
    let n5047: ZW = zw_mix2(n5031, n5045, 360u64);
    let n5048: ZW = zw_mix1(n5046, n5018, 361u64);
    let n5049: ZW = zw_mix2(n5047, n5018, 361u64);
    let n5050: ZW = zw_mix1(n5048, n4934, 362u64);
    let n5051: ZW = zw_mix2(n5049, n4934, 362u64);
    let n5052: ZW = zw_bits_n(n2160);
    let n5053: ZW = zw_mix1(n5050, n5052, 370u64);
    let n5054: ZW = zw_mix2(n5051, n5052, 370u64);
    let n5055: ZW = zw_bits_n(n2145);
    let n5056: ZW = zw_mix1(n5053, n5055, 371u64);
    let n5057: ZW = zw_mix2(n5054, n5055, 371u64);
    let n5058: ZW = zw_bits_n(n2165);
    let n5059: ZW = zw_mix1(n5007, n5058, 358u64);
    let n5060: ZW = zw_mix2(n5008, n5058, 358u64);
    let n5061: ZW = zw_bits_n(n2172);
    let n5062: ZW = zw_mix1(n5059, n5061, 359u64);
    let n5063: ZW = zw_mix2(n5060, n5061, 359u64);
    let n5064: ZW = zw_bits_n(n2173);
    let n5065: ZW = zw_mix1(n5062, n5064, 360u64);
    let n5066: ZW = zw_mix2(n5063, n5064, 360u64);
    let n5067: ZW = zw_bits_n(n2166);
    let n5068: ZW = zw_mix1(n5065, n5067, 361u64);
    let n5069: ZW = zw_mix2(n5066, n5067, 361u64);
    let n5070: ZW = zw_mix1(n5068, n4916, 362u64);
    let n5071: ZW = zw_mix2(n5069, n4916, 362u64);
    let n5072: ZW = zw_bits_n(n2190);
    let n5073: ZW = zw_mix1(n5070, n5072, 370u64);
    let n5074: ZW = zw_mix2(n5071, n5072, 370u64);
    let n5075: ZW = zw_bits_n(n2175);
    let n5076: ZW = zw_mix1(n5073, n5075, 371u64);
    let n5077: ZW = zw_mix2(n5074, n5075, 371u64);
    let n5078: ZW = zw_mix1(n5059, n5029, 359u64);
    let n5079: ZW = zw_mix2(n5060, n5029, 359u64);
    let n5080: ZW = zw_mix1(n5078, n5032, 360u64);
    let n5081: ZW = zw_mix2(n5079, n5032, 360u64);
    let n5082: ZW = zw_mix1(n5080, n5067, 361u64);
    let n5083: ZW = zw_mix2(n5081, n5067, 361u64);
    let n5084: ZW = zw_mix1(n5082, n4925, 362u64);
    let n5085: ZW = zw_mix2(n5083, n4925, 362u64);
    let n5086: ZW = zw_bits_n(n2197);
    let n5087: ZW = zw_mix1(n5084, n5086, 370u64);
    let n5088: ZW = zw_mix2(n5085, n5086, 370u64);
    let n5089: ZW = zw_bits_n(n2195);
    let n5090: ZW = zw_mix1(n5087, n5089, 371u64);
    let n5091: ZW = zw_mix2(n5088, n5089, 371u64);
    let n5092: ZW = zw_mix1(n5078, n5045, 360u64);
    let n5093: ZW = zw_mix2(n5079, n5045, 360u64);
    let n5094: ZW = zw_mix1(n5092, n5067, 361u64);
    let n5095: ZW = zw_mix2(n5093, n5067, 361u64);
    let n5096: ZW = zw_mix1(n5094, n4934, 362u64);
    let n5097: ZW = zw_mix2(n5095, n4934, 362u64);
    let n5098: ZW = zw_bits_n(n2203);
    let n5099: ZW = zw_mix1(n5096, n5098, 370u64);
    let n5100: ZW = zw_mix2(n5097, n5098, 370u64);
    let n5101: ZW = zw_bits_n(n2201);
    let n5102: ZW = zw_mix1(n5099, n5101, 371u64);
    let n5103: ZW = zw_mix2(n5100, n5101, 371u64);
    let n5104: ZW = zw_bits_n(n2205);
    let n5105: ZW = zw_mix1(n5065, n5104, 361u64);
    let n5106: ZW = zw_mix2(n5066, n5104, 361u64);
    let n5107: ZW = zw_mix1(n5105, n4916, 362u64);
    let n5108: ZW = zw_mix2(n5106, n4916, 362u64);
    let n5109: ZW = zw_mix1(n5107, n5072, 370u64);
    let n5110: ZW = zw_mix2(n5108, n5072, 370u64);
    let n5111: ZW = zw_bits_n(n2207);
    let n5112: ZW = zw_mix1(n5109, n5111, 371u64);
    let n5113: ZW = zw_mix2(n5110, n5111, 371u64);
    let n5114: ZW = zw_mix1(n5080, n5104, 361u64);
    let n5115: ZW = zw_mix2(n5081, n5104, 361u64);
    let n5116: ZW = zw_mix1(n5114, n4925, 362u64);
    let n5117: ZW = zw_mix2(n5115, n4925, 362u64);
    let n5118: ZW = zw_mix1(n5116, n5086, 370u64);
    let n5119: ZW = zw_mix2(n5117, n5086, 370u64);
    let n5120: ZW = zw_bits_n(n2209);
    let n5121: ZW = zw_mix1(n5118, n5120, 371u64);
    let n5122: ZW = zw_mix2(n5119, n5120, 371u64);
    let n5123: ZW = zw_mix1(n5092, n5104, 361u64);
    let n5124: ZW = zw_mix2(n5093, n5104, 361u64);
    let n5125: ZW = zw_mix1(n5123, n4934, 362u64);
    let n5126: ZW = zw_mix2(n5124, n4934, 362u64);
    let n5127: ZW = zw_mix1(n5125, n5098, 370u64);
    let n5128: ZW = zw_mix2(n5126, n5098, 370u64);
    let n5129: ZW = zw_bits_n(n2211);
    let n5130: ZW = zw_mix1(n5127, n5129, 371u64);
    let n5131: ZW = zw_mix2(n5128, n5129, 371u64);
    let n5132: ZW = zw_mix1(n4998, n4943, 287u64);
    let n5133: ZW = zw_mix2(n4999, n4943, 287u64);
    let n5134: ZW = zw_mix1(n5132, zw_splat(n4948), 294u64);
    let n5135: ZW = zw_mix2(n5133, zw_splat(n4948), 294u64);
    let n5136: ZW = zw_mix1(n5134, zw_splat(n4948), 295u64);
    let n5137: ZW = zw_mix2(n5135, zw_splat(n4948), 295u64);
    let n5138: ZW = zw_mix1(n5136, n5006, 301u64);
    let n5139: ZW = zw_mix2(n5137, n5006, 301u64);
    let n5140: ZW = zw_mix1(n5138, n5009, 358u64);
    let n5141: ZW = zw_mix2(n5139, n5009, 358u64);
    let n5142: ZW = zw_mix1(n5140, n5012, 359u64);
    let n5143: ZW = zw_mix2(n5141, n5012, 359u64);
    let n5144: ZW = zw_mix1(n5142, n5015, 360u64);
    let n5145: ZW = zw_mix2(n5143, n5015, 360u64);
    let n5146: ZW = zw_mix1(n5144, n5018, 361u64);
    let n5147: ZW = zw_mix2(n5145, n5018, 361u64);
    let n5148: ZW = zw_mix1(n5146, n4916, 362u64);
    let n5149: ZW = zw_mix2(n5147, n4916, 362u64);
    let n5150: ZW = zw_bits_n(n2245);
    let n5151: ZW = zw_mix1(n5148, n5150, 370u64);
    let n5152: ZW = zw_mix2(n5149, n5150, 370u64);
    let n5153: ZW = zw_bits_n(n2230);
    let n5154: ZW = zw_mix1(n5151, n5153, 371u64);
    let n5155: ZW = zw_mix2(n5152, n5153, 371u64);
    let n5156: ZW = zw_mix1(n5140, n5029, 359u64);
    let n5157: ZW = zw_mix2(n5141, n5029, 359u64);
    let n5158: ZW = zw_mix1(n5156, n5032, 360u64);
    let n5159: ZW = zw_mix2(n5157, n5032, 360u64);
    let n5160: ZW = zw_mix1(n5158, n5018, 361u64);
    let n5161: ZW = zw_mix2(n5159, n5018, 361u64);
    let n5162: ZW = zw_mix1(n5160, n4925, 362u64);
    let n5163: ZW = zw_mix2(n5161, n4925, 362u64);
    let n5164: ZW = zw_bits_n(n2268);
    let n5165: ZW = zw_mix1(n5162, n5164, 370u64);
    let n5166: ZW = zw_mix2(n5163, n5164, 370u64);
    let n5167: ZW = zw_bits_n(n2253);
    let n5168: ZW = zw_mix1(n5165, n5167, 371u64);
    let n5169: ZW = zw_mix2(n5166, n5167, 371u64);
    let n5170: ZW = zw_mix1(n5156, n5045, 360u64);
    let n5171: ZW = zw_mix2(n5157, n5045, 360u64);
    let n5172: ZW = zw_mix1(n5170, n5018, 361u64);
    let n5173: ZW = zw_mix2(n5171, n5018, 361u64);
    let n5174: ZW = zw_mix1(n5172, n4934, 362u64);
    let n5175: ZW = zw_mix2(n5173, n4934, 362u64);
    let n5176: ZW = zw_bits_n(n2291);
    let n5177: ZW = zw_mix1(n5174, n5176, 370u64);
    let n5178: ZW = zw_mix2(n5175, n5176, 370u64);
    let n5179: ZW = zw_bits_n(n2276);
    let n5180: ZW = zw_mix1(n5177, n5179, 371u64);
    let n5181: ZW = zw_mix2(n5178, n5179, 371u64);
    let n5182: ZW = zw_mix1(n5138, n5058, 358u64);
    let n5183: ZW = zw_mix2(n5139, n5058, 358u64);
    let n5184: ZW = zw_mix1(n5182, n5061, 359u64);
    let n5185: ZW = zw_mix2(n5183, n5061, 359u64);
    let n5186: ZW = zw_mix1(n5184, n5064, 360u64);
    let n5187: ZW = zw_mix2(n5185, n5064, 360u64);
    let n5188: ZW = zw_mix1(n5186, n5067, 361u64);
    let n5189: ZW = zw_mix2(n5187, n5067, 361u64);
    let n5190: ZW = zw_mix1(n5188, n4916, 362u64);
    let n5191: ZW = zw_mix2(n5189, n4916, 362u64);
    let n5192: ZW = zw_bits_n(n2312);
    let n5193: ZW = zw_mix1(n5190, n5192, 370u64);
    let n5194: ZW = zw_mix2(n5191, n5192, 370u64);
    let n5195: ZW = zw_bits_n(n2297);
    let n5196: ZW = zw_mix1(n5193, n5195, 371u64);
    let n5197: ZW = zw_mix2(n5194, n5195, 371u64);
    let n5198: ZW = zw_mix1(n5182, n5029, 359u64);
    let n5199: ZW = zw_mix2(n5183, n5029, 359u64);
    let n5200: ZW = zw_mix1(n5198, n5032, 360u64);
    let n5201: ZW = zw_mix2(n5199, n5032, 360u64);
    let n5202: ZW = zw_mix1(n5200, n5067, 361u64);
    let n5203: ZW = zw_mix2(n5201, n5067, 361u64);
    let n5204: ZW = zw_mix1(n5202, n4925, 362u64);
    let n5205: ZW = zw_mix2(n5203, n4925, 362u64);
    let n5206: ZW = zw_bits_n(n2319);
    let n5207: ZW = zw_mix1(n5204, n5206, 370u64);
    let n5208: ZW = zw_mix2(n5205, n5206, 370u64);
    let n5209: ZW = zw_bits_n(n2317);
    let n5210: ZW = zw_mix1(n5207, n5209, 371u64);
    let n5211: ZW = zw_mix2(n5208, n5209, 371u64);
    let n5212: ZW = zw_mix1(n5198, n5045, 360u64);
    let n5213: ZW = zw_mix2(n5199, n5045, 360u64);
    let n5214: ZW = zw_mix1(n5212, n5067, 361u64);
    let n5215: ZW = zw_mix2(n5213, n5067, 361u64);
    let n5216: ZW = zw_mix1(n5214, n4934, 362u64);
    let n5217: ZW = zw_mix2(n5215, n4934, 362u64);
    let n5218: ZW = zw_bits_n(n2325);
    let n5219: ZW = zw_mix1(n5216, n5218, 370u64);
    let n5220: ZW = zw_mix2(n5217, n5218, 370u64);
    let n5221: ZW = zw_bits_n(n2323);
    let n5222: ZW = zw_mix1(n5219, n5221, 371u64);
    let n5223: ZW = zw_mix2(n5220, n5221, 371u64);
    let n5224: ZW = zw_mix1(n5186, n5104, 361u64);
    let n5225: ZW = zw_mix2(n5187, n5104, 361u64);
    let n5226: ZW = zw_mix1(n5224, n4916, 362u64);
    let n5227: ZW = zw_mix2(n5225, n4916, 362u64);
    let n5228: ZW = zw_mix1(n5226, n5192, 370u64);
    let n5229: ZW = zw_mix2(n5227, n5192, 370u64);
    let n5230: ZW = zw_bits_n(n2327);
    let n5231: ZW = zw_mix1(n5228, n5230, 371u64);
    let n5232: ZW = zw_mix2(n5229, n5230, 371u64);
    let n5233: ZW = zw_mix1(n5200, n5104, 361u64);
    let n5234: ZW = zw_mix2(n5201, n5104, 361u64);
    let n5235: ZW = zw_mix1(n5233, n4925, 362u64);
    let n5236: ZW = zw_mix2(n5234, n4925, 362u64);
    let n5237: ZW = zw_mix1(n5235, n5206, 370u64);
    let n5238: ZW = zw_mix2(n5236, n5206, 370u64);
    let n5239: ZW = zw_bits_n(n2329);
    let n5240: ZW = zw_mix1(n5237, n5239, 371u64);
    let n5241: ZW = zw_mix2(n5238, n5239, 371u64);
    let n5242: ZW = zw_mix1(n5212, n5104, 361u64);
    let n5243: ZW = zw_mix2(n5213, n5104, 361u64);
    let n5244: ZW = zw_mix1(n5242, n4934, 362u64);
    let n5245: ZW = zw_mix2(n5243, n4934, 362u64);
    let n5246: ZW = zw_mix1(n5244, n5218, 370u64);
    let n5247: ZW = zw_mix2(n5245, n5218, 370u64);
    let n5248: ZW = zw_bits_n(n2331);
    let n5249: ZW = zw_mix1(n5246, n5248, 371u64);
    let n5250: ZW = zw_mix2(n5247, n5248, 371u64);
    let n5251: ZW = zw_mix1(zw_splat(11400714819323198485u64), n4845, 84u64);
    let n5252: ZW = zw_mix2(zw_splat(11562461410679940143u64), n4845, 84u64);
    let n5253: ZW = zw_mix1(n5251, n4848, 85u64);
    let n5254: ZW = zw_mix2(n5252, n4848, 85u64);
    let n5255: ZW = zw_mix1(n5253, n4851, 86u64);
    let n5256: ZW = zw_mix2(n5254, n4851, 86u64);
    let n5257: ZW = zw_bits_n(n2475);
    let n5258: ZW = zw_mix1(n5255, n5257, 87u64);
    let n5259: ZW = zw_mix2(n5256, n5257, 87u64);
    let n5260: ZW = zw_mix1(n5258, n4857, 240u64);
    let n5261: ZW = zw_mix2(n5259, n4857, 240u64);
    let n5262: ZW = zw_mix1(n5260, n4860, 253u64);
    let n5263: ZW = zw_mix2(n5261, n4860, 253u64);
    let n5264: ZW = zw_mix1(n5262, n4863, 260u64);
    let n5265: ZW = zw_mix2(n5263, n4863, 260u64);
    let n5266: ZW = zw_mix1(n5264, n4866, 273u64);
    let n5267: ZW = zw_mix2(n5265, n4866, 273u64);
    let n5268: ZW = zw_mix1(n5266, n4878, 20u64);
    let n5269: ZW = zw_mix2(n5267, n4878, 20u64);
    let n5270: ZW = zw_mix1(n5268, n4881, 41u64);
    let n5271: ZW = zw_mix2(n5269, n4881, 41u64);
    let n5272: ZW = zw_mix1(n5266, n4985, 20u64);
    let n5273: ZW = zw_mix2(n5267, n4985, 20u64);
    let n5274: ZW = zw_mix1(n5272, n4988, 41u64);
    let n5275: ZW = zw_mix2(n5273, n4988, 41u64);
    let n5276: ZW = zw_bits_n(n3561);
    let n5277: ZW = zw_mix1(n5255, n5276, 87u64);
    let n5278: ZW = zw_mix2(n5256, n5276, 87u64);
    let n5279: ZW = zw_bits_n(n2754);
    let n5280: ZW = zw_mix1(n5277, n5279, 245u64);
    let n5281: ZW = zw_mix2(n5278, n5279, 245u64);
    let n5282: ZW = zw_bits_n(n2759);
    let n5283: ZW = zw_mix1(n5280, n5282, 253u64);
    let n5284: ZW = zw_mix2(n5281, n5282, 253u64);
    let n5285: ZW = zw_mix1(n5283, n4857, 257u64);
    let n5286: ZW = zw_mix2(n5284, n4857, 257u64);
    let n5287: ZW = zw_mix1(n5285, n4860, 270u64);
    let n5288: ZW = zw_mix2(n5286, n4860, 270u64);
    let n5289: ZW = zw_mix1(n5287, n4863, 277u64);
    let n5290: ZW = zw_mix2(n5288, n4863, 277u64);
    let n5291: ZW = zw_mix1(n5289, n4866, 290u64);
    let n5292: ZW = zw_mix2(n5290, n4866, 290u64);
    let n5293: ZW = zw_mix1(n5291, n4878, 20u64);
    let n5294: ZW = zw_mix2(n5292, n4878, 20u64);
    let n5295: ZW = zw_mix1(n5293, n4881, 41u64);
    let n5296: ZW = zw_mix2(n5294, n4881, 41u64);
    let n5297: ZW = zw_bits_n(n3749);
    let n5298: ZW = zw_mix1(n5291, n5297, 20u64);
    let n5299: ZW = zw_mix2(n5292, n5297, 20u64);
    let n5300: ZW = zw_bits_b(n3750);
    let n5301: ZW = zw_mix1(n5298, n5300, 41u64);
    let n5302: ZW = zw_mix2(n5299, n5300, 41u64);
    let n5303: ZW = zw_mix1(n5255, n4878, 20u64);
    let n5304: ZW = zw_mix2(n5256, n4878, 20u64);
    let n5305: ZW = zw_bits_b(n3930);
    let n5306: ZW = zw_mix1(n5303, n5305, 38u64);
    let n5307: ZW = zw_mix2(n5304, n5305, 38u64);
    let n5308: ZW = zw_bits_n(n3935);
    let n5309: ZW = zw_mix1(n5306, n5308, 39u64);
    let n5310: ZW = zw_mix2(n5307, n5308, 39u64);
    let n5311: ZW = zw_bits_n(n3934);
    let n5312: ZW = zw_mix1(n5309, n5311, 87u64);
    let n5313: ZW = zw_mix2(n5310, n5311, 87u64);
    let n5314: ZW = zw_bits_b(n3974);
    let n5315: ZW = zw_mix1(n5303, n5314, 38u64);
    let n5316: ZW = zw_mix2(n5304, n5314, 38u64);
    let n5317: ZW = zw_bits_n(n3979);
    let n5318: ZW = zw_mix1(n5315, n5317, 39u64);
    let n5319: ZW = zw_mix2(n5316, n5317, 39u64);
    let n5320: ZW = zw_bits_n(n3978);
    let n5321: ZW = zw_mix1(n5318, n5320, 87u64);
    let n5322: ZW = zw_mix2(n5319, n5320, 87u64);
    let n5323: ZW = zw_bits_b(n4018);
    let n5324: ZW = zw_mix1(n5303, n5323, 38u64);
    let n5325: ZW = zw_mix2(n5304, n5323, 38u64);
    let n5326: ZW = zw_bits_n(n4023);
    let n5327: ZW = zw_mix1(n5324, n5326, 39u64);
    let n5328: ZW = zw_mix2(n5325, n5326, 39u64);
    let n5329: ZW = zw_bits_n(n4022);
    let n5330: ZW = zw_mix1(n5327, n5329, 87u64);
    let n5331: ZW = zw_mix2(n5328, n5329, 87u64);
    let n5332: ZW = zw_bits_b(n4061);
    let n5333: ZW = zw_mix1(n5303, n5332, 38u64);
    let n5334: ZW = zw_mix2(n5304, n5332, 38u64);
    let n5335: ZW = zw_bits_n(n4066);
    let n5336: ZW = zw_mix1(n5333, n5335, 39u64);
    let n5337: ZW = zw_mix2(n5334, n5335, 39u64);
    let n5338: ZW = zw_bits_n(n4065);
    let n5339: ZW = zw_mix1(n5336, n5338, 87u64);
    let n5340: ZW = zw_mix2(n5337, n5338, 87u64);
    let n5341: ZW = zw_bits_b(n4104);
    let n5342: ZW = zw_mix1(n5303, n5341, 38u64);
    let n5343: ZW = zw_mix2(n5304, n5341, 38u64);
    let n5344: ZW = zw_bits_n(n4109);
    let n5345: ZW = zw_mix1(n5342, n5344, 39u64);
    let n5346: ZW = zw_mix2(n5343, n5344, 39u64);
    let n5347: ZW = zw_bits_n(n4108);
    let n5348: ZW = zw_mix1(n5345, n5347, 87u64);
    let n5349: ZW = zw_mix2(n5346, n5347, 87u64);
    let n5350: ZW = zw_bits_b(n4147);
    let n5351: ZW = zw_mix1(n5303, n5350, 38u64);
    let n5352: ZW = zw_mix2(n5304, n5350, 38u64);
    let n5353: ZW = zw_bits_n(n4152);
    let n5354: ZW = zw_mix1(n5351, n5353, 39u64);
    let n5355: ZW = zw_mix2(n5352, n5353, 39u64);
    let n5356: ZW = zw_bits_n(n4151);
    let n5357: ZW = zw_mix1(n5354, n5356, 87u64);
    let n5358: ZW = zw_mix2(n5355, n5356, 87u64);
    let n5359: ZW = zw_mix1(n5255, n5297, 20u64);
    let n5360: ZW = zw_mix2(n5256, n5297, 20u64);
    let n5361: ZW = zw_bits_b(n4175);
    let n5362: ZW = zw_mix1(n5359, n5361, 38u64);
    let n5363: ZW = zw_mix2(n5360, n5361, 38u64);
    let n5364: ZW = zw_bits_n(n4182);
    let n5365: ZW = zw_mix1(n5362, n5364, 39u64);
    let n5366: ZW = zw_mix2(n5363, n5364, 39u64);
    let n5367: ZW = zw_bits_n(n4181);
    let n5368: ZW = zw_mix1(n5365, n5367, 87u64);
    let n5369: ZW = zw_mix2(n5366, n5367, 87u64);
    let n5370: ZW = zw_bits_b(n4193);
    let n5371: ZW = zw_mix1(n5359, n5370, 38u64);
    let n5372: ZW = zw_mix2(n5360, n5370, 38u64);
    let n5373: ZW = zw_bits_n(n4200);
    let n5374: ZW = zw_mix1(n5371, n5373, 39u64);
    let n5375: ZW = zw_mix2(n5372, n5373, 39u64);
    let n5376: ZW = zw_bits_n(n4199);
    let n5377: ZW = zw_mix1(n5374, n5376, 87u64);
    let n5378: ZW = zw_mix2(n5375, n5376, 87u64);
    let n5379: ZW = zw_bits_b(n4211);
    let n5380: ZW = zw_mix1(n5359, n5379, 38u64);
    let n5381: ZW = zw_mix2(n5360, n5379, 38u64);
    let n5382: ZW = zw_bits_n(n4218);
    let n5383: ZW = zw_mix1(n5380, n5382, 39u64);
    let n5384: ZW = zw_mix2(n5381, n5382, 39u64);
    let n5385: ZW = zw_bits_n(n4217);
    let n5386: ZW = zw_mix1(n5383, n5385, 87u64);
    let n5387: ZW = zw_mix2(n5384, n5385, 87u64);
    let n5388: ZW = zw_bits_b(n4227);
    let n5389: ZW = zw_mix1(n5359, n5388, 38u64);
    let n5390: ZW = zw_mix2(n5360, n5388, 38u64);
    let n5391: ZW = zw_bits_n(n4234);
    let n5392: ZW = zw_mix1(n5389, n5391, 39u64);
    let n5393: ZW = zw_mix2(n5390, n5391, 39u64);
    let n5394: ZW = zw_bits_n(n4233);
    let n5395: ZW = zw_mix1(n5392, n5394, 87u64);
    let n5396: ZW = zw_mix2(n5393, n5394, 87u64);
    let n5397: ZW = zw_bits_b(n4257);
    let n5398: ZW = zw_mix1(n5359, n5397, 38u64);
    let n5399: ZW = zw_mix2(n5360, n5397, 38u64);
    let n5400: ZW = zw_bits_n(n4264);
    let n5401: ZW = zw_mix1(n5398, n5400, 39u64);
    let n5402: ZW = zw_mix2(n5399, n5400, 39u64);
    let n5403: ZW = zw_bits_n(n4263);
    let n5404: ZW = zw_mix1(n5401, n5403, 87u64);
    let n5405: ZW = zw_mix2(n5402, n5403, 87u64);
    let n5406: ZW = zw_bits_b(n4275);
    let n5407: ZW = zw_mix1(n5359, n5406, 38u64);
    let n5408: ZW = zw_mix2(n5360, n5406, 38u64);
    let n5409: ZW = zw_bits_n(n4282);
    let n5410: ZW = zw_mix1(n5407, n5409, 39u64);
    let n5411: ZW = zw_mix2(n5408, n5409, 39u64);
    let n5412: ZW = zw_bits_n(n4281);
    let n5413: ZW = zw_mix1(n5410, n5412, 87u64);
    let n5414: ZW = zw_mix2(n5411, n5412, 87u64);
    let n5415: ZW = zw_bits_b(n4293);
    let n5416: ZW = zw_mix1(n5359, n5415, 38u64);
    let n5417: ZW = zw_mix2(n5360, n5415, 38u64);
    let n5418: ZW = zw_bits_n(n4300);
    let n5419: ZW = zw_mix1(n5416, n5418, 39u64);
    let n5420: ZW = zw_mix2(n5417, n5418, 39u64);
    let n5421: ZW = zw_bits_n(n4299);
    let n5422: ZW = zw_mix1(n5419, n5421, 87u64);
    let n5423: ZW = zw_mix2(n5420, n5421, 87u64);
    let n5424: ZW = zw_bits_b(n4309);
    let n5425: ZW = zw_mix1(n5359, n5424, 38u64);
    let n5426: ZW = zw_mix2(n5360, n5424, 38u64);
    let n5427: ZW = zw_bits_n(n4316);
    let n5428: ZW = zw_mix1(n5425, n5427, 39u64);
    let n5429: ZW = zw_mix2(n5426, n5427, 39u64);
    let n5430: ZW = zw_bits_n(n4315);
    let n5431: ZW = zw_mix1(n5428, n5430, 87u64);
    let n5432: ZW = zw_mix2(n5429, n5430, 87u64);
    let n5433: ZW = zw_bits_b(n4320);
    let n5434: ZW = zw_mix1(n5303, n5433, 38u64);
    let n5435: ZW = zw_mix2(n5304, n5433, 38u64);
    let n5436: ZW = zw_bits_n(n4325);
    let n5437: ZW = zw_mix1(n5434, n5436, 39u64);
    let n5438: ZW = zw_mix2(n5435, n5436, 39u64);
    let n5439: ZW = zw_bits_n(n4324);
    let n5440: ZW = zw_mix1(n5437, n5439, 87u64);
    let n5441: ZW = zw_mix2(n5438, n5439, 87u64);
    let n5442: ZW = zw_bits_b(n4329);
    let n5443: ZW = zw_mix1(n5303, n5442, 38u64);
    let n5444: ZW = zw_mix2(n5304, n5442, 38u64);
    let n5445: ZW = zw_bits_n(n4334);
    let n5446: ZW = zw_mix1(n5443, n5445, 39u64);
    let n5447: ZW = zw_mix2(n5444, n5445, 39u64);
    let n5448: ZW = zw_bits_n(n4333);
    let n5449: ZW = zw_mix1(n5446, n5448, 87u64);
    let n5450: ZW = zw_mix2(n5447, n5448, 87u64);
    let n5451: ZW = zw_bits_b(n4338);
    let n5452: ZW = zw_mix1(n5303, n5451, 38u64);
    let n5453: ZW = zw_mix2(n5304, n5451, 38u64);
    let n5454: ZW = zw_bits_n(n4343);
    let n5455: ZW = zw_mix1(n5452, n5454, 39u64);
    let n5456: ZW = zw_mix2(n5453, n5454, 39u64);
    let n5457: ZW = zw_bits_n(n4342);
    let n5458: ZW = zw_mix1(n5455, n5457, 87u64);
    let n5459: ZW = zw_mix2(n5456, n5457, 87u64);
    let n5460: ZW = zw_bits_b(n4347);
    let n5461: ZW = zw_mix1(n5303, n5460, 38u64);
    let n5462: ZW = zw_mix2(n5304, n5460, 38u64);
    let n5463: ZW = zw_bits_n(n4352);
    let n5464: ZW = zw_mix1(n5461, n5463, 39u64);
    let n5465: ZW = zw_mix2(n5462, n5463, 39u64);
    let n5466: ZW = zw_bits_n(n4351);
    let n5467: ZW = zw_mix1(n5464, n5466, 87u64);
    let n5468: ZW = zw_mix2(n5465, n5466, 87u64);
    let n5469: ZW = zw_bits_b(n4356);
    let n5470: ZW = zw_mix1(n5303, n5469, 38u64);
    let n5471: ZW = zw_mix2(n5304, n5469, 38u64);
    let n5472: ZW = zw_bits_n(n4361);
    let n5473: ZW = zw_mix1(n5470, n5472, 39u64);
    let n5474: ZW = zw_mix2(n5471, n5472, 39u64);
    let n5475: ZW = zw_bits_n(n4360);
    let n5476: ZW = zw_mix1(n5473, n5475, 87u64);
    let n5477: ZW = zw_mix2(n5474, n5475, 87u64);
    let n5478: ZW = zw_bits_b(n4365);
    let n5479: ZW = zw_mix1(n5303, n5478, 38u64);
    let n5480: ZW = zw_mix2(n5304, n5478, 38u64);
    let n5481: ZW = zw_bits_n(n4370);
    let n5482: ZW = zw_mix1(n5479, n5481, 39u64);
    let n5483: ZW = zw_mix2(n5480, n5481, 39u64);
    let n5484: ZW = zw_bits_n(n4369);
    let n5485: ZW = zw_mix1(n5482, n5484, 87u64);
    let n5486: ZW = zw_mix2(n5483, n5484, 87u64);
    let n5487: ZW = zw_mix1(n5255, n4985, 20u64);
    let n5488: ZW = zw_mix2(n5256, n4985, 20u64);
    let n5489: ZW = zw_bits_b(n4374);
    let n5490: ZW = zw_mix1(n5487, n5489, 38u64);
    let n5491: ZW = zw_mix2(n5488, n5489, 38u64);
    let n5492: ZW = zw_bits_n(n4381);
    let n5493: ZW = zw_mix1(n5490, n5492, 39u64);
    let n5494: ZW = zw_mix2(n5491, n5492, 39u64);
    let n5495: ZW = zw_bits_n(n4380);
    let n5496: ZW = zw_mix1(n5493, n5495, 87u64);
    let n5497: ZW = zw_mix2(n5494, n5495, 87u64);
    let n5498: ZW = zw_bits_b(n4385);
    let n5499: ZW = zw_mix1(n5487, n5498, 38u64);
    let n5500: ZW = zw_mix2(n5488, n5498, 38u64);
    let n5501: ZW = zw_bits_n(n4392);
    let n5502: ZW = zw_mix1(n5499, n5501, 39u64);
    let n5503: ZW = zw_mix2(n5500, n5501, 39u64);
    let n5504: ZW = zw_bits_n(n4391);
    let n5505: ZW = zw_mix1(n5502, n5504, 87u64);
    let n5506: ZW = zw_mix2(n5503, n5504, 87u64);
    let n5507: ZW = zw_bits_b(n4396);
    let n5508: ZW = zw_mix1(n5487, n5507, 38u64);
    let n5509: ZW = zw_mix2(n5488, n5507, 38u64);
    let n5510: ZW = zw_bits_n(n4403);
    let n5511: ZW = zw_mix1(n5508, n5510, 39u64);
    let n5512: ZW = zw_mix2(n5509, n5510, 39u64);
    let n5513: ZW = zw_bits_n(n4402);
    let n5514: ZW = zw_mix1(n5511, n5513, 87u64);
    let n5515: ZW = zw_mix2(n5512, n5513, 87u64);
    let n5516: ZW = zw_bits_b(n4407);
    let n5517: ZW = zw_mix1(n5487, n5516, 38u64);
    let n5518: ZW = zw_mix2(n5488, n5516, 38u64);
    let n5519: ZW = zw_bits_n(n4414);
    let n5520: ZW = zw_mix1(n5517, n5519, 39u64);
    let n5521: ZW = zw_mix2(n5518, n5519, 39u64);
    let n5522: ZW = zw_bits_n(n4413);
    let n5523: ZW = zw_mix1(n5520, n5522, 87u64);
    let n5524: ZW = zw_mix2(n5521, n5522, 87u64);
    let n5525: ZW = zw_bits_b(n4418);
    let n5526: ZW = zw_mix1(n5487, n5525, 38u64);
    let n5527: ZW = zw_mix2(n5488, n5525, 38u64);
    let n5528: ZW = zw_bits_n(n4425);
    let n5529: ZW = zw_mix1(n5526, n5528, 39u64);
    let n5530: ZW = zw_mix2(n5527, n5528, 39u64);
    let n5531: ZW = zw_bits_n(n4424);
    let n5532: ZW = zw_mix1(n5529, n5531, 87u64);
    let n5533: ZW = zw_mix2(n5530, n5531, 87u64);
    let n5534: ZW = zw_bits_b(n4429);
    let n5535: ZW = zw_mix1(n5487, n5534, 38u64);
    let n5536: ZW = zw_mix2(n5488, n5534, 38u64);
    let n5537: ZW = zw_bits_n(n4436);
    let n5538: ZW = zw_mix1(n5535, n5537, 39u64);
    let n5539: ZW = zw_mix2(n5536, n5537, 39u64);
    let n5540: ZW = zw_bits_n(n4435);
    let n5541: ZW = zw_mix1(n5538, n5540, 87u64);
    let n5542: ZW = zw_mix2(n5539, n5540, 87u64);
    let n5543: ZW = zw_bits_b(n4440);
    let n5544: ZW = zw_mix1(n5487, n5543, 38u64);
    let n5545: ZW = zw_mix2(n5488, n5543, 38u64);
    let n5546: ZW = zw_bits_n(n4447);
    let n5547: ZW = zw_mix1(n5544, n5546, 39u64);
    let n5548: ZW = zw_mix2(n5545, n5546, 39u64);
    let n5549: ZW = zw_bits_n(n4446);
    let n5550: ZW = zw_mix1(n5547, n5549, 87u64);
    let n5551: ZW = zw_mix2(n5548, n5549, 87u64);
    let n5552: ZW = zw_bits_b(n4451);
    let n5553: ZW = zw_mix1(n5487, n5552, 38u64);
    let n5554: ZW = zw_mix2(n5488, n5552, 38u64);
    let n5555: ZW = zw_bits_n(n4458);
    let n5556: ZW = zw_mix1(n5553, n5555, 39u64);
    let n5557: ZW = zw_mix2(n5554, n5555, 39u64);
    let n5558: ZW = zw_bits_n(n4457);
    let n5559: ZW = zw_mix1(n5556, n5558, 87u64);
    let n5560: ZW = zw_mix2(n5557, n5558, 87u64);
    let n5561: ZW = zw_bits_n(n4461);
    let n5562: ZW = zw_mix1(n4855, n5561, 246u64);
    let n5563: ZW = zw_mix2(n4856, n5561, 246u64);
    let n5564: ZW = zw_bits_n(n4462);
    let n5565: ZW = zw_mix1(n5562, n5564, 254u64);
    let n5566: ZW = zw_mix2(n5563, n5564, 254u64);
    let n5567: ZW = zw_bits_n(n4468);
    let n5568: ZW = zw_mix1(n5565, n5567, 258u64);
    let n5569: ZW = zw_mix2(n5566, n5567, 258u64);
    let n5570: ZW = zw_bits_n(n4469);
    let n5571: ZW = zw_mix1(n5568, n5570, 271u64);
    let n5572: ZW = zw_mix2(n5569, n5570, 271u64);
    let n5573: ZW = zw_bits_n(n4470);
    let n5574: ZW = zw_mix1(n5571, n5573, 278u64);
    let n5575: ZW = zw_mix2(n5572, n5573, 278u64);
    let n5576: ZW = zw_bits_n(n4471);
    let n5577: ZW = zw_mix1(n5574, n5576, 291u64);
    let n5578: ZW = zw_mix2(n5575, n5576, 291u64);
    let n5579: ZW = zw_bits_n(n4477);
    let n5580: ZW = zw_mix1(n5577, n5579, 319u64);
    let n5581: ZW = zw_mix2(n5578, n5579, 319u64);
    let n5582: ZW = zw_bits_n(n4479);
    let n5583: ZW = zw_mix1(n5580, n5582, 406u64);
    let n5584: ZW = zw_mix2(n5581, n5582, 406u64);
    let n5585: ZW = zw_bits_n(n4480);
    let n5586: ZW = zw_mix1(n5583, n5585, 407u64);
    let n5587: ZW = zw_mix2(n5584, n5585, 407u64);
    let n5588: ZW = zw_bits_n(n4467);
    let n5589: ZW = zw_mix1(n5586, n5588, 20u64);
    let n5590: ZW = zw_mix2(n5587, n5588, 20u64);
    let n5591: ZW = zw_mix1(n5589, n4881, 41u64);
    let n5592: ZW = zw_mix2(n5590, n4881, 41u64);
    let n5593: ZW = zw_bits_n(n4472);
    let n5594: ZW = zw_mix1(n5591, n5593, 299u64);
    let n5595: ZW = zw_mix2(n5592, n5593, 299u64);
    let n5596: ZW = zw_bits_n(n4473);
    let n5597: ZW = zw_mix1(n5594, n5596, 301u64);
    let n5598: ZW = zw_mix2(n5595, n5596, 301u64);
    let n5599: ZW = zw_bits_n(n4474);
    let n5600: ZW = zw_mix1(n5597, n5599, 302u64);
    let n5601: ZW = zw_mix2(n5598, n5599, 302u64);
    let n5602: ZW = zw_bits_n(n4475);
    let n5603: ZW = zw_mix1(n5600, n5602, 304u64);
    let n5604: ZW = zw_mix2(n5601, n5602, 304u64);
    let n5605: ZW = zw_bits_b(n4463);
    let n5606: ZW = zw_mix1(n5603, n5605, 311u64);
    let n5607: ZW = zw_mix2(n5604, n5605, 311u64);
    let n5608: ZW = zw_bits_b(n4464);
    let n5609: ZW = zw_mix1(n5606, n5608, 312u64);
    let n5610: ZW = zw_mix2(n5607, n5608, 312u64);
    let n5611: ZW = zw_bits_n(n4504);
    let n5612: ZW = zw_mix1(n5609, n5611, 318u64);
    let n5613: ZW = zw_mix2(n5610, n5611, 318u64);
    let n5614: ZW = zw_mix1(n5612, n4904, 396u64);
    let n5615: ZW = zw_mix2(n5613, n4904, 396u64);
    let n5616: ZW = zw_mix1(n5614, n4907, 397u64);
    let n5617: ZW = zw_mix2(n5615, n4907, 397u64);
    let n5618: ZW = zw_mix1(n5616, n4910, 398u64);
    let n5619: ZW = zw_mix2(n5617, n4910, 398u64);
    let n5620: ZW = zw_mix1(n5618, n4913, 399u64);
    let n5621: ZW = zw_mix2(n5619, n4913, 399u64);
    let n5622: ZW = zw_bits_b(n4478);
    let n5623: ZW = zw_mix1(n5620, n5622, 400u64);
    let n5624: ZW = zw_mix2(n5621, n5622, 400u64);
    let n5625: ZW = zw_bits_n(n4505);
    let n5626: ZW = zw_mix1(n5623, n5625, 408u64);
    let n5627: ZW = zw_mix2(n5624, n5625, 408u64);
    let n5628: ZW = zw_bits_n(n4482);
    let n5629: ZW = zw_mix1(n5626, n5628, 409u64);
    let n5630: ZW = zw_mix2(n5627, n5628, 409u64);
    let n5631: ZW = zw_bits_b(n4509);
    let n5632: ZW = zw_mix1(n5620, n5631, 400u64);
    let n5633: ZW = zw_mix2(n5621, n5631, 400u64);
    let n5634: ZW = zw_bits_n(n4522);
    let n5635: ZW = zw_mix1(n5632, n5634, 408u64);
    let n5636: ZW = zw_mix2(n5633, n5634, 408u64);
    let n5637: ZW = zw_bits_n(n4511);
    let n5638: ZW = zw_mix1(n5635, n5637, 409u64);
    let n5639: ZW = zw_mix2(n5636, n5637, 409u64);
    let n5640: ZW = zw_bits_b(n4525);
    let n5641: ZW = zw_mix1(n5620, n5640, 400u64);
    let n5642: ZW = zw_mix2(n5621, n5640, 400u64);
    let n5643: ZW = zw_bits_n(n4538);
    let n5644: ZW = zw_mix1(n5641, n5643, 408u64);
    let n5645: ZW = zw_mix2(n5642, n5643, 408u64);
    let n5646: ZW = zw_bits_n(n4527);
    let n5647: ZW = zw_mix1(n5644, n5646, 409u64);
    let n5648: ZW = zw_mix2(n5645, n5646, 409u64);
    let n5649: ZW = zw_bits_n(n4542);
    let n5650: ZW = zw_mix1(n5600, n5649, 304u64);
    let n5651: ZW = zw_mix2(n5601, n5649, 304u64);
    let n5652: ZW = zw_mix1(n5650, n5605, 311u64);
    let n5653: ZW = zw_mix2(n5651, n5605, 311u64);
    let n5654: ZW = zw_bits_b(n4540);
    let n5655: ZW = zw_mix1(n5652, n5654, 312u64);
    let n5656: ZW = zw_mix2(n5653, n5654, 312u64);
    let n5657: ZW = zw_mix1(n5655, n5611, 318u64);
    let n5658: ZW = zw_mix2(n5656, n5611, 318u64);
    let n5659: ZW = zw_mix1(n5657, n4904, 396u64);
    let n5660: ZW = zw_mix2(n5658, n4904, 396u64);
    let n5661: ZW = zw_mix1(n5659, n4907, 397u64);
    let n5662: ZW = zw_mix2(n5660, n4907, 397u64);
    let n5663: ZW = zw_mix1(n5661, n4910, 398u64);
    let n5664: ZW = zw_mix2(n5662, n4910, 398u64);
    let n5665: ZW = zw_mix1(n5663, n4913, 399u64);
    let n5666: ZW = zw_mix2(n5664, n4913, 399u64);
    let n5667: ZW = zw_mix1(n5665, n5622, 400u64);
    let n5668: ZW = zw_mix2(n5666, n5622, 400u64);
    let n5669: ZW = zw_bits_n(n4555);
    let n5670: ZW = zw_mix1(n5667, n5669, 408u64);
    let n5671: ZW = zw_mix2(n5668, n5669, 408u64);
    let n5672: ZW = zw_bits_n(n4544);
    let n5673: ZW = zw_mix1(n5670, n5672, 409u64);
    let n5674: ZW = zw_mix2(n5671, n5672, 409u64);
    let n5675: ZW = zw_mix1(n5665, n5631, 400u64);
    let n5676: ZW = zw_mix2(n5666, n5631, 400u64);
    let n5677: ZW = zw_bits_n(n4570);
    let n5678: ZW = zw_mix1(n5675, n5677, 408u64);
    let n5679: ZW = zw_mix2(n5676, n5677, 408u64);
    let n5680: ZW = zw_bits_n(n4559);
    let n5681: ZW = zw_mix1(n5678, n5680, 409u64);
    let n5682: ZW = zw_mix2(n5679, n5680, 409u64);
    let n5683: ZW = zw_mix1(n5665, n5640, 400u64);
    let n5684: ZW = zw_mix2(n5666, n5640, 400u64);
    let n5685: ZW = zw_bits_n(n4585);
    let n5686: ZW = zw_mix1(n5683, n5685, 408u64);
    let n5687: ZW = zw_mix2(n5684, n5685, 408u64);
    let n5688: ZW = zw_bits_n(n4574);
    let n5689: ZW = zw_mix1(n5686, n5688, 409u64);
    let n5690: ZW = zw_mix2(n5687, n5688, 409u64);
    let n5691: ZW = zw_bits_n(n4607);
    let n5692: ZW = zw_mix1(n5586, n5691, 20u64);
    let n5693: ZW = zw_mix2(n5587, n5691, 20u64);
    let n5694: ZW = zw_bits_b(n4608);
    let n5695: ZW = zw_mix1(n5692, n5694, 41u64);
    let n5696: ZW = zw_mix2(n5693, n5694, 41u64);
    let n5697: ZW = zw_bits_n(n4609);
    let n5698: ZW = zw_mix1(n5695, n5697, 299u64);
    let n5699: ZW = zw_mix2(n5696, n5697, 299u64);
    let n5700: ZW = zw_bits_n(n4610);
    let n5701: ZW = zw_mix1(n5698, n5700, 301u64);
    let n5702: ZW = zw_mix2(n5699, n5700, 301u64);
    let n5703: ZW = zw_bits_n(n4611);
    let n5704: ZW = zw_mix1(n5701, n5703, 302u64);
    let n5705: ZW = zw_mix2(n5702, n5703, 302u64);
    let n5706: ZW = zw_mix1(n5704, n5602, 304u64);
    let n5707: ZW = zw_mix2(n5705, n5602, 304u64);
    let n5708: ZW = zw_bits_b(n4587);
    let n5709: ZW = zw_mix1(n5706, n5708, 311u64);
    let n5710: ZW = zw_mix2(n5707, n5708, 311u64);
    let n5711: ZW = zw_mix1(n5709, n5608, 312u64);
    let n5712: ZW = zw_mix2(n5710, n5608, 312u64);
    let n5713: ZW = zw_bits_n(n4630);
    let n5714: ZW = zw_mix1(n5711, n5713, 318u64);
    let n5715: ZW = zw_mix2(n5712, n5713, 318u64);
    let n5716: ZW = zw_bits_n(n4612);
    let n5717: ZW = zw_mix1(n5714, n5716, 396u64);
    let n5718: ZW = zw_mix2(n5715, n5716, 396u64);
    let n5719: ZW = zw_bits_n(n4613);
    let n5720: ZW = zw_mix1(n5717, n5719, 397u64);
    let n5721: ZW = zw_mix2(n5718, n5719, 397u64);
    let n5722: ZW = zw_bits_n(n4614);
    let n5723: ZW = zw_mix1(n5720, n5722, 398u64);
    let n5724: ZW = zw_mix2(n5721, n5722, 398u64);
    let n5725: ZW = zw_bits_n(n4615);
    let n5726: ZW = zw_mix1(n5723, n5725, 399u64);
    let n5727: ZW = zw_mix2(n5724, n5725, 399u64);
    let n5728: ZW = zw_mix1(n5726, n5622, 400u64);
    let n5729: ZW = zw_mix2(n5727, n5622, 400u64);
    let n5730: ZW = zw_bits_n(n4631);
    let n5731: ZW = zw_mix1(n5728, n5730, 408u64);
    let n5732: ZW = zw_mix2(n5729, n5730, 408u64);
    let n5733: ZW = zw_bits_n(n4617);
    let n5734: ZW = zw_mix1(n5731, n5733, 409u64);
    let n5735: ZW = zw_mix2(n5732, n5733, 409u64);
    let n5736: ZW = zw_bits_n(n4642);
    let n5737: ZW = zw_mix1(n5717, n5736, 397u64);
    let n5738: ZW = zw_mix2(n5718, n5736, 397u64);
    let n5739: ZW = zw_bits_n(n4643);
    let n5740: ZW = zw_mix1(n5737, n5739, 398u64);
    let n5741: ZW = zw_mix2(n5738, n5739, 398u64);
    let n5742: ZW = zw_mix1(n5740, n5725, 399u64);
    let n5743: ZW = zw_mix2(n5741, n5725, 399u64);
    let n5744: ZW = zw_mix1(n5742, n5631, 400u64);
    let n5745: ZW = zw_mix2(n5743, n5631, 400u64);
    let n5746: ZW = zw_bits_n(n4656);
    let n5747: ZW = zw_mix1(n5744, n5746, 408u64);
    let n5748: ZW = zw_mix2(n5745, n5746, 408u64);
    let n5749: ZW = zw_bits_n(n4645);
    let n5750: ZW = zw_mix1(n5747, n5749, 409u64);
    let n5751: ZW = zw_mix2(n5748, n5749, 409u64);
    let n5752: ZW = zw_bits_n(n4665);
    let n5753: ZW = zw_mix1(n5737, n5752, 398u64);
    let n5754: ZW = zw_mix2(n5738, n5752, 398u64);
    let n5755: ZW = zw_mix1(n5753, n5725, 399u64);
    let n5756: ZW = zw_mix2(n5754, n5725, 399u64);
    let n5757: ZW = zw_mix1(n5755, n5640, 400u64);
    let n5758: ZW = zw_mix2(n5756, n5640, 400u64);
    let n5759: ZW = zw_bits_n(n4678);
    let n5760: ZW = zw_mix1(n5757, n5759, 408u64);
    let n5761: ZW = zw_mix2(n5758, n5759, 408u64);
    let n5762: ZW = zw_bits_n(n4667);
    let n5763: ZW = zw_mix1(n5760, n5762, 409u64);
    let n5764: ZW = zw_mix2(n5761, n5762, 409u64);
    let n5765: ZW = zw_bits_n(n4693);
    let n5766: ZW = zw_mix1(n5714, n5765, 396u64);
    let n5767: ZW = zw_mix2(n5715, n5765, 396u64);
    let n5768: ZW = zw_bits_n(n4694);
    let n5769: ZW = zw_mix1(n5766, n5768, 397u64);
    let n5770: ZW = zw_mix2(n5767, n5768, 397u64);
    let n5771: ZW = zw_bits_n(n4695);
    let n5772: ZW = zw_mix1(n5769, n5771, 398u64);
    let n5773: ZW = zw_mix2(n5770, n5771, 398u64);
    let n5774: ZW = zw_bits_n(n4696);
    let n5775: ZW = zw_mix1(n5772, n5774, 399u64);
    let n5776: ZW = zw_mix2(n5773, n5774, 399u64);
    let n5777: ZW = zw_mix1(n5775, n5622, 400u64);
    let n5778: ZW = zw_mix2(n5776, n5622, 400u64);
    let n5779: ZW = zw_bits_n(n4709);
    let n5780: ZW = zw_mix1(n5777, n5779, 408u64);
    let n5781: ZW = zw_mix2(n5778, n5779, 408u64);
    let n5782: ZW = zw_bits_n(n4698);
    let n5783: ZW = zw_mix1(n5780, n5782, 409u64);
    let n5784: ZW = zw_mix2(n5781, n5782, 409u64);
    let n5785: ZW = zw_mix1(n5766, n5736, 397u64);
    let n5786: ZW = zw_mix2(n5767, n5736, 397u64);
    let n5787: ZW = zw_mix1(n5785, n5739, 398u64);
    let n5788: ZW = zw_mix2(n5786, n5739, 398u64);
    let n5789: ZW = zw_mix1(n5787, n5774, 399u64);
    let n5790: ZW = zw_mix2(n5788, n5774, 399u64);
    let n5791: ZW = zw_mix1(n5789, n5631, 400u64);
    let n5792: ZW = zw_mix2(n5790, n5631, 400u64);
    let n5793: ZW = zw_bits_n(n4718);
    let n5794: ZW = zw_mix1(n5791, n5793, 408u64);
    let n5795: ZW = zw_mix2(n5792, n5793, 408u64);
    let n5796: ZW = zw_bits_n(n4716);
    let n5797: ZW = zw_mix1(n5794, n5796, 409u64);
    let n5798: ZW = zw_mix2(n5795, n5796, 409u64);
    let n5799: ZW = zw_mix1(n5785, n5752, 398u64);
    let n5800: ZW = zw_mix2(n5786, n5752, 398u64);
    let n5801: ZW = zw_mix1(n5799, n5774, 399u64);
    let n5802: ZW = zw_mix2(n5800, n5774, 399u64);
    let n5803: ZW = zw_mix1(n5801, n5640, 400u64);
    let n5804: ZW = zw_mix2(n5802, n5640, 400u64);
    let n5805: ZW = zw_bits_n(n4726);
    let n5806: ZW = zw_mix1(n5803, n5805, 408u64);
    let n5807: ZW = zw_mix2(n5804, n5805, 408u64);
    let n5808: ZW = zw_bits_n(n4724);
    let n5809: ZW = zw_mix1(n5806, n5808, 409u64);
    let n5810: ZW = zw_mix2(n5807, n5808, 409u64);
    let n5811: ZW = zw_bits_n(n4731);
    let n5812: ZW = zw_mix1(n5772, n5811, 399u64);
    let n5813: ZW = zw_mix2(n5773, n5811, 399u64);
    let n5814: ZW = zw_mix1(n5812, n5622, 400u64);
    let n5815: ZW = zw_mix2(n5813, n5622, 400u64);
    let n5816: ZW = zw_mix1(n5814, n5779, 408u64);
    let n5817: ZW = zw_mix2(n5815, n5779, 408u64);
    let n5818: ZW = zw_bits_n(n4732);
    let n5819: ZW = zw_mix1(n5816, n5818, 409u64);
    let n5820: ZW = zw_mix2(n5817, n5818, 409u64);
    let n5821: ZW = zw_mix1(n5787, n5811, 399u64);
    let n5822: ZW = zw_mix2(n5788, n5811, 399u64);
    let n5823: ZW = zw_mix1(n5821, n5631, 400u64);
    let n5824: ZW = zw_mix2(n5822, n5631, 400u64);
    let n5825: ZW = zw_mix1(n5823, n5793, 408u64);
    let n5826: ZW = zw_mix2(n5824, n5793, 408u64);
    let n5827: ZW = zw_bits_n(n4735);
    let n5828: ZW = zw_mix1(n5825, n5827, 409u64);
    let n5829: ZW = zw_mix2(n5826, n5827, 409u64);
    let n5830: ZW = zw_mix1(n5799, n5811, 399u64);
    let n5831: ZW = zw_mix2(n5800, n5811, 399u64);
    let n5832: ZW = zw_mix1(n5830, n5640, 400u64);
    let n5833: ZW = zw_mix2(n5831, n5640, 400u64);
    let n5834: ZW = zw_mix1(n5832, n5805, 408u64);
    let n5835: ZW = zw_mix2(n5833, n5805, 408u64);
    let n5836: ZW = zw_bits_n(n4738);
    let n5837: ZW = zw_mix1(n5834, n5836, 409u64);
    let n5838: ZW = zw_mix2(n5835, n5836, 409u64);
    let n5839: ZW = zw_mix1(n5704, n5649, 304u64);
    let n5840: ZW = zw_mix2(n5705, n5649, 304u64);
    let n5841: ZW = zw_mix1(n5839, n5708, 311u64);
    let n5842: ZW = zw_mix2(n5840, n5708, 311u64);
    let n5843: ZW = zw_mix1(n5841, n5654, 312u64);
    let n5844: ZW = zw_mix2(n5842, n5654, 312u64);
    let n5845: ZW = zw_mix1(n5843, n5713, 318u64);
    let n5846: ZW = zw_mix2(n5844, n5713, 318u64);
    let n5847: ZW = zw_mix1(n5845, n5716, 396u64);
    let n5848: ZW = zw_mix2(n5846, n5716, 396u64);
    let n5849: ZW = zw_mix1(n5847, n5719, 397u64);
    let n5850: ZW = zw_mix2(n5848, n5719, 397u64);
    let n5851: ZW = zw_mix1(n5849, n5722, 398u64);
    let n5852: ZW = zw_mix2(n5850, n5722, 398u64);
    let n5853: ZW = zw_mix1(n5851, n5725, 399u64);
    let n5854: ZW = zw_mix2(n5852, n5725, 399u64);
    let n5855: ZW = zw_mix1(n5853, n5622, 400u64);
    let n5856: ZW = zw_mix2(n5854, n5622, 400u64);
    let n5857: ZW = zw_bits_n(n4756);
    let n5858: ZW = zw_mix1(n5855, n5857, 408u64);
    let n5859: ZW = zw_mix2(n5856, n5857, 408u64);
    let n5860: ZW = zw_bits_n(n4745);
    let n5861: ZW = zw_mix1(n5858, n5860, 409u64);
    let n5862: ZW = zw_mix2(n5859, n5860, 409u64);
    let n5863: ZW = zw_mix1(n5847, n5736, 397u64);
    let n5864: ZW = zw_mix2(n5848, n5736, 397u64);
    let n5865: ZW = zw_mix1(n5863, n5739, 398u64);
    let n5866: ZW = zw_mix2(n5864, n5739, 398u64);
    let n5867: ZW = zw_mix1(n5865, n5725, 399u64);
    let n5868: ZW = zw_mix2(n5866, n5725, 399u64);
    let n5869: ZW = zw_mix1(n5867, n5631, 400u64);
    let n5870: ZW = zw_mix2(n5868, n5631, 400u64);
    let n5871: ZW = zw_bits_n(n4775);
    let n5872: ZW = zw_mix1(n5869, n5871, 408u64);
    let n5873: ZW = zw_mix2(n5870, n5871, 408u64);
    let n5874: ZW = zw_bits_n(n4764);
    let n5875: ZW = zw_mix1(n5872, n5874, 409u64);
    let n5876: ZW = zw_mix2(n5873, n5874, 409u64);
    let n5877: ZW = zw_mix1(n5863, n5752, 398u64);
    let n5878: ZW = zw_mix2(n5864, n5752, 398u64);
    let n5879: ZW = zw_mix1(n5877, n5725, 399u64);
    let n5880: ZW = zw_mix2(n5878, n5725, 399u64);
    let n5881: ZW = zw_mix1(n5879, n5640, 400u64);
    let n5882: ZW = zw_mix2(n5880, n5640, 400u64);
    let n5883: ZW = zw_bits_n(n4794);
    let n5884: ZW = zw_mix1(n5881, n5883, 408u64);
    let n5885: ZW = zw_mix2(n5882, n5883, 408u64);
    let n5886: ZW = zw_bits_n(n4783);
    let n5887: ZW = zw_mix1(n5884, n5886, 409u64);
    let n5888: ZW = zw_mix2(n5885, n5886, 409u64);
    let n5889: ZW = zw_mix1(n5845, n5765, 396u64);
    let n5890: ZW = zw_mix2(n5846, n5765, 396u64);
    let n5891: ZW = zw_mix1(n5889, n5768, 397u64);
    let n5892: ZW = zw_mix2(n5890, n5768, 397u64);
    let n5893: ZW = zw_mix1(n5891, n5771, 398u64);
    let n5894: ZW = zw_mix2(n5892, n5771, 398u64);
    let n5895: ZW = zw_mix1(n5893, n5774, 399u64);
    let n5896: ZW = zw_mix2(n5894, n5774, 399u64);
    let n5897: ZW = zw_mix1(n5895, n5622, 400u64);
    let n5898: ZW = zw_mix2(n5896, n5622, 400u64);
    let n5899: ZW = zw_bits_n(n4813);
    let n5900: ZW = zw_mix1(n5897, n5899, 408u64);
    let n5901: ZW = zw_mix2(n5898, n5899, 408u64);
    let n5902: ZW = zw_bits_n(n4802);
    let n5903: ZW = zw_mix1(n5900, n5902, 409u64);
    let n5904: ZW = zw_mix2(n5901, n5902, 409u64);
    let n5905: ZW = zw_mix1(n5889, n5736, 397u64);
    let n5906: ZW = zw_mix2(n5890, n5736, 397u64);
    let n5907: ZW = zw_mix1(n5905, n5739, 398u64);
    let n5908: ZW = zw_mix2(n5906, n5739, 398u64);
    let n5909: ZW = zw_mix1(n5907, n5774, 399u64);
    let n5910: ZW = zw_mix2(n5908, n5774, 399u64);
    let n5911: ZW = zw_mix1(n5909, n5631, 400u64);
    let n5912: ZW = zw_mix2(n5910, n5631, 400u64);
    let n5913: ZW = zw_bits_n(n4822);
    let n5914: ZW = zw_mix1(n5911, n5913, 408u64);
    let n5915: ZW = zw_mix2(n5912, n5913, 408u64);
    let n5916: ZW = zw_bits_n(n4820);
    let n5917: ZW = zw_mix1(n5914, n5916, 409u64);
    let n5918: ZW = zw_mix2(n5915, n5916, 409u64);
    let n5919: ZW = zw_mix1(n5905, n5752, 398u64);
    let n5920: ZW = zw_mix2(n5906, n5752, 398u64);
    let n5921: ZW = zw_mix1(n5919, n5774, 399u64);
    let n5922: ZW = zw_mix2(n5920, n5774, 399u64);
    let n5923: ZW = zw_mix1(n5921, n5640, 400u64);
    let n5924: ZW = zw_mix2(n5922, n5640, 400u64);
    let n5925: ZW = zw_bits_n(n4830);
    let n5926: ZW = zw_mix1(n5923, n5925, 408u64);
    let n5927: ZW = zw_mix2(n5924, n5925, 408u64);
    let n5928: ZW = zw_bits_n(n4828);
    let n5929: ZW = zw_mix1(n5926, n5928, 409u64);
    let n5930: ZW = zw_mix2(n5927, n5928, 409u64);
    let n5931: ZW = zw_mix1(n5893, n5811, 399u64);
    let n5932: ZW = zw_mix2(n5894, n5811, 399u64);
    let n5933: ZW = zw_mix1(n5931, n5622, 400u64);
    let n5934: ZW = zw_mix2(n5932, n5622, 400u64);
    let n5935: ZW = zw_mix1(n5933, n5899, 408u64);
    let n5936: ZW = zw_mix2(n5934, n5899, 408u64);
    let n5937: ZW = zw_bits_n(n4833);
    let n5938: ZW = zw_mix1(n5935, n5937, 409u64);
    let n5939: ZW = zw_mix2(n5936, n5937, 409u64);
    let n5940: ZW = zw_mix1(n5907, n5811, 399u64);
    let n5941: ZW = zw_mix2(n5908, n5811, 399u64);
    let n5942: ZW = zw_mix1(n5940, n5631, 400u64);
    let n5943: ZW = zw_mix2(n5941, n5631, 400u64);
    let n5944: ZW = zw_mix1(n5942, n5913, 408u64);
    let n5945: ZW = zw_mix2(n5943, n5913, 408u64);
    let n5946: ZW = zw_bits_n(n4836);
    let n5947: ZW = zw_mix1(n5944, n5946, 409u64);
    let n5948: ZW = zw_mix2(n5945, n5946, 409u64);
    let n5949: ZW = zw_mix1(n5919, n5811, 399u64);
    let n5950: ZW = zw_mix2(n5920, n5811, 399u64);
    let n5951: ZW = zw_mix1(n5949, n5640, 400u64);
    let n5952: ZW = zw_mix2(n5950, n5640, 400u64);
    let n5953: ZW = zw_mix1(n5951, n5925, 408u64);
    let n5954: ZW = zw_mix2(n5952, n5925, 408u64);
    let n5955: ZW = zw_bits_n(n4839);
    let n5956: ZW = zw_mix1(n5953, n5955, 409u64);
    let n5957: ZW = zw_mix2(n5954, n5955, 409u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b0: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b0: u16 = ALL & zb_holds(n1768);
    let ok_v1_b1: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b1: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b1: u16 = ALL & zb_holds(n1848);
    let ok_v2_b2: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b2: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b2: u16 = ALL & zb_holds(n1915);
    let ok_v16_b3: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b3: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b3: u16 = ALL & zb_holds(n1962);
    let ok_v17_b4: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b4: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b4: u16 = ALL & zb_holds(n2007);
    let ok_v18_b5: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b5: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b5: u16 = ALL & zb_holds(n2052);
    let ok_v32_b6: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b6: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b6: u16 = ALL & zb_holds(n2109);
    let ok_v33_b7: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b7: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b7: u16 = ALL & zb_holds(n2136);
    let ok_v34_b8: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b8: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b8: u16 = ALL & zb_holds(n2161);
    let ok_v36_b9: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b9: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b9: u16 = ALL & zb_holds(n2191);
    let ok_v37_b10: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b10: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b10: u16 = ALL & zb_holds(n2136);
    let ok_v38_b11: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b11: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b11: u16 = ALL & zb_holds(n2161);
    let ok_v40_b12: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b12: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b12: u16 = ALL & zb_holds(n2191);
    let ok_v41_b13: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b13: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b13: u16 = ALL & zb_holds(n2136);
    let ok_v42_b14: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b14: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b14: u16 = ALL & zb_holds(n2161);
    let ok_v48_b15: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b15: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b15: u16 = ALL & zb_holds(n2246);
    let ok_v49_b16: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b16: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b16: u16 = ALL & zb_holds(n2269);
    let ok_v50_b17: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b17: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b17: u16 = ALL & zb_holds(n2292);
    let ok_v52_b18: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b18: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b18: u16 = ALL & zb_holds(n2313);
    let ok_v53_b19: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b19: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b19: u16 = ALL & zb_holds(n2269);
    let ok_v54_b20: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b20: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b20: u16 = ALL & zb_holds(n2292);
    let ok_v56_b21: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b21: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b21: u16 = ALL & zb_holds(n2313);
    let ok_v57_b22: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b22: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b22: u16 = ALL & zb_holds(n2269);
    let ok_v58_b23: u16 = ALL & zb_holds(n1591) & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b23: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b23: u16 = ALL & zb_holds(n2292);
    let ok_v0_b24: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v0_b24: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b24: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2471);
    let ok_v1_b25: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v1_b25: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b25: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2508);
    let ok_v2_b26: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v2_b26: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b26: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2542);
    let ok_v16_b27: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v16_b27: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b27: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2575);
    let ok_v17_b28: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v17_b28: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b28: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2608);
    let ok_v18_b29: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v18_b29: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b29: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n2641);
    let ok_v32_b30: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v32_b30: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b30: u16 = ALL & zb_holds(n2666);
    let ok_v33_b31: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v33_b31: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b31: u16 = ALL & zb_holds(n2677);
    let ok_v34_b32: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v34_b32: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b32: u16 = ALL & zb_holds(n2688);
    let ok_v36_b33: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v36_b33: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b33: u16 = ALL & zb_holds(n2697);
    let ok_v48_b34: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v48_b34: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b34: u16 = ALL & zb_holds(n2720);
    let ok_v49_b35: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v49_b35: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b35: u16 = ALL & zb_holds(n2731);
    let ok_v50_b36: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v50_b36: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b36: u16 = ALL & zb_holds(n2742);
    let ok_v52_b37: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2413);
    let bd_v52_b37: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b37: u16 = ALL & zb_holds(n2751);
    let ok_v0_b38: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v0_b38: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b38: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3558);
    let ok_v1_b39: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v1_b39: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b39: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3597);
    let ok_v2_b40: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v2_b40: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b40: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3634);
    let ok_v16_b41: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v16_b41: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b41: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3670);
    let ok_v17_b42: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v17_b42: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b42: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3706);
    let ok_v18_b43: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v18_b43: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b43: u16 = ALL & zb_holds(n172) & zb_holds(n1748) & zb_holds(n3742);
    let ok_v32_b44: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v32_b44: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b44: u16 = ALL & zb_holds(n3775);
    let ok_v33_b45: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v33_b45: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b45: u16 = ALL & zb_holds(n3786);
    let ok_v34_b46: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v34_b46: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b46: u16 = ALL & zb_holds(n3797);
    let ok_v36_b47: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v36_b47: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b47: u16 = ALL & zb_holds(n3806);
    let ok_v48_b48: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v48_b48: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b48: u16 = ALL & zb_holds(n3829);
    let ok_v49_b49: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v49_b49: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b49: u16 = ALL & zb_holds(n3840);
    let ok_v50_b50: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v50_b50: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b50: u16 = ALL & zb_holds(n3851);
    let ok_v52_b51: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3488);
    let bd_v52_b51: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b51: u16 = ALL & zb_holds(n3860);
    let ok_v0_b52: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3932);
    let bd_v0_b52: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b52: u16 = ALL & zb_holds(n172) & zb_holds(n3931);
    let ok_v1_b53: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n3976);
    let bd_v1_b53: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b53: u16 = ALL & zb_holds(n172) & zb_holds(n3975);
    let ok_v2_b54: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4020);
    let bd_v2_b54: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b54: u16 = ALL & zb_holds(n172) & zb_holds(n4019);
    let ok_v16_b55: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4063);
    let bd_v16_b55: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b55: u16 = ALL & zb_holds(n172) & zb_holds(n4062);
    let ok_v17_b56: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4106);
    let bd_v17_b56: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b56: u16 = ALL & zb_holds(n172) & zb_holds(n4105);
    let ok_v18_b57: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4149);
    let bd_v18_b57: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b57: u16 = ALL & zb_holds(n172) & zb_holds(n4148);
    let ok_v32_b58: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4177);
    let bd_v32_b58: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b58: u16 = ALL & zb_holds(n4180);
    let ok_v33_b59: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4195);
    let bd_v33_b59: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b59: u16 = ALL & zb_holds(n4198);
    let ok_v34_b60: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4213);
    let bd_v34_b60: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b60: u16 = ALL & zb_holds(n4216);
    let ok_v36_b61: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4229);
    let bd_v36_b61: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b61: u16 = ALL & zb_holds(n4232);
    let ok_v48_b62: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4259);
    let bd_v48_b62: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b62: u16 = ALL & zb_holds(n4262);
    let ok_v49_b63: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4277);
    let bd_v49_b63: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b63: u16 = ALL & zb_holds(n4280);
    let ok_v50_b64: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4295);
    let bd_v50_b64: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b64: u16 = ALL & zb_holds(n4298);
    let ok_v52_b65: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4311);
    let bd_v52_b65: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b65: u16 = ALL & zb_holds(n4314);
    let ok_v0_b66: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4322);
    let bd_v0_b66: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b66: u16 = ALL & zb_holds(n172) & zb_holds(n4321);
    let ok_v1_b67: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4331);
    let bd_v1_b67: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b67: u16 = ALL & zb_holds(n172) & zb_holds(n4330);
    let ok_v2_b68: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4340);
    let bd_v2_b68: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b68: u16 = ALL & zb_holds(n172) & zb_holds(n4339);
    let ok_v16_b69: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4349);
    let bd_v16_b69: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b69: u16 = ALL & zb_holds(n172) & zb_holds(n4348);
    let ok_v17_b70: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4358);
    let bd_v17_b70: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b70: u16 = ALL & zb_holds(n172) & zb_holds(n4357);
    let ok_v18_b71: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4367);
    let bd_v18_b71: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b71: u16 = ALL & zb_holds(n172) & zb_holds(n4366);
    let ok_v32_b72: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4376);
    let bd_v32_b72: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b72: u16 = ALL & zb_holds(n4379);
    let ok_v33_b73: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4387);
    let bd_v33_b73: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b73: u16 = ALL & zb_holds(n4390);
    let ok_v34_b74: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4398);
    let bd_v34_b74: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b74: u16 = ALL & zb_holds(n4401);
    let ok_v36_b75: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4409);
    let bd_v36_b75: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b75: u16 = ALL & zb_holds(n4412);
    let ok_v48_b76: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4420);
    let bd_v48_b76: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b76: u16 = ALL & zb_holds(n4423);
    let ok_v49_b77: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4431);
    let bd_v49_b77: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b77: u16 = ALL & zb_holds(n4434);
    let ok_v50_b78: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4442);
    let bd_v50_b78: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b78: u16 = ALL & zb_holds(n4445);
    let ok_v52_b79: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4453);
    let bd_v52_b79: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b79: u16 = ALL & zb_holds(n4456);
    let ok_v0_b80: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v0_b80: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b80: u16 = ALL & zb_holds(n4506);
    let ok_v1_b81: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v1_b81: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b81: u16 = ALL & zb_holds(n4523);
    let ok_v2_b82: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v2_b82: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b82: u16 = ALL & zb_holds(n4539);
    let ok_v16_b83: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v16_b83: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b83: u16 = ALL & zb_holds(n4556);
    let ok_v17_b84: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v17_b84: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b84: u16 = ALL & zb_holds(n4571);
    let ok_v18_b85: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v18_b85: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b85: u16 = ALL & zb_holds(n4586);
    let ok_v32_b86: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v32_b86: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b86: u16 = ALL & zb_holds(n4632);
    let ok_v33_b87: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v33_b87: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b87: u16 = ALL & zb_holds(n4657);
    let ok_v34_b88: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v34_b88: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b88: u16 = ALL & zb_holds(n4679);
    let ok_v36_b89: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v36_b89: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b89: u16 = ALL & zb_holds(n4710);
    let ok_v37_b90: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v37_b90: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b90: u16 = ALL & zb_holds(n4657);
    let ok_v38_b91: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v38_b91: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b91: u16 = ALL & zb_holds(n4679);
    let ok_v40_b92: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v40_b92: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b92: u16 = ALL & zb_holds(n4710);
    let ok_v41_b93: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v41_b93: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b93: u16 = ALL & zb_holds(n4657);
    let ok_v42_b94: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v42_b94: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b94: u16 = ALL & zb_holds(n4679);
    let ok_v48_b95: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v48_b95: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b95: u16 = ALL & zb_holds(n4757);
    let ok_v49_b96: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v49_b96: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b96: u16 = ALL & zb_holds(n4776);
    let ok_v50_b97: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v50_b97: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b97: u16 = ALL & zb_holds(n4795);
    let ok_v52_b98: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v52_b98: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b98: u16 = ALL & zb_holds(n4814);
    let ok_v53_b99: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v53_b99: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b99: u16 = ALL & zb_holds(n4776);
    let ok_v54_b100: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v54_b100: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b100: u16 = ALL & zb_holds(n4795);
    let ok_v56_b101: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v56_b101: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b101: u16 = ALL & zb_holds(n4814);
    let ok_v57_b102: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v57_b102: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b102: u16 = ALL & zb_holds(n4776);
    let ok_v58_b103: u16 = ALL & zb_holds(n265) & zb_holds(n195) & zb_holds(r_c314) & zb_holds(n260) & zb_holds(r_c297) & zb_holds(n145) & zb_holds(n234) & zb_holds(n143) & zb_holds(n142) & zb_holds(r_c289) & zb_holds(n141) & zb_holds(n140) & zb_holds(n135) & zb_holds(n134) & zb_holds(n133) & zb_holds(n132) & zb_holds(r_c277) & zb_holds(n206) & zb_holds(n205) & zb_holds(n131) & zb_holds(n170) & zb_holds(r_c269) & zb_holds(n130) & zb_holds(n169) & zb_holds(n203) & zb_holds(n167) & zb_holds(n127) & zb_holds(n126) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n4484);
    let bd_v58_b103: bool = !n264 || !n263 || !n262 || !n261 || !n139 || !n138 || !n137 || !n136 || !n129 || !n128 || !n204 || !n168 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b103: u16 = ALL & zb_holds(n4795);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n150,
        c86: n270,
        c241: n345,
        c254: n346,
        c261: n397,
        c274: n398,
        c368: n921,
        c369: n922,
        c302: n920,
        c85: n271,
    };
    let sh1 = KShared1 {
        c87: n2475,
        c84: n150,
        c86: n270,
        c240: n345,
        c253: n346,
        c260: n397,
        c273: n398,
        c85: n271,
    };
    let sh2 = KShared2 {
        c87: n3561,
        c84: n150,
        c86: n270,
        c245: n2754,
        c253: n2759,
        c257: n345,
        c270: n346,
        c277: n397,
        c290: n398,
        c85: n271,
    };
    let sh3 = KShared3 {
        c84: n150,
        c86: n270,
        c85: n271,
    };
    let sh4 = KShared4 {
        c84: n150,
        c86: n270,
        c85: n271,
    };
    let sh5 = KShared5 {
        c87: r_c87,
        c39: r_c39,
        c84: n150,
        c86: n270,
        c246: n4461,
        c254: n4462,
        c258: n4468,
        c271: n4469,
        c278: n4470,
        c291: n4471,
        c406: n4479,
        c407: n4480,
        c319: n4477,
        c85: n271,
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
    let mut take_3_2: u16 = 0;
    let mut take_3_3: u16 = 0;
    let mut take_3_4: u16 = 0;
    let mut take_3_5: u16 = 0;
    let mut take_3_6: u16 = 0;
    let mut take_3_7: u16 = 0;
    let mut take_3_8: u16 = 0;
    let mut take_3_9: u16 = 0;
    let mut take_3_10: u16 = 0;
    let mut take_3_11: u16 = 0;
    let mut take_3_12: u16 = 0;
    let mut take_3_13: u16 = 0;
    let mut take_4_0: u16 = 0;
    let mut take_4_1: u16 = 0;
    let mut take_4_2: u16 = 0;
    let mut take_4_3: u16 = 0;
    let mut take_4_4: u16 = 0;
    let mut take_4_5: u16 = 0;
    let mut take_4_6: u16 = 0;
    let mut take_4_7: u16 = 0;
    let mut take_4_8: u16 = 0;
    let mut take_4_9: u16 = 0;
    let mut take_4_10: u16 = 0;
    let mut take_4_11: u16 = 0;
    let mut take_4_12: u16 = 0;
    let mut take_4_13: u16 = 0;
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
    // 104 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 14, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c396,
        c359: r_c397,
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1743,
        c287: n1609,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1770,
        c371: n1745,
        c301: n1769,
        h1: n4923, h2: n4924,
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
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1833,
        c287: n1609,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1849,
        c371: n1835,
        c301: n1769,
        h1: n4932, h2: n4933,
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
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1900,
        c287: n1609,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n1916,
        c371: n1902,
        c301: n1769,
        h1: n4941, h2: n4942,
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
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1743,
        c287: n1947,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1963,
        c371: n1949,
        c301: n1769,
        h1: n4967, h2: n4968,
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
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1833,
        c287: n1947,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n2008,
        c371: n1994,
        c301: n1769,
        h1: n4975, h2: n4976,
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
        c282: n300,
        c360: r_c398,
        c361: r_c399,
        c284: n304,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n1900,
        c287: n1947,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n2053,
        c371: n2039,
        c301: n1769,
        h1: n4983, h2: n4984,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2089,
        c282: n2063,
        c360: n2090,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2108,
        c371: n2092,
        c301: n2107,
        h1: n5027, h2: n5028,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2135,
        c371: n2120,
        c301: n2107,
        h1: n5043, h2: n5044,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2160,
        c371: n2145,
        c301: n2107,
        h1: n5056, h2: n5057,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2172,
        c282: n2063,
        c360: n2173,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2190,
        c371: n2175,
        c301: n2107,
        h1: n5076, h2: n5077,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2197,
        c371: n2195,
        c301: n2107,
        h1: n5090, h2: n5091,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2203,
        c371: n2201,
        c301: n2107,
        h1: n5102, h2: n5103,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2172,
        c282: n2063,
        c360: n2173,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2190,
        c371: n2207,
        c301: n2107,
        h1: n5112, h2: n5113,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2197,
        c371: n2209,
        c301: n2107,
        h1: n5121, h2: n5122,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1609,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n2203,
        c371: n2211,
        c301: n2107,
        h1: n5130, h2: n5131,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2089,
        c282: n2063,
        c360: n2090,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2245,
        c371: n2230,
        c301: n2107,
        h1: n5154, h2: n5155,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2268,
        c371: n2253,
        c301: n2107,
        h1: n5168, h2: n5169,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2066,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2067,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2291,
        c371: n2276,
        c301: n2107,
        h1: n5180, h2: n5181,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2172,
        c282: n2063,
        c360: n2173,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2312,
        c371: n2297,
        c301: n2107,
        h1: n5196, h2: n5197,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2319,
        c371: n2317,
        c301: n2107,
        h1: n5210, h2: n5211,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2166,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2325,
        c371: n2323,
        c301: n2107,
        h1: n5222, h2: n5223,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2172,
        c282: n2063,
        c360: n2173,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1743,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2312,
        c371: n2327,
        c301: n2107,
        h1: n5231, h2: n5232,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2118,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1833,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2319,
        c371: n2329,
        c301: n2107,
        h1: n5240, h2: n5241,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n2061,
        c41: n2062,
        c358: n2165,
        c359: n2117,
        c282: n2063,
        c360: n2143,
        c361: n2205,
        c284: n2064,
        c285: n2065,
        c362: n1900,
        c287: n1947,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n2325,
        c371: n2331,
        c301: n2107,
        h1: n5249, h2: n5250,
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
        c41: r_c41,
        h1: n5270, h2: n5271,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_1 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_1 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_1 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_1 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    declined |= live_v48_b34 & (if bd_v48_b34 { ALL } else { !ok_v48_b34 });
    take_1_1 |= live_v48_b34 & ok_v48_b34 & (if bd_v48_b34 { 0 } else { ALL });
    declined |= live_v49_b35 & (if bd_v49_b35 { ALL } else { !ok_v49_b35 });
    take_1_1 |= live_v49_b35 & ok_v49_b35 & (if bd_v49_b35 { 0 } else { ALL });
    declined |= live_v50_b36 & (if bd_v50_b36 { ALL } else { !ok_v50_b36 });
    take_1_1 |= live_v50_b36 & ok_v50_b36 & (if bd_v50_b36 { 0 } else { ALL });
    declined |= live_v52_b37 & (if bd_v52_b37 { ALL } else { !ok_v52_b37 });
    take_1_1 |= live_v52_b37 & ok_v52_b37 & (if bd_v52_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n2061,
        c41: n2062,
        h1: n5274, h2: n5275,
    };
    // body 37: buttons 0x34, forks 0x0
    sink.o1(52, take_1_1, &sh1, &o1);
    declined |= live_v0_b38 & (if bd_v0_b38 { ALL } else { !ok_v0_b38 });
    take_2_0 |= live_v0_b38 & ok_v0_b38 & (if bd_v0_b38 { 0 } else { ALL });
    declined |= live_v1_b39 & (if bd_v1_b39 { ALL } else { !ok_v1_b39 });
    take_2_0 |= live_v1_b39 & ok_v1_b39 & (if bd_v1_b39 { 0 } else { ALL });
    declined |= live_v2_b40 & (if bd_v2_b40 { ALL } else { !ok_v2_b40 });
    take_2_0 |= live_v2_b40 & ok_v2_b40 & (if bd_v2_b40 { 0 } else { ALL });
    declined |= live_v16_b41 & (if bd_v16_b41 { ALL } else { !ok_v16_b41 });
    take_2_0 |= live_v16_b41 & ok_v16_b41 & (if bd_v16_b41 { 0 } else { ALL });
    declined |= live_v17_b42 & (if bd_v17_b42 { ALL } else { !ok_v17_b42 });
    take_2_0 |= live_v17_b42 & ok_v17_b42 & (if bd_v17_b42 { 0 } else { ALL });
    declined |= live_v18_b43 & (if bd_v18_b43 { ALL } else { !ok_v18_b43 });
    take_2_0 |= live_v18_b43 & ok_v18_b43 & (if bd_v18_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n5295, h2: n5296,
    };
    // body 43: buttons 0x12, forks 0x0
    sink.o2(18, take_2_0, &sh2, &o2);
    declined |= live_v32_b44 & (if bd_v32_b44 { ALL } else { !ok_v32_b44 });
    take_2_1 |= live_v32_b44 & ok_v32_b44 & (if bd_v32_b44 { 0 } else { ALL });
    declined |= live_v33_b45 & (if bd_v33_b45 { ALL } else { !ok_v33_b45 });
    take_2_1 |= live_v33_b45 & ok_v33_b45 & (if bd_v33_b45 { 0 } else { ALL });
    declined |= live_v34_b46 & (if bd_v34_b46 { ALL } else { !ok_v34_b46 });
    take_2_1 |= live_v34_b46 & ok_v34_b46 & (if bd_v34_b46 { 0 } else { ALL });
    declined |= live_v36_b47 & (if bd_v36_b47 { ALL } else { !ok_v36_b47 });
    take_2_1 |= live_v36_b47 & ok_v36_b47 & (if bd_v36_b47 { 0 } else { ALL });
    declined |= live_v48_b48 & (if bd_v48_b48 { ALL } else { !ok_v48_b48 });
    take_2_1 |= live_v48_b48 & ok_v48_b48 & (if bd_v48_b48 { 0 } else { ALL });
    declined |= live_v49_b49 & (if bd_v49_b49 { ALL } else { !ok_v49_b49 });
    take_2_1 |= live_v49_b49 & ok_v49_b49 & (if bd_v49_b49 { 0 } else { ALL });
    declined |= live_v50_b50 & (if bd_v50_b50 { ALL } else { !ok_v50_b50 });
    take_2_1 |= live_v50_b50 & ok_v50_b50 & (if bd_v50_b50 { 0 } else { ALL });
    declined |= live_v52_b51 & (if bd_v52_b51 { ALL } else { !ok_v52_b51 });
    take_2_1 |= live_v52_b51 & ok_v52_b51 & (if bd_v52_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n3749,
        c41: n3750,
        h1: n5301, h2: n5302,
    };
    // body 51: buttons 0x34, forks 0x0
    sink.o2(52, take_2_1, &sh2, &o2);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_3_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n3934,
        c39: n3935,
        c20: r_c20,
        c38: n3930,
        h1: n5312, h2: n5313,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b53 & (if bd_v1_b53 { ALL } else { !ok_v1_b53 });
    take_3_1 |= live_v1_b53 & ok_v1_b53 & (if bd_v1_b53 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n3978,
        c39: n3979,
        c20: r_c20,
        c38: n3974,
        h1: n5321, h2: n5322,
    };
    // body 53: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b54 & (if bd_v2_b54 { ALL } else { !ok_v2_b54 });
    take_3_2 |= live_v2_b54 & ok_v2_b54 & (if bd_v2_b54 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4022,
        c39: n4023,
        c20: r_c20,
        c38: n4018,
        h1: n5330, h2: n5331,
    };
    // body 54: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b55 & (if bd_v16_b55 { ALL } else { !ok_v16_b55 });
    take_3_3 |= live_v16_b55 & ok_v16_b55 & (if bd_v16_b55 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4065,
        c39: n4066,
        c20: r_c20,
        c38: n4061,
        h1: n5339, h2: n5340,
    };
    // body 55: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b56 & (if bd_v17_b56 { ALL } else { !ok_v17_b56 });
    take_3_4 |= live_v17_b56 & ok_v17_b56 & (if bd_v17_b56 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4108,
        c39: n4109,
        c20: r_c20,
        c38: n4104,
        h1: n5348, h2: n5349,
    };
    // body 56: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b57 & (if bd_v18_b57 { ALL } else { !ok_v18_b57 });
    take_3_5 |= live_v18_b57 & ok_v18_b57 & (if bd_v18_b57 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4151,
        c39: n4152,
        c20: r_c20,
        c38: n4147,
        h1: n5357, h2: n5358,
    };
    // body 57: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b58 & (if bd_v32_b58 { ALL } else { !ok_v32_b58 });
    take_3_6 |= live_v32_b58 & ok_v32_b58 & (if bd_v32_b58 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4181,
        c39: n4182,
        c20: n3749,
        c38: n4175,
        h1: n5368, h2: n5369,
    };
    // body 58: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b59 & (if bd_v33_b59 { ALL } else { !ok_v33_b59 });
    take_3_7 |= live_v33_b59 & ok_v33_b59 & (if bd_v33_b59 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4199,
        c39: n4200,
        c20: n3749,
        c38: n4193,
        h1: n5377, h2: n5378,
    };
    // body 59: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b60 & (if bd_v34_b60 { ALL } else { !ok_v34_b60 });
    take_3_8 |= live_v34_b60 & ok_v34_b60 & (if bd_v34_b60 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4217,
        c39: n4218,
        c20: n3749,
        c38: n4211,
        h1: n5386, h2: n5387,
    };
    // body 60: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b61 & (if bd_v36_b61 { ALL } else { !ok_v36_b61 });
    take_3_9 |= live_v36_b61 & ok_v36_b61 & (if bd_v36_b61 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4233,
        c39: n4234,
        c20: n3749,
        c38: n4227,
        h1: n5395, h2: n5396,
    };
    // body 61: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v48_b62 & (if bd_v48_b62 { ALL } else { !ok_v48_b62 });
    take_3_10 |= live_v48_b62 & ok_v48_b62 & (if bd_v48_b62 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4263,
        c39: n4264,
        c20: n3749,
        c38: n4257,
        h1: n5404, h2: n5405,
    };
    // body 62: buttons 0x30, forks 0x0
    sink.o3(48, take_3_10, &sh3, &o3);
    declined |= live_v49_b63 & (if bd_v49_b63 { ALL } else { !ok_v49_b63 });
    take_3_11 |= live_v49_b63 & ok_v49_b63 & (if bd_v49_b63 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4281,
        c39: n4282,
        c20: n3749,
        c38: n4275,
        h1: n5413, h2: n5414,
    };
    // body 63: buttons 0x31, forks 0x0
    sink.o3(49, take_3_11, &sh3, &o3);
    declined |= live_v50_b64 & (if bd_v50_b64 { ALL } else { !ok_v50_b64 });
    take_3_12 |= live_v50_b64 & ok_v50_b64 & (if bd_v50_b64 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4299,
        c39: n4300,
        c20: n3749,
        c38: n4293,
        h1: n5422, h2: n5423,
    };
    // body 64: buttons 0x32, forks 0x0
    sink.o3(50, take_3_12, &sh3, &o3);
    declined |= live_v52_b65 & (if bd_v52_b65 { ALL } else { !ok_v52_b65 });
    take_3_13 |= live_v52_b65 & ok_v52_b65 & (if bd_v52_b65 { 0 } else { ALL });
    let o3 = KOut3 {
        c87: n4315,
        c39: n4316,
        c20: n3749,
        c38: n4309,
        h1: n5431, h2: n5432,
    };
    // body 65: buttons 0x34, forks 0x0
    sink.o3(52, take_3_13, &sh3, &o3);
    declined |= live_v0_b66 & (if bd_v0_b66 { ALL } else { !ok_v0_b66 });
    take_4_0 |= live_v0_b66 & ok_v0_b66 & (if bd_v0_b66 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4324,
        c39: n4325,
        c20: r_c20,
        c38: n4320,
        h1: n5440, h2: n5441,
    };
    // body 66: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b67 & (if bd_v1_b67 { ALL } else { !ok_v1_b67 });
    take_4_1 |= live_v1_b67 & ok_v1_b67 & (if bd_v1_b67 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4333,
        c39: n4334,
        c20: r_c20,
        c38: n4329,
        h1: n5449, h2: n5450,
    };
    // body 67: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b68 & (if bd_v2_b68 { ALL } else { !ok_v2_b68 });
    take_4_2 |= live_v2_b68 & ok_v2_b68 & (if bd_v2_b68 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4342,
        c39: n4343,
        c20: r_c20,
        c38: n4338,
        h1: n5458, h2: n5459,
    };
    // body 68: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b69 & (if bd_v16_b69 { ALL } else { !ok_v16_b69 });
    take_4_3 |= live_v16_b69 & ok_v16_b69 & (if bd_v16_b69 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4351,
        c39: n4352,
        c20: r_c20,
        c38: n4347,
        h1: n5467, h2: n5468,
    };
    // body 69: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b70 & (if bd_v17_b70 { ALL } else { !ok_v17_b70 });
    take_4_4 |= live_v17_b70 & ok_v17_b70 & (if bd_v17_b70 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4360,
        c39: n4361,
        c20: r_c20,
        c38: n4356,
        h1: n5476, h2: n5477,
    };
    // body 70: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b71 & (if bd_v18_b71 { ALL } else { !ok_v18_b71 });
    take_4_5 |= live_v18_b71 & ok_v18_b71 & (if bd_v18_b71 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4369,
        c39: n4370,
        c20: r_c20,
        c38: n4365,
        h1: n5485, h2: n5486,
    };
    // body 71: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b72 & (if bd_v32_b72 { ALL } else { !ok_v32_b72 });
    take_4_6 |= live_v32_b72 & ok_v32_b72 & (if bd_v32_b72 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4380,
        c39: n4381,
        c20: n2061,
        c38: n4374,
        h1: n5496, h2: n5497,
    };
    // body 72: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b73 & (if bd_v33_b73 { ALL } else { !ok_v33_b73 });
    take_4_7 |= live_v33_b73 & ok_v33_b73 & (if bd_v33_b73 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4391,
        c39: n4392,
        c20: n2061,
        c38: n4385,
        h1: n5505, h2: n5506,
    };
    // body 73: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b74 & (if bd_v34_b74 { ALL } else { !ok_v34_b74 });
    take_4_8 |= live_v34_b74 & ok_v34_b74 & (if bd_v34_b74 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4402,
        c39: n4403,
        c20: n2061,
        c38: n4396,
        h1: n5514, h2: n5515,
    };
    // body 74: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b75 & (if bd_v36_b75 { ALL } else { !ok_v36_b75 });
    take_4_9 |= live_v36_b75 & ok_v36_b75 & (if bd_v36_b75 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4413,
        c39: n4414,
        c20: n2061,
        c38: n4407,
        h1: n5523, h2: n5524,
    };
    // body 75: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v48_b76 & (if bd_v48_b76 { ALL } else { !ok_v48_b76 });
    take_4_10 |= live_v48_b76 & ok_v48_b76 & (if bd_v48_b76 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4424,
        c39: n4425,
        c20: n2061,
        c38: n4418,
        h1: n5532, h2: n5533,
    };
    // body 76: buttons 0x30, forks 0x0
    sink.o4(48, take_4_10, &sh4, &o4);
    declined |= live_v49_b77 & (if bd_v49_b77 { ALL } else { !ok_v49_b77 });
    take_4_11 |= live_v49_b77 & ok_v49_b77 & (if bd_v49_b77 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4435,
        c39: n4436,
        c20: n2061,
        c38: n4429,
        h1: n5541, h2: n5542,
    };
    // body 77: buttons 0x31, forks 0x0
    sink.o4(49, take_4_11, &sh4, &o4);
    declined |= live_v50_b78 & (if bd_v50_b78 { ALL } else { !ok_v50_b78 });
    take_4_12 |= live_v50_b78 & ok_v50_b78 & (if bd_v50_b78 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4446,
        c39: n4447,
        c20: n2061,
        c38: n4440,
        h1: n5550, h2: n5551,
    };
    // body 78: buttons 0x32, forks 0x0
    sink.o4(50, take_4_12, &sh4, &o4);
    declined |= live_v52_b79 & (if bd_v52_b79 { ALL } else { !ok_v52_b79 });
    take_4_13 |= live_v52_b79 & ok_v52_b79 & (if bd_v52_b79 { 0 } else { ALL });
    let o4 = KOut4 {
        c87: n4457,
        c39: n4458,
        c20: n2061,
        c38: n4451,
        h1: n5559, h2: n5560,
    };
    // body 79: buttons 0x34, forks 0x0
    sink.o4(52, take_4_13, &sh4, &o4);
    declined |= live_v0_b80 & (if bd_v0_b80 { ALL } else { !ok_v0_b80 });
    take_5_0 |= live_v0_b80 & ok_v0_b80 & (if bd_v0_b80 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4478,
        c304: n4475,
        c311: n4463,
        c312: n4464,
        c408: n4505,
        c409: n4482,
        c318: n4504,
        h1: n5629, h2: n5630,
    };
    // body 80: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v1_b81 & (if bd_v1_b81 { ALL } else { !ok_v1_b81 });
    take_5_1 |= live_v1_b81 & ok_v1_b81 & (if bd_v1_b81 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4509,
        c304: n4475,
        c311: n4463,
        c312: n4464,
        c408: n4522,
        c409: n4511,
        c318: n4504,
        h1: n5638, h2: n5639,
    };
    // body 81: buttons 0x01, forks 0x0
    sink.o5(1, take_5_1, &sh5, &o5);
    declined |= live_v2_b82 & (if bd_v2_b82 { ALL } else { !ok_v2_b82 });
    take_5_2 |= live_v2_b82 & ok_v2_b82 & (if bd_v2_b82 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4525,
        c304: n4475,
        c311: n4463,
        c312: n4464,
        c408: n4538,
        c409: n4527,
        c318: n4504,
        h1: n5647, h2: n5648,
    };
    // body 82: buttons 0x02, forks 0x0
    sink.o5(2, take_5_2, &sh5, &o5);
    declined |= live_v16_b83 & (if bd_v16_b83 { ALL } else { !ok_v16_b83 });
    take_5_3 |= live_v16_b83 & ok_v16_b83 & (if bd_v16_b83 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4478,
        c304: n4542,
        c311: n4463,
        c312: n4540,
        c408: n4555,
        c409: n4544,
        c318: n4504,
        h1: n5673, h2: n5674,
    };
    // body 83: buttons 0x10, forks 0x0
    sink.o5(16, take_5_3, &sh5, &o5);
    declined |= live_v17_b84 & (if bd_v17_b84 { ALL } else { !ok_v17_b84 });
    take_5_4 |= live_v17_b84 & ok_v17_b84 & (if bd_v17_b84 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4509,
        c304: n4542,
        c311: n4463,
        c312: n4540,
        c408: n4570,
        c409: n4559,
        c318: n4504,
        h1: n5681, h2: n5682,
    };
    // body 84: buttons 0x11, forks 0x0
    sink.o5(17, take_5_4, &sh5, &o5);
    declined |= live_v18_b85 & (if bd_v18_b85 { ALL } else { !ok_v18_b85 });
    take_5_5 |= live_v18_b85 & ok_v18_b85 & (if bd_v18_b85 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4467,
        c41: r_c41,
        c396: r_c396,
        c397: r_c397,
        c299: n4472,
        c398: r_c398,
        c399: r_c399,
        c301: n4473,
        c302: n4474,
        c400: n4525,
        c304: n4542,
        c311: n4463,
        c312: n4540,
        c408: n4585,
        c409: n4574,
        c318: n4504,
        h1: n5689, h2: n5690,
    };
    // body 85: buttons 0x12, forks 0x0
    sink.o5(18, take_5_5, &sh5, &o5);
    declined |= live_v32_b86 & (if bd_v32_b86 { ALL } else { !ok_v32_b86 });
    take_5_6 |= live_v32_b86 & ok_v32_b86 & (if bd_v32_b86 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4613,
        c299: n4609,
        c398: n4614,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4631,
        c409: n4617,
        c318: n4630,
        h1: n5734, h2: n5735,
    };
    // body 86: buttons 0x20, forks 0x0
    sink.o5(32, take_5_6, &sh5, &o5);
    declined |= live_v33_b87 & (if bd_v33_b87 { ALL } else { !ok_v33_b87 });
    take_5_7 |= live_v33_b87 & ok_v33_b87 & (if bd_v33_b87 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4656,
        c409: n4645,
        c318: n4630,
        h1: n5750, h2: n5751,
    };
    // body 87: buttons 0x21, forks 0x0
    sink.o5(33, take_5_7, &sh5, &o5);
    declined |= live_v34_b88 & (if bd_v34_b88 { ALL } else { !ok_v34_b88 });
    take_5_8 |= live_v34_b88 & ok_v34_b88 & (if bd_v34_b88 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4678,
        c409: n4667,
        c318: n4630,
        h1: n5763, h2: n5764,
    };
    // body 88: buttons 0x22, forks 0x0
    sink.o5(34, take_5_8, &sh5, &o5);
    declined |= live_v36_b89 & (if bd_v36_b89 { ALL } else { !ok_v36_b89 });
    take_5_9 |= live_v36_b89 & ok_v36_b89 & (if bd_v36_b89 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4694,
        c299: n4609,
        c398: n4695,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4709,
        c409: n4698,
        c318: n4630,
        h1: n5783, h2: n5784,
    };
    // body 89: buttons 0x24, forks 0x0
    sink.o5(36, take_5_9, &sh5, &o5);
    declined |= live_v37_b90 & (if bd_v37_b90 { ALL } else { !ok_v37_b90 });
    take_5_10 |= live_v37_b90 & ok_v37_b90 & (if bd_v37_b90 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4718,
        c409: n4716,
        c318: n4630,
        h1: n5797, h2: n5798,
    };
    // body 90: buttons 0x25, forks 0x0
    sink.o5(37, take_5_10, &sh5, &o5);
    declined |= live_v38_b91 & (if bd_v38_b91 { ALL } else { !ok_v38_b91 });
    take_5_11 |= live_v38_b91 & ok_v38_b91 & (if bd_v38_b91 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4726,
        c409: n4724,
        c318: n4630,
        h1: n5809, h2: n5810,
    };
    // body 91: buttons 0x26, forks 0x0
    sink.o5(38, take_5_11, &sh5, &o5);
    declined |= live_v40_b92 & (if bd_v40_b92 { ALL } else { !ok_v40_b92 });
    take_5_12 |= live_v40_b92 & ok_v40_b92 & (if bd_v40_b92 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4694,
        c299: n4609,
        c398: n4695,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4709,
        c409: n4732,
        c318: n4630,
        h1: n5819, h2: n5820,
    };
    // body 92: buttons 0x28, forks 0x0
    sink.o5(40, take_5_12, &sh5, &o5);
    declined |= live_v41_b93 & (if bd_v41_b93 { ALL } else { !ok_v41_b93 });
    take_5_13 |= live_v41_b93 & ok_v41_b93 & (if bd_v41_b93 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4718,
        c409: n4735,
        c318: n4630,
        h1: n5828, h2: n5829,
    };
    // body 93: buttons 0x29, forks 0x0
    sink.o5(41, take_5_13, &sh5, &o5);
    declined |= live_v42_b94 & (if bd_v42_b94 { ALL } else { !ok_v42_b94 });
    take_5_14 |= live_v42_b94 & ok_v42_b94 & (if bd_v42_b94 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4475,
        c311: n4587,
        c312: n4464,
        c408: n4726,
        c409: n4738,
        c318: n4630,
        h1: n5837, h2: n5838,
    };
    // body 94: buttons 0x2a, forks 0x0
    sink.o5(42, take_5_14, &sh5, &o5);
    declined |= live_v48_b95 & (if bd_v48_b95 { ALL } else { !ok_v48_b95 });
    take_5_15 |= live_v48_b95 & ok_v48_b95 & (if bd_v48_b95 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4613,
        c299: n4609,
        c398: n4614,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4756,
        c409: n4745,
        c318: n4630,
        h1: n5861, h2: n5862,
    };
    // body 95: buttons 0x30, forks 0x0
    sink.o5(48, take_5_15, &sh5, &o5);
    declined |= live_v49_b96 & (if bd_v49_b96 { ALL } else { !ok_v49_b96 });
    take_5_16 |= live_v49_b96 & ok_v49_b96 & (if bd_v49_b96 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4775,
        c409: n4764,
        c318: n4630,
        h1: n5875, h2: n5876,
    };
    // body 96: buttons 0x31, forks 0x0
    sink.o5(49, take_5_16, &sh5, &o5);
    declined |= live_v50_b97 & (if bd_v50_b97 { ALL } else { !ok_v50_b97 });
    take_5_17 |= live_v50_b97 & ok_v50_b97 & (if bd_v50_b97 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4612,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4615,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4794,
        c409: n4783,
        c318: n4630,
        h1: n5887, h2: n5888,
    };
    // body 97: buttons 0x32, forks 0x0
    sink.o5(50, take_5_17, &sh5, &o5);
    declined |= live_v52_b98 & (if bd_v52_b98 { ALL } else { !ok_v52_b98 });
    take_5_18 |= live_v52_b98 & ok_v52_b98 & (if bd_v52_b98 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4694,
        c299: n4609,
        c398: n4695,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4813,
        c409: n4802,
        c318: n4630,
        h1: n5903, h2: n5904,
    };
    // body 98: buttons 0x34, forks 0x0
    sink.o5(52, take_5_18, &sh5, &o5);
    declined |= live_v53_b99 & (if bd_v53_b99 { ALL } else { !ok_v53_b99 });
    take_5_19 |= live_v53_b99 & ok_v53_b99 & (if bd_v53_b99 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4822,
        c409: n4820,
        c318: n4630,
        h1: n5917, h2: n5918,
    };
    // body 99: buttons 0x35, forks 0x0
    sink.o5(53, take_5_19, &sh5, &o5);
    declined |= live_v54_b100 & (if bd_v54_b100 { ALL } else { !ok_v54_b100 });
    take_5_20 |= live_v54_b100 & ok_v54_b100 & (if bd_v54_b100 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4696,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4830,
        c409: n4828,
        c318: n4630,
        h1: n5929, h2: n5930,
    };
    // body 100: buttons 0x36, forks 0x0
    sink.o5(54, take_5_20, &sh5, &o5);
    declined |= live_v56_b101 & (if bd_v56_b101 { ALL } else { !ok_v56_b101 });
    take_5_21 |= live_v56_b101 & ok_v56_b101 & (if bd_v56_b101 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4694,
        c299: n4609,
        c398: n4695,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4478,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4813,
        c409: n4833,
        c318: n4630,
        h1: n5938, h2: n5939,
    };
    // body 101: buttons 0x38, forks 0x0
    sink.o5(56, take_5_21, &sh5, &o5);
    declined |= live_v57_b102 & (if bd_v57_b102 { ALL } else { !ok_v57_b102 });
    take_5_22 |= live_v57_b102 & ok_v57_b102 & (if bd_v57_b102 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4643,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4509,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4822,
        c409: n4836,
        c318: n4630,
        h1: n5947, h2: n5948,
    };
    // body 102: buttons 0x39, forks 0x0
    sink.o5(57, take_5_22, &sh5, &o5);
    declined |= live_v58_b103 & (if bd_v58_b103 { ALL } else { !ok_v58_b103 });
    take_5_23 |= live_v58_b103 & ok_v58_b103 & (if bd_v58_b103 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n4607,
        c41: n4608,
        c396: n4693,
        c397: n4642,
        c299: n4609,
        c398: n4665,
        c399: n4731,
        c301: n4610,
        c302: n4611,
        c400: n4525,
        c304: n4542,
        c311: n4587,
        c312: n4540,
        c408: n4830,
        c409: n4839,
        c318: n4630,
        h1: n5956, h2: n5957,
    };
    // body 103: buttons 0x3a, forks 0x0
    sink.o5(58, take_5_23, &sh5, &o5);
    declined
}
