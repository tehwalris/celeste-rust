// GENERATED from a TRACED frame (shape 17). Do not edit.
//
// One input shape, 14 output shapes, 160 distinct button
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
pub const SHAPE: u64 = 12241359978389760646;

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
    pub c366: P8,
    pub c367: P8,
    pub c368: P8,
    pub c369: P8,
    pub c376: P8,
    pub c377: P8,
    pub c378: P8,
    pub c379: P8,
    pub c386: P8,
    pub c387: P8,
    pub c388: P8,
    pub c389: P8,
    pub c400: P8,
    pub c401: P8,
    pub c402: P8,
    pub c403: P8,
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
    pub c364: u16,
    pub c365: u16,
    pub c246: ZN,
    pub c370: ZN,
    pub c371: ZN,
    pub c248: u16,
    pub c372: ZN,
    pub c373: ZN,
    pub c250: ZN,
    pub c251: ZN,
    pub c253: ZN,
    pub c254: ZN,
    pub c257: u16,
    pub c374: u16,
    pub c375: u16,
    pub c259: ZN,
    pub c260: ZN,
    pub c380: ZN,
    pub c381: ZN,
    pub c268: u16,
    pub c382: ZN,
    pub c383: ZN,
    pub c270: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c276: u16,
    pub c384: u16,
    pub c385: u16,
    pub c278: ZN,
    pub c279: ZN,
    pub c390: ZN,
    pub c391: ZN,
    pub c287: u16,
    pub c392: ZN,
    pub c393: ZN,
    pub c289: ZN,
    pub c291: ZN,
    pub c292: ZN,
    pub c295: u16,
    pub c394: ZN,
    pub c395: ZN,
    pub c297: ZN,
    pub c396: ZN,
    pub c397: ZN,
    pub c299: ZN,
    pub c300: ZN,
    pub c398: u16,
    pub c399: u16,
    pub c302: ZN,
    pub c309: u16,
    pub c310: u16,
    pub c404: ZN,
    pub c405: ZN,
    pub c312: u16,
    pub c406: ZN,
    pub c407: ZN,
    pub c316: ZN,
    pub c317: ZN,
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
    pub c364: u32,
    pub c365: u32,
    pub c246: u32,
    pub c370: u32,
    pub c371: u32,
    pub c248: u32,
    pub c372: u32,
    pub c373: u32,
    pub c250: u32,
    pub c251: u32,
    pub c253: u32,
    pub c254: u32,
    pub c257: u32,
    pub c374: u32,
    pub c375: u32,
    pub c259: u32,
    pub c260: u32,
    pub c380: u32,
    pub c381: u32,
    pub c268: u32,
    pub c382: u32,
    pub c383: u32,
    pub c270: u32,
    pub c272: u32,
    pub c273: u32,
    pub c276: u32,
    pub c384: u32,
    pub c385: u32,
    pub c278: u32,
    pub c279: u32,
    pub c390: u32,
    pub c391: u32,
    pub c287: u32,
    pub c392: u32,
    pub c393: u32,
    pub c289: u32,
    pub c291: u32,
    pub c292: u32,
    pub c295: u32,
    pub c394: u32,
    pub c395: u32,
    pub c297: u32,
    pub c396: u32,
    pub c397: u32,
    pub c299: u32,
    pub c300: u32,
    pub c398: u32,
    pub c399: u32,
    pub c302: u32,
    pub c309: u32,
    pub c310: u32,
    pub c404: u32,
    pub c405: u32,
    pub c312: u32,
    pub c406: u32,
    pub c407: u32,
    pub c316: u32,
    pub c317: u32,
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
        c366: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c367: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c368: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c369: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c376: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c377: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c378: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c379: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c386: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c387: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c388: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c389: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c400: match &b.cols[cell("objects[3].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c401: match &b.cols[cell("objects[3].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c402: match &b.cols[cell("objects[3].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c403: match &b.cols[cell("objects[3].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c364: cell("objects[0].flip.x")?,
        c365: cell("objects[0].flip.y")?,
        c246: cell("objects[0].off")?,
        c370: cell("objects[0].rem.x")?,
        c371: cell("objects[0].rem.y")?,
        c248: cell("objects[0].solids")?,
        c372: cell("objects[0].spd.x")?,
        c373: cell("objects[0].spd.y")?,
        c250: cell("objects[0].spr")?,
        c251: cell("objects[0].start")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
        c257: cell("objects[1].collideable")?,
        c374: cell("objects[1].flip.x")?,
        c375: cell("objects[1].flip.y")?,
        c259: cell("objects[1].hide_for")?,
        c260: cell("objects[1].hide_in")?,
        c380: cell("objects[1].rem.x")?,
        c381: cell("objects[1].rem.y")?,
        c268: cell("objects[1].solids")?,
        c382: cell("objects[1].spd.x")?,
        c383: cell("objects[1].spd.y")?,
        c270: cell("objects[1].spr")?,
        c272: cell("objects[1].x")?,
        c273: cell("objects[1].y")?,
        c276: cell("objects[2].collideable")?,
        c384: cell("objects[2].flip.x")?,
        c385: cell("objects[2].flip.y")?,
        c278: cell("objects[2].hide_for")?,
        c279: cell("objects[2].hide_in")?,
        c390: cell("objects[2].rem.x")?,
        c391: cell("objects[2].rem.y")?,
        c287: cell("objects[2].solids")?,
        c392: cell("objects[2].spd.x")?,
        c393: cell("objects[2].spd.y")?,
        c289: cell("objects[2].spr")?,
        c291: cell("objects[2].x")?,
        c292: cell("objects[2].y")?,
        c295: cell("objects[3].collideable")?,
        c394: cell("objects[3].dash_accel.x")?,
        c395: cell("objects[3].dash_accel.y")?,
        c297: cell("objects[3].dash_effect_time")?,
        c396: cell("objects[3].dash_target.x")?,
        c397: cell("objects[3].dash_target.y")?,
        c299: cell("objects[3].dash_time")?,
        c300: cell("objects[3].djump")?,
        c398: cell("objects[3].flip.x")?,
        c399: cell("objects[3].flip.y")?,
        c302: cell("objects[3].grace")?,
        c309: cell("objects[3].p_dash")?,
        c310: cell("objects[3].p_jump")?,
        c404: cell("objects[3].rem.x")?,
        c405: cell("objects[3].rem.y")?,
        c312: cell("objects[3].solids")?,
        c406: cell("objects[3].spd.x")?,
        c407: cell("objects[3].spd.y")?,
        c316: cell("objects[3].x")?,
        c317: cell("objects[3].y")?,
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
        c364: match &b.cols[s.c364 as usize] {
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
        c365: match &b.cols[s.c365 as usize] {
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
        c370: match &b.cols[s.c370 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c371: match &b.cols[s.c371 as usize] {
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
        c374: match &b.cols[s.c374 as usize] {
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
        c375: match &b.cols[s.c375 as usize] {
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
        c259: match &b.cols[s.c259 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c260: match &b.cols[s.c260 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c380: match &b.cols[s.c380 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c381: match &b.cols[s.c381 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
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
        c270: match &b.cols[s.c270 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c276: match &b.cols[s.c276 as usize] {
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
        c385: match &b.cols[s.c385 as usize] {
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
        c279: match &b.cols[s.c279 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c390: match &b.cols[s.c390 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c391: match &b.cols[s.c391 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c287: match &b.cols[s.c287 as usize] {
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
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c291: match &b.cols[s.c291 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c292: match &b.cols[s.c292 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c295: match &b.cols[s.c295 as usize] {
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
        c297: match &b.cols[s.c297 as usize] {
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
        c299: match &b.cols[s.c299 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c300: match &b.cols[s.c300 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c398: match &b.cols[s.c398 as usize] {
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
        c399: match &b.cols[s.c399 as usize] {
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
        c302: match &b.cols[s.c302 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c309: match &b.cols[s.c309 as usize] {
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
        c310: match &b.cols[s.c310 as usize] {
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
        c404: match &b.cols[s.c404 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c405: match &b.cols[s.c405 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c316: match &b.cols[s.c316 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c317: match &b.cols[s.c317 as usize] {
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
    (337, "objects[0].flip.x"),
    (338, "objects[0].flip.y"),
    (243, "objects[0].hide_for"),
    (244, "objects[0].hide_in"),
    (339, "objects[0].hitbox.h"),
    (340, "objects[0].hitbox.w"),
    (341, "objects[0].hitbox.x"),
    (342, "objects[0].hitbox.y"),
    (343, "objects[0].rem.x"),
    (344, "objects[0].rem.y"),
    (252, "objects[0].solids"),
    (345, "objects[0].spd.x"),
    (346, "objects[0].spd.y"),
    (254, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (256, "objects[0].x"),
    (257, "objects[0].y"),
    (260, "objects[1].collideable"),
    (347, "objects[1].flip.x"),
    (348, "objects[1].flip.y"),
    (262, "objects[1].hide_for"),
    (263, "objects[1].hide_in"),
    (349, "objects[1].hitbox.h"),
    (350, "objects[1].hitbox.w"),
    (351, "objects[1].hitbox.x"),
    (352, "objects[1].hitbox.y"),
    (353, "objects[1].rem.x"),
    (354, "objects[1].rem.y"),
    (271, "objects[1].solids"),
    (355, "objects[1].spd.x"),
    (356, "objects[1].spd.y"),
    (273, "objects[1].spr"),
    (275, "objects[1].x"),
    (276, "objects[1].y"),
    (279, "objects[2].collideable"),
    (357, "objects[2].dash_accel.x"),
    (358, "objects[2].dash_accel.y"),
    (281, "objects[2].dash_effect_time"),
    (359, "objects[2].dash_target.x"),
    (360, "objects[2].dash_target.y"),
    (283, "objects[2].dash_time"),
    (284, "objects[2].djump"),
    (361, "objects[2].flip.x"),
    (362, "objects[2].flip.y"),
    (286, "objects[2].grace"),
    (363, "objects[2].hitbox.h"),
    (364, "objects[2].hitbox.w"),
    (365, "objects[2].hitbox.x"),
    (366, "objects[2].hitbox.y"),
    (293, "objects[2].p_dash"),
    (294, "objects[2].p_jump"),
    (367, "objects[2].rem.x"),
    (368, "objects[2].rem.y"),
    (296, "objects[2].solids"),
    (369, "objects[2].spd.x"),
    (370, "objects[2].spd.y"),
    (300, "objects[2].x"),
    (301, "objects[2].y"),
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
    SCell::Obj(&[(21, 258), (20, 259), (11, 260), (14, 261), (45, 262), (42, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (0, 274), (1, 275), (2, 276)]),
    SCell::Obj(&[(21, 277), (20, 278), (11, 279), (28, 280), (33, 281), (27, 282), (26, 283), (30, 284), (14, 285), (29, 286), (15, 287), (19, 288), (18, 289), (22, 290), (23, 291), (24, 292), (32, 293), (31, 294), (4, 295), (12, 296), (3, 297), (13, 298), (0, 299), (1, 300), (2, 301)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 337), (2, 338)]),
    SCell::Obj(&[(17, 339), (16, 340), (1, 341), (2, 342)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 343), (2, 344)]),
    SCell::Obj(&[(1, 345), (2, 346)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 347), (2, 348)]),
    SCell::Obj(&[(17, 349), (16, 350), (1, 351), (2, 352)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 353), (2, 354)]),
    SCell::Obj(&[(1, 355), (2, 356)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 357), (2, 358)]),
    SCell::Obj(&[(1, 359), (2, 360)]),
    SCell::Obj(&[(1, 361), (2, 362)]),
    SCell::Obj(&[(17, 363), (16, 364), (1, 365), (2, 366)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 367), (2, 368)]),
    SCell::Obj(&[(1, 369), (2, 370)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (238, 302),
    (239, 303),
    (242, 304),
    (245, 305),
    (246, 306),
    (247, 307),
    (248, 308),
    (249, 309),
    (250, 310),
    (251, 311),
    (253, 312),
    (255, 116),
    (258, 313),
    (259, 314),
    (261, 315),
    (264, 316),
    (265, 317),
    (266, 318),
    (267, 319),
    (268, 320),
    (269, 321),
    (270, 322),
    (272, 323),
    (274, 116),
    (277, 324),
    (278, 325),
    (280, 326),
    (282, 327),
    (285, 328),
    (287, 329),
    (288, 330),
    (289, 331),
    (290, 332),
    (291, 333),
    (292, 334),
    (295, 335),
    (297, 336),
    (299, 93),
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
    pub c367: ZN,
    pub c368: ZN,
    pub c301: ZN,
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
    pub c357: ZN,
    pub c358: ZN,
    pub c281: ZN,
    pub c359: ZN,
    pub c360: ZN,
    pub c283: ZN,
    pub c284: ZN,
    pub c361: ZB,
    pub c286: ZN,
    pub c293: ZB,
    pub c294: ZB,
    pub c369: ZN,
    pub c370: ZN,
    pub c300: ZN,
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
    (240, "objects[0].collideable"),
    (337, "objects[0].flip.x"),
    (338, "objects[0].flip.y"),
    (242, "objects[0].hide_for"),
    (243, "objects[0].hide_in"),
    (339, "objects[0].hitbox.h"),
    (340, "objects[0].hitbox.w"),
    (341, "objects[0].hitbox.x"),
    (342, "objects[0].hitbox.y"),
    (343, "objects[0].rem.x"),
    (344, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (345, "objects[0].spd.x"),
    (346, "objects[0].spd.y"),
    (253, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (260, "objects[1].delay"),
    (347, "objects[1].flip.x"),
    (348, "objects[1].flip.y"),
    (262, "objects[1].hide_for"),
    (263, "objects[1].hide_in"),
    (349, "objects[1].hitbox.h"),
    (350, "objects[1].hitbox.w"),
    (351, "objects[1].hitbox.x"),
    (352, "objects[1].hitbox.y"),
    (353, "objects[1].rem.x"),
    (354, "objects[1].rem.y"),
    (271, "objects[1].solids"),
    (355, "objects[1].spd.x"),
    (356, "objects[1].spd.y"),
    (273, "objects[1].spr"),
    (275, "objects[1].x"),
    (276, "objects[1].y"),
    (279, "objects[2].collideable"),
    (357, "objects[2].dash_accel.x"),
    (358, "objects[2].dash_accel.y"),
    (281, "objects[2].dash_effect_time"),
    (359, "objects[2].dash_target.x"),
    (360, "objects[2].dash_target.y"),
    (283, "objects[2].dash_time"),
    (284, "objects[2].djump"),
    (361, "objects[2].flip.x"),
    (362, "objects[2].flip.y"),
    (286, "objects[2].grace"),
    (363, "objects[2].hitbox.h"),
    (364, "objects[2].hitbox.w"),
    (365, "objects[2].hitbox.x"),
    (366, "objects[2].hitbox.y"),
    (293, "objects[2].p_dash"),
    (294, "objects[2].p_jump"),
    (367, "objects[2].rem.x"),
    (368, "objects[2].rem.y"),
    (296, "objects[2].solids"),
    (369, "objects[2].spd.x"),
    (370, "objects[2].spd.y"),
    (300, "objects[2].x"),
    (301, "objects[2].y"),
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
    SCell::Obj(&[(21, 238), (20, 239), (11, 240), (14, 241), (45, 242), (42, 243), (15, 244), (19, 245), (18, 246), (22, 247), (23, 248), (24, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (35, 260), (14, 261), (45, 262), (42, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (0, 274), (1, 275), (2, 276)]),
    SCell::Obj(&[(21, 277), (20, 278), (11, 279), (28, 280), (33, 281), (27, 282), (26, 283), (30, 284), (14, 285), (29, 286), (15, 287), (19, 288), (18, 289), (22, 290), (23, 291), (24, 292), (32, 293), (31, 294), (4, 295), (12, 296), (3, 297), (13, 298), (0, 299), (1, 300), (2, 301)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 337), (2, 338)]),
    SCell::Obj(&[(17, 339), (16, 340), (1, 341), (2, 342)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 343), (2, 344)]),
    SCell::Obj(&[(1, 345), (2, 346)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 347), (2, 348)]),
    SCell::Obj(&[(17, 349), (16, 350), (1, 351), (2, 352)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 353), (2, 354)]),
    SCell::Obj(&[(1, 355), (2, 356)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 357), (2, 358)]),
    SCell::Obj(&[(1, 359), (2, 360)]),
    SCell::Obj(&[(1, 361), (2, 362)]),
    SCell::Obj(&[(17, 363), (16, 364), (1, 365), (2, 366)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 367), (2, 368)]),
    SCell::Obj(&[(1, 369), (2, 370)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (238, 302),
    (239, 303),
    (241, 304),
    (244, 305),
    (245, 306),
    (246, 307),
    (247, 308),
    (248, 309),
    (249, 310),
    (250, 311),
    (252, 312),
    (254, 116),
    (257, 313),
    (258, 314),
    (261, 315),
    (264, 316),
    (265, 317),
    (266, 318),
    (267, 319),
    (268, 320),
    (269, 321),
    (270, 322),
    (272, 323),
    (274, 116),
    (277, 324),
    (278, 325),
    (280, 326),
    (282, 327),
    (285, 328),
    (287, 329),
    (288, 330),
    (289, 331),
    (290, 332),
    (291, 333),
    (292, 334),
    (295, 335),
    (297, 336),
    (299, 93),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c367: ZN,
    pub c368: ZN,
    pub c301: ZN,
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
    pub c357: ZN,
    pub c358: ZN,
    pub c281: ZN,
    pub c359: ZN,
    pub c360: ZN,
    pub c283: ZN,
    pub c284: ZN,
    pub c361: ZB,
    pub c286: ZN,
    pub c293: ZB,
    pub c294: ZB,
    pub c369: ZN,
    pub c370: ZN,
    pub c300: ZN,
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
    (238, "objects[0].collideable"),
    (365, "objects[0].flip.x"),
    (366, "objects[0].flip.y"),
    (367, "objects[0].hitbox.h"),
    (368, "objects[0].hitbox.w"),
    (369, "objects[0].hitbox.x"),
    (370, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (371, "objects[0].rem.x"),
    (372, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (373, "objects[0].spd.x"),
    (374, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (258, "objects[1].delay"),
    (375, "objects[1].flip.x"),
    (376, "objects[1].flip.y"),
    (260, "objects[1].hide_for"),
    (261, "objects[1].hide_in"),
    (377, "objects[1].hitbox.h"),
    (378, "objects[1].hitbox.w"),
    (379, "objects[1].hitbox.x"),
    (380, "objects[1].hitbox.y"),
    (381, "objects[1].rem.x"),
    (382, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (383, "objects[1].spd.x"),
    (384, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (277, "objects[2].collideable"),
    (385, "objects[2].flip.x"),
    (386, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (387, "objects[2].hitbox.h"),
    (388, "objects[2].hitbox.w"),
    (389, "objects[2].hitbox.x"),
    (390, "objects[2].hitbox.y"),
    (391, "objects[2].rem.x"),
    (392, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (393, "objects[2].spd.x"),
    (394, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (395, "objects[3].dash_accel.x"),
    (396, "objects[3].dash_accel.y"),
    (298, "objects[3].dash_effect_time"),
    (397, "objects[3].dash_target.x"),
    (398, "objects[3].dash_target.y"),
    (300, "objects[3].dash_time"),
    (301, "objects[3].djump"),
    (399, "objects[3].flip.x"),
    (400, "objects[3].flip.y"),
    (303, "objects[3].grace"),
    (401, "objects[3].hitbox.h"),
    (402, "objects[3].hitbox.w"),
    (403, "objects[3].hitbox.x"),
    (404, "objects[3].hitbox.y"),
    (310, "objects[3].p_dash"),
    (311, "objects[3].p_jump"),
    (405, "objects[3].rem.x"),
    (406, "objects[3].rem.y"),
    (313, "objects[3].solids"),
    (407, "objects[3].spd.x"),
    (408, "objects[3].spd.y"),
    (317, "objects[3].x"),
    (318, "objects[3].y"),
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
    SCell::Obj(&[(21, 236), (20, 237), (11, 238), (14, 239), (15, 240), (19, 241), (18, 242), (22, 243), (23, 244), (24, 245), (41, 246), (4, 247), (12, 248), (3, 249), (13, 250), (38, 251), (0, 252), (1, 253), (2, 254)]),
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (35, 258), (14, 259), (45, 260), (42, 261), (15, 262), (19, 263), (18, 264), (22, 265), (23, 266), (24, 267), (4, 268), (12, 269), (3, 270), (13, 271), (0, 272), (1, 273), (2, 274)]),
    SCell::Obj(&[(21, 275), (20, 276), (11, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (28, 297), (33, 298), (27, 299), (26, 300), (30, 301), (14, 302), (29, 303), (15, 304), (19, 305), (18, 306), (22, 307), (23, 308), (24, 309), (32, 310), (31, 311), (4, 312), (12, 313), (3, 314), (13, 315), (0, 316), (1, 317), (2, 318)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 365), (2, 366)]),
    SCell::Obj(&[(17, 367), (16, 368), (1, 369), (2, 370)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 371), (2, 372)]),
    SCell::Obj(&[(1, 373), (2, 374)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 375), (2, 376)]),
    SCell::Obj(&[(17, 377), (16, 378), (1, 379), (2, 380)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 381), (2, 382)]),
    SCell::Obj(&[(1, 383), (2, 384)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 385), (2, 386)]),
    SCell::Obj(&[(17, 387), (16, 388), (1, 389), (2, 390)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 391), (2, 392)]),
    SCell::Obj(&[(1, 393), (2, 394)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 395), (2, 396)]),
    SCell::Obj(&[(1, 397), (2, 398)]),
    SCell::Obj(&[(1, 399), (2, 400)]),
    SCell::Obj(&[(17, 401), (16, 402), (1, 403), (2, 404)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 405), (2, 406)]),
    SCell::Obj(&[(1, 407), (2, 408)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (236, 319),
    (237, 320),
    (239, 321),
    (240, 322),
    (241, 323),
    (242, 324),
    (243, 325),
    (244, 326),
    (245, 327),
    (247, 328),
    (249, 329),
    (252, 121),
    (255, 330),
    (256, 331),
    (259, 332),
    (262, 333),
    (263, 334),
    (264, 335),
    (265, 336),
    (266, 337),
    (267, 338),
    (268, 339),
    (270, 340),
    (272, 116),
    (275, 341),
    (276, 342),
    (278, 343),
    (281, 344),
    (282, 345),
    (283, 346),
    (284, 347),
    (285, 348),
    (286, 349),
    (287, 350),
    (289, 351),
    (291, 116),
    (294, 352),
    (295, 353),
    (297, 354),
    (299, 355),
    (302, 356),
    (304, 357),
    (305, 358),
    (306, 359),
    (307, 360),
    (308, 361),
    (309, 362),
    (312, 363),
    (314, 364),
    (316, 93),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c246: ZN,
    pub c254: ZN,
    pub c405: ZN,
    pub c406: ZN,
    pub c318: ZN,
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
    pub c395: ZN,
    pub c396: ZN,
    pub c298: ZN,
    pub c397: ZN,
    pub c398: ZN,
    pub c300: ZN,
    pub c301: ZN,
    pub c399: ZB,
    pub c303: ZN,
    pub c310: ZB,
    pub c311: ZB,
    pub c407: ZN,
    pub c408: ZN,
    pub c317: ZN,
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
    (365, "objects[0].flip.x"),
    (366, "objects[0].flip.y"),
    (367, "objects[0].hitbox.h"),
    (368, "objects[0].hitbox.w"),
    (369, "objects[0].hitbox.x"),
    (370, "objects[0].hitbox.y"),
    (246, "objects[0].off"),
    (371, "objects[0].rem.x"),
    (372, "objects[0].rem.y"),
    (248, "objects[0].solids"),
    (373, "objects[0].spd.x"),
    (374, "objects[0].spd.y"),
    (250, "objects[0].spr"),
    (251, "objects[0].start"),
    (253, "objects[0].x"),
    (254, "objects[0].y"),
    (257, "objects[1].collideable"),
    (375, "objects[1].flip.x"),
    (376, "objects[1].flip.y"),
    (259, "objects[1].hide_for"),
    (260, "objects[1].hide_in"),
    (377, "objects[1].hitbox.h"),
    (378, "objects[1].hitbox.w"),
    (379, "objects[1].hitbox.x"),
    (380, "objects[1].hitbox.y"),
    (381, "objects[1].rem.x"),
    (382, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (383, "objects[1].spd.x"),
    (384, "objects[1].spd.y"),
    (270, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (276, "objects[2].collideable"),
    (277, "objects[2].delay"),
    (385, "objects[2].flip.x"),
    (386, "objects[2].flip.y"),
    (279, "objects[2].hide_for"),
    (280, "objects[2].hide_in"),
    (387, "objects[2].hitbox.h"),
    (388, "objects[2].hitbox.w"),
    (389, "objects[2].hitbox.x"),
    (390, "objects[2].hitbox.y"),
    (391, "objects[2].rem.x"),
    (392, "objects[2].rem.y"),
    (288, "objects[2].solids"),
    (393, "objects[2].spd.x"),
    (394, "objects[2].spd.y"),
    (290, "objects[2].spr"),
    (292, "objects[2].x"),
    (293, "objects[2].y"),
    (296, "objects[3].collideable"),
    (395, "objects[3].dash_accel.x"),
    (396, "objects[3].dash_accel.y"),
    (298, "objects[3].dash_effect_time"),
    (397, "objects[3].dash_target.x"),
    (398, "objects[3].dash_target.y"),
    (300, "objects[3].dash_time"),
    (301, "objects[3].djump"),
    (399, "objects[3].flip.x"),
    (400, "objects[3].flip.y"),
    (303, "objects[3].grace"),
    (401, "objects[3].hitbox.h"),
    (402, "objects[3].hitbox.w"),
    (403, "objects[3].hitbox.x"),
    (404, "objects[3].hitbox.y"),
    (310, "objects[3].p_dash"),
    (311, "objects[3].p_jump"),
    (405, "objects[3].rem.x"),
    (406, "objects[3].rem.y"),
    (313, "objects[3].solids"),
    (407, "objects[3].spd.x"),
    (408, "objects[3].spd.y"),
    (317, "objects[3].x"),
    (318, "objects[3].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
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
    SCell::Obj(&[(21, 255), (20, 256), (11, 257), (14, 258), (45, 259), (42, 260), (15, 261), (19, 262), (18, 263), (22, 264), (23, 265), (24, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
    SCell::Obj(&[(21, 274), (20, 275), (11, 276), (35, 277), (14, 278), (45, 279), (42, 280), (15, 281), (19, 282), (18, 283), (22, 284), (23, 285), (24, 286), (4, 287), (12, 288), (3, 289), (13, 290), (0, 291), (1, 292), (2, 293)]),
    SCell::Obj(&[(21, 294), (20, 295), (11, 296), (28, 297), (33, 298), (27, 299), (26, 300), (30, 301), (14, 302), (29, 303), (15, 304), (19, 305), (18, 306), (22, 307), (23, 308), (24, 309), (32, 310), (31, 311), (4, 312), (12, 313), (3, 314), (13, 315), (0, 316), (1, 317), (2, 318)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 365), (2, 366)]),
    SCell::Obj(&[(17, 367), (16, 368), (1, 369), (2, 370)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 371), (2, 372)]),
    SCell::Obj(&[(1, 373), (2, 374)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 375), (2, 376)]),
    SCell::Obj(&[(17, 377), (16, 378), (1, 379), (2, 380)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 381), (2, 382)]),
    SCell::Obj(&[(1, 383), (2, 384)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 385), (2, 386)]),
    SCell::Obj(&[(17, 387), (16, 388), (1, 389), (2, 390)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 391), (2, 392)]),
    SCell::Obj(&[(1, 393), (2, 394)]),
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 395), (2, 396)]),
    SCell::Obj(&[(1, 397), (2, 398)]),
    SCell::Obj(&[(1, 399), (2, 400)]),
    SCell::Obj(&[(17, 401), (16, 402), (1, 403), (2, 404)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 405), (2, 406)]),
    SCell::Obj(&[(1, 407), (2, 408)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (236, 319),
    (237, 320),
    (239, 321),
    (240, 322),
    (241, 323),
    (242, 324),
    (243, 325),
    (244, 326),
    (245, 327),
    (247, 328),
    (249, 329),
    (252, 121),
    (255, 330),
    (256, 331),
    (258, 332),
    (261, 333),
    (262, 334),
    (263, 335),
    (264, 336),
    (265, 337),
    (266, 338),
    (267, 339),
    (269, 340),
    (271, 116),
    (274, 341),
    (275, 342),
    (278, 343),
    (281, 344),
    (282, 345),
    (283, 346),
    (284, 347),
    (285, 348),
    (286, 349),
    (287, 350),
    (289, 351),
    (291, 116),
    (294, 352),
    (295, 353),
    (297, 354),
    (299, 355),
    (302, 356),
    (304, 357),
    (305, 358),
    (306, 359),
    (307, 360),
    (308, 361),
    (309, 362),
    (312, 363),
    (314, 364),
    (316, 93),
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
    pub c246: ZN,
    pub c254: ZN,
    pub c405: ZN,
    pub c406: ZN,
    pub c318: ZN,
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
    pub c20: ZN,
    pub c41: ZB,
    pub c395: ZN,
    pub c396: ZN,
    pub c298: ZN,
    pub c397: ZN,
    pub c398: ZN,
    pub c300: ZN,
    pub c301: ZN,
    pub c399: ZB,
    pub c303: ZN,
    pub c310: ZB,
    pub c311: ZB,
    pub c407: ZN,
    pub c408: ZN,
    pub c317: ZN,
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
    (298, "objects[0].flip.x"),
    (299, "objects[0].flip.y"),
    (242, "objects[0].hide_for"),
    (243, "objects[0].hide_in"),
    (300, "objects[0].hitbox.h"),
    (301, "objects[0].hitbox.w"),
    (302, "objects[0].hitbox.x"),
    (303, "objects[0].hitbox.y"),
    (304, "objects[0].rem.x"),
    (305, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (306, "objects[0].spd.x"),
    (307, "objects[0].spd.y"),
    (253, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (308, "objects[1].flip.x"),
    (309, "objects[1].flip.y"),
    (261, "objects[1].hide_for"),
    (262, "objects[1].hide_in"),
    (310, "objects[1].hitbox.h"),
    (311, "objects[1].hitbox.w"),
    (312, "objects[1].hitbox.x"),
    (313, "objects[1].hitbox.y"),
    (314, "objects[1].rem.x"),
    (315, "objects[1].rem.y"),
    (270, "objects[1].solids"),
    (316, "objects[1].spd.x"),
    (317, "objects[1].spd.y"),
    (272, "objects[1].spr"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
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
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (14, 260), (45, 261), (42, 262), (15, 263), (19, 264), (18, 265), (22, 266), (23, 267), (24, 268), (4, 269), (12, 270), (3, 271), (13, 272), (0, 273), (1, 274), (2, 275)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(17, 300), (16, 301), (1, 302), (2, 303)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(1, 306), (2, 307)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(17, 310), (16, 311), (1, 312), (2, 313)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 314), (2, 315)]),
    SCell::Obj(&[(1, 316), (2, 317)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (237, 276),
    (238, 277),
    (241, 278),
    (244, 279),
    (245, 280),
    (246, 281),
    (247, 282),
    (248, 283),
    (249, 284),
    (250, 285),
    (252, 286),
    (254, 116),
    (257, 287),
    (258, 288),
    (260, 289),
    (263, 290),
    (264, 291),
    (265, 292),
    (266, 293),
    (267, 294),
    (268, 295),
    (269, 296),
    (271, 297),
    (273, 116),
];

/// Outcome 4's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared4 {
    pub c87: ZN,
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

// ---------------- outcome 5 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_5: &[(u32, &str)] = &[
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
    (298, "objects[0].flip.x"),
    (299, "objects[0].flip.y"),
    (241, "objects[0].hide_for"),
    (242, "objects[0].hide_in"),
    (300, "objects[0].hitbox.h"),
    (301, "objects[0].hitbox.w"),
    (302, "objects[0].hitbox.x"),
    (303, "objects[0].hitbox.y"),
    (304, "objects[0].rem.x"),
    (305, "objects[0].rem.y"),
    (250, "objects[0].solids"),
    (306, "objects[0].spd.x"),
    (307, "objects[0].spd.y"),
    (252, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (254, "objects[0].x"),
    (255, "objects[0].y"),
    (258, "objects[1].collideable"),
    (259, "objects[1].delay"),
    (308, "objects[1].flip.x"),
    (309, "objects[1].flip.y"),
    (261, "objects[1].hide_for"),
    (262, "objects[1].hide_in"),
    (310, "objects[1].hitbox.h"),
    (311, "objects[1].hitbox.w"),
    (312, "objects[1].hitbox.x"),
    (313, "objects[1].hitbox.y"),
    (314, "objects[1].rem.x"),
    (315, "objects[1].rem.y"),
    (270, "objects[1].solids"),
    (316, "objects[1].spd.x"),
    (317, "objects[1].spd.y"),
    (272, "objects[1].spr"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
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
    SCell::Obj(&[(21, 237), (20, 238), (11, 239), (14, 240), (45, 241), (42, 242), (15, 243), (19, 244), (18, 245), (22, 246), (23, 247), (24, 248), (4, 249), (12, 250), (3, 251), (13, 252), (0, 253), (1, 254), (2, 255)]),
    SCell::Obj(&[(21, 256), (20, 257), (11, 258), (35, 259), (14, 260), (45, 261), (42, 262), (15, 263), (19, 264), (18, 265), (22, 266), (23, 267), (24, 268), (4, 269), (12, 270), (3, 271), (13, 272), (0, 273), (1, 274), (2, 275)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(17, 300), (16, 301), (1, 302), (2, 303)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(1, 306), (2, 307)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(17, 310), (16, 311), (1, 312), (2, 313)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 314), (2, 315)]),
    SCell::Obj(&[(1, 316), (2, 317)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (237, 276),
    (238, 277),
    (240, 278),
    (243, 279),
    (244, 280),
    (245, 281),
    (246, 282),
    (247, 283),
    (248, 284),
    (249, 285),
    (251, 286),
    (253, 116),
    (256, 287),
    (257, 288),
    (260, 289),
    (263, 290),
    (264, 291),
    (265, 292),
    (266, 293),
    (267, 294),
    (268, 295),
    (269, 296),
    (271, 297),
    (273, 116),
];

/// Outcome 5's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared5 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
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
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 6 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_6: &[(u32, &str)] = &[
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
    (326, "objects[0].flip.x"),
    (327, "objects[0].flip.y"),
    (328, "objects[0].hitbox.h"),
    (329, "objects[0].hitbox.w"),
    (330, "objects[0].hitbox.x"),
    (331, "objects[0].hitbox.y"),
    (245, "objects[0].off"),
    (332, "objects[0].rem.x"),
    (333, "objects[0].rem.y"),
    (247, "objects[0].solids"),
    (334, "objects[0].spd.x"),
    (335, "objects[0].spd.y"),
    (249, "objects[0].spr"),
    (250, "objects[0].start"),
    (252, "objects[0].x"),
    (253, "objects[0].y"),
    (256, "objects[1].collideable"),
    (257, "objects[1].delay"),
    (336, "objects[1].flip.x"),
    (337, "objects[1].flip.y"),
    (259, "objects[1].hide_for"),
    (260, "objects[1].hide_in"),
    (338, "objects[1].hitbox.h"),
    (339, "objects[1].hitbox.w"),
    (340, "objects[1].hitbox.x"),
    (341, "objects[1].hitbox.y"),
    (342, "objects[1].rem.x"),
    (343, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (344, "objects[1].spd.x"),
    (345, "objects[1].spd.y"),
    (270, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (276, "objects[2].collideable"),
    (346, "objects[2].flip.x"),
    (347, "objects[2].flip.y"),
    (278, "objects[2].hide_for"),
    (279, "objects[2].hide_in"),
    (348, "objects[2].hitbox.h"),
    (349, "objects[2].hitbox.w"),
    (350, "objects[2].hitbox.x"),
    (351, "objects[2].hitbox.y"),
    (352, "objects[2].rem.x"),
    (353, "objects[2].rem.y"),
    (287, "objects[2].solids"),
    (354, "objects[2].spd.x"),
    (355, "objects[2].spd.y"),
    (289, "objects[2].spr"),
    (291, "objects[2].x"),
    (292, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_6: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_6: &[SCell] = &[
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
    SCell::Obj(&[(21, 274), (20, 275), (11, 276), (14, 277), (45, 278), (42, 279), (15, 280), (19, 281), (18, 282), (22, 283), (23, 284), (24, 285), (4, 286), (12, 287), (3, 288), (13, 289), (0, 290), (1, 291), (2, 292)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 326), (2, 327)]),
    SCell::Obj(&[(17, 328), (16, 329), (1, 330), (2, 331)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 332), (2, 333)]),
    SCell::Obj(&[(1, 334), (2, 335)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 336), (2, 337)]),
    SCell::Obj(&[(17, 338), (16, 339), (1, 340), (2, 341)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 342), (2, 343)]),
    SCell::Obj(&[(1, 344), (2, 345)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 346), (2, 347)]),
    SCell::Obj(&[(17, 348), (16, 349), (1, 350), (2, 351)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 352), (2, 353)]),
    SCell::Obj(&[(1, 354), (2, 355)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_6: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_6: &[(u32, u32)] = &[
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
    (235, 293),
    (236, 294),
    (238, 295),
    (239, 296),
    (240, 297),
    (241, 298),
    (242, 299),
    (243, 300),
    (244, 301),
    (246, 302),
    (248, 303),
    (251, 121),
    (254, 304),
    (255, 305),
    (258, 306),
    (261, 307),
    (262, 308),
    (263, 309),
    (264, 310),
    (265, 311),
    (266, 312),
    (267, 313),
    (269, 314),
    (271, 116),
    (274, 315),
    (275, 316),
    (277, 317),
    (280, 318),
    (281, 319),
    (282, 320),
    (283, 321),
    (284, 322),
    (285, 323),
    (286, 324),
    (288, 325),
    (290, 116),
];

/// Outcome 6's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared6 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c85: ZN,
}

/// Outcome 6's per-assignment values and lane masks.
/// The cells of outcome 6 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared6`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut6 {
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

// ---------------- outcome 7 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_7: &[(u32, &str)] = &[
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
    (326, "objects[0].flip.x"),
    (327, "objects[0].flip.y"),
    (328, "objects[0].hitbox.h"),
    (329, "objects[0].hitbox.w"),
    (330, "objects[0].hitbox.x"),
    (331, "objects[0].hitbox.y"),
    (245, "objects[0].off"),
    (332, "objects[0].rem.x"),
    (333, "objects[0].rem.y"),
    (247, "objects[0].solids"),
    (334, "objects[0].spd.x"),
    (335, "objects[0].spd.y"),
    (249, "objects[0].spr"),
    (250, "objects[0].start"),
    (252, "objects[0].x"),
    (253, "objects[0].y"),
    (256, "objects[1].collideable"),
    (336, "objects[1].flip.x"),
    (337, "objects[1].flip.y"),
    (258, "objects[1].hide_for"),
    (259, "objects[1].hide_in"),
    (338, "objects[1].hitbox.h"),
    (339, "objects[1].hitbox.w"),
    (340, "objects[1].hitbox.x"),
    (341, "objects[1].hitbox.y"),
    (342, "objects[1].rem.x"),
    (343, "objects[1].rem.y"),
    (267, "objects[1].solids"),
    (344, "objects[1].spd.x"),
    (345, "objects[1].spd.y"),
    (269, "objects[1].spr"),
    (174, "objects[1].type.tile"),
    (271, "objects[1].x"),
    (272, "objects[1].y"),
    (275, "objects[2].collideable"),
    (276, "objects[2].delay"),
    (346, "objects[2].flip.x"),
    (347, "objects[2].flip.y"),
    (278, "objects[2].hide_for"),
    (279, "objects[2].hide_in"),
    (348, "objects[2].hitbox.h"),
    (349, "objects[2].hitbox.w"),
    (350, "objects[2].hitbox.x"),
    (351, "objects[2].hitbox.y"),
    (352, "objects[2].rem.x"),
    (353, "objects[2].rem.y"),
    (287, "objects[2].solids"),
    (354, "objects[2].spd.x"),
    (355, "objects[2].spd.y"),
    (289, "objects[2].spr"),
    (291, "objects[2].x"),
    (292, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_7: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_7: &[SCell] = &[
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
    SCell::Obj(&[(21, 273), (20, 274), (11, 275), (35, 276), (14, 277), (45, 278), (42, 279), (15, 280), (19, 281), (18, 282), (22, 283), (23, 284), (24, 285), (4, 286), (12, 287), (3, 288), (13, 289), (0, 290), (1, 291), (2, 292)]),
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
    SCell::Clo(26, &[207]),
    SCell::Clo(25, &[207]),
    SCell::Obj(&[(1, 326), (2, 327)]),
    SCell::Obj(&[(17, 328), (16, 329), (1, 330), (2, 331)]),
    SCell::Clo(24, &[207]),
    SCell::Clo(23, &[207]),
    SCell::Clo(27, &[207]),
    SCell::Clo(28, &[207]),
    SCell::Clo(29, &[207]),
    SCell::Obj(&[(1, 332), (2, 333)]),
    SCell::Obj(&[(1, 334), (2, 335)]),
    SCell::Clo(26, &[208]),
    SCell::Clo(25, &[208]),
    SCell::Obj(&[(1, 336), (2, 337)]),
    SCell::Obj(&[(17, 338), (16, 339), (1, 340), (2, 341)]),
    SCell::Clo(24, &[208]),
    SCell::Clo(23, &[208]),
    SCell::Clo(27, &[208]),
    SCell::Clo(28, &[208]),
    SCell::Clo(29, &[208]),
    SCell::Obj(&[(1, 342), (2, 343)]),
    SCell::Obj(&[(1, 344), (2, 345)]),
    SCell::Clo(26, &[209]),
    SCell::Clo(25, &[209]),
    SCell::Obj(&[(1, 346), (2, 347)]),
    SCell::Obj(&[(17, 348), (16, 349), (1, 350), (2, 351)]),
    SCell::Clo(24, &[209]),
    SCell::Clo(23, &[209]),
    SCell::Clo(27, &[209]),
    SCell::Clo(28, &[209]),
    SCell::Clo(29, &[209]),
    SCell::Obj(&[(1, 352), (2, 353)]),
    SCell::Obj(&[(1, 354), (2, 355)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_7: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_7: &[(u32, u32)] = &[
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
    (235, 293),
    (236, 294),
    (238, 295),
    (239, 296),
    (240, 297),
    (241, 298),
    (242, 299),
    (243, 300),
    (244, 301),
    (246, 302),
    (248, 303),
    (251, 121),
    (254, 304),
    (255, 305),
    (257, 306),
    (260, 307),
    (261, 308),
    (262, 309),
    (263, 310),
    (264, 311),
    (265, 312),
    (266, 313),
    (268, 314),
    (270, 116),
    (273, 315),
    (274, 316),
    (277, 317),
    (280, 318),
    (281, 319),
    (282, 320),
    (283, 321),
    (284, 322),
    (285, 323),
    (286, 324),
    (288, 325),
    (290, 116),
];

/// Outcome 7's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared7 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c85: ZN,
}

/// Outcome 7's per-assignment values and lane masks.
/// The cells of outcome 7 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared7`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut7 {
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

// ---------------- outcome 8 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_8: &[(u32, &str)] = &[
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
    (336, "objects[0].flip.x"),
    (337, "objects[0].flip.y"),
    (242, "objects[0].hide_for"),
    (243, "objects[0].hide_in"),
    (338, "objects[0].hitbox.h"),
    (339, "objects[0].hitbox.w"),
    (340, "objects[0].hitbox.x"),
    (341, "objects[0].hitbox.y"),
    (342, "objects[0].rem.x"),
    (343, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (344, "objects[0].spd.x"),
    (345, "objects[0].spd.y"),
    (253, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (346, "objects[1].flip.x"),
    (347, "objects[1].flip.y"),
    (261, "objects[1].hide_for"),
    (262, "objects[1].hide_in"),
    (348, "objects[1].hitbox.h"),
    (349, "objects[1].hitbox.w"),
    (350, "objects[1].hitbox.x"),
    (351, "objects[1].hitbox.y"),
    (352, "objects[1].rem.x"),
    (353, "objects[1].rem.y"),
    (270, "objects[1].solids"),
    (354, "objects[1].spd.x"),
    (355, "objects[1].spd.y"),
    (272, "objects[1].spr"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
    (278, "objects[2].collideable"),
    (356, "objects[2].dash_accel.x"),
    (357, "objects[2].dash_accel.y"),
    (280, "objects[2].dash_effect_time"),
    (358, "objects[2].dash_target.x"),
    (359, "objects[2].dash_target.y"),
    (282, "objects[2].dash_time"),
    (283, "objects[2].djump"),
    (360, "objects[2].flip.x"),
    (361, "objects[2].flip.y"),
    (285, "objects[2].grace"),
    (362, "objects[2].hitbox.h"),
    (363, "objects[2].hitbox.w"),
    (364, "objects[2].hitbox.x"),
    (365, "objects[2].hitbox.y"),
    (292, "objects[2].p_dash"),
    (293, "objects[2].p_jump"),
    (366, "objects[2].rem.x"),
    (367, "objects[2].rem.y"),
    (295, "objects[2].solids"),
    (368, "objects[2].spd.x"),
    (369, "objects[2].spd.y"),
    (299, "objects[2].x"),
    (300, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_8: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_8: &[SCell] = &[
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
    SCell::Obj(&[(21, 238), (20, 239), (11, 240), (14, 241), (45, 242), (42, 243), (15, 244), (19, 245), (18, 246), (22, 247), (23, 248), (24, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (14, 260), (45, 261), (42, 262), (15, 263), (19, 264), (18, 265), (22, 266), (23, 267), (24, 268), (4, 269), (12, 270), (3, 271), (13, 272), (0, 273), (1, 274), (2, 275)]),
    SCell::Obj(&[(21, 276), (20, 277), (11, 278), (28, 279), (33, 280), (27, 281), (26, 282), (30, 283), (14, 284), (29, 285), (15, 286), (19, 287), (18, 288), (22, 289), (23, 290), (24, 291), (32, 292), (31, 293), (4, 294), (12, 295), (3, 296), (13, 297), (0, 298), (1, 299), (2, 300)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 336), (2, 337)]),
    SCell::Obj(&[(17, 338), (16, 339), (1, 340), (2, 341)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 342), (2, 343)]),
    SCell::Obj(&[(1, 344), (2, 345)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 346), (2, 347)]),
    SCell::Obj(&[(17, 348), (16, 349), (1, 350), (2, 351)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 352), (2, 353)]),
    SCell::Obj(&[(1, 354), (2, 355)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 356), (2, 357)]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(1, 360), (2, 361)]),
    SCell::Obj(&[(17, 362), (16, 363), (1, 364), (2, 365)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_8: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_8: &[(u32, u32)] = &[
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
    (238, 301),
    (239, 302),
    (241, 303),
    (244, 304),
    (245, 305),
    (246, 306),
    (247, 307),
    (248, 308),
    (249, 309),
    (250, 310),
    (252, 311),
    (254, 116),
    (257, 312),
    (258, 313),
    (260, 314),
    (263, 315),
    (264, 316),
    (265, 317),
    (266, 318),
    (267, 319),
    (268, 320),
    (269, 321),
    (271, 322),
    (273, 116),
    (276, 323),
    (277, 324),
    (279, 325),
    (281, 326),
    (284, 327),
    (286, 328),
    (287, 329),
    (288, 330),
    (289, 331),
    (290, 332),
    (291, 333),
    (294, 334),
    (296, 335),
    (298, 93),
];

/// Outcome 8's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared8 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c366: ZN,
    pub c367: ZN,
    pub c300: ZN,
    pub c85: ZN,
}

/// Outcome 8's per-assignment values and lane masks.
/// The cells of outcome 8 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared8`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut8 {
    pub c20: ZN,
    pub c41: ZB,
    pub c356: ZN,
    pub c357: ZN,
    pub c280: ZN,
    pub c358: ZN,
    pub c359: ZN,
    pub c282: ZN,
    pub c283: ZN,
    pub c360: ZB,
    pub c285: ZN,
    pub c292: ZB,
    pub c293: ZB,
    pub c368: ZN,
    pub c369: ZN,
    pub c299: ZN,
    /// This successor's ROW KEY, both halves, 16 lanes at
    /// once. Folded by the graph rather than by `append`:
    /// the fold is sequential over cells but every step is
    /// a vector, and its button-independent prefix is one
    /// shared chain across all the assignments instead of
    /// being recomputed per candidate row.
    pub h1: ZW,
    pub h2: ZW,
}

// ---------------- outcome 9 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_9: &[(u32, &str)] = &[
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
    (297, "objects[0].flip.x"),
    (298, "objects[0].flip.y"),
    (241, "objects[0].hide_for"),
    (242, "objects[0].hide_in"),
    (299, "objects[0].hitbox.h"),
    (300, "objects[0].hitbox.w"),
    (301, "objects[0].hitbox.x"),
    (302, "objects[0].hitbox.y"),
    (303, "objects[0].rem.x"),
    (304, "objects[0].rem.y"),
    (250, "objects[0].solids"),
    (305, "objects[0].spd.x"),
    (306, "objects[0].spd.y"),
    (252, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (254, "objects[0].x"),
    (255, "objects[0].y"),
    (258, "objects[1].collideable"),
    (307, "objects[1].flip.x"),
    (308, "objects[1].flip.y"),
    (260, "objects[1].hide_for"),
    (261, "objects[1].hide_in"),
    (309, "objects[1].hitbox.h"),
    (310, "objects[1].hitbox.w"),
    (311, "objects[1].hitbox.x"),
    (312, "objects[1].hitbox.y"),
    (313, "objects[1].rem.x"),
    (314, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (315, "objects[1].spd.x"),
    (316, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
    (38, "will_restart"),
];

/// Cells that end the frame as a fresh UnknownBool - next
/// frame's button inputs. The boundary writes UBool, no data.
pub const OUT_UBOOL_9: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_9: &[SCell] = &[
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
    SCell::Obj(&[(21, 237), (20, 238), (11, 239), (14, 240), (45, 241), (42, 242), (15, 243), (19, 244), (18, 245), (22, 246), (23, 247), (24, 248), (4, 249), (12, 250), (3, 251), (13, 252), (0, 253), (1, 254), (2, 255)]),
    SCell::Obj(&[(21, 256), (20, 257), (11, 258), (14, 259), (45, 260), (42, 261), (15, 262), (19, 263), (18, 264), (22, 265), (23, 266), (24, 267), (4, 268), (12, 269), (3, 270), (13, 271), (0, 272), (1, 273), (2, 274)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 297), (2, 298)]),
    SCell::Obj(&[(17, 299), (16, 300), (1, 301), (2, 302)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 303), (2, 304)]),
    SCell::Obj(&[(1, 305), (2, 306)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 307), (2, 308)]),
    SCell::Obj(&[(17, 309), (16, 310), (1, 311), (2, 312)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 313), (2, 314)]),
    SCell::Obj(&[(1, 315), (2, 316)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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

pub const OUT_GLOBALS_9: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_9: &[(u32, u32)] = &[
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
    (237, 275),
    (238, 276),
    (240, 277),
    (243, 278),
    (244, 279),
    (245, 280),
    (246, 281),
    (247, 282),
    (248, 283),
    (249, 284),
    (251, 285),
    (253, 116),
    (256, 286),
    (257, 287),
    (259, 288),
    (262, 289),
    (263, 290),
    (264, 291),
    (265, 292),
    (266, 293),
    (267, 294),
    (268, 295),
    (270, 296),
    (272, 116),
];

/// Outcome 9's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared9 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
}

/// Outcome 9's per-assignment values and lane masks.
/// The cells of outcome 9 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared9`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut9 {
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

// ---------------- outcome 10 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_10: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_10: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_10: &[SCell] = &[
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

pub const OUT_GLOBALS_10: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_10: &[(u32, u32)] = &[
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

/// Outcome 10's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared10 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c85: ZN,
}

/// Outcome 10's per-assignment values and lane masks.
/// The cells of outcome 10 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared10`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut10 {
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

// ---------------- outcome 11 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_11: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_11: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_11: &[SCell] = &[
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

pub const OUT_GLOBALS_11: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_11: &[(u32, u32)] = &[
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

/// Outcome 11's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared11 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
}

/// Outcome 11's per-assignment values and lane masks.
/// The cells of outcome 11 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared11`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut11 {
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

// ---------------- outcome 12 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_12: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_12: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_12: &[SCell] = &[
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

pub const OUT_GLOBALS_12: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_12: &[(u32, u32)] = &[
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

/// Outcome 12's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared12 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
}

/// Outcome 12's per-assignment values and lane masks.
/// The cells of outcome 12 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared12`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut12 {
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

// ---------------- outcome 13 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_13: &[(u32, &str)] = &[
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
pub const OUT_UBOOL_13: &[(u32, &str)] = &[
    (145, "__button_states[0]"),
    (146, "__button_states[1]"),
    (147, "__button_states[2]"),
    (148, "__button_states[3]"),
    (149, "__button_states[4]"),
    (150, "__button_states[5]"),
];

pub const OUT_SHAPE_13: &[SCell] = &[
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

pub const OUT_GLOBALS_13: &[u32] = &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 4294967295, 4294967295, 4294967295];

/// (cell, target) - the pointer topology, fixed by the shape.
pub const OUT_PTRS_13: &[(u32, u32)] = &[
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

/// Outcome 13's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared13 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c246: ZN,
    pub c254: ZN,
    pub c404: ZN,
    pub c405: ZN,
    pub c317: ZN,
    pub c85: ZN,
}

/// Outcome 13's per-assignment values and lane masks.
/// The cells of outcome 13 that DIFFER between button
/// assignments. Everything else is either constant (written
/// once when the block is built) or shared (`KShared13`).
///
/// No `live`/`deopt` here: which lanes a group writes is its
/// `take` argument, and declined lanes are accumulated by
/// `frame` itself.
pub struct KOut13 {
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
    pub c316: ZN,
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
    b.cols[241] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[338] = Col::U(AV::Bool(false));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[252] = Col::U(AV::Bool(true));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[346] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[257] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[260] = Col::U(AV::Bool(true));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[348] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(true));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[356] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[279] = Col::U(AV::Bool(true));
    b.cols[357] = Col::N(Vec::new());
    b.cols[358] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[359] = Col::N(Vec::new());
    b.cols[360] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[284] = Col::N(Vec::new());
    b.cols[361] = Col::V(Vec::new());
    b.cols[362] = Col::U(AV::Bool(false));
    b.cols[286] = Col::N(Vec::new());
    b.cols[363] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[293] = Col::V(Vec::new());
    b.cols[294] = Col::V(Vec::new());
    b.cols[367] = Col::N(Vec::new());
    b.cols[368] = Col::N(Vec::new());
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[369] = Col::N(Vec::new());
    b.cols[370] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
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
pub const KPART1_0: u64 = 17310884889625053153;
pub const KPART2_0: u64 = 8678418314782866327;

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
        if let Col::N(v) = &mut acc.cols[357] { v.push(kv.c357.lane(i)); }
        if let Col::N(v) = &mut acc.cols[358] { v.push(kv.c358.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(kv.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[359] { v.push(kv.c359.lane(i)); }
        if let Col::N(v) = &mut acc.cols[360] { v.push(kv.c360.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::N(v) = &mut acc.cols[284] { v.push(kv.c284.lane(i)); }
        if let Col::V(v) = &mut acc.cols[361] {
            v.push(if kv.c361.known & (1 << i) != 0 {
                AV::Bool(kv.c361.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[286] { v.push(kv.c286.lane(i)); }
        if let Col::V(v) = &mut acc.cols[293] {
            v.push(if kv.c293.known & (1 << i) != 0 {
                AV::Bool(kv.c293.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[294] {
            v.push(if kv.c294.known & (1 << i) != 0 {
                AV::Bool(kv.c294.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[367] { v.push(sh.c367.lane(i)); }
        if let Col::N(v) = &mut acc.cols[368] { v.push(sh.c368.lane(i)); }
        if let Col::N(v) = &mut acc.cols[369] { v.push(kv.c369.lane(i)); }
        if let Col::N(v) = &mut acc.cols[370] { v.push(kv.c370.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(sh.c301.lane(i)); }
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
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[338] = Col::U(AV::Bool(false));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[346] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[348] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(true));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[356] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[279] = Col::U(AV::Bool(true));
    b.cols[357] = Col::N(Vec::new());
    b.cols[358] = Col::N(Vec::new());
    b.cols[281] = Col::N(Vec::new());
    b.cols[359] = Col::N(Vec::new());
    b.cols[360] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[284] = Col::N(Vec::new());
    b.cols[361] = Col::V(Vec::new());
    b.cols[362] = Col::U(AV::Bool(false));
    b.cols[286] = Col::N(Vec::new());
    b.cols[363] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[293] = Col::V(Vec::new());
    b.cols[294] = Col::V(Vec::new());
    b.cols[367] = Col::N(Vec::new());
    b.cols[368] = Col::N(Vec::new());
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[369] = Col::N(Vec::new());
    b.cols[370] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(false));
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
pub const KPART1_1: u64 = 15990151978929535894;
pub const KPART2_1: u64 = 3831756090133193594;

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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[357] { v.push(kv.c357.lane(i)); }
        if let Col::N(v) = &mut acc.cols[358] { v.push(kv.c358.lane(i)); }
        if let Col::N(v) = &mut acc.cols[281] { v.push(kv.c281.lane(i)); }
        if let Col::N(v) = &mut acc.cols[359] { v.push(kv.c359.lane(i)); }
        if let Col::N(v) = &mut acc.cols[360] { v.push(kv.c360.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::N(v) = &mut acc.cols[284] { v.push(kv.c284.lane(i)); }
        if let Col::V(v) = &mut acc.cols[361] {
            v.push(if kv.c361.known & (1 << i) != 0 {
                AV::Bool(kv.c361.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[286] { v.push(kv.c286.lane(i)); }
        if let Col::V(v) = &mut acc.cols[293] {
            v.push(if kv.c293.known & (1 << i) != 0 {
                AV::Bool(kv.c293.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[294] {
            v.push(if kv.c294.known & (1 << i) != 0 {
                AV::Bool(kv.c294.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[367] { v.push(sh.c367.lane(i)); }
        if let Col::N(v) = &mut acc.cols[368] { v.push(sh.c368.lane(i)); }
        if let Col::N(v) = &mut acc.cols[369] { v.push(kv.c369.lane(i)); }
        if let Col::N(v) = &mut acc.cols[370] { v.push(kv.c370.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(sh.c301.lane(i)); }
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
    b.cols[365] = Col::U(AV::Bool(false));
    b.cols[366] = Col::U(AV::Bool(false));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[369] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::N(Vec::new());
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[375] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[380] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[381] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[277] = Col::U(AV::Bool(true));
    b.cols[385] = Col::U(AV::Bool(false));
    b.cols[386] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[391] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[395] = Col::N(Vec::new());
    b.cols[396] = Col::N(Vec::new());
    b.cols[298] = Col::N(Vec::new());
    b.cols[397] = Col::N(Vec::new());
    b.cols[398] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[399] = Col::V(Vec::new());
    b.cols[400] = Col::U(AV::Bool(false));
    b.cols[303] = Col::N(Vec::new());
    b.cols[401] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[402] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[403] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[404] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[310] = Col::V(Vec::new());
    b.cols[311] = Col::V(Vec::new());
    b.cols[405] = Col::N(Vec::new());
    b.cols[406] = Col::N(Vec::new());
    b.cols[313] = Col::U(AV::Bool(true));
    b.cols[407] = Col::N(Vec::new());
    b.cols[408] = Col::N(Vec::new());
    b.cols[317] = Col::N(Vec::new());
    b.cols[318] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(false));
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
pub const KPART1_2: u64 = 5055231813092918663;
pub const KPART2_2: u64 = 7605369903329149322;

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
        if let Col::N(v) = &mut acc.cols[395] { v.push(kv.c395.lane(i)); }
        if let Col::N(v) = &mut acc.cols[396] { v.push(kv.c396.lane(i)); }
        if let Col::N(v) = &mut acc.cols[298] { v.push(kv.c298.lane(i)); }
        if let Col::N(v) = &mut acc.cols[397] { v.push(kv.c397.lane(i)); }
        if let Col::N(v) = &mut acc.cols[398] { v.push(kv.c398.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::V(v) = &mut acc.cols[399] {
            v.push(if kv.c399.known & (1 << i) != 0 {
                AV::Bool(kv.c399.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[303] { v.push(kv.c303.lane(i)); }
        if let Col::V(v) = &mut acc.cols[310] {
            v.push(if kv.c310.known & (1 << i) != 0 {
                AV::Bool(kv.c310.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[311] {
            v.push(if kv.c311.known & (1 << i) != 0 {
                AV::Bool(kv.c311.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[405] { v.push(sh.c405.lane(i)); }
        if let Col::N(v) = &mut acc.cols[406] { v.push(sh.c406.lane(i)); }
        if let Col::N(v) = &mut acc.cols[407] { v.push(kv.c407.lane(i)); }
        if let Col::N(v) = &mut acc.cols[408] { v.push(kv.c408.lane(i)); }
        if let Col::N(v) = &mut acc.cols[317] { v.push(kv.c317.lane(i)); }
        if let Col::N(v) = &mut acc.cols[318] { v.push(sh.c318.lane(i)); }
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
    b.cols[365] = Col::U(AV::Bool(false));
    b.cols[366] = Col::U(AV::Bool(false));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[369] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::N(Vec::new());
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
    b.cols[257] = Col::U(AV::Bool(true));
    b.cols[375] = Col::U(AV::Bool(false));
    b.cols[376] = Col::U(AV::Bool(false));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[377] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[378] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[379] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[380] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[381] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[382] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[383] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[384] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[276] = Col::U(AV::Bool(true));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[385] = Col::U(AV::Bool(false));
    b.cols[386] = Col::U(AV::Bool(false));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[280] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[387] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[388] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[391] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[288] = Col::U(AV::Bool(true));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[394] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[296] = Col::U(AV::Bool(true));
    b.cols[395] = Col::N(Vec::new());
    b.cols[396] = Col::N(Vec::new());
    b.cols[298] = Col::N(Vec::new());
    b.cols[397] = Col::N(Vec::new());
    b.cols[398] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[301] = Col::N(Vec::new());
    b.cols[399] = Col::V(Vec::new());
    b.cols[400] = Col::U(AV::Bool(false));
    b.cols[303] = Col::N(Vec::new());
    b.cols[401] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[402] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[403] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[404] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[310] = Col::V(Vec::new());
    b.cols[311] = Col::V(Vec::new());
    b.cols[405] = Col::N(Vec::new());
    b.cols[406] = Col::N(Vec::new());
    b.cols[313] = Col::U(AV::Bool(true));
    b.cols[407] = Col::N(Vec::new());
    b.cols[408] = Col::N(Vec::new());
    b.cols[317] = Col::N(Vec::new());
    b.cols[318] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
pub const KPART1_3: u64 = 16903100713655481144;
pub const KPART2_3: u64 = 9833413725603708729;

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
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
        if let Col::N(v) = &mut acc.cols[395] { v.push(kv.c395.lane(i)); }
        if let Col::N(v) = &mut acc.cols[396] { v.push(kv.c396.lane(i)); }
        if let Col::N(v) = &mut acc.cols[298] { v.push(kv.c298.lane(i)); }
        if let Col::N(v) = &mut acc.cols[397] { v.push(kv.c397.lane(i)); }
        if let Col::N(v) = &mut acc.cols[398] { v.push(kv.c398.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(kv.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[301] { v.push(kv.c301.lane(i)); }
        if let Col::V(v) = &mut acc.cols[399] {
            v.push(if kv.c399.known & (1 << i) != 0 {
                AV::Bool(kv.c399.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[303] { v.push(kv.c303.lane(i)); }
        if let Col::V(v) = &mut acc.cols[310] {
            v.push(if kv.c310.known & (1 << i) != 0 {
                AV::Bool(kv.c310.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[311] {
            v.push(if kv.c311.known & (1 << i) != 0 {
                AV::Bool(kv.c311.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[405] { v.push(sh.c405.lane(i)); }
        if let Col::N(v) = &mut acc.cols[406] { v.push(sh.c406.lane(i)); }
        if let Col::N(v) = &mut acc.cols[407] { v.push(kv.c407.lane(i)); }
        if let Col::N(v) = &mut acc.cols[408] { v.push(kv.c408.lane(i)); }
        if let Col::N(v) = &mut acc.cols[317] { v.push(kv.c317.lane(i)); }
        if let Col::N(v) = &mut acc.cols[318] { v.push(sh.c318.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
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
    b.cols[240] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[308] = Col::U(AV::Bool(false));
    b.cols[309] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Bool(true));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
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
pub const KPART1_4: u64 = 12324093367925927381;
pub const KPART2_4: u64 = 12364506016476404550;

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
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
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
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[241] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Bool(true));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[258] = Col::U(AV::Bool(true));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[308] = Col::U(AV::Bool(false));
    b.cols[309] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Bool(true));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
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
pub const KPART1_5: u64 = 10611605397908074725;
pub const KPART2_5: u64 = 10354055476980256846;

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
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 6's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append6`.
pub fn acc6(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_6, OUT_GLOBALS_6, OUT_PTRS_6, 0, cart, cache);
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
    b.cols[326] = Col::U(AV::Bool(false));
    b.cols[327] = Col::U(AV::Bool(false));
    b.cols[328] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[331] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::N(Vec::new());
    b.cols[332] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[247] = Col::U(AV::Bool(true));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::N(Vec::new());
    b.cols[256] = Col::U(AV::Bool(true));
    b.cols[257] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[336] = Col::U(AV::Bool(false));
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[276] = Col::U(AV::Bool(true));
    b.cols[346] = Col::U(AV::Bool(false));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[287] = Col::U(AV::Bool(true));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[289] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[291] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_6 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_6: u64 = 11136836713617634455;
pub const KPART2_6: u64 = 15030401020721226940;

/// Append this assignment's lanes that TAKE outcome 6 and
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
pub fn append6(
    acc: &mut Rt2, sh: &KShared6, kv: &KOut6, take: u16,
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
        let k0 = mix64(KPART1_6.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_6.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 7's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append7`.
pub fn acc7(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_7, OUT_GLOBALS_7, OUT_PTRS_7, 0, cart, cache);
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
    b.cols[326] = Col::U(AV::Bool(false));
    b.cols[327] = Col::U(AV::Bool(false));
    b.cols[328] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[331] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::N(Vec::new());
    b.cols[332] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[247] = Col::U(AV::Bool(true));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::N(Vec::new());
    b.cols[256] = Col::U(AV::Bool(true));
    b.cols[336] = Col::U(AV::Bool(false));
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[258] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[259] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[267] = Col::U(AV::Bool(true));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[174] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[275] = Col::U(AV::Bool(true));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(655360i32)));
    b.cols[346] = Col::U(AV::Bool(false));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[279] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[287] = Col::U(AV::Bool(true));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[289] = Col::U(AV::Num(P8::from_raw(1245184i32)));
    b.cols[291] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_7 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_7: u64 = 17078508449713251950;
pub const KPART2_7: u64 = 17747118913405544487;

/// Append this assignment's lanes that TAKE outcome 7 and
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
pub fn append7(
    acc: &mut Rt2, sh: &KShared7, kv: &KOut7, take: u16,
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
        let k0 = mix64(KPART1_7.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_7.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 8's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append8`.
pub fn acc8(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_8, OUT_GLOBALS_8, OUT_PTRS_8, 0, cart, cache);
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
    b.cols[336] = Col::U(AV::Bool(false));
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[346] = Col::U(AV::Bool(false));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Bool(true));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[278] = Col::U(AV::Bool(true));
    b.cols[356] = Col::N(Vec::new());
    b.cols[357] = Col::N(Vec::new());
    b.cols[280] = Col::N(Vec::new());
    b.cols[358] = Col::N(Vec::new());
    b.cols[359] = Col::N(Vec::new());
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[360] = Col::V(Vec::new());
    b.cols[361] = Col::U(AV::Bool(false));
    b.cols[285] = Col::N(Vec::new());
    b.cols[362] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[292] = Col::V(Vec::new());
    b.cols[293] = Col::V(Vec::new());
    b.cols[366] = Col::N(Vec::new());
    b.cols[367] = Col::N(Vec::new());
    b.cols[295] = Col::U(AV::Bool(true));
    b.cols[368] = Col::N(Vec::new());
    b.cols[369] = Col::N(Vec::new());
    b.cols[299] = Col::N(Vec::new());
    b.cols[300] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_8 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_8: u64 = 6556206805066229762;
pub const KPART2_8: u64 = 15592723234554252880;

/// Append this assignment's lanes that TAKE outcome 8 and
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
pub fn append8(
    acc: &mut Rt2, sh: &KShared8, kv: &KOut8, take: u16,
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
        let k0 = mix64(KPART1_8.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_8.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[356] { v.push(kv.c356.lane(i)); }
        if let Col::N(v) = &mut acc.cols[357] { v.push(kv.c357.lane(i)); }
        if let Col::N(v) = &mut acc.cols[280] { v.push(kv.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[358] { v.push(kv.c358.lane(i)); }
        if let Col::N(v) = &mut acc.cols[359] { v.push(kv.c359.lane(i)); }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::V(v) = &mut acc.cols[360] {
            v.push(if kv.c360.known & (1 << i) != 0 {
                AV::Bool(kv.c360.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[285] { v.push(kv.c285.lane(i)); }
        if let Col::V(v) = &mut acc.cols[292] {
            v.push(if kv.c292.known & (1 << i) != 0 {
                AV::Bool(kv.c292.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[293] {
            v.push(if kv.c293.known & (1 << i) != 0 {
                AV::Bool(kv.c293.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[366] { v.push(sh.c366.lane(i)); }
        if let Col::N(v) = &mut acc.cols[367] { v.push(sh.c367.lane(i)); }
        if let Col::N(v) = &mut acc.cols[368] { v.push(kv.c368.lane(i)); }
        if let Col::N(v) = &mut acc.cols[369] { v.push(kv.c369.lane(i)); }
        if let Col::N(v) = &mut acc.cols[299] { v.push(kv.c299.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(sh.c300.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 9's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append9`.
pub fn acc9(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_9, OUT_GLOBALS_9, OUT_PTRS_9, 0, cart, cache);
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
    b.cols[297] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[241] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Bool(true));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[258] = Col::U(AV::Bool(true));
    b.cols[307] = Col::U(AV::Bool(false));
    b.cols[308] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_9 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_9: u64 = 7359791922650843579;
pub const KPART2_9: u64 = 17068849822392135886;

/// Append this assignment's lanes that TAKE outcome 9 and
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
pub fn append9(
    acc: &mut Rt2, sh: &KShared9, kv: &KOut9, take: u16,
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
        let k0 = mix64(KPART1_9.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_9.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 10's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append10`.
pub fn acc10(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_10, OUT_GLOBALS_10, OUT_PTRS_10, 0, cart, cache);
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
    b.cols[325] = Col::U(AV::Bool(false));
    b.cols[326] = Col::U(AV::Bool(false));
    b.cols[327] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[328] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[329] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[330] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::N(Vec::new());
    b.cols[331] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[247] = Col::U(AV::Bool(true));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[253] = Col::N(Vec::new());
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
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(true));
    for (cell, _) in OUT_UBOOL_10 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_10: u64 = 6505727788572677553;
pub const KPART2_10: u64 = 6750100009566586826;

/// Append this assignment's lanes that TAKE outcome 10 and
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
pub fn append10(
    acc: &mut Rt2, sh: &KShared10, kv: &KOut10, take: u16,
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
        let k0 = mix64(KPART1_10.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_10.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

/// An EMPTY accumulator with outcome 11's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append11`.
pub fn acc11(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_11, OUT_GLOBALS_11, OUT_PTRS_11, 0, cart, cache);
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
    for (cell, _) in OUT_UBOOL_11 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_11: u64 = 7538238740480024082;
pub const KPART2_11: u64 = 8274342854545347923;

/// Append this assignment's lanes that TAKE outcome 11 and
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
pub fn append11(
    acc: &mut Rt2, sh: &KShared11, kv: &KOut11, take: u16,
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
        let k0 = mix64(KPART1_11.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_11.wrapping_add(h2[i]));
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
    }
    wrote
}

/// An EMPTY accumulator with outcome 12's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append12`.
pub fn acc12(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_12, OUT_GLOBALS_12, OUT_PTRS_12, 0, cart, cache);
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
    for (cell, _) in OUT_UBOOL_12 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_12: u64 = 16651243698737751054;
pub const KPART2_12: u64 = 14372757396449290073;

/// Append this assignment's lanes that TAKE outcome 12 and
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
pub fn append12(
    acc: &mut Rt2, sh: &KShared12, kv: &KOut12, take: u16,
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
        let k0 = mix64(KPART1_12.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_12.wrapping_add(h2[i]));
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
    }
    wrote
}

/// An EMPTY accumulator with outcome 13's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append13`.
pub fn acc13(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_13, OUT_GLOBALS_13, OUT_PTRS_13, 0, cart, cache);
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
    b.cols[364] = Col::U(AV::Bool(false));
    b.cols[365] = Col::U(AV::Bool(false));
    b.cols[366] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[368] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[369] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[246] = Col::N(Vec::new());
    b.cols[370] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[248] = Col::U(AV::Bool(true));
    b.cols[372] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[251] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[254] = Col::N(Vec::new());
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
    b.cols[404] = Col::N(Vec::new());
    b.cols[405] = Col::N(Vec::new());
    b.cols[312] = Col::U(AV::Bool(true));
    b.cols[406] = Col::N(Vec::new());
    b.cols[407] = Col::N(Vec::new());
    b.cols[316] = Col::N(Vec::new());
    b.cols[317] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_13 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_13: u64 = 343323083668812483;
pub const KPART2_13: u64 = 13760168619098720467;

/// Append this assignment's lanes that TAKE outcome 13 and
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
pub fn append13(
    acc: &mut Rt2, sh: &KShared13, kv: &KOut13, take: u16,
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
        let k0 = mix64(KPART1_13.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_13.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[404] { v.push(sh.c404.lane(i)); }
        if let Col::N(v) = &mut acc.cols[405] { v.push(sh.c405.lane(i)); }
        if let Col::N(v) = &mut acc.cols[406] { v.push(kv.c406.lane(i)); }
        if let Col::N(v) = &mut acc.cols[407] { v.push(kv.c407.lane(i)); }
        if let Col::N(v) = &mut acc.cols[316] { v.push(kv.c316.lane(i)); }
        if let Col::N(v) = &mut acc.cols[317] { v.push(sh.c317.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
    }
    wrote
}

pub const OUTCOMES: usize = 14;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        4 => acc4(cart, cache),
        5 => acc5(cart, cache),
        6 => acc6(cart, cache),
        7 => acc7(cart, cache),
        8 => acc8(cart, cache),
        9 => acc9(cart, cache),
        10 => acc10(cart, cache),
        11 => acc11(cart, cache),
        12 => acc12(cart, cache),
        13 => acc13(cart, cache),
        _ => panic!("outcome {} of 14", i),
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
        6 => OUT_SLOTS_6,
        7 => OUT_SLOTS_7,
        8 => OUT_SLOTS_8,
        9 => OUT_SLOTS_9,
        10 => OUT_SLOTS_10,
        11 => OUT_SLOTS_11,
        12 => OUT_SLOTS_12,
        13 => OUT_SLOTS_13,
        _ => panic!("outcome {} of 14", i),
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
    fn o6(&mut self, _mask: u8, take: u16, sh: &KShared6, v: &KOut6) {
        append6(&mut self.accs[6], sh, v, take, self.n, &mut self.seen[6], self.org, self.skip);
    }
    fn o7(&mut self, _mask: u8, take: u16, sh: &KShared7, v: &KOut7) {
        append7(&mut self.accs[7], sh, v, take, self.n, &mut self.seen[7], self.org, self.skip);
    }
    fn o8(&mut self, _mask: u8, take: u16, sh: &KShared8, v: &KOut8) {
        append8(&mut self.accs[8], sh, v, take, self.n, &mut self.seen[8], self.org, self.skip);
    }
    fn o9(&mut self, _mask: u8, take: u16, sh: &KShared9, v: &KOut9) {
        append9(&mut self.accs[9], sh, v, take, self.n, &mut self.seen[9], self.org, self.skip);
    }
    fn o10(&mut self, _mask: u8, take: u16, sh: &KShared10, v: &KOut10) {
        append10(&mut self.accs[10], sh, v, take, self.n, &mut self.seen[10], self.org, self.skip);
    }
    fn o11(&mut self, _mask: u8, take: u16, sh: &KShared11, v: &KOut11) {
        append11(&mut self.accs[11], sh, v, take, self.n, &mut self.seen[11], self.org, self.skip);
    }
    fn o12(&mut self, _mask: u8, take: u16, sh: &KShared12, v: &KOut12) {
        append12(&mut self.accs[12], sh, v, take, self.n, &mut self.seen[12], self.org, self.skip);
    }
    fn o13(&mut self, _mask: u8, take: u16, sh: &KShared13, v: &KOut13) {
        append13(&mut self.accs[13], sh, v, take, self.n, &mut self.seen[13], self.org, self.skip);
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
    fn o6(&mut self, mask: u8, take: u16, sh: &KShared6, v: &KOut6);
    fn o7(&mut self, mask: u8, take: u16, sh: &KShared7, v: &KOut7);
    fn o8(&mut self, mask: u8, take: u16, sh: &KShared8, v: &KOut8);
    fn o9(&mut self, mask: u8, take: u16, sh: &KShared9, v: &KOut9);
    fn o10(&mut self, mask: u8, take: u16, sh: &KShared10, v: &KOut10);
    fn o11(&mut self, mask: u8, take: u16, sh: &KShared11, v: &KOut11);
    fn o12(&mut self, mask: u8, take: u16, sh: &KShared12, v: &KOut12);
    fn o13(&mut self, mask: u8, take: u16, sh: &KShared13, v: &KOut13);
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
    let r_c259: ZN = rin.c259;
    let r_c260: ZN = rin.c260;
    let r_c268: ZB = ZB { val: rin.c268, known: ALL };
    let r_c270: ZN = rin.c270;
    let r_c272: ZN = rin.c272;
    let r_c273: ZN = rin.c273;
    let r_c276: ZB = ZB { val: rin.c276, known: ALL };
    let r_c278: ZN = rin.c278;
    let r_c279: ZN = rin.c279;
    let r_c287: ZB = ZB { val: rin.c287, known: ALL };
    let r_c289: ZN = rin.c289;
    let r_c291: ZN = rin.c291;
    let r_c292: ZN = rin.c292;
    let r_c295: ZB = ZB { val: rin.c295, known: ALL };
    let r_c297: ZN = rin.c297;
    let r_c299: ZN = rin.c299;
    let r_c300: ZN = rin.c300;
    let r_c302: ZN = rin.c302;
    let r_c309: ZB = ZB { val: rin.c309, known: ALL };
    let r_c310: ZB = ZB { val: rin.c310, known: ALL };
    let r_c312: ZB = ZB { val: rin.c312, known: ALL };
    let r_c316: ZN = rin.c316;
    let r_c317: ZN = rin.c317;
    let r_c364: ZB = ZB { val: rin.c364, known: ALL };
    let r_c365: ZB = ZB { val: rin.c365, known: ALL };
    let r_c370: ZN = rin.c370;
    let r_c371: ZN = rin.c371;
    let r_c372: ZN = rin.c372;
    let r_c373: ZN = rin.c373;
    let r_c374: ZB = ZB { val: rin.c374, known: ALL };
    let r_c375: ZB = ZB { val: rin.c375, known: ALL };
    let r_c380: ZN = rin.c380;
    let r_c381: ZN = rin.c381;
    let r_c382: ZN = rin.c382;
    let r_c383: ZN = rin.c383;
    let r_c384: ZB = ZB { val: rin.c384, known: ALL };
    let r_c385: ZB = ZB { val: rin.c385, known: ALL };
    let r_c390: ZN = rin.c390;
    let r_c391: ZN = rin.c391;
    let r_c392: ZN = rin.c392;
    let r_c393: ZN = rin.c393;
    let r_c394: ZN = rin.c394;
    let r_c395: ZN = rin.c395;
    let r_c396: ZN = rin.c396;
    let r_c397: ZN = rin.c397;
    let r_c398: ZB = ZB { val: rin.c398, known: ALL };
    let r_c399: ZB = ZB { val: rin.c399, known: ALL };
    let r_c404: ZN = rin.c404;
    let r_c405: ZN = rin.c405;
    let r_c406: ZN = rin.c406;
    let r_c407: ZN = rin.c407;
    let n111: ZB = zb_not(r_c42);
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n113: ZB = zb_not(r_c364);
    let n114: ZB = zb_not(r_c365);
    let n115: bool = P8::from_raw(524288i32) == u.c366;
    let n116: bool = P8::from_raw(524288i32) == u.c367;
    let n117: bool = P8::from_raw(0i32) == u.c368;
    let n118: bool = P8::from_raw(0i32) == u.c369;
    let n119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c370);
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c371);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c372);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c373);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c250);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c251);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c253);
    let n126: ZB = zb_not(r_c375);
    let n127: bool = P8::from_raw(524288i32) == u.c376;
    let n128: bool = P8::from_raw(0i32) == u.c378;
    let n129: bool = P8::from_raw(0i32) == u.c379;
    let n130: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c380);
    let n131: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c390);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c391);
    let n133: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c392);
    let n137: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n138: ZN = zn_rem(n137, zn_splat(P8::from_raw(1966080i32)));
    let n139: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n138);
    let n154: ZB = zb_not(r_c374);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c259);
    let n156: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c260);
    let n157: bool = P8::from_raw(524288i32) == u.c377;
    let n158: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c381);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c382);
    let n160: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c383);
    let n161: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c270);
    let n162: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c272);
    let n163: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n164: ZB = zb_not(r_c384);
    let n165: ZB = zb_not(r_c385);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c278);
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c279);
    let n168: bool = P8::from_raw(524288i32) == u.c386;
    let n169: bool = P8::from_raw(524288i32) == u.c387;
    let n170: bool = P8::from_raw(0i32) == u.c388;
    let n171: bool = P8::from_raw(0i32) == u.c389;
    let n172: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c393);
    let n173: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c289);
    let n174: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c291);
    let n175: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c292);
    let n176: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n177: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n233: ZB = zb_not(r_c399);
    let n234: bool = P8::from_raw(327680i32) == u.c400;
    let n235: bool = P8::from_raw(393216i32) == u.c401;
    let n236: bool = P8::from_raw(65536i32) == u.c402;
    let n237: bool = P8::from_raw(196608i32) == u.c403;
    let n238: ZB = zb_not(r_c43);
    let n239: ZB = zb_not(r_c38);
    let n240: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n241: ZN = zn_rem(n240, zn_splat(P8::from_raw(3932160i32)));
    let n242: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n241);
    let n243: ZN = zsel_n(n242, n176, r_c86);
    let n244: ZN = zsel_n(n139, n243, r_c86);
    let n245: ZN = zsel_n(n139, n241, r_c85);
    let n246: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c317);
    let n247: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n246);
    let n248: ZN = zn_add(zn_splat(P8::from_raw(0i32)), r_c254);
    let n249: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n248);
    let n250: ZB = zn_gt(n247, n249);
    let n251: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n248);
    let n252: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n251);
    let n253: ZB = zn_lt(n246, n252);
    let n254: ZB = zb_not(r_c310);
    let n255: ZB = zb_not(r_c309);
    let n256: ZB = zn_gt(r_c302, zn_splat(P8::from_raw(0i32)));
    let n257: ZN = zn_sub(r_c302, zn_splat(P8::from_raw(65536i32)));
    let n258: ZN = zsel_n(n256, n257, r_c302);
    let n259: ZN = zn_sub(r_c297, zn_splat(P8::from_raw(65536i32)));
    let n260: ZB = zn_gt(r_c299, zn_splat(P8::from_raw(0i32)));
    let n261: ZN = zn_sub(r_c299, zn_splat(P8::from_raw(65536i32)));
    let n262: ZN = zsel_n(n260, n261, r_c299);
    let n263: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n264: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c316);
    let n265: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n264);
    let n266: ZB = zn_gt(n265, zn_splat(P8::from_raw(524288i32)));
    let n267: ZB = zb_and(n250, n266);
    let n268: ZB = zn_lt(n264, zn_splat(P8::from_raw(1048576i32)));
    let n269: ZB = zb_and(n267, n268);
    let n270: ZB = zb_and(n253, n269);
    let n271: ZB = zn_gt(n265, zn_splat(P8::from_raw(2621440i32)));
    let n272: ZB = zn_gt(n247, zn_splat(P8::from_raw(7340032i32)));
    let n273: ZB = zb_and(n177, n250);
    let n274: ZB = zb_and(n253, n273);
    let n275: ZB = zb_and(n266, n274);
    let n276: ZB = zb_and(n268, n275);
    let n277: ZB = zb_and(n271, n272);
    let n278: ZB = zn_lt(n264, zn_splat(P8::from_raw(3145728i32)));
    let n279: ZB = zb_and(n277, n278);
    let n280: ZB = zn_lt(n246, zn_splat(P8::from_raw(7864320i32)));
    let n281: ZB = zb_and(n279, n280);
    let n282: ZB = zb_and(n276, n281);
    let n283: ZB = zn_ge(r_c407, zn_splat(P8::from_raw(0i32)));
    let n287: ZN = zn_mul(r_c406, zn_splat(P8::from_raw(13107i32)));
    let n288: ZB = zn_gt(n265, zn_splat(P8::from_raw(6815744i32)));
    let n290: ZB = zn_lt(n264, zn_splat(P8::from_raw(7340032i32)));
    let n291: ZN = zn_add(r_c404, n287);
    let n292: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n291);
    let n293: ZN = zn_flr(n292);
    let n294: ZN = zn_sub(n292, zn_splat(P8::from_raw(32768i32)));
    let n295: ZN = zn_sub(n294, n293);
    let n296: ZB = zn_gt(n293, zn_splat(P8::from_raw(0i32)));
    let n297: ZB = zn_lt(n293, zn_splat(P8::from_raw(0i32)));
    let n298: ZN = zsel_n(n297, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n299: ZN = zsel_n(n296, zn_splat(P8::from_raw(65536i32)), n298);
    let n300: ZN = zn_abs(n293);
    let n301: ZN = zn_add(n264, n299);
    let n302: ZB = zn_tile_flag_at(g.cache, g.cart, n301, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n303: ZN = zn_add(r_c316, n299);
    let n304: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n300);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n303);
    let n306: ZN = zn_add(n299, n305);
    let n307: ZB = zn_tile_flag_at(g.cache, g.cart, n306, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n308: ZN = zn_add(n299, n303);
    let n309: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n300);
    let n310: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n308);
    let n311: ZN = zn_add(n299, n310);
    let n312: ZB = zn_tile_flag_at(g.cache, g.cart, n311, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n313: ZN = zn_add(n299, n308);
    let n314: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n300);
    let n315: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n313);
    let n316: ZN = zn_add(n299, n315);
    let n317: ZB = zn_tile_flag_at(g.cache, g.cart, n316, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n318: ZN = zn_add(n299, n313);
    let n319: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n300);
    let n320: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n318);
    let n321: ZN = zn_add(n299, n320);
    let n322: ZB = zn_tile_flag_at(g.cache, g.cart, n321, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n323: ZN = zn_add(n299, n318);
    let n324: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n300);
    let n325: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n323);
    let n326: ZN = zn_add(n299, n325);
    let n327: ZB = zn_tile_flag_at(g.cache, g.cart, n326, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n328: ZN = zn_add(n299, n323);
    let n329: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n300);
    let n330: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n328);
    let n331: ZN = zn_add(n299, n330);
    let n332: ZB = zn_tile_flag_at(g.cache, g.cart, n331, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n333: ZN = zn_add(n299, n328);
    let n334: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n300);
    let n335: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n333);
    let n336: ZN = zn_add(n299, n335);
    let n337: ZB = zn_tile_flag_at(g.cache, g.cart, n336, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n338: ZN = zn_add(n299, n333);
    let n339: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n300);
    let n340: ZN = zsel_n(n337, n333, n338);
    let n341: ZN = zsel_n(n337, zn_splat(P8::from_raw(0i32)), n295);
    let n342: ZN = zsel_n(n337, zn_splat(P8::from_raw(0i32)), n287);
    let n343: ZB = zb_or(n337, n339);
    let n344: ZN = zsel_n(n334, n333, n340);
    let n345: ZN = zsel_n(n334, n295, n341);
    let n346: ZN = zsel_n(n334, n287, n342);
    let n347: ZB = zb_and(n271, n276);
    let n348: ZB = zb_and(n272, n347);
    let n349: ZB = zb_and(n278, n348);
    let n350: ZB = zb_and(n280, n349);
    let n351: ZB = zb_and(n283, n350);
    let n352: ZB = zb_or(n334, n343);
    let n353: ZN = zsel_n(n332, n328, n344);
    let n354: ZN = zsel_n(n332, zn_splat(P8::from_raw(0i32)), n345);
    let n355: ZN = zsel_n(n332, zn_splat(P8::from_raw(0i32)), n346);
    let n356: ZB = zb_or(n332, n352);
    let n357: ZN = zsel_n(n329, n328, n353);
    let n358: ZN = zsel_n(n329, n295, n354);
    let n359: ZN = zsel_n(n329, n287, n355);
    let n360: ZB = zb_or(n329, n356);
    let n361: ZN = zsel_n(n327, n323, n357);
    let n362: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), n358);
    let n363: ZN = zsel_n(n327, zn_splat(P8::from_raw(0i32)), n359);
    let n364: ZB = zb_or(n327, n360);
    let n365: ZN = zsel_n(n324, n323, n361);
    let n366: ZN = zsel_n(n324, n295, n362);
    let n367: ZN = zsel_n(n324, n287, n363);
    let n368: ZB = zb_or(n324, n364);
    let n369: ZN = zsel_n(n322, n318, n365);
    let n370: ZN = zsel_n(n322, zn_splat(P8::from_raw(0i32)), n366);
    let n371: ZN = zsel_n(n322, zn_splat(P8::from_raw(0i32)), n367);
    let n372: ZB = zb_or(n322, n368);
    let n373: ZN = zsel_n(n319, n318, n369);
    let n374: ZN = zsel_n(n319, n295, n370);
    let n375: ZN = zsel_n(n319, n287, n371);
    let n376: ZB = zb_or(n319, n372);
    let n377: ZN = zsel_n(n317, n313, n373);
    let n378: ZN = zsel_n(n317, zn_splat(P8::from_raw(0i32)), n374);
    let n379: ZN = zsel_n(n317, zn_splat(P8::from_raw(0i32)), n375);
    let n380: ZB = zb_or(n317, n376);
    let n381: ZN = zsel_n(n314, n313, n377);
    let n382: ZN = zsel_n(n314, n295, n378);
    let n383: ZN = zsel_n(n314, n287, n379);
    let n384: ZB = zb_or(n314, n380);
    let n385: ZN = zsel_n(n312, n308, n381);
    let n386: ZN = zsel_n(n312, zn_splat(P8::from_raw(0i32)), n382);
    let n387: ZN = zsel_n(n312, zn_splat(P8::from_raw(0i32)), n383);
    let n388: ZB = zb_or(n312, n384);
    let n389: ZN = zsel_n(n309, n308, n385);
    let n390: ZN = zsel_n(n309, n295, n386);
    let n391: ZN = zsel_n(n309, n287, n387);
    let n392: ZB = zb_or(n309, n388);
    let n393: ZN = zsel_n(n307, n303, n389);
    let n394: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n390);
    let n395: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n391);
    let n396: ZB = zb_or(n307, n392);
    let n397: ZN = zsel_n(n304, n303, n393);
    let n398: ZN = zsel_n(n304, n295, n394);
    let n399: ZN = zsel_n(n304, n287, n395);
    let n400: ZB = zb_or(n304, n396);
    let n401: ZN = zsel_n(n302, r_c316, n397);
    let n402: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n398);
    let n403: ZN = zsel_n(n302, zn_splat(P8::from_raw(0i32)), n399);
    let n404: ZB = zb_or(n302, n400);
    let n405: ZN = zn_add(r_c405, zn_splat(P8::from_raw(-196608i32)));
    let n406: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n405);
    let n407: ZN = zn_flr(n406);
    let n408: ZN = zn_sub(n406, zn_splat(P8::from_raw(32768i32)));
    let n409: ZN = zn_sub(n408, n407);
    let n410: ZB = zn_gt(n407, zn_splat(P8::from_raw(0i32)));
    let n411: ZB = zn_lt(n407, zn_splat(P8::from_raw(0i32)));
    let n412: ZN = zsel_n(n411, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n413: ZN = zsel_n(n410, zn_splat(P8::from_raw(65536i32)), n412);
    let n414: ZN = zn_abs(n407);
    let n415: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n401);
    let n416: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n415);
    let n417: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n413);
    let n418: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n417, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n419: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n413);
    let n420: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n414);
    let n421: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n419);
    let n422: ZN = zn_add(n413, n421);
    let n423: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n422, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n424: ZN = zn_add(n413, n419);
    let n425: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n414);
    let n426: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n424);
    let n427: ZN = zn_add(n413, n426);
    let n428: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n427, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n429: ZN = zn_add(n413, n424);
    let n430: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n414);
    let n431: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n429);
    let n432: ZN = zn_add(n413, n431);
    let n433: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n432, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n434: ZN = zn_add(n413, n429);
    let n435: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n414);
    let n436: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n434);
    let n437: ZN = zn_add(n413, n436);
    let n438: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n437, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n439: ZN = zn_add(n413, n434);
    let n440: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n414);
    let n441: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n439);
    let n442: ZN = zn_add(n413, n441);
    let n443: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n442, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n444: ZN = zn_add(n413, n439);
    let n445: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n414);
    let n446: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n444);
    let n447: ZN = zn_add(n413, n446);
    let n448: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n447, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n449: ZN = zn_add(n413, n444);
    let n450: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n414);
    let n451: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n449);
    let n452: ZN = zn_add(n413, n451);
    let n453: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n452, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n454: ZN = zn_add(n413, n449);
    let n455: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n414);
    let n456: ZB = zb_and(n404, n455);
    let n457: ZN = zsel_n(n453, n449, n454);
    let n458: ZN = zsel_n(n453, zn_splat(P8::from_raw(0i32)), n409);
    let n459: ZN = zsel_n(n453, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n460: ZB = zsel_b(n453, n404, n456);
    let n461: ZN = zsel_n(n450, n449, n457);
    let n462: ZN = zsel_n(n450, n409, n458);
    let n463: ZN = zsel_n(n450, zn_splat(P8::from_raw(-196608i32)), n459);
    let n464: ZB = zsel_b(n450, n404, n460);
    let n465: ZN = zsel_n(n448, n444, n461);
    let n466: ZN = zsel_n(n448, zn_splat(P8::from_raw(0i32)), n462);
    let n467: ZN = zsel_n(n448, zn_splat(P8::from_raw(0i32)), n463);
    let n468: ZB = zsel_b(n448, n404, n464);
    let n469: ZN = zsel_n(n445, n444, n465);
    let n470: ZN = zsel_n(n445, n409, n466);
    let n471: ZN = zsel_n(n445, zn_splat(P8::from_raw(-196608i32)), n467);
    let n472: ZB = zsel_b(n445, n404, n468);
    let n473: ZN = zsel_n(n443, n439, n469);
    let n474: ZN = zsel_n(n443, zn_splat(P8::from_raw(0i32)), n470);
    let n475: ZN = zsel_n(n443, zn_splat(P8::from_raw(0i32)), n471);
    let n476: ZB = zsel_b(n443, n404, n472);
    let n477: ZN = zsel_n(n440, n439, n473);
    let n478: ZN = zsel_n(n440, n409, n474);
    let n479: ZN = zsel_n(n440, zn_splat(P8::from_raw(-196608i32)), n475);
    let n480: ZB = zsel_b(n440, n404, n476);
    let n481: ZN = zsel_n(n438, n434, n477);
    let n482: ZN = zsel_n(n438, zn_splat(P8::from_raw(0i32)), n478);
    let n483: ZN = zsel_n(n438, zn_splat(P8::from_raw(0i32)), n479);
    let n484: ZB = zsel_b(n438, n404, n480);
    let n485: ZN = zsel_n(n435, n434, n481);
    let n486: ZN = zsel_n(n435, n409, n482);
    let n487: ZN = zsel_n(n435, zn_splat(P8::from_raw(-196608i32)), n483);
    let n488: ZB = zsel_b(n435, n404, n484);
    let n489: ZN = zsel_n(n433, n429, n485);
    let n490: ZN = zsel_n(n433, zn_splat(P8::from_raw(0i32)), n486);
    let n491: ZN = zsel_n(n433, zn_splat(P8::from_raw(0i32)), n487);
    let n492: ZB = zsel_b(n433, n404, n488);
    let n493: ZN = zsel_n(n430, n429, n489);
    let n494: ZN = zsel_n(n430, n409, n490);
    let n495: ZN = zsel_n(n430, zn_splat(P8::from_raw(-196608i32)), n491);
    let n496: ZB = zsel_b(n430, n404, n492);
    let n497: ZN = zsel_n(n428, n424, n493);
    let n498: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), n494);
    let n499: ZN = zsel_n(n428, zn_splat(P8::from_raw(0i32)), n495);
    let n500: ZB = zsel_b(n428, n404, n496);
    let n501: ZN = zsel_n(n425, n424, n497);
    let n502: ZN = zsel_n(n425, n409, n498);
    let n503: ZN = zsel_n(n425, zn_splat(P8::from_raw(-196608i32)), n499);
    let n504: ZB = zsel_b(n425, n404, n500);
    let n505: ZN = zsel_n(n423, n419, n501);
    let n506: ZN = zsel_n(n423, zn_splat(P8::from_raw(0i32)), n502);
    let n507: ZN = zsel_n(n423, zn_splat(P8::from_raw(0i32)), n503);
    let n508: ZB = zsel_b(n423, n404, n504);
    let n509: ZN = zsel_n(n420, n419, n505);
    let n510: ZN = zsel_n(n420, n409, n506);
    let n511: ZN = zsel_n(n420, zn_splat(P8::from_raw(-196608i32)), n507);
    let n512: ZB = zsel_b(n420, n404, n508);
    let n513: ZN = zsel_n(n418, zn_splat(P8::from_raw(7077888i32)), n509);
    let n514: ZN = zsel_n(n418, zn_splat(P8::from_raw(0i32)), n510);
    let n515: ZN = zsel_n(n418, zn_splat(P8::from_raw(0i32)), n511);
    let n516: ZB = zsel_b(n418, n404, n512);
    let n517: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n513);
    let n518: ZN = zn_div(n415, zn_splat(P8::from_raw(524288i32)));
    let n519: ZN = zn_flr(n518);
    let n520: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n519);
    let n521: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n415);
    let n522: ZN = zn_sub(n521, zn_splat(P8::from_raw(65536i32)));
    let n523: ZN = zn_div(n522, zn_splat(P8::from_raw(524288i32)));
    let n524: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n523);
    let n525: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n520);
    let n526: ZB = zn_le(n525, n524);
    let n527: ZB = zn_gt(n525, n524);
    let n528: ZB = zb_and(n351, n526);
    let n529: ZB = zb_and(n351, n527);
    let n530: ZN = zn_div(n517, zn_splat(P8::from_raw(524288i32)));
    let n531: ZN = zn_flr(n530);
    let n532: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n531);
    let n533: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n517);
    let n534: ZN = zn_sub(n533, zn_splat(P8::from_raw(65536i32)));
    let n535: ZN = zn_div(n534, zn_splat(P8::from_raw(524288i32)));
    let n536: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n535);
    let n537: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n532);
    let n538: ZB = zn_le(n537, n536);
    let n539: ZB = zn_gt(n537, n536);
    let n540: ZB = zb_and(n528, n538);
    let n541: ZB = zb_and(n528, n539);
    let n542: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n525);
    let n543: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n537);
    let n544: ZN = zn_mget(g.cart, n542, n543);
    let n545: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n544);
    let n546: ZN = zn_rem(n534, zn_splat(P8::from_raw(524288i32)));
    let n547: ZB = zn_ge(n546, zn_splat(P8::from_raw(393216i32)));
    let n548: ZN = zn_mul(n537, zn_splat(P8::from_raw(524288i32)));
    let n549: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n548);
    let n550: ZB = zn_eq(n533, n549);
    let n551: ZB = zb_or(n547, n550);
    let n552: ZB = zb_and(n545, n551);
    let n553: ZB = zn_ge(n515, zn_splat(P8::from_raw(0i32)));
    let n554: ZB = zb_and(n552, n553);
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n540, n555);
    let n557: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n544);
    let n558: ZN = zn_rem(n517, zn_splat(P8::from_raw(524288i32)));
    let n559: ZB = zn_le(n558, zn_splat(P8::from_raw(131072i32)));
    let n560: ZB = zb_and(n557, n559);
    let n561: ZB = zb_not(n560);
    let n562: ZB = zb_and(n556, n560);
    let n563: ZB = zb_and(n556, n561);
    let n564: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n544);
    let n565: ZN = zn_rem(n415, zn_splat(P8::from_raw(524288i32)));
    let n566: ZB = zn_le(n565, zn_splat(P8::from_raw(131072i32)));
    let n567: ZB = zb_and(n564, n566);
    let n568: ZB = zn_le(n403, zn_splat(P8::from_raw(0i32)));
    let n569: ZB = zb_and(n567, n568);
    let n570: ZB = zb_not(n569);
    let n571: ZB = zb_and(n563, n570);
    let n572: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n544);
    let n573: ZN = zn_rem(n522, zn_splat(P8::from_raw(524288i32)));
    let n574: ZB = zn_ge(n573, zn_splat(P8::from_raw(393216i32)));
    let n575: ZN = zn_mul(n525, zn_splat(P8::from_raw(524288i32)));
    let n576: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n575);
    let n577: ZB = zn_eq(n521, n576);
    let n578: ZB = zb_or(n574, n577);
    let n579: ZB = zb_and(n572, n578);
    let n580: ZB = zn_ge(n403, zn_splat(P8::from_raw(0i32)));
    let n581: ZB = zb_and(n579, n580);
    let n582: ZB = zb_not(n581);
    let n583: ZB = zb_and(n571, n582);
    let n584: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n532);
    let n585: ZB = zn_le(n584, n536);
    let n586: ZB = zn_gt(n584, n536);
    let n587: ZB = zb_and(n583, n585);
    let n588: ZB = zb_and(n583, n586);
    let n589: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n584);
    let n590: ZN = zn_mget(g.cart, n542, n589);
    let n591: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n590);
    let n592: ZN = zn_mul(n584, zn_splat(P8::from_raw(524288i32)));
    let n593: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n592);
    let n594: ZB = zn_eq(n533, n593);
    let n595: ZB = zb_or(n547, n594);
    let n596: ZB = zb_and(n591, n595);
    let n597: ZB = zb_and(n553, n596);
    let n598: ZB = zb_not(n597);
    let n599: ZB = zb_and(n587, n598);
    let n600: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n590);
    let n601: ZB = zb_and(n559, n600);
    let n602: ZB = zb_not(n601);
    let n603: ZB = zb_and(n599, n601);
    let n604: ZB = zb_and(n599, n602);
    let n605: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n590);
    let n606: ZB = zb_and(n566, n605);
    let n607: ZB = zb_and(n568, n606);
    let n608: ZB = zb_not(n607);
    let n609: ZB = zb_and(n604, n608);
    let n610: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n590);
    let n611: ZB = zb_and(n578, n610);
    let n612: ZB = zb_and(n580, n611);
    let n613: ZB = zb_not(n612);
    let n614: ZB = zb_and(n609, n613);
    let n615: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n532);
    let n616: ZB = zn_le(n615, n536);
    let n617: ZB = zn_gt(n615, n536);
    let n618: ZB = zb_and(n614, n616);
    let n619: ZB = zb_and(n614, n617);
    let n620: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n615);
    let n621: ZN = zn_mget(g.cart, n542, n620);
    let n622: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n621);
    let n623: ZN = zn_mul(n615, zn_splat(P8::from_raw(524288i32)));
    let n624: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n623);
    let n625: ZB = zn_eq(n533, n624);
    let n626: ZB = zb_or(n547, n625);
    let n627: ZB = zb_and(n622, n626);
    let n628: ZB = zb_and(n553, n627);
    let n629: ZB = zb_not(n628);
    let n630: ZB = zb_and(n618, n629);
    let n631: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n621);
    let n632: ZB = zb_and(n559, n631);
    let n633: ZB = zb_not(n632);
    let n634: ZB = zb_and(n630, n632);
    let n635: ZB = zb_and(n630, n633);
    let n636: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n621);
    let n637: ZB = zb_and(n566, n636);
    let n638: ZB = zb_and(n568, n637);
    let n639: ZB = zb_not(n638);
    let n640: ZB = zb_and(n635, n639);
    let n641: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n621);
    let n642: ZB = zb_and(n578, n641);
    let n643: ZB = zb_and(n580, n642);
    let n644: ZB = zb_not(n643);
    let n645: ZB = zb_and(n640, n644);
    let n646: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n532);
    let n647: ZB = zn_gt(n646, n536);
    let n648: ZB = zb_and(n516, n647);
    let n649: ZB = zb_or(n619, n645);
    let n650: ZB = zsel_b(n617, n516, n648);
    let n651: ZB = zb_or(n588, n649);
    let n652: ZB = zsel_b(n586, n516, n650);
    let n653: ZB = zb_or(n541, n651);
    let n654: ZB = zsel_b(n539, n516, n652);
    let n655: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n520);
    let n656: ZB = zn_le(n655, n524);
    let n657: ZB = zn_gt(n655, n524);
    let n658: ZB = zb_and(n653, n656);
    let n659: ZB = zb_and(n653, n657);
    let n660: ZB = zb_and(n539, n658);
    let n661: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n655);
    let n662: ZN = zn_mget(g.cart, n661, n543);
    let n663: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n662);
    let n664: ZB = zb_and(n538, n653);
    let n665: ZB = zb_and(n656, n664);
    let n666: ZB = zb_and(n551, n663);
    let n667: ZB = zb_and(n553, n666);
    let n668: ZB = zb_not(n667);
    let n669: ZB = zb_and(n665, n668);
    let n670: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n662);
    let n671: ZB = zb_and(n559, n670);
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n669, n671);
    let n674: ZB = zb_and(n669, n672);
    let n675: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n662);
    let n676: ZB = zb_and(n566, n675);
    let n677: ZB = zb_and(n568, n676);
    let n678: ZB = zb_not(n677);
    let n679: ZB = zb_and(n674, n678);
    let n680: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n662);
    let n681: ZN = zn_mul(n655, zn_splat(P8::from_raw(524288i32)));
    let n682: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n681);
    let n683: ZB = zn_eq(n521, n682);
    let n684: ZB = zb_or(n574, n683);
    let n685: ZB = zb_and(n680, n684);
    let n686: ZB = zb_and(n580, n685);
    let n687: ZB = zb_not(n686);
    let n688: ZB = zb_and(n679, n687);
    let n689: ZB = zb_and(n586, n688);
    let n690: ZN = zn_mget(g.cart, n661, n589);
    let n691: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n690);
    let n692: ZB = zb_and(n585, n679);
    let n693: ZB = zb_and(n687, n692);
    let n694: ZB = zb_and(n595, n691);
    let n695: ZB = zb_and(n553, n694);
    let n696: ZB = zb_not(n695);
    let n697: ZB = zb_and(n693, n696);
    let n698: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n690);
    let n699: ZB = zb_and(n559, n698);
    let n700: ZB = zb_not(n699);
    let n701: ZB = zb_and(n697, n699);
    let n702: ZB = zb_and(n697, n700);
    let n703: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n690);
    let n704: ZB = zb_and(n566, n703);
    let n705: ZB = zb_and(n568, n704);
    let n706: ZB = zb_not(n705);
    let n707: ZB = zb_and(n702, n706);
    let n708: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n690);
    let n709: ZB = zb_and(n684, n708);
    let n710: ZB = zb_and(n580, n709);
    let n711: ZB = zb_not(n710);
    let n712: ZB = zb_and(n707, n711);
    let n713: ZB = zb_and(n617, n712);
    let n714: ZN = zn_mget(g.cart, n661, n620);
    let n715: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n714);
    let n716: ZB = zb_and(n616, n707);
    let n717: ZB = zb_and(n711, n716);
    let n718: ZB = zb_and(n626, n715);
    let n719: ZB = zb_and(n553, n718);
    let n720: ZB = zb_not(n719);
    let n721: ZB = zb_and(n717, n720);
    let n722: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n714);
    let n723: ZB = zb_and(n559, n722);
    let n724: ZB = zb_not(n723);
    let n725: ZB = zb_and(n721, n723);
    let n726: ZB = zb_and(n721, n724);
    let n727: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n714);
    let n728: ZB = zb_and(n566, n727);
    let n729: ZB = zb_and(n568, n728);
    let n730: ZB = zb_not(n729);
    let n731: ZB = zb_and(n726, n730);
    let n732: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n714);
    let n733: ZB = zb_and(n684, n732);
    let n734: ZB = zb_and(n580, n733);
    let n735: ZB = zb_not(n734);
    let n736: ZB = zb_and(n731, n735);
    let n737: ZB = zb_and(n647, n654);
    let n738: ZB = zb_or(n713, n736);
    let n739: ZB = zsel_b(n617, n654, n737);
    let n740: ZB = zb_or(n689, n738);
    let n741: ZB = zsel_b(n586, n654, n739);
    let n742: ZB = zb_or(n660, n740);
    let n743: ZB = zsel_b(n539, n654, n741);
    let n744: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n520);
    let n745: ZB = zn_le(n744, n524);
    let n746: ZB = zn_gt(n744, n524);
    let n747: ZB = zb_and(n742, n745);
    let n748: ZB = zb_and(n742, n746);
    let n749: ZB = zb_and(n539, n747);
    let n750: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n744);
    let n751: ZN = zn_mget(g.cart, n750, n543);
    let n752: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n751);
    let n753: ZB = zb_and(n538, n742);
    let n754: ZB = zb_and(n745, n753);
    let n755: ZB = zb_and(n551, n752);
    let n756: ZB = zb_and(n553, n755);
    let n757: ZB = zb_not(n756);
    let n758: ZB = zb_and(n754, n757);
    let n759: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n751);
    let n760: ZB = zb_and(n559, n759);
    let n761: ZB = zb_not(n760);
    let n762: ZB = zb_and(n758, n760);
    let n763: ZB = zb_and(n758, n761);
    let n764: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n751);
    let n765: ZB = zb_and(n566, n764);
    let n766: ZB = zb_and(n568, n765);
    let n767: ZB = zb_not(n766);
    let n768: ZB = zb_and(n763, n767);
    let n769: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n751);
    let n770: ZN = zn_mul(n744, zn_splat(P8::from_raw(524288i32)));
    let n771: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n770);
    let n772: ZB = zn_eq(n521, n771);
    let n773: ZB = zb_or(n574, n772);
    let n774: ZB = zb_and(n769, n773);
    let n775: ZB = zb_and(n580, n774);
    let n776: ZB = zb_not(n775);
    let n777: ZB = zb_and(n768, n776);
    let n778: ZB = zb_and(n586, n777);
    let n779: ZN = zn_mget(g.cart, n750, n589);
    let n780: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n779);
    let n781: ZB = zb_and(n585, n768);
    let n782: ZB = zb_and(n776, n781);
    let n783: ZB = zb_and(n595, n780);
    let n784: ZB = zb_and(n553, n783);
    let n785: ZB = zb_not(n784);
    let n786: ZB = zb_and(n782, n785);
    let n787: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n779);
    let n788: ZB = zb_and(n559, n787);
    let n789: ZB = zb_not(n788);
    let n790: ZB = zb_and(n786, n788);
    let n791: ZB = zb_and(n786, n789);
    let n792: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n779);
    let n793: ZB = zb_and(n566, n792);
    let n794: ZB = zb_and(n568, n793);
    let n795: ZB = zb_not(n794);
    let n796: ZB = zb_and(n791, n795);
    let n797: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n779);
    let n798: ZB = zb_and(n773, n797);
    let n799: ZB = zb_and(n580, n798);
    let n800: ZB = zb_not(n799);
    let n801: ZB = zb_and(n796, n800);
    let n802: ZB = zb_and(n617, n801);
    let n803: ZN = zn_mget(g.cart, n750, n620);
    let n804: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n803);
    let n805: ZB = zb_and(n616, n796);
    let n806: ZB = zb_and(n800, n805);
    let n807: ZB = zb_and(n626, n804);
    let n808: ZB = zb_and(n553, n807);
    let n809: ZB = zb_not(n808);
    let n810: ZB = zb_and(n806, n809);
    let n811: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n803);
    let n812: ZB = zb_and(n559, n811);
    let n813: ZB = zb_not(n812);
    let n814: ZB = zb_and(n810, n812);
    let n815: ZB = zb_and(n810, n813);
    let n816: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n803);
    let n817: ZB = zb_and(n566, n816);
    let n818: ZB = zb_and(n568, n817);
    let n819: ZB = zb_not(n818);
    let n820: ZB = zb_and(n815, n819);
    let n821: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n803);
    let n822: ZB = zb_and(n773, n821);
    let n823: ZB = zb_and(n580, n822);
    let n824: ZB = zb_not(n823);
    let n825: ZB = zb_and(n820, n824);
    let n826: ZB = zb_and(n647, n743);
    let n827: ZB = zb_or(n802, n825);
    let n828: ZB = zsel_b(n617, n743, n826);
    let n829: ZB = zb_or(n778, n827);
    let n830: ZB = zsel_b(n586, n743, n828);
    let n831: ZB = zb_or(n749, n829);
    let n832: ZB = zsel_b(n539, n743, n830);
    let n833: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n520);
    let n834: ZB = zn_gt(n833, n524);
    let n835: ZB = zb_and(n832, n834);
    let n836: ZB = zb_or(n748, n831);
    let n837: ZB = zsel_b(n746, n743, n835);
    let n838: ZB = zb_or(n659, n836);
    let n839: ZB = zsel_b(n657, n654, n837);
    let n840: ZB = zb_or(n529, n838);
    let n841: ZB = zsel_b(n527, n516, n839);
    let n842: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n517);
    let n843: ZB = zn_tile_flag_at(g.cache, g.cart, n416, n842, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n844: ZB = zb_not(n843);
    let n845: ZN = zsel_n(n843, zn_splat(P8::from_raw(393216i32)), n258);
    let n846: ZB = zn_gt(n403, r_c396);
    let n847: ZN = zn_sub(n403, r_c394);
    let n848: ZN = zn_max(r_c396, n847);
    let n849: ZN = zn_add(r_c394, n403);
    let n850: ZN = zn_min(r_c396, n849);
    let n851: ZN = zsel_n(n846, n848, n850);
    let n852: ZB = zn_gt(n515, r_c397);
    let n853: ZN = zn_sub(n515, r_c395);
    let n854: ZN = zn_max(r_c397, n853);
    let n855: ZN = zn_add(r_c395, n515);
    let n856: ZN = zn_min(r_c397, n855);
    let n857: ZN = zsel_n(n852, n854, n856);
    let n858: ZN = zsel_n(n844, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n859: ZN = zn_abs(n403);
    let n860: ZB = zn_gt(n859, zn_splat(P8::from_raw(65536i32)));
    let n861: ZB = zn_gt(n403, zn_splat(P8::from_raw(0i32)));
    let n862: ZB = zn_lt(n403, zn_splat(P8::from_raw(0i32)));
    let n863: ZB = zn_gt(n403, zn_splat(P8::from_raw(65536i32)));
    let n864: ZN = zn_sub(n403, zn_splat(P8::from_raw(9830i32)));
    let n865: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n864);
    let n866: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n403);
    let n867: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n866);
    let n868: ZB = zn_gt(n403, zn_splat(P8::from_raw(-65536i32)));
    let n869: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n864);
    let n870: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n866);
    let n871: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n864);
    let n872: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n866);
    let n873: ZN = zsel_n(n868, n869, n870);
    let n874: ZN = zsel_n(n861, n871, n872);
    let n875: ZN = zsel_n(n863, n865, n867);
    let n876: ZN = zsel_n(n862, n873, n874);
    let n877: ZN = zsel_n(n861, n875, n876);
    let n878: ZN = zn_sub(n403, n858);
    let n879: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n878);
    let n880: ZN = zn_add(n403, n858);
    let n881: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n880);
    let n882: ZN = zsel_n(n861, n879, n881);
    let n883: ZN = zsel_n(n860, n877, n882);
    let n884: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n883);
    let n885: ZB = zb_not(n884);
    let n886: ZB = zn_lt(n883, zn_splat(P8::from_raw(0i32)));
    let n887: ZB = zsel_b(n885, n886, r_c398);
    let n888: ZN = zn_abs(n515);
    let n889: ZB = zn_le(n888, zn_splat(P8::from_raw(9830i32)));
    let n890: ZN = zsel_n(n889, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n891: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n517);
    let n892: ZN = zn_add(n515, n890);
    let n893: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n892);
    let n894: ZN = zsel_n(n844, n893, n515);
    let n895: ZB = zn_gt(n845, zn_splat(P8::from_raw(0i32)));
    let n896: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n415);
    let n897: ZB = zn_tile_flag_at(g.cache, g.cart, n896, n891, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n898: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n415);
    let n899: ZB = zn_tile_flag_at(g.cache, g.cart, n898, n891, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n900: ZN = zsel_n(n899, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n901: ZN = zsel_n(n897, zn_splat(P8::from_raw(-65536i32)), n900);
    let n902: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n901);
    let n903: ZB = zb_not(n902);
    let n904: ZN = zn_neg(n901);
    let n905: ZN = zn_mul(n904, zn_splat(P8::from_raw(131072i32)));
    let n906: ZN = zsel_n(n903, n905, n883);
    let n907: ZN = zsel_n(n903, zn_splat(P8::from_raw(-131072i32)), n894);
    let n908: ZN = zsel_n(n895, zn_splat(P8::from_raw(0i32)), n845);
    let n909: ZN = zsel_n(n895, n883, n906);
    let n910: ZN = zsel_n(n895, zn_splat(P8::from_raw(-131072i32)), n907);
    let n911: ZN = zsel_n(n887, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n912: ZB = zn_gt(n911, zn_splat(P8::from_raw(0i32)));
    let n913: ZB = zn_lt(n911, zn_splat(P8::from_raw(0i32)));
    let n914: ZN = zsel_n(n913, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n915: ZN = zsel_n(n912, zn_splat(P8::from_raw(131072i32)), n914);
    let n916: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n911);
    let n917: ZB = zb_not(n916);
    let n918: ZN = zsel_n(n917, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n919: ZB = zsel_b(n260, r_c398, n887);
    let n920: ZN = zsel_n(n260, n851, n883);
    let n921: ZN = zsel_n(n260, n857, n894);
    let n923: ZB = zn_lt(n401, zn_splat(P8::from_raw(-65536i32)));
    let n924: ZB = zn_gt(n401, zn_splat(P8::from_raw(7929856i32)));
    let n925: ZB = zb_or(n923, n924);
    let n926: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n401);
    let n927: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n926);
    let n928: ZN = zsel_n(n925, n927, n401);
    let n929: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n920);
    let n930: ZN = zsel_n(n263, n401, n928);
    let n931: ZN = zsel_n(n263, n920, n929);
    let n944: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n878);
    let n945: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n880);
    let n946: ZN = zsel_n(n868, n944, n945);
    let n947: ZN = zsel_n(n860, n877, n946);
    let n948: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n947);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zn_lt(n947, zn_splat(P8::from_raw(0i32)));
    let n951: ZB = zsel_b(n949, n950, r_c398);
    let n952: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n415);
    let n953: ZB = zn_tile_flag_at(g.cache, g.cart, n952, n891, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n954: ZN = zsel_n(n953, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n955: ZN = zn_min(n892, n954);
    let n956: ZN = zsel_n(n844, n955, n515);
    let n957: ZN = zsel_n(n903, n905, n947);
    let n958: ZN = zsel_n(n903, zn_splat(P8::from_raw(-131072i32)), n956);
    let n959: ZN = zsel_n(n895, n947, n957);
    let n960: ZN = zsel_n(n895, zn_splat(P8::from_raw(-131072i32)), n958);
    let n961: ZB = zsel_b(n260, r_c398, n951);
    let n962: ZN = zsel_n(n260, n851, n947);
    let n963: ZN = zsel_n(n260, n857, n956);
    let n964: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n962);
    let n965: ZN = zsel_n(n263, n962, n964);
    let n966: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n878);
    let n967: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n880);
    let n968: ZN = zsel_n(n863, n966, n967);
    let n969: ZN = zsel_n(n860, n877, n968);
    let n970: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n969);
    let n971: ZB = zb_not(n970);
    let n972: ZB = zn_lt(n969, zn_splat(P8::from_raw(0i32)));
    let n973: ZB = zsel_b(n971, n972, r_c398);
    let n974: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n415);
    let n975: ZB = zn_tile_flag_at(g.cache, g.cart, n974, n891, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n976: ZN = zsel_n(n975, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n977: ZN = zn_min(n892, n976);
    let n978: ZN = zsel_n(n844, n977, n515);
    let n979: ZN = zsel_n(n903, n905, n969);
    let n980: ZN = zsel_n(n903, zn_splat(P8::from_raw(-131072i32)), n978);
    let n981: ZN = zsel_n(n895, n969, n979);
    let n982: ZN = zsel_n(n895, zn_splat(P8::from_raw(-131072i32)), n980);
    let n983: ZB = zsel_b(n260, r_c398, n973);
    let n984: ZN = zsel_n(n260, n851, n969);
    let n985: ZN = zsel_n(n260, n857, n978);
    let n986: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n984);
    let n987: ZN = zsel_n(n263, n984, n986);
    let n988: ZN = zsel_n(n254, n908, n845);
    let n989: ZN = zsel_n(n254, n909, n883);
    let n990: ZN = zsel_n(n254, n910, n894);
    let n991: ZN = zsel_n(n260, n845, n988);
    let n992: ZN = zsel_n(n260, n851, n989);
    let n993: ZN = zsel_n(n260, n857, n990);
    let n994: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n992);
    let n995: ZN = zsel_n(n263, n992, n994);
    let n996: ZN = zsel_n(n254, n959, n947);
    let n997: ZN = zsel_n(n254, n960, n956);
    let n998: ZN = zsel_n(n260, n851, n996);
    let n999: ZN = zsel_n(n260, n857, n997);
    let n1000: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n998);
    let n1001: ZN = zsel_n(n263, n998, n1000);
    let n1002: ZN = zsel_n(n254, n981, n969);
    let n1003: ZN = zsel_n(n254, n982, n978);
    let n1004: ZN = zsel_n(n260, n851, n1002);
    let n1005: ZN = zsel_n(n260, n857, n1003);
    let n1006: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1004);
    let n1007: ZN = zsel_n(n263, n1004, n1006);
    let n1008: ZN = zsel_n(n255, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1009: ZB = zb_or(r_c41, n255);
    let n1010: ZN = zsel_n(n255, zn_splat(P8::from_raw(655360i32)), n259);
    let n1011: ZN = zsel_n(n255, zn_splat(P8::from_raw(262144i32)), r_c299);
    let n1012: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1013: ZN = zsel_n(n255, zn_splat(P8::from_raw(98304i32)), r_c394);
    let n1014: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), r_c397);
    let n1015: ZN = zsel_n(n260, r_c20, n1008);
    let n1016: ZB = zsel_b(n260, r_c41, n1009);
    let n1017: ZN = zsel_n(n260, n259, n1010);
    let n1018: ZN = zsel_n(n260, n261, n1011);
    let n1019: ZN = zsel_n(n260, zn_splat(P8::from_raw(65536i32)), n1012);
    let n1020: ZN = zsel_n(n260, r_c394, n1013);
    let n1021: ZN = zsel_n(n260, r_c397, n1014);
    let n1022: ZB = zn_gt(n1015, zn_splat(P8::from_raw(0i32)));
    let n1023: ZN = zsel_n(n255, n918, r_c395);
    let n1024: ZN = zsel_n(n255, n915, r_c396);
    let n1025: ZN = zsel_n(n255, n911, n883);
    let n1026: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n894);
    let n1027: ZN = zsel_n(n260, r_c395, n1023);
    let n1028: ZN = zsel_n(n260, r_c396, n1024);
    let n1029: ZN = zsel_n(n260, n851, n1025);
    let n1030: ZN = zsel_n(n260, n857, n1026);
    let n1031: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1029);
    let n1032: ZN = zsel_n(n1022, n401, n928);
    let n1033: ZN = zsel_n(n1022, n1029, n1031);
    let n1034: ZN = zsel_n(n255, zn_splat(P8::from_raw(69510i32)), r_c395);
    let n1035: ZN = zsel_n(n255, zn_splat(P8::from_raw(-131072i32)), r_c396);
    let n1036: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n947);
    let n1037: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n956);
    let n1038: ZN = zsel_n(n260, r_c395, n1034);
    let n1039: ZN = zsel_n(n260, r_c396, n1035);
    let n1040: ZN = zsel_n(n260, n851, n1036);
    let n1041: ZN = zsel_n(n260, n857, n1037);
    let n1042: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1040);
    let n1043: ZN = zsel_n(n1022, n1040, n1042);
    let n1044: ZN = zsel_n(n255, zn_splat(P8::from_raw(131072i32)), r_c396);
    let n1045: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n969);
    let n1046: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n978);
    let n1047: ZN = zsel_n(n260, r_c396, n1044);
    let n1048: ZN = zsel_n(n260, n851, n1045);
    let n1049: ZN = zsel_n(n260, n857, n1046);
    let n1050: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1048);
    let n1051: ZN = zsel_n(n1022, n1048, n1050);
    let n1053: ZN = zsel_n(n255, zn_splat(P8::from_raw(69510i32)), r_c394);
    let n1054: ZN = zsel_n(n255, zn_splat(P8::from_raw(-98304i32)), r_c397);
    let n1055: ZN = zsel_n(n260, r_c394, n1053);
    let n1056: ZN = zsel_n(n260, r_c397, n1054);
    let n1057: ZN = zsel_n(n255, zn_splat(P8::from_raw(98304i32)), r_c395);
    let n1058: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), r_c396);
    let n1059: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n883);
    let n1060: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n894);
    let n1061: ZN = zsel_n(n260, r_c395, n1057);
    let n1062: ZN = zsel_n(n260, r_c396, n1058);
    let n1063: ZN = zsel_n(n260, n851, n1059);
    let n1064: ZN = zsel_n(n260, n857, n1060);
    let n1065: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1063);
    let n1066: ZN = zsel_n(n1022, n1063, n1065);
    let n1067: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n947);
    let n1068: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n956);
    let n1069: ZN = zsel_n(n260, n851, n1067);
    let n1070: ZN = zsel_n(n260, n857, n1068);
    let n1071: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1069);
    let n1072: ZN = zsel_n(n1022, n1069, n1071);
    let n1073: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n969);
    let n1074: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n978);
    let n1075: ZN = zsel_n(n260, n851, n1073);
    let n1076: ZN = zsel_n(n260, n857, n1074);
    let n1077: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1075);
    let n1078: ZN = zsel_n(n1022, n1075, n1077);
    let n1079: ZN = zsel_n(n255, zn_splat(P8::from_raw(131072i32)), r_c397);
    let n1080: ZN = zsel_n(n260, r_c397, n1079);
    let n1081: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n894);
    let n1082: ZN = zsel_n(n260, n857, n1081);
    let n1083: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n956);
    let n1084: ZN = zsel_n(n260, n857, n1083);
    let n1085: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n978);
    let n1086: ZN = zsel_n(n260, n857, n1085);
    let n1087: ZN = zsel_n(n255, n911, n989);
    let n1088: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n990);
    let n1089: ZN = zsel_n(n260, n851, n1087);
    let n1090: ZN = zsel_n(n260, n857, n1088);
    let n1091: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1089);
    let n1092: ZN = zsel_n(n1022, n1089, n1091);
    let n1093: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n996);
    let n1094: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n997);
    let n1095: ZN = zsel_n(n260, n851, n1093);
    let n1096: ZN = zsel_n(n260, n857, n1094);
    let n1097: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1095);
    let n1098: ZN = zsel_n(n1022, n1095, n1097);
    let n1099: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n1002);
    let n1100: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n1003);
    let n1101: ZN = zsel_n(n260, n851, n1099);
    let n1102: ZN = zsel_n(n260, n857, n1100);
    let n1103: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1101);
    let n1104: ZN = zsel_n(n1022, n1101, n1103);
    let n1105: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n989);
    let n1106: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n990);
    let n1107: ZN = zsel_n(n260, n851, n1105);
    let n1108: ZN = zsel_n(n260, n857, n1106);
    let n1109: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1107);
    let n1110: ZN = zsel_n(n1022, n1107, n1109);
    let n1111: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n996);
    let n1112: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n997);
    let n1113: ZN = zsel_n(n260, n851, n1111);
    let n1114: ZN = zsel_n(n260, n857, n1112);
    let n1115: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1113);
    let n1116: ZN = zsel_n(n1022, n1113, n1115);
    let n1117: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n1002);
    let n1118: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n1003);
    let n1119: ZN = zsel_n(n260, n851, n1117);
    let n1120: ZN = zsel_n(n260, n857, n1118);
    let n1121: ZN = zsel_n(n925, zn_splat(P8::from_raw(0i32)), n1119);
    let n1122: ZN = zsel_n(n1022, n1119, n1121);
    let n1123: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n990);
    let n1124: ZN = zsel_n(n260, n857, n1123);
    let n1125: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n997);
    let n1126: ZN = zsel_n(n260, n857, n1125);
    let n1127: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n1003);
    let n1128: ZN = zsel_n(n260, n857, n1127);
    let n1129: ZB = zb_not(n281);
    let n1130: ZB = zb_and(n276, n1129);
    let n1131: ZB = zn_lt(r_c407, zn_splat(P8::from_raw(0i32)));
    let n1132: ZB = zb_and(n282, n1131);
    let n1133: ZB = zb_or(n1130, n1132);
    let n1134: ZB = zb_and(n272, n288);
    let n1135: ZB = zb_and(n290, n1134);
    let n1136: ZB = zb_and(n280, n1135);
    let n1137: ZB = zb_and(n1133, n1136);
    let n1138: ZB = zb_and(n272, n280);
    let n1139: ZB = zb_and(n283, n1138);
    let n1140: ZB = zb_and(n288, n1139);
    let n1141: ZB = zb_and(n290, n1140);
    let n1142: ZB = zb_and(n1133, n1141);
    let n1143: ZB = zb_and(n526, n1142);
    let n1144: ZB = zb_and(n527, n1142);
    let n1145: ZB = zb_and(n539, n1143);
    let n1146: ZB = zb_and(n526, n538);
    let n1147: ZB = zb_and(n1142, n1146);
    let n1148: ZB = zb_and(n555, n1147);
    let n1149: ZB = zb_and(n560, n1148);
    let n1150: ZB = zb_and(n561, n1148);
    let n1151: ZB = zb_and(n570, n1150);
    let n1152: ZB = zb_and(n582, n1151);
    let n1153: ZB = zb_and(n586, n1152);
    let n1154: ZB = zb_and(n582, n585);
    let n1155: ZB = zb_and(n1151, n1154);
    let n1156: ZB = zb_and(n598, n1155);
    let n1157: ZB = zb_and(n601, n1156);
    let n1158: ZB = zb_and(n602, n1156);
    let n1159: ZB = zb_and(n608, n1158);
    let n1160: ZB = zb_and(n613, n1159);
    let n1161: ZB = zb_and(n617, n1160);
    let n1162: ZB = zb_and(n613, n616);
    let n1163: ZB = zb_and(n1159, n1162);
    let n1164: ZB = zb_and(n629, n1163);
    let n1165: ZB = zb_and(n632, n1164);
    let n1166: ZB = zb_and(n633, n1164);
    let n1167: ZB = zb_and(n639, n1166);
    let n1168: ZB = zb_and(n644, n1167);
    let n1169: ZB = zb_or(n1161, n1168);
    let n1170: ZB = zb_or(n1153, n1169);
    let n1171: ZB = zb_or(n1145, n1170);
    let n1172: ZB = zb_and(n656, n1171);
    let n1173: ZB = zb_and(n657, n1171);
    let n1174: ZB = zb_and(n539, n1172);
    let n1175: ZB = zb_and(n538, n656);
    let n1176: ZB = zb_and(n1171, n1175);
    let n1177: ZB = zb_and(n668, n1176);
    let n1178: ZB = zb_and(n671, n1177);
    let n1179: ZB = zb_and(n672, n1177);
    let n1180: ZB = zb_and(n678, n1179);
    let n1181: ZB = zb_and(n687, n1180);
    let n1182: ZB = zb_and(n586, n1181);
    let n1183: ZB = zb_and(n585, n687);
    let n1184: ZB = zb_and(n1180, n1183);
    let n1185: ZB = zb_and(n696, n1184);
    let n1186: ZB = zb_and(n699, n1185);
    let n1187: ZB = zb_and(n700, n1185);
    let n1188: ZB = zb_and(n706, n1187);
    let n1189: ZB = zb_and(n711, n1188);
    let n1190: ZB = zb_and(n617, n1189);
    let n1191: ZB = zb_and(n616, n711);
    let n1192: ZB = zb_and(n1188, n1191);
    let n1193: ZB = zb_and(n720, n1192);
    let n1194: ZB = zb_and(n723, n1193);
    let n1195: ZB = zb_and(n724, n1193);
    let n1196: ZB = zb_and(n730, n1195);
    let n1197: ZB = zb_and(n735, n1196);
    let n1198: ZB = zb_or(n1190, n1197);
    let n1199: ZB = zb_or(n1182, n1198);
    let n1200: ZB = zb_or(n1174, n1199);
    let n1201: ZB = zb_and(n745, n1200);
    let n1202: ZB = zb_and(n746, n1200);
    let n1203: ZB = zb_and(n539, n1201);
    let n1204: ZB = zb_and(n538, n745);
    let n1205: ZB = zb_and(n1200, n1204);
    let n1206: ZB = zb_and(n757, n1205);
    let n1207: ZB = zb_and(n760, n1206);
    let n1208: ZB = zb_and(n761, n1206);
    let n1209: ZB = zb_and(n767, n1208);
    let n1210: ZB = zb_and(n776, n1209);
    let n1211: ZB = zb_and(n586, n1210);
    let n1212: ZB = zb_and(n585, n776);
    let n1213: ZB = zb_and(n1209, n1212);
    let n1214: ZB = zb_and(n785, n1213);
    let n1215: ZB = zb_and(n788, n1214);
    let n1216: ZB = zb_and(n789, n1214);
    let n1217: ZB = zb_and(n795, n1216);
    let n1218: ZB = zb_and(n800, n1217);
    let n1219: ZB = zb_and(n617, n1218);
    let n1220: ZB = zb_and(n616, n800);
    let n1221: ZB = zb_and(n1217, n1220);
    let n1222: ZB = zb_and(n809, n1221);
    let n1223: ZB = zb_and(n812, n1222);
    let n1224: ZB = zb_and(n813, n1222);
    let n1225: ZB = zb_and(n819, n1224);
    let n1226: ZB = zb_and(n824, n1225);
    let n1227: ZB = zb_or(n1219, n1226);
    let n1228: ZB = zb_or(n1211, n1227);
    let n1229: ZB = zb_or(n1203, n1228);
    let n1230: ZB = zb_or(n1202, n1229);
    let n1231: ZB = zb_or(n1173, n1230);
    let n1232: ZB = zb_or(n1144, n1231);
    let n1235: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c246);
    let n1236: ZN = zn_div(n1235, zn_splat(P8::from_raw(2621440i32)));
    let n1237: ZN = zn_sin(n1236);
    let n1239: ZN = zn_mul(n1237, zn_splat(P8::from_raw(163840i32)));
    let n1240: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n1239);
    let n1241: ZB = zb_not(n270);
    let n1242: ZB = zb_and(n177, n1241);
    let n1243: ZB = zb_and(n281, n1242);
    let n1244: ZB = zb_and(n177, n271);
    let n1245: ZB = zb_and(n272, n1244);
    let n1246: ZB = zb_and(n278, n1245);
    let n1247: ZB = zb_and(n280, n1246);
    let n1248: ZB = zb_and(n283, n1247);
    let n1249: ZB = zb_and(n1241, n1248);
    let n1250: ZB = zb_and(n526, n1249);
    let n1251: ZB = zb_and(n527, n1249);
    let n1252: ZB = zb_and(n539, n1250);
    let n1253: ZB = zb_and(n1146, n1249);
    let n1254: ZB = zb_and(n555, n1253);
    let n1255: ZB = zb_and(n560, n1254);
    let n1256: ZB = zb_and(n561, n1254);
    let n1257: ZB = zb_and(n570, n1256);
    let n1258: ZB = zb_and(n582, n1257);
    let n1259: ZB = zb_and(n586, n1258);
    let n1260: ZB = zb_and(n1154, n1257);
    let n1261: ZB = zb_and(n598, n1260);
    let n1262: ZB = zb_and(n601, n1261);
    let n1263: ZB = zb_and(n602, n1261);
    let n1264: ZB = zb_and(n608, n1263);
    let n1265: ZB = zb_and(n613, n1264);
    let n1266: ZB = zb_and(n617, n1265);
    let n1267: ZB = zb_and(n1162, n1264);
    let n1268: ZB = zb_and(n629, n1267);
    let n1269: ZB = zb_and(n632, n1268);
    let n1270: ZB = zb_and(n633, n1268);
    let n1271: ZB = zb_and(n639, n1270);
    let n1272: ZB = zb_and(n644, n1271);
    let n1273: ZB = zb_or(n1266, n1272);
    let n1274: ZB = zb_or(n1259, n1273);
    let n1275: ZB = zb_or(n1252, n1274);
    let n1276: ZB = zb_and(n656, n1275);
    let n1277: ZB = zb_and(n657, n1275);
    let n1278: ZB = zb_and(n539, n1276);
    let n1279: ZB = zb_and(n1175, n1275);
    let n1280: ZB = zb_and(n668, n1279);
    let n1281: ZB = zb_and(n671, n1280);
    let n1282: ZB = zb_and(n672, n1280);
    let n1283: ZB = zb_and(n678, n1282);
    let n1284: ZB = zb_and(n687, n1283);
    let n1285: ZB = zb_and(n586, n1284);
    let n1286: ZB = zb_and(n1183, n1283);
    let n1287: ZB = zb_and(n696, n1286);
    let n1288: ZB = zb_and(n699, n1287);
    let n1289: ZB = zb_and(n700, n1287);
    let n1290: ZB = zb_and(n706, n1289);
    let n1291: ZB = zb_and(n711, n1290);
    let n1292: ZB = zb_and(n617, n1291);
    let n1293: ZB = zb_and(n1191, n1290);
    let n1294: ZB = zb_and(n720, n1293);
    let n1295: ZB = zb_and(n723, n1294);
    let n1296: ZB = zb_and(n724, n1294);
    let n1297: ZB = zb_and(n730, n1296);
    let n1298: ZB = zb_and(n735, n1297);
    let n1299: ZB = zb_or(n1292, n1298);
    let n1300: ZB = zb_or(n1285, n1299);
    let n1301: ZB = zb_or(n1278, n1300);
    let n1302: ZB = zb_and(n745, n1301);
    let n1303: ZB = zb_and(n746, n1301);
    let n1304: ZB = zb_and(n539, n1302);
    let n1305: ZB = zb_and(n1204, n1301);
    let n1306: ZB = zb_and(n757, n1305);
    let n1307: ZB = zb_and(n760, n1306);
    let n1308: ZB = zb_and(n761, n1306);
    let n1309: ZB = zb_and(n767, n1308);
    let n1310: ZB = zb_and(n776, n1309);
    let n1311: ZB = zb_and(n586, n1310);
    let n1312: ZB = zb_and(n1212, n1309);
    let n1313: ZB = zb_and(n785, n1312);
    let n1314: ZB = zb_and(n788, n1313);
    let n1315: ZB = zb_and(n789, n1313);
    let n1316: ZB = zb_and(n795, n1315);
    let n1317: ZB = zb_and(n800, n1316);
    let n1318: ZB = zb_and(n617, n1317);
    let n1319: ZB = zb_and(n1220, n1316);
    let n1320: ZB = zb_and(n809, n1319);
    let n1321: ZB = zb_and(n812, n1320);
    let n1322: ZB = zb_and(n813, n1320);
    let n1323: ZB = zb_and(n819, n1322);
    let n1324: ZB = zb_and(n824, n1323);
    let n1325: ZB = zb_or(n1318, n1324);
    let n1326: ZB = zb_or(n1311, n1325);
    let n1327: ZB = zb_or(n1304, n1326);
    let n1328: ZB = zb_or(n1303, n1327);
    let n1329: ZB = zb_or(n1277, n1328);
    let n1330: ZB = zb_or(n1251, n1329);
    let n1332: ZB = zb_and(n1129, n1242);
    let n1333: ZB = zb_and(n1131, n1243);
    let n1334: ZB = zb_or(n1332, n1333);
    let n1335: ZB = zb_and(n1136, n1334);
    let n1336: ZB = zb_and(n1141, n1334);
    let n1337: ZB = zb_and(n526, n1336);
    let n1338: ZB = zb_and(n527, n1336);
    let n1339: ZB = zb_and(n539, n1337);
    let n1340: ZB = zb_and(n1146, n1336);
    let n1341: ZB = zb_and(n555, n1340);
    let n1342: ZB = zb_and(n560, n1341);
    let n1343: ZB = zb_and(n561, n1341);
    let n1344: ZB = zb_and(n570, n1343);
    let n1345: ZB = zb_and(n582, n1344);
    let n1346: ZB = zb_and(n586, n1345);
    let n1347: ZB = zb_and(n1154, n1344);
    let n1348: ZB = zb_and(n598, n1347);
    let n1349: ZB = zb_and(n601, n1348);
    let n1350: ZB = zb_and(n602, n1348);
    let n1351: ZB = zb_and(n608, n1350);
    let n1352: ZB = zb_and(n613, n1351);
    let n1353: ZB = zb_and(n617, n1352);
    let n1354: ZB = zb_and(n1162, n1351);
    let n1355: ZB = zb_and(n629, n1354);
    let n1356: ZB = zb_and(n632, n1355);
    let n1357: ZB = zb_and(n633, n1355);
    let n1358: ZB = zb_and(n639, n1357);
    let n1359: ZB = zb_and(n644, n1358);
    let n1360: ZB = zb_or(n1353, n1359);
    let n1361: ZB = zb_or(n1346, n1360);
    let n1362: ZB = zb_or(n1339, n1361);
    let n1363: ZB = zb_and(n656, n1362);
    let n1364: ZB = zb_and(n657, n1362);
    let n1365: ZB = zb_and(n539, n1363);
    let n1366: ZB = zb_and(n1175, n1362);
    let n1367: ZB = zb_and(n668, n1366);
    let n1368: ZB = zb_and(n671, n1367);
    let n1369: ZB = zb_and(n672, n1367);
    let n1370: ZB = zb_and(n678, n1369);
    let n1371: ZB = zb_and(n687, n1370);
    let n1372: ZB = zb_and(n586, n1371);
    let n1373: ZB = zb_and(n1183, n1370);
    let n1374: ZB = zb_and(n696, n1373);
    let n1375: ZB = zb_and(n699, n1374);
    let n1376: ZB = zb_and(n700, n1374);
    let n1377: ZB = zb_and(n706, n1376);
    let n1378: ZB = zb_and(n711, n1377);
    let n1379: ZB = zb_and(n617, n1378);
    let n1380: ZB = zb_and(n1191, n1377);
    let n1381: ZB = zb_and(n720, n1380);
    let n1382: ZB = zb_and(n723, n1381);
    let n1383: ZB = zb_and(n724, n1381);
    let n1384: ZB = zb_and(n730, n1383);
    let n1385: ZB = zb_and(n735, n1384);
    let n1386: ZB = zb_or(n1379, n1385);
    let n1387: ZB = zb_or(n1372, n1386);
    let n1388: ZB = zb_or(n1365, n1387);
    let n1389: ZB = zb_and(n745, n1388);
    let n1390: ZB = zb_and(n746, n1388);
    let n1391: ZB = zb_and(n539, n1389);
    let n1392: ZB = zb_and(n1204, n1388);
    let n1393: ZB = zb_and(n757, n1392);
    let n1394: ZB = zb_and(n760, n1393);
    let n1395: ZB = zb_and(n761, n1393);
    let n1396: ZB = zb_and(n767, n1395);
    let n1397: ZB = zb_and(n776, n1396);
    let n1398: ZB = zb_and(n586, n1397);
    let n1399: ZB = zb_and(n1212, n1396);
    let n1400: ZB = zb_and(n785, n1399);
    let n1401: ZB = zb_and(n788, n1400);
    let n1402: ZB = zb_and(n789, n1400);
    let n1403: ZB = zb_and(n795, n1402);
    let n1404: ZB = zb_and(n800, n1403);
    let n1405: ZB = zb_and(n617, n1404);
    let n1406: ZB = zb_and(n1220, n1403);
    let n1407: ZB = zb_and(n809, n1406);
    let n1408: ZB = zb_and(n812, n1407);
    let n1409: ZB = zb_and(n813, n1407);
    let n1410: ZB = zb_and(n819, n1409);
    let n1411: ZB = zb_and(n824, n1410);
    let n1412: ZB = zb_or(n1405, n1411);
    let n1413: ZB = zb_or(n1398, n1412);
    let n1414: ZB = zb_or(n1391, n1413);
    let n1415: ZB = zb_or(n1390, n1414);
    let n1416: ZB = zb_or(n1364, n1415);
    let n1417: ZB = zb_or(n1338, n1416);
    let n1419: ZB = zb_and(n540, n554);
    let n1420: ZB = zb_and(n563, n569);
    let n1421: ZB = zb_and(n571, n581);
    let n1422: ZB = zb_or(n1420, n1421);
    let n1423: ZB = zb_or(n562, n1422);
    let n1424: ZB = zb_or(n1419, n1423);
    let n1425: ZB = zb_and(n587, n597);
    let n1426: ZB = zb_and(n604, n607);
    let n1427: ZB = zb_and(n609, n612);
    let n1428: ZB = zb_or(n1426, n1427);
    let n1429: ZB = zb_or(n603, n1428);
    let n1430: ZB = zb_or(n1425, n1429);
    let n1431: ZB = zb_and(n618, n628);
    let n1432: ZB = zb_and(n635, n638);
    let n1433: ZB = zb_and(n640, n643);
    let n1434: ZB = zb_or(n1432, n1433);
    let n1435: ZB = zb_or(n634, n1434);
    let n1436: ZB = zb_or(n1431, n1435);
    let n1437: ZB = zb_or(n1430, n1436);
    let n1438: ZB = zb_or(n1424, n1437);
    let n1439: ZB = zb_and(n665, n667);
    let n1440: ZB = zb_and(n674, n677);
    let n1441: ZB = zb_and(n679, n686);
    let n1442: ZB = zb_or(n1440, n1441);
    let n1443: ZB = zb_or(n673, n1442);
    let n1444: ZB = zb_or(n1439, n1443);
    let n1445: ZB = zb_and(n693, n695);
    let n1446: ZB = zb_and(n702, n705);
    let n1447: ZB = zb_and(n707, n710);
    let n1448: ZB = zb_or(n1446, n1447);
    let n1449: ZB = zb_or(n701, n1448);
    let n1450: ZB = zb_or(n1445, n1449);
    let n1451: ZB = zb_and(n717, n719);
    let n1452: ZB = zb_and(n726, n729);
    let n1453: ZB = zb_and(n731, n734);
    let n1454: ZB = zb_or(n1452, n1453);
    let n1455: ZB = zb_or(n725, n1454);
    let n1456: ZB = zb_or(n1451, n1455);
    let n1457: ZB = zb_or(n1450, n1456);
    let n1458: ZB = zb_or(n1444, n1457);
    let n1459: ZB = zb_and(n754, n756);
    let n1460: ZB = zb_and(n763, n766);
    let n1461: ZB = zb_and(n768, n775);
    let n1462: ZB = zb_or(n1460, n1461);
    let n1463: ZB = zb_or(n762, n1462);
    let n1464: ZB = zb_or(n1459, n1463);
    let n1465: ZB = zb_and(n782, n784);
    let n1466: ZB = zb_and(n791, n794);
    let n1467: ZB = zb_and(n796, n799);
    let n1468: ZB = zb_or(n1466, n1467);
    let n1469: ZB = zb_or(n790, n1468);
    let n1470: ZB = zb_or(n1465, n1469);
    let n1471: ZB = zb_and(n806, n808);
    let n1472: ZB = zb_and(n815, n818);
    let n1473: ZB = zb_and(n820, n823);
    let n1474: ZB = zb_or(n1472, n1473);
    let n1475: ZB = zb_or(n814, n1474);
    let n1476: ZB = zb_or(n1471, n1475);
    let n1477: ZB = zb_or(n1470, n1476);
    let n1478: ZB = zb_or(n1464, n1477);
    let n1479: ZB = zb_or(n1458, n1478);
    let n1480: ZB = zsel_b(n1458, n654, n743);
    let n1481: ZB = zb_or(n1438, n1479);
    let n1482: ZB = zsel_b(n1438, n516, n1480);
    let n1483: ZB = zsel_b(n1481, n1482, n841);
    let n1484: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1485: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1484);
    let n1487: ZB = zb_and(n554, n1147);
    let n1488: ZB = zb_and(n569, n1150);
    let n1489: ZB = zb_and(n581, n1151);
    let n1490: ZB = zb_or(n1488, n1489);
    let n1491: ZB = zb_or(n1149, n1490);
    let n1492: ZB = zb_or(n1487, n1491);
    let n1493: ZB = zb_and(n597, n1155);
    let n1494: ZB = zb_and(n607, n1158);
    let n1495: ZB = zb_and(n612, n1159);
    let n1496: ZB = zb_or(n1494, n1495);
    let n1497: ZB = zb_or(n1157, n1496);
    let n1498: ZB = zb_or(n1493, n1497);
    let n1499: ZB = zb_and(n628, n1163);
    let n1500: ZB = zb_and(n638, n1166);
    let n1501: ZB = zb_and(n643, n1167);
    let n1502: ZB = zb_or(n1500, n1501);
    let n1503: ZB = zb_or(n1165, n1502);
    let n1504: ZB = zb_or(n1499, n1503);
    let n1505: ZB = zb_or(n1498, n1504);
    let n1506: ZB = zb_or(n1492, n1505);
    let n1507: ZB = zb_and(n667, n1176);
    let n1508: ZB = zb_and(n677, n1179);
    let n1509: ZB = zb_and(n686, n1180);
    let n1510: ZB = zb_or(n1508, n1509);
    let n1511: ZB = zb_or(n1178, n1510);
    let n1512: ZB = zb_or(n1507, n1511);
    let n1513: ZB = zb_and(n695, n1184);
    let n1514: ZB = zb_and(n705, n1187);
    let n1515: ZB = zb_and(n710, n1188);
    let n1516: ZB = zb_or(n1514, n1515);
    let n1517: ZB = zb_or(n1186, n1516);
    let n1518: ZB = zb_or(n1513, n1517);
    let n1519: ZB = zb_and(n719, n1192);
    let n1520: ZB = zb_and(n729, n1195);
    let n1521: ZB = zb_and(n734, n1196);
    let n1522: ZB = zb_or(n1520, n1521);
    let n1523: ZB = zb_or(n1194, n1522);
    let n1524: ZB = zb_or(n1519, n1523);
    let n1525: ZB = zb_or(n1518, n1524);
    let n1526: ZB = zb_or(n1512, n1525);
    let n1527: ZB = zb_and(n756, n1205);
    let n1528: ZB = zb_and(n766, n1208);
    let n1529: ZB = zb_and(n775, n1209);
    let n1530: ZB = zb_or(n1528, n1529);
    let n1531: ZB = zb_or(n1207, n1530);
    let n1532: ZB = zb_or(n1527, n1531);
    let n1533: ZB = zb_and(n784, n1213);
    let n1534: ZB = zb_and(n794, n1216);
    let n1535: ZB = zb_and(n799, n1217);
    let n1536: ZB = zb_or(n1534, n1535);
    let n1537: ZB = zb_or(n1215, n1536);
    let n1538: ZB = zb_or(n1533, n1537);
    let n1539: ZB = zb_and(n808, n1221);
    let n1540: ZB = zb_and(n818, n1224);
    let n1541: ZB = zb_and(n823, n1225);
    let n1542: ZB = zb_or(n1540, n1541);
    let n1543: ZB = zb_or(n1223, n1542);
    let n1544: ZB = zb_or(n1539, n1543);
    let n1545: ZB = zb_or(n1538, n1544);
    let n1546: ZB = zb_or(n1532, n1545);
    let n1547: ZB = zb_or(n1526, n1546);
    let n1548: ZB = zsel_b(n1526, n654, n743);
    let n1549: ZB = zb_or(n1506, n1547);
    let n1550: ZB = zsel_b(n1506, n516, n1548);
    let n1551: ZB = zsel_b(n1549, n1550, n841);
    let n1553: ZB = zb_and(n554, n1253);
    let n1554: ZB = zb_and(n569, n1256);
    let n1555: ZB = zb_and(n581, n1257);
    let n1556: ZB = zb_or(n1554, n1555);
    let n1557: ZB = zb_or(n1255, n1556);
    let n1558: ZB = zb_or(n1553, n1557);
    let n1559: ZB = zb_and(n597, n1260);
    let n1560: ZB = zb_and(n607, n1263);
    let n1561: ZB = zb_and(n612, n1264);
    let n1562: ZB = zb_or(n1560, n1561);
    let n1563: ZB = zb_or(n1262, n1562);
    let n1564: ZB = zb_or(n1559, n1563);
    let n1565: ZB = zb_and(n628, n1267);
    let n1566: ZB = zb_and(n638, n1270);
    let n1567: ZB = zb_and(n643, n1271);
    let n1568: ZB = zb_or(n1566, n1567);
    let n1569: ZB = zb_or(n1269, n1568);
    let n1570: ZB = zb_or(n1565, n1569);
    let n1571: ZB = zb_or(n1564, n1570);
    let n1572: ZB = zb_or(n1558, n1571);
    let n1573: ZB = zb_and(n667, n1279);
    let n1574: ZB = zb_and(n677, n1282);
    let n1575: ZB = zb_and(n686, n1283);
    let n1576: ZB = zb_or(n1574, n1575);
    let n1577: ZB = zb_or(n1281, n1576);
    let n1578: ZB = zb_or(n1573, n1577);
    let n1579: ZB = zb_and(n695, n1286);
    let n1580: ZB = zb_and(n705, n1289);
    let n1581: ZB = zb_and(n710, n1290);
    let n1582: ZB = zb_or(n1580, n1581);
    let n1583: ZB = zb_or(n1288, n1582);
    let n1584: ZB = zb_or(n1579, n1583);
    let n1585: ZB = zb_and(n719, n1293);
    let n1586: ZB = zb_and(n729, n1296);
    let n1587: ZB = zb_and(n734, n1297);
    let n1588: ZB = zb_or(n1586, n1587);
    let n1589: ZB = zb_or(n1295, n1588);
    let n1590: ZB = zb_or(n1585, n1589);
    let n1591: ZB = zb_or(n1584, n1590);
    let n1592: ZB = zb_or(n1578, n1591);
    let n1593: ZB = zb_and(n756, n1305);
    let n1594: ZB = zb_and(n766, n1308);
    let n1595: ZB = zb_and(n775, n1309);
    let n1596: ZB = zb_or(n1594, n1595);
    let n1597: ZB = zb_or(n1307, n1596);
    let n1598: ZB = zb_or(n1593, n1597);
    let n1599: ZB = zb_and(n784, n1312);
    let n1600: ZB = zb_and(n794, n1315);
    let n1601: ZB = zb_and(n799, n1316);
    let n1602: ZB = zb_or(n1600, n1601);
    let n1603: ZB = zb_or(n1314, n1602);
    let n1604: ZB = zb_or(n1599, n1603);
    let n1605: ZB = zb_and(n808, n1319);
    let n1606: ZB = zb_and(n818, n1322);
    let n1607: ZB = zb_and(n823, n1323);
    let n1608: ZB = zb_or(n1606, n1607);
    let n1609: ZB = zb_or(n1321, n1608);
    let n1610: ZB = zb_or(n1605, n1609);
    let n1611: ZB = zb_or(n1604, n1610);
    let n1612: ZB = zb_or(n1598, n1611);
    let n1613: ZB = zb_or(n1592, n1612);
    let n1614: ZB = zsel_b(n1592, n654, n743);
    let n1615: ZB = zb_or(n1572, n1613);
    let n1616: ZB = zsel_b(n1572, n516, n1614);
    let n1617: ZB = zsel_b(n1615, n1616, n841);
    let n1619: ZB = zb_and(n554, n1340);
    let n1620: ZB = zb_and(n569, n1343);
    let n1621: ZB = zb_and(n581, n1344);
    let n1622: ZB = zb_or(n1620, n1621);
    let n1623: ZB = zb_or(n1342, n1622);
    let n1624: ZB = zb_or(n1619, n1623);
    let n1625: ZB = zb_and(n597, n1347);
    let n1626: ZB = zb_and(n607, n1350);
    let n1627: ZB = zb_and(n612, n1351);
    let n1628: ZB = zb_or(n1626, n1627);
    let n1629: ZB = zb_or(n1349, n1628);
    let n1630: ZB = zb_or(n1625, n1629);
    let n1631: ZB = zb_and(n628, n1354);
    let n1632: ZB = zb_and(n638, n1357);
    let n1633: ZB = zb_and(n643, n1358);
    let n1634: ZB = zb_or(n1632, n1633);
    let n1635: ZB = zb_or(n1356, n1634);
    let n1636: ZB = zb_or(n1631, n1635);
    let n1637: ZB = zb_or(n1630, n1636);
    let n1638: ZB = zb_or(n1624, n1637);
    let n1639: ZB = zb_and(n667, n1366);
    let n1640: ZB = zb_and(n677, n1369);
    let n1641: ZB = zb_and(n686, n1370);
    let n1642: ZB = zb_or(n1640, n1641);
    let n1643: ZB = zb_or(n1368, n1642);
    let n1644: ZB = zb_or(n1639, n1643);
    let n1645: ZB = zb_and(n695, n1373);
    let n1646: ZB = zb_and(n705, n1376);
    let n1647: ZB = zb_and(n710, n1377);
    let n1648: ZB = zb_or(n1646, n1647);
    let n1649: ZB = zb_or(n1375, n1648);
    let n1650: ZB = zb_or(n1645, n1649);
    let n1651: ZB = zb_and(n719, n1380);
    let n1652: ZB = zb_and(n729, n1383);
    let n1653: ZB = zb_and(n734, n1384);
    let n1654: ZB = zb_or(n1652, n1653);
    let n1655: ZB = zb_or(n1382, n1654);
    let n1656: ZB = zb_or(n1651, n1655);
    let n1657: ZB = zb_or(n1650, n1656);
    let n1658: ZB = zb_or(n1644, n1657);
    let n1659: ZB = zb_and(n756, n1392);
    let n1660: ZB = zb_and(n766, n1395);
    let n1661: ZB = zb_and(n775, n1396);
    let n1662: ZB = zb_or(n1660, n1661);
    let n1663: ZB = zb_or(n1394, n1662);
    let n1664: ZB = zb_or(n1659, n1663);
    let n1665: ZB = zb_and(n784, n1399);
    let n1666: ZB = zb_and(n794, n1402);
    let n1667: ZB = zb_and(n799, n1403);
    let n1668: ZB = zb_or(n1666, n1667);
    let n1669: ZB = zb_or(n1401, n1668);
    let n1670: ZB = zb_or(n1665, n1669);
    let n1671: ZB = zb_and(n808, n1406);
    let n1672: ZB = zb_and(n818, n1409);
    let n1673: ZB = zb_and(n823, n1410);
    let n1674: ZB = zb_or(n1672, n1673);
    let n1675: ZB = zb_or(n1408, n1674);
    let n1676: ZB = zb_or(n1671, n1675);
    let n1677: ZB = zb_or(n1670, n1676);
    let n1678: ZB = zb_or(n1664, n1677);
    let n1679: ZB = zb_or(n1658, n1678);
    let n1680: ZB = zsel_b(n1658, n654, n743);
    let n1681: ZB = zb_or(n1638, n1679);
    let n1682: ZB = zsel_b(n1638, n516, n1680);
    let n1683: ZB = zsel_b(n1681, n1682, n841);
    let n1685: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c406);
    let n1686: ZB = zb_not(n1685);
    let n1687: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c407);
    let n1688: ZB = zb_not(n1687);
    let n1689: ZB = zb_or(n1686, n1688);
    let n1690: ZB = zb_not(n1689);
    let n1691: ZN = zn_add(r_c404, r_c406);
    let n1692: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1691);
    let n1693: ZN = zn_flr(n1692);
    let n1694: ZN = zn_sub(n1692, zn_splat(P8::from_raw(32768i32)));
    let n1695: ZN = zn_sub(n1694, n1693);
    let n1696: ZB = zn_gt(n1693, zn_splat(P8::from_raw(0i32)));
    let n1697: ZB = zn_lt(n1693, zn_splat(P8::from_raw(0i32)));
    let n1698: ZN = zsel_n(n1697, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1699: ZN = zsel_n(n1696, zn_splat(P8::from_raw(65536i32)), n1698);
    let n1700: ZN = zn_abs(n1693);
    let n1701: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n246);
    let n1702: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1700);
    let n1703: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1700);
    let n1704: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1700);
    let n1705: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1700);
    let n1706: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1700);
    let n1707: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1700);
    let n1708: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1700);
    let n1709: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1700);
    let n1710: ZN = zn_add(r_c405, r_c407);
    let n1711: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1710);
    let n1712: ZN = zn_flr(n1711);
    let n1713: ZN = zn_sub(n1711, zn_splat(P8::from_raw(32768i32)));
    let n1714: ZN = zn_sub(n1713, n1712);
    let n1715: ZB = zn_gt(n1712, zn_splat(P8::from_raw(0i32)));
    let n1716: ZB = zn_lt(n1712, zn_splat(P8::from_raw(0i32)));
    let n1717: ZN = zsel_n(n1716, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1718: ZN = zsel_n(n1715, zn_splat(P8::from_raw(65536i32)), n1717);
    let n1719: ZN = zn_abs(n1712);
    let n1720: ZN = zn_add(n246, n1718);
    let n1721: ZN = zn_add(r_c317, n1718);
    let n1722: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1719);
    let n1723: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1721);
    let n1724: ZN = zn_add(n1718, n1723);
    let n1725: ZN = zn_add(n1718, n1721);
    let n1726: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1719);
    let n1727: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1725);
    let n1728: ZN = zn_add(n1718, n1727);
    let n1729: ZN = zn_add(n1718, n1725);
    let n1730: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1719);
    let n1731: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1729);
    let n1732: ZN = zn_add(n1718, n1731);
    let n1733: ZN = zn_add(n1718, n1729);
    let n1734: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1719);
    let n1735: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1733);
    let n1736: ZN = zn_add(n1718, n1735);
    let n1737: ZN = zn_add(n1718, n1733);
    let n1738: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1719);
    let n1739: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1737);
    let n1740: ZN = zn_add(n1718, n1739);
    let n1741: ZN = zn_add(n1718, n1737);
    let n1742: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1719);
    let n1743: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1741);
    let n1744: ZN = zn_add(n1718, n1743);
    let n1745: ZN = zn_add(n1718, n1741);
    let n1746: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1719);
    let n1747: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1745);
    let n1748: ZN = zn_add(n1718, n1747);
    let n1749: ZN = zn_add(n1718, n1745);
    let n1750: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1719);
    let n1751: ZB = zb_not(n1136);
    let n1752: ZB = zb_and(n1133, n1751);
    let n1753: ZB = zb_and(n1131, n1137);
    let n1754: ZB = zb_or(n1752, n1753);
    let n1755: ZN = zn_add(n264, n1699);
    let n1756: ZB = zn_tile_flag_at(g.cache, g.cart, n1755, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1757: ZN = zn_add(r_c316, n1699);
    let n1758: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1757);
    let n1759: ZN = zn_add(n1699, n1758);
    let n1760: ZB = zn_tile_flag_at(g.cache, g.cart, n1759, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1761: ZN = zn_add(n1699, n1757);
    let n1762: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1761);
    let n1763: ZN = zn_add(n1699, n1762);
    let n1764: ZB = zn_tile_flag_at(g.cache, g.cart, n1763, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1765: ZN = zn_add(n1699, n1761);
    let n1766: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1765);
    let n1767: ZN = zn_add(n1699, n1766);
    let n1768: ZB = zn_tile_flag_at(g.cache, g.cart, n1767, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1769: ZN = zn_add(n1699, n1765);
    let n1770: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1769);
    let n1771: ZN = zn_add(n1699, n1770);
    let n1772: ZB = zn_tile_flag_at(g.cache, g.cart, n1771, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1773: ZN = zn_add(n1699, n1769);
    let n1774: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1773);
    let n1775: ZN = zn_add(n1699, n1774);
    let n1776: ZB = zn_tile_flag_at(g.cache, g.cart, n1775, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1777: ZN = zn_add(n1699, n1773);
    let n1778: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1777);
    let n1779: ZN = zn_add(n1699, n1778);
    let n1780: ZB = zn_tile_flag_at(g.cache, g.cart, n1779, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1781: ZN = zn_add(n1699, n1777);
    let n1782: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1781);
    let n1783: ZN = zn_add(n1699, n1782);
    let n1784: ZB = zn_tile_flag_at(g.cache, g.cart, n1783, n1701, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1785: ZN = zn_add(n1699, n1781);
    let n1786: ZN = zsel_n(n1784, n1781, n1785);
    let n1787: ZN = zsel_n(n1784, zn_splat(P8::from_raw(0i32)), n1695);
    let n1788: ZN = zsel_n(n1784, zn_splat(P8::from_raw(0i32)), r_c406);
    let n1789: ZB = zb_or(n1709, n1784);
    let n1790: ZN = zsel_n(n1708, n1781, n1786);
    let n1791: ZN = zsel_n(n1708, n1695, n1787);
    let n1792: ZN = zsel_n(n1708, r_c406, n1788);
    let n1793: ZB = zb_or(n1708, n1789);
    let n1794: ZN = zsel_n(n1780, n1777, n1790);
    let n1795: ZN = zsel_n(n1780, zn_splat(P8::from_raw(0i32)), n1791);
    let n1796: ZN = zsel_n(n1780, zn_splat(P8::from_raw(0i32)), n1792);
    let n1797: ZB = zb_or(n1780, n1793);
    let n1798: ZN = zsel_n(n1707, n1777, n1794);
    let n1799: ZN = zsel_n(n1707, n1695, n1795);
    let n1800: ZN = zsel_n(n1707, r_c406, n1796);
    let n1801: ZB = zb_or(n1707, n1797);
    let n1802: ZN = zsel_n(n1776, n1773, n1798);
    let n1803: ZN = zsel_n(n1776, zn_splat(P8::from_raw(0i32)), n1799);
    let n1804: ZN = zsel_n(n1776, zn_splat(P8::from_raw(0i32)), n1800);
    let n1805: ZB = zb_or(n1776, n1801);
    let n1806: ZN = zsel_n(n1706, n1773, n1802);
    let n1807: ZN = zsel_n(n1706, n1695, n1803);
    let n1808: ZN = zsel_n(n1706, r_c406, n1804);
    let n1809: ZB = zb_or(n1706, n1805);
    let n1810: ZN = zsel_n(n1772, n1769, n1806);
    let n1811: ZN = zsel_n(n1772, zn_splat(P8::from_raw(0i32)), n1807);
    let n1812: ZN = zsel_n(n1772, zn_splat(P8::from_raw(0i32)), n1808);
    let n1813: ZB = zb_or(n1772, n1809);
    let n1814: ZN = zsel_n(n1705, n1769, n1810);
    let n1815: ZN = zsel_n(n1705, n1695, n1811);
    let n1816: ZN = zsel_n(n1705, r_c406, n1812);
    let n1817: ZB = zb_or(n1705, n1813);
    let n1818: ZN = zsel_n(n1768, n1765, n1814);
    let n1819: ZN = zsel_n(n1768, zn_splat(P8::from_raw(0i32)), n1815);
    let n1820: ZN = zsel_n(n1768, zn_splat(P8::from_raw(0i32)), n1816);
    let n1821: ZB = zb_or(n1768, n1817);
    let n1822: ZN = zsel_n(n1704, n1765, n1818);
    let n1823: ZN = zsel_n(n1704, n1695, n1819);
    let n1824: ZN = zsel_n(n1704, r_c406, n1820);
    let n1825: ZB = zb_or(n1704, n1821);
    let n1826: ZN = zsel_n(n1764, n1761, n1822);
    let n1827: ZN = zsel_n(n1764, zn_splat(P8::from_raw(0i32)), n1823);
    let n1828: ZN = zsel_n(n1764, zn_splat(P8::from_raw(0i32)), n1824);
    let n1829: ZB = zb_or(n1764, n1825);
    let n1830: ZN = zsel_n(n1703, n1761, n1826);
    let n1831: ZN = zsel_n(n1703, n1695, n1827);
    let n1832: ZN = zsel_n(n1703, r_c406, n1828);
    let n1833: ZB = zb_or(n1703, n1829);
    let n1834: ZN = zsel_n(n1760, n1757, n1830);
    let n1835: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1831);
    let n1836: ZN = zsel_n(n1760, zn_splat(P8::from_raw(0i32)), n1832);
    let n1837: ZB = zb_or(n1760, n1833);
    let n1838: ZN = zsel_n(n1702, n1757, n1834);
    let n1839: ZN = zsel_n(n1702, n1695, n1835);
    let n1840: ZN = zsel_n(n1702, r_c406, n1836);
    let n1841: ZB = zb_or(n1702, n1837);
    let n1842: ZN = zsel_n(n1756, r_c316, n1838);
    let n1843: ZN = zsel_n(n1756, zn_splat(P8::from_raw(0i32)), n1839);
    let n1844: ZN = zsel_n(n1756, zn_splat(P8::from_raw(0i32)), n1840);
    let n1845: ZB = zb_or(n1756, n1841);
    let n1846: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1842);
    let n1847: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1846);
    let n1848: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1720, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1849: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1724, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1850: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1728, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1851: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1732, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1852: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1736, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1853: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1740, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1854: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1744, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1855: ZB = zn_tile_flag_at(g.cache, g.cart, n1847, n1748, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1856: ZB = zb_and(n1750, n1845);
    let n1857: ZN = zsel_n(n1855, n1745, n1749);
    let n1858: ZN = zsel_n(n1855, zn_splat(P8::from_raw(0i32)), n1714);
    let n1859: ZN = zsel_n(n1855, zn_splat(P8::from_raw(0i32)), r_c407);
    let n1860: ZB = zsel_b(n1855, n1845, n1856);
    let n1861: ZN = zsel_n(n1746, n1745, n1857);
    let n1862: ZN = zsel_n(n1746, n1714, n1858);
    let n1863: ZN = zsel_n(n1746, r_c407, n1859);
    let n1864: ZB = zsel_b(n1746, n1845, n1860);
    let n1865: ZN = zsel_n(n1854, n1741, n1861);
    let n1866: ZN = zsel_n(n1854, zn_splat(P8::from_raw(0i32)), n1862);
    let n1867: ZN = zsel_n(n1854, zn_splat(P8::from_raw(0i32)), n1863);
    let n1868: ZB = zsel_b(n1854, n1845, n1864);
    let n1869: ZN = zsel_n(n1742, n1741, n1865);
    let n1870: ZN = zsel_n(n1742, n1714, n1866);
    let n1871: ZN = zsel_n(n1742, r_c407, n1867);
    let n1872: ZB = zsel_b(n1742, n1845, n1868);
    let n1873: ZN = zsel_n(n1853, n1737, n1869);
    let n1874: ZN = zsel_n(n1853, zn_splat(P8::from_raw(0i32)), n1870);
    let n1875: ZN = zsel_n(n1853, zn_splat(P8::from_raw(0i32)), n1871);
    let n1876: ZB = zsel_b(n1853, n1845, n1872);
    let n1877: ZN = zsel_n(n1738, n1737, n1873);
    let n1878: ZN = zsel_n(n1738, n1714, n1874);
    let n1879: ZN = zsel_n(n1738, r_c407, n1875);
    let n1880: ZB = zsel_b(n1738, n1845, n1876);
    let n1881: ZN = zsel_n(n1852, n1733, n1877);
    let n1882: ZN = zsel_n(n1852, zn_splat(P8::from_raw(0i32)), n1878);
    let n1883: ZN = zsel_n(n1852, zn_splat(P8::from_raw(0i32)), n1879);
    let n1884: ZB = zsel_b(n1852, n1845, n1880);
    let n1885: ZN = zsel_n(n1734, n1733, n1881);
    let n1886: ZN = zsel_n(n1734, n1714, n1882);
    let n1887: ZN = zsel_n(n1734, r_c407, n1883);
    let n1888: ZB = zsel_b(n1734, n1845, n1884);
    let n1889: ZN = zsel_n(n1851, n1729, n1885);
    let n1890: ZN = zsel_n(n1851, zn_splat(P8::from_raw(0i32)), n1886);
    let n1891: ZN = zsel_n(n1851, zn_splat(P8::from_raw(0i32)), n1887);
    let n1892: ZB = zsel_b(n1851, n1845, n1888);
    let n1893: ZN = zsel_n(n1730, n1729, n1889);
    let n1894: ZN = zsel_n(n1730, n1714, n1890);
    let n1895: ZN = zsel_n(n1730, r_c407, n1891);
    let n1896: ZB = zsel_b(n1730, n1845, n1892);
    let n1897: ZN = zsel_n(n1850, n1725, n1893);
    let n1898: ZN = zsel_n(n1850, zn_splat(P8::from_raw(0i32)), n1894);
    let n1899: ZN = zsel_n(n1850, zn_splat(P8::from_raw(0i32)), n1895);
    let n1900: ZB = zsel_b(n1850, n1845, n1896);
    let n1901: ZN = zsel_n(n1726, n1725, n1897);
    let n1902: ZN = zsel_n(n1726, n1714, n1898);
    let n1903: ZN = zsel_n(n1726, r_c407, n1899);
    let n1904: ZB = zsel_b(n1726, n1845, n1900);
    let n1905: ZN = zsel_n(n1849, n1721, n1901);
    let n1906: ZN = zsel_n(n1849, zn_splat(P8::from_raw(0i32)), n1902);
    let n1907: ZN = zsel_n(n1849, zn_splat(P8::from_raw(0i32)), n1903);
    let n1908: ZB = zsel_b(n1849, n1845, n1904);
    let n1909: ZN = zsel_n(n1722, n1721, n1905);
    let n1910: ZN = zsel_n(n1722, n1714, n1906);
    let n1911: ZN = zsel_n(n1722, r_c407, n1907);
    let n1912: ZB = zsel_b(n1722, n1845, n1908);
    let n1913: ZN = zsel_n(n1848, r_c317, n1909);
    let n1914: ZN = zsel_n(n1848, zn_splat(P8::from_raw(0i32)), n1910);
    let n1915: ZN = zsel_n(n1848, zn_splat(P8::from_raw(0i32)), n1911);
    let n1916: ZB = zsel_b(n1848, n1845, n1912);
    let n1917: ZN = zsel_n(n1689, n1842, r_c316);
    let n1918: ZN = zsel_n(n1689, n1913, r_c317);
    let n1919: ZN = zsel_n(n1689, n1843, r_c404);
    let n1920: ZN = zsel_n(n1689, n1914, r_c405);
    let n1921: ZN = zsel_n(n1689, n1844, r_c406);
    let n1922: ZN = zsel_n(n1689, n1915, r_c407);
    let n1923: ZB = zb_or(n1690, n1916);
    let n1924: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1917);
    let n1925: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1918);
    let n1926: ZN = zn_div(n1924, zn_splat(P8::from_raw(524288i32)));
    let n1927: ZN = zn_flr(n1926);
    let n1928: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1927);
    let n1929: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1924);
    let n1930: ZN = zn_sub(n1929, zn_splat(P8::from_raw(65536i32)));
    let n1931: ZN = zn_div(n1930, zn_splat(P8::from_raw(524288i32)));
    let n1932: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1931);
    let n1933: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1928);
    let n1934: ZB = zn_le(n1933, n1932);
    let n1935: ZB = zn_gt(n1933, n1932);
    let n1936: ZB = zb_and(n1754, n1934);
    let n1937: ZB = zb_and(n1754, n1935);
    let n1938: ZN = zn_div(n1925, zn_splat(P8::from_raw(524288i32)));
    let n1939: ZN = zn_flr(n1938);
    let n1940: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1939);
    let n1941: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1925);
    let n1942: ZN = zn_sub(n1941, zn_splat(P8::from_raw(65536i32)));
    let n1943: ZN = zn_div(n1942, zn_splat(P8::from_raw(524288i32)));
    let n1944: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1943);
    let n1945: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1940);
    let n1946: ZB = zn_le(n1945, n1944);
    let n1947: ZB = zn_gt(n1945, n1944);
    let n1948: ZB = zb_and(n1936, n1946);
    let n1949: ZB = zb_and(n1936, n1947);
    let n1950: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1933);
    let n1951: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1945);
    let n1952: ZN = zn_mget(g.cart, n1950, n1951);
    let n1953: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1952);
    let n1954: ZN = zn_rem(n1942, zn_splat(P8::from_raw(524288i32)));
    let n1955: ZB = zn_ge(n1954, zn_splat(P8::from_raw(393216i32)));
    let n1956: ZN = zn_mul(n1945, zn_splat(P8::from_raw(524288i32)));
    let n1957: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1956);
    let n1958: ZB = zn_eq(n1941, n1957);
    let n1959: ZB = zb_or(n1955, n1958);
    let n1960: ZB = zb_and(n1953, n1959);
    let n1961: ZB = zn_ge(n1922, zn_splat(P8::from_raw(0i32)));
    let n1962: ZB = zb_and(n1960, n1961);
    let n1963: ZB = zb_not(n1962);
    let n1964: ZB = zb_and(n1948, n1963);
    let n1965: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1952);
    let n1966: ZN = zn_rem(n1925, zn_splat(P8::from_raw(524288i32)));
    let n1967: ZB = zn_le(n1966, zn_splat(P8::from_raw(131072i32)));
    let n1968: ZB = zb_and(n1965, n1967);
    let n1969: ZB = zn_le(n1922, zn_splat(P8::from_raw(0i32)));
    let n1970: ZB = zb_and(n1968, n1969);
    let n1971: ZB = zb_not(n1970);
    let n1972: ZB = zb_and(n1964, n1971);
    let n1973: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1952);
    let n1974: ZN = zn_rem(n1924, zn_splat(P8::from_raw(524288i32)));
    let n1975: ZB = zn_le(n1974, zn_splat(P8::from_raw(131072i32)));
    let n1976: ZB = zb_and(n1973, n1975);
    let n1977: ZB = zn_le(n1921, zn_splat(P8::from_raw(0i32)));
    let n1978: ZB = zb_and(n1976, n1977);
    let n1979: ZB = zb_not(n1978);
    let n1980: ZB = zb_and(n1972, n1979);
    let n1981: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1952);
    let n1982: ZN = zn_rem(n1930, zn_splat(P8::from_raw(524288i32)));
    let n1983: ZB = zn_ge(n1982, zn_splat(P8::from_raw(393216i32)));
    let n1984: ZN = zn_mul(n1933, zn_splat(P8::from_raw(524288i32)));
    let n1985: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1984);
    let n1986: ZB = zn_eq(n1929, n1985);
    let n1987: ZB = zb_or(n1983, n1986);
    let n1988: ZB = zb_and(n1981, n1987);
    let n1989: ZB = zn_ge(n1921, zn_splat(P8::from_raw(0i32)));
    let n1990: ZB = zb_and(n1988, n1989);
    let n1991: ZB = zb_not(n1990);
    let n1992: ZB = zb_and(n1980, n1991);
    let n1993: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1940);
    let n1994: ZB = zn_le(n1993, n1944);
    let n1995: ZB = zn_gt(n1993, n1944);
    let n1996: ZB = zb_and(n1992, n1994);
    let n1997: ZB = zb_and(n1992, n1995);
    let n1998: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1993);
    let n1999: ZN = zn_mget(g.cart, n1950, n1998);
    let n2000: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1999);
    let n2001: ZN = zn_mul(n1993, zn_splat(P8::from_raw(524288i32)));
    let n2002: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n2001);
    let n2003: ZB = zn_eq(n1941, n2002);
    let n2004: ZB = zb_or(n1955, n2003);
    let n2005: ZB = zb_and(n2000, n2004);
    let n2006: ZB = zb_and(n1961, n2005);
    let n2007: ZB = zb_not(n2006);
    let n2008: ZB = zb_and(n1996, n2007);
    let n2009: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1999);
    let n2010: ZB = zb_and(n1967, n2009);
    let n2011: ZB = zb_and(n1969, n2010);
    let n2012: ZB = zb_not(n2011);
    let n2013: ZB = zb_and(n2008, n2012);
    let n2014: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1999);
    let n2015: ZB = zb_and(n1975, n2014);
    let n2016: ZB = zb_and(n1977, n2015);
    let n2017: ZB = zb_not(n2016);
    let n2018: ZB = zb_and(n2013, n2017);
    let n2019: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1999);
    let n2020: ZB = zb_and(n1987, n2019);
    let n2021: ZB = zb_and(n1989, n2020);
    let n2022: ZB = zb_not(n2021);
    let n2023: ZB = zb_and(n2018, n2022);
    let n2024: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1940);
    let n2025: ZB = zn_le(n2024, n1944);
    let n2026: ZB = zn_gt(n2024, n1944);
    let n2027: ZB = zb_and(n2023, n2025);
    let n2028: ZB = zb_and(n2023, n2026);
    let n2029: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n2024);
    let n2030: ZN = zn_mget(g.cart, n1950, n2029);
    let n2031: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2030);
    let n2032: ZN = zn_mul(n2024, zn_splat(P8::from_raw(524288i32)));
    let n2033: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n2032);
    let n2034: ZB = zn_eq(n1941, n2033);
    let n2035: ZB = zb_or(n1955, n2034);
    let n2036: ZB = zb_and(n2031, n2035);
    let n2037: ZB = zb_and(n1961, n2036);
    let n2038: ZB = zb_not(n2037);
    let n2039: ZB = zb_and(n2027, n2038);
    let n2040: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2030);
    let n2041: ZB = zb_and(n1967, n2040);
    let n2042: ZB = zb_and(n1969, n2041);
    let n2043: ZB = zb_not(n2042);
    let n2044: ZB = zb_and(n2039, n2043);
    let n2045: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2030);
    let n2046: ZB = zb_and(n1975, n2045);
    let n2047: ZB = zb_and(n1977, n2046);
    let n2048: ZB = zb_not(n2047);
    let n2049: ZB = zb_and(n2044, n2048);
    let n2050: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2030);
    let n2051: ZB = zb_and(n1987, n2050);
    let n2052: ZB = zb_and(n1989, n2051);
    let n2053: ZB = zb_not(n2052);
    let n2054: ZB = zb_and(n2049, n2053);
    let n2055: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1940);
    let n2056: ZB = zn_gt(n2055, n1944);
    let n2057: ZB = zb_and(n1923, n2056);
    let n2058: ZB = zb_or(n2028, n2054);
    let n2059: ZB = zsel_b(n2026, n1923, n2057);
    let n2060: ZB = zb_or(n1997, n2058);
    let n2061: ZB = zsel_b(n1995, n1923, n2059);
    let n2062: ZB = zb_or(n1949, n2060);
    let n2063: ZB = zsel_b(n1947, n1923, n2061);
    let n2064: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1928);
    let n2065: ZB = zn_le(n2064, n1932);
    let n2066: ZB = zn_gt(n2064, n1932);
    let n2067: ZB = zb_and(n2062, n2065);
    let n2068: ZB = zb_and(n2062, n2066);
    let n2069: ZB = zb_and(n1947, n2067);
    let n2070: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n2064);
    let n2071: ZN = zn_mget(g.cart, n2070, n1951);
    let n2072: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2071);
    let n2073: ZB = zb_and(n1946, n2062);
    let n2074: ZB = zb_and(n2065, n2073);
    let n2075: ZB = zb_and(n1959, n2072);
    let n2076: ZB = zb_and(n1961, n2075);
    let n2077: ZB = zb_not(n2076);
    let n2078: ZB = zb_and(n2074, n2077);
    let n2079: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2071);
    let n2080: ZB = zb_and(n1967, n2079);
    let n2081: ZB = zb_and(n1969, n2080);
    let n2082: ZB = zb_not(n2081);
    let n2083: ZB = zb_and(n2078, n2082);
    let n2084: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2071);
    let n2085: ZB = zb_and(n1975, n2084);
    let n2086: ZB = zb_and(n1977, n2085);
    let n2087: ZB = zb_not(n2086);
    let n2088: ZB = zb_and(n2083, n2087);
    let n2089: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2071);
    let n2090: ZN = zn_mul(n2064, zn_splat(P8::from_raw(524288i32)));
    let n2091: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n2090);
    let n2092: ZB = zn_eq(n1929, n2091);
    let n2093: ZB = zb_or(n1983, n2092);
    let n2094: ZB = zb_and(n2089, n2093);
    let n2095: ZB = zb_and(n1989, n2094);
    let n2096: ZB = zb_not(n2095);
    let n2097: ZB = zb_and(n2088, n2096);
    let n2098: ZB = zb_and(n1995, n2097);
    let n2099: ZN = zn_mget(g.cart, n2070, n1998);
    let n2100: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2099);
    let n2101: ZB = zb_and(n1994, n2088);
    let n2102: ZB = zb_and(n2096, n2101);
    let n2103: ZB = zb_and(n2004, n2100);
    let n2104: ZB = zb_and(n1961, n2103);
    let n2105: ZB = zb_not(n2104);
    let n2106: ZB = zb_and(n2102, n2105);
    let n2107: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2099);
    let n2108: ZB = zb_and(n1967, n2107);
    let n2109: ZB = zb_and(n1969, n2108);
    let n2110: ZB = zb_not(n2109);
    let n2111: ZB = zb_and(n2106, n2110);
    let n2112: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2099);
    let n2113: ZB = zb_and(n1975, n2112);
    let n2114: ZB = zb_and(n1977, n2113);
    let n2115: ZB = zb_not(n2114);
    let n2116: ZB = zb_and(n2111, n2115);
    let n2117: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2099);
    let n2118: ZB = zb_and(n2093, n2117);
    let n2119: ZB = zb_and(n1989, n2118);
    let n2120: ZB = zb_not(n2119);
    let n2121: ZB = zb_and(n2116, n2120);
    let n2122: ZB = zb_and(n2026, n2121);
    let n2123: ZN = zn_mget(g.cart, n2070, n2029);
    let n2124: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2123);
    let n2125: ZB = zb_and(n2025, n2116);
    let n2126: ZB = zb_and(n2120, n2125);
    let n2127: ZB = zb_and(n2035, n2124);
    let n2128: ZB = zb_and(n1961, n2127);
    let n2129: ZB = zb_not(n2128);
    let n2130: ZB = zb_and(n2126, n2129);
    let n2131: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2123);
    let n2132: ZB = zb_and(n1967, n2131);
    let n2133: ZB = zb_and(n1969, n2132);
    let n2134: ZB = zb_not(n2133);
    let n2135: ZB = zb_and(n2130, n2134);
    let n2136: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2123);
    let n2137: ZB = zb_and(n1975, n2136);
    let n2138: ZB = zb_and(n1977, n2137);
    let n2139: ZB = zb_not(n2138);
    let n2140: ZB = zb_and(n2135, n2139);
    let n2141: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2123);
    let n2142: ZB = zb_and(n2093, n2141);
    let n2143: ZB = zb_and(n1989, n2142);
    let n2144: ZB = zb_not(n2143);
    let n2145: ZB = zb_and(n2140, n2144);
    let n2146: ZB = zb_and(n2056, n2063);
    let n2147: ZB = zb_or(n2122, n2145);
    let n2148: ZB = zsel_b(n2026, n2063, n2146);
    let n2149: ZB = zb_or(n2098, n2147);
    let n2150: ZB = zsel_b(n1995, n2063, n2148);
    let n2151: ZB = zb_or(n2069, n2149);
    let n2152: ZB = zsel_b(n1947, n2063, n2150);
    let n2153: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1928);
    let n2154: ZB = zn_le(n2153, n1932);
    let n2155: ZB = zn_gt(n2153, n1932);
    let n2156: ZB = zb_and(n2151, n2154);
    let n2157: ZB = zb_and(n2151, n2155);
    let n2158: ZB = zb_and(n1947, n2156);
    let n2159: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n2153);
    let n2160: ZN = zn_mget(g.cart, n2159, n1951);
    let n2161: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2160);
    let n2162: ZB = zb_and(n1946, n2151);
    let n2163: ZB = zb_and(n2154, n2162);
    let n2164: ZB = zb_and(n1959, n2161);
    let n2165: ZB = zb_and(n1961, n2164);
    let n2166: ZB = zb_not(n2165);
    let n2167: ZB = zb_and(n2163, n2166);
    let n2168: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2160);
    let n2169: ZB = zb_and(n1967, n2168);
    let n2170: ZB = zb_and(n1969, n2169);
    let n2171: ZB = zb_not(n2170);
    let n2172: ZB = zb_and(n2167, n2171);
    let n2173: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2160);
    let n2174: ZB = zb_and(n1975, n2173);
    let n2175: ZB = zb_and(n1977, n2174);
    let n2176: ZB = zb_not(n2175);
    let n2177: ZB = zb_and(n2172, n2176);
    let n2178: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2160);
    let n2179: ZN = zn_mul(n2153, zn_splat(P8::from_raw(524288i32)));
    let n2180: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n2179);
    let n2181: ZB = zn_eq(n1929, n2180);
    let n2182: ZB = zb_or(n1983, n2181);
    let n2183: ZB = zb_and(n2178, n2182);
    let n2184: ZB = zb_and(n1989, n2183);
    let n2185: ZB = zb_not(n2184);
    let n2186: ZB = zb_and(n2177, n2185);
    let n2187: ZB = zb_and(n1995, n2186);
    let n2188: ZN = zn_mget(g.cart, n2159, n1998);
    let n2189: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2188);
    let n2190: ZB = zb_and(n1994, n2177);
    let n2191: ZB = zb_and(n2185, n2190);
    let n2192: ZB = zb_and(n2004, n2189);
    let n2193: ZB = zb_and(n1961, n2192);
    let n2194: ZB = zb_not(n2193);
    let n2195: ZB = zb_and(n2191, n2194);
    let n2196: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2188);
    let n2197: ZB = zb_and(n1967, n2196);
    let n2198: ZB = zb_and(n1969, n2197);
    let n2199: ZB = zb_not(n2198);
    let n2200: ZB = zb_and(n2195, n2199);
    let n2201: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2188);
    let n2202: ZB = zb_and(n1975, n2201);
    let n2203: ZB = zb_and(n1977, n2202);
    let n2204: ZB = zb_not(n2203);
    let n2205: ZB = zb_and(n2200, n2204);
    let n2206: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2188);
    let n2207: ZB = zb_and(n2182, n2206);
    let n2208: ZB = zb_and(n1989, n2207);
    let n2209: ZB = zb_not(n2208);
    let n2210: ZB = zb_and(n2205, n2209);
    let n2211: ZB = zb_and(n2026, n2210);
    let n2212: ZN = zn_mget(g.cart, n2159, n2029);
    let n2213: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n2212);
    let n2214: ZB = zb_and(n2025, n2205);
    let n2215: ZB = zb_and(n2209, n2214);
    let n2216: ZB = zb_and(n2035, n2213);
    let n2217: ZB = zb_and(n1961, n2216);
    let n2218: ZB = zb_not(n2217);
    let n2219: ZB = zb_and(n2215, n2218);
    let n2220: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n2212);
    let n2221: ZB = zb_and(n1967, n2220);
    let n2222: ZB = zb_and(n1969, n2221);
    let n2223: ZB = zb_not(n2222);
    let n2224: ZB = zb_and(n2219, n2223);
    let n2225: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n2212);
    let n2226: ZB = zb_and(n1975, n2225);
    let n2227: ZB = zb_and(n1977, n2226);
    let n2228: ZB = zb_not(n2227);
    let n2229: ZB = zb_and(n2224, n2228);
    let n2230: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n2212);
    let n2231: ZB = zb_and(n2182, n2230);
    let n2232: ZB = zb_and(n1989, n2231);
    let n2233: ZB = zb_not(n2232);
    let n2234: ZB = zb_and(n2229, n2233);
    let n2235: ZB = zb_and(n2056, n2152);
    let n2236: ZB = zb_or(n2211, n2234);
    let n2237: ZB = zsel_b(n2026, n2152, n2235);
    let n2238: ZB = zb_or(n2187, n2236);
    let n2239: ZB = zsel_b(n1995, n2152, n2237);
    let n2240: ZB = zb_or(n2158, n2238);
    let n2241: ZB = zsel_b(n1947, n2152, n2239);
    let n2242: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1928);
    let n2243: ZB = zn_gt(n2242, n1932);
    let n2244: ZB = zb_and(n2241, n2243);
    let n2245: ZB = zb_or(n2157, n2240);
    let n2246: ZB = zsel_b(n2155, n2152, n2244);
    let n2247: ZB = zb_or(n2068, n2245);
    let n2248: ZB = zsel_b(n2066, n2063, n2246);
    let n2249: ZB = zb_or(n1937, n2247);
    let n2250: ZB = zsel_b(n1935, n1923, n2248);
    let n2251: ZB = zn_le(n1918, zn_splat(P8::from_raw(8388608i32)));
    let n2252: ZB = zb_and(n2249, n2251);
    let n2253: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1924);
    let n2254: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1925);
    let n2255: ZB = zn_tile_flag_at(g.cache, g.cart, n2253, n2254, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2256: ZB = zb_not(n2255);
    let n2257: ZN = zsel_n(n2255, zn_splat(P8::from_raw(393216i32)), n258);
    let n2258: ZB = zn_gt(n1921, r_c396);
    let n2259: ZN = zn_sub(n1921, r_c394);
    let n2260: ZN = zn_max(r_c396, n2259);
    let n2261: ZN = zn_add(r_c394, n1921);
    let n2262: ZN = zn_min(r_c396, n2261);
    let n2263: ZN = zsel_n(n2258, n2260, n2262);
    let n2264: ZB = zn_gt(n1922, r_c397);
    let n2265: ZN = zn_sub(n1922, r_c395);
    let n2266: ZN = zn_max(r_c397, n2265);
    let n2267: ZN = zn_add(r_c395, n1922);
    let n2268: ZN = zn_min(r_c397, n2267);
    let n2269: ZN = zsel_n(n2264, n2266, n2268);
    let n2270: ZN = zsel_n(n2256, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n2271: ZN = zn_abs(n1921);
    let n2272: ZB = zn_gt(n2271, zn_splat(P8::from_raw(65536i32)));
    let n2273: ZB = zn_gt(n1921, zn_splat(P8::from_raw(0i32)));
    let n2274: ZB = zn_lt(n1921, zn_splat(P8::from_raw(0i32)));
    let n2275: ZB = zn_gt(n1921, zn_splat(P8::from_raw(65536i32)));
    let n2276: ZN = zn_sub(n1921, zn_splat(P8::from_raw(9830i32)));
    let n2277: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n2276);
    let n2278: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n1921);
    let n2279: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n2278);
    let n2280: ZB = zn_gt(n1921, zn_splat(P8::from_raw(-65536i32)));
    let n2281: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2276);
    let n2282: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n2278);
    let n2283: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2276);
    let n2284: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n2278);
    let n2285: ZN = zsel_n(n2280, n2281, n2282);
    let n2286: ZN = zsel_n(n2273, n2283, n2284);
    let n2287: ZN = zsel_n(n2275, n2277, n2279);
    let n2288: ZN = zsel_n(n2274, n2285, n2286);
    let n2289: ZN = zsel_n(n2273, n2287, n2288);
    let n2290: ZN = zn_sub(n1921, n2270);
    let n2291: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n2290);
    let n2292: ZN = zn_add(n1921, n2270);
    let n2293: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n2292);
    let n2294: ZN = zsel_n(n2273, n2291, n2293);
    let n2295: ZN = zsel_n(n2272, n2289, n2294);
    let n2296: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2295);
    let n2297: ZB = zb_not(n2296);
    let n2298: ZB = zn_lt(n2295, zn_splat(P8::from_raw(0i32)));
    let n2299: ZB = zsel_b(n2297, n2298, r_c398);
    let n2300: ZN = zn_abs(n1922);
    let n2301: ZB = zn_le(n2300, zn_splat(P8::from_raw(9830i32)));
    let n2302: ZN = zsel_n(n2301, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2303: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1925);
    let n2304: ZB = zn_gt(n1922, zn_splat(P8::from_raw(131072i32)));
    let n2305: ZN = zn_sub(n1922, n2302);
    let n2306: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2305);
    let n2307: ZN = zn_add(n1922, n2302);
    let n2308: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2307);
    let n2309: ZN = zsel_n(n2304, n2306, n2308);
    let n2310: ZN = zsel_n(n2256, n2309, n1922);
    let n2311: ZB = zn_gt(n2257, zn_splat(P8::from_raw(0i32)));
    let n2312: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1924);
    let n2313: ZB = zn_tile_flag_at(g.cache, g.cart, n2312, n2303, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2314: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1924);
    let n2315: ZB = zn_tile_flag_at(g.cache, g.cart, n2314, n2303, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2316: ZN = zsel_n(n2315, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2317: ZN = zsel_n(n2313, zn_splat(P8::from_raw(-65536i32)), n2316);
    let n2318: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2317);
    let n2319: ZB = zb_not(n2318);
    let n2320: ZN = zn_neg(n2317);
    let n2321: ZN = zn_mul(n2320, zn_splat(P8::from_raw(131072i32)));
    let n2322: ZN = zsel_n(n2319, n2321, n2295);
    let n2323: ZN = zsel_n(n2319, zn_splat(P8::from_raw(-131072i32)), n2310);
    let n2324: ZN = zsel_n(n2311, zn_splat(P8::from_raw(0i32)), n2257);
    let n2325: ZN = zsel_n(n2311, n2295, n2322);
    let n2326: ZN = zsel_n(n2311, zn_splat(P8::from_raw(-131072i32)), n2323);
    let n2327: ZN = zsel_n(n2299, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2328: ZB = zn_gt(n2327, zn_splat(P8::from_raw(0i32)));
    let n2329: ZB = zn_lt(n2327, zn_splat(P8::from_raw(0i32)));
    let n2330: ZN = zsel_n(n2329, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2331: ZN = zsel_n(n2328, zn_splat(P8::from_raw(131072i32)), n2330);
    let n2332: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2327);
    let n2333: ZB = zb_not(n2332);
    let n2334: ZN = zsel_n(n2333, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2335: ZB = zsel_b(n260, r_c398, n2299);
    let n2336: ZN = zsel_n(n260, n2263, n2295);
    let n2337: ZN = zsel_n(n260, n2269, n2310);
    let n2338: ZB = zn_lt(n1918, zn_splat(P8::from_raw(-262144i32)));
    let n2339: ZB = zn_ge(n1918, zn_splat(P8::from_raw(-262144i32)));
    let n2340: ZB = zb_and(n2252, n2338);
    let n2342: ZB = zn_lt(n1917, zn_splat(P8::from_raw(-65536i32)));
    let n2343: ZB = zn_gt(n1917, zn_splat(P8::from_raw(7929856i32)));
    let n2346: ZB = zb_or(n2342, n2343);
    let n2347: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1917);
    let n2348: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2347);
    let n2349: ZN = zsel_n(n2346, n2348, n1917);
    let n2350: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2336);
    let n2351: ZN = zsel_n(n263, n1917, n2349);
    let n2352: ZN = zsel_n(n263, n2336, n2350);
    let n2354: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2290);
    let n2355: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n2292);
    let n2356: ZN = zsel_n(n2280, n2354, n2355);
    let n2357: ZN = zsel_n(n2272, n2289, n2356);
    let n2358: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2357);
    let n2359: ZB = zb_not(n2358);
    let n2360: ZB = zn_lt(n2357, zn_splat(P8::from_raw(0i32)));
    let n2361: ZB = zsel_b(n2359, n2360, r_c398);
    let n2362: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1924);
    let n2363: ZB = zn_tile_flag_at(g.cache, g.cart, n2362, n2303, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2364: ZN = zsel_n(n2363, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n2365: ZB = zn_gt(n1922, n2364);
    let n2366: ZN = zn_max(n2305, n2364);
    let n2367: ZN = zn_min(n2307, n2364);
    let n2368: ZN = zsel_n(n2365, n2366, n2367);
    let n2369: ZN = zsel_n(n2256, n2368, n1922);
    let n2370: ZN = zsel_n(n2319, n2321, n2357);
    let n2371: ZN = zsel_n(n2319, zn_splat(P8::from_raw(-131072i32)), n2369);
    let n2372: ZN = zsel_n(n2311, n2357, n2370);
    let n2373: ZN = zsel_n(n2311, zn_splat(P8::from_raw(-131072i32)), n2371);
    let n2374: ZB = zsel_b(n260, r_c398, n2361);
    let n2375: ZN = zsel_n(n260, n2263, n2357);
    let n2376: ZN = zsel_n(n260, n2269, n2369);
    let n2377: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2375);
    let n2378: ZN = zsel_n(n263, n2375, n2377);
    let n2379: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n2290);
    let n2380: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n2292);
    let n2381: ZN = zsel_n(n2275, n2379, n2380);
    let n2382: ZN = zsel_n(n2272, n2289, n2381);
    let n2383: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2382);
    let n2384: ZB = zb_not(n2383);
    let n2385: ZB = zn_lt(n2382, zn_splat(P8::from_raw(0i32)));
    let n2386: ZB = zsel_b(n2384, n2385, r_c398);
    let n2387: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1924);
    let n2388: ZB = zn_tile_flag_at(g.cache, g.cart, n2387, n2303, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2389: ZN = zsel_n(n2388, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n2390: ZB = zn_gt(n1922, n2389);
    let n2391: ZN = zn_max(n2305, n2389);
    let n2392: ZN = zn_min(n2307, n2389);
    let n2393: ZN = zsel_n(n2390, n2391, n2392);
    let n2394: ZN = zsel_n(n2256, n2393, n1922);
    let n2395: ZN = zsel_n(n2319, n2321, n2382);
    let n2396: ZN = zsel_n(n2319, zn_splat(P8::from_raw(-131072i32)), n2394);
    let n2397: ZN = zsel_n(n2311, n2382, n2395);
    let n2398: ZN = zsel_n(n2311, zn_splat(P8::from_raw(-131072i32)), n2396);
    let n2399: ZB = zsel_b(n260, r_c398, n2386);
    let n2400: ZN = zsel_n(n260, n2263, n2382);
    let n2401: ZN = zsel_n(n260, n2269, n2394);
    let n2402: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2400);
    let n2403: ZN = zsel_n(n263, n2400, n2402);
    let n2404: ZN = zsel_n(n254, n2324, n2257);
    let n2405: ZN = zsel_n(n254, n2325, n2295);
    let n2406: ZN = zsel_n(n254, n2326, n2310);
    let n2407: ZN = zsel_n(n260, n2257, n2404);
    let n2408: ZN = zsel_n(n260, n2263, n2405);
    let n2409: ZN = zsel_n(n260, n2269, n2406);
    let n2412: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2408);
    let n2413: ZN = zsel_n(n263, n2408, n2412);
    let n2414: ZN = zsel_n(n254, n2372, n2357);
    let n2415: ZN = zsel_n(n254, n2373, n2369);
    let n2416: ZN = zsel_n(n260, n2263, n2414);
    let n2417: ZN = zsel_n(n260, n2269, n2415);
    let n2418: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2416);
    let n2419: ZN = zsel_n(n263, n2416, n2418);
    let n2420: ZN = zsel_n(n254, n2397, n2382);
    let n2421: ZN = zsel_n(n254, n2398, n2394);
    let n2422: ZN = zsel_n(n260, n2263, n2420);
    let n2423: ZN = zsel_n(n260, n2269, n2421);
    let n2424: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2422);
    let n2425: ZN = zsel_n(n263, n2422, n2424);
    let n2426: ZN = zsel_n(n255, n2334, r_c395);
    let n2427: ZN = zsel_n(n255, n2331, r_c396);
    let n2428: ZN = zsel_n(n255, n2327, n2295);
    let n2429: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2310);
    let n2430: ZN = zsel_n(n260, r_c395, n2426);
    let n2431: ZN = zsel_n(n260, r_c396, n2427);
    let n2432: ZN = zsel_n(n260, n2263, n2428);
    let n2433: ZN = zsel_n(n260, n2269, n2429);
    let n2434: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2432);
    let n2435: ZN = zsel_n(n1022, n1917, n2349);
    let n2436: ZN = zsel_n(n1022, n2432, n2434);
    let n2437: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n2357);
    let n2438: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2369);
    let n2439: ZN = zsel_n(n260, n2263, n2437);
    let n2440: ZN = zsel_n(n260, n2269, n2438);
    let n2441: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2439);
    let n2442: ZN = zsel_n(n1022, n2439, n2441);
    let n2443: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n2382);
    let n2444: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2394);
    let n2445: ZN = zsel_n(n260, n2263, n2443);
    let n2446: ZN = zsel_n(n260, n2269, n2444);
    let n2447: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2445);
    let n2448: ZN = zsel_n(n1022, n2445, n2447);
    let n2449: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2295);
    let n2450: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n2310);
    let n2451: ZN = zsel_n(n260, n2263, n2449);
    let n2452: ZN = zsel_n(n260, n2269, n2450);
    let n2453: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2451);
    let n2454: ZN = zsel_n(n1022, n2451, n2453);
    let n2455: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2357);
    let n2456: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2369);
    let n2457: ZN = zsel_n(n260, n2263, n2455);
    let n2458: ZN = zsel_n(n260, n2269, n2456);
    let n2459: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2457);
    let n2460: ZN = zsel_n(n1022, n2457, n2459);
    let n2461: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2382);
    let n2462: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2394);
    let n2463: ZN = zsel_n(n260, n2263, n2461);
    let n2464: ZN = zsel_n(n260, n2269, n2462);
    let n2465: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2463);
    let n2466: ZN = zsel_n(n1022, n2463, n2465);
    let n2467: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n2310);
    let n2468: ZN = zsel_n(n260, n2269, n2467);
    let n2469: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2369);
    let n2470: ZN = zsel_n(n260, n2269, n2469);
    let n2471: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2394);
    let n2472: ZN = zsel_n(n260, n2269, n2471);
    let n2473: ZN = zsel_n(n255, n2327, n2405);
    let n2474: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2406);
    let n2475: ZN = zsel_n(n260, n2263, n2473);
    let n2476: ZN = zsel_n(n260, n2269, n2474);
    let n2477: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2475);
    let n2478: ZN = zsel_n(n1022, n2475, n2477);
    let n2479: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n2414);
    let n2480: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2415);
    let n2481: ZN = zsel_n(n260, n2263, n2479);
    let n2482: ZN = zsel_n(n260, n2269, n2480);
    let n2483: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2481);
    let n2484: ZN = zsel_n(n1022, n2481, n2483);
    let n2485: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n2420);
    let n2486: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2421);
    let n2487: ZN = zsel_n(n260, n2263, n2485);
    let n2488: ZN = zsel_n(n260, n2269, n2486);
    let n2489: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2487);
    let n2490: ZN = zsel_n(n1022, n2487, n2489);
    let n2491: ZN = zsel_n(n255, zn_splat(P8::from_raw(0i32)), n2405);
    let n2492: ZN = zsel_n(n255, zn_splat(P8::from_raw(-327680i32)), n2406);
    let n2493: ZN = zsel_n(n260, n2263, n2491);
    let n2494: ZN = zsel_n(n260, n2269, n2492);
    let n2495: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2493);
    let n2496: ZN = zsel_n(n1022, n2493, n2495);
    let n2497: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2414);
    let n2498: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2415);
    let n2499: ZN = zsel_n(n260, n2263, n2497);
    let n2500: ZN = zsel_n(n260, n2269, n2498);
    let n2501: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2499);
    let n2502: ZN = zsel_n(n1022, n2499, n2501);
    let n2503: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2420);
    let n2504: ZN = zsel_n(n255, zn_splat(P8::from_raw(-231700i32)), n2421);
    let n2505: ZN = zsel_n(n260, n2263, n2503);
    let n2506: ZN = zsel_n(n260, n2269, n2504);
    let n2507: ZN = zsel_n(n2346, zn_splat(P8::from_raw(0i32)), n2505);
    let n2508: ZN = zsel_n(n1022, n2505, n2507);
    let n2509: ZN = zsel_n(n255, zn_splat(P8::from_raw(327680i32)), n2406);
    let n2510: ZN = zsel_n(n260, n2269, n2509);
    let n2511: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2415);
    let n2512: ZN = zsel_n(n260, n2269, n2511);
    let n2513: ZN = zsel_n(n255, zn_splat(P8::from_raw(231700i32)), n2421);
    let n2514: ZN = zsel_n(n260, n2269, n2513);
    let n2515: ZB = zb_and(n1948, n1962);
    let n2516: ZB = zb_and(n1964, n1970);
    let n2517: ZB = zb_and(n1972, n1978);
    let n2518: ZB = zb_and(n1980, n1990);
    let n2519: ZB = zb_or(n2517, n2518);
    let n2520: ZB = zb_or(n2516, n2519);
    let n2521: ZB = zb_or(n2515, n2520);
    let n2522: ZB = zb_and(n1996, n2006);
    let n2523: ZB = zb_and(n2008, n2011);
    let n2524: ZB = zb_and(n2013, n2016);
    let n2525: ZB = zb_and(n2018, n2021);
    let n2526: ZB = zb_or(n2524, n2525);
    let n2527: ZB = zb_or(n2523, n2526);
    let n2528: ZB = zb_or(n2522, n2527);
    let n2529: ZB = zb_and(n2027, n2037);
    let n2530: ZB = zb_and(n2039, n2042);
    let n2531: ZB = zb_and(n2044, n2047);
    let n2532: ZB = zb_and(n2049, n2052);
    let n2533: ZB = zb_or(n2531, n2532);
    let n2534: ZB = zb_or(n2530, n2533);
    let n2535: ZB = zb_or(n2529, n2534);
    let n2536: ZB = zb_or(n2528, n2535);
    let n2537: ZB = zb_or(n2521, n2536);
    let n2538: ZB = zb_and(n2074, n2076);
    let n2539: ZB = zb_and(n2078, n2081);
    let n2540: ZB = zb_and(n2083, n2086);
    let n2541: ZB = zb_and(n2088, n2095);
    let n2542: ZB = zb_or(n2540, n2541);
    let n2543: ZB = zb_or(n2539, n2542);
    let n2544: ZB = zb_or(n2538, n2543);
    let n2545: ZB = zb_and(n2102, n2104);
    let n2546: ZB = zb_and(n2106, n2109);
    let n2547: ZB = zb_and(n2111, n2114);
    let n2548: ZB = zb_and(n2116, n2119);
    let n2549: ZB = zb_or(n2547, n2548);
    let n2550: ZB = zb_or(n2546, n2549);
    let n2551: ZB = zb_or(n2545, n2550);
    let n2552: ZB = zb_and(n2126, n2128);
    let n2553: ZB = zb_and(n2130, n2133);
    let n2554: ZB = zb_and(n2135, n2138);
    let n2555: ZB = zb_and(n2140, n2143);
    let n2556: ZB = zb_or(n2554, n2555);
    let n2557: ZB = zb_or(n2553, n2556);
    let n2558: ZB = zb_or(n2552, n2557);
    let n2559: ZB = zb_or(n2551, n2558);
    let n2560: ZB = zb_or(n2544, n2559);
    let n2561: ZB = zb_and(n2163, n2165);
    let n2562: ZB = zb_and(n2167, n2170);
    let n2563: ZB = zb_and(n2172, n2175);
    let n2564: ZB = zb_and(n2177, n2184);
    let n2565: ZB = zb_or(n2563, n2564);
    let n2566: ZB = zb_or(n2562, n2565);
    let n2567: ZB = zb_or(n2561, n2566);
    let n2568: ZB = zb_and(n2191, n2193);
    let n2569: ZB = zb_and(n2195, n2198);
    let n2570: ZB = zb_and(n2200, n2203);
    let n2571: ZB = zb_and(n2205, n2208);
    let n2572: ZB = zb_or(n2570, n2571);
    let n2573: ZB = zb_or(n2569, n2572);
    let n2574: ZB = zb_or(n2568, n2573);
    let n2575: ZB = zb_and(n2215, n2217);
    let n2576: ZB = zb_and(n2219, n2222);
    let n2577: ZB = zb_and(n2224, n2227);
    let n2578: ZB = zb_and(n2229, n2232);
    let n2579: ZB = zb_or(n2577, n2578);
    let n2580: ZB = zb_or(n2576, n2579);
    let n2581: ZB = zb_or(n2575, n2580);
    let n2582: ZB = zb_or(n2574, n2581);
    let n2583: ZB = zb_or(n2567, n2582);
    let n2584: ZB = zb_or(n2560, n2583);
    let n2585: ZB = zsel_b(n2560, n2063, n2152);
    let n2586: ZB = zb_or(n2537, n2584);
    let n2587: ZB = zsel_b(n2537, n1923, n2585);
    let n2588: ZB = zn_gt(n1918, zn_splat(P8::from_raw(8388608i32)));
    let n2589: ZB = zb_and(n2249, n2588);
    let n2590: ZB = zb_or(n2586, n2589);
    let n2591: ZB = zsel_b(n2586, n2587, n2250);
    let n2592: ZB = zb_and(n2338, n2590);
    let n2594: ZN = zsel_n(n2588, n1485, n1484);
    let n2595: ZN = zsel_n(n2586, n2594, n1484);
    let n2598: ZB = zn_lt(r_c300, zn_splat(P8::from_raw(65536i32)));
    let n2599: ZN = zsel_n(n2598, zn_splat(P8::from_raw(65536i32)), r_c300);
    let n2600: ZB = zb_and(n1334, n1751);
    let n2601: ZB = zb_and(n1131, n1335);
    let n2602: ZB = zb_or(n2600, n2601);
    let n2603: ZB = zb_and(n1934, n2602);
    let n2604: ZB = zb_and(n1935, n2602);
    let n2605: ZB = zb_and(n1947, n2603);
    let n2606: ZB = zb_and(n1934, n1946);
    let n2607: ZB = zb_and(n2602, n2606);
    let n2608: ZB = zb_and(n1962, n2607);
    let n2609: ZB = zb_and(n1963, n2607);
    let n2610: ZB = zb_and(n1970, n2609);
    let n2611: ZB = zb_and(n1971, n2609);
    let n2612: ZB = zb_and(n1978, n2611);
    let n2613: ZB = zb_and(n1979, n2611);
    let n2614: ZB = zb_and(n1990, n2613);
    let n2615: ZB = zb_and(n1991, n2613);
    let n2616: ZB = zb_or(n2612, n2614);
    let n2617: ZB = zb_or(n2610, n2616);
    let n2618: ZB = zb_or(n2608, n2617);
    let n2619: ZB = zb_and(n1995, n2615);
    let n2620: ZB = zb_and(n1991, n1994);
    let n2621: ZB = zb_and(n2613, n2620);
    let n2622: ZB = zb_and(n2006, n2621);
    let n2623: ZB = zb_and(n2007, n2621);
    let n2624: ZB = zb_and(n2011, n2623);
    let n2625: ZB = zb_and(n2012, n2623);
    let n2626: ZB = zb_and(n2016, n2625);
    let n2627: ZB = zb_and(n2017, n2625);
    let n2628: ZB = zb_and(n2021, n2627);
    let n2629: ZB = zb_and(n2022, n2627);
    let n2630: ZB = zb_or(n2626, n2628);
    let n2631: ZB = zb_or(n2624, n2630);
    let n2632: ZB = zb_or(n2622, n2631);
    let n2633: ZB = zb_and(n2026, n2629);
    let n2634: ZB = zb_and(n2022, n2025);
    let n2635: ZB = zb_and(n2627, n2634);
    let n2636: ZB = zb_and(n2037, n2635);
    let n2637: ZB = zb_and(n2038, n2635);
    let n2638: ZB = zb_and(n2042, n2637);
    let n2639: ZB = zb_and(n2043, n2637);
    let n2640: ZB = zb_and(n2047, n2639);
    let n2641: ZB = zb_and(n2048, n2639);
    let n2642: ZB = zb_and(n2052, n2641);
    let n2643: ZB = zb_and(n2053, n2641);
    let n2644: ZB = zb_or(n2640, n2642);
    let n2645: ZB = zb_or(n2638, n2644);
    let n2646: ZB = zb_or(n2636, n2645);
    let n2647: ZB = zb_or(n2633, n2643);
    let n2648: ZB = zb_or(n2632, n2646);
    let n2649: ZB = zb_or(n2619, n2647);
    let n2650: ZB = zb_or(n2618, n2648);
    let n2651: ZB = zb_or(n2605, n2649);
    let n2652: ZB = zb_and(n2065, n2651);
    let n2653: ZB = zb_and(n2066, n2651);
    let n2654: ZB = zb_and(n1947, n2652);
    let n2655: ZB = zb_and(n1946, n2065);
    let n2656: ZB = zb_and(n2651, n2655);
    let n2657: ZB = zb_and(n2076, n2656);
    let n2658: ZB = zb_and(n2077, n2656);
    let n2659: ZB = zb_and(n2081, n2658);
    let n2660: ZB = zb_and(n2082, n2658);
    let n2661: ZB = zb_and(n2086, n2660);
    let n2662: ZB = zb_and(n2087, n2660);
    let n2663: ZB = zb_and(n2095, n2662);
    let n2664: ZB = zb_and(n2096, n2662);
    let n2665: ZB = zb_or(n2661, n2663);
    let n2666: ZB = zb_or(n2659, n2665);
    let n2667: ZB = zb_or(n2657, n2666);
    let n2668: ZB = zb_and(n1995, n2664);
    let n2669: ZB = zb_and(n1994, n2096);
    let n2670: ZB = zb_and(n2662, n2669);
    let n2671: ZB = zb_and(n2104, n2670);
    let n2672: ZB = zb_and(n2105, n2670);
    let n2673: ZB = zb_and(n2109, n2672);
    let n2674: ZB = zb_and(n2110, n2672);
    let n2675: ZB = zb_and(n2114, n2674);
    let n2676: ZB = zb_and(n2115, n2674);
    let n2677: ZB = zb_and(n2119, n2676);
    let n2678: ZB = zb_and(n2120, n2676);
    let n2679: ZB = zb_or(n2675, n2677);
    let n2680: ZB = zb_or(n2673, n2679);
    let n2681: ZB = zb_or(n2671, n2680);
    let n2682: ZB = zb_and(n2026, n2678);
    let n2683: ZB = zb_and(n2025, n2120);
    let n2684: ZB = zb_and(n2676, n2683);
    let n2685: ZB = zb_and(n2128, n2684);
    let n2686: ZB = zb_and(n2129, n2684);
    let n2687: ZB = zb_and(n2133, n2686);
    let n2688: ZB = zb_and(n2134, n2686);
    let n2689: ZB = zb_and(n2138, n2688);
    let n2690: ZB = zb_and(n2139, n2688);
    let n2691: ZB = zb_and(n2143, n2690);
    let n2692: ZB = zb_and(n2144, n2690);
    let n2693: ZB = zb_or(n2689, n2691);
    let n2694: ZB = zb_or(n2687, n2693);
    let n2695: ZB = zb_or(n2685, n2694);
    let n2696: ZB = zb_or(n2682, n2692);
    let n2697: ZB = zb_or(n2681, n2695);
    let n2698: ZB = zb_or(n2668, n2696);
    let n2699: ZB = zb_or(n2667, n2697);
    let n2700: ZB = zb_or(n2654, n2698);
    let n2701: ZB = zb_and(n2154, n2700);
    let n2702: ZB = zb_and(n2155, n2700);
    let n2703: ZB = zb_and(n1947, n2701);
    let n2704: ZB = zb_and(n1946, n2154);
    let n2705: ZB = zb_and(n2700, n2704);
    let n2706: ZB = zb_and(n2165, n2705);
    let n2707: ZB = zb_and(n2166, n2705);
    let n2708: ZB = zb_and(n2170, n2707);
    let n2709: ZB = zb_and(n2171, n2707);
    let n2710: ZB = zb_and(n2175, n2709);
    let n2711: ZB = zb_and(n2176, n2709);
    let n2712: ZB = zb_and(n2184, n2711);
    let n2713: ZB = zb_and(n2185, n2711);
    let n2714: ZB = zb_or(n2710, n2712);
    let n2715: ZB = zb_or(n2708, n2714);
    let n2716: ZB = zb_or(n2706, n2715);
    let n2717: ZB = zb_and(n1995, n2713);
    let n2718: ZB = zb_and(n1994, n2185);
    let n2719: ZB = zb_and(n2711, n2718);
    let n2720: ZB = zb_and(n2193, n2719);
    let n2721: ZB = zb_and(n2194, n2719);
    let n2722: ZB = zb_and(n2198, n2721);
    let n2723: ZB = zb_and(n2199, n2721);
    let n2724: ZB = zb_and(n2203, n2723);
    let n2725: ZB = zb_and(n2204, n2723);
    let n2726: ZB = zb_and(n2208, n2725);
    let n2727: ZB = zb_and(n2209, n2725);
    let n2728: ZB = zb_or(n2724, n2726);
    let n2729: ZB = zb_or(n2722, n2728);
    let n2730: ZB = zb_or(n2720, n2729);
    let n2731: ZB = zb_and(n2026, n2727);
    let n2732: ZB = zb_and(n2025, n2209);
    let n2733: ZB = zb_and(n2725, n2732);
    let n2734: ZB = zb_and(n2217, n2733);
    let n2735: ZB = zb_and(n2218, n2733);
    let n2736: ZB = zb_and(n2222, n2735);
    let n2737: ZB = zb_and(n2223, n2735);
    let n2738: ZB = zb_and(n2227, n2737);
    let n2739: ZB = zb_and(n2228, n2737);
    let n2740: ZB = zb_and(n2232, n2739);
    let n2741: ZB = zb_and(n2233, n2739);
    let n2742: ZB = zb_or(n2738, n2740);
    let n2743: ZB = zb_or(n2736, n2742);
    let n2744: ZB = zb_or(n2734, n2743);
    let n2745: ZB = zb_or(n2731, n2741);
    let n2746: ZB = zb_or(n2730, n2744);
    let n2747: ZB = zb_or(n2717, n2745);
    let n2748: ZB = zb_or(n2716, n2746);
    let n2749: ZB = zb_or(n2703, n2747);
    let n2750: ZB = zb_or(n2699, n2748);
    let n2751: ZB = zsel_b(n2699, n2063, n2152);
    let n2752: ZB = zb_or(n2702, n2749);
    let n2753: ZB = zb_or(n2650, n2750);
    let n2754: ZB = zsel_b(n2650, n1923, n2751);
    let n2755: ZB = zb_or(n2653, n2752);
    let n2756: ZB = zb_or(n2604, n2755);
    let n2757: ZB = zb_and(n2588, n2756);
    let n2758: ZB = zb_or(n2753, n2757);
    let n2759: ZB = zsel_b(n2753, n2754, n2250);
    let n2760: ZN = zsel_n(n2255, n2599, r_c300);
    let n2761: ZB = zn_gt(n2760, zn_splat(P8::from_raw(0i32)));
    let n2762: ZB = zb_and(n2338, n2758);
    let n2764: ZN = zsel_n(n2753, n2594, n1484);
    let n2766: ZB = zb_and(n255, n2761);
    let n2767: ZN = zsel_n(n2766, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2768: ZB = zb_or(r_c41, n2766);
    let n2769: ZN = zsel_n(n260, r_c20, n2767);
    let n2770: ZB = zsel_b(n260, r_c41, n2768);
    let n2773: ZB = zb_and(n2251, n2756);
    let n2774: ZB = zb_and(n2338, n2773);
    let n2775: ZB = zb_and(n2339, n2773);
    let n2776: ZB = zb_not(n2774);
    let n2777: ZB = zb_or(n2762, n2774);
    let n2778: ZB = zsel_b(n2774, n2250, n2759);
    let n2779: ZB = zb_not(n2777);
    let n2780: ZB = zb_or(n2776, n2779);
    let n2781: ZB = zsel_b(n2777, n2778, n1683);
    let n2782: ZN = zsel_n(n2774, r_c87, n2764);
    let n2783: ZN = zsel_n(n2774, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2784: ZN = zsel_n(n2777, n2782, n1484);
    let n2785: ZN = zsel_n(n2777, n2783, zn_splat(P8::from_raw(983040i32)));
    let n2787: ZN = zsel_n(n2777, n2769, n1015);
    let n2788: ZB = zb_not(n2340);
    let n2789: ZB = zb_or(n2340, n2592);
    let n2790: ZB = zsel_b(n2340, n2250, n2591);
    let n2791: ZB = zb_not(n2789);
    let n2792: ZB = zb_or(n2788, n2791);
    let n2793: ZB = zsel_b(n2789, n2790, n1551);
    let n2794: ZN = zsel_n(n2340, r_c87, n2595);
    let n2795: ZN = zsel_n(n2340, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2796: ZN = zsel_n(n2789, n2794, n1484);
    let n2797: ZN = zsel_n(n2789, n2795, zn_splat(P8::from_raw(983040i32)));
    let n2799: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2800: ZN = zsel_n(n263, r_c246, n1235);
    let n2801: ZN = zsel_n(n263, r_c254, n1240);
    let n2802: ZB = zb_and(r_c309, n263);
    let n2803: ZB = zb_and(r_c310, n263);
    let n2804: ZN = zn_sub(n2760, zn_splat(P8::from_raw(65536i32)));
    let n2805: ZN = zsel_n(n263, n2799, r_c20);
    let n2806: ZN = zsel_n(n263, r_c297, n259);
    let n2807: ZN = zsel_n(n263, r_c299, n262);
    let n2808: ZN = zsel_n(n263, r_c300, n2760);
    let n2809: ZN = zsel_n(n263, r_c302, n2257);
    let n2810: ZN = zsel_n(n263, r_c316, n1917);
    let n2811: ZN = zsel_n(n263, r_c317, n1918);
    let n2812: ZB = zsel_b(n263, r_c398, n2335);
    let n2813: ZN = zsel_n(n263, r_c404, n1919);
    let n2814: ZN = zsel_n(n263, r_c405, n1920);
    let n2815: ZN = zsel_n(n263, r_c406, n2336);
    let n2816: ZN = zsel_n(n263, r_c407, n2337);
    let n2817: ZB = zb_or(n263, n2775);
    let n2818: ZB = zb_or(n263, n2250);
    let n2819: ZB = zn_gt(n2805, zn_splat(P8::from_raw(0i32)));
    let n2820: ZB = zn_lt(n2810, zn_splat(P8::from_raw(-65536i32)));
    let n2821: ZB = zn_gt(n2810, zn_splat(P8::from_raw(7929856i32)));
    let n2822: ZB = zb_or(n2820, n2821);
    let n2823: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2810);
    let n2824: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2823);
    let n2825: ZN = zsel_n(n2822, n2824, n2810);
    let n2826: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2815);
    let n2827: ZN = zsel_n(n2819, n2810, n2825);
    let n2828: ZN = zsel_n(n2819, n2815, n2826);
    let n2830: ZB = zsel_b(n263, r_c398, n2374);
    let n2831: ZN = zsel_n(n263, r_c406, n2375);
    let n2832: ZN = zsel_n(n263, r_c407, n2376);
    let n2833: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2831);
    let n2834: ZN = zsel_n(n2819, n2831, n2833);
    let n2835: ZB = zsel_b(n263, r_c398, n2399);
    let n2836: ZN = zsel_n(n263, r_c406, n2400);
    let n2837: ZN = zsel_n(n263, r_c407, n2401);
    let n2838: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2836);
    let n2839: ZN = zsel_n(n2819, n2836, n2838);
    let n2840: ZB = zb_or(r_c310, n177);
    let n2841: ZN = zsel_n(n263, r_c302, n2407);
    let n2842: ZN = zsel_n(n263, r_c406, n2408);
    let n2843: ZN = zsel_n(n263, r_c407, n2409);
    let n2844: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2842);
    let n2845: ZN = zsel_n(n2819, n2842, n2844);
    let n2846: ZN = zsel_n(n263, r_c406, n2416);
    let n2847: ZN = zsel_n(n263, r_c407, n2417);
    let n2848: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2846);
    let n2849: ZN = zsel_n(n2819, n2846, n2848);
    let n2850: ZN = zsel_n(n263, r_c406, n2422);
    let n2851: ZN = zsel_n(n263, r_c407, n2423);
    let n2852: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2850);
    let n2853: ZN = zsel_n(n2819, n2850, n2852);
    let n2854: ZB = zb_or(r_c309, n177);
    let n2855: ZN = zsel_n(n2766, zn_splat(P8::from_raw(655360i32)), n259);
    let n2856: ZN = zsel_n(n2766, zn_splat(P8::from_raw(262144i32)), r_c299);
    let n2857: ZN = zsel_n(n2766, n2804, n2760);
    let n2858: ZN = zsel_n(n2766, zn_splat(P8::from_raw(98304i32)), r_c394);
    let n2859: ZN = zsel_n(n2766, n2334, r_c395);
    let n2860: ZN = zsel_n(n2766, n2331, r_c396);
    let n2861: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), r_c397);
    let n2862: ZN = zsel_n(n2766, n2327, n2295);
    let n2863: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2310);
    let n2864: ZN = zsel_n(n260, n259, n2855);
    let n2865: ZN = zsel_n(n260, n261, n2856);
    let n2866: ZN = zsel_n(n260, n2760, n2857);
    let n2867: ZN = zsel_n(n260, r_c394, n2858);
    let n2868: ZN = zsel_n(n260, r_c395, n2859);
    let n2869: ZN = zsel_n(n260, r_c396, n2860);
    let n2870: ZN = zsel_n(n260, r_c397, n2861);
    let n2871: ZN = zsel_n(n260, n2263, n2862);
    let n2872: ZN = zsel_n(n260, n2269, n2863);
    let n2873: ZN = zsel_n(n263, n2799, n2769);
    let n2874: ZB = zsel_b(n263, r_c41, n2770);
    let n2875: ZN = zsel_n(n263, r_c297, n2864);
    let n2876: ZN = zsel_n(n263, r_c299, n2865);
    let n2877: ZN = zsel_n(n263, r_c300, n2866);
    let n2878: ZN = zsel_n(n263, r_c394, n2867);
    let n2879: ZN = zsel_n(n263, r_c395, n2868);
    let n2880: ZN = zsel_n(n263, r_c396, n2869);
    let n2881: ZN = zsel_n(n263, r_c397, n2870);
    let n2882: ZN = zsel_n(n263, r_c406, n2871);
    let n2883: ZN = zsel_n(n263, r_c407, n2872);
    let n2884: ZB = zn_gt(n2873, zn_splat(P8::from_raw(0i32)));
    let n2885: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2882);
    let n2886: ZN = zsel_n(n2884, n2810, n2825);
    let n2887: ZN = zsel_n(n2884, n2882, n2885);
    let n2888: ZN = zsel_n(n2766, zn_splat(P8::from_raw(69510i32)), r_c395);
    let n2889: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-131072i32)), r_c396);
    let n2890: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-327680i32)), n2357);
    let n2891: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2369);
    let n2892: ZN = zsel_n(n260, r_c395, n2888);
    let n2893: ZN = zsel_n(n260, r_c396, n2889);
    let n2894: ZN = zsel_n(n260, n2263, n2890);
    let n2895: ZN = zsel_n(n260, n2269, n2891);
    let n2896: ZN = zsel_n(n263, r_c395, n2892);
    let n2897: ZN = zsel_n(n263, r_c396, n2893);
    let n2898: ZN = zsel_n(n263, r_c406, n2894);
    let n2899: ZN = zsel_n(n263, r_c407, n2895);
    let n2900: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2898);
    let n2901: ZN = zsel_n(n2884, n2898, n2900);
    let n2902: ZN = zsel_n(n2766, zn_splat(P8::from_raw(131072i32)), r_c396);
    let n2903: ZN = zsel_n(n2766, zn_splat(P8::from_raw(327680i32)), n2382);
    let n2904: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2394);
    let n2905: ZN = zsel_n(n260, r_c396, n2902);
    let n2906: ZN = zsel_n(n260, n2263, n2903);
    let n2907: ZN = zsel_n(n260, n2269, n2904);
    let n2908: ZN = zsel_n(n263, r_c396, n2905);
    let n2909: ZN = zsel_n(n263, r_c406, n2906);
    let n2910: ZN = zsel_n(n263, r_c407, n2907);
    let n2911: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2909);
    let n2912: ZN = zsel_n(n2884, n2909, n2911);
    let n2913: ZN = zsel_n(n2766, zn_splat(P8::from_raw(69510i32)), r_c394);
    let n2914: ZN = zsel_n(n2766, zn_splat(P8::from_raw(98304i32)), r_c395);
    let n2915: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), r_c396);
    let n2916: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-98304i32)), r_c397);
    let n2917: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2295);
    let n2918: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-327680i32)), n2310);
    let n2919: ZN = zsel_n(n260, r_c394, n2913);
    let n2920: ZN = zsel_n(n260, r_c395, n2914);
    let n2921: ZN = zsel_n(n260, r_c396, n2915);
    let n2922: ZN = zsel_n(n260, r_c397, n2916);
    let n2923: ZN = zsel_n(n260, n2263, n2917);
    let n2924: ZN = zsel_n(n260, n2269, n2918);
    let n2925: ZN = zsel_n(n263, r_c394, n2919);
    let n2926: ZN = zsel_n(n263, r_c395, n2920);
    let n2927: ZN = zsel_n(n263, r_c396, n2921);
    let n2928: ZN = zsel_n(n263, r_c397, n2922);
    let n2929: ZN = zsel_n(n263, r_c406, n2923);
    let n2930: ZN = zsel_n(n263, r_c407, n2924);
    let n2931: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2929);
    let n2932: ZN = zsel_n(n2884, n2929, n2931);
    let n2933: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2357);
    let n2934: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2369);
    let n2935: ZN = zsel_n(n260, n2263, n2933);
    let n2936: ZN = zsel_n(n260, n2269, n2934);
    let n2937: ZN = zsel_n(n263, r_c406, n2935);
    let n2938: ZN = zsel_n(n263, r_c407, n2936);
    let n2939: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2937);
    let n2940: ZN = zsel_n(n2884, n2937, n2939);
    let n2941: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2382);
    let n2942: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2394);
    let n2943: ZN = zsel_n(n260, n2263, n2941);
    let n2944: ZN = zsel_n(n260, n2269, n2942);
    let n2945: ZN = zsel_n(n263, r_c406, n2943);
    let n2946: ZN = zsel_n(n263, r_c407, n2944);
    let n2947: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2945);
    let n2948: ZN = zsel_n(n2884, n2945, n2947);
    let n2949: ZN = zsel_n(n2766, zn_splat(P8::from_raw(131072i32)), r_c397);
    let n2950: ZN = zsel_n(n2766, zn_splat(P8::from_raw(327680i32)), n2310);
    let n2951: ZN = zsel_n(n260, r_c397, n2949);
    let n2952: ZN = zsel_n(n260, n2269, n2950);
    let n2953: ZN = zsel_n(n263, r_c397, n2951);
    let n2954: ZN = zsel_n(n263, r_c407, n2952);
    let n2955: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2369);
    let n2956: ZN = zsel_n(n260, n2269, n2955);
    let n2957: ZN = zsel_n(n263, r_c407, n2956);
    let n2958: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2394);
    let n2959: ZN = zsel_n(n260, n2269, n2958);
    let n2960: ZN = zsel_n(n263, r_c407, n2959);
    let n2961: ZN = zsel_n(n2766, n2327, n2405);
    let n2962: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2406);
    let n2963: ZN = zsel_n(n260, n2263, n2961);
    let n2964: ZN = zsel_n(n260, n2269, n2962);
    let n2965: ZN = zsel_n(n263, r_c406, n2963);
    let n2966: ZN = zsel_n(n263, r_c407, n2964);
    let n2967: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2965);
    let n2968: ZN = zsel_n(n2884, n2965, n2967);
    let n2969: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-327680i32)), n2414);
    let n2970: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2415);
    let n2971: ZN = zsel_n(n260, n2263, n2969);
    let n2972: ZN = zsel_n(n260, n2269, n2970);
    let n2973: ZN = zsel_n(n263, r_c406, n2971);
    let n2974: ZN = zsel_n(n263, r_c407, n2972);
    let n2975: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2973);
    let n2976: ZN = zsel_n(n2884, n2973, n2975);
    let n2977: ZN = zsel_n(n2766, zn_splat(P8::from_raw(327680i32)), n2420);
    let n2978: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2421);
    let n2979: ZN = zsel_n(n260, n2263, n2977);
    let n2980: ZN = zsel_n(n260, n2269, n2978);
    let n2981: ZN = zsel_n(n263, r_c406, n2979);
    let n2982: ZN = zsel_n(n263, r_c407, n2980);
    let n2983: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2981);
    let n2984: ZN = zsel_n(n2884, n2981, n2983);
    let n2985: ZN = zsel_n(n2766, zn_splat(P8::from_raw(0i32)), n2405);
    let n2986: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-327680i32)), n2406);
    let n2987: ZN = zsel_n(n260, n2263, n2985);
    let n2988: ZN = zsel_n(n260, n2269, n2986);
    let n2989: ZN = zsel_n(n263, r_c406, n2987);
    let n2990: ZN = zsel_n(n263, r_c407, n2988);
    let n2991: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2989);
    let n2992: ZN = zsel_n(n2884, n2989, n2991);
    let n2993: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2414);
    let n2994: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2415);
    let n2995: ZN = zsel_n(n260, n2263, n2993);
    let n2996: ZN = zsel_n(n260, n2269, n2994);
    let n2997: ZN = zsel_n(n263, r_c406, n2995);
    let n2998: ZN = zsel_n(n263, r_c407, n2996);
    let n2999: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n2997);
    let n3000: ZN = zsel_n(n2884, n2997, n2999);
    let n3001: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2420);
    let n3002: ZN = zsel_n(n2766, zn_splat(P8::from_raw(-231700i32)), n2421);
    let n3003: ZN = zsel_n(n260, n2263, n3001);
    let n3004: ZN = zsel_n(n260, n2269, n3002);
    let n3005: ZN = zsel_n(n263, r_c406, n3003);
    let n3006: ZN = zsel_n(n263, r_c407, n3004);
    let n3007: ZN = zsel_n(n2822, zn_splat(P8::from_raw(0i32)), n3005);
    let n3008: ZN = zsel_n(n2884, n3005, n3007);
    let n3009: ZN = zsel_n(n2766, zn_splat(P8::from_raw(327680i32)), n2406);
    let n3010: ZN = zsel_n(n260, n2269, n3009);
    let n3011: ZN = zsel_n(n263, r_c407, n3010);
    let n3012: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2415);
    let n3013: ZN = zsel_n(n260, n2269, n3012);
    let n3014: ZN = zsel_n(n263, r_c407, n3013);
    let n3015: ZN = zsel_n(n2766, zn_splat(P8::from_raw(231700i32)), n2421);
    let n3016: ZN = zsel_n(n260, n2269, n3015);
    let n3017: ZN = zsel_n(n263, r_c407, n3016);
    let n3019: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n3020: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n3021: ZW = zw_add(zw_splat(0u64), n3019);
    let n3022: ZW = zw_add(zw_splat(0u64), n3020);
    let n3023: ZW = zw_cellmix_n(84u64, n138, 1542469173u64);
    let n3024: ZW = zw_cellmix_n(84u64, n138, 668265263u64);
    let n3025: ZW = zw_add(n3021, n3023);
    let n3026: ZW = zw_add(n3022, n3024);
    let n3027: ZW = zw_cellmix_n(85u64, n245, 1542469173u64);
    let n3028: ZW = zw_cellmix_n(85u64, n245, 668265263u64);
    let n3029: ZW = zw_add(n3025, n3027);
    let n3030: ZW = zw_add(n3026, n3028);
    let n3031: ZW = zw_cellmix_n(86u64, n244, 1542469173u64);
    let n3032: ZW = zw_cellmix_n(86u64, n244, 668265263u64);
    let n3033: ZW = zw_add(n3029, n3031);
    let n3034: ZW = zw_add(n3030, n3032);
    let n3035: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n3036: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n3037: ZW = zw_add(n3033, n3035);
    let n3038: ZW = zw_add(n3034, n3036);
    let n3039: ZW = zw_cellmix_n(301u64, n513, 1542469173u64);
    let n3040: ZW = zw_cellmix_n(301u64, n513, 668265263u64);
    let n3041: ZW = zw_add(n3037, n3039);
    let n3042: ZW = zw_add(n3038, n3040);
    let n3043: ZW = zw_cellmix_n(367u64, n402, 1542469173u64);
    let n3044: ZW = zw_cellmix_n(367u64, n402, 668265263u64);
    let n3045: ZW = zw_add(n3041, n3043);
    let n3046: ZW = zw_add(n3042, n3044);
    let n3047: ZW = zw_cellmix_n(368u64, n514, 1542469173u64);
    let n3048: ZW = zw_cellmix_n(368u64, n514, 668265263u64);
    let n3049: ZW = zw_add(n3045, n3047);
    let n3050: ZW = zw_add(n3046, n3048);
    let n3051: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n3052: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n3053: ZW = zw_add(n3049, n3051);
    let n3054: ZW = zw_add(n3050, n3052);
    let n3055: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n3056: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n3057: ZW = zw_add(n3053, n3055);
    let n3058: ZW = zw_add(n3054, n3056);
    let n3059: ZW = zw_cellmix_n(281u64, n259, 1542469173u64);
    let n3060: ZW = zw_cellmix_n(281u64, n259, 668265263u64);
    let n3061: ZW = zw_add(n3057, n3059);
    let n3062: ZW = zw_add(n3058, n3060);
    let n3063: ZW = zw_cellmix_n(283u64, n262, 1542469173u64);
    let n3064: ZW = zw_cellmix_n(283u64, n262, 668265263u64);
    let n3065: ZW = zw_add(n3061, n3063);
    let n3066: ZW = zw_add(n3062, n3064);
    let n3067: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n3068: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n3069: ZW = zw_add(n3065, n3067);
    let n3070: ZW = zw_add(n3066, n3068);
    let n3071: ZW = zw_cellmix_n(286u64, n845, 1542469173u64);
    let n3072: ZW = zw_cellmix_n(286u64, n845, 668265263u64);
    let n3073: ZW = zw_add(n3069, n3071);
    let n3074: ZW = zw_add(n3070, n3072);
    let n3075: ZW = zw_cellmix_b(293u64, zb_splat(false), 1542469173u64);
    let n3076: ZW = zw_cellmix_b(293u64, zb_splat(false), 668265263u64);
    let n3077: ZW = zw_add(n3073, n3075);
    let n3078: ZW = zw_add(n3074, n3076);
    let n3079: ZW = zw_cellmix_b(294u64, zb_splat(false), 1542469173u64);
    let n3080: ZW = zw_cellmix_b(294u64, zb_splat(false), 668265263u64);
    let n3081: ZW = zw_add(n3077, n3079);
    let n3082: ZW = zw_add(n3078, n3080);
    let n3083: ZW = zw_cellmix_n(300u64, n930, 1542469173u64);
    let n3084: ZW = zw_cellmix_n(300u64, n930, 668265263u64);
    let n3085: ZW = zw_add(n3081, n3083);
    let n3086: ZW = zw_add(n3082, n3084);
    let n3087: ZW = zw_cellmix_n(357u64, r_c394, 1542469173u64);
    let n3088: ZW = zw_cellmix_n(357u64, r_c394, 668265263u64);
    let n3089: ZW = zw_add(n3085, n3087);
    let n3090: ZW = zw_add(n3086, n3088);
    let n3091: ZW = zw_cellmix_n(358u64, r_c395, 1542469173u64);
    let n3092: ZW = zw_cellmix_n(358u64, r_c395, 668265263u64);
    let n3093: ZW = zw_add(n3089, n3091);
    let n3094: ZW = zw_add(n3090, n3092);
    let n3095: ZW = zw_cellmix_n(359u64, r_c396, 1542469173u64);
    let n3096: ZW = zw_cellmix_n(359u64, r_c396, 668265263u64);
    let n3097: ZW = zw_add(n3093, n3095);
    let n3098: ZW = zw_add(n3094, n3096);
    let n3099: ZW = zw_cellmix_n(360u64, r_c397, 1542469173u64);
    let n3100: ZW = zw_cellmix_n(360u64, r_c397, 668265263u64);
    let n3101: ZW = zw_add(n3097, n3099);
    let n3102: ZW = zw_add(n3098, n3100);
    let n3103: ZW = zw_cellmix_b(361u64, n919, 1542469173u64);
    let n3104: ZW = zw_cellmix_b(361u64, n919, 668265263u64);
    let n3105: ZW = zw_add(n3101, n3103);
    let n3106: ZW = zw_add(n3102, n3104);
    let n3107: ZW = zw_cellmix_n(369u64, n931, 1542469173u64);
    let n3108: ZW = zw_cellmix_n(369u64, n931, 668265263u64);
    let n3109: ZW = zw_add(n3105, n3107);
    let n3110: ZW = zw_add(n3106, n3108);
    let n3111: ZW = zw_cellmix_n(370u64, n921, 1542469173u64);
    let n3112: ZW = zw_cellmix_n(370u64, n921, 668265263u64);
    let n3113: ZW = zw_add(n3109, n3111);
    let n3114: ZW = zw_add(n3110, n3112);
    let n3115: ZW = zw_cellmix_b(361u64, n961, 1542469173u64);
    let n3116: ZW = zw_cellmix_b(361u64, n961, 668265263u64);
    let n3117: ZW = zw_add(n3101, n3115);
    let n3118: ZW = zw_add(n3102, n3116);
    let n3119: ZW = zw_cellmix_n(369u64, n965, 1542469173u64);
    let n3120: ZW = zw_cellmix_n(369u64, n965, 668265263u64);
    let n3121: ZW = zw_add(n3117, n3119);
    let n3122: ZW = zw_add(n3118, n3120);
    let n3123: ZW = zw_cellmix_n(370u64, n963, 1542469173u64);
    let n3124: ZW = zw_cellmix_n(370u64, n963, 668265263u64);
    let n3125: ZW = zw_add(n3121, n3123);
    let n3126: ZW = zw_add(n3122, n3124);
    let n3127: ZW = zw_cellmix_b(361u64, n983, 1542469173u64);
    let n3128: ZW = zw_cellmix_b(361u64, n983, 668265263u64);
    let n3129: ZW = zw_add(n3101, n3127);
    let n3130: ZW = zw_add(n3102, n3128);
    let n3131: ZW = zw_cellmix_n(369u64, n987, 1542469173u64);
    let n3132: ZW = zw_cellmix_n(369u64, n987, 668265263u64);
    let n3133: ZW = zw_add(n3129, n3131);
    let n3134: ZW = zw_add(n3130, n3132);
    let n3135: ZW = zw_cellmix_n(370u64, n985, 1542469173u64);
    let n3136: ZW = zw_cellmix_n(370u64, n985, 668265263u64);
    let n3137: ZW = zw_add(n3133, n3135);
    let n3138: ZW = zw_add(n3134, n3136);
    let n3139: ZW = zw_cellmix_n(286u64, n991, 1542469173u64);
    let n3140: ZW = zw_cellmix_n(286u64, n991, 668265263u64);
    let n3141: ZW = zw_add(n3069, n3139);
    let n3142: ZW = zw_add(n3070, n3140);
    let n3143: ZW = zw_add(n3141, n3075);
    let n3144: ZW = zw_add(n3142, n3076);
    let n3145: ZW = zw_cellmix_b(294u64, zb_splat(true), 1542469173u64);
    let n3146: ZW = zw_cellmix_b(294u64, zb_splat(true), 668265263u64);
    let n3147: ZW = zw_add(n3143, n3145);
    let n3148: ZW = zw_add(n3144, n3146);
    let n3149: ZW = zw_add(n3147, n3083);
    let n3150: ZW = zw_add(n3148, n3084);
    let n3151: ZW = zw_add(n3149, n3087);
    let n3152: ZW = zw_add(n3150, n3088);
    let n3153: ZW = zw_add(n3151, n3091);
    let n3154: ZW = zw_add(n3152, n3092);
    let n3155: ZW = zw_add(n3153, n3095);
    let n3156: ZW = zw_add(n3154, n3096);
    let n3157: ZW = zw_add(n3155, n3099);
    let n3158: ZW = zw_add(n3156, n3100);
    let n3159: ZW = zw_add(n3157, n3103);
    let n3160: ZW = zw_add(n3158, n3104);
    let n3161: ZW = zw_cellmix_n(369u64, n995, 1542469173u64);
    let n3162: ZW = zw_cellmix_n(369u64, n995, 668265263u64);
    let n3163: ZW = zw_add(n3159, n3161);
    let n3164: ZW = zw_add(n3160, n3162);
    let n3165: ZW = zw_cellmix_n(370u64, n993, 1542469173u64);
    let n3166: ZW = zw_cellmix_n(370u64, n993, 668265263u64);
    let n3167: ZW = zw_add(n3163, n3165);
    let n3168: ZW = zw_add(n3164, n3166);
    let n3169: ZW = zw_add(n3157, n3115);
    let n3170: ZW = zw_add(n3158, n3116);
    let n3171: ZW = zw_cellmix_n(369u64, n1001, 1542469173u64);
    let n3172: ZW = zw_cellmix_n(369u64, n1001, 668265263u64);
    let n3173: ZW = zw_add(n3169, n3171);
    let n3174: ZW = zw_add(n3170, n3172);
    let n3175: ZW = zw_cellmix_n(370u64, n999, 1542469173u64);
    let n3176: ZW = zw_cellmix_n(370u64, n999, 668265263u64);
    let n3177: ZW = zw_add(n3173, n3175);
    let n3178: ZW = zw_add(n3174, n3176);
    let n3179: ZW = zw_add(n3157, n3127);
    let n3180: ZW = zw_add(n3158, n3128);
    let n3181: ZW = zw_cellmix_n(369u64, n1007, 1542469173u64);
    let n3182: ZW = zw_cellmix_n(369u64, n1007, 668265263u64);
    let n3183: ZW = zw_add(n3179, n3181);
    let n3184: ZW = zw_add(n3180, n3182);
    let n3185: ZW = zw_cellmix_n(370u64, n1005, 1542469173u64);
    let n3186: ZW = zw_cellmix_n(370u64, n1005, 668265263u64);
    let n3187: ZW = zw_add(n3183, n3185);
    let n3188: ZW = zw_add(n3184, n3186);
    let n3189: ZW = zw_cellmix_n(20u64, n1015, 1542469173u64);
    let n3190: ZW = zw_cellmix_n(20u64, n1015, 668265263u64);
    let n3191: ZW = zw_add(n3049, n3189);
    let n3192: ZW = zw_add(n3050, n3190);
    let n3193: ZW = zw_cellmix_b(41u64, n1016, 1542469173u64);
    let n3194: ZW = zw_cellmix_b(41u64, n1016, 668265263u64);
    let n3195: ZW = zw_add(n3191, n3193);
    let n3196: ZW = zw_add(n3192, n3194);
    let n3197: ZW = zw_cellmix_n(281u64, n1017, 1542469173u64);
    let n3198: ZW = zw_cellmix_n(281u64, n1017, 668265263u64);
    let n3199: ZW = zw_add(n3195, n3197);
    let n3200: ZW = zw_add(n3196, n3198);
    let n3201: ZW = zw_cellmix_n(283u64, n1018, 1542469173u64);
    let n3202: ZW = zw_cellmix_n(283u64, n1018, 668265263u64);
    let n3203: ZW = zw_add(n3199, n3201);
    let n3204: ZW = zw_add(n3200, n3202);
    let n3205: ZW = zw_cellmix_n(284u64, n1019, 1542469173u64);
    let n3206: ZW = zw_cellmix_n(284u64, n1019, 668265263u64);
    let n3207: ZW = zw_add(n3203, n3205);
    let n3208: ZW = zw_add(n3204, n3206);
    let n3209: ZW = zw_add(n3207, n3071);
    let n3210: ZW = zw_add(n3208, n3072);
    let n3211: ZW = zw_cellmix_b(293u64, zb_splat(true), 1542469173u64);
    let n3212: ZW = zw_cellmix_b(293u64, zb_splat(true), 668265263u64);
    let n3213: ZW = zw_add(n3209, n3211);
    let n3214: ZW = zw_add(n3210, n3212);
    let n3215: ZW = zw_add(n3213, n3079);
    let n3216: ZW = zw_add(n3214, n3080);
    let n3217: ZW = zw_cellmix_n(300u64, n1032, 1542469173u64);
    let n3218: ZW = zw_cellmix_n(300u64, n1032, 668265263u64);
    let n3219: ZW = zw_add(n3215, n3217);
    let n3220: ZW = zw_add(n3216, n3218);
    let n3221: ZW = zw_cellmix_n(357u64, n1020, 1542469173u64);
    let n3222: ZW = zw_cellmix_n(357u64, n1020, 668265263u64);
    let n3223: ZW = zw_add(n3219, n3221);
    let n3224: ZW = zw_add(n3220, n3222);
    let n3225: ZW = zw_cellmix_n(358u64, n1027, 1542469173u64);
    let n3226: ZW = zw_cellmix_n(358u64, n1027, 668265263u64);
    let n3227: ZW = zw_add(n3223, n3225);
    let n3228: ZW = zw_add(n3224, n3226);
    let n3229: ZW = zw_cellmix_n(359u64, n1028, 1542469173u64);
    let n3230: ZW = zw_cellmix_n(359u64, n1028, 668265263u64);
    let n3231: ZW = zw_add(n3227, n3229);
    let n3232: ZW = zw_add(n3228, n3230);
    let n3233: ZW = zw_cellmix_n(360u64, n1021, 1542469173u64);
    let n3234: ZW = zw_cellmix_n(360u64, n1021, 668265263u64);
    let n3235: ZW = zw_add(n3231, n3233);
    let n3236: ZW = zw_add(n3232, n3234);
    let n3237: ZW = zw_add(n3235, n3103);
    let n3238: ZW = zw_add(n3236, n3104);
    let n3239: ZW = zw_cellmix_n(369u64, n1033, 1542469173u64);
    let n3240: ZW = zw_cellmix_n(369u64, n1033, 668265263u64);
    let n3241: ZW = zw_add(n3237, n3239);
    let n3242: ZW = zw_add(n3238, n3240);
    let n3243: ZW = zw_cellmix_n(370u64, n1030, 1542469173u64);
    let n3244: ZW = zw_cellmix_n(370u64, n1030, 668265263u64);
    let n3245: ZW = zw_add(n3241, n3243);
    let n3246: ZW = zw_add(n3242, n3244);
    let n3247: ZW = zw_cellmix_n(358u64, n1038, 1542469173u64);
    let n3248: ZW = zw_cellmix_n(358u64, n1038, 668265263u64);
    let n3249: ZW = zw_add(n3223, n3247);
    let n3250: ZW = zw_add(n3224, n3248);
    let n3251: ZW = zw_cellmix_n(359u64, n1039, 1542469173u64);
    let n3252: ZW = zw_cellmix_n(359u64, n1039, 668265263u64);
    let n3253: ZW = zw_add(n3249, n3251);
    let n3254: ZW = zw_add(n3250, n3252);
    let n3255: ZW = zw_add(n3253, n3233);
    let n3256: ZW = zw_add(n3254, n3234);
    let n3257: ZW = zw_add(n3255, n3115);
    let n3258: ZW = zw_add(n3256, n3116);
    let n3259: ZW = zw_cellmix_n(369u64, n1043, 1542469173u64);
    let n3260: ZW = zw_cellmix_n(369u64, n1043, 668265263u64);
    let n3261: ZW = zw_add(n3257, n3259);
    let n3262: ZW = zw_add(n3258, n3260);
    let n3263: ZW = zw_cellmix_n(370u64, n1041, 1542469173u64);
    let n3264: ZW = zw_cellmix_n(370u64, n1041, 668265263u64);
    let n3265: ZW = zw_add(n3261, n3263);
    let n3266: ZW = zw_add(n3262, n3264);
    let n3267: ZW = zw_cellmix_n(359u64, n1047, 1542469173u64);
    let n3268: ZW = zw_cellmix_n(359u64, n1047, 668265263u64);
    let n3269: ZW = zw_add(n3249, n3267);
    let n3270: ZW = zw_add(n3250, n3268);
    let n3271: ZW = zw_add(n3269, n3233);
    let n3272: ZW = zw_add(n3270, n3234);
    let n3273: ZW = zw_add(n3271, n3127);
    let n3274: ZW = zw_add(n3272, n3128);
    let n3275: ZW = zw_cellmix_n(369u64, n1051, 1542469173u64);
    let n3276: ZW = zw_cellmix_n(369u64, n1051, 668265263u64);
    let n3277: ZW = zw_add(n3273, n3275);
    let n3278: ZW = zw_add(n3274, n3276);
    let n3279: ZW = zw_cellmix_n(370u64, n1049, 1542469173u64);
    let n3280: ZW = zw_cellmix_n(370u64, n1049, 668265263u64);
    let n3281: ZW = zw_add(n3277, n3279);
    let n3282: ZW = zw_add(n3278, n3280);
    let n3283: ZW = zw_cellmix_n(357u64, n1055, 1542469173u64);
    let n3284: ZW = zw_cellmix_n(357u64, n1055, 668265263u64);
    let n3285: ZW = zw_add(n3219, n3283);
    let n3286: ZW = zw_add(n3220, n3284);
    let n3287: ZW = zw_cellmix_n(358u64, n1061, 1542469173u64);
    let n3288: ZW = zw_cellmix_n(358u64, n1061, 668265263u64);
    let n3289: ZW = zw_add(n3285, n3287);
    let n3290: ZW = zw_add(n3286, n3288);
    let n3291: ZW = zw_cellmix_n(359u64, n1062, 1542469173u64);
    let n3292: ZW = zw_cellmix_n(359u64, n1062, 668265263u64);
    let n3293: ZW = zw_add(n3289, n3291);
    let n3294: ZW = zw_add(n3290, n3292);
    let n3295: ZW = zw_cellmix_n(360u64, n1056, 1542469173u64);
    let n3296: ZW = zw_cellmix_n(360u64, n1056, 668265263u64);
    let n3297: ZW = zw_add(n3293, n3295);
    let n3298: ZW = zw_add(n3294, n3296);
    let n3299: ZW = zw_add(n3297, n3103);
    let n3300: ZW = zw_add(n3298, n3104);
    let n3301: ZW = zw_cellmix_n(369u64, n1066, 1542469173u64);
    let n3302: ZW = zw_cellmix_n(369u64, n1066, 668265263u64);
    let n3303: ZW = zw_add(n3299, n3301);
    let n3304: ZW = zw_add(n3300, n3302);
    let n3305: ZW = zw_cellmix_n(370u64, n1064, 1542469173u64);
    let n3306: ZW = zw_cellmix_n(370u64, n1064, 668265263u64);
    let n3307: ZW = zw_add(n3303, n3305);
    let n3308: ZW = zw_add(n3304, n3306);
    let n3309: ZW = zw_add(n3285, n3247);
    let n3310: ZW = zw_add(n3286, n3248);
    let n3311: ZW = zw_add(n3309, n3251);
    let n3312: ZW = zw_add(n3310, n3252);
    let n3313: ZW = zw_add(n3311, n3295);
    let n3314: ZW = zw_add(n3312, n3296);
    let n3315: ZW = zw_add(n3313, n3115);
    let n3316: ZW = zw_add(n3314, n3116);
    let n3317: ZW = zw_cellmix_n(369u64, n1072, 1542469173u64);
    let n3318: ZW = zw_cellmix_n(369u64, n1072, 668265263u64);
    let n3319: ZW = zw_add(n3315, n3317);
    let n3320: ZW = zw_add(n3316, n3318);
    let n3321: ZW = zw_cellmix_n(370u64, n1070, 1542469173u64);
    let n3322: ZW = zw_cellmix_n(370u64, n1070, 668265263u64);
    let n3323: ZW = zw_add(n3319, n3321);
    let n3324: ZW = zw_add(n3320, n3322);
    let n3325: ZW = zw_add(n3309, n3267);
    let n3326: ZW = zw_add(n3310, n3268);
    let n3327: ZW = zw_add(n3325, n3295);
    let n3328: ZW = zw_add(n3326, n3296);
    let n3329: ZW = zw_add(n3327, n3127);
    let n3330: ZW = zw_add(n3328, n3128);
    let n3331: ZW = zw_cellmix_n(369u64, n1078, 1542469173u64);
    let n3332: ZW = zw_cellmix_n(369u64, n1078, 668265263u64);
    let n3333: ZW = zw_add(n3329, n3331);
    let n3334: ZW = zw_add(n3330, n3332);
    let n3335: ZW = zw_cellmix_n(370u64, n1076, 1542469173u64);
    let n3336: ZW = zw_cellmix_n(370u64, n1076, 668265263u64);
    let n3337: ZW = zw_add(n3333, n3335);
    let n3338: ZW = zw_add(n3334, n3336);
    let n3339: ZW = zw_cellmix_n(360u64, n1080, 1542469173u64);
    let n3340: ZW = zw_cellmix_n(360u64, n1080, 668265263u64);
    let n3341: ZW = zw_add(n3293, n3339);
    let n3342: ZW = zw_add(n3294, n3340);
    let n3343: ZW = zw_add(n3341, n3103);
    let n3344: ZW = zw_add(n3342, n3104);
    let n3345: ZW = zw_add(n3343, n3301);
    let n3346: ZW = zw_add(n3344, n3302);
    let n3347: ZW = zw_cellmix_n(370u64, n1082, 1542469173u64);
    let n3348: ZW = zw_cellmix_n(370u64, n1082, 668265263u64);
    let n3349: ZW = zw_add(n3345, n3347);
    let n3350: ZW = zw_add(n3346, n3348);
    let n3351: ZW = zw_add(n3311, n3339);
    let n3352: ZW = zw_add(n3312, n3340);
    let n3353: ZW = zw_add(n3351, n3115);
    let n3354: ZW = zw_add(n3352, n3116);
    let n3355: ZW = zw_add(n3353, n3317);
    let n3356: ZW = zw_add(n3354, n3318);
    let n3357: ZW = zw_cellmix_n(370u64, n1084, 1542469173u64);
    let n3358: ZW = zw_cellmix_n(370u64, n1084, 668265263u64);
    let n3359: ZW = zw_add(n3355, n3357);
    let n3360: ZW = zw_add(n3356, n3358);
    let n3361: ZW = zw_add(n3325, n3339);
    let n3362: ZW = zw_add(n3326, n3340);
    let n3363: ZW = zw_add(n3361, n3127);
    let n3364: ZW = zw_add(n3362, n3128);
    let n3365: ZW = zw_add(n3363, n3331);
    let n3366: ZW = zw_add(n3364, n3332);
    let n3367: ZW = zw_cellmix_n(370u64, n1086, 1542469173u64);
    let n3368: ZW = zw_cellmix_n(370u64, n1086, 668265263u64);
    let n3369: ZW = zw_add(n3365, n3367);
    let n3370: ZW = zw_add(n3366, n3368);
    let n3371: ZW = zw_add(n3207, n3139);
    let n3372: ZW = zw_add(n3208, n3140);
    let n3373: ZW = zw_add(n3371, n3211);
    let n3374: ZW = zw_add(n3372, n3212);
    let n3375: ZW = zw_add(n3373, n3145);
    let n3376: ZW = zw_add(n3374, n3146);
    let n3377: ZW = zw_add(n3375, n3217);
    let n3378: ZW = zw_add(n3376, n3218);
    let n3379: ZW = zw_add(n3377, n3221);
    let n3380: ZW = zw_add(n3378, n3222);
    let n3381: ZW = zw_add(n3379, n3225);
    let n3382: ZW = zw_add(n3380, n3226);
    let n3383: ZW = zw_add(n3381, n3229);
    let n3384: ZW = zw_add(n3382, n3230);
    let n3385: ZW = zw_add(n3383, n3233);
    let n3386: ZW = zw_add(n3384, n3234);
    let n3387: ZW = zw_add(n3385, n3103);
    let n3388: ZW = zw_add(n3386, n3104);
    let n3389: ZW = zw_cellmix_n(369u64, n1092, 1542469173u64);
    let n3390: ZW = zw_cellmix_n(369u64, n1092, 668265263u64);
    let n3391: ZW = zw_add(n3387, n3389);
    let n3392: ZW = zw_add(n3388, n3390);
    let n3393: ZW = zw_cellmix_n(370u64, n1090, 1542469173u64);
    let n3394: ZW = zw_cellmix_n(370u64, n1090, 668265263u64);
    let n3395: ZW = zw_add(n3391, n3393);
    let n3396: ZW = zw_add(n3392, n3394);
    let n3397: ZW = zw_add(n3379, n3247);
    let n3398: ZW = zw_add(n3380, n3248);
    let n3399: ZW = zw_add(n3397, n3251);
    let n3400: ZW = zw_add(n3398, n3252);
    let n3401: ZW = zw_add(n3399, n3233);
    let n3402: ZW = zw_add(n3400, n3234);
    let n3403: ZW = zw_add(n3401, n3115);
    let n3404: ZW = zw_add(n3402, n3116);
    let n3405: ZW = zw_cellmix_n(369u64, n1098, 1542469173u64);
    let n3406: ZW = zw_cellmix_n(369u64, n1098, 668265263u64);
    let n3407: ZW = zw_add(n3403, n3405);
    let n3408: ZW = zw_add(n3404, n3406);
    let n3409: ZW = zw_cellmix_n(370u64, n1096, 1542469173u64);
    let n3410: ZW = zw_cellmix_n(370u64, n1096, 668265263u64);
    let n3411: ZW = zw_add(n3407, n3409);
    let n3412: ZW = zw_add(n3408, n3410);
    let n3413: ZW = zw_add(n3397, n3267);
    let n3414: ZW = zw_add(n3398, n3268);
    let n3415: ZW = zw_add(n3413, n3233);
    let n3416: ZW = zw_add(n3414, n3234);
    let n3417: ZW = zw_add(n3415, n3127);
    let n3418: ZW = zw_add(n3416, n3128);
    let n3419: ZW = zw_cellmix_n(369u64, n1104, 1542469173u64);
    let n3420: ZW = zw_cellmix_n(369u64, n1104, 668265263u64);
    let n3421: ZW = zw_add(n3417, n3419);
    let n3422: ZW = zw_add(n3418, n3420);
    let n3423: ZW = zw_cellmix_n(370u64, n1102, 1542469173u64);
    let n3424: ZW = zw_cellmix_n(370u64, n1102, 668265263u64);
    let n3425: ZW = zw_add(n3421, n3423);
    let n3426: ZW = zw_add(n3422, n3424);
    let n3427: ZW = zw_add(n3377, n3283);
    let n3428: ZW = zw_add(n3378, n3284);
    let n3429: ZW = zw_add(n3427, n3287);
    let n3430: ZW = zw_add(n3428, n3288);
    let n3431: ZW = zw_add(n3429, n3291);
    let n3432: ZW = zw_add(n3430, n3292);
    let n3433: ZW = zw_add(n3431, n3295);
    let n3434: ZW = zw_add(n3432, n3296);
    let n3435: ZW = zw_add(n3433, n3103);
    let n3436: ZW = zw_add(n3434, n3104);
    let n3437: ZW = zw_cellmix_n(369u64, n1110, 1542469173u64);
    let n3438: ZW = zw_cellmix_n(369u64, n1110, 668265263u64);
    let n3439: ZW = zw_add(n3435, n3437);
    let n3440: ZW = zw_add(n3436, n3438);
    let n3441: ZW = zw_cellmix_n(370u64, n1108, 1542469173u64);
    let n3442: ZW = zw_cellmix_n(370u64, n1108, 668265263u64);
    let n3443: ZW = zw_add(n3439, n3441);
    let n3444: ZW = zw_add(n3440, n3442);
    let n3445: ZW = zw_add(n3427, n3247);
    let n3446: ZW = zw_add(n3428, n3248);
    let n3447: ZW = zw_add(n3445, n3251);
    let n3448: ZW = zw_add(n3446, n3252);
    let n3449: ZW = zw_add(n3447, n3295);
    let n3450: ZW = zw_add(n3448, n3296);
    let n3451: ZW = zw_add(n3449, n3115);
    let n3452: ZW = zw_add(n3450, n3116);
    let n3453: ZW = zw_cellmix_n(369u64, n1116, 1542469173u64);
    let n3454: ZW = zw_cellmix_n(369u64, n1116, 668265263u64);
    let n3455: ZW = zw_add(n3451, n3453);
    let n3456: ZW = zw_add(n3452, n3454);
    let n3457: ZW = zw_cellmix_n(370u64, n1114, 1542469173u64);
    let n3458: ZW = zw_cellmix_n(370u64, n1114, 668265263u64);
    let n3459: ZW = zw_add(n3455, n3457);
    let n3460: ZW = zw_add(n3456, n3458);
    let n3461: ZW = zw_add(n3445, n3267);
    let n3462: ZW = zw_add(n3446, n3268);
    let n3463: ZW = zw_add(n3461, n3295);
    let n3464: ZW = zw_add(n3462, n3296);
    let n3465: ZW = zw_add(n3463, n3127);
    let n3466: ZW = zw_add(n3464, n3128);
    let n3467: ZW = zw_cellmix_n(369u64, n1122, 1542469173u64);
    let n3468: ZW = zw_cellmix_n(369u64, n1122, 668265263u64);
    let n3469: ZW = zw_add(n3465, n3467);
    let n3470: ZW = zw_add(n3466, n3468);
    let n3471: ZW = zw_cellmix_n(370u64, n1120, 1542469173u64);
    let n3472: ZW = zw_cellmix_n(370u64, n1120, 668265263u64);
    let n3473: ZW = zw_add(n3469, n3471);
    let n3474: ZW = zw_add(n3470, n3472);
    let n3475: ZW = zw_add(n3431, n3339);
    let n3476: ZW = zw_add(n3432, n3340);
    let n3477: ZW = zw_add(n3475, n3103);
    let n3478: ZW = zw_add(n3476, n3104);
    let n3479: ZW = zw_add(n3477, n3437);
    let n3480: ZW = zw_add(n3478, n3438);
    let n3481: ZW = zw_cellmix_n(370u64, n1124, 1542469173u64);
    let n3482: ZW = zw_cellmix_n(370u64, n1124, 668265263u64);
    let n3483: ZW = zw_add(n3479, n3481);
    let n3484: ZW = zw_add(n3480, n3482);
    let n3485: ZW = zw_add(n3447, n3339);
    let n3486: ZW = zw_add(n3448, n3340);
    let n3487: ZW = zw_add(n3485, n3115);
    let n3488: ZW = zw_add(n3486, n3116);
    let n3489: ZW = zw_add(n3487, n3453);
    let n3490: ZW = zw_add(n3488, n3454);
    let n3491: ZW = zw_cellmix_n(370u64, n1126, 1542469173u64);
    let n3492: ZW = zw_cellmix_n(370u64, n1126, 668265263u64);
    let n3493: ZW = zw_add(n3489, n3491);
    let n3494: ZW = zw_add(n3490, n3492);
    let n3495: ZW = zw_add(n3461, n3339);
    let n3496: ZW = zw_add(n3462, n3340);
    let n3497: ZW = zw_add(n3495, n3127);
    let n3498: ZW = zw_add(n3496, n3128);
    let n3499: ZW = zw_add(n3497, n3467);
    let n3500: ZW = zw_add(n3498, n3468);
    let n3501: ZW = zw_cellmix_n(370u64, n1128, 1542469173u64);
    let n3502: ZW = zw_cellmix_n(370u64, n1128, 668265263u64);
    let n3503: ZW = zw_add(n3499, n3501);
    let n3504: ZW = zw_add(n3500, n3502);
    let n3505: ZW = zw_cellmix_n(246u64, n1235, 1542469173u64);
    let n3506: ZW = zw_cellmix_n(246u64, n1235, 668265263u64);
    let n3507: ZW = zw_add(n3037, n3505);
    let n3508: ZW = zw_add(n3038, n3506);
    let n3509: ZW = zw_cellmix_n(254u64, n1240, 1542469173u64);
    let n3510: ZW = zw_cellmix_n(254u64, n1240, 668265263u64);
    let n3511: ZW = zw_add(n3507, n3509);
    let n3512: ZW = zw_add(n3508, n3510);
    let n3513: ZW = zw_cellmix_n(318u64, n513, 1542469173u64);
    let n3514: ZW = zw_cellmix_n(318u64, n513, 668265263u64);
    let n3515: ZW = zw_add(n3511, n3513);
    let n3516: ZW = zw_add(n3512, n3514);
    let n3517: ZW = zw_cellmix_n(405u64, n402, 1542469173u64);
    let n3518: ZW = zw_cellmix_n(405u64, n402, 668265263u64);
    let n3519: ZW = zw_add(n3515, n3517);
    let n3520: ZW = zw_add(n3516, n3518);
    let n3521: ZW = zw_cellmix_n(406u64, n514, 1542469173u64);
    let n3522: ZW = zw_cellmix_n(406u64, n514, 668265263u64);
    let n3523: ZW = zw_add(n3519, n3521);
    let n3524: ZW = zw_add(n3520, n3522);
    let n3525: ZW = zw_add(n3523, n3051);
    let n3526: ZW = zw_add(n3524, n3052);
    let n3527: ZW = zw_add(n3525, n3055);
    let n3528: ZW = zw_add(n3526, n3056);
    let n3529: ZW = zw_cellmix_n(298u64, n259, 1542469173u64);
    let n3530: ZW = zw_cellmix_n(298u64, n259, 668265263u64);
    let n3531: ZW = zw_add(n3527, n3529);
    let n3532: ZW = zw_add(n3528, n3530);
    let n3533: ZW = zw_cellmix_n(300u64, n262, 1542469173u64);
    let n3534: ZW = zw_cellmix_n(300u64, n262, 668265263u64);
    let n3535: ZW = zw_add(n3531, n3533);
    let n3536: ZW = zw_add(n3532, n3534);
    let n3537: ZW = zw_cellmix_n(301u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n3538: ZW = zw_cellmix_n(301u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n3539: ZW = zw_add(n3535, n3537);
    let n3540: ZW = zw_add(n3536, n3538);
    let n3541: ZW = zw_cellmix_n(303u64, n845, 1542469173u64);
    let n3542: ZW = zw_cellmix_n(303u64, n845, 668265263u64);
    let n3543: ZW = zw_add(n3539, n3541);
    let n3544: ZW = zw_add(n3540, n3542);
    let n3545: ZW = zw_cellmix_b(310u64, zb_splat(false), 1542469173u64);
    let n3546: ZW = zw_cellmix_b(310u64, zb_splat(false), 668265263u64);
    let n3547: ZW = zw_add(n3543, n3545);
    let n3548: ZW = zw_add(n3544, n3546);
    let n3549: ZW = zw_cellmix_b(311u64, zb_splat(false), 1542469173u64);
    let n3550: ZW = zw_cellmix_b(311u64, zb_splat(false), 668265263u64);
    let n3551: ZW = zw_add(n3547, n3549);
    let n3552: ZW = zw_add(n3548, n3550);
    let n3553: ZW = zw_cellmix_n(317u64, n930, 1542469173u64);
    let n3554: ZW = zw_cellmix_n(317u64, n930, 668265263u64);
    let n3555: ZW = zw_add(n3551, n3553);
    let n3556: ZW = zw_add(n3552, n3554);
    let n3557: ZW = zw_cellmix_n(395u64, r_c394, 1542469173u64);
    let n3558: ZW = zw_cellmix_n(395u64, r_c394, 668265263u64);
    let n3559: ZW = zw_add(n3555, n3557);
    let n3560: ZW = zw_add(n3556, n3558);
    let n3561: ZW = zw_cellmix_n(396u64, r_c395, 1542469173u64);
    let n3562: ZW = zw_cellmix_n(396u64, r_c395, 668265263u64);
    let n3563: ZW = zw_add(n3559, n3561);
    let n3564: ZW = zw_add(n3560, n3562);
    let n3565: ZW = zw_cellmix_n(397u64, r_c396, 1542469173u64);
    let n3566: ZW = zw_cellmix_n(397u64, r_c396, 668265263u64);
    let n3567: ZW = zw_add(n3563, n3565);
    let n3568: ZW = zw_add(n3564, n3566);
    let n3569: ZW = zw_cellmix_n(398u64, r_c397, 1542469173u64);
    let n3570: ZW = zw_cellmix_n(398u64, r_c397, 668265263u64);
    let n3571: ZW = zw_add(n3567, n3569);
    let n3572: ZW = zw_add(n3568, n3570);
    let n3573: ZW = zw_cellmix_b(399u64, n919, 1542469173u64);
    let n3574: ZW = zw_cellmix_b(399u64, n919, 668265263u64);
    let n3575: ZW = zw_add(n3571, n3573);
    let n3576: ZW = zw_add(n3572, n3574);
    let n3577: ZW = zw_cellmix_n(407u64, n931, 1542469173u64);
    let n3578: ZW = zw_cellmix_n(407u64, n931, 668265263u64);
    let n3579: ZW = zw_add(n3575, n3577);
    let n3580: ZW = zw_add(n3576, n3578);
    let n3581: ZW = zw_cellmix_n(408u64, n921, 1542469173u64);
    let n3582: ZW = zw_cellmix_n(408u64, n921, 668265263u64);
    let n3583: ZW = zw_add(n3579, n3581);
    let n3584: ZW = zw_add(n3580, n3582);
    let n3585: ZW = zw_cellmix_b(399u64, n961, 1542469173u64);
    let n3586: ZW = zw_cellmix_b(399u64, n961, 668265263u64);
    let n3587: ZW = zw_add(n3571, n3585);
    let n3588: ZW = zw_add(n3572, n3586);
    let n3589: ZW = zw_cellmix_n(407u64, n965, 1542469173u64);
    let n3590: ZW = zw_cellmix_n(407u64, n965, 668265263u64);
    let n3591: ZW = zw_add(n3587, n3589);
    let n3592: ZW = zw_add(n3588, n3590);
    let n3593: ZW = zw_cellmix_n(408u64, n963, 1542469173u64);
    let n3594: ZW = zw_cellmix_n(408u64, n963, 668265263u64);
    let n3595: ZW = zw_add(n3591, n3593);
    let n3596: ZW = zw_add(n3592, n3594);
    let n3597: ZW = zw_cellmix_b(399u64, n983, 1542469173u64);
    let n3598: ZW = zw_cellmix_b(399u64, n983, 668265263u64);
    let n3599: ZW = zw_add(n3571, n3597);
    let n3600: ZW = zw_add(n3572, n3598);
    let n3601: ZW = zw_cellmix_n(407u64, n987, 1542469173u64);
    let n3602: ZW = zw_cellmix_n(407u64, n987, 668265263u64);
    let n3603: ZW = zw_add(n3599, n3601);
    let n3604: ZW = zw_add(n3600, n3602);
    let n3605: ZW = zw_cellmix_n(408u64, n985, 1542469173u64);
    let n3606: ZW = zw_cellmix_n(408u64, n985, 668265263u64);
    let n3607: ZW = zw_add(n3603, n3605);
    let n3608: ZW = zw_add(n3604, n3606);
    let n3609: ZW = zw_cellmix_n(303u64, n991, 1542469173u64);
    let n3610: ZW = zw_cellmix_n(303u64, n991, 668265263u64);
    let n3611: ZW = zw_add(n3539, n3609);
    let n3612: ZW = zw_add(n3540, n3610);
    let n3613: ZW = zw_add(n3611, n3545);
    let n3614: ZW = zw_add(n3612, n3546);
    let n3615: ZW = zw_cellmix_b(311u64, zb_splat(true), 1542469173u64);
    let n3616: ZW = zw_cellmix_b(311u64, zb_splat(true), 668265263u64);
    let n3617: ZW = zw_add(n3613, n3615);
    let n3618: ZW = zw_add(n3614, n3616);
    let n3619: ZW = zw_add(n3617, n3553);
    let n3620: ZW = zw_add(n3618, n3554);
    let n3621: ZW = zw_add(n3619, n3557);
    let n3622: ZW = zw_add(n3620, n3558);
    let n3623: ZW = zw_add(n3621, n3561);
    let n3624: ZW = zw_add(n3622, n3562);
    let n3625: ZW = zw_add(n3623, n3565);
    let n3626: ZW = zw_add(n3624, n3566);
    let n3627: ZW = zw_add(n3625, n3569);
    let n3628: ZW = zw_add(n3626, n3570);
    let n3629: ZW = zw_add(n3627, n3573);
    let n3630: ZW = zw_add(n3628, n3574);
    let n3631: ZW = zw_cellmix_n(407u64, n995, 1542469173u64);
    let n3632: ZW = zw_cellmix_n(407u64, n995, 668265263u64);
    let n3633: ZW = zw_add(n3629, n3631);
    let n3634: ZW = zw_add(n3630, n3632);
    let n3635: ZW = zw_cellmix_n(408u64, n993, 1542469173u64);
    let n3636: ZW = zw_cellmix_n(408u64, n993, 668265263u64);
    let n3637: ZW = zw_add(n3633, n3635);
    let n3638: ZW = zw_add(n3634, n3636);
    let n3639: ZW = zw_add(n3627, n3585);
    let n3640: ZW = zw_add(n3628, n3586);
    let n3641: ZW = zw_cellmix_n(407u64, n1001, 1542469173u64);
    let n3642: ZW = zw_cellmix_n(407u64, n1001, 668265263u64);
    let n3643: ZW = zw_add(n3639, n3641);
    let n3644: ZW = zw_add(n3640, n3642);
    let n3645: ZW = zw_cellmix_n(408u64, n999, 1542469173u64);
    let n3646: ZW = zw_cellmix_n(408u64, n999, 668265263u64);
    let n3647: ZW = zw_add(n3643, n3645);
    let n3648: ZW = zw_add(n3644, n3646);
    let n3649: ZW = zw_add(n3627, n3597);
    let n3650: ZW = zw_add(n3628, n3598);
    let n3651: ZW = zw_cellmix_n(407u64, n1007, 1542469173u64);
    let n3652: ZW = zw_cellmix_n(407u64, n1007, 668265263u64);
    let n3653: ZW = zw_add(n3649, n3651);
    let n3654: ZW = zw_add(n3650, n3652);
    let n3655: ZW = zw_cellmix_n(408u64, n1005, 1542469173u64);
    let n3656: ZW = zw_cellmix_n(408u64, n1005, 668265263u64);
    let n3657: ZW = zw_add(n3653, n3655);
    let n3658: ZW = zw_add(n3654, n3656);
    let n3659: ZW = zw_add(n3523, n3189);
    let n3660: ZW = zw_add(n3524, n3190);
    let n3661: ZW = zw_add(n3659, n3193);
    let n3662: ZW = zw_add(n3660, n3194);
    let n3663: ZW = zw_cellmix_n(298u64, n1017, 1542469173u64);
    let n3664: ZW = zw_cellmix_n(298u64, n1017, 668265263u64);
    let n3665: ZW = zw_add(n3661, n3663);
    let n3666: ZW = zw_add(n3662, n3664);
    let n3667: ZW = zw_cellmix_n(300u64, n1018, 1542469173u64);
    let n3668: ZW = zw_cellmix_n(300u64, n1018, 668265263u64);
    let n3669: ZW = zw_add(n3665, n3667);
    let n3670: ZW = zw_add(n3666, n3668);
    let n3671: ZW = zw_cellmix_n(301u64, n1019, 1542469173u64);
    let n3672: ZW = zw_cellmix_n(301u64, n1019, 668265263u64);
    let n3673: ZW = zw_add(n3669, n3671);
    let n3674: ZW = zw_add(n3670, n3672);
    let n3675: ZW = zw_add(n3673, n3541);
    let n3676: ZW = zw_add(n3674, n3542);
    let n3677: ZW = zw_cellmix_b(310u64, zb_splat(true), 1542469173u64);
    let n3678: ZW = zw_cellmix_b(310u64, zb_splat(true), 668265263u64);
    let n3679: ZW = zw_add(n3675, n3677);
    let n3680: ZW = zw_add(n3676, n3678);
    let n3681: ZW = zw_add(n3679, n3549);
    let n3682: ZW = zw_add(n3680, n3550);
    let n3683: ZW = zw_cellmix_n(317u64, n1032, 1542469173u64);
    let n3684: ZW = zw_cellmix_n(317u64, n1032, 668265263u64);
    let n3685: ZW = zw_add(n3681, n3683);
    let n3686: ZW = zw_add(n3682, n3684);
    let n3687: ZW = zw_cellmix_n(395u64, n1020, 1542469173u64);
    let n3688: ZW = zw_cellmix_n(395u64, n1020, 668265263u64);
    let n3689: ZW = zw_add(n3685, n3687);
    let n3690: ZW = zw_add(n3686, n3688);
    let n3691: ZW = zw_cellmix_n(396u64, n1027, 1542469173u64);
    let n3692: ZW = zw_cellmix_n(396u64, n1027, 668265263u64);
    let n3693: ZW = zw_add(n3689, n3691);
    let n3694: ZW = zw_add(n3690, n3692);
    let n3695: ZW = zw_cellmix_n(397u64, n1028, 1542469173u64);
    let n3696: ZW = zw_cellmix_n(397u64, n1028, 668265263u64);
    let n3697: ZW = zw_add(n3693, n3695);
    let n3698: ZW = zw_add(n3694, n3696);
    let n3699: ZW = zw_cellmix_n(398u64, n1021, 1542469173u64);
    let n3700: ZW = zw_cellmix_n(398u64, n1021, 668265263u64);
    let n3701: ZW = zw_add(n3697, n3699);
    let n3702: ZW = zw_add(n3698, n3700);
    let n3703: ZW = zw_add(n3701, n3573);
    let n3704: ZW = zw_add(n3702, n3574);
    let n3705: ZW = zw_cellmix_n(407u64, n1033, 1542469173u64);
    let n3706: ZW = zw_cellmix_n(407u64, n1033, 668265263u64);
    let n3707: ZW = zw_add(n3703, n3705);
    let n3708: ZW = zw_add(n3704, n3706);
    let n3709: ZW = zw_cellmix_n(408u64, n1030, 1542469173u64);
    let n3710: ZW = zw_cellmix_n(408u64, n1030, 668265263u64);
    let n3711: ZW = zw_add(n3707, n3709);
    let n3712: ZW = zw_add(n3708, n3710);
    let n3713: ZW = zw_cellmix_n(396u64, n1038, 1542469173u64);
    let n3714: ZW = zw_cellmix_n(396u64, n1038, 668265263u64);
    let n3715: ZW = zw_add(n3689, n3713);
    let n3716: ZW = zw_add(n3690, n3714);
    let n3717: ZW = zw_cellmix_n(397u64, n1039, 1542469173u64);
    let n3718: ZW = zw_cellmix_n(397u64, n1039, 668265263u64);
    let n3719: ZW = zw_add(n3715, n3717);
    let n3720: ZW = zw_add(n3716, n3718);
    let n3721: ZW = zw_add(n3719, n3699);
    let n3722: ZW = zw_add(n3720, n3700);
    let n3723: ZW = zw_add(n3721, n3585);
    let n3724: ZW = zw_add(n3722, n3586);
    let n3725: ZW = zw_cellmix_n(407u64, n1043, 1542469173u64);
    let n3726: ZW = zw_cellmix_n(407u64, n1043, 668265263u64);
    let n3727: ZW = zw_add(n3723, n3725);
    let n3728: ZW = zw_add(n3724, n3726);
    let n3729: ZW = zw_cellmix_n(408u64, n1041, 1542469173u64);
    let n3730: ZW = zw_cellmix_n(408u64, n1041, 668265263u64);
    let n3731: ZW = zw_add(n3727, n3729);
    let n3732: ZW = zw_add(n3728, n3730);
    let n3733: ZW = zw_cellmix_n(397u64, n1047, 1542469173u64);
    let n3734: ZW = zw_cellmix_n(397u64, n1047, 668265263u64);
    let n3735: ZW = zw_add(n3715, n3733);
    let n3736: ZW = zw_add(n3716, n3734);
    let n3737: ZW = zw_add(n3735, n3699);
    let n3738: ZW = zw_add(n3736, n3700);
    let n3739: ZW = zw_add(n3737, n3597);
    let n3740: ZW = zw_add(n3738, n3598);
    let n3741: ZW = zw_cellmix_n(407u64, n1051, 1542469173u64);
    let n3742: ZW = zw_cellmix_n(407u64, n1051, 668265263u64);
    let n3743: ZW = zw_add(n3739, n3741);
    let n3744: ZW = zw_add(n3740, n3742);
    let n3745: ZW = zw_cellmix_n(408u64, n1049, 1542469173u64);
    let n3746: ZW = zw_cellmix_n(408u64, n1049, 668265263u64);
    let n3747: ZW = zw_add(n3743, n3745);
    let n3748: ZW = zw_add(n3744, n3746);
    let n3749: ZW = zw_cellmix_n(395u64, n1055, 1542469173u64);
    let n3750: ZW = zw_cellmix_n(395u64, n1055, 668265263u64);
    let n3751: ZW = zw_add(n3685, n3749);
    let n3752: ZW = zw_add(n3686, n3750);
    let n3753: ZW = zw_cellmix_n(396u64, n1061, 1542469173u64);
    let n3754: ZW = zw_cellmix_n(396u64, n1061, 668265263u64);
    let n3755: ZW = zw_add(n3751, n3753);
    let n3756: ZW = zw_add(n3752, n3754);
    let n3757: ZW = zw_cellmix_n(397u64, n1062, 1542469173u64);
    let n3758: ZW = zw_cellmix_n(397u64, n1062, 668265263u64);
    let n3759: ZW = zw_add(n3755, n3757);
    let n3760: ZW = zw_add(n3756, n3758);
    let n3761: ZW = zw_cellmix_n(398u64, n1056, 1542469173u64);
    let n3762: ZW = zw_cellmix_n(398u64, n1056, 668265263u64);
    let n3763: ZW = zw_add(n3759, n3761);
    let n3764: ZW = zw_add(n3760, n3762);
    let n3765: ZW = zw_add(n3763, n3573);
    let n3766: ZW = zw_add(n3764, n3574);
    let n3767: ZW = zw_cellmix_n(407u64, n1066, 1542469173u64);
    let n3768: ZW = zw_cellmix_n(407u64, n1066, 668265263u64);
    let n3769: ZW = zw_add(n3765, n3767);
    let n3770: ZW = zw_add(n3766, n3768);
    let n3771: ZW = zw_cellmix_n(408u64, n1064, 1542469173u64);
    let n3772: ZW = zw_cellmix_n(408u64, n1064, 668265263u64);
    let n3773: ZW = zw_add(n3769, n3771);
    let n3774: ZW = zw_add(n3770, n3772);
    let n3775: ZW = zw_add(n3751, n3713);
    let n3776: ZW = zw_add(n3752, n3714);
    let n3777: ZW = zw_add(n3775, n3717);
    let n3778: ZW = zw_add(n3776, n3718);
    let n3779: ZW = zw_add(n3777, n3761);
    let n3780: ZW = zw_add(n3778, n3762);
    let n3781: ZW = zw_add(n3779, n3585);
    let n3782: ZW = zw_add(n3780, n3586);
    let n3783: ZW = zw_cellmix_n(407u64, n1072, 1542469173u64);
    let n3784: ZW = zw_cellmix_n(407u64, n1072, 668265263u64);
    let n3785: ZW = zw_add(n3781, n3783);
    let n3786: ZW = zw_add(n3782, n3784);
    let n3787: ZW = zw_cellmix_n(408u64, n1070, 1542469173u64);
    let n3788: ZW = zw_cellmix_n(408u64, n1070, 668265263u64);
    let n3789: ZW = zw_add(n3785, n3787);
    let n3790: ZW = zw_add(n3786, n3788);
    let n3791: ZW = zw_add(n3775, n3733);
    let n3792: ZW = zw_add(n3776, n3734);
    let n3793: ZW = zw_add(n3791, n3761);
    let n3794: ZW = zw_add(n3792, n3762);
    let n3795: ZW = zw_add(n3793, n3597);
    let n3796: ZW = zw_add(n3794, n3598);
    let n3797: ZW = zw_cellmix_n(407u64, n1078, 1542469173u64);
    let n3798: ZW = zw_cellmix_n(407u64, n1078, 668265263u64);
    let n3799: ZW = zw_add(n3795, n3797);
    let n3800: ZW = zw_add(n3796, n3798);
    let n3801: ZW = zw_cellmix_n(408u64, n1076, 1542469173u64);
    let n3802: ZW = zw_cellmix_n(408u64, n1076, 668265263u64);
    let n3803: ZW = zw_add(n3799, n3801);
    let n3804: ZW = zw_add(n3800, n3802);
    let n3805: ZW = zw_cellmix_n(398u64, n1080, 1542469173u64);
    let n3806: ZW = zw_cellmix_n(398u64, n1080, 668265263u64);
    let n3807: ZW = zw_add(n3759, n3805);
    let n3808: ZW = zw_add(n3760, n3806);
    let n3809: ZW = zw_add(n3807, n3573);
    let n3810: ZW = zw_add(n3808, n3574);
    let n3811: ZW = zw_add(n3809, n3767);
    let n3812: ZW = zw_add(n3810, n3768);
    let n3813: ZW = zw_cellmix_n(408u64, n1082, 1542469173u64);
    let n3814: ZW = zw_cellmix_n(408u64, n1082, 668265263u64);
    let n3815: ZW = zw_add(n3811, n3813);
    let n3816: ZW = zw_add(n3812, n3814);
    let n3817: ZW = zw_add(n3777, n3805);
    let n3818: ZW = zw_add(n3778, n3806);
    let n3819: ZW = zw_add(n3817, n3585);
    let n3820: ZW = zw_add(n3818, n3586);
    let n3821: ZW = zw_add(n3819, n3783);
    let n3822: ZW = zw_add(n3820, n3784);
    let n3823: ZW = zw_cellmix_n(408u64, n1084, 1542469173u64);
    let n3824: ZW = zw_cellmix_n(408u64, n1084, 668265263u64);
    let n3825: ZW = zw_add(n3821, n3823);
    let n3826: ZW = zw_add(n3822, n3824);
    let n3827: ZW = zw_add(n3791, n3805);
    let n3828: ZW = zw_add(n3792, n3806);
    let n3829: ZW = zw_add(n3827, n3597);
    let n3830: ZW = zw_add(n3828, n3598);
    let n3831: ZW = zw_add(n3829, n3797);
    let n3832: ZW = zw_add(n3830, n3798);
    let n3833: ZW = zw_cellmix_n(408u64, n1086, 1542469173u64);
    let n3834: ZW = zw_cellmix_n(408u64, n1086, 668265263u64);
    let n3835: ZW = zw_add(n3831, n3833);
    let n3836: ZW = zw_add(n3832, n3834);
    let n3837: ZW = zw_add(n3673, n3609);
    let n3838: ZW = zw_add(n3674, n3610);
    let n3839: ZW = zw_add(n3837, n3677);
    let n3840: ZW = zw_add(n3838, n3678);
    let n3841: ZW = zw_add(n3839, n3615);
    let n3842: ZW = zw_add(n3840, n3616);
    let n3843: ZW = zw_add(n3841, n3683);
    let n3844: ZW = zw_add(n3842, n3684);
    let n3845: ZW = zw_add(n3843, n3687);
    let n3846: ZW = zw_add(n3844, n3688);
    let n3847: ZW = zw_add(n3845, n3691);
    let n3848: ZW = zw_add(n3846, n3692);
    let n3849: ZW = zw_add(n3847, n3695);
    let n3850: ZW = zw_add(n3848, n3696);
    let n3851: ZW = zw_add(n3849, n3699);
    let n3852: ZW = zw_add(n3850, n3700);
    let n3853: ZW = zw_add(n3851, n3573);
    let n3854: ZW = zw_add(n3852, n3574);
    let n3855: ZW = zw_cellmix_n(407u64, n1092, 1542469173u64);
    let n3856: ZW = zw_cellmix_n(407u64, n1092, 668265263u64);
    let n3857: ZW = zw_add(n3853, n3855);
    let n3858: ZW = zw_add(n3854, n3856);
    let n3859: ZW = zw_cellmix_n(408u64, n1090, 1542469173u64);
    let n3860: ZW = zw_cellmix_n(408u64, n1090, 668265263u64);
    let n3861: ZW = zw_add(n3857, n3859);
    let n3862: ZW = zw_add(n3858, n3860);
    let n3863: ZW = zw_add(n3845, n3713);
    let n3864: ZW = zw_add(n3846, n3714);
    let n3865: ZW = zw_add(n3863, n3717);
    let n3866: ZW = zw_add(n3864, n3718);
    let n3867: ZW = zw_add(n3865, n3699);
    let n3868: ZW = zw_add(n3866, n3700);
    let n3869: ZW = zw_add(n3867, n3585);
    let n3870: ZW = zw_add(n3868, n3586);
    let n3871: ZW = zw_cellmix_n(407u64, n1098, 1542469173u64);
    let n3872: ZW = zw_cellmix_n(407u64, n1098, 668265263u64);
    let n3873: ZW = zw_add(n3869, n3871);
    let n3874: ZW = zw_add(n3870, n3872);
    let n3875: ZW = zw_cellmix_n(408u64, n1096, 1542469173u64);
    let n3876: ZW = zw_cellmix_n(408u64, n1096, 668265263u64);
    let n3877: ZW = zw_add(n3873, n3875);
    let n3878: ZW = zw_add(n3874, n3876);
    let n3879: ZW = zw_add(n3863, n3733);
    let n3880: ZW = zw_add(n3864, n3734);
    let n3881: ZW = zw_add(n3879, n3699);
    let n3882: ZW = zw_add(n3880, n3700);
    let n3883: ZW = zw_add(n3881, n3597);
    let n3884: ZW = zw_add(n3882, n3598);
    let n3885: ZW = zw_cellmix_n(407u64, n1104, 1542469173u64);
    let n3886: ZW = zw_cellmix_n(407u64, n1104, 668265263u64);
    let n3887: ZW = zw_add(n3883, n3885);
    let n3888: ZW = zw_add(n3884, n3886);
    let n3889: ZW = zw_cellmix_n(408u64, n1102, 1542469173u64);
    let n3890: ZW = zw_cellmix_n(408u64, n1102, 668265263u64);
    let n3891: ZW = zw_add(n3887, n3889);
    let n3892: ZW = zw_add(n3888, n3890);
    let n3893: ZW = zw_add(n3843, n3749);
    let n3894: ZW = zw_add(n3844, n3750);
    let n3895: ZW = zw_add(n3893, n3753);
    let n3896: ZW = zw_add(n3894, n3754);
    let n3897: ZW = zw_add(n3895, n3757);
    let n3898: ZW = zw_add(n3896, n3758);
    let n3899: ZW = zw_add(n3897, n3761);
    let n3900: ZW = zw_add(n3898, n3762);
    let n3901: ZW = zw_add(n3899, n3573);
    let n3902: ZW = zw_add(n3900, n3574);
    let n3903: ZW = zw_cellmix_n(407u64, n1110, 1542469173u64);
    let n3904: ZW = zw_cellmix_n(407u64, n1110, 668265263u64);
    let n3905: ZW = zw_add(n3901, n3903);
    let n3906: ZW = zw_add(n3902, n3904);
    let n3907: ZW = zw_cellmix_n(408u64, n1108, 1542469173u64);
    let n3908: ZW = zw_cellmix_n(408u64, n1108, 668265263u64);
    let n3909: ZW = zw_add(n3905, n3907);
    let n3910: ZW = zw_add(n3906, n3908);
    let n3911: ZW = zw_add(n3893, n3713);
    let n3912: ZW = zw_add(n3894, n3714);
    let n3913: ZW = zw_add(n3911, n3717);
    let n3914: ZW = zw_add(n3912, n3718);
    let n3915: ZW = zw_add(n3913, n3761);
    let n3916: ZW = zw_add(n3914, n3762);
    let n3917: ZW = zw_add(n3915, n3585);
    let n3918: ZW = zw_add(n3916, n3586);
    let n3919: ZW = zw_cellmix_n(407u64, n1116, 1542469173u64);
    let n3920: ZW = zw_cellmix_n(407u64, n1116, 668265263u64);
    let n3921: ZW = zw_add(n3917, n3919);
    let n3922: ZW = zw_add(n3918, n3920);
    let n3923: ZW = zw_cellmix_n(408u64, n1114, 1542469173u64);
    let n3924: ZW = zw_cellmix_n(408u64, n1114, 668265263u64);
    let n3925: ZW = zw_add(n3921, n3923);
    let n3926: ZW = zw_add(n3922, n3924);
    let n3927: ZW = zw_add(n3911, n3733);
    let n3928: ZW = zw_add(n3912, n3734);
    let n3929: ZW = zw_add(n3927, n3761);
    let n3930: ZW = zw_add(n3928, n3762);
    let n3931: ZW = zw_add(n3929, n3597);
    let n3932: ZW = zw_add(n3930, n3598);
    let n3933: ZW = zw_cellmix_n(407u64, n1122, 1542469173u64);
    let n3934: ZW = zw_cellmix_n(407u64, n1122, 668265263u64);
    let n3935: ZW = zw_add(n3931, n3933);
    let n3936: ZW = zw_add(n3932, n3934);
    let n3937: ZW = zw_cellmix_n(408u64, n1120, 1542469173u64);
    let n3938: ZW = zw_cellmix_n(408u64, n1120, 668265263u64);
    let n3939: ZW = zw_add(n3935, n3937);
    let n3940: ZW = zw_add(n3936, n3938);
    let n3941: ZW = zw_add(n3897, n3805);
    let n3942: ZW = zw_add(n3898, n3806);
    let n3943: ZW = zw_add(n3941, n3573);
    let n3944: ZW = zw_add(n3942, n3574);
    let n3945: ZW = zw_add(n3943, n3903);
    let n3946: ZW = zw_add(n3944, n3904);
    let n3947: ZW = zw_cellmix_n(408u64, n1124, 1542469173u64);
    let n3948: ZW = zw_cellmix_n(408u64, n1124, 668265263u64);
    let n3949: ZW = zw_add(n3945, n3947);
    let n3950: ZW = zw_add(n3946, n3948);
    let n3951: ZW = zw_add(n3913, n3805);
    let n3952: ZW = zw_add(n3914, n3806);
    let n3953: ZW = zw_add(n3951, n3585);
    let n3954: ZW = zw_add(n3952, n3586);
    let n3955: ZW = zw_add(n3953, n3919);
    let n3956: ZW = zw_add(n3954, n3920);
    let n3957: ZW = zw_cellmix_n(408u64, n1126, 1542469173u64);
    let n3958: ZW = zw_cellmix_n(408u64, n1126, 668265263u64);
    let n3959: ZW = zw_add(n3955, n3957);
    let n3960: ZW = zw_add(n3956, n3958);
    let n3961: ZW = zw_add(n3927, n3805);
    let n3962: ZW = zw_add(n3928, n3806);
    let n3963: ZW = zw_add(n3961, n3597);
    let n3964: ZW = zw_add(n3962, n3598);
    let n3965: ZW = zw_add(n3963, n3933);
    let n3966: ZW = zw_add(n3964, n3934);
    let n3967: ZW = zw_cellmix_n(408u64, n1128, 1542469173u64);
    let n3968: ZW = zw_cellmix_n(408u64, n1128, 668265263u64);
    let n3969: ZW = zw_add(n3965, n3967);
    let n3970: ZW = zw_add(n3966, n3968);
    let n3971: ZW = zw_add(zw_splat(0u64), n3023);
    let n3972: ZW = zw_add(zw_splat(0u64), n3024);
    let n3973: ZW = zw_add(n3971, n3027);
    let n3974: ZW = zw_add(n3972, n3028);
    let n3975: ZW = zw_add(n3973, n3031);
    let n3976: ZW = zw_add(n3974, n3032);
    let n3977: ZW = zw_cellmix_n(87u64, n1484, 1542469173u64);
    let n3978: ZW = zw_cellmix_n(87u64, n1484, 668265263u64);
    let n3979: ZW = zw_add(n3975, n3977);
    let n3980: ZW = zw_add(n3976, n3978);
    let n3981: ZW = zw_add(n3979, n3051);
    let n3982: ZW = zw_add(n3980, n3052);
    let n3983: ZW = zw_add(n3981, n3055);
    let n3984: ZW = zw_add(n3982, n3056);
    let n3985: ZW = zw_add(n3979, n3189);
    let n3986: ZW = zw_add(n3980, n3190);
    let n3987: ZW = zw_add(n3985, n3193);
    let n3988: ZW = zw_add(n3986, n3194);
    let n3989: ZW = zw_cellmix_n(245u64, n1235, 1542469173u64);
    let n3990: ZW = zw_cellmix_n(245u64, n1235, 668265263u64);
    let n3991: ZW = zw_add(n3979, n3989);
    let n3992: ZW = zw_add(n3980, n3990);
    let n3993: ZW = zw_cellmix_n(253u64, n1240, 1542469173u64);
    let n3994: ZW = zw_cellmix_n(253u64, n1240, 668265263u64);
    let n3995: ZW = zw_add(n3991, n3993);
    let n3996: ZW = zw_add(n3992, n3994);
    let n3997: ZW = zw_add(n3995, n3051);
    let n3998: ZW = zw_add(n3996, n3052);
    let n3999: ZW = zw_add(n3997, n3055);
    let n4000: ZW = zw_add(n3998, n3056);
    let n4001: ZW = zw_add(n3995, n3189);
    let n4002: ZW = zw_add(n3996, n3190);
    let n4003: ZW = zw_add(n4001, n3193);
    let n4004: ZW = zw_add(n4002, n3194);
    let n4005: ZW = zw_cellmix_n(300u64, n1918, 1542469173u64);
    let n4006: ZW = zw_cellmix_n(300u64, n1918, 668265263u64);
    let n4007: ZW = zw_add(n3037, n4005);
    let n4008: ZW = zw_add(n3038, n4006);
    let n4009: ZW = zw_cellmix_n(366u64, n1919, 1542469173u64);
    let n4010: ZW = zw_cellmix_n(366u64, n1919, 668265263u64);
    let n4011: ZW = zw_add(n4007, n4009);
    let n4012: ZW = zw_add(n4008, n4010);
    let n4013: ZW = zw_cellmix_n(367u64, n1920, 1542469173u64);
    let n4014: ZW = zw_cellmix_n(367u64, n1920, 668265263u64);
    let n4015: ZW = zw_add(n4011, n4013);
    let n4016: ZW = zw_add(n4012, n4014);
    let n4017: ZW = zw_add(n4015, n3051);
    let n4018: ZW = zw_add(n4016, n3052);
    let n4019: ZW = zw_add(n4017, n3055);
    let n4020: ZW = zw_add(n4018, n3056);
    let n4021: ZW = zw_cellmix_n(280u64, n259, 1542469173u64);
    let n4022: ZW = zw_cellmix_n(280u64, n259, 668265263u64);
    let n4023: ZW = zw_add(n4019, n4021);
    let n4024: ZW = zw_add(n4020, n4022);
    let n4025: ZW = zw_cellmix_n(282u64, n262, 1542469173u64);
    let n4026: ZW = zw_cellmix_n(282u64, n262, 668265263u64);
    let n4027: ZW = zw_add(n4023, n4025);
    let n4028: ZW = zw_add(n4024, n4026);
    let n4029: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n4030: ZW = zw_cellmix_n(283u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n4031: ZW = zw_add(n4027, n4029);
    let n4032: ZW = zw_add(n4028, n4030);
    let n4033: ZW = zw_cellmix_n(285u64, n2257, 1542469173u64);
    let n4034: ZW = zw_cellmix_n(285u64, n2257, 668265263u64);
    let n4035: ZW = zw_add(n4031, n4033);
    let n4036: ZW = zw_add(n4032, n4034);
    let n4037: ZW = zw_cellmix_b(292u64, zb_splat(false), 1542469173u64);
    let n4038: ZW = zw_cellmix_b(292u64, zb_splat(false), 668265263u64);
    let n4039: ZW = zw_add(n4035, n4037);
    let n4040: ZW = zw_add(n4036, n4038);
    let n4041: ZW = zw_add(n4039, n3075);
    let n4042: ZW = zw_add(n4040, n3076);
    let n4043: ZW = zw_cellmix_n(299u64, n2351, 1542469173u64);
    let n4044: ZW = zw_cellmix_n(299u64, n2351, 668265263u64);
    let n4045: ZW = zw_add(n4041, n4043);
    let n4046: ZW = zw_add(n4042, n4044);
    let n4047: ZW = zw_cellmix_n(356u64, r_c394, 1542469173u64);
    let n4048: ZW = zw_cellmix_n(356u64, r_c394, 668265263u64);
    let n4049: ZW = zw_add(n4045, n4047);
    let n4050: ZW = zw_add(n4046, n4048);
    let n4051: ZW = zw_cellmix_n(357u64, r_c395, 1542469173u64);
    let n4052: ZW = zw_cellmix_n(357u64, r_c395, 668265263u64);
    let n4053: ZW = zw_add(n4049, n4051);
    let n4054: ZW = zw_add(n4050, n4052);
    let n4055: ZW = zw_cellmix_n(358u64, r_c396, 1542469173u64);
    let n4056: ZW = zw_cellmix_n(358u64, r_c396, 668265263u64);
    let n4057: ZW = zw_add(n4053, n4055);
    let n4058: ZW = zw_add(n4054, n4056);
    let n4059: ZW = zw_cellmix_n(359u64, r_c397, 1542469173u64);
    let n4060: ZW = zw_cellmix_n(359u64, r_c397, 668265263u64);
    let n4061: ZW = zw_add(n4057, n4059);
    let n4062: ZW = zw_add(n4058, n4060);
    let n4063: ZW = zw_cellmix_b(360u64, n2335, 1542469173u64);
    let n4064: ZW = zw_cellmix_b(360u64, n2335, 668265263u64);
    let n4065: ZW = zw_add(n4061, n4063);
    let n4066: ZW = zw_add(n4062, n4064);
    let n4067: ZW = zw_cellmix_n(368u64, n2352, 1542469173u64);
    let n4068: ZW = zw_cellmix_n(368u64, n2352, 668265263u64);
    let n4069: ZW = zw_add(n4065, n4067);
    let n4070: ZW = zw_add(n4066, n4068);
    let n4071: ZW = zw_cellmix_n(369u64, n2337, 1542469173u64);
    let n4072: ZW = zw_cellmix_n(369u64, n2337, 668265263u64);
    let n4073: ZW = zw_add(n4069, n4071);
    let n4074: ZW = zw_add(n4070, n4072);
    let n4075: ZW = zw_cellmix_b(360u64, n2374, 1542469173u64);
    let n4076: ZW = zw_cellmix_b(360u64, n2374, 668265263u64);
    let n4077: ZW = zw_add(n4061, n4075);
    let n4078: ZW = zw_add(n4062, n4076);
    let n4079: ZW = zw_cellmix_n(368u64, n2378, 1542469173u64);
    let n4080: ZW = zw_cellmix_n(368u64, n2378, 668265263u64);
    let n4081: ZW = zw_add(n4077, n4079);
    let n4082: ZW = zw_add(n4078, n4080);
    let n4083: ZW = zw_cellmix_n(369u64, n2376, 1542469173u64);
    let n4084: ZW = zw_cellmix_n(369u64, n2376, 668265263u64);
    let n4085: ZW = zw_add(n4081, n4083);
    let n4086: ZW = zw_add(n4082, n4084);
    let n4087: ZW = zw_cellmix_b(360u64, n2399, 1542469173u64);
    let n4088: ZW = zw_cellmix_b(360u64, n2399, 668265263u64);
    let n4089: ZW = zw_add(n4061, n4087);
    let n4090: ZW = zw_add(n4062, n4088);
    let n4091: ZW = zw_cellmix_n(368u64, n2403, 1542469173u64);
    let n4092: ZW = zw_cellmix_n(368u64, n2403, 668265263u64);
    let n4093: ZW = zw_add(n4089, n4091);
    let n4094: ZW = zw_add(n4090, n4092);
    let n4095: ZW = zw_cellmix_n(369u64, n2401, 1542469173u64);
    let n4096: ZW = zw_cellmix_n(369u64, n2401, 668265263u64);
    let n4097: ZW = zw_add(n4093, n4095);
    let n4098: ZW = zw_add(n4094, n4096);
    let n4099: ZW = zw_cellmix_n(285u64, n2407, 1542469173u64);
    let n4100: ZW = zw_cellmix_n(285u64, n2407, 668265263u64);
    let n4101: ZW = zw_add(n4031, n4099);
    let n4102: ZW = zw_add(n4032, n4100);
    let n4103: ZW = zw_add(n4101, n4037);
    let n4104: ZW = zw_add(n4102, n4038);
    let n4105: ZW = zw_add(n4103, n3211);
    let n4106: ZW = zw_add(n4104, n3212);
    let n4107: ZW = zw_add(n4105, n4043);
    let n4108: ZW = zw_add(n4106, n4044);
    let n4109: ZW = zw_add(n4107, n4047);
    let n4110: ZW = zw_add(n4108, n4048);
    let n4111: ZW = zw_add(n4109, n4051);
    let n4112: ZW = zw_add(n4110, n4052);
    let n4113: ZW = zw_add(n4111, n4055);
    let n4114: ZW = zw_add(n4112, n4056);
    let n4115: ZW = zw_add(n4113, n4059);
    let n4116: ZW = zw_add(n4114, n4060);
    let n4117: ZW = zw_add(n4115, n4063);
    let n4118: ZW = zw_add(n4116, n4064);
    let n4119: ZW = zw_cellmix_n(368u64, n2413, 1542469173u64);
    let n4120: ZW = zw_cellmix_n(368u64, n2413, 668265263u64);
    let n4121: ZW = zw_add(n4117, n4119);
    let n4122: ZW = zw_add(n4118, n4120);
    let n4123: ZW = zw_cellmix_n(369u64, n2409, 1542469173u64);
    let n4124: ZW = zw_cellmix_n(369u64, n2409, 668265263u64);
    let n4125: ZW = zw_add(n4121, n4123);
    let n4126: ZW = zw_add(n4122, n4124);
    let n4127: ZW = zw_add(n4115, n4075);
    let n4128: ZW = zw_add(n4116, n4076);
    let n4129: ZW = zw_cellmix_n(368u64, n2419, 1542469173u64);
    let n4130: ZW = zw_cellmix_n(368u64, n2419, 668265263u64);
    let n4131: ZW = zw_add(n4127, n4129);
    let n4132: ZW = zw_add(n4128, n4130);
    let n4133: ZW = zw_cellmix_n(369u64, n2417, 1542469173u64);
    let n4134: ZW = zw_cellmix_n(369u64, n2417, 668265263u64);
    let n4135: ZW = zw_add(n4131, n4133);
    let n4136: ZW = zw_add(n4132, n4134);
    let n4137: ZW = zw_add(n4115, n4087);
    let n4138: ZW = zw_add(n4116, n4088);
    let n4139: ZW = zw_cellmix_n(368u64, n2425, 1542469173u64);
    let n4140: ZW = zw_cellmix_n(368u64, n2425, 668265263u64);
    let n4141: ZW = zw_add(n4137, n4139);
    let n4142: ZW = zw_add(n4138, n4140);
    let n4143: ZW = zw_cellmix_n(369u64, n2423, 1542469173u64);
    let n4144: ZW = zw_cellmix_n(369u64, n2423, 668265263u64);
    let n4145: ZW = zw_add(n4141, n4143);
    let n4146: ZW = zw_add(n4142, n4144);
    let n4147: ZW = zw_add(n4015, n3189);
    let n4148: ZW = zw_add(n4016, n3190);
    let n4149: ZW = zw_add(n4147, n3193);
    let n4150: ZW = zw_add(n4148, n3194);
    let n4151: ZW = zw_cellmix_n(280u64, n1017, 1542469173u64);
    let n4152: ZW = zw_cellmix_n(280u64, n1017, 668265263u64);
    let n4153: ZW = zw_add(n4149, n4151);
    let n4154: ZW = zw_add(n4150, n4152);
    let n4155: ZW = zw_cellmix_n(282u64, n1018, 1542469173u64);
    let n4156: ZW = zw_cellmix_n(282u64, n1018, 668265263u64);
    let n4157: ZW = zw_add(n4153, n4155);
    let n4158: ZW = zw_add(n4154, n4156);
    let n4159: ZW = zw_cellmix_n(283u64, n1019, 1542469173u64);
    let n4160: ZW = zw_cellmix_n(283u64, n1019, 668265263u64);
    let n4161: ZW = zw_add(n4157, n4159);
    let n4162: ZW = zw_add(n4158, n4160);
    let n4163: ZW = zw_add(n4161, n4033);
    let n4164: ZW = zw_add(n4162, n4034);
    let n4165: ZW = zw_cellmix_b(292u64, zb_splat(true), 1542469173u64);
    let n4166: ZW = zw_cellmix_b(292u64, zb_splat(true), 668265263u64);
    let n4167: ZW = zw_add(n4163, n4165);
    let n4168: ZW = zw_add(n4164, n4166);
    let n4169: ZW = zw_add(n4167, n3075);
    let n4170: ZW = zw_add(n4168, n3076);
    let n4171: ZW = zw_cellmix_n(299u64, n2435, 1542469173u64);
    let n4172: ZW = zw_cellmix_n(299u64, n2435, 668265263u64);
    let n4173: ZW = zw_add(n4169, n4171);
    let n4174: ZW = zw_add(n4170, n4172);
    let n4175: ZW = zw_cellmix_n(356u64, n1020, 1542469173u64);
    let n4176: ZW = zw_cellmix_n(356u64, n1020, 668265263u64);
    let n4177: ZW = zw_add(n4173, n4175);
    let n4178: ZW = zw_add(n4174, n4176);
    let n4179: ZW = zw_cellmix_n(357u64, n2430, 1542469173u64);
    let n4180: ZW = zw_cellmix_n(357u64, n2430, 668265263u64);
    let n4181: ZW = zw_add(n4177, n4179);
    let n4182: ZW = zw_add(n4178, n4180);
    let n4183: ZW = zw_cellmix_n(358u64, n2431, 1542469173u64);
    let n4184: ZW = zw_cellmix_n(358u64, n2431, 668265263u64);
    let n4185: ZW = zw_add(n4181, n4183);
    let n4186: ZW = zw_add(n4182, n4184);
    let n4187: ZW = zw_cellmix_n(359u64, n1021, 1542469173u64);
    let n4188: ZW = zw_cellmix_n(359u64, n1021, 668265263u64);
    let n4189: ZW = zw_add(n4185, n4187);
    let n4190: ZW = zw_add(n4186, n4188);
    let n4191: ZW = zw_add(n4189, n4063);
    let n4192: ZW = zw_add(n4190, n4064);
    let n4193: ZW = zw_cellmix_n(368u64, n2436, 1542469173u64);
    let n4194: ZW = zw_cellmix_n(368u64, n2436, 668265263u64);
    let n4195: ZW = zw_add(n4191, n4193);
    let n4196: ZW = zw_add(n4192, n4194);
    let n4197: ZW = zw_cellmix_n(369u64, n2433, 1542469173u64);
    let n4198: ZW = zw_cellmix_n(369u64, n2433, 668265263u64);
    let n4199: ZW = zw_add(n4195, n4197);
    let n4200: ZW = zw_add(n4196, n4198);
    let n4201: ZW = zw_cellmix_n(357u64, n1038, 1542469173u64);
    let n4202: ZW = zw_cellmix_n(357u64, n1038, 668265263u64);
    let n4203: ZW = zw_add(n4177, n4201);
    let n4204: ZW = zw_add(n4178, n4202);
    let n4205: ZW = zw_cellmix_n(358u64, n1039, 1542469173u64);
    let n4206: ZW = zw_cellmix_n(358u64, n1039, 668265263u64);
    let n4207: ZW = zw_add(n4203, n4205);
    let n4208: ZW = zw_add(n4204, n4206);
    let n4209: ZW = zw_add(n4207, n4187);
    let n4210: ZW = zw_add(n4208, n4188);
    let n4211: ZW = zw_add(n4209, n4075);
    let n4212: ZW = zw_add(n4210, n4076);
    let n4213: ZW = zw_cellmix_n(368u64, n2442, 1542469173u64);
    let n4214: ZW = zw_cellmix_n(368u64, n2442, 668265263u64);
    let n4215: ZW = zw_add(n4211, n4213);
    let n4216: ZW = zw_add(n4212, n4214);
    let n4217: ZW = zw_cellmix_n(369u64, n2440, 1542469173u64);
    let n4218: ZW = zw_cellmix_n(369u64, n2440, 668265263u64);
    let n4219: ZW = zw_add(n4215, n4217);
    let n4220: ZW = zw_add(n4216, n4218);
    let n4221: ZW = zw_cellmix_n(358u64, n1047, 1542469173u64);
    let n4222: ZW = zw_cellmix_n(358u64, n1047, 668265263u64);
    let n4223: ZW = zw_add(n4203, n4221);
    let n4224: ZW = zw_add(n4204, n4222);
    let n4225: ZW = zw_add(n4223, n4187);
    let n4226: ZW = zw_add(n4224, n4188);
    let n4227: ZW = zw_add(n4225, n4087);
    let n4228: ZW = zw_add(n4226, n4088);
    let n4229: ZW = zw_cellmix_n(368u64, n2448, 1542469173u64);
    let n4230: ZW = zw_cellmix_n(368u64, n2448, 668265263u64);
    let n4231: ZW = zw_add(n4227, n4229);
    let n4232: ZW = zw_add(n4228, n4230);
    let n4233: ZW = zw_cellmix_n(369u64, n2446, 1542469173u64);
    let n4234: ZW = zw_cellmix_n(369u64, n2446, 668265263u64);
    let n4235: ZW = zw_add(n4231, n4233);
    let n4236: ZW = zw_add(n4232, n4234);
    let n4237: ZW = zw_cellmix_n(356u64, n1055, 1542469173u64);
    let n4238: ZW = zw_cellmix_n(356u64, n1055, 668265263u64);
    let n4239: ZW = zw_add(n4173, n4237);
    let n4240: ZW = zw_add(n4174, n4238);
    let n4241: ZW = zw_cellmix_n(357u64, n1061, 1542469173u64);
    let n4242: ZW = zw_cellmix_n(357u64, n1061, 668265263u64);
    let n4243: ZW = zw_add(n4239, n4241);
    let n4244: ZW = zw_add(n4240, n4242);
    let n4245: ZW = zw_cellmix_n(358u64, n1062, 1542469173u64);
    let n4246: ZW = zw_cellmix_n(358u64, n1062, 668265263u64);
    let n4247: ZW = zw_add(n4243, n4245);
    let n4248: ZW = zw_add(n4244, n4246);
    let n4249: ZW = zw_cellmix_n(359u64, n1056, 1542469173u64);
    let n4250: ZW = zw_cellmix_n(359u64, n1056, 668265263u64);
    let n4251: ZW = zw_add(n4247, n4249);
    let n4252: ZW = zw_add(n4248, n4250);
    let n4253: ZW = zw_add(n4251, n4063);
    let n4254: ZW = zw_add(n4252, n4064);
    let n4255: ZW = zw_cellmix_n(368u64, n2454, 1542469173u64);
    let n4256: ZW = zw_cellmix_n(368u64, n2454, 668265263u64);
    let n4257: ZW = zw_add(n4253, n4255);
    let n4258: ZW = zw_add(n4254, n4256);
    let n4259: ZW = zw_cellmix_n(369u64, n2452, 1542469173u64);
    let n4260: ZW = zw_cellmix_n(369u64, n2452, 668265263u64);
    let n4261: ZW = zw_add(n4257, n4259);
    let n4262: ZW = zw_add(n4258, n4260);
    let n4263: ZW = zw_add(n4239, n4201);
    let n4264: ZW = zw_add(n4240, n4202);
    let n4265: ZW = zw_add(n4263, n4205);
    let n4266: ZW = zw_add(n4264, n4206);
    let n4267: ZW = zw_add(n4265, n4249);
    let n4268: ZW = zw_add(n4266, n4250);
    let n4269: ZW = zw_add(n4267, n4075);
    let n4270: ZW = zw_add(n4268, n4076);
    let n4271: ZW = zw_cellmix_n(368u64, n2460, 1542469173u64);
    let n4272: ZW = zw_cellmix_n(368u64, n2460, 668265263u64);
    let n4273: ZW = zw_add(n4269, n4271);
    let n4274: ZW = zw_add(n4270, n4272);
    let n4275: ZW = zw_cellmix_n(369u64, n2458, 1542469173u64);
    let n4276: ZW = zw_cellmix_n(369u64, n2458, 668265263u64);
    let n4277: ZW = zw_add(n4273, n4275);
    let n4278: ZW = zw_add(n4274, n4276);
    let n4279: ZW = zw_add(n4263, n4221);
    let n4280: ZW = zw_add(n4264, n4222);
    let n4281: ZW = zw_add(n4279, n4249);
    let n4282: ZW = zw_add(n4280, n4250);
    let n4283: ZW = zw_add(n4281, n4087);
    let n4284: ZW = zw_add(n4282, n4088);
    let n4285: ZW = zw_cellmix_n(368u64, n2466, 1542469173u64);
    let n4286: ZW = zw_cellmix_n(368u64, n2466, 668265263u64);
    let n4287: ZW = zw_add(n4283, n4285);
    let n4288: ZW = zw_add(n4284, n4286);
    let n4289: ZW = zw_cellmix_n(369u64, n2464, 1542469173u64);
    let n4290: ZW = zw_cellmix_n(369u64, n2464, 668265263u64);
    let n4291: ZW = zw_add(n4287, n4289);
    let n4292: ZW = zw_add(n4288, n4290);
    let n4293: ZW = zw_cellmix_n(359u64, n1080, 1542469173u64);
    let n4294: ZW = zw_cellmix_n(359u64, n1080, 668265263u64);
    let n4295: ZW = zw_add(n4247, n4293);
    let n4296: ZW = zw_add(n4248, n4294);
    let n4297: ZW = zw_add(n4295, n4063);
    let n4298: ZW = zw_add(n4296, n4064);
    let n4299: ZW = zw_add(n4297, n4255);
    let n4300: ZW = zw_add(n4298, n4256);
    let n4301: ZW = zw_cellmix_n(369u64, n2468, 1542469173u64);
    let n4302: ZW = zw_cellmix_n(369u64, n2468, 668265263u64);
    let n4303: ZW = zw_add(n4299, n4301);
    let n4304: ZW = zw_add(n4300, n4302);
    let n4305: ZW = zw_add(n4265, n4293);
    let n4306: ZW = zw_add(n4266, n4294);
    let n4307: ZW = zw_add(n4305, n4075);
    let n4308: ZW = zw_add(n4306, n4076);
    let n4309: ZW = zw_add(n4307, n4271);
    let n4310: ZW = zw_add(n4308, n4272);
    let n4311: ZW = zw_cellmix_n(369u64, n2470, 1542469173u64);
    let n4312: ZW = zw_cellmix_n(369u64, n2470, 668265263u64);
    let n4313: ZW = zw_add(n4309, n4311);
    let n4314: ZW = zw_add(n4310, n4312);
    let n4315: ZW = zw_add(n4279, n4293);
    let n4316: ZW = zw_add(n4280, n4294);
    let n4317: ZW = zw_add(n4315, n4087);
    let n4318: ZW = zw_add(n4316, n4088);
    let n4319: ZW = zw_add(n4317, n4285);
    let n4320: ZW = zw_add(n4318, n4286);
    let n4321: ZW = zw_cellmix_n(369u64, n2472, 1542469173u64);
    let n4322: ZW = zw_cellmix_n(369u64, n2472, 668265263u64);
    let n4323: ZW = zw_add(n4319, n4321);
    let n4324: ZW = zw_add(n4320, n4322);
    let n4325: ZW = zw_add(n4161, n4099);
    let n4326: ZW = zw_add(n4162, n4100);
    let n4327: ZW = zw_add(n4325, n4165);
    let n4328: ZW = zw_add(n4326, n4166);
    let n4329: ZW = zw_add(n4327, n3211);
    let n4330: ZW = zw_add(n4328, n3212);
    let n4331: ZW = zw_add(n4329, n4171);
    let n4332: ZW = zw_add(n4330, n4172);
    let n4333: ZW = zw_add(n4331, n4175);
    let n4334: ZW = zw_add(n4332, n4176);
    let n4335: ZW = zw_add(n4333, n4179);
    let n4336: ZW = zw_add(n4334, n4180);
    let n4337: ZW = zw_add(n4335, n4183);
    let n4338: ZW = zw_add(n4336, n4184);
    let n4339: ZW = zw_add(n4337, n4187);
    let n4340: ZW = zw_add(n4338, n4188);
    let n4341: ZW = zw_add(n4339, n4063);
    let n4342: ZW = zw_add(n4340, n4064);
    let n4343: ZW = zw_cellmix_n(368u64, n2478, 1542469173u64);
    let n4344: ZW = zw_cellmix_n(368u64, n2478, 668265263u64);
    let n4345: ZW = zw_add(n4341, n4343);
    let n4346: ZW = zw_add(n4342, n4344);
    let n4347: ZW = zw_cellmix_n(369u64, n2476, 1542469173u64);
    let n4348: ZW = zw_cellmix_n(369u64, n2476, 668265263u64);
    let n4349: ZW = zw_add(n4345, n4347);
    let n4350: ZW = zw_add(n4346, n4348);
    let n4351: ZW = zw_add(n4333, n4201);
    let n4352: ZW = zw_add(n4334, n4202);
    let n4353: ZW = zw_add(n4351, n4205);
    let n4354: ZW = zw_add(n4352, n4206);
    let n4355: ZW = zw_add(n4353, n4187);
    let n4356: ZW = zw_add(n4354, n4188);
    let n4357: ZW = zw_add(n4355, n4075);
    let n4358: ZW = zw_add(n4356, n4076);
    let n4359: ZW = zw_cellmix_n(368u64, n2484, 1542469173u64);
    let n4360: ZW = zw_cellmix_n(368u64, n2484, 668265263u64);
    let n4361: ZW = zw_add(n4357, n4359);
    let n4362: ZW = zw_add(n4358, n4360);
    let n4363: ZW = zw_cellmix_n(369u64, n2482, 1542469173u64);
    let n4364: ZW = zw_cellmix_n(369u64, n2482, 668265263u64);
    let n4365: ZW = zw_add(n4361, n4363);
    let n4366: ZW = zw_add(n4362, n4364);
    let n4367: ZW = zw_add(n4351, n4221);
    let n4368: ZW = zw_add(n4352, n4222);
    let n4369: ZW = zw_add(n4367, n4187);
    let n4370: ZW = zw_add(n4368, n4188);
    let n4371: ZW = zw_add(n4369, n4087);
    let n4372: ZW = zw_add(n4370, n4088);
    let n4373: ZW = zw_cellmix_n(368u64, n2490, 1542469173u64);
    let n4374: ZW = zw_cellmix_n(368u64, n2490, 668265263u64);
    let n4375: ZW = zw_add(n4371, n4373);
    let n4376: ZW = zw_add(n4372, n4374);
    let n4377: ZW = zw_cellmix_n(369u64, n2488, 1542469173u64);
    let n4378: ZW = zw_cellmix_n(369u64, n2488, 668265263u64);
    let n4379: ZW = zw_add(n4375, n4377);
    let n4380: ZW = zw_add(n4376, n4378);
    let n4381: ZW = zw_add(n4331, n4237);
    let n4382: ZW = zw_add(n4332, n4238);
    let n4383: ZW = zw_add(n4381, n4241);
    let n4384: ZW = zw_add(n4382, n4242);
    let n4385: ZW = zw_add(n4383, n4245);
    let n4386: ZW = zw_add(n4384, n4246);
    let n4387: ZW = zw_add(n4385, n4249);
    let n4388: ZW = zw_add(n4386, n4250);
    let n4389: ZW = zw_add(n4387, n4063);
    let n4390: ZW = zw_add(n4388, n4064);
    let n4391: ZW = zw_cellmix_n(368u64, n2496, 1542469173u64);
    let n4392: ZW = zw_cellmix_n(368u64, n2496, 668265263u64);
    let n4393: ZW = zw_add(n4389, n4391);
    let n4394: ZW = zw_add(n4390, n4392);
    let n4395: ZW = zw_cellmix_n(369u64, n2494, 1542469173u64);
    let n4396: ZW = zw_cellmix_n(369u64, n2494, 668265263u64);
    let n4397: ZW = zw_add(n4393, n4395);
    let n4398: ZW = zw_add(n4394, n4396);
    let n4399: ZW = zw_add(n4381, n4201);
    let n4400: ZW = zw_add(n4382, n4202);
    let n4401: ZW = zw_add(n4399, n4205);
    let n4402: ZW = zw_add(n4400, n4206);
    let n4403: ZW = zw_add(n4401, n4249);
    let n4404: ZW = zw_add(n4402, n4250);
    let n4405: ZW = zw_add(n4403, n4075);
    let n4406: ZW = zw_add(n4404, n4076);
    let n4407: ZW = zw_cellmix_n(368u64, n2502, 1542469173u64);
    let n4408: ZW = zw_cellmix_n(368u64, n2502, 668265263u64);
    let n4409: ZW = zw_add(n4405, n4407);
    let n4410: ZW = zw_add(n4406, n4408);
    let n4411: ZW = zw_cellmix_n(369u64, n2500, 1542469173u64);
    let n4412: ZW = zw_cellmix_n(369u64, n2500, 668265263u64);
    let n4413: ZW = zw_add(n4409, n4411);
    let n4414: ZW = zw_add(n4410, n4412);
    let n4415: ZW = zw_add(n4399, n4221);
    let n4416: ZW = zw_add(n4400, n4222);
    let n4417: ZW = zw_add(n4415, n4249);
    let n4418: ZW = zw_add(n4416, n4250);
    let n4419: ZW = zw_add(n4417, n4087);
    let n4420: ZW = zw_add(n4418, n4088);
    let n4421: ZW = zw_cellmix_n(368u64, n2508, 1542469173u64);
    let n4422: ZW = zw_cellmix_n(368u64, n2508, 668265263u64);
    let n4423: ZW = zw_add(n4419, n4421);
    let n4424: ZW = zw_add(n4420, n4422);
    let n4425: ZW = zw_cellmix_n(369u64, n2506, 1542469173u64);
    let n4426: ZW = zw_cellmix_n(369u64, n2506, 668265263u64);
    let n4427: ZW = zw_add(n4423, n4425);
    let n4428: ZW = zw_add(n4424, n4426);
    let n4429: ZW = zw_add(n4385, n4293);
    let n4430: ZW = zw_add(n4386, n4294);
    let n4431: ZW = zw_add(n4429, n4063);
    let n4432: ZW = zw_add(n4430, n4064);
    let n4433: ZW = zw_add(n4431, n4391);
    let n4434: ZW = zw_add(n4432, n4392);
    let n4435: ZW = zw_cellmix_n(369u64, n2510, 1542469173u64);
    let n4436: ZW = zw_cellmix_n(369u64, n2510, 668265263u64);
    let n4437: ZW = zw_add(n4433, n4435);
    let n4438: ZW = zw_add(n4434, n4436);
    let n4439: ZW = zw_add(n4401, n4293);
    let n4440: ZW = zw_add(n4402, n4294);
    let n4441: ZW = zw_add(n4439, n4075);
    let n4442: ZW = zw_add(n4440, n4076);
    let n4443: ZW = zw_add(n4441, n4407);
    let n4444: ZW = zw_add(n4442, n4408);
    let n4445: ZW = zw_cellmix_n(369u64, n2512, 1542469173u64);
    let n4446: ZW = zw_cellmix_n(369u64, n2512, 668265263u64);
    let n4447: ZW = zw_add(n4443, n4445);
    let n4448: ZW = zw_add(n4444, n4446);
    let n4449: ZW = zw_add(n4415, n4293);
    let n4450: ZW = zw_add(n4416, n4294);
    let n4451: ZW = zw_add(n4449, n4087);
    let n4452: ZW = zw_add(n4450, n4088);
    let n4453: ZW = zw_add(n4451, n4421);
    let n4454: ZW = zw_add(n4452, n4422);
    let n4455: ZW = zw_cellmix_n(369u64, n2514, 1542469173u64);
    let n4456: ZW = zw_cellmix_n(369u64, n2514, 668265263u64);
    let n4457: ZW = zw_add(n4453, n4455);
    let n4458: ZW = zw_add(n4454, n4456);
    let n4459: ZW = zw_cellmix_n(87u64, n2595, 1542469173u64);
    let n4460: ZW = zw_cellmix_n(87u64, n2595, 668265263u64);
    let n4461: ZW = zw_add(n3975, n4459);
    let n4462: ZW = zw_add(n3976, n4460);
    let n4463: ZW = zw_add(n4461, n3051);
    let n4464: ZW = zw_add(n4462, n3052);
    let n4465: ZW = zw_add(n4463, n3055);
    let n4466: ZW = zw_add(n4464, n3056);
    let n4467: ZW = zw_add(n4461, n3189);
    let n4468: ZW = zw_add(n4462, n3190);
    let n4469: ZW = zw_add(n4467, n3193);
    let n4470: ZW = zw_add(n4468, n3194);
    let n4471: ZW = zw_cellmix_n(87u64, n2764, 1542469173u64);
    let n4472: ZW = zw_cellmix_n(87u64, n2764, 668265263u64);
    let n4473: ZW = zw_add(n3975, n4471);
    let n4474: ZW = zw_add(n3976, n4472);
    let n4475: ZW = zw_add(n4473, n3989);
    let n4476: ZW = zw_add(n4474, n3990);
    let n4477: ZW = zw_add(n4475, n3993);
    let n4478: ZW = zw_add(n4476, n3994);
    let n4479: ZW = zw_add(n4477, n3051);
    let n4480: ZW = zw_add(n4478, n3052);
    let n4481: ZW = zw_add(n4479, n3055);
    let n4482: ZW = zw_add(n4480, n3056);
    let n4483: ZW = zw_cellmix_n(20u64, n2769, 1542469173u64);
    let n4484: ZW = zw_cellmix_n(20u64, n2769, 668265263u64);
    let n4485: ZW = zw_add(n4477, n4483);
    let n4486: ZW = zw_add(n4478, n4484);
    let n4487: ZW = zw_cellmix_b(41u64, n2770, 1542469173u64);
    let n4488: ZW = zw_cellmix_b(41u64, n2770, 668265263u64);
    let n4489: ZW = zw_add(n4485, n4487);
    let n4490: ZW = zw_add(n4486, n4488);
    let n4491: ZW = zw_cellmix_b(38u64, n2780, 1542469173u64);
    let n4492: ZW = zw_cellmix_b(38u64, n2780, 668265263u64);
    let n4493: ZW = zw_add(zw_splat(0u64), n4491);
    let n4494: ZW = zw_add(zw_splat(0u64), n4492);
    let n4495: ZW = zw_cellmix_n(39u64, n2785, 1542469173u64);
    let n4496: ZW = zw_cellmix_n(39u64, n2785, 668265263u64);
    let n4497: ZW = zw_add(n4493, n4495);
    let n4498: ZW = zw_add(n4494, n4496);
    let n4499: ZW = zw_add(n4497, n3023);
    let n4500: ZW = zw_add(n4498, n3024);
    let n4501: ZW = zw_add(n4499, n3027);
    let n4502: ZW = zw_add(n4500, n3028);
    let n4503: ZW = zw_add(n4501, n3031);
    let n4504: ZW = zw_add(n4502, n3032);
    let n4505: ZW = zw_cellmix_n(87u64, n2784, 1542469173u64);
    let n4506: ZW = zw_cellmix_n(87u64, n2784, 668265263u64);
    let n4507: ZW = zw_add(n4503, n4505);
    let n4508: ZW = zw_add(n4504, n4506);
    let n4509: ZW = zw_add(n4507, n3051);
    let n4510: ZW = zw_add(n4508, n3052);
    let n4511: ZW = zw_cellmix_n(20u64, n2787, 1542469173u64);
    let n4512: ZW = zw_cellmix_n(20u64, n2787, 668265263u64);
    let n4513: ZW = zw_add(n4507, n4511);
    let n4514: ZW = zw_add(n4508, n4512);
    let n4515: ZW = zw_cellmix_b(38u64, n2792, 1542469173u64);
    let n4516: ZW = zw_cellmix_b(38u64, n2792, 668265263u64);
    let n4517: ZW = zw_add(zw_splat(0u64), n4515);
    let n4518: ZW = zw_add(zw_splat(0u64), n4516);
    let n4519: ZW = zw_cellmix_n(39u64, n2797, 1542469173u64);
    let n4520: ZW = zw_cellmix_n(39u64, n2797, 668265263u64);
    let n4521: ZW = zw_add(n4517, n4519);
    let n4522: ZW = zw_add(n4518, n4520);
    let n4523: ZW = zw_add(n4521, n3023);
    let n4524: ZW = zw_add(n4522, n3024);
    let n4525: ZW = zw_add(n4523, n3027);
    let n4526: ZW = zw_add(n4524, n3028);
    let n4527: ZW = zw_add(n4525, n3031);
    let n4528: ZW = zw_add(n4526, n3032);
    let n4529: ZW = zw_cellmix_n(87u64, n2796, 1542469173u64);
    let n4530: ZW = zw_cellmix_n(87u64, n2796, 668265263u64);
    let n4531: ZW = zw_add(n4527, n4529);
    let n4532: ZW = zw_add(n4528, n4530);
    let n4533: ZW = zw_add(n4531, n3051);
    let n4534: ZW = zw_add(n4532, n3052);
    let n4535: ZW = zw_add(n4531, n3189);
    let n4536: ZW = zw_add(n4532, n3190);
    let n4537: ZW = zw_cellmix_n(246u64, n2800, 1542469173u64);
    let n4538: ZW = zw_cellmix_n(246u64, n2800, 668265263u64);
    let n4539: ZW = zw_add(n3037, n4537);
    let n4540: ZW = zw_add(n3038, n4538);
    let n4541: ZW = zw_cellmix_n(254u64, n2801, 1542469173u64);
    let n4542: ZW = zw_cellmix_n(254u64, n2801, 668265263u64);
    let n4543: ZW = zw_add(n4539, n4541);
    let n4544: ZW = zw_add(n4540, n4542);
    let n4545: ZW = zw_cellmix_n(317u64, n2811, 1542469173u64);
    let n4546: ZW = zw_cellmix_n(317u64, n2811, 668265263u64);
    let n4547: ZW = zw_add(n4543, n4545);
    let n4548: ZW = zw_add(n4544, n4546);
    let n4549: ZW = zw_cellmix_n(404u64, n2813, 1542469173u64);
    let n4550: ZW = zw_cellmix_n(404u64, n2813, 668265263u64);
    let n4551: ZW = zw_add(n4547, n4549);
    let n4552: ZW = zw_add(n4548, n4550);
    let n4553: ZW = zw_cellmix_n(405u64, n2814, 1542469173u64);
    let n4554: ZW = zw_cellmix_n(405u64, n2814, 668265263u64);
    let n4555: ZW = zw_add(n4551, n4553);
    let n4556: ZW = zw_add(n4552, n4554);
    let n4557: ZW = zw_cellmix_n(20u64, n2805, 1542469173u64);
    let n4558: ZW = zw_cellmix_n(20u64, n2805, 668265263u64);
    let n4559: ZW = zw_add(n4555, n4557);
    let n4560: ZW = zw_add(n4556, n4558);
    let n4561: ZW = zw_add(n4559, n3055);
    let n4562: ZW = zw_add(n4560, n3056);
    let n4563: ZW = zw_cellmix_n(297u64, n2806, 1542469173u64);
    let n4564: ZW = zw_cellmix_n(297u64, n2806, 668265263u64);
    let n4565: ZW = zw_add(n4561, n4563);
    let n4566: ZW = zw_add(n4562, n4564);
    let n4567: ZW = zw_cellmix_n(299u64, n2807, 1542469173u64);
    let n4568: ZW = zw_cellmix_n(299u64, n2807, 668265263u64);
    let n4569: ZW = zw_add(n4565, n4567);
    let n4570: ZW = zw_add(n4566, n4568);
    let n4571: ZW = zw_cellmix_n(300u64, n2808, 1542469173u64);
    let n4572: ZW = zw_cellmix_n(300u64, n2808, 668265263u64);
    let n4573: ZW = zw_add(n4569, n4571);
    let n4574: ZW = zw_add(n4570, n4572);
    let n4575: ZW = zw_cellmix_n(302u64, n2809, 1542469173u64);
    let n4576: ZW = zw_cellmix_n(302u64, n2809, 668265263u64);
    let n4577: ZW = zw_add(n4573, n4575);
    let n4578: ZW = zw_add(n4574, n4576);
    let n4579: ZW = zw_cellmix_b(309u64, n2802, 1542469173u64);
    let n4580: ZW = zw_cellmix_b(309u64, n2802, 668265263u64);
    let n4581: ZW = zw_add(n4577, n4579);
    let n4582: ZW = zw_add(n4578, n4580);
    let n4583: ZW = zw_cellmix_b(310u64, n2803, 1542469173u64);
    let n4584: ZW = zw_cellmix_b(310u64, n2803, 668265263u64);
    let n4585: ZW = zw_add(n4581, n4583);
    let n4586: ZW = zw_add(n4582, n4584);
    let n4587: ZW = zw_cellmix_n(316u64, n2827, 1542469173u64);
    let n4588: ZW = zw_cellmix_n(316u64, n2827, 668265263u64);
    let n4589: ZW = zw_add(n4585, n4587);
    let n4590: ZW = zw_add(n4586, n4588);
    let n4591: ZW = zw_cellmix_n(394u64, r_c394, 1542469173u64);
    let n4592: ZW = zw_cellmix_n(394u64, r_c394, 668265263u64);
    let n4593: ZW = zw_add(n4589, n4591);
    let n4594: ZW = zw_add(n4590, n4592);
    let n4595: ZW = zw_cellmix_n(395u64, r_c395, 1542469173u64);
    let n4596: ZW = zw_cellmix_n(395u64, r_c395, 668265263u64);
    let n4597: ZW = zw_add(n4593, n4595);
    let n4598: ZW = zw_add(n4594, n4596);
    let n4599: ZW = zw_cellmix_n(396u64, r_c396, 1542469173u64);
    let n4600: ZW = zw_cellmix_n(396u64, r_c396, 668265263u64);
    let n4601: ZW = zw_add(n4597, n4599);
    let n4602: ZW = zw_add(n4598, n4600);
    let n4603: ZW = zw_cellmix_n(397u64, r_c397, 1542469173u64);
    let n4604: ZW = zw_cellmix_n(397u64, r_c397, 668265263u64);
    let n4605: ZW = zw_add(n4601, n4603);
    let n4606: ZW = zw_add(n4602, n4604);
    let n4607: ZW = zw_cellmix_b(398u64, n2812, 1542469173u64);
    let n4608: ZW = zw_cellmix_b(398u64, n2812, 668265263u64);
    let n4609: ZW = zw_add(n4605, n4607);
    let n4610: ZW = zw_add(n4606, n4608);
    let n4611: ZW = zw_cellmix_n(406u64, n2828, 1542469173u64);
    let n4612: ZW = zw_cellmix_n(406u64, n2828, 668265263u64);
    let n4613: ZW = zw_add(n4609, n4611);
    let n4614: ZW = zw_add(n4610, n4612);
    let n4615: ZW = zw_cellmix_n(407u64, n2816, 1542469173u64);
    let n4616: ZW = zw_cellmix_n(407u64, n2816, 668265263u64);
    let n4617: ZW = zw_add(n4613, n4615);
    let n4618: ZW = zw_add(n4614, n4616);
    let n4619: ZW = zw_cellmix_b(398u64, n2830, 1542469173u64);
    let n4620: ZW = zw_cellmix_b(398u64, n2830, 668265263u64);
    let n4621: ZW = zw_add(n4605, n4619);
    let n4622: ZW = zw_add(n4606, n4620);
    let n4623: ZW = zw_cellmix_n(406u64, n2834, 1542469173u64);
    let n4624: ZW = zw_cellmix_n(406u64, n2834, 668265263u64);
    let n4625: ZW = zw_add(n4621, n4623);
    let n4626: ZW = zw_add(n4622, n4624);
    let n4627: ZW = zw_cellmix_n(407u64, n2832, 1542469173u64);
    let n4628: ZW = zw_cellmix_n(407u64, n2832, 668265263u64);
    let n4629: ZW = zw_add(n4625, n4627);
    let n4630: ZW = zw_add(n4626, n4628);
    let n4631: ZW = zw_cellmix_b(398u64, n2835, 1542469173u64);
    let n4632: ZW = zw_cellmix_b(398u64, n2835, 668265263u64);
    let n4633: ZW = zw_add(n4605, n4631);
    let n4634: ZW = zw_add(n4606, n4632);
    let n4635: ZW = zw_cellmix_n(406u64, n2839, 1542469173u64);
    let n4636: ZW = zw_cellmix_n(406u64, n2839, 668265263u64);
    let n4637: ZW = zw_add(n4633, n4635);
    let n4638: ZW = zw_add(n4634, n4636);
    let n4639: ZW = zw_cellmix_n(407u64, n2837, 1542469173u64);
    let n4640: ZW = zw_cellmix_n(407u64, n2837, 668265263u64);
    let n4641: ZW = zw_add(n4637, n4639);
    let n4642: ZW = zw_add(n4638, n4640);
    let n4643: ZW = zw_cellmix_n(302u64, n2841, 1542469173u64);
    let n4644: ZW = zw_cellmix_n(302u64, n2841, 668265263u64);
    let n4645: ZW = zw_add(n4573, n4643);
    let n4646: ZW = zw_add(n4574, n4644);
    let n4647: ZW = zw_add(n4645, n4579);
    let n4648: ZW = zw_add(n4646, n4580);
    let n4649: ZW = zw_cellmix_b(310u64, n2840, 1542469173u64);
    let n4650: ZW = zw_cellmix_b(310u64, n2840, 668265263u64);
    let n4651: ZW = zw_add(n4647, n4649);
    let n4652: ZW = zw_add(n4648, n4650);
    let n4653: ZW = zw_add(n4651, n4587);
    let n4654: ZW = zw_add(n4652, n4588);
    let n4655: ZW = zw_add(n4653, n4591);
    let n4656: ZW = zw_add(n4654, n4592);
    let n4657: ZW = zw_add(n4655, n4595);
    let n4658: ZW = zw_add(n4656, n4596);
    let n4659: ZW = zw_add(n4657, n4599);
    let n4660: ZW = zw_add(n4658, n4600);
    let n4661: ZW = zw_add(n4659, n4603);
    let n4662: ZW = zw_add(n4660, n4604);
    let n4663: ZW = zw_add(n4661, n4607);
    let n4664: ZW = zw_add(n4662, n4608);
    let n4665: ZW = zw_cellmix_n(406u64, n2845, 1542469173u64);
    let n4666: ZW = zw_cellmix_n(406u64, n2845, 668265263u64);
    let n4667: ZW = zw_add(n4663, n4665);
    let n4668: ZW = zw_add(n4664, n4666);
    let n4669: ZW = zw_cellmix_n(407u64, n2843, 1542469173u64);
    let n4670: ZW = zw_cellmix_n(407u64, n2843, 668265263u64);
    let n4671: ZW = zw_add(n4667, n4669);
    let n4672: ZW = zw_add(n4668, n4670);
    let n4673: ZW = zw_add(n4661, n4619);
    let n4674: ZW = zw_add(n4662, n4620);
    let n4675: ZW = zw_cellmix_n(406u64, n2849, 1542469173u64);
    let n4676: ZW = zw_cellmix_n(406u64, n2849, 668265263u64);
    let n4677: ZW = zw_add(n4673, n4675);
    let n4678: ZW = zw_add(n4674, n4676);
    let n4679: ZW = zw_cellmix_n(407u64, n2847, 1542469173u64);
    let n4680: ZW = zw_cellmix_n(407u64, n2847, 668265263u64);
    let n4681: ZW = zw_add(n4677, n4679);
    let n4682: ZW = zw_add(n4678, n4680);
    let n4683: ZW = zw_add(n4661, n4631);
    let n4684: ZW = zw_add(n4662, n4632);
    let n4685: ZW = zw_cellmix_n(406u64, n2853, 1542469173u64);
    let n4686: ZW = zw_cellmix_n(406u64, n2853, 668265263u64);
    let n4687: ZW = zw_add(n4683, n4685);
    let n4688: ZW = zw_add(n4684, n4686);
    let n4689: ZW = zw_cellmix_n(407u64, n2851, 1542469173u64);
    let n4690: ZW = zw_cellmix_n(407u64, n2851, 668265263u64);
    let n4691: ZW = zw_add(n4687, n4689);
    let n4692: ZW = zw_add(n4688, n4690);
    let n4693: ZW = zw_cellmix_n(20u64, n2873, 1542469173u64);
    let n4694: ZW = zw_cellmix_n(20u64, n2873, 668265263u64);
    let n4695: ZW = zw_add(n4555, n4693);
    let n4696: ZW = zw_add(n4556, n4694);
    let n4697: ZW = zw_cellmix_b(41u64, n2874, 1542469173u64);
    let n4698: ZW = zw_cellmix_b(41u64, n2874, 668265263u64);
    let n4699: ZW = zw_add(n4695, n4697);
    let n4700: ZW = zw_add(n4696, n4698);
    let n4701: ZW = zw_cellmix_n(297u64, n2875, 1542469173u64);
    let n4702: ZW = zw_cellmix_n(297u64, n2875, 668265263u64);
    let n4703: ZW = zw_add(n4699, n4701);
    let n4704: ZW = zw_add(n4700, n4702);
    let n4705: ZW = zw_cellmix_n(299u64, n2876, 1542469173u64);
    let n4706: ZW = zw_cellmix_n(299u64, n2876, 668265263u64);
    let n4707: ZW = zw_add(n4703, n4705);
    let n4708: ZW = zw_add(n4704, n4706);
    let n4709: ZW = zw_cellmix_n(300u64, n2877, 1542469173u64);
    let n4710: ZW = zw_cellmix_n(300u64, n2877, 668265263u64);
    let n4711: ZW = zw_add(n4707, n4709);
    let n4712: ZW = zw_add(n4708, n4710);
    let n4713: ZW = zw_add(n4711, n4575);
    let n4714: ZW = zw_add(n4712, n4576);
    let n4715: ZW = zw_cellmix_b(309u64, n2854, 1542469173u64);
    let n4716: ZW = zw_cellmix_b(309u64, n2854, 668265263u64);
    let n4717: ZW = zw_add(n4713, n4715);
    let n4718: ZW = zw_add(n4714, n4716);
    let n4719: ZW = zw_add(n4717, n4583);
    let n4720: ZW = zw_add(n4718, n4584);
    let n4721: ZW = zw_cellmix_n(316u64, n2886, 1542469173u64);
    let n4722: ZW = zw_cellmix_n(316u64, n2886, 668265263u64);
    let n4723: ZW = zw_add(n4719, n4721);
    let n4724: ZW = zw_add(n4720, n4722);
    let n4725: ZW = zw_cellmix_n(394u64, n2878, 1542469173u64);
    let n4726: ZW = zw_cellmix_n(394u64, n2878, 668265263u64);
    let n4727: ZW = zw_add(n4723, n4725);
    let n4728: ZW = zw_add(n4724, n4726);
    let n4729: ZW = zw_cellmix_n(395u64, n2879, 1542469173u64);
    let n4730: ZW = zw_cellmix_n(395u64, n2879, 668265263u64);
    let n4731: ZW = zw_add(n4727, n4729);
    let n4732: ZW = zw_add(n4728, n4730);
    let n4733: ZW = zw_cellmix_n(396u64, n2880, 1542469173u64);
    let n4734: ZW = zw_cellmix_n(396u64, n2880, 668265263u64);
    let n4735: ZW = zw_add(n4731, n4733);
    let n4736: ZW = zw_add(n4732, n4734);
    let n4737: ZW = zw_cellmix_n(397u64, n2881, 1542469173u64);
    let n4738: ZW = zw_cellmix_n(397u64, n2881, 668265263u64);
    let n4739: ZW = zw_add(n4735, n4737);
    let n4740: ZW = zw_add(n4736, n4738);
    let n4741: ZW = zw_add(n4739, n4607);
    let n4742: ZW = zw_add(n4740, n4608);
    let n4743: ZW = zw_cellmix_n(406u64, n2887, 1542469173u64);
    let n4744: ZW = zw_cellmix_n(406u64, n2887, 668265263u64);
    let n4745: ZW = zw_add(n4741, n4743);
    let n4746: ZW = zw_add(n4742, n4744);
    let n4747: ZW = zw_cellmix_n(407u64, n2883, 1542469173u64);
    let n4748: ZW = zw_cellmix_n(407u64, n2883, 668265263u64);
    let n4749: ZW = zw_add(n4745, n4747);
    let n4750: ZW = zw_add(n4746, n4748);
    let n4751: ZW = zw_cellmix_n(395u64, n2896, 1542469173u64);
    let n4752: ZW = zw_cellmix_n(395u64, n2896, 668265263u64);
    let n4753: ZW = zw_add(n4727, n4751);
    let n4754: ZW = zw_add(n4728, n4752);
    let n4755: ZW = zw_cellmix_n(396u64, n2897, 1542469173u64);
    let n4756: ZW = zw_cellmix_n(396u64, n2897, 668265263u64);
    let n4757: ZW = zw_add(n4753, n4755);
    let n4758: ZW = zw_add(n4754, n4756);
    let n4759: ZW = zw_add(n4757, n4737);
    let n4760: ZW = zw_add(n4758, n4738);
    let n4761: ZW = zw_add(n4759, n4619);
    let n4762: ZW = zw_add(n4760, n4620);
    let n4763: ZW = zw_cellmix_n(406u64, n2901, 1542469173u64);
    let n4764: ZW = zw_cellmix_n(406u64, n2901, 668265263u64);
    let n4765: ZW = zw_add(n4761, n4763);
    let n4766: ZW = zw_add(n4762, n4764);
    let n4767: ZW = zw_cellmix_n(407u64, n2899, 1542469173u64);
    let n4768: ZW = zw_cellmix_n(407u64, n2899, 668265263u64);
    let n4769: ZW = zw_add(n4765, n4767);
    let n4770: ZW = zw_add(n4766, n4768);
    let n4771: ZW = zw_cellmix_n(396u64, n2908, 1542469173u64);
    let n4772: ZW = zw_cellmix_n(396u64, n2908, 668265263u64);
    let n4773: ZW = zw_add(n4753, n4771);
    let n4774: ZW = zw_add(n4754, n4772);
    let n4775: ZW = zw_add(n4773, n4737);
    let n4776: ZW = zw_add(n4774, n4738);
    let n4777: ZW = zw_add(n4775, n4631);
    let n4778: ZW = zw_add(n4776, n4632);
    let n4779: ZW = zw_cellmix_n(406u64, n2912, 1542469173u64);
    let n4780: ZW = zw_cellmix_n(406u64, n2912, 668265263u64);
    let n4781: ZW = zw_add(n4777, n4779);
    let n4782: ZW = zw_add(n4778, n4780);
    let n4783: ZW = zw_cellmix_n(407u64, n2910, 1542469173u64);
    let n4784: ZW = zw_cellmix_n(407u64, n2910, 668265263u64);
    let n4785: ZW = zw_add(n4781, n4783);
    let n4786: ZW = zw_add(n4782, n4784);
    let n4787: ZW = zw_cellmix_n(394u64, n2925, 1542469173u64);
    let n4788: ZW = zw_cellmix_n(394u64, n2925, 668265263u64);
    let n4789: ZW = zw_add(n4723, n4787);
    let n4790: ZW = zw_add(n4724, n4788);
    let n4791: ZW = zw_cellmix_n(395u64, n2926, 1542469173u64);
    let n4792: ZW = zw_cellmix_n(395u64, n2926, 668265263u64);
    let n4793: ZW = zw_add(n4789, n4791);
    let n4794: ZW = zw_add(n4790, n4792);
    let n4795: ZW = zw_cellmix_n(396u64, n2927, 1542469173u64);
    let n4796: ZW = zw_cellmix_n(396u64, n2927, 668265263u64);
    let n4797: ZW = zw_add(n4793, n4795);
    let n4798: ZW = zw_add(n4794, n4796);
    let n4799: ZW = zw_cellmix_n(397u64, n2928, 1542469173u64);
    let n4800: ZW = zw_cellmix_n(397u64, n2928, 668265263u64);
    let n4801: ZW = zw_add(n4797, n4799);
    let n4802: ZW = zw_add(n4798, n4800);
    let n4803: ZW = zw_add(n4801, n4607);
    let n4804: ZW = zw_add(n4802, n4608);
    let n4805: ZW = zw_cellmix_n(406u64, n2932, 1542469173u64);
    let n4806: ZW = zw_cellmix_n(406u64, n2932, 668265263u64);
    let n4807: ZW = zw_add(n4803, n4805);
    let n4808: ZW = zw_add(n4804, n4806);
    let n4809: ZW = zw_cellmix_n(407u64, n2930, 1542469173u64);
    let n4810: ZW = zw_cellmix_n(407u64, n2930, 668265263u64);
    let n4811: ZW = zw_add(n4807, n4809);
    let n4812: ZW = zw_add(n4808, n4810);
    let n4813: ZW = zw_add(n4789, n4751);
    let n4814: ZW = zw_add(n4790, n4752);
    let n4815: ZW = zw_add(n4813, n4755);
    let n4816: ZW = zw_add(n4814, n4756);
    let n4817: ZW = zw_add(n4815, n4799);
    let n4818: ZW = zw_add(n4816, n4800);
    let n4819: ZW = zw_add(n4817, n4619);
    let n4820: ZW = zw_add(n4818, n4620);
    let n4821: ZW = zw_cellmix_n(406u64, n2940, 1542469173u64);
    let n4822: ZW = zw_cellmix_n(406u64, n2940, 668265263u64);
    let n4823: ZW = zw_add(n4819, n4821);
    let n4824: ZW = zw_add(n4820, n4822);
    let n4825: ZW = zw_cellmix_n(407u64, n2938, 1542469173u64);
    let n4826: ZW = zw_cellmix_n(407u64, n2938, 668265263u64);
    let n4827: ZW = zw_add(n4823, n4825);
    let n4828: ZW = zw_add(n4824, n4826);
    let n4829: ZW = zw_add(n4813, n4771);
    let n4830: ZW = zw_add(n4814, n4772);
    let n4831: ZW = zw_add(n4829, n4799);
    let n4832: ZW = zw_add(n4830, n4800);
    let n4833: ZW = zw_add(n4831, n4631);
    let n4834: ZW = zw_add(n4832, n4632);
    let n4835: ZW = zw_cellmix_n(406u64, n2948, 1542469173u64);
    let n4836: ZW = zw_cellmix_n(406u64, n2948, 668265263u64);
    let n4837: ZW = zw_add(n4833, n4835);
    let n4838: ZW = zw_add(n4834, n4836);
    let n4839: ZW = zw_cellmix_n(407u64, n2946, 1542469173u64);
    let n4840: ZW = zw_cellmix_n(407u64, n2946, 668265263u64);
    let n4841: ZW = zw_add(n4837, n4839);
    let n4842: ZW = zw_add(n4838, n4840);
    let n4843: ZW = zw_cellmix_n(397u64, n2953, 1542469173u64);
    let n4844: ZW = zw_cellmix_n(397u64, n2953, 668265263u64);
    let n4845: ZW = zw_add(n4797, n4843);
    let n4846: ZW = zw_add(n4798, n4844);
    let n4847: ZW = zw_add(n4845, n4607);
    let n4848: ZW = zw_add(n4846, n4608);
    let n4849: ZW = zw_add(n4847, n4805);
    let n4850: ZW = zw_add(n4848, n4806);
    let n4851: ZW = zw_cellmix_n(407u64, n2954, 1542469173u64);
    let n4852: ZW = zw_cellmix_n(407u64, n2954, 668265263u64);
    let n4853: ZW = zw_add(n4849, n4851);
    let n4854: ZW = zw_add(n4850, n4852);
    let n4855: ZW = zw_add(n4815, n4843);
    let n4856: ZW = zw_add(n4816, n4844);
    let n4857: ZW = zw_add(n4855, n4619);
    let n4858: ZW = zw_add(n4856, n4620);
    let n4859: ZW = zw_add(n4857, n4821);
    let n4860: ZW = zw_add(n4858, n4822);
    let n4861: ZW = zw_cellmix_n(407u64, n2957, 1542469173u64);
    let n4862: ZW = zw_cellmix_n(407u64, n2957, 668265263u64);
    let n4863: ZW = zw_add(n4859, n4861);
    let n4864: ZW = zw_add(n4860, n4862);
    let n4865: ZW = zw_add(n4829, n4843);
    let n4866: ZW = zw_add(n4830, n4844);
    let n4867: ZW = zw_add(n4865, n4631);
    let n4868: ZW = zw_add(n4866, n4632);
    let n4869: ZW = zw_add(n4867, n4835);
    let n4870: ZW = zw_add(n4868, n4836);
    let n4871: ZW = zw_cellmix_n(407u64, n2960, 1542469173u64);
    let n4872: ZW = zw_cellmix_n(407u64, n2960, 668265263u64);
    let n4873: ZW = zw_add(n4869, n4871);
    let n4874: ZW = zw_add(n4870, n4872);
    let n4875: ZW = zw_add(n4711, n4643);
    let n4876: ZW = zw_add(n4712, n4644);
    let n4877: ZW = zw_add(n4875, n4715);
    let n4878: ZW = zw_add(n4876, n4716);
    let n4879: ZW = zw_add(n4877, n4649);
    let n4880: ZW = zw_add(n4878, n4650);
    let n4881: ZW = zw_add(n4879, n4721);
    let n4882: ZW = zw_add(n4880, n4722);
    let n4883: ZW = zw_add(n4881, n4725);
    let n4884: ZW = zw_add(n4882, n4726);
    let n4885: ZW = zw_add(n4883, n4729);
    let n4886: ZW = zw_add(n4884, n4730);
    let n4887: ZW = zw_add(n4885, n4733);
    let n4888: ZW = zw_add(n4886, n4734);
    let n4889: ZW = zw_add(n4887, n4737);
    let n4890: ZW = zw_add(n4888, n4738);
    let n4891: ZW = zw_add(n4889, n4607);
    let n4892: ZW = zw_add(n4890, n4608);
    let n4893: ZW = zw_cellmix_n(406u64, n2968, 1542469173u64);
    let n4894: ZW = zw_cellmix_n(406u64, n2968, 668265263u64);
    let n4895: ZW = zw_add(n4891, n4893);
    let n4896: ZW = zw_add(n4892, n4894);
    let n4897: ZW = zw_cellmix_n(407u64, n2966, 1542469173u64);
    let n4898: ZW = zw_cellmix_n(407u64, n2966, 668265263u64);
    let n4899: ZW = zw_add(n4895, n4897);
    let n4900: ZW = zw_add(n4896, n4898);
    let n4901: ZW = zw_add(n4883, n4751);
    let n4902: ZW = zw_add(n4884, n4752);
    let n4903: ZW = zw_add(n4901, n4755);
    let n4904: ZW = zw_add(n4902, n4756);
    let n4905: ZW = zw_add(n4903, n4737);
    let n4906: ZW = zw_add(n4904, n4738);
    let n4907: ZW = zw_add(n4905, n4619);
    let n4908: ZW = zw_add(n4906, n4620);
    let n4909: ZW = zw_cellmix_n(406u64, n2976, 1542469173u64);
    let n4910: ZW = zw_cellmix_n(406u64, n2976, 668265263u64);
    let n4911: ZW = zw_add(n4907, n4909);
    let n4912: ZW = zw_add(n4908, n4910);
    let n4913: ZW = zw_cellmix_n(407u64, n2974, 1542469173u64);
    let n4914: ZW = zw_cellmix_n(407u64, n2974, 668265263u64);
    let n4915: ZW = zw_add(n4911, n4913);
    let n4916: ZW = zw_add(n4912, n4914);
    let n4917: ZW = zw_add(n4901, n4771);
    let n4918: ZW = zw_add(n4902, n4772);
    let n4919: ZW = zw_add(n4917, n4737);
    let n4920: ZW = zw_add(n4918, n4738);
    let n4921: ZW = zw_add(n4919, n4631);
    let n4922: ZW = zw_add(n4920, n4632);
    let n4923: ZW = zw_cellmix_n(406u64, n2984, 1542469173u64);
    let n4924: ZW = zw_cellmix_n(406u64, n2984, 668265263u64);
    let n4925: ZW = zw_add(n4921, n4923);
    let n4926: ZW = zw_add(n4922, n4924);
    let n4927: ZW = zw_cellmix_n(407u64, n2982, 1542469173u64);
    let n4928: ZW = zw_cellmix_n(407u64, n2982, 668265263u64);
    let n4929: ZW = zw_add(n4925, n4927);
    let n4930: ZW = zw_add(n4926, n4928);
    let n4931: ZW = zw_add(n4881, n4787);
    let n4932: ZW = zw_add(n4882, n4788);
    let n4933: ZW = zw_add(n4931, n4791);
    let n4934: ZW = zw_add(n4932, n4792);
    let n4935: ZW = zw_add(n4933, n4795);
    let n4936: ZW = zw_add(n4934, n4796);
    let n4937: ZW = zw_add(n4935, n4799);
    let n4938: ZW = zw_add(n4936, n4800);
    let n4939: ZW = zw_add(n4937, n4607);
    let n4940: ZW = zw_add(n4938, n4608);
    let n4941: ZW = zw_cellmix_n(406u64, n2992, 1542469173u64);
    let n4942: ZW = zw_cellmix_n(406u64, n2992, 668265263u64);
    let n4943: ZW = zw_add(n4939, n4941);
    let n4944: ZW = zw_add(n4940, n4942);
    let n4945: ZW = zw_cellmix_n(407u64, n2990, 1542469173u64);
    let n4946: ZW = zw_cellmix_n(407u64, n2990, 668265263u64);
    let n4947: ZW = zw_add(n4943, n4945);
    let n4948: ZW = zw_add(n4944, n4946);
    let n4949: ZW = zw_add(n4931, n4751);
    let n4950: ZW = zw_add(n4932, n4752);
    let n4951: ZW = zw_add(n4949, n4755);
    let n4952: ZW = zw_add(n4950, n4756);
    let n4953: ZW = zw_add(n4951, n4799);
    let n4954: ZW = zw_add(n4952, n4800);
    let n4955: ZW = zw_add(n4953, n4619);
    let n4956: ZW = zw_add(n4954, n4620);
    let n4957: ZW = zw_cellmix_n(406u64, n3000, 1542469173u64);
    let n4958: ZW = zw_cellmix_n(406u64, n3000, 668265263u64);
    let n4959: ZW = zw_add(n4955, n4957);
    let n4960: ZW = zw_add(n4956, n4958);
    let n4961: ZW = zw_cellmix_n(407u64, n2998, 1542469173u64);
    let n4962: ZW = zw_cellmix_n(407u64, n2998, 668265263u64);
    let n4963: ZW = zw_add(n4959, n4961);
    let n4964: ZW = zw_add(n4960, n4962);
    let n4965: ZW = zw_add(n4949, n4771);
    let n4966: ZW = zw_add(n4950, n4772);
    let n4967: ZW = zw_add(n4965, n4799);
    let n4968: ZW = zw_add(n4966, n4800);
    let n4969: ZW = zw_add(n4967, n4631);
    let n4970: ZW = zw_add(n4968, n4632);
    let n4971: ZW = zw_cellmix_n(406u64, n3008, 1542469173u64);
    let n4972: ZW = zw_cellmix_n(406u64, n3008, 668265263u64);
    let n4973: ZW = zw_add(n4969, n4971);
    let n4974: ZW = zw_add(n4970, n4972);
    let n4975: ZW = zw_cellmix_n(407u64, n3006, 1542469173u64);
    let n4976: ZW = zw_cellmix_n(407u64, n3006, 668265263u64);
    let n4977: ZW = zw_add(n4973, n4975);
    let n4978: ZW = zw_add(n4974, n4976);
    let n4979: ZW = zw_add(n4935, n4843);
    let n4980: ZW = zw_add(n4936, n4844);
    let n4981: ZW = zw_add(n4979, n4607);
    let n4982: ZW = zw_add(n4980, n4608);
    let n4983: ZW = zw_add(n4981, n4941);
    let n4984: ZW = zw_add(n4982, n4942);
    let n4985: ZW = zw_cellmix_n(407u64, n3011, 1542469173u64);
    let n4986: ZW = zw_cellmix_n(407u64, n3011, 668265263u64);
    let n4987: ZW = zw_add(n4983, n4985);
    let n4988: ZW = zw_add(n4984, n4986);
    let n4989: ZW = zw_add(n4951, n4843);
    let n4990: ZW = zw_add(n4952, n4844);
    let n4991: ZW = zw_add(n4989, n4619);
    let n4992: ZW = zw_add(n4990, n4620);
    let n4993: ZW = zw_add(n4991, n4957);
    let n4994: ZW = zw_add(n4992, n4958);
    let n4995: ZW = zw_cellmix_n(407u64, n3014, 1542469173u64);
    let n4996: ZW = zw_cellmix_n(407u64, n3014, 668265263u64);
    let n4997: ZW = zw_add(n4993, n4995);
    let n4998: ZW = zw_add(n4994, n4996);
    let n4999: ZW = zw_add(n4965, n4843);
    let n5000: ZW = zw_add(n4966, n4844);
    let n5001: ZW = zw_add(n4999, n4631);
    let n5002: ZW = zw_add(n5000, n4632);
    let n5003: ZW = zw_add(n5001, n4971);
    let n5004: ZW = zw_add(n5002, n4972);
    let n5005: ZW = zw_cellmix_n(407u64, n3017, 1542469173u64);
    let n5006: ZW = zw_cellmix_n(407u64, n3017, 668265263u64);
    let n5007: ZW = zw_add(n5003, n5005);
    let n5008: ZW = zw_add(n5004, n5006);
    let ok_v0_b0: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b0: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b0: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v1_b1: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b1: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b1: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v2_b2: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b2: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b2: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v16_b3: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b3: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b3: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v17_b4: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b4: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b4: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v18_b5: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b5: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b5: u16 = ALL & zb_holds(n177) & zb_holds(n840);
    let ok_v32_b6: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b6: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b6: u16 = ALL & zb_holds(n840);
    let ok_v33_b7: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b7: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b7: u16 = ALL & zb_holds(n840);
    let ok_v34_b8: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b8: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b8: u16 = ALL & zb_holds(n840);
    let ok_v36_b9: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b9: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b9: u16 = ALL & zb_holds(n840);
    let ok_v37_b10: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b10: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b10: u16 = ALL & zb_holds(n840);
    let ok_v38_b11: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b11: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b11: u16 = ALL & zb_holds(n840);
    let ok_v40_b12: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b12: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b12: u16 = ALL & zb_holds(n840);
    let ok_v41_b13: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b13: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b13: u16 = ALL & zb_holds(n840);
    let ok_v42_b14: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b14: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b14: u16 = ALL & zb_holds(n840);
    let ok_v48_b15: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b15: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b15: u16 = ALL & zb_holds(n840);
    let ok_v49_b16: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b16: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b16: u16 = ALL & zb_holds(n840);
    let ok_v50_b17: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b17: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b17: u16 = ALL & zb_holds(n840);
    let ok_v52_b18: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b18: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b18: u16 = ALL & zb_holds(n840);
    let ok_v53_b19: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b19: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b19: u16 = ALL & zb_holds(n840);
    let ok_v54_b20: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b20: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b20: u16 = ALL & zb_holds(n840);
    let ok_v56_b21: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b21: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b21: u16 = ALL & zb_holds(n840);
    let ok_v57_b22: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b22: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b22: u16 = ALL & zb_holds(n840);
    let ok_v58_b23: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b23: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b23: u16 = ALL & zb_holds(n840);
    let ok_v0_b24: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b24: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b24: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v1_b25: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b25: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b25: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v2_b26: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b26: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b26: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v16_b27: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b27: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b27: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v17_b28: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b28: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b28: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v18_b29: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b29: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b29: u16 = ALL & zb_holds(n177) & zb_holds(n1232);
    let ok_v32_b30: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b30: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b30: u16 = ALL & zb_holds(n1232);
    let ok_v33_b31: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b31: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b31: u16 = ALL & zb_holds(n1232);
    let ok_v34_b32: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b32: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b32: u16 = ALL & zb_holds(n1232);
    let ok_v36_b33: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b33: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b33: u16 = ALL & zb_holds(n1232);
    let ok_v37_b34: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b34: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b34: u16 = ALL & zb_holds(n1232);
    let ok_v38_b35: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b35: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b35: u16 = ALL & zb_holds(n1232);
    let ok_v40_b36: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b36: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b36: u16 = ALL & zb_holds(n1232);
    let ok_v41_b37: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b37: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b37: u16 = ALL & zb_holds(n1232);
    let ok_v42_b38: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b38: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b38: u16 = ALL & zb_holds(n1232);
    let ok_v48_b39: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b39: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b39: u16 = ALL & zb_holds(n1232);
    let ok_v49_b40: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b40: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b40: u16 = ALL & zb_holds(n1232);
    let ok_v50_b41: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b41: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b41: u16 = ALL & zb_holds(n1232);
    let ok_v52_b42: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b42: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b42: u16 = ALL & zb_holds(n1232);
    let ok_v53_b43: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b43: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b43: u16 = ALL & zb_holds(n1232);
    let ok_v54_b44: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b44: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b44: u16 = ALL & zb_holds(n1232);
    let ok_v56_b45: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b45: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b45: u16 = ALL & zb_holds(n1232);
    let ok_v57_b46: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b46: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b46: u16 = ALL & zb_holds(n1232);
    let ok_v58_b47: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b47: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b47: u16 = ALL & zb_holds(n1232);
    let ok_v0_b48: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b48: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b48: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v1_b49: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b49: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b49: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v2_b50: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b50: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b50: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v16_b51: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b51: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b51: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v17_b52: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b52: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b52: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v18_b53: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b53: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b53: u16 = ALL & zb_holds(n177) & zb_holds(n1330);
    let ok_v32_b54: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b54: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b54: u16 = ALL & zb_holds(n1330);
    let ok_v33_b55: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b55: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b55: u16 = ALL & zb_holds(n1330);
    let ok_v34_b56: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b56: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b56: u16 = ALL & zb_holds(n1330);
    let ok_v36_b57: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b57: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b57: u16 = ALL & zb_holds(n1330);
    let ok_v37_b58: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b58: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b58: u16 = ALL & zb_holds(n1330);
    let ok_v38_b59: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b59: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b59: u16 = ALL & zb_holds(n1330);
    let ok_v40_b60: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b60: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b60: u16 = ALL & zb_holds(n1330);
    let ok_v41_b61: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b61: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b61: u16 = ALL & zb_holds(n1330);
    let ok_v42_b62: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b62: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b62: u16 = ALL & zb_holds(n1330);
    let ok_v48_b63: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b63: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b63: u16 = ALL & zb_holds(n1330);
    let ok_v49_b64: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b64: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b64: u16 = ALL & zb_holds(n1330);
    let ok_v50_b65: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b65: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b65: u16 = ALL & zb_holds(n1330);
    let ok_v52_b66: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b66: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b66: u16 = ALL & zb_holds(n1330);
    let ok_v53_b67: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b67: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b67: u16 = ALL & zb_holds(n1330);
    let ok_v54_b68: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b68: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b68: u16 = ALL & zb_holds(n1330);
    let ok_v56_b69: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b69: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b69: u16 = ALL & zb_holds(n1330);
    let ok_v57_b70: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b70: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b70: u16 = ALL & zb_holds(n1330);
    let ok_v58_b71: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b71: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b71: u16 = ALL & zb_holds(n1330);
    let ok_v0_b72: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b72: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b72: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v1_b73: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b73: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b73: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v2_b74: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b74: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b74: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v16_b75: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b75: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b75: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v17_b76: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b76: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b76: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v18_b77: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b77: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b77: u16 = ALL & zb_holds(n177) & zb_holds(n1417);
    let ok_v32_b78: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b78: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b78: u16 = ALL & zb_holds(n1417);
    let ok_v33_b79: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b79: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b79: u16 = ALL & zb_holds(n1417);
    let ok_v34_b80: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b80: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b80: u16 = ALL & zb_holds(n1417);
    let ok_v36_b81: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b81: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b81: u16 = ALL & zb_holds(n1417);
    let ok_v37_b82: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b82: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b82: u16 = ALL & zb_holds(n1417);
    let ok_v38_b83: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b83: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b83: u16 = ALL & zb_holds(n1417);
    let ok_v40_b84: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b84: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b84: u16 = ALL & zb_holds(n1417);
    let ok_v41_b85: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b85: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b85: u16 = ALL & zb_holds(n1417);
    let ok_v42_b86: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b86: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b86: u16 = ALL & zb_holds(n1417);
    let ok_v48_b87: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b87: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b87: u16 = ALL & zb_holds(n1417);
    let ok_v49_b88: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b88: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b88: u16 = ALL & zb_holds(n1417);
    let ok_v50_b89: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b89: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b89: u16 = ALL & zb_holds(n1417);
    let ok_v52_b90: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b90: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b90: u16 = ALL & zb_holds(n1417);
    let ok_v53_b91: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b91: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b91: u16 = ALL & zb_holds(n1417);
    let ok_v54_b92: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b92: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b92: u16 = ALL & zb_holds(n1417);
    let ok_v56_b93: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b93: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b93: u16 = ALL & zb_holds(n1417);
    let ok_v57_b94: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b94: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b94: u16 = ALL & zb_holds(n1417);
    let ok_v58_b95: u16 = ALL & zb_holds(n841) & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b95: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b95: u16 = ALL & zb_holds(n1417);
    let ok_v0_b96: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1483);
    let bd_v0_b96: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b96: u16 = ALL & zb_holds(n1481);
    let ok_v32_b97: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1483);
    let bd_v32_b97: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b97: u16 = ALL & zb_holds(n1481);
    let ok_v0_b98: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1551);
    let bd_v0_b98: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b98: u16 = ALL & zb_holds(n1549);
    let ok_v32_b99: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1551);
    let bd_v32_b99: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b99: u16 = ALL & zb_holds(n1549);
    let ok_v0_b100: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1617);
    let bd_v0_b100: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b100: u16 = ALL & zb_holds(n1615);
    let ok_v32_b101: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1617);
    let bd_v32_b101: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b101: u16 = ALL & zb_holds(n1615);
    let ok_v0_b102: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1683);
    let bd_v0_b102: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b102: u16 = ALL & zb_holds(n1681);
    let ok_v32_b103: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1683);
    let bd_v32_b103: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b103: u16 = ALL & zb_holds(n1681);
    let ok_v0_b104: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v0_b104: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b104: u16 = ALL & zb_holds(n2339) & zb_holds(n177) & zb_holds(n2249) & zb_holds(n2251);
    let ok_v1_b105: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v1_b105: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b105: u16 = ALL & zb_holds(n2339) & zb_holds(n177) & zb_holds(n2249) & zb_holds(n2251);
    let ok_v2_b106: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v2_b106: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b106: u16 = ALL & zb_holds(n2339) & zb_holds(n177) & zb_holds(n2249) & zb_holds(n2251);
    let ok_v16_b107: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v16_b107: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b107: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n177) & zb_holds(n2339);
    let ok_v17_b108: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v17_b108: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b108: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n177) & zb_holds(n2339);
    let ok_v18_b109: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v18_b109: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b109: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n177) & zb_holds(n2339);
    let ok_v32_b110: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v32_b110: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b110: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v33_b111: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v33_b111: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b111: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v34_b112: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v34_b112: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b112: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v36_b113: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v36_b113: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b113: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v37_b114: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v37_b114: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b114: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v38_b115: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v38_b115: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b115: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v40_b116: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v40_b116: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b116: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v41_b117: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v41_b117: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b117: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v42_b118: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v42_b118: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b118: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v48_b119: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v48_b119: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b119: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v49_b120: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v49_b120: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b120: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v50_b121: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v50_b121: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b121: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v52_b122: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v52_b122: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b122: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v53_b123: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v53_b123: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b123: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v54_b124: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v54_b124: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b124: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v56_b125: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v56_b125: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b125: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v57_b126: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v57_b126: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b126: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v58_b127: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2250);
    let bd_v58_b127: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b127: u16 = ALL & zb_holds(n2249) & zb_holds(n2251) & zb_holds(n2339);
    let ok_v0_b128: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2591);
    let bd_v0_b128: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b128: u16 = ALL & zb_holds(n2339) & zb_holds(n2590);
    let ok_v32_b129: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2591);
    let bd_v32_b129: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b129: u16 = ALL & zb_holds(n2339) & zb_holds(n2590);
    let ok_v0_b130: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2759);
    let bd_v0_b130: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b130: u16 = ALL & zb_holds(n2339) & zb_holds(n2758);
    let ok_v32_b131: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2759);
    let bd_v32_b131: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b131: u16 = ALL & zb_holds(n2339) & zb_holds(n2758);
    let ok_v0_b132: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2781);
    let bd_v0_b132: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b132: u16 = ALL & zb_holds(n2777);
    let ok_v32_b133: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2781);
    let bd_v32_b133: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b133: u16 = ALL & zb_holds(n2777);
    let ok_v0_b134: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2793);
    let bd_v0_b134: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b134: u16 = ALL & zb_holds(n2789);
    let ok_v32_b135: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2793);
    let bd_v32_b135: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b135: u16 = ALL & zb_holds(n2789);
    let ok_v0_b136: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v0_b136: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b136: u16 = ALL & zb_holds(n2817);
    let ok_v1_b137: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v1_b137: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b137: u16 = ALL & zb_holds(n2817);
    let ok_v2_b138: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v2_b138: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b138: u16 = ALL & zb_holds(n2817);
    let ok_v16_b139: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v16_b139: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b139: u16 = ALL & zb_holds(n2817);
    let ok_v17_b140: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v17_b140: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b140: u16 = ALL & zb_holds(n2817);
    let ok_v18_b141: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v18_b141: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b141: u16 = ALL & zb_holds(n2817);
    let ok_v32_b142: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v32_b142: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b142: u16 = ALL & zb_holds(n2817);
    let ok_v33_b143: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v33_b143: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b143: u16 = ALL & zb_holds(n2817);
    let ok_v34_b144: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v34_b144: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b144: u16 = ALL & zb_holds(n2817);
    let ok_v36_b145: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v36_b145: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b145: u16 = ALL & zb_holds(n2817);
    let ok_v37_b146: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v37_b146: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b146: u16 = ALL & zb_holds(n2817);
    let ok_v38_b147: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v38_b147: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b147: u16 = ALL & zb_holds(n2817);
    let ok_v40_b148: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v40_b148: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b148: u16 = ALL & zb_holds(n2817);
    let ok_v41_b149: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v41_b149: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b149: u16 = ALL & zb_holds(n2817);
    let ok_v42_b150: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v42_b150: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b150: u16 = ALL & zb_holds(n2817);
    let ok_v48_b151: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v48_b151: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b151: u16 = ALL & zb_holds(n2817);
    let ok_v49_b152: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v49_b152: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b152: u16 = ALL & zb_holds(n2817);
    let ok_v50_b153: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v50_b153: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b153: u16 = ALL & zb_holds(n2817);
    let ok_v52_b154: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v52_b154: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b154: u16 = ALL & zb_holds(n2817);
    let ok_v53_b155: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v53_b155: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b155: u16 = ALL & zb_holds(n2817);
    let ok_v54_b156: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v54_b156: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b156: u16 = ALL & zb_holds(n2817);
    let ok_v56_b157: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v56_b157: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b157: u16 = ALL & zb_holds(n2817);
    let ok_v57_b158: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v57_b158: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b158: u16 = ALL & zb_holds(n2817);
    let ok_v58_b159: u16 = ALL & zb_holds(n239) & zb_holds(n238) & zb_holds(r_c312) & zb_holds(n233) & zb_holds(r_c295) & zb_holds(n175) & zb_holds(n174) & zb_holds(n173) & zb_holds(n172) & zb_holds(n133) & zb_holds(r_c287) & zb_holds(n132) & zb_holds(n131) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(r_c276) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n159) & zb_holds(r_c268) & zb_holds(n158) & zb_holds(n130) & zb_holds(n156) & zb_holds(n155) & zb_holds(n126) & zb_holds(n154) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2818);
    let bd_v58_b159: bool = !n237 || !n236 || !n235 || !n234 || !n171 || !n170 || !n169 || !n168 || !n129 || !n128 || !n157 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b159: u16 = ALL & zb_holds(n2817);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c367: n402,
        c368: n514,
        c301: n513,
        c85: n245,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c367: n402,
        c368: n514,
        c301: n513,
        c85: n245,
    };
    let sh2 = KShared2 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c246: n1235,
        c254: n1240,
        c405: n402,
        c406: n514,
        c318: n513,
        c85: n245,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c246: n1235,
        c254: n1240,
        c405: n402,
        c406: n514,
        c318: n513,
        c85: n245,
    };
    let sh4 = KShared4 {
        c87: n1484,
        c84: n138,
        c86: n244,
        c85: n245,
    };
    let sh5 = KShared5 {
        c87: n1484,
        c84: n138,
        c86: n244,
        c85: n245,
    };
    let sh6 = KShared6 {
        c87: n1484,
        c84: n138,
        c86: n244,
        c245: n1235,
        c253: n1240,
        c85: n245,
    };
    let sh7 = KShared7 {
        c87: n1484,
        c84: n138,
        c86: n244,
        c245: n1235,
        c253: n1240,
        c85: n245,
    };
    let sh8 = KShared8 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c366: n1919,
        c367: n1920,
        c300: n1918,
        c85: n245,
    };
    let sh9 = KShared9 {
        c87: n2595,
        c84: n138,
        c86: n244,
        c85: n245,
    };
    let sh10 = KShared10 {
        c87: n2764,
        c84: n138,
        c86: n244,
        c245: n1235,
        c253: n1240,
        c85: n245,
    };
    let sh11 = KShared11 {
        c87: n2784,
        c39: n2785,
        c84: n138,
        c86: n244,
        c85: n245,
        c38: n2780,
    };
    let sh12 = KShared12 {
        c87: n2796,
        c39: n2797,
        c84: n138,
        c86: n244,
        c85: n245,
        c38: n2792,
    };
    let sh13 = KShared13 {
        c87: r_c87,
        c39: r_c39,
        c84: n138,
        c86: n244,
        c246: n2800,
        c254: n2801,
        c404: n2813,
        c405: n2814,
        c317: n2811,
        c85: n245,
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
    let mut take_1_2: u16 = 0;
    let mut take_1_3: u16 = 0;
    let mut take_1_4: u16 = 0;
    let mut take_1_5: u16 = 0;
    let mut take_1_6: u16 = 0;
    let mut take_1_7: u16 = 0;
    let mut take_1_8: u16 = 0;
    let mut take_1_9: u16 = 0;
    let mut take_1_10: u16 = 0;
    let mut take_1_11: u16 = 0;
    let mut take_1_12: u16 = 0;
    let mut take_1_13: u16 = 0;
    let mut take_1_14: u16 = 0;
    let mut take_1_15: u16 = 0;
    let mut take_1_16: u16 = 0;
    let mut take_1_17: u16 = 0;
    let mut take_1_18: u16 = 0;
    let mut take_1_19: u16 = 0;
    let mut take_1_20: u16 = 0;
    let mut take_1_21: u16 = 0;
    let mut take_1_22: u16 = 0;
    let mut take_1_23: u16 = 0;
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
    let mut take_2_12: u16 = 0;
    let mut take_2_13: u16 = 0;
    let mut take_2_14: u16 = 0;
    let mut take_2_15: u16 = 0;
    let mut take_2_16: u16 = 0;
    let mut take_2_17: u16 = 0;
    let mut take_2_18: u16 = 0;
    let mut take_2_19: u16 = 0;
    let mut take_2_20: u16 = 0;
    let mut take_2_21: u16 = 0;
    let mut take_2_22: u16 = 0;
    let mut take_2_23: u16 = 0;
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
    let mut take_3_14: u16 = 0;
    let mut take_3_15: u16 = 0;
    let mut take_3_16: u16 = 0;
    let mut take_3_17: u16 = 0;
    let mut take_3_18: u16 = 0;
    let mut take_3_19: u16 = 0;
    let mut take_3_20: u16 = 0;
    let mut take_3_21: u16 = 0;
    let mut take_3_22: u16 = 0;
    let mut take_3_23: u16 = 0;
    let mut take_4_0: u16 = 0;
    let mut take_4_1: u16 = 0;
    let mut take_5_0: u16 = 0;
    let mut take_5_1: u16 = 0;
    let mut take_6_0: u16 = 0;
    let mut take_6_1: u16 = 0;
    let mut take_7_0: u16 = 0;
    let mut take_7_1: u16 = 0;
    let mut take_8_0: u16 = 0;
    let mut take_8_1: u16 = 0;
    let mut take_8_2: u16 = 0;
    let mut take_8_3: u16 = 0;
    let mut take_8_4: u16 = 0;
    let mut take_8_5: u16 = 0;
    let mut take_8_6: u16 = 0;
    let mut take_8_7: u16 = 0;
    let mut take_8_8: u16 = 0;
    let mut take_8_9: u16 = 0;
    let mut take_8_10: u16 = 0;
    let mut take_8_11: u16 = 0;
    let mut take_8_12: u16 = 0;
    let mut take_8_13: u16 = 0;
    let mut take_8_14: u16 = 0;
    let mut take_8_15: u16 = 0;
    let mut take_8_16: u16 = 0;
    let mut take_8_17: u16 = 0;
    let mut take_8_18: u16 = 0;
    let mut take_8_19: u16 = 0;
    let mut take_8_20: u16 = 0;
    let mut take_8_21: u16 = 0;
    let mut take_8_22: u16 = 0;
    let mut take_8_23: u16 = 0;
    let mut take_9_0: u16 = 0;
    let mut take_9_1: u16 = 0;
    let mut take_10_0: u16 = 0;
    let mut take_10_1: u16 = 0;
    let mut take_11_0: u16 = 0;
    let mut take_11_1: u16 = 0;
    let mut take_12_0: u16 = 0;
    let mut take_12_1: u16 = 0;
    let mut take_13_0: u16 = 0;
    let mut take_13_1: u16 = 0;
    let mut take_13_2: u16 = 0;
    let mut take_13_3: u16 = 0;
    let mut take_13_4: u16 = 0;
    let mut take_13_5: u16 = 0;
    let mut take_13_6: u16 = 0;
    let mut take_13_7: u16 = 0;
    let mut take_13_8: u16 = 0;
    let mut take_13_9: u16 = 0;
    let mut take_13_10: u16 = 0;
    let mut take_13_11: u16 = 0;
    let mut take_13_12: u16 = 0;
    let mut take_13_13: u16 = 0;
    let mut take_13_14: u16 = 0;
    let mut take_13_15: u16 = 0;
    let mut take_13_16: u16 = 0;
    let mut take_13_17: u16 = 0;
    let mut take_13_18: u16 = 0;
    let mut take_13_19: u16 = 0;
    let mut take_13_20: u16 = 0;
    let mut take_13_21: u16 = 0;
    let mut take_13_22: u16 = 0;
    let mut take_13_23: u16 = 0;
    // 160 distinct button assignments; per outcome they fall
    // into [24, 24, 24, 24, 2, 2, 2, 2, 24, 2, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n919,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n931,
        c370: n921,
        c300: n930,
        h1: n3113, h2: n3114,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n961,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n965,
        c370: n963,
        c300: n930,
        h1: n3125, h2: n3126,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n983,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n987,
        c370: n985,
        c300: n930,
        h1: n3137, h2: n3138,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n919,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n995,
        c370: n993,
        c300: n930,
        h1: n3167, h2: n3168,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n961,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1001,
        c370: n999,
        c300: n930,
        h1: n3177, h2: n3178,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n983,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1007,
        c370: n1005,
        c300: n930,
        h1: n3187, h2: n3188,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1027,
        c281: n1017,
        c359: n1028,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1033,
        c370: n1030,
        c300: n1032,
        h1: n3245, h2: n3246,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1043,
        c370: n1041,
        c300: n1032,
        h1: n3265, h2: n3266,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1051,
        c370: n1049,
        c300: n1032,
        h1: n3281, h2: n3282,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1066,
        c370: n1064,
        c300: n1032,
        h1: n3307, h2: n3308,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1072,
        c370: n1070,
        c300: n1032,
        h1: n3323, h2: n3324,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1078,
        c370: n1076,
        c300: n1032,
        h1: n3337, h2: n3338,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1066,
        c370: n1082,
        c300: n1032,
        h1: n3349, h2: n3350,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1072,
        c370: n1084,
        c300: n1032,
        h1: n3359, h2: n3360,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1078,
        c370: n1086,
        c300: n1032,
        h1: n3369, h2: n3370,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1027,
        c281: n1017,
        c359: n1028,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1092,
        c370: n1090,
        c300: n1032,
        h1: n3395, h2: n3396,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1098,
        c370: n1096,
        c300: n1032,
        h1: n3411, h2: n3412,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1104,
        c370: n1102,
        c300: n1032,
        h1: n3425, h2: n3426,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1110,
        c370: n1108,
        c300: n1032,
        h1: n3443, h2: n3444,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1116,
        c370: n1114,
        c300: n1032,
        h1: n3459, h2: n3460,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1122,
        c370: n1120,
        c300: n1032,
        h1: n3473, h2: n3474,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1110,
        c370: n1124,
        c300: n1032,
        h1: n3483, h2: n3484,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1116,
        c370: n1126,
        c300: n1032,
        h1: n3493, h2: n3494,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1122,
        c370: n1128,
        c300: n1032,
        h1: n3503, h2: n3504,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n919,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n931,
        c370: n921,
        c300: n930,
        h1: n3113, h2: n3114,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_1 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n961,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n965,
        c370: n963,
        c300: n930,
        h1: n3125, h2: n3126,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_2 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n983,
        c286: n845,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n987,
        c370: n985,
        c300: n930,
        h1: n3137, h2: n3138,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_3 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n919,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n995,
        c370: n993,
        c300: n930,
        h1: n3167, h2: n3168,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_4 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n961,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1001,
        c370: n999,
        c300: n930,
        h1: n3177, h2: n3178,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_5 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c394,
        c358: r_c395,
        c281: n259,
        c359: r_c396,
        c360: r_c397,
        c283: n262,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n983,
        c286: n991,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1007,
        c370: n1005,
        c300: n930,
        h1: n3187, h2: n3188,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1027,
        c281: n1017,
        c359: n1028,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1033,
        c370: n1030,
        c300: n1032,
        h1: n3245, h2: n3246,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1043,
        c370: n1041,
        c300: n1032,
        h1: n3265, h2: n3266,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1051,
        c370: n1049,
        c300: n1032,
        h1: n3281, h2: n3282,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1066,
        c370: n1064,
        c300: n1032,
        h1: n3307, h2: n3308,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1072,
        c370: n1070,
        c300: n1032,
        h1: n3323, h2: n3324,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1078,
        c370: n1076,
        c300: n1032,
        h1: n3337, h2: n3338,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1066,
        c370: n1082,
        c300: n1032,
        h1: n3349, h2: n3350,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1072,
        c370: n1084,
        c300: n1032,
        h1: n3359, h2: n3360,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n845,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1078,
        c370: n1086,
        c300: n1032,
        h1: n3369, h2: n3370,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1027,
        c281: n1017,
        c359: n1028,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1092,
        c370: n1090,
        c300: n1032,
        h1: n3395, h2: n3396,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1098,
        c370: n1096,
        c300: n1032,
        h1: n3411, h2: n3412,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1020,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1021,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1104,
        c370: n1102,
        c300: n1032,
        h1: n3425, h2: n3426,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1110,
        c370: n1108,
        c300: n1032,
        h1: n3443, h2: n3444,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1116,
        c370: n1114,
        c300: n1032,
        h1: n3459, h2: n3460,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1056,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1122,
        c370: n1120,
        c300: n1032,
        h1: n3473, h2: n3474,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1061,
        c281: n1017,
        c359: n1062,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n919,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1110,
        c370: n1124,
        c300: n1032,
        h1: n3483, h2: n3484,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1039,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n961,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1116,
        c370: n1126,
        c300: n1032,
        h1: n3493, h2: n3494,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1015,
        c41: n1016,
        c357: n1055,
        c358: n1038,
        c281: n1017,
        c359: n1047,
        c360: n1080,
        c283: n1018,
        c284: n1019,
        c361: n983,
        c286: n991,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1122,
        c370: n1128,
        c300: n1032,
        h1: n3503, h2: n3504,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n919,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n931,
        c408: n921,
        c317: n930,
        h1: n3583, h2: n3584,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b49 & (if bd_v1_b49 { ALL } else { !ok_v1_b49 });
    take_2_1 |= live_v1_b49 & ok_v1_b49 & (if bd_v1_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n961,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n965,
        c408: n963,
        c317: n930,
        h1: n3595, h2: n3596,
    };
    // body 49: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b50 & (if bd_v2_b50 { ALL } else { !ok_v2_b50 });
    take_2_2 |= live_v2_b50 & ok_v2_b50 & (if bd_v2_b50 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n983,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n987,
        c408: n985,
        c317: n930,
        h1: n3607, h2: n3608,
    };
    // body 50: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b51 & (if bd_v16_b51 { ALL } else { !ok_v16_b51 });
    take_2_3 |= live_v16_b51 & ok_v16_b51 & (if bd_v16_b51 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n919,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n995,
        c408: n993,
        c317: n930,
        h1: n3637, h2: n3638,
    };
    // body 51: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b52 & (if bd_v17_b52 { ALL } else { !ok_v17_b52 });
    take_2_4 |= live_v17_b52 & ok_v17_b52 & (if bd_v17_b52 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n961,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n1001,
        c408: n999,
        c317: n930,
        h1: n3647, h2: n3648,
    };
    // body 52: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b53 & (if bd_v18_b53 { ALL } else { !ok_v18_b53 });
    take_2_5 |= live_v18_b53 & ok_v18_b53 & (if bd_v18_b53 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n983,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n1007,
        c408: n1005,
        c317: n930,
        h1: n3657, h2: n3658,
    };
    // body 53: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b54 & (if bd_v32_b54 { ALL } else { !ok_v32_b54 });
    take_2_6 |= live_v32_b54 & ok_v32_b54 & (if bd_v32_b54 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1027,
        c298: n1017,
        c397: n1028,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1033,
        c408: n1030,
        c317: n1032,
        h1: n3711, h2: n3712,
    };
    // body 54: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b55 & (if bd_v33_b55 { ALL } else { !ok_v33_b55 });
    take_2_7 |= live_v33_b55 & ok_v33_b55 & (if bd_v33_b55 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1043,
        c408: n1041,
        c317: n1032,
        h1: n3731, h2: n3732,
    };
    // body 55: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b56 & (if bd_v34_b56 { ALL } else { !ok_v34_b56 });
    take_2_8 |= live_v34_b56 & ok_v34_b56 & (if bd_v34_b56 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1051,
        c408: n1049,
        c317: n1032,
        h1: n3747, h2: n3748,
    };
    // body 56: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v36_b57 & (if bd_v36_b57 { ALL } else { !ok_v36_b57 });
    take_2_9 |= live_v36_b57 & ok_v36_b57 & (if bd_v36_b57 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1066,
        c408: n1064,
        c317: n1032,
        h1: n3773, h2: n3774,
    };
    // body 57: buttons 0x24, forks 0x0
    sink.o2(36, take_2_9, &sh2, &o2);
    declined |= live_v37_b58 & (if bd_v37_b58 { ALL } else { !ok_v37_b58 });
    take_2_10 |= live_v37_b58 & ok_v37_b58 & (if bd_v37_b58 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1072,
        c408: n1070,
        c317: n1032,
        h1: n3789, h2: n3790,
    };
    // body 58: buttons 0x25, forks 0x0
    sink.o2(37, take_2_10, &sh2, &o2);
    declined |= live_v38_b59 & (if bd_v38_b59 { ALL } else { !ok_v38_b59 });
    take_2_11 |= live_v38_b59 & ok_v38_b59 & (if bd_v38_b59 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1078,
        c408: n1076,
        c317: n1032,
        h1: n3803, h2: n3804,
    };
    // body 59: buttons 0x26, forks 0x0
    sink.o2(38, take_2_11, &sh2, &o2);
    declined |= live_v40_b60 & (if bd_v40_b60 { ALL } else { !ok_v40_b60 });
    take_2_12 |= live_v40_b60 & ok_v40_b60 & (if bd_v40_b60 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1066,
        c408: n1082,
        c317: n1032,
        h1: n3815, h2: n3816,
    };
    // body 60: buttons 0x28, forks 0x0
    sink.o2(40, take_2_12, &sh2, &o2);
    declined |= live_v41_b61 & (if bd_v41_b61 { ALL } else { !ok_v41_b61 });
    take_2_13 |= live_v41_b61 & ok_v41_b61 & (if bd_v41_b61 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1072,
        c408: n1084,
        c317: n1032,
        h1: n3825, h2: n3826,
    };
    // body 61: buttons 0x29, forks 0x0
    sink.o2(41, take_2_13, &sh2, &o2);
    declined |= live_v42_b62 & (if bd_v42_b62 { ALL } else { !ok_v42_b62 });
    take_2_14 |= live_v42_b62 & ok_v42_b62 & (if bd_v42_b62 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1078,
        c408: n1086,
        c317: n1032,
        h1: n3835, h2: n3836,
    };
    // body 62: buttons 0x2a, forks 0x0
    sink.o2(42, take_2_14, &sh2, &o2);
    declined |= live_v48_b63 & (if bd_v48_b63 { ALL } else { !ok_v48_b63 });
    take_2_15 |= live_v48_b63 & ok_v48_b63 & (if bd_v48_b63 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1027,
        c298: n1017,
        c397: n1028,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1092,
        c408: n1090,
        c317: n1032,
        h1: n3861, h2: n3862,
    };
    // body 63: buttons 0x30, forks 0x0
    sink.o2(48, take_2_15, &sh2, &o2);
    declined |= live_v49_b64 & (if bd_v49_b64 { ALL } else { !ok_v49_b64 });
    take_2_16 |= live_v49_b64 & ok_v49_b64 & (if bd_v49_b64 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1098,
        c408: n1096,
        c317: n1032,
        h1: n3877, h2: n3878,
    };
    // body 64: buttons 0x31, forks 0x0
    sink.o2(49, take_2_16, &sh2, &o2);
    declined |= live_v50_b65 & (if bd_v50_b65 { ALL } else { !ok_v50_b65 });
    take_2_17 |= live_v50_b65 & ok_v50_b65 & (if bd_v50_b65 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1104,
        c408: n1102,
        c317: n1032,
        h1: n3891, h2: n3892,
    };
    // body 65: buttons 0x32, forks 0x0
    sink.o2(50, take_2_17, &sh2, &o2);
    declined |= live_v52_b66 & (if bd_v52_b66 { ALL } else { !ok_v52_b66 });
    take_2_18 |= live_v52_b66 & ok_v52_b66 & (if bd_v52_b66 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1110,
        c408: n1108,
        c317: n1032,
        h1: n3909, h2: n3910,
    };
    // body 66: buttons 0x34, forks 0x0
    sink.o2(52, take_2_18, &sh2, &o2);
    declined |= live_v53_b67 & (if bd_v53_b67 { ALL } else { !ok_v53_b67 });
    take_2_19 |= live_v53_b67 & ok_v53_b67 & (if bd_v53_b67 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1116,
        c408: n1114,
        c317: n1032,
        h1: n3925, h2: n3926,
    };
    // body 67: buttons 0x35, forks 0x0
    sink.o2(53, take_2_19, &sh2, &o2);
    declined |= live_v54_b68 & (if bd_v54_b68 { ALL } else { !ok_v54_b68 });
    take_2_20 |= live_v54_b68 & ok_v54_b68 & (if bd_v54_b68 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1122,
        c408: n1120,
        c317: n1032,
        h1: n3939, h2: n3940,
    };
    // body 68: buttons 0x36, forks 0x0
    sink.o2(54, take_2_20, &sh2, &o2);
    declined |= live_v56_b69 & (if bd_v56_b69 { ALL } else { !ok_v56_b69 });
    take_2_21 |= live_v56_b69 & ok_v56_b69 & (if bd_v56_b69 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1110,
        c408: n1124,
        c317: n1032,
        h1: n3949, h2: n3950,
    };
    // body 69: buttons 0x38, forks 0x0
    sink.o2(56, take_2_21, &sh2, &o2);
    declined |= live_v57_b70 & (if bd_v57_b70 { ALL } else { !ok_v57_b70 });
    take_2_22 |= live_v57_b70 & ok_v57_b70 & (if bd_v57_b70 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1116,
        c408: n1126,
        c317: n1032,
        h1: n3959, h2: n3960,
    };
    // body 70: buttons 0x39, forks 0x0
    sink.o2(57, take_2_22, &sh2, &o2);
    declined |= live_v58_b71 & (if bd_v58_b71 { ALL } else { !ok_v58_b71 });
    take_2_23 |= live_v58_b71 & ok_v58_b71 & (if bd_v58_b71 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1122,
        c408: n1128,
        c317: n1032,
        h1: n3969, h2: n3970,
    };
    // body 71: buttons 0x3a, forks 0x0
    sink.o2(58, take_2_23, &sh2, &o2);
    declined |= live_v0_b72 & (if bd_v0_b72 { ALL } else { !ok_v0_b72 });
    take_3_0 |= live_v0_b72 & ok_v0_b72 & (if bd_v0_b72 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n919,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n931,
        c408: n921,
        c317: n930,
        h1: n3583, h2: n3584,
    };
    // body 72: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v1_b73 & (if bd_v1_b73 { ALL } else { !ok_v1_b73 });
    take_3_1 |= live_v1_b73 & ok_v1_b73 & (if bd_v1_b73 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n961,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n965,
        c408: n963,
        c317: n930,
        h1: n3595, h2: n3596,
    };
    // body 73: buttons 0x01, forks 0x0
    sink.o3(1, take_3_1, &sh3, &o3);
    declined |= live_v2_b74 & (if bd_v2_b74 { ALL } else { !ok_v2_b74 });
    take_3_2 |= live_v2_b74 & ok_v2_b74 & (if bd_v2_b74 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n983,
        c303: n845,
        c310: zb_splat(false),
        c311: zb_splat(false),
        c407: n987,
        c408: n985,
        c317: n930,
        h1: n3607, h2: n3608,
    };
    // body 74: buttons 0x02, forks 0x0
    sink.o3(2, take_3_2, &sh3, &o3);
    declined |= live_v16_b75 & (if bd_v16_b75 { ALL } else { !ok_v16_b75 });
    take_3_3 |= live_v16_b75 & ok_v16_b75 & (if bd_v16_b75 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n919,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n995,
        c408: n993,
        c317: n930,
        h1: n3637, h2: n3638,
    };
    // body 75: buttons 0x10, forks 0x0
    sink.o3(16, take_3_3, &sh3, &o3);
    declined |= live_v17_b76 & (if bd_v17_b76 { ALL } else { !ok_v17_b76 });
    take_3_4 |= live_v17_b76 & ok_v17_b76 & (if bd_v17_b76 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n961,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n1001,
        c408: n999,
        c317: n930,
        h1: n3647, h2: n3648,
    };
    // body 76: buttons 0x11, forks 0x0
    sink.o3(17, take_3_4, &sh3, &o3);
    declined |= live_v18_b77 & (if bd_v18_b77 { ALL } else { !ok_v18_b77 });
    take_3_5 |= live_v18_b77 & ok_v18_b77 & (if bd_v18_b77 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        c395: r_c394,
        c396: r_c395,
        c298: n259,
        c397: r_c396,
        c398: r_c397,
        c300: n262,
        c301: zn_splat(P8::from_raw(65536i32)),
        c399: n983,
        c303: n991,
        c310: zb_splat(false),
        c311: zb_splat(true),
        c407: n1007,
        c408: n1005,
        c317: n930,
        h1: n3657, h2: n3658,
    };
    // body 77: buttons 0x12, forks 0x0
    sink.o3(18, take_3_5, &sh3, &o3);
    declined |= live_v32_b78 & (if bd_v32_b78 { ALL } else { !ok_v32_b78 });
    take_3_6 |= live_v32_b78 & ok_v32_b78 & (if bd_v32_b78 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1027,
        c298: n1017,
        c397: n1028,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1033,
        c408: n1030,
        c317: n1032,
        h1: n3711, h2: n3712,
    };
    // body 78: buttons 0x20, forks 0x0
    sink.o3(32, take_3_6, &sh3, &o3);
    declined |= live_v33_b79 & (if bd_v33_b79 { ALL } else { !ok_v33_b79 });
    take_3_7 |= live_v33_b79 & ok_v33_b79 & (if bd_v33_b79 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1043,
        c408: n1041,
        c317: n1032,
        h1: n3731, h2: n3732,
    };
    // body 79: buttons 0x21, forks 0x0
    sink.o3(33, take_3_7, &sh3, &o3);
    declined |= live_v34_b80 & (if bd_v34_b80 { ALL } else { !ok_v34_b80 });
    take_3_8 |= live_v34_b80 & ok_v34_b80 & (if bd_v34_b80 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1051,
        c408: n1049,
        c317: n1032,
        h1: n3747, h2: n3748,
    };
    // body 80: buttons 0x22, forks 0x0
    sink.o3(34, take_3_8, &sh3, &o3);
    declined |= live_v36_b81 & (if bd_v36_b81 { ALL } else { !ok_v36_b81 });
    take_3_9 |= live_v36_b81 & ok_v36_b81 & (if bd_v36_b81 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1066,
        c408: n1064,
        c317: n1032,
        h1: n3773, h2: n3774,
    };
    // body 81: buttons 0x24, forks 0x0
    sink.o3(36, take_3_9, &sh3, &o3);
    declined |= live_v37_b82 & (if bd_v37_b82 { ALL } else { !ok_v37_b82 });
    take_3_10 |= live_v37_b82 & ok_v37_b82 & (if bd_v37_b82 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1072,
        c408: n1070,
        c317: n1032,
        h1: n3789, h2: n3790,
    };
    // body 82: buttons 0x25, forks 0x0
    sink.o3(37, take_3_10, &sh3, &o3);
    declined |= live_v38_b83 & (if bd_v38_b83 { ALL } else { !ok_v38_b83 });
    take_3_11 |= live_v38_b83 & ok_v38_b83 & (if bd_v38_b83 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1078,
        c408: n1076,
        c317: n1032,
        h1: n3803, h2: n3804,
    };
    // body 83: buttons 0x26, forks 0x0
    sink.o3(38, take_3_11, &sh3, &o3);
    declined |= live_v40_b84 & (if bd_v40_b84 { ALL } else { !ok_v40_b84 });
    take_3_12 |= live_v40_b84 & ok_v40_b84 & (if bd_v40_b84 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1066,
        c408: n1082,
        c317: n1032,
        h1: n3815, h2: n3816,
    };
    // body 84: buttons 0x28, forks 0x0
    sink.o3(40, take_3_12, &sh3, &o3);
    declined |= live_v41_b85 & (if bd_v41_b85 { ALL } else { !ok_v41_b85 });
    take_3_13 |= live_v41_b85 & ok_v41_b85 & (if bd_v41_b85 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1072,
        c408: n1084,
        c317: n1032,
        h1: n3825, h2: n3826,
    };
    // body 85: buttons 0x29, forks 0x0
    sink.o3(41, take_3_13, &sh3, &o3);
    declined |= live_v42_b86 & (if bd_v42_b86 { ALL } else { !ok_v42_b86 });
    take_3_14 |= live_v42_b86 & ok_v42_b86 & (if bd_v42_b86 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n845,
        c310: zb_splat(true),
        c311: zb_splat(false),
        c407: n1078,
        c408: n1086,
        c317: n1032,
        h1: n3835, h2: n3836,
    };
    // body 86: buttons 0x2a, forks 0x0
    sink.o3(42, take_3_14, &sh3, &o3);
    declined |= live_v48_b87 & (if bd_v48_b87 { ALL } else { !ok_v48_b87 });
    take_3_15 |= live_v48_b87 & ok_v48_b87 & (if bd_v48_b87 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1027,
        c298: n1017,
        c397: n1028,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1092,
        c408: n1090,
        c317: n1032,
        h1: n3861, h2: n3862,
    };
    // body 87: buttons 0x30, forks 0x0
    sink.o3(48, take_3_15, &sh3, &o3);
    declined |= live_v49_b88 & (if bd_v49_b88 { ALL } else { !ok_v49_b88 });
    take_3_16 |= live_v49_b88 & ok_v49_b88 & (if bd_v49_b88 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1098,
        c408: n1096,
        c317: n1032,
        h1: n3877, h2: n3878,
    };
    // body 88: buttons 0x31, forks 0x0
    sink.o3(49, take_3_16, &sh3, &o3);
    declined |= live_v50_b89 & (if bd_v50_b89 { ALL } else { !ok_v50_b89 });
    take_3_17 |= live_v50_b89 & ok_v50_b89 & (if bd_v50_b89 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1020,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1021,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1104,
        c408: n1102,
        c317: n1032,
        h1: n3891, h2: n3892,
    };
    // body 89: buttons 0x32, forks 0x0
    sink.o3(50, take_3_17, &sh3, &o3);
    declined |= live_v52_b90 & (if bd_v52_b90 { ALL } else { !ok_v52_b90 });
    take_3_18 |= live_v52_b90 & ok_v52_b90 & (if bd_v52_b90 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1110,
        c408: n1108,
        c317: n1032,
        h1: n3909, h2: n3910,
    };
    // body 90: buttons 0x34, forks 0x0
    sink.o3(52, take_3_18, &sh3, &o3);
    declined |= live_v53_b91 & (if bd_v53_b91 { ALL } else { !ok_v53_b91 });
    take_3_19 |= live_v53_b91 & ok_v53_b91 & (if bd_v53_b91 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1116,
        c408: n1114,
        c317: n1032,
        h1: n3925, h2: n3926,
    };
    // body 91: buttons 0x35, forks 0x0
    sink.o3(53, take_3_19, &sh3, &o3);
    declined |= live_v54_b92 & (if bd_v54_b92 { ALL } else { !ok_v54_b92 });
    take_3_20 |= live_v54_b92 & ok_v54_b92 & (if bd_v54_b92 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1056,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1122,
        c408: n1120,
        c317: n1032,
        h1: n3939, h2: n3940,
    };
    // body 92: buttons 0x36, forks 0x0
    sink.o3(54, take_3_20, &sh3, &o3);
    declined |= live_v56_b93 & (if bd_v56_b93 { ALL } else { !ok_v56_b93 });
    take_3_21 |= live_v56_b93 & ok_v56_b93 & (if bd_v56_b93 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1061,
        c298: n1017,
        c397: n1062,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n919,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1110,
        c408: n1124,
        c317: n1032,
        h1: n3949, h2: n3950,
    };
    // body 93: buttons 0x38, forks 0x0
    sink.o3(56, take_3_21, &sh3, &o3);
    declined |= live_v57_b94 & (if bd_v57_b94 { ALL } else { !ok_v57_b94 });
    take_3_22 |= live_v57_b94 & ok_v57_b94 & (if bd_v57_b94 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1039,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n961,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1116,
        c408: n1126,
        c317: n1032,
        h1: n3959, h2: n3960,
    };
    // body 94: buttons 0x39, forks 0x0
    sink.o3(57, take_3_22, &sh3, &o3);
    declined |= live_v58_b95 & (if bd_v58_b95 { ALL } else { !ok_v58_b95 });
    take_3_23 |= live_v58_b95 & ok_v58_b95 & (if bd_v58_b95 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1015,
        c41: n1016,
        c395: n1055,
        c396: n1038,
        c298: n1017,
        c397: n1047,
        c398: n1080,
        c300: n1018,
        c301: n1019,
        c399: n983,
        c303: n991,
        c310: zb_splat(true),
        c311: zb_splat(true),
        c407: n1122,
        c408: n1128,
        c317: n1032,
        h1: n3969, h2: n3970,
    };
    // body 95: buttons 0x3a, forks 0x0
    sink.o3(58, take_3_23, &sh3, &o3);
    declined |= live_v0_b96 & (if bd_v0_b96 { ALL } else { !ok_v0_b96 });
    take_4_0 |= live_v0_b96 & ok_v0_b96 & (if bd_v0_b96 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        h1: n3983, h2: n3984,
    };
    // body 96: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v32_b97 & (if bd_v32_b97 { ALL } else { !ok_v32_b97 });
    take_4_1 |= live_v32_b97 & ok_v32_b97 & (if bd_v32_b97 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1015,
        c41: n1016,
        h1: n3987, h2: n3988,
    };
    // body 97: buttons 0x20, forks 0x0
    sink.o4(32, take_4_1, &sh4, &o4);
    declined |= live_v0_b98 & (if bd_v0_b98 { ALL } else { !ok_v0_b98 });
    take_5_0 |= live_v0_b98 & ok_v0_b98 & (if bd_v0_b98 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: r_c20,
        c41: r_c41,
        h1: n3983, h2: n3984,
    };
    // body 98: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v32_b99 & (if bd_v32_b99 { ALL } else { !ok_v32_b99 });
    take_5_1 |= live_v32_b99 & ok_v32_b99 & (if bd_v32_b99 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1015,
        c41: n1016,
        h1: n3987, h2: n3988,
    };
    // body 99: buttons 0x20, forks 0x0
    sink.o5(32, take_5_1, &sh5, &o5);
    declined |= live_v0_b100 & (if bd_v0_b100 { ALL } else { !ok_v0_b100 });
    take_6_0 |= live_v0_b100 & ok_v0_b100 & (if bd_v0_b100 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: r_c20,
        c41: r_c41,
        h1: n3999, h2: n4000,
    };
    // body 100: buttons 0x00, forks 0x0
    sink.o6(0, take_6_0, &sh6, &o6);
    declined |= live_v32_b101 & (if bd_v32_b101 { ALL } else { !ok_v32_b101 });
    take_6_1 |= live_v32_b101 & ok_v32_b101 & (if bd_v32_b101 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n1015,
        c41: n1016,
        h1: n4003, h2: n4004,
    };
    // body 101: buttons 0x20, forks 0x0
    sink.o6(32, take_6_1, &sh6, &o6);
    declined |= live_v0_b102 & (if bd_v0_b102 { ALL } else { !ok_v0_b102 });
    take_7_0 |= live_v0_b102 & ok_v0_b102 & (if bd_v0_b102 { 0 } else { ALL });
    let o7 = KOut7 {
        c20: r_c20,
        c41: r_c41,
        h1: n3999, h2: n4000,
    };
    // body 102: buttons 0x00, forks 0x0
    sink.o7(0, take_7_0, &sh7, &o7);
    declined |= live_v32_b103 & (if bd_v32_b103 { ALL } else { !ok_v32_b103 });
    take_7_1 |= live_v32_b103 & ok_v32_b103 & (if bd_v32_b103 { 0 } else { ALL });
    let o7 = KOut7 {
        c20: n1015,
        c41: n1016,
        h1: n4003, h2: n4004,
    };
    // body 103: buttons 0x20, forks 0x0
    sink.o7(32, take_7_1, &sh7, &o7);
    declined |= live_v0_b104 & (if bd_v0_b104 { ALL } else { !ok_v0_b104 });
    take_8_0 |= live_v0_b104 & ok_v0_b104 & (if bd_v0_b104 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2335,
        c285: n2257,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: n2352,
        c369: n2337,
        c299: n2351,
        h1: n4073, h2: n4074,
    };
    // body 104: buttons 0x00, forks 0x0
    sink.o8(0, take_8_0, &sh8, &o8);
    declined |= live_v1_b105 & (if bd_v1_b105 { ALL } else { !ok_v1_b105 });
    take_8_1 |= live_v1_b105 & ok_v1_b105 & (if bd_v1_b105 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2374,
        c285: n2257,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: n2378,
        c369: n2376,
        c299: n2351,
        h1: n4085, h2: n4086,
    };
    // body 105: buttons 0x01, forks 0x0
    sink.o8(1, take_8_1, &sh8, &o8);
    declined |= live_v2_b106 & (if bd_v2_b106 { ALL } else { !ok_v2_b106 });
    take_8_2 |= live_v2_b106 & ok_v2_b106 & (if bd_v2_b106 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2399,
        c285: n2257,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: n2403,
        c369: n2401,
        c299: n2351,
        h1: n4097, h2: n4098,
    };
    // body 106: buttons 0x02, forks 0x0
    sink.o8(2, take_8_2, &sh8, &o8);
    declined |= live_v16_b107 & (if bd_v16_b107 { ALL } else { !ok_v16_b107 });
    take_8_3 |= live_v16_b107 & ok_v16_b107 & (if bd_v16_b107 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2335,
        c285: n2407,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n2413,
        c369: n2409,
        c299: n2351,
        h1: n4125, h2: n4126,
    };
    // body 107: buttons 0x10, forks 0x0
    sink.o8(16, take_8_3, &sh8, &o8);
    declined |= live_v17_b108 & (if bd_v17_b108 { ALL } else { !ok_v17_b108 });
    take_8_4 |= live_v17_b108 & ok_v17_b108 & (if bd_v17_b108 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2374,
        c285: n2407,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n2419,
        c369: n2417,
        c299: n2351,
        h1: n4135, h2: n4136,
    };
    // body 108: buttons 0x11, forks 0x0
    sink.o8(17, take_8_4, &sh8, &o8);
    declined |= live_v18_b109 & (if bd_v18_b109 { ALL } else { !ok_v18_b109 });
    take_8_5 |= live_v18_b109 & ok_v18_b109 & (if bd_v18_b109 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        c41: r_c41,
        c356: r_c394,
        c357: r_c395,
        c280: n259,
        c358: r_c396,
        c359: r_c397,
        c282: n262,
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: n2399,
        c285: n2407,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n2425,
        c369: n2423,
        c299: n2351,
        h1: n4145, h2: n4146,
    };
    // body 109: buttons 0x12, forks 0x0
    sink.o8(18, take_8_5, &sh8, &o8);
    declined |= live_v32_b110 & (if bd_v32_b110 { ALL } else { !ok_v32_b110 });
    take_8_6 |= live_v32_b110 & ok_v32_b110 & (if bd_v32_b110 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n2430,
        c280: n1017,
        c358: n2431,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2436,
        c369: n2433,
        c299: n2435,
        h1: n4199, h2: n4200,
    };
    // body 110: buttons 0x20, forks 0x0
    sink.o8(32, take_8_6, &sh8, &o8);
    declined |= live_v33_b111 & (if bd_v33_b111 { ALL } else { !ok_v33_b111 });
    take_8_7 |= live_v33_b111 & ok_v33_b111 & (if bd_v33_b111 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2442,
        c369: n2440,
        c299: n2435,
        h1: n4219, h2: n4220,
    };
    // body 111: buttons 0x21, forks 0x0
    sink.o8(33, take_8_7, &sh8, &o8);
    declined |= live_v34_b112 & (if bd_v34_b112 { ALL } else { !ok_v34_b112 });
    take_8_8 |= live_v34_b112 & ok_v34_b112 & (if bd_v34_b112 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2448,
        c369: n2446,
        c299: n2435,
        h1: n4235, h2: n4236,
    };
    // body 112: buttons 0x22, forks 0x0
    sink.o8(34, take_8_8, &sh8, &o8);
    declined |= live_v36_b113 & (if bd_v36_b113 { ALL } else { !ok_v36_b113 });
    take_8_9 |= live_v36_b113 & ok_v36_b113 & (if bd_v36_b113 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1061,
        c280: n1017,
        c358: n1062,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2454,
        c369: n2452,
        c299: n2435,
        h1: n4261, h2: n4262,
    };
    // body 113: buttons 0x24, forks 0x0
    sink.o8(36, take_8_9, &sh8, &o8);
    declined |= live_v37_b114 & (if bd_v37_b114 { ALL } else { !ok_v37_b114 });
    take_8_10 |= live_v37_b114 & ok_v37_b114 & (if bd_v37_b114 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2460,
        c369: n2458,
        c299: n2435,
        h1: n4277, h2: n4278,
    };
    // body 114: buttons 0x25, forks 0x0
    sink.o8(37, take_8_10, &sh8, &o8);
    declined |= live_v38_b115 & (if bd_v38_b115 { ALL } else { !ok_v38_b115 });
    take_8_11 |= live_v38_b115 & ok_v38_b115 & (if bd_v38_b115 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2466,
        c369: n2464,
        c299: n2435,
        h1: n4291, h2: n4292,
    };
    // body 115: buttons 0x26, forks 0x0
    sink.o8(38, take_8_11, &sh8, &o8);
    declined |= live_v40_b116 & (if bd_v40_b116 { ALL } else { !ok_v40_b116 });
    take_8_12 |= live_v40_b116 & ok_v40_b116 & (if bd_v40_b116 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1061,
        c280: n1017,
        c358: n1062,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2454,
        c369: n2468,
        c299: n2435,
        h1: n4303, h2: n4304,
    };
    // body 116: buttons 0x28, forks 0x0
    sink.o8(40, take_8_12, &sh8, &o8);
    declined |= live_v41_b117 & (if bd_v41_b117 { ALL } else { !ok_v41_b117 });
    take_8_13 |= live_v41_b117 & ok_v41_b117 & (if bd_v41_b117 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2460,
        c369: n2470,
        c299: n2435,
        h1: n4313, h2: n4314,
    };
    // body 117: buttons 0x29, forks 0x0
    sink.o8(41, take_8_13, &sh8, &o8);
    declined |= live_v42_b118 & (if bd_v42_b118 { ALL } else { !ok_v42_b118 });
    take_8_14 |= live_v42_b118 & ok_v42_b118 & (if bd_v42_b118 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2257,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: n2466,
        c369: n2472,
        c299: n2435,
        h1: n4323, h2: n4324,
    };
    // body 118: buttons 0x2a, forks 0x0
    sink.o8(42, take_8_14, &sh8, &o8);
    declined |= live_v48_b119 & (if bd_v48_b119 { ALL } else { !ok_v48_b119 });
    take_8_15 |= live_v48_b119 & ok_v48_b119 & (if bd_v48_b119 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n2430,
        c280: n1017,
        c358: n2431,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2478,
        c369: n2476,
        c299: n2435,
        h1: n4349, h2: n4350,
    };
    // body 119: buttons 0x30, forks 0x0
    sink.o8(48, take_8_15, &sh8, &o8);
    declined |= live_v49_b120 & (if bd_v49_b120 { ALL } else { !ok_v49_b120 });
    take_8_16 |= live_v49_b120 & ok_v49_b120 & (if bd_v49_b120 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2484,
        c369: n2482,
        c299: n2435,
        h1: n4365, h2: n4366,
    };
    // body 120: buttons 0x31, forks 0x0
    sink.o8(49, take_8_16, &sh8, &o8);
    declined |= live_v50_b121 & (if bd_v50_b121 { ALL } else { !ok_v50_b121 });
    take_8_17 |= live_v50_b121 & ok_v50_b121 & (if bd_v50_b121 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1020,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1021,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2490,
        c369: n2488,
        c299: n2435,
        h1: n4379, h2: n4380,
    };
    // body 121: buttons 0x32, forks 0x0
    sink.o8(50, take_8_17, &sh8, &o8);
    declined |= live_v52_b122 & (if bd_v52_b122 { ALL } else { !ok_v52_b122 });
    take_8_18 |= live_v52_b122 & ok_v52_b122 & (if bd_v52_b122 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1061,
        c280: n1017,
        c358: n1062,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2496,
        c369: n2494,
        c299: n2435,
        h1: n4397, h2: n4398,
    };
    // body 122: buttons 0x34, forks 0x0
    sink.o8(52, take_8_18, &sh8, &o8);
    declined |= live_v53_b123 & (if bd_v53_b123 { ALL } else { !ok_v53_b123 });
    take_8_19 |= live_v53_b123 & ok_v53_b123 & (if bd_v53_b123 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2502,
        c369: n2500,
        c299: n2435,
        h1: n4413, h2: n4414,
    };
    // body 123: buttons 0x35, forks 0x0
    sink.o8(53, take_8_19, &sh8, &o8);
    declined |= live_v54_b124 & (if bd_v54_b124 { ALL } else { !ok_v54_b124 });
    take_8_20 |= live_v54_b124 & ok_v54_b124 & (if bd_v54_b124 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1056,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2508,
        c369: n2506,
        c299: n2435,
        h1: n4427, h2: n4428,
    };
    // body 124: buttons 0x36, forks 0x0
    sink.o8(54, take_8_20, &sh8, &o8);
    declined |= live_v56_b125 & (if bd_v56_b125 { ALL } else { !ok_v56_b125 });
    take_8_21 |= live_v56_b125 & ok_v56_b125 & (if bd_v56_b125 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1061,
        c280: n1017,
        c358: n1062,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2335,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2496,
        c369: n2510,
        c299: n2435,
        h1: n4437, h2: n4438,
    };
    // body 125: buttons 0x38, forks 0x0
    sink.o8(56, take_8_21, &sh8, &o8);
    declined |= live_v57_b126 & (if bd_v57_b126 { ALL } else { !ok_v57_b126 });
    take_8_22 |= live_v57_b126 & ok_v57_b126 & (if bd_v57_b126 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1039,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2374,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2502,
        c369: n2512,
        c299: n2435,
        h1: n4447, h2: n4448,
    };
    // body 126: buttons 0x39, forks 0x0
    sink.o8(57, take_8_22, &sh8, &o8);
    declined |= live_v58_b127 & (if bd_v58_b127 { ALL } else { !ok_v58_b127 });
    take_8_23 |= live_v58_b127 & ok_v58_b127 & (if bd_v58_b127 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1015,
        c41: n1016,
        c356: n1055,
        c357: n1038,
        c280: n1017,
        c358: n1047,
        c359: n1080,
        c282: n1018,
        c283: n1019,
        c360: n2399,
        c285: n2407,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: n2508,
        c369: n2514,
        c299: n2435,
        h1: n4457, h2: n4458,
    };
    // body 127: buttons 0x3a, forks 0x0
    sink.o8(58, take_8_23, &sh8, &o8);
    declined |= live_v0_b128 & (if bd_v0_b128 { ALL } else { !ok_v0_b128 });
    take_9_0 |= live_v0_b128 & ok_v0_b128 & (if bd_v0_b128 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: r_c20,
        c41: r_c41,
        h1: n4465, h2: n4466,
    };
    // body 128: buttons 0x00, forks 0x0
    sink.o9(0, take_9_0, &sh9, &o9);
    declined |= live_v32_b129 & (if bd_v32_b129 { ALL } else { !ok_v32_b129 });
    take_9_1 |= live_v32_b129 & ok_v32_b129 & (if bd_v32_b129 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n1015,
        c41: n1016,
        h1: n4469, h2: n4470,
    };
    // body 129: buttons 0x20, forks 0x0
    sink.o9(32, take_9_1, &sh9, &o9);
    declined |= live_v0_b130 & (if bd_v0_b130 { ALL } else { !ok_v0_b130 });
    take_10_0 |= live_v0_b130 & ok_v0_b130 & (if bd_v0_b130 { 0 } else { ALL });
    let o10 = KOut10 {
        c20: r_c20,
        c41: r_c41,
        h1: n4481, h2: n4482,
    };
    // body 130: buttons 0x00, forks 0x0
    sink.o10(0, take_10_0, &sh10, &o10);
    declined |= live_v32_b131 & (if bd_v32_b131 { ALL } else { !ok_v32_b131 });
    take_10_1 |= live_v32_b131 & ok_v32_b131 & (if bd_v32_b131 { 0 } else { ALL });
    let o10 = KOut10 {
        c20: n2769,
        c41: n2770,
        h1: n4489, h2: n4490,
    };
    // body 131: buttons 0x20, forks 0x0
    sink.o10(32, take_10_1, &sh10, &o10);
    declined |= live_v0_b132 & (if bd_v0_b132 { ALL } else { !ok_v0_b132 });
    take_11_0 |= live_v0_b132 & ok_v0_b132 & (if bd_v0_b132 { 0 } else { ALL });
    let o11 = KOut11 {
        c20: r_c20,
        h1: n4509, h2: n4510,
    };
    // body 132: buttons 0x00, forks 0x0
    sink.o11(0, take_11_0, &sh11, &o11);
    declined |= live_v32_b133 & (if bd_v32_b133 { ALL } else { !ok_v32_b133 });
    take_11_1 |= live_v32_b133 & ok_v32_b133 & (if bd_v32_b133 { 0 } else { ALL });
    let o11 = KOut11 {
        c20: n2787,
        h1: n4513, h2: n4514,
    };
    // body 133: buttons 0x20, forks 0x0
    sink.o11(32, take_11_1, &sh11, &o11);
    declined |= live_v0_b134 & (if bd_v0_b134 { ALL } else { !ok_v0_b134 });
    take_12_0 |= live_v0_b134 & ok_v0_b134 & (if bd_v0_b134 { 0 } else { ALL });
    let o12 = KOut12 {
        c20: r_c20,
        h1: n4533, h2: n4534,
    };
    // body 134: buttons 0x00, forks 0x0
    sink.o12(0, take_12_0, &sh12, &o12);
    declined |= live_v32_b135 & (if bd_v32_b135 { ALL } else { !ok_v32_b135 });
    take_12_1 |= live_v32_b135 & ok_v32_b135 & (if bd_v32_b135 { 0 } else { ALL });
    let o12 = KOut12 {
        c20: n1015,
        h1: n4535, h2: n4536,
    };
    // body 135: buttons 0x20, forks 0x0
    sink.o12(32, take_12_1, &sh12, &o12);
    declined |= live_v0_b136 & (if bd_v0_b136 { ALL } else { !ok_v0_b136 });
    take_13_0 |= live_v0_b136 & ok_v0_b136 & (if bd_v0_b136 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2812,
        c302: n2809,
        c309: n2802,
        c310: n2803,
        c406: n2828,
        c407: n2816,
        c316: n2827,
        h1: n4617, h2: n4618,
    };
    // body 136: buttons 0x00, forks 0x0
    sink.o13(0, take_13_0, &sh13, &o13);
    declined |= live_v1_b137 & (if bd_v1_b137 { ALL } else { !ok_v1_b137 });
    take_13_1 |= live_v1_b137 & ok_v1_b137 & (if bd_v1_b137 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2830,
        c302: n2809,
        c309: n2802,
        c310: n2803,
        c406: n2834,
        c407: n2832,
        c316: n2827,
        h1: n4629, h2: n4630,
    };
    // body 137: buttons 0x01, forks 0x0
    sink.o13(1, take_13_1, &sh13, &o13);
    declined |= live_v2_b138 & (if bd_v2_b138 { ALL } else { !ok_v2_b138 });
    take_13_2 |= live_v2_b138 & ok_v2_b138 & (if bd_v2_b138 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2835,
        c302: n2809,
        c309: n2802,
        c310: n2803,
        c406: n2839,
        c407: n2837,
        c316: n2827,
        h1: n4641, h2: n4642,
    };
    // body 138: buttons 0x02, forks 0x0
    sink.o13(2, take_13_2, &sh13, &o13);
    declined |= live_v16_b139 & (if bd_v16_b139 { ALL } else { !ok_v16_b139 });
    take_13_3 |= live_v16_b139 & ok_v16_b139 & (if bd_v16_b139 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2812,
        c302: n2841,
        c309: n2802,
        c310: n2840,
        c406: n2845,
        c407: n2843,
        c316: n2827,
        h1: n4671, h2: n4672,
    };
    // body 139: buttons 0x10, forks 0x0
    sink.o13(16, take_13_3, &sh13, &o13);
    declined |= live_v17_b140 & (if bd_v17_b140 { ALL } else { !ok_v17_b140 });
    take_13_4 |= live_v17_b140 & ok_v17_b140 & (if bd_v17_b140 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2830,
        c302: n2841,
        c309: n2802,
        c310: n2840,
        c406: n2849,
        c407: n2847,
        c316: n2827,
        h1: n4681, h2: n4682,
    };
    // body 140: buttons 0x11, forks 0x0
    sink.o13(17, take_13_4, &sh13, &o13);
    declined |= live_v18_b141 & (if bd_v18_b141 { ALL } else { !ok_v18_b141 });
    take_13_5 |= live_v18_b141 & ok_v18_b141 & (if bd_v18_b141 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2805,
        c41: r_c41,
        c394: r_c394,
        c395: r_c395,
        c297: n2806,
        c396: r_c396,
        c397: r_c397,
        c299: n2807,
        c300: n2808,
        c398: n2835,
        c302: n2841,
        c309: n2802,
        c310: n2840,
        c406: n2853,
        c407: n2851,
        c316: n2827,
        h1: n4691, h2: n4692,
    };
    // body 141: buttons 0x12, forks 0x0
    sink.o13(18, take_13_5, &sh13, &o13);
    declined |= live_v32_b142 & (if bd_v32_b142 { ALL } else { !ok_v32_b142 });
    take_13_6 |= live_v32_b142 & ok_v32_b142 & (if bd_v32_b142 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2879,
        c297: n2875,
        c396: n2880,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2887,
        c407: n2883,
        c316: n2886,
        h1: n4749, h2: n4750,
    };
    // body 142: buttons 0x20, forks 0x0
    sink.o13(32, take_13_6, &sh13, &o13);
    declined |= live_v33_b143 & (if bd_v33_b143 { ALL } else { !ok_v33_b143 });
    take_13_7 |= live_v33_b143 & ok_v33_b143 & (if bd_v33_b143 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2901,
        c407: n2899,
        c316: n2886,
        h1: n4769, h2: n4770,
    };
    // body 143: buttons 0x21, forks 0x0
    sink.o13(33, take_13_7, &sh13, &o13);
    declined |= live_v34_b144 & (if bd_v34_b144 { ALL } else { !ok_v34_b144 });
    take_13_8 |= live_v34_b144 & ok_v34_b144 & (if bd_v34_b144 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2912,
        c407: n2910,
        c316: n2886,
        h1: n4785, h2: n4786,
    };
    // body 144: buttons 0x22, forks 0x0
    sink.o13(34, take_13_8, &sh13, &o13);
    declined |= live_v36_b145 & (if bd_v36_b145 { ALL } else { !ok_v36_b145 });
    take_13_9 |= live_v36_b145 & ok_v36_b145 & (if bd_v36_b145 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2926,
        c297: n2875,
        c396: n2927,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2932,
        c407: n2930,
        c316: n2886,
        h1: n4811, h2: n4812,
    };
    // body 145: buttons 0x24, forks 0x0
    sink.o13(36, take_13_9, &sh13, &o13);
    declined |= live_v37_b146 & (if bd_v37_b146 { ALL } else { !ok_v37_b146 });
    take_13_10 |= live_v37_b146 & ok_v37_b146 & (if bd_v37_b146 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2940,
        c407: n2938,
        c316: n2886,
        h1: n4827, h2: n4828,
    };
    // body 146: buttons 0x25, forks 0x0
    sink.o13(37, take_13_10, &sh13, &o13);
    declined |= live_v38_b147 & (if bd_v38_b147 { ALL } else { !ok_v38_b147 });
    take_13_11 |= live_v38_b147 & ok_v38_b147 & (if bd_v38_b147 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2948,
        c407: n2946,
        c316: n2886,
        h1: n4841, h2: n4842,
    };
    // body 147: buttons 0x26, forks 0x0
    sink.o13(38, take_13_11, &sh13, &o13);
    declined |= live_v40_b148 & (if bd_v40_b148 { ALL } else { !ok_v40_b148 });
    take_13_12 |= live_v40_b148 & ok_v40_b148 & (if bd_v40_b148 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2926,
        c297: n2875,
        c396: n2927,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2932,
        c407: n2954,
        c316: n2886,
        h1: n4853, h2: n4854,
    };
    // body 148: buttons 0x28, forks 0x0
    sink.o13(40, take_13_12, &sh13, &o13);
    declined |= live_v41_b149 & (if bd_v41_b149 { ALL } else { !ok_v41_b149 });
    take_13_13 |= live_v41_b149 & ok_v41_b149 & (if bd_v41_b149 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2940,
        c407: n2957,
        c316: n2886,
        h1: n4863, h2: n4864,
    };
    // body 149: buttons 0x29, forks 0x0
    sink.o13(41, take_13_13, &sh13, &o13);
    declined |= live_v42_b150 & (if bd_v42_b150 { ALL } else { !ok_v42_b150 });
    take_13_14 |= live_v42_b150 & ok_v42_b150 & (if bd_v42_b150 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2809,
        c309: n2854,
        c310: n2803,
        c406: n2948,
        c407: n2960,
        c316: n2886,
        h1: n4873, h2: n4874,
    };
    // body 150: buttons 0x2a, forks 0x0
    sink.o13(42, take_13_14, &sh13, &o13);
    declined |= live_v48_b151 & (if bd_v48_b151 { ALL } else { !ok_v48_b151 });
    take_13_15 |= live_v48_b151 & ok_v48_b151 & (if bd_v48_b151 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2879,
        c297: n2875,
        c396: n2880,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n2968,
        c407: n2966,
        c316: n2886,
        h1: n4899, h2: n4900,
    };
    // body 151: buttons 0x30, forks 0x0
    sink.o13(48, take_13_15, &sh13, &o13);
    declined |= live_v49_b152 & (if bd_v49_b152 { ALL } else { !ok_v49_b152 });
    take_13_16 |= live_v49_b152 & ok_v49_b152 & (if bd_v49_b152 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n2976,
        c407: n2974,
        c316: n2886,
        h1: n4915, h2: n4916,
    };
    // body 152: buttons 0x31, forks 0x0
    sink.o13(49, take_13_16, &sh13, &o13);
    declined |= live_v50_b153 & (if bd_v50_b153 { ALL } else { !ok_v50_b153 });
    take_13_17 |= live_v50_b153 & ok_v50_b153 & (if bd_v50_b153 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2878,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2881,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n2984,
        c407: n2982,
        c316: n2886,
        h1: n4929, h2: n4930,
    };
    // body 153: buttons 0x32, forks 0x0
    sink.o13(50, take_13_17, &sh13, &o13);
    declined |= live_v52_b154 & (if bd_v52_b154 { ALL } else { !ok_v52_b154 });
    take_13_18 |= live_v52_b154 & ok_v52_b154 & (if bd_v52_b154 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2926,
        c297: n2875,
        c396: n2927,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n2992,
        c407: n2990,
        c316: n2886,
        h1: n4947, h2: n4948,
    };
    // body 154: buttons 0x34, forks 0x0
    sink.o13(52, take_13_18, &sh13, &o13);
    declined |= live_v53_b155 & (if bd_v53_b155 { ALL } else { !ok_v53_b155 });
    take_13_19 |= live_v53_b155 & ok_v53_b155 & (if bd_v53_b155 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n3000,
        c407: n2998,
        c316: n2886,
        h1: n4963, h2: n4964,
    };
    // body 155: buttons 0x35, forks 0x0
    sink.o13(53, take_13_19, &sh13, &o13);
    declined |= live_v54_b156 & (if bd_v54_b156 { ALL } else { !ok_v54_b156 });
    take_13_20 |= live_v54_b156 & ok_v54_b156 & (if bd_v54_b156 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2928,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n3008,
        c407: n3006,
        c316: n2886,
        h1: n4977, h2: n4978,
    };
    // body 156: buttons 0x36, forks 0x0
    sink.o13(54, take_13_20, &sh13, &o13);
    declined |= live_v56_b157 & (if bd_v56_b157 { ALL } else { !ok_v56_b157 });
    take_13_21 |= live_v56_b157 & ok_v56_b157 & (if bd_v56_b157 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2926,
        c297: n2875,
        c396: n2927,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2812,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n2992,
        c407: n3011,
        c316: n2886,
        h1: n4987, h2: n4988,
    };
    // body 157: buttons 0x38, forks 0x0
    sink.o13(56, take_13_21, &sh13, &o13);
    declined |= live_v57_b158 & (if bd_v57_b158 { ALL } else { !ok_v57_b158 });
    take_13_22 |= live_v57_b158 & ok_v57_b158 & (if bd_v57_b158 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2897,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2830,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n3000,
        c407: n3014,
        c316: n2886,
        h1: n4997, h2: n4998,
    };
    // body 158: buttons 0x39, forks 0x0
    sink.o13(57, take_13_22, &sh13, &o13);
    declined |= live_v58_b159 & (if bd_v58_b159 { ALL } else { !ok_v58_b159 });
    take_13_23 |= live_v58_b159 & ok_v58_b159 & (if bd_v58_b159 { 0 } else { ALL });
    let o13 = KOut13 {
        c20: n2873,
        c41: n2874,
        c394: n2925,
        c395: n2896,
        c297: n2875,
        c396: n2908,
        c397: n2953,
        c299: n2876,
        c300: n2877,
        c398: n2835,
        c302: n2841,
        c309: n2854,
        c310: n2840,
        c406: n3008,
        c407: n3017,
        c316: n2886,
        h1: n5007, h2: n5008,
    };
    // body 159: buttons 0x3a, forks 0x0
    sink.o13(58, take_13_23, &sh13, &o13);
    declined
}
