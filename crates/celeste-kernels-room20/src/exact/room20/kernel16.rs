// GENERATED from a TRACED frame (shape 16). Do not edit.
//
// One input shape, 10 output shapes, 108 distinct button
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
pub const SHAPE: u64 = 1962433466245747040;

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
    pub c367: P8,
    pub c368: P8,
    pub c369: P8,
    pub c370: P8,
    pub c377: P8,
    pub c378: P8,
    pub c379: P8,
    pub c380: P8,
    pub c387: P8,
    pub c388: P8,
    pub c389: P8,
    pub c390: P8,
    pub c401: P8,
    pub c402: P8,
    pub c403: P8,
    pub c404: P8,
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
    pub c365: u16,
    pub c366: u16,
    pub c246: ZN,
    pub c371: ZN,
    pub c372: ZN,
    pub c248: u16,
    pub c373: ZN,
    pub c374: ZN,
    pub c250: ZN,
    pub c251: ZN,
    pub c253: ZN,
    pub c254: ZN,
    pub c257: u16,
    pub c375: u16,
    pub c376: u16,
    pub c259: ZN,
    pub c260: ZN,
    pub c381: ZN,
    pub c382: ZN,
    pub c268: u16,
    pub c383: ZN,
    pub c384: ZN,
    pub c270: ZN,
    pub c272: ZN,
    pub c273: ZN,
    pub c276: u16,
    pub c277: ZN,
    pub c385: u16,
    pub c386: u16,
    pub c279: ZN,
    pub c280: ZN,
    pub c391: ZN,
    pub c392: ZN,
    pub c288: u16,
    pub c393: ZN,
    pub c394: ZN,
    pub c290: ZN,
    pub c292: ZN,
    pub c293: ZN,
    pub c296: u16,
    pub c395: ZN,
    pub c396: ZN,
    pub c298: ZN,
    pub c397: ZN,
    pub c398: ZN,
    pub c300: ZN,
    pub c301: ZN,
    pub c399: u16,
    pub c400: u16,
    pub c303: ZN,
    pub c310: u16,
    pub c311: u16,
    pub c405: ZN,
    pub c406: ZN,
    pub c313: u16,
    pub c407: ZN,
    pub c408: ZN,
    pub c317: ZN,
    pub c318: ZN,
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
    pub c365: u32,
    pub c366: u32,
    pub c246: u32,
    pub c371: u32,
    pub c372: u32,
    pub c248: u32,
    pub c373: u32,
    pub c374: u32,
    pub c250: u32,
    pub c251: u32,
    pub c253: u32,
    pub c254: u32,
    pub c257: u32,
    pub c375: u32,
    pub c376: u32,
    pub c259: u32,
    pub c260: u32,
    pub c381: u32,
    pub c382: u32,
    pub c268: u32,
    pub c383: u32,
    pub c384: u32,
    pub c270: u32,
    pub c272: u32,
    pub c273: u32,
    pub c276: u32,
    pub c277: u32,
    pub c385: u32,
    pub c386: u32,
    pub c279: u32,
    pub c280: u32,
    pub c391: u32,
    pub c392: u32,
    pub c288: u32,
    pub c393: u32,
    pub c394: u32,
    pub c290: u32,
    pub c292: u32,
    pub c293: u32,
    pub c296: u32,
    pub c395: u32,
    pub c396: u32,
    pub c298: u32,
    pub c397: u32,
    pub c398: u32,
    pub c300: u32,
    pub c301: u32,
    pub c399: u32,
    pub c400: u32,
    pub c303: u32,
    pub c310: u32,
    pub c311: u32,
    pub c405: u32,
    pub c406: u32,
    pub c313: u32,
    pub c407: u32,
    pub c408: u32,
    pub c317: u32,
    pub c318: u32,
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
        c367: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c368: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c369: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c370: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c377: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c378: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c379: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c380: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c387: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c388: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c389: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c390: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c401: match &b.cols[cell("objects[3].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c402: match &b.cols[cell("objects[3].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c403: match &b.cols[cell("objects[3].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c404: match &b.cols[cell("objects[3].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c365: cell("objects[0].flip.x")?,
        c366: cell("objects[0].flip.y")?,
        c246: cell("objects[0].off")?,
        c371: cell("objects[0].rem.x")?,
        c372: cell("objects[0].rem.y")?,
        c248: cell("objects[0].solids")?,
        c373: cell("objects[0].spd.x")?,
        c374: cell("objects[0].spd.y")?,
        c250: cell("objects[0].spr")?,
        c251: cell("objects[0].start")?,
        c253: cell("objects[0].x")?,
        c254: cell("objects[0].y")?,
        c257: cell("objects[1].collideable")?,
        c375: cell("objects[1].flip.x")?,
        c376: cell("objects[1].flip.y")?,
        c259: cell("objects[1].hide_for")?,
        c260: cell("objects[1].hide_in")?,
        c381: cell("objects[1].rem.x")?,
        c382: cell("objects[1].rem.y")?,
        c268: cell("objects[1].solids")?,
        c383: cell("objects[1].spd.x")?,
        c384: cell("objects[1].spd.y")?,
        c270: cell("objects[1].spr")?,
        c272: cell("objects[1].x")?,
        c273: cell("objects[1].y")?,
        c276: cell("objects[2].collideable")?,
        c277: cell("objects[2].delay")?,
        c385: cell("objects[2].flip.x")?,
        c386: cell("objects[2].flip.y")?,
        c279: cell("objects[2].hide_for")?,
        c280: cell("objects[2].hide_in")?,
        c391: cell("objects[2].rem.x")?,
        c392: cell("objects[2].rem.y")?,
        c288: cell("objects[2].solids")?,
        c393: cell("objects[2].spd.x")?,
        c394: cell("objects[2].spd.y")?,
        c290: cell("objects[2].spr")?,
        c292: cell("objects[2].x")?,
        c293: cell("objects[2].y")?,
        c296: cell("objects[3].collideable")?,
        c395: cell("objects[3].dash_accel.x")?,
        c396: cell("objects[3].dash_accel.y")?,
        c298: cell("objects[3].dash_effect_time")?,
        c397: cell("objects[3].dash_target.x")?,
        c398: cell("objects[3].dash_target.y")?,
        c300: cell("objects[3].dash_time")?,
        c301: cell("objects[3].djump")?,
        c399: cell("objects[3].flip.x")?,
        c400: cell("objects[3].flip.y")?,
        c303: cell("objects[3].grace")?,
        c310: cell("objects[3].p_dash")?,
        c311: cell("objects[3].p_jump")?,
        c405: cell("objects[3].rem.x")?,
        c406: cell("objects[3].rem.y")?,
        c313: cell("objects[3].solids")?,
        c407: cell("objects[3].spd.x")?,
        c408: cell("objects[3].spd.y")?,
        c317: cell("objects[3].x")?,
        c318: cell("objects[3].y")?,
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
        c246: match &b.cols[s.c246 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c371: match &b.cols[s.c371 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c372: match &b.cols[s.c372 as usize] {
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
        c373: match &b.cols[s.c373 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c374: match &b.cols[s.c374 as usize] {
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
        c381: match &b.cols[s.c381 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c382: match &b.cols[s.c382 as usize] {
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
        c383: match &b.cols[s.c383 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c384: match &b.cols[s.c384 as usize] {
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
        c277: match &b.cols[s.c277 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c391: match &b.cols[s.c391 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c392: match &b.cols[s.c392 as usize] {
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
        c393: match &b.cols[s.c393 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c394: match &b.cols[s.c394 as usize] {
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
        c395: match &b.cols[s.c395 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c396: match &b.cols[s.c396 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c298: match &b.cols[s.c298 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c397: match &b.cols[s.c397 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c398: match &b.cols[s.c398 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c300: match &b.cols[s.c300 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c301: match &b.cols[s.c301 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c303: match &b.cols[s.c303 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
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
        c405: match &b.cols[s.c405 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c406: match &b.cols[s.c406 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c313: match &b.cols[s.c313 as usize] {
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
        c407: match &b.cols[s.c407 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c408: match &b.cols[s.c408 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c317: match &b.cols[s.c317 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c318: match &b.cols[s.c318 as usize] {
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
    pub c246: ZN,
    pub c254: ZN,
    pub c278: ZN,
    pub c291: ZN,
    pub c406: ZN,
    pub c407: ZN,
    pub c319: ZN,
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

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
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

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c260: ZN,
    pub c273: ZN,
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

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c245: ZN,
    pub c253: ZN,
    pub c277: ZN,
    pub c290: ZN,
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
    pub c260: ZN,
    pub c273: ZN,
    pub c367: ZN,
    pub c368: ZN,
    pub c301: ZN,
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
    pub c259: ZN,
    pub c272: ZN,
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
    pub c276: ZN,
    pub c289: ZN,
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

/// Outcome 7's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared7 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
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
    pub c85: ZN,
    pub c38: ZB,
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

/// Outcome 9's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared9 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c246: ZN,
    pub c254: ZN,
    pub c277: ZN,
    pub c290: ZN,
    pub c405: ZN,
    pub c406: ZN,
    pub c318: ZN,
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
    b.cols[254] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
pub const KPART1_0: u64 = 13077461461730657904;
pub const KPART2_0: u64 = 9938928850316090955;

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
    b.cols[258] = Col::U(AV::Num(P8::from_raw(655360i32)));
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
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
pub const KPART1_1: u64 = 1492315489879319740;
pub const KPART2_1: u64 = 424812375752515420;

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
        if let Col::N(v) = &mut acc.cols[246] { v.push(sh.c246.lane(i)); }
        if let Col::N(v) = &mut acc.cols[254] { v.push(sh.c254.lane(i)); }
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

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
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
    b.cols[253] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
pub const KPART1_2: u64 = 16618994386668658147;
pub const KPART2_2: u64 = 5090439915145293908;

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
        if let Col::N(v) = &mut acc.cols[260] { v.push(sh.c260.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
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
    b.cols[257] = Col::U(AV::Num(P8::from_raw(655360i32)));
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
    b.cols[270] = Col::U(AV::Num(P8::from_raw(1245184i32)));
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
pub const KPART1_3: u64 = 11220106180808458446;
pub const KPART2_3: u64 = 9688989490377742291;

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
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[290] { v.push(sh.c290.lane(i)); }
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
    b.cols[260] = Col::N(Vec::new());
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
    b.cols[273] = Col::N(Vec::new());
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
pub const KPART1_4: u64 = 9304807789196445260;
pub const KPART2_4: u64 = 1427079830798055268;

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
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[260] { v.push(sh.c260.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
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
    b.cols[259] = Col::N(Vec::new());
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
    b.cols[272] = Col::N(Vec::new());
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
pub const KPART1_5: u64 = 9216004371733693402;
pub const KPART2_5: u64 = 17364244640132226714;

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
        if let Col::N(v) = &mut acc.cols[259] { v.push(sh.c259.lane(i)); }
        if let Col::N(v) = &mut acc.cols[272] { v.push(sh.c272.lane(i)); }
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
    b.cols[276] = Col::N(Vec::new());
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
    b.cols[289] = Col::N(Vec::new());
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
pub const KPART1_6: u64 = 15809050007735389447;
pub const KPART2_6: u64 = 2379528079316838283;

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
        if let Col::N(v) = &mut acc.cols[276] { v.push(sh.c276.lane(i)); }
        if let Col::N(v) = &mut acc.cols[289] { v.push(sh.c289.lane(i)); }
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
pub const KPART1_7: u64 = 7538238740480024082;
pub const KPART2_7: u64 = 8274342854545347923;

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

/// An EMPTY accumulator with outcome 8's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append8`.
pub fn acc8(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_8, OUT_GLOBALS_8, OUT_PTRS_8, 0, cart, cache);
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
pub const KPART1_8: u64 = 16651243698737751054;
pub const KPART2_8: u64 = 14372757396449290073;

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

/// An EMPTY accumulator with outcome 9's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append9`.
pub fn acc9(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_9, OUT_GLOBALS_9, OUT_PTRS_9, 0, cart, cache);
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
    b.cols[277] = Col::N(Vec::new());
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
    b.cols[290] = Col::N(Vec::new());
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
pub const KPART1_9: u64 = 13080660387806388623;
pub const KPART2_9: u64 = 6325318273100992337;

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
        if let Col::N(v) = &mut acc.cols[277] { v.push(sh.c277.lane(i)); }
        if let Col::N(v) = &mut acc.cols[290] { v.push(sh.c290.lane(i)); }
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

pub const OUTCOMES: usize = 10;

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
        _ => panic!("outcome {} of 10", i),
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
        _ => panic!("outcome {} of 10", i),
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
    let r_c277: ZN = rin.c277;
    let r_c279: ZN = rin.c279;
    let r_c280: ZN = rin.c280;
    let r_c288: ZB = ZB { val: rin.c288, known: ALL };
    let r_c290: ZN = rin.c290;
    let r_c292: ZN = rin.c292;
    let r_c293: ZN = rin.c293;
    let r_c296: ZB = ZB { val: rin.c296, known: ALL };
    let r_c298: ZN = rin.c298;
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c303: ZN = rin.c303;
    let r_c310: ZB = ZB { val: rin.c310, known: ALL };
    let r_c311: ZB = ZB { val: rin.c311, known: ALL };
    let r_c313: ZB = ZB { val: rin.c313, known: ALL };
    let r_c317: ZN = rin.c317;
    let r_c318: ZN = rin.c318;
    let r_c365: ZB = ZB { val: rin.c365, known: ALL };
    let r_c366: ZB = ZB { val: rin.c366, known: ALL };
    let r_c371: ZN = rin.c371;
    let r_c372: ZN = rin.c372;
    let r_c373: ZN = rin.c373;
    let r_c374: ZN = rin.c374;
    let r_c375: ZB = ZB { val: rin.c375, known: ALL };
    let r_c376: ZB = ZB { val: rin.c376, known: ALL };
    let r_c381: ZN = rin.c381;
    let r_c382: ZN = rin.c382;
    let r_c383: ZN = rin.c383;
    let r_c384: ZN = rin.c384;
    let r_c385: ZB = ZB { val: rin.c385, known: ALL };
    let r_c386: ZB = ZB { val: rin.c386, known: ALL };
    let r_c391: ZN = rin.c391;
    let r_c392: ZN = rin.c392;
    let r_c393: ZN = rin.c393;
    let r_c394: ZN = rin.c394;
    let r_c395: ZN = rin.c395;
    let r_c396: ZN = rin.c396;
    let r_c397: ZN = rin.c397;
    let r_c398: ZN = rin.c398;
    let r_c399: ZB = ZB { val: rin.c399, known: ALL };
    let r_c400: ZB = ZB { val: rin.c400, known: ALL };
    let r_c405: ZN = rin.c405;
    let r_c406: ZN = rin.c406;
    let r_c407: ZN = rin.c407;
    let r_c408: ZN = rin.c408;
    let n111: ZB = zb_not(r_c42);
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n113: ZB = zb_not(r_c365);
    let n114: ZB = zb_not(r_c366);
    let n115: bool = P8::from_raw(524288i32) == u.c367;
    let n116: bool = P8::from_raw(524288i32) == u.c368;
    let n117: bool = P8::from_raw(0i32) == u.c369;
    let n118: bool = P8::from_raw(0i32) == u.c370;
    let n119: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c371);
    let n120: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c372);
    let n121: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c373);
    let n122: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c374);
    let n123: ZB = zn_eq(zn_splat(P8::from_raw(1703936i32)), r_c250);
    let n124: ZB = zn_eq(zn_splat(P8::from_raw(3145728i32)), r_c251);
    let n125: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c253);
    let n126: ZB = zb_not(r_c376);
    let n127: bool = P8::from_raw(524288i32) == u.c377;
    let n128: bool = P8::from_raw(0i32) == u.c379;
    let n129: bool = P8::from_raw(0i32) == u.c380;
    let n130: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c381);
    let n131: ZB = zb_not(r_c386);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c280);
    let n133: bool = P8::from_raw(524288i32) == u.c388;
    let n134: bool = P8::from_raw(0i32) == u.c390;
    let n135: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c391);
    let n136: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c392);
    let n137: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c394);
    let n141: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n142: ZN = zn_rem(n141, zn_splat(P8::from_raw(1966080i32)));
    let n143: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n142);
    let n158: ZB = zb_not(r_c375);
    let n159: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c259);
    let n160: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c260);
    let n161: bool = P8::from_raw(524288i32) == u.c378;
    let n162: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c382);
    let n163: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c383);
    let n164: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c384);
    let n165: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c270);
    let n166: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c272);
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c273);
    let n168: ZB = zb_not(r_c385);
    let n169: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c279);
    let n170: bool = P8::from_raw(524288i32) == u.c387;
    let n171: bool = P8::from_raw(0i32) == u.c389;
    let n172: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c393);
    let n173: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n174: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n214: ZN = zn_add(zn_splat(P8::from_raw(0i32)), r_c254);
    let n215: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n214);
    let n216: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n214);
    let n217: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n216);
    let n218: ZB = zb_not(r_c310);
    let n223: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c292);
    let n224: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c293);
    let n238: ZB = zn_gt(r_c277, zn_splat(P8::from_raw(0i32)));
    let n239: ZN = zn_sub(r_c277, zn_splat(P8::from_raw(65536i32)));
    let n240: ZB = zn_le(n239, zn_splat(P8::from_raw(0i32)));
    let n241: ZN = zsel_n(n238, n239, r_c277);
    let n244: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c290);
    let n245: ZB = zb_not(n244);
    let n246: ZN = zsel_n(n240, zn_splat(P8::from_raw(1179648i32)), r_c290);
    let n247: ZN = zsel_n(n238, n246, r_c290);
    let n249: ZB = zb_not(r_c400);
    let n250: bool = P8::from_raw(327680i32) == u.c401;
    let n251: bool = P8::from_raw(393216i32) == u.c402;
    let n252: bool = P8::from_raw(65536i32) == u.c403;
    let n253: bool = P8::from_raw(196608i32) == u.c404;
    let n254: ZB = zb_not(r_c43);
    let n255: ZB = zb_not(r_c38);
    let n256: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n257: ZN = zn_rem(n256, zn_splat(P8::from_raw(3932160i32)));
    let n258: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n257);
    let n259: ZN = zsel_n(n258, n173, r_c86);
    let n260: ZN = zsel_n(n143, n259, r_c86);
    let n261: ZN = zsel_n(n143, n257, r_c85);
    let n262: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c317);
    let n263: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n262);
    let n264: ZB = zn_gt(n263, zn_splat(P8::from_raw(524288i32)));
    let n265: ZB = zb_and(n174, n264);
    let n266: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c318);
    let n267: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n266);
    let n268: ZB = zn_gt(n267, n215);
    let n269: ZB = zb_and(n264, n268);
    let n270: ZB = zn_lt(n262, zn_splat(P8::from_raw(1048576i32)));
    let n271: ZB = zb_and(n269, n270);
    let n272: ZB = zn_lt(n266, n217);
    let n273: ZB = zb_and(n271, n272);
    let n274: ZB = zn_gt(n263, zn_splat(P8::from_raw(2621440i32)));
    let n275: ZB = zn_gt(n267, zn_splat(P8::from_raw(7340032i32)));
    let n276: ZB = zb_and(n265, n268);
    let n277: ZB = zb_and(n270, n276);
    let n278: ZB = zb_and(n272, n277);
    let n279: ZB = zb_and(n274, n275);
    let n280: ZB = zn_lt(n262, zn_splat(P8::from_raw(3145728i32)));
    let n281: ZB = zb_and(n279, n280);
    let n282: ZB = zn_lt(n266, zn_splat(P8::from_raw(7864320i32)));
    let n283: ZB = zb_and(n281, n282);
    let n284: ZB = zb_and(n278, n283);
    let n285: ZB = zn_ge(r_c408, zn_splat(P8::from_raw(0i32)));
    let n286: ZN = zn_mul(r_c407, zn_splat(P8::from_raw(13107i32)));
    let n287: ZN = zn_add(r_c405, n286);
    let n288: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n287);
    let n289: ZN = zn_flr(n288);
    let n290: ZN = zn_sub(n288, zn_splat(P8::from_raw(32768i32)));
    let n291: ZN = zn_sub(n290, n289);
    let n292: ZB = zn_gt(n289, zn_splat(P8::from_raw(0i32)));
    let n293: ZB = zn_lt(n289, zn_splat(P8::from_raw(0i32)));
    let n294: ZN = zsel_n(n293, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n295: ZN = zsel_n(n292, zn_splat(P8::from_raw(65536i32)), n294);
    let n296: ZN = zn_abs(n289);
    let n297: ZN = zn_add(n262, n295);
    let n298: ZB = zn_tile_flag_at(g.cache, g.cart, n297, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n299: ZN = zn_add(r_c317, n295);
    let n300: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n296);
    let n301: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n299);
    let n302: ZN = zn_add(n295, n301);
    let n303: ZB = zn_tile_flag_at(g.cache, g.cart, n302, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n304: ZN = zn_add(n295, n299);
    let n305: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n296);
    let n306: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n304);
    let n307: ZN = zn_add(n295, n306);
    let n308: ZB = zn_tile_flag_at(g.cache, g.cart, n307, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n309: ZN = zn_add(n295, n304);
    let n310: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n296);
    let n311: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n309);
    let n312: ZN = zn_add(n295, n311);
    let n313: ZB = zn_tile_flag_at(g.cache, g.cart, n312, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n314: ZN = zn_add(n295, n309);
    let n315: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n296);
    let n316: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n314);
    let n317: ZN = zn_add(n295, n316);
    let n318: ZB = zn_tile_flag_at(g.cache, g.cart, n317, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n319: ZN = zn_add(n295, n314);
    let n320: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n296);
    let n321: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n319);
    let n322: ZN = zn_add(n295, n321);
    let n323: ZB = zn_tile_flag_at(g.cache, g.cart, n322, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n324: ZN = zn_add(n295, n319);
    let n325: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n296);
    let n326: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n324);
    let n327: ZN = zn_add(n295, n326);
    let n328: ZB = zn_tile_flag_at(g.cache, g.cart, n327, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n329: ZN = zn_add(n295, n324);
    let n330: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n296);
    let n331: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n329);
    let n332: ZN = zn_add(n295, n331);
    let n333: ZB = zn_tile_flag_at(g.cache, g.cart, n332, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n334: ZN = zn_add(n295, n329);
    let n335: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n296);
    let n336: ZN = zsel_n(n333, n329, n334);
    let n337: ZN = zsel_n(n333, zn_splat(P8::from_raw(0i32)), n291);
    let n338: ZN = zsel_n(n333, zn_splat(P8::from_raw(0i32)), n286);
    let n339: ZB = zb_or(n333, n335);
    let n340: ZN = zsel_n(n330, n329, n336);
    let n341: ZN = zsel_n(n330, n291, n337);
    let n342: ZN = zsel_n(n330, n286, n338);
    let n343: ZB = zb_or(n330, n339);
    let n344: ZN = zsel_n(n328, n324, n340);
    let n345: ZN = zsel_n(n328, zn_splat(P8::from_raw(0i32)), n341);
    let n346: ZN = zsel_n(n328, zn_splat(P8::from_raw(0i32)), n342);
    let n347: ZB = zb_or(n328, n343);
    let n348: ZN = zsel_n(n325, n324, n344);
    let n349: ZN = zsel_n(n325, n291, n345);
    let n350: ZN = zsel_n(n325, n286, n346);
    let n351: ZB = zb_or(n325, n347);
    let n352: ZN = zsel_n(n323, n319, n348);
    let n353: ZN = zsel_n(n323, zn_splat(P8::from_raw(0i32)), n349);
    let n354: ZN = zsel_n(n323, zn_splat(P8::from_raw(0i32)), n350);
    let n355: ZB = zb_or(n323, n351);
    let n356: ZN = zsel_n(n320, n319, n352);
    let n357: ZN = zsel_n(n320, n291, n353);
    let n358: ZN = zsel_n(n320, n286, n354);
    let n359: ZB = zb_or(n320, n355);
    let n360: ZN = zsel_n(n318, n314, n356);
    let n361: ZN = zsel_n(n318, zn_splat(P8::from_raw(0i32)), n357);
    let n362: ZN = zsel_n(n318, zn_splat(P8::from_raw(0i32)), n358);
    let n363: ZB = zb_or(n318, n359);
    let n364: ZN = zsel_n(n315, n314, n360);
    let n365: ZN = zsel_n(n315, n291, n361);
    let n366: ZN = zsel_n(n315, n286, n362);
    let n367: ZB = zb_or(n315, n363);
    let n368: ZN = zsel_n(n313, n309, n364);
    let n369: ZN = zsel_n(n313, zn_splat(P8::from_raw(0i32)), n365);
    let n370: ZN = zsel_n(n313, zn_splat(P8::from_raw(0i32)), n366);
    let n371: ZB = zb_or(n313, n367);
    let n372: ZN = zsel_n(n310, n309, n368);
    let n373: ZN = zsel_n(n310, n291, n369);
    let n374: ZN = zsel_n(n310, n286, n370);
    let n375: ZB = zb_or(n310, n371);
    let n376: ZN = zsel_n(n308, n304, n372);
    let n377: ZN = zsel_n(n308, zn_splat(P8::from_raw(0i32)), n373);
    let n378: ZN = zsel_n(n308, zn_splat(P8::from_raw(0i32)), n374);
    let n379: ZB = zb_or(n308, n375);
    let n380: ZN = zsel_n(n305, n304, n376);
    let n381: ZN = zsel_n(n305, n291, n377);
    let n382: ZN = zsel_n(n305, n286, n378);
    let n383: ZB = zb_or(n305, n379);
    let n384: ZN = zsel_n(n303, n299, n380);
    let n385: ZN = zsel_n(n303, zn_splat(P8::from_raw(0i32)), n381);
    let n386: ZN = zsel_n(n303, zn_splat(P8::from_raw(0i32)), n382);
    let n387: ZB = zb_or(n303, n383);
    let n388: ZN = zsel_n(n300, n299, n384);
    let n389: ZN = zsel_n(n300, n291, n385);
    let n390: ZN = zsel_n(n300, n286, n386);
    let n391: ZB = zb_or(n300, n387);
    let n392: ZN = zsel_n(n298, r_c317, n388);
    let n393: ZN = zsel_n(n298, zn_splat(P8::from_raw(0i32)), n389);
    let n394: ZN = zsel_n(n298, zn_splat(P8::from_raw(0i32)), n390);
    let n395: ZB = zb_or(n298, n391);
    let n396: ZN = zn_add(r_c406, zn_splat(P8::from_raw(-196608i32)));
    let n397: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n396);
    let n398: ZN = zn_flr(n397);
    let n399: ZN = zn_sub(n397, zn_splat(P8::from_raw(32768i32)));
    let n400: ZN = zn_sub(n399, n398);
    let n401: ZB = zn_gt(n398, zn_splat(P8::from_raw(0i32)));
    let n402: ZB = zn_lt(n398, zn_splat(P8::from_raw(0i32)));
    let n403: ZN = zsel_n(n402, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n404: ZN = zsel_n(n401, zn_splat(P8::from_raw(65536i32)), n403);
    let n405: ZN = zn_abs(n398);
    let n406: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n392);
    let n407: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n406);
    let n408: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n404);
    let n409: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n408, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n410: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n404);
    let n411: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n405);
    let n412: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n410);
    let n413: ZN = zn_add(n404, n412);
    let n414: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n413, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n415: ZN = zn_add(n404, n410);
    let n416: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n405);
    let n417: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n415);
    let n418: ZN = zn_add(n404, n417);
    let n419: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n418, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n420: ZN = zn_add(n404, n415);
    let n421: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n405);
    let n422: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n420);
    let n423: ZN = zn_add(n404, n422);
    let n424: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n423, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n425: ZN = zn_add(n404, n420);
    let n426: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n405);
    let n427: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n425);
    let n428: ZN = zn_add(n404, n427);
    let n429: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n428, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n430: ZN = zn_add(n404, n425);
    let n431: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n405);
    let n432: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n430);
    let n433: ZN = zn_add(n404, n432);
    let n434: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n433, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n435: ZN = zn_add(n404, n430);
    let n436: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n405);
    let n437: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n435);
    let n438: ZN = zn_add(n404, n437);
    let n439: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n438, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n440: ZN = zn_add(n404, n435);
    let n441: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n405);
    let n442: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n440);
    let n443: ZN = zn_add(n404, n442);
    let n444: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n443, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n445: ZN = zn_add(n404, n440);
    let n446: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n405);
    let n447: ZB = zb_and(n395, n446);
    let n448: ZN = zsel_n(n444, n440, n445);
    let n449: ZN = zsel_n(n444, zn_splat(P8::from_raw(0i32)), n400);
    let n450: ZN = zsel_n(n444, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n451: ZB = zsel_b(n444, n395, n447);
    let n452: ZN = zsel_n(n441, n440, n448);
    let n453: ZN = zsel_n(n441, n400, n449);
    let n454: ZN = zsel_n(n441, zn_splat(P8::from_raw(-196608i32)), n450);
    let n455: ZB = zsel_b(n441, n395, n451);
    let n456: ZN = zsel_n(n439, n435, n452);
    let n457: ZN = zsel_n(n439, zn_splat(P8::from_raw(0i32)), n453);
    let n458: ZN = zsel_n(n439, zn_splat(P8::from_raw(0i32)), n454);
    let n459: ZB = zsel_b(n439, n395, n455);
    let n460: ZN = zsel_n(n436, n435, n456);
    let n461: ZN = zsel_n(n436, n400, n457);
    let n462: ZN = zsel_n(n436, zn_splat(P8::from_raw(-196608i32)), n458);
    let n463: ZB = zsel_b(n436, n395, n459);
    let n464: ZN = zsel_n(n434, n430, n460);
    let n465: ZN = zsel_n(n434, zn_splat(P8::from_raw(0i32)), n461);
    let n466: ZN = zsel_n(n434, zn_splat(P8::from_raw(0i32)), n462);
    let n467: ZB = zsel_b(n434, n395, n463);
    let n468: ZN = zsel_n(n431, n430, n464);
    let n469: ZN = zsel_n(n431, n400, n465);
    let n470: ZN = zsel_n(n431, zn_splat(P8::from_raw(-196608i32)), n466);
    let n471: ZB = zsel_b(n431, n395, n467);
    let n472: ZN = zsel_n(n429, n425, n468);
    let n473: ZN = zsel_n(n429, zn_splat(P8::from_raw(0i32)), n469);
    let n474: ZN = zsel_n(n429, zn_splat(P8::from_raw(0i32)), n470);
    let n475: ZB = zsel_b(n429, n395, n471);
    let n476: ZN = zsel_n(n426, n425, n472);
    let n477: ZN = zsel_n(n426, n400, n473);
    let n478: ZN = zsel_n(n426, zn_splat(P8::from_raw(-196608i32)), n474);
    let n479: ZB = zsel_b(n426, n395, n475);
    let n480: ZN = zsel_n(n424, n420, n476);
    let n481: ZN = zsel_n(n424, zn_splat(P8::from_raw(0i32)), n477);
    let n482: ZN = zsel_n(n424, zn_splat(P8::from_raw(0i32)), n478);
    let n483: ZB = zsel_b(n424, n395, n479);
    let n484: ZN = zsel_n(n421, n420, n480);
    let n485: ZN = zsel_n(n421, n400, n481);
    let n486: ZN = zsel_n(n421, zn_splat(P8::from_raw(-196608i32)), n482);
    let n487: ZB = zsel_b(n421, n395, n483);
    let n488: ZN = zsel_n(n419, n415, n484);
    let n489: ZN = zsel_n(n419, zn_splat(P8::from_raw(0i32)), n485);
    let n490: ZN = zsel_n(n419, zn_splat(P8::from_raw(0i32)), n486);
    let n491: ZB = zsel_b(n419, n395, n487);
    let n492: ZN = zsel_n(n416, n415, n488);
    let n493: ZN = zsel_n(n416, n400, n489);
    let n494: ZN = zsel_n(n416, zn_splat(P8::from_raw(-196608i32)), n490);
    let n495: ZB = zsel_b(n416, n395, n491);
    let n496: ZN = zsel_n(n414, n410, n492);
    let n497: ZN = zsel_n(n414, zn_splat(P8::from_raw(0i32)), n493);
    let n498: ZN = zsel_n(n414, zn_splat(P8::from_raw(0i32)), n494);
    let n499: ZB = zsel_b(n414, n395, n495);
    let n500: ZN = zsel_n(n411, n410, n496);
    let n501: ZN = zsel_n(n411, n400, n497);
    let n502: ZN = zsel_n(n411, zn_splat(P8::from_raw(-196608i32)), n498);
    let n503: ZB = zsel_b(n411, n395, n499);
    let n504: ZN = zsel_n(n409, zn_splat(P8::from_raw(7077888i32)), n500);
    let n505: ZN = zsel_n(n409, zn_splat(P8::from_raw(0i32)), n501);
    let n506: ZN = zsel_n(n409, zn_splat(P8::from_raw(0i32)), n502);
    let n507: ZB = zsel_b(n409, n395, n503);
    let n508: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n504);
    let n509: ZN = zn_div(n406, zn_splat(P8::from_raw(524288i32)));
    let n510: ZN = zn_flr(n509);
    let n511: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n510);
    let n512: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n406);
    let n513: ZN = zn_sub(n512, zn_splat(P8::from_raw(65536i32)));
    let n514: ZN = zn_div(n513, zn_splat(P8::from_raw(524288i32)));
    let n515: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n514);
    let n516: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n511);
    let n517: ZB = zn_le(n516, n515);
    let n518: ZB = zn_gt(n516, n515);
    let n519: ZN = zn_div(n508, zn_splat(P8::from_raw(524288i32)));
    let n520: ZN = zn_flr(n519);
    let n521: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n520);
    let n522: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n508);
    let n523: ZN = zn_sub(n522, zn_splat(P8::from_raw(65536i32)));
    let n524: ZN = zn_div(n523, zn_splat(P8::from_raw(524288i32)));
    let n525: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n524);
    let n526: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n521);
    let n527: ZB = zn_le(n526, n525);
    let n528: ZB = zn_gt(n526, n525);
    let n529: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n516);
    let n530: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n526);
    let n531: ZN = zn_mget(g.cart, n529, n530);
    let n532: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n531);
    let n533: ZN = zn_rem(n523, zn_splat(P8::from_raw(524288i32)));
    let n534: ZB = zn_ge(n533, zn_splat(P8::from_raw(393216i32)));
    let n535: ZN = zn_mul(n526, zn_splat(P8::from_raw(524288i32)));
    let n536: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n535);
    let n537: ZB = zn_eq(n522, n536);
    let n538: ZB = zb_or(n534, n537);
    let n539: ZB = zb_and(n532, n538);
    let n540: ZB = zn_ge(n506, zn_splat(P8::from_raw(0i32)));
    let n541: ZB = zb_and(n539, n540);
    let n542: ZB = zb_not(n541);
    let n543: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n531);
    let n544: ZN = zn_rem(n508, zn_splat(P8::from_raw(524288i32)));
    let n545: ZB = zn_le(n544, zn_splat(P8::from_raw(131072i32)));
    let n546: ZB = zb_and(n543, n545);
    let n547: ZB = zb_not(n546);
    let n548: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n531);
    let n549: ZN = zn_rem(n406, zn_splat(P8::from_raw(524288i32)));
    let n550: ZB = zn_le(n549, zn_splat(P8::from_raw(131072i32)));
    let n551: ZB = zb_and(n548, n550);
    let n552: ZB = zn_le(n394, zn_splat(P8::from_raw(0i32)));
    let n553: ZB = zb_and(n551, n552);
    let n554: ZB = zb_not(n553);
    let n555: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n531);
    let n556: ZN = zn_rem(n513, zn_splat(P8::from_raw(524288i32)));
    let n557: ZB = zn_ge(n556, zn_splat(P8::from_raw(393216i32)));
    let n558: ZN = zn_mul(n516, zn_splat(P8::from_raw(524288i32)));
    let n559: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n558);
    let n560: ZB = zn_eq(n512, n559);
    let n561: ZB = zb_or(n557, n560);
    let n562: ZB = zb_and(n555, n561);
    let n563: ZB = zn_ge(n394, zn_splat(P8::from_raw(0i32)));
    let n564: ZB = zb_and(n562, n563);
    let n565: ZB = zb_not(n564);
    let n566: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n521);
    let n567: ZB = zn_le(n566, n525);
    let n568: ZB = zn_gt(n566, n525);
    let n569: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n566);
    let n570: ZN = zn_mget(g.cart, n529, n569);
    let n571: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n570);
    let n572: ZN = zn_mul(n566, zn_splat(P8::from_raw(524288i32)));
    let n573: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n572);
    let n574: ZB = zn_eq(n522, n573);
    let n575: ZB = zb_or(n534, n574);
    let n576: ZB = zb_and(n571, n575);
    let n577: ZB = zb_and(n540, n576);
    let n578: ZB = zb_not(n577);
    let n579: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n570);
    let n580: ZB = zb_and(n545, n579);
    let n581: ZB = zb_not(n580);
    let n582: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n570);
    let n583: ZB = zb_and(n550, n582);
    let n584: ZB = zb_and(n552, n583);
    let n585: ZB = zb_not(n584);
    let n586: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n570);
    let n587: ZB = zb_and(n561, n586);
    let n588: ZB = zb_and(n563, n587);
    let n589: ZB = zb_not(n588);
    let n590: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n521);
    let n591: ZB = zn_le(n590, n525);
    let n592: ZB = zn_gt(n590, n525);
    let n593: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n590);
    let n594: ZN = zn_mget(g.cart, n529, n593);
    let n595: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n594);
    let n596: ZN = zn_mul(n590, zn_splat(P8::from_raw(524288i32)));
    let n597: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n596);
    let n598: ZB = zn_eq(n522, n597);
    let n599: ZB = zb_or(n534, n598);
    let n600: ZB = zb_and(n595, n599);
    let n601: ZB = zb_and(n540, n600);
    let n602: ZB = zb_not(n601);
    let n603: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n594);
    let n604: ZB = zb_and(n545, n603);
    let n605: ZB = zb_not(n604);
    let n606: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n594);
    let n607: ZB = zb_and(n550, n606);
    let n608: ZB = zb_and(n552, n607);
    let n609: ZB = zb_not(n608);
    let n610: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n594);
    let n611: ZB = zb_and(n561, n610);
    let n612: ZB = zb_and(n563, n611);
    let n613: ZB = zb_not(n612);
    let n614: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n521);
    let n615: ZB = zn_gt(n614, n525);
    let n616: ZB = zb_and(n507, n615);
    let n617: ZB = zsel_b(n592, n507, n616);
    let n618: ZB = zsel_b(n568, n507, n617);
    let n619: ZB = zsel_b(n528, n507, n618);
    let n620: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n511);
    let n621: ZB = zn_le(n620, n515);
    let n622: ZB = zn_gt(n620, n515);
    let n623: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n620);
    let n624: ZN = zn_mget(g.cart, n623, n530);
    let n625: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n624);
    let n626: ZB = zb_and(n538, n625);
    let n627: ZB = zb_and(n540, n626);
    let n628: ZB = zb_not(n627);
    let n629: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n624);
    let n630: ZB = zb_and(n545, n629);
    let n631: ZB = zb_not(n630);
    let n632: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n624);
    let n633: ZB = zb_and(n550, n632);
    let n634: ZB = zb_and(n552, n633);
    let n635: ZB = zb_not(n634);
    let n636: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n624);
    let n637: ZN = zn_mul(n620, zn_splat(P8::from_raw(524288i32)));
    let n638: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n637);
    let n639: ZB = zn_eq(n512, n638);
    let n640: ZB = zb_or(n557, n639);
    let n641: ZB = zb_and(n636, n640);
    let n642: ZB = zb_and(n563, n641);
    let n643: ZB = zb_not(n642);
    let n644: ZN = zn_mget(g.cart, n623, n569);
    let n645: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n644);
    let n646: ZB = zb_and(n575, n645);
    let n647: ZB = zb_and(n540, n646);
    let n648: ZB = zb_not(n647);
    let n649: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n644);
    let n650: ZB = zb_and(n545, n649);
    let n651: ZB = zb_not(n650);
    let n652: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n644);
    let n653: ZB = zb_and(n550, n652);
    let n654: ZB = zb_and(n552, n653);
    let n655: ZB = zb_not(n654);
    let n656: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n644);
    let n657: ZB = zb_and(n640, n656);
    let n658: ZB = zb_and(n563, n657);
    let n659: ZB = zb_not(n658);
    let n660: ZN = zn_mget(g.cart, n623, n593);
    let n661: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n660);
    let n662: ZB = zb_and(n599, n661);
    let n663: ZB = zb_and(n540, n662);
    let n664: ZB = zb_not(n663);
    let n665: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n660);
    let n666: ZB = zb_and(n545, n665);
    let n667: ZB = zb_not(n666);
    let n668: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n660);
    let n669: ZB = zb_and(n550, n668);
    let n670: ZB = zb_and(n552, n669);
    let n671: ZB = zb_not(n670);
    let n672: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n660);
    let n673: ZB = zb_and(n640, n672);
    let n674: ZB = zb_and(n563, n673);
    let n675: ZB = zb_not(n674);
    let n676: ZB = zb_and(n615, n619);
    let n677: ZB = zsel_b(n592, n619, n676);
    let n678: ZB = zsel_b(n568, n619, n677);
    let n679: ZB = zsel_b(n528, n619, n678);
    let n680: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n511);
    let n681: ZB = zn_le(n680, n515);
    let n682: ZB = zn_gt(n680, n515);
    let n683: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n680);
    let n684: ZN = zn_mget(g.cart, n683, n530);
    let n685: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n684);
    let n686: ZB = zb_and(n538, n685);
    let n687: ZB = zb_and(n540, n686);
    let n688: ZB = zb_not(n687);
    let n689: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n684);
    let n690: ZB = zb_and(n545, n689);
    let n691: ZB = zb_not(n690);
    let n692: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n684);
    let n693: ZB = zb_and(n550, n692);
    let n694: ZB = zb_and(n552, n693);
    let n695: ZB = zb_not(n694);
    let n696: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n684);
    let n697: ZN = zn_mul(n680, zn_splat(P8::from_raw(524288i32)));
    let n698: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n697);
    let n699: ZB = zn_eq(n512, n698);
    let n700: ZB = zb_or(n557, n699);
    let n701: ZB = zb_and(n696, n700);
    let n702: ZB = zb_and(n563, n701);
    let n703: ZB = zb_not(n702);
    let n704: ZN = zn_mget(g.cart, n683, n569);
    let n705: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n704);
    let n706: ZB = zb_and(n575, n705);
    let n707: ZB = zb_and(n540, n706);
    let n708: ZB = zb_not(n707);
    let n709: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n704);
    let n710: ZB = zb_and(n545, n709);
    let n711: ZB = zb_not(n710);
    let n712: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n704);
    let n713: ZB = zb_and(n550, n712);
    let n714: ZB = zb_and(n552, n713);
    let n715: ZB = zb_not(n714);
    let n716: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n704);
    let n717: ZB = zb_and(n700, n716);
    let n718: ZB = zb_and(n563, n717);
    let n719: ZB = zb_not(n718);
    let n720: ZN = zn_mget(g.cart, n683, n593);
    let n721: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n720);
    let n722: ZB = zb_and(n599, n721);
    let n723: ZB = zb_and(n540, n722);
    let n724: ZB = zb_not(n723);
    let n725: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n720);
    let n726: ZB = zb_and(n545, n725);
    let n727: ZB = zb_not(n726);
    let n728: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n720);
    let n729: ZB = zb_and(n550, n728);
    let n730: ZB = zb_and(n552, n729);
    let n731: ZB = zb_not(n730);
    let n732: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n720);
    let n733: ZB = zb_and(n700, n732);
    let n734: ZB = zb_and(n563, n733);
    let n735: ZB = zb_not(n734);
    let n736: ZB = zb_and(n615, n679);
    let n737: ZB = zsel_b(n592, n679, n736);
    let n738: ZB = zsel_b(n568, n679, n737);
    let n739: ZB = zsel_b(n528, n679, n738);
    let n740: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n511);
    let n741: ZB = zn_gt(n740, n515);
    let n742: ZB = zb_and(n739, n741);
    let n743: ZB = zsel_b(n682, n679, n742);
    let n744: ZB = zsel_b(n622, n619, n743);
    let n745: ZB = zsel_b(n518, n507, n744);
    let n746: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n508);
    let n747: ZB = zn_tile_flag_at(g.cache, g.cart, n407, n746, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n748: ZB = zb_not(n747);
    let n749: ZB = zb_not(r_c311);
    let n750: ZB = zn_gt(r_c303, zn_splat(P8::from_raw(0i32)));
    let n751: ZN = zn_sub(r_c303, zn_splat(P8::from_raw(65536i32)));
    let n752: ZN = zsel_n(n750, n751, r_c303);
    let n753: ZN = zsel_n(n747, zn_splat(P8::from_raw(393216i32)), n752);
    let n754: ZN = zn_sub(r_c298, zn_splat(P8::from_raw(65536i32)));
    let n755: ZB = zn_gt(r_c300, zn_splat(P8::from_raw(0i32)));
    let n756: ZN = zn_sub(r_c300, zn_splat(P8::from_raw(65536i32)));
    let n757: ZB = zn_gt(n394, r_c397);
    let n758: ZN = zn_sub(n394, r_c395);
    let n759: ZN = zn_max(r_c397, n758);
    let n760: ZN = zn_add(r_c395, n394);
    let n761: ZN = zn_min(r_c397, n760);
    let n762: ZN = zsel_n(n757, n759, n761);
    let n763: ZB = zn_gt(n506, r_c398);
    let n764: ZN = zn_sub(n506, r_c396);
    let n765: ZN = zn_max(r_c398, n764);
    let n766: ZN = zn_add(r_c396, n506);
    let n767: ZN = zn_min(r_c398, n766);
    let n768: ZN = zsel_n(n763, n765, n767);
    let n769: ZN = zsel_n(n748, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n770: ZN = zn_abs(n394);
    let n771: ZB = zn_gt(n770, zn_splat(P8::from_raw(65536i32)));
    let n772: ZB = zn_gt(n394, zn_splat(P8::from_raw(0i32)));
    let n773: ZB = zn_lt(n394, zn_splat(P8::from_raw(0i32)));
    let n774: ZB = zn_gt(n394, zn_splat(P8::from_raw(65536i32)));
    let n775: ZN = zn_sub(n394, zn_splat(P8::from_raw(9830i32)));
    let n776: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n775);
    let n777: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n394);
    let n778: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n777);
    let n779: ZB = zn_gt(n394, zn_splat(P8::from_raw(-65536i32)));
    let n780: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n775);
    let n781: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n777);
    let n782: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n775);
    let n783: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n777);
    let n784: ZN = zsel_n(n779, n780, n781);
    let n785: ZN = zsel_n(n772, n782, n783);
    let n786: ZN = zsel_n(n774, n776, n778);
    let n787: ZN = zsel_n(n773, n784, n785);
    let n788: ZN = zsel_n(n772, n786, n787);
    let n789: ZN = zn_sub(n394, n769);
    let n790: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n789);
    let n791: ZN = zn_add(n394, n769);
    let n792: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n791);
    let n793: ZN = zsel_n(n772, n790, n792);
    let n794: ZN = zsel_n(n771, n788, n793);
    let n795: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n794);
    let n796: ZB = zb_not(n795);
    let n797: ZB = zn_lt(n794, zn_splat(P8::from_raw(0i32)));
    let n798: ZB = zsel_b(n796, n797, r_c399);
    let n799: ZN = zn_abs(n506);
    let n800: ZB = zn_le(n799, zn_splat(P8::from_raw(9830i32)));
    let n801: ZN = zsel_n(n800, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n802: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n508);
    let n803: ZN = zn_add(n506, n801);
    let n804: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n803);
    let n805: ZN = zsel_n(n748, n804, n506);
    let n806: ZB = zn_gt(n753, zn_splat(P8::from_raw(0i32)));
    let n807: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n406);
    let n808: ZB = zn_tile_flag_at(g.cache, g.cart, n807, n802, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n809: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n406);
    let n810: ZB = zn_tile_flag_at(g.cache, g.cart, n809, n802, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n811: ZN = zsel_n(n810, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n812: ZN = zsel_n(n808, zn_splat(P8::from_raw(-65536i32)), n811);
    let n813: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n812);
    let n814: ZB = zb_not(n813);
    let n815: ZN = zn_neg(n812);
    let n816: ZN = zn_mul(n815, zn_splat(P8::from_raw(131072i32)));
    let n817: ZN = zsel_n(n814, n816, n794);
    let n818: ZN = zsel_n(n814, zn_splat(P8::from_raw(-131072i32)), n805);
    let n819: ZN = zsel_n(n806, zn_splat(P8::from_raw(0i32)), n753);
    let n820: ZN = zsel_n(n806, n794, n817);
    let n821: ZN = zsel_n(n806, zn_splat(P8::from_raw(-131072i32)), n818);
    let n822: ZN = zsel_n(n798, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n823: ZB = zn_gt(n822, zn_splat(P8::from_raw(0i32)));
    let n824: ZB = zn_lt(n822, zn_splat(P8::from_raw(0i32)));
    let n825: ZN = zsel_n(n824, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n826: ZN = zsel_n(n823, zn_splat(P8::from_raw(131072i32)), n825);
    let n827: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n822);
    let n828: ZB = zb_not(n827);
    let n829: ZN = zsel_n(n828, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n830: ZN = zsel_n(n755, n756, r_c300);
    let n831: ZB = zsel_b(n755, r_c399, n798);
    let n832: ZN = zsel_n(n755, n762, n794);
    let n833: ZN = zsel_n(n755, n768, n805);
    let n834: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n835: ZB = zn_lt(n392, zn_splat(P8::from_raw(-65536i32)));
    let n836: ZB = zn_gt(n392, zn_splat(P8::from_raw(7929856i32)));
    let n837: ZB = zb_or(n835, n836);
    let n838: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n392);
    let n839: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n838);
    let n840: ZN = zsel_n(n837, n839, n392);
    let n841: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n832);
    let n842: ZN = zsel_n(n834, n392, n840);
    let n843: ZN = zsel_n(n834, n832, n841);
    let n844: ZB = zn_gt(n263, zn_splat(P8::from_raw(6815744i32)));
    let n845: ZB = zn_lt(n262, zn_splat(P8::from_raw(7340032i32)));
    let n846: ZN = zsel_n(n245, n241, r_c277);
    let n847: ZN = zsel_n(n245, n247, r_c290);
    let n848: ZB = zb_and(n274, n278);
    let n849: ZB = zb_and(n275, n848);
    let n850: ZB = zb_and(n280, n849);
    let n851: ZB = zb_and(n282, n850);
    let n852: ZB = zb_and(n285, n851);
    let n853: ZB = zb_and(n517, n852);
    let n854: ZB = zb_and(n518, n852);
    let n855: ZB = zb_and(n528, n853);
    let n856: ZB = zb_and(n517, n527);
    let n857: ZB = zb_and(n852, n856);
    let n858: ZB = zb_and(n542, n857);
    let n859: ZB = zb_and(n546, n858);
    let n860: ZB = zb_and(n547, n858);
    let n861: ZB = zb_and(n554, n860);
    let n862: ZB = zb_and(n565, n861);
    let n863: ZB = zb_and(n568, n862);
    let n864: ZB = zb_and(n565, n567);
    let n865: ZB = zb_and(n861, n864);
    let n866: ZB = zb_and(n578, n865);
    let n867: ZB = zb_and(n580, n866);
    let n868: ZB = zb_and(n581, n866);
    let n869: ZB = zb_and(n585, n868);
    let n870: ZB = zb_and(n589, n869);
    let n871: ZB = zb_and(n592, n870);
    let n872: ZB = zb_and(n589, n591);
    let n873: ZB = zb_and(n869, n872);
    let n874: ZB = zb_and(n602, n873);
    let n875: ZB = zb_and(n604, n874);
    let n876: ZB = zb_and(n605, n874);
    let n877: ZB = zb_and(n609, n876);
    let n878: ZB = zb_and(n613, n877);
    let n879: ZB = zb_or(n871, n878);
    let n880: ZB = zb_or(n863, n879);
    let n881: ZB = zb_or(n855, n880);
    let n882: ZB = zb_and(n621, n881);
    let n883: ZB = zb_and(n622, n881);
    let n884: ZB = zb_and(n528, n882);
    let n885: ZB = zb_and(n527, n621);
    let n886: ZB = zb_and(n881, n885);
    let n887: ZB = zb_and(n628, n886);
    let n888: ZB = zb_and(n630, n887);
    let n889: ZB = zb_and(n631, n887);
    let n890: ZB = zb_and(n635, n889);
    let n891: ZB = zb_and(n643, n890);
    let n892: ZB = zb_and(n568, n891);
    let n893: ZB = zb_and(n567, n643);
    let n894: ZB = zb_and(n890, n893);
    let n895: ZB = zb_and(n648, n894);
    let n896: ZB = zb_and(n650, n895);
    let n897: ZB = zb_and(n651, n895);
    let n898: ZB = zb_and(n655, n897);
    let n899: ZB = zb_and(n659, n898);
    let n900: ZB = zb_and(n592, n899);
    let n901: ZB = zb_and(n591, n659);
    let n902: ZB = zb_and(n898, n901);
    let n903: ZB = zb_and(n664, n902);
    let n904: ZB = zb_and(n666, n903);
    let n905: ZB = zb_and(n667, n903);
    let n906: ZB = zb_and(n671, n905);
    let n907: ZB = zb_and(n675, n906);
    let n908: ZB = zb_or(n900, n907);
    let n909: ZB = zb_or(n892, n908);
    let n910: ZB = zb_or(n884, n909);
    let n911: ZB = zb_and(n681, n910);
    let n912: ZB = zb_and(n682, n910);
    let n913: ZB = zb_and(n528, n911);
    let n914: ZB = zb_and(n527, n681);
    let n915: ZB = zb_and(n910, n914);
    let n916: ZB = zb_and(n688, n915);
    let n917: ZB = zb_and(n690, n916);
    let n918: ZB = zb_and(n691, n916);
    let n919: ZB = zb_and(n695, n918);
    let n920: ZB = zb_and(n703, n919);
    let n921: ZB = zb_and(n568, n920);
    let n922: ZB = zb_and(n567, n703);
    let n923: ZB = zb_and(n919, n922);
    let n924: ZB = zb_and(n708, n923);
    let n925: ZB = zb_and(n710, n924);
    let n926: ZB = zb_and(n711, n924);
    let n927: ZB = zb_and(n715, n926);
    let n928: ZB = zb_and(n719, n927);
    let n929: ZB = zb_and(n592, n928);
    let n930: ZB = zb_and(n591, n719);
    let n931: ZB = zb_and(n927, n930);
    let n932: ZB = zb_and(n724, n931);
    let n933: ZB = zb_and(n726, n932);
    let n934: ZB = zb_and(n727, n932);
    let n935: ZB = zb_and(n731, n934);
    let n936: ZB = zb_and(n735, n935);
    let n937: ZB = zb_or(n929, n936);
    let n938: ZB = zb_or(n921, n937);
    let n939: ZB = zb_or(n913, n938);
    let n940: ZB = zb_or(n912, n939);
    let n941: ZB = zb_or(n883, n940);
    let n942: ZB = zb_or(n854, n941);
    let n956: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n789);
    let n957: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n791);
    let n958: ZN = zsel_n(n779, n956, n957);
    let n959: ZN = zsel_n(n771, n788, n958);
    let n960: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n959);
    let n961: ZB = zb_not(n960);
    let n962: ZB = zn_lt(n959, zn_splat(P8::from_raw(0i32)));
    let n963: ZB = zsel_b(n961, n962, r_c399);
    let n964: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n406);
    let n965: ZB = zn_tile_flag_at(g.cache, g.cart, n964, n802, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n966: ZN = zsel_n(n965, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n967: ZN = zn_min(n803, n966);
    let n968: ZN = zsel_n(n748, n967, n506);
    let n969: ZN = zsel_n(n814, n816, n959);
    let n970: ZN = zsel_n(n814, zn_splat(P8::from_raw(-131072i32)), n968);
    let n971: ZN = zsel_n(n806, n959, n969);
    let n972: ZN = zsel_n(n806, zn_splat(P8::from_raw(-131072i32)), n970);
    let n973: ZB = zsel_b(n755, r_c399, n963);
    let n974: ZN = zsel_n(n755, n762, n959);
    let n975: ZN = zsel_n(n755, n768, n968);
    let n976: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n974);
    let n977: ZN = zsel_n(n834, n974, n976);
    let n978: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n789);
    let n979: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n791);
    let n980: ZN = zsel_n(n774, n978, n979);
    let n981: ZN = zsel_n(n771, n788, n980);
    let n982: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n981);
    let n983: ZB = zb_not(n982);
    let n984: ZB = zn_lt(n981, zn_splat(P8::from_raw(0i32)));
    let n985: ZB = zsel_b(n983, n984, r_c399);
    let n986: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n406);
    let n987: ZB = zn_tile_flag_at(g.cache, g.cart, n986, n802, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n988: ZN = zsel_n(n987, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n989: ZN = zn_min(n803, n988);
    let n990: ZN = zsel_n(n748, n989, n506);
    let n991: ZN = zsel_n(n814, n816, n981);
    let n992: ZN = zsel_n(n814, zn_splat(P8::from_raw(-131072i32)), n990);
    let n993: ZN = zsel_n(n806, n981, n991);
    let n994: ZN = zsel_n(n806, zn_splat(P8::from_raw(-131072i32)), n992);
    let n995: ZB = zsel_b(n755, r_c399, n985);
    let n996: ZN = zsel_n(n755, n762, n981);
    let n997: ZN = zsel_n(n755, n768, n990);
    let n998: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n996);
    let n999: ZN = zsel_n(n834, n996, n998);
    let n1000: ZN = zsel_n(n749, n819, n753);
    let n1001: ZN = zsel_n(n749, n820, n794);
    let n1002: ZN = zsel_n(n749, n821, n805);
    let n1003: ZN = zsel_n(n755, n753, n1000);
    let n1004: ZN = zsel_n(n755, n762, n1001);
    let n1005: ZN = zsel_n(n755, n768, n1002);
    let n1006: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1004);
    let n1007: ZN = zsel_n(n834, n1004, n1006);
    let n1008: ZN = zsel_n(n749, n971, n959);
    let n1009: ZN = zsel_n(n749, n972, n968);
    let n1010: ZN = zsel_n(n755, n762, n1008);
    let n1011: ZN = zsel_n(n755, n768, n1009);
    let n1012: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1010);
    let n1013: ZN = zsel_n(n834, n1010, n1012);
    let n1014: ZN = zsel_n(n749, n993, n981);
    let n1015: ZN = zsel_n(n749, n994, n990);
    let n1016: ZN = zsel_n(n755, n762, n1014);
    let n1017: ZN = zsel_n(n755, n768, n1015);
    let n1018: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1016);
    let n1019: ZN = zsel_n(n834, n1016, n1018);
    let n1020: ZN = zsel_n(n218, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1021: ZB = zb_or(r_c41, n218);
    let n1022: ZN = zsel_n(n218, zn_splat(P8::from_raw(655360i32)), n754);
    let n1023: ZN = zsel_n(n218, zn_splat(P8::from_raw(262144i32)), r_c300);
    let n1024: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1025: ZN = zsel_n(n218, zn_splat(P8::from_raw(98304i32)), r_c395);
    let n1026: ZN = zsel_n(n218, n829, r_c396);
    let n1027: ZN = zsel_n(n218, n826, r_c397);
    let n1028: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), r_c398);
    let n1029: ZN = zsel_n(n218, n822, n794);
    let n1030: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n805);
    let n1031: ZN = zsel_n(n755, r_c20, n1020);
    let n1032: ZB = zsel_b(n755, r_c41, n1021);
    let n1033: ZN = zsel_n(n755, n754, n1022);
    let n1034: ZN = zsel_n(n755, n756, n1023);
    let n1035: ZN = zsel_n(n755, zn_splat(P8::from_raw(65536i32)), n1024);
    let n1036: ZN = zsel_n(n755, r_c395, n1025);
    let n1037: ZN = zsel_n(n755, r_c396, n1026);
    let n1038: ZN = zsel_n(n755, r_c397, n1027);
    let n1039: ZN = zsel_n(n755, r_c398, n1028);
    let n1040: ZN = zsel_n(n755, n762, n1029);
    let n1041: ZN = zsel_n(n755, n768, n1030);
    let n1042: ZB = zn_gt(n1031, zn_splat(P8::from_raw(0i32)));
    let n1043: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1040);
    let n1044: ZN = zsel_n(n1042, n392, n840);
    let n1045: ZN = zsel_n(n1042, n1040, n1043);
    let n1046: ZN = zsel_n(n218, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n1047: ZN = zsel_n(n218, zn_splat(P8::from_raw(-131072i32)), r_c397);
    let n1048: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n959);
    let n1049: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n968);
    let n1050: ZN = zsel_n(n755, r_c396, n1046);
    let n1051: ZN = zsel_n(n755, r_c397, n1047);
    let n1052: ZN = zsel_n(n755, n762, n1048);
    let n1053: ZN = zsel_n(n755, n768, n1049);
    let n1054: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1052);
    let n1055: ZN = zsel_n(n1042, n1052, n1054);
    let n1056: ZN = zsel_n(n218, zn_splat(P8::from_raw(131072i32)), r_c397);
    let n1057: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n981);
    let n1058: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n990);
    let n1059: ZN = zsel_n(n755, r_c397, n1056);
    let n1060: ZN = zsel_n(n755, n762, n1057);
    let n1061: ZN = zsel_n(n755, n768, n1058);
    let n1062: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1060);
    let n1063: ZN = zsel_n(n1042, n1060, n1062);
    let n1065: ZN = zsel_n(n218, zn_splat(P8::from_raw(69510i32)), r_c395);
    let n1066: ZN = zsel_n(n218, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n1067: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), r_c397);
    let n1068: ZN = zsel_n(n218, zn_splat(P8::from_raw(-98304i32)), r_c398);
    let n1069: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n794);
    let n1070: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n805);
    let n1071: ZN = zsel_n(n755, r_c395, n1065);
    let n1072: ZN = zsel_n(n755, r_c396, n1066);
    let n1073: ZN = zsel_n(n755, r_c397, n1067);
    let n1074: ZN = zsel_n(n755, r_c398, n1068);
    let n1075: ZN = zsel_n(n755, n762, n1069);
    let n1076: ZN = zsel_n(n755, n768, n1070);
    let n1077: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1075);
    let n1078: ZN = zsel_n(n1042, n1075, n1077);
    let n1079: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n959);
    let n1080: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n968);
    let n1081: ZN = zsel_n(n755, n762, n1079);
    let n1082: ZN = zsel_n(n755, n768, n1080);
    let n1083: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1081);
    let n1084: ZN = zsel_n(n1042, n1081, n1083);
    let n1085: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n981);
    let n1086: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n990);
    let n1087: ZN = zsel_n(n755, n762, n1085);
    let n1088: ZN = zsel_n(n755, n768, n1086);
    let n1089: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1087);
    let n1090: ZN = zsel_n(n1042, n1087, n1089);
    let n1091: ZN = zsel_n(n218, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n1092: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n805);
    let n1093: ZN = zsel_n(n755, r_c398, n1091);
    let n1094: ZN = zsel_n(n755, n768, n1092);
    let n1095: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n968);
    let n1096: ZN = zsel_n(n755, n768, n1095);
    let n1097: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n990);
    let n1098: ZN = zsel_n(n755, n768, n1097);
    let n1099: ZN = zsel_n(n218, n822, n1001);
    let n1100: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n1002);
    let n1101: ZN = zsel_n(n755, n762, n1099);
    let n1102: ZN = zsel_n(n755, n768, n1100);
    let n1103: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1101);
    let n1104: ZN = zsel_n(n1042, n1101, n1103);
    let n1105: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n1008);
    let n1106: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n1009);
    let n1107: ZN = zsel_n(n755, n762, n1105);
    let n1108: ZN = zsel_n(n755, n768, n1106);
    let n1109: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1107);
    let n1110: ZN = zsel_n(n1042, n1107, n1109);
    let n1111: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n1014);
    let n1112: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n1015);
    let n1113: ZN = zsel_n(n755, n762, n1111);
    let n1114: ZN = zsel_n(n755, n768, n1112);
    let n1115: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1113);
    let n1116: ZN = zsel_n(n1042, n1113, n1115);
    let n1117: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n1001);
    let n1118: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n1002);
    let n1119: ZN = zsel_n(n755, n762, n1117);
    let n1120: ZN = zsel_n(n755, n768, n1118);
    let n1121: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1119);
    let n1122: ZN = zsel_n(n1042, n1119, n1121);
    let n1123: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n1008);
    let n1124: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n1009);
    let n1125: ZN = zsel_n(n755, n762, n1123);
    let n1126: ZN = zsel_n(n755, n768, n1124);
    let n1127: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1125);
    let n1128: ZN = zsel_n(n1042, n1125, n1127);
    let n1129: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n1014);
    let n1130: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n1015);
    let n1131: ZN = zsel_n(n755, n762, n1129);
    let n1132: ZN = zsel_n(n755, n768, n1130);
    let n1133: ZN = zsel_n(n837, zn_splat(P8::from_raw(0i32)), n1131);
    let n1134: ZN = zsel_n(n1042, n1131, n1133);
    let n1135: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n1002);
    let n1136: ZN = zsel_n(n755, n768, n1135);
    let n1137: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n1009);
    let n1138: ZN = zsel_n(n755, n768, n1137);
    let n1139: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n1015);
    let n1140: ZN = zsel_n(n755, n768, n1139);
    let n1142: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c246);
    let n1143: ZN = zn_div(n1142, zn_splat(P8::from_raw(2621440i32)));
    let n1144: ZN = zn_sin(n1143);
    let n1146: ZN = zn_mul(n1144, zn_splat(P8::from_raw(163840i32)));
    let n1147: ZN = zn_add(zn_splat(P8::from_raw(3145728i32)), n1146);
    let n1148: ZB = zb_not(n273);
    let n1149: ZB = zb_and(n174, n1148);
    let n1150: ZB = zb_and(n283, n1149);
    let n1151: ZB = zb_and(n174, n274);
    let n1152: ZB = zb_and(n275, n1151);
    let n1153: ZB = zb_and(n280, n1152);
    let n1154: ZB = zb_and(n282, n1153);
    let n1155: ZB = zb_and(n285, n1154);
    let n1156: ZB = zb_and(n1148, n1155);
    let n1157: ZB = zb_and(n517, n1156);
    let n1158: ZB = zb_and(n518, n1156);
    let n1159: ZB = zb_and(n528, n1157);
    let n1160: ZB = zb_and(n856, n1156);
    let n1161: ZB = zb_and(n542, n1160);
    let n1162: ZB = zb_and(n546, n1161);
    let n1163: ZB = zb_and(n547, n1161);
    let n1164: ZB = zb_and(n554, n1163);
    let n1165: ZB = zb_and(n565, n1164);
    let n1166: ZB = zb_and(n568, n1165);
    let n1167: ZB = zb_and(n864, n1164);
    let n1168: ZB = zb_and(n578, n1167);
    let n1169: ZB = zb_and(n580, n1168);
    let n1170: ZB = zb_and(n581, n1168);
    let n1171: ZB = zb_and(n585, n1170);
    let n1172: ZB = zb_and(n589, n1171);
    let n1173: ZB = zb_and(n592, n1172);
    let n1174: ZB = zb_and(n872, n1171);
    let n1175: ZB = zb_and(n602, n1174);
    let n1176: ZB = zb_and(n604, n1175);
    let n1177: ZB = zb_and(n605, n1175);
    let n1178: ZB = zb_and(n609, n1177);
    let n1179: ZB = zb_and(n613, n1178);
    let n1180: ZB = zb_or(n1173, n1179);
    let n1181: ZB = zb_or(n1166, n1180);
    let n1182: ZB = zb_or(n1159, n1181);
    let n1183: ZB = zb_and(n621, n1182);
    let n1184: ZB = zb_and(n622, n1182);
    let n1185: ZB = zb_and(n528, n1183);
    let n1186: ZB = zb_and(n885, n1182);
    let n1187: ZB = zb_and(n628, n1186);
    let n1188: ZB = zb_and(n630, n1187);
    let n1189: ZB = zb_and(n631, n1187);
    let n1190: ZB = zb_and(n635, n1189);
    let n1191: ZB = zb_and(n643, n1190);
    let n1192: ZB = zb_and(n568, n1191);
    let n1193: ZB = zb_and(n893, n1190);
    let n1194: ZB = zb_and(n648, n1193);
    let n1195: ZB = zb_and(n650, n1194);
    let n1196: ZB = zb_and(n651, n1194);
    let n1197: ZB = zb_and(n655, n1196);
    let n1198: ZB = zb_and(n659, n1197);
    let n1199: ZB = zb_and(n592, n1198);
    let n1200: ZB = zb_and(n901, n1197);
    let n1201: ZB = zb_and(n664, n1200);
    let n1202: ZB = zb_and(n666, n1201);
    let n1203: ZB = zb_and(n667, n1201);
    let n1204: ZB = zb_and(n671, n1203);
    let n1205: ZB = zb_and(n675, n1204);
    let n1206: ZB = zb_or(n1199, n1205);
    let n1207: ZB = zb_or(n1192, n1206);
    let n1208: ZB = zb_or(n1185, n1207);
    let n1209: ZB = zb_and(n681, n1208);
    let n1210: ZB = zb_and(n682, n1208);
    let n1211: ZB = zb_and(n528, n1209);
    let n1212: ZB = zb_and(n914, n1208);
    let n1213: ZB = zb_and(n688, n1212);
    let n1214: ZB = zb_and(n690, n1213);
    let n1215: ZB = zb_and(n691, n1213);
    let n1216: ZB = zb_and(n695, n1215);
    let n1217: ZB = zb_and(n703, n1216);
    let n1218: ZB = zb_and(n568, n1217);
    let n1219: ZB = zb_and(n922, n1216);
    let n1220: ZB = zb_and(n708, n1219);
    let n1221: ZB = zb_and(n710, n1220);
    let n1222: ZB = zb_and(n711, n1220);
    let n1223: ZB = zb_and(n715, n1222);
    let n1224: ZB = zb_and(n719, n1223);
    let n1225: ZB = zb_and(n592, n1224);
    let n1226: ZB = zb_and(n930, n1223);
    let n1227: ZB = zb_and(n724, n1226);
    let n1228: ZB = zb_and(n726, n1227);
    let n1229: ZB = zb_and(n727, n1227);
    let n1230: ZB = zb_and(n731, n1229);
    let n1231: ZB = zb_and(n735, n1230);
    let n1232: ZB = zb_or(n1225, n1231);
    let n1233: ZB = zb_or(n1218, n1232);
    let n1234: ZB = zb_or(n1211, n1233);
    let n1235: ZB = zb_or(n1210, n1234);
    let n1236: ZB = zb_or(n1184, n1235);
    let n1237: ZB = zb_or(n1158, n1236);
    let n1239: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1240: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1239);
    let n1241: ZB = zb_and(n541, n857);
    let n1242: ZB = zb_and(n553, n860);
    let n1243: ZB = zb_and(n564, n861);
    let n1244: ZB = zb_or(n1242, n1243);
    let n1245: ZB = zb_or(n859, n1244);
    let n1246: ZB = zb_or(n1241, n1245);
    let n1247: ZB = zb_and(n577, n865);
    let n1248: ZB = zb_and(n584, n868);
    let n1249: ZB = zb_and(n588, n869);
    let n1250: ZB = zb_or(n1248, n1249);
    let n1251: ZB = zb_or(n867, n1250);
    let n1252: ZB = zb_or(n1247, n1251);
    let n1253: ZB = zb_and(n601, n873);
    let n1254: ZB = zb_and(n608, n876);
    let n1255: ZB = zb_and(n612, n877);
    let n1256: ZB = zb_or(n1254, n1255);
    let n1257: ZB = zb_or(n875, n1256);
    let n1258: ZB = zb_or(n1253, n1257);
    let n1259: ZB = zb_or(n1252, n1258);
    let n1260: ZB = zb_or(n1246, n1259);
    let n1261: ZB = zb_and(n627, n886);
    let n1262: ZB = zb_and(n634, n889);
    let n1263: ZB = zb_and(n642, n890);
    let n1264: ZB = zb_or(n1262, n1263);
    let n1265: ZB = zb_or(n888, n1264);
    let n1266: ZB = zb_or(n1261, n1265);
    let n1267: ZB = zb_and(n647, n894);
    let n1268: ZB = zb_and(n654, n897);
    let n1269: ZB = zb_and(n658, n898);
    let n1270: ZB = zb_or(n1268, n1269);
    let n1271: ZB = zb_or(n896, n1270);
    let n1272: ZB = zb_or(n1267, n1271);
    let n1273: ZB = zb_and(n663, n902);
    let n1274: ZB = zb_and(n670, n905);
    let n1275: ZB = zb_and(n674, n906);
    let n1276: ZB = zb_or(n1274, n1275);
    let n1277: ZB = zb_or(n904, n1276);
    let n1278: ZB = zb_or(n1273, n1277);
    let n1279: ZB = zb_or(n1272, n1278);
    let n1280: ZB = zb_or(n1266, n1279);
    let n1281: ZB = zb_and(n687, n915);
    let n1282: ZB = zb_and(n694, n918);
    let n1283: ZB = zb_and(n702, n919);
    let n1284: ZB = zb_or(n1282, n1283);
    let n1285: ZB = zb_or(n917, n1284);
    let n1286: ZB = zb_or(n1281, n1285);
    let n1287: ZB = zb_and(n707, n923);
    let n1288: ZB = zb_and(n714, n926);
    let n1289: ZB = zb_and(n718, n927);
    let n1290: ZB = zb_or(n1288, n1289);
    let n1291: ZB = zb_or(n925, n1290);
    let n1292: ZB = zb_or(n1287, n1291);
    let n1293: ZB = zb_and(n723, n931);
    let n1294: ZB = zb_and(n730, n934);
    let n1295: ZB = zb_and(n734, n935);
    let n1296: ZB = zb_or(n1294, n1295);
    let n1297: ZB = zb_or(n933, n1296);
    let n1298: ZB = zb_or(n1293, n1297);
    let n1299: ZB = zb_or(n1292, n1298);
    let n1300: ZB = zb_or(n1286, n1299);
    let n1301: ZB = zb_or(n1280, n1300);
    let n1302: ZB = zsel_b(n1280, n619, n679);
    let n1303: ZB = zb_or(n1260, n1301);
    let n1304: ZB = zsel_b(n1260, n507, n1302);
    let n1305: ZB = zsel_b(n1303, n1304, n745);
    let n1307: ZB = zb_and(n541, n1160);
    let n1308: ZB = zb_and(n553, n1163);
    let n1309: ZB = zb_and(n564, n1164);
    let n1310: ZB = zb_or(n1308, n1309);
    let n1311: ZB = zb_or(n1162, n1310);
    let n1312: ZB = zb_or(n1307, n1311);
    let n1313: ZB = zb_and(n577, n1167);
    let n1314: ZB = zb_and(n584, n1170);
    let n1315: ZB = zb_and(n588, n1171);
    let n1316: ZB = zb_or(n1314, n1315);
    let n1317: ZB = zb_or(n1169, n1316);
    let n1318: ZB = zb_or(n1313, n1317);
    let n1319: ZB = zb_and(n601, n1174);
    let n1320: ZB = zb_and(n608, n1177);
    let n1321: ZB = zb_and(n612, n1178);
    let n1322: ZB = zb_or(n1320, n1321);
    let n1323: ZB = zb_or(n1176, n1322);
    let n1324: ZB = zb_or(n1319, n1323);
    let n1325: ZB = zb_or(n1318, n1324);
    let n1326: ZB = zb_or(n1312, n1325);
    let n1327: ZB = zb_and(n627, n1186);
    let n1328: ZB = zb_and(n634, n1189);
    let n1329: ZB = zb_and(n642, n1190);
    let n1330: ZB = zb_or(n1328, n1329);
    let n1331: ZB = zb_or(n1188, n1330);
    let n1332: ZB = zb_or(n1327, n1331);
    let n1333: ZB = zb_and(n647, n1193);
    let n1334: ZB = zb_and(n654, n1196);
    let n1335: ZB = zb_and(n658, n1197);
    let n1336: ZB = zb_or(n1334, n1335);
    let n1337: ZB = zb_or(n1195, n1336);
    let n1338: ZB = zb_or(n1333, n1337);
    let n1339: ZB = zb_and(n663, n1200);
    let n1340: ZB = zb_and(n670, n1203);
    let n1341: ZB = zb_and(n674, n1204);
    let n1342: ZB = zb_or(n1340, n1341);
    let n1343: ZB = zb_or(n1202, n1342);
    let n1344: ZB = zb_or(n1339, n1343);
    let n1345: ZB = zb_or(n1338, n1344);
    let n1346: ZB = zb_or(n1332, n1345);
    let n1347: ZB = zb_and(n687, n1212);
    let n1348: ZB = zb_and(n694, n1215);
    let n1349: ZB = zb_and(n702, n1216);
    let n1350: ZB = zb_or(n1348, n1349);
    let n1351: ZB = zb_or(n1214, n1350);
    let n1352: ZB = zb_or(n1347, n1351);
    let n1353: ZB = zb_and(n707, n1219);
    let n1354: ZB = zb_and(n714, n1222);
    let n1355: ZB = zb_and(n718, n1223);
    let n1356: ZB = zb_or(n1354, n1355);
    let n1357: ZB = zb_or(n1221, n1356);
    let n1358: ZB = zb_or(n1353, n1357);
    let n1359: ZB = zb_and(n723, n1226);
    let n1360: ZB = zb_and(n730, n1229);
    let n1361: ZB = zb_and(n734, n1230);
    let n1362: ZB = zb_or(n1360, n1361);
    let n1363: ZB = zb_or(n1228, n1362);
    let n1364: ZB = zb_or(n1359, n1363);
    let n1365: ZB = zb_or(n1358, n1364);
    let n1366: ZB = zb_or(n1352, n1365);
    let n1367: ZB = zb_or(n1346, n1366);
    let n1368: ZB = zsel_b(n1346, n619, n679);
    let n1369: ZB = zb_or(n1326, n1367);
    let n1370: ZB = zsel_b(n1326, n507, n1368);
    let n1371: ZB = zsel_b(n1369, n1370, n745);
    let n1373: ZB = zb_not(n283);
    let n1374: ZB = zb_and(n278, n1373);
    let n1375: ZB = zn_lt(r_c408, zn_splat(P8::from_raw(0i32)));
    let n1376: ZB = zb_and(n284, n1375);
    let n1377: ZB = zb_or(n1374, n1376);
    let n1378: ZB = zb_and(n275, n844);
    let n1379: ZB = zb_and(n845, n1378);
    let n1380: ZB = zb_and(n282, n1379);
    let n1381: ZN = zsel_n(n285, zn_splat(P8::from_raw(7077888i32)), r_c318);
    let n1382: ZN = zsel_n(n285, zn_splat(P8::from_raw(655360i32)), r_c277);
    let n1383: ZN = zsel_n(n285, zn_splat(P8::from_raw(1245184i32)), r_c290);
    let n1384: ZN = zsel_n(n285, n286, r_c407);
    let n1385: ZN = zsel_n(n285, zn_splat(P8::from_raw(-196608i32)), r_c408);
    let n1386: ZN = zsel_n(n1380, n1382, r_c277);
    let n1387: ZN = zsel_n(n1380, n1383, r_c290);
    let n1388: ZN = zsel_n(n1380, n1381, r_c318);
    let n1389: ZN = zsel_n(n1380, n1384, r_c407);
    let n1390: ZN = zsel_n(n1380, n1385, r_c408);
    let n1391: ZN = zsel_n(n245, n241, n1386);
    let n1392: ZN = zsel_n(n245, n247, n1387);
    let n1393: ZN = zsel_n(n245, r_c318, n1388);
    let n1394: ZN = zsel_n(n245, r_c407, n1389);
    let n1395: ZN = zsel_n(n245, r_c408, n1390);
    let n1396: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1394);
    let n1397: ZB = zb_not(n1396);
    let n1398: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1395);
    let n1399: ZB = zb_not(n1398);
    let n1400: ZB = zb_or(n1397, n1399);
    let n1401: ZB = zb_not(n1400);
    let n1402: ZN = zn_add(r_c405, n1394);
    let n1403: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1402);
    let n1404: ZN = zn_flr(n1403);
    let n1405: ZN = zn_sub(n1403, zn_splat(P8::from_raw(32768i32)));
    let n1406: ZN = zn_sub(n1405, n1404);
    let n1407: ZB = zn_gt(n1404, zn_splat(P8::from_raw(0i32)));
    let n1408: ZB = zn_lt(n1404, zn_splat(P8::from_raw(0i32)));
    let n1409: ZN = zsel_n(n1408, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1410: ZN = zsel_n(n1407, zn_splat(P8::from_raw(65536i32)), n1409);
    let n1411: ZN = zn_abs(n1404);
    let n1412: ZN = zn_add(n262, n1410);
    let n1413: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1393);
    let n1414: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1413);
    let n1415: ZB = zn_tile_flag_at(g.cache, g.cart, n1412, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1416: ZN = zn_add(r_c317, n1410);
    let n1417: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1411);
    let n1418: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1416);
    let n1419: ZN = zn_add(n1410, n1418);
    let n1420: ZB = zn_tile_flag_at(g.cache, g.cart, n1419, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1421: ZN = zn_add(n1410, n1416);
    let n1422: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1411);
    let n1423: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1421);
    let n1424: ZN = zn_add(n1410, n1423);
    let n1425: ZB = zn_tile_flag_at(g.cache, g.cart, n1424, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1426: ZN = zn_add(n1410, n1421);
    let n1427: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1411);
    let n1428: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1426);
    let n1429: ZN = zn_add(n1410, n1428);
    let n1430: ZB = zn_tile_flag_at(g.cache, g.cart, n1429, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1431: ZN = zn_add(n1410, n1426);
    let n1432: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1411);
    let n1433: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1431);
    let n1434: ZN = zn_add(n1410, n1433);
    let n1435: ZB = zn_tile_flag_at(g.cache, g.cart, n1434, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1436: ZN = zn_add(n1410, n1431);
    let n1437: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1411);
    let n1438: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1436);
    let n1439: ZN = zn_add(n1410, n1438);
    let n1440: ZB = zn_tile_flag_at(g.cache, g.cart, n1439, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1441: ZN = zn_add(n1410, n1436);
    let n1442: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1411);
    let n1443: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1441);
    let n1444: ZN = zn_add(n1410, n1443);
    let n1445: ZB = zn_tile_flag_at(g.cache, g.cart, n1444, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1446: ZN = zn_add(n1410, n1441);
    let n1447: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1411);
    let n1448: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1446);
    let n1449: ZN = zn_add(n1410, n1448);
    let n1450: ZB = zn_tile_flag_at(g.cache, g.cart, n1449, n1414, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1451: ZN = zn_add(n1410, n1446);
    let n1452: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1411);
    let n1453: ZN = zsel_n(n1450, n1446, n1451);
    let n1454: ZN = zsel_n(n1450, zn_splat(P8::from_raw(0i32)), n1406);
    let n1455: ZN = zsel_n(n1450, zn_splat(P8::from_raw(0i32)), n1394);
    let n1456: ZB = zb_or(n1450, n1452);
    let n1457: ZN = zsel_n(n1447, n1446, n1453);
    let n1458: ZN = zsel_n(n1447, n1406, n1454);
    let n1459: ZN = zsel_n(n1447, n1394, n1455);
    let n1460: ZB = zb_or(n1447, n1456);
    let n1461: ZN = zsel_n(n1445, n1441, n1457);
    let n1462: ZN = zsel_n(n1445, zn_splat(P8::from_raw(0i32)), n1458);
    let n1463: ZN = zsel_n(n1445, zn_splat(P8::from_raw(0i32)), n1459);
    let n1464: ZB = zb_or(n1445, n1460);
    let n1465: ZN = zsel_n(n1442, n1441, n1461);
    let n1466: ZN = zsel_n(n1442, n1406, n1462);
    let n1467: ZN = zsel_n(n1442, n1394, n1463);
    let n1468: ZB = zb_or(n1442, n1464);
    let n1469: ZN = zsel_n(n1440, n1436, n1465);
    let n1470: ZN = zsel_n(n1440, zn_splat(P8::from_raw(0i32)), n1466);
    let n1471: ZN = zsel_n(n1440, zn_splat(P8::from_raw(0i32)), n1467);
    let n1472: ZB = zb_or(n1440, n1468);
    let n1473: ZN = zsel_n(n1437, n1436, n1469);
    let n1474: ZN = zsel_n(n1437, n1406, n1470);
    let n1475: ZN = zsel_n(n1437, n1394, n1471);
    let n1476: ZB = zb_or(n1437, n1472);
    let n1477: ZN = zsel_n(n1435, n1431, n1473);
    let n1478: ZN = zsel_n(n1435, zn_splat(P8::from_raw(0i32)), n1474);
    let n1479: ZN = zsel_n(n1435, zn_splat(P8::from_raw(0i32)), n1475);
    let n1480: ZB = zb_or(n1435, n1476);
    let n1481: ZN = zsel_n(n1432, n1431, n1477);
    let n1482: ZN = zsel_n(n1432, n1406, n1478);
    let n1483: ZN = zsel_n(n1432, n1394, n1479);
    let n1484: ZB = zb_or(n1432, n1480);
    let n1485: ZN = zsel_n(n1430, n1426, n1481);
    let n1486: ZN = zsel_n(n1430, zn_splat(P8::from_raw(0i32)), n1482);
    let n1487: ZN = zsel_n(n1430, zn_splat(P8::from_raw(0i32)), n1483);
    let n1488: ZB = zb_or(n1430, n1484);
    let n1489: ZN = zsel_n(n1427, n1426, n1485);
    let n1490: ZN = zsel_n(n1427, n1406, n1486);
    let n1491: ZN = zsel_n(n1427, n1394, n1487);
    let n1492: ZB = zb_or(n1427, n1488);
    let n1493: ZN = zsel_n(n1425, n1421, n1489);
    let n1494: ZN = zsel_n(n1425, zn_splat(P8::from_raw(0i32)), n1490);
    let n1495: ZN = zsel_n(n1425, zn_splat(P8::from_raw(0i32)), n1491);
    let n1496: ZB = zb_or(n1425, n1492);
    let n1497: ZN = zsel_n(n1422, n1421, n1493);
    let n1498: ZN = zsel_n(n1422, n1406, n1494);
    let n1499: ZN = zsel_n(n1422, n1394, n1495);
    let n1500: ZB = zb_or(n1422, n1496);
    let n1501: ZN = zsel_n(n1420, n1416, n1497);
    let n1502: ZN = zsel_n(n1420, zn_splat(P8::from_raw(0i32)), n1498);
    let n1503: ZN = zsel_n(n1420, zn_splat(P8::from_raw(0i32)), n1499);
    let n1504: ZB = zb_or(n1420, n1500);
    let n1505: ZN = zsel_n(n1417, n1416, n1501);
    let n1506: ZN = zsel_n(n1417, n1406, n1502);
    let n1507: ZN = zsel_n(n1417, n1394, n1503);
    let n1508: ZB = zb_or(n1417, n1504);
    let n1509: ZN = zsel_n(n1415, r_c317, n1505);
    let n1510: ZN = zsel_n(n1415, zn_splat(P8::from_raw(0i32)), n1506);
    let n1511: ZN = zsel_n(n1415, zn_splat(P8::from_raw(0i32)), n1507);
    let n1512: ZB = zb_or(n1415, n1508);
    let n1513: ZN = zn_add(r_c406, n1395);
    let n1514: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1513);
    let n1515: ZN = zn_flr(n1514);
    let n1516: ZN = zn_sub(n1514, zn_splat(P8::from_raw(32768i32)));
    let n1517: ZN = zn_sub(n1516, n1515);
    let n1518: ZB = zn_gt(n1515, zn_splat(P8::from_raw(0i32)));
    let n1519: ZB = zn_lt(n1515, zn_splat(P8::from_raw(0i32)));
    let n1520: ZN = zsel_n(n1519, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1521: ZN = zsel_n(n1518, zn_splat(P8::from_raw(65536i32)), n1520);
    let n1522: ZN = zn_abs(n1515);
    let n1523: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1509);
    let n1524: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1523);
    let n1525: ZN = zn_add(n1413, n1521);
    let n1526: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1525, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1527: ZN = zn_add(n1393, n1521);
    let n1528: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1522);
    let n1529: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1527);
    let n1530: ZN = zn_add(n1521, n1529);
    let n1531: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1530, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1532: ZN = zn_add(n1521, n1527);
    let n1533: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1522);
    let n1534: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1532);
    let n1535: ZN = zn_add(n1521, n1534);
    let n1536: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1535, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1537: ZN = zn_add(n1521, n1532);
    let n1538: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1522);
    let n1539: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1537);
    let n1540: ZN = zn_add(n1521, n1539);
    let n1541: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1540, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1542: ZN = zn_add(n1521, n1537);
    let n1543: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1522);
    let n1544: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1542);
    let n1545: ZN = zn_add(n1521, n1544);
    let n1546: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1545, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1547: ZN = zn_add(n1521, n1542);
    let n1548: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1522);
    let n1549: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1547);
    let n1550: ZN = zn_add(n1521, n1549);
    let n1551: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1550, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1552: ZN = zn_add(n1521, n1547);
    let n1553: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1522);
    let n1554: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1552);
    let n1555: ZN = zn_add(n1521, n1554);
    let n1556: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1555, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1557: ZN = zn_add(n1521, n1552);
    let n1558: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1522);
    let n1559: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1557);
    let n1560: ZN = zn_add(n1521, n1559);
    let n1561: ZB = zn_tile_flag_at(g.cache, g.cart, n1524, n1560, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1562: ZN = zn_add(n1521, n1557);
    let n1563: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1522);
    let n1564: ZB = zb_and(n1512, n1563);
    let n1565: ZN = zsel_n(n1561, n1557, n1562);
    let n1566: ZN = zsel_n(n1561, zn_splat(P8::from_raw(0i32)), n1517);
    let n1567: ZN = zsel_n(n1561, zn_splat(P8::from_raw(0i32)), n1395);
    let n1568: ZB = zsel_b(n1561, n1512, n1564);
    let n1569: ZN = zsel_n(n1558, n1557, n1565);
    let n1570: ZN = zsel_n(n1558, n1517, n1566);
    let n1571: ZN = zsel_n(n1558, n1395, n1567);
    let n1572: ZB = zsel_b(n1558, n1512, n1568);
    let n1573: ZN = zsel_n(n1556, n1552, n1569);
    let n1574: ZN = zsel_n(n1556, zn_splat(P8::from_raw(0i32)), n1570);
    let n1575: ZN = zsel_n(n1556, zn_splat(P8::from_raw(0i32)), n1571);
    let n1576: ZB = zsel_b(n1556, n1512, n1572);
    let n1577: ZN = zsel_n(n1553, n1552, n1573);
    let n1578: ZN = zsel_n(n1553, n1517, n1574);
    let n1579: ZN = zsel_n(n1553, n1395, n1575);
    let n1580: ZB = zsel_b(n1553, n1512, n1576);
    let n1581: ZN = zsel_n(n1551, n1547, n1577);
    let n1582: ZN = zsel_n(n1551, zn_splat(P8::from_raw(0i32)), n1578);
    let n1583: ZN = zsel_n(n1551, zn_splat(P8::from_raw(0i32)), n1579);
    let n1584: ZB = zsel_b(n1551, n1512, n1580);
    let n1585: ZN = zsel_n(n1548, n1547, n1581);
    let n1586: ZN = zsel_n(n1548, n1517, n1582);
    let n1587: ZN = zsel_n(n1548, n1395, n1583);
    let n1588: ZB = zsel_b(n1548, n1512, n1584);
    let n1589: ZN = zsel_n(n1546, n1542, n1585);
    let n1590: ZN = zsel_n(n1546, zn_splat(P8::from_raw(0i32)), n1586);
    let n1591: ZN = zsel_n(n1546, zn_splat(P8::from_raw(0i32)), n1587);
    let n1592: ZB = zsel_b(n1546, n1512, n1588);
    let n1593: ZN = zsel_n(n1543, n1542, n1589);
    let n1594: ZN = zsel_n(n1543, n1517, n1590);
    let n1595: ZN = zsel_n(n1543, n1395, n1591);
    let n1596: ZB = zsel_b(n1543, n1512, n1592);
    let n1597: ZN = zsel_n(n1541, n1537, n1593);
    let n1598: ZN = zsel_n(n1541, zn_splat(P8::from_raw(0i32)), n1594);
    let n1599: ZN = zsel_n(n1541, zn_splat(P8::from_raw(0i32)), n1595);
    let n1600: ZB = zsel_b(n1541, n1512, n1596);
    let n1601: ZN = zsel_n(n1538, n1537, n1597);
    let n1602: ZN = zsel_n(n1538, n1517, n1598);
    let n1603: ZN = zsel_n(n1538, n1395, n1599);
    let n1604: ZB = zsel_b(n1538, n1512, n1600);
    let n1605: ZN = zsel_n(n1536, n1532, n1601);
    let n1606: ZN = zsel_n(n1536, zn_splat(P8::from_raw(0i32)), n1602);
    let n1607: ZN = zsel_n(n1536, zn_splat(P8::from_raw(0i32)), n1603);
    let n1608: ZB = zsel_b(n1536, n1512, n1604);
    let n1609: ZN = zsel_n(n1533, n1532, n1605);
    let n1610: ZN = zsel_n(n1533, n1517, n1606);
    let n1611: ZN = zsel_n(n1533, n1395, n1607);
    let n1612: ZB = zsel_b(n1533, n1512, n1608);
    let n1613: ZN = zsel_n(n1531, n1527, n1609);
    let n1614: ZN = zsel_n(n1531, zn_splat(P8::from_raw(0i32)), n1610);
    let n1615: ZN = zsel_n(n1531, zn_splat(P8::from_raw(0i32)), n1611);
    let n1616: ZB = zsel_b(n1531, n1512, n1612);
    let n1617: ZN = zsel_n(n1528, n1527, n1613);
    let n1618: ZN = zsel_n(n1528, n1517, n1614);
    let n1619: ZN = zsel_n(n1528, n1395, n1615);
    let n1620: ZB = zsel_b(n1528, n1512, n1616);
    let n1621: ZN = zsel_n(n1526, n1393, n1617);
    let n1622: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1618);
    let n1623: ZN = zsel_n(n1526, zn_splat(P8::from_raw(0i32)), n1619);
    let n1624: ZB = zsel_b(n1526, n1512, n1620);
    let n1625: ZN = zsel_n(n1400, n1509, r_c317);
    let n1626: ZN = zsel_n(n1400, n1621, n1393);
    let n1627: ZN = zsel_n(n1400, n1510, r_c405);
    let n1628: ZN = zsel_n(n1400, n1622, r_c406);
    let n1629: ZN = zsel_n(n1400, n1511, n1394);
    let n1630: ZN = zsel_n(n1400, n1623, n1395);
    let n1631: ZB = zb_or(n1401, n1624);
    let n1632: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1625);
    let n1633: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1626);
    let n1634: ZN = zn_div(n1632, zn_splat(P8::from_raw(524288i32)));
    let n1635: ZN = zn_flr(n1634);
    let n1636: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1635);
    let n1637: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1632);
    let n1638: ZN = zn_sub(n1637, zn_splat(P8::from_raw(65536i32)));
    let n1639: ZN = zn_div(n1638, zn_splat(P8::from_raw(524288i32)));
    let n1640: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1639);
    let n1641: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1636);
    let n1642: ZB = zn_le(n1641, n1640);
    let n1643: ZB = zn_gt(n1641, n1640);
    let n1644: ZB = zb_and(n1377, n1642);
    let n1645: ZB = zb_and(n1377, n1643);
    let n1646: ZN = zn_div(n1633, zn_splat(P8::from_raw(524288i32)));
    let n1647: ZN = zn_flr(n1646);
    let n1648: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1647);
    let n1649: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1633);
    let n1650: ZN = zn_sub(n1649, zn_splat(P8::from_raw(65536i32)));
    let n1651: ZN = zn_div(n1650, zn_splat(P8::from_raw(524288i32)));
    let n1652: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1651);
    let n1653: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1648);
    let n1654: ZB = zn_le(n1653, n1652);
    let n1655: ZB = zn_gt(n1653, n1652);
    let n1656: ZB = zb_and(n1644, n1654);
    let n1657: ZB = zb_and(n1644, n1655);
    let n1658: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1641);
    let n1659: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1653);
    let n1660: ZN = zn_mget(g.cart, n1658, n1659);
    let n1661: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1660);
    let n1662: ZN = zn_rem(n1650, zn_splat(P8::from_raw(524288i32)));
    let n1663: ZB = zn_ge(n1662, zn_splat(P8::from_raw(393216i32)));
    let n1664: ZN = zn_mul(n1653, zn_splat(P8::from_raw(524288i32)));
    let n1665: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1664);
    let n1666: ZB = zn_eq(n1649, n1665);
    let n1667: ZB = zb_or(n1663, n1666);
    let n1668: ZB = zb_and(n1661, n1667);
    let n1669: ZB = zn_ge(n1630, zn_splat(P8::from_raw(0i32)));
    let n1670: ZB = zb_and(n1668, n1669);
    let n1671: ZB = zb_not(n1670);
    let n1672: ZB = zb_and(n1656, n1671);
    let n1673: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1660);
    let n1674: ZN = zn_rem(n1633, zn_splat(P8::from_raw(524288i32)));
    let n1675: ZB = zn_le(n1674, zn_splat(P8::from_raw(131072i32)));
    let n1676: ZB = zb_and(n1673, n1675);
    let n1677: ZB = zn_le(n1630, zn_splat(P8::from_raw(0i32)));
    let n1678: ZB = zb_and(n1676, n1677);
    let n1679: ZB = zb_not(n1678);
    let n1680: ZB = zb_and(n1672, n1679);
    let n1681: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1660);
    let n1682: ZN = zn_rem(n1632, zn_splat(P8::from_raw(524288i32)));
    let n1683: ZB = zn_le(n1682, zn_splat(P8::from_raw(131072i32)));
    let n1684: ZB = zb_and(n1681, n1683);
    let n1685: ZB = zn_le(n1629, zn_splat(P8::from_raw(0i32)));
    let n1686: ZB = zb_and(n1684, n1685);
    let n1687: ZB = zb_not(n1686);
    let n1688: ZB = zb_and(n1680, n1687);
    let n1689: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1660);
    let n1690: ZN = zn_rem(n1638, zn_splat(P8::from_raw(524288i32)));
    let n1691: ZB = zn_ge(n1690, zn_splat(P8::from_raw(393216i32)));
    let n1692: ZN = zn_mul(n1641, zn_splat(P8::from_raw(524288i32)));
    let n1693: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1692);
    let n1694: ZB = zn_eq(n1637, n1693);
    let n1695: ZB = zb_or(n1691, n1694);
    let n1696: ZB = zb_and(n1689, n1695);
    let n1697: ZB = zn_ge(n1629, zn_splat(P8::from_raw(0i32)));
    let n1698: ZB = zb_and(n1696, n1697);
    let n1699: ZB = zb_not(n1698);
    let n1700: ZB = zb_and(n1688, n1699);
    let n1701: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1648);
    let n1702: ZB = zn_le(n1701, n1652);
    let n1703: ZB = zn_gt(n1701, n1652);
    let n1704: ZB = zb_and(n1700, n1702);
    let n1705: ZB = zb_and(n1700, n1703);
    let n1706: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1701);
    let n1707: ZN = zn_mget(g.cart, n1658, n1706);
    let n1708: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1707);
    let n1709: ZN = zn_mul(n1701, zn_splat(P8::from_raw(524288i32)));
    let n1710: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1709);
    let n1711: ZB = zn_eq(n1649, n1710);
    let n1712: ZB = zb_or(n1663, n1711);
    let n1713: ZB = zb_and(n1708, n1712);
    let n1714: ZB = zb_and(n1669, n1713);
    let n1715: ZB = zb_not(n1714);
    let n1716: ZB = zb_and(n1704, n1715);
    let n1717: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1707);
    let n1718: ZB = zb_and(n1675, n1717);
    let n1719: ZB = zb_and(n1677, n1718);
    let n1720: ZB = zb_not(n1719);
    let n1721: ZB = zb_and(n1716, n1720);
    let n1722: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1707);
    let n1723: ZB = zb_and(n1683, n1722);
    let n1724: ZB = zb_and(n1685, n1723);
    let n1725: ZB = zb_not(n1724);
    let n1726: ZB = zb_and(n1721, n1725);
    let n1727: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1707);
    let n1728: ZB = zb_and(n1695, n1727);
    let n1729: ZB = zb_and(n1697, n1728);
    let n1730: ZB = zb_not(n1729);
    let n1731: ZB = zb_and(n1726, n1730);
    let n1732: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1648);
    let n1733: ZB = zn_le(n1732, n1652);
    let n1734: ZB = zn_gt(n1732, n1652);
    let n1735: ZB = zb_and(n1731, n1733);
    let n1736: ZB = zb_and(n1731, n1734);
    let n1737: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1732);
    let n1738: ZN = zn_mget(g.cart, n1658, n1737);
    let n1739: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1738);
    let n1740: ZN = zn_mul(n1732, zn_splat(P8::from_raw(524288i32)));
    let n1741: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1740);
    let n1742: ZB = zn_eq(n1649, n1741);
    let n1743: ZB = zb_or(n1663, n1742);
    let n1744: ZB = zb_and(n1739, n1743);
    let n1745: ZB = zb_and(n1669, n1744);
    let n1746: ZB = zb_not(n1745);
    let n1747: ZB = zb_and(n1735, n1746);
    let n1748: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1738);
    let n1749: ZB = zb_and(n1675, n1748);
    let n1750: ZB = zb_and(n1677, n1749);
    let n1751: ZB = zb_not(n1750);
    let n1752: ZB = zb_and(n1747, n1751);
    let n1753: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1738);
    let n1754: ZB = zb_and(n1683, n1753);
    let n1755: ZB = zb_and(n1685, n1754);
    let n1756: ZB = zb_not(n1755);
    let n1757: ZB = zb_and(n1752, n1756);
    let n1758: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1738);
    let n1759: ZB = zb_and(n1695, n1758);
    let n1760: ZB = zb_and(n1697, n1759);
    let n1761: ZB = zb_not(n1760);
    let n1762: ZB = zb_and(n1757, n1761);
    let n1763: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1648);
    let n1764: ZB = zn_gt(n1763, n1652);
    let n1765: ZB = zb_and(n1631, n1764);
    let n1766: ZB = zb_or(n1736, n1762);
    let n1767: ZB = zsel_b(n1734, n1631, n1765);
    let n1768: ZB = zb_or(n1705, n1766);
    let n1769: ZB = zsel_b(n1703, n1631, n1767);
    let n1770: ZB = zb_or(n1657, n1768);
    let n1771: ZB = zsel_b(n1655, n1631, n1769);
    let n1772: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1636);
    let n1773: ZB = zn_le(n1772, n1640);
    let n1774: ZB = zn_gt(n1772, n1640);
    let n1775: ZB = zb_and(n1770, n1773);
    let n1776: ZB = zb_and(n1770, n1774);
    let n1777: ZB = zb_and(n1655, n1775);
    let n1778: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1772);
    let n1779: ZN = zn_mget(g.cart, n1778, n1659);
    let n1780: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1779);
    let n1781: ZB = zb_and(n1654, n1770);
    let n1782: ZB = zb_and(n1773, n1781);
    let n1783: ZB = zb_and(n1667, n1780);
    let n1784: ZB = zb_and(n1669, n1783);
    let n1785: ZB = zb_not(n1784);
    let n1786: ZB = zb_and(n1782, n1785);
    let n1787: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1779);
    let n1788: ZB = zb_and(n1675, n1787);
    let n1789: ZB = zb_and(n1677, n1788);
    let n1790: ZB = zb_not(n1789);
    let n1791: ZB = zb_and(n1786, n1790);
    let n1792: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1779);
    let n1793: ZB = zb_and(n1683, n1792);
    let n1794: ZB = zb_and(n1685, n1793);
    let n1795: ZB = zb_not(n1794);
    let n1796: ZB = zb_and(n1791, n1795);
    let n1797: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1779);
    let n1798: ZN = zn_mul(n1772, zn_splat(P8::from_raw(524288i32)));
    let n1799: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1798);
    let n1800: ZB = zn_eq(n1637, n1799);
    let n1801: ZB = zb_or(n1691, n1800);
    let n1802: ZB = zb_and(n1797, n1801);
    let n1803: ZB = zb_and(n1697, n1802);
    let n1804: ZB = zb_not(n1803);
    let n1805: ZB = zb_and(n1796, n1804);
    let n1806: ZB = zb_and(n1703, n1805);
    let n1807: ZN = zn_mget(g.cart, n1778, n1706);
    let n1808: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1807);
    let n1809: ZB = zb_and(n1702, n1796);
    let n1810: ZB = zb_and(n1804, n1809);
    let n1811: ZB = zb_and(n1712, n1808);
    let n1812: ZB = zb_and(n1669, n1811);
    let n1813: ZB = zb_not(n1812);
    let n1814: ZB = zb_and(n1810, n1813);
    let n1815: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1807);
    let n1816: ZB = zb_and(n1675, n1815);
    let n1817: ZB = zb_and(n1677, n1816);
    let n1818: ZB = zb_not(n1817);
    let n1819: ZB = zb_and(n1814, n1818);
    let n1820: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1807);
    let n1821: ZB = zb_and(n1683, n1820);
    let n1822: ZB = zb_and(n1685, n1821);
    let n1823: ZB = zb_not(n1822);
    let n1824: ZB = zb_and(n1819, n1823);
    let n1825: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1807);
    let n1826: ZB = zb_and(n1801, n1825);
    let n1827: ZB = zb_and(n1697, n1826);
    let n1828: ZB = zb_not(n1827);
    let n1829: ZB = zb_and(n1824, n1828);
    let n1830: ZB = zb_and(n1734, n1829);
    let n1831: ZN = zn_mget(g.cart, n1778, n1737);
    let n1832: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1831);
    let n1833: ZB = zb_and(n1733, n1824);
    let n1834: ZB = zb_and(n1828, n1833);
    let n1835: ZB = zb_and(n1743, n1832);
    let n1836: ZB = zb_and(n1669, n1835);
    let n1837: ZB = zb_not(n1836);
    let n1838: ZB = zb_and(n1834, n1837);
    let n1839: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1831);
    let n1840: ZB = zb_and(n1675, n1839);
    let n1841: ZB = zb_and(n1677, n1840);
    let n1842: ZB = zb_not(n1841);
    let n1843: ZB = zb_and(n1838, n1842);
    let n1844: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1831);
    let n1845: ZB = zb_and(n1683, n1844);
    let n1846: ZB = zb_and(n1685, n1845);
    let n1847: ZB = zb_not(n1846);
    let n1848: ZB = zb_and(n1843, n1847);
    let n1849: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1831);
    let n1850: ZB = zb_and(n1801, n1849);
    let n1851: ZB = zb_and(n1697, n1850);
    let n1852: ZB = zb_not(n1851);
    let n1853: ZB = zb_and(n1848, n1852);
    let n1854: ZB = zb_and(n1764, n1771);
    let n1855: ZB = zb_or(n1830, n1853);
    let n1856: ZB = zsel_b(n1734, n1771, n1854);
    let n1857: ZB = zb_or(n1806, n1855);
    let n1858: ZB = zsel_b(n1703, n1771, n1856);
    let n1859: ZB = zb_or(n1777, n1857);
    let n1860: ZB = zsel_b(n1655, n1771, n1858);
    let n1861: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1636);
    let n1862: ZB = zn_le(n1861, n1640);
    let n1863: ZB = zn_gt(n1861, n1640);
    let n1864: ZB = zb_and(n1859, n1862);
    let n1865: ZB = zb_and(n1859, n1863);
    let n1866: ZB = zb_and(n1655, n1864);
    let n1867: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1861);
    let n1868: ZN = zn_mget(g.cart, n1867, n1659);
    let n1869: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1868);
    let n1870: ZB = zb_and(n1654, n1859);
    let n1871: ZB = zb_and(n1862, n1870);
    let n1872: ZB = zb_and(n1667, n1869);
    let n1873: ZB = zb_and(n1669, n1872);
    let n1874: ZB = zb_not(n1873);
    let n1875: ZB = zb_and(n1871, n1874);
    let n1876: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1868);
    let n1877: ZB = zb_and(n1675, n1876);
    let n1878: ZB = zb_and(n1677, n1877);
    let n1879: ZB = zb_not(n1878);
    let n1880: ZB = zb_and(n1875, n1879);
    let n1881: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1868);
    let n1882: ZB = zb_and(n1683, n1881);
    let n1883: ZB = zb_and(n1685, n1882);
    let n1884: ZB = zb_not(n1883);
    let n1885: ZB = zb_and(n1880, n1884);
    let n1886: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1868);
    let n1887: ZN = zn_mul(n1861, zn_splat(P8::from_raw(524288i32)));
    let n1888: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1887);
    let n1889: ZB = zn_eq(n1637, n1888);
    let n1890: ZB = zb_or(n1691, n1889);
    let n1891: ZB = zb_and(n1886, n1890);
    let n1892: ZB = zb_and(n1697, n1891);
    let n1893: ZB = zb_not(n1892);
    let n1894: ZB = zb_and(n1885, n1893);
    let n1895: ZB = zb_and(n1703, n1894);
    let n1896: ZN = zn_mget(g.cart, n1867, n1706);
    let n1897: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1896);
    let n1898: ZB = zb_and(n1702, n1885);
    let n1899: ZB = zb_and(n1893, n1898);
    let n1900: ZB = zb_and(n1712, n1897);
    let n1901: ZB = zb_and(n1669, n1900);
    let n1902: ZB = zb_not(n1901);
    let n1903: ZB = zb_and(n1899, n1902);
    let n1904: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1896);
    let n1905: ZB = zb_and(n1675, n1904);
    let n1906: ZB = zb_and(n1677, n1905);
    let n1907: ZB = zb_not(n1906);
    let n1908: ZB = zb_and(n1903, n1907);
    let n1909: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1896);
    let n1910: ZB = zb_and(n1683, n1909);
    let n1911: ZB = zb_and(n1685, n1910);
    let n1912: ZB = zb_not(n1911);
    let n1913: ZB = zb_and(n1908, n1912);
    let n1914: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1896);
    let n1915: ZB = zb_and(n1890, n1914);
    let n1916: ZB = zb_and(n1697, n1915);
    let n1917: ZB = zb_not(n1916);
    let n1918: ZB = zb_and(n1913, n1917);
    let n1919: ZB = zb_and(n1734, n1918);
    let n1920: ZN = zn_mget(g.cart, n1867, n1737);
    let n1921: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1920);
    let n1922: ZB = zb_and(n1733, n1913);
    let n1923: ZB = zb_and(n1917, n1922);
    let n1924: ZB = zb_and(n1743, n1921);
    let n1925: ZB = zb_and(n1669, n1924);
    let n1926: ZB = zb_not(n1925);
    let n1927: ZB = zb_and(n1923, n1926);
    let n1928: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1920);
    let n1929: ZB = zb_and(n1675, n1928);
    let n1930: ZB = zb_and(n1677, n1929);
    let n1931: ZB = zb_not(n1930);
    let n1932: ZB = zb_and(n1927, n1931);
    let n1933: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1920);
    let n1934: ZB = zb_and(n1683, n1933);
    let n1935: ZB = zb_and(n1685, n1934);
    let n1936: ZB = zb_not(n1935);
    let n1937: ZB = zb_and(n1932, n1936);
    let n1938: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1920);
    let n1939: ZB = zb_and(n1890, n1938);
    let n1940: ZB = zb_and(n1697, n1939);
    let n1941: ZB = zb_not(n1940);
    let n1942: ZB = zb_and(n1937, n1941);
    let n1943: ZB = zb_and(n1764, n1860);
    let n1944: ZB = zb_or(n1919, n1942);
    let n1945: ZB = zsel_b(n1734, n1860, n1943);
    let n1946: ZB = zb_or(n1895, n1944);
    let n1947: ZB = zsel_b(n1703, n1860, n1945);
    let n1948: ZB = zb_or(n1866, n1946);
    let n1949: ZB = zsel_b(n1655, n1860, n1947);
    let n1950: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1636);
    let n1951: ZB = zn_gt(n1950, n1640);
    let n1952: ZB = zb_and(n1949, n1951);
    let n1953: ZB = zb_or(n1865, n1948);
    let n1954: ZB = zsel_b(n1863, n1860, n1952);
    let n1955: ZB = zb_or(n1776, n1953);
    let n1956: ZB = zsel_b(n1774, n1771, n1954);
    let n1957: ZB = zb_or(n1645, n1955);
    let n1958: ZB = zsel_b(n1643, n1631, n1956);
    let n1959: ZB = zn_le(n1626, zn_splat(P8::from_raw(8388608i32)));
    let n1960: ZB = zb_and(n1957, n1959);
    let n1961: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1632);
    let n1962: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1633);
    let n1963: ZB = zn_tile_flag_at(g.cache, g.cart, n1961, n1962, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1964: ZB = zb_not(n1963);
    let n1965: ZN = zsel_n(n1963, zn_splat(P8::from_raw(393216i32)), n752);
    let n1966: ZB = zn_gt(n1629, r_c397);
    let n1967: ZN = zn_sub(n1629, r_c395);
    let n1968: ZN = zn_max(r_c397, n1967);
    let n1969: ZN = zn_add(r_c395, n1629);
    let n1970: ZN = zn_min(r_c397, n1969);
    let n1971: ZN = zsel_n(n1966, n1968, n1970);
    let n1972: ZB = zn_gt(n1630, r_c398);
    let n1973: ZN = zn_sub(n1630, r_c396);
    let n1974: ZN = zn_max(r_c398, n1973);
    let n1975: ZN = zn_add(r_c396, n1630);
    let n1976: ZN = zn_min(r_c398, n1975);
    let n1977: ZN = zsel_n(n1972, n1974, n1976);
    let n1978: ZN = zsel_n(n1964, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1979: ZN = zn_abs(n1629);
    let n1980: ZB = zn_gt(n1979, zn_splat(P8::from_raw(65536i32)));
    let n1981: ZB = zn_gt(n1629, zn_splat(P8::from_raw(0i32)));
    let n1982: ZB = zn_lt(n1629, zn_splat(P8::from_raw(0i32)));
    let n1983: ZB = zn_gt(n1629, zn_splat(P8::from_raw(65536i32)));
    let n1984: ZN = zn_sub(n1629, zn_splat(P8::from_raw(9830i32)));
    let n1985: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1984);
    let n1986: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n1629);
    let n1987: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1986);
    let n1988: ZB = zn_gt(n1629, zn_splat(P8::from_raw(-65536i32)));
    let n1989: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1984);
    let n1990: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1986);
    let n1991: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1984);
    let n1992: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1986);
    let n1993: ZN = zsel_n(n1988, n1989, n1990);
    let n1994: ZN = zsel_n(n1981, n1991, n1992);
    let n1995: ZN = zsel_n(n1983, n1985, n1987);
    let n1996: ZN = zsel_n(n1982, n1993, n1994);
    let n1997: ZN = zsel_n(n1981, n1995, n1996);
    let n1998: ZN = zn_sub(n1629, n1978);
    let n1999: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1998);
    let n2000: ZN = zn_add(n1629, n1978);
    let n2001: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n2000);
    let n2002: ZN = zsel_n(n1981, n1999, n2001);
    let n2003: ZN = zsel_n(n1980, n1997, n2002);
    let n2004: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2003);
    let n2005: ZB = zb_not(n2004);
    let n2006: ZB = zn_lt(n2003, zn_splat(P8::from_raw(0i32)));
    let n2007: ZB = zsel_b(n2005, n2006, r_c399);
    let n2008: ZN = zn_abs(n1630);
    let n2009: ZB = zn_le(n2008, zn_splat(P8::from_raw(9830i32)));
    let n2010: ZN = zsel_n(n2009, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2011: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1633);
    let n2012: ZB = zn_gt(n1630, zn_splat(P8::from_raw(131072i32)));
    let n2013: ZN = zn_sub(n1630, n2010);
    let n2014: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2013);
    let n2015: ZN = zn_add(n1630, n2010);
    let n2016: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2015);
    let n2017: ZN = zsel_n(n2012, n2014, n2016);
    let n2018: ZN = zsel_n(n1964, n2017, n1630);
    let n2019: ZB = zn_gt(n1965, zn_splat(P8::from_raw(0i32)));
    let n2020: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1632);
    let n2021: ZB = zn_tile_flag_at(g.cache, g.cart, n2020, n2011, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2022: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1632);
    let n2023: ZB = zn_tile_flag_at(g.cache, g.cart, n2022, n2011, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2024: ZN = zsel_n(n2023, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n2025: ZN = zsel_n(n2021, zn_splat(P8::from_raw(-65536i32)), n2024);
    let n2026: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2025);
    let n2027: ZB = zb_not(n2026);
    let n2028: ZN = zn_neg(n2025);
    let n2029: ZN = zn_mul(n2028, zn_splat(P8::from_raw(131072i32)));
    let n2030: ZN = zsel_n(n2027, n2029, n2003);
    let n2031: ZN = zsel_n(n2027, zn_splat(P8::from_raw(-131072i32)), n2018);
    let n2032: ZN = zsel_n(n2019, zn_splat(P8::from_raw(0i32)), n1965);
    let n2033: ZN = zsel_n(n2019, n2003, n2030);
    let n2034: ZN = zsel_n(n2019, zn_splat(P8::from_raw(-131072i32)), n2031);
    let n2035: ZN = zsel_n(n2007, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n2036: ZB = zn_gt(n2035, zn_splat(P8::from_raw(0i32)));
    let n2037: ZB = zn_lt(n2035, zn_splat(P8::from_raw(0i32)));
    let n2038: ZN = zsel_n(n2037, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2039: ZN = zsel_n(n2036, zn_splat(P8::from_raw(131072i32)), n2038);
    let n2040: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2035);
    let n2041: ZB = zb_not(n2040);
    let n2042: ZN = zsel_n(n2041, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2043: ZB = zsel_b(n755, r_c399, n2007);
    let n2044: ZN = zsel_n(n755, n1971, n2003);
    let n2045: ZN = zsel_n(n755, n1977, n2018);
    let n2046: ZB = zn_lt(n1626, zn_splat(P8::from_raw(-262144i32)));
    let n2047: ZB = zn_ge(n1626, zn_splat(P8::from_raw(-262144i32)));
    let n2048: ZB = zb_and(n1960, n2046);
    let n2050: ZB = zn_lt(n1625, zn_splat(P8::from_raw(-65536i32)));
    let n2051: ZB = zn_gt(n1625, zn_splat(P8::from_raw(7929856i32)));
    let n2054: ZB = zb_or(n2050, n2051);
    let n2055: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n1625);
    let n2056: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2055);
    let n2057: ZN = zsel_n(n2054, n2056, n1625);
    let n2058: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2044);
    let n2059: ZN = zsel_n(n834, n1625, n2057);
    let n2060: ZN = zsel_n(n834, n2044, n2058);
    let n2062: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1998);
    let n2063: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n2000);
    let n2064: ZN = zsel_n(n1988, n2062, n2063);
    let n2065: ZN = zsel_n(n1980, n1997, n2064);
    let n2066: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2065);
    let n2067: ZB = zb_not(n2066);
    let n2068: ZB = zn_lt(n2065, zn_splat(P8::from_raw(0i32)));
    let n2069: ZB = zsel_b(n2067, n2068, r_c399);
    let n2070: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1632);
    let n2071: ZB = zn_tile_flag_at(g.cache, g.cart, n2070, n2011, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2072: ZN = zsel_n(n2071, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n2073: ZB = zn_gt(n1630, n2072);
    let n2074: ZN = zn_max(n2013, n2072);
    let n2075: ZN = zn_min(n2015, n2072);
    let n2076: ZN = zsel_n(n2073, n2074, n2075);
    let n2077: ZN = zsel_n(n1964, n2076, n1630);
    let n2078: ZN = zsel_n(n2027, n2029, n2065);
    let n2079: ZN = zsel_n(n2027, zn_splat(P8::from_raw(-131072i32)), n2077);
    let n2080: ZN = zsel_n(n2019, n2065, n2078);
    let n2081: ZN = zsel_n(n2019, zn_splat(P8::from_raw(-131072i32)), n2079);
    let n2082: ZB = zsel_b(n755, r_c399, n2069);
    let n2083: ZN = zsel_n(n755, n1971, n2065);
    let n2084: ZN = zsel_n(n755, n1977, n2077);
    let n2085: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2083);
    let n2086: ZN = zsel_n(n834, n2083, n2085);
    let n2087: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1998);
    let n2088: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n2000);
    let n2089: ZN = zsel_n(n1983, n2087, n2088);
    let n2090: ZN = zsel_n(n1980, n1997, n2089);
    let n2091: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2090);
    let n2092: ZB = zb_not(n2091);
    let n2093: ZB = zn_lt(n2090, zn_splat(P8::from_raw(0i32)));
    let n2094: ZB = zsel_b(n2092, n2093, r_c399);
    let n2095: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1632);
    let n2096: ZB = zn_tile_flag_at(g.cache, g.cart, n2095, n2011, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2097: ZN = zsel_n(n2096, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n2098: ZB = zn_gt(n1630, n2097);
    let n2099: ZN = zn_max(n2013, n2097);
    let n2100: ZN = zn_min(n2015, n2097);
    let n2101: ZN = zsel_n(n2098, n2099, n2100);
    let n2102: ZN = zsel_n(n1964, n2101, n1630);
    let n2103: ZN = zsel_n(n2027, n2029, n2090);
    let n2104: ZN = zsel_n(n2027, zn_splat(P8::from_raw(-131072i32)), n2102);
    let n2105: ZN = zsel_n(n2019, n2090, n2103);
    let n2106: ZN = zsel_n(n2019, zn_splat(P8::from_raw(-131072i32)), n2104);
    let n2107: ZB = zsel_b(n755, r_c399, n2094);
    let n2108: ZN = zsel_n(n755, n1971, n2090);
    let n2109: ZN = zsel_n(n755, n1977, n2102);
    let n2110: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2108);
    let n2111: ZN = zsel_n(n834, n2108, n2110);
    let n2112: ZN = zsel_n(n749, n2032, n1965);
    let n2113: ZN = zsel_n(n749, n2033, n2003);
    let n2114: ZN = zsel_n(n749, n2034, n2018);
    let n2115: ZN = zsel_n(n755, n1965, n2112);
    let n2116: ZN = zsel_n(n755, n1971, n2113);
    let n2117: ZN = zsel_n(n755, n1977, n2114);
    let n2120: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2116);
    let n2121: ZN = zsel_n(n834, n2116, n2120);
    let n2122: ZN = zsel_n(n749, n2080, n2065);
    let n2123: ZN = zsel_n(n749, n2081, n2077);
    let n2124: ZN = zsel_n(n755, n1971, n2122);
    let n2125: ZN = zsel_n(n755, n1977, n2123);
    let n2126: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2124);
    let n2127: ZN = zsel_n(n834, n2124, n2126);
    let n2128: ZN = zsel_n(n749, n2105, n2090);
    let n2129: ZN = zsel_n(n749, n2106, n2102);
    let n2130: ZN = zsel_n(n755, n1971, n2128);
    let n2131: ZN = zsel_n(n755, n1977, n2129);
    let n2132: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2130);
    let n2133: ZN = zsel_n(n834, n2130, n2132);
    let n2134: ZN = zsel_n(n218, n2042, r_c396);
    let n2135: ZN = zsel_n(n218, n2039, r_c397);
    let n2136: ZN = zsel_n(n218, n2035, n2003);
    let n2137: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2018);
    let n2138: ZN = zsel_n(n755, r_c396, n2134);
    let n2139: ZN = zsel_n(n755, r_c397, n2135);
    let n2140: ZN = zsel_n(n755, n1971, n2136);
    let n2141: ZN = zsel_n(n755, n1977, n2137);
    let n2142: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2140);
    let n2143: ZN = zsel_n(n1042, n1625, n2057);
    let n2144: ZN = zsel_n(n1042, n2140, n2142);
    let n2145: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n2065);
    let n2146: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2077);
    let n2147: ZN = zsel_n(n755, n1971, n2145);
    let n2148: ZN = zsel_n(n755, n1977, n2146);
    let n2149: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2147);
    let n2150: ZN = zsel_n(n1042, n2147, n2149);
    let n2151: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n2090);
    let n2152: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2102);
    let n2153: ZN = zsel_n(n755, n1971, n2151);
    let n2154: ZN = zsel_n(n755, n1977, n2152);
    let n2155: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2153);
    let n2156: ZN = zsel_n(n1042, n2153, n2155);
    let n2157: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2003);
    let n2158: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n2018);
    let n2159: ZN = zsel_n(n755, n1971, n2157);
    let n2160: ZN = zsel_n(n755, n1977, n2158);
    let n2161: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2159);
    let n2162: ZN = zsel_n(n1042, n2159, n2161);
    let n2163: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2065);
    let n2164: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2077);
    let n2165: ZN = zsel_n(n755, n1971, n2163);
    let n2166: ZN = zsel_n(n755, n1977, n2164);
    let n2167: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2165);
    let n2168: ZN = zsel_n(n1042, n2165, n2167);
    let n2169: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2090);
    let n2170: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2102);
    let n2171: ZN = zsel_n(n755, n1971, n2169);
    let n2172: ZN = zsel_n(n755, n1977, n2170);
    let n2173: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2171);
    let n2174: ZN = zsel_n(n1042, n2171, n2173);
    let n2175: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n2018);
    let n2176: ZN = zsel_n(n755, n1977, n2175);
    let n2177: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2077);
    let n2178: ZN = zsel_n(n755, n1977, n2177);
    let n2179: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2102);
    let n2180: ZN = zsel_n(n755, n1977, n2179);
    let n2181: ZN = zsel_n(n218, n2035, n2113);
    let n2182: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2114);
    let n2183: ZN = zsel_n(n755, n1971, n2181);
    let n2184: ZN = zsel_n(n755, n1977, n2182);
    let n2185: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2183);
    let n2186: ZN = zsel_n(n1042, n2183, n2185);
    let n2187: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n2122);
    let n2188: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2123);
    let n2189: ZN = zsel_n(n755, n1971, n2187);
    let n2190: ZN = zsel_n(n755, n1977, n2188);
    let n2191: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2189);
    let n2192: ZN = zsel_n(n1042, n2189, n2191);
    let n2193: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n2128);
    let n2194: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2129);
    let n2195: ZN = zsel_n(n755, n1971, n2193);
    let n2196: ZN = zsel_n(n755, n1977, n2194);
    let n2197: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2195);
    let n2198: ZN = zsel_n(n1042, n2195, n2197);
    let n2199: ZN = zsel_n(n218, zn_splat(P8::from_raw(0i32)), n2113);
    let n2200: ZN = zsel_n(n218, zn_splat(P8::from_raw(-327680i32)), n2114);
    let n2201: ZN = zsel_n(n755, n1971, n2199);
    let n2202: ZN = zsel_n(n755, n1977, n2200);
    let n2203: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2201);
    let n2204: ZN = zsel_n(n1042, n2201, n2203);
    let n2205: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2122);
    let n2206: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2123);
    let n2207: ZN = zsel_n(n755, n1971, n2205);
    let n2208: ZN = zsel_n(n755, n1977, n2206);
    let n2209: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2207);
    let n2210: ZN = zsel_n(n1042, n2207, n2209);
    let n2211: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2128);
    let n2212: ZN = zsel_n(n218, zn_splat(P8::from_raw(-231700i32)), n2129);
    let n2213: ZN = zsel_n(n755, n1971, n2211);
    let n2214: ZN = zsel_n(n755, n1977, n2212);
    let n2215: ZN = zsel_n(n2054, zn_splat(P8::from_raw(0i32)), n2213);
    let n2216: ZN = zsel_n(n1042, n2213, n2215);
    let n2217: ZN = zsel_n(n218, zn_splat(P8::from_raw(327680i32)), n2114);
    let n2218: ZN = zsel_n(n755, n1977, n2217);
    let n2219: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2123);
    let n2220: ZN = zsel_n(n755, n1977, n2219);
    let n2221: ZN = zsel_n(n218, zn_splat(P8::from_raw(231700i32)), n2129);
    let n2222: ZN = zsel_n(n755, n1977, n2221);
    let n2223: ZB = zb_and(n1656, n1670);
    let n2224: ZB = zb_and(n1672, n1678);
    let n2225: ZB = zb_and(n1680, n1686);
    let n2226: ZB = zb_and(n1688, n1698);
    let n2227: ZB = zb_or(n2225, n2226);
    let n2228: ZB = zb_or(n2224, n2227);
    let n2229: ZB = zb_or(n2223, n2228);
    let n2230: ZB = zb_and(n1704, n1714);
    let n2231: ZB = zb_and(n1716, n1719);
    let n2232: ZB = zb_and(n1721, n1724);
    let n2233: ZB = zb_and(n1726, n1729);
    let n2234: ZB = zb_or(n2232, n2233);
    let n2235: ZB = zb_or(n2231, n2234);
    let n2236: ZB = zb_or(n2230, n2235);
    let n2237: ZB = zb_and(n1735, n1745);
    let n2238: ZB = zb_and(n1747, n1750);
    let n2239: ZB = zb_and(n1752, n1755);
    let n2240: ZB = zb_and(n1757, n1760);
    let n2241: ZB = zb_or(n2239, n2240);
    let n2242: ZB = zb_or(n2238, n2241);
    let n2243: ZB = zb_or(n2237, n2242);
    let n2244: ZB = zb_or(n2236, n2243);
    let n2245: ZB = zb_or(n2229, n2244);
    let n2246: ZB = zb_and(n1782, n1784);
    let n2247: ZB = zb_and(n1786, n1789);
    let n2248: ZB = zb_and(n1791, n1794);
    let n2249: ZB = zb_and(n1796, n1803);
    let n2250: ZB = zb_or(n2248, n2249);
    let n2251: ZB = zb_or(n2247, n2250);
    let n2252: ZB = zb_or(n2246, n2251);
    let n2253: ZB = zb_and(n1810, n1812);
    let n2254: ZB = zb_and(n1814, n1817);
    let n2255: ZB = zb_and(n1819, n1822);
    let n2256: ZB = zb_and(n1824, n1827);
    let n2257: ZB = zb_or(n2255, n2256);
    let n2258: ZB = zb_or(n2254, n2257);
    let n2259: ZB = zb_or(n2253, n2258);
    let n2260: ZB = zb_and(n1834, n1836);
    let n2261: ZB = zb_and(n1838, n1841);
    let n2262: ZB = zb_and(n1843, n1846);
    let n2263: ZB = zb_and(n1848, n1851);
    let n2264: ZB = zb_or(n2262, n2263);
    let n2265: ZB = zb_or(n2261, n2264);
    let n2266: ZB = zb_or(n2260, n2265);
    let n2267: ZB = zb_or(n2259, n2266);
    let n2268: ZB = zb_or(n2252, n2267);
    let n2269: ZB = zb_and(n1871, n1873);
    let n2270: ZB = zb_and(n1875, n1878);
    let n2271: ZB = zb_and(n1880, n1883);
    let n2272: ZB = zb_and(n1885, n1892);
    let n2273: ZB = zb_or(n2271, n2272);
    let n2274: ZB = zb_or(n2270, n2273);
    let n2275: ZB = zb_or(n2269, n2274);
    let n2276: ZB = zb_and(n1899, n1901);
    let n2277: ZB = zb_and(n1903, n1906);
    let n2278: ZB = zb_and(n1908, n1911);
    let n2279: ZB = zb_and(n1913, n1916);
    let n2280: ZB = zb_or(n2278, n2279);
    let n2281: ZB = zb_or(n2277, n2280);
    let n2282: ZB = zb_or(n2276, n2281);
    let n2283: ZB = zb_and(n1923, n1925);
    let n2284: ZB = zb_and(n1927, n1930);
    let n2285: ZB = zb_and(n1932, n1935);
    let n2286: ZB = zb_and(n1937, n1940);
    let n2287: ZB = zb_or(n2285, n2286);
    let n2288: ZB = zb_or(n2284, n2287);
    let n2289: ZB = zb_or(n2283, n2288);
    let n2290: ZB = zb_or(n2282, n2289);
    let n2291: ZB = zb_or(n2275, n2290);
    let n2292: ZB = zb_or(n2268, n2291);
    let n2293: ZB = zsel_b(n2268, n1771, n1860);
    let n2294: ZB = zb_or(n2245, n2292);
    let n2295: ZB = zsel_b(n2245, n1631, n2293);
    let n2296: ZB = zn_gt(n1626, zn_splat(P8::from_raw(8388608i32)));
    let n2297: ZB = zb_and(n1957, n2296);
    let n2298: ZB = zb_or(n2294, n2297);
    let n2299: ZB = zsel_b(n2294, n2295, n1958);
    let n2300: ZB = zb_and(n2046, n2298);
    let n2302: ZN = zsel_n(n2296, n1240, n1239);
    let n2303: ZN = zsel_n(n2294, n2302, n1239);
    let n2306: ZB = zb_and(n1149, n1373);
    let n2307: ZB = zb_and(n1150, n1375);
    let n2308: ZB = zb_or(n2306, n2307);
    let n2309: ZN = zsel_n(n285, zn_splat(P8::from_raw(65536i32)), r_c301);
    let n2310: ZN = zsel_n(n1380, n2309, r_c301);
    let n2311: ZN = zsel_n(n245, r_c301, n2310);
    let n2312: ZB = zb_and(n1642, n2308);
    let n2313: ZB = zb_and(n1643, n2308);
    let n2314: ZB = zb_and(n1655, n2312);
    let n2315: ZB = zb_and(n1642, n1654);
    let n2316: ZB = zb_and(n2308, n2315);
    let n2317: ZB = zb_and(n1670, n2316);
    let n2318: ZB = zb_and(n1671, n2316);
    let n2319: ZB = zb_and(n1678, n2318);
    let n2320: ZB = zb_and(n1679, n2318);
    let n2321: ZB = zb_and(n1686, n2320);
    let n2322: ZB = zb_and(n1687, n2320);
    let n2323: ZB = zb_and(n1698, n2322);
    let n2324: ZB = zb_and(n1699, n2322);
    let n2325: ZB = zb_or(n2321, n2323);
    let n2326: ZB = zb_or(n2319, n2325);
    let n2327: ZB = zb_or(n2317, n2326);
    let n2328: ZB = zb_and(n1703, n2324);
    let n2329: ZB = zb_and(n1699, n1702);
    let n2330: ZB = zb_and(n2322, n2329);
    let n2331: ZB = zb_and(n1714, n2330);
    let n2332: ZB = zb_and(n1715, n2330);
    let n2333: ZB = zb_and(n1719, n2332);
    let n2334: ZB = zb_and(n1720, n2332);
    let n2335: ZB = zb_and(n1724, n2334);
    let n2336: ZB = zb_and(n1725, n2334);
    let n2337: ZB = zb_and(n1729, n2336);
    let n2338: ZB = zb_and(n1730, n2336);
    let n2339: ZB = zb_or(n2335, n2337);
    let n2340: ZB = zb_or(n2333, n2339);
    let n2341: ZB = zb_or(n2331, n2340);
    let n2342: ZB = zb_and(n1734, n2338);
    let n2343: ZB = zb_and(n1730, n1733);
    let n2344: ZB = zb_and(n2336, n2343);
    let n2345: ZB = zb_and(n1745, n2344);
    let n2346: ZB = zb_and(n1746, n2344);
    let n2347: ZB = zb_and(n1750, n2346);
    let n2348: ZB = zb_and(n1751, n2346);
    let n2349: ZB = zb_and(n1755, n2348);
    let n2350: ZB = zb_and(n1756, n2348);
    let n2351: ZB = zb_and(n1760, n2350);
    let n2352: ZB = zb_and(n1761, n2350);
    let n2353: ZB = zb_or(n2349, n2351);
    let n2354: ZB = zb_or(n2347, n2353);
    let n2355: ZB = zb_or(n2345, n2354);
    let n2356: ZB = zb_or(n2342, n2352);
    let n2357: ZB = zb_or(n2341, n2355);
    let n2358: ZB = zb_or(n2328, n2356);
    let n2359: ZB = zb_or(n2327, n2357);
    let n2360: ZB = zb_or(n2314, n2358);
    let n2361: ZB = zb_and(n1773, n2360);
    let n2362: ZB = zb_and(n1774, n2360);
    let n2363: ZB = zb_and(n1655, n2361);
    let n2364: ZB = zb_and(n1654, n1773);
    let n2365: ZB = zb_and(n2360, n2364);
    let n2366: ZB = zb_and(n1784, n2365);
    let n2367: ZB = zb_and(n1785, n2365);
    let n2368: ZB = zb_and(n1789, n2367);
    let n2369: ZB = zb_and(n1790, n2367);
    let n2370: ZB = zb_and(n1794, n2369);
    let n2371: ZB = zb_and(n1795, n2369);
    let n2372: ZB = zb_and(n1803, n2371);
    let n2373: ZB = zb_and(n1804, n2371);
    let n2374: ZB = zb_or(n2370, n2372);
    let n2375: ZB = zb_or(n2368, n2374);
    let n2376: ZB = zb_or(n2366, n2375);
    let n2377: ZB = zb_and(n1703, n2373);
    let n2378: ZB = zb_and(n1702, n1804);
    let n2379: ZB = zb_and(n2371, n2378);
    let n2380: ZB = zb_and(n1812, n2379);
    let n2381: ZB = zb_and(n1813, n2379);
    let n2382: ZB = zb_and(n1817, n2381);
    let n2383: ZB = zb_and(n1818, n2381);
    let n2384: ZB = zb_and(n1822, n2383);
    let n2385: ZB = zb_and(n1823, n2383);
    let n2386: ZB = zb_and(n1827, n2385);
    let n2387: ZB = zb_and(n1828, n2385);
    let n2388: ZB = zb_or(n2384, n2386);
    let n2389: ZB = zb_or(n2382, n2388);
    let n2390: ZB = zb_or(n2380, n2389);
    let n2391: ZB = zb_and(n1734, n2387);
    let n2392: ZB = zb_and(n1733, n1828);
    let n2393: ZB = zb_and(n2385, n2392);
    let n2394: ZB = zb_and(n1836, n2393);
    let n2395: ZB = zb_and(n1837, n2393);
    let n2396: ZB = zb_and(n1841, n2395);
    let n2397: ZB = zb_and(n1842, n2395);
    let n2398: ZB = zb_and(n1846, n2397);
    let n2399: ZB = zb_and(n1847, n2397);
    let n2400: ZB = zb_and(n1851, n2399);
    let n2401: ZB = zb_and(n1852, n2399);
    let n2402: ZB = zb_or(n2398, n2400);
    let n2403: ZB = zb_or(n2396, n2402);
    let n2404: ZB = zb_or(n2394, n2403);
    let n2405: ZB = zb_or(n2391, n2401);
    let n2406: ZB = zb_or(n2390, n2404);
    let n2407: ZB = zb_or(n2377, n2405);
    let n2408: ZB = zb_or(n2376, n2406);
    let n2409: ZB = zb_or(n2363, n2407);
    let n2410: ZB = zb_and(n1862, n2409);
    let n2411: ZB = zb_and(n1863, n2409);
    let n2412: ZB = zb_and(n1655, n2410);
    let n2413: ZB = zb_and(n1654, n1862);
    let n2414: ZB = zb_and(n2409, n2413);
    let n2415: ZB = zb_and(n1873, n2414);
    let n2416: ZB = zb_and(n1874, n2414);
    let n2417: ZB = zb_and(n1878, n2416);
    let n2418: ZB = zb_and(n1879, n2416);
    let n2419: ZB = zb_and(n1883, n2418);
    let n2420: ZB = zb_and(n1884, n2418);
    let n2421: ZB = zb_and(n1892, n2420);
    let n2422: ZB = zb_and(n1893, n2420);
    let n2423: ZB = zb_or(n2419, n2421);
    let n2424: ZB = zb_or(n2417, n2423);
    let n2425: ZB = zb_or(n2415, n2424);
    let n2426: ZB = zb_and(n1703, n2422);
    let n2427: ZB = zb_and(n1702, n1893);
    let n2428: ZB = zb_and(n2420, n2427);
    let n2429: ZB = zb_and(n1901, n2428);
    let n2430: ZB = zb_and(n1902, n2428);
    let n2431: ZB = zb_and(n1906, n2430);
    let n2432: ZB = zb_and(n1907, n2430);
    let n2433: ZB = zb_and(n1911, n2432);
    let n2434: ZB = zb_and(n1912, n2432);
    let n2435: ZB = zb_and(n1916, n2434);
    let n2436: ZB = zb_and(n1917, n2434);
    let n2437: ZB = zb_or(n2433, n2435);
    let n2438: ZB = zb_or(n2431, n2437);
    let n2439: ZB = zb_or(n2429, n2438);
    let n2440: ZB = zb_and(n1734, n2436);
    let n2441: ZB = zb_and(n1733, n1917);
    let n2442: ZB = zb_and(n2434, n2441);
    let n2443: ZB = zb_and(n1925, n2442);
    let n2444: ZB = zb_and(n1926, n2442);
    let n2445: ZB = zb_and(n1930, n2444);
    let n2446: ZB = zb_and(n1931, n2444);
    let n2447: ZB = zb_and(n1935, n2446);
    let n2448: ZB = zb_and(n1936, n2446);
    let n2449: ZB = zb_and(n1940, n2448);
    let n2450: ZB = zb_and(n1941, n2448);
    let n2451: ZB = zb_or(n2447, n2449);
    let n2452: ZB = zb_or(n2445, n2451);
    let n2453: ZB = zb_or(n2443, n2452);
    let n2454: ZB = zb_or(n2440, n2450);
    let n2455: ZB = zb_or(n2439, n2453);
    let n2456: ZB = zb_or(n2426, n2454);
    let n2457: ZB = zb_or(n2425, n2455);
    let n2458: ZB = zb_or(n2412, n2456);
    let n2459: ZB = zb_or(n2408, n2457);
    let n2460: ZB = zsel_b(n2408, n1771, n1860);
    let n2461: ZB = zb_or(n2411, n2458);
    let n2462: ZB = zb_or(n2359, n2459);
    let n2463: ZB = zsel_b(n2359, n1631, n2460);
    let n2464: ZB = zb_or(n2362, n2461);
    let n2465: ZB = zb_or(n2313, n2464);
    let n2466: ZB = zb_and(n2296, n2465);
    let n2467: ZB = zb_or(n2462, n2466);
    let n2468: ZB = zsel_b(n2462, n2463, n1958);
    let n2469: ZB = zn_lt(n2311, zn_splat(P8::from_raw(65536i32)));
    let n2470: ZN = zsel_n(n2469, zn_splat(P8::from_raw(65536i32)), n2311);
    let n2471: ZN = zsel_n(n1963, n2470, n2311);
    let n2472: ZB = zn_gt(n2471, zn_splat(P8::from_raw(0i32)));
    let n2473: ZB = zb_and(n2046, n2467);
    let n2475: ZN = zsel_n(n2462, n2302, n1239);
    let n2477: ZB = zb_and(n218, n2472);
    let n2478: ZN = zsel_n(n2477, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2479: ZB = zb_or(r_c41, n2477);
    let n2480: ZN = zsel_n(n755, r_c20, n2478);
    let n2481: ZB = zsel_b(n755, r_c41, n2479);
    let n2484: ZB = zb_and(n1959, n2465);
    let n2485: ZB = zb_and(n2046, n2484);
    let n2486: ZB = zb_and(n2047, n2484);
    let n2487: ZB = zb_not(n2485);
    let n2488: ZB = zb_or(n2473, n2485);
    let n2489: ZB = zsel_b(n2485, n1958, n2468);
    let n2490: ZN = zsel_n(n2485, r_c87, n2475);
    let n2491: ZN = zsel_n(n2485, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2493: ZB = zb_not(n2048);
    let n2494: ZB = zb_or(n2048, n2300);
    let n2495: ZB = zsel_b(n2048, n1958, n2299);
    let n2496: ZN = zsel_n(n2048, r_c87, n2303);
    let n2497: ZN = zsel_n(n2048, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2499: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2500: ZN = zsel_n(n834, r_c246, n1142);
    let n2501: ZN = zsel_n(n834, r_c254, n1147);
    let n2502: ZB = zb_and(r_c310, n834);
    let n2503: ZB = zb_and(r_c311, n834);
    let n2504: ZN = zn_sub(n2471, zn_splat(P8::from_raw(65536i32)));
    let n2505: ZN = zsel_n(n834, n2499, r_c20);
    let n2506: ZN = zsel_n(n834, r_c277, n1391);
    let n2507: ZN = zsel_n(n834, r_c290, n1392);
    let n2508: ZN = zsel_n(n834, r_c298, n754);
    let n2509: ZN = zsel_n(n834, r_c300, n830);
    let n2510: ZN = zsel_n(n834, r_c301, n2471);
    let n2511: ZN = zsel_n(n834, r_c303, n1965);
    let n2512: ZN = zsel_n(n834, r_c317, n1625);
    let n2513: ZN = zsel_n(n834, r_c318, n1626);
    let n2514: ZB = zsel_b(n834, r_c399, n2043);
    let n2515: ZN = zsel_n(n834, r_c405, n1627);
    let n2516: ZN = zsel_n(n834, r_c406, n1628);
    let n2517: ZN = zsel_n(n834, r_c407, n2044);
    let n2518: ZN = zsel_n(n834, r_c408, n2045);
    let n2519: ZB = zb_or(n834, n2486);
    let n2520: ZB = zb_or(n834, n1958);
    let n2521: ZB = zn_gt(n2505, zn_splat(P8::from_raw(0i32)));
    let n2522: ZB = zn_lt(n2512, zn_splat(P8::from_raw(-65536i32)));
    let n2523: ZB = zn_gt(n2512, zn_splat(P8::from_raw(7929856i32)));
    let n2524: ZB = zb_or(n2522, n2523);
    let n2525: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2512);
    let n2526: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2525);
    let n2527: ZN = zsel_n(n2524, n2526, n2512);
    let n2528: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2517);
    let n2529: ZN = zsel_n(n2521, n2512, n2527);
    let n2530: ZN = zsel_n(n2521, n2517, n2528);
    let n2532: ZB = zsel_b(n834, r_c399, n2082);
    let n2533: ZN = zsel_n(n834, r_c407, n2083);
    let n2534: ZN = zsel_n(n834, r_c408, n2084);
    let n2535: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2533);
    let n2536: ZN = zsel_n(n2521, n2533, n2535);
    let n2537: ZB = zsel_b(n834, r_c399, n2107);
    let n2538: ZN = zsel_n(n834, r_c407, n2108);
    let n2539: ZN = zsel_n(n834, r_c408, n2109);
    let n2540: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2538);
    let n2541: ZN = zsel_n(n2521, n2538, n2540);
    let n2542: ZB = zb_or(r_c311, n174);
    let n2543: ZN = zsel_n(n834, r_c303, n2115);
    let n2544: ZN = zsel_n(n834, r_c407, n2116);
    let n2545: ZN = zsel_n(n834, r_c408, n2117);
    let n2546: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2544);
    let n2547: ZN = zsel_n(n2521, n2544, n2546);
    let n2548: ZN = zsel_n(n834, r_c407, n2124);
    let n2549: ZN = zsel_n(n834, r_c408, n2125);
    let n2550: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2548);
    let n2551: ZN = zsel_n(n2521, n2548, n2550);
    let n2552: ZN = zsel_n(n834, r_c407, n2130);
    let n2553: ZN = zsel_n(n834, r_c408, n2131);
    let n2554: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2552);
    let n2555: ZN = zsel_n(n2521, n2552, n2554);
    let n2556: ZB = zb_or(r_c310, n174);
    let n2557: ZN = zsel_n(n2477, zn_splat(P8::from_raw(655360i32)), n754);
    let n2558: ZN = zsel_n(n2477, zn_splat(P8::from_raw(262144i32)), r_c300);
    let n2559: ZN = zsel_n(n2477, n2504, n2471);
    let n2560: ZN = zsel_n(n2477, zn_splat(P8::from_raw(98304i32)), r_c395);
    let n2561: ZN = zsel_n(n2477, n2042, r_c396);
    let n2562: ZN = zsel_n(n2477, n2039, r_c397);
    let n2563: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), r_c398);
    let n2564: ZN = zsel_n(n2477, n2035, n2003);
    let n2565: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2018);
    let n2566: ZN = zsel_n(n755, n754, n2557);
    let n2567: ZN = zsel_n(n755, n756, n2558);
    let n2568: ZN = zsel_n(n755, n2471, n2559);
    let n2569: ZN = zsel_n(n755, r_c395, n2560);
    let n2570: ZN = zsel_n(n755, r_c396, n2561);
    let n2571: ZN = zsel_n(n755, r_c397, n2562);
    let n2572: ZN = zsel_n(n755, r_c398, n2563);
    let n2573: ZN = zsel_n(n755, n1971, n2564);
    let n2574: ZN = zsel_n(n755, n1977, n2565);
    let n2575: ZN = zsel_n(n834, n2499, n2480);
    let n2576: ZB = zsel_b(n834, r_c41, n2481);
    let n2577: ZN = zsel_n(n834, r_c298, n2566);
    let n2578: ZN = zsel_n(n834, r_c300, n2567);
    let n2579: ZN = zsel_n(n834, r_c301, n2568);
    let n2580: ZN = zsel_n(n834, r_c395, n2569);
    let n2581: ZN = zsel_n(n834, r_c396, n2570);
    let n2582: ZN = zsel_n(n834, r_c397, n2571);
    let n2583: ZN = zsel_n(n834, r_c398, n2572);
    let n2584: ZN = zsel_n(n834, r_c407, n2573);
    let n2585: ZN = zsel_n(n834, r_c408, n2574);
    let n2586: ZB = zn_gt(n2575, zn_splat(P8::from_raw(0i32)));
    let n2587: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2584);
    let n2588: ZN = zsel_n(n2586, n2512, n2527);
    let n2589: ZN = zsel_n(n2586, n2584, n2587);
    let n2590: ZN = zsel_n(n2477, zn_splat(P8::from_raw(69510i32)), r_c396);
    let n2591: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-131072i32)), r_c397);
    let n2592: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-327680i32)), n2065);
    let n2593: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2077);
    let n2594: ZN = zsel_n(n755, r_c396, n2590);
    let n2595: ZN = zsel_n(n755, r_c397, n2591);
    let n2596: ZN = zsel_n(n755, n1971, n2592);
    let n2597: ZN = zsel_n(n755, n1977, n2593);
    let n2598: ZN = zsel_n(n834, r_c396, n2594);
    let n2599: ZN = zsel_n(n834, r_c397, n2595);
    let n2600: ZN = zsel_n(n834, r_c407, n2596);
    let n2601: ZN = zsel_n(n834, r_c408, n2597);
    let n2602: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2600);
    let n2603: ZN = zsel_n(n2586, n2600, n2602);
    let n2604: ZN = zsel_n(n2477, zn_splat(P8::from_raw(131072i32)), r_c397);
    let n2605: ZN = zsel_n(n2477, zn_splat(P8::from_raw(327680i32)), n2090);
    let n2606: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2102);
    let n2607: ZN = zsel_n(n755, r_c397, n2604);
    let n2608: ZN = zsel_n(n755, n1971, n2605);
    let n2609: ZN = zsel_n(n755, n1977, n2606);
    let n2610: ZN = zsel_n(n834, r_c397, n2607);
    let n2611: ZN = zsel_n(n834, r_c407, n2608);
    let n2612: ZN = zsel_n(n834, r_c408, n2609);
    let n2613: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2611);
    let n2614: ZN = zsel_n(n2586, n2611, n2613);
    let n2615: ZN = zsel_n(n2477, zn_splat(P8::from_raw(69510i32)), r_c395);
    let n2616: ZN = zsel_n(n2477, zn_splat(P8::from_raw(98304i32)), r_c396);
    let n2617: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), r_c397);
    let n2618: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-98304i32)), r_c398);
    let n2619: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2003);
    let n2620: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-327680i32)), n2018);
    let n2621: ZN = zsel_n(n755, r_c395, n2615);
    let n2622: ZN = zsel_n(n755, r_c396, n2616);
    let n2623: ZN = zsel_n(n755, r_c397, n2617);
    let n2624: ZN = zsel_n(n755, r_c398, n2618);
    let n2625: ZN = zsel_n(n755, n1971, n2619);
    let n2626: ZN = zsel_n(n755, n1977, n2620);
    let n2627: ZN = zsel_n(n834, r_c395, n2621);
    let n2628: ZN = zsel_n(n834, r_c396, n2622);
    let n2629: ZN = zsel_n(n834, r_c397, n2623);
    let n2630: ZN = zsel_n(n834, r_c398, n2624);
    let n2631: ZN = zsel_n(n834, r_c407, n2625);
    let n2632: ZN = zsel_n(n834, r_c408, n2626);
    let n2633: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2631);
    let n2634: ZN = zsel_n(n2586, n2631, n2633);
    let n2635: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2065);
    let n2636: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2077);
    let n2637: ZN = zsel_n(n755, n1971, n2635);
    let n2638: ZN = zsel_n(n755, n1977, n2636);
    let n2639: ZN = zsel_n(n834, r_c407, n2637);
    let n2640: ZN = zsel_n(n834, r_c408, n2638);
    let n2641: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2639);
    let n2642: ZN = zsel_n(n2586, n2639, n2641);
    let n2643: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2090);
    let n2644: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2102);
    let n2645: ZN = zsel_n(n755, n1971, n2643);
    let n2646: ZN = zsel_n(n755, n1977, n2644);
    let n2647: ZN = zsel_n(n834, r_c407, n2645);
    let n2648: ZN = zsel_n(n834, r_c408, n2646);
    let n2649: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2647);
    let n2650: ZN = zsel_n(n2586, n2647, n2649);
    let n2651: ZN = zsel_n(n2477, zn_splat(P8::from_raw(131072i32)), r_c398);
    let n2652: ZN = zsel_n(n2477, zn_splat(P8::from_raw(327680i32)), n2018);
    let n2653: ZN = zsel_n(n755, r_c398, n2651);
    let n2654: ZN = zsel_n(n755, n1977, n2652);
    let n2655: ZN = zsel_n(n834, r_c398, n2653);
    let n2656: ZN = zsel_n(n834, r_c408, n2654);
    let n2657: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2077);
    let n2658: ZN = zsel_n(n755, n1977, n2657);
    let n2659: ZN = zsel_n(n834, r_c408, n2658);
    let n2660: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2102);
    let n2661: ZN = zsel_n(n755, n1977, n2660);
    let n2662: ZN = zsel_n(n834, r_c408, n2661);
    let n2663: ZN = zsel_n(n2477, n2035, n2113);
    let n2664: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2114);
    let n2665: ZN = zsel_n(n755, n1971, n2663);
    let n2666: ZN = zsel_n(n755, n1977, n2664);
    let n2667: ZN = zsel_n(n834, r_c407, n2665);
    let n2668: ZN = zsel_n(n834, r_c408, n2666);
    let n2669: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2667);
    let n2670: ZN = zsel_n(n2586, n2667, n2669);
    let n2671: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-327680i32)), n2122);
    let n2672: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2123);
    let n2673: ZN = zsel_n(n755, n1971, n2671);
    let n2674: ZN = zsel_n(n755, n1977, n2672);
    let n2675: ZN = zsel_n(n834, r_c407, n2673);
    let n2676: ZN = zsel_n(n834, r_c408, n2674);
    let n2677: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2675);
    let n2678: ZN = zsel_n(n2586, n2675, n2677);
    let n2679: ZN = zsel_n(n2477, zn_splat(P8::from_raw(327680i32)), n2128);
    let n2680: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2129);
    let n2681: ZN = zsel_n(n755, n1971, n2679);
    let n2682: ZN = zsel_n(n755, n1977, n2680);
    let n2683: ZN = zsel_n(n834, r_c407, n2681);
    let n2684: ZN = zsel_n(n834, r_c408, n2682);
    let n2685: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2683);
    let n2686: ZN = zsel_n(n2586, n2683, n2685);
    let n2687: ZN = zsel_n(n2477, zn_splat(P8::from_raw(0i32)), n2113);
    let n2688: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-327680i32)), n2114);
    let n2689: ZN = zsel_n(n755, n1971, n2687);
    let n2690: ZN = zsel_n(n755, n1977, n2688);
    let n2691: ZN = zsel_n(n834, r_c407, n2689);
    let n2692: ZN = zsel_n(n834, r_c408, n2690);
    let n2693: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2691);
    let n2694: ZN = zsel_n(n2586, n2691, n2693);
    let n2695: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2122);
    let n2696: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2123);
    let n2697: ZN = zsel_n(n755, n1971, n2695);
    let n2698: ZN = zsel_n(n755, n1977, n2696);
    let n2699: ZN = zsel_n(n834, r_c407, n2697);
    let n2700: ZN = zsel_n(n834, r_c408, n2698);
    let n2701: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2699);
    let n2702: ZN = zsel_n(n2586, n2699, n2701);
    let n2703: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2128);
    let n2704: ZN = zsel_n(n2477, zn_splat(P8::from_raw(-231700i32)), n2129);
    let n2705: ZN = zsel_n(n755, n1971, n2703);
    let n2706: ZN = zsel_n(n755, n1977, n2704);
    let n2707: ZN = zsel_n(n834, r_c407, n2705);
    let n2708: ZN = zsel_n(n834, r_c408, n2706);
    let n2709: ZN = zsel_n(n2524, zn_splat(P8::from_raw(0i32)), n2707);
    let n2710: ZN = zsel_n(n2586, n2707, n2709);
    let n2711: ZN = zsel_n(n2477, zn_splat(P8::from_raw(327680i32)), n2114);
    let n2712: ZN = zsel_n(n755, n1977, n2711);
    let n2713: ZN = zsel_n(n834, r_c408, n2712);
    let n2714: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2123);
    let n2715: ZN = zsel_n(n755, n1977, n2714);
    let n2716: ZN = zsel_n(n834, r_c408, n2715);
    let n2717: ZN = zsel_n(n2477, zn_splat(P8::from_raw(231700i32)), n2129);
    let n2718: ZN = zsel_n(n755, n1977, n2717);
    let n2719: ZN = zsel_n(n834, r_c408, n2718);
    let n2721: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n2722: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n2723: ZW = zw_add(zw_splat(0u64), n2721);
    let n2724: ZW = zw_add(zw_splat(0u64), n2722);
    let n2725: ZW = zw_cellmix_n(84u64, n142, 1542469173u64);
    let n2726: ZW = zw_cellmix_n(84u64, n142, 668265263u64);
    let n2727: ZW = zw_add(n2723, n2725);
    let n2728: ZW = zw_add(n2724, n2726);
    let n2729: ZW = zw_cellmix_n(85u64, n261, 1542469173u64);
    let n2730: ZW = zw_cellmix_n(85u64, n261, 668265263u64);
    let n2731: ZW = zw_add(n2727, n2729);
    let n2732: ZW = zw_add(n2728, n2730);
    let n2733: ZW = zw_cellmix_n(86u64, n260, 1542469173u64);
    let n2734: ZW = zw_cellmix_n(86u64, n260, 668265263u64);
    let n2735: ZW = zw_add(n2731, n2733);
    let n2736: ZW = zw_add(n2732, n2734);
    let n2737: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n2738: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n2739: ZW = zw_add(n2735, n2737);
    let n2740: ZW = zw_add(n2736, n2738);
    let n2741: ZW = zw_cellmix_n(261u64, n846, 1542469173u64);
    let n2742: ZW = zw_cellmix_n(261u64, n846, 668265263u64);
    let n2743: ZW = zw_add(n2739, n2741);
    let n2744: ZW = zw_add(n2740, n2742);
    let n2745: ZW = zw_cellmix_n(274u64, n847, 1542469173u64);
    let n2746: ZW = zw_cellmix_n(274u64, n847, 668265263u64);
    let n2747: ZW = zw_add(n2743, n2745);
    let n2748: ZW = zw_add(n2744, n2746);
    let n2749: ZW = zw_cellmix_n(302u64, n504, 1542469173u64);
    let n2750: ZW = zw_cellmix_n(302u64, n504, 668265263u64);
    let n2751: ZW = zw_add(n2747, n2749);
    let n2752: ZW = zw_add(n2748, n2750);
    let n2753: ZW = zw_cellmix_n(368u64, n393, 1542469173u64);
    let n2754: ZW = zw_cellmix_n(368u64, n393, 668265263u64);
    let n2755: ZW = zw_add(n2751, n2753);
    let n2756: ZW = zw_add(n2752, n2754);
    let n2757: ZW = zw_cellmix_n(369u64, n505, 1542469173u64);
    let n2758: ZW = zw_cellmix_n(369u64, n505, 668265263u64);
    let n2759: ZW = zw_add(n2755, n2757);
    let n2760: ZW = zw_add(n2756, n2758);
    let n2761: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n2762: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n2763: ZW = zw_add(n2759, n2761);
    let n2764: ZW = zw_add(n2760, n2762);
    let n2765: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n2766: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n2767: ZW = zw_add(n2763, n2765);
    let n2768: ZW = zw_add(n2764, n2766);
    let n2769: ZW = zw_cellmix_n(282u64, n754, 1542469173u64);
    let n2770: ZW = zw_cellmix_n(282u64, n754, 668265263u64);
    let n2771: ZW = zw_add(n2767, n2769);
    let n2772: ZW = zw_add(n2768, n2770);
    let n2773: ZW = zw_cellmix_n(284u64, n830, 1542469173u64);
    let n2774: ZW = zw_cellmix_n(284u64, n830, 668265263u64);
    let n2775: ZW = zw_add(n2771, n2773);
    let n2776: ZW = zw_add(n2772, n2774);
    let n2777: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n2778: ZW = zw_cellmix_n(285u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n2779: ZW = zw_add(n2775, n2777);
    let n2780: ZW = zw_add(n2776, n2778);
    let n2781: ZW = zw_cellmix_n(287u64, n753, 1542469173u64);
    let n2782: ZW = zw_cellmix_n(287u64, n753, 668265263u64);
    let n2783: ZW = zw_add(n2779, n2781);
    let n2784: ZW = zw_add(n2780, n2782);
    let n2785: ZW = zw_cellmix_b(294u64, zb_splat(false), 1542469173u64);
    let n2786: ZW = zw_cellmix_b(294u64, zb_splat(false), 668265263u64);
    let n2787: ZW = zw_add(n2783, n2785);
    let n2788: ZW = zw_add(n2784, n2786);
    let n2789: ZW = zw_cellmix_b(295u64, zb_splat(false), 1542469173u64);
    let n2790: ZW = zw_cellmix_b(295u64, zb_splat(false), 668265263u64);
    let n2791: ZW = zw_add(n2787, n2789);
    let n2792: ZW = zw_add(n2788, n2790);
    let n2793: ZW = zw_cellmix_n(301u64, n842, 1542469173u64);
    let n2794: ZW = zw_cellmix_n(301u64, n842, 668265263u64);
    let n2795: ZW = zw_add(n2791, n2793);
    let n2796: ZW = zw_add(n2792, n2794);
    let n2797: ZW = zw_cellmix_n(358u64, r_c395, 1542469173u64);
    let n2798: ZW = zw_cellmix_n(358u64, r_c395, 668265263u64);
    let n2799: ZW = zw_add(n2795, n2797);
    let n2800: ZW = zw_add(n2796, n2798);
    let n2801: ZW = zw_cellmix_n(359u64, r_c396, 1542469173u64);
    let n2802: ZW = zw_cellmix_n(359u64, r_c396, 668265263u64);
    let n2803: ZW = zw_add(n2799, n2801);
    let n2804: ZW = zw_add(n2800, n2802);
    let n2805: ZW = zw_cellmix_n(360u64, r_c397, 1542469173u64);
    let n2806: ZW = zw_cellmix_n(360u64, r_c397, 668265263u64);
    let n2807: ZW = zw_add(n2803, n2805);
    let n2808: ZW = zw_add(n2804, n2806);
    let n2809: ZW = zw_cellmix_n(361u64, r_c398, 1542469173u64);
    let n2810: ZW = zw_cellmix_n(361u64, r_c398, 668265263u64);
    let n2811: ZW = zw_add(n2807, n2809);
    let n2812: ZW = zw_add(n2808, n2810);
    let n2813: ZW = zw_cellmix_b(362u64, n831, 1542469173u64);
    let n2814: ZW = zw_cellmix_b(362u64, n831, 668265263u64);
    let n2815: ZW = zw_add(n2811, n2813);
    let n2816: ZW = zw_add(n2812, n2814);
    let n2817: ZW = zw_cellmix_n(370u64, n843, 1542469173u64);
    let n2818: ZW = zw_cellmix_n(370u64, n843, 668265263u64);
    let n2819: ZW = zw_add(n2815, n2817);
    let n2820: ZW = zw_add(n2816, n2818);
    let n2821: ZW = zw_cellmix_n(371u64, n833, 1542469173u64);
    let n2822: ZW = zw_cellmix_n(371u64, n833, 668265263u64);
    let n2823: ZW = zw_add(n2819, n2821);
    let n2824: ZW = zw_add(n2820, n2822);
    let n2825: ZW = zw_cellmix_b(362u64, n973, 1542469173u64);
    let n2826: ZW = zw_cellmix_b(362u64, n973, 668265263u64);
    let n2827: ZW = zw_add(n2811, n2825);
    let n2828: ZW = zw_add(n2812, n2826);
    let n2829: ZW = zw_cellmix_n(370u64, n977, 1542469173u64);
    let n2830: ZW = zw_cellmix_n(370u64, n977, 668265263u64);
    let n2831: ZW = zw_add(n2827, n2829);
    let n2832: ZW = zw_add(n2828, n2830);
    let n2833: ZW = zw_cellmix_n(371u64, n975, 1542469173u64);
    let n2834: ZW = zw_cellmix_n(371u64, n975, 668265263u64);
    let n2835: ZW = zw_add(n2831, n2833);
    let n2836: ZW = zw_add(n2832, n2834);
    let n2837: ZW = zw_cellmix_b(362u64, n995, 1542469173u64);
    let n2838: ZW = zw_cellmix_b(362u64, n995, 668265263u64);
    let n2839: ZW = zw_add(n2811, n2837);
    let n2840: ZW = zw_add(n2812, n2838);
    let n2841: ZW = zw_cellmix_n(370u64, n999, 1542469173u64);
    let n2842: ZW = zw_cellmix_n(370u64, n999, 668265263u64);
    let n2843: ZW = zw_add(n2839, n2841);
    let n2844: ZW = zw_add(n2840, n2842);
    let n2845: ZW = zw_cellmix_n(371u64, n997, 1542469173u64);
    let n2846: ZW = zw_cellmix_n(371u64, n997, 668265263u64);
    let n2847: ZW = zw_add(n2843, n2845);
    let n2848: ZW = zw_add(n2844, n2846);
    let n2849: ZW = zw_cellmix_n(287u64, n1003, 1542469173u64);
    let n2850: ZW = zw_cellmix_n(287u64, n1003, 668265263u64);
    let n2851: ZW = zw_add(n2779, n2849);
    let n2852: ZW = zw_add(n2780, n2850);
    let n2853: ZW = zw_add(n2851, n2785);
    let n2854: ZW = zw_add(n2852, n2786);
    let n2855: ZW = zw_cellmix_b(295u64, zb_splat(true), 1542469173u64);
    let n2856: ZW = zw_cellmix_b(295u64, zb_splat(true), 668265263u64);
    let n2857: ZW = zw_add(n2853, n2855);
    let n2858: ZW = zw_add(n2854, n2856);
    let n2859: ZW = zw_add(n2857, n2793);
    let n2860: ZW = zw_add(n2858, n2794);
    let n2861: ZW = zw_add(n2859, n2797);
    let n2862: ZW = zw_add(n2860, n2798);
    let n2863: ZW = zw_add(n2861, n2801);
    let n2864: ZW = zw_add(n2862, n2802);
    let n2865: ZW = zw_add(n2863, n2805);
    let n2866: ZW = zw_add(n2864, n2806);
    let n2867: ZW = zw_add(n2865, n2809);
    let n2868: ZW = zw_add(n2866, n2810);
    let n2869: ZW = zw_add(n2867, n2813);
    let n2870: ZW = zw_add(n2868, n2814);
    let n2871: ZW = zw_cellmix_n(370u64, n1007, 1542469173u64);
    let n2872: ZW = zw_cellmix_n(370u64, n1007, 668265263u64);
    let n2873: ZW = zw_add(n2869, n2871);
    let n2874: ZW = zw_add(n2870, n2872);
    let n2875: ZW = zw_cellmix_n(371u64, n1005, 1542469173u64);
    let n2876: ZW = zw_cellmix_n(371u64, n1005, 668265263u64);
    let n2877: ZW = zw_add(n2873, n2875);
    let n2878: ZW = zw_add(n2874, n2876);
    let n2879: ZW = zw_add(n2867, n2825);
    let n2880: ZW = zw_add(n2868, n2826);
    let n2881: ZW = zw_cellmix_n(370u64, n1013, 1542469173u64);
    let n2882: ZW = zw_cellmix_n(370u64, n1013, 668265263u64);
    let n2883: ZW = zw_add(n2879, n2881);
    let n2884: ZW = zw_add(n2880, n2882);
    let n2885: ZW = zw_cellmix_n(371u64, n1011, 1542469173u64);
    let n2886: ZW = zw_cellmix_n(371u64, n1011, 668265263u64);
    let n2887: ZW = zw_add(n2883, n2885);
    let n2888: ZW = zw_add(n2884, n2886);
    let n2889: ZW = zw_add(n2867, n2837);
    let n2890: ZW = zw_add(n2868, n2838);
    let n2891: ZW = zw_cellmix_n(370u64, n1019, 1542469173u64);
    let n2892: ZW = zw_cellmix_n(370u64, n1019, 668265263u64);
    let n2893: ZW = zw_add(n2889, n2891);
    let n2894: ZW = zw_add(n2890, n2892);
    let n2895: ZW = zw_cellmix_n(371u64, n1017, 1542469173u64);
    let n2896: ZW = zw_cellmix_n(371u64, n1017, 668265263u64);
    let n2897: ZW = zw_add(n2893, n2895);
    let n2898: ZW = zw_add(n2894, n2896);
    let n2899: ZW = zw_cellmix_n(20u64, n1031, 1542469173u64);
    let n2900: ZW = zw_cellmix_n(20u64, n1031, 668265263u64);
    let n2901: ZW = zw_add(n2759, n2899);
    let n2902: ZW = zw_add(n2760, n2900);
    let n2903: ZW = zw_cellmix_b(41u64, n1032, 1542469173u64);
    let n2904: ZW = zw_cellmix_b(41u64, n1032, 668265263u64);
    let n2905: ZW = zw_add(n2901, n2903);
    let n2906: ZW = zw_add(n2902, n2904);
    let n2907: ZW = zw_cellmix_n(282u64, n1033, 1542469173u64);
    let n2908: ZW = zw_cellmix_n(282u64, n1033, 668265263u64);
    let n2909: ZW = zw_add(n2905, n2907);
    let n2910: ZW = zw_add(n2906, n2908);
    let n2911: ZW = zw_cellmix_n(284u64, n1034, 1542469173u64);
    let n2912: ZW = zw_cellmix_n(284u64, n1034, 668265263u64);
    let n2913: ZW = zw_add(n2909, n2911);
    let n2914: ZW = zw_add(n2910, n2912);
    let n2915: ZW = zw_cellmix_n(285u64, n1035, 1542469173u64);
    let n2916: ZW = zw_cellmix_n(285u64, n1035, 668265263u64);
    let n2917: ZW = zw_add(n2913, n2915);
    let n2918: ZW = zw_add(n2914, n2916);
    let n2919: ZW = zw_add(n2917, n2781);
    let n2920: ZW = zw_add(n2918, n2782);
    let n2921: ZW = zw_cellmix_b(294u64, zb_splat(true), 1542469173u64);
    let n2922: ZW = zw_cellmix_b(294u64, zb_splat(true), 668265263u64);
    let n2923: ZW = zw_add(n2919, n2921);
    let n2924: ZW = zw_add(n2920, n2922);
    let n2925: ZW = zw_add(n2923, n2789);
    let n2926: ZW = zw_add(n2924, n2790);
    let n2927: ZW = zw_cellmix_n(301u64, n1044, 1542469173u64);
    let n2928: ZW = zw_cellmix_n(301u64, n1044, 668265263u64);
    let n2929: ZW = zw_add(n2925, n2927);
    let n2930: ZW = zw_add(n2926, n2928);
    let n2931: ZW = zw_cellmix_n(358u64, n1036, 1542469173u64);
    let n2932: ZW = zw_cellmix_n(358u64, n1036, 668265263u64);
    let n2933: ZW = zw_add(n2929, n2931);
    let n2934: ZW = zw_add(n2930, n2932);
    let n2935: ZW = zw_cellmix_n(359u64, n1037, 1542469173u64);
    let n2936: ZW = zw_cellmix_n(359u64, n1037, 668265263u64);
    let n2937: ZW = zw_add(n2933, n2935);
    let n2938: ZW = zw_add(n2934, n2936);
    let n2939: ZW = zw_cellmix_n(360u64, n1038, 1542469173u64);
    let n2940: ZW = zw_cellmix_n(360u64, n1038, 668265263u64);
    let n2941: ZW = zw_add(n2937, n2939);
    let n2942: ZW = zw_add(n2938, n2940);
    let n2943: ZW = zw_cellmix_n(361u64, n1039, 1542469173u64);
    let n2944: ZW = zw_cellmix_n(361u64, n1039, 668265263u64);
    let n2945: ZW = zw_add(n2941, n2943);
    let n2946: ZW = zw_add(n2942, n2944);
    let n2947: ZW = zw_add(n2945, n2813);
    let n2948: ZW = zw_add(n2946, n2814);
    let n2949: ZW = zw_cellmix_n(370u64, n1045, 1542469173u64);
    let n2950: ZW = zw_cellmix_n(370u64, n1045, 668265263u64);
    let n2951: ZW = zw_add(n2947, n2949);
    let n2952: ZW = zw_add(n2948, n2950);
    let n2953: ZW = zw_cellmix_n(371u64, n1041, 1542469173u64);
    let n2954: ZW = zw_cellmix_n(371u64, n1041, 668265263u64);
    let n2955: ZW = zw_add(n2951, n2953);
    let n2956: ZW = zw_add(n2952, n2954);
    let n2957: ZW = zw_cellmix_n(359u64, n1050, 1542469173u64);
    let n2958: ZW = zw_cellmix_n(359u64, n1050, 668265263u64);
    let n2959: ZW = zw_add(n2933, n2957);
    let n2960: ZW = zw_add(n2934, n2958);
    let n2961: ZW = zw_cellmix_n(360u64, n1051, 1542469173u64);
    let n2962: ZW = zw_cellmix_n(360u64, n1051, 668265263u64);
    let n2963: ZW = zw_add(n2959, n2961);
    let n2964: ZW = zw_add(n2960, n2962);
    let n2965: ZW = zw_add(n2963, n2943);
    let n2966: ZW = zw_add(n2964, n2944);
    let n2967: ZW = zw_add(n2965, n2825);
    let n2968: ZW = zw_add(n2966, n2826);
    let n2969: ZW = zw_cellmix_n(370u64, n1055, 1542469173u64);
    let n2970: ZW = zw_cellmix_n(370u64, n1055, 668265263u64);
    let n2971: ZW = zw_add(n2967, n2969);
    let n2972: ZW = zw_add(n2968, n2970);
    let n2973: ZW = zw_cellmix_n(371u64, n1053, 1542469173u64);
    let n2974: ZW = zw_cellmix_n(371u64, n1053, 668265263u64);
    let n2975: ZW = zw_add(n2971, n2973);
    let n2976: ZW = zw_add(n2972, n2974);
    let n2977: ZW = zw_cellmix_n(360u64, n1059, 1542469173u64);
    let n2978: ZW = zw_cellmix_n(360u64, n1059, 668265263u64);
    let n2979: ZW = zw_add(n2959, n2977);
    let n2980: ZW = zw_add(n2960, n2978);
    let n2981: ZW = zw_add(n2979, n2943);
    let n2982: ZW = zw_add(n2980, n2944);
    let n2983: ZW = zw_add(n2981, n2837);
    let n2984: ZW = zw_add(n2982, n2838);
    let n2985: ZW = zw_cellmix_n(370u64, n1063, 1542469173u64);
    let n2986: ZW = zw_cellmix_n(370u64, n1063, 668265263u64);
    let n2987: ZW = zw_add(n2983, n2985);
    let n2988: ZW = zw_add(n2984, n2986);
    let n2989: ZW = zw_cellmix_n(371u64, n1061, 1542469173u64);
    let n2990: ZW = zw_cellmix_n(371u64, n1061, 668265263u64);
    let n2991: ZW = zw_add(n2987, n2989);
    let n2992: ZW = zw_add(n2988, n2990);
    let n2993: ZW = zw_cellmix_n(358u64, n1071, 1542469173u64);
    let n2994: ZW = zw_cellmix_n(358u64, n1071, 668265263u64);
    let n2995: ZW = zw_add(n2929, n2993);
    let n2996: ZW = zw_add(n2930, n2994);
    let n2997: ZW = zw_cellmix_n(359u64, n1072, 1542469173u64);
    let n2998: ZW = zw_cellmix_n(359u64, n1072, 668265263u64);
    let n2999: ZW = zw_add(n2995, n2997);
    let n3000: ZW = zw_add(n2996, n2998);
    let n3001: ZW = zw_cellmix_n(360u64, n1073, 1542469173u64);
    let n3002: ZW = zw_cellmix_n(360u64, n1073, 668265263u64);
    let n3003: ZW = zw_add(n2999, n3001);
    let n3004: ZW = zw_add(n3000, n3002);
    let n3005: ZW = zw_cellmix_n(361u64, n1074, 1542469173u64);
    let n3006: ZW = zw_cellmix_n(361u64, n1074, 668265263u64);
    let n3007: ZW = zw_add(n3003, n3005);
    let n3008: ZW = zw_add(n3004, n3006);
    let n3009: ZW = zw_add(n3007, n2813);
    let n3010: ZW = zw_add(n3008, n2814);
    let n3011: ZW = zw_cellmix_n(370u64, n1078, 1542469173u64);
    let n3012: ZW = zw_cellmix_n(370u64, n1078, 668265263u64);
    let n3013: ZW = zw_add(n3009, n3011);
    let n3014: ZW = zw_add(n3010, n3012);
    let n3015: ZW = zw_cellmix_n(371u64, n1076, 1542469173u64);
    let n3016: ZW = zw_cellmix_n(371u64, n1076, 668265263u64);
    let n3017: ZW = zw_add(n3013, n3015);
    let n3018: ZW = zw_add(n3014, n3016);
    let n3019: ZW = zw_add(n2995, n2957);
    let n3020: ZW = zw_add(n2996, n2958);
    let n3021: ZW = zw_add(n3019, n2961);
    let n3022: ZW = zw_add(n3020, n2962);
    let n3023: ZW = zw_add(n3021, n3005);
    let n3024: ZW = zw_add(n3022, n3006);
    let n3025: ZW = zw_add(n3023, n2825);
    let n3026: ZW = zw_add(n3024, n2826);
    let n3027: ZW = zw_cellmix_n(370u64, n1084, 1542469173u64);
    let n3028: ZW = zw_cellmix_n(370u64, n1084, 668265263u64);
    let n3029: ZW = zw_add(n3025, n3027);
    let n3030: ZW = zw_add(n3026, n3028);
    let n3031: ZW = zw_cellmix_n(371u64, n1082, 1542469173u64);
    let n3032: ZW = zw_cellmix_n(371u64, n1082, 668265263u64);
    let n3033: ZW = zw_add(n3029, n3031);
    let n3034: ZW = zw_add(n3030, n3032);
    let n3035: ZW = zw_add(n3019, n2977);
    let n3036: ZW = zw_add(n3020, n2978);
    let n3037: ZW = zw_add(n3035, n3005);
    let n3038: ZW = zw_add(n3036, n3006);
    let n3039: ZW = zw_add(n3037, n2837);
    let n3040: ZW = zw_add(n3038, n2838);
    let n3041: ZW = zw_cellmix_n(370u64, n1090, 1542469173u64);
    let n3042: ZW = zw_cellmix_n(370u64, n1090, 668265263u64);
    let n3043: ZW = zw_add(n3039, n3041);
    let n3044: ZW = zw_add(n3040, n3042);
    let n3045: ZW = zw_cellmix_n(371u64, n1088, 1542469173u64);
    let n3046: ZW = zw_cellmix_n(371u64, n1088, 668265263u64);
    let n3047: ZW = zw_add(n3043, n3045);
    let n3048: ZW = zw_add(n3044, n3046);
    let n3049: ZW = zw_cellmix_n(361u64, n1093, 1542469173u64);
    let n3050: ZW = zw_cellmix_n(361u64, n1093, 668265263u64);
    let n3051: ZW = zw_add(n3003, n3049);
    let n3052: ZW = zw_add(n3004, n3050);
    let n3053: ZW = zw_add(n3051, n2813);
    let n3054: ZW = zw_add(n3052, n2814);
    let n3055: ZW = zw_add(n3053, n3011);
    let n3056: ZW = zw_add(n3054, n3012);
    let n3057: ZW = zw_cellmix_n(371u64, n1094, 1542469173u64);
    let n3058: ZW = zw_cellmix_n(371u64, n1094, 668265263u64);
    let n3059: ZW = zw_add(n3055, n3057);
    let n3060: ZW = zw_add(n3056, n3058);
    let n3061: ZW = zw_add(n3021, n3049);
    let n3062: ZW = zw_add(n3022, n3050);
    let n3063: ZW = zw_add(n3061, n2825);
    let n3064: ZW = zw_add(n3062, n2826);
    let n3065: ZW = zw_add(n3063, n3027);
    let n3066: ZW = zw_add(n3064, n3028);
    let n3067: ZW = zw_cellmix_n(371u64, n1096, 1542469173u64);
    let n3068: ZW = zw_cellmix_n(371u64, n1096, 668265263u64);
    let n3069: ZW = zw_add(n3065, n3067);
    let n3070: ZW = zw_add(n3066, n3068);
    let n3071: ZW = zw_add(n3035, n3049);
    let n3072: ZW = zw_add(n3036, n3050);
    let n3073: ZW = zw_add(n3071, n2837);
    let n3074: ZW = zw_add(n3072, n2838);
    let n3075: ZW = zw_add(n3073, n3041);
    let n3076: ZW = zw_add(n3074, n3042);
    let n3077: ZW = zw_cellmix_n(371u64, n1098, 1542469173u64);
    let n3078: ZW = zw_cellmix_n(371u64, n1098, 668265263u64);
    let n3079: ZW = zw_add(n3075, n3077);
    let n3080: ZW = zw_add(n3076, n3078);
    let n3081: ZW = zw_add(n2917, n2849);
    let n3082: ZW = zw_add(n2918, n2850);
    let n3083: ZW = zw_add(n3081, n2921);
    let n3084: ZW = zw_add(n3082, n2922);
    let n3085: ZW = zw_add(n3083, n2855);
    let n3086: ZW = zw_add(n3084, n2856);
    let n3087: ZW = zw_add(n3085, n2927);
    let n3088: ZW = zw_add(n3086, n2928);
    let n3089: ZW = zw_add(n3087, n2931);
    let n3090: ZW = zw_add(n3088, n2932);
    let n3091: ZW = zw_add(n3089, n2935);
    let n3092: ZW = zw_add(n3090, n2936);
    let n3093: ZW = zw_add(n3091, n2939);
    let n3094: ZW = zw_add(n3092, n2940);
    let n3095: ZW = zw_add(n3093, n2943);
    let n3096: ZW = zw_add(n3094, n2944);
    let n3097: ZW = zw_add(n3095, n2813);
    let n3098: ZW = zw_add(n3096, n2814);
    let n3099: ZW = zw_cellmix_n(370u64, n1104, 1542469173u64);
    let n3100: ZW = zw_cellmix_n(370u64, n1104, 668265263u64);
    let n3101: ZW = zw_add(n3097, n3099);
    let n3102: ZW = zw_add(n3098, n3100);
    let n3103: ZW = zw_cellmix_n(371u64, n1102, 1542469173u64);
    let n3104: ZW = zw_cellmix_n(371u64, n1102, 668265263u64);
    let n3105: ZW = zw_add(n3101, n3103);
    let n3106: ZW = zw_add(n3102, n3104);
    let n3107: ZW = zw_add(n3089, n2957);
    let n3108: ZW = zw_add(n3090, n2958);
    let n3109: ZW = zw_add(n3107, n2961);
    let n3110: ZW = zw_add(n3108, n2962);
    let n3111: ZW = zw_add(n3109, n2943);
    let n3112: ZW = zw_add(n3110, n2944);
    let n3113: ZW = zw_add(n3111, n2825);
    let n3114: ZW = zw_add(n3112, n2826);
    let n3115: ZW = zw_cellmix_n(370u64, n1110, 1542469173u64);
    let n3116: ZW = zw_cellmix_n(370u64, n1110, 668265263u64);
    let n3117: ZW = zw_add(n3113, n3115);
    let n3118: ZW = zw_add(n3114, n3116);
    let n3119: ZW = zw_cellmix_n(371u64, n1108, 1542469173u64);
    let n3120: ZW = zw_cellmix_n(371u64, n1108, 668265263u64);
    let n3121: ZW = zw_add(n3117, n3119);
    let n3122: ZW = zw_add(n3118, n3120);
    let n3123: ZW = zw_add(n3107, n2977);
    let n3124: ZW = zw_add(n3108, n2978);
    let n3125: ZW = zw_add(n3123, n2943);
    let n3126: ZW = zw_add(n3124, n2944);
    let n3127: ZW = zw_add(n3125, n2837);
    let n3128: ZW = zw_add(n3126, n2838);
    let n3129: ZW = zw_cellmix_n(370u64, n1116, 1542469173u64);
    let n3130: ZW = zw_cellmix_n(370u64, n1116, 668265263u64);
    let n3131: ZW = zw_add(n3127, n3129);
    let n3132: ZW = zw_add(n3128, n3130);
    let n3133: ZW = zw_cellmix_n(371u64, n1114, 1542469173u64);
    let n3134: ZW = zw_cellmix_n(371u64, n1114, 668265263u64);
    let n3135: ZW = zw_add(n3131, n3133);
    let n3136: ZW = zw_add(n3132, n3134);
    let n3137: ZW = zw_add(n3087, n2993);
    let n3138: ZW = zw_add(n3088, n2994);
    let n3139: ZW = zw_add(n3137, n2997);
    let n3140: ZW = zw_add(n3138, n2998);
    let n3141: ZW = zw_add(n3139, n3001);
    let n3142: ZW = zw_add(n3140, n3002);
    let n3143: ZW = zw_add(n3141, n3005);
    let n3144: ZW = zw_add(n3142, n3006);
    let n3145: ZW = zw_add(n3143, n2813);
    let n3146: ZW = zw_add(n3144, n2814);
    let n3147: ZW = zw_cellmix_n(370u64, n1122, 1542469173u64);
    let n3148: ZW = zw_cellmix_n(370u64, n1122, 668265263u64);
    let n3149: ZW = zw_add(n3145, n3147);
    let n3150: ZW = zw_add(n3146, n3148);
    let n3151: ZW = zw_cellmix_n(371u64, n1120, 1542469173u64);
    let n3152: ZW = zw_cellmix_n(371u64, n1120, 668265263u64);
    let n3153: ZW = zw_add(n3149, n3151);
    let n3154: ZW = zw_add(n3150, n3152);
    let n3155: ZW = zw_add(n3137, n2957);
    let n3156: ZW = zw_add(n3138, n2958);
    let n3157: ZW = zw_add(n3155, n2961);
    let n3158: ZW = zw_add(n3156, n2962);
    let n3159: ZW = zw_add(n3157, n3005);
    let n3160: ZW = zw_add(n3158, n3006);
    let n3161: ZW = zw_add(n3159, n2825);
    let n3162: ZW = zw_add(n3160, n2826);
    let n3163: ZW = zw_cellmix_n(370u64, n1128, 1542469173u64);
    let n3164: ZW = zw_cellmix_n(370u64, n1128, 668265263u64);
    let n3165: ZW = zw_add(n3161, n3163);
    let n3166: ZW = zw_add(n3162, n3164);
    let n3167: ZW = zw_cellmix_n(371u64, n1126, 1542469173u64);
    let n3168: ZW = zw_cellmix_n(371u64, n1126, 668265263u64);
    let n3169: ZW = zw_add(n3165, n3167);
    let n3170: ZW = zw_add(n3166, n3168);
    let n3171: ZW = zw_add(n3155, n2977);
    let n3172: ZW = zw_add(n3156, n2978);
    let n3173: ZW = zw_add(n3171, n3005);
    let n3174: ZW = zw_add(n3172, n3006);
    let n3175: ZW = zw_add(n3173, n2837);
    let n3176: ZW = zw_add(n3174, n2838);
    let n3177: ZW = zw_cellmix_n(370u64, n1134, 1542469173u64);
    let n3178: ZW = zw_cellmix_n(370u64, n1134, 668265263u64);
    let n3179: ZW = zw_add(n3175, n3177);
    let n3180: ZW = zw_add(n3176, n3178);
    let n3181: ZW = zw_cellmix_n(371u64, n1132, 1542469173u64);
    let n3182: ZW = zw_cellmix_n(371u64, n1132, 668265263u64);
    let n3183: ZW = zw_add(n3179, n3181);
    let n3184: ZW = zw_add(n3180, n3182);
    let n3185: ZW = zw_add(n3141, n3049);
    let n3186: ZW = zw_add(n3142, n3050);
    let n3187: ZW = zw_add(n3185, n2813);
    let n3188: ZW = zw_add(n3186, n2814);
    let n3189: ZW = zw_add(n3187, n3147);
    let n3190: ZW = zw_add(n3188, n3148);
    let n3191: ZW = zw_cellmix_n(371u64, n1136, 1542469173u64);
    let n3192: ZW = zw_cellmix_n(371u64, n1136, 668265263u64);
    let n3193: ZW = zw_add(n3189, n3191);
    let n3194: ZW = zw_add(n3190, n3192);
    let n3195: ZW = zw_add(n3157, n3049);
    let n3196: ZW = zw_add(n3158, n3050);
    let n3197: ZW = zw_add(n3195, n2825);
    let n3198: ZW = zw_add(n3196, n2826);
    let n3199: ZW = zw_add(n3197, n3163);
    let n3200: ZW = zw_add(n3198, n3164);
    let n3201: ZW = zw_cellmix_n(371u64, n1138, 1542469173u64);
    let n3202: ZW = zw_cellmix_n(371u64, n1138, 668265263u64);
    let n3203: ZW = zw_add(n3199, n3201);
    let n3204: ZW = zw_add(n3200, n3202);
    let n3205: ZW = zw_add(n3171, n3049);
    let n3206: ZW = zw_add(n3172, n3050);
    let n3207: ZW = zw_add(n3205, n2837);
    let n3208: ZW = zw_add(n3206, n2838);
    let n3209: ZW = zw_add(n3207, n3177);
    let n3210: ZW = zw_add(n3208, n3178);
    let n3211: ZW = zw_cellmix_n(371u64, n1140, 1542469173u64);
    let n3212: ZW = zw_cellmix_n(371u64, n1140, 668265263u64);
    let n3213: ZW = zw_add(n3209, n3211);
    let n3214: ZW = zw_add(n3210, n3212);
    let n3215: ZW = zw_cellmix_n(246u64, n1142, 1542469173u64);
    let n3216: ZW = zw_cellmix_n(246u64, n1142, 668265263u64);
    let n3217: ZW = zw_add(n2739, n3215);
    let n3218: ZW = zw_add(n2740, n3216);
    let n3219: ZW = zw_cellmix_n(254u64, n1147, 1542469173u64);
    let n3220: ZW = zw_cellmix_n(254u64, n1147, 668265263u64);
    let n3221: ZW = zw_add(n3217, n3219);
    let n3222: ZW = zw_add(n3218, n3220);
    let n3223: ZW = zw_cellmix_n(278u64, n846, 1542469173u64);
    let n3224: ZW = zw_cellmix_n(278u64, n846, 668265263u64);
    let n3225: ZW = zw_add(n3221, n3223);
    let n3226: ZW = zw_add(n3222, n3224);
    let n3227: ZW = zw_cellmix_n(291u64, n847, 1542469173u64);
    let n3228: ZW = zw_cellmix_n(291u64, n847, 668265263u64);
    let n3229: ZW = zw_add(n3225, n3227);
    let n3230: ZW = zw_add(n3226, n3228);
    let n3231: ZW = zw_cellmix_n(319u64, n504, 1542469173u64);
    let n3232: ZW = zw_cellmix_n(319u64, n504, 668265263u64);
    let n3233: ZW = zw_add(n3229, n3231);
    let n3234: ZW = zw_add(n3230, n3232);
    let n3235: ZW = zw_cellmix_n(406u64, n393, 1542469173u64);
    let n3236: ZW = zw_cellmix_n(406u64, n393, 668265263u64);
    let n3237: ZW = zw_add(n3233, n3235);
    let n3238: ZW = zw_add(n3234, n3236);
    let n3239: ZW = zw_cellmix_n(407u64, n505, 1542469173u64);
    let n3240: ZW = zw_cellmix_n(407u64, n505, 668265263u64);
    let n3241: ZW = zw_add(n3237, n3239);
    let n3242: ZW = zw_add(n3238, n3240);
    let n3243: ZW = zw_add(n3241, n2761);
    let n3244: ZW = zw_add(n3242, n2762);
    let n3245: ZW = zw_add(n3243, n2765);
    let n3246: ZW = zw_add(n3244, n2766);
    let n3247: ZW = zw_cellmix_n(299u64, n754, 1542469173u64);
    let n3248: ZW = zw_cellmix_n(299u64, n754, 668265263u64);
    let n3249: ZW = zw_add(n3245, n3247);
    let n3250: ZW = zw_add(n3246, n3248);
    let n3251: ZW = zw_cellmix_n(301u64, n830, 1542469173u64);
    let n3252: ZW = zw_cellmix_n(301u64, n830, 668265263u64);
    let n3253: ZW = zw_add(n3249, n3251);
    let n3254: ZW = zw_add(n3250, n3252);
    let n3255: ZW = zw_cellmix_n(302u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n3256: ZW = zw_cellmix_n(302u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n3257: ZW = zw_add(n3253, n3255);
    let n3258: ZW = zw_add(n3254, n3256);
    let n3259: ZW = zw_cellmix_n(304u64, n753, 1542469173u64);
    let n3260: ZW = zw_cellmix_n(304u64, n753, 668265263u64);
    let n3261: ZW = zw_add(n3257, n3259);
    let n3262: ZW = zw_add(n3258, n3260);
    let n3263: ZW = zw_cellmix_b(311u64, zb_splat(false), 1542469173u64);
    let n3264: ZW = zw_cellmix_b(311u64, zb_splat(false), 668265263u64);
    let n3265: ZW = zw_add(n3261, n3263);
    let n3266: ZW = zw_add(n3262, n3264);
    let n3267: ZW = zw_cellmix_b(312u64, zb_splat(false), 1542469173u64);
    let n3268: ZW = zw_cellmix_b(312u64, zb_splat(false), 668265263u64);
    let n3269: ZW = zw_add(n3265, n3267);
    let n3270: ZW = zw_add(n3266, n3268);
    let n3271: ZW = zw_cellmix_n(318u64, n842, 1542469173u64);
    let n3272: ZW = zw_cellmix_n(318u64, n842, 668265263u64);
    let n3273: ZW = zw_add(n3269, n3271);
    let n3274: ZW = zw_add(n3270, n3272);
    let n3275: ZW = zw_cellmix_n(396u64, r_c395, 1542469173u64);
    let n3276: ZW = zw_cellmix_n(396u64, r_c395, 668265263u64);
    let n3277: ZW = zw_add(n3273, n3275);
    let n3278: ZW = zw_add(n3274, n3276);
    let n3279: ZW = zw_cellmix_n(397u64, r_c396, 1542469173u64);
    let n3280: ZW = zw_cellmix_n(397u64, r_c396, 668265263u64);
    let n3281: ZW = zw_add(n3277, n3279);
    let n3282: ZW = zw_add(n3278, n3280);
    let n3283: ZW = zw_cellmix_n(398u64, r_c397, 1542469173u64);
    let n3284: ZW = zw_cellmix_n(398u64, r_c397, 668265263u64);
    let n3285: ZW = zw_add(n3281, n3283);
    let n3286: ZW = zw_add(n3282, n3284);
    let n3287: ZW = zw_cellmix_n(399u64, r_c398, 1542469173u64);
    let n3288: ZW = zw_cellmix_n(399u64, r_c398, 668265263u64);
    let n3289: ZW = zw_add(n3285, n3287);
    let n3290: ZW = zw_add(n3286, n3288);
    let n3291: ZW = zw_cellmix_b(400u64, n831, 1542469173u64);
    let n3292: ZW = zw_cellmix_b(400u64, n831, 668265263u64);
    let n3293: ZW = zw_add(n3289, n3291);
    let n3294: ZW = zw_add(n3290, n3292);
    let n3295: ZW = zw_cellmix_n(408u64, n843, 1542469173u64);
    let n3296: ZW = zw_cellmix_n(408u64, n843, 668265263u64);
    let n3297: ZW = zw_add(n3293, n3295);
    let n3298: ZW = zw_add(n3294, n3296);
    let n3299: ZW = zw_cellmix_n(409u64, n833, 1542469173u64);
    let n3300: ZW = zw_cellmix_n(409u64, n833, 668265263u64);
    let n3301: ZW = zw_add(n3297, n3299);
    let n3302: ZW = zw_add(n3298, n3300);
    let n3303: ZW = zw_cellmix_b(400u64, n973, 1542469173u64);
    let n3304: ZW = zw_cellmix_b(400u64, n973, 668265263u64);
    let n3305: ZW = zw_add(n3289, n3303);
    let n3306: ZW = zw_add(n3290, n3304);
    let n3307: ZW = zw_cellmix_n(408u64, n977, 1542469173u64);
    let n3308: ZW = zw_cellmix_n(408u64, n977, 668265263u64);
    let n3309: ZW = zw_add(n3305, n3307);
    let n3310: ZW = zw_add(n3306, n3308);
    let n3311: ZW = zw_cellmix_n(409u64, n975, 1542469173u64);
    let n3312: ZW = zw_cellmix_n(409u64, n975, 668265263u64);
    let n3313: ZW = zw_add(n3309, n3311);
    let n3314: ZW = zw_add(n3310, n3312);
    let n3315: ZW = zw_cellmix_b(400u64, n995, 1542469173u64);
    let n3316: ZW = zw_cellmix_b(400u64, n995, 668265263u64);
    let n3317: ZW = zw_add(n3289, n3315);
    let n3318: ZW = zw_add(n3290, n3316);
    let n3319: ZW = zw_cellmix_n(408u64, n999, 1542469173u64);
    let n3320: ZW = zw_cellmix_n(408u64, n999, 668265263u64);
    let n3321: ZW = zw_add(n3317, n3319);
    let n3322: ZW = zw_add(n3318, n3320);
    let n3323: ZW = zw_cellmix_n(409u64, n997, 1542469173u64);
    let n3324: ZW = zw_cellmix_n(409u64, n997, 668265263u64);
    let n3325: ZW = zw_add(n3321, n3323);
    let n3326: ZW = zw_add(n3322, n3324);
    let n3327: ZW = zw_cellmix_n(304u64, n1003, 1542469173u64);
    let n3328: ZW = zw_cellmix_n(304u64, n1003, 668265263u64);
    let n3329: ZW = zw_add(n3257, n3327);
    let n3330: ZW = zw_add(n3258, n3328);
    let n3331: ZW = zw_add(n3329, n3263);
    let n3332: ZW = zw_add(n3330, n3264);
    let n3333: ZW = zw_cellmix_b(312u64, zb_splat(true), 1542469173u64);
    let n3334: ZW = zw_cellmix_b(312u64, zb_splat(true), 668265263u64);
    let n3335: ZW = zw_add(n3331, n3333);
    let n3336: ZW = zw_add(n3332, n3334);
    let n3337: ZW = zw_add(n3335, n3271);
    let n3338: ZW = zw_add(n3336, n3272);
    let n3339: ZW = zw_add(n3337, n3275);
    let n3340: ZW = zw_add(n3338, n3276);
    let n3341: ZW = zw_add(n3339, n3279);
    let n3342: ZW = zw_add(n3340, n3280);
    let n3343: ZW = zw_add(n3341, n3283);
    let n3344: ZW = zw_add(n3342, n3284);
    let n3345: ZW = zw_add(n3343, n3287);
    let n3346: ZW = zw_add(n3344, n3288);
    let n3347: ZW = zw_add(n3345, n3291);
    let n3348: ZW = zw_add(n3346, n3292);
    let n3349: ZW = zw_cellmix_n(408u64, n1007, 1542469173u64);
    let n3350: ZW = zw_cellmix_n(408u64, n1007, 668265263u64);
    let n3351: ZW = zw_add(n3347, n3349);
    let n3352: ZW = zw_add(n3348, n3350);
    let n3353: ZW = zw_cellmix_n(409u64, n1005, 1542469173u64);
    let n3354: ZW = zw_cellmix_n(409u64, n1005, 668265263u64);
    let n3355: ZW = zw_add(n3351, n3353);
    let n3356: ZW = zw_add(n3352, n3354);
    let n3357: ZW = zw_add(n3345, n3303);
    let n3358: ZW = zw_add(n3346, n3304);
    let n3359: ZW = zw_cellmix_n(408u64, n1013, 1542469173u64);
    let n3360: ZW = zw_cellmix_n(408u64, n1013, 668265263u64);
    let n3361: ZW = zw_add(n3357, n3359);
    let n3362: ZW = zw_add(n3358, n3360);
    let n3363: ZW = zw_cellmix_n(409u64, n1011, 1542469173u64);
    let n3364: ZW = zw_cellmix_n(409u64, n1011, 668265263u64);
    let n3365: ZW = zw_add(n3361, n3363);
    let n3366: ZW = zw_add(n3362, n3364);
    let n3367: ZW = zw_add(n3345, n3315);
    let n3368: ZW = zw_add(n3346, n3316);
    let n3369: ZW = zw_cellmix_n(408u64, n1019, 1542469173u64);
    let n3370: ZW = zw_cellmix_n(408u64, n1019, 668265263u64);
    let n3371: ZW = zw_add(n3367, n3369);
    let n3372: ZW = zw_add(n3368, n3370);
    let n3373: ZW = zw_cellmix_n(409u64, n1017, 1542469173u64);
    let n3374: ZW = zw_cellmix_n(409u64, n1017, 668265263u64);
    let n3375: ZW = zw_add(n3371, n3373);
    let n3376: ZW = zw_add(n3372, n3374);
    let n3377: ZW = zw_add(n3241, n2899);
    let n3378: ZW = zw_add(n3242, n2900);
    let n3379: ZW = zw_add(n3377, n2903);
    let n3380: ZW = zw_add(n3378, n2904);
    let n3381: ZW = zw_cellmix_n(299u64, n1033, 1542469173u64);
    let n3382: ZW = zw_cellmix_n(299u64, n1033, 668265263u64);
    let n3383: ZW = zw_add(n3379, n3381);
    let n3384: ZW = zw_add(n3380, n3382);
    let n3385: ZW = zw_cellmix_n(301u64, n1034, 1542469173u64);
    let n3386: ZW = zw_cellmix_n(301u64, n1034, 668265263u64);
    let n3387: ZW = zw_add(n3383, n3385);
    let n3388: ZW = zw_add(n3384, n3386);
    let n3389: ZW = zw_cellmix_n(302u64, n1035, 1542469173u64);
    let n3390: ZW = zw_cellmix_n(302u64, n1035, 668265263u64);
    let n3391: ZW = zw_add(n3387, n3389);
    let n3392: ZW = zw_add(n3388, n3390);
    let n3393: ZW = zw_add(n3391, n3259);
    let n3394: ZW = zw_add(n3392, n3260);
    let n3395: ZW = zw_cellmix_b(311u64, zb_splat(true), 1542469173u64);
    let n3396: ZW = zw_cellmix_b(311u64, zb_splat(true), 668265263u64);
    let n3397: ZW = zw_add(n3393, n3395);
    let n3398: ZW = zw_add(n3394, n3396);
    let n3399: ZW = zw_add(n3397, n3267);
    let n3400: ZW = zw_add(n3398, n3268);
    let n3401: ZW = zw_cellmix_n(318u64, n1044, 1542469173u64);
    let n3402: ZW = zw_cellmix_n(318u64, n1044, 668265263u64);
    let n3403: ZW = zw_add(n3399, n3401);
    let n3404: ZW = zw_add(n3400, n3402);
    let n3405: ZW = zw_cellmix_n(396u64, n1036, 1542469173u64);
    let n3406: ZW = zw_cellmix_n(396u64, n1036, 668265263u64);
    let n3407: ZW = zw_add(n3403, n3405);
    let n3408: ZW = zw_add(n3404, n3406);
    let n3409: ZW = zw_cellmix_n(397u64, n1037, 1542469173u64);
    let n3410: ZW = zw_cellmix_n(397u64, n1037, 668265263u64);
    let n3411: ZW = zw_add(n3407, n3409);
    let n3412: ZW = zw_add(n3408, n3410);
    let n3413: ZW = zw_cellmix_n(398u64, n1038, 1542469173u64);
    let n3414: ZW = zw_cellmix_n(398u64, n1038, 668265263u64);
    let n3415: ZW = zw_add(n3411, n3413);
    let n3416: ZW = zw_add(n3412, n3414);
    let n3417: ZW = zw_cellmix_n(399u64, n1039, 1542469173u64);
    let n3418: ZW = zw_cellmix_n(399u64, n1039, 668265263u64);
    let n3419: ZW = zw_add(n3415, n3417);
    let n3420: ZW = zw_add(n3416, n3418);
    let n3421: ZW = zw_add(n3419, n3291);
    let n3422: ZW = zw_add(n3420, n3292);
    let n3423: ZW = zw_cellmix_n(408u64, n1045, 1542469173u64);
    let n3424: ZW = zw_cellmix_n(408u64, n1045, 668265263u64);
    let n3425: ZW = zw_add(n3421, n3423);
    let n3426: ZW = zw_add(n3422, n3424);
    let n3427: ZW = zw_cellmix_n(409u64, n1041, 1542469173u64);
    let n3428: ZW = zw_cellmix_n(409u64, n1041, 668265263u64);
    let n3429: ZW = zw_add(n3425, n3427);
    let n3430: ZW = zw_add(n3426, n3428);
    let n3431: ZW = zw_cellmix_n(397u64, n1050, 1542469173u64);
    let n3432: ZW = zw_cellmix_n(397u64, n1050, 668265263u64);
    let n3433: ZW = zw_add(n3407, n3431);
    let n3434: ZW = zw_add(n3408, n3432);
    let n3435: ZW = zw_cellmix_n(398u64, n1051, 1542469173u64);
    let n3436: ZW = zw_cellmix_n(398u64, n1051, 668265263u64);
    let n3437: ZW = zw_add(n3433, n3435);
    let n3438: ZW = zw_add(n3434, n3436);
    let n3439: ZW = zw_add(n3437, n3417);
    let n3440: ZW = zw_add(n3438, n3418);
    let n3441: ZW = zw_add(n3439, n3303);
    let n3442: ZW = zw_add(n3440, n3304);
    let n3443: ZW = zw_cellmix_n(408u64, n1055, 1542469173u64);
    let n3444: ZW = zw_cellmix_n(408u64, n1055, 668265263u64);
    let n3445: ZW = zw_add(n3441, n3443);
    let n3446: ZW = zw_add(n3442, n3444);
    let n3447: ZW = zw_cellmix_n(409u64, n1053, 1542469173u64);
    let n3448: ZW = zw_cellmix_n(409u64, n1053, 668265263u64);
    let n3449: ZW = zw_add(n3445, n3447);
    let n3450: ZW = zw_add(n3446, n3448);
    let n3451: ZW = zw_cellmix_n(398u64, n1059, 1542469173u64);
    let n3452: ZW = zw_cellmix_n(398u64, n1059, 668265263u64);
    let n3453: ZW = zw_add(n3433, n3451);
    let n3454: ZW = zw_add(n3434, n3452);
    let n3455: ZW = zw_add(n3453, n3417);
    let n3456: ZW = zw_add(n3454, n3418);
    let n3457: ZW = zw_add(n3455, n3315);
    let n3458: ZW = zw_add(n3456, n3316);
    let n3459: ZW = zw_cellmix_n(408u64, n1063, 1542469173u64);
    let n3460: ZW = zw_cellmix_n(408u64, n1063, 668265263u64);
    let n3461: ZW = zw_add(n3457, n3459);
    let n3462: ZW = zw_add(n3458, n3460);
    let n3463: ZW = zw_cellmix_n(409u64, n1061, 1542469173u64);
    let n3464: ZW = zw_cellmix_n(409u64, n1061, 668265263u64);
    let n3465: ZW = zw_add(n3461, n3463);
    let n3466: ZW = zw_add(n3462, n3464);
    let n3467: ZW = zw_cellmix_n(396u64, n1071, 1542469173u64);
    let n3468: ZW = zw_cellmix_n(396u64, n1071, 668265263u64);
    let n3469: ZW = zw_add(n3403, n3467);
    let n3470: ZW = zw_add(n3404, n3468);
    let n3471: ZW = zw_cellmix_n(397u64, n1072, 1542469173u64);
    let n3472: ZW = zw_cellmix_n(397u64, n1072, 668265263u64);
    let n3473: ZW = zw_add(n3469, n3471);
    let n3474: ZW = zw_add(n3470, n3472);
    let n3475: ZW = zw_cellmix_n(398u64, n1073, 1542469173u64);
    let n3476: ZW = zw_cellmix_n(398u64, n1073, 668265263u64);
    let n3477: ZW = zw_add(n3473, n3475);
    let n3478: ZW = zw_add(n3474, n3476);
    let n3479: ZW = zw_cellmix_n(399u64, n1074, 1542469173u64);
    let n3480: ZW = zw_cellmix_n(399u64, n1074, 668265263u64);
    let n3481: ZW = zw_add(n3477, n3479);
    let n3482: ZW = zw_add(n3478, n3480);
    let n3483: ZW = zw_add(n3481, n3291);
    let n3484: ZW = zw_add(n3482, n3292);
    let n3485: ZW = zw_cellmix_n(408u64, n1078, 1542469173u64);
    let n3486: ZW = zw_cellmix_n(408u64, n1078, 668265263u64);
    let n3487: ZW = zw_add(n3483, n3485);
    let n3488: ZW = zw_add(n3484, n3486);
    let n3489: ZW = zw_cellmix_n(409u64, n1076, 1542469173u64);
    let n3490: ZW = zw_cellmix_n(409u64, n1076, 668265263u64);
    let n3491: ZW = zw_add(n3487, n3489);
    let n3492: ZW = zw_add(n3488, n3490);
    let n3493: ZW = zw_add(n3469, n3431);
    let n3494: ZW = zw_add(n3470, n3432);
    let n3495: ZW = zw_add(n3493, n3435);
    let n3496: ZW = zw_add(n3494, n3436);
    let n3497: ZW = zw_add(n3495, n3479);
    let n3498: ZW = zw_add(n3496, n3480);
    let n3499: ZW = zw_add(n3497, n3303);
    let n3500: ZW = zw_add(n3498, n3304);
    let n3501: ZW = zw_cellmix_n(408u64, n1084, 1542469173u64);
    let n3502: ZW = zw_cellmix_n(408u64, n1084, 668265263u64);
    let n3503: ZW = zw_add(n3499, n3501);
    let n3504: ZW = zw_add(n3500, n3502);
    let n3505: ZW = zw_cellmix_n(409u64, n1082, 1542469173u64);
    let n3506: ZW = zw_cellmix_n(409u64, n1082, 668265263u64);
    let n3507: ZW = zw_add(n3503, n3505);
    let n3508: ZW = zw_add(n3504, n3506);
    let n3509: ZW = zw_add(n3493, n3451);
    let n3510: ZW = zw_add(n3494, n3452);
    let n3511: ZW = zw_add(n3509, n3479);
    let n3512: ZW = zw_add(n3510, n3480);
    let n3513: ZW = zw_add(n3511, n3315);
    let n3514: ZW = zw_add(n3512, n3316);
    let n3515: ZW = zw_cellmix_n(408u64, n1090, 1542469173u64);
    let n3516: ZW = zw_cellmix_n(408u64, n1090, 668265263u64);
    let n3517: ZW = zw_add(n3513, n3515);
    let n3518: ZW = zw_add(n3514, n3516);
    let n3519: ZW = zw_cellmix_n(409u64, n1088, 1542469173u64);
    let n3520: ZW = zw_cellmix_n(409u64, n1088, 668265263u64);
    let n3521: ZW = zw_add(n3517, n3519);
    let n3522: ZW = zw_add(n3518, n3520);
    let n3523: ZW = zw_cellmix_n(399u64, n1093, 1542469173u64);
    let n3524: ZW = zw_cellmix_n(399u64, n1093, 668265263u64);
    let n3525: ZW = zw_add(n3477, n3523);
    let n3526: ZW = zw_add(n3478, n3524);
    let n3527: ZW = zw_add(n3525, n3291);
    let n3528: ZW = zw_add(n3526, n3292);
    let n3529: ZW = zw_add(n3527, n3485);
    let n3530: ZW = zw_add(n3528, n3486);
    let n3531: ZW = zw_cellmix_n(409u64, n1094, 1542469173u64);
    let n3532: ZW = zw_cellmix_n(409u64, n1094, 668265263u64);
    let n3533: ZW = zw_add(n3529, n3531);
    let n3534: ZW = zw_add(n3530, n3532);
    let n3535: ZW = zw_add(n3495, n3523);
    let n3536: ZW = zw_add(n3496, n3524);
    let n3537: ZW = zw_add(n3535, n3303);
    let n3538: ZW = zw_add(n3536, n3304);
    let n3539: ZW = zw_add(n3537, n3501);
    let n3540: ZW = zw_add(n3538, n3502);
    let n3541: ZW = zw_cellmix_n(409u64, n1096, 1542469173u64);
    let n3542: ZW = zw_cellmix_n(409u64, n1096, 668265263u64);
    let n3543: ZW = zw_add(n3539, n3541);
    let n3544: ZW = zw_add(n3540, n3542);
    let n3545: ZW = zw_add(n3509, n3523);
    let n3546: ZW = zw_add(n3510, n3524);
    let n3547: ZW = zw_add(n3545, n3315);
    let n3548: ZW = zw_add(n3546, n3316);
    let n3549: ZW = zw_add(n3547, n3515);
    let n3550: ZW = zw_add(n3548, n3516);
    let n3551: ZW = zw_cellmix_n(409u64, n1098, 1542469173u64);
    let n3552: ZW = zw_cellmix_n(409u64, n1098, 668265263u64);
    let n3553: ZW = zw_add(n3549, n3551);
    let n3554: ZW = zw_add(n3550, n3552);
    let n3555: ZW = zw_add(n3391, n3327);
    let n3556: ZW = zw_add(n3392, n3328);
    let n3557: ZW = zw_add(n3555, n3395);
    let n3558: ZW = zw_add(n3556, n3396);
    let n3559: ZW = zw_add(n3557, n3333);
    let n3560: ZW = zw_add(n3558, n3334);
    let n3561: ZW = zw_add(n3559, n3401);
    let n3562: ZW = zw_add(n3560, n3402);
    let n3563: ZW = zw_add(n3561, n3405);
    let n3564: ZW = zw_add(n3562, n3406);
    let n3565: ZW = zw_add(n3563, n3409);
    let n3566: ZW = zw_add(n3564, n3410);
    let n3567: ZW = zw_add(n3565, n3413);
    let n3568: ZW = zw_add(n3566, n3414);
    let n3569: ZW = zw_add(n3567, n3417);
    let n3570: ZW = zw_add(n3568, n3418);
    let n3571: ZW = zw_add(n3569, n3291);
    let n3572: ZW = zw_add(n3570, n3292);
    let n3573: ZW = zw_cellmix_n(408u64, n1104, 1542469173u64);
    let n3574: ZW = zw_cellmix_n(408u64, n1104, 668265263u64);
    let n3575: ZW = zw_add(n3571, n3573);
    let n3576: ZW = zw_add(n3572, n3574);
    let n3577: ZW = zw_cellmix_n(409u64, n1102, 1542469173u64);
    let n3578: ZW = zw_cellmix_n(409u64, n1102, 668265263u64);
    let n3579: ZW = zw_add(n3575, n3577);
    let n3580: ZW = zw_add(n3576, n3578);
    let n3581: ZW = zw_add(n3563, n3431);
    let n3582: ZW = zw_add(n3564, n3432);
    let n3583: ZW = zw_add(n3581, n3435);
    let n3584: ZW = zw_add(n3582, n3436);
    let n3585: ZW = zw_add(n3583, n3417);
    let n3586: ZW = zw_add(n3584, n3418);
    let n3587: ZW = zw_add(n3585, n3303);
    let n3588: ZW = zw_add(n3586, n3304);
    let n3589: ZW = zw_cellmix_n(408u64, n1110, 1542469173u64);
    let n3590: ZW = zw_cellmix_n(408u64, n1110, 668265263u64);
    let n3591: ZW = zw_add(n3587, n3589);
    let n3592: ZW = zw_add(n3588, n3590);
    let n3593: ZW = zw_cellmix_n(409u64, n1108, 1542469173u64);
    let n3594: ZW = zw_cellmix_n(409u64, n1108, 668265263u64);
    let n3595: ZW = zw_add(n3591, n3593);
    let n3596: ZW = zw_add(n3592, n3594);
    let n3597: ZW = zw_add(n3581, n3451);
    let n3598: ZW = zw_add(n3582, n3452);
    let n3599: ZW = zw_add(n3597, n3417);
    let n3600: ZW = zw_add(n3598, n3418);
    let n3601: ZW = zw_add(n3599, n3315);
    let n3602: ZW = zw_add(n3600, n3316);
    let n3603: ZW = zw_cellmix_n(408u64, n1116, 1542469173u64);
    let n3604: ZW = zw_cellmix_n(408u64, n1116, 668265263u64);
    let n3605: ZW = zw_add(n3601, n3603);
    let n3606: ZW = zw_add(n3602, n3604);
    let n3607: ZW = zw_cellmix_n(409u64, n1114, 1542469173u64);
    let n3608: ZW = zw_cellmix_n(409u64, n1114, 668265263u64);
    let n3609: ZW = zw_add(n3605, n3607);
    let n3610: ZW = zw_add(n3606, n3608);
    let n3611: ZW = zw_add(n3561, n3467);
    let n3612: ZW = zw_add(n3562, n3468);
    let n3613: ZW = zw_add(n3611, n3471);
    let n3614: ZW = zw_add(n3612, n3472);
    let n3615: ZW = zw_add(n3613, n3475);
    let n3616: ZW = zw_add(n3614, n3476);
    let n3617: ZW = zw_add(n3615, n3479);
    let n3618: ZW = zw_add(n3616, n3480);
    let n3619: ZW = zw_add(n3617, n3291);
    let n3620: ZW = zw_add(n3618, n3292);
    let n3621: ZW = zw_cellmix_n(408u64, n1122, 1542469173u64);
    let n3622: ZW = zw_cellmix_n(408u64, n1122, 668265263u64);
    let n3623: ZW = zw_add(n3619, n3621);
    let n3624: ZW = zw_add(n3620, n3622);
    let n3625: ZW = zw_cellmix_n(409u64, n1120, 1542469173u64);
    let n3626: ZW = zw_cellmix_n(409u64, n1120, 668265263u64);
    let n3627: ZW = zw_add(n3623, n3625);
    let n3628: ZW = zw_add(n3624, n3626);
    let n3629: ZW = zw_add(n3611, n3431);
    let n3630: ZW = zw_add(n3612, n3432);
    let n3631: ZW = zw_add(n3629, n3435);
    let n3632: ZW = zw_add(n3630, n3436);
    let n3633: ZW = zw_add(n3631, n3479);
    let n3634: ZW = zw_add(n3632, n3480);
    let n3635: ZW = zw_add(n3633, n3303);
    let n3636: ZW = zw_add(n3634, n3304);
    let n3637: ZW = zw_cellmix_n(408u64, n1128, 1542469173u64);
    let n3638: ZW = zw_cellmix_n(408u64, n1128, 668265263u64);
    let n3639: ZW = zw_add(n3635, n3637);
    let n3640: ZW = zw_add(n3636, n3638);
    let n3641: ZW = zw_cellmix_n(409u64, n1126, 1542469173u64);
    let n3642: ZW = zw_cellmix_n(409u64, n1126, 668265263u64);
    let n3643: ZW = zw_add(n3639, n3641);
    let n3644: ZW = zw_add(n3640, n3642);
    let n3645: ZW = zw_add(n3629, n3451);
    let n3646: ZW = zw_add(n3630, n3452);
    let n3647: ZW = zw_add(n3645, n3479);
    let n3648: ZW = zw_add(n3646, n3480);
    let n3649: ZW = zw_add(n3647, n3315);
    let n3650: ZW = zw_add(n3648, n3316);
    let n3651: ZW = zw_cellmix_n(408u64, n1134, 1542469173u64);
    let n3652: ZW = zw_cellmix_n(408u64, n1134, 668265263u64);
    let n3653: ZW = zw_add(n3649, n3651);
    let n3654: ZW = zw_add(n3650, n3652);
    let n3655: ZW = zw_cellmix_n(409u64, n1132, 1542469173u64);
    let n3656: ZW = zw_cellmix_n(409u64, n1132, 668265263u64);
    let n3657: ZW = zw_add(n3653, n3655);
    let n3658: ZW = zw_add(n3654, n3656);
    let n3659: ZW = zw_add(n3615, n3523);
    let n3660: ZW = zw_add(n3616, n3524);
    let n3661: ZW = zw_add(n3659, n3291);
    let n3662: ZW = zw_add(n3660, n3292);
    let n3663: ZW = zw_add(n3661, n3621);
    let n3664: ZW = zw_add(n3662, n3622);
    let n3665: ZW = zw_cellmix_n(409u64, n1136, 1542469173u64);
    let n3666: ZW = zw_cellmix_n(409u64, n1136, 668265263u64);
    let n3667: ZW = zw_add(n3663, n3665);
    let n3668: ZW = zw_add(n3664, n3666);
    let n3669: ZW = zw_add(n3631, n3523);
    let n3670: ZW = zw_add(n3632, n3524);
    let n3671: ZW = zw_add(n3669, n3303);
    let n3672: ZW = zw_add(n3670, n3304);
    let n3673: ZW = zw_add(n3671, n3637);
    let n3674: ZW = zw_add(n3672, n3638);
    let n3675: ZW = zw_cellmix_n(409u64, n1138, 1542469173u64);
    let n3676: ZW = zw_cellmix_n(409u64, n1138, 668265263u64);
    let n3677: ZW = zw_add(n3673, n3675);
    let n3678: ZW = zw_add(n3674, n3676);
    let n3679: ZW = zw_add(n3645, n3523);
    let n3680: ZW = zw_add(n3646, n3524);
    let n3681: ZW = zw_add(n3679, n3315);
    let n3682: ZW = zw_add(n3680, n3316);
    let n3683: ZW = zw_add(n3681, n3651);
    let n3684: ZW = zw_add(n3682, n3652);
    let n3685: ZW = zw_cellmix_n(409u64, n1140, 1542469173u64);
    let n3686: ZW = zw_cellmix_n(409u64, n1140, 668265263u64);
    let n3687: ZW = zw_add(n3683, n3685);
    let n3688: ZW = zw_add(n3684, n3686);
    let n3689: ZW = zw_add(zw_splat(0u64), n2725);
    let n3690: ZW = zw_add(zw_splat(0u64), n2726);
    let n3691: ZW = zw_add(n3689, n2729);
    let n3692: ZW = zw_add(n3690, n2730);
    let n3693: ZW = zw_add(n3691, n2733);
    let n3694: ZW = zw_add(n3692, n2734);
    let n3695: ZW = zw_cellmix_n(87u64, n1239, 1542469173u64);
    let n3696: ZW = zw_cellmix_n(87u64, n1239, 668265263u64);
    let n3697: ZW = zw_add(n3693, n3695);
    let n3698: ZW = zw_add(n3694, n3696);
    let n3699: ZW = zw_cellmix_n(260u64, n846, 1542469173u64);
    let n3700: ZW = zw_cellmix_n(260u64, n846, 668265263u64);
    let n3701: ZW = zw_add(n3697, n3699);
    let n3702: ZW = zw_add(n3698, n3700);
    let n3703: ZW = zw_cellmix_n(273u64, n847, 1542469173u64);
    let n3704: ZW = zw_cellmix_n(273u64, n847, 668265263u64);
    let n3705: ZW = zw_add(n3701, n3703);
    let n3706: ZW = zw_add(n3702, n3704);
    let n3707: ZW = zw_add(n3705, n2761);
    let n3708: ZW = zw_add(n3706, n2762);
    let n3709: ZW = zw_add(n3707, n2765);
    let n3710: ZW = zw_add(n3708, n2766);
    let n3711: ZW = zw_add(n3705, n2899);
    let n3712: ZW = zw_add(n3706, n2900);
    let n3713: ZW = zw_add(n3711, n2903);
    let n3714: ZW = zw_add(n3712, n2904);
    let n3715: ZW = zw_cellmix_n(245u64, n1142, 1542469173u64);
    let n3716: ZW = zw_cellmix_n(245u64, n1142, 668265263u64);
    let n3717: ZW = zw_add(n3697, n3715);
    let n3718: ZW = zw_add(n3698, n3716);
    let n3719: ZW = zw_cellmix_n(253u64, n1147, 1542469173u64);
    let n3720: ZW = zw_cellmix_n(253u64, n1147, 668265263u64);
    let n3721: ZW = zw_add(n3717, n3719);
    let n3722: ZW = zw_add(n3718, n3720);
    let n3723: ZW = zw_cellmix_n(277u64, n846, 1542469173u64);
    let n3724: ZW = zw_cellmix_n(277u64, n846, 668265263u64);
    let n3725: ZW = zw_add(n3721, n3723);
    let n3726: ZW = zw_add(n3722, n3724);
    let n3727: ZW = zw_cellmix_n(290u64, n847, 1542469173u64);
    let n3728: ZW = zw_cellmix_n(290u64, n847, 668265263u64);
    let n3729: ZW = zw_add(n3725, n3727);
    let n3730: ZW = zw_add(n3726, n3728);
    let n3731: ZW = zw_add(n3729, n2761);
    let n3732: ZW = zw_add(n3730, n2762);
    let n3733: ZW = zw_add(n3731, n2765);
    let n3734: ZW = zw_add(n3732, n2766);
    let n3735: ZW = zw_add(n3729, n2899);
    let n3736: ZW = zw_add(n3730, n2900);
    let n3737: ZW = zw_add(n3735, n2903);
    let n3738: ZW = zw_add(n3736, n2904);
    let n3739: ZW = zw_cellmix_n(260u64, n1391, 1542469173u64);
    let n3740: ZW = zw_cellmix_n(260u64, n1391, 668265263u64);
    let n3741: ZW = zw_add(n2739, n3739);
    let n3742: ZW = zw_add(n2740, n3740);
    let n3743: ZW = zw_cellmix_n(273u64, n1392, 1542469173u64);
    let n3744: ZW = zw_cellmix_n(273u64, n1392, 668265263u64);
    let n3745: ZW = zw_add(n3741, n3743);
    let n3746: ZW = zw_add(n3742, n3744);
    let n3747: ZW = zw_cellmix_n(301u64, n1626, 1542469173u64);
    let n3748: ZW = zw_cellmix_n(301u64, n1626, 668265263u64);
    let n3749: ZW = zw_add(n3745, n3747);
    let n3750: ZW = zw_add(n3746, n3748);
    let n3751: ZW = zw_cellmix_n(367u64, n1627, 1542469173u64);
    let n3752: ZW = zw_cellmix_n(367u64, n1627, 668265263u64);
    let n3753: ZW = zw_add(n3749, n3751);
    let n3754: ZW = zw_add(n3750, n3752);
    let n3755: ZW = zw_cellmix_n(368u64, n1628, 1542469173u64);
    let n3756: ZW = zw_cellmix_n(368u64, n1628, 668265263u64);
    let n3757: ZW = zw_add(n3753, n3755);
    let n3758: ZW = zw_add(n3754, n3756);
    let n3759: ZW = zw_add(n3757, n2761);
    let n3760: ZW = zw_add(n3758, n2762);
    let n3761: ZW = zw_add(n3759, n2765);
    let n3762: ZW = zw_add(n3760, n2766);
    let n3763: ZW = zw_cellmix_n(281u64, n754, 1542469173u64);
    let n3764: ZW = zw_cellmix_n(281u64, n754, 668265263u64);
    let n3765: ZW = zw_add(n3761, n3763);
    let n3766: ZW = zw_add(n3762, n3764);
    let n3767: ZW = zw_cellmix_n(283u64, n830, 1542469173u64);
    let n3768: ZW = zw_cellmix_n(283u64, n830, 668265263u64);
    let n3769: ZW = zw_add(n3765, n3767);
    let n3770: ZW = zw_add(n3766, n3768);
    let n3771: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n3772: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n3773: ZW = zw_add(n3769, n3771);
    let n3774: ZW = zw_add(n3770, n3772);
    let n3775: ZW = zw_cellmix_n(286u64, n1965, 1542469173u64);
    let n3776: ZW = zw_cellmix_n(286u64, n1965, 668265263u64);
    let n3777: ZW = zw_add(n3773, n3775);
    let n3778: ZW = zw_add(n3774, n3776);
    let n3779: ZW = zw_cellmix_b(293u64, zb_splat(false), 1542469173u64);
    let n3780: ZW = zw_cellmix_b(293u64, zb_splat(false), 668265263u64);
    let n3781: ZW = zw_add(n3777, n3779);
    let n3782: ZW = zw_add(n3778, n3780);
    let n3783: ZW = zw_add(n3781, n2785);
    let n3784: ZW = zw_add(n3782, n2786);
    let n3785: ZW = zw_cellmix_n(300u64, n2059, 1542469173u64);
    let n3786: ZW = zw_cellmix_n(300u64, n2059, 668265263u64);
    let n3787: ZW = zw_add(n3783, n3785);
    let n3788: ZW = zw_add(n3784, n3786);
    let n3789: ZW = zw_cellmix_n(357u64, r_c395, 1542469173u64);
    let n3790: ZW = zw_cellmix_n(357u64, r_c395, 668265263u64);
    let n3791: ZW = zw_add(n3787, n3789);
    let n3792: ZW = zw_add(n3788, n3790);
    let n3793: ZW = zw_cellmix_n(358u64, r_c396, 1542469173u64);
    let n3794: ZW = zw_cellmix_n(358u64, r_c396, 668265263u64);
    let n3795: ZW = zw_add(n3791, n3793);
    let n3796: ZW = zw_add(n3792, n3794);
    let n3797: ZW = zw_cellmix_n(359u64, r_c397, 1542469173u64);
    let n3798: ZW = zw_cellmix_n(359u64, r_c397, 668265263u64);
    let n3799: ZW = zw_add(n3795, n3797);
    let n3800: ZW = zw_add(n3796, n3798);
    let n3801: ZW = zw_cellmix_n(360u64, r_c398, 1542469173u64);
    let n3802: ZW = zw_cellmix_n(360u64, r_c398, 668265263u64);
    let n3803: ZW = zw_add(n3799, n3801);
    let n3804: ZW = zw_add(n3800, n3802);
    let n3805: ZW = zw_cellmix_b(361u64, n2043, 1542469173u64);
    let n3806: ZW = zw_cellmix_b(361u64, n2043, 668265263u64);
    let n3807: ZW = zw_add(n3803, n3805);
    let n3808: ZW = zw_add(n3804, n3806);
    let n3809: ZW = zw_cellmix_n(369u64, n2060, 1542469173u64);
    let n3810: ZW = zw_cellmix_n(369u64, n2060, 668265263u64);
    let n3811: ZW = zw_add(n3807, n3809);
    let n3812: ZW = zw_add(n3808, n3810);
    let n3813: ZW = zw_cellmix_n(370u64, n2045, 1542469173u64);
    let n3814: ZW = zw_cellmix_n(370u64, n2045, 668265263u64);
    let n3815: ZW = zw_add(n3811, n3813);
    let n3816: ZW = zw_add(n3812, n3814);
    let n3817: ZW = zw_cellmix_b(361u64, n2082, 1542469173u64);
    let n3818: ZW = zw_cellmix_b(361u64, n2082, 668265263u64);
    let n3819: ZW = zw_add(n3803, n3817);
    let n3820: ZW = zw_add(n3804, n3818);
    let n3821: ZW = zw_cellmix_n(369u64, n2086, 1542469173u64);
    let n3822: ZW = zw_cellmix_n(369u64, n2086, 668265263u64);
    let n3823: ZW = zw_add(n3819, n3821);
    let n3824: ZW = zw_add(n3820, n3822);
    let n3825: ZW = zw_cellmix_n(370u64, n2084, 1542469173u64);
    let n3826: ZW = zw_cellmix_n(370u64, n2084, 668265263u64);
    let n3827: ZW = zw_add(n3823, n3825);
    let n3828: ZW = zw_add(n3824, n3826);
    let n3829: ZW = zw_cellmix_b(361u64, n2107, 1542469173u64);
    let n3830: ZW = zw_cellmix_b(361u64, n2107, 668265263u64);
    let n3831: ZW = zw_add(n3803, n3829);
    let n3832: ZW = zw_add(n3804, n3830);
    let n3833: ZW = zw_cellmix_n(369u64, n2111, 1542469173u64);
    let n3834: ZW = zw_cellmix_n(369u64, n2111, 668265263u64);
    let n3835: ZW = zw_add(n3831, n3833);
    let n3836: ZW = zw_add(n3832, n3834);
    let n3837: ZW = zw_cellmix_n(370u64, n2109, 1542469173u64);
    let n3838: ZW = zw_cellmix_n(370u64, n2109, 668265263u64);
    let n3839: ZW = zw_add(n3835, n3837);
    let n3840: ZW = zw_add(n3836, n3838);
    let n3841: ZW = zw_cellmix_n(286u64, n2115, 1542469173u64);
    let n3842: ZW = zw_cellmix_n(286u64, n2115, 668265263u64);
    let n3843: ZW = zw_add(n3773, n3841);
    let n3844: ZW = zw_add(n3774, n3842);
    let n3845: ZW = zw_add(n3843, n3779);
    let n3846: ZW = zw_add(n3844, n3780);
    let n3847: ZW = zw_add(n3845, n2921);
    let n3848: ZW = zw_add(n3846, n2922);
    let n3849: ZW = zw_add(n3847, n3785);
    let n3850: ZW = zw_add(n3848, n3786);
    let n3851: ZW = zw_add(n3849, n3789);
    let n3852: ZW = zw_add(n3850, n3790);
    let n3853: ZW = zw_add(n3851, n3793);
    let n3854: ZW = zw_add(n3852, n3794);
    let n3855: ZW = zw_add(n3853, n3797);
    let n3856: ZW = zw_add(n3854, n3798);
    let n3857: ZW = zw_add(n3855, n3801);
    let n3858: ZW = zw_add(n3856, n3802);
    let n3859: ZW = zw_add(n3857, n3805);
    let n3860: ZW = zw_add(n3858, n3806);
    let n3861: ZW = zw_cellmix_n(369u64, n2121, 1542469173u64);
    let n3862: ZW = zw_cellmix_n(369u64, n2121, 668265263u64);
    let n3863: ZW = zw_add(n3859, n3861);
    let n3864: ZW = zw_add(n3860, n3862);
    let n3865: ZW = zw_cellmix_n(370u64, n2117, 1542469173u64);
    let n3866: ZW = zw_cellmix_n(370u64, n2117, 668265263u64);
    let n3867: ZW = zw_add(n3863, n3865);
    let n3868: ZW = zw_add(n3864, n3866);
    let n3869: ZW = zw_add(n3857, n3817);
    let n3870: ZW = zw_add(n3858, n3818);
    let n3871: ZW = zw_cellmix_n(369u64, n2127, 1542469173u64);
    let n3872: ZW = zw_cellmix_n(369u64, n2127, 668265263u64);
    let n3873: ZW = zw_add(n3869, n3871);
    let n3874: ZW = zw_add(n3870, n3872);
    let n3875: ZW = zw_cellmix_n(370u64, n2125, 1542469173u64);
    let n3876: ZW = zw_cellmix_n(370u64, n2125, 668265263u64);
    let n3877: ZW = zw_add(n3873, n3875);
    let n3878: ZW = zw_add(n3874, n3876);
    let n3879: ZW = zw_add(n3857, n3829);
    let n3880: ZW = zw_add(n3858, n3830);
    let n3881: ZW = zw_cellmix_n(369u64, n2133, 1542469173u64);
    let n3882: ZW = zw_cellmix_n(369u64, n2133, 668265263u64);
    let n3883: ZW = zw_add(n3879, n3881);
    let n3884: ZW = zw_add(n3880, n3882);
    let n3885: ZW = zw_cellmix_n(370u64, n2131, 1542469173u64);
    let n3886: ZW = zw_cellmix_n(370u64, n2131, 668265263u64);
    let n3887: ZW = zw_add(n3883, n3885);
    let n3888: ZW = zw_add(n3884, n3886);
    let n3889: ZW = zw_add(n3757, n2899);
    let n3890: ZW = zw_add(n3758, n2900);
    let n3891: ZW = zw_add(n3889, n2903);
    let n3892: ZW = zw_add(n3890, n2904);
    let n3893: ZW = zw_cellmix_n(281u64, n1033, 1542469173u64);
    let n3894: ZW = zw_cellmix_n(281u64, n1033, 668265263u64);
    let n3895: ZW = zw_add(n3891, n3893);
    let n3896: ZW = zw_add(n3892, n3894);
    let n3897: ZW = zw_cellmix_n(283u64, n1034, 1542469173u64);
    let n3898: ZW = zw_cellmix_n(283u64, n1034, 668265263u64);
    let n3899: ZW = zw_add(n3895, n3897);
    let n3900: ZW = zw_add(n3896, n3898);
    let n3901: ZW = zw_cellmix_n(284u64, n1035, 1542469173u64);
    let n3902: ZW = zw_cellmix_n(284u64, n1035, 668265263u64);
    let n3903: ZW = zw_add(n3899, n3901);
    let n3904: ZW = zw_add(n3900, n3902);
    let n3905: ZW = zw_add(n3903, n3775);
    let n3906: ZW = zw_add(n3904, n3776);
    let n3907: ZW = zw_cellmix_b(293u64, zb_splat(true), 1542469173u64);
    let n3908: ZW = zw_cellmix_b(293u64, zb_splat(true), 668265263u64);
    let n3909: ZW = zw_add(n3905, n3907);
    let n3910: ZW = zw_add(n3906, n3908);
    let n3911: ZW = zw_add(n3909, n2785);
    let n3912: ZW = zw_add(n3910, n2786);
    let n3913: ZW = zw_cellmix_n(300u64, n2143, 1542469173u64);
    let n3914: ZW = zw_cellmix_n(300u64, n2143, 668265263u64);
    let n3915: ZW = zw_add(n3911, n3913);
    let n3916: ZW = zw_add(n3912, n3914);
    let n3917: ZW = zw_cellmix_n(357u64, n1036, 1542469173u64);
    let n3918: ZW = zw_cellmix_n(357u64, n1036, 668265263u64);
    let n3919: ZW = zw_add(n3915, n3917);
    let n3920: ZW = zw_add(n3916, n3918);
    let n3921: ZW = zw_cellmix_n(358u64, n2138, 1542469173u64);
    let n3922: ZW = zw_cellmix_n(358u64, n2138, 668265263u64);
    let n3923: ZW = zw_add(n3919, n3921);
    let n3924: ZW = zw_add(n3920, n3922);
    let n3925: ZW = zw_cellmix_n(359u64, n2139, 1542469173u64);
    let n3926: ZW = zw_cellmix_n(359u64, n2139, 668265263u64);
    let n3927: ZW = zw_add(n3923, n3925);
    let n3928: ZW = zw_add(n3924, n3926);
    let n3929: ZW = zw_cellmix_n(360u64, n1039, 1542469173u64);
    let n3930: ZW = zw_cellmix_n(360u64, n1039, 668265263u64);
    let n3931: ZW = zw_add(n3927, n3929);
    let n3932: ZW = zw_add(n3928, n3930);
    let n3933: ZW = zw_add(n3931, n3805);
    let n3934: ZW = zw_add(n3932, n3806);
    let n3935: ZW = zw_cellmix_n(369u64, n2144, 1542469173u64);
    let n3936: ZW = zw_cellmix_n(369u64, n2144, 668265263u64);
    let n3937: ZW = zw_add(n3933, n3935);
    let n3938: ZW = zw_add(n3934, n3936);
    let n3939: ZW = zw_cellmix_n(370u64, n2141, 1542469173u64);
    let n3940: ZW = zw_cellmix_n(370u64, n2141, 668265263u64);
    let n3941: ZW = zw_add(n3937, n3939);
    let n3942: ZW = zw_add(n3938, n3940);
    let n3943: ZW = zw_cellmix_n(358u64, n1050, 1542469173u64);
    let n3944: ZW = zw_cellmix_n(358u64, n1050, 668265263u64);
    let n3945: ZW = zw_add(n3919, n3943);
    let n3946: ZW = zw_add(n3920, n3944);
    let n3947: ZW = zw_cellmix_n(359u64, n1051, 1542469173u64);
    let n3948: ZW = zw_cellmix_n(359u64, n1051, 668265263u64);
    let n3949: ZW = zw_add(n3945, n3947);
    let n3950: ZW = zw_add(n3946, n3948);
    let n3951: ZW = zw_add(n3949, n3929);
    let n3952: ZW = zw_add(n3950, n3930);
    let n3953: ZW = zw_add(n3951, n3817);
    let n3954: ZW = zw_add(n3952, n3818);
    let n3955: ZW = zw_cellmix_n(369u64, n2150, 1542469173u64);
    let n3956: ZW = zw_cellmix_n(369u64, n2150, 668265263u64);
    let n3957: ZW = zw_add(n3953, n3955);
    let n3958: ZW = zw_add(n3954, n3956);
    let n3959: ZW = zw_cellmix_n(370u64, n2148, 1542469173u64);
    let n3960: ZW = zw_cellmix_n(370u64, n2148, 668265263u64);
    let n3961: ZW = zw_add(n3957, n3959);
    let n3962: ZW = zw_add(n3958, n3960);
    let n3963: ZW = zw_cellmix_n(359u64, n1059, 1542469173u64);
    let n3964: ZW = zw_cellmix_n(359u64, n1059, 668265263u64);
    let n3965: ZW = zw_add(n3945, n3963);
    let n3966: ZW = zw_add(n3946, n3964);
    let n3967: ZW = zw_add(n3965, n3929);
    let n3968: ZW = zw_add(n3966, n3930);
    let n3969: ZW = zw_add(n3967, n3829);
    let n3970: ZW = zw_add(n3968, n3830);
    let n3971: ZW = zw_cellmix_n(369u64, n2156, 1542469173u64);
    let n3972: ZW = zw_cellmix_n(369u64, n2156, 668265263u64);
    let n3973: ZW = zw_add(n3969, n3971);
    let n3974: ZW = zw_add(n3970, n3972);
    let n3975: ZW = zw_cellmix_n(370u64, n2154, 1542469173u64);
    let n3976: ZW = zw_cellmix_n(370u64, n2154, 668265263u64);
    let n3977: ZW = zw_add(n3973, n3975);
    let n3978: ZW = zw_add(n3974, n3976);
    let n3979: ZW = zw_cellmix_n(357u64, n1071, 1542469173u64);
    let n3980: ZW = zw_cellmix_n(357u64, n1071, 668265263u64);
    let n3981: ZW = zw_add(n3915, n3979);
    let n3982: ZW = zw_add(n3916, n3980);
    let n3983: ZW = zw_cellmix_n(358u64, n1072, 1542469173u64);
    let n3984: ZW = zw_cellmix_n(358u64, n1072, 668265263u64);
    let n3985: ZW = zw_add(n3981, n3983);
    let n3986: ZW = zw_add(n3982, n3984);
    let n3987: ZW = zw_cellmix_n(359u64, n1073, 1542469173u64);
    let n3988: ZW = zw_cellmix_n(359u64, n1073, 668265263u64);
    let n3989: ZW = zw_add(n3985, n3987);
    let n3990: ZW = zw_add(n3986, n3988);
    let n3991: ZW = zw_cellmix_n(360u64, n1074, 1542469173u64);
    let n3992: ZW = zw_cellmix_n(360u64, n1074, 668265263u64);
    let n3993: ZW = zw_add(n3989, n3991);
    let n3994: ZW = zw_add(n3990, n3992);
    let n3995: ZW = zw_add(n3993, n3805);
    let n3996: ZW = zw_add(n3994, n3806);
    let n3997: ZW = zw_cellmix_n(369u64, n2162, 1542469173u64);
    let n3998: ZW = zw_cellmix_n(369u64, n2162, 668265263u64);
    let n3999: ZW = zw_add(n3995, n3997);
    let n4000: ZW = zw_add(n3996, n3998);
    let n4001: ZW = zw_cellmix_n(370u64, n2160, 1542469173u64);
    let n4002: ZW = zw_cellmix_n(370u64, n2160, 668265263u64);
    let n4003: ZW = zw_add(n3999, n4001);
    let n4004: ZW = zw_add(n4000, n4002);
    let n4005: ZW = zw_add(n3981, n3943);
    let n4006: ZW = zw_add(n3982, n3944);
    let n4007: ZW = zw_add(n4005, n3947);
    let n4008: ZW = zw_add(n4006, n3948);
    let n4009: ZW = zw_add(n4007, n3991);
    let n4010: ZW = zw_add(n4008, n3992);
    let n4011: ZW = zw_add(n4009, n3817);
    let n4012: ZW = zw_add(n4010, n3818);
    let n4013: ZW = zw_cellmix_n(369u64, n2168, 1542469173u64);
    let n4014: ZW = zw_cellmix_n(369u64, n2168, 668265263u64);
    let n4015: ZW = zw_add(n4011, n4013);
    let n4016: ZW = zw_add(n4012, n4014);
    let n4017: ZW = zw_cellmix_n(370u64, n2166, 1542469173u64);
    let n4018: ZW = zw_cellmix_n(370u64, n2166, 668265263u64);
    let n4019: ZW = zw_add(n4015, n4017);
    let n4020: ZW = zw_add(n4016, n4018);
    let n4021: ZW = zw_add(n4005, n3963);
    let n4022: ZW = zw_add(n4006, n3964);
    let n4023: ZW = zw_add(n4021, n3991);
    let n4024: ZW = zw_add(n4022, n3992);
    let n4025: ZW = zw_add(n4023, n3829);
    let n4026: ZW = zw_add(n4024, n3830);
    let n4027: ZW = zw_cellmix_n(369u64, n2174, 1542469173u64);
    let n4028: ZW = zw_cellmix_n(369u64, n2174, 668265263u64);
    let n4029: ZW = zw_add(n4025, n4027);
    let n4030: ZW = zw_add(n4026, n4028);
    let n4031: ZW = zw_cellmix_n(370u64, n2172, 1542469173u64);
    let n4032: ZW = zw_cellmix_n(370u64, n2172, 668265263u64);
    let n4033: ZW = zw_add(n4029, n4031);
    let n4034: ZW = zw_add(n4030, n4032);
    let n4035: ZW = zw_cellmix_n(360u64, n1093, 1542469173u64);
    let n4036: ZW = zw_cellmix_n(360u64, n1093, 668265263u64);
    let n4037: ZW = zw_add(n3989, n4035);
    let n4038: ZW = zw_add(n3990, n4036);
    let n4039: ZW = zw_add(n4037, n3805);
    let n4040: ZW = zw_add(n4038, n3806);
    let n4041: ZW = zw_add(n4039, n3997);
    let n4042: ZW = zw_add(n4040, n3998);
    let n4043: ZW = zw_cellmix_n(370u64, n2176, 1542469173u64);
    let n4044: ZW = zw_cellmix_n(370u64, n2176, 668265263u64);
    let n4045: ZW = zw_add(n4041, n4043);
    let n4046: ZW = zw_add(n4042, n4044);
    let n4047: ZW = zw_add(n4007, n4035);
    let n4048: ZW = zw_add(n4008, n4036);
    let n4049: ZW = zw_add(n4047, n3817);
    let n4050: ZW = zw_add(n4048, n3818);
    let n4051: ZW = zw_add(n4049, n4013);
    let n4052: ZW = zw_add(n4050, n4014);
    let n4053: ZW = zw_cellmix_n(370u64, n2178, 1542469173u64);
    let n4054: ZW = zw_cellmix_n(370u64, n2178, 668265263u64);
    let n4055: ZW = zw_add(n4051, n4053);
    let n4056: ZW = zw_add(n4052, n4054);
    let n4057: ZW = zw_add(n4021, n4035);
    let n4058: ZW = zw_add(n4022, n4036);
    let n4059: ZW = zw_add(n4057, n3829);
    let n4060: ZW = zw_add(n4058, n3830);
    let n4061: ZW = zw_add(n4059, n4027);
    let n4062: ZW = zw_add(n4060, n4028);
    let n4063: ZW = zw_cellmix_n(370u64, n2180, 1542469173u64);
    let n4064: ZW = zw_cellmix_n(370u64, n2180, 668265263u64);
    let n4065: ZW = zw_add(n4061, n4063);
    let n4066: ZW = zw_add(n4062, n4064);
    let n4067: ZW = zw_add(n3903, n3841);
    let n4068: ZW = zw_add(n3904, n3842);
    let n4069: ZW = zw_add(n4067, n3907);
    let n4070: ZW = zw_add(n4068, n3908);
    let n4071: ZW = zw_add(n4069, n2921);
    let n4072: ZW = zw_add(n4070, n2922);
    let n4073: ZW = zw_add(n4071, n3913);
    let n4074: ZW = zw_add(n4072, n3914);
    let n4075: ZW = zw_add(n4073, n3917);
    let n4076: ZW = zw_add(n4074, n3918);
    let n4077: ZW = zw_add(n4075, n3921);
    let n4078: ZW = zw_add(n4076, n3922);
    let n4079: ZW = zw_add(n4077, n3925);
    let n4080: ZW = zw_add(n4078, n3926);
    let n4081: ZW = zw_add(n4079, n3929);
    let n4082: ZW = zw_add(n4080, n3930);
    let n4083: ZW = zw_add(n4081, n3805);
    let n4084: ZW = zw_add(n4082, n3806);
    let n4085: ZW = zw_cellmix_n(369u64, n2186, 1542469173u64);
    let n4086: ZW = zw_cellmix_n(369u64, n2186, 668265263u64);
    let n4087: ZW = zw_add(n4083, n4085);
    let n4088: ZW = zw_add(n4084, n4086);
    let n4089: ZW = zw_cellmix_n(370u64, n2184, 1542469173u64);
    let n4090: ZW = zw_cellmix_n(370u64, n2184, 668265263u64);
    let n4091: ZW = zw_add(n4087, n4089);
    let n4092: ZW = zw_add(n4088, n4090);
    let n4093: ZW = zw_add(n4075, n3943);
    let n4094: ZW = zw_add(n4076, n3944);
    let n4095: ZW = zw_add(n4093, n3947);
    let n4096: ZW = zw_add(n4094, n3948);
    let n4097: ZW = zw_add(n4095, n3929);
    let n4098: ZW = zw_add(n4096, n3930);
    let n4099: ZW = zw_add(n4097, n3817);
    let n4100: ZW = zw_add(n4098, n3818);
    let n4101: ZW = zw_cellmix_n(369u64, n2192, 1542469173u64);
    let n4102: ZW = zw_cellmix_n(369u64, n2192, 668265263u64);
    let n4103: ZW = zw_add(n4099, n4101);
    let n4104: ZW = zw_add(n4100, n4102);
    let n4105: ZW = zw_cellmix_n(370u64, n2190, 1542469173u64);
    let n4106: ZW = zw_cellmix_n(370u64, n2190, 668265263u64);
    let n4107: ZW = zw_add(n4103, n4105);
    let n4108: ZW = zw_add(n4104, n4106);
    let n4109: ZW = zw_add(n4093, n3963);
    let n4110: ZW = zw_add(n4094, n3964);
    let n4111: ZW = zw_add(n4109, n3929);
    let n4112: ZW = zw_add(n4110, n3930);
    let n4113: ZW = zw_add(n4111, n3829);
    let n4114: ZW = zw_add(n4112, n3830);
    let n4115: ZW = zw_cellmix_n(369u64, n2198, 1542469173u64);
    let n4116: ZW = zw_cellmix_n(369u64, n2198, 668265263u64);
    let n4117: ZW = zw_add(n4113, n4115);
    let n4118: ZW = zw_add(n4114, n4116);
    let n4119: ZW = zw_cellmix_n(370u64, n2196, 1542469173u64);
    let n4120: ZW = zw_cellmix_n(370u64, n2196, 668265263u64);
    let n4121: ZW = zw_add(n4117, n4119);
    let n4122: ZW = zw_add(n4118, n4120);
    let n4123: ZW = zw_add(n4073, n3979);
    let n4124: ZW = zw_add(n4074, n3980);
    let n4125: ZW = zw_add(n4123, n3983);
    let n4126: ZW = zw_add(n4124, n3984);
    let n4127: ZW = zw_add(n4125, n3987);
    let n4128: ZW = zw_add(n4126, n3988);
    let n4129: ZW = zw_add(n4127, n3991);
    let n4130: ZW = zw_add(n4128, n3992);
    let n4131: ZW = zw_add(n4129, n3805);
    let n4132: ZW = zw_add(n4130, n3806);
    let n4133: ZW = zw_cellmix_n(369u64, n2204, 1542469173u64);
    let n4134: ZW = zw_cellmix_n(369u64, n2204, 668265263u64);
    let n4135: ZW = zw_add(n4131, n4133);
    let n4136: ZW = zw_add(n4132, n4134);
    let n4137: ZW = zw_cellmix_n(370u64, n2202, 1542469173u64);
    let n4138: ZW = zw_cellmix_n(370u64, n2202, 668265263u64);
    let n4139: ZW = zw_add(n4135, n4137);
    let n4140: ZW = zw_add(n4136, n4138);
    let n4141: ZW = zw_add(n4123, n3943);
    let n4142: ZW = zw_add(n4124, n3944);
    let n4143: ZW = zw_add(n4141, n3947);
    let n4144: ZW = zw_add(n4142, n3948);
    let n4145: ZW = zw_add(n4143, n3991);
    let n4146: ZW = zw_add(n4144, n3992);
    let n4147: ZW = zw_add(n4145, n3817);
    let n4148: ZW = zw_add(n4146, n3818);
    let n4149: ZW = zw_cellmix_n(369u64, n2210, 1542469173u64);
    let n4150: ZW = zw_cellmix_n(369u64, n2210, 668265263u64);
    let n4151: ZW = zw_add(n4147, n4149);
    let n4152: ZW = zw_add(n4148, n4150);
    let n4153: ZW = zw_cellmix_n(370u64, n2208, 1542469173u64);
    let n4154: ZW = zw_cellmix_n(370u64, n2208, 668265263u64);
    let n4155: ZW = zw_add(n4151, n4153);
    let n4156: ZW = zw_add(n4152, n4154);
    let n4157: ZW = zw_add(n4141, n3963);
    let n4158: ZW = zw_add(n4142, n3964);
    let n4159: ZW = zw_add(n4157, n3991);
    let n4160: ZW = zw_add(n4158, n3992);
    let n4161: ZW = zw_add(n4159, n3829);
    let n4162: ZW = zw_add(n4160, n3830);
    let n4163: ZW = zw_cellmix_n(369u64, n2216, 1542469173u64);
    let n4164: ZW = zw_cellmix_n(369u64, n2216, 668265263u64);
    let n4165: ZW = zw_add(n4161, n4163);
    let n4166: ZW = zw_add(n4162, n4164);
    let n4167: ZW = zw_cellmix_n(370u64, n2214, 1542469173u64);
    let n4168: ZW = zw_cellmix_n(370u64, n2214, 668265263u64);
    let n4169: ZW = zw_add(n4165, n4167);
    let n4170: ZW = zw_add(n4166, n4168);
    let n4171: ZW = zw_add(n4127, n4035);
    let n4172: ZW = zw_add(n4128, n4036);
    let n4173: ZW = zw_add(n4171, n3805);
    let n4174: ZW = zw_add(n4172, n3806);
    let n4175: ZW = zw_add(n4173, n4133);
    let n4176: ZW = zw_add(n4174, n4134);
    let n4177: ZW = zw_cellmix_n(370u64, n2218, 1542469173u64);
    let n4178: ZW = zw_cellmix_n(370u64, n2218, 668265263u64);
    let n4179: ZW = zw_add(n4175, n4177);
    let n4180: ZW = zw_add(n4176, n4178);
    let n4181: ZW = zw_add(n4143, n4035);
    let n4182: ZW = zw_add(n4144, n4036);
    let n4183: ZW = zw_add(n4181, n3817);
    let n4184: ZW = zw_add(n4182, n3818);
    let n4185: ZW = zw_add(n4183, n4149);
    let n4186: ZW = zw_add(n4184, n4150);
    let n4187: ZW = zw_cellmix_n(370u64, n2220, 1542469173u64);
    let n4188: ZW = zw_cellmix_n(370u64, n2220, 668265263u64);
    let n4189: ZW = zw_add(n4185, n4187);
    let n4190: ZW = zw_add(n4186, n4188);
    let n4191: ZW = zw_add(n4157, n4035);
    let n4192: ZW = zw_add(n4158, n4036);
    let n4193: ZW = zw_add(n4191, n3829);
    let n4194: ZW = zw_add(n4192, n3830);
    let n4195: ZW = zw_add(n4193, n4163);
    let n4196: ZW = zw_add(n4194, n4164);
    let n4197: ZW = zw_cellmix_n(370u64, n2222, 1542469173u64);
    let n4198: ZW = zw_cellmix_n(370u64, n2222, 668265263u64);
    let n4199: ZW = zw_add(n4195, n4197);
    let n4200: ZW = zw_add(n4196, n4198);
    let n4201: ZW = zw_cellmix_n(87u64, n2303, 1542469173u64);
    let n4202: ZW = zw_cellmix_n(87u64, n2303, 668265263u64);
    let n4203: ZW = zw_add(n3693, n4201);
    let n4204: ZW = zw_add(n3694, n4202);
    let n4205: ZW = zw_cellmix_n(259u64, n1391, 1542469173u64);
    let n4206: ZW = zw_cellmix_n(259u64, n1391, 668265263u64);
    let n4207: ZW = zw_add(n4203, n4205);
    let n4208: ZW = zw_add(n4204, n4206);
    let n4209: ZW = zw_cellmix_n(272u64, n1392, 1542469173u64);
    let n4210: ZW = zw_cellmix_n(272u64, n1392, 668265263u64);
    let n4211: ZW = zw_add(n4207, n4209);
    let n4212: ZW = zw_add(n4208, n4210);
    let n4213: ZW = zw_add(n4211, n2761);
    let n4214: ZW = zw_add(n4212, n2762);
    let n4215: ZW = zw_add(n4213, n2765);
    let n4216: ZW = zw_add(n4214, n2766);
    let n4217: ZW = zw_add(n4211, n2899);
    let n4218: ZW = zw_add(n4212, n2900);
    let n4219: ZW = zw_add(n4217, n2903);
    let n4220: ZW = zw_add(n4218, n2904);
    let n4221: ZW = zw_cellmix_n(87u64, n2475, 1542469173u64);
    let n4222: ZW = zw_cellmix_n(87u64, n2475, 668265263u64);
    let n4223: ZW = zw_add(n3693, n4221);
    let n4224: ZW = zw_add(n3694, n4222);
    let n4225: ZW = zw_add(n4223, n3715);
    let n4226: ZW = zw_add(n4224, n3716);
    let n4227: ZW = zw_add(n4225, n3719);
    let n4228: ZW = zw_add(n4226, n3720);
    let n4229: ZW = zw_cellmix_n(276u64, n1391, 1542469173u64);
    let n4230: ZW = zw_cellmix_n(276u64, n1391, 668265263u64);
    let n4231: ZW = zw_add(n4227, n4229);
    let n4232: ZW = zw_add(n4228, n4230);
    let n4233: ZW = zw_cellmix_n(289u64, n1392, 1542469173u64);
    let n4234: ZW = zw_cellmix_n(289u64, n1392, 668265263u64);
    let n4235: ZW = zw_add(n4231, n4233);
    let n4236: ZW = zw_add(n4232, n4234);
    let n4237: ZW = zw_add(n4235, n2761);
    let n4238: ZW = zw_add(n4236, n2762);
    let n4239: ZW = zw_add(n4237, n2765);
    let n4240: ZW = zw_add(n4238, n2766);
    let n4241: ZW = zw_cellmix_n(20u64, n2480, 1542469173u64);
    let n4242: ZW = zw_cellmix_n(20u64, n2480, 668265263u64);
    let n4243: ZW = zw_add(n4235, n4241);
    let n4244: ZW = zw_add(n4236, n4242);
    let n4245: ZW = zw_cellmix_b(41u64, n2481, 1542469173u64);
    let n4246: ZW = zw_cellmix_b(41u64, n2481, 668265263u64);
    let n4247: ZW = zw_add(n4243, n4245);
    let n4248: ZW = zw_add(n4244, n4246);
    let n4249: ZW = zw_cellmix_b(38u64, n2487, 1542469173u64);
    let n4250: ZW = zw_cellmix_b(38u64, n2487, 668265263u64);
    let n4251: ZW = zw_add(zw_splat(0u64), n4249);
    let n4252: ZW = zw_add(zw_splat(0u64), n4250);
    let n4253: ZW = zw_cellmix_n(39u64, n2491, 1542469173u64);
    let n4254: ZW = zw_cellmix_n(39u64, n2491, 668265263u64);
    let n4255: ZW = zw_add(n4251, n4253);
    let n4256: ZW = zw_add(n4252, n4254);
    let n4257: ZW = zw_add(n4255, n2725);
    let n4258: ZW = zw_add(n4256, n2726);
    let n4259: ZW = zw_add(n4257, n2729);
    let n4260: ZW = zw_add(n4258, n2730);
    let n4261: ZW = zw_add(n4259, n2733);
    let n4262: ZW = zw_add(n4260, n2734);
    let n4263: ZW = zw_cellmix_n(87u64, n2490, 1542469173u64);
    let n4264: ZW = zw_cellmix_n(87u64, n2490, 668265263u64);
    let n4265: ZW = zw_add(n4261, n4263);
    let n4266: ZW = zw_add(n4262, n4264);
    let n4267: ZW = zw_add(n4265, n2761);
    let n4268: ZW = zw_add(n4266, n2762);
    let n4269: ZW = zw_add(n4265, n4241);
    let n4270: ZW = zw_add(n4266, n4242);
    let n4271: ZW = zw_cellmix_b(38u64, n2493, 1542469173u64);
    let n4272: ZW = zw_cellmix_b(38u64, n2493, 668265263u64);
    let n4273: ZW = zw_add(zw_splat(0u64), n4271);
    let n4274: ZW = zw_add(zw_splat(0u64), n4272);
    let n4275: ZW = zw_cellmix_n(39u64, n2497, 1542469173u64);
    let n4276: ZW = zw_cellmix_n(39u64, n2497, 668265263u64);
    let n4277: ZW = zw_add(n4273, n4275);
    let n4278: ZW = zw_add(n4274, n4276);
    let n4279: ZW = zw_add(n4277, n2725);
    let n4280: ZW = zw_add(n4278, n2726);
    let n4281: ZW = zw_add(n4279, n2729);
    let n4282: ZW = zw_add(n4280, n2730);
    let n4283: ZW = zw_add(n4281, n2733);
    let n4284: ZW = zw_add(n4282, n2734);
    let n4285: ZW = zw_cellmix_n(87u64, n2496, 1542469173u64);
    let n4286: ZW = zw_cellmix_n(87u64, n2496, 668265263u64);
    let n4287: ZW = zw_add(n4283, n4285);
    let n4288: ZW = zw_add(n4284, n4286);
    let n4289: ZW = zw_add(n4287, n2761);
    let n4290: ZW = zw_add(n4288, n2762);
    let n4291: ZW = zw_add(n4287, n2899);
    let n4292: ZW = zw_add(n4288, n2900);
    let n4293: ZW = zw_cellmix_n(246u64, n2500, 1542469173u64);
    let n4294: ZW = zw_cellmix_n(246u64, n2500, 668265263u64);
    let n4295: ZW = zw_add(n2739, n4293);
    let n4296: ZW = zw_add(n2740, n4294);
    let n4297: ZW = zw_cellmix_n(254u64, n2501, 1542469173u64);
    let n4298: ZW = zw_cellmix_n(254u64, n2501, 668265263u64);
    let n4299: ZW = zw_add(n4295, n4297);
    let n4300: ZW = zw_add(n4296, n4298);
    let n4301: ZW = zw_cellmix_n(277u64, n2506, 1542469173u64);
    let n4302: ZW = zw_cellmix_n(277u64, n2506, 668265263u64);
    let n4303: ZW = zw_add(n4299, n4301);
    let n4304: ZW = zw_add(n4300, n4302);
    let n4305: ZW = zw_cellmix_n(290u64, n2507, 1542469173u64);
    let n4306: ZW = zw_cellmix_n(290u64, n2507, 668265263u64);
    let n4307: ZW = zw_add(n4303, n4305);
    let n4308: ZW = zw_add(n4304, n4306);
    let n4309: ZW = zw_cellmix_n(318u64, n2513, 1542469173u64);
    let n4310: ZW = zw_cellmix_n(318u64, n2513, 668265263u64);
    let n4311: ZW = zw_add(n4307, n4309);
    let n4312: ZW = zw_add(n4308, n4310);
    let n4313: ZW = zw_cellmix_n(405u64, n2515, 1542469173u64);
    let n4314: ZW = zw_cellmix_n(405u64, n2515, 668265263u64);
    let n4315: ZW = zw_add(n4311, n4313);
    let n4316: ZW = zw_add(n4312, n4314);
    let n4317: ZW = zw_cellmix_n(406u64, n2516, 1542469173u64);
    let n4318: ZW = zw_cellmix_n(406u64, n2516, 668265263u64);
    let n4319: ZW = zw_add(n4315, n4317);
    let n4320: ZW = zw_add(n4316, n4318);
    let n4321: ZW = zw_cellmix_n(20u64, n2505, 1542469173u64);
    let n4322: ZW = zw_cellmix_n(20u64, n2505, 668265263u64);
    let n4323: ZW = zw_add(n4319, n4321);
    let n4324: ZW = zw_add(n4320, n4322);
    let n4325: ZW = zw_add(n4323, n2765);
    let n4326: ZW = zw_add(n4324, n2766);
    let n4327: ZW = zw_cellmix_n(298u64, n2508, 1542469173u64);
    let n4328: ZW = zw_cellmix_n(298u64, n2508, 668265263u64);
    let n4329: ZW = zw_add(n4325, n4327);
    let n4330: ZW = zw_add(n4326, n4328);
    let n4331: ZW = zw_cellmix_n(300u64, n2509, 1542469173u64);
    let n4332: ZW = zw_cellmix_n(300u64, n2509, 668265263u64);
    let n4333: ZW = zw_add(n4329, n4331);
    let n4334: ZW = zw_add(n4330, n4332);
    let n4335: ZW = zw_cellmix_n(301u64, n2510, 1542469173u64);
    let n4336: ZW = zw_cellmix_n(301u64, n2510, 668265263u64);
    let n4337: ZW = zw_add(n4333, n4335);
    let n4338: ZW = zw_add(n4334, n4336);
    let n4339: ZW = zw_cellmix_n(303u64, n2511, 1542469173u64);
    let n4340: ZW = zw_cellmix_n(303u64, n2511, 668265263u64);
    let n4341: ZW = zw_add(n4337, n4339);
    let n4342: ZW = zw_add(n4338, n4340);
    let n4343: ZW = zw_cellmix_b(310u64, n2502, 1542469173u64);
    let n4344: ZW = zw_cellmix_b(310u64, n2502, 668265263u64);
    let n4345: ZW = zw_add(n4341, n4343);
    let n4346: ZW = zw_add(n4342, n4344);
    let n4347: ZW = zw_cellmix_b(311u64, n2503, 1542469173u64);
    let n4348: ZW = zw_cellmix_b(311u64, n2503, 668265263u64);
    let n4349: ZW = zw_add(n4345, n4347);
    let n4350: ZW = zw_add(n4346, n4348);
    let n4351: ZW = zw_cellmix_n(317u64, n2529, 1542469173u64);
    let n4352: ZW = zw_cellmix_n(317u64, n2529, 668265263u64);
    let n4353: ZW = zw_add(n4349, n4351);
    let n4354: ZW = zw_add(n4350, n4352);
    let n4355: ZW = zw_cellmix_n(395u64, r_c395, 1542469173u64);
    let n4356: ZW = zw_cellmix_n(395u64, r_c395, 668265263u64);
    let n4357: ZW = zw_add(n4353, n4355);
    let n4358: ZW = zw_add(n4354, n4356);
    let n4359: ZW = zw_cellmix_n(396u64, r_c396, 1542469173u64);
    let n4360: ZW = zw_cellmix_n(396u64, r_c396, 668265263u64);
    let n4361: ZW = zw_add(n4357, n4359);
    let n4362: ZW = zw_add(n4358, n4360);
    let n4363: ZW = zw_cellmix_n(397u64, r_c397, 1542469173u64);
    let n4364: ZW = zw_cellmix_n(397u64, r_c397, 668265263u64);
    let n4365: ZW = zw_add(n4361, n4363);
    let n4366: ZW = zw_add(n4362, n4364);
    let n4367: ZW = zw_cellmix_n(398u64, r_c398, 1542469173u64);
    let n4368: ZW = zw_cellmix_n(398u64, r_c398, 668265263u64);
    let n4369: ZW = zw_add(n4365, n4367);
    let n4370: ZW = zw_add(n4366, n4368);
    let n4371: ZW = zw_cellmix_b(399u64, n2514, 1542469173u64);
    let n4372: ZW = zw_cellmix_b(399u64, n2514, 668265263u64);
    let n4373: ZW = zw_add(n4369, n4371);
    let n4374: ZW = zw_add(n4370, n4372);
    let n4375: ZW = zw_cellmix_n(407u64, n2530, 1542469173u64);
    let n4376: ZW = zw_cellmix_n(407u64, n2530, 668265263u64);
    let n4377: ZW = zw_add(n4373, n4375);
    let n4378: ZW = zw_add(n4374, n4376);
    let n4379: ZW = zw_cellmix_n(408u64, n2518, 1542469173u64);
    let n4380: ZW = zw_cellmix_n(408u64, n2518, 668265263u64);
    let n4381: ZW = zw_add(n4377, n4379);
    let n4382: ZW = zw_add(n4378, n4380);
    let n4383: ZW = zw_cellmix_b(399u64, n2532, 1542469173u64);
    let n4384: ZW = zw_cellmix_b(399u64, n2532, 668265263u64);
    let n4385: ZW = zw_add(n4369, n4383);
    let n4386: ZW = zw_add(n4370, n4384);
    let n4387: ZW = zw_cellmix_n(407u64, n2536, 1542469173u64);
    let n4388: ZW = zw_cellmix_n(407u64, n2536, 668265263u64);
    let n4389: ZW = zw_add(n4385, n4387);
    let n4390: ZW = zw_add(n4386, n4388);
    let n4391: ZW = zw_cellmix_n(408u64, n2534, 1542469173u64);
    let n4392: ZW = zw_cellmix_n(408u64, n2534, 668265263u64);
    let n4393: ZW = zw_add(n4389, n4391);
    let n4394: ZW = zw_add(n4390, n4392);
    let n4395: ZW = zw_cellmix_b(399u64, n2537, 1542469173u64);
    let n4396: ZW = zw_cellmix_b(399u64, n2537, 668265263u64);
    let n4397: ZW = zw_add(n4369, n4395);
    let n4398: ZW = zw_add(n4370, n4396);
    let n4399: ZW = zw_cellmix_n(407u64, n2541, 1542469173u64);
    let n4400: ZW = zw_cellmix_n(407u64, n2541, 668265263u64);
    let n4401: ZW = zw_add(n4397, n4399);
    let n4402: ZW = zw_add(n4398, n4400);
    let n4403: ZW = zw_cellmix_n(408u64, n2539, 1542469173u64);
    let n4404: ZW = zw_cellmix_n(408u64, n2539, 668265263u64);
    let n4405: ZW = zw_add(n4401, n4403);
    let n4406: ZW = zw_add(n4402, n4404);
    let n4407: ZW = zw_cellmix_n(303u64, n2543, 1542469173u64);
    let n4408: ZW = zw_cellmix_n(303u64, n2543, 668265263u64);
    let n4409: ZW = zw_add(n4337, n4407);
    let n4410: ZW = zw_add(n4338, n4408);
    let n4411: ZW = zw_add(n4409, n4343);
    let n4412: ZW = zw_add(n4410, n4344);
    let n4413: ZW = zw_cellmix_b(311u64, n2542, 1542469173u64);
    let n4414: ZW = zw_cellmix_b(311u64, n2542, 668265263u64);
    let n4415: ZW = zw_add(n4411, n4413);
    let n4416: ZW = zw_add(n4412, n4414);
    let n4417: ZW = zw_add(n4415, n4351);
    let n4418: ZW = zw_add(n4416, n4352);
    let n4419: ZW = zw_add(n4417, n4355);
    let n4420: ZW = zw_add(n4418, n4356);
    let n4421: ZW = zw_add(n4419, n4359);
    let n4422: ZW = zw_add(n4420, n4360);
    let n4423: ZW = zw_add(n4421, n4363);
    let n4424: ZW = zw_add(n4422, n4364);
    let n4425: ZW = zw_add(n4423, n4367);
    let n4426: ZW = zw_add(n4424, n4368);
    let n4427: ZW = zw_add(n4425, n4371);
    let n4428: ZW = zw_add(n4426, n4372);
    let n4429: ZW = zw_cellmix_n(407u64, n2547, 1542469173u64);
    let n4430: ZW = zw_cellmix_n(407u64, n2547, 668265263u64);
    let n4431: ZW = zw_add(n4427, n4429);
    let n4432: ZW = zw_add(n4428, n4430);
    let n4433: ZW = zw_cellmix_n(408u64, n2545, 1542469173u64);
    let n4434: ZW = zw_cellmix_n(408u64, n2545, 668265263u64);
    let n4435: ZW = zw_add(n4431, n4433);
    let n4436: ZW = zw_add(n4432, n4434);
    let n4437: ZW = zw_add(n4425, n4383);
    let n4438: ZW = zw_add(n4426, n4384);
    let n4439: ZW = zw_cellmix_n(407u64, n2551, 1542469173u64);
    let n4440: ZW = zw_cellmix_n(407u64, n2551, 668265263u64);
    let n4441: ZW = zw_add(n4437, n4439);
    let n4442: ZW = zw_add(n4438, n4440);
    let n4443: ZW = zw_cellmix_n(408u64, n2549, 1542469173u64);
    let n4444: ZW = zw_cellmix_n(408u64, n2549, 668265263u64);
    let n4445: ZW = zw_add(n4441, n4443);
    let n4446: ZW = zw_add(n4442, n4444);
    let n4447: ZW = zw_add(n4425, n4395);
    let n4448: ZW = zw_add(n4426, n4396);
    let n4449: ZW = zw_cellmix_n(407u64, n2555, 1542469173u64);
    let n4450: ZW = zw_cellmix_n(407u64, n2555, 668265263u64);
    let n4451: ZW = zw_add(n4447, n4449);
    let n4452: ZW = zw_add(n4448, n4450);
    let n4453: ZW = zw_cellmix_n(408u64, n2553, 1542469173u64);
    let n4454: ZW = zw_cellmix_n(408u64, n2553, 668265263u64);
    let n4455: ZW = zw_add(n4451, n4453);
    let n4456: ZW = zw_add(n4452, n4454);
    let n4457: ZW = zw_cellmix_n(20u64, n2575, 1542469173u64);
    let n4458: ZW = zw_cellmix_n(20u64, n2575, 668265263u64);
    let n4459: ZW = zw_add(n4319, n4457);
    let n4460: ZW = zw_add(n4320, n4458);
    let n4461: ZW = zw_cellmix_b(41u64, n2576, 1542469173u64);
    let n4462: ZW = zw_cellmix_b(41u64, n2576, 668265263u64);
    let n4463: ZW = zw_add(n4459, n4461);
    let n4464: ZW = zw_add(n4460, n4462);
    let n4465: ZW = zw_cellmix_n(298u64, n2577, 1542469173u64);
    let n4466: ZW = zw_cellmix_n(298u64, n2577, 668265263u64);
    let n4467: ZW = zw_add(n4463, n4465);
    let n4468: ZW = zw_add(n4464, n4466);
    let n4469: ZW = zw_cellmix_n(300u64, n2578, 1542469173u64);
    let n4470: ZW = zw_cellmix_n(300u64, n2578, 668265263u64);
    let n4471: ZW = zw_add(n4467, n4469);
    let n4472: ZW = zw_add(n4468, n4470);
    let n4473: ZW = zw_cellmix_n(301u64, n2579, 1542469173u64);
    let n4474: ZW = zw_cellmix_n(301u64, n2579, 668265263u64);
    let n4475: ZW = zw_add(n4471, n4473);
    let n4476: ZW = zw_add(n4472, n4474);
    let n4477: ZW = zw_add(n4475, n4339);
    let n4478: ZW = zw_add(n4476, n4340);
    let n4479: ZW = zw_cellmix_b(310u64, n2556, 1542469173u64);
    let n4480: ZW = zw_cellmix_b(310u64, n2556, 668265263u64);
    let n4481: ZW = zw_add(n4477, n4479);
    let n4482: ZW = zw_add(n4478, n4480);
    let n4483: ZW = zw_add(n4481, n4347);
    let n4484: ZW = zw_add(n4482, n4348);
    let n4485: ZW = zw_cellmix_n(317u64, n2588, 1542469173u64);
    let n4486: ZW = zw_cellmix_n(317u64, n2588, 668265263u64);
    let n4487: ZW = zw_add(n4483, n4485);
    let n4488: ZW = zw_add(n4484, n4486);
    let n4489: ZW = zw_cellmix_n(395u64, n2580, 1542469173u64);
    let n4490: ZW = zw_cellmix_n(395u64, n2580, 668265263u64);
    let n4491: ZW = zw_add(n4487, n4489);
    let n4492: ZW = zw_add(n4488, n4490);
    let n4493: ZW = zw_cellmix_n(396u64, n2581, 1542469173u64);
    let n4494: ZW = zw_cellmix_n(396u64, n2581, 668265263u64);
    let n4495: ZW = zw_add(n4491, n4493);
    let n4496: ZW = zw_add(n4492, n4494);
    let n4497: ZW = zw_cellmix_n(397u64, n2582, 1542469173u64);
    let n4498: ZW = zw_cellmix_n(397u64, n2582, 668265263u64);
    let n4499: ZW = zw_add(n4495, n4497);
    let n4500: ZW = zw_add(n4496, n4498);
    let n4501: ZW = zw_cellmix_n(398u64, n2583, 1542469173u64);
    let n4502: ZW = zw_cellmix_n(398u64, n2583, 668265263u64);
    let n4503: ZW = zw_add(n4499, n4501);
    let n4504: ZW = zw_add(n4500, n4502);
    let n4505: ZW = zw_add(n4503, n4371);
    let n4506: ZW = zw_add(n4504, n4372);
    let n4507: ZW = zw_cellmix_n(407u64, n2589, 1542469173u64);
    let n4508: ZW = zw_cellmix_n(407u64, n2589, 668265263u64);
    let n4509: ZW = zw_add(n4505, n4507);
    let n4510: ZW = zw_add(n4506, n4508);
    let n4511: ZW = zw_cellmix_n(408u64, n2585, 1542469173u64);
    let n4512: ZW = zw_cellmix_n(408u64, n2585, 668265263u64);
    let n4513: ZW = zw_add(n4509, n4511);
    let n4514: ZW = zw_add(n4510, n4512);
    let n4515: ZW = zw_cellmix_n(396u64, n2598, 1542469173u64);
    let n4516: ZW = zw_cellmix_n(396u64, n2598, 668265263u64);
    let n4517: ZW = zw_add(n4491, n4515);
    let n4518: ZW = zw_add(n4492, n4516);
    let n4519: ZW = zw_cellmix_n(397u64, n2599, 1542469173u64);
    let n4520: ZW = zw_cellmix_n(397u64, n2599, 668265263u64);
    let n4521: ZW = zw_add(n4517, n4519);
    let n4522: ZW = zw_add(n4518, n4520);
    let n4523: ZW = zw_add(n4521, n4501);
    let n4524: ZW = zw_add(n4522, n4502);
    let n4525: ZW = zw_add(n4523, n4383);
    let n4526: ZW = zw_add(n4524, n4384);
    let n4527: ZW = zw_cellmix_n(407u64, n2603, 1542469173u64);
    let n4528: ZW = zw_cellmix_n(407u64, n2603, 668265263u64);
    let n4529: ZW = zw_add(n4525, n4527);
    let n4530: ZW = zw_add(n4526, n4528);
    let n4531: ZW = zw_cellmix_n(408u64, n2601, 1542469173u64);
    let n4532: ZW = zw_cellmix_n(408u64, n2601, 668265263u64);
    let n4533: ZW = zw_add(n4529, n4531);
    let n4534: ZW = zw_add(n4530, n4532);
    let n4535: ZW = zw_cellmix_n(397u64, n2610, 1542469173u64);
    let n4536: ZW = zw_cellmix_n(397u64, n2610, 668265263u64);
    let n4537: ZW = zw_add(n4517, n4535);
    let n4538: ZW = zw_add(n4518, n4536);
    let n4539: ZW = zw_add(n4537, n4501);
    let n4540: ZW = zw_add(n4538, n4502);
    let n4541: ZW = zw_add(n4539, n4395);
    let n4542: ZW = zw_add(n4540, n4396);
    let n4543: ZW = zw_cellmix_n(407u64, n2614, 1542469173u64);
    let n4544: ZW = zw_cellmix_n(407u64, n2614, 668265263u64);
    let n4545: ZW = zw_add(n4541, n4543);
    let n4546: ZW = zw_add(n4542, n4544);
    let n4547: ZW = zw_cellmix_n(408u64, n2612, 1542469173u64);
    let n4548: ZW = zw_cellmix_n(408u64, n2612, 668265263u64);
    let n4549: ZW = zw_add(n4545, n4547);
    let n4550: ZW = zw_add(n4546, n4548);
    let n4551: ZW = zw_cellmix_n(395u64, n2627, 1542469173u64);
    let n4552: ZW = zw_cellmix_n(395u64, n2627, 668265263u64);
    let n4553: ZW = zw_add(n4487, n4551);
    let n4554: ZW = zw_add(n4488, n4552);
    let n4555: ZW = zw_cellmix_n(396u64, n2628, 1542469173u64);
    let n4556: ZW = zw_cellmix_n(396u64, n2628, 668265263u64);
    let n4557: ZW = zw_add(n4553, n4555);
    let n4558: ZW = zw_add(n4554, n4556);
    let n4559: ZW = zw_cellmix_n(397u64, n2629, 1542469173u64);
    let n4560: ZW = zw_cellmix_n(397u64, n2629, 668265263u64);
    let n4561: ZW = zw_add(n4557, n4559);
    let n4562: ZW = zw_add(n4558, n4560);
    let n4563: ZW = zw_cellmix_n(398u64, n2630, 1542469173u64);
    let n4564: ZW = zw_cellmix_n(398u64, n2630, 668265263u64);
    let n4565: ZW = zw_add(n4561, n4563);
    let n4566: ZW = zw_add(n4562, n4564);
    let n4567: ZW = zw_add(n4565, n4371);
    let n4568: ZW = zw_add(n4566, n4372);
    let n4569: ZW = zw_cellmix_n(407u64, n2634, 1542469173u64);
    let n4570: ZW = zw_cellmix_n(407u64, n2634, 668265263u64);
    let n4571: ZW = zw_add(n4567, n4569);
    let n4572: ZW = zw_add(n4568, n4570);
    let n4573: ZW = zw_cellmix_n(408u64, n2632, 1542469173u64);
    let n4574: ZW = zw_cellmix_n(408u64, n2632, 668265263u64);
    let n4575: ZW = zw_add(n4571, n4573);
    let n4576: ZW = zw_add(n4572, n4574);
    let n4577: ZW = zw_add(n4553, n4515);
    let n4578: ZW = zw_add(n4554, n4516);
    let n4579: ZW = zw_add(n4577, n4519);
    let n4580: ZW = zw_add(n4578, n4520);
    let n4581: ZW = zw_add(n4579, n4563);
    let n4582: ZW = zw_add(n4580, n4564);
    let n4583: ZW = zw_add(n4581, n4383);
    let n4584: ZW = zw_add(n4582, n4384);
    let n4585: ZW = zw_cellmix_n(407u64, n2642, 1542469173u64);
    let n4586: ZW = zw_cellmix_n(407u64, n2642, 668265263u64);
    let n4587: ZW = zw_add(n4583, n4585);
    let n4588: ZW = zw_add(n4584, n4586);
    let n4589: ZW = zw_cellmix_n(408u64, n2640, 1542469173u64);
    let n4590: ZW = zw_cellmix_n(408u64, n2640, 668265263u64);
    let n4591: ZW = zw_add(n4587, n4589);
    let n4592: ZW = zw_add(n4588, n4590);
    let n4593: ZW = zw_add(n4577, n4535);
    let n4594: ZW = zw_add(n4578, n4536);
    let n4595: ZW = zw_add(n4593, n4563);
    let n4596: ZW = zw_add(n4594, n4564);
    let n4597: ZW = zw_add(n4595, n4395);
    let n4598: ZW = zw_add(n4596, n4396);
    let n4599: ZW = zw_cellmix_n(407u64, n2650, 1542469173u64);
    let n4600: ZW = zw_cellmix_n(407u64, n2650, 668265263u64);
    let n4601: ZW = zw_add(n4597, n4599);
    let n4602: ZW = zw_add(n4598, n4600);
    let n4603: ZW = zw_cellmix_n(408u64, n2648, 1542469173u64);
    let n4604: ZW = zw_cellmix_n(408u64, n2648, 668265263u64);
    let n4605: ZW = zw_add(n4601, n4603);
    let n4606: ZW = zw_add(n4602, n4604);
    let n4607: ZW = zw_cellmix_n(398u64, n2655, 1542469173u64);
    let n4608: ZW = zw_cellmix_n(398u64, n2655, 668265263u64);
    let n4609: ZW = zw_add(n4561, n4607);
    let n4610: ZW = zw_add(n4562, n4608);
    let n4611: ZW = zw_add(n4609, n4371);
    let n4612: ZW = zw_add(n4610, n4372);
    let n4613: ZW = zw_add(n4611, n4569);
    let n4614: ZW = zw_add(n4612, n4570);
    let n4615: ZW = zw_cellmix_n(408u64, n2656, 1542469173u64);
    let n4616: ZW = zw_cellmix_n(408u64, n2656, 668265263u64);
    let n4617: ZW = zw_add(n4613, n4615);
    let n4618: ZW = zw_add(n4614, n4616);
    let n4619: ZW = zw_add(n4579, n4607);
    let n4620: ZW = zw_add(n4580, n4608);
    let n4621: ZW = zw_add(n4619, n4383);
    let n4622: ZW = zw_add(n4620, n4384);
    let n4623: ZW = zw_add(n4621, n4585);
    let n4624: ZW = zw_add(n4622, n4586);
    let n4625: ZW = zw_cellmix_n(408u64, n2659, 1542469173u64);
    let n4626: ZW = zw_cellmix_n(408u64, n2659, 668265263u64);
    let n4627: ZW = zw_add(n4623, n4625);
    let n4628: ZW = zw_add(n4624, n4626);
    let n4629: ZW = zw_add(n4593, n4607);
    let n4630: ZW = zw_add(n4594, n4608);
    let n4631: ZW = zw_add(n4629, n4395);
    let n4632: ZW = zw_add(n4630, n4396);
    let n4633: ZW = zw_add(n4631, n4599);
    let n4634: ZW = zw_add(n4632, n4600);
    let n4635: ZW = zw_cellmix_n(408u64, n2662, 1542469173u64);
    let n4636: ZW = zw_cellmix_n(408u64, n2662, 668265263u64);
    let n4637: ZW = zw_add(n4633, n4635);
    let n4638: ZW = zw_add(n4634, n4636);
    let n4639: ZW = zw_add(n4475, n4407);
    let n4640: ZW = zw_add(n4476, n4408);
    let n4641: ZW = zw_add(n4639, n4479);
    let n4642: ZW = zw_add(n4640, n4480);
    let n4643: ZW = zw_add(n4641, n4413);
    let n4644: ZW = zw_add(n4642, n4414);
    let n4645: ZW = zw_add(n4643, n4485);
    let n4646: ZW = zw_add(n4644, n4486);
    let n4647: ZW = zw_add(n4645, n4489);
    let n4648: ZW = zw_add(n4646, n4490);
    let n4649: ZW = zw_add(n4647, n4493);
    let n4650: ZW = zw_add(n4648, n4494);
    let n4651: ZW = zw_add(n4649, n4497);
    let n4652: ZW = zw_add(n4650, n4498);
    let n4653: ZW = zw_add(n4651, n4501);
    let n4654: ZW = zw_add(n4652, n4502);
    let n4655: ZW = zw_add(n4653, n4371);
    let n4656: ZW = zw_add(n4654, n4372);
    let n4657: ZW = zw_cellmix_n(407u64, n2670, 1542469173u64);
    let n4658: ZW = zw_cellmix_n(407u64, n2670, 668265263u64);
    let n4659: ZW = zw_add(n4655, n4657);
    let n4660: ZW = zw_add(n4656, n4658);
    let n4661: ZW = zw_cellmix_n(408u64, n2668, 1542469173u64);
    let n4662: ZW = zw_cellmix_n(408u64, n2668, 668265263u64);
    let n4663: ZW = zw_add(n4659, n4661);
    let n4664: ZW = zw_add(n4660, n4662);
    let n4665: ZW = zw_add(n4647, n4515);
    let n4666: ZW = zw_add(n4648, n4516);
    let n4667: ZW = zw_add(n4665, n4519);
    let n4668: ZW = zw_add(n4666, n4520);
    let n4669: ZW = zw_add(n4667, n4501);
    let n4670: ZW = zw_add(n4668, n4502);
    let n4671: ZW = zw_add(n4669, n4383);
    let n4672: ZW = zw_add(n4670, n4384);
    let n4673: ZW = zw_cellmix_n(407u64, n2678, 1542469173u64);
    let n4674: ZW = zw_cellmix_n(407u64, n2678, 668265263u64);
    let n4675: ZW = zw_add(n4671, n4673);
    let n4676: ZW = zw_add(n4672, n4674);
    let n4677: ZW = zw_cellmix_n(408u64, n2676, 1542469173u64);
    let n4678: ZW = zw_cellmix_n(408u64, n2676, 668265263u64);
    let n4679: ZW = zw_add(n4675, n4677);
    let n4680: ZW = zw_add(n4676, n4678);
    let n4681: ZW = zw_add(n4665, n4535);
    let n4682: ZW = zw_add(n4666, n4536);
    let n4683: ZW = zw_add(n4681, n4501);
    let n4684: ZW = zw_add(n4682, n4502);
    let n4685: ZW = zw_add(n4683, n4395);
    let n4686: ZW = zw_add(n4684, n4396);
    let n4687: ZW = zw_cellmix_n(407u64, n2686, 1542469173u64);
    let n4688: ZW = zw_cellmix_n(407u64, n2686, 668265263u64);
    let n4689: ZW = zw_add(n4685, n4687);
    let n4690: ZW = zw_add(n4686, n4688);
    let n4691: ZW = zw_cellmix_n(408u64, n2684, 1542469173u64);
    let n4692: ZW = zw_cellmix_n(408u64, n2684, 668265263u64);
    let n4693: ZW = zw_add(n4689, n4691);
    let n4694: ZW = zw_add(n4690, n4692);
    let n4695: ZW = zw_add(n4645, n4551);
    let n4696: ZW = zw_add(n4646, n4552);
    let n4697: ZW = zw_add(n4695, n4555);
    let n4698: ZW = zw_add(n4696, n4556);
    let n4699: ZW = zw_add(n4697, n4559);
    let n4700: ZW = zw_add(n4698, n4560);
    let n4701: ZW = zw_add(n4699, n4563);
    let n4702: ZW = zw_add(n4700, n4564);
    let n4703: ZW = zw_add(n4701, n4371);
    let n4704: ZW = zw_add(n4702, n4372);
    let n4705: ZW = zw_cellmix_n(407u64, n2694, 1542469173u64);
    let n4706: ZW = zw_cellmix_n(407u64, n2694, 668265263u64);
    let n4707: ZW = zw_add(n4703, n4705);
    let n4708: ZW = zw_add(n4704, n4706);
    let n4709: ZW = zw_cellmix_n(408u64, n2692, 1542469173u64);
    let n4710: ZW = zw_cellmix_n(408u64, n2692, 668265263u64);
    let n4711: ZW = zw_add(n4707, n4709);
    let n4712: ZW = zw_add(n4708, n4710);
    let n4713: ZW = zw_add(n4695, n4515);
    let n4714: ZW = zw_add(n4696, n4516);
    let n4715: ZW = zw_add(n4713, n4519);
    let n4716: ZW = zw_add(n4714, n4520);
    let n4717: ZW = zw_add(n4715, n4563);
    let n4718: ZW = zw_add(n4716, n4564);
    let n4719: ZW = zw_add(n4717, n4383);
    let n4720: ZW = zw_add(n4718, n4384);
    let n4721: ZW = zw_cellmix_n(407u64, n2702, 1542469173u64);
    let n4722: ZW = zw_cellmix_n(407u64, n2702, 668265263u64);
    let n4723: ZW = zw_add(n4719, n4721);
    let n4724: ZW = zw_add(n4720, n4722);
    let n4725: ZW = zw_cellmix_n(408u64, n2700, 1542469173u64);
    let n4726: ZW = zw_cellmix_n(408u64, n2700, 668265263u64);
    let n4727: ZW = zw_add(n4723, n4725);
    let n4728: ZW = zw_add(n4724, n4726);
    let n4729: ZW = zw_add(n4713, n4535);
    let n4730: ZW = zw_add(n4714, n4536);
    let n4731: ZW = zw_add(n4729, n4563);
    let n4732: ZW = zw_add(n4730, n4564);
    let n4733: ZW = zw_add(n4731, n4395);
    let n4734: ZW = zw_add(n4732, n4396);
    let n4735: ZW = zw_cellmix_n(407u64, n2710, 1542469173u64);
    let n4736: ZW = zw_cellmix_n(407u64, n2710, 668265263u64);
    let n4737: ZW = zw_add(n4733, n4735);
    let n4738: ZW = zw_add(n4734, n4736);
    let n4739: ZW = zw_cellmix_n(408u64, n2708, 1542469173u64);
    let n4740: ZW = zw_cellmix_n(408u64, n2708, 668265263u64);
    let n4741: ZW = zw_add(n4737, n4739);
    let n4742: ZW = zw_add(n4738, n4740);
    let n4743: ZW = zw_add(n4699, n4607);
    let n4744: ZW = zw_add(n4700, n4608);
    let n4745: ZW = zw_add(n4743, n4371);
    let n4746: ZW = zw_add(n4744, n4372);
    let n4747: ZW = zw_add(n4745, n4705);
    let n4748: ZW = zw_add(n4746, n4706);
    let n4749: ZW = zw_cellmix_n(408u64, n2713, 1542469173u64);
    let n4750: ZW = zw_cellmix_n(408u64, n2713, 668265263u64);
    let n4751: ZW = zw_add(n4747, n4749);
    let n4752: ZW = zw_add(n4748, n4750);
    let n4753: ZW = zw_add(n4715, n4607);
    let n4754: ZW = zw_add(n4716, n4608);
    let n4755: ZW = zw_add(n4753, n4383);
    let n4756: ZW = zw_add(n4754, n4384);
    let n4757: ZW = zw_add(n4755, n4721);
    let n4758: ZW = zw_add(n4756, n4722);
    let n4759: ZW = zw_cellmix_n(408u64, n2716, 1542469173u64);
    let n4760: ZW = zw_cellmix_n(408u64, n2716, 668265263u64);
    let n4761: ZW = zw_add(n4757, n4759);
    let n4762: ZW = zw_add(n4758, n4760);
    let n4763: ZW = zw_add(n4729, n4607);
    let n4764: ZW = zw_add(n4730, n4608);
    let n4765: ZW = zw_add(n4763, n4395);
    let n4766: ZW = zw_add(n4764, n4396);
    let n4767: ZW = zw_add(n4765, n4735);
    let n4768: ZW = zw_add(n4766, n4736);
    let n4769: ZW = zw_cellmix_n(408u64, n2719, 1542469173u64);
    let n4770: ZW = zw_cellmix_n(408u64, n2719, 668265263u64);
    let n4771: ZW = zw_add(n4767, n4769);
    let n4772: ZW = zw_add(n4768, n4770);
    let ok_v0_b0: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b0: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b0: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v1_b1: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b1: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b1: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v2_b2: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b2: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b2: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v16_b3: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b3: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b3: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v17_b4: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b4: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b4: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v18_b5: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b5: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b5: u16 = ALL & zb_holds(n174) & zb_holds(n942);
    let ok_v32_b6: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b6: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b6: u16 = ALL & zb_holds(n942);
    let ok_v33_b7: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b7: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b7: u16 = ALL & zb_holds(n942);
    let ok_v34_b8: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b8: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b8: u16 = ALL & zb_holds(n942);
    let ok_v36_b9: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b9: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b9: u16 = ALL & zb_holds(n942);
    let ok_v37_b10: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b10: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b10: u16 = ALL & zb_holds(n942);
    let ok_v38_b11: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b11: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b11: u16 = ALL & zb_holds(n942);
    let ok_v40_b12: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b12: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b12: u16 = ALL & zb_holds(n942);
    let ok_v41_b13: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b13: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b13: u16 = ALL & zb_holds(n942);
    let ok_v42_b14: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b14: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b14: u16 = ALL & zb_holds(n942);
    let ok_v48_b15: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b15: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b15: u16 = ALL & zb_holds(n942);
    let ok_v49_b16: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b16: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b16: u16 = ALL & zb_holds(n942);
    let ok_v50_b17: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b17: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b17: u16 = ALL & zb_holds(n942);
    let ok_v52_b18: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b18: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b18: u16 = ALL & zb_holds(n942);
    let ok_v53_b19: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b19: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b19: u16 = ALL & zb_holds(n942);
    let ok_v54_b20: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b20: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b20: u16 = ALL & zb_holds(n942);
    let ok_v56_b21: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b21: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b21: u16 = ALL & zb_holds(n942);
    let ok_v57_b22: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b22: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b22: u16 = ALL & zb_holds(n942);
    let ok_v58_b23: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b23: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b23: u16 = ALL & zb_holds(n942);
    let ok_v0_b24: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v0_b24: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b24: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v1_b25: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v1_b25: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b25: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v2_b26: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v2_b26: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b26: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v16_b27: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v16_b27: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b27: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v17_b28: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v17_b28: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b28: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v18_b29: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v18_b29: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b29: u16 = ALL & zb_holds(n174) & zb_holds(n1237);
    let ok_v32_b30: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v32_b30: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b30: u16 = ALL & zb_holds(n1237);
    let ok_v33_b31: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v33_b31: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b31: u16 = ALL & zb_holds(n1237);
    let ok_v34_b32: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v34_b32: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b32: u16 = ALL & zb_holds(n1237);
    let ok_v36_b33: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v36_b33: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b33: u16 = ALL & zb_holds(n1237);
    let ok_v37_b34: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v37_b34: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b34: u16 = ALL & zb_holds(n1237);
    let ok_v38_b35: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v38_b35: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b35: u16 = ALL & zb_holds(n1237);
    let ok_v40_b36: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v40_b36: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b36: u16 = ALL & zb_holds(n1237);
    let ok_v41_b37: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v41_b37: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b37: u16 = ALL & zb_holds(n1237);
    let ok_v42_b38: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v42_b38: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b38: u16 = ALL & zb_holds(n1237);
    let ok_v48_b39: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v48_b39: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b39: u16 = ALL & zb_holds(n1237);
    let ok_v49_b40: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v49_b40: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b40: u16 = ALL & zb_holds(n1237);
    let ok_v50_b41: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v50_b41: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b41: u16 = ALL & zb_holds(n1237);
    let ok_v52_b42: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v52_b42: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b42: u16 = ALL & zb_holds(n1237);
    let ok_v53_b43: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v53_b43: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b43: u16 = ALL & zb_holds(n1237);
    let ok_v54_b44: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v54_b44: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b44: u16 = ALL & zb_holds(n1237);
    let ok_v56_b45: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v56_b45: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b45: u16 = ALL & zb_holds(n1237);
    let ok_v57_b46: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v57_b46: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b46: u16 = ALL & zb_holds(n1237);
    let ok_v58_b47: u16 = ALL & zb_holds(n745) & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112);
    let bd_v58_b47: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b47: u16 = ALL & zb_holds(n1237);
    let ok_v0_b48: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1305);
    let bd_v0_b48: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b48: u16 = ALL & zb_holds(n1303);
    let ok_v32_b49: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1305);
    let bd_v32_b49: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b49: u16 = ALL & zb_holds(n1303);
    let ok_v0_b50: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1371);
    let bd_v0_b50: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b50: u16 = ALL & zb_holds(n1369);
    let ok_v32_b51: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1371);
    let bd_v32_b51: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b51: u16 = ALL & zb_holds(n1369);
    let ok_v0_b52: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v0_b52: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b52: u16 = ALL & zb_holds(n2047) & zb_holds(n174) & zb_holds(n1957) & zb_holds(n1959);
    let ok_v1_b53: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v1_b53: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b53: u16 = ALL & zb_holds(n2047) & zb_holds(n174) & zb_holds(n1957) & zb_holds(n1959);
    let ok_v2_b54: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v2_b54: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b54: u16 = ALL & zb_holds(n2047) & zb_holds(n174) & zb_holds(n1957) & zb_holds(n1959);
    let ok_v16_b55: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v16_b55: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b55: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n174) & zb_holds(n2047);
    let ok_v17_b56: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v17_b56: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b56: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n174) & zb_holds(n2047);
    let ok_v18_b57: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v18_b57: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b57: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n174) & zb_holds(n2047);
    let ok_v32_b58: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v32_b58: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b58: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v33_b59: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v33_b59: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b59: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v34_b60: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v34_b60: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b60: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v36_b61: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v36_b61: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b61: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v37_b62: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v37_b62: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b62: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v38_b63: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v38_b63: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b63: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v40_b64: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v40_b64: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b64: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v41_b65: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v41_b65: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b65: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v42_b66: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v42_b66: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b66: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v48_b67: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v48_b67: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b67: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v49_b68: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v49_b68: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b68: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v50_b69: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v50_b69: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b69: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v52_b70: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v52_b70: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b70: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v53_b71: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v53_b71: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b71: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v54_b72: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v54_b72: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b72: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v56_b73: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v56_b73: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b73: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v57_b74: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v57_b74: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b74: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v58_b75: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n1958);
    let bd_v58_b75: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b75: u16 = ALL & zb_holds(n1957) & zb_holds(n1959) & zb_holds(n2047);
    let ok_v0_b76: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2299);
    let bd_v0_b76: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b76: u16 = ALL & zb_holds(n2047) & zb_holds(n2298);
    let ok_v32_b77: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2299);
    let bd_v32_b77: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b77: u16 = ALL & zb_holds(n2047) & zb_holds(n2298);
    let ok_v0_b78: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2468);
    let bd_v0_b78: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b78: u16 = ALL & zb_holds(n2047) & zb_holds(n2467);
    let ok_v32_b79: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2468);
    let bd_v32_b79: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b79: u16 = ALL & zb_holds(n2047) & zb_holds(n2467);
    let ok_v0_b80: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2489);
    let bd_v0_b80: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b80: u16 = ALL & zb_holds(n2488);
    let ok_v32_b81: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2489);
    let bd_v32_b81: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b81: u16 = ALL & zb_holds(n2488);
    let ok_v0_b82: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2495);
    let bd_v0_b82: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b82: u16 = ALL & zb_holds(n2494);
    let ok_v32_b83: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2495);
    let bd_v32_b83: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b83: u16 = ALL & zb_holds(n2494);
    let ok_v0_b84: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v0_b84: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v0_b84: u16 = ALL & zb_holds(n2519);
    let ok_v1_b85: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v1_b85: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v1_b85: u16 = ALL & zb_holds(n2519);
    let ok_v2_b86: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v2_b86: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v2_b86: u16 = ALL & zb_holds(n2519);
    let ok_v16_b87: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v16_b87: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v16_b87: u16 = ALL & zb_holds(n2519);
    let ok_v17_b88: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v17_b88: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v17_b88: u16 = ALL & zb_holds(n2519);
    let ok_v18_b89: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v18_b89: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v18_b89: u16 = ALL & zb_holds(n2519);
    let ok_v32_b90: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v32_b90: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v32_b90: u16 = ALL & zb_holds(n2519);
    let ok_v33_b91: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v33_b91: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v33_b91: u16 = ALL & zb_holds(n2519);
    let ok_v34_b92: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v34_b92: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v34_b92: u16 = ALL & zb_holds(n2519);
    let ok_v36_b93: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v36_b93: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v36_b93: u16 = ALL & zb_holds(n2519);
    let ok_v37_b94: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v37_b94: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v37_b94: u16 = ALL & zb_holds(n2519);
    let ok_v38_b95: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v38_b95: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v38_b95: u16 = ALL & zb_holds(n2519);
    let ok_v40_b96: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v40_b96: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v40_b96: u16 = ALL & zb_holds(n2519);
    let ok_v41_b97: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v41_b97: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v41_b97: u16 = ALL & zb_holds(n2519);
    let ok_v42_b98: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v42_b98: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v42_b98: u16 = ALL & zb_holds(n2519);
    let ok_v48_b99: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v48_b99: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v48_b99: u16 = ALL & zb_holds(n2519);
    let ok_v49_b100: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v49_b100: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v49_b100: u16 = ALL & zb_holds(n2519);
    let ok_v50_b101: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v50_b101: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v50_b101: u16 = ALL & zb_holds(n2519);
    let ok_v52_b102: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v52_b102: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v52_b102: u16 = ALL & zb_holds(n2519);
    let ok_v53_b103: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v53_b103: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v53_b103: u16 = ALL & zb_holds(n2519);
    let ok_v54_b104: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v54_b104: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v54_b104: u16 = ALL & zb_holds(n2519);
    let ok_v56_b105: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v56_b105: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v56_b105: u16 = ALL & zb_holds(n2519);
    let ok_v57_b106: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v57_b106: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v57_b106: u16 = ALL & zb_holds(n2519);
    let ok_v58_b107: u16 = ALL & zb_holds(n255) & zb_holds(n254) & zb_holds(r_c313) & zb_holds(n249) & zb_holds(r_c296) & zb_holds(n224) & zb_holds(n223) & zb_holds(n137) & zb_holds(n172) & zb_holds(r_c288) & zb_holds(n136) & zb_holds(n135) & zb_holds(n132) & zb_holds(n169) & zb_holds(n131) & zb_holds(n168) & zb_holds(r_c276) & zb_holds(n167) & zb_holds(n166) & zb_holds(n165) & zb_holds(n164) & zb_holds(n163) & zb_holds(r_c268) & zb_holds(n162) & zb_holds(n130) & zb_holds(n160) & zb_holds(n159) & zb_holds(n126) & zb_holds(n158) & zb_holds(r_c257) & zb_holds(n125) & zb_holds(n124) & zb_holds(n123) & zb_holds(n122) & zb_holds(n121) & zb_holds(r_c248) & zb_holds(n120) & zb_holds(n119) & zb_holds(n114) & zb_holds(n113) & zb_holds(r_c238) & zb_holds(n111) & zb_holds(n112) & zb_holds(n2520);
    let bd_v58_b107: bool = !n253 || !n252 || !n251 || !n250 || !n134 || !n171 || !n133 || !n170 || !n129 || !n128 || !n161 || !n127 || !n118 || !n117 || !n116 || !n115;
    let live_v58_b107: u16 = ALL & zb_holds(n2519);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n142,
        c86: n260,
        c261: n846,
        c274: n847,
        c368: n393,
        c369: n505,
        c302: n504,
        c85: n261,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n142,
        c86: n260,
        c246: n1142,
        c254: n1147,
        c278: n846,
        c291: n847,
        c406: n393,
        c407: n505,
        c319: n504,
        c85: n261,
    };
    let sh2 = KShared2 {
        c87: n1239,
        c84: n142,
        c86: n260,
        c260: n846,
        c273: n847,
        c85: n261,
    };
    let sh3 = KShared3 {
        c87: n1239,
        c84: n142,
        c86: n260,
        c245: n1142,
        c253: n1147,
        c277: n846,
        c290: n847,
        c85: n261,
    };
    let sh4 = KShared4 {
        c87: r_c87,
        c39: r_c39,
        c84: n142,
        c86: n260,
        c260: n1391,
        c273: n1392,
        c367: n1627,
        c368: n1628,
        c301: n1626,
        c85: n261,
    };
    let sh5 = KShared5 {
        c87: n2303,
        c84: n142,
        c86: n260,
        c259: n1391,
        c272: n1392,
        c85: n261,
    };
    let sh6 = KShared6 {
        c87: n2475,
        c84: n142,
        c86: n260,
        c245: n1142,
        c253: n1147,
        c276: n1391,
        c289: n1392,
        c85: n261,
    };
    let sh7 = KShared7 {
        c87: n2490,
        c39: n2491,
        c84: n142,
        c86: n260,
        c85: n261,
        c38: n2487,
    };
    let sh8 = KShared8 {
        c87: n2496,
        c39: n2497,
        c84: n142,
        c86: n260,
        c85: n261,
        c38: n2493,
    };
    let sh9 = KShared9 {
        c87: r_c87,
        c39: r_c39,
        c84: n142,
        c86: n260,
        c246: n2500,
        c254: n2501,
        c277: n2506,
        c290: n2507,
        c405: n2515,
        c406: n2516,
        c318: n2513,
        c85: n261,
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
    let mut take_3_0: u16 = 0;
    let mut take_3_1: u16 = 0;
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
    let mut take_4_14: u16 = 0;
    let mut take_4_15: u16 = 0;
    let mut take_4_16: u16 = 0;
    let mut take_4_17: u16 = 0;
    let mut take_4_18: u16 = 0;
    let mut take_4_19: u16 = 0;
    let mut take_4_20: u16 = 0;
    let mut take_4_21: u16 = 0;
    let mut take_4_22: u16 = 0;
    let mut take_4_23: u16 = 0;
    let mut take_5_0: u16 = 0;
    let mut take_5_1: u16 = 0;
    let mut take_6_0: u16 = 0;
    let mut take_6_1: u16 = 0;
    let mut take_7_0: u16 = 0;
    let mut take_7_1: u16 = 0;
    let mut take_8_0: u16 = 0;
    let mut take_8_1: u16 = 0;
    let mut take_9_0: u16 = 0;
    let mut take_9_1: u16 = 0;
    let mut take_9_2: u16 = 0;
    let mut take_9_3: u16 = 0;
    let mut take_9_4: u16 = 0;
    let mut take_9_5: u16 = 0;
    let mut take_9_6: u16 = 0;
    let mut take_9_7: u16 = 0;
    let mut take_9_8: u16 = 0;
    let mut take_9_9: u16 = 0;
    let mut take_9_10: u16 = 0;
    let mut take_9_11: u16 = 0;
    let mut take_9_12: u16 = 0;
    let mut take_9_13: u16 = 0;
    let mut take_9_14: u16 = 0;
    let mut take_9_15: u16 = 0;
    let mut take_9_16: u16 = 0;
    let mut take_9_17: u16 = 0;
    let mut take_9_18: u16 = 0;
    let mut take_9_19: u16 = 0;
    let mut take_9_20: u16 = 0;
    let mut take_9_21: u16 = 0;
    let mut take_9_22: u16 = 0;
    let mut take_9_23: u16 = 0;
    // 108 distinct button assignments; per outcome they fall
    // into [24, 24, 2, 2, 24, 2, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n831,
        c287: n753,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n843,
        c371: n833,
        c301: n842,
        h1: n2823, h2: n2824,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n973,
        c287: n753,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n977,
        c371: n975,
        c301: n842,
        h1: n2835, h2: n2836,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n995,
        c287: n753,
        c294: zb_splat(false),
        c295: zb_splat(false),
        c370: n999,
        c371: n997,
        c301: n842,
        h1: n2847, h2: n2848,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n831,
        c287: n1003,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1007,
        c371: n1005,
        c301: n842,
        h1: n2877, h2: n2878,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n973,
        c287: n1003,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1013,
        c371: n1011,
        c301: n842,
        h1: n2887, h2: n2888,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c358: r_c395,
        c359: r_c396,
        c282: n754,
        c360: r_c397,
        c361: r_c398,
        c284: n830,
        c285: zn_splat(P8::from_raw(65536i32)),
        c362: n995,
        c287: n1003,
        c294: zb_splat(false),
        c295: zb_splat(true),
        c370: n1019,
        c371: n1017,
        c301: n842,
        h1: n2897, h2: n2898,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1037,
        c282: n1033,
        c360: n1038,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1045,
        c371: n1041,
        c301: n1044,
        h1: n2955, h2: n2956,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1055,
        c371: n1053,
        c301: n1044,
        h1: n2975, h2: n2976,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1063,
        c371: n1061,
        c301: n1044,
        h1: n2991, h2: n2992,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1072,
        c282: n1033,
        c360: n1073,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1078,
        c371: n1076,
        c301: n1044,
        h1: n3017, h2: n3018,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1084,
        c371: n1082,
        c301: n1044,
        h1: n3033, h2: n3034,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1090,
        c371: n1088,
        c301: n1044,
        h1: n3047, h2: n3048,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1072,
        c282: n1033,
        c360: n1073,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1078,
        c371: n1094,
        c301: n1044,
        h1: n3059, h2: n3060,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1084,
        c371: n1096,
        c301: n1044,
        h1: n3069, h2: n3070,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n753,
        c294: zb_splat(true),
        c295: zb_splat(false),
        c370: n1090,
        c371: n1098,
        c301: n1044,
        h1: n3079, h2: n3080,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1037,
        c282: n1033,
        c360: n1038,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1104,
        c371: n1102,
        c301: n1044,
        h1: n3105, h2: n3106,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1110,
        c371: n1108,
        c301: n1044,
        h1: n3121, h2: n3122,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1036,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1039,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1116,
        c371: n1114,
        c301: n1044,
        h1: n3135, h2: n3136,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1072,
        c282: n1033,
        c360: n1073,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1122,
        c371: n1120,
        c301: n1044,
        h1: n3153, h2: n3154,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1128,
        c371: n1126,
        c301: n1044,
        h1: n3169, h2: n3170,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1074,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1134,
        c371: n1132,
        c301: n1044,
        h1: n3183, h2: n3184,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1072,
        c282: n1033,
        c360: n1073,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n831,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1122,
        c371: n1136,
        c301: n1044,
        h1: n3193, h2: n3194,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1051,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n973,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1128,
        c371: n1138,
        c301: n1044,
        h1: n3203, h2: n3204,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1031,
        c41: n1032,
        c358: n1071,
        c359: n1050,
        c282: n1033,
        c360: n1059,
        c361: n1093,
        c284: n1034,
        c285: n1035,
        c362: n995,
        c287: n1003,
        c294: zb_splat(true),
        c295: zb_splat(true),
        c370: n1134,
        c371: n1140,
        c301: n1044,
        h1: n3213, h2: n3214,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n831,
        c304: n753,
        c311: zb_splat(false),
        c312: zb_splat(false),
        c408: n843,
        c409: n833,
        c318: n842,
        h1: n3301, h2: n3302,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_1 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n973,
        c304: n753,
        c311: zb_splat(false),
        c312: zb_splat(false),
        c408: n977,
        c409: n975,
        c318: n842,
        h1: n3313, h2: n3314,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_2 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n995,
        c304: n753,
        c311: zb_splat(false),
        c312: zb_splat(false),
        c408: n999,
        c409: n997,
        c318: n842,
        h1: n3325, h2: n3326,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_3 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n831,
        c304: n1003,
        c311: zb_splat(false),
        c312: zb_splat(true),
        c408: n1007,
        c409: n1005,
        c318: n842,
        h1: n3355, h2: n3356,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_4 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n973,
        c304: n1003,
        c311: zb_splat(false),
        c312: zb_splat(true),
        c408: n1013,
        c409: n1011,
        c318: n842,
        h1: n3365, h2: n3366,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_5 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c396: r_c395,
        c397: r_c396,
        c299: n754,
        c398: r_c397,
        c399: r_c398,
        c301: n830,
        c302: zn_splat(P8::from_raw(65536i32)),
        c400: n995,
        c304: n1003,
        c311: zb_splat(false),
        c312: zb_splat(true),
        c408: n1019,
        c409: n1017,
        c318: n842,
        h1: n3375, h2: n3376,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1037,
        c299: n1033,
        c398: n1038,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1045,
        c409: n1041,
        c318: n1044,
        h1: n3429, h2: n3430,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1055,
        c409: n1053,
        c318: n1044,
        h1: n3449, h2: n3450,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1063,
        c409: n1061,
        c318: n1044,
        h1: n3465, h2: n3466,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1072,
        c299: n1033,
        c398: n1073,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1078,
        c409: n1076,
        c318: n1044,
        h1: n3491, h2: n3492,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1084,
        c409: n1082,
        c318: n1044,
        h1: n3507, h2: n3508,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1090,
        c409: n1088,
        c318: n1044,
        h1: n3521, h2: n3522,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1072,
        c299: n1033,
        c398: n1073,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1078,
        c409: n1094,
        c318: n1044,
        h1: n3533, h2: n3534,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1084,
        c409: n1096,
        c318: n1044,
        h1: n3543, h2: n3544,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n753,
        c311: zb_splat(true),
        c312: zb_splat(false),
        c408: n1090,
        c409: n1098,
        c318: n1044,
        h1: n3553, h2: n3554,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1037,
        c299: n1033,
        c398: n1038,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1104,
        c409: n1102,
        c318: n1044,
        h1: n3579, h2: n3580,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1110,
        c409: n1108,
        c318: n1044,
        h1: n3595, h2: n3596,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1036,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1039,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1116,
        c409: n1114,
        c318: n1044,
        h1: n3609, h2: n3610,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1072,
        c299: n1033,
        c398: n1073,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1122,
        c409: n1120,
        c318: n1044,
        h1: n3627, h2: n3628,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1128,
        c409: n1126,
        c318: n1044,
        h1: n3643, h2: n3644,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1074,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1134,
        c409: n1132,
        c318: n1044,
        h1: n3657, h2: n3658,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1072,
        c299: n1033,
        c398: n1073,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n831,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1122,
        c409: n1136,
        c318: n1044,
        h1: n3667, h2: n3668,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1051,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n973,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1128,
        c409: n1138,
        c318: n1044,
        h1: n3677, h2: n3678,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1031,
        c41: n1032,
        c396: n1071,
        c397: n1050,
        c299: n1033,
        c398: n1059,
        c399: n1093,
        c301: n1034,
        c302: n1035,
        c400: n995,
        c304: n1003,
        c311: zb_splat(true),
        c312: zb_splat(true),
        c408: n1134,
        c409: n1140,
        c318: n1044,
        h1: n3687, h2: n3688,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n3709, h2: n3710,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b49 & (if bd_v32_b49 { ALL } else { !ok_v32_b49 });
    take_2_1 |= live_v32_b49 & ok_v32_b49 & (if bd_v32_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1031,
        c41: n1032,
        h1: n3713, h2: n3714,
    };
    // body 49: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b50 & (if bd_v0_b50 { ALL } else { !ok_v0_b50 });
    take_3_0 |= live_v0_b50 & ok_v0_b50 & (if bd_v0_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        h1: n3733, h2: n3734,
    };
    // body 50: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v32_b51 & (if bd_v32_b51 { ALL } else { !ok_v32_b51 });
    take_3_1 |= live_v32_b51 & ok_v32_b51 & (if bd_v32_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1031,
        c41: n1032,
        h1: n3737, h2: n3738,
    };
    // body 51: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_4_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2043,
        c286: n1965,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n2060,
        c370: n2045,
        c300: n2059,
        h1: n3815, h2: n3816,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v1_b53 & (if bd_v1_b53 { ALL } else { !ok_v1_b53 });
    take_4_1 |= live_v1_b53 & ok_v1_b53 & (if bd_v1_b53 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2082,
        c286: n1965,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n2086,
        c370: n2084,
        c300: n2059,
        h1: n3827, h2: n3828,
    };
    // body 53: buttons 0x01, forks 0x0
    sink.o4(1, take_4_1, &sh4, &o4);
    declined |= live_v2_b54 & (if bd_v2_b54 { ALL } else { !ok_v2_b54 });
    take_4_2 |= live_v2_b54 & ok_v2_b54 & (if bd_v2_b54 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2107,
        c286: n1965,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n2111,
        c370: n2109,
        c300: n2059,
        h1: n3839, h2: n3840,
    };
    // body 54: buttons 0x02, forks 0x0
    sink.o4(2, take_4_2, &sh4, &o4);
    declined |= live_v16_b55 & (if bd_v16_b55 { ALL } else { !ok_v16_b55 });
    take_4_3 |= live_v16_b55 & ok_v16_b55 & (if bd_v16_b55 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2043,
        c286: n2115,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n2121,
        c370: n2117,
        c300: n2059,
        h1: n3867, h2: n3868,
    };
    // body 55: buttons 0x10, forks 0x0
    sink.o4(16, take_4_3, &sh4, &o4);
    declined |= live_v17_b56 & (if bd_v17_b56 { ALL } else { !ok_v17_b56 });
    take_4_4 |= live_v17_b56 & ok_v17_b56 & (if bd_v17_b56 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2082,
        c286: n2115,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n2127,
        c370: n2125,
        c300: n2059,
        h1: n3877, h2: n3878,
    };
    // body 56: buttons 0x11, forks 0x0
    sink.o4(17, take_4_4, &sh4, &o4);
    declined |= live_v18_b57 & (if bd_v18_b57 { ALL } else { !ok_v18_b57 });
    take_4_5 |= live_v18_b57 & ok_v18_b57 & (if bd_v18_b57 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c395,
        c358: r_c396,
        c281: n754,
        c359: r_c397,
        c360: r_c398,
        c283: n830,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n2107,
        c286: n2115,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n2133,
        c370: n2131,
        c300: n2059,
        h1: n3887, h2: n3888,
    };
    // body 57: buttons 0x12, forks 0x0
    sink.o4(18, take_4_5, &sh4, &o4);
    declined |= live_v32_b58 & (if bd_v32_b58 { ALL } else { !ok_v32_b58 });
    take_4_6 |= live_v32_b58 & ok_v32_b58 & (if bd_v32_b58 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n2138,
        c281: n1033,
        c359: n2139,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2144,
        c370: n2141,
        c300: n2143,
        h1: n3941, h2: n3942,
    };
    // body 58: buttons 0x20, forks 0x0
    sink.o4(32, take_4_6, &sh4, &o4);
    declined |= live_v33_b59 & (if bd_v33_b59 { ALL } else { !ok_v33_b59 });
    take_4_7 |= live_v33_b59 & ok_v33_b59 & (if bd_v33_b59 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2150,
        c370: n2148,
        c300: n2143,
        h1: n3961, h2: n3962,
    };
    // body 59: buttons 0x21, forks 0x0
    sink.o4(33, take_4_7, &sh4, &o4);
    declined |= live_v34_b60 & (if bd_v34_b60 { ALL } else { !ok_v34_b60 });
    take_4_8 |= live_v34_b60 & ok_v34_b60 & (if bd_v34_b60 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2156,
        c370: n2154,
        c300: n2143,
        h1: n3977, h2: n3978,
    };
    // body 60: buttons 0x22, forks 0x0
    sink.o4(34, take_4_8, &sh4, &o4);
    declined |= live_v36_b61 & (if bd_v36_b61 { ALL } else { !ok_v36_b61 });
    take_4_9 |= live_v36_b61 & ok_v36_b61 & (if bd_v36_b61 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1072,
        c281: n1033,
        c359: n1073,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2162,
        c370: n2160,
        c300: n2143,
        h1: n4003, h2: n4004,
    };
    // body 61: buttons 0x24, forks 0x0
    sink.o4(36, take_4_9, &sh4, &o4);
    declined |= live_v37_b62 & (if bd_v37_b62 { ALL } else { !ok_v37_b62 });
    take_4_10 |= live_v37_b62 & ok_v37_b62 & (if bd_v37_b62 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2168,
        c370: n2166,
        c300: n2143,
        h1: n4019, h2: n4020,
    };
    // body 62: buttons 0x25, forks 0x0
    sink.o4(37, take_4_10, &sh4, &o4);
    declined |= live_v38_b63 & (if bd_v38_b63 { ALL } else { !ok_v38_b63 });
    take_4_11 |= live_v38_b63 & ok_v38_b63 & (if bd_v38_b63 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2174,
        c370: n2172,
        c300: n2143,
        h1: n4033, h2: n4034,
    };
    // body 63: buttons 0x26, forks 0x0
    sink.o4(38, take_4_11, &sh4, &o4);
    declined |= live_v40_b64 & (if bd_v40_b64 { ALL } else { !ok_v40_b64 });
    take_4_12 |= live_v40_b64 & ok_v40_b64 & (if bd_v40_b64 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1072,
        c281: n1033,
        c359: n1073,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2162,
        c370: n2176,
        c300: n2143,
        h1: n4045, h2: n4046,
    };
    // body 64: buttons 0x28, forks 0x0
    sink.o4(40, take_4_12, &sh4, &o4);
    declined |= live_v41_b65 & (if bd_v41_b65 { ALL } else { !ok_v41_b65 });
    take_4_13 |= live_v41_b65 & ok_v41_b65 & (if bd_v41_b65 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2168,
        c370: n2178,
        c300: n2143,
        h1: n4055, h2: n4056,
    };
    // body 65: buttons 0x29, forks 0x0
    sink.o4(41, take_4_13, &sh4, &o4);
    declined |= live_v42_b66 & (if bd_v42_b66 { ALL } else { !ok_v42_b66 });
    take_4_14 |= live_v42_b66 & ok_v42_b66 & (if bd_v42_b66 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n1965,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n2174,
        c370: n2180,
        c300: n2143,
        h1: n4065, h2: n4066,
    };
    // body 66: buttons 0x2a, forks 0x0
    sink.o4(42, take_4_14, &sh4, &o4);
    declined |= live_v48_b67 & (if bd_v48_b67 { ALL } else { !ok_v48_b67 });
    take_4_15 |= live_v48_b67 & ok_v48_b67 & (if bd_v48_b67 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n2138,
        c281: n1033,
        c359: n2139,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2186,
        c370: n2184,
        c300: n2143,
        h1: n4091, h2: n4092,
    };
    // body 67: buttons 0x30, forks 0x0
    sink.o4(48, take_4_15, &sh4, &o4);
    declined |= live_v49_b68 & (if bd_v49_b68 { ALL } else { !ok_v49_b68 });
    take_4_16 |= live_v49_b68 & ok_v49_b68 & (if bd_v49_b68 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2192,
        c370: n2190,
        c300: n2143,
        h1: n4107, h2: n4108,
    };
    // body 68: buttons 0x31, forks 0x0
    sink.o4(49, take_4_16, &sh4, &o4);
    declined |= live_v50_b69 & (if bd_v50_b69 { ALL } else { !ok_v50_b69 });
    take_4_17 |= live_v50_b69 & ok_v50_b69 & (if bd_v50_b69 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1036,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1039,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2198,
        c370: n2196,
        c300: n2143,
        h1: n4121, h2: n4122,
    };
    // body 69: buttons 0x32, forks 0x0
    sink.o4(50, take_4_17, &sh4, &o4);
    declined |= live_v52_b70 & (if bd_v52_b70 { ALL } else { !ok_v52_b70 });
    take_4_18 |= live_v52_b70 & ok_v52_b70 & (if bd_v52_b70 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1072,
        c281: n1033,
        c359: n1073,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2204,
        c370: n2202,
        c300: n2143,
        h1: n4139, h2: n4140,
    };
    // body 70: buttons 0x34, forks 0x0
    sink.o4(52, take_4_18, &sh4, &o4);
    declined |= live_v53_b71 & (if bd_v53_b71 { ALL } else { !ok_v53_b71 });
    take_4_19 |= live_v53_b71 & ok_v53_b71 & (if bd_v53_b71 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2210,
        c370: n2208,
        c300: n2143,
        h1: n4155, h2: n4156,
    };
    // body 71: buttons 0x35, forks 0x0
    sink.o4(53, take_4_19, &sh4, &o4);
    declined |= live_v54_b72 & (if bd_v54_b72 { ALL } else { !ok_v54_b72 });
    take_4_20 |= live_v54_b72 & ok_v54_b72 & (if bd_v54_b72 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1074,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2216,
        c370: n2214,
        c300: n2143,
        h1: n4169, h2: n4170,
    };
    // body 72: buttons 0x36, forks 0x0
    sink.o4(54, take_4_20, &sh4, &o4);
    declined |= live_v56_b73 & (if bd_v56_b73 { ALL } else { !ok_v56_b73 });
    take_4_21 |= live_v56_b73 & ok_v56_b73 & (if bd_v56_b73 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1072,
        c281: n1033,
        c359: n1073,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2043,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2204,
        c370: n2218,
        c300: n2143,
        h1: n4179, h2: n4180,
    };
    // body 73: buttons 0x38, forks 0x0
    sink.o4(56, take_4_21, &sh4, &o4);
    declined |= live_v57_b74 & (if bd_v57_b74 { ALL } else { !ok_v57_b74 });
    take_4_22 |= live_v57_b74 & ok_v57_b74 & (if bd_v57_b74 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1051,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2082,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2210,
        c370: n2220,
        c300: n2143,
        h1: n4189, h2: n4190,
    };
    // body 74: buttons 0x39, forks 0x0
    sink.o4(57, take_4_22, &sh4, &o4);
    declined |= live_v58_b75 & (if bd_v58_b75 { ALL } else { !ok_v58_b75 });
    take_4_23 |= live_v58_b75 & ok_v58_b75 & (if bd_v58_b75 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n1031,
        c41: n1032,
        c357: n1071,
        c358: n1050,
        c281: n1033,
        c359: n1059,
        c360: n1093,
        c283: n1034,
        c284: n1035,
        c361: n2107,
        c286: n2115,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2216,
        c370: n2222,
        c300: n2143,
        h1: n4199, h2: n4200,
    };
    // body 75: buttons 0x3a, forks 0x0
    sink.o4(58, take_4_23, &sh4, &o4);
    declined |= live_v0_b76 & (if bd_v0_b76 { ALL } else { !ok_v0_b76 });
    take_5_0 |= live_v0_b76 & ok_v0_b76 & (if bd_v0_b76 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: r_c20,
        c41: r_c41,
        h1: n4215, h2: n4216,
    };
    // body 76: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v32_b77 & (if bd_v32_b77 { ALL } else { !ok_v32_b77 });
    take_5_1 |= live_v32_b77 & ok_v32_b77 & (if bd_v32_b77 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n1031,
        c41: n1032,
        h1: n4219, h2: n4220,
    };
    // body 77: buttons 0x20, forks 0x0
    sink.o5(32, take_5_1, &sh5, &o5);
    declined |= live_v0_b78 & (if bd_v0_b78 { ALL } else { !ok_v0_b78 });
    take_6_0 |= live_v0_b78 & ok_v0_b78 & (if bd_v0_b78 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: r_c20,
        c41: r_c41,
        h1: n4239, h2: n4240,
    };
    // body 78: buttons 0x00, forks 0x0
    sink.o6(0, take_6_0, &sh6, &o6);
    declined |= live_v32_b79 & (if bd_v32_b79 { ALL } else { !ok_v32_b79 });
    take_6_1 |= live_v32_b79 & ok_v32_b79 & (if bd_v32_b79 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2480,
        c41: n2481,
        h1: n4247, h2: n4248,
    };
    // body 79: buttons 0x20, forks 0x0
    sink.o6(32, take_6_1, &sh6, &o6);
    declined |= live_v0_b80 & (if bd_v0_b80 { ALL } else { !ok_v0_b80 });
    take_7_0 |= live_v0_b80 & ok_v0_b80 & (if bd_v0_b80 { 0 } else { ALL });
    let o7 = KOut7 {
        c20: r_c20,
        h1: n4267, h2: n4268,
    };
    // body 80: buttons 0x00, forks 0x0
    sink.o7(0, take_7_0, &sh7, &o7);
    declined |= live_v32_b81 & (if bd_v32_b81 { ALL } else { !ok_v32_b81 });
    take_7_1 |= live_v32_b81 & ok_v32_b81 & (if bd_v32_b81 { 0 } else { ALL });
    let o7 = KOut7 {
        c20: n2480,
        h1: n4269, h2: n4270,
    };
    // body 81: buttons 0x20, forks 0x0
    sink.o7(32, take_7_1, &sh7, &o7);
    declined |= live_v0_b82 & (if bd_v0_b82 { ALL } else { !ok_v0_b82 });
    take_8_0 |= live_v0_b82 & ok_v0_b82 & (if bd_v0_b82 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: r_c20,
        h1: n4289, h2: n4290,
    };
    // body 82: buttons 0x00, forks 0x0
    sink.o8(0, take_8_0, &sh8, &o8);
    declined |= live_v32_b83 & (if bd_v32_b83 { ALL } else { !ok_v32_b83 });
    take_8_1 |= live_v32_b83 & ok_v32_b83 & (if bd_v32_b83 { 0 } else { ALL });
    let o8 = KOut8 {
        c20: n1031,
        h1: n4291, h2: n4292,
    };
    // body 83: buttons 0x20, forks 0x0
    sink.o8(32, take_8_1, &sh8, &o8);
    declined |= live_v0_b84 & (if bd_v0_b84 { ALL } else { !ok_v0_b84 });
    take_9_0 |= live_v0_b84 & ok_v0_b84 & (if bd_v0_b84 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2514,
        c303: n2511,
        c310: n2502,
        c311: n2503,
        c407: n2530,
        c408: n2518,
        c317: n2529,
        h1: n4381, h2: n4382,
    };
    // body 84: buttons 0x00, forks 0x0
    sink.o9(0, take_9_0, &sh9, &o9);
    declined |= live_v1_b85 & (if bd_v1_b85 { ALL } else { !ok_v1_b85 });
    take_9_1 |= live_v1_b85 & ok_v1_b85 & (if bd_v1_b85 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2532,
        c303: n2511,
        c310: n2502,
        c311: n2503,
        c407: n2536,
        c408: n2534,
        c317: n2529,
        h1: n4393, h2: n4394,
    };
    // body 85: buttons 0x01, forks 0x0
    sink.o9(1, take_9_1, &sh9, &o9);
    declined |= live_v2_b86 & (if bd_v2_b86 { ALL } else { !ok_v2_b86 });
    take_9_2 |= live_v2_b86 & ok_v2_b86 & (if bd_v2_b86 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2537,
        c303: n2511,
        c310: n2502,
        c311: n2503,
        c407: n2541,
        c408: n2539,
        c317: n2529,
        h1: n4405, h2: n4406,
    };
    // body 86: buttons 0x02, forks 0x0
    sink.o9(2, take_9_2, &sh9, &o9);
    declined |= live_v16_b87 & (if bd_v16_b87 { ALL } else { !ok_v16_b87 });
    take_9_3 |= live_v16_b87 & ok_v16_b87 & (if bd_v16_b87 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2514,
        c303: n2543,
        c310: n2502,
        c311: n2542,
        c407: n2547,
        c408: n2545,
        c317: n2529,
        h1: n4435, h2: n4436,
    };
    // body 87: buttons 0x10, forks 0x0
    sink.o9(16, take_9_3, &sh9, &o9);
    declined |= live_v17_b88 & (if bd_v17_b88 { ALL } else { !ok_v17_b88 });
    take_9_4 |= live_v17_b88 & ok_v17_b88 & (if bd_v17_b88 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2532,
        c303: n2543,
        c310: n2502,
        c311: n2542,
        c407: n2551,
        c408: n2549,
        c317: n2529,
        h1: n4445, h2: n4446,
    };
    // body 88: buttons 0x11, forks 0x0
    sink.o9(17, take_9_4, &sh9, &o9);
    declined |= live_v18_b89 & (if bd_v18_b89 { ALL } else { !ok_v18_b89 });
    take_9_5 |= live_v18_b89 & ok_v18_b89 & (if bd_v18_b89 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2505,
        c41: r_c41,
        c395: r_c395,
        c396: r_c396,
        c298: n2508,
        c397: r_c397,
        c398: r_c398,
        c300: n2509,
        c301: n2510,
        c399: n2537,
        c303: n2543,
        c310: n2502,
        c311: n2542,
        c407: n2555,
        c408: n2553,
        c317: n2529,
        h1: n4455, h2: n4456,
    };
    // body 89: buttons 0x12, forks 0x0
    sink.o9(18, take_9_5, &sh9, &o9);
    declined |= live_v32_b90 & (if bd_v32_b90 { ALL } else { !ok_v32_b90 });
    take_9_6 |= live_v32_b90 & ok_v32_b90 & (if bd_v32_b90 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2581,
        c298: n2577,
        c397: n2582,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2589,
        c408: n2585,
        c317: n2588,
        h1: n4513, h2: n4514,
    };
    // body 90: buttons 0x20, forks 0x0
    sink.o9(32, take_9_6, &sh9, &o9);
    declined |= live_v33_b91 & (if bd_v33_b91 { ALL } else { !ok_v33_b91 });
    take_9_7 |= live_v33_b91 & ok_v33_b91 & (if bd_v33_b91 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2603,
        c408: n2601,
        c317: n2588,
        h1: n4533, h2: n4534,
    };
    // body 91: buttons 0x21, forks 0x0
    sink.o9(33, take_9_7, &sh9, &o9);
    declined |= live_v34_b92 & (if bd_v34_b92 { ALL } else { !ok_v34_b92 });
    take_9_8 |= live_v34_b92 & ok_v34_b92 & (if bd_v34_b92 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2614,
        c408: n2612,
        c317: n2588,
        h1: n4549, h2: n4550,
    };
    // body 92: buttons 0x22, forks 0x0
    sink.o9(34, take_9_8, &sh9, &o9);
    declined |= live_v36_b93 & (if bd_v36_b93 { ALL } else { !ok_v36_b93 });
    take_9_9 |= live_v36_b93 & ok_v36_b93 & (if bd_v36_b93 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2628,
        c298: n2577,
        c397: n2629,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2634,
        c408: n2632,
        c317: n2588,
        h1: n4575, h2: n4576,
    };
    // body 93: buttons 0x24, forks 0x0
    sink.o9(36, take_9_9, &sh9, &o9);
    declined |= live_v37_b94 & (if bd_v37_b94 { ALL } else { !ok_v37_b94 });
    take_9_10 |= live_v37_b94 & ok_v37_b94 & (if bd_v37_b94 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2642,
        c408: n2640,
        c317: n2588,
        h1: n4591, h2: n4592,
    };
    // body 94: buttons 0x25, forks 0x0
    sink.o9(37, take_9_10, &sh9, &o9);
    declined |= live_v38_b95 & (if bd_v38_b95 { ALL } else { !ok_v38_b95 });
    take_9_11 |= live_v38_b95 & ok_v38_b95 & (if bd_v38_b95 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2650,
        c408: n2648,
        c317: n2588,
        h1: n4605, h2: n4606,
    };
    // body 95: buttons 0x26, forks 0x0
    sink.o9(38, take_9_11, &sh9, &o9);
    declined |= live_v40_b96 & (if bd_v40_b96 { ALL } else { !ok_v40_b96 });
    take_9_12 |= live_v40_b96 & ok_v40_b96 & (if bd_v40_b96 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2628,
        c298: n2577,
        c397: n2629,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2634,
        c408: n2656,
        c317: n2588,
        h1: n4617, h2: n4618,
    };
    // body 96: buttons 0x28, forks 0x0
    sink.o9(40, take_9_12, &sh9, &o9);
    declined |= live_v41_b97 & (if bd_v41_b97 { ALL } else { !ok_v41_b97 });
    take_9_13 |= live_v41_b97 & ok_v41_b97 & (if bd_v41_b97 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2642,
        c408: n2659,
        c317: n2588,
        h1: n4627, h2: n4628,
    };
    // body 97: buttons 0x29, forks 0x0
    sink.o9(41, take_9_13, &sh9, &o9);
    declined |= live_v42_b98 & (if bd_v42_b98 { ALL } else { !ok_v42_b98 });
    take_9_14 |= live_v42_b98 & ok_v42_b98 & (if bd_v42_b98 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2511,
        c310: n2556,
        c311: n2503,
        c407: n2650,
        c408: n2662,
        c317: n2588,
        h1: n4637, h2: n4638,
    };
    // body 98: buttons 0x2a, forks 0x0
    sink.o9(42, take_9_14, &sh9, &o9);
    declined |= live_v48_b99 & (if bd_v48_b99 { ALL } else { !ok_v48_b99 });
    take_9_15 |= live_v48_b99 & ok_v48_b99 & (if bd_v48_b99 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2581,
        c298: n2577,
        c397: n2582,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2670,
        c408: n2668,
        c317: n2588,
        h1: n4663, h2: n4664,
    };
    // body 99: buttons 0x30, forks 0x0
    sink.o9(48, take_9_15, &sh9, &o9);
    declined |= live_v49_b100 & (if bd_v49_b100 { ALL } else { !ok_v49_b100 });
    take_9_16 |= live_v49_b100 & ok_v49_b100 & (if bd_v49_b100 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2678,
        c408: n2676,
        c317: n2588,
        h1: n4679, h2: n4680,
    };
    // body 100: buttons 0x31, forks 0x0
    sink.o9(49, take_9_16, &sh9, &o9);
    declined |= live_v50_b101 & (if bd_v50_b101 { ALL } else { !ok_v50_b101 });
    take_9_17 |= live_v50_b101 & ok_v50_b101 & (if bd_v50_b101 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2580,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2583,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2686,
        c408: n2684,
        c317: n2588,
        h1: n4693, h2: n4694,
    };
    // body 101: buttons 0x32, forks 0x0
    sink.o9(50, take_9_17, &sh9, &o9);
    declined |= live_v52_b102 & (if bd_v52_b102 { ALL } else { !ok_v52_b102 });
    take_9_18 |= live_v52_b102 & ok_v52_b102 & (if bd_v52_b102 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2628,
        c298: n2577,
        c397: n2629,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2694,
        c408: n2692,
        c317: n2588,
        h1: n4711, h2: n4712,
    };
    // body 102: buttons 0x34, forks 0x0
    sink.o9(52, take_9_18, &sh9, &o9);
    declined |= live_v53_b103 & (if bd_v53_b103 { ALL } else { !ok_v53_b103 });
    take_9_19 |= live_v53_b103 & ok_v53_b103 & (if bd_v53_b103 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2702,
        c408: n2700,
        c317: n2588,
        h1: n4727, h2: n4728,
    };
    // body 103: buttons 0x35, forks 0x0
    sink.o9(53, take_9_19, &sh9, &o9);
    declined |= live_v54_b104 & (if bd_v54_b104 { ALL } else { !ok_v54_b104 });
    take_9_20 |= live_v54_b104 & ok_v54_b104 & (if bd_v54_b104 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2630,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2710,
        c408: n2708,
        c317: n2588,
        h1: n4741, h2: n4742,
    };
    // body 104: buttons 0x36, forks 0x0
    sink.o9(54, take_9_20, &sh9, &o9);
    declined |= live_v56_b105 & (if bd_v56_b105 { ALL } else { !ok_v56_b105 });
    take_9_21 |= live_v56_b105 & ok_v56_b105 & (if bd_v56_b105 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2628,
        c298: n2577,
        c397: n2629,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2514,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2694,
        c408: n2713,
        c317: n2588,
        h1: n4751, h2: n4752,
    };
    // body 105: buttons 0x38, forks 0x0
    sink.o9(56, take_9_21, &sh9, &o9);
    declined |= live_v57_b106 & (if bd_v57_b106 { ALL } else { !ok_v57_b106 });
    take_9_22 |= live_v57_b106 & ok_v57_b106 & (if bd_v57_b106 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2599,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2532,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2702,
        c408: n2716,
        c317: n2588,
        h1: n4761, h2: n4762,
    };
    // body 106: buttons 0x39, forks 0x0
    sink.o9(57, take_9_22, &sh9, &o9);
    declined |= live_v58_b107 & (if bd_v58_b107 { ALL } else { !ok_v58_b107 });
    take_9_23 |= live_v58_b107 & ok_v58_b107 & (if bd_v58_b107 { 0 } else { ALL });
    let o9 = KOut9 {
        c20: n2575,
        c41: n2576,
        c395: n2627,
        c396: n2598,
        c298: n2577,
        c397: n2610,
        c398: n2655,
        c300: n2578,
        c301: n2579,
        c399: n2537,
        c303: n2543,
        c310: n2556,
        c311: n2542,
        c407: n2710,
        c408: n2719,
        c317: n2588,
        h1: n4771, h2: n4772,
    };
    // body 107: buttons 0x3a, forks 0x0
    sink.o9(58, take_9_23, &sh9, &o9);
    declined
}
