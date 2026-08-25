// GENERATED from a TRACED frame (shape 8). Do not edit.
//
// One input shape, 7 output shapes, 128 distinct button
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
pub const SHAPE: u64 = 16270143996677107556;

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
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c338: P8,
    pub c339: P8,
    pub c340: P8,
    pub c341: P8,
    pub c348: P8,
    pub c349: P8,
    pub c350: P8,
    pub c351: P8,
    pub c362: P8,
    pub c363: P8,
    pub c364: P8,
    pub c365: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("got_fruit[#3]", "bool"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("minutes", "num"),
    ("objects[0].collideable", "bool"),
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].hide_for", "num"),
    ("objects[0].hide_in", "num"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
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
    ("objects[2].dash_accel.x", "num"),
    ("objects[2].dash_accel.y", "num"),
    ("objects[2].dash_effect_time", "num"),
    ("objects[2].dash_target.x", "num"),
    ("objects[2].dash_target.y", "num"),
    ("objects[2].dash_time", "num"),
    ("objects[2].djump", "num"),
    ("objects[2].flip.x", "bool"),
    ("objects[2].flip.y", "bool"),
    ("objects[2].grace", "num"),
    ("objects[2].p_dash", "bool"),
    ("objects[2].p_jump", "bool"),
    ("objects[2].rem.x", "num"),
    ("objects[2].rem.y", "num"),
    ("objects[2].solids", "bool"),
    ("objects[2].spd.x", "num"),
    ("objects[2].spd.y", "num"),
    ("objects[2].x", "num"),
    ("objects[2].y", "num"),
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
    pub c175: u16,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c240: u16,
    pub c336: u16,
    pub c337: u16,
    pub c242: ZN,
    pub c243: ZN,
    pub c342: ZN,
    pub c343: ZN,
    pub c251: u16,
    pub c344: ZN,
    pub c345: ZN,
    pub c253: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c259: u16,
    pub c346: u16,
    pub c347: u16,
    pub c261: ZN,
    pub c262: ZN,
    pub c352: ZN,
    pub c353: ZN,
    pub c270: u16,
    pub c354: ZN,
    pub c355: ZN,
    pub c272: ZN,
    pub c274: ZN,
    pub c275: ZN,
    pub c278: u16,
    pub c356: ZN,
    pub c357: ZN,
    pub c280: ZN,
    pub c358: ZN,
    pub c359: ZN,
    pub c282: ZN,
    pub c283: ZN,
    pub c360: u16,
    pub c361: u16,
    pub c285: ZN,
    pub c292: u16,
    pub c293: u16,
    pub c366: ZN,
    pub c367: ZN,
    pub c295: u16,
    pub c368: ZN,
    pub c369: ZN,
    pub c299: ZN,
    pub c300: ZN,
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
    pub c175: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c240: u32,
    pub c336: u32,
    pub c337: u32,
    pub c242: u32,
    pub c243: u32,
    pub c342: u32,
    pub c343: u32,
    pub c251: u32,
    pub c344: u32,
    pub c345: u32,
    pub c253: u32,
    pub c255: u32,
    pub c256: u32,
    pub c259: u32,
    pub c346: u32,
    pub c347: u32,
    pub c261: u32,
    pub c262: u32,
    pub c352: u32,
    pub c353: u32,
    pub c270: u32,
    pub c354: u32,
    pub c355: u32,
    pub c272: u32,
    pub c274: u32,
    pub c275: u32,
    pub c278: u32,
    pub c356: u32,
    pub c357: u32,
    pub c280: u32,
    pub c358: u32,
    pub c359: u32,
    pub c282: u32,
    pub c283: u32,
    pub c360: u32,
    pub c361: u32,
    pub c285: u32,
    pub c292: u32,
    pub c293: u32,
    pub c366: u32,
    pub c367: u32,
    pub c295: u32,
    pub c368: u32,
    pub c369: u32,
    pub c299: u32,
    pub c300: u32,
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
        c338: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c339: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c340: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c341: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c348: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c349: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c350: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c351: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c362: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c363: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c364: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c365: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c175: cell("got_fruit[#3]")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c240: cell("objects[0].collideable")?,
        c336: cell("objects[0].flip.x")?,
        c337: cell("objects[0].flip.y")?,
        c242: cell("objects[0].hide_for")?,
        c243: cell("objects[0].hide_in")?,
        c342: cell("objects[0].rem.x")?,
        c343: cell("objects[0].rem.y")?,
        c251: cell("objects[0].solids")?,
        c344: cell("objects[0].spd.x")?,
        c345: cell("objects[0].spd.y")?,
        c253: cell("objects[0].spr")?,
        c255: cell("objects[0].x")?,
        c256: cell("objects[0].y")?,
        c259: cell("objects[1].collideable")?,
        c346: cell("objects[1].flip.x")?,
        c347: cell("objects[1].flip.y")?,
        c261: cell("objects[1].hide_for")?,
        c262: cell("objects[1].hide_in")?,
        c352: cell("objects[1].rem.x")?,
        c353: cell("objects[1].rem.y")?,
        c270: cell("objects[1].solids")?,
        c354: cell("objects[1].spd.x")?,
        c355: cell("objects[1].spd.y")?,
        c272: cell("objects[1].spr")?,
        c274: cell("objects[1].x")?,
        c275: cell("objects[1].y")?,
        c278: cell("objects[2].collideable")?,
        c356: cell("objects[2].dash_accel.x")?,
        c357: cell("objects[2].dash_accel.y")?,
        c280: cell("objects[2].dash_effect_time")?,
        c358: cell("objects[2].dash_target.x")?,
        c359: cell("objects[2].dash_target.y")?,
        c282: cell("objects[2].dash_time")?,
        c283: cell("objects[2].djump")?,
        c360: cell("objects[2].flip.x")?,
        c361: cell("objects[2].flip.y")?,
        c285: cell("objects[2].grace")?,
        c292: cell("objects[2].p_dash")?,
        c293: cell("objects[2].p_jump")?,
        c366: cell("objects[2].rem.x")?,
        c367: cell("objects[2].rem.y")?,
        c295: cell("objects[2].solids")?,
        c368: cell("objects[2].spd.x")?,
        c369: cell("objects[2].spd.y")?,
        c299: cell("objects[2].x")?,
        c300: cell("objects[2].y")?,
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
        c175: match &b.cols[s.c175 as usize] {
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
        c240: match &b.cols[s.c240 as usize] {
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
        c336: match &b.cols[s.c336 as usize] {
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
        c337: match &b.cols[s.c337 as usize] {
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
        c242: match &b.cols[s.c242 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c243: match &b.cols[s.c243 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c342: match &b.cols[s.c342 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c343: match &b.cols[s.c343 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c251: match &b.cols[s.c251 as usize] {
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
        c344: match &b.cols[s.c344 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c345: match &b.cols[s.c345 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c255: match &b.cols[s.c255 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c256: match &b.cols[s.c256 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c259: match &b.cols[s.c259 as usize] {
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
        c346: match &b.cols[s.c346 as usize] {
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
        c347: match &b.cols[s.c347 as usize] {
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
        c261: match &b.cols[s.c261 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c262: match &b.cols[s.c262 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c352: match &b.cols[s.c352 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c353: match &b.cols[s.c353 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c270: match &b.cols[s.c270 as usize] {
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
        c354: match &b.cols[s.c354 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c355: match &b.cols[s.c355 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c272: match &b.cols[s.c272 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c274: match &b.cols[s.c274 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c275: match &b.cols[s.c275 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c278: match &b.cols[s.c278 as usize] {
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
        c356: match &b.cols[s.c356 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c357: match &b.cols[s.c357 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c280: match &b.cols[s.c280 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c358: match &b.cols[s.c358 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c359: match &b.cols[s.c359 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c282: match &b.cols[s.c282 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c283: match &b.cols[s.c283 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c360: match &b.cols[s.c360 as usize] {
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
        c361: match &b.cols[s.c361 as usize] {
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
        c285: match &b.cols[s.c285 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c292: match &b.cols[s.c292 as usize] {
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
        c293: match &b.cols[s.c293 as usize] {
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
        c368: match &b.cols[s.c368 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c369: match &b.cols[s.c369 as usize] {
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

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c87: ZN,
    pub c84: ZN,
    pub c86: ZN,
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

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
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

/// Outcome 5's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared5 {
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

// ---------------- outcome 6 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_6: &[(u32, &str)] = &[
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

/// Outcome 6's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared6 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c366: ZN,
    pub c367: ZN,
    pub c300: ZN,
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

/// An EMPTY accumulator with outcome 6's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append6`.
pub fn acc6(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_6, OUT_GLOBALS_6, OUT_PTRS_6, 0, cart, cache);
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
    for (cell, _) in OUT_UBOOL_6 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// Append this assignment's lanes that TAKE outcome 6 and
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
pub fn append6(
    acc: &mut Rt2, sh: &KShared6, kv: &KOut6, take: u16,
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

pub const OUTCOMES: usize = 7;

pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    match i {
        0 => acc0(cart, cache),
        1 => acc1(cart, cache),
        2 => acc2(cart, cache),
        3 => acc3(cart, cache),
        4 => acc4(cart, cache),
        5 => acc5(cart, cache),
        6 => acc6(cart, cache),
        _ => panic!("outcome {} of 7", i),
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
        _ => panic!("outcome {} of 7", i),
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
    fn o6(&mut self, _mask: u8, take: u16, sh: &KShared6, v: &KOut6) {
        append6(&mut self.accs[6], sh, v, take, self.n, &mut self.seen[6], self.org);
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
    fn o6(&mut self, mask: u8, take: u16, sh: &KShared6, v: &KOut6);
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
    let r_c175: ZB = ZB { val: rin.c175, known: ALL };
    let r_c240: ZB = ZB { val: rin.c240, known: ALL };
    let r_c242: ZN = rin.c242;
    let r_c243: ZN = rin.c243;
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c253: ZN = rin.c253;
    let r_c255: ZN = rin.c255;
    let r_c256: ZN = rin.c256;
    let r_c259: ZB = ZB { val: rin.c259, known: ALL };
    let r_c261: ZN = rin.c261;
    let r_c262: ZN = rin.c262;
    let r_c270: ZB = ZB { val: rin.c270, known: ALL };
    let r_c272: ZN = rin.c272;
    let r_c274: ZN = rin.c274;
    let r_c275: ZN = rin.c275;
    let r_c278: ZB = ZB { val: rin.c278, known: ALL };
    let r_c280: ZN = rin.c280;
    let r_c282: ZN = rin.c282;
    let r_c283: ZN = rin.c283;
    let r_c285: ZN = rin.c285;
    let r_c292: ZB = ZB { val: rin.c292, known: ALL };
    let r_c293: ZB = ZB { val: rin.c293, known: ALL };
    let r_c295: ZB = ZB { val: rin.c295, known: ALL };
    let r_c299: ZN = rin.c299;
    let r_c300: ZN = rin.c300;
    let r_c336: ZB = ZB { val: rin.c336, known: ALL };
    let r_c337: ZB = ZB { val: rin.c337, known: ALL };
    let r_c342: ZN = rin.c342;
    let r_c343: ZN = rin.c343;
    let r_c344: ZN = rin.c344;
    let r_c345: ZN = rin.c345;
    let r_c346: ZB = ZB { val: rin.c346, known: ALL };
    let r_c347: ZB = ZB { val: rin.c347, known: ALL };
    let r_c352: ZN = rin.c352;
    let r_c353: ZN = rin.c353;
    let r_c354: ZN = rin.c354;
    let r_c355: ZN = rin.c355;
    let r_c356: ZN = rin.c356;
    let r_c357: ZN = rin.c357;
    let r_c358: ZN = rin.c358;
    let r_c359: ZN = rin.c359;
    let r_c360: ZB = ZB { val: rin.c360, known: ALL };
    let r_c361: ZB = ZB { val: rin.c361, known: ALL };
    let r_c366: ZN = rin.c366;
    let r_c367: ZN = rin.c367;
    let r_c368: ZN = rin.c368;
    let r_c369: ZN = rin.c369;
    let n100: ZB = zb_not(r_c336);
    let n101: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c243);
    let n102: bool = P8::from_raw(0i32) == u.c340;
    let n103: bool = P8::from_raw(0i32) == u.c341;
    let n104: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c343);
    let n105: ZB = zb_not(r_c346);
    let n106: ZB = zb_not(r_c347);
    let n107: bool = P8::from_raw(0i32) == u.c350;
    let n108: bool = P8::from_raw(0i32) == u.c351;
    let n109: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c353);
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c355);
    let n111: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c274);
    let n115: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n116: ZN = zn_rem(n115, zn_splat(P8::from_raw(1966080i32)));
    let n117: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n116);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c261);
    let n133: bool = P8::from_raw(524288i32) == u.c348;
    let n134: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c352);
    let n135: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c354);
    let n136: ZB = zb_not(r_c293);
    let n137: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n142: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c262);
    let n143: bool = P8::from_raw(524288i32) == u.c349;
    let n144: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c275);
    let n145: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c272);
    let n146: ZB = zb_not(r_c42);
    let n147: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n148: ZB = zb_not(r_c337);
    let n149: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c242);
    let n150: bool = P8::from_raw(524288i32) == u.c338;
    let n151: bool = P8::from_raw(524288i32) == u.c339;
    let n152: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c342);
    let n153: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c344);
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c345);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c253);
    let n156: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c255);
    let n157: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c256);
    let n158: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n179: ZB = zb_not(r_c38);
    let n197: ZB = zb_not(r_c361);
    let n199: bool = P8::from_raw(327680i32) == u.c362;
    let n201: bool = P8::from_raw(393216i32) == u.c363;
    let n203: bool = P8::from_raw(65536i32) == u.c364;
    let n205: bool = P8::from_raw(196608i32) == u.c365;
    let n208: ZB = zb_not(r_c43);
    let n211: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n212: ZN = zn_rem(n211, zn_splat(P8::from_raw(3932160i32)));
    let n213: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n212);
    let n214: ZN = zsel_n(n213, n158, r_c86);
    let n215: ZN = zsel_n(n117, n214, r_c86);
    let n216: ZN = zsel_n(n117, n212, r_c85);
    let n217: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c299);
    let n218: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n217);
    let n219: ZB = zn_gt(n218, zn_splat(P8::from_raw(2621440i32)));
    let n220: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c300);
    let n221: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n220);
    let n222: ZB = zn_gt(n221, zn_splat(P8::from_raw(7340032i32)));
    let n223: ZB = zb_and(n219, n222);
    let n224: ZB = zn_lt(n217, zn_splat(P8::from_raw(3145728i32)));
    let n225: ZB = zb_and(n223, n224);
    let n226: ZB = zn_lt(n220, zn_splat(P8::from_raw(7864320i32)));
    let n227: ZB = zb_and(n225, n226);
    let n228: ZB = zb_and(n137, n227);
    let n229: ZB = zn_ge(r_c369, zn_splat(P8::from_raw(0i32)));
    let n230: ZB = zb_and(n228, n229);
    let n231: ZN = zn_mul(r_c368, zn_splat(P8::from_raw(13107i32)));
    let n232: ZB = zn_gt(n218, zn_splat(P8::from_raw(6815744i32)));
    let n233: ZB = zn_le(n218, zn_splat(P8::from_raw(6815744i32)));
    let n234: ZB = zb_and(n230, n232);
    let n235: ZB = zb_and(n230, n233);
    let n236: ZB = zb_or(n234, n235);
    let n237: ZB = zb_and(n232, n236);
    let n238: ZB = zb_and(n233, n236);
    let n239: ZB = zn_lt(n217, zn_splat(P8::from_raw(7340032i32)));
    let n240: ZB = zb_or(n237, n238);
    let n241: ZB = zb_and(n232, n239);
    let n242: ZB = zb_not(n241);
    let n243: ZB = zb_and(n240, n241);
    let n244: ZB = zb_and(n240, n242);
    let n245: ZB = zb_or(n243, n244);
    let n246: ZB = zb_and(n241, n245);
    let n247: ZB = zb_and(n242, n245);
    let n248: ZB = zb_or(n246, n247);
    let n249: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n231);
    let n250: ZB = zb_not(n249);
    let n251: ZB = zb_and(n248, n249);
    let n252: ZB = zb_and(n248, n250);
    let n253: ZB = zb_or(n251, n252);
    let n254: ZN = zn_add(r_c366, n231);
    let n255: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n254);
    let n256: ZN = zn_flr(n255);
    let n257: ZN = zn_sub(n255, zn_splat(P8::from_raw(32768i32)));
    let n258: ZN = zn_sub(n257, n256);
    let n259: ZB = zn_gt(n256, zn_splat(P8::from_raw(0i32)));
    let n260: ZB = zn_le(n256, zn_splat(P8::from_raw(0i32)));
    let n261: ZB = zb_and(n253, n259);
    let n262: ZB = zb_and(n253, n260);
    let n263: ZB = zn_lt(n256, zn_splat(P8::from_raw(0i32)));
    let n264: ZB = zn_ge(n256, zn_splat(P8::from_raw(0i32)));
    let n265: ZB = zb_and(n262, n263);
    let n266: ZB = zb_and(n262, n264);
    let n267: ZN = zsel_n(n263, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n268: ZB = zb_or(n265, n266);
    let n269: ZN = zsel_n(n259, zn_splat(P8::from_raw(65536i32)), n267);
    let n270: ZB = zb_or(n261, n268);
    let n271: ZN = zn_abs(n256);
    let n272: ZN = zn_add(n217, n269);
    let n273: ZB = zn_tile_flag_at(g.cache, g.cart, n272, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n274: ZB = zb_not(n273);
    let n275: ZB = zb_and(n270, n274);
    let n276: ZB = zb_and(n270, n273);
    let n277: ZB = zb_or(n275, n276);
    let n278: ZB = zb_and(n274, n277);
    let n279: ZB = zb_and(n273, n277);
    let n280: ZB = zb_or(n278, n279);
    let n281: ZB = zb_and(n274, n280);
    let n282: ZB = zb_and(n273, n280);
    let n283: ZN = zn_add(r_c299, n269);
    let n284: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n271);
    let n285: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n271);
    let n286: ZB = zb_and(n281, n284);
    let n287: ZB = zb_and(n281, n285);
    let n288: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n283);
    let n289: ZN = zn_add(n269, n288);
    let n290: ZB = zn_tile_flag_at(g.cache, g.cart, n289, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n291: ZB = zb_not(n290);
    let n292: ZB = zb_and(n286, n291);
    let n293: ZB = zb_and(n286, n290);
    let n294: ZB = zb_or(n292, n293);
    let n295: ZB = zb_and(n291, n294);
    let n296: ZB = zb_and(n290, n294);
    let n297: ZB = zb_or(n295, n296);
    let n298: ZB = zb_and(n291, n297);
    let n299: ZB = zb_and(n290, n297);
    let n300: ZN = zn_add(n269, n283);
    let n301: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n271);
    let n302: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n271);
    let n303: ZB = zb_and(n298, n301);
    let n304: ZB = zb_and(n298, n302);
    let n305: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n300);
    let n306: ZN = zn_add(n269, n305);
    let n307: ZB = zn_tile_flag_at(g.cache, g.cart, n306, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n308: ZB = zb_not(n307);
    let n309: ZB = zb_and(n303, n308);
    let n310: ZB = zb_and(n303, n307);
    let n311: ZB = zb_or(n309, n310);
    let n312: ZB = zb_and(n308, n311);
    let n313: ZB = zb_and(n307, n311);
    let n314: ZB = zb_or(n312, n313);
    let n315: ZB = zb_and(n308, n314);
    let n316: ZB = zb_and(n307, n314);
    let n317: ZN = zn_add(n269, n300);
    let n318: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n271);
    let n319: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n271);
    let n320: ZB = zb_and(n315, n318);
    let n321: ZB = zb_and(n315, n319);
    let n322: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n317);
    let n323: ZN = zn_add(n269, n322);
    let n324: ZB = zn_tile_flag_at(g.cache, g.cart, n323, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n325: ZB = zb_not(n324);
    let n326: ZB = zb_and(n320, n325);
    let n327: ZB = zb_and(n320, n324);
    let n328: ZB = zb_or(n326, n327);
    let n329: ZB = zb_and(n325, n328);
    let n330: ZB = zb_and(n324, n328);
    let n331: ZB = zb_or(n329, n330);
    let n332: ZB = zb_and(n325, n331);
    let n333: ZB = zb_and(n324, n331);
    let n334: ZN = zn_add(n269, n317);
    let n335: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n271);
    let n336: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n271);
    let n337: ZB = zb_and(n332, n335);
    let n338: ZB = zb_and(n332, n336);
    let n339: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n334);
    let n340: ZN = zn_add(n269, n339);
    let n341: ZB = zn_tile_flag_at(g.cache, g.cart, n340, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n342: ZB = zb_not(n341);
    let n343: ZB = zb_and(n337, n342);
    let n344: ZB = zb_and(n337, n341);
    let n345: ZB = zb_or(n343, n344);
    let n346: ZB = zb_and(n342, n345);
    let n347: ZB = zb_and(n341, n345);
    let n348: ZB = zb_or(n346, n347);
    let n349: ZB = zb_and(n342, n348);
    let n350: ZB = zb_and(n341, n348);
    let n351: ZN = zn_add(n269, n334);
    let n352: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n271);
    let n353: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n271);
    let n354: ZB = zb_and(n349, n352);
    let n355: ZB = zb_and(n349, n353);
    let n356: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n351);
    let n357: ZN = zn_add(n269, n356);
    let n358: ZB = zn_tile_flag_at(g.cache, g.cart, n357, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n359: ZB = zb_not(n358);
    let n360: ZB = zb_and(n354, n359);
    let n361: ZB = zb_and(n354, n358);
    let n362: ZB = zb_or(n360, n361);
    let n363: ZB = zb_and(n359, n362);
    let n364: ZB = zb_and(n358, n362);
    let n365: ZB = zb_or(n363, n364);
    let n366: ZB = zb_and(n359, n365);
    let n367: ZB = zb_and(n358, n365);
    let n368: ZN = zn_add(n269, n351);
    let n369: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n271);
    let n370: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n271);
    let n371: ZB = zb_and(n366, n369);
    let n372: ZB = zb_and(n366, n370);
    let n373: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n368);
    let n374: ZN = zn_add(n269, n373);
    let n375: ZB = zn_tile_flag_at(g.cache, g.cart, n374, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n376: ZB = zb_not(n375);
    let n377: ZB = zb_and(n371, n376);
    let n378: ZB = zb_and(n371, n375);
    let n379: ZB = zb_or(n377, n378);
    let n380: ZB = zb_and(n376, n379);
    let n381: ZB = zb_and(n375, n379);
    let n382: ZB = zb_or(n380, n381);
    let n383: ZB = zb_and(n376, n382);
    let n384: ZB = zb_and(n375, n382);
    let n385: ZN = zn_add(n269, n368);
    let n386: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n271);
    let n387: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n271);
    let n388: ZB = zb_and(n383, n386);
    let n389: ZB = zb_and(n383, n387);
    let n390: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n385);
    let n391: ZN = zn_add(n269, n390);
    let n392: ZB = zn_tile_flag_at(g.cache, g.cart, n391, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n393: ZB = zb_not(n392);
    let n394: ZB = zb_and(n388, n393);
    let n395: ZB = zb_and(n388, n392);
    let n396: ZB = zb_or(n394, n395);
    let n397: ZB = zb_and(n393, n396);
    let n398: ZB = zb_and(n392, n396);
    let n399: ZB = zb_or(n397, n398);
    let n400: ZB = zb_and(n393, n399);
    let n401: ZB = zb_and(n392, n399);
    let n402: ZN = zn_add(n269, n385);
    let n403: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n271);
    let n404: ZN = zsel_n(n392, n385, n402);
    let n405: ZN = zsel_n(n392, zn_splat(P8::from_raw(0i32)), n258);
    let n406: ZN = zsel_n(n392, zn_splat(P8::from_raw(0i32)), n231);
    let n407: ZB = zb_or(n400, n401);
    let n408: ZB = zb_or(n392, n403);
    let n409: ZN = zsel_n(n387, n385, n404);
    let n410: ZN = zsel_n(n387, n258, n405);
    let n411: ZN = zsel_n(n387, n231, n406);
    let n412: ZB = zb_or(n389, n407);
    let n413: ZB = zb_or(n387, n408);
    let n414: ZN = zsel_n(n375, n368, n409);
    let n415: ZN = zsel_n(n375, zn_splat(P8::from_raw(0i32)), n410);
    let n416: ZN = zsel_n(n375, zn_splat(P8::from_raw(0i32)), n411);
    let n417: ZB = zb_or(n384, n412);
    let n418: ZB = zb_or(n375, n413);
    let n419: ZN = zsel_n(n370, n368, n414);
    let n420: ZN = zsel_n(n370, n258, n415);
    let n421: ZN = zsel_n(n370, n231, n416);
    let n422: ZB = zb_or(n372, n417);
    let n423: ZB = zb_or(n370, n418);
    let n424: ZN = zsel_n(n358, n351, n419);
    let n425: ZN = zsel_n(n358, zn_splat(P8::from_raw(0i32)), n420);
    let n426: ZN = zsel_n(n358, zn_splat(P8::from_raw(0i32)), n421);
    let n427: ZB = zb_or(n367, n422);
    let n428: ZB = zb_or(n358, n423);
    let n429: ZN = zsel_n(n353, n351, n424);
    let n430: ZN = zsel_n(n353, n258, n425);
    let n431: ZN = zsel_n(n353, n231, n426);
    let n432: ZB = zb_or(n355, n427);
    let n433: ZB = zb_or(n353, n428);
    let n434: ZN = zsel_n(n341, n334, n429);
    let n435: ZN = zsel_n(n341, zn_splat(P8::from_raw(0i32)), n430);
    let n436: ZN = zsel_n(n341, zn_splat(P8::from_raw(0i32)), n431);
    let n437: ZB = zb_or(n350, n432);
    let n438: ZB = zb_or(n341, n433);
    let n439: ZN = zsel_n(n336, n334, n434);
    let n440: ZN = zsel_n(n336, n258, n435);
    let n441: ZN = zsel_n(n336, n231, n436);
    let n442: ZB = zb_or(n338, n437);
    let n443: ZB = zb_or(n336, n438);
    let n444: ZN = zsel_n(n324, n317, n439);
    let n445: ZN = zsel_n(n324, zn_splat(P8::from_raw(0i32)), n440);
    let n446: ZN = zsel_n(n324, zn_splat(P8::from_raw(0i32)), n441);
    let n447: ZB = zb_or(n333, n442);
    let n448: ZB = zb_or(n324, n443);
    let n449: ZN = zsel_n(n319, n317, n444);
    let n450: ZN = zsel_n(n319, n258, n445);
    let n451: ZN = zsel_n(n319, n231, n446);
    let n452: ZB = zb_or(n321, n447);
    let n453: ZB = zb_or(n319, n448);
    let n454: ZN = zsel_n(n307, n300, n449);
    let n455: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n450);
    let n456: ZN = zsel_n(n307, zn_splat(P8::from_raw(0i32)), n451);
    let n457: ZB = zb_or(n316, n452);
    let n458: ZB = zb_or(n307, n453);
    let n459: ZN = zsel_n(n302, n300, n454);
    let n460: ZN = zsel_n(n302, n258, n455);
    let n461: ZN = zsel_n(n302, n231, n456);
    let n462: ZB = zb_or(n304, n457);
    let n463: ZB = zb_or(n302, n458);
    let n464: ZN = zsel_n(n290, n283, n459);
    let n465: ZN = zsel_n(n290, zn_splat(P8::from_raw(0i32)), n460);
    let n466: ZN = zsel_n(n290, zn_splat(P8::from_raw(0i32)), n461);
    let n467: ZB = zb_or(n299, n462);
    let n468: ZB = zb_or(n290, n463);
    let n469: ZN = zsel_n(n285, n283, n464);
    let n470: ZN = zsel_n(n285, n258, n465);
    let n471: ZN = zsel_n(n285, n231, n466);
    let n472: ZB = zb_or(n287, n467);
    let n473: ZB = zb_or(n285, n468);
    let n474: ZN = zsel_n(n273, r_c299, n469);
    let n475: ZN = zsel_n(n273, zn_splat(P8::from_raw(0i32)), n470);
    let n476: ZN = zsel_n(n273, zn_splat(P8::from_raw(0i32)), n471);
    let n477: ZB = zb_or(n282, n472);
    let n478: ZB = zb_or(n273, n473);
    let n479: ZN = zn_add(r_c367, zn_splat(P8::from_raw(-196608i32)));
    let n480: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n479);
    let n481: ZN = zn_flr(n480);
    let n482: ZN = zn_sub(n480, zn_splat(P8::from_raw(32768i32)));
    let n483: ZN = zn_sub(n482, n481);
    let n484: ZB = zn_gt(n481, zn_splat(P8::from_raw(0i32)));
    let n485: ZB = zn_le(n481, zn_splat(P8::from_raw(0i32)));
    let n486: ZB = zb_and(n477, n484);
    let n487: ZB = zb_and(n477, n485);
    let n488: ZB = zn_lt(n481, zn_splat(P8::from_raw(0i32)));
    let n489: ZB = zn_ge(n481, zn_splat(P8::from_raw(0i32)));
    let n490: ZB = zb_and(n487, n488);
    let n491: ZB = zb_and(n487, n489);
    let n492: ZN = zsel_n(n488, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n493: ZB = zb_or(n490, n491);
    let n494: ZN = zsel_n(n484, zn_splat(P8::from_raw(65536i32)), n492);
    let n495: ZB = zb_or(n486, n493);
    let n496: ZN = zn_abs(n481);
    let n497: ZB = zn_gt(n494, zn_splat(P8::from_raw(0i32)));
    let n498: ZB = zn_le(n494, zn_splat(P8::from_raw(0i32)));
    let n499: ZB = zb_and(n495, n497);
    let n500: ZB = zb_and(n495, n498);
    let n501: ZB = zb_or(n499, n500);
    let n502: ZB = zb_and(n497, n501);
    let n503: ZB = zb_and(n498, n501);
    let n504: ZB = zb_or(n502, n503);
    let n505: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n474);
    let n506: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n505);
    let n507: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n494);
    let n508: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n507, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n509: ZB = zb_not(n508);
    let n510: ZB = zb_and(n504, n509);
    let n511: ZB = zb_and(n504, n508);
    let n512: ZB = zb_or(n510, n511);
    let n513: ZB = zb_and(n509, n512);
    let n514: ZB = zb_and(n508, n512);
    let n515: ZB = zb_or(n513, n514);
    let n516: ZB = zb_and(n509, n515);
    let n517: ZB = zb_and(n508, n515);
    let n518: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n494);
    let n519: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n496);
    let n520: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n496);
    let n521: ZB = zb_and(n516, n519);
    let n522: ZB = zb_and(n516, n520);
    let n523: ZB = zb_and(n497, n521);
    let n524: ZB = zb_and(n498, n521);
    let n525: ZB = zb_or(n523, n524);
    let n526: ZB = zb_and(n497, n525);
    let n527: ZB = zb_and(n498, n525);
    let n528: ZB = zb_or(n526, n527);
    let n529: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n518);
    let n530: ZN = zn_add(n494, n529);
    let n531: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n530, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n532: ZB = zb_not(n531);
    let n533: ZB = zb_and(n528, n532);
    let n534: ZB = zb_and(n528, n531);
    let n535: ZB = zb_or(n533, n534);
    let n536: ZB = zb_and(n532, n535);
    let n537: ZB = zb_and(n531, n535);
    let n538: ZB = zb_or(n536, n537);
    let n539: ZB = zb_and(n532, n538);
    let n540: ZB = zb_and(n531, n538);
    let n541: ZN = zn_add(n494, n518);
    let n542: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n496);
    let n543: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n496);
    let n544: ZB = zb_and(n539, n542);
    let n545: ZB = zb_and(n539, n543);
    let n546: ZB = zb_and(n497, n544);
    let n547: ZB = zb_and(n498, n544);
    let n548: ZB = zb_or(n546, n547);
    let n549: ZB = zb_and(n497, n548);
    let n550: ZB = zb_and(n498, n548);
    let n551: ZB = zb_or(n549, n550);
    let n552: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n541);
    let n553: ZN = zn_add(n494, n552);
    let n554: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n553, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n551, n555);
    let n557: ZB = zb_and(n551, n554);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n555, n558);
    let n560: ZB = zb_and(n554, n558);
    let n561: ZB = zb_or(n559, n560);
    let n562: ZB = zb_and(n555, n561);
    let n563: ZB = zb_and(n554, n561);
    let n564: ZN = zn_add(n494, n541);
    let n565: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n496);
    let n566: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n496);
    let n567: ZB = zb_and(n562, n565);
    let n568: ZB = zb_and(n562, n566);
    let n569: ZB = zb_and(n497, n567);
    let n570: ZB = zb_and(n498, n567);
    let n571: ZB = zb_or(n569, n570);
    let n572: ZB = zb_and(n497, n571);
    let n573: ZB = zb_and(n498, n571);
    let n574: ZB = zb_or(n572, n573);
    let n575: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n564);
    let n576: ZN = zn_add(n494, n575);
    let n577: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n576, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n578: ZB = zb_not(n577);
    let n579: ZB = zb_and(n574, n578);
    let n580: ZB = zb_and(n574, n577);
    let n581: ZB = zb_or(n579, n580);
    let n582: ZB = zb_and(n578, n581);
    let n583: ZB = zb_and(n577, n581);
    let n584: ZB = zb_or(n582, n583);
    let n585: ZB = zb_and(n578, n584);
    let n586: ZB = zb_and(n577, n584);
    let n587: ZN = zn_add(n494, n564);
    let n588: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n496);
    let n589: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n496);
    let n590: ZB = zb_and(n585, n588);
    let n591: ZB = zb_and(n585, n589);
    let n592: ZB = zb_and(n497, n590);
    let n593: ZB = zb_and(n498, n590);
    let n594: ZB = zb_or(n592, n593);
    let n595: ZB = zb_and(n497, n594);
    let n596: ZB = zb_and(n498, n594);
    let n597: ZB = zb_or(n595, n596);
    let n598: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n587);
    let n599: ZN = zn_add(n494, n598);
    let n600: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n599, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n601: ZB = zb_not(n600);
    let n602: ZB = zb_and(n597, n601);
    let n603: ZB = zb_and(n597, n600);
    let n604: ZB = zb_or(n602, n603);
    let n605: ZB = zb_and(n601, n604);
    let n606: ZB = zb_and(n600, n604);
    let n607: ZB = zb_or(n605, n606);
    let n608: ZB = zb_and(n601, n607);
    let n609: ZB = zb_and(n600, n607);
    let n610: ZN = zn_add(n494, n587);
    let n611: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n496);
    let n612: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n496);
    let n613: ZB = zb_and(n608, n611);
    let n614: ZB = zb_and(n608, n612);
    let n615: ZB = zb_and(n497, n613);
    let n616: ZB = zb_and(n498, n613);
    let n617: ZB = zb_or(n615, n616);
    let n618: ZB = zb_and(n497, n617);
    let n619: ZB = zb_and(n498, n617);
    let n620: ZB = zb_or(n618, n619);
    let n621: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n610);
    let n622: ZN = zn_add(n494, n621);
    let n623: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n622, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n624: ZB = zb_not(n623);
    let n625: ZB = zb_and(n620, n624);
    let n626: ZB = zb_and(n620, n623);
    let n627: ZB = zb_or(n625, n626);
    let n628: ZB = zb_and(n624, n627);
    let n629: ZB = zb_and(n623, n627);
    let n630: ZB = zb_or(n628, n629);
    let n631: ZB = zb_and(n624, n630);
    let n632: ZB = zb_and(n623, n630);
    let n633: ZN = zn_add(n494, n610);
    let n634: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n496);
    let n635: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n496);
    let n636: ZB = zb_and(n631, n634);
    let n637: ZB = zb_and(n631, n635);
    let n638: ZB = zb_and(n497, n636);
    let n639: ZB = zb_and(n498, n636);
    let n640: ZB = zb_or(n638, n639);
    let n641: ZB = zb_and(n497, n640);
    let n642: ZB = zb_and(n498, n640);
    let n643: ZB = zb_or(n641, n642);
    let n644: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n633);
    let n645: ZN = zn_add(n494, n644);
    let n646: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n645, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n647: ZB = zb_not(n646);
    let n648: ZB = zb_and(n643, n647);
    let n649: ZB = zb_and(n643, n646);
    let n650: ZB = zb_or(n648, n649);
    let n651: ZB = zb_and(n647, n650);
    let n652: ZB = zb_and(n646, n650);
    let n653: ZB = zb_or(n651, n652);
    let n654: ZB = zb_and(n647, n653);
    let n655: ZB = zb_and(n646, n653);
    let n656: ZN = zn_add(n494, n633);
    let n657: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n496);
    let n658: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n496);
    let n659: ZB = zb_and(n654, n657);
    let n660: ZB = zb_and(n654, n658);
    let n661: ZB = zb_and(n497, n659);
    let n662: ZB = zb_and(n498, n659);
    let n663: ZB = zb_or(n661, n662);
    let n664: ZB = zb_and(n497, n663);
    let n665: ZB = zb_and(n498, n663);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n656);
    let n668: ZN = zn_add(n494, n667);
    let n669: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n668, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n670: ZB = zb_not(n669);
    let n671: ZB = zb_and(n666, n670);
    let n672: ZB = zb_and(n666, n669);
    let n673: ZB = zb_or(n671, n672);
    let n674: ZB = zb_and(n670, n673);
    let n675: ZB = zb_and(n669, n673);
    let n676: ZB = zb_or(n674, n675);
    let n677: ZB = zb_and(n670, n676);
    let n678: ZB = zb_and(n669, n676);
    let n679: ZN = zn_add(n494, n656);
    let n680: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n496);
    let n681: ZB = zb_and(n478, n680);
    let n682: ZN = zsel_n(n669, n656, n679);
    let n683: ZN = zsel_n(n669, zn_splat(P8::from_raw(0i32)), n483);
    let n684: ZN = zsel_n(n669, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n685: ZB = zb_or(n677, n678);
    let n686: ZB = zsel_b(n669, n478, n681);
    let n687: ZN = zsel_n(n658, n656, n682);
    let n688: ZN = zsel_n(n658, n483, n683);
    let n689: ZN = zsel_n(n658, zn_splat(P8::from_raw(-196608i32)), n684);
    let n690: ZB = zb_or(n660, n685);
    let n691: ZB = zsel_b(n658, n478, n686);
    let n692: ZN = zsel_n(n646, n633, n687);
    let n693: ZN = zsel_n(n646, zn_splat(P8::from_raw(0i32)), n688);
    let n694: ZN = zsel_n(n646, zn_splat(P8::from_raw(0i32)), n689);
    let n695: ZB = zb_or(n655, n690);
    let n696: ZB = zsel_b(n646, n478, n691);
    let n697: ZN = zsel_n(n635, n633, n692);
    let n698: ZN = zsel_n(n635, n483, n693);
    let n699: ZN = zsel_n(n635, zn_splat(P8::from_raw(-196608i32)), n694);
    let n700: ZB = zb_or(n637, n695);
    let n701: ZB = zsel_b(n635, n478, n696);
    let n702: ZN = zsel_n(n623, n610, n697);
    let n703: ZN = zsel_n(n623, zn_splat(P8::from_raw(0i32)), n698);
    let n704: ZN = zsel_n(n623, zn_splat(P8::from_raw(0i32)), n699);
    let n705: ZB = zb_or(n632, n700);
    let n706: ZB = zsel_b(n623, n478, n701);
    let n707: ZN = zsel_n(n612, n610, n702);
    let n708: ZN = zsel_n(n612, n483, n703);
    let n709: ZN = zsel_n(n612, zn_splat(P8::from_raw(-196608i32)), n704);
    let n710: ZB = zb_or(n614, n705);
    let n711: ZB = zsel_b(n612, n478, n706);
    let n712: ZN = zsel_n(n600, n587, n707);
    let n713: ZN = zsel_n(n600, zn_splat(P8::from_raw(0i32)), n708);
    let n714: ZN = zsel_n(n600, zn_splat(P8::from_raw(0i32)), n709);
    let n715: ZB = zb_or(n609, n710);
    let n716: ZB = zsel_b(n600, n478, n711);
    let n717: ZN = zsel_n(n589, n587, n712);
    let n718: ZN = zsel_n(n589, n483, n713);
    let n719: ZN = zsel_n(n589, zn_splat(P8::from_raw(-196608i32)), n714);
    let n720: ZB = zb_or(n591, n715);
    let n721: ZB = zsel_b(n589, n478, n716);
    let n722: ZN = zsel_n(n577, n564, n717);
    let n723: ZN = zsel_n(n577, zn_splat(P8::from_raw(0i32)), n718);
    let n724: ZN = zsel_n(n577, zn_splat(P8::from_raw(0i32)), n719);
    let n725: ZB = zb_or(n586, n720);
    let n726: ZB = zsel_b(n577, n478, n721);
    let n727: ZN = zsel_n(n566, n564, n722);
    let n728: ZN = zsel_n(n566, n483, n723);
    let n729: ZN = zsel_n(n566, zn_splat(P8::from_raw(-196608i32)), n724);
    let n730: ZB = zb_or(n568, n725);
    let n731: ZB = zsel_b(n566, n478, n726);
    let n732: ZN = zsel_n(n554, n541, n727);
    let n733: ZN = zsel_n(n554, zn_splat(P8::from_raw(0i32)), n728);
    let n734: ZN = zsel_n(n554, zn_splat(P8::from_raw(0i32)), n729);
    let n735: ZB = zb_or(n563, n730);
    let n736: ZB = zsel_b(n554, n478, n731);
    let n737: ZN = zsel_n(n543, n541, n732);
    let n738: ZN = zsel_n(n543, n483, n733);
    let n739: ZN = zsel_n(n543, zn_splat(P8::from_raw(-196608i32)), n734);
    let n740: ZB = zb_or(n545, n735);
    let n741: ZB = zsel_b(n543, n478, n736);
    let n742: ZN = zsel_n(n531, n518, n737);
    let n743: ZN = zsel_n(n531, zn_splat(P8::from_raw(0i32)), n738);
    let n744: ZN = zsel_n(n531, zn_splat(P8::from_raw(0i32)), n739);
    let n745: ZB = zb_or(n540, n740);
    let n746: ZB = zsel_b(n531, n478, n741);
    let n747: ZN = zsel_n(n520, n518, n742);
    let n748: ZN = zsel_n(n520, n483, n743);
    let n749: ZN = zsel_n(n520, zn_splat(P8::from_raw(-196608i32)), n744);
    let n750: ZB = zb_or(n522, n745);
    let n751: ZB = zsel_b(n520, n478, n746);
    let n752: ZN = zsel_n(n508, zn_splat(P8::from_raw(7077888i32)), n747);
    let n753: ZN = zsel_n(n508, zn_splat(P8::from_raw(0i32)), n748);
    let n754: ZN = zsel_n(n508, zn_splat(P8::from_raw(0i32)), n749);
    let n755: ZB = zb_or(n517, n750);
    let n756: ZB = zsel_b(n508, n478, n751);
    let n757: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n752);
    let n758: ZN = zn_div(n505, zn_splat(P8::from_raw(524288i32)));
    let n759: ZN = zn_flr(n758);
    let n760: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n759);
    let n761: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n505);
    let n762: ZN = zn_sub(n761, zn_splat(P8::from_raw(65536i32)));
    let n763: ZN = zn_div(n762, zn_splat(P8::from_raw(524288i32)));
    let n764: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n763);
    let n765: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n760);
    let n766: ZB = zn_le(n765, n764);
    let n767: ZB = zn_gt(n765, n764);
    let n768: ZB = zb_and(n755, n766);
    let n769: ZB = zb_and(n755, n767);
    let n770: ZN = zn_div(n757, zn_splat(P8::from_raw(524288i32)));
    let n771: ZN = zn_flr(n770);
    let n772: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n771);
    let n773: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n757);
    let n774: ZN = zn_sub(n773, zn_splat(P8::from_raw(65536i32)));
    let n775: ZN = zn_div(n774, zn_splat(P8::from_raw(524288i32)));
    let n776: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n775);
    let n777: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n772);
    let n778: ZB = zn_le(n777, n776);
    let n779: ZB = zn_gt(n777, n776);
    let n780: ZB = zb_and(n768, n778);
    let n781: ZB = zb_and(n768, n779);
    let n782: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n765);
    let n783: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n777);
    let n784: ZN = zn_mget(g.cart, n782, n783);
    let n785: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n784);
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_and(n780, n785);
    let n788: ZB = zb_and(n780, n786);
    let n789: ZN = zn_rem(n774, zn_splat(P8::from_raw(524288i32)));
    let n790: ZB = zn_ge(n789, zn_splat(P8::from_raw(393216i32)));
    let n791: ZB = zn_lt(n789, zn_splat(P8::from_raw(393216i32)));
    let n792: ZB = zb_and(n787, n791);
    let n793: ZB = zb_and(n787, n790);
    let n794: ZN = zn_mul(n777, zn_splat(P8::from_raw(524288i32)));
    let n795: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n794);
    let n796: ZB = zn_eq(n773, n795);
    let n797: ZB = zb_or(n792, n793);
    let n798: ZB = zb_or(n790, n796);
    let n799: ZB = zb_or(n788, n797);
    let n800: ZB = zb_and(n785, n798);
    let n801: ZB = zb_not(n800);
    let n802: ZB = zb_and(n799, n800);
    let n803: ZB = zb_and(n799, n801);
    let n804: ZB = zn_ge(n754, zn_splat(P8::from_raw(0i32)));
    let n805: ZB = zb_or(n802, n803);
    let n806: ZB = zb_and(n800, n804);
    let n807: ZB = zb_not(n806);
    let n808: ZB = zb_and(n805, n807);
    let n809: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n784);
    let n810: ZB = zb_not(n809);
    let n811: ZB = zb_and(n808, n809);
    let n812: ZB = zb_and(n808, n810);
    let n813: ZN = zn_rem(n757, zn_splat(P8::from_raw(524288i32)));
    let n814: ZB = zn_le(n813, zn_splat(P8::from_raw(131072i32)));
    let n815: ZB = zb_or(n811, n812);
    let n816: ZB = zb_and(n809, n814);
    let n817: ZB = zb_not(n816);
    let n818: ZB = zb_and(n815, n816);
    let n819: ZB = zb_and(n815, n817);
    let n820: ZB = zb_or(n818, n819);
    let n821: ZB = zb_and(n817, n820);
    let n822: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n784);
    let n823: ZB = zb_not(n822);
    let n824: ZB = zb_and(n821, n822);
    let n825: ZB = zb_and(n821, n823);
    let n826: ZN = zn_rem(n505, zn_splat(P8::from_raw(524288i32)));
    let n827: ZB = zn_le(n826, zn_splat(P8::from_raw(131072i32)));
    let n828: ZB = zb_or(n824, n825);
    let n829: ZB = zb_and(n822, n827);
    let n830: ZB = zb_not(n829);
    let n831: ZB = zb_and(n828, n829);
    let n832: ZB = zb_and(n828, n830);
    let n833: ZB = zn_le(n476, zn_splat(P8::from_raw(0i32)));
    let n834: ZB = zb_or(n831, n832);
    let n835: ZB = zb_and(n829, n833);
    let n836: ZB = zb_not(n835);
    let n837: ZB = zb_and(n834, n836);
    let n838: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n784);
    let n839: ZB = zb_not(n838);
    let n840: ZB = zb_and(n837, n838);
    let n841: ZB = zb_and(n837, n839);
    let n842: ZN = zn_rem(n762, zn_splat(P8::from_raw(524288i32)));
    let n843: ZB = zn_ge(n842, zn_splat(P8::from_raw(393216i32)));
    let n844: ZB = zn_lt(n842, zn_splat(P8::from_raw(393216i32)));
    let n845: ZB = zb_and(n840, n844);
    let n846: ZB = zb_and(n840, n843);
    let n847: ZN = zn_mul(n765, zn_splat(P8::from_raw(524288i32)));
    let n848: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n847);
    let n849: ZB = zn_eq(n761, n848);
    let n850: ZB = zb_or(n845, n846);
    let n851: ZB = zb_or(n843, n849);
    let n852: ZB = zb_or(n841, n850);
    let n853: ZB = zb_and(n838, n851);
    let n854: ZB = zb_not(n853);
    let n855: ZB = zb_and(n852, n853);
    let n856: ZB = zb_and(n852, n854);
    let n857: ZB = zn_ge(n476, zn_splat(P8::from_raw(0i32)));
    let n858: ZB = zb_or(n855, n856);
    let n859: ZB = zb_and(n853, n857);
    let n860: ZB = zb_not(n859);
    let n861: ZB = zb_and(n858, n860);
    let n862: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n772);
    let n863: ZB = zn_le(n862, n776);
    let n864: ZB = zn_gt(n862, n776);
    let n865: ZB = zb_and(n861, n863);
    let n866: ZB = zb_and(n861, n864);
    let n867: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n862);
    let n868: ZN = zn_mget(g.cart, n782, n867);
    let n869: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n868);
    let n870: ZB = zb_not(n869);
    let n871: ZB = zb_and(n865, n869);
    let n872: ZB = zb_and(n865, n870);
    let n873: ZB = zb_and(n791, n871);
    let n874: ZB = zb_and(n790, n871);
    let n875: ZN = zn_mul(n862, zn_splat(P8::from_raw(524288i32)));
    let n876: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n875);
    let n877: ZB = zn_eq(n773, n876);
    let n878: ZB = zb_or(n873, n874);
    let n879: ZB = zb_or(n790, n877);
    let n880: ZB = zb_or(n872, n878);
    let n881: ZB = zb_and(n869, n879);
    let n882: ZB = zb_not(n881);
    let n883: ZB = zb_and(n880, n881);
    let n884: ZB = zb_and(n880, n882);
    let n885: ZB = zb_or(n883, n884);
    let n886: ZB = zb_and(n804, n881);
    let n887: ZB = zb_not(n886);
    let n888: ZB = zb_and(n885, n887);
    let n889: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n868);
    let n890: ZB = zb_not(n889);
    let n891: ZB = zb_and(n888, n889);
    let n892: ZB = zb_and(n888, n890);
    let n893: ZB = zb_or(n891, n892);
    let n894: ZB = zb_and(n814, n889);
    let n895: ZB = zb_not(n894);
    let n896: ZB = zb_and(n893, n894);
    let n897: ZB = zb_and(n893, n895);
    let n898: ZB = zb_or(n896, n897);
    let n899: ZB = zb_and(n895, n898);
    let n900: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n868);
    let n901: ZB = zb_not(n900);
    let n902: ZB = zb_and(n899, n900);
    let n903: ZB = zb_and(n899, n901);
    let n904: ZB = zb_or(n902, n903);
    let n905: ZB = zb_and(n827, n900);
    let n906: ZB = zb_not(n905);
    let n907: ZB = zb_and(n904, n905);
    let n908: ZB = zb_and(n904, n906);
    let n909: ZB = zb_or(n907, n908);
    let n910: ZB = zb_and(n833, n905);
    let n911: ZB = zb_not(n910);
    let n912: ZB = zb_and(n909, n911);
    let n913: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n868);
    let n914: ZB = zb_not(n913);
    let n915: ZB = zb_and(n912, n913);
    let n916: ZB = zb_and(n912, n914);
    let n917: ZB = zb_and(n844, n915);
    let n918: ZB = zb_and(n843, n915);
    let n919: ZB = zb_or(n917, n918);
    let n920: ZB = zb_or(n916, n919);
    let n921: ZB = zb_and(n851, n913);
    let n922: ZB = zb_not(n921);
    let n923: ZB = zb_and(n920, n921);
    let n924: ZB = zb_and(n920, n922);
    let n925: ZB = zb_or(n923, n924);
    let n926: ZB = zb_and(n857, n921);
    let n927: ZB = zb_not(n926);
    let n928: ZB = zb_and(n925, n927);
    let n929: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n772);
    let n930: ZB = zn_le(n929, n776);
    let n931: ZB = zn_gt(n929, n776);
    let n932: ZB = zb_and(n928, n930);
    let n933: ZB = zb_and(n928, n931);
    let n934: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n929);
    let n935: ZN = zn_mget(g.cart, n782, n934);
    let n936: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n935);
    let n937: ZB = zb_not(n936);
    let n938: ZB = zb_and(n932, n936);
    let n939: ZB = zb_and(n932, n937);
    let n940: ZB = zb_and(n791, n938);
    let n941: ZB = zb_and(n790, n938);
    let n942: ZN = zn_mul(n929, zn_splat(P8::from_raw(524288i32)));
    let n943: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n942);
    let n944: ZB = zn_eq(n773, n943);
    let n945: ZB = zb_or(n940, n941);
    let n946: ZB = zb_or(n790, n944);
    let n947: ZB = zb_or(n939, n945);
    let n948: ZB = zb_and(n936, n946);
    let n949: ZB = zb_not(n948);
    let n950: ZB = zb_and(n947, n948);
    let n951: ZB = zb_and(n947, n949);
    let n952: ZB = zb_or(n950, n951);
    let n953: ZB = zb_and(n804, n948);
    let n954: ZB = zb_not(n953);
    let n955: ZB = zb_and(n952, n954);
    let n956: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n935);
    let n957: ZB = zb_not(n956);
    let n958: ZB = zb_and(n955, n956);
    let n959: ZB = zb_and(n955, n957);
    let n960: ZB = zb_or(n958, n959);
    let n961: ZB = zb_and(n814, n956);
    let n962: ZB = zb_not(n961);
    let n963: ZB = zb_and(n960, n961);
    let n964: ZB = zb_and(n960, n962);
    let n965: ZB = zb_or(n963, n964);
    let n966: ZB = zb_and(n962, n965);
    let n967: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n935);
    let n968: ZB = zb_not(n967);
    let n969: ZB = zb_and(n966, n967);
    let n970: ZB = zb_and(n966, n968);
    let n971: ZB = zb_or(n969, n970);
    let n972: ZB = zb_and(n827, n967);
    let n973: ZB = zb_not(n972);
    let n974: ZB = zb_and(n971, n972);
    let n975: ZB = zb_and(n971, n973);
    let n976: ZB = zb_or(n974, n975);
    let n977: ZB = zb_and(n833, n972);
    let n978: ZB = zb_not(n977);
    let n979: ZB = zb_and(n976, n978);
    let n980: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n935);
    let n981: ZB = zb_not(n980);
    let n982: ZB = zb_and(n979, n980);
    let n983: ZB = zb_and(n979, n981);
    let n984: ZB = zb_and(n844, n982);
    let n985: ZB = zb_and(n843, n982);
    let n986: ZB = zb_or(n984, n985);
    let n987: ZB = zb_or(n983, n986);
    let n988: ZB = zb_and(n851, n980);
    let n989: ZB = zb_not(n988);
    let n990: ZB = zb_and(n987, n988);
    let n991: ZB = zb_and(n987, n989);
    let n992: ZB = zb_or(n990, n991);
    let n993: ZB = zb_and(n857, n988);
    let n994: ZB = zb_not(n993);
    let n995: ZB = zb_and(n992, n994);
    let n996: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n772);
    let n997: ZB = zn_gt(n996, n776);
    let n998: ZB = zb_and(n756, n997);
    let n999: ZB = zb_or(n933, n995);
    let n1000: ZB = zsel_b(n931, n756, n998);
    let n1001: ZB = zb_or(n866, n999);
    let n1002: ZB = zsel_b(n864, n756, n1000);
    let n1003: ZB = zb_or(n781, n1001);
    let n1004: ZB = zsel_b(n779, n756, n1002);
    let n1005: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n760);
    let n1006: ZB = zn_le(n1005, n764);
    let n1007: ZB = zn_gt(n1005, n764);
    let n1008: ZB = zb_and(n1003, n1006);
    let n1009: ZB = zb_and(n1003, n1007);
    let n1010: ZB = zb_and(n778, n1008);
    let n1011: ZB = zb_and(n779, n1008);
    let n1012: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1005);
    let n1013: ZN = zn_mget(g.cart, n1012, n783);
    let n1014: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1013);
    let n1015: ZB = zb_not(n1014);
    let n1016: ZB = zb_and(n1010, n1014);
    let n1017: ZB = zb_and(n1010, n1015);
    let n1018: ZB = zb_and(n791, n1016);
    let n1019: ZB = zb_and(n790, n1016);
    let n1020: ZB = zb_or(n1018, n1019);
    let n1021: ZB = zb_or(n1017, n1020);
    let n1022: ZB = zb_and(n798, n1014);
    let n1023: ZB = zb_not(n1022);
    let n1024: ZB = zb_and(n1021, n1022);
    let n1025: ZB = zb_and(n1021, n1023);
    let n1026: ZB = zb_or(n1024, n1025);
    let n1027: ZB = zb_and(n804, n1022);
    let n1028: ZB = zb_not(n1027);
    let n1029: ZB = zb_and(n1026, n1028);
    let n1030: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1013);
    let n1031: ZB = zb_not(n1030);
    let n1032: ZB = zb_and(n1029, n1030);
    let n1033: ZB = zb_and(n1029, n1031);
    let n1034: ZB = zb_or(n1032, n1033);
    let n1035: ZB = zb_and(n814, n1030);
    let n1036: ZB = zb_not(n1035);
    let n1037: ZB = zb_and(n1034, n1035);
    let n1038: ZB = zb_and(n1034, n1036);
    let n1039: ZB = zb_or(n1037, n1038);
    let n1040: ZB = zb_and(n1036, n1039);
    let n1041: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1013);
    let n1042: ZB = zb_not(n1041);
    let n1043: ZB = zb_and(n1040, n1041);
    let n1044: ZB = zb_and(n1040, n1042);
    let n1045: ZB = zb_or(n1043, n1044);
    let n1046: ZB = zb_and(n827, n1041);
    let n1047: ZB = zb_not(n1046);
    let n1048: ZB = zb_and(n1045, n1046);
    let n1049: ZB = zb_and(n1045, n1047);
    let n1050: ZB = zb_or(n1048, n1049);
    let n1051: ZB = zb_and(n833, n1046);
    let n1052: ZB = zb_not(n1051);
    let n1053: ZB = zb_and(n1050, n1052);
    let n1054: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1013);
    let n1055: ZB = zb_not(n1054);
    let n1056: ZB = zb_and(n1053, n1054);
    let n1057: ZB = zb_and(n1053, n1055);
    let n1058: ZB = zb_and(n844, n1056);
    let n1059: ZB = zb_and(n843, n1056);
    let n1060: ZN = zn_mul(n1005, zn_splat(P8::from_raw(524288i32)));
    let n1061: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1060);
    let n1062: ZB = zn_eq(n761, n1061);
    let n1063: ZB = zb_or(n1058, n1059);
    let n1064: ZB = zb_or(n843, n1062);
    let n1065: ZB = zb_or(n1057, n1063);
    let n1066: ZB = zb_and(n1054, n1064);
    let n1067: ZB = zb_not(n1066);
    let n1068: ZB = zb_and(n1065, n1066);
    let n1069: ZB = zb_and(n1065, n1067);
    let n1070: ZB = zb_or(n1068, n1069);
    let n1071: ZB = zb_and(n857, n1066);
    let n1072: ZB = zb_not(n1071);
    let n1073: ZB = zb_and(n1070, n1072);
    let n1074: ZB = zb_and(n863, n1073);
    let n1075: ZB = zb_and(n864, n1073);
    let n1076: ZN = zn_mget(g.cart, n1012, n867);
    let n1077: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1076);
    let n1078: ZB = zb_not(n1077);
    let n1079: ZB = zb_and(n1074, n1077);
    let n1080: ZB = zb_and(n1074, n1078);
    let n1081: ZB = zb_and(n791, n1079);
    let n1082: ZB = zb_and(n790, n1079);
    let n1083: ZB = zb_or(n1081, n1082);
    let n1084: ZB = zb_or(n1080, n1083);
    let n1085: ZB = zb_and(n879, n1077);
    let n1086: ZB = zb_not(n1085);
    let n1087: ZB = zb_and(n1084, n1085);
    let n1088: ZB = zb_and(n1084, n1086);
    let n1089: ZB = zb_or(n1087, n1088);
    let n1090: ZB = zb_and(n804, n1085);
    let n1091: ZB = zb_not(n1090);
    let n1092: ZB = zb_and(n1089, n1091);
    let n1093: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1076);
    let n1094: ZB = zb_not(n1093);
    let n1095: ZB = zb_and(n1092, n1093);
    let n1096: ZB = zb_and(n1092, n1094);
    let n1097: ZB = zb_or(n1095, n1096);
    let n1098: ZB = zb_and(n814, n1093);
    let n1099: ZB = zb_not(n1098);
    let n1100: ZB = zb_and(n1097, n1098);
    let n1101: ZB = zb_and(n1097, n1099);
    let n1102: ZB = zb_or(n1100, n1101);
    let n1103: ZB = zb_and(n1099, n1102);
    let n1104: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1076);
    let n1105: ZB = zb_not(n1104);
    let n1106: ZB = zb_and(n1103, n1104);
    let n1107: ZB = zb_and(n1103, n1105);
    let n1108: ZB = zb_or(n1106, n1107);
    let n1109: ZB = zb_and(n827, n1104);
    let n1110: ZB = zb_not(n1109);
    let n1111: ZB = zb_and(n1108, n1109);
    let n1112: ZB = zb_and(n1108, n1110);
    let n1113: ZB = zb_or(n1111, n1112);
    let n1114: ZB = zb_and(n833, n1109);
    let n1115: ZB = zb_not(n1114);
    let n1116: ZB = zb_and(n1113, n1115);
    let n1117: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1076);
    let n1118: ZB = zb_not(n1117);
    let n1119: ZB = zb_and(n1116, n1117);
    let n1120: ZB = zb_and(n1116, n1118);
    let n1121: ZB = zb_and(n844, n1119);
    let n1122: ZB = zb_and(n843, n1119);
    let n1123: ZB = zb_or(n1121, n1122);
    let n1124: ZB = zb_or(n1120, n1123);
    let n1125: ZB = zb_and(n1064, n1117);
    let n1126: ZB = zb_not(n1125);
    let n1127: ZB = zb_and(n1124, n1125);
    let n1128: ZB = zb_and(n1124, n1126);
    let n1129: ZB = zb_or(n1127, n1128);
    let n1130: ZB = zb_and(n857, n1125);
    let n1131: ZB = zb_not(n1130);
    let n1132: ZB = zb_and(n1129, n1131);
    let n1133: ZB = zb_and(n930, n1132);
    let n1134: ZB = zb_and(n931, n1132);
    let n1135: ZN = zn_mget(g.cart, n1012, n934);
    let n1136: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1135);
    let n1137: ZB = zb_not(n1136);
    let n1138: ZB = zb_and(n1133, n1136);
    let n1139: ZB = zb_and(n1133, n1137);
    let n1140: ZB = zb_and(n791, n1138);
    let n1141: ZB = zb_and(n790, n1138);
    let n1142: ZB = zb_or(n1140, n1141);
    let n1143: ZB = zb_or(n1139, n1142);
    let n1144: ZB = zb_and(n946, n1136);
    let n1145: ZB = zb_not(n1144);
    let n1146: ZB = zb_and(n1143, n1144);
    let n1147: ZB = zb_and(n1143, n1145);
    let n1148: ZB = zb_or(n1146, n1147);
    let n1149: ZB = zb_and(n804, n1144);
    let n1150: ZB = zb_not(n1149);
    let n1151: ZB = zb_and(n1148, n1150);
    let n1152: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1135);
    let n1153: ZB = zb_not(n1152);
    let n1154: ZB = zb_and(n1151, n1152);
    let n1155: ZB = zb_and(n1151, n1153);
    let n1156: ZB = zb_or(n1154, n1155);
    let n1157: ZB = zb_and(n814, n1152);
    let n1158: ZB = zb_not(n1157);
    let n1159: ZB = zb_and(n1156, n1157);
    let n1160: ZB = zb_and(n1156, n1158);
    let n1161: ZB = zb_or(n1159, n1160);
    let n1162: ZB = zb_and(n1158, n1161);
    let n1163: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1135);
    let n1164: ZB = zb_not(n1163);
    let n1165: ZB = zb_and(n1162, n1163);
    let n1166: ZB = zb_and(n1162, n1164);
    let n1167: ZB = zb_or(n1165, n1166);
    let n1168: ZB = zb_and(n827, n1163);
    let n1169: ZB = zb_not(n1168);
    let n1170: ZB = zb_and(n1167, n1168);
    let n1171: ZB = zb_and(n1167, n1169);
    let n1172: ZB = zb_or(n1170, n1171);
    let n1173: ZB = zb_and(n833, n1168);
    let n1174: ZB = zb_not(n1173);
    let n1175: ZB = zb_and(n1172, n1174);
    let n1176: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1135);
    let n1177: ZB = zb_not(n1176);
    let n1178: ZB = zb_and(n1175, n1176);
    let n1179: ZB = zb_and(n1175, n1177);
    let n1180: ZB = zb_and(n844, n1178);
    let n1181: ZB = zb_and(n843, n1178);
    let n1182: ZB = zb_or(n1180, n1181);
    let n1183: ZB = zb_or(n1179, n1182);
    let n1184: ZB = zb_and(n1064, n1176);
    let n1185: ZB = zb_not(n1184);
    let n1186: ZB = zb_and(n1183, n1184);
    let n1187: ZB = zb_and(n1183, n1185);
    let n1188: ZB = zb_or(n1186, n1187);
    let n1189: ZB = zb_and(n857, n1184);
    let n1190: ZB = zb_not(n1189);
    let n1191: ZB = zb_and(n1188, n1190);
    let n1192: ZB = zb_and(n997, n1004);
    let n1193: ZB = zb_or(n1134, n1191);
    let n1194: ZB = zsel_b(n931, n1004, n1192);
    let n1195: ZB = zb_or(n1075, n1193);
    let n1196: ZB = zsel_b(n864, n1004, n1194);
    let n1197: ZB = zb_or(n1011, n1195);
    let n1198: ZB = zsel_b(n779, n1004, n1196);
    let n1199: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n760);
    let n1200: ZB = zn_le(n1199, n764);
    let n1201: ZB = zn_gt(n1199, n764);
    let n1202: ZB = zb_and(n1197, n1200);
    let n1203: ZB = zb_and(n1197, n1201);
    let n1204: ZB = zb_and(n778, n1202);
    let n1205: ZB = zb_and(n779, n1202);
    let n1206: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1199);
    let n1207: ZN = zn_mget(g.cart, n1206, n783);
    let n1208: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1207);
    let n1209: ZB = zb_not(n1208);
    let n1210: ZB = zb_and(n1204, n1208);
    let n1211: ZB = zb_and(n1204, n1209);
    let n1212: ZB = zb_and(n791, n1210);
    let n1213: ZB = zb_and(n790, n1210);
    let n1214: ZB = zb_or(n1212, n1213);
    let n1215: ZB = zb_or(n1211, n1214);
    let n1216: ZB = zb_and(n798, n1208);
    let n1217: ZB = zb_not(n1216);
    let n1218: ZB = zb_and(n1215, n1216);
    let n1219: ZB = zb_and(n1215, n1217);
    let n1220: ZB = zb_or(n1218, n1219);
    let n1221: ZB = zb_and(n804, n1216);
    let n1222: ZB = zb_not(n1221);
    let n1223: ZB = zb_and(n1220, n1222);
    let n1224: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1207);
    let n1225: ZB = zb_not(n1224);
    let n1226: ZB = zb_and(n1223, n1224);
    let n1227: ZB = zb_and(n1223, n1225);
    let n1228: ZB = zb_or(n1226, n1227);
    let n1229: ZB = zb_and(n814, n1224);
    let n1230: ZB = zb_not(n1229);
    let n1231: ZB = zb_and(n1228, n1229);
    let n1232: ZB = zb_and(n1228, n1230);
    let n1233: ZB = zb_or(n1231, n1232);
    let n1234: ZB = zb_and(n1230, n1233);
    let n1235: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1207);
    let n1236: ZB = zb_not(n1235);
    let n1237: ZB = zb_and(n1234, n1235);
    let n1238: ZB = zb_and(n1234, n1236);
    let n1239: ZB = zb_or(n1237, n1238);
    let n1240: ZB = zb_and(n827, n1235);
    let n1241: ZB = zb_not(n1240);
    let n1242: ZB = zb_and(n1239, n1240);
    let n1243: ZB = zb_and(n1239, n1241);
    let n1244: ZB = zb_or(n1242, n1243);
    let n1245: ZB = zb_and(n833, n1240);
    let n1246: ZB = zb_not(n1245);
    let n1247: ZB = zb_and(n1244, n1246);
    let n1248: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1207);
    let n1249: ZB = zb_not(n1248);
    let n1250: ZB = zb_and(n1247, n1248);
    let n1251: ZB = zb_and(n1247, n1249);
    let n1252: ZB = zb_and(n844, n1250);
    let n1253: ZB = zb_and(n843, n1250);
    let n1254: ZN = zn_mul(n1199, zn_splat(P8::from_raw(524288i32)));
    let n1255: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1254);
    let n1256: ZB = zn_eq(n761, n1255);
    let n1257: ZB = zb_or(n1252, n1253);
    let n1258: ZB = zb_or(n843, n1256);
    let n1259: ZB = zb_or(n1251, n1257);
    let n1260: ZB = zb_and(n1248, n1258);
    let n1261: ZB = zb_not(n1260);
    let n1262: ZB = zb_and(n1259, n1260);
    let n1263: ZB = zb_and(n1259, n1261);
    let n1264: ZB = zb_or(n1262, n1263);
    let n1265: ZB = zb_and(n857, n1260);
    let n1266: ZB = zb_not(n1265);
    let n1267: ZB = zb_and(n1264, n1266);
    let n1268: ZB = zb_and(n863, n1267);
    let n1269: ZB = zb_and(n864, n1267);
    let n1270: ZN = zn_mget(g.cart, n1206, n867);
    let n1271: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1270);
    let n1272: ZB = zb_not(n1271);
    let n1273: ZB = zb_and(n1268, n1271);
    let n1274: ZB = zb_and(n1268, n1272);
    let n1275: ZB = zb_and(n791, n1273);
    let n1276: ZB = zb_and(n790, n1273);
    let n1277: ZB = zb_or(n1275, n1276);
    let n1278: ZB = zb_or(n1274, n1277);
    let n1279: ZB = zb_and(n879, n1271);
    let n1280: ZB = zb_not(n1279);
    let n1281: ZB = zb_and(n1278, n1279);
    let n1282: ZB = zb_and(n1278, n1280);
    let n1283: ZB = zb_or(n1281, n1282);
    let n1284: ZB = zb_and(n804, n1279);
    let n1285: ZB = zb_not(n1284);
    let n1286: ZB = zb_and(n1283, n1285);
    let n1287: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1270);
    let n1288: ZB = zb_not(n1287);
    let n1289: ZB = zb_and(n1286, n1287);
    let n1290: ZB = zb_and(n1286, n1288);
    let n1291: ZB = zb_or(n1289, n1290);
    let n1292: ZB = zb_and(n814, n1287);
    let n1293: ZB = zb_not(n1292);
    let n1294: ZB = zb_and(n1291, n1292);
    let n1295: ZB = zb_and(n1291, n1293);
    let n1296: ZB = zb_or(n1294, n1295);
    let n1297: ZB = zb_and(n1293, n1296);
    let n1298: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1270);
    let n1299: ZB = zb_not(n1298);
    let n1300: ZB = zb_and(n1297, n1298);
    let n1301: ZB = zb_and(n1297, n1299);
    let n1302: ZB = zb_or(n1300, n1301);
    let n1303: ZB = zb_and(n827, n1298);
    let n1304: ZB = zb_not(n1303);
    let n1305: ZB = zb_and(n1302, n1303);
    let n1306: ZB = zb_and(n1302, n1304);
    let n1307: ZB = zb_or(n1305, n1306);
    let n1308: ZB = zb_and(n833, n1303);
    let n1309: ZB = zb_not(n1308);
    let n1310: ZB = zb_and(n1307, n1309);
    let n1311: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1270);
    let n1312: ZB = zb_not(n1311);
    let n1313: ZB = zb_and(n1310, n1311);
    let n1314: ZB = zb_and(n1310, n1312);
    let n1315: ZB = zb_and(n844, n1313);
    let n1316: ZB = zb_and(n843, n1313);
    let n1317: ZB = zb_or(n1315, n1316);
    let n1318: ZB = zb_or(n1314, n1317);
    let n1319: ZB = zb_and(n1258, n1311);
    let n1320: ZB = zb_not(n1319);
    let n1321: ZB = zb_and(n1318, n1319);
    let n1322: ZB = zb_and(n1318, n1320);
    let n1323: ZB = zb_or(n1321, n1322);
    let n1324: ZB = zb_and(n857, n1319);
    let n1325: ZB = zb_not(n1324);
    let n1326: ZB = zb_and(n1323, n1325);
    let n1327: ZB = zb_and(n930, n1326);
    let n1328: ZB = zb_and(n931, n1326);
    let n1329: ZN = zn_mget(g.cart, n1206, n934);
    let n1330: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1329);
    let n1331: ZB = zb_not(n1330);
    let n1332: ZB = zb_and(n1327, n1330);
    let n1333: ZB = zb_and(n1327, n1331);
    let n1334: ZB = zb_and(n791, n1332);
    let n1335: ZB = zb_and(n790, n1332);
    let n1336: ZB = zb_or(n1334, n1335);
    let n1337: ZB = zb_or(n1333, n1336);
    let n1338: ZB = zb_and(n946, n1330);
    let n1339: ZB = zb_not(n1338);
    let n1340: ZB = zb_and(n1337, n1338);
    let n1341: ZB = zb_and(n1337, n1339);
    let n1342: ZB = zb_or(n1340, n1341);
    let n1343: ZB = zb_and(n804, n1338);
    let n1344: ZB = zb_not(n1343);
    let n1345: ZB = zb_and(n1342, n1344);
    let n1346: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1329);
    let n1347: ZB = zb_not(n1346);
    let n1348: ZB = zb_and(n1345, n1346);
    let n1349: ZB = zb_and(n1345, n1347);
    let n1350: ZB = zb_or(n1348, n1349);
    let n1351: ZB = zb_and(n814, n1346);
    let n1352: ZB = zb_not(n1351);
    let n1353: ZB = zb_and(n1350, n1351);
    let n1354: ZB = zb_and(n1350, n1352);
    let n1355: ZB = zb_or(n1353, n1354);
    let n1356: ZB = zb_and(n1352, n1355);
    let n1357: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1329);
    let n1358: ZB = zb_not(n1357);
    let n1359: ZB = zb_and(n1356, n1357);
    let n1360: ZB = zb_and(n1356, n1358);
    let n1361: ZB = zb_or(n1359, n1360);
    let n1362: ZB = zb_and(n827, n1357);
    let n1363: ZB = zb_not(n1362);
    let n1364: ZB = zb_and(n1361, n1362);
    let n1365: ZB = zb_and(n1361, n1363);
    let n1366: ZB = zb_or(n1364, n1365);
    let n1367: ZB = zb_and(n833, n1362);
    let n1368: ZB = zb_not(n1367);
    let n1369: ZB = zb_and(n1366, n1368);
    let n1370: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1329);
    let n1371: ZB = zb_not(n1370);
    let n1372: ZB = zb_and(n1369, n1370);
    let n1373: ZB = zb_and(n1369, n1371);
    let n1374: ZB = zb_and(n844, n1372);
    let n1375: ZB = zb_and(n843, n1372);
    let n1376: ZB = zb_or(n1374, n1375);
    let n1377: ZB = zb_or(n1373, n1376);
    let n1378: ZB = zb_and(n1258, n1370);
    let n1379: ZB = zb_not(n1378);
    let n1380: ZB = zb_and(n1377, n1378);
    let n1381: ZB = zb_and(n1377, n1379);
    let n1382: ZB = zb_or(n1380, n1381);
    let n1383: ZB = zb_and(n857, n1378);
    let n1384: ZB = zb_not(n1383);
    let n1385: ZB = zb_and(n1382, n1384);
    let n1386: ZB = zb_and(n997, n1198);
    let n1387: ZB = zb_or(n1328, n1385);
    let n1388: ZB = zsel_b(n931, n1198, n1386);
    let n1389: ZB = zb_or(n1269, n1387);
    let n1390: ZB = zsel_b(n864, n1198, n1388);
    let n1391: ZB = zb_or(n1205, n1389);
    let n1392: ZB = zsel_b(n779, n1198, n1390);
    let n1393: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n760);
    let n1394: ZB = zn_gt(n1393, n764);
    let n1395: ZB = zb_and(n1392, n1394);
    let n1396: ZB = zb_or(n1203, n1391);
    let n1397: ZB = zsel_b(n1201, n1198, n1395);
    let n1398: ZB = zb_or(n1009, n1396);
    let n1399: ZB = zsel_b(n1007, n1004, n1397);
    let n1400: ZB = zb_or(n769, n1398);
    let n1401: ZB = zsel_b(n767, n756, n1399);
    let n1402: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n757);
    let n1403: ZB = zn_tile_flag_at(g.cache, g.cart, n506, n1402, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1404: ZB = zb_not(n1403);
    let n1405: ZB = zb_and(n1400, n1404);
    let n1406: ZB = zb_and(n1400, n1403);
    let n1407: ZB = zb_or(n1405, n1406);
    let n1408: ZB = zb_and(n1404, n1407);
    let n1409: ZB = zb_and(n1403, n1407);
    let n1410: ZB = zb_or(n1408, n1409);
    let n1411: ZB = zb_not(r_c292);
    let n1412: ZB = zb_and(n1403, n1410);
    let n1413: ZB = zb_and(n1404, n1410);
    let n1414: ZB = zn_gt(r_c285, zn_splat(P8::from_raw(0i32)));
    let n1415: ZB = zn_le(r_c285, zn_splat(P8::from_raw(0i32)));
    let n1416: ZB = zb_and(n1413, n1414);
    let n1417: ZB = zb_and(n1413, n1415);
    let n1418: ZN = zn_sub(r_c285, zn_splat(P8::from_raw(65536i32)));
    let n1419: ZN = zsel_n(n1414, n1418, r_c285);
    let n1420: ZB = zb_or(n1416, n1417);
    let n1421: ZN = zsel_n(n1403, zn_splat(P8::from_raw(393216i32)), n1419);
    let n1422: ZB = zb_or(n1412, n1420);
    let n1423: ZN = zn_sub(r_c280, zn_splat(P8::from_raw(65536i32)));
    let n1424: ZB = zn_gt(r_c282, zn_splat(P8::from_raw(0i32)));
    let n1425: ZB = zn_le(r_c282, zn_splat(P8::from_raw(0i32)));
    let n1426: ZB = zb_and(n1422, n1424);
    let n1427: ZB = zb_and(n1422, n1425);
    let n1428: ZN = zn_sub(r_c282, zn_splat(P8::from_raw(65536i32)));
    let n1429: ZB = zn_gt(n476, r_c358);
    let n1430: ZB = zn_le(n476, r_c358);
    let n1431: ZB = zb_and(n1426, n1429);
    let n1432: ZB = zb_and(n1426, n1430);
    let n1433: ZN = zn_sub(n476, r_c356);
    let n1434: ZN = zn_max(r_c358, n1433);
    let n1435: ZN = zn_add(r_c356, n476);
    let n1436: ZN = zn_min(r_c358, n1435);
    let n1437: ZN = zsel_n(n1429, n1434, n1436);
    let n1438: ZB = zb_or(n1431, n1432);
    let n1439: ZB = zn_gt(n754, r_c359);
    let n1440: ZB = zn_le(n754, r_c359);
    let n1441: ZB = zb_and(n1438, n1439);
    let n1442: ZB = zb_and(n1438, n1440);
    let n1443: ZN = zn_sub(n754, r_c357);
    let n1444: ZN = zn_max(r_c359, n1443);
    let n1445: ZN = zn_add(r_c357, n754);
    let n1446: ZN = zn_min(r_c359, n1445);
    let n1447: ZN = zsel_n(n1439, n1444, n1446);
    let n1448: ZB = zb_or(n1441, n1442);
    let n1449: ZB = zb_and(n1404, n1427);
    let n1450: ZB = zb_and(n1403, n1427);
    let n1451: ZN = zsel_n(n1404, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1452: ZB = zb_or(n1449, n1450);
    let n1453: ZN = zn_abs(n476);
    let n1454: ZB = zn_gt(n1453, zn_splat(P8::from_raw(65536i32)));
    let n1455: ZB = zn_le(n1453, zn_splat(P8::from_raw(65536i32)));
    let n1456: ZB = zb_and(n1452, n1454);
    let n1457: ZB = zb_and(n1452, n1455);
    let n1458: ZB = zn_gt(n476, zn_splat(P8::from_raw(0i32)));
    let n1459: ZB = zb_and(n1456, n1458);
    let n1460: ZB = zb_and(n833, n1456);
    let n1461: ZB = zn_lt(n476, zn_splat(P8::from_raw(0i32)));
    let n1462: ZB = zb_and(n1460, n1461);
    let n1463: ZB = zb_and(n857, n1460);
    let n1464: ZB = zn_gt(n476, zn_splat(P8::from_raw(65536i32)));
    let n1465: ZB = zn_le(n476, zn_splat(P8::from_raw(65536i32)));
    let n1466: ZB = zb_and(n1459, n1464);
    let n1467: ZB = zb_and(n1459, n1465);
    let n1468: ZN = zn_sub(n476, zn_splat(P8::from_raw(9830i32)));
    let n1469: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1468);
    let n1470: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n476);
    let n1471: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1470);
    let n1472: ZB = zn_gt(n476, zn_splat(P8::from_raw(-65536i32)));
    let n1473: ZB = zn_le(n476, zn_splat(P8::from_raw(-65536i32)));
    let n1474: ZB = zb_and(n1462, n1472);
    let n1475: ZB = zb_and(n1462, n1473);
    let n1476: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1468);
    let n1477: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1470);
    let n1478: ZB = zb_and(n833, n1463);
    let n1479: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1468);
    let n1480: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1470);
    let n1481: ZN = zsel_n(n1472, n1476, n1477);
    let n1482: ZB = zb_or(n1474, n1475);
    let n1483: ZN = zsel_n(n1458, n1479, n1480);
    let n1484: ZN = zsel_n(n1464, n1469, n1471);
    let n1485: ZB = zb_or(n1466, n1467);
    let n1486: ZN = zsel_n(n1461, n1481, n1483);
    let n1487: ZB = zb_or(n1478, n1482);
    let n1488: ZN = zsel_n(n1458, n1484, n1486);
    let n1489: ZB = zb_or(n1485, n1487);
    let n1490: ZB = zb_and(n1457, n1458);
    let n1491: ZB = zb_and(n833, n1457);
    let n1492: ZN = zn_sub(n476, n1451);
    let n1493: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1492);
    let n1494: ZN = zn_add(n476, n1451);
    let n1495: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1494);
    let n1496: ZN = zsel_n(n1458, n1493, n1495);
    let n1497: ZB = zb_or(n1490, n1491);
    let n1498: ZN = zsel_n(n1454, n1488, n1496);
    let n1499: ZB = zb_or(n1489, n1497);
    let n1500: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1498);
    let n1501: ZB = zb_not(n1500);
    let n1502: ZB = zb_and(n1499, n1501);
    let n1503: ZB = zb_and(n1499, n1500);
    let n1504: ZB = zn_lt(n1498, zn_splat(P8::from_raw(0i32)));
    let n1505: ZB = zsel_b(n1501, n1504, r_c360);
    let n1506: ZB = zb_or(n1502, n1503);
    let n1507: ZN = zn_abs(n754);
    let n1508: ZB = zn_le(n1507, zn_splat(P8::from_raw(9830i32)));
    let n1509: ZB = zn_gt(n1507, zn_splat(P8::from_raw(9830i32)));
    let n1510: ZB = zb_and(n1506, n1508);
    let n1511: ZB = zb_and(n1506, n1509);
    let n1512: ZN = zsel_n(n1508, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n1513: ZB = zb_or(n1510, n1511);
    let n1514: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n757);
    let n1515: ZB = zb_and(n1404, n1513);
    let n1516: ZB = zb_and(n1403, n1513);
    let n1517: ZN = zn_add(n754, n1512);
    let n1518: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n1517);
    let n1519: ZN = zsel_n(n1404, n1518, n754);
    let n1520: ZB = zb_or(n1515, n1516);
    let n1521: ZB = zn_gt(n1421, zn_splat(P8::from_raw(0i32)));
    let n1522: ZB = zn_le(n1421, zn_splat(P8::from_raw(0i32)));
    let n1523: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n505);
    let n1524: ZB = zn_tile_flag_at(g.cache, g.cart, n1523, n1514, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1525: ZB = zb_not(n1524);
    let n1526: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n505);
    let n1527: ZB = zn_tile_flag_at(g.cache, g.cart, n1526, n1514, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1528: ZB = zb_not(n1527);
    let n1529: ZN = zsel_n(n1527, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1530: ZN = zsel_n(n1524, zn_splat(P8::from_raw(-65536i32)), n1529);
    let n1531: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1530);
    let n1532: ZB = zb_not(n1531);
    let n1533: ZN = zn_neg(n1530);
    let n1534: ZN = zn_mul(n1533, zn_splat(P8::from_raw(131072i32)));
    let n1535: ZN = zsel_n(n1532, n1534, n1498);
    let n1536: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1519);
    let n1537: ZN = zsel_n(n1521, zn_splat(P8::from_raw(0i32)), n1421);
    let n1538: ZN = zsel_n(n1521, n1498, n1535);
    let n1539: ZN = zsel_n(n1521, zn_splat(P8::from_raw(-131072i32)), n1536);
    let n1540: ZB = zb_not(n1505);
    let n1541: ZN = zsel_n(n1505, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1542: ZB = zn_gt(n1541, zn_splat(P8::from_raw(0i32)));
    let n1543: ZB = zn_le(n1541, zn_splat(P8::from_raw(0i32)));
    let n1544: ZB = zn_lt(n1541, zn_splat(P8::from_raw(0i32)));
    let n1545: ZB = zn_ge(n1541, zn_splat(P8::from_raw(0i32)));
    let n1546: ZN = zsel_n(n1544, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n1547: ZN = zsel_n(n1542, zn_splat(P8::from_raw(131072i32)), n1546);
    let n1548: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1541);
    let n1549: ZB = zb_not(n1548);
    let n1550: ZN = zsel_n(n1549, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n1551: ZN = zsel_n(n1424, n1428, r_c282);
    let n1552: ZB = zsel_b(n1424, r_c360, n1505);
    let n1553: ZN = zsel_n(n1424, n1437, n1498);
    let n1554: ZN = zsel_n(n1424, n1447, n1519);
    let n1555: ZB = zb_or(n1448, n1520);
    let n1556: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n1557: ZB = zb_and(n137, n1555);
    let n1558: ZB = zn_lt(n474, zn_splat(P8::from_raw(-65536i32)));
    let n1559: ZB = zn_ge(n474, zn_splat(P8::from_raw(-65536i32)));
    let n1560: ZB = zb_and(n1557, n1559);
    let n1561: ZB = zb_and(n1557, n1558);
    let n1562: ZB = zn_gt(n474, zn_splat(P8::from_raw(7929856i32)));
    let n1563: ZB = zb_or(n1560, n1561);
    let n1564: ZB = zb_or(n1558, n1562);
    let n1565: ZB = zb_not(n1564);
    let n1566: ZB = zb_and(n1563, n1564);
    let n1567: ZB = zb_and(n1563, n1565);
    let n1568: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n474);
    let n1569: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1568);
    let n1570: ZN = zsel_n(n1564, n1569, n474);
    let n1571: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1553);
    let n1572: ZB = zb_or(n1566, n1567);
    let n1573: ZN = zsel_n(n1556, n474, n1570);
    let n1574: ZN = zsel_n(n1556, n1553, n1571);
    let n1578: ZB = zb_and(n1457, n1472);
    let n1579: ZB = zb_and(n1457, n1473);
    let n1580: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1492);
    let n1581: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1494);
    let n1582: ZN = zsel_n(n1472, n1580, n1581);
    let n1583: ZB = zb_or(n1578, n1579);
    let n1584: ZN = zsel_n(n1454, n1488, n1582);
    let n1585: ZB = zb_or(n1489, n1583);
    let n1586: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1584);
    let n1587: ZB = zb_not(n1586);
    let n1588: ZB = zb_and(n1585, n1587);
    let n1589: ZB = zb_and(n1585, n1586);
    let n1590: ZB = zn_lt(n1584, zn_splat(P8::from_raw(0i32)));
    let n1591: ZB = zsel_b(n1587, n1590, r_c360);
    let n1592: ZB = zb_or(n1588, n1589);
    let n1593: ZB = zb_and(n1508, n1592);
    let n1594: ZB = zb_and(n1509, n1592);
    let n1595: ZB = zb_or(n1593, n1594);
    let n1596: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n505);
    let n1597: ZB = zn_tile_flag_at(g.cache, g.cart, n1596, n1514, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1598: ZB = zb_not(n1597);
    let n1599: ZB = zb_and(n1595, n1598);
    let n1600: ZB = zb_and(n1595, n1597);
    let n1601: ZB = zb_or(n1599, n1600);
    let n1602: ZB = zb_and(n1598, n1601);
    let n1603: ZB = zb_and(n1597, n1601);
    let n1604: ZB = zb_or(n1602, n1603);
    let n1605: ZB = zb_and(n1597, n1604);
    let n1606: ZB = zb_and(n1598, n1604);
    let n1607: ZB = zb_or(n1605, n1606);
    let n1608: ZB = zb_and(n1597, n1607);
    let n1609: ZB = zb_and(n1598, n1607);
    let n1610: ZN = zsel_n(n1597, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1611: ZB = zb_or(n1608, n1609);
    let n1612: ZB = zb_and(n1404, n1611);
    let n1613: ZB = zb_and(n1403, n1611);
    let n1614: ZN = zn_min(n1517, n1610);
    let n1615: ZN = zsel_n(n1404, n1614, n754);
    let n1616: ZB = zb_or(n1612, n1613);
    let n1617: ZN = zsel_n(n1532, n1534, n1584);
    let n1618: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1615);
    let n1619: ZN = zsel_n(n1521, n1584, n1617);
    let n1620: ZN = zsel_n(n1521, zn_splat(P8::from_raw(-131072i32)), n1618);
    let n1621: ZB = zsel_b(n1424, r_c360, n1591);
    let n1622: ZN = zsel_n(n1424, n1437, n1584);
    let n1623: ZN = zsel_n(n1424, n1447, n1615);
    let n1624: ZB = zb_or(n1448, n1616);
    let n1625: ZB = zb_and(n137, n1624);
    let n1626: ZB = zb_and(n1559, n1625);
    let n1627: ZB = zb_and(n1558, n1625);
    let n1628: ZB = zb_or(n1626, n1627);
    let n1629: ZB = zb_and(n1564, n1628);
    let n1630: ZB = zb_and(n1565, n1628);
    let n1631: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1622);
    let n1632: ZB = zb_or(n1629, n1630);
    let n1633: ZN = zsel_n(n1556, n1622, n1631);
    let n1634: ZB = zb_and(n1457, n1464);
    let n1635: ZB = zb_and(n1457, n1465);
    let n1636: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1492);
    let n1637: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1494);
    let n1638: ZN = zsel_n(n1464, n1636, n1637);
    let n1639: ZB = zb_or(n1634, n1635);
    let n1640: ZN = zsel_n(n1454, n1488, n1638);
    let n1641: ZB = zb_or(n1489, n1639);
    let n1642: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1640);
    let n1643: ZB = zb_not(n1642);
    let n1644: ZB = zb_and(n1641, n1643);
    let n1645: ZB = zb_and(n1641, n1642);
    let n1646: ZB = zn_lt(n1640, zn_splat(P8::from_raw(0i32)));
    let n1647: ZB = zsel_b(n1643, n1646, r_c360);
    let n1648: ZB = zb_or(n1644, n1645);
    let n1649: ZB = zb_and(n1508, n1648);
    let n1650: ZB = zb_and(n1509, n1648);
    let n1651: ZB = zb_or(n1649, n1650);
    let n1652: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n505);
    let n1653: ZB = zn_tile_flag_at(g.cache, g.cart, n1652, n1514, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1654: ZB = zb_not(n1653);
    let n1655: ZB = zb_and(n1651, n1654);
    let n1656: ZB = zb_and(n1651, n1653);
    let n1657: ZB = zb_or(n1655, n1656);
    let n1658: ZB = zb_and(n1654, n1657);
    let n1659: ZB = zb_and(n1653, n1657);
    let n1660: ZB = zb_or(n1658, n1659);
    let n1661: ZB = zb_and(n1653, n1660);
    let n1662: ZB = zb_and(n1654, n1660);
    let n1663: ZB = zb_or(n1661, n1662);
    let n1664: ZB = zb_and(n1653, n1663);
    let n1665: ZB = zb_and(n1654, n1663);
    let n1666: ZN = zsel_n(n1653, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1667: ZB = zb_or(n1664, n1665);
    let n1668: ZB = zb_and(n1404, n1667);
    let n1669: ZB = zb_and(n1403, n1667);
    let n1670: ZN = zn_min(n1517, n1666);
    let n1671: ZN = zsel_n(n1404, n1670, n754);
    let n1672: ZB = zb_or(n1668, n1669);
    let n1673: ZN = zsel_n(n1532, n1534, n1640);
    let n1674: ZN = zsel_n(n1532, zn_splat(P8::from_raw(-131072i32)), n1671);
    let n1675: ZN = zsel_n(n1521, n1640, n1673);
    let n1676: ZN = zsel_n(n1521, zn_splat(P8::from_raw(-131072i32)), n1674);
    let n1677: ZB = zsel_b(n1424, r_c360, n1647);
    let n1678: ZN = zsel_n(n1424, n1437, n1640);
    let n1679: ZN = zsel_n(n1424, n1447, n1671);
    let n1680: ZB = zb_or(n1448, n1672);
    let n1681: ZB = zb_and(n137, n1680);
    let n1682: ZB = zb_and(n1559, n1681);
    let n1683: ZB = zb_and(n1558, n1681);
    let n1684: ZB = zb_or(n1682, n1683);
    let n1685: ZB = zb_and(n1564, n1684);
    let n1686: ZB = zb_and(n1565, n1684);
    let n1687: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1678);
    let n1688: ZB = zb_or(n1685, n1686);
    let n1689: ZN = zsel_n(n1556, n1678, n1687);
    let n1690: ZB = zb_and(n136, n1520);
    let n1691: ZB = zb_and(r_c293, n1520);
    let n1692: ZB = zb_and(n1521, n1690);
    let n1693: ZB = zb_and(n1522, n1690);
    let n1694: ZB = zb_and(n1525, n1693);
    let n1695: ZB = zb_and(n1524, n1693);
    let n1696: ZB = zb_or(n1694, n1695);
    let n1697: ZB = zb_and(n1525, n1696);
    let n1698: ZB = zb_and(n1524, n1696);
    let n1699: ZB = zb_or(n1697, n1698);
    let n1700: ZB = zb_and(n1524, n1699);
    let n1701: ZB = zb_and(n1525, n1699);
    let n1702: ZB = zb_and(n1528, n1701);
    let n1703: ZB = zb_and(n1527, n1701);
    let n1704: ZB = zb_or(n1702, n1703);
    let n1705: ZB = zb_and(n1528, n1704);
    let n1706: ZB = zb_and(n1527, n1704);
    let n1707: ZB = zb_or(n1705, n1706);
    let n1708: ZB = zb_and(n1527, n1707);
    let n1709: ZB = zb_and(n1528, n1707);
    let n1710: ZB = zb_or(n1708, n1709);
    let n1711: ZB = zb_or(n1700, n1710);
    let n1712: ZB = zb_and(n1532, n1711);
    let n1713: ZB = zb_and(n1531, n1711);
    let n1714: ZB = zb_or(n1712, n1713);
    let n1715: ZB = zb_or(n1692, n1714);
    let n1716: ZN = zsel_n(n136, n1537, n1421);
    let n1717: ZN = zsel_n(n136, n1538, n1498);
    let n1718: ZN = zsel_n(n136, n1539, n1519);
    let n1719: ZB = zb_or(n1691, n1715);
    let n1720: ZN = zsel_n(n1424, n1421, n1716);
    let n1721: ZN = zsel_n(n1424, n1437, n1717);
    let n1722: ZN = zsel_n(n1424, n1447, n1718);
    let n1723: ZB = zb_or(n1448, n1719);
    let n1724: ZB = zb_and(n137, n1723);
    let n1725: ZB = zb_and(n1559, n1724);
    let n1726: ZB = zb_and(n1558, n1724);
    let n1727: ZB = zb_or(n1725, n1726);
    let n1728: ZB = zb_and(n1564, n1727);
    let n1729: ZB = zb_and(n1565, n1727);
    let n1730: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1721);
    let n1731: ZB = zb_or(n1728, n1729);
    let n1732: ZN = zsel_n(n1556, n1721, n1730);
    let n1733: ZB = zb_and(n136, n1616);
    let n1734: ZB = zb_and(r_c293, n1616);
    let n1735: ZB = zb_and(n1521, n1733);
    let n1736: ZB = zb_and(n1522, n1733);
    let n1737: ZB = zb_and(n1525, n1736);
    let n1738: ZB = zb_and(n1524, n1736);
    let n1739: ZB = zb_or(n1737, n1738);
    let n1740: ZB = zb_and(n1525, n1739);
    let n1741: ZB = zb_and(n1524, n1739);
    let n1742: ZB = zb_or(n1740, n1741);
    let n1743: ZB = zb_and(n1524, n1742);
    let n1744: ZB = zb_and(n1525, n1742);
    let n1745: ZB = zb_and(n1528, n1744);
    let n1746: ZB = zb_and(n1527, n1744);
    let n1747: ZB = zb_or(n1745, n1746);
    let n1748: ZB = zb_and(n1528, n1747);
    let n1749: ZB = zb_and(n1527, n1747);
    let n1750: ZB = zb_or(n1748, n1749);
    let n1751: ZB = zb_and(n1527, n1750);
    let n1752: ZB = zb_and(n1528, n1750);
    let n1753: ZB = zb_or(n1751, n1752);
    let n1754: ZB = zb_or(n1743, n1753);
    let n1755: ZB = zb_and(n1532, n1754);
    let n1756: ZB = zb_and(n1531, n1754);
    let n1757: ZB = zb_or(n1755, n1756);
    let n1758: ZB = zb_or(n1735, n1757);
    let n1759: ZN = zsel_n(n136, n1619, n1584);
    let n1760: ZN = zsel_n(n136, n1620, n1615);
    let n1761: ZB = zb_or(n1734, n1758);
    let n1762: ZN = zsel_n(n1424, n1437, n1759);
    let n1763: ZN = zsel_n(n1424, n1447, n1760);
    let n1764: ZB = zb_or(n1448, n1761);
    let n1765: ZB = zb_and(n137, n1764);
    let n1766: ZB = zb_and(n1559, n1765);
    let n1767: ZB = zb_and(n1558, n1765);
    let n1768: ZB = zb_or(n1766, n1767);
    let n1769: ZB = zb_and(n1564, n1768);
    let n1770: ZB = zb_and(n1565, n1768);
    let n1771: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1762);
    let n1772: ZB = zb_or(n1769, n1770);
    let n1773: ZN = zsel_n(n1556, n1762, n1771);
    let n1774: ZB = zb_and(n136, n1672);
    let n1775: ZB = zb_and(r_c293, n1672);
    let n1776: ZB = zb_and(n1521, n1774);
    let n1777: ZB = zb_and(n1522, n1774);
    let n1778: ZB = zb_and(n1525, n1777);
    let n1779: ZB = zb_and(n1524, n1777);
    let n1780: ZB = zb_or(n1778, n1779);
    let n1781: ZB = zb_and(n1525, n1780);
    let n1782: ZB = zb_and(n1524, n1780);
    let n1783: ZB = zb_or(n1781, n1782);
    let n1784: ZB = zb_and(n1524, n1783);
    let n1785: ZB = zb_and(n1525, n1783);
    let n1786: ZB = zb_and(n1528, n1785);
    let n1787: ZB = zb_and(n1527, n1785);
    let n1788: ZB = zb_or(n1786, n1787);
    let n1789: ZB = zb_and(n1528, n1788);
    let n1790: ZB = zb_and(n1527, n1788);
    let n1791: ZB = zb_or(n1789, n1790);
    let n1792: ZB = zb_and(n1527, n1791);
    let n1793: ZB = zb_and(n1528, n1791);
    let n1794: ZB = zb_or(n1792, n1793);
    let n1795: ZB = zb_or(n1784, n1794);
    let n1796: ZB = zb_and(n1532, n1795);
    let n1797: ZB = zb_and(n1531, n1795);
    let n1798: ZB = zb_or(n1796, n1797);
    let n1799: ZB = zb_or(n1776, n1798);
    let n1800: ZN = zsel_n(n136, n1675, n1640);
    let n1801: ZN = zsel_n(n136, n1676, n1671);
    let n1802: ZB = zb_or(n1775, n1799);
    let n1803: ZN = zsel_n(n1424, n1437, n1800);
    let n1804: ZN = zsel_n(n1424, n1447, n1801);
    let n1805: ZB = zb_or(n1448, n1802);
    let n1806: ZB = zb_and(n137, n1805);
    let n1807: ZB = zb_and(n1559, n1806);
    let n1808: ZB = zb_and(n1558, n1806);
    let n1809: ZB = zb_or(n1807, n1808);
    let n1810: ZB = zb_and(n1564, n1809);
    let n1811: ZB = zb_and(n1565, n1809);
    let n1812: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1803);
    let n1813: ZB = zb_or(n1810, n1811);
    let n1814: ZN = zsel_n(n1556, n1803, n1812);
    let n1815: ZB = zb_and(n1411, n1520);
    let n1816: ZB = zb_and(r_c292, n1520);
    let n1817: ZB = zb_and(n1505, n1815);
    let n1818: ZB = zb_and(n1540, n1815);
    let n1819: ZB = zb_or(n1817, n1818);
    let n1820: ZB = zb_and(n1542, n1819);
    let n1821: ZB = zb_and(n1543, n1819);
    let n1822: ZB = zb_and(n1544, n1821);
    let n1823: ZB = zb_and(n1545, n1821);
    let n1824: ZB = zb_or(n1822, n1823);
    let n1825: ZB = zb_or(n1820, n1824);
    let n1826: ZB = zb_and(n1549, n1825);
    let n1827: ZB = zb_and(n1548, n1825);
    let n1828: ZB = zb_or(n1826, n1827);
    let n1829: ZN = zsel_n(n1411, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n1830: ZB = zb_or(r_c41, n1411);
    let n1831: ZN = zsel_n(n1411, zn_splat(P8::from_raw(655360i32)), n1423);
    let n1832: ZN = zsel_n(n1411, zn_splat(P8::from_raw(262144i32)), r_c282);
    let n1833: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n1834: ZN = zsel_n(n1411, zn_splat(P8::from_raw(98304i32)), r_c356);
    let n1835: ZN = zsel_n(n1411, n1550, r_c357);
    let n1836: ZN = zsel_n(n1411, n1547, r_c358);
    let n1837: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), r_c359);
    let n1838: ZN = zsel_n(n1411, n1541, n1498);
    let n1839: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1519);
    let n1840: ZB = zb_or(n1816, n1828);
    let n1841: ZN = zsel_n(n1424, r_c20, n1829);
    let n1842: ZB = zsel_b(n1424, r_c41, n1830);
    let n1843: ZN = zsel_n(n1424, n1423, n1831);
    let n1844: ZN = zsel_n(n1424, n1428, n1832);
    let n1845: ZN = zsel_n(n1424, zn_splat(P8::from_raw(65536i32)), n1833);
    let n1846: ZN = zsel_n(n1424, r_c356, n1834);
    let n1847: ZN = zsel_n(n1424, r_c357, n1835);
    let n1848: ZN = zsel_n(n1424, r_c358, n1836);
    let n1849: ZN = zsel_n(n1424, r_c359, n1837);
    let n1850: ZN = zsel_n(n1424, n1437, n1838);
    let n1851: ZN = zsel_n(n1424, n1447, n1839);
    let n1852: ZB = zb_or(n1448, n1840);
    let n1853: ZB = zn_gt(n1841, zn_splat(P8::from_raw(0i32)));
    let n1854: ZB = zn_le(n1841, zn_splat(P8::from_raw(0i32)));
    let n1855: ZB = zb_and(n1852, n1853);
    let n1856: ZB = zb_and(n1852, n1854);
    let n1857: ZB = zb_and(n1559, n1856);
    let n1858: ZB = zb_and(n1558, n1856);
    let n1859: ZB = zb_or(n1857, n1858);
    let n1860: ZB = zb_and(n1564, n1859);
    let n1861: ZB = zb_and(n1565, n1859);
    let n1862: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1850);
    let n1863: ZB = zb_or(n1860, n1861);
    let n1864: ZN = zsel_n(n1853, n474, n1570);
    let n1865: ZN = zsel_n(n1853, n1850, n1862);
    let n1866: ZB = zb_or(n1855, n1863);
    let n1867: ZB = zb_and(n1411, n1616);
    let n1868: ZB = zb_and(r_c292, n1616);
    let n1869: ZN = zsel_n(n1411, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n1870: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-131072i32)), r_c358);
    let n1871: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-327680i32)), n1584);
    let n1872: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1615);
    let n1873: ZB = zb_or(n1867, n1868);
    let n1874: ZN = zsel_n(n1424, r_c357, n1869);
    let n1875: ZN = zsel_n(n1424, r_c358, n1870);
    let n1876: ZN = zsel_n(n1424, n1437, n1871);
    let n1877: ZN = zsel_n(n1424, n1447, n1872);
    let n1878: ZB = zb_or(n1448, n1873);
    let n1879: ZB = zb_and(n1853, n1878);
    let n1880: ZB = zb_and(n1854, n1878);
    let n1881: ZB = zb_and(n1559, n1880);
    let n1882: ZB = zb_and(n1558, n1880);
    let n1883: ZB = zb_or(n1881, n1882);
    let n1884: ZB = zb_and(n1564, n1883);
    let n1885: ZB = zb_and(n1565, n1883);
    let n1886: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1876);
    let n1887: ZB = zb_or(n1884, n1885);
    let n1888: ZN = zsel_n(n1853, n1876, n1886);
    let n1889: ZB = zb_or(n1879, n1887);
    let n1890: ZB = zb_and(n1411, n1672);
    let n1891: ZB = zb_and(r_c292, n1672);
    let n1892: ZN = zsel_n(n1411, zn_splat(P8::from_raw(131072i32)), r_c358);
    let n1893: ZN = zsel_n(n1411, zn_splat(P8::from_raw(327680i32)), n1640);
    let n1894: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1671);
    let n1895: ZB = zb_or(n1890, n1891);
    let n1896: ZN = zsel_n(n1424, r_c358, n1892);
    let n1897: ZN = zsel_n(n1424, n1437, n1893);
    let n1898: ZN = zsel_n(n1424, n1447, n1894);
    let n1899: ZB = zb_or(n1448, n1895);
    let n1900: ZB = zb_and(n1853, n1899);
    let n1901: ZB = zb_and(n1854, n1899);
    let n1902: ZB = zb_and(n1559, n1901);
    let n1903: ZB = zb_and(n1558, n1901);
    let n1904: ZB = zb_or(n1902, n1903);
    let n1905: ZB = zb_and(n1564, n1904);
    let n1906: ZB = zb_and(n1565, n1904);
    let n1907: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1897);
    let n1908: ZB = zb_or(n1905, n1906);
    let n1909: ZN = zsel_n(n1853, n1897, n1907);
    let n1910: ZB = zb_or(n1900, n1908);
    let n1912: ZN = zsel_n(n1411, zn_splat(P8::from_raw(69510i32)), r_c356);
    let n1913: ZN = zsel_n(n1411, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n1914: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), r_c358);
    let n1915: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-98304i32)), r_c359);
    let n1916: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1498);
    let n1917: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-327680i32)), n1519);
    let n1918: ZB = zb_or(n1815, n1816);
    let n1919: ZN = zsel_n(n1424, r_c356, n1912);
    let n1920: ZN = zsel_n(n1424, r_c357, n1913);
    let n1921: ZN = zsel_n(n1424, r_c358, n1914);
    let n1922: ZN = zsel_n(n1424, r_c359, n1915);
    let n1923: ZN = zsel_n(n1424, n1437, n1916);
    let n1924: ZN = zsel_n(n1424, n1447, n1917);
    let n1925: ZB = zb_or(n1448, n1918);
    let n1926: ZB = zb_and(n1853, n1925);
    let n1927: ZB = zb_and(n1854, n1925);
    let n1928: ZB = zb_and(n1559, n1927);
    let n1929: ZB = zb_and(n1558, n1927);
    let n1930: ZB = zb_or(n1928, n1929);
    let n1931: ZB = zb_and(n1564, n1930);
    let n1932: ZB = zb_and(n1565, n1930);
    let n1933: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1923);
    let n1934: ZB = zb_or(n1931, n1932);
    let n1935: ZN = zsel_n(n1853, n1923, n1933);
    let n1936: ZB = zb_or(n1926, n1934);
    let n1937: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1584);
    let n1938: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1615);
    let n1939: ZN = zsel_n(n1424, n1437, n1937);
    let n1940: ZN = zsel_n(n1424, n1447, n1938);
    let n1941: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1939);
    let n1942: ZN = zsel_n(n1853, n1939, n1941);
    let n1943: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1640);
    let n1944: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1671);
    let n1945: ZN = zsel_n(n1424, n1437, n1943);
    let n1946: ZN = zsel_n(n1424, n1447, n1944);
    let n1947: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1945);
    let n1948: ZN = zsel_n(n1853, n1945, n1947);
    let n1949: ZN = zsel_n(n1411, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n1950: ZN = zsel_n(n1411, zn_splat(P8::from_raw(327680i32)), n1519);
    let n1951: ZN = zsel_n(n1424, r_c359, n1949);
    let n1952: ZN = zsel_n(n1424, n1447, n1950);
    let n1953: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1615);
    let n1954: ZN = zsel_n(n1424, n1447, n1953);
    let n1955: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1671);
    let n1956: ZN = zsel_n(n1424, n1447, n1955);
    let n1957: ZB = zb_and(n1411, n1719);
    let n1958: ZB = zb_and(r_c292, n1719);
    let n1959: ZB = zb_and(n1505, n1957);
    let n1960: ZB = zb_and(n1540, n1957);
    let n1961: ZB = zb_or(n1959, n1960);
    let n1962: ZB = zb_and(n1542, n1961);
    let n1963: ZB = zb_and(n1543, n1961);
    let n1964: ZB = zb_and(n1544, n1963);
    let n1965: ZB = zb_and(n1545, n1963);
    let n1966: ZB = zb_or(n1964, n1965);
    let n1967: ZB = zb_or(n1962, n1966);
    let n1968: ZB = zb_and(n1549, n1967);
    let n1969: ZB = zb_and(n1548, n1967);
    let n1970: ZB = zb_or(n1968, n1969);
    let n1971: ZN = zsel_n(n1411, n1541, n1717);
    let n1972: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1718);
    let n1973: ZB = zb_or(n1958, n1970);
    let n1974: ZN = zsel_n(n1424, n1437, n1971);
    let n1975: ZN = zsel_n(n1424, n1447, n1972);
    let n1976: ZB = zb_or(n1448, n1973);
    let n1977: ZB = zb_and(n1853, n1976);
    let n1978: ZB = zb_and(n1854, n1976);
    let n1979: ZB = zb_and(n1559, n1978);
    let n1980: ZB = zb_and(n1558, n1978);
    let n1981: ZB = zb_or(n1979, n1980);
    let n1982: ZB = zb_and(n1564, n1981);
    let n1983: ZB = zb_and(n1565, n1981);
    let n1984: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1974);
    let n1985: ZB = zb_or(n1982, n1983);
    let n1986: ZN = zsel_n(n1853, n1974, n1984);
    let n1987: ZB = zb_or(n1977, n1985);
    let n1988: ZB = zb_and(n1411, n1761);
    let n1989: ZB = zb_and(r_c292, n1761);
    let n1990: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-327680i32)), n1759);
    let n1991: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1760);
    let n1992: ZB = zb_or(n1988, n1989);
    let n1993: ZN = zsel_n(n1424, n1437, n1990);
    let n1994: ZN = zsel_n(n1424, n1447, n1991);
    let n1995: ZB = zb_or(n1448, n1992);
    let n1996: ZB = zb_and(n1853, n1995);
    let n1997: ZB = zb_and(n1854, n1995);
    let n1998: ZB = zb_and(n1559, n1997);
    let n1999: ZB = zb_and(n1558, n1997);
    let n2000: ZB = zb_or(n1998, n1999);
    let n2001: ZB = zb_and(n1564, n2000);
    let n2002: ZB = zb_and(n1565, n2000);
    let n2003: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n1993);
    let n2004: ZB = zb_or(n2001, n2002);
    let n2005: ZN = zsel_n(n1853, n1993, n2003);
    let n2006: ZB = zb_or(n1996, n2004);
    let n2007: ZB = zb_and(n1411, n1802);
    let n2008: ZB = zb_and(r_c292, n1802);
    let n2009: ZN = zsel_n(n1411, zn_splat(P8::from_raw(327680i32)), n1800);
    let n2010: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1801);
    let n2011: ZB = zb_or(n2007, n2008);
    let n2012: ZN = zsel_n(n1424, n1437, n2009);
    let n2013: ZN = zsel_n(n1424, n1447, n2010);
    let n2014: ZB = zb_or(n1448, n2011);
    let n2015: ZB = zb_and(n1853, n2014);
    let n2016: ZB = zb_and(n1854, n2014);
    let n2017: ZB = zb_and(n1559, n2016);
    let n2018: ZB = zb_and(n1558, n2016);
    let n2019: ZB = zb_or(n2017, n2018);
    let n2020: ZB = zb_and(n1564, n2019);
    let n2021: ZB = zb_and(n1565, n2019);
    let n2022: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n2012);
    let n2023: ZB = zb_or(n2020, n2021);
    let n2024: ZN = zsel_n(n1853, n2012, n2022);
    let n2025: ZB = zb_or(n2015, n2023);
    let n2026: ZN = zsel_n(n1411, zn_splat(P8::from_raw(0i32)), n1717);
    let n2027: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-327680i32)), n1718);
    let n2028: ZB = zb_or(n1957, n1958);
    let n2029: ZN = zsel_n(n1424, n1437, n2026);
    let n2030: ZN = zsel_n(n1424, n1447, n2027);
    let n2031: ZB = zb_or(n1448, n2028);
    let n2032: ZB = zb_and(n1853, n2031);
    let n2033: ZB = zb_and(n1854, n2031);
    let n2034: ZB = zb_and(n1559, n2033);
    let n2035: ZB = zb_and(n1558, n2033);
    let n2036: ZB = zb_or(n2034, n2035);
    let n2037: ZB = zb_and(n1564, n2036);
    let n2038: ZB = zb_and(n1565, n2036);
    let n2039: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n2029);
    let n2040: ZB = zb_or(n2037, n2038);
    let n2041: ZN = zsel_n(n1853, n2029, n2039);
    let n2042: ZB = zb_or(n2032, n2040);
    let n2043: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1759);
    let n2044: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1760);
    let n2045: ZN = zsel_n(n1424, n1437, n2043);
    let n2046: ZN = zsel_n(n1424, n1447, n2044);
    let n2047: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n2045);
    let n2048: ZN = zsel_n(n1853, n2045, n2047);
    let n2049: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1800);
    let n2050: ZN = zsel_n(n1411, zn_splat(P8::from_raw(-231700i32)), n1801);
    let n2051: ZN = zsel_n(n1424, n1437, n2049);
    let n2052: ZN = zsel_n(n1424, n1447, n2050);
    let n2053: ZN = zsel_n(n1564, zn_splat(P8::from_raw(0i32)), n2051);
    let n2054: ZN = zsel_n(n1853, n2051, n2053);
    let n2055: ZN = zsel_n(n1411, zn_splat(P8::from_raw(327680i32)), n1718);
    let n2056: ZN = zsel_n(n1424, n1447, n2055);
    let n2057: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1760);
    let n2058: ZN = zsel_n(n1424, n1447, n2057);
    let n2059: ZN = zsel_n(n1411, zn_splat(P8::from_raw(231700i32)), n1801);
    let n2060: ZN = zsel_n(n1424, n1447, n2059);
    let n2061: ZB = zb_not(n227);
    let n2062: ZB = zb_and(n137, n2061);
    let n2063: ZB = zn_lt(r_c369, zn_splat(P8::from_raw(0i32)));
    let n2064: ZB = zb_and(n228, n2063);
    let n2065: ZB = zb_or(n2062, n2064);
    let n2066: ZB = zb_and(n232, n2065);
    let n2067: ZB = zb_and(n233, n2065);
    let n2068: ZB = zb_or(n2066, n2067);
    let n2069: ZB = zb_and(n222, n232);
    let n2070: ZB = zb_not(n2069);
    let n2071: ZB = zb_and(n2068, n2069);
    let n2072: ZB = zb_and(n2068, n2070);
    let n2073: ZB = zb_or(n2071, n2072);
    let n2074: ZB = zb_and(n239, n2069);
    let n2075: ZB = zb_not(n2074);
    let n2076: ZB = zb_and(n2073, n2074);
    let n2077: ZB = zb_and(n2073, n2075);
    let n2078: ZB = zb_or(n2076, n2077);
    let n2079: ZB = zb_and(n226, n2074);
    let n2080: ZB = zb_and(n2078, n2079);
    let n2081: ZB = zb_and(n229, n2080);
    let n2082: ZB = zb_and(n249, n2081);
    let n2083: ZB = zb_and(n250, n2081);
    let n2084: ZB = zb_or(n2082, n2083);
    let n2085: ZB = zb_and(n259, n2084);
    let n2086: ZB = zb_and(n260, n2084);
    let n2087: ZB = zb_and(n263, n2086);
    let n2088: ZB = zb_and(n264, n2086);
    let n2089: ZB = zb_or(n2087, n2088);
    let n2090: ZB = zb_or(n2085, n2089);
    let n2091: ZB = zb_and(n274, n2090);
    let n2092: ZB = zb_and(n273, n2090);
    let n2093: ZB = zb_or(n2091, n2092);
    let n2094: ZB = zb_and(n274, n2093);
    let n2095: ZB = zb_and(n273, n2093);
    let n2096: ZB = zb_or(n2094, n2095);
    let n2097: ZB = zb_and(n274, n2096);
    let n2098: ZB = zb_and(n273, n2096);
    let n2099: ZB = zb_and(n284, n2097);
    let n2100: ZB = zb_and(n285, n2097);
    let n2101: ZB = zb_and(n291, n2099);
    let n2102: ZB = zb_and(n290, n2099);
    let n2103: ZB = zb_or(n2101, n2102);
    let n2104: ZB = zb_and(n291, n2103);
    let n2105: ZB = zb_and(n290, n2103);
    let n2106: ZB = zb_or(n2104, n2105);
    let n2107: ZB = zb_and(n291, n2106);
    let n2108: ZB = zb_and(n290, n2106);
    let n2109: ZB = zb_and(n301, n2107);
    let n2110: ZB = zb_and(n302, n2107);
    let n2111: ZB = zb_and(n308, n2109);
    let n2112: ZB = zb_and(n307, n2109);
    let n2113: ZB = zb_or(n2111, n2112);
    let n2114: ZB = zb_and(n308, n2113);
    let n2115: ZB = zb_and(n307, n2113);
    let n2116: ZB = zb_or(n2114, n2115);
    let n2117: ZB = zb_and(n308, n2116);
    let n2118: ZB = zb_and(n307, n2116);
    let n2119: ZB = zb_and(n318, n2117);
    let n2120: ZB = zb_and(n319, n2117);
    let n2121: ZB = zb_and(n325, n2119);
    let n2122: ZB = zb_and(n324, n2119);
    let n2123: ZB = zb_or(n2121, n2122);
    let n2124: ZB = zb_and(n325, n2123);
    let n2125: ZB = zb_and(n324, n2123);
    let n2126: ZB = zb_or(n2124, n2125);
    let n2127: ZB = zb_and(n325, n2126);
    let n2128: ZB = zb_and(n324, n2126);
    let n2129: ZB = zb_and(n335, n2127);
    let n2130: ZB = zb_and(n336, n2127);
    let n2131: ZB = zb_and(n342, n2129);
    let n2132: ZB = zb_and(n341, n2129);
    let n2133: ZB = zb_or(n2131, n2132);
    let n2134: ZB = zb_and(n342, n2133);
    let n2135: ZB = zb_and(n341, n2133);
    let n2136: ZB = zb_or(n2134, n2135);
    let n2137: ZB = zb_and(n342, n2136);
    let n2138: ZB = zb_and(n341, n2136);
    let n2139: ZB = zb_and(n352, n2137);
    let n2140: ZB = zb_and(n353, n2137);
    let n2141: ZB = zb_and(n359, n2139);
    let n2142: ZB = zb_and(n358, n2139);
    let n2143: ZB = zb_or(n2141, n2142);
    let n2144: ZB = zb_and(n359, n2143);
    let n2145: ZB = zb_and(n358, n2143);
    let n2146: ZB = zb_or(n2144, n2145);
    let n2147: ZB = zb_and(n359, n2146);
    let n2148: ZB = zb_and(n358, n2146);
    let n2149: ZB = zb_and(n369, n2147);
    let n2150: ZB = zb_and(n370, n2147);
    let n2151: ZB = zb_and(n376, n2149);
    let n2152: ZB = zb_and(n375, n2149);
    let n2153: ZB = zb_or(n2151, n2152);
    let n2154: ZB = zb_and(n376, n2153);
    let n2155: ZB = zb_and(n375, n2153);
    let n2156: ZB = zb_or(n2154, n2155);
    let n2157: ZB = zb_and(n376, n2156);
    let n2158: ZB = zb_and(n375, n2156);
    let n2159: ZB = zb_and(n386, n2157);
    let n2160: ZB = zb_and(n387, n2157);
    let n2161: ZB = zb_and(n393, n2159);
    let n2162: ZB = zb_and(n392, n2159);
    let n2163: ZB = zb_or(n2161, n2162);
    let n2164: ZB = zb_and(n393, n2163);
    let n2165: ZB = zb_and(n392, n2163);
    let n2166: ZB = zb_or(n2164, n2165);
    let n2167: ZB = zb_and(n393, n2166);
    let n2168: ZB = zb_and(n392, n2166);
    let n2169: ZB = zb_or(n2167, n2168);
    let n2170: ZB = zb_or(n2160, n2169);
    let n2171: ZB = zb_or(n2158, n2170);
    let n2172: ZB = zb_or(n2150, n2171);
    let n2173: ZB = zb_or(n2148, n2172);
    let n2174: ZB = zb_or(n2140, n2173);
    let n2175: ZB = zb_or(n2138, n2174);
    let n2176: ZB = zb_or(n2130, n2175);
    let n2177: ZB = zb_or(n2128, n2176);
    let n2178: ZB = zb_or(n2120, n2177);
    let n2179: ZB = zb_or(n2118, n2178);
    let n2180: ZB = zb_or(n2110, n2179);
    let n2181: ZB = zb_or(n2108, n2180);
    let n2182: ZB = zb_or(n2100, n2181);
    let n2183: ZB = zb_or(n2098, n2182);
    let n2184: ZB = zb_and(n484, n2183);
    let n2185: ZB = zb_and(n485, n2183);
    let n2186: ZB = zb_and(n488, n2185);
    let n2187: ZB = zb_and(n489, n2185);
    let n2188: ZB = zb_or(n2186, n2187);
    let n2189: ZB = zb_or(n2184, n2188);
    let n2190: ZB = zb_and(n497, n2189);
    let n2191: ZB = zb_and(n498, n2189);
    let n2192: ZB = zb_or(n2190, n2191);
    let n2193: ZB = zb_and(n497, n2192);
    let n2194: ZB = zb_and(n498, n2192);
    let n2195: ZB = zb_or(n2193, n2194);
    let n2196: ZB = zb_and(n509, n2195);
    let n2197: ZB = zb_and(n508, n2195);
    let n2198: ZB = zb_or(n2196, n2197);
    let n2199: ZB = zb_and(n509, n2198);
    let n2200: ZB = zb_and(n508, n2198);
    let n2201: ZB = zb_or(n2199, n2200);
    let n2202: ZB = zb_and(n509, n2201);
    let n2203: ZB = zb_and(n508, n2201);
    let n2204: ZB = zb_and(n519, n2202);
    let n2205: ZB = zb_and(n520, n2202);
    let n2206: ZB = zb_and(n497, n2204);
    let n2207: ZB = zb_and(n498, n2204);
    let n2208: ZB = zb_or(n2206, n2207);
    let n2209: ZB = zb_and(n497, n2208);
    let n2210: ZB = zb_and(n498, n2208);
    let n2211: ZB = zb_or(n2209, n2210);
    let n2212: ZB = zb_and(n532, n2211);
    let n2213: ZB = zb_and(n531, n2211);
    let n2214: ZB = zb_or(n2212, n2213);
    let n2215: ZB = zb_and(n532, n2214);
    let n2216: ZB = zb_and(n531, n2214);
    let n2217: ZB = zb_or(n2215, n2216);
    let n2218: ZB = zb_and(n532, n2217);
    let n2219: ZB = zb_and(n531, n2217);
    let n2220: ZB = zb_and(n542, n2218);
    let n2221: ZB = zb_and(n543, n2218);
    let n2222: ZB = zb_and(n497, n2220);
    let n2223: ZB = zb_and(n498, n2220);
    let n2224: ZB = zb_or(n2222, n2223);
    let n2225: ZB = zb_and(n497, n2224);
    let n2226: ZB = zb_and(n498, n2224);
    let n2227: ZB = zb_or(n2225, n2226);
    let n2228: ZB = zb_and(n555, n2227);
    let n2229: ZB = zb_and(n554, n2227);
    let n2230: ZB = zb_or(n2228, n2229);
    let n2231: ZB = zb_and(n555, n2230);
    let n2232: ZB = zb_and(n554, n2230);
    let n2233: ZB = zb_or(n2231, n2232);
    let n2234: ZB = zb_and(n555, n2233);
    let n2235: ZB = zb_and(n554, n2233);
    let n2236: ZB = zb_and(n565, n2234);
    let n2237: ZB = zb_and(n566, n2234);
    let n2238: ZB = zb_and(n497, n2236);
    let n2239: ZB = zb_and(n498, n2236);
    let n2240: ZB = zb_or(n2238, n2239);
    let n2241: ZB = zb_and(n497, n2240);
    let n2242: ZB = zb_and(n498, n2240);
    let n2243: ZB = zb_or(n2241, n2242);
    let n2244: ZB = zb_and(n578, n2243);
    let n2245: ZB = zb_and(n577, n2243);
    let n2246: ZB = zb_or(n2244, n2245);
    let n2247: ZB = zb_and(n578, n2246);
    let n2248: ZB = zb_and(n577, n2246);
    let n2249: ZB = zb_or(n2247, n2248);
    let n2250: ZB = zb_and(n578, n2249);
    let n2251: ZB = zb_and(n577, n2249);
    let n2252: ZB = zb_and(n588, n2250);
    let n2253: ZB = zb_and(n589, n2250);
    let n2254: ZB = zb_and(n497, n2252);
    let n2255: ZB = zb_and(n498, n2252);
    let n2256: ZB = zb_or(n2254, n2255);
    let n2257: ZB = zb_and(n497, n2256);
    let n2258: ZB = zb_and(n498, n2256);
    let n2259: ZB = zb_or(n2257, n2258);
    let n2260: ZB = zb_and(n601, n2259);
    let n2261: ZB = zb_and(n600, n2259);
    let n2262: ZB = zb_or(n2260, n2261);
    let n2263: ZB = zb_and(n601, n2262);
    let n2264: ZB = zb_and(n600, n2262);
    let n2265: ZB = zb_or(n2263, n2264);
    let n2266: ZB = zb_and(n601, n2265);
    let n2267: ZB = zb_and(n600, n2265);
    let n2268: ZB = zb_and(n611, n2266);
    let n2269: ZB = zb_and(n612, n2266);
    let n2270: ZB = zb_and(n497, n2268);
    let n2271: ZB = zb_and(n498, n2268);
    let n2272: ZB = zb_or(n2270, n2271);
    let n2273: ZB = zb_and(n497, n2272);
    let n2274: ZB = zb_and(n498, n2272);
    let n2275: ZB = zb_or(n2273, n2274);
    let n2276: ZB = zb_and(n624, n2275);
    let n2277: ZB = zb_and(n623, n2275);
    let n2278: ZB = zb_or(n2276, n2277);
    let n2279: ZB = zb_and(n624, n2278);
    let n2280: ZB = zb_and(n623, n2278);
    let n2281: ZB = zb_or(n2279, n2280);
    let n2282: ZB = zb_and(n624, n2281);
    let n2283: ZB = zb_and(n623, n2281);
    let n2284: ZB = zb_and(n634, n2282);
    let n2285: ZB = zb_and(n635, n2282);
    let n2286: ZB = zb_and(n497, n2284);
    let n2287: ZB = zb_and(n498, n2284);
    let n2288: ZB = zb_or(n2286, n2287);
    let n2289: ZB = zb_and(n497, n2288);
    let n2290: ZB = zb_and(n498, n2288);
    let n2291: ZB = zb_or(n2289, n2290);
    let n2292: ZB = zb_and(n647, n2291);
    let n2293: ZB = zb_and(n646, n2291);
    let n2294: ZB = zb_or(n2292, n2293);
    let n2295: ZB = zb_and(n647, n2294);
    let n2296: ZB = zb_and(n646, n2294);
    let n2297: ZB = zb_or(n2295, n2296);
    let n2298: ZB = zb_and(n647, n2297);
    let n2299: ZB = zb_and(n646, n2297);
    let n2300: ZB = zb_and(n657, n2298);
    let n2301: ZB = zb_and(n658, n2298);
    let n2302: ZB = zb_and(n497, n2300);
    let n2303: ZB = zb_and(n498, n2300);
    let n2304: ZB = zb_or(n2302, n2303);
    let n2305: ZB = zb_and(n497, n2304);
    let n2306: ZB = zb_and(n498, n2304);
    let n2307: ZB = zb_or(n2305, n2306);
    let n2308: ZB = zb_and(n670, n2307);
    let n2309: ZB = zb_and(n669, n2307);
    let n2310: ZB = zb_or(n2308, n2309);
    let n2311: ZB = zb_and(n670, n2310);
    let n2312: ZB = zb_and(n669, n2310);
    let n2313: ZB = zb_or(n2311, n2312);
    let n2314: ZB = zb_and(n670, n2313);
    let n2315: ZB = zb_and(n669, n2313);
    let n2316: ZB = zb_or(n2314, n2315);
    let n2317: ZB = zb_or(n2301, n2316);
    let n2318: ZB = zb_or(n2299, n2317);
    let n2319: ZB = zb_or(n2285, n2318);
    let n2320: ZB = zb_or(n2283, n2319);
    let n2321: ZB = zb_or(n2269, n2320);
    let n2322: ZB = zb_or(n2267, n2321);
    let n2323: ZB = zb_or(n2253, n2322);
    let n2324: ZB = zb_or(n2251, n2323);
    let n2325: ZB = zb_or(n2237, n2324);
    let n2326: ZB = zb_or(n2235, n2325);
    let n2327: ZB = zb_or(n2221, n2326);
    let n2328: ZB = zb_or(n2219, n2327);
    let n2329: ZB = zb_or(n2205, n2328);
    let n2330: ZB = zb_or(n2203, n2329);
    let n2331: ZB = zb_and(n766, n2330);
    let n2332: ZB = zb_and(n767, n2330);
    let n2333: ZB = zb_and(n778, n2331);
    let n2334: ZB = zb_and(n779, n2331);
    let n2335: ZB = zb_and(n785, n2333);
    let n2336: ZB = zb_and(n786, n2333);
    let n2337: ZB = zb_and(n791, n2335);
    let n2338: ZB = zb_and(n790, n2335);
    let n2339: ZB = zb_or(n2337, n2338);
    let n2340: ZB = zb_or(n2336, n2339);
    let n2341: ZB = zb_and(n800, n2340);
    let n2342: ZB = zb_and(n801, n2340);
    let n2343: ZB = zb_or(n2341, n2342);
    let n2344: ZB = zb_and(n807, n2343);
    let n2345: ZB = zb_and(n809, n2344);
    let n2346: ZB = zb_and(n810, n2344);
    let n2347: ZB = zb_or(n2345, n2346);
    let n2348: ZB = zb_and(n816, n2347);
    let n2349: ZB = zb_and(n817, n2347);
    let n2350: ZB = zb_or(n2348, n2349);
    let n2351: ZB = zb_and(n817, n2350);
    let n2352: ZB = zb_and(n822, n2351);
    let n2353: ZB = zb_and(n823, n2351);
    let n2354: ZB = zb_or(n2352, n2353);
    let n2355: ZB = zb_and(n829, n2354);
    let n2356: ZB = zb_and(n830, n2354);
    let n2357: ZB = zb_or(n2355, n2356);
    let n2358: ZB = zb_and(n836, n2357);
    let n2359: ZB = zb_and(n838, n2358);
    let n2360: ZB = zb_and(n839, n2358);
    let n2361: ZB = zb_and(n844, n2359);
    let n2362: ZB = zb_and(n843, n2359);
    let n2363: ZB = zb_or(n2361, n2362);
    let n2364: ZB = zb_or(n2360, n2363);
    let n2365: ZB = zb_and(n853, n2364);
    let n2366: ZB = zb_and(n854, n2364);
    let n2367: ZB = zb_or(n2365, n2366);
    let n2368: ZB = zb_and(n860, n2367);
    let n2369: ZB = zb_and(n863, n2368);
    let n2370: ZB = zb_and(n864, n2368);
    let n2371: ZB = zb_and(n869, n2369);
    let n2372: ZB = zb_and(n870, n2369);
    let n2373: ZB = zb_and(n791, n2371);
    let n2374: ZB = zb_and(n790, n2371);
    let n2375: ZB = zb_or(n2373, n2374);
    let n2376: ZB = zb_or(n2372, n2375);
    let n2377: ZB = zb_and(n881, n2376);
    let n2378: ZB = zb_and(n882, n2376);
    let n2379: ZB = zb_or(n2377, n2378);
    let n2380: ZB = zb_and(n887, n2379);
    let n2381: ZB = zb_and(n889, n2380);
    let n2382: ZB = zb_and(n890, n2380);
    let n2383: ZB = zb_or(n2381, n2382);
    let n2384: ZB = zb_and(n894, n2383);
    let n2385: ZB = zb_and(n895, n2383);
    let n2386: ZB = zb_or(n2384, n2385);
    let n2387: ZB = zb_and(n895, n2386);
    let n2388: ZB = zb_and(n900, n2387);
    let n2389: ZB = zb_and(n901, n2387);
    let n2390: ZB = zb_or(n2388, n2389);
    let n2391: ZB = zb_and(n905, n2390);
    let n2392: ZB = zb_and(n906, n2390);
    let n2393: ZB = zb_or(n2391, n2392);
    let n2394: ZB = zb_and(n911, n2393);
    let n2395: ZB = zb_and(n913, n2394);
    let n2396: ZB = zb_and(n914, n2394);
    let n2397: ZB = zb_and(n844, n2395);
    let n2398: ZB = zb_and(n843, n2395);
    let n2399: ZB = zb_or(n2397, n2398);
    let n2400: ZB = zb_or(n2396, n2399);
    let n2401: ZB = zb_and(n921, n2400);
    let n2402: ZB = zb_and(n922, n2400);
    let n2403: ZB = zb_or(n2401, n2402);
    let n2404: ZB = zb_and(n927, n2403);
    let n2405: ZB = zb_and(n930, n2404);
    let n2406: ZB = zb_and(n931, n2404);
    let n2407: ZB = zb_and(n936, n2405);
    let n2408: ZB = zb_and(n937, n2405);
    let n2409: ZB = zb_and(n791, n2407);
    let n2410: ZB = zb_and(n790, n2407);
    let n2411: ZB = zb_or(n2409, n2410);
    let n2412: ZB = zb_or(n2408, n2411);
    let n2413: ZB = zb_and(n948, n2412);
    let n2414: ZB = zb_and(n949, n2412);
    let n2415: ZB = zb_or(n2413, n2414);
    let n2416: ZB = zb_and(n954, n2415);
    let n2417: ZB = zb_and(n956, n2416);
    let n2418: ZB = zb_and(n957, n2416);
    let n2419: ZB = zb_or(n2417, n2418);
    let n2420: ZB = zb_and(n961, n2419);
    let n2421: ZB = zb_and(n962, n2419);
    let n2422: ZB = zb_or(n2420, n2421);
    let n2423: ZB = zb_and(n962, n2422);
    let n2424: ZB = zb_and(n967, n2423);
    let n2425: ZB = zb_and(n968, n2423);
    let n2426: ZB = zb_or(n2424, n2425);
    let n2427: ZB = zb_and(n972, n2426);
    let n2428: ZB = zb_and(n973, n2426);
    let n2429: ZB = zb_or(n2427, n2428);
    let n2430: ZB = zb_and(n978, n2429);
    let n2431: ZB = zb_and(n980, n2430);
    let n2432: ZB = zb_and(n981, n2430);
    let n2433: ZB = zb_and(n844, n2431);
    let n2434: ZB = zb_and(n843, n2431);
    let n2435: ZB = zb_or(n2433, n2434);
    let n2436: ZB = zb_or(n2432, n2435);
    let n2437: ZB = zb_and(n988, n2436);
    let n2438: ZB = zb_and(n989, n2436);
    let n2439: ZB = zb_or(n2437, n2438);
    let n2440: ZB = zb_and(n994, n2439);
    let n2441: ZB = zb_or(n2406, n2440);
    let n2442: ZB = zb_or(n2370, n2441);
    let n2443: ZB = zb_or(n2334, n2442);
    let n2444: ZB = zb_and(n1006, n2443);
    let n2445: ZB = zb_and(n1007, n2443);
    let n2446: ZB = zb_and(n778, n2444);
    let n2447: ZB = zb_and(n779, n2444);
    let n2448: ZB = zb_and(n1014, n2446);
    let n2449: ZB = zb_and(n1015, n2446);
    let n2450: ZB = zb_and(n791, n2448);
    let n2451: ZB = zb_and(n790, n2448);
    let n2452: ZB = zb_or(n2450, n2451);
    let n2453: ZB = zb_or(n2449, n2452);
    let n2454: ZB = zb_and(n1022, n2453);
    let n2455: ZB = zb_and(n1023, n2453);
    let n2456: ZB = zb_or(n2454, n2455);
    let n2457: ZB = zb_and(n1028, n2456);
    let n2458: ZB = zb_and(n1030, n2457);
    let n2459: ZB = zb_and(n1031, n2457);
    let n2460: ZB = zb_or(n2458, n2459);
    let n2461: ZB = zb_and(n1035, n2460);
    let n2462: ZB = zb_and(n1036, n2460);
    let n2463: ZB = zb_or(n2461, n2462);
    let n2464: ZB = zb_and(n1036, n2463);
    let n2465: ZB = zb_and(n1041, n2464);
    let n2466: ZB = zb_and(n1042, n2464);
    let n2467: ZB = zb_or(n2465, n2466);
    let n2468: ZB = zb_and(n1046, n2467);
    let n2469: ZB = zb_and(n1047, n2467);
    let n2470: ZB = zb_or(n2468, n2469);
    let n2471: ZB = zb_and(n1052, n2470);
    let n2472: ZB = zb_and(n1054, n2471);
    let n2473: ZB = zb_and(n1055, n2471);
    let n2474: ZB = zb_and(n844, n2472);
    let n2475: ZB = zb_and(n843, n2472);
    let n2476: ZB = zb_or(n2474, n2475);
    let n2477: ZB = zb_or(n2473, n2476);
    let n2478: ZB = zb_and(n1066, n2477);
    let n2479: ZB = zb_and(n1067, n2477);
    let n2480: ZB = zb_or(n2478, n2479);
    let n2481: ZB = zb_and(n1072, n2480);
    let n2482: ZB = zb_and(n863, n2481);
    let n2483: ZB = zb_and(n864, n2481);
    let n2484: ZB = zb_and(n1077, n2482);
    let n2485: ZB = zb_and(n1078, n2482);
    let n2486: ZB = zb_and(n791, n2484);
    let n2487: ZB = zb_and(n790, n2484);
    let n2488: ZB = zb_or(n2486, n2487);
    let n2489: ZB = zb_or(n2485, n2488);
    let n2490: ZB = zb_and(n1085, n2489);
    let n2491: ZB = zb_and(n1086, n2489);
    let n2492: ZB = zb_or(n2490, n2491);
    let n2493: ZB = zb_and(n1091, n2492);
    let n2494: ZB = zb_and(n1093, n2493);
    let n2495: ZB = zb_and(n1094, n2493);
    let n2496: ZB = zb_or(n2494, n2495);
    let n2497: ZB = zb_and(n1098, n2496);
    let n2498: ZB = zb_and(n1099, n2496);
    let n2499: ZB = zb_or(n2497, n2498);
    let n2500: ZB = zb_and(n1099, n2499);
    let n2501: ZB = zb_and(n1104, n2500);
    let n2502: ZB = zb_and(n1105, n2500);
    let n2503: ZB = zb_or(n2501, n2502);
    let n2504: ZB = zb_and(n1109, n2503);
    let n2505: ZB = zb_and(n1110, n2503);
    let n2506: ZB = zb_or(n2504, n2505);
    let n2507: ZB = zb_and(n1115, n2506);
    let n2508: ZB = zb_and(n1117, n2507);
    let n2509: ZB = zb_and(n1118, n2507);
    let n2510: ZB = zb_and(n844, n2508);
    let n2511: ZB = zb_and(n843, n2508);
    let n2512: ZB = zb_or(n2510, n2511);
    let n2513: ZB = zb_or(n2509, n2512);
    let n2514: ZB = zb_and(n1125, n2513);
    let n2515: ZB = zb_and(n1126, n2513);
    let n2516: ZB = zb_or(n2514, n2515);
    let n2517: ZB = zb_and(n1131, n2516);
    let n2518: ZB = zb_and(n930, n2517);
    let n2519: ZB = zb_and(n931, n2517);
    let n2520: ZB = zb_and(n1136, n2518);
    let n2521: ZB = zb_and(n1137, n2518);
    let n2522: ZB = zb_and(n791, n2520);
    let n2523: ZB = zb_and(n790, n2520);
    let n2524: ZB = zb_or(n2522, n2523);
    let n2525: ZB = zb_or(n2521, n2524);
    let n2526: ZB = zb_and(n1144, n2525);
    let n2527: ZB = zb_and(n1145, n2525);
    let n2528: ZB = zb_or(n2526, n2527);
    let n2529: ZB = zb_and(n1150, n2528);
    let n2530: ZB = zb_and(n1152, n2529);
    let n2531: ZB = zb_and(n1153, n2529);
    let n2532: ZB = zb_or(n2530, n2531);
    let n2533: ZB = zb_and(n1157, n2532);
    let n2534: ZB = zb_and(n1158, n2532);
    let n2535: ZB = zb_or(n2533, n2534);
    let n2536: ZB = zb_and(n1158, n2535);
    let n2537: ZB = zb_and(n1163, n2536);
    let n2538: ZB = zb_and(n1164, n2536);
    let n2539: ZB = zb_or(n2537, n2538);
    let n2540: ZB = zb_and(n1168, n2539);
    let n2541: ZB = zb_and(n1169, n2539);
    let n2542: ZB = zb_or(n2540, n2541);
    let n2543: ZB = zb_and(n1174, n2542);
    let n2544: ZB = zb_and(n1176, n2543);
    let n2545: ZB = zb_and(n1177, n2543);
    let n2546: ZB = zb_and(n844, n2544);
    let n2547: ZB = zb_and(n843, n2544);
    let n2548: ZB = zb_or(n2546, n2547);
    let n2549: ZB = zb_or(n2545, n2548);
    let n2550: ZB = zb_and(n1184, n2549);
    let n2551: ZB = zb_and(n1185, n2549);
    let n2552: ZB = zb_or(n2550, n2551);
    let n2553: ZB = zb_and(n1190, n2552);
    let n2554: ZB = zb_or(n2519, n2553);
    let n2555: ZB = zb_or(n2483, n2554);
    let n2556: ZB = zb_or(n2447, n2555);
    let n2557: ZB = zb_and(n1200, n2556);
    let n2558: ZB = zb_and(n1201, n2556);
    let n2559: ZB = zb_and(n778, n2557);
    let n2560: ZB = zb_and(n779, n2557);
    let n2561: ZB = zb_and(n1208, n2559);
    let n2562: ZB = zb_and(n1209, n2559);
    let n2563: ZB = zb_and(n791, n2561);
    let n2564: ZB = zb_and(n790, n2561);
    let n2565: ZB = zb_or(n2563, n2564);
    let n2566: ZB = zb_or(n2562, n2565);
    let n2567: ZB = zb_and(n1216, n2566);
    let n2568: ZB = zb_and(n1217, n2566);
    let n2569: ZB = zb_or(n2567, n2568);
    let n2570: ZB = zb_and(n1222, n2569);
    let n2571: ZB = zb_and(n1224, n2570);
    let n2572: ZB = zb_and(n1225, n2570);
    let n2573: ZB = zb_or(n2571, n2572);
    let n2574: ZB = zb_and(n1229, n2573);
    let n2575: ZB = zb_and(n1230, n2573);
    let n2576: ZB = zb_or(n2574, n2575);
    let n2577: ZB = zb_and(n1230, n2576);
    let n2578: ZB = zb_and(n1235, n2577);
    let n2579: ZB = zb_and(n1236, n2577);
    let n2580: ZB = zb_or(n2578, n2579);
    let n2581: ZB = zb_and(n1240, n2580);
    let n2582: ZB = zb_and(n1241, n2580);
    let n2583: ZB = zb_or(n2581, n2582);
    let n2584: ZB = zb_and(n1246, n2583);
    let n2585: ZB = zb_and(n1248, n2584);
    let n2586: ZB = zb_and(n1249, n2584);
    let n2587: ZB = zb_and(n844, n2585);
    let n2588: ZB = zb_and(n843, n2585);
    let n2589: ZB = zb_or(n2587, n2588);
    let n2590: ZB = zb_or(n2586, n2589);
    let n2591: ZB = zb_and(n1260, n2590);
    let n2592: ZB = zb_and(n1261, n2590);
    let n2593: ZB = zb_or(n2591, n2592);
    let n2594: ZB = zb_and(n1266, n2593);
    let n2595: ZB = zb_and(n863, n2594);
    let n2596: ZB = zb_and(n864, n2594);
    let n2597: ZB = zb_and(n1271, n2595);
    let n2598: ZB = zb_and(n1272, n2595);
    let n2599: ZB = zb_and(n791, n2597);
    let n2600: ZB = zb_and(n790, n2597);
    let n2601: ZB = zb_or(n2599, n2600);
    let n2602: ZB = zb_or(n2598, n2601);
    let n2603: ZB = zb_and(n1279, n2602);
    let n2604: ZB = zb_and(n1280, n2602);
    let n2605: ZB = zb_or(n2603, n2604);
    let n2606: ZB = zb_and(n1285, n2605);
    let n2607: ZB = zb_and(n1287, n2606);
    let n2608: ZB = zb_and(n1288, n2606);
    let n2609: ZB = zb_or(n2607, n2608);
    let n2610: ZB = zb_and(n1292, n2609);
    let n2611: ZB = zb_and(n1293, n2609);
    let n2612: ZB = zb_or(n2610, n2611);
    let n2613: ZB = zb_and(n1293, n2612);
    let n2614: ZB = zb_and(n1298, n2613);
    let n2615: ZB = zb_and(n1299, n2613);
    let n2616: ZB = zb_or(n2614, n2615);
    let n2617: ZB = zb_and(n1303, n2616);
    let n2618: ZB = zb_and(n1304, n2616);
    let n2619: ZB = zb_or(n2617, n2618);
    let n2620: ZB = zb_and(n1309, n2619);
    let n2621: ZB = zb_and(n1311, n2620);
    let n2622: ZB = zb_and(n1312, n2620);
    let n2623: ZB = zb_and(n844, n2621);
    let n2624: ZB = zb_and(n843, n2621);
    let n2625: ZB = zb_or(n2623, n2624);
    let n2626: ZB = zb_or(n2622, n2625);
    let n2627: ZB = zb_and(n1319, n2626);
    let n2628: ZB = zb_and(n1320, n2626);
    let n2629: ZB = zb_or(n2627, n2628);
    let n2630: ZB = zb_and(n1325, n2629);
    let n2631: ZB = zb_and(n930, n2630);
    let n2632: ZB = zb_and(n931, n2630);
    let n2633: ZB = zb_and(n1330, n2631);
    let n2634: ZB = zb_and(n1331, n2631);
    let n2635: ZB = zb_and(n791, n2633);
    let n2636: ZB = zb_and(n790, n2633);
    let n2637: ZB = zb_or(n2635, n2636);
    let n2638: ZB = zb_or(n2634, n2637);
    let n2639: ZB = zb_and(n1338, n2638);
    let n2640: ZB = zb_and(n1339, n2638);
    let n2641: ZB = zb_or(n2639, n2640);
    let n2642: ZB = zb_and(n1344, n2641);
    let n2643: ZB = zb_and(n1346, n2642);
    let n2644: ZB = zb_and(n1347, n2642);
    let n2645: ZB = zb_or(n2643, n2644);
    let n2646: ZB = zb_and(n1351, n2645);
    let n2647: ZB = zb_and(n1352, n2645);
    let n2648: ZB = zb_or(n2646, n2647);
    let n2649: ZB = zb_and(n1352, n2648);
    let n2650: ZB = zb_and(n1357, n2649);
    let n2651: ZB = zb_and(n1358, n2649);
    let n2652: ZB = zb_or(n2650, n2651);
    let n2653: ZB = zb_and(n1362, n2652);
    let n2654: ZB = zb_and(n1363, n2652);
    let n2655: ZB = zb_or(n2653, n2654);
    let n2656: ZB = zb_and(n1368, n2655);
    let n2657: ZB = zb_and(n1370, n2656);
    let n2658: ZB = zb_and(n1371, n2656);
    let n2659: ZB = zb_and(n844, n2657);
    let n2660: ZB = zb_and(n843, n2657);
    let n2661: ZB = zb_or(n2659, n2660);
    let n2662: ZB = zb_or(n2658, n2661);
    let n2663: ZB = zb_and(n1378, n2662);
    let n2664: ZB = zb_and(n1379, n2662);
    let n2665: ZB = zb_or(n2663, n2664);
    let n2666: ZB = zb_and(n1384, n2665);
    let n2667: ZB = zb_or(n2632, n2666);
    let n2668: ZB = zb_or(n2596, n2667);
    let n2669: ZB = zb_or(n2560, n2668);
    let n2670: ZB = zb_or(n2558, n2669);
    let n2671: ZB = zb_or(n2445, n2670);
    let n2672: ZB = zb_or(n2332, n2671);
    let n2673: ZB = zb_and(n1404, n2672);
    let n2674: ZB = zb_and(n1403, n2672);
    let n2675: ZB = zb_or(n2673, n2674);
    let n2676: ZB = zb_and(n1404, n2675);
    let n2677: ZB = zb_and(n1403, n2675);
    let n2678: ZB = zb_or(n2676, n2677);
    let n2679: ZB = zb_and(n1403, n2678);
    let n2680: ZB = zb_and(n1404, n2678);
    let n2681: ZB = zb_and(n1414, n2680);
    let n2682: ZB = zb_and(n1415, n2680);
    let n2683: ZB = zb_or(n2681, n2682);
    let n2684: ZB = zb_or(n2679, n2683);
    let n2685: ZB = zb_and(n1424, n2684);
    let n2686: ZB = zb_and(n1425, n2684);
    let n2687: ZB = zb_and(n1429, n2685);
    let n2688: ZB = zb_and(n1430, n2685);
    let n2689: ZB = zb_or(n2687, n2688);
    let n2690: ZB = zb_and(n1439, n2689);
    let n2691: ZB = zb_and(n1440, n2689);
    let n2692: ZB = zb_or(n2690, n2691);
    let n2693: ZB = zb_and(n1404, n2686);
    let n2694: ZB = zb_and(n1403, n2686);
    let n2695: ZB = zb_or(n2693, n2694);
    let n2696: ZB = zb_and(n1454, n2695);
    let n2697: ZB = zb_and(n1455, n2695);
    let n2698: ZB = zb_and(n1458, n2696);
    let n2699: ZB = zb_and(n833, n2696);
    let n2700: ZB = zb_and(n1461, n2699);
    let n2701: ZB = zb_and(n857, n2699);
    let n2702: ZB = zb_and(n1464, n2698);
    let n2703: ZB = zb_and(n1465, n2698);
    let n2704: ZB = zb_and(n1472, n2700);
    let n2705: ZB = zb_and(n1473, n2700);
    let n2706: ZB = zb_and(n833, n2701);
    let n2707: ZB = zb_or(n2704, n2705);
    let n2708: ZB = zb_or(n2702, n2703);
    let n2709: ZB = zb_or(n2706, n2707);
    let n2710: ZB = zb_or(n2708, n2709);
    let n2711: ZB = zb_and(n1458, n2697);
    let n2712: ZB = zb_and(n833, n2697);
    let n2713: ZB = zb_or(n2711, n2712);
    let n2714: ZB = zb_or(n2710, n2713);
    let n2715: ZB = zb_and(n1501, n2714);
    let n2716: ZB = zb_and(n1500, n2714);
    let n2717: ZB = zb_or(n2715, n2716);
    let n2718: ZB = zb_and(n1508, n2717);
    let n2719: ZB = zb_and(n1509, n2717);
    let n2720: ZB = zb_or(n2718, n2719);
    let n2721: ZB = zb_and(n1404, n2720);
    let n2722: ZB = zb_and(n1403, n2720);
    let n2723: ZB = zb_or(n2721, n2722);
    let n2724: ZB = zb_or(n2692, n2723);
    let n2725: ZB = zb_and(n137, n2724);
    let n2726: ZB = zb_and(n1559, n2725);
    let n2727: ZB = zb_and(n1558, n2725);
    let n2728: ZB = zb_or(n2726, n2727);
    let n2729: ZB = zb_and(n1564, n2728);
    let n2730: ZB = zb_and(n1565, n2728);
    let n2731: ZB = zb_or(n2729, n2730);
    let n2732: ZB = zb_and(n1472, n2697);
    let n2733: ZB = zb_and(n1473, n2697);
    let n2734: ZB = zb_or(n2732, n2733);
    let n2735: ZB = zb_or(n2710, n2734);
    let n2736: ZB = zb_and(n1587, n2735);
    let n2737: ZB = zb_and(n1586, n2735);
    let n2738: ZB = zb_or(n2736, n2737);
    let n2739: ZB = zb_and(n1508, n2738);
    let n2740: ZB = zb_and(n1509, n2738);
    let n2741: ZB = zb_or(n2739, n2740);
    let n2742: ZB = zb_and(n1598, n2741);
    let n2743: ZB = zb_and(n1597, n2741);
    let n2744: ZB = zb_or(n2742, n2743);
    let n2745: ZB = zb_and(n1598, n2744);
    let n2746: ZB = zb_and(n1597, n2744);
    let n2747: ZB = zb_or(n2745, n2746);
    let n2748: ZB = zb_and(n1597, n2747);
    let n2749: ZB = zb_and(n1598, n2747);
    let n2750: ZB = zb_or(n2748, n2749);
    let n2751: ZB = zb_and(n1597, n2750);
    let n2752: ZB = zb_and(n1598, n2750);
    let n2753: ZB = zb_or(n2751, n2752);
    let n2754: ZB = zb_and(n1404, n2753);
    let n2755: ZB = zb_and(n1403, n2753);
    let n2756: ZB = zb_or(n2754, n2755);
    let n2757: ZB = zb_or(n2692, n2756);
    let n2758: ZB = zb_and(n137, n2757);
    let n2759: ZB = zb_and(n1559, n2758);
    let n2760: ZB = zb_and(n1558, n2758);
    let n2761: ZB = zb_or(n2759, n2760);
    let n2762: ZB = zb_and(n1564, n2761);
    let n2763: ZB = zb_and(n1565, n2761);
    let n2764: ZB = zb_or(n2762, n2763);
    let n2765: ZB = zb_and(n1464, n2697);
    let n2766: ZB = zb_and(n1465, n2697);
    let n2767: ZB = zb_or(n2765, n2766);
    let n2768: ZB = zb_or(n2710, n2767);
    let n2769: ZB = zb_and(n1643, n2768);
    let n2770: ZB = zb_and(n1642, n2768);
    let n2771: ZB = zb_or(n2769, n2770);
    let n2772: ZB = zb_and(n1508, n2771);
    let n2773: ZB = zb_and(n1509, n2771);
    let n2774: ZB = zb_or(n2772, n2773);
    let n2775: ZB = zb_and(n1654, n2774);
    let n2776: ZB = zb_and(n1653, n2774);
    let n2777: ZB = zb_or(n2775, n2776);
    let n2778: ZB = zb_and(n1654, n2777);
    let n2779: ZB = zb_and(n1653, n2777);
    let n2780: ZB = zb_or(n2778, n2779);
    let n2781: ZB = zb_and(n1653, n2780);
    let n2782: ZB = zb_and(n1654, n2780);
    let n2783: ZB = zb_or(n2781, n2782);
    let n2784: ZB = zb_and(n1653, n2783);
    let n2785: ZB = zb_and(n1654, n2783);
    let n2786: ZB = zb_or(n2784, n2785);
    let n2787: ZB = zb_and(n1404, n2786);
    let n2788: ZB = zb_and(n1403, n2786);
    let n2789: ZB = zb_or(n2787, n2788);
    let n2790: ZB = zb_or(n2692, n2789);
    let n2791: ZB = zb_and(n137, n2790);
    let n2792: ZB = zb_and(n1559, n2791);
    let n2793: ZB = zb_and(n1558, n2791);
    let n2794: ZB = zb_or(n2792, n2793);
    let n2795: ZB = zb_and(n1564, n2794);
    let n2796: ZB = zb_and(n1565, n2794);
    let n2797: ZB = zb_or(n2795, n2796);
    let n2798: ZB = zb_and(n136, n2723);
    let n2799: ZB = zb_and(r_c293, n2723);
    let n2800: ZB = zb_and(n1521, n2798);
    let n2801: ZB = zb_and(n1522, n2798);
    let n2802: ZB = zb_and(n1525, n2801);
    let n2803: ZB = zb_and(n1524, n2801);
    let n2804: ZB = zb_or(n2802, n2803);
    let n2805: ZB = zb_and(n1525, n2804);
    let n2806: ZB = zb_and(n1524, n2804);
    let n2807: ZB = zb_or(n2805, n2806);
    let n2808: ZB = zb_and(n1524, n2807);
    let n2809: ZB = zb_and(n1525, n2807);
    let n2810: ZB = zb_and(n1528, n2809);
    let n2811: ZB = zb_and(n1527, n2809);
    let n2812: ZB = zb_or(n2810, n2811);
    let n2813: ZB = zb_and(n1528, n2812);
    let n2814: ZB = zb_and(n1527, n2812);
    let n2815: ZB = zb_or(n2813, n2814);
    let n2816: ZB = zb_and(n1527, n2815);
    let n2817: ZB = zb_and(n1528, n2815);
    let n2818: ZB = zb_or(n2816, n2817);
    let n2819: ZB = zb_or(n2808, n2818);
    let n2820: ZB = zb_and(n1532, n2819);
    let n2821: ZB = zb_and(n1531, n2819);
    let n2822: ZB = zb_or(n2820, n2821);
    let n2823: ZB = zb_or(n2800, n2822);
    let n2824: ZB = zb_or(n2799, n2823);
    let n2825: ZB = zb_or(n2692, n2824);
    let n2826: ZB = zb_and(n137, n2825);
    let n2827: ZB = zb_and(n1559, n2826);
    let n2828: ZB = zb_and(n1558, n2826);
    let n2829: ZB = zb_or(n2827, n2828);
    let n2830: ZB = zb_and(n1564, n2829);
    let n2831: ZB = zb_and(n1565, n2829);
    let n2832: ZB = zb_or(n2830, n2831);
    let n2833: ZB = zb_and(n136, n2756);
    let n2834: ZB = zb_and(r_c293, n2756);
    let n2835: ZB = zb_and(n1521, n2833);
    let n2836: ZB = zb_and(n1522, n2833);
    let n2837: ZB = zb_and(n1525, n2836);
    let n2838: ZB = zb_and(n1524, n2836);
    let n2839: ZB = zb_or(n2837, n2838);
    let n2840: ZB = zb_and(n1525, n2839);
    let n2841: ZB = zb_and(n1524, n2839);
    let n2842: ZB = zb_or(n2840, n2841);
    let n2843: ZB = zb_and(n1524, n2842);
    let n2844: ZB = zb_and(n1525, n2842);
    let n2845: ZB = zb_and(n1528, n2844);
    let n2846: ZB = zb_and(n1527, n2844);
    let n2847: ZB = zb_or(n2845, n2846);
    let n2848: ZB = zb_and(n1528, n2847);
    let n2849: ZB = zb_and(n1527, n2847);
    let n2850: ZB = zb_or(n2848, n2849);
    let n2851: ZB = zb_and(n1527, n2850);
    let n2852: ZB = zb_and(n1528, n2850);
    let n2853: ZB = zb_or(n2851, n2852);
    let n2854: ZB = zb_or(n2843, n2853);
    let n2855: ZB = zb_and(n1532, n2854);
    let n2856: ZB = zb_and(n1531, n2854);
    let n2857: ZB = zb_or(n2855, n2856);
    let n2858: ZB = zb_or(n2835, n2857);
    let n2859: ZB = zb_or(n2834, n2858);
    let n2860: ZB = zb_or(n2692, n2859);
    let n2861: ZB = zb_and(n137, n2860);
    let n2862: ZB = zb_and(n1559, n2861);
    let n2863: ZB = zb_and(n1558, n2861);
    let n2864: ZB = zb_or(n2862, n2863);
    let n2865: ZB = zb_and(n1564, n2864);
    let n2866: ZB = zb_and(n1565, n2864);
    let n2867: ZB = zb_or(n2865, n2866);
    let n2868: ZB = zb_and(n136, n2789);
    let n2869: ZB = zb_and(r_c293, n2789);
    let n2870: ZB = zb_and(n1521, n2868);
    let n2871: ZB = zb_and(n1522, n2868);
    let n2872: ZB = zb_and(n1525, n2871);
    let n2873: ZB = zb_and(n1524, n2871);
    let n2874: ZB = zb_or(n2872, n2873);
    let n2875: ZB = zb_and(n1525, n2874);
    let n2876: ZB = zb_and(n1524, n2874);
    let n2877: ZB = zb_or(n2875, n2876);
    let n2878: ZB = zb_and(n1524, n2877);
    let n2879: ZB = zb_and(n1525, n2877);
    let n2880: ZB = zb_and(n1528, n2879);
    let n2881: ZB = zb_and(n1527, n2879);
    let n2882: ZB = zb_or(n2880, n2881);
    let n2883: ZB = zb_and(n1528, n2882);
    let n2884: ZB = zb_and(n1527, n2882);
    let n2885: ZB = zb_or(n2883, n2884);
    let n2886: ZB = zb_and(n1527, n2885);
    let n2887: ZB = zb_and(n1528, n2885);
    let n2888: ZB = zb_or(n2886, n2887);
    let n2889: ZB = zb_or(n2878, n2888);
    let n2890: ZB = zb_and(n1532, n2889);
    let n2891: ZB = zb_and(n1531, n2889);
    let n2892: ZB = zb_or(n2890, n2891);
    let n2893: ZB = zb_or(n2870, n2892);
    let n2894: ZB = zb_or(n2869, n2893);
    let n2895: ZB = zb_or(n2692, n2894);
    let n2896: ZB = zb_and(n137, n2895);
    let n2897: ZB = zb_and(n1559, n2896);
    let n2898: ZB = zb_and(n1558, n2896);
    let n2899: ZB = zb_or(n2897, n2898);
    let n2900: ZB = zb_and(n1564, n2899);
    let n2901: ZB = zb_and(n1565, n2899);
    let n2902: ZB = zb_or(n2900, n2901);
    let n2903: ZB = zb_and(n1411, n2723);
    let n2904: ZB = zb_and(r_c292, n2723);
    let n2905: ZB = zb_and(n1505, n2903);
    let n2906: ZB = zb_and(n1540, n2903);
    let n2907: ZB = zb_or(n2905, n2906);
    let n2908: ZB = zb_and(n1542, n2907);
    let n2909: ZB = zb_and(n1543, n2907);
    let n2910: ZB = zb_and(n1544, n2909);
    let n2911: ZB = zb_and(n1545, n2909);
    let n2912: ZB = zb_or(n2910, n2911);
    let n2913: ZB = zb_or(n2908, n2912);
    let n2914: ZB = zb_and(n1549, n2913);
    let n2915: ZB = zb_and(n1548, n2913);
    let n2916: ZB = zb_or(n2914, n2915);
    let n2917: ZB = zb_or(n2904, n2916);
    let n2918: ZB = zb_or(n2692, n2917);
    let n2919: ZB = zb_and(n1853, n2918);
    let n2920: ZB = zb_and(n1854, n2918);
    let n2921: ZB = zb_and(n1559, n2920);
    let n2922: ZB = zb_and(n1558, n2920);
    let n2923: ZB = zb_or(n2921, n2922);
    let n2924: ZB = zb_and(n1564, n2923);
    let n2925: ZB = zb_and(n1565, n2923);
    let n2926: ZB = zb_or(n2924, n2925);
    let n2927: ZB = zb_or(n2919, n2926);
    let n2928: ZB = zb_and(n1411, n2756);
    let n2929: ZB = zb_and(r_c292, n2756);
    let n2930: ZB = zb_or(n2928, n2929);
    let n2931: ZB = zb_or(n2692, n2930);
    let n2932: ZB = zb_and(n1853, n2931);
    let n2933: ZB = zb_and(n1854, n2931);
    let n2934: ZB = zb_and(n1559, n2933);
    let n2935: ZB = zb_and(n1558, n2933);
    let n2936: ZB = zb_or(n2934, n2935);
    let n2937: ZB = zb_and(n1564, n2936);
    let n2938: ZB = zb_and(n1565, n2936);
    let n2939: ZB = zb_or(n2937, n2938);
    let n2940: ZB = zb_or(n2932, n2939);
    let n2941: ZB = zb_and(n1411, n2789);
    let n2942: ZB = zb_and(r_c292, n2789);
    let n2943: ZB = zb_or(n2941, n2942);
    let n2944: ZB = zb_or(n2692, n2943);
    let n2945: ZB = zb_and(n1853, n2944);
    let n2946: ZB = zb_and(n1854, n2944);
    let n2947: ZB = zb_and(n1559, n2946);
    let n2948: ZB = zb_and(n1558, n2946);
    let n2949: ZB = zb_or(n2947, n2948);
    let n2950: ZB = zb_and(n1564, n2949);
    let n2951: ZB = zb_and(n1565, n2949);
    let n2952: ZB = zb_or(n2950, n2951);
    let n2953: ZB = zb_or(n2945, n2952);
    let n2954: ZB = zb_or(n2903, n2904);
    let n2955: ZB = zb_or(n2692, n2954);
    let n2956: ZB = zb_and(n1853, n2955);
    let n2957: ZB = zb_and(n1854, n2955);
    let n2958: ZB = zb_and(n1559, n2957);
    let n2959: ZB = zb_and(n1558, n2957);
    let n2960: ZB = zb_or(n2958, n2959);
    let n2961: ZB = zb_and(n1564, n2960);
    let n2962: ZB = zb_and(n1565, n2960);
    let n2963: ZB = zb_or(n2961, n2962);
    let n2964: ZB = zb_or(n2956, n2963);
    let n2965: ZB = zb_and(n1411, n2824);
    let n2966: ZB = zb_and(r_c292, n2824);
    let n2967: ZB = zb_and(n1505, n2965);
    let n2968: ZB = zb_and(n1540, n2965);
    let n2969: ZB = zb_or(n2967, n2968);
    let n2970: ZB = zb_and(n1542, n2969);
    let n2971: ZB = zb_and(n1543, n2969);
    let n2972: ZB = zb_and(n1544, n2971);
    let n2973: ZB = zb_and(n1545, n2971);
    let n2974: ZB = zb_or(n2972, n2973);
    let n2975: ZB = zb_or(n2970, n2974);
    let n2976: ZB = zb_and(n1549, n2975);
    let n2977: ZB = zb_and(n1548, n2975);
    let n2978: ZB = zb_or(n2976, n2977);
    let n2979: ZB = zb_or(n2966, n2978);
    let n2980: ZB = zb_or(n2692, n2979);
    let n2981: ZB = zb_and(n1853, n2980);
    let n2982: ZB = zb_and(n1854, n2980);
    let n2983: ZB = zb_and(n1559, n2982);
    let n2984: ZB = zb_and(n1558, n2982);
    let n2985: ZB = zb_or(n2983, n2984);
    let n2986: ZB = zb_and(n1564, n2985);
    let n2987: ZB = zb_and(n1565, n2985);
    let n2988: ZB = zb_or(n2986, n2987);
    let n2989: ZB = zb_or(n2981, n2988);
    let n2990: ZB = zb_and(n1411, n2859);
    let n2991: ZB = zb_and(r_c292, n2859);
    let n2992: ZB = zb_or(n2990, n2991);
    let n2993: ZB = zb_or(n2692, n2992);
    let n2994: ZB = zb_and(n1853, n2993);
    let n2995: ZB = zb_and(n1854, n2993);
    let n2996: ZB = zb_and(n1559, n2995);
    let n2997: ZB = zb_and(n1558, n2995);
    let n2998: ZB = zb_or(n2996, n2997);
    let n2999: ZB = zb_and(n1564, n2998);
    let n3000: ZB = zb_and(n1565, n2998);
    let n3001: ZB = zb_or(n2999, n3000);
    let n3002: ZB = zb_or(n2994, n3001);
    let n3003: ZB = zb_and(n1411, n2894);
    let n3004: ZB = zb_and(r_c292, n2894);
    let n3005: ZB = zb_or(n3003, n3004);
    let n3006: ZB = zb_or(n2692, n3005);
    let n3007: ZB = zb_and(n1853, n3006);
    let n3008: ZB = zb_and(n1854, n3006);
    let n3009: ZB = zb_and(n1559, n3008);
    let n3010: ZB = zb_and(n1558, n3008);
    let n3011: ZB = zb_or(n3009, n3010);
    let n3012: ZB = zb_and(n1564, n3011);
    let n3013: ZB = zb_and(n1565, n3011);
    let n3014: ZB = zb_or(n3012, n3013);
    let n3015: ZB = zb_or(n3007, n3014);
    let n3016: ZB = zb_or(n2965, n2966);
    let n3017: ZB = zb_or(n2692, n3016);
    let n3018: ZB = zb_and(n1853, n3017);
    let n3019: ZB = zb_and(n1854, n3017);
    let n3020: ZB = zb_and(n1559, n3019);
    let n3021: ZB = zb_and(n1558, n3019);
    let n3022: ZB = zb_or(n3020, n3021);
    let n3023: ZB = zb_and(n1564, n3022);
    let n3024: ZB = zb_and(n1565, n3022);
    let n3025: ZB = zb_or(n3023, n3024);
    let n3026: ZB = zb_or(n3018, n3025);
    let n3027: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n3028: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3027);
    let n3029: ZB = zb_and(n805, n806);
    let n3030: ZB = zb_and(n816, n820);
    let n3031: ZB = zb_and(n834, n835);
    let n3032: ZB = zb_and(n858, n859);
    let n3033: ZB = zb_or(n3031, n3032);
    let n3034: ZB = zb_or(n3030, n3033);
    let n3035: ZB = zb_or(n3029, n3034);
    let n3036: ZB = zb_and(n885, n886);
    let n3037: ZB = zb_and(n894, n898);
    let n3038: ZB = zb_and(n909, n910);
    let n3039: ZB = zb_and(n925, n926);
    let n3040: ZB = zb_or(n3038, n3039);
    let n3041: ZB = zb_or(n3037, n3040);
    let n3042: ZB = zb_or(n3036, n3041);
    let n3043: ZB = zb_and(n952, n953);
    let n3044: ZB = zb_and(n961, n965);
    let n3045: ZB = zb_and(n976, n977);
    let n3046: ZB = zb_and(n992, n993);
    let n3047: ZB = zb_or(n3045, n3046);
    let n3048: ZB = zb_or(n3044, n3047);
    let n3049: ZB = zb_or(n3043, n3048);
    let n3050: ZB = zb_or(n3042, n3049);
    let n3051: ZB = zb_or(n3035, n3050);
    let n3052: ZB = zb_and(n1026, n1027);
    let n3053: ZB = zb_and(n1035, n1039);
    let n3054: ZB = zb_and(n1050, n1051);
    let n3055: ZB = zb_and(n1070, n1071);
    let n3056: ZB = zb_or(n3054, n3055);
    let n3057: ZB = zb_or(n3053, n3056);
    let n3058: ZB = zb_or(n3052, n3057);
    let n3059: ZB = zb_and(n1089, n1090);
    let n3060: ZB = zb_and(n1098, n1102);
    let n3061: ZB = zb_and(n1113, n1114);
    let n3062: ZB = zb_and(n1129, n1130);
    let n3063: ZB = zb_or(n3061, n3062);
    let n3064: ZB = zb_or(n3060, n3063);
    let n3065: ZB = zb_or(n3059, n3064);
    let n3066: ZB = zb_and(n1148, n1149);
    let n3067: ZB = zb_and(n1157, n1161);
    let n3068: ZB = zb_and(n1172, n1173);
    let n3069: ZB = zb_and(n1188, n1189);
    let n3070: ZB = zb_or(n3068, n3069);
    let n3071: ZB = zb_or(n3067, n3070);
    let n3072: ZB = zb_or(n3066, n3071);
    let n3073: ZB = zb_or(n3065, n3072);
    let n3074: ZB = zb_or(n3058, n3073);
    let n3075: ZB = zb_and(n1220, n1221);
    let n3076: ZB = zb_and(n1229, n1233);
    let n3077: ZB = zb_and(n1244, n1245);
    let n3078: ZB = zb_and(n1264, n1265);
    let n3079: ZB = zb_or(n3077, n3078);
    let n3080: ZB = zb_or(n3076, n3079);
    let n3081: ZB = zb_or(n3075, n3080);
    let n3082: ZB = zb_and(n1283, n1284);
    let n3083: ZB = zb_and(n1292, n1296);
    let n3084: ZB = zb_and(n1307, n1308);
    let n3085: ZB = zb_and(n1323, n1324);
    let n3086: ZB = zb_or(n3084, n3085);
    let n3087: ZB = zb_or(n3083, n3086);
    let n3088: ZB = zb_or(n3082, n3087);
    let n3089: ZB = zb_and(n1342, n1343);
    let n3090: ZB = zb_and(n1351, n1355);
    let n3091: ZB = zb_and(n1366, n1367);
    let n3092: ZB = zb_and(n1382, n1383);
    let n3093: ZB = zb_or(n3091, n3092);
    let n3094: ZB = zb_or(n3090, n3093);
    let n3095: ZB = zb_or(n3089, n3094);
    let n3096: ZB = zb_or(n3088, n3095);
    let n3097: ZB = zb_or(n3081, n3096);
    let n3098: ZB = zb_or(n3074, n3097);
    let n3099: ZB = zsel_b(n3074, n1004, n1198);
    let n3100: ZB = zb_or(n3051, n3098);
    let n3101: ZB = zsel_b(n3051, n756, n3099);
    let n3102: ZB = zsel_b(n3100, n3101, n1401);
    let n3103: ZB = zb_and(n1404, n3100);
    let n3104: ZB = zb_and(n1403, n3100);
    let n3105: ZB = zb_or(n3103, n3104);
    let n3106: ZB = zb_and(n1404, n3105);
    let n3107: ZB = zb_and(n1403, n3105);
    let n3108: ZB = zb_or(n3106, n3107);
    let n3109: ZB = zb_and(n1403, n3108);
    let n3110: ZB = zb_and(n1404, n3108);
    let n3111: ZB = zb_and(n1414, n3110);
    let n3112: ZB = zb_and(n1415, n3110);
    let n3113: ZB = zb_or(n3111, n3112);
    let n3114: ZB = zb_or(n3109, n3113);
    let n3115: ZB = zb_and(n1424, n3114);
    let n3116: ZB = zb_and(n1425, n3114);
    let n3117: ZB = zb_and(n1429, n3115);
    let n3118: ZB = zb_and(n1430, n3115);
    let n3119: ZB = zb_or(n3117, n3118);
    let n3120: ZB = zb_and(n1439, n3119);
    let n3121: ZB = zb_and(n1440, n3119);
    let n3122: ZB = zb_or(n3120, n3121);
    let n3123: ZB = zb_and(n1404, n3116);
    let n3124: ZB = zb_and(n1403, n3116);
    let n3125: ZB = zb_or(n3123, n3124);
    let n3126: ZB = zb_and(n1454, n3125);
    let n3127: ZB = zb_and(n1455, n3125);
    let n3128: ZB = zb_and(n1458, n3126);
    let n3129: ZB = zb_and(n833, n3126);
    let n3130: ZB = zb_and(n1461, n3129);
    let n3131: ZB = zb_and(n857, n3129);
    let n3132: ZB = zb_and(n1464, n3128);
    let n3133: ZB = zb_and(n1465, n3128);
    let n3134: ZB = zb_and(n1472, n3130);
    let n3135: ZB = zb_and(n1473, n3130);
    let n3136: ZB = zb_and(n833, n3131);
    let n3137: ZB = zb_or(n3134, n3135);
    let n3138: ZB = zb_or(n3132, n3133);
    let n3139: ZB = zb_or(n3136, n3137);
    let n3140: ZB = zb_or(n3138, n3139);
    let n3141: ZB = zb_and(n1458, n3127);
    let n3142: ZB = zb_and(n833, n3127);
    let n3143: ZB = zb_or(n3141, n3142);
    let n3144: ZB = zb_or(n3140, n3143);
    let n3145: ZB = zb_and(n1501, n3144);
    let n3146: ZB = zb_and(n1500, n3144);
    let n3147: ZB = zb_or(n3145, n3146);
    let n3148: ZB = zb_and(n1508, n3147);
    let n3149: ZB = zb_and(n1509, n3147);
    let n3150: ZB = zb_or(n3148, n3149);
    let n3151: ZB = zb_and(n1404, n3150);
    let n3152: ZB = zb_and(n1403, n3150);
    let n3153: ZB = zb_or(n3151, n3152);
    let n3154: ZB = zb_or(n3122, n3153);
    let n3157: ZB = zb_and(n1472, n3127);
    let n3158: ZB = zb_and(n1473, n3127);
    let n3159: ZB = zb_or(n3157, n3158);
    let n3160: ZB = zb_or(n3140, n3159);
    let n3161: ZB = zb_and(n1587, n3160);
    let n3162: ZB = zb_and(n1586, n3160);
    let n3163: ZB = zb_or(n3161, n3162);
    let n3164: ZB = zb_and(n1508, n3163);
    let n3165: ZB = zb_and(n1509, n3163);
    let n3166: ZB = zb_or(n3164, n3165);
    let n3167: ZB = zb_and(n1598, n3166);
    let n3168: ZB = zb_and(n1597, n3166);
    let n3169: ZB = zb_or(n3167, n3168);
    let n3170: ZB = zb_and(n1598, n3169);
    let n3171: ZB = zb_and(n1597, n3169);
    let n3172: ZB = zb_or(n3170, n3171);
    let n3173: ZB = zb_and(n1597, n3172);
    let n3174: ZB = zb_and(n1598, n3172);
    let n3175: ZB = zb_or(n3173, n3174);
    let n3176: ZB = zb_and(n1597, n3175);
    let n3177: ZB = zb_and(n1598, n3175);
    let n3178: ZB = zb_or(n3176, n3177);
    let n3179: ZB = zb_and(n1404, n3178);
    let n3180: ZB = zb_and(n1403, n3178);
    let n3181: ZB = zb_or(n3179, n3180);
    let n3182: ZB = zb_or(n3122, n3181);
    let n3184: ZB = zb_and(n1464, n3127);
    let n3185: ZB = zb_and(n1465, n3127);
    let n3186: ZB = zb_or(n3184, n3185);
    let n3187: ZB = zb_or(n3140, n3186);
    let n3188: ZB = zb_and(n1643, n3187);
    let n3189: ZB = zb_and(n1642, n3187);
    let n3190: ZB = zb_or(n3188, n3189);
    let n3191: ZB = zb_and(n1508, n3190);
    let n3192: ZB = zb_and(n1509, n3190);
    let n3193: ZB = zb_or(n3191, n3192);
    let n3194: ZB = zb_and(n1654, n3193);
    let n3195: ZB = zb_and(n1653, n3193);
    let n3196: ZB = zb_or(n3194, n3195);
    let n3197: ZB = zb_and(n1654, n3196);
    let n3198: ZB = zb_and(n1653, n3196);
    let n3199: ZB = zb_or(n3197, n3198);
    let n3200: ZB = zb_and(n1653, n3199);
    let n3201: ZB = zb_and(n1654, n3199);
    let n3202: ZB = zb_or(n3200, n3201);
    let n3203: ZB = zb_and(n1653, n3202);
    let n3204: ZB = zb_and(n1654, n3202);
    let n3205: ZB = zb_or(n3203, n3204);
    let n3206: ZB = zb_and(n1404, n3205);
    let n3207: ZB = zb_and(n1403, n3205);
    let n3208: ZB = zb_or(n3206, n3207);
    let n3209: ZB = zb_or(n3122, n3208);
    let n3211: ZB = zb_and(n136, n3153);
    let n3212: ZB = zb_and(r_c293, n3153);
    let n3213: ZB = zb_and(n1521, n3211);
    let n3214: ZB = zb_and(n1522, n3211);
    let n3215: ZB = zb_and(n1525, n3214);
    let n3216: ZB = zb_and(n1524, n3214);
    let n3217: ZB = zb_or(n3215, n3216);
    let n3218: ZB = zb_and(n1525, n3217);
    let n3219: ZB = zb_and(n1524, n3217);
    let n3220: ZB = zb_or(n3218, n3219);
    let n3221: ZB = zb_and(n1524, n3220);
    let n3222: ZB = zb_and(n1525, n3220);
    let n3223: ZB = zb_and(n1528, n3222);
    let n3224: ZB = zb_and(n1527, n3222);
    let n3225: ZB = zb_or(n3223, n3224);
    let n3226: ZB = zb_and(n1528, n3225);
    let n3227: ZB = zb_and(n1527, n3225);
    let n3228: ZB = zb_or(n3226, n3227);
    let n3229: ZB = zb_and(n1527, n3228);
    let n3230: ZB = zb_and(n1528, n3228);
    let n3231: ZB = zb_or(n3229, n3230);
    let n3232: ZB = zb_or(n3221, n3231);
    let n3233: ZB = zb_and(n1532, n3232);
    let n3234: ZB = zb_and(n1531, n3232);
    let n3235: ZB = zb_or(n3233, n3234);
    let n3236: ZB = zb_or(n3213, n3235);
    let n3237: ZB = zb_or(n3212, n3236);
    let n3238: ZB = zb_or(n3122, n3237);
    let n3240: ZB = zb_and(n136, n3181);
    let n3241: ZB = zb_and(r_c293, n3181);
    let n3242: ZB = zb_and(n1521, n3240);
    let n3243: ZB = zb_and(n1522, n3240);
    let n3244: ZB = zb_and(n1525, n3243);
    let n3245: ZB = zb_and(n1524, n3243);
    let n3246: ZB = zb_or(n3244, n3245);
    let n3247: ZB = zb_and(n1525, n3246);
    let n3248: ZB = zb_and(n1524, n3246);
    let n3249: ZB = zb_or(n3247, n3248);
    let n3250: ZB = zb_and(n1524, n3249);
    let n3251: ZB = zb_and(n1525, n3249);
    let n3252: ZB = zb_and(n1528, n3251);
    let n3253: ZB = zb_and(n1527, n3251);
    let n3254: ZB = zb_or(n3252, n3253);
    let n3255: ZB = zb_and(n1528, n3254);
    let n3256: ZB = zb_and(n1527, n3254);
    let n3257: ZB = zb_or(n3255, n3256);
    let n3258: ZB = zb_and(n1527, n3257);
    let n3259: ZB = zb_and(n1528, n3257);
    let n3260: ZB = zb_or(n3258, n3259);
    let n3261: ZB = zb_or(n3250, n3260);
    let n3262: ZB = zb_and(n1532, n3261);
    let n3263: ZB = zb_and(n1531, n3261);
    let n3264: ZB = zb_or(n3262, n3263);
    let n3265: ZB = zb_or(n3242, n3264);
    let n3266: ZB = zb_or(n3241, n3265);
    let n3267: ZB = zb_or(n3122, n3266);
    let n3269: ZB = zb_and(n136, n3208);
    let n3270: ZB = zb_and(r_c293, n3208);
    let n3271: ZB = zb_and(n1521, n3269);
    let n3272: ZB = zb_and(n1522, n3269);
    let n3273: ZB = zb_and(n1525, n3272);
    let n3274: ZB = zb_and(n1524, n3272);
    let n3275: ZB = zb_or(n3273, n3274);
    let n3276: ZB = zb_and(n1525, n3275);
    let n3277: ZB = zb_and(n1524, n3275);
    let n3278: ZB = zb_or(n3276, n3277);
    let n3279: ZB = zb_and(n1524, n3278);
    let n3280: ZB = zb_and(n1525, n3278);
    let n3281: ZB = zb_and(n1528, n3280);
    let n3282: ZB = zb_and(n1527, n3280);
    let n3283: ZB = zb_or(n3281, n3282);
    let n3284: ZB = zb_and(n1528, n3283);
    let n3285: ZB = zb_and(n1527, n3283);
    let n3286: ZB = zb_or(n3284, n3285);
    let n3287: ZB = zb_and(n1527, n3286);
    let n3288: ZB = zb_and(n1528, n3286);
    let n3289: ZB = zb_or(n3287, n3288);
    let n3290: ZB = zb_or(n3279, n3289);
    let n3291: ZB = zb_and(n1532, n3290);
    let n3292: ZB = zb_and(n1531, n3290);
    let n3293: ZB = zb_or(n3291, n3292);
    let n3294: ZB = zb_or(n3271, n3293);
    let n3295: ZB = zb_or(n3270, n3294);
    let n3296: ZB = zb_or(n3122, n3295);
    let n3298: ZB = zb_and(n1411, n3153);
    let n3299: ZB = zb_and(r_c292, n3153);
    let n3300: ZB = zb_and(n1505, n3298);
    let n3301: ZB = zb_and(n1540, n3298);
    let n3302: ZB = zb_or(n3300, n3301);
    let n3303: ZB = zb_and(n1542, n3302);
    let n3304: ZB = zb_and(n1543, n3302);
    let n3305: ZB = zb_and(n1544, n3304);
    let n3306: ZB = zb_and(n1545, n3304);
    let n3307: ZB = zb_or(n3305, n3306);
    let n3308: ZB = zb_or(n3303, n3307);
    let n3309: ZB = zb_and(n1549, n3308);
    let n3310: ZB = zb_and(n1548, n3308);
    let n3311: ZB = zb_or(n3309, n3310);
    let n3312: ZB = zb_or(n3299, n3311);
    let n3313: ZB = zb_or(n3122, n3312);
    let n3314: ZB = zb_and(n1853, n3313);
    let n3315: ZB = zb_and(n1854, n3313);
    let n3316: ZB = zb_or(n3314, n3315);
    let n3317: ZB = zb_and(n1411, n3181);
    let n3318: ZB = zb_and(r_c292, n3181);
    let n3319: ZB = zb_or(n3317, n3318);
    let n3320: ZB = zb_or(n3122, n3319);
    let n3321: ZB = zb_and(n1853, n3320);
    let n3322: ZB = zb_and(n1854, n3320);
    let n3323: ZB = zb_or(n3321, n3322);
    let n3324: ZB = zb_and(n1411, n3208);
    let n3325: ZB = zb_and(r_c292, n3208);
    let n3326: ZB = zb_or(n3324, n3325);
    let n3327: ZB = zb_or(n3122, n3326);
    let n3328: ZB = zb_and(n1853, n3327);
    let n3329: ZB = zb_and(n1854, n3327);
    let n3330: ZB = zb_or(n3328, n3329);
    let n3331: ZB = zb_or(n3298, n3299);
    let n3332: ZB = zb_or(n3122, n3331);
    let n3333: ZB = zb_and(n1853, n3332);
    let n3334: ZB = zb_and(n1854, n3332);
    let n3335: ZB = zb_or(n3333, n3334);
    let n3336: ZB = zb_and(n1411, n3237);
    let n3337: ZB = zb_and(r_c292, n3237);
    let n3338: ZB = zb_and(n1505, n3336);
    let n3339: ZB = zb_and(n1540, n3336);
    let n3340: ZB = zb_or(n3338, n3339);
    let n3341: ZB = zb_and(n1542, n3340);
    let n3342: ZB = zb_and(n1543, n3340);
    let n3343: ZB = zb_and(n1544, n3342);
    let n3344: ZB = zb_and(n1545, n3342);
    let n3345: ZB = zb_or(n3343, n3344);
    let n3346: ZB = zb_or(n3341, n3345);
    let n3347: ZB = zb_and(n1549, n3346);
    let n3348: ZB = zb_and(n1548, n3346);
    let n3349: ZB = zb_or(n3347, n3348);
    let n3350: ZB = zb_or(n3337, n3349);
    let n3351: ZB = zb_or(n3122, n3350);
    let n3352: ZB = zb_and(n1853, n3351);
    let n3353: ZB = zb_and(n1854, n3351);
    let n3354: ZB = zb_or(n3352, n3353);
    let n3355: ZB = zb_and(n1411, n3266);
    let n3356: ZB = zb_and(r_c292, n3266);
    let n3357: ZB = zb_or(n3355, n3356);
    let n3358: ZB = zb_or(n3122, n3357);
    let n3359: ZB = zb_and(n1853, n3358);
    let n3360: ZB = zb_and(n1854, n3358);
    let n3361: ZB = zb_or(n3359, n3360);
    let n3362: ZB = zb_and(n1411, n3295);
    let n3363: ZB = zb_and(r_c292, n3295);
    let n3364: ZB = zb_or(n3362, n3363);
    let n3365: ZB = zb_or(n3122, n3364);
    let n3366: ZB = zb_and(n1853, n3365);
    let n3367: ZB = zb_and(n1854, n3365);
    let n3368: ZB = zb_or(n3366, n3367);
    let n3369: ZB = zb_or(n3336, n3337);
    let n3370: ZB = zb_or(n3122, n3369);
    let n3371: ZB = zb_and(n1853, n3370);
    let n3372: ZB = zb_and(n1854, n3370);
    let n3373: ZB = zb_or(n3371, n3372);
    let n3374: ZB = zb_and(n806, n2343);
    let n3375: ZB = zb_and(n816, n2350);
    let n3376: ZB = zb_and(n835, n2357);
    let n3377: ZB = zb_and(n859, n2367);
    let n3378: ZB = zb_or(n3376, n3377);
    let n3379: ZB = zb_or(n3375, n3378);
    let n3380: ZB = zb_or(n3374, n3379);
    let n3381: ZB = zb_and(n886, n2379);
    let n3382: ZB = zb_and(n894, n2386);
    let n3383: ZB = zb_and(n910, n2393);
    let n3384: ZB = zb_and(n926, n2403);
    let n3385: ZB = zb_or(n3383, n3384);
    let n3386: ZB = zb_or(n3382, n3385);
    let n3387: ZB = zb_or(n3381, n3386);
    let n3388: ZB = zb_and(n953, n2415);
    let n3389: ZB = zb_and(n961, n2422);
    let n3390: ZB = zb_and(n977, n2429);
    let n3391: ZB = zb_and(n993, n2439);
    let n3392: ZB = zb_or(n3390, n3391);
    let n3393: ZB = zb_or(n3389, n3392);
    let n3394: ZB = zb_or(n3388, n3393);
    let n3395: ZB = zb_or(n3387, n3394);
    let n3396: ZB = zb_or(n3380, n3395);
    let n3397: ZB = zb_and(n1027, n2456);
    let n3398: ZB = zb_and(n1035, n2463);
    let n3399: ZB = zb_and(n1051, n2470);
    let n3400: ZB = zb_and(n1071, n2480);
    let n3401: ZB = zb_or(n3399, n3400);
    let n3402: ZB = zb_or(n3398, n3401);
    let n3403: ZB = zb_or(n3397, n3402);
    let n3404: ZB = zb_and(n1090, n2492);
    let n3405: ZB = zb_and(n1098, n2499);
    let n3406: ZB = zb_and(n1114, n2506);
    let n3407: ZB = zb_and(n1130, n2516);
    let n3408: ZB = zb_or(n3406, n3407);
    let n3409: ZB = zb_or(n3405, n3408);
    let n3410: ZB = zb_or(n3404, n3409);
    let n3411: ZB = zb_and(n1149, n2528);
    let n3412: ZB = zb_and(n1157, n2535);
    let n3413: ZB = zb_and(n1173, n2542);
    let n3414: ZB = zb_and(n1189, n2552);
    let n3415: ZB = zb_or(n3413, n3414);
    let n3416: ZB = zb_or(n3412, n3415);
    let n3417: ZB = zb_or(n3411, n3416);
    let n3418: ZB = zb_or(n3410, n3417);
    let n3419: ZB = zb_or(n3403, n3418);
    let n3420: ZB = zb_and(n1221, n2569);
    let n3421: ZB = zb_and(n1229, n2576);
    let n3422: ZB = zb_and(n1245, n2583);
    let n3423: ZB = zb_and(n1265, n2593);
    let n3424: ZB = zb_or(n3422, n3423);
    let n3425: ZB = zb_or(n3421, n3424);
    let n3426: ZB = zb_or(n3420, n3425);
    let n3427: ZB = zb_and(n1284, n2605);
    let n3428: ZB = zb_and(n1292, n2612);
    let n3429: ZB = zb_and(n1308, n2619);
    let n3430: ZB = zb_and(n1324, n2629);
    let n3431: ZB = zb_or(n3429, n3430);
    let n3432: ZB = zb_or(n3428, n3431);
    let n3433: ZB = zb_or(n3427, n3432);
    let n3434: ZB = zb_and(n1343, n2641);
    let n3435: ZB = zb_and(n1351, n2648);
    let n3436: ZB = zb_and(n1367, n2655);
    let n3437: ZB = zb_and(n1383, n2665);
    let n3438: ZB = zb_or(n3436, n3437);
    let n3439: ZB = zb_or(n3435, n3438);
    let n3440: ZB = zb_or(n3434, n3439);
    let n3441: ZB = zb_or(n3433, n3440);
    let n3442: ZB = zb_or(n3426, n3441);
    let n3443: ZB = zb_or(n3419, n3442);
    let n3444: ZB = zsel_b(n3419, n1004, n1198);
    let n3445: ZB = zb_or(n3396, n3443);
    let n3446: ZB = zsel_b(n3396, n756, n3444);
    let n3447: ZB = zsel_b(n3445, n3446, n1401);
    let n3448: ZB = zb_and(n1404, n3445);
    let n3449: ZB = zb_and(n1403, n3445);
    let n3450: ZB = zb_or(n3448, n3449);
    let n3451: ZB = zb_and(n1404, n3450);
    let n3452: ZB = zb_and(n1403, n3450);
    let n3453: ZB = zb_or(n3451, n3452);
    let n3454: ZB = zb_and(n1403, n3453);
    let n3455: ZB = zb_and(n1404, n3453);
    let n3456: ZB = zb_and(n1414, n3455);
    let n3457: ZB = zb_and(n1415, n3455);
    let n3458: ZB = zb_or(n3456, n3457);
    let n3459: ZB = zb_or(n3454, n3458);
    let n3460: ZB = zb_and(n1424, n3459);
    let n3461: ZB = zb_and(n1425, n3459);
    let n3462: ZB = zb_and(n1429, n3460);
    let n3463: ZB = zb_and(n1430, n3460);
    let n3464: ZB = zb_or(n3462, n3463);
    let n3465: ZB = zb_and(n1439, n3464);
    let n3466: ZB = zb_and(n1440, n3464);
    let n3467: ZB = zb_or(n3465, n3466);
    let n3468: ZB = zb_and(n1404, n3461);
    let n3469: ZB = zb_and(n1403, n3461);
    let n3470: ZB = zb_or(n3468, n3469);
    let n3471: ZB = zb_and(n1454, n3470);
    let n3472: ZB = zb_and(n1455, n3470);
    let n3473: ZB = zb_and(n1458, n3471);
    let n3474: ZB = zb_and(n833, n3471);
    let n3475: ZB = zb_and(n1461, n3474);
    let n3476: ZB = zb_and(n857, n3474);
    let n3477: ZB = zb_and(n1464, n3473);
    let n3478: ZB = zb_and(n1465, n3473);
    let n3479: ZB = zb_and(n1472, n3475);
    let n3480: ZB = zb_and(n1473, n3475);
    let n3481: ZB = zb_and(n833, n3476);
    let n3482: ZB = zb_or(n3479, n3480);
    let n3483: ZB = zb_or(n3477, n3478);
    let n3484: ZB = zb_or(n3481, n3482);
    let n3485: ZB = zb_or(n3483, n3484);
    let n3486: ZB = zb_and(n1458, n3472);
    let n3487: ZB = zb_and(n833, n3472);
    let n3488: ZB = zb_or(n3486, n3487);
    let n3489: ZB = zb_or(n3485, n3488);
    let n3490: ZB = zb_and(n1501, n3489);
    let n3491: ZB = zb_and(n1500, n3489);
    let n3492: ZB = zb_or(n3490, n3491);
    let n3493: ZB = zb_and(n1508, n3492);
    let n3494: ZB = zb_and(n1509, n3492);
    let n3495: ZB = zb_or(n3493, n3494);
    let n3496: ZB = zb_and(n1404, n3495);
    let n3497: ZB = zb_and(n1403, n3495);
    let n3498: ZB = zb_or(n3496, n3497);
    let n3499: ZB = zb_or(n3467, n3498);
    let n3502: ZB = zb_and(n1472, n3472);
    let n3503: ZB = zb_and(n1473, n3472);
    let n3504: ZB = zb_or(n3502, n3503);
    let n3505: ZB = zb_or(n3485, n3504);
    let n3506: ZB = zb_and(n1587, n3505);
    let n3507: ZB = zb_and(n1586, n3505);
    let n3508: ZB = zb_or(n3506, n3507);
    let n3509: ZB = zb_and(n1508, n3508);
    let n3510: ZB = zb_and(n1509, n3508);
    let n3511: ZB = zb_or(n3509, n3510);
    let n3512: ZB = zb_and(n1598, n3511);
    let n3513: ZB = zb_and(n1597, n3511);
    let n3514: ZB = zb_or(n3512, n3513);
    let n3515: ZB = zb_and(n1598, n3514);
    let n3516: ZB = zb_and(n1597, n3514);
    let n3517: ZB = zb_or(n3515, n3516);
    let n3518: ZB = zb_and(n1597, n3517);
    let n3519: ZB = zb_and(n1598, n3517);
    let n3520: ZB = zb_or(n3518, n3519);
    let n3521: ZB = zb_and(n1597, n3520);
    let n3522: ZB = zb_and(n1598, n3520);
    let n3523: ZB = zb_or(n3521, n3522);
    let n3524: ZB = zb_and(n1404, n3523);
    let n3525: ZB = zb_and(n1403, n3523);
    let n3526: ZB = zb_or(n3524, n3525);
    let n3527: ZB = zb_or(n3467, n3526);
    let n3529: ZB = zb_and(n1464, n3472);
    let n3530: ZB = zb_and(n1465, n3472);
    let n3531: ZB = zb_or(n3529, n3530);
    let n3532: ZB = zb_or(n3485, n3531);
    let n3533: ZB = zb_and(n1643, n3532);
    let n3534: ZB = zb_and(n1642, n3532);
    let n3535: ZB = zb_or(n3533, n3534);
    let n3536: ZB = zb_and(n1508, n3535);
    let n3537: ZB = zb_and(n1509, n3535);
    let n3538: ZB = zb_or(n3536, n3537);
    let n3539: ZB = zb_and(n1654, n3538);
    let n3540: ZB = zb_and(n1653, n3538);
    let n3541: ZB = zb_or(n3539, n3540);
    let n3542: ZB = zb_and(n1654, n3541);
    let n3543: ZB = zb_and(n1653, n3541);
    let n3544: ZB = zb_or(n3542, n3543);
    let n3545: ZB = zb_and(n1653, n3544);
    let n3546: ZB = zb_and(n1654, n3544);
    let n3547: ZB = zb_or(n3545, n3546);
    let n3548: ZB = zb_and(n1653, n3547);
    let n3549: ZB = zb_and(n1654, n3547);
    let n3550: ZB = zb_or(n3548, n3549);
    let n3551: ZB = zb_and(n1404, n3550);
    let n3552: ZB = zb_and(n1403, n3550);
    let n3553: ZB = zb_or(n3551, n3552);
    let n3554: ZB = zb_or(n3467, n3553);
    let n3556: ZB = zb_and(n136, n3498);
    let n3557: ZB = zb_and(r_c293, n3498);
    let n3558: ZB = zb_and(n1521, n3556);
    let n3559: ZB = zb_and(n1522, n3556);
    let n3560: ZB = zb_and(n1525, n3559);
    let n3561: ZB = zb_and(n1524, n3559);
    let n3562: ZB = zb_or(n3560, n3561);
    let n3563: ZB = zb_and(n1525, n3562);
    let n3564: ZB = zb_and(n1524, n3562);
    let n3565: ZB = zb_or(n3563, n3564);
    let n3566: ZB = zb_and(n1524, n3565);
    let n3567: ZB = zb_and(n1525, n3565);
    let n3568: ZB = zb_and(n1528, n3567);
    let n3569: ZB = zb_and(n1527, n3567);
    let n3570: ZB = zb_or(n3568, n3569);
    let n3571: ZB = zb_and(n1528, n3570);
    let n3572: ZB = zb_and(n1527, n3570);
    let n3573: ZB = zb_or(n3571, n3572);
    let n3574: ZB = zb_and(n1527, n3573);
    let n3575: ZB = zb_and(n1528, n3573);
    let n3576: ZB = zb_or(n3574, n3575);
    let n3577: ZB = zb_or(n3566, n3576);
    let n3578: ZB = zb_and(n1532, n3577);
    let n3579: ZB = zb_and(n1531, n3577);
    let n3580: ZB = zb_or(n3578, n3579);
    let n3581: ZB = zb_or(n3558, n3580);
    let n3582: ZB = zb_or(n3557, n3581);
    let n3583: ZB = zb_or(n3467, n3582);
    let n3585: ZB = zb_and(n136, n3526);
    let n3586: ZB = zb_and(r_c293, n3526);
    let n3587: ZB = zb_and(n1521, n3585);
    let n3588: ZB = zb_and(n1522, n3585);
    let n3589: ZB = zb_and(n1525, n3588);
    let n3590: ZB = zb_and(n1524, n3588);
    let n3591: ZB = zb_or(n3589, n3590);
    let n3592: ZB = zb_and(n1525, n3591);
    let n3593: ZB = zb_and(n1524, n3591);
    let n3594: ZB = zb_or(n3592, n3593);
    let n3595: ZB = zb_and(n1524, n3594);
    let n3596: ZB = zb_and(n1525, n3594);
    let n3597: ZB = zb_and(n1528, n3596);
    let n3598: ZB = zb_and(n1527, n3596);
    let n3599: ZB = zb_or(n3597, n3598);
    let n3600: ZB = zb_and(n1528, n3599);
    let n3601: ZB = zb_and(n1527, n3599);
    let n3602: ZB = zb_or(n3600, n3601);
    let n3603: ZB = zb_and(n1527, n3602);
    let n3604: ZB = zb_and(n1528, n3602);
    let n3605: ZB = zb_or(n3603, n3604);
    let n3606: ZB = zb_or(n3595, n3605);
    let n3607: ZB = zb_and(n1532, n3606);
    let n3608: ZB = zb_and(n1531, n3606);
    let n3609: ZB = zb_or(n3607, n3608);
    let n3610: ZB = zb_or(n3587, n3609);
    let n3611: ZB = zb_or(n3586, n3610);
    let n3612: ZB = zb_or(n3467, n3611);
    let n3614: ZB = zb_and(n136, n3553);
    let n3615: ZB = zb_and(r_c293, n3553);
    let n3616: ZB = zb_and(n1521, n3614);
    let n3617: ZB = zb_and(n1522, n3614);
    let n3618: ZB = zb_and(n1525, n3617);
    let n3619: ZB = zb_and(n1524, n3617);
    let n3620: ZB = zb_or(n3618, n3619);
    let n3621: ZB = zb_and(n1525, n3620);
    let n3622: ZB = zb_and(n1524, n3620);
    let n3623: ZB = zb_or(n3621, n3622);
    let n3624: ZB = zb_and(n1524, n3623);
    let n3625: ZB = zb_and(n1525, n3623);
    let n3626: ZB = zb_and(n1528, n3625);
    let n3627: ZB = zb_and(n1527, n3625);
    let n3628: ZB = zb_or(n3626, n3627);
    let n3629: ZB = zb_and(n1528, n3628);
    let n3630: ZB = zb_and(n1527, n3628);
    let n3631: ZB = zb_or(n3629, n3630);
    let n3632: ZB = zb_and(n1527, n3631);
    let n3633: ZB = zb_and(n1528, n3631);
    let n3634: ZB = zb_or(n3632, n3633);
    let n3635: ZB = zb_or(n3624, n3634);
    let n3636: ZB = zb_and(n1532, n3635);
    let n3637: ZB = zb_and(n1531, n3635);
    let n3638: ZB = zb_or(n3636, n3637);
    let n3639: ZB = zb_or(n3616, n3638);
    let n3640: ZB = zb_or(n3615, n3639);
    let n3641: ZB = zb_or(n3467, n3640);
    let n3643: ZB = zb_and(n1411, n3498);
    let n3644: ZB = zb_and(r_c292, n3498);
    let n3645: ZB = zb_and(n1505, n3643);
    let n3646: ZB = zb_and(n1540, n3643);
    let n3647: ZB = zb_or(n3645, n3646);
    let n3648: ZB = zb_and(n1542, n3647);
    let n3649: ZB = zb_and(n1543, n3647);
    let n3650: ZB = zb_and(n1544, n3649);
    let n3651: ZB = zb_and(n1545, n3649);
    let n3652: ZB = zb_or(n3650, n3651);
    let n3653: ZB = zb_or(n3648, n3652);
    let n3654: ZB = zb_and(n1549, n3653);
    let n3655: ZB = zb_and(n1548, n3653);
    let n3656: ZB = zb_or(n3654, n3655);
    let n3657: ZB = zb_or(n3644, n3656);
    let n3658: ZB = zb_or(n3467, n3657);
    let n3659: ZB = zb_and(n1853, n3658);
    let n3660: ZB = zb_and(n1854, n3658);
    let n3661: ZB = zb_or(n3659, n3660);
    let n3662: ZB = zb_and(n1411, n3526);
    let n3663: ZB = zb_and(r_c292, n3526);
    let n3664: ZB = zb_or(n3662, n3663);
    let n3665: ZB = zb_or(n3467, n3664);
    let n3666: ZB = zb_and(n1853, n3665);
    let n3667: ZB = zb_and(n1854, n3665);
    let n3668: ZB = zb_or(n3666, n3667);
    let n3669: ZB = zb_and(n1411, n3553);
    let n3670: ZB = zb_and(r_c292, n3553);
    let n3671: ZB = zb_or(n3669, n3670);
    let n3672: ZB = zb_or(n3467, n3671);
    let n3673: ZB = zb_and(n1853, n3672);
    let n3674: ZB = zb_and(n1854, n3672);
    let n3675: ZB = zb_or(n3673, n3674);
    let n3676: ZB = zb_or(n3643, n3644);
    let n3677: ZB = zb_or(n3467, n3676);
    let n3678: ZB = zb_and(n1853, n3677);
    let n3679: ZB = zb_and(n1854, n3677);
    let n3680: ZB = zb_or(n3678, n3679);
    let n3681: ZB = zb_and(n1411, n3582);
    let n3682: ZB = zb_and(r_c292, n3582);
    let n3683: ZB = zb_and(n1505, n3681);
    let n3684: ZB = zb_and(n1540, n3681);
    let n3685: ZB = zb_or(n3683, n3684);
    let n3686: ZB = zb_and(n1542, n3685);
    let n3687: ZB = zb_and(n1543, n3685);
    let n3688: ZB = zb_and(n1544, n3687);
    let n3689: ZB = zb_and(n1545, n3687);
    let n3690: ZB = zb_or(n3688, n3689);
    let n3691: ZB = zb_or(n3686, n3690);
    let n3692: ZB = zb_and(n1549, n3691);
    let n3693: ZB = zb_and(n1548, n3691);
    let n3694: ZB = zb_or(n3692, n3693);
    let n3695: ZB = zb_or(n3682, n3694);
    let n3696: ZB = zb_or(n3467, n3695);
    let n3697: ZB = zb_and(n1853, n3696);
    let n3698: ZB = zb_and(n1854, n3696);
    let n3699: ZB = zb_or(n3697, n3698);
    let n3700: ZB = zb_and(n1411, n3611);
    let n3701: ZB = zb_and(r_c292, n3611);
    let n3702: ZB = zb_or(n3700, n3701);
    let n3703: ZB = zb_or(n3467, n3702);
    let n3704: ZB = zb_and(n1853, n3703);
    let n3705: ZB = zb_and(n1854, n3703);
    let n3706: ZB = zb_or(n3704, n3705);
    let n3707: ZB = zb_and(n1411, n3640);
    let n3708: ZB = zb_and(r_c292, n3640);
    let n3709: ZB = zb_or(n3707, n3708);
    let n3710: ZB = zb_or(n3467, n3709);
    let n3711: ZB = zb_and(n1853, n3710);
    let n3712: ZB = zb_and(n1854, n3710);
    let n3713: ZB = zb_or(n3711, n3712);
    let n3714: ZB = zb_or(n3681, n3682);
    let n3715: ZB = zb_or(n3467, n3714);
    let n3716: ZB = zb_and(n1853, n3715);
    let n3717: ZB = zb_and(n1854, n3715);
    let n3718: ZB = zb_or(n3716, n3717);
    let n3720: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c369);
    let n3721: ZB = zb_not(n2079);
    let n3722: ZB = zb_and(n2078, n3721);
    let n3723: ZB = zb_and(n2063, n2080);
    let n3724: ZB = zb_or(n3722, n3723);
    let n3725: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c368);
    let n3726: ZB = zb_not(n3725);
    let n3727: ZB = zb_and(n3724, n3725);
    let n3728: ZB = zb_and(n3724, n3726);
    let n3729: ZB = zb_not(n3720);
    let n3730: ZB = zb_or(n3727, n3728);
    let n3731: ZB = zb_or(n3726, n3729);
    let n3732: ZB = zb_not(n3731);
    let n3733: ZB = zb_and(n3730, n3731);
    let n3734: ZB = zb_and(n3730, n3732);
    let n3735: ZN = zn_add(r_c366, r_c368);
    let n3736: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n3735);
    let n3737: ZN = zn_flr(n3736);
    let n3738: ZB = zn_gt(n3737, zn_splat(P8::from_raw(0i32)));
    let n3739: ZB = zn_le(n3737, zn_splat(P8::from_raw(0i32)));
    let n3740: ZB = zb_and(n3733, n3738);
    let n3741: ZB = zb_and(n3733, n3739);
    let n3742: ZB = zn_lt(n3737, zn_splat(P8::from_raw(0i32)));
    let n3743: ZB = zn_ge(n3737, zn_splat(P8::from_raw(0i32)));
    let n3744: ZB = zb_and(n3741, n3742);
    let n3745: ZB = zb_and(n3741, n3743);
    let n3746: ZN = zsel_n(n3742, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n3747: ZB = zb_or(n3744, n3745);
    let n3748: ZN = zsel_n(n3738, zn_splat(P8::from_raw(65536i32)), n3746);
    let n3749: ZB = zb_or(n3740, n3747);
    let n3750: ZN = zn_abs(n3737);
    let n3751: ZN = zn_add(n217, n3748);
    let n3752: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n220);
    let n3753: ZB = zn_tile_flag_at(g.cache, g.cart, n3751, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3754: ZB = zb_not(n3753);
    let n3755: ZB = zb_and(n3749, n3754);
    let n3756: ZB = zb_and(n3749, n3753);
    let n3757: ZB = zb_or(n3755, n3756);
    let n3758: ZB = zb_and(n3754, n3757);
    let n3759: ZB = zb_and(n3753, n3757);
    let n3760: ZB = zb_or(n3758, n3759);
    let n3761: ZB = zb_and(n3754, n3760);
    let n3762: ZB = zb_and(n3753, n3760);
    let n3763: ZN = zn_add(r_c299, n3748);
    let n3764: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n3750);
    let n3765: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n3750);
    let n3766: ZB = zb_and(n3761, n3764);
    let n3767: ZB = zb_and(n3761, n3765);
    let n3768: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3763);
    let n3769: ZN = zn_add(n3748, n3768);
    let n3770: ZB = zn_tile_flag_at(g.cache, g.cart, n3769, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3771: ZB = zb_not(n3770);
    let n3772: ZB = zb_and(n3766, n3771);
    let n3773: ZB = zb_and(n3766, n3770);
    let n3774: ZB = zb_or(n3772, n3773);
    let n3775: ZB = zb_and(n3771, n3774);
    let n3776: ZB = zb_and(n3770, n3774);
    let n3777: ZB = zb_or(n3775, n3776);
    let n3778: ZB = zb_and(n3771, n3777);
    let n3779: ZB = zb_and(n3770, n3777);
    let n3780: ZN = zn_add(n3748, n3763);
    let n3781: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n3750);
    let n3782: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n3750);
    let n3783: ZB = zb_and(n3778, n3781);
    let n3784: ZB = zb_and(n3778, n3782);
    let n3785: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3780);
    let n3786: ZN = zn_add(n3748, n3785);
    let n3787: ZB = zn_tile_flag_at(g.cache, g.cart, n3786, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3788: ZB = zb_not(n3787);
    let n3789: ZB = zb_and(n3783, n3788);
    let n3790: ZB = zb_and(n3783, n3787);
    let n3791: ZB = zb_or(n3789, n3790);
    let n3792: ZB = zb_and(n3788, n3791);
    let n3793: ZB = zb_and(n3787, n3791);
    let n3794: ZB = zb_or(n3792, n3793);
    let n3795: ZB = zb_and(n3788, n3794);
    let n3796: ZB = zb_and(n3787, n3794);
    let n3797: ZN = zn_add(n3748, n3780);
    let n3798: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n3750);
    let n3799: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n3750);
    let n3800: ZB = zb_and(n3795, n3798);
    let n3801: ZB = zb_and(n3795, n3799);
    let n3802: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3797);
    let n3803: ZN = zn_add(n3748, n3802);
    let n3804: ZB = zn_tile_flag_at(g.cache, g.cart, n3803, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3805: ZB = zb_not(n3804);
    let n3806: ZB = zb_and(n3800, n3805);
    let n3807: ZB = zb_and(n3800, n3804);
    let n3808: ZB = zb_or(n3806, n3807);
    let n3809: ZB = zb_and(n3805, n3808);
    let n3810: ZB = zb_and(n3804, n3808);
    let n3811: ZB = zb_or(n3809, n3810);
    let n3812: ZB = zb_and(n3805, n3811);
    let n3813: ZB = zb_and(n3804, n3811);
    let n3814: ZN = zn_add(n3748, n3797);
    let n3815: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n3750);
    let n3816: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n3750);
    let n3817: ZB = zb_and(n3812, n3815);
    let n3818: ZB = zb_and(n3812, n3816);
    let n3819: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3814);
    let n3820: ZN = zn_add(n3748, n3819);
    let n3821: ZB = zn_tile_flag_at(g.cache, g.cart, n3820, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3822: ZB = zb_not(n3821);
    let n3823: ZB = zb_and(n3817, n3822);
    let n3824: ZB = zb_and(n3817, n3821);
    let n3825: ZB = zb_or(n3823, n3824);
    let n3826: ZB = zb_and(n3822, n3825);
    let n3827: ZB = zb_and(n3821, n3825);
    let n3828: ZB = zb_or(n3826, n3827);
    let n3829: ZB = zb_and(n3822, n3828);
    let n3830: ZB = zb_and(n3821, n3828);
    let n3831: ZN = zn_add(n3748, n3814);
    let n3832: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n3750);
    let n3833: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n3750);
    let n3834: ZB = zb_and(n3829, n3832);
    let n3835: ZB = zb_and(n3829, n3833);
    let n3836: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3831);
    let n3837: ZN = zn_add(n3748, n3836);
    let n3838: ZB = zn_tile_flag_at(g.cache, g.cart, n3837, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3839: ZB = zb_not(n3838);
    let n3840: ZB = zb_and(n3834, n3839);
    let n3841: ZB = zb_and(n3834, n3838);
    let n3842: ZB = zb_or(n3840, n3841);
    let n3843: ZB = zb_and(n3839, n3842);
    let n3844: ZB = zb_and(n3838, n3842);
    let n3845: ZB = zb_or(n3843, n3844);
    let n3846: ZB = zb_and(n3839, n3845);
    let n3847: ZB = zb_and(n3838, n3845);
    let n3848: ZN = zn_add(n3748, n3831);
    let n3849: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n3750);
    let n3850: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n3750);
    let n3851: ZB = zb_and(n3846, n3849);
    let n3852: ZB = zb_and(n3846, n3850);
    let n3853: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3848);
    let n3854: ZN = zn_add(n3748, n3853);
    let n3855: ZB = zn_tile_flag_at(g.cache, g.cart, n3854, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3856: ZB = zb_not(n3855);
    let n3857: ZB = zb_and(n3851, n3856);
    let n3858: ZB = zb_and(n3851, n3855);
    let n3859: ZB = zb_or(n3857, n3858);
    let n3860: ZB = zb_and(n3856, n3859);
    let n3861: ZB = zb_and(n3855, n3859);
    let n3862: ZB = zb_or(n3860, n3861);
    let n3863: ZB = zb_and(n3856, n3862);
    let n3864: ZB = zb_and(n3855, n3862);
    let n3865: ZN = zn_add(n3748, n3848);
    let n3866: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n3750);
    let n3867: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n3750);
    let n3868: ZB = zb_and(n3863, n3866);
    let n3869: ZB = zb_and(n3863, n3867);
    let n3870: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3865);
    let n3871: ZN = zn_add(n3748, n3870);
    let n3872: ZB = zn_tile_flag_at(g.cache, g.cart, n3871, n3752, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3873: ZB = zb_not(n3872);
    let n3874: ZB = zb_and(n3868, n3873);
    let n3875: ZB = zb_and(n3868, n3872);
    let n3876: ZB = zb_or(n3874, n3875);
    let n3877: ZB = zb_and(n3873, n3876);
    let n3878: ZB = zb_and(n3872, n3876);
    let n3879: ZB = zb_or(n3877, n3878);
    let n3880: ZB = zb_and(n3873, n3879);
    let n3881: ZB = zb_and(n3872, n3879);
    let n3882: ZN = zn_add(n3748, n3865);
    let n3883: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n3750);
    let n3884: ZN = zsel_n(n3872, n3865, n3882);
    let n3885: ZN = zsel_n(n3872, zn_splat(P8::from_raw(0i32)), r_c368);
    let n3886: ZB = zb_or(n3880, n3881);
    let n3887: ZB = zb_or(n3872, n3883);
    let n3888: ZN = zsel_n(n3867, n3865, n3884);
    let n3889: ZN = zsel_n(n3867, r_c368, n3885);
    let n3890: ZB = zb_or(n3869, n3886);
    let n3891: ZB = zb_or(n3867, n3887);
    let n3892: ZN = zsel_n(n3855, n3848, n3888);
    let n3893: ZN = zsel_n(n3855, zn_splat(P8::from_raw(0i32)), n3889);
    let n3894: ZB = zb_or(n3864, n3890);
    let n3895: ZB = zb_or(n3855, n3891);
    let n3896: ZN = zsel_n(n3850, n3848, n3892);
    let n3897: ZN = zsel_n(n3850, r_c368, n3893);
    let n3898: ZB = zb_or(n3852, n3894);
    let n3899: ZB = zb_or(n3850, n3895);
    let n3900: ZN = zsel_n(n3838, n3831, n3896);
    let n3901: ZN = zsel_n(n3838, zn_splat(P8::from_raw(0i32)), n3897);
    let n3902: ZB = zb_or(n3847, n3898);
    let n3903: ZB = zb_or(n3838, n3899);
    let n3904: ZN = zsel_n(n3833, n3831, n3900);
    let n3905: ZN = zsel_n(n3833, r_c368, n3901);
    let n3906: ZB = zb_or(n3835, n3902);
    let n3907: ZB = zb_or(n3833, n3903);
    let n3908: ZN = zsel_n(n3821, n3814, n3904);
    let n3909: ZN = zsel_n(n3821, zn_splat(P8::from_raw(0i32)), n3905);
    let n3910: ZB = zb_or(n3830, n3906);
    let n3911: ZB = zb_or(n3821, n3907);
    let n3912: ZN = zsel_n(n3816, n3814, n3908);
    let n3913: ZN = zsel_n(n3816, r_c368, n3909);
    let n3914: ZB = zb_or(n3818, n3910);
    let n3915: ZB = zb_or(n3816, n3911);
    let n3916: ZN = zsel_n(n3804, n3797, n3912);
    let n3917: ZN = zsel_n(n3804, zn_splat(P8::from_raw(0i32)), n3913);
    let n3918: ZB = zb_or(n3813, n3914);
    let n3919: ZB = zb_or(n3804, n3915);
    let n3920: ZN = zsel_n(n3799, n3797, n3916);
    let n3921: ZN = zsel_n(n3799, r_c368, n3917);
    let n3922: ZB = zb_or(n3801, n3918);
    let n3923: ZB = zb_or(n3799, n3919);
    let n3924: ZN = zsel_n(n3787, n3780, n3920);
    let n3925: ZN = zsel_n(n3787, zn_splat(P8::from_raw(0i32)), n3921);
    let n3926: ZB = zb_or(n3796, n3922);
    let n3927: ZB = zb_or(n3787, n3923);
    let n3928: ZN = zsel_n(n3782, n3780, n3924);
    let n3929: ZN = zsel_n(n3782, r_c368, n3925);
    let n3930: ZB = zb_or(n3784, n3926);
    let n3931: ZB = zb_or(n3782, n3927);
    let n3932: ZN = zsel_n(n3770, n3763, n3928);
    let n3933: ZN = zsel_n(n3770, zn_splat(P8::from_raw(0i32)), n3929);
    let n3934: ZB = zb_or(n3779, n3930);
    let n3935: ZB = zb_or(n3770, n3931);
    let n3936: ZN = zsel_n(n3765, n3763, n3932);
    let n3937: ZN = zsel_n(n3765, r_c368, n3933);
    let n3938: ZB = zb_or(n3767, n3934);
    let n3939: ZB = zb_or(n3765, n3935);
    let n3940: ZN = zsel_n(n3753, r_c299, n3936);
    let n3941: ZN = zsel_n(n3753, zn_splat(P8::from_raw(0i32)), n3937);
    let n3942: ZB = zb_or(n3762, n3938);
    let n3943: ZB = zb_or(n3753, n3939);
    let n3944: ZN = zn_add(r_c367, r_c369);
    let n3945: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n3944);
    let n3946: ZN = zn_flr(n3945);
    let n3947: ZB = zn_gt(n3946, zn_splat(P8::from_raw(0i32)));
    let n3948: ZB = zn_le(n3946, zn_splat(P8::from_raw(0i32)));
    let n3949: ZB = zb_and(n3942, n3947);
    let n3950: ZB = zb_and(n3942, n3948);
    let n3951: ZB = zn_lt(n3946, zn_splat(P8::from_raw(0i32)));
    let n3952: ZB = zn_ge(n3946, zn_splat(P8::from_raw(0i32)));
    let n3953: ZB = zb_and(n3950, n3951);
    let n3954: ZB = zb_and(n3950, n3952);
    let n3955: ZN = zsel_n(n3951, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n3956: ZB = zb_or(n3953, n3954);
    let n3957: ZN = zsel_n(n3947, zn_splat(P8::from_raw(65536i32)), n3955);
    let n3958: ZB = zb_or(n3949, n3956);
    let n3959: ZN = zn_abs(n3946);
    let n3960: ZB = zn_gt(n3957, zn_splat(P8::from_raw(0i32)));
    let n3961: ZB = zn_le(n3957, zn_splat(P8::from_raw(0i32)));
    let n3962: ZB = zb_and(n3958, n3960);
    let n3963: ZB = zb_and(n3958, n3961);
    let n3964: ZB = zb_or(n3962, n3963);
    let n3965: ZB = zb_and(n3960, n3964);
    let n3966: ZB = zb_and(n3961, n3964);
    let n3967: ZB = zb_or(n3965, n3966);
    let n3968: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n3940);
    let n3969: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n3968);
    let n3970: ZN = zn_add(n220, n3957);
    let n3971: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n3970, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3972: ZB = zb_not(n3971);
    let n3973: ZB = zb_and(n3967, n3972);
    let n3974: ZB = zb_and(n3967, n3971);
    let n3975: ZB = zb_or(n3973, n3974);
    let n3976: ZB = zb_and(n3972, n3975);
    let n3977: ZB = zb_and(n3971, n3975);
    let n3978: ZB = zb_or(n3976, n3977);
    let n3979: ZB = zb_and(n3972, n3978);
    let n3980: ZB = zb_and(n3971, n3978);
    let n3981: ZN = zn_add(r_c300, n3957);
    let n3982: ZB = zn_le(zn_splat(P8::from_raw(65536i32)), n3959);
    let n3983: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n3959);
    let n3984: ZB = zb_and(n3979, n3982);
    let n3985: ZB = zb_and(n3979, n3983);
    let n3986: ZB = zb_and(n3960, n3984);
    let n3987: ZB = zb_and(n3961, n3984);
    let n3988: ZB = zb_or(n3986, n3987);
    let n3989: ZB = zb_and(n3960, n3988);
    let n3990: ZB = zb_and(n3961, n3988);
    let n3991: ZB = zb_or(n3989, n3990);
    let n3992: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n3981);
    let n3993: ZN = zn_add(n3957, n3992);
    let n3994: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n3993, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n3995: ZB = zb_not(n3994);
    let n3996: ZB = zb_and(n3991, n3995);
    let n3997: ZB = zb_and(n3991, n3994);
    let n3998: ZB = zb_or(n3996, n3997);
    let n3999: ZB = zb_and(n3995, n3998);
    let n4000: ZB = zb_and(n3994, n3998);
    let n4001: ZB = zb_or(n3999, n4000);
    let n4002: ZB = zb_and(n3995, n4001);
    let n4003: ZB = zb_and(n3994, n4001);
    let n4004: ZN = zn_add(n3957, n3981);
    let n4005: ZB = zn_le(zn_splat(P8::from_raw(131072i32)), n3959);
    let n4006: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n3959);
    let n4007: ZB = zb_and(n4002, n4005);
    let n4008: ZB = zb_and(n4002, n4006);
    let n4009: ZB = zb_and(n3960, n4007);
    let n4010: ZB = zb_and(n3961, n4007);
    let n4011: ZB = zb_or(n4009, n4010);
    let n4012: ZB = zb_and(n3960, n4011);
    let n4013: ZB = zb_and(n3961, n4011);
    let n4014: ZB = zb_or(n4012, n4013);
    let n4015: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4004);
    let n4016: ZN = zn_add(n3957, n4015);
    let n4017: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4016, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4018: ZB = zb_not(n4017);
    let n4019: ZB = zb_and(n4014, n4018);
    let n4020: ZB = zb_and(n4014, n4017);
    let n4021: ZB = zb_or(n4019, n4020);
    let n4022: ZB = zb_and(n4018, n4021);
    let n4023: ZB = zb_and(n4017, n4021);
    let n4024: ZB = zb_or(n4022, n4023);
    let n4025: ZB = zb_and(n4018, n4024);
    let n4026: ZB = zb_and(n4017, n4024);
    let n4027: ZN = zn_add(n3957, n4004);
    let n4028: ZB = zn_le(zn_splat(P8::from_raw(196608i32)), n3959);
    let n4029: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n3959);
    let n4030: ZB = zb_and(n4025, n4028);
    let n4031: ZB = zb_and(n4025, n4029);
    let n4032: ZB = zb_and(n3960, n4030);
    let n4033: ZB = zb_and(n3961, n4030);
    let n4034: ZB = zb_or(n4032, n4033);
    let n4035: ZB = zb_and(n3960, n4034);
    let n4036: ZB = zb_and(n3961, n4034);
    let n4037: ZB = zb_or(n4035, n4036);
    let n4038: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4027);
    let n4039: ZN = zn_add(n3957, n4038);
    let n4040: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4039, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4041: ZB = zb_not(n4040);
    let n4042: ZB = zb_and(n4037, n4041);
    let n4043: ZB = zb_and(n4037, n4040);
    let n4044: ZB = zb_or(n4042, n4043);
    let n4045: ZB = zb_and(n4041, n4044);
    let n4046: ZB = zb_and(n4040, n4044);
    let n4047: ZB = zb_or(n4045, n4046);
    let n4048: ZB = zb_and(n4041, n4047);
    let n4049: ZB = zb_and(n4040, n4047);
    let n4050: ZN = zn_add(n3957, n4027);
    let n4051: ZB = zn_le(zn_splat(P8::from_raw(262144i32)), n3959);
    let n4052: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n3959);
    let n4053: ZB = zb_and(n4048, n4051);
    let n4054: ZB = zb_and(n4048, n4052);
    let n4055: ZB = zb_and(n3960, n4053);
    let n4056: ZB = zb_and(n3961, n4053);
    let n4057: ZB = zb_or(n4055, n4056);
    let n4058: ZB = zb_and(n3960, n4057);
    let n4059: ZB = zb_and(n3961, n4057);
    let n4060: ZB = zb_or(n4058, n4059);
    let n4061: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4050);
    let n4062: ZN = zn_add(n3957, n4061);
    let n4063: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4062, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4064: ZB = zb_not(n4063);
    let n4065: ZB = zb_and(n4060, n4064);
    let n4066: ZB = zb_and(n4060, n4063);
    let n4067: ZB = zb_or(n4065, n4066);
    let n4068: ZB = zb_and(n4064, n4067);
    let n4069: ZB = zb_and(n4063, n4067);
    let n4070: ZB = zb_or(n4068, n4069);
    let n4071: ZB = zb_and(n4064, n4070);
    let n4072: ZB = zb_and(n4063, n4070);
    let n4073: ZN = zn_add(n3957, n4050);
    let n4074: ZB = zn_le(zn_splat(P8::from_raw(327680i32)), n3959);
    let n4075: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n3959);
    let n4076: ZB = zb_and(n4071, n4074);
    let n4077: ZB = zb_and(n4071, n4075);
    let n4078: ZB = zb_and(n3960, n4076);
    let n4079: ZB = zb_and(n3961, n4076);
    let n4080: ZB = zb_or(n4078, n4079);
    let n4081: ZB = zb_and(n3960, n4080);
    let n4082: ZB = zb_and(n3961, n4080);
    let n4083: ZB = zb_or(n4081, n4082);
    let n4084: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4073);
    let n4085: ZN = zn_add(n3957, n4084);
    let n4086: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4085, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4087: ZB = zb_not(n4086);
    let n4088: ZB = zb_and(n4083, n4087);
    let n4089: ZB = zb_and(n4083, n4086);
    let n4090: ZB = zb_or(n4088, n4089);
    let n4091: ZB = zb_and(n4087, n4090);
    let n4092: ZB = zb_and(n4086, n4090);
    let n4093: ZB = zb_or(n4091, n4092);
    let n4094: ZB = zb_and(n4087, n4093);
    let n4095: ZB = zb_and(n4086, n4093);
    let n4096: ZN = zn_add(n3957, n4073);
    let n4097: ZB = zn_le(zn_splat(P8::from_raw(393216i32)), n3959);
    let n4098: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n3959);
    let n4099: ZB = zb_and(n4094, n4097);
    let n4100: ZB = zb_and(n4094, n4098);
    let n4101: ZB = zb_and(n3960, n4099);
    let n4102: ZB = zb_and(n3961, n4099);
    let n4103: ZB = zb_or(n4101, n4102);
    let n4104: ZB = zb_and(n3960, n4103);
    let n4105: ZB = zb_and(n3961, n4103);
    let n4106: ZB = zb_or(n4104, n4105);
    let n4107: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4096);
    let n4108: ZN = zn_add(n3957, n4107);
    let n4109: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4108, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4110: ZB = zb_not(n4109);
    let n4111: ZB = zb_and(n4106, n4110);
    let n4112: ZB = zb_and(n4106, n4109);
    let n4113: ZB = zb_or(n4111, n4112);
    let n4114: ZB = zb_and(n4110, n4113);
    let n4115: ZB = zb_and(n4109, n4113);
    let n4116: ZB = zb_or(n4114, n4115);
    let n4117: ZB = zb_and(n4110, n4116);
    let n4118: ZB = zb_and(n4109, n4116);
    let n4119: ZN = zn_add(n3957, n4096);
    let n4120: ZB = zn_le(zn_splat(P8::from_raw(458752i32)), n3959);
    let n4121: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n3959);
    let n4122: ZB = zb_and(n4117, n4120);
    let n4123: ZB = zb_and(n4117, n4121);
    let n4124: ZB = zb_and(n3960, n4122);
    let n4125: ZB = zb_and(n3961, n4122);
    let n4126: ZB = zb_or(n4124, n4125);
    let n4127: ZB = zb_and(n3960, n4126);
    let n4128: ZB = zb_and(n3961, n4126);
    let n4129: ZB = zb_or(n4127, n4128);
    let n4130: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4119);
    let n4131: ZN = zn_add(n3957, n4130);
    let n4132: ZB = zn_tile_flag_at(g.cache, g.cart, n3969, n4131, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4133: ZB = zb_not(n4132);
    let n4134: ZB = zb_and(n4129, n4133);
    let n4135: ZB = zb_and(n4129, n4132);
    let n4136: ZB = zb_or(n4134, n4135);
    let n4137: ZB = zb_and(n4133, n4136);
    let n4138: ZB = zb_and(n4132, n4136);
    let n4139: ZB = zb_or(n4137, n4138);
    let n4140: ZB = zb_and(n4133, n4139);
    let n4141: ZB = zb_and(n4132, n4139);
    let n4142: ZN = zn_add(n3957, n4119);
    let n4143: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n3959);
    let n4144: ZB = zb_and(n3943, n4143);
    let n4145: ZN = zsel_n(n4132, n4119, n4142);
    let n4146: ZN = zsel_n(n4132, zn_splat(P8::from_raw(0i32)), r_c369);
    let n4147: ZB = zb_or(n4140, n4141);
    let n4148: ZB = zsel_b(n4132, n3943, n4144);
    let n4149: ZN = zsel_n(n4121, n4119, n4145);
    let n4150: ZN = zsel_n(n4121, r_c369, n4146);
    let n4151: ZB = zb_or(n4123, n4147);
    let n4152: ZB = zsel_b(n4121, n3943, n4148);
    let n4153: ZN = zsel_n(n4109, n4096, n4149);
    let n4154: ZN = zsel_n(n4109, zn_splat(P8::from_raw(0i32)), n4150);
    let n4155: ZB = zb_or(n4118, n4151);
    let n4156: ZB = zsel_b(n4109, n3943, n4152);
    let n4157: ZN = zsel_n(n4098, n4096, n4153);
    let n4158: ZN = zsel_n(n4098, r_c369, n4154);
    let n4159: ZB = zb_or(n4100, n4155);
    let n4160: ZB = zsel_b(n4098, n3943, n4156);
    let n4161: ZN = zsel_n(n4086, n4073, n4157);
    let n4162: ZN = zsel_n(n4086, zn_splat(P8::from_raw(0i32)), n4158);
    let n4163: ZB = zb_or(n4095, n4159);
    let n4164: ZB = zsel_b(n4086, n3943, n4160);
    let n4165: ZN = zsel_n(n4075, n4073, n4161);
    let n4166: ZN = zsel_n(n4075, r_c369, n4162);
    let n4167: ZB = zb_or(n4077, n4163);
    let n4168: ZB = zsel_b(n4075, n3943, n4164);
    let n4169: ZN = zsel_n(n4063, n4050, n4165);
    let n4170: ZN = zsel_n(n4063, zn_splat(P8::from_raw(0i32)), n4166);
    let n4171: ZB = zb_or(n4072, n4167);
    let n4172: ZB = zsel_b(n4063, n3943, n4168);
    let n4173: ZN = zsel_n(n4052, n4050, n4169);
    let n4174: ZN = zsel_n(n4052, r_c369, n4170);
    let n4175: ZB = zb_or(n4054, n4171);
    let n4176: ZB = zsel_b(n4052, n3943, n4172);
    let n4177: ZN = zsel_n(n4040, n4027, n4173);
    let n4178: ZN = zsel_n(n4040, zn_splat(P8::from_raw(0i32)), n4174);
    let n4179: ZB = zb_or(n4049, n4175);
    let n4180: ZB = zsel_b(n4040, n3943, n4176);
    let n4181: ZN = zsel_n(n4029, n4027, n4177);
    let n4182: ZN = zsel_n(n4029, r_c369, n4178);
    let n4183: ZB = zb_or(n4031, n4179);
    let n4184: ZB = zsel_b(n4029, n3943, n4180);
    let n4185: ZN = zsel_n(n4017, n4004, n4181);
    let n4186: ZN = zsel_n(n4017, zn_splat(P8::from_raw(0i32)), n4182);
    let n4187: ZB = zb_or(n4026, n4183);
    let n4188: ZB = zsel_b(n4017, n3943, n4184);
    let n4189: ZN = zsel_n(n4006, n4004, n4185);
    let n4190: ZN = zsel_n(n4006, r_c369, n4186);
    let n4191: ZB = zb_or(n4008, n4187);
    let n4192: ZB = zsel_b(n4006, n3943, n4188);
    let n4193: ZN = zsel_n(n3994, n3981, n4189);
    let n4194: ZN = zsel_n(n3994, zn_splat(P8::from_raw(0i32)), n4190);
    let n4195: ZB = zb_or(n4003, n4191);
    let n4196: ZB = zsel_b(n3994, n3943, n4192);
    let n4197: ZN = zsel_n(n3983, n3981, n4193);
    let n4198: ZN = zsel_n(n3983, r_c369, n4194);
    let n4199: ZB = zb_or(n3985, n4195);
    let n4200: ZB = zsel_b(n3983, n3943, n4196);
    let n4201: ZN = zsel_n(n3971, r_c300, n4197);
    let n4202: ZN = zsel_n(n3971, zn_splat(P8::from_raw(0i32)), n4198);
    let n4203: ZB = zb_or(n3980, n4199);
    let n4204: ZB = zsel_b(n3971, n3943, n4200);
    let n4205: ZN = zsel_n(n3731, n3940, r_c299);
    let n4206: ZN = zsel_n(n3731, n4201, r_c300);
    let n4207: ZN = zsel_n(n3731, n3941, r_c368);
    let n4208: ZN = zsel_n(n3731, n4202, r_c369);
    let n4209: ZB = zb_or(n3734, n4203);
    let n4210: ZB = zb_or(n3732, n4204);
    let n4211: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n4205);
    let n4212: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4206);
    let n4213: ZN = zn_div(n4211, zn_splat(P8::from_raw(524288i32)));
    let n4214: ZN = zn_flr(n4213);
    let n4215: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4214);
    let n4216: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n4211);
    let n4217: ZN = zn_sub(n4216, zn_splat(P8::from_raw(65536i32)));
    let n4218: ZN = zn_div(n4217, zn_splat(P8::from_raw(524288i32)));
    let n4219: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n4218);
    let n4220: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4215);
    let n4221: ZB = zn_le(n4220, n4219);
    let n4222: ZB = zn_gt(n4220, n4219);
    let n4223: ZB = zb_and(n4209, n4221);
    let n4224: ZB = zb_and(n4209, n4222);
    let n4225: ZN = zn_div(n4212, zn_splat(P8::from_raw(524288i32)));
    let n4226: ZN = zn_flr(n4225);
    let n4227: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4226);
    let n4228: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n4212);
    let n4229: ZN = zn_sub(n4228, zn_splat(P8::from_raw(65536i32)));
    let n4230: ZN = zn_div(n4229, zn_splat(P8::from_raw(524288i32)));
    let n4231: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n4230);
    let n4232: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4227);
    let n4233: ZB = zn_le(n4232, n4231);
    let n4234: ZB = zn_gt(n4232, n4231);
    let n4235: ZB = zb_and(n4223, n4233);
    let n4236: ZB = zb_and(n4223, n4234);
    let n4237: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n4220);
    let n4238: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4232);
    let n4239: ZN = zn_mget(g.cart, n4237, n4238);
    let n4240: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4239);
    let n4241: ZB = zb_not(n4240);
    let n4242: ZB = zb_and(n4235, n4240);
    let n4243: ZB = zb_and(n4235, n4241);
    let n4244: ZN = zn_rem(n4229, zn_splat(P8::from_raw(524288i32)));
    let n4245: ZB = zn_ge(n4244, zn_splat(P8::from_raw(393216i32)));
    let n4246: ZB = zn_lt(n4244, zn_splat(P8::from_raw(393216i32)));
    let n4247: ZB = zb_and(n4242, n4246);
    let n4248: ZB = zb_and(n4242, n4245);
    let n4249: ZN = zn_mul(n4232, zn_splat(P8::from_raw(524288i32)));
    let n4250: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4249);
    let n4251: ZB = zn_eq(n4228, n4250);
    let n4252: ZB = zb_or(n4247, n4248);
    let n4253: ZB = zb_or(n4245, n4251);
    let n4254: ZB = zb_or(n4243, n4252);
    let n4255: ZB = zb_and(n4240, n4253);
    let n4256: ZB = zb_not(n4255);
    let n4257: ZB = zb_and(n4254, n4255);
    let n4258: ZB = zb_and(n4254, n4256);
    let n4259: ZB = zn_ge(n4208, zn_splat(P8::from_raw(0i32)));
    let n4260: ZB = zb_or(n4257, n4258);
    let n4261: ZB = zb_and(n4255, n4259);
    let n4262: ZB = zb_not(n4261);
    let n4263: ZB = zb_and(n4260, n4261);
    let n4264: ZB = zb_and(n4260, n4262);
    let n4265: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4239);
    let n4266: ZB = zb_not(n4265);
    let n4267: ZB = zb_and(n4264, n4265);
    let n4268: ZB = zb_and(n4264, n4266);
    let n4269: ZN = zn_rem(n4212, zn_splat(P8::from_raw(524288i32)));
    let n4270: ZB = zn_le(n4269, zn_splat(P8::from_raw(131072i32)));
    let n4271: ZB = zb_or(n4267, n4268);
    let n4272: ZB = zb_and(n4265, n4270);
    let n4273: ZB = zb_not(n4272);
    let n4274: ZB = zb_and(n4271, n4272);
    let n4275: ZB = zb_and(n4271, n4273);
    let n4276: ZB = zn_le(n4208, zn_splat(P8::from_raw(0i32)));
    let n4277: ZB = zb_or(n4274, n4275);
    let n4278: ZB = zb_and(n4272, n4276);
    let n4279: ZB = zb_not(n4278);
    let n4280: ZB = zb_and(n4277, n4278);
    let n4281: ZB = zb_and(n4277, n4279);
    let n4282: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4239);
    let n4283: ZB = zb_not(n4282);
    let n4284: ZB = zb_and(n4281, n4282);
    let n4285: ZB = zb_and(n4281, n4283);
    let n4286: ZN = zn_rem(n4211, zn_splat(P8::from_raw(524288i32)));
    let n4287: ZB = zn_le(n4286, zn_splat(P8::from_raw(131072i32)));
    let n4288: ZB = zb_or(n4284, n4285);
    let n4289: ZB = zb_and(n4282, n4287);
    let n4290: ZB = zb_not(n4289);
    let n4291: ZB = zb_and(n4288, n4289);
    let n4292: ZB = zb_and(n4288, n4290);
    let n4293: ZB = zn_le(n4207, zn_splat(P8::from_raw(0i32)));
    let n4294: ZB = zb_or(n4291, n4292);
    let n4295: ZB = zb_and(n4289, n4293);
    let n4296: ZB = zb_not(n4295);
    let n4297: ZB = zb_and(n4294, n4295);
    let n4298: ZB = zb_and(n4294, n4296);
    let n4299: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4239);
    let n4300: ZB = zb_not(n4299);
    let n4301: ZB = zb_and(n4298, n4299);
    let n4302: ZB = zb_and(n4298, n4300);
    let n4303: ZN = zn_rem(n4217, zn_splat(P8::from_raw(524288i32)));
    let n4304: ZB = zn_ge(n4303, zn_splat(P8::from_raw(393216i32)));
    let n4305: ZB = zn_lt(n4303, zn_splat(P8::from_raw(393216i32)));
    let n4306: ZB = zb_and(n4301, n4305);
    let n4307: ZB = zb_and(n4301, n4304);
    let n4308: ZN = zn_mul(n4220, zn_splat(P8::from_raw(524288i32)));
    let n4309: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4308);
    let n4310: ZB = zn_eq(n4216, n4309);
    let n4311: ZB = zb_or(n4306, n4307);
    let n4312: ZB = zb_or(n4304, n4310);
    let n4313: ZB = zb_or(n4302, n4311);
    let n4314: ZB = zb_and(n4299, n4312);
    let n4315: ZB = zb_not(n4314);
    let n4316: ZB = zb_and(n4313, n4314);
    let n4317: ZB = zb_and(n4313, n4315);
    let n4318: ZB = zn_ge(n4207, zn_splat(P8::from_raw(0i32)));
    let n4319: ZB = zb_or(n4316, n4317);
    let n4320: ZB = zb_and(n4314, n4318);
    let n4321: ZB = zb_not(n4320);
    let n4322: ZB = zb_and(n4319, n4320);
    let n4323: ZB = zb_and(n4319, n4321);
    let n4324: ZB = zb_or(n4297, n4322);
    let n4325: ZB = zb_or(n4280, n4324);
    let n4326: ZB = zb_or(n4263, n4325);
    let n4327: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n4227);
    let n4328: ZB = zn_le(n4327, n4231);
    let n4329: ZB = zn_gt(n4327, n4231);
    let n4330: ZB = zb_and(n4323, n4328);
    let n4331: ZB = zb_and(n4323, n4329);
    let n4332: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4327);
    let n4333: ZN = zn_mget(g.cart, n4237, n4332);
    let n4334: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4333);
    let n4335: ZB = zb_not(n4334);
    let n4336: ZB = zb_and(n4330, n4334);
    let n4337: ZB = zb_and(n4330, n4335);
    let n4338: ZB = zb_and(n4246, n4336);
    let n4339: ZB = zb_and(n4245, n4336);
    let n4340: ZN = zn_mul(n4327, zn_splat(P8::from_raw(524288i32)));
    let n4341: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4340);
    let n4342: ZB = zn_eq(n4228, n4341);
    let n4343: ZB = zb_or(n4338, n4339);
    let n4344: ZB = zb_or(n4245, n4342);
    let n4345: ZB = zb_or(n4337, n4343);
    let n4346: ZB = zb_and(n4334, n4344);
    let n4347: ZB = zb_not(n4346);
    let n4348: ZB = zb_and(n4345, n4346);
    let n4349: ZB = zb_and(n4345, n4347);
    let n4350: ZB = zb_or(n4348, n4349);
    let n4351: ZB = zb_and(n4259, n4346);
    let n4352: ZB = zb_not(n4351);
    let n4353: ZB = zb_and(n4350, n4351);
    let n4354: ZB = zb_and(n4350, n4352);
    let n4355: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4333);
    let n4356: ZB = zb_not(n4355);
    let n4357: ZB = zb_and(n4354, n4355);
    let n4358: ZB = zb_and(n4354, n4356);
    let n4359: ZB = zb_or(n4357, n4358);
    let n4360: ZB = zb_and(n4270, n4355);
    let n4361: ZB = zb_not(n4360);
    let n4362: ZB = zb_and(n4359, n4360);
    let n4363: ZB = zb_and(n4359, n4361);
    let n4364: ZB = zb_or(n4362, n4363);
    let n4365: ZB = zb_and(n4276, n4360);
    let n4366: ZB = zb_not(n4365);
    let n4367: ZB = zb_and(n4364, n4365);
    let n4368: ZB = zb_and(n4364, n4366);
    let n4369: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4333);
    let n4370: ZB = zb_not(n4369);
    let n4371: ZB = zb_and(n4368, n4369);
    let n4372: ZB = zb_and(n4368, n4370);
    let n4373: ZB = zb_or(n4371, n4372);
    let n4374: ZB = zb_and(n4287, n4369);
    let n4375: ZB = zb_not(n4374);
    let n4376: ZB = zb_and(n4373, n4374);
    let n4377: ZB = zb_and(n4373, n4375);
    let n4378: ZB = zb_or(n4376, n4377);
    let n4379: ZB = zb_and(n4293, n4374);
    let n4380: ZB = zb_not(n4379);
    let n4381: ZB = zb_and(n4378, n4379);
    let n4382: ZB = zb_and(n4378, n4380);
    let n4383: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4333);
    let n4384: ZB = zb_not(n4383);
    let n4385: ZB = zb_and(n4382, n4383);
    let n4386: ZB = zb_and(n4382, n4384);
    let n4387: ZB = zb_and(n4305, n4385);
    let n4388: ZB = zb_and(n4304, n4385);
    let n4389: ZB = zb_or(n4387, n4388);
    let n4390: ZB = zb_or(n4386, n4389);
    let n4391: ZB = zb_and(n4312, n4383);
    let n4392: ZB = zb_not(n4391);
    let n4393: ZB = zb_and(n4390, n4391);
    let n4394: ZB = zb_and(n4390, n4392);
    let n4395: ZB = zb_or(n4393, n4394);
    let n4396: ZB = zb_and(n4318, n4391);
    let n4397: ZB = zb_not(n4396);
    let n4398: ZB = zb_and(n4395, n4396);
    let n4399: ZB = zb_and(n4395, n4397);
    let n4400: ZB = zb_or(n4381, n4398);
    let n4401: ZB = zb_or(n4367, n4400);
    let n4402: ZB = zb_or(n4353, n4401);
    let n4403: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n4227);
    let n4404: ZB = zn_le(n4403, n4231);
    let n4405: ZB = zn_gt(n4403, n4231);
    let n4406: ZB = zb_and(n4399, n4404);
    let n4407: ZB = zb_and(n4399, n4405);
    let n4408: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4403);
    let n4409: ZN = zn_mget(g.cart, n4237, n4408);
    let n4410: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4409);
    let n4411: ZB = zb_not(n4410);
    let n4412: ZB = zb_and(n4406, n4410);
    let n4413: ZB = zb_and(n4406, n4411);
    let n4414: ZB = zb_and(n4246, n4412);
    let n4415: ZB = zb_and(n4245, n4412);
    let n4416: ZN = zn_mul(n4403, zn_splat(P8::from_raw(524288i32)));
    let n4417: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4416);
    let n4418: ZB = zn_eq(n4228, n4417);
    let n4419: ZB = zb_or(n4414, n4415);
    let n4420: ZB = zb_or(n4245, n4418);
    let n4421: ZB = zb_or(n4413, n4419);
    let n4422: ZB = zb_and(n4410, n4420);
    let n4423: ZB = zb_not(n4422);
    let n4424: ZB = zb_and(n4421, n4422);
    let n4425: ZB = zb_and(n4421, n4423);
    let n4426: ZB = zb_or(n4424, n4425);
    let n4427: ZB = zb_and(n4259, n4422);
    let n4428: ZB = zb_not(n4427);
    let n4429: ZB = zb_and(n4426, n4427);
    let n4430: ZB = zb_and(n4426, n4428);
    let n4431: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4409);
    let n4432: ZB = zb_not(n4431);
    let n4433: ZB = zb_and(n4430, n4431);
    let n4434: ZB = zb_and(n4430, n4432);
    let n4435: ZB = zb_or(n4433, n4434);
    let n4436: ZB = zb_and(n4270, n4431);
    let n4437: ZB = zb_not(n4436);
    let n4438: ZB = zb_and(n4435, n4436);
    let n4439: ZB = zb_and(n4435, n4437);
    let n4440: ZB = zb_or(n4438, n4439);
    let n4441: ZB = zb_and(n4276, n4436);
    let n4442: ZB = zb_not(n4441);
    let n4443: ZB = zb_and(n4440, n4441);
    let n4444: ZB = zb_and(n4440, n4442);
    let n4445: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4409);
    let n4446: ZB = zb_not(n4445);
    let n4447: ZB = zb_and(n4444, n4445);
    let n4448: ZB = zb_and(n4444, n4446);
    let n4449: ZB = zb_or(n4447, n4448);
    let n4450: ZB = zb_and(n4287, n4445);
    let n4451: ZB = zb_not(n4450);
    let n4452: ZB = zb_and(n4449, n4450);
    let n4453: ZB = zb_and(n4449, n4451);
    let n4454: ZB = zb_or(n4452, n4453);
    let n4455: ZB = zb_and(n4293, n4450);
    let n4456: ZB = zb_not(n4455);
    let n4457: ZB = zb_and(n4454, n4455);
    let n4458: ZB = zb_and(n4454, n4456);
    let n4459: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4409);
    let n4460: ZB = zb_not(n4459);
    let n4461: ZB = zb_and(n4458, n4459);
    let n4462: ZB = zb_and(n4458, n4460);
    let n4463: ZB = zb_and(n4305, n4461);
    let n4464: ZB = zb_and(n4304, n4461);
    let n4465: ZB = zb_or(n4463, n4464);
    let n4466: ZB = zb_or(n4462, n4465);
    let n4467: ZB = zb_and(n4312, n4459);
    let n4468: ZB = zb_not(n4467);
    let n4469: ZB = zb_and(n4466, n4467);
    let n4470: ZB = zb_and(n4466, n4468);
    let n4471: ZB = zb_or(n4469, n4470);
    let n4472: ZB = zb_and(n4318, n4467);
    let n4473: ZB = zb_not(n4472);
    let n4474: ZB = zb_and(n4471, n4472);
    let n4475: ZB = zb_and(n4471, n4473);
    let n4476: ZB = zb_or(n4457, n4474);
    let n4477: ZB = zb_or(n4443, n4476);
    let n4478: ZB = zb_or(n4429, n4477);
    let n4479: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4227);
    let n4480: ZB = zn_gt(n4479, n4231);
    let n4481: ZB = zb_and(n4210, n4480);
    let n4482: ZB = zb_or(n4407, n4475);
    let n4483: ZB = zsel_b(n4405, n4210, n4481);
    let n4484: ZB = zb_or(n4402, n4478);
    let n4485: ZB = zb_or(n4331, n4482);
    let n4486: ZB = zsel_b(n4329, n4210, n4483);
    let n4487: ZB = zb_or(n4326, n4484);
    let n4488: ZB = zb_or(n4236, n4485);
    let n4489: ZB = zsel_b(n4234, n4210, n4486);
    let n4490: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n4215);
    let n4491: ZB = zn_le(n4490, n4219);
    let n4492: ZB = zn_gt(n4490, n4219);
    let n4493: ZB = zb_and(n4488, n4491);
    let n4494: ZB = zb_and(n4488, n4492);
    let n4495: ZB = zb_and(n4233, n4493);
    let n4496: ZB = zb_and(n4234, n4493);
    let n4497: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n4490);
    let n4498: ZN = zn_mget(g.cart, n4497, n4238);
    let n4499: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4498);
    let n4500: ZB = zb_not(n4499);
    let n4501: ZB = zb_and(n4495, n4499);
    let n4502: ZB = zb_and(n4495, n4500);
    let n4503: ZB = zb_and(n4246, n4501);
    let n4504: ZB = zb_and(n4245, n4501);
    let n4505: ZB = zb_or(n4503, n4504);
    let n4506: ZB = zb_or(n4502, n4505);
    let n4507: ZB = zb_and(n4253, n4499);
    let n4508: ZB = zb_not(n4507);
    let n4509: ZB = zb_and(n4506, n4507);
    let n4510: ZB = zb_and(n4506, n4508);
    let n4511: ZB = zb_or(n4509, n4510);
    let n4512: ZB = zb_and(n4259, n4507);
    let n4513: ZB = zb_not(n4512);
    let n4514: ZB = zb_and(n4511, n4512);
    let n4515: ZB = zb_and(n4511, n4513);
    let n4516: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4498);
    let n4517: ZB = zb_not(n4516);
    let n4518: ZB = zb_and(n4515, n4516);
    let n4519: ZB = zb_and(n4515, n4517);
    let n4520: ZB = zb_or(n4518, n4519);
    let n4521: ZB = zb_and(n4270, n4516);
    let n4522: ZB = zb_not(n4521);
    let n4523: ZB = zb_and(n4520, n4521);
    let n4524: ZB = zb_and(n4520, n4522);
    let n4525: ZB = zb_or(n4523, n4524);
    let n4526: ZB = zb_and(n4276, n4521);
    let n4527: ZB = zb_not(n4526);
    let n4528: ZB = zb_and(n4525, n4526);
    let n4529: ZB = zb_and(n4525, n4527);
    let n4530: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4498);
    let n4531: ZB = zb_not(n4530);
    let n4532: ZB = zb_and(n4529, n4530);
    let n4533: ZB = zb_and(n4529, n4531);
    let n4534: ZB = zb_or(n4532, n4533);
    let n4535: ZB = zb_and(n4287, n4530);
    let n4536: ZB = zb_not(n4535);
    let n4537: ZB = zb_and(n4534, n4535);
    let n4538: ZB = zb_and(n4534, n4536);
    let n4539: ZB = zb_or(n4537, n4538);
    let n4540: ZB = zb_and(n4293, n4535);
    let n4541: ZB = zb_not(n4540);
    let n4542: ZB = zb_and(n4539, n4540);
    let n4543: ZB = zb_and(n4539, n4541);
    let n4544: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4498);
    let n4545: ZB = zb_not(n4544);
    let n4546: ZB = zb_and(n4543, n4544);
    let n4547: ZB = zb_and(n4543, n4545);
    let n4548: ZB = zb_and(n4305, n4546);
    let n4549: ZB = zb_and(n4304, n4546);
    let n4550: ZN = zn_mul(n4490, zn_splat(P8::from_raw(524288i32)));
    let n4551: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4550);
    let n4552: ZB = zn_eq(n4216, n4551);
    let n4553: ZB = zb_or(n4548, n4549);
    let n4554: ZB = zb_or(n4304, n4552);
    let n4555: ZB = zb_or(n4547, n4553);
    let n4556: ZB = zb_and(n4544, n4554);
    let n4557: ZB = zb_not(n4556);
    let n4558: ZB = zb_and(n4555, n4556);
    let n4559: ZB = zb_and(n4555, n4557);
    let n4560: ZB = zb_or(n4558, n4559);
    let n4561: ZB = zb_and(n4318, n4556);
    let n4562: ZB = zb_not(n4561);
    let n4563: ZB = zb_and(n4560, n4561);
    let n4564: ZB = zb_and(n4560, n4562);
    let n4565: ZB = zb_or(n4542, n4563);
    let n4566: ZB = zb_or(n4528, n4565);
    let n4567: ZB = zb_or(n4514, n4566);
    let n4568: ZB = zb_and(n4328, n4564);
    let n4569: ZB = zb_and(n4329, n4564);
    let n4570: ZN = zn_mget(g.cart, n4497, n4332);
    let n4571: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4570);
    let n4572: ZB = zb_not(n4571);
    let n4573: ZB = zb_and(n4568, n4571);
    let n4574: ZB = zb_and(n4568, n4572);
    let n4575: ZB = zb_and(n4246, n4573);
    let n4576: ZB = zb_and(n4245, n4573);
    let n4577: ZB = zb_or(n4575, n4576);
    let n4578: ZB = zb_or(n4574, n4577);
    let n4579: ZB = zb_and(n4344, n4571);
    let n4580: ZB = zb_not(n4579);
    let n4581: ZB = zb_and(n4578, n4579);
    let n4582: ZB = zb_and(n4578, n4580);
    let n4583: ZB = zb_or(n4581, n4582);
    let n4584: ZB = zb_and(n4259, n4579);
    let n4585: ZB = zb_not(n4584);
    let n4586: ZB = zb_and(n4583, n4584);
    let n4587: ZB = zb_and(n4583, n4585);
    let n4588: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4570);
    let n4589: ZB = zb_not(n4588);
    let n4590: ZB = zb_and(n4587, n4588);
    let n4591: ZB = zb_and(n4587, n4589);
    let n4592: ZB = zb_or(n4590, n4591);
    let n4593: ZB = zb_and(n4270, n4588);
    let n4594: ZB = zb_not(n4593);
    let n4595: ZB = zb_and(n4592, n4593);
    let n4596: ZB = zb_and(n4592, n4594);
    let n4597: ZB = zb_or(n4595, n4596);
    let n4598: ZB = zb_and(n4276, n4593);
    let n4599: ZB = zb_not(n4598);
    let n4600: ZB = zb_and(n4597, n4598);
    let n4601: ZB = zb_and(n4597, n4599);
    let n4602: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4570);
    let n4603: ZB = zb_not(n4602);
    let n4604: ZB = zb_and(n4601, n4602);
    let n4605: ZB = zb_and(n4601, n4603);
    let n4606: ZB = zb_or(n4604, n4605);
    let n4607: ZB = zb_and(n4287, n4602);
    let n4608: ZB = zb_not(n4607);
    let n4609: ZB = zb_and(n4606, n4607);
    let n4610: ZB = zb_and(n4606, n4608);
    let n4611: ZB = zb_or(n4609, n4610);
    let n4612: ZB = zb_and(n4293, n4607);
    let n4613: ZB = zb_not(n4612);
    let n4614: ZB = zb_and(n4611, n4612);
    let n4615: ZB = zb_and(n4611, n4613);
    let n4616: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4570);
    let n4617: ZB = zb_not(n4616);
    let n4618: ZB = zb_and(n4615, n4616);
    let n4619: ZB = zb_and(n4615, n4617);
    let n4620: ZB = zb_and(n4305, n4618);
    let n4621: ZB = zb_and(n4304, n4618);
    let n4622: ZB = zb_or(n4620, n4621);
    let n4623: ZB = zb_or(n4619, n4622);
    let n4624: ZB = zb_and(n4554, n4616);
    let n4625: ZB = zb_not(n4624);
    let n4626: ZB = zb_and(n4623, n4624);
    let n4627: ZB = zb_and(n4623, n4625);
    let n4628: ZB = zb_or(n4626, n4627);
    let n4629: ZB = zb_and(n4318, n4624);
    let n4630: ZB = zb_not(n4629);
    let n4631: ZB = zb_and(n4628, n4629);
    let n4632: ZB = zb_and(n4628, n4630);
    let n4633: ZB = zb_or(n4614, n4631);
    let n4634: ZB = zb_or(n4600, n4633);
    let n4635: ZB = zb_or(n4586, n4634);
    let n4636: ZB = zb_and(n4404, n4632);
    let n4637: ZB = zb_and(n4405, n4632);
    let n4638: ZN = zn_mget(g.cart, n4497, n4408);
    let n4639: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4638);
    let n4640: ZB = zb_not(n4639);
    let n4641: ZB = zb_and(n4636, n4639);
    let n4642: ZB = zb_and(n4636, n4640);
    let n4643: ZB = zb_and(n4246, n4641);
    let n4644: ZB = zb_and(n4245, n4641);
    let n4645: ZB = zb_or(n4643, n4644);
    let n4646: ZB = zb_or(n4642, n4645);
    let n4647: ZB = zb_and(n4420, n4639);
    let n4648: ZB = zb_not(n4647);
    let n4649: ZB = zb_and(n4646, n4647);
    let n4650: ZB = zb_and(n4646, n4648);
    let n4651: ZB = zb_or(n4649, n4650);
    let n4652: ZB = zb_and(n4259, n4647);
    let n4653: ZB = zb_not(n4652);
    let n4654: ZB = zb_and(n4651, n4652);
    let n4655: ZB = zb_and(n4651, n4653);
    let n4656: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4638);
    let n4657: ZB = zb_not(n4656);
    let n4658: ZB = zb_and(n4655, n4656);
    let n4659: ZB = zb_and(n4655, n4657);
    let n4660: ZB = zb_or(n4658, n4659);
    let n4661: ZB = zb_and(n4270, n4656);
    let n4662: ZB = zb_not(n4661);
    let n4663: ZB = zb_and(n4660, n4661);
    let n4664: ZB = zb_and(n4660, n4662);
    let n4665: ZB = zb_or(n4663, n4664);
    let n4666: ZB = zb_and(n4276, n4661);
    let n4667: ZB = zb_not(n4666);
    let n4668: ZB = zb_and(n4665, n4666);
    let n4669: ZB = zb_and(n4665, n4667);
    let n4670: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4638);
    let n4671: ZB = zb_not(n4670);
    let n4672: ZB = zb_and(n4669, n4670);
    let n4673: ZB = zb_and(n4669, n4671);
    let n4674: ZB = zb_or(n4672, n4673);
    let n4675: ZB = zb_and(n4287, n4670);
    let n4676: ZB = zb_not(n4675);
    let n4677: ZB = zb_and(n4674, n4675);
    let n4678: ZB = zb_and(n4674, n4676);
    let n4679: ZB = zb_or(n4677, n4678);
    let n4680: ZB = zb_and(n4293, n4675);
    let n4681: ZB = zb_not(n4680);
    let n4682: ZB = zb_and(n4679, n4680);
    let n4683: ZB = zb_and(n4679, n4681);
    let n4684: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4638);
    let n4685: ZB = zb_not(n4684);
    let n4686: ZB = zb_and(n4683, n4684);
    let n4687: ZB = zb_and(n4683, n4685);
    let n4688: ZB = zb_and(n4305, n4686);
    let n4689: ZB = zb_and(n4304, n4686);
    let n4690: ZB = zb_or(n4688, n4689);
    let n4691: ZB = zb_or(n4687, n4690);
    let n4692: ZB = zb_and(n4554, n4684);
    let n4693: ZB = zb_not(n4692);
    let n4694: ZB = zb_and(n4691, n4692);
    let n4695: ZB = zb_and(n4691, n4693);
    let n4696: ZB = zb_or(n4694, n4695);
    let n4697: ZB = zb_and(n4318, n4692);
    let n4698: ZB = zb_not(n4697);
    let n4699: ZB = zb_and(n4696, n4697);
    let n4700: ZB = zb_and(n4696, n4698);
    let n4701: ZB = zb_or(n4682, n4699);
    let n4702: ZB = zb_or(n4668, n4701);
    let n4703: ZB = zb_or(n4654, n4702);
    let n4704: ZB = zb_and(n4480, n4489);
    let n4705: ZB = zb_or(n4637, n4700);
    let n4706: ZB = zsel_b(n4405, n4489, n4704);
    let n4707: ZB = zb_or(n4635, n4703);
    let n4708: ZB = zb_or(n4569, n4705);
    let n4709: ZB = zsel_b(n4329, n4489, n4706);
    let n4710: ZB = zb_or(n4567, n4707);
    let n4711: ZB = zb_or(n4496, n4708);
    let n4712: ZB = zsel_b(n4234, n4489, n4709);
    let n4713: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n4215);
    let n4714: ZB = zn_le(n4713, n4219);
    let n4715: ZB = zn_gt(n4713, n4219);
    let n4716: ZB = zb_and(n4711, n4714);
    let n4717: ZB = zb_and(n4711, n4715);
    let n4718: ZB = zb_and(n4233, n4716);
    let n4719: ZB = zb_and(n4234, n4716);
    let n4720: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n4713);
    let n4721: ZN = zn_mget(g.cart, n4720, n4238);
    let n4722: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4721);
    let n4723: ZB = zb_not(n4722);
    let n4724: ZB = zb_and(n4718, n4722);
    let n4725: ZB = zb_and(n4718, n4723);
    let n4726: ZB = zb_and(n4246, n4724);
    let n4727: ZB = zb_and(n4245, n4724);
    let n4728: ZB = zb_or(n4726, n4727);
    let n4729: ZB = zb_or(n4725, n4728);
    let n4730: ZB = zb_and(n4253, n4722);
    let n4731: ZB = zb_not(n4730);
    let n4732: ZB = zb_and(n4729, n4730);
    let n4733: ZB = zb_and(n4729, n4731);
    let n4734: ZB = zb_or(n4732, n4733);
    let n4735: ZB = zb_and(n4259, n4730);
    let n4736: ZB = zb_not(n4735);
    let n4737: ZB = zb_and(n4734, n4735);
    let n4738: ZB = zb_and(n4734, n4736);
    let n4739: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4721);
    let n4740: ZB = zb_not(n4739);
    let n4741: ZB = zb_and(n4738, n4739);
    let n4742: ZB = zb_and(n4738, n4740);
    let n4743: ZB = zb_or(n4741, n4742);
    let n4744: ZB = zb_and(n4270, n4739);
    let n4745: ZB = zb_not(n4744);
    let n4746: ZB = zb_and(n4743, n4744);
    let n4747: ZB = zb_and(n4743, n4745);
    let n4748: ZB = zb_or(n4746, n4747);
    let n4749: ZB = zb_and(n4276, n4744);
    let n4750: ZB = zb_not(n4749);
    let n4751: ZB = zb_and(n4748, n4749);
    let n4752: ZB = zb_and(n4748, n4750);
    let n4753: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4721);
    let n4754: ZB = zb_not(n4753);
    let n4755: ZB = zb_and(n4752, n4753);
    let n4756: ZB = zb_and(n4752, n4754);
    let n4757: ZB = zb_or(n4755, n4756);
    let n4758: ZB = zb_and(n4287, n4753);
    let n4759: ZB = zb_not(n4758);
    let n4760: ZB = zb_and(n4757, n4758);
    let n4761: ZB = zb_and(n4757, n4759);
    let n4762: ZB = zb_or(n4760, n4761);
    let n4763: ZB = zb_and(n4293, n4758);
    let n4764: ZB = zb_not(n4763);
    let n4765: ZB = zb_and(n4762, n4763);
    let n4766: ZB = zb_and(n4762, n4764);
    let n4767: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4721);
    let n4768: ZB = zb_not(n4767);
    let n4769: ZB = zb_and(n4766, n4767);
    let n4770: ZB = zb_and(n4766, n4768);
    let n4771: ZB = zb_and(n4305, n4769);
    let n4772: ZB = zb_and(n4304, n4769);
    let n4773: ZN = zn_mul(n4713, zn_splat(P8::from_raw(524288i32)));
    let n4774: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n4773);
    let n4775: ZB = zn_eq(n4216, n4774);
    let n4776: ZB = zb_or(n4771, n4772);
    let n4777: ZB = zb_or(n4304, n4775);
    let n4778: ZB = zb_or(n4770, n4776);
    let n4779: ZB = zb_and(n4767, n4777);
    let n4780: ZB = zb_not(n4779);
    let n4781: ZB = zb_and(n4778, n4779);
    let n4782: ZB = zb_and(n4778, n4780);
    let n4783: ZB = zb_or(n4781, n4782);
    let n4784: ZB = zb_and(n4318, n4779);
    let n4785: ZB = zb_not(n4784);
    let n4786: ZB = zb_and(n4783, n4784);
    let n4787: ZB = zb_and(n4783, n4785);
    let n4788: ZB = zb_or(n4765, n4786);
    let n4789: ZB = zb_or(n4751, n4788);
    let n4790: ZB = zb_or(n4737, n4789);
    let n4791: ZB = zb_and(n4328, n4787);
    let n4792: ZB = zb_and(n4329, n4787);
    let n4793: ZN = zn_mget(g.cart, n4720, n4332);
    let n4794: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4793);
    let n4795: ZB = zb_not(n4794);
    let n4796: ZB = zb_and(n4791, n4794);
    let n4797: ZB = zb_and(n4791, n4795);
    let n4798: ZB = zb_and(n4246, n4796);
    let n4799: ZB = zb_and(n4245, n4796);
    let n4800: ZB = zb_or(n4798, n4799);
    let n4801: ZB = zb_or(n4797, n4800);
    let n4802: ZB = zb_and(n4344, n4794);
    let n4803: ZB = zb_not(n4802);
    let n4804: ZB = zb_and(n4801, n4802);
    let n4805: ZB = zb_and(n4801, n4803);
    let n4806: ZB = zb_or(n4804, n4805);
    let n4807: ZB = zb_and(n4259, n4802);
    let n4808: ZB = zb_not(n4807);
    let n4809: ZB = zb_and(n4806, n4807);
    let n4810: ZB = zb_and(n4806, n4808);
    let n4811: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4793);
    let n4812: ZB = zb_not(n4811);
    let n4813: ZB = zb_and(n4810, n4811);
    let n4814: ZB = zb_and(n4810, n4812);
    let n4815: ZB = zb_or(n4813, n4814);
    let n4816: ZB = zb_and(n4270, n4811);
    let n4817: ZB = zb_not(n4816);
    let n4818: ZB = zb_and(n4815, n4816);
    let n4819: ZB = zb_and(n4815, n4817);
    let n4820: ZB = zb_or(n4818, n4819);
    let n4821: ZB = zb_and(n4276, n4816);
    let n4822: ZB = zb_not(n4821);
    let n4823: ZB = zb_and(n4820, n4821);
    let n4824: ZB = zb_and(n4820, n4822);
    let n4825: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4793);
    let n4826: ZB = zb_not(n4825);
    let n4827: ZB = zb_and(n4824, n4825);
    let n4828: ZB = zb_and(n4824, n4826);
    let n4829: ZB = zb_or(n4827, n4828);
    let n4830: ZB = zb_and(n4287, n4825);
    let n4831: ZB = zb_not(n4830);
    let n4832: ZB = zb_and(n4829, n4830);
    let n4833: ZB = zb_and(n4829, n4831);
    let n4834: ZB = zb_or(n4832, n4833);
    let n4835: ZB = zb_and(n4293, n4830);
    let n4836: ZB = zb_not(n4835);
    let n4837: ZB = zb_and(n4834, n4835);
    let n4838: ZB = zb_and(n4834, n4836);
    let n4839: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4793);
    let n4840: ZB = zb_not(n4839);
    let n4841: ZB = zb_and(n4838, n4839);
    let n4842: ZB = zb_and(n4838, n4840);
    let n4843: ZB = zb_and(n4305, n4841);
    let n4844: ZB = zb_and(n4304, n4841);
    let n4845: ZB = zb_or(n4843, n4844);
    let n4846: ZB = zb_or(n4842, n4845);
    let n4847: ZB = zb_and(n4777, n4839);
    let n4848: ZB = zb_not(n4847);
    let n4849: ZB = zb_and(n4846, n4847);
    let n4850: ZB = zb_and(n4846, n4848);
    let n4851: ZB = zb_or(n4849, n4850);
    let n4852: ZB = zb_and(n4318, n4847);
    let n4853: ZB = zb_not(n4852);
    let n4854: ZB = zb_and(n4851, n4852);
    let n4855: ZB = zb_and(n4851, n4853);
    let n4856: ZB = zb_or(n4837, n4854);
    let n4857: ZB = zb_or(n4823, n4856);
    let n4858: ZB = zb_or(n4809, n4857);
    let n4859: ZB = zb_and(n4404, n4855);
    let n4860: ZB = zb_and(n4405, n4855);
    let n4861: ZN = zn_mget(g.cart, n4720, n4408);
    let n4862: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n4861);
    let n4863: ZB = zb_not(n4862);
    let n4864: ZB = zb_and(n4859, n4862);
    let n4865: ZB = zb_and(n4859, n4863);
    let n4866: ZB = zb_and(n4246, n4864);
    let n4867: ZB = zb_and(n4245, n4864);
    let n4868: ZB = zb_or(n4866, n4867);
    let n4869: ZB = zb_or(n4865, n4868);
    let n4870: ZB = zb_and(n4420, n4862);
    let n4871: ZB = zb_not(n4870);
    let n4872: ZB = zb_and(n4869, n4870);
    let n4873: ZB = zb_and(n4869, n4871);
    let n4874: ZB = zb_or(n4872, n4873);
    let n4875: ZB = zb_and(n4259, n4870);
    let n4876: ZB = zb_not(n4875);
    let n4877: ZB = zb_and(n4874, n4875);
    let n4878: ZB = zb_and(n4874, n4876);
    let n4879: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n4861);
    let n4880: ZB = zb_not(n4879);
    let n4881: ZB = zb_and(n4878, n4879);
    let n4882: ZB = zb_and(n4878, n4880);
    let n4883: ZB = zb_or(n4881, n4882);
    let n4884: ZB = zb_and(n4270, n4879);
    let n4885: ZB = zb_not(n4884);
    let n4886: ZB = zb_and(n4883, n4884);
    let n4887: ZB = zb_and(n4883, n4885);
    let n4888: ZB = zb_or(n4886, n4887);
    let n4889: ZB = zb_and(n4276, n4884);
    let n4890: ZB = zb_not(n4889);
    let n4891: ZB = zb_and(n4888, n4889);
    let n4892: ZB = zb_and(n4888, n4890);
    let n4893: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n4861);
    let n4894: ZB = zb_not(n4893);
    let n4895: ZB = zb_and(n4892, n4893);
    let n4896: ZB = zb_and(n4892, n4894);
    let n4897: ZB = zb_or(n4895, n4896);
    let n4898: ZB = zb_and(n4287, n4893);
    let n4899: ZB = zb_not(n4898);
    let n4900: ZB = zb_and(n4897, n4898);
    let n4901: ZB = zb_and(n4897, n4899);
    let n4902: ZB = zb_or(n4900, n4901);
    let n4903: ZB = zb_and(n4293, n4898);
    let n4904: ZB = zb_not(n4903);
    let n4905: ZB = zb_and(n4902, n4903);
    let n4906: ZB = zb_and(n4902, n4904);
    let n4907: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n4861);
    let n4908: ZB = zb_not(n4907);
    let n4909: ZB = zb_and(n4906, n4907);
    let n4910: ZB = zb_and(n4906, n4908);
    let n4911: ZB = zb_and(n4305, n4909);
    let n4912: ZB = zb_and(n4304, n4909);
    let n4913: ZB = zb_or(n4911, n4912);
    let n4914: ZB = zb_or(n4910, n4913);
    let n4915: ZB = zb_and(n4777, n4907);
    let n4916: ZB = zb_not(n4915);
    let n4917: ZB = zb_and(n4914, n4915);
    let n4918: ZB = zb_and(n4914, n4916);
    let n4919: ZB = zb_or(n4917, n4918);
    let n4920: ZB = zb_and(n4318, n4915);
    let n4921: ZB = zb_not(n4920);
    let n4922: ZB = zb_and(n4919, n4920);
    let n4923: ZB = zb_and(n4919, n4921);
    let n4924: ZB = zb_or(n4905, n4922);
    let n4925: ZB = zb_or(n4891, n4924);
    let n4926: ZB = zb_or(n4877, n4925);
    let n4927: ZB = zb_and(n4480, n4712);
    let n4928: ZB = zb_or(n4860, n4923);
    let n4929: ZB = zsel_b(n4405, n4712, n4927);
    let n4930: ZB = zb_or(n4858, n4926);
    let n4931: ZB = zb_or(n4792, n4928);
    let n4932: ZB = zsel_b(n4329, n4712, n4929);
    let n4933: ZB = zb_or(n4790, n4930);
    let n4934: ZB = zb_or(n4719, n4931);
    let n4935: ZB = zsel_b(n4234, n4712, n4932);
    let n4936: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4215);
    let n4937: ZB = zn_gt(n4936, n4219);
    let n4938: ZB = zb_and(n4935, n4937);
    let n4939: ZB = zb_or(n4710, n4933);
    let n4940: ZB = zsel_b(n4710, n4489, n4712);
    let n4941: ZB = zb_or(n4717, n4934);
    let n4942: ZB = zsel_b(n4715, n4712, n4938);
    let n4943: ZB = zb_or(n4487, n4939);
    let n4944: ZB = zsel_b(n4487, n4210, n4940);
    let n4945: ZB = zb_or(n4494, n4941);
    let n4946: ZB = zsel_b(n4492, n4489, n4942);
    let n4947: ZB = zb_or(n4224, n4945);
    let n4948: ZB = zsel_b(n4222, n4210, n4946);
    let n4949: ZB = zn_gt(n4206, zn_splat(P8::from_raw(8388608i32)));
    let n4950: ZB = zn_le(n4206, zn_splat(P8::from_raw(8388608i32)));
    let n4951: ZB = zb_and(n4943, n4949);
    let n4952: ZB = zb_and(n4943, n4950);
    let n4953: ZN = zsel_n(n4949, n3028, n3027);
    let n4954: ZB = zb_or(n4951, n4952);
    let n4955: ZB = zb_and(n4947, n4949);
    let n4956: ZN = zsel_n(n4954, n4953, n3027);
    let n4957: ZB = zb_or(n4954, n4955);
    let n4958: ZB = zsel_b(n4954, n4944, n4948);
    let n4959: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4211);
    let n4960: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n4212);
    let n4961: ZB = zn_tile_flag_at(g.cache, g.cart, n4959, n4960, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n4962: ZB = zb_not(n4961);
    let n4963: ZB = zb_and(n4957, n4962);
    let n4964: ZB = zb_and(n4957, n4961);
    let n4965: ZB = zb_or(n4963, n4964);
    let n4966: ZB = zb_and(n4962, n4965);
    let n4967: ZB = zb_and(n4961, n4965);
    let n4968: ZB = zb_or(n4966, n4967);
    let n4969: ZB = zn_lt(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n4970: ZB = zn_ge(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n4971: ZN = zsel_n(n4969, zn_splat(P8::from_raw(65536i32)), r_c283);
    let n4972: ZN = zsel_n(n4961, n4971, r_c283);
    let n4973: ZN = zsel_n(n4961, zn_splat(P8::from_raw(393216i32)), n1419);
    let n4974: ZB = zb_and(n4961, n4968);
    let n4975: ZB = zb_and(n4962, n4968);
    let n4976: ZB = zb_and(n4969, n4974);
    let n4977: ZB = zb_and(n4970, n4974);
    let n4978: ZB = zb_or(n4976, n4977);
    let n4979: ZB = zb_and(n1414, n4975);
    let n4980: ZB = zb_and(n1415, n4975);
    let n4981: ZB = zb_or(n4979, n4980);
    let n4982: ZB = zb_or(n4978, n4981);
    let n4983: ZB = zn_gt(n4207, r_c358);
    let n4984: ZB = zn_le(n4207, r_c358);
    let n4985: ZB = zn_gt(n4208, r_c359);
    let n4986: ZB = zn_le(n4208, r_c359);
    let n4987: ZN = zsel_n(n4962, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n4988: ZN = zn_abs(n4207);
    let n4989: ZB = zn_gt(n4988, zn_splat(P8::from_raw(65536i32)));
    let n4990: ZB = zn_le(n4988, zn_splat(P8::from_raw(65536i32)));
    let n4991: ZB = zn_gt(n4207, zn_splat(P8::from_raw(0i32)));
    let n4992: ZB = zn_lt(n4207, zn_splat(P8::from_raw(0i32)));
    let n4993: ZB = zn_gt(n4207, zn_splat(P8::from_raw(65536i32)));
    let n4994: ZB = zn_le(n4207, zn_splat(P8::from_raw(65536i32)));
    let n4995: ZN = zn_sub(n4207, zn_splat(P8::from_raw(9830i32)));
    let n4996: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n4995);
    let n4997: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n4207);
    let n4998: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n4997);
    let n4999: ZB = zn_gt(n4207, zn_splat(P8::from_raw(-65536i32)));
    let n5000: ZB = zn_le(n4207, zn_splat(P8::from_raw(-65536i32)));
    let n5001: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n4995);
    let n5002: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n4997);
    let n5003: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n4995);
    let n5004: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n4997);
    let n5005: ZN = zsel_n(n4999, n5001, n5002);
    let n5006: ZN = zsel_n(n4991, n5003, n5004);
    let n5007: ZN = zsel_n(n4993, n4996, n4998);
    let n5008: ZN = zsel_n(n4992, n5005, n5006);
    let n5009: ZN = zsel_n(n4991, n5007, n5008);
    let n5010: ZN = zn_sub(n4207, n4987);
    let n5011: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n5010);
    let n5012: ZN = zn_add(n4207, n4987);
    let n5013: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n5012);
    let n5014: ZN = zsel_n(n4991, n5011, n5013);
    let n5015: ZN = zsel_n(n4989, n5009, n5014);
    let n5016: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5015);
    let n5017: ZB = zb_not(n5016);
    let n5018: ZB = zn_lt(n5015, zn_splat(P8::from_raw(0i32)));
    let n5019: ZB = zsel_b(n5017, n5018, r_c360);
    let n5020: ZN = zn_abs(n4208);
    let n5021: ZB = zn_le(n5020, zn_splat(P8::from_raw(9830i32)));
    let n5022: ZB = zn_gt(n5020, zn_splat(P8::from_raw(9830i32)));
    let n5023: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n4212);
    let n5024: ZB = zn_gt(n4208, zn_splat(P8::from_raw(131072i32)));
    let n5025: ZB = zn_le(n4208, zn_splat(P8::from_raw(131072i32)));
    let n5026: ZB = zn_gt(n4973, zn_splat(P8::from_raw(0i32)));
    let n5027: ZB = zn_le(n4973, zn_splat(P8::from_raw(0i32)));
    let n5028: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n4211);
    let n5029: ZB = zn_tile_flag_at(g.cache, g.cart, n5028, n5023, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n5030: ZB = zb_not(n5029);
    let n5031: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n4211);
    let n5032: ZB = zn_tile_flag_at(g.cache, g.cart, n5031, n5023, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n5033: ZB = zb_not(n5032);
    let n5034: ZN = zsel_n(n5032, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n5035: ZN = zsel_n(n5029, zn_splat(P8::from_raw(-65536i32)), n5034);
    let n5036: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5035);
    let n5037: ZB = zb_not(n5036);
    let n5038: ZB = zn_gt(n4972, zn_splat(P8::from_raw(0i32)));
    let n5039: ZB = zn_le(n4972, zn_splat(P8::from_raw(0i32)));
    let n5040: ZB = zb_not(n5019);
    let n5041: ZN = zsel_n(n5019, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n5042: ZB = zn_gt(n5041, zn_splat(P8::from_raw(0i32)));
    let n5043: ZB = zn_le(n5041, zn_splat(P8::from_raw(0i32)));
    let n5044: ZB = zn_lt(n5041, zn_splat(P8::from_raw(0i32)));
    let n5045: ZB = zn_ge(n5041, zn_splat(P8::from_raw(0i32)));
    let n5046: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5041);
    let n5047: ZB = zb_not(n5046);
    let n5048: ZB = zb_and(n1424, n4982);
    let n5049: ZB = zb_and(n1425, n4982);
    let n5050: ZB = zb_and(n4983, n5048);
    let n5051: ZB = zb_and(n4984, n5048);
    let n5052: ZB = zb_or(n5050, n5051);
    let n5053: ZB = zb_and(n4985, n5052);
    let n5054: ZB = zb_and(n4986, n5052);
    let n5055: ZB = zb_or(n5053, n5054);
    let n5056: ZB = zb_and(n4962, n5049);
    let n5057: ZB = zb_and(n4961, n5049);
    let n5058: ZB = zb_or(n5056, n5057);
    let n5059: ZB = zb_and(n4989, n5058);
    let n5060: ZB = zb_and(n4990, n5058);
    let n5061: ZB = zb_and(n4991, n5059);
    let n5062: ZB = zb_and(n4293, n5059);
    let n5063: ZB = zb_and(n4992, n5062);
    let n5064: ZB = zb_and(n4318, n5062);
    let n5065: ZB = zb_and(n4993, n5061);
    let n5066: ZB = zb_and(n4994, n5061);
    let n5067: ZB = zb_and(n4999, n5063);
    let n5068: ZB = zb_and(n5000, n5063);
    let n5069: ZB = zb_and(n4293, n5064);
    let n5070: ZB = zb_or(n5067, n5068);
    let n5071: ZB = zb_or(n5065, n5066);
    let n5072: ZB = zb_or(n5069, n5070);
    let n5073: ZB = zb_or(n5071, n5072);
    let n5074: ZB = zb_and(n4991, n5060);
    let n5075: ZB = zb_and(n4293, n5060);
    let n5076: ZB = zb_or(n5074, n5075);
    let n5077: ZB = zb_or(n5073, n5076);
    let n5078: ZB = zb_and(n5017, n5077);
    let n5079: ZB = zb_and(n5016, n5077);
    let n5080: ZB = zb_or(n5078, n5079);
    let n5081: ZB = zb_and(n5021, n5080);
    let n5082: ZB = zb_and(n5022, n5080);
    let n5083: ZB = zb_or(n5081, n5082);
    let n5084: ZB = zb_and(n4962, n5083);
    let n5085: ZB = zb_and(n4961, n5083);
    let n5086: ZB = zb_and(n5024, n5084);
    let n5087: ZB = zb_and(n5025, n5084);
    let n5088: ZB = zb_or(n5086, n5087);
    let n5089: ZB = zb_or(n5085, n5088);
    let n5090: ZB = zb_and(n5038, n5089);
    let n5091: ZB = zb_and(n5039, n5089);
    let n5092: ZB = zb_or(n5090, n5091);
    let n5093: ZB = zb_or(n5055, n5092);
    let n5094: ZB = zn_lt(n4206, zn_splat(P8::from_raw(-262144i32)));
    let n5095: ZB = zn_ge(n4206, zn_splat(P8::from_raw(-262144i32)));
    let n5096: ZB = zb_and(n5093, n5094);
    let n5097: ZB = zb_and(n5093, n5095);
    let n5098: ZB = zb_or(n5096, n5097);
    let n5102: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n5010);
    let n5103: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n5012);
    let n5104: ZN = zsel_n(n4999, n5102, n5103);
    let n5105: ZN = zsel_n(n4989, n5009, n5104);
    let n5106: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5105);
    let n5107: ZB = zb_not(n5106);
    let n5108: ZB = zn_lt(n5105, zn_splat(P8::from_raw(0i32)));
    let n5109: ZB = zsel_b(n5107, n5108, r_c360);
    let n5110: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n4211);
    let n5111: ZB = zn_tile_flag_at(g.cache, g.cart, n5110, n5023, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n5112: ZB = zb_not(n5111);
    let n5113: ZN = zsel_n(n5111, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n5114: ZB = zn_gt(n4208, n5113);
    let n5115: ZB = zn_le(n4208, n5113);
    let n5116: ZB = zb_and(n4999, n5060);
    let n5117: ZB = zb_and(n5000, n5060);
    let n5118: ZB = zb_or(n5116, n5117);
    let n5119: ZB = zb_or(n5073, n5118);
    let n5120: ZB = zb_and(n5107, n5119);
    let n5121: ZB = zb_and(n5106, n5119);
    let n5122: ZB = zb_or(n5120, n5121);
    let n5123: ZB = zb_and(n5021, n5122);
    let n5124: ZB = zb_and(n5022, n5122);
    let n5125: ZB = zb_or(n5123, n5124);
    let n5126: ZB = zb_and(n5112, n5125);
    let n5127: ZB = zb_and(n5111, n5125);
    let n5128: ZB = zb_or(n5126, n5127);
    let n5129: ZB = zb_and(n5112, n5128);
    let n5130: ZB = zb_and(n5111, n5128);
    let n5131: ZB = zb_or(n5129, n5130);
    let n5132: ZB = zb_and(n5111, n5131);
    let n5133: ZB = zb_and(n5112, n5131);
    let n5134: ZB = zb_or(n5132, n5133);
    let n5135: ZB = zb_and(n5111, n5134);
    let n5136: ZB = zb_and(n5112, n5134);
    let n5137: ZB = zb_or(n5135, n5136);
    let n5138: ZB = zb_and(n4962, n5137);
    let n5139: ZB = zb_and(n4961, n5137);
    let n5140: ZB = zb_and(n5114, n5138);
    let n5141: ZB = zb_and(n5115, n5138);
    let n5142: ZB = zb_or(n5140, n5141);
    let n5143: ZB = zb_or(n5139, n5142);
    let n5144: ZB = zb_and(n5038, n5143);
    let n5145: ZB = zb_and(n5039, n5143);
    let n5146: ZB = zb_or(n5144, n5145);
    let n5147: ZB = zb_or(n5055, n5146);
    let n5148: ZB = zb_and(n5094, n5147);
    let n5149: ZB = zb_and(n5095, n5147);
    let n5150: ZB = zb_or(n5148, n5149);
    let n5153: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n5010);
    let n5154: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n5012);
    let n5155: ZN = zsel_n(n4993, n5153, n5154);
    let n5156: ZN = zsel_n(n4989, n5009, n5155);
    let n5157: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n5156);
    let n5158: ZB = zb_not(n5157);
    let n5159: ZB = zn_lt(n5156, zn_splat(P8::from_raw(0i32)));
    let n5160: ZB = zsel_b(n5158, n5159, r_c360);
    let n5161: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n4211);
    let n5162: ZB = zn_tile_flag_at(g.cache, g.cart, n5161, n5023, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n5163: ZB = zb_not(n5162);
    let n5164: ZN = zsel_n(n5162, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n5165: ZB = zn_gt(n4208, n5164);
    let n5166: ZB = zn_le(n4208, n5164);
    let n5167: ZB = zb_and(n4993, n5060);
    let n5168: ZB = zb_and(n4994, n5060);
    let n5169: ZB = zb_or(n5167, n5168);
    let n5170: ZB = zb_or(n5073, n5169);
    let n5171: ZB = zb_and(n5158, n5170);
    let n5172: ZB = zb_and(n5157, n5170);
    let n5173: ZB = zb_or(n5171, n5172);
    let n5174: ZB = zb_and(n5021, n5173);
    let n5175: ZB = zb_and(n5022, n5173);
    let n5176: ZB = zb_or(n5174, n5175);
    let n5177: ZB = zb_and(n5163, n5176);
    let n5178: ZB = zb_and(n5162, n5176);
    let n5179: ZB = zb_or(n5177, n5178);
    let n5180: ZB = zb_and(n5163, n5179);
    let n5181: ZB = zb_and(n5162, n5179);
    let n5182: ZB = zb_or(n5180, n5181);
    let n5183: ZB = zb_and(n5162, n5182);
    let n5184: ZB = zb_and(n5163, n5182);
    let n5185: ZB = zb_or(n5183, n5184);
    let n5186: ZB = zb_and(n5162, n5185);
    let n5187: ZB = zb_and(n5163, n5185);
    let n5188: ZB = zb_or(n5186, n5187);
    let n5189: ZB = zb_and(n4962, n5188);
    let n5190: ZB = zb_and(n4961, n5188);
    let n5191: ZB = zb_and(n5165, n5189);
    let n5192: ZB = zb_and(n5166, n5189);
    let n5193: ZB = zb_or(n5191, n5192);
    let n5194: ZB = zb_or(n5190, n5193);
    let n5195: ZB = zb_and(n5038, n5194);
    let n5196: ZB = zb_and(n5039, n5194);
    let n5197: ZB = zb_or(n5195, n5196);
    let n5198: ZB = zb_or(n5055, n5197);
    let n5199: ZB = zb_and(n5094, n5198);
    let n5200: ZB = zb_and(n5095, n5198);
    let n5201: ZB = zb_or(n5199, n5200);
    let n5204: ZB = zb_and(n136, n5089);
    let n5205: ZB = zb_and(r_c293, n5089);
    let n5206: ZB = zb_and(n5026, n5204);
    let n5207: ZB = zb_and(n5027, n5204);
    let n5208: ZB = zb_and(n5030, n5207);
    let n5209: ZB = zb_and(n5029, n5207);
    let n5210: ZB = zb_or(n5208, n5209);
    let n5211: ZB = zb_and(n5030, n5210);
    let n5212: ZB = zb_and(n5029, n5210);
    let n5213: ZB = zb_or(n5211, n5212);
    let n5214: ZB = zb_and(n5029, n5213);
    let n5215: ZB = zb_and(n5030, n5213);
    let n5216: ZB = zb_and(n5033, n5215);
    let n5217: ZB = zb_and(n5032, n5215);
    let n5218: ZB = zb_or(n5216, n5217);
    let n5219: ZB = zb_and(n5033, n5218);
    let n5220: ZB = zb_and(n5032, n5218);
    let n5221: ZB = zb_or(n5219, n5220);
    let n5222: ZB = zb_and(n5032, n5221);
    let n5223: ZB = zb_and(n5033, n5221);
    let n5224: ZB = zb_or(n5222, n5223);
    let n5225: ZB = zb_or(n5214, n5224);
    let n5226: ZB = zb_and(n5037, n5225);
    let n5227: ZB = zb_and(n5036, n5225);
    let n5228: ZB = zb_or(n5226, n5227);
    let n5229: ZB = zb_or(n5206, n5228);
    let n5230: ZB = zb_or(n5205, n5229);
    let n5231: ZB = zb_and(n5038, n5230);
    let n5232: ZB = zb_and(n5039, n5230);
    let n5233: ZB = zb_or(n5231, n5232);
    let n5234: ZB = zb_or(n5055, n5233);
    let n5235: ZB = zb_and(n5094, n5234);
    let n5236: ZB = zb_and(n5095, n5234);
    let n5237: ZB = zb_or(n5235, n5236);
    let n5240: ZB = zb_and(n136, n5143);
    let n5241: ZB = zb_and(r_c293, n5143);
    let n5242: ZB = zb_and(n5026, n5240);
    let n5243: ZB = zb_and(n5027, n5240);
    let n5244: ZB = zb_and(n5030, n5243);
    let n5245: ZB = zb_and(n5029, n5243);
    let n5246: ZB = zb_or(n5244, n5245);
    let n5247: ZB = zb_and(n5030, n5246);
    let n5248: ZB = zb_and(n5029, n5246);
    let n5249: ZB = zb_or(n5247, n5248);
    let n5250: ZB = zb_and(n5029, n5249);
    let n5251: ZB = zb_and(n5030, n5249);
    let n5252: ZB = zb_and(n5033, n5251);
    let n5253: ZB = zb_and(n5032, n5251);
    let n5254: ZB = zb_or(n5252, n5253);
    let n5255: ZB = zb_and(n5033, n5254);
    let n5256: ZB = zb_and(n5032, n5254);
    let n5257: ZB = zb_or(n5255, n5256);
    let n5258: ZB = zb_and(n5032, n5257);
    let n5259: ZB = zb_and(n5033, n5257);
    let n5260: ZB = zb_or(n5258, n5259);
    let n5261: ZB = zb_or(n5250, n5260);
    let n5262: ZB = zb_and(n5037, n5261);
    let n5263: ZB = zb_and(n5036, n5261);
    let n5264: ZB = zb_or(n5262, n5263);
    let n5265: ZB = zb_or(n5242, n5264);
    let n5266: ZB = zb_or(n5241, n5265);
    let n5267: ZB = zb_and(n5038, n5266);
    let n5268: ZB = zb_and(n5039, n5266);
    let n5269: ZB = zb_or(n5267, n5268);
    let n5270: ZB = zb_or(n5055, n5269);
    let n5271: ZB = zb_and(n5094, n5270);
    let n5272: ZB = zb_and(n5095, n5270);
    let n5273: ZB = zb_or(n5271, n5272);
    let n5276: ZB = zb_and(n136, n5194);
    let n5277: ZB = zb_and(r_c293, n5194);
    let n5278: ZB = zb_and(n5026, n5276);
    let n5279: ZB = zb_and(n5027, n5276);
    let n5280: ZB = zb_and(n5030, n5279);
    let n5281: ZB = zb_and(n5029, n5279);
    let n5282: ZB = zb_or(n5280, n5281);
    let n5283: ZB = zb_and(n5030, n5282);
    let n5284: ZB = zb_and(n5029, n5282);
    let n5285: ZB = zb_or(n5283, n5284);
    let n5286: ZB = zb_and(n5029, n5285);
    let n5287: ZB = zb_and(n5030, n5285);
    let n5288: ZB = zb_and(n5033, n5287);
    let n5289: ZB = zb_and(n5032, n5287);
    let n5290: ZB = zb_or(n5288, n5289);
    let n5291: ZB = zb_and(n5033, n5290);
    let n5292: ZB = zb_and(n5032, n5290);
    let n5293: ZB = zb_or(n5291, n5292);
    let n5294: ZB = zb_and(n5032, n5293);
    let n5295: ZB = zb_and(n5033, n5293);
    let n5296: ZB = zb_or(n5294, n5295);
    let n5297: ZB = zb_or(n5286, n5296);
    let n5298: ZB = zb_and(n5037, n5297);
    let n5299: ZB = zb_and(n5036, n5297);
    let n5300: ZB = zb_or(n5298, n5299);
    let n5301: ZB = zb_or(n5278, n5300);
    let n5302: ZB = zb_or(n5277, n5301);
    let n5303: ZB = zb_and(n5038, n5302);
    let n5304: ZB = zb_and(n5039, n5302);
    let n5305: ZB = zb_or(n5303, n5304);
    let n5306: ZB = zb_or(n5055, n5305);
    let n5307: ZB = zb_and(n5094, n5306);
    let n5308: ZB = zb_and(n5095, n5306);
    let n5309: ZB = zb_or(n5307, n5308);
    let n5312: ZB = zb_and(n1411, n5038);
    let n5313: ZB = zb_not(n5312);
    let n5314: ZN = zsel_n(n5312, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n5315: ZB = zb_or(r_c41, n5312);
    let n5316: ZN = zsel_n(n1424, r_c20, n5314);
    let n5317: ZB = zsel_b(n1424, r_c41, n5315);
    let n5318: ZB = zb_and(n5092, n5312);
    let n5319: ZB = zb_and(n5092, n5313);
    let n5320: ZB = zb_and(n5019, n5318);
    let n5321: ZB = zb_and(n5040, n5318);
    let n5322: ZB = zb_or(n5320, n5321);
    let n5323: ZB = zb_and(n5042, n5322);
    let n5324: ZB = zb_and(n5043, n5322);
    let n5325: ZB = zb_and(n5044, n5324);
    let n5326: ZB = zb_and(n5045, n5324);
    let n5327: ZB = zb_or(n5325, n5326);
    let n5328: ZB = zb_or(n5323, n5327);
    let n5329: ZB = zb_and(n5047, n5328);
    let n5330: ZB = zb_and(n5046, n5328);
    let n5331: ZB = zb_or(n5329, n5330);
    let n5332: ZB = zb_or(n5319, n5331);
    let n5333: ZB = zb_or(n5055, n5332);
    let n5334: ZB = zb_and(n5094, n5333);
    let n5335: ZB = zb_and(n5095, n5333);
    let n5336: ZB = zb_or(n5334, n5335);
    let n5337: ZB = zb_and(n5095, n5336);
    let n5338: ZB = zn_gt(n5316, zn_splat(P8::from_raw(0i32)));
    let n5339: ZB = zn_le(n5316, zn_splat(P8::from_raw(0i32)));
    let n5340: ZB = zb_and(n5337, n5338);
    let n5341: ZB = zb_and(n5337, n5339);
    let n5342: ZB = zb_or(n5340, n5341);
    let n5343: ZB = zb_and(n5146, n5312);
    let n5344: ZB = zb_and(n5146, n5313);
    let n5345: ZB = zb_or(n5343, n5344);
    let n5346: ZB = zb_or(n5055, n5345);
    let n5347: ZB = zb_and(n5094, n5346);
    let n5348: ZB = zb_and(n5095, n5346);
    let n5349: ZB = zb_or(n5347, n5348);
    let n5350: ZB = zb_and(n5095, n5349);
    let n5351: ZB = zb_and(n5338, n5350);
    let n5352: ZB = zb_and(n5339, n5350);
    let n5353: ZB = zb_or(n5351, n5352);
    let n5354: ZB = zb_and(n5197, n5312);
    let n5355: ZB = zb_and(n5197, n5313);
    let n5356: ZB = zb_or(n5354, n5355);
    let n5357: ZB = zb_or(n5055, n5356);
    let n5358: ZB = zb_and(n5094, n5357);
    let n5359: ZB = zb_and(n5095, n5357);
    let n5360: ZB = zb_or(n5358, n5359);
    let n5361: ZB = zb_and(n5095, n5360);
    let n5362: ZB = zb_and(n5338, n5361);
    let n5363: ZB = zb_and(n5339, n5361);
    let n5364: ZB = zb_or(n5362, n5363);
    let n5365: ZB = zb_or(n5318, n5319);
    let n5366: ZB = zb_or(n5055, n5365);
    let n5367: ZB = zb_and(n5094, n5366);
    let n5368: ZB = zb_and(n5095, n5366);
    let n5369: ZB = zb_or(n5367, n5368);
    let n5370: ZB = zb_and(n5095, n5369);
    let n5371: ZB = zb_and(n5338, n5370);
    let n5372: ZB = zb_and(n5339, n5370);
    let n5373: ZB = zb_or(n5371, n5372);
    let n5374: ZB = zb_and(n5233, n5312);
    let n5375: ZB = zb_and(n5233, n5313);
    let n5376: ZB = zb_and(n5019, n5374);
    let n5377: ZB = zb_and(n5040, n5374);
    let n5378: ZB = zb_or(n5376, n5377);
    let n5379: ZB = zb_and(n5042, n5378);
    let n5380: ZB = zb_and(n5043, n5378);
    let n5381: ZB = zb_and(n5044, n5380);
    let n5382: ZB = zb_and(n5045, n5380);
    let n5383: ZB = zb_or(n5381, n5382);
    let n5384: ZB = zb_or(n5379, n5383);
    let n5385: ZB = zb_and(n5047, n5384);
    let n5386: ZB = zb_and(n5046, n5384);
    let n5387: ZB = zb_or(n5385, n5386);
    let n5388: ZB = zb_or(n5375, n5387);
    let n5389: ZB = zb_or(n5055, n5388);
    let n5390: ZB = zb_and(n5094, n5389);
    let n5391: ZB = zb_and(n5095, n5389);
    let n5392: ZB = zb_or(n5390, n5391);
    let n5393: ZB = zb_and(n5095, n5392);
    let n5394: ZB = zb_and(n5338, n5393);
    let n5395: ZB = zb_and(n5339, n5393);
    let n5396: ZB = zb_or(n5394, n5395);
    let n5397: ZB = zb_and(n5269, n5312);
    let n5398: ZB = zb_and(n5269, n5313);
    let n5399: ZB = zb_or(n5397, n5398);
    let n5400: ZB = zb_or(n5055, n5399);
    let n5401: ZB = zb_and(n5094, n5400);
    let n5402: ZB = zb_and(n5095, n5400);
    let n5403: ZB = zb_or(n5401, n5402);
    let n5404: ZB = zb_and(n5095, n5403);
    let n5405: ZB = zb_and(n5338, n5404);
    let n5406: ZB = zb_and(n5339, n5404);
    let n5407: ZB = zb_or(n5405, n5406);
    let n5408: ZB = zb_and(n5305, n5312);
    let n5409: ZB = zb_and(n5305, n5313);
    let n5410: ZB = zb_or(n5408, n5409);
    let n5411: ZB = zb_or(n5055, n5410);
    let n5412: ZB = zb_and(n5094, n5411);
    let n5413: ZB = zb_and(n5095, n5411);
    let n5414: ZB = zb_or(n5412, n5413);
    let n5415: ZB = zb_and(n5095, n5414);
    let n5416: ZB = zb_and(n5338, n5415);
    let n5417: ZB = zb_and(n5339, n5415);
    let n5418: ZB = zb_or(n5416, n5417);
    let n5419: ZB = zb_or(n5374, n5375);
    let n5420: ZB = zb_or(n5055, n5419);
    let n5421: ZB = zb_and(n5094, n5420);
    let n5422: ZB = zb_and(n5095, n5420);
    let n5423: ZB = zb_or(n5421, n5422);
    let n5424: ZB = zb_and(n5095, n5423);
    let n5425: ZB = zb_and(n5338, n5424);
    let n5426: ZB = zb_and(n5339, n5424);
    let n5427: ZB = zb_or(n5425, n5426);
    let n5431: ZB = zb_and(n4947, n4950);
    let n5432: ZB = zb_and(n4962, n5431);
    let n5433: ZB = zb_and(n4961, n5431);
    let n5434: ZB = zb_or(n5432, n5433);
    let n5435: ZB = zb_and(n4962, n5434);
    let n5436: ZB = zb_and(n4961, n5434);
    let n5437: ZB = zb_or(n5435, n5436);
    let n5438: ZB = zb_and(n4961, n5437);
    let n5439: ZB = zb_and(n4962, n5437);
    let n5440: ZB = zb_and(n4969, n5438);
    let n5441: ZB = zb_and(n4970, n5438);
    let n5442: ZB = zb_or(n5440, n5441);
    let n5443: ZB = zb_and(n1414, n5439);
    let n5444: ZB = zb_and(n1415, n5439);
    let n5445: ZB = zb_or(n5443, n5444);
    let n5446: ZB = zb_or(n5442, n5445);
    let n5447: ZB = zb_and(n1424, n5446);
    let n5448: ZB = zb_and(n1425, n5446);
    let n5449: ZB = zb_and(n4983, n5447);
    let n5450: ZB = zb_and(n4984, n5447);
    let n5451: ZB = zb_or(n5449, n5450);
    let n5452: ZB = zb_and(n4985, n5451);
    let n5453: ZB = zb_and(n4986, n5451);
    let n5454: ZB = zb_or(n5452, n5453);
    let n5455: ZB = zb_and(n4962, n5448);
    let n5456: ZB = zb_and(n4961, n5448);
    let n5457: ZB = zb_or(n5455, n5456);
    let n5458: ZB = zb_and(n4989, n5457);
    let n5459: ZB = zb_and(n4990, n5457);
    let n5460: ZB = zb_and(n4991, n5458);
    let n5461: ZB = zb_and(n4293, n5458);
    let n5462: ZB = zb_and(n4992, n5461);
    let n5463: ZB = zb_and(n4318, n5461);
    let n5464: ZB = zb_and(n4993, n5460);
    let n5465: ZB = zb_and(n4994, n5460);
    let n5466: ZB = zb_and(n4999, n5462);
    let n5467: ZB = zb_and(n5000, n5462);
    let n5468: ZB = zb_and(n4293, n5463);
    let n5469: ZB = zb_or(n5466, n5467);
    let n5470: ZB = zb_or(n5464, n5465);
    let n5471: ZB = zb_or(n5468, n5469);
    let n5472: ZB = zb_or(n5470, n5471);
    let n5473: ZB = zb_and(n4991, n5459);
    let n5474: ZB = zb_and(n4293, n5459);
    let n5475: ZB = zb_or(n5473, n5474);
    let n5476: ZB = zb_or(n5472, n5475);
    let n5477: ZB = zb_and(n5017, n5476);
    let n5478: ZB = zb_and(n5016, n5476);
    let n5479: ZB = zb_or(n5477, n5478);
    let n5480: ZB = zb_and(n5021, n5479);
    let n5481: ZB = zb_and(n5022, n5479);
    let n5482: ZB = zb_or(n5480, n5481);
    let n5483: ZB = zb_and(n4962, n5482);
    let n5484: ZB = zb_and(n4961, n5482);
    let n5485: ZB = zb_and(n5024, n5483);
    let n5486: ZB = zb_and(n5025, n5483);
    let n5487: ZB = zb_or(n5485, n5486);
    let n5488: ZB = zb_or(n5484, n5487);
    let n5489: ZB = zb_and(n5038, n5488);
    let n5490: ZB = zb_and(n5039, n5488);
    let n5491: ZB = zb_or(n5489, n5490);
    let n5492: ZB = zb_or(n5454, n5491);
    let n5493: ZB = zb_and(n5094, n5492);
    let n5494: ZB = zb_and(n5095, n5492);
    let n5495: ZB = zb_or(n5493, n5494);
    let n5496: ZB = zb_and(n5094, n5495);
    let n5497: ZB = zb_and(n5094, n5098);
    let n5498: ZN = zsel_n(n5496, r_c87, n4956);
    let n5499: ZN = zsel_n(n5496, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5500: ZB = zb_not(n5496);
    let n5501: ZB = zb_or(n5496, n5497);
    let n5502: ZB = zsel_b(n5496, n4948, n4958);
    let n5503: ZN = zsel_n(n5501, n5498, n3027);
    let n5504: ZN = zsel_n(n5501, n5499, zn_splat(P8::from_raw(983040i32)));
    let n5505: ZB = zb_not(n5501);
    let n5506: ZB = zb_or(n5500, n5505);
    let n5507: ZB = zsel_b(n5501, n5502, n3447);
    let n5510: ZB = zb_and(n4999, n5459);
    let n5511: ZB = zb_and(n5000, n5459);
    let n5512: ZB = zb_or(n5510, n5511);
    let n5513: ZB = zb_or(n5472, n5512);
    let n5514: ZB = zb_and(n5107, n5513);
    let n5515: ZB = zb_and(n5106, n5513);
    let n5516: ZB = zb_or(n5514, n5515);
    let n5517: ZB = zb_and(n5021, n5516);
    let n5518: ZB = zb_and(n5022, n5516);
    let n5519: ZB = zb_or(n5517, n5518);
    let n5520: ZB = zb_and(n5112, n5519);
    let n5521: ZB = zb_and(n5111, n5519);
    let n5522: ZB = zb_or(n5520, n5521);
    let n5523: ZB = zb_and(n5112, n5522);
    let n5524: ZB = zb_and(n5111, n5522);
    let n5525: ZB = zb_or(n5523, n5524);
    let n5526: ZB = zb_and(n5111, n5525);
    let n5527: ZB = zb_and(n5112, n5525);
    let n5528: ZB = zb_or(n5526, n5527);
    let n5529: ZB = zb_and(n5111, n5528);
    let n5530: ZB = zb_and(n5112, n5528);
    let n5531: ZB = zb_or(n5529, n5530);
    let n5532: ZB = zb_and(n4962, n5531);
    let n5533: ZB = zb_and(n4961, n5531);
    let n5534: ZB = zb_and(n5114, n5532);
    let n5535: ZB = zb_and(n5115, n5532);
    let n5536: ZB = zb_or(n5534, n5535);
    let n5537: ZB = zb_or(n5533, n5536);
    let n5538: ZB = zb_and(n5038, n5537);
    let n5539: ZB = zb_and(n5039, n5537);
    let n5540: ZB = zb_or(n5538, n5539);
    let n5541: ZB = zb_or(n5454, n5540);
    let n5542: ZB = zb_and(n5094, n5541);
    let n5543: ZB = zb_and(n5095, n5541);
    let n5544: ZB = zb_or(n5542, n5543);
    let n5545: ZB = zb_and(n5094, n5544);
    let n5546: ZB = zb_and(n5094, n5150);
    let n5547: ZN = zsel_n(n5545, r_c87, n4956);
    let n5548: ZN = zsel_n(n5545, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5549: ZB = zb_not(n5545);
    let n5550: ZB = zb_or(n5545, n5546);
    let n5551: ZB = zsel_b(n5545, n4948, n4958);
    let n5552: ZN = zsel_n(n5550, n5547, n3027);
    let n5553: ZN = zsel_n(n5550, n5548, zn_splat(P8::from_raw(983040i32)));
    let n5554: ZB = zb_not(n5550);
    let n5555: ZB = zb_or(n5549, n5554);
    let n5556: ZB = zsel_b(n5550, n5551, n3447);
    let n5559: ZB = zb_and(n4993, n5459);
    let n5560: ZB = zb_and(n4994, n5459);
    let n5561: ZB = zb_or(n5559, n5560);
    let n5562: ZB = zb_or(n5472, n5561);
    let n5563: ZB = zb_and(n5158, n5562);
    let n5564: ZB = zb_and(n5157, n5562);
    let n5565: ZB = zb_or(n5563, n5564);
    let n5566: ZB = zb_and(n5021, n5565);
    let n5567: ZB = zb_and(n5022, n5565);
    let n5568: ZB = zb_or(n5566, n5567);
    let n5569: ZB = zb_and(n5163, n5568);
    let n5570: ZB = zb_and(n5162, n5568);
    let n5571: ZB = zb_or(n5569, n5570);
    let n5572: ZB = zb_and(n5163, n5571);
    let n5573: ZB = zb_and(n5162, n5571);
    let n5574: ZB = zb_or(n5572, n5573);
    let n5575: ZB = zb_and(n5162, n5574);
    let n5576: ZB = zb_and(n5163, n5574);
    let n5577: ZB = zb_or(n5575, n5576);
    let n5578: ZB = zb_and(n5162, n5577);
    let n5579: ZB = zb_and(n5163, n5577);
    let n5580: ZB = zb_or(n5578, n5579);
    let n5581: ZB = zb_and(n4962, n5580);
    let n5582: ZB = zb_and(n4961, n5580);
    let n5583: ZB = zb_and(n5165, n5581);
    let n5584: ZB = zb_and(n5166, n5581);
    let n5585: ZB = zb_or(n5583, n5584);
    let n5586: ZB = zb_or(n5582, n5585);
    let n5587: ZB = zb_and(n5038, n5586);
    let n5588: ZB = zb_and(n5039, n5586);
    let n5589: ZB = zb_or(n5587, n5588);
    let n5590: ZB = zb_or(n5454, n5589);
    let n5591: ZB = zb_and(n5094, n5590);
    let n5592: ZB = zb_and(n5095, n5590);
    let n5593: ZB = zb_or(n5591, n5592);
    let n5594: ZB = zb_and(n5094, n5593);
    let n5595: ZB = zb_and(n5094, n5201);
    let n5596: ZN = zsel_n(n5594, r_c87, n4956);
    let n5597: ZN = zsel_n(n5594, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5598: ZB = zb_not(n5594);
    let n5599: ZB = zb_or(n5594, n5595);
    let n5600: ZB = zsel_b(n5594, n4948, n4958);
    let n5601: ZN = zsel_n(n5599, n5596, n3027);
    let n5602: ZN = zsel_n(n5599, n5597, zn_splat(P8::from_raw(983040i32)));
    let n5603: ZB = zb_not(n5599);
    let n5604: ZB = zb_or(n5598, n5603);
    let n5605: ZB = zsel_b(n5599, n5600, n3447);
    let n5608: ZB = zb_and(n136, n5488);
    let n5609: ZB = zb_and(r_c293, n5488);
    let n5610: ZB = zb_and(n5026, n5608);
    let n5611: ZB = zb_and(n5027, n5608);
    let n5612: ZB = zb_and(n5030, n5611);
    let n5613: ZB = zb_and(n5029, n5611);
    let n5614: ZB = zb_or(n5612, n5613);
    let n5615: ZB = zb_and(n5030, n5614);
    let n5616: ZB = zb_and(n5029, n5614);
    let n5617: ZB = zb_or(n5615, n5616);
    let n5618: ZB = zb_and(n5029, n5617);
    let n5619: ZB = zb_and(n5030, n5617);
    let n5620: ZB = zb_and(n5033, n5619);
    let n5621: ZB = zb_and(n5032, n5619);
    let n5622: ZB = zb_or(n5620, n5621);
    let n5623: ZB = zb_and(n5033, n5622);
    let n5624: ZB = zb_and(n5032, n5622);
    let n5625: ZB = zb_or(n5623, n5624);
    let n5626: ZB = zb_and(n5032, n5625);
    let n5627: ZB = zb_and(n5033, n5625);
    let n5628: ZB = zb_or(n5626, n5627);
    let n5629: ZB = zb_or(n5618, n5628);
    let n5630: ZB = zb_and(n5037, n5629);
    let n5631: ZB = zb_and(n5036, n5629);
    let n5632: ZB = zb_or(n5630, n5631);
    let n5633: ZB = zb_or(n5610, n5632);
    let n5634: ZB = zb_or(n5609, n5633);
    let n5635: ZB = zb_and(n5038, n5634);
    let n5636: ZB = zb_and(n5039, n5634);
    let n5637: ZB = zb_or(n5635, n5636);
    let n5638: ZB = zb_or(n5454, n5637);
    let n5639: ZB = zb_and(n5094, n5638);
    let n5640: ZB = zb_and(n5095, n5638);
    let n5641: ZB = zb_or(n5639, n5640);
    let n5642: ZB = zb_and(n5094, n5641);
    let n5643: ZB = zb_and(n5094, n5237);
    let n5644: ZN = zsel_n(n5642, r_c87, n4956);
    let n5645: ZN = zsel_n(n5642, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5646: ZB = zb_not(n5642);
    let n5647: ZB = zb_or(n5642, n5643);
    let n5648: ZB = zsel_b(n5642, n4948, n4958);
    let n5649: ZN = zsel_n(n5647, n5644, n3027);
    let n5650: ZN = zsel_n(n5647, n5645, zn_splat(P8::from_raw(983040i32)));
    let n5651: ZB = zb_not(n5647);
    let n5652: ZB = zb_or(n5646, n5651);
    let n5653: ZB = zsel_b(n5647, n5648, n3447);
    let n5656: ZB = zb_and(n136, n5537);
    let n5657: ZB = zb_and(r_c293, n5537);
    let n5658: ZB = zb_and(n5026, n5656);
    let n5659: ZB = zb_and(n5027, n5656);
    let n5660: ZB = zb_and(n5030, n5659);
    let n5661: ZB = zb_and(n5029, n5659);
    let n5662: ZB = zb_or(n5660, n5661);
    let n5663: ZB = zb_and(n5030, n5662);
    let n5664: ZB = zb_and(n5029, n5662);
    let n5665: ZB = zb_or(n5663, n5664);
    let n5666: ZB = zb_and(n5029, n5665);
    let n5667: ZB = zb_and(n5030, n5665);
    let n5668: ZB = zb_and(n5033, n5667);
    let n5669: ZB = zb_and(n5032, n5667);
    let n5670: ZB = zb_or(n5668, n5669);
    let n5671: ZB = zb_and(n5033, n5670);
    let n5672: ZB = zb_and(n5032, n5670);
    let n5673: ZB = zb_or(n5671, n5672);
    let n5674: ZB = zb_and(n5032, n5673);
    let n5675: ZB = zb_and(n5033, n5673);
    let n5676: ZB = zb_or(n5674, n5675);
    let n5677: ZB = zb_or(n5666, n5676);
    let n5678: ZB = zb_and(n5037, n5677);
    let n5679: ZB = zb_and(n5036, n5677);
    let n5680: ZB = zb_or(n5678, n5679);
    let n5681: ZB = zb_or(n5658, n5680);
    let n5682: ZB = zb_or(n5657, n5681);
    let n5683: ZB = zb_and(n5038, n5682);
    let n5684: ZB = zb_and(n5039, n5682);
    let n5685: ZB = zb_or(n5683, n5684);
    let n5686: ZB = zb_or(n5454, n5685);
    let n5687: ZB = zb_and(n5094, n5686);
    let n5688: ZB = zb_and(n5095, n5686);
    let n5689: ZB = zb_or(n5687, n5688);
    let n5690: ZB = zb_and(n5094, n5689);
    let n5691: ZB = zb_and(n5094, n5273);
    let n5692: ZN = zsel_n(n5690, r_c87, n4956);
    let n5693: ZN = zsel_n(n5690, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5694: ZB = zb_not(n5690);
    let n5695: ZB = zb_or(n5690, n5691);
    let n5696: ZB = zsel_b(n5690, n4948, n4958);
    let n5697: ZN = zsel_n(n5695, n5692, n3027);
    let n5698: ZN = zsel_n(n5695, n5693, zn_splat(P8::from_raw(983040i32)));
    let n5699: ZB = zb_not(n5695);
    let n5700: ZB = zb_or(n5694, n5699);
    let n5701: ZB = zsel_b(n5695, n5696, n3447);
    let n5704: ZB = zb_and(n136, n5586);
    let n5705: ZB = zb_and(r_c293, n5586);
    let n5706: ZB = zb_and(n5026, n5704);
    let n5707: ZB = zb_and(n5027, n5704);
    let n5708: ZB = zb_and(n5030, n5707);
    let n5709: ZB = zb_and(n5029, n5707);
    let n5710: ZB = zb_or(n5708, n5709);
    let n5711: ZB = zb_and(n5030, n5710);
    let n5712: ZB = zb_and(n5029, n5710);
    let n5713: ZB = zb_or(n5711, n5712);
    let n5714: ZB = zb_and(n5029, n5713);
    let n5715: ZB = zb_and(n5030, n5713);
    let n5716: ZB = zb_and(n5033, n5715);
    let n5717: ZB = zb_and(n5032, n5715);
    let n5718: ZB = zb_or(n5716, n5717);
    let n5719: ZB = zb_and(n5033, n5718);
    let n5720: ZB = zb_and(n5032, n5718);
    let n5721: ZB = zb_or(n5719, n5720);
    let n5722: ZB = zb_and(n5032, n5721);
    let n5723: ZB = zb_and(n5033, n5721);
    let n5724: ZB = zb_or(n5722, n5723);
    let n5725: ZB = zb_or(n5714, n5724);
    let n5726: ZB = zb_and(n5037, n5725);
    let n5727: ZB = zb_and(n5036, n5725);
    let n5728: ZB = zb_or(n5726, n5727);
    let n5729: ZB = zb_or(n5706, n5728);
    let n5730: ZB = zb_or(n5705, n5729);
    let n5731: ZB = zb_and(n5038, n5730);
    let n5732: ZB = zb_and(n5039, n5730);
    let n5733: ZB = zb_or(n5731, n5732);
    let n5734: ZB = zb_or(n5454, n5733);
    let n5735: ZB = zb_and(n5094, n5734);
    let n5736: ZB = zb_and(n5095, n5734);
    let n5737: ZB = zb_or(n5735, n5736);
    let n5738: ZB = zb_and(n5094, n5737);
    let n5739: ZB = zb_and(n5094, n5309);
    let n5740: ZN = zsel_n(n5738, r_c87, n4956);
    let n5741: ZN = zsel_n(n5738, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5742: ZB = zb_not(n5738);
    let n5743: ZB = zb_or(n5738, n5739);
    let n5744: ZB = zsel_b(n5738, n4948, n4958);
    let n5745: ZN = zsel_n(n5743, n5740, n3027);
    let n5746: ZN = zsel_n(n5743, n5741, zn_splat(P8::from_raw(983040i32)));
    let n5747: ZB = zb_not(n5743);
    let n5748: ZB = zb_or(n5742, n5747);
    let n5749: ZB = zsel_b(n5743, n5744, n3447);
    let n5752: ZB = zb_and(n5312, n5491);
    let n5753: ZB = zb_and(n5313, n5491);
    let n5754: ZB = zb_and(n5019, n5752);
    let n5755: ZB = zb_and(n5040, n5752);
    let n5756: ZB = zb_or(n5754, n5755);
    let n5757: ZB = zb_and(n5042, n5756);
    let n5758: ZB = zb_and(n5043, n5756);
    let n5759: ZB = zb_and(n5044, n5758);
    let n5760: ZB = zb_and(n5045, n5758);
    let n5761: ZB = zb_or(n5759, n5760);
    let n5762: ZB = zb_or(n5757, n5761);
    let n5763: ZB = zb_and(n5047, n5762);
    let n5764: ZB = zb_and(n5046, n5762);
    let n5765: ZB = zb_or(n5763, n5764);
    let n5766: ZB = zb_or(n5753, n5765);
    let n5767: ZB = zb_or(n5454, n5766);
    let n5768: ZB = zb_and(n5094, n5767);
    let n5769: ZB = zb_and(n5095, n5767);
    let n5770: ZB = zb_or(n5768, n5769);
    let n5771: ZB = zb_and(n5094, n5770);
    let n5772: ZB = zb_and(n5094, n5336);
    let n5773: ZN = zsel_n(n5771, r_c87, n4956);
    let n5774: ZN = zsel_n(n5771, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5775: ZB = zb_not(n5771);
    let n5776: ZB = zb_or(n5771, n5772);
    let n5777: ZB = zsel_b(n5771, n4948, n4958);
    let n5778: ZN = zsel_n(n5776, n5773, n3027);
    let n5779: ZN = zsel_n(n5776, n5774, zn_splat(P8::from_raw(983040i32)));
    let n5780: ZN = zsel_n(n5776, n5316, n1841);
    let n5781: ZB = zb_not(n5776);
    let n5782: ZB = zb_or(n5775, n5781);
    let n5783: ZB = zsel_b(n5776, n5777, n3447);
    let n5784: ZB = zn_gt(n5780, zn_splat(P8::from_raw(0i32)));
    let n5785: ZB = zn_le(n5780, zn_splat(P8::from_raw(0i32)));
    let n5786: ZB = zb_and(n5776, n5784);
    let n5787: ZB = zb_and(n5776, n5785);
    let n5788: ZB = zb_or(n5786, n5787);
    let n5790: ZB = zb_and(n5312, n5540);
    let n5791: ZB = zb_and(n5313, n5540);
    let n5792: ZB = zb_or(n5790, n5791);
    let n5793: ZB = zb_or(n5454, n5792);
    let n5794: ZB = zb_and(n5094, n5793);
    let n5795: ZB = zb_and(n5095, n5793);
    let n5796: ZB = zb_or(n5794, n5795);
    let n5797: ZB = zb_and(n5094, n5796);
    let n5798: ZB = zb_and(n5094, n5349);
    let n5799: ZN = zsel_n(n5797, r_c87, n4956);
    let n5800: ZN = zsel_n(n5797, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5801: ZB = zb_not(n5797);
    let n5802: ZB = zb_or(n5797, n5798);
    let n5803: ZB = zsel_b(n5797, n4948, n4958);
    let n5804: ZN = zsel_n(n5802, n5799, n3027);
    let n5805: ZN = zsel_n(n5802, n5800, zn_splat(P8::from_raw(983040i32)));
    let n5806: ZN = zsel_n(n5802, n5316, n1841);
    let n5807: ZB = zb_not(n5802);
    let n5808: ZB = zb_or(n5801, n5807);
    let n5809: ZB = zsel_b(n5802, n5803, n3447);
    let n5810: ZB = zn_gt(n5806, zn_splat(P8::from_raw(0i32)));
    let n5811: ZB = zn_le(n5806, zn_splat(P8::from_raw(0i32)));
    let n5812: ZB = zb_and(n5802, n5810);
    let n5813: ZB = zb_and(n5802, n5811);
    let n5814: ZB = zb_or(n5812, n5813);
    let n5816: ZB = zb_and(n5312, n5589);
    let n5817: ZB = zb_and(n5313, n5589);
    let n5818: ZB = zb_or(n5816, n5817);
    let n5819: ZB = zb_or(n5454, n5818);
    let n5820: ZB = zb_and(n5094, n5819);
    let n5821: ZB = zb_and(n5095, n5819);
    let n5822: ZB = zb_or(n5820, n5821);
    let n5823: ZB = zb_and(n5094, n5822);
    let n5824: ZB = zb_and(n5094, n5360);
    let n5825: ZN = zsel_n(n5823, r_c87, n4956);
    let n5826: ZN = zsel_n(n5823, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5827: ZB = zb_not(n5823);
    let n5828: ZB = zb_or(n5823, n5824);
    let n5829: ZB = zsel_b(n5823, n4948, n4958);
    let n5830: ZN = zsel_n(n5828, n5825, n3027);
    let n5831: ZN = zsel_n(n5828, n5826, zn_splat(P8::from_raw(983040i32)));
    let n5832: ZN = zsel_n(n5828, n5316, n1841);
    let n5833: ZB = zb_not(n5828);
    let n5834: ZB = zb_or(n5827, n5833);
    let n5835: ZB = zsel_b(n5828, n5829, n3447);
    let n5836: ZB = zn_gt(n5832, zn_splat(P8::from_raw(0i32)));
    let n5837: ZB = zn_le(n5832, zn_splat(P8::from_raw(0i32)));
    let n5838: ZB = zb_and(n5828, n5836);
    let n5839: ZB = zb_and(n5828, n5837);
    let n5840: ZB = zb_or(n5838, n5839);
    let n5842: ZB = zb_or(n5752, n5753);
    let n5843: ZB = zb_or(n5454, n5842);
    let n5844: ZB = zb_and(n5094, n5843);
    let n5845: ZB = zb_and(n5095, n5843);
    let n5846: ZB = zb_or(n5844, n5845);
    let n5847: ZB = zb_and(n5094, n5846);
    let n5848: ZB = zb_and(n5094, n5369);
    let n5849: ZN = zsel_n(n5847, r_c87, n4956);
    let n5850: ZN = zsel_n(n5847, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5851: ZB = zb_not(n5847);
    let n5852: ZB = zb_or(n5847, n5848);
    let n5853: ZB = zsel_b(n5847, n4948, n4958);
    let n5854: ZN = zsel_n(n5852, n5849, n3027);
    let n5855: ZN = zsel_n(n5852, n5850, zn_splat(P8::from_raw(983040i32)));
    let n5856: ZN = zsel_n(n5852, n5316, n1841);
    let n5857: ZB = zb_not(n5852);
    let n5858: ZB = zb_or(n5851, n5857);
    let n5859: ZB = zsel_b(n5852, n5853, n3447);
    let n5860: ZB = zn_gt(n5856, zn_splat(P8::from_raw(0i32)));
    let n5861: ZB = zn_le(n5856, zn_splat(P8::from_raw(0i32)));
    let n5862: ZB = zb_and(n5852, n5860);
    let n5863: ZB = zb_and(n5852, n5861);
    let n5864: ZB = zb_or(n5862, n5863);
    let n5866: ZB = zb_and(n5312, n5637);
    let n5867: ZB = zb_and(n5313, n5637);
    let n5868: ZB = zb_and(n5019, n5866);
    let n5869: ZB = zb_and(n5040, n5866);
    let n5870: ZB = zb_or(n5868, n5869);
    let n5871: ZB = zb_and(n5042, n5870);
    let n5872: ZB = zb_and(n5043, n5870);
    let n5873: ZB = zb_and(n5044, n5872);
    let n5874: ZB = zb_and(n5045, n5872);
    let n5875: ZB = zb_or(n5873, n5874);
    let n5876: ZB = zb_or(n5871, n5875);
    let n5877: ZB = zb_and(n5047, n5876);
    let n5878: ZB = zb_and(n5046, n5876);
    let n5879: ZB = zb_or(n5877, n5878);
    let n5880: ZB = zb_or(n5867, n5879);
    let n5881: ZB = zb_or(n5454, n5880);
    let n5882: ZB = zb_and(n5094, n5881);
    let n5883: ZB = zb_and(n5095, n5881);
    let n5884: ZB = zb_or(n5882, n5883);
    let n5885: ZB = zb_and(n5094, n5884);
    let n5886: ZB = zb_and(n5094, n5392);
    let n5887: ZN = zsel_n(n5885, r_c87, n4956);
    let n5888: ZN = zsel_n(n5885, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5889: ZB = zb_not(n5885);
    let n5890: ZB = zb_or(n5885, n5886);
    let n5891: ZB = zsel_b(n5885, n4948, n4958);
    let n5892: ZN = zsel_n(n5890, n5887, n3027);
    let n5893: ZN = zsel_n(n5890, n5888, zn_splat(P8::from_raw(983040i32)));
    let n5894: ZN = zsel_n(n5890, n5316, n1841);
    let n5895: ZB = zb_not(n5890);
    let n5896: ZB = zb_or(n5889, n5895);
    let n5897: ZB = zsel_b(n5890, n5891, n3447);
    let n5898: ZB = zn_gt(n5894, zn_splat(P8::from_raw(0i32)));
    let n5899: ZB = zn_le(n5894, zn_splat(P8::from_raw(0i32)));
    let n5900: ZB = zb_and(n5890, n5898);
    let n5901: ZB = zb_and(n5890, n5899);
    let n5902: ZB = zb_or(n5900, n5901);
    let n5904: ZB = zb_and(n5312, n5685);
    let n5905: ZB = zb_and(n5313, n5685);
    let n5906: ZB = zb_or(n5904, n5905);
    let n5907: ZB = zb_or(n5454, n5906);
    let n5908: ZB = zb_and(n5094, n5907);
    let n5909: ZB = zb_and(n5095, n5907);
    let n5910: ZB = zb_or(n5908, n5909);
    let n5911: ZB = zb_and(n5094, n5910);
    let n5912: ZB = zb_and(n5094, n5403);
    let n5913: ZN = zsel_n(n5911, r_c87, n4956);
    let n5914: ZN = zsel_n(n5911, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5915: ZB = zb_not(n5911);
    let n5916: ZB = zb_or(n5911, n5912);
    let n5917: ZB = zsel_b(n5911, n4948, n4958);
    let n5918: ZN = zsel_n(n5916, n5913, n3027);
    let n5919: ZN = zsel_n(n5916, n5914, zn_splat(P8::from_raw(983040i32)));
    let n5920: ZN = zsel_n(n5916, n5316, n1841);
    let n5921: ZB = zb_not(n5916);
    let n5922: ZB = zb_or(n5915, n5921);
    let n5923: ZB = zsel_b(n5916, n5917, n3447);
    let n5924: ZB = zn_gt(n5920, zn_splat(P8::from_raw(0i32)));
    let n5925: ZB = zn_le(n5920, zn_splat(P8::from_raw(0i32)));
    let n5926: ZB = zb_and(n5916, n5924);
    let n5927: ZB = zb_and(n5916, n5925);
    let n5928: ZB = zb_or(n5926, n5927);
    let n5930: ZB = zb_and(n5312, n5733);
    let n5931: ZB = zb_and(n5313, n5733);
    let n5932: ZB = zb_or(n5930, n5931);
    let n5933: ZB = zb_or(n5454, n5932);
    let n5934: ZB = zb_and(n5094, n5933);
    let n5935: ZB = zb_and(n5095, n5933);
    let n5936: ZB = zb_or(n5934, n5935);
    let n5937: ZB = zb_and(n5094, n5936);
    let n5938: ZB = zb_and(n5094, n5414);
    let n5939: ZN = zsel_n(n5937, r_c87, n4956);
    let n5940: ZN = zsel_n(n5937, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5941: ZB = zb_not(n5937);
    let n5942: ZB = zb_or(n5937, n5938);
    let n5943: ZB = zsel_b(n5937, n4948, n4958);
    let n5944: ZN = zsel_n(n5942, n5939, n3027);
    let n5945: ZN = zsel_n(n5942, n5940, zn_splat(P8::from_raw(983040i32)));
    let n5946: ZN = zsel_n(n5942, n5316, n1841);
    let n5947: ZB = zb_not(n5942);
    let n5948: ZB = zb_or(n5941, n5947);
    let n5949: ZB = zsel_b(n5942, n5943, n3447);
    let n5950: ZB = zn_gt(n5946, zn_splat(P8::from_raw(0i32)));
    let n5951: ZB = zn_le(n5946, zn_splat(P8::from_raw(0i32)));
    let n5952: ZB = zb_and(n5942, n5950);
    let n5953: ZB = zb_and(n5942, n5951);
    let n5954: ZB = zb_or(n5952, n5953);
    let n5956: ZB = zb_or(n5866, n5867);
    let n5957: ZB = zb_or(n5454, n5956);
    let n5958: ZB = zb_and(n5094, n5957);
    let n5959: ZB = zb_and(n5095, n5957);
    let n5960: ZB = zb_or(n5958, n5959);
    let n5961: ZB = zb_and(n5094, n5960);
    let n5962: ZB = zb_and(n5094, n5423);
    let n5963: ZN = zsel_n(n5961, r_c87, n4956);
    let n5964: ZN = zsel_n(n5961, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n5965: ZB = zb_not(n5961);
    let n5966: ZB = zb_or(n5961, n5962);
    let n5967: ZB = zsel_b(n5961, n4948, n4958);
    let n5968: ZN = zsel_n(n5966, n5963, n3027);
    let n5969: ZN = zsel_n(n5966, n5964, zn_splat(P8::from_raw(983040i32)));
    let n5970: ZN = zsel_n(n5966, n5316, n1841);
    let n5971: ZB = zb_not(n5966);
    let n5972: ZB = zb_or(n5965, n5971);
    let n5973: ZB = zsel_b(n5966, n5967, n3447);
    let n5974: ZB = zn_gt(n5970, zn_splat(P8::from_raw(0i32)));
    let n5975: ZB = zn_le(n5970, zn_splat(P8::from_raw(0i32)));
    let n5976: ZB = zb_and(n5966, n5974);
    let n5977: ZB = zb_and(n5966, n5975);
    let n5978: ZB = zb_or(n5976, n5977);
    let n5980: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n5981: ZN = zn_sub(n3736, zn_splat(P8::from_raw(32768i32)));
    let n5982: ZN = zn_sub(n5981, n3737);
    let n5983: ZN = zsel_n(n3872, zn_splat(P8::from_raw(0i32)), n5982);
    let n5984: ZN = zsel_n(n3867, n5982, n5983);
    let n5985: ZN = zsel_n(n3855, zn_splat(P8::from_raw(0i32)), n5984);
    let n5986: ZN = zsel_n(n3850, n5982, n5985);
    let n5987: ZN = zsel_n(n3838, zn_splat(P8::from_raw(0i32)), n5986);
    let n5988: ZN = zsel_n(n3833, n5982, n5987);
    let n5989: ZN = zsel_n(n3821, zn_splat(P8::from_raw(0i32)), n5988);
    let n5990: ZN = zsel_n(n3816, n5982, n5989);
    let n5991: ZN = zsel_n(n3804, zn_splat(P8::from_raw(0i32)), n5990);
    let n5992: ZN = zsel_n(n3799, n5982, n5991);
    let n5993: ZN = zsel_n(n3787, zn_splat(P8::from_raw(0i32)), n5992);
    let n5994: ZN = zsel_n(n3782, n5982, n5993);
    let n5995: ZN = zsel_n(n3770, zn_splat(P8::from_raw(0i32)), n5994);
    let n5996: ZN = zsel_n(n3765, n5982, n5995);
    let n5997: ZN = zsel_n(n3753, zn_splat(P8::from_raw(0i32)), n5996);
    let n5998: ZN = zn_sub(n3945, zn_splat(P8::from_raw(32768i32)));
    let n5999: ZN = zn_sub(n5998, n3946);
    let n6000: ZN = zsel_n(n4132, zn_splat(P8::from_raw(0i32)), n5999);
    let n6001: ZN = zsel_n(n4121, n5999, n6000);
    let n6002: ZN = zsel_n(n4109, zn_splat(P8::from_raw(0i32)), n6001);
    let n6003: ZN = zsel_n(n4098, n5999, n6002);
    let n6004: ZN = zsel_n(n4086, zn_splat(P8::from_raw(0i32)), n6003);
    let n6005: ZN = zsel_n(n4075, n5999, n6004);
    let n6006: ZN = zsel_n(n4063, zn_splat(P8::from_raw(0i32)), n6005);
    let n6007: ZN = zsel_n(n4052, n5999, n6006);
    let n6008: ZN = zsel_n(n4040, zn_splat(P8::from_raw(0i32)), n6007);
    let n6009: ZN = zsel_n(n4029, n5999, n6008);
    let n6010: ZN = zsel_n(n4017, zn_splat(P8::from_raw(0i32)), n6009);
    let n6011: ZN = zsel_n(n4006, n5999, n6010);
    let n6012: ZN = zsel_n(n3994, zn_splat(P8::from_raw(0i32)), n6011);
    let n6013: ZN = zsel_n(n3983, n5999, n6012);
    let n6014: ZN = zsel_n(n3971, zn_splat(P8::from_raw(0i32)), n6013);
    let n6015: ZN = zsel_n(n3731, n5997, r_c366);
    let n6016: ZN = zsel_n(n3731, n6014, r_c367);
    let n6017: ZN = zn_sub(n4207, r_c356);
    let n6018: ZN = zn_max(r_c358, n6017);
    let n6019: ZN = zn_add(r_c356, n4207);
    let n6020: ZN = zn_min(r_c358, n6019);
    let n6021: ZN = zsel_n(n4983, n6018, n6020);
    let n6022: ZN = zn_sub(n4208, r_c357);
    let n6023: ZN = zn_max(r_c359, n6022);
    let n6024: ZN = zn_add(r_c357, n4208);
    let n6025: ZN = zn_min(r_c359, n6024);
    let n6026: ZN = zsel_n(n4985, n6023, n6025);
    let n6027: ZN = zsel_n(n5021, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n6028: ZN = zn_sub(n4208, n6027);
    let n6029: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n6028);
    let n6030: ZN = zn_add(n4208, n6027);
    let n6031: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n6030);
    let n6032: ZN = zsel_n(n5024, n6029, n6031);
    let n6033: ZN = zsel_n(n4962, n6032, n4208);
    let n6034: ZN = zn_neg(n5035);
    let n6035: ZN = zn_mul(n6034, zn_splat(P8::from_raw(131072i32)));
    let n6036: ZN = zsel_n(n5037, n6035, n5015);
    let n6037: ZN = zsel_n(n5037, zn_splat(P8::from_raw(-131072i32)), n6033);
    let n6038: ZN = zsel_n(n5026, zn_splat(P8::from_raw(0i32)), n4973);
    let n6039: ZN = zsel_n(n5026, n5015, n6036);
    let n6040: ZN = zsel_n(n5026, zn_splat(P8::from_raw(-131072i32)), n6037);
    let n6041: ZN = zn_sub(n4972, zn_splat(P8::from_raw(65536i32)));
    let n6042: ZN = zsel_n(n5044, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n6043: ZN = zsel_n(n5042, zn_splat(P8::from_raw(131072i32)), n6042);
    let n6044: ZN = zsel_n(n5047, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n6045: ZB = zsel_b(n1424, r_c360, n5019);
    let n6046: ZN = zsel_n(n1424, n6021, n5015);
    let n6047: ZN = zsel_n(n1424, n6026, n6033);
    let n6048: ZB = zb_and(n5095, n5495);
    let n6049: ZN = zsel_n(n1556, n5980, r_c20);
    let n6050: ZN = zsel_n(n1556, r_c280, n1423);
    let n6051: ZN = zsel_n(n1556, r_c282, n1551);
    let n6052: ZN = zsel_n(n1556, r_c283, n4972);
    let n6053: ZN = zsel_n(n1556, r_c285, n4973);
    let n6054: ZB = zb_and(r_c292, n1556);
    let n6055: ZB = zb_and(r_c293, n1556);
    let n6056: ZN = zsel_n(n1556, r_c299, n4205);
    let n6057: ZN = zsel_n(n1556, r_c300, n4206);
    let n6058: ZB = zsel_b(n1556, r_c360, n6045);
    let n6059: ZN = zsel_n(n1556, r_c366, n6015);
    let n6060: ZN = zsel_n(n1556, r_c367, n6016);
    let n6061: ZN = zsel_n(n1556, r_c368, n6046);
    let n6062: ZN = zsel_n(n1556, r_c369, n6047);
    let n6063: ZB = zb_or(n1556, n6048);
    let n6064: ZB = zb_or(n1556, n4948);
    let n6065: ZB = zn_gt(n6049, zn_splat(P8::from_raw(0i32)));
    let n6066: ZB = zn_le(n6049, zn_splat(P8::from_raw(0i32)));
    let n6067: ZB = zb_and(n6063, n6065);
    let n6068: ZB = zb_and(n6063, n6066);
    let n6069: ZB = zn_lt(n6056, zn_splat(P8::from_raw(-65536i32)));
    let n6070: ZB = zn_ge(n6056, zn_splat(P8::from_raw(-65536i32)));
    let n6071: ZB = zb_and(n6068, n6070);
    let n6072: ZB = zb_and(n6068, n6069);
    let n6073: ZB = zn_gt(n6056, zn_splat(P8::from_raw(7929856i32)));
    let n6074: ZB = zb_or(n6071, n6072);
    let n6075: ZB = zb_or(n6069, n6073);
    let n6076: ZB = zb_not(n6075);
    let n6077: ZB = zb_and(n6074, n6075);
    let n6078: ZB = zb_and(n6074, n6076);
    let n6079: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n6056);
    let n6080: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n6079);
    let n6081: ZN = zsel_n(n6075, n6080, n6056);
    let n6082: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6061);
    let n6083: ZB = zb_or(n6077, n6078);
    let n6084: ZN = zsel_n(n6065, n6056, n6081);
    let n6085: ZN = zsel_n(n6065, n6061, n6082);
    let n6086: ZB = zb_or(n6067, n6083);
    let n6088: ZN = zn_max(n5113, n6028);
    let n6089: ZN = zn_min(n5113, n6030);
    let n6090: ZN = zsel_n(n5114, n6088, n6089);
    let n6091: ZN = zsel_n(n4962, n6090, n4208);
    let n6092: ZN = zsel_n(n5037, n6035, n5105);
    let n6093: ZN = zsel_n(n5037, zn_splat(P8::from_raw(-131072i32)), n6091);
    let n6094: ZN = zsel_n(n5026, n5105, n6092);
    let n6095: ZN = zsel_n(n5026, zn_splat(P8::from_raw(-131072i32)), n6093);
    let n6096: ZB = zsel_b(n1424, r_c360, n5109);
    let n6097: ZN = zsel_n(n1424, n6021, n5105);
    let n6098: ZN = zsel_n(n1424, n6026, n6091);
    let n6099: ZB = zb_and(n5095, n5544);
    let n6100: ZB = zsel_b(n1556, r_c360, n6096);
    let n6101: ZN = zsel_n(n1556, r_c368, n6097);
    let n6102: ZN = zsel_n(n1556, r_c369, n6098);
    let n6103: ZB = zb_or(n1556, n6099);
    let n6104: ZB = zb_and(n6065, n6103);
    let n6105: ZB = zb_and(n6066, n6103);
    let n6106: ZB = zb_and(n6070, n6105);
    let n6107: ZB = zb_and(n6069, n6105);
    let n6108: ZB = zb_or(n6106, n6107);
    let n6109: ZB = zb_and(n6075, n6108);
    let n6110: ZB = zb_and(n6076, n6108);
    let n6111: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6101);
    let n6112: ZB = zb_or(n6109, n6110);
    let n6113: ZN = zsel_n(n6065, n6101, n6111);
    let n6114: ZB = zb_or(n6104, n6112);
    let n6115: ZN = zn_max(n5164, n6028);
    let n6116: ZN = zn_min(n5164, n6030);
    let n6117: ZN = zsel_n(n5165, n6115, n6116);
    let n6118: ZN = zsel_n(n4962, n6117, n4208);
    let n6119: ZN = zsel_n(n5037, n6035, n5156);
    let n6120: ZN = zsel_n(n5037, zn_splat(P8::from_raw(-131072i32)), n6118);
    let n6121: ZN = zsel_n(n5026, n5156, n6119);
    let n6122: ZN = zsel_n(n5026, zn_splat(P8::from_raw(-131072i32)), n6120);
    let n6123: ZB = zsel_b(n1424, r_c360, n5160);
    let n6124: ZN = zsel_n(n1424, n6021, n5156);
    let n6125: ZN = zsel_n(n1424, n6026, n6118);
    let n6126: ZB = zb_and(n5095, n5593);
    let n6127: ZB = zsel_b(n1556, r_c360, n6123);
    let n6128: ZN = zsel_n(n1556, r_c368, n6124);
    let n6129: ZN = zsel_n(n1556, r_c369, n6125);
    let n6130: ZB = zb_or(n1556, n6126);
    let n6131: ZB = zb_and(n6065, n6130);
    let n6132: ZB = zb_and(n6066, n6130);
    let n6133: ZB = zb_and(n6070, n6132);
    let n6134: ZB = zb_and(n6069, n6132);
    let n6135: ZB = zb_or(n6133, n6134);
    let n6136: ZB = zb_and(n6075, n6135);
    let n6137: ZB = zb_and(n6076, n6135);
    let n6138: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6128);
    let n6139: ZB = zb_or(n6136, n6137);
    let n6140: ZN = zsel_n(n6065, n6128, n6138);
    let n6141: ZB = zb_or(n6131, n6139);
    let n6142: ZN = zsel_n(n136, n6038, n4973);
    let n6143: ZN = zsel_n(n136, n6039, n5015);
    let n6144: ZN = zsel_n(n136, n6040, n6033);
    let n6145: ZN = zsel_n(n1424, n4973, n6142);
    let n6146: ZN = zsel_n(n1424, n6021, n6143);
    let n6147: ZN = zsel_n(n1424, n6026, n6144);
    let n6148: ZB = zb_and(n5095, n5641);
    let n6149: ZN = zsel_n(n1556, r_c285, n6145);
    let n6150: ZB = zb_or(r_c293, n137);
    let n6151: ZN = zsel_n(n1556, r_c368, n6146);
    let n6152: ZN = zsel_n(n1556, r_c369, n6147);
    let n6153: ZB = zb_or(n1556, n6148);
    let n6154: ZB = zb_and(n6065, n6153);
    let n6155: ZB = zb_and(n6066, n6153);
    let n6156: ZB = zb_and(n6070, n6155);
    let n6157: ZB = zb_and(n6069, n6155);
    let n6158: ZB = zb_or(n6156, n6157);
    let n6159: ZB = zb_and(n6075, n6158);
    let n6160: ZB = zb_and(n6076, n6158);
    let n6161: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6151);
    let n6162: ZB = zb_or(n6159, n6160);
    let n6163: ZN = zsel_n(n6065, n6151, n6161);
    let n6164: ZB = zb_or(n6154, n6162);
    let n6165: ZN = zsel_n(n136, n6094, n5105);
    let n6166: ZN = zsel_n(n136, n6095, n6091);
    let n6167: ZN = zsel_n(n1424, n6021, n6165);
    let n6168: ZN = zsel_n(n1424, n6026, n6166);
    let n6169: ZB = zb_and(n5095, n5689);
    let n6170: ZN = zsel_n(n1556, r_c368, n6167);
    let n6171: ZN = zsel_n(n1556, r_c369, n6168);
    let n6172: ZB = zb_or(n1556, n6169);
    let n6173: ZB = zb_and(n6065, n6172);
    let n6174: ZB = zb_and(n6066, n6172);
    let n6175: ZB = zb_and(n6070, n6174);
    let n6176: ZB = zb_and(n6069, n6174);
    let n6177: ZB = zb_or(n6175, n6176);
    let n6178: ZB = zb_and(n6075, n6177);
    let n6179: ZB = zb_and(n6076, n6177);
    let n6180: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6170);
    let n6181: ZB = zb_or(n6178, n6179);
    let n6182: ZN = zsel_n(n6065, n6170, n6180);
    let n6183: ZB = zb_or(n6173, n6181);
    let n6184: ZN = zsel_n(n136, n6121, n5156);
    let n6185: ZN = zsel_n(n136, n6122, n6118);
    let n6186: ZN = zsel_n(n1424, n6021, n6184);
    let n6187: ZN = zsel_n(n1424, n6026, n6185);
    let n6188: ZB = zb_and(n5095, n5737);
    let n6189: ZN = zsel_n(n1556, r_c368, n6186);
    let n6190: ZN = zsel_n(n1556, r_c369, n6187);
    let n6191: ZB = zb_or(n1556, n6188);
    let n6192: ZB = zb_and(n6065, n6191);
    let n6193: ZB = zb_and(n6066, n6191);
    let n6194: ZB = zb_and(n6070, n6193);
    let n6195: ZB = zb_and(n6069, n6193);
    let n6196: ZB = zb_or(n6194, n6195);
    let n6197: ZB = zb_and(n6075, n6196);
    let n6198: ZB = zb_and(n6076, n6196);
    let n6199: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6189);
    let n6200: ZB = zb_or(n6197, n6198);
    let n6201: ZN = zsel_n(n6065, n6189, n6199);
    let n6202: ZB = zb_or(n6192, n6200);
    let n6203: ZN = zsel_n(n5312, zn_splat(P8::from_raw(655360i32)), n1423);
    let n6204: ZN = zsel_n(n5312, zn_splat(P8::from_raw(262144i32)), r_c282);
    let n6205: ZN = zsel_n(n5312, n6041, n4972);
    let n6206: ZN = zsel_n(n5312, zn_splat(P8::from_raw(98304i32)), r_c356);
    let n6207: ZN = zsel_n(n5312, n6044, r_c357);
    let n6208: ZN = zsel_n(n5312, n6043, r_c358);
    let n6209: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), r_c359);
    let n6210: ZN = zsel_n(n5312, n5041, n5015);
    let n6211: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6033);
    let n6212: ZN = zsel_n(n1424, n1423, n6203);
    let n6213: ZN = zsel_n(n1424, n1428, n6204);
    let n6214: ZN = zsel_n(n1424, n4972, n6205);
    let n6215: ZN = zsel_n(n1424, r_c356, n6206);
    let n6216: ZN = zsel_n(n1424, r_c357, n6207);
    let n6217: ZN = zsel_n(n1424, r_c358, n6208);
    let n6218: ZN = zsel_n(n1424, r_c359, n6209);
    let n6219: ZN = zsel_n(n1424, n6021, n6210);
    let n6220: ZN = zsel_n(n1424, n6026, n6211);
    let n6221: ZB = zb_and(n5095, n5770);
    let n6222: ZN = zsel_n(n1556, n5980, n5316);
    let n6223: ZB = zsel_b(n1556, r_c41, n5317);
    let n6224: ZN = zsel_n(n1556, r_c280, n6212);
    let n6225: ZN = zsel_n(n1556, r_c282, n6213);
    let n6226: ZN = zsel_n(n1556, r_c283, n6214);
    let n6227: ZB = zb_or(r_c292, n137);
    let n6228: ZN = zsel_n(n1556, r_c356, n6215);
    let n6229: ZN = zsel_n(n1556, r_c357, n6216);
    let n6230: ZN = zsel_n(n1556, r_c358, n6217);
    let n6231: ZN = zsel_n(n1556, r_c359, n6218);
    let n6232: ZN = zsel_n(n1556, r_c368, n6219);
    let n6233: ZN = zsel_n(n1556, r_c369, n6220);
    let n6234: ZB = zb_or(n1556, n6221);
    let n6235: ZB = zn_gt(n6222, zn_splat(P8::from_raw(0i32)));
    let n6236: ZB = zn_le(n6222, zn_splat(P8::from_raw(0i32)));
    let n6237: ZB = zb_and(n6234, n6235);
    let n6238: ZB = zb_and(n6234, n6236);
    let n6239: ZB = zb_and(n6070, n6238);
    let n6240: ZB = zb_and(n6069, n6238);
    let n6241: ZB = zb_or(n6239, n6240);
    let n6242: ZB = zb_and(n6075, n6241);
    let n6243: ZB = zb_and(n6076, n6241);
    let n6244: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6232);
    let n6245: ZB = zb_or(n6242, n6243);
    let n6246: ZN = zsel_n(n6235, n6056, n6081);
    let n6247: ZN = zsel_n(n6235, n6232, n6244);
    let n6248: ZB = zb_or(n6237, n6245);
    let n6249: ZN = zsel_n(n5312, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n6250: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-131072i32)), r_c358);
    let n6251: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-327680i32)), n5105);
    let n6252: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6091);
    let n6253: ZN = zsel_n(n1424, r_c357, n6249);
    let n6254: ZN = zsel_n(n1424, r_c358, n6250);
    let n6255: ZN = zsel_n(n1424, n6021, n6251);
    let n6256: ZN = zsel_n(n1424, n6026, n6252);
    let n6257: ZB = zb_and(n5095, n5796);
    let n6258: ZN = zsel_n(n1556, r_c357, n6253);
    let n6259: ZN = zsel_n(n1556, r_c358, n6254);
    let n6260: ZN = zsel_n(n1556, r_c368, n6255);
    let n6261: ZN = zsel_n(n1556, r_c369, n6256);
    let n6262: ZB = zb_or(n1556, n6257);
    let n6263: ZB = zb_and(n6235, n6262);
    let n6264: ZB = zb_and(n6236, n6262);
    let n6265: ZB = zb_and(n6070, n6264);
    let n6266: ZB = zb_and(n6069, n6264);
    let n6267: ZB = zb_or(n6265, n6266);
    let n6268: ZB = zb_and(n6075, n6267);
    let n6269: ZB = zb_and(n6076, n6267);
    let n6270: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6260);
    let n6271: ZB = zb_or(n6268, n6269);
    let n6272: ZN = zsel_n(n6235, n6260, n6270);
    let n6273: ZB = zb_or(n6263, n6271);
    let n6274: ZN = zsel_n(n5312, zn_splat(P8::from_raw(131072i32)), r_c358);
    let n6275: ZN = zsel_n(n5312, zn_splat(P8::from_raw(327680i32)), n5156);
    let n6276: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6118);
    let n6277: ZN = zsel_n(n1424, r_c358, n6274);
    let n6278: ZN = zsel_n(n1424, n6021, n6275);
    let n6279: ZN = zsel_n(n1424, n6026, n6276);
    let n6280: ZB = zb_and(n5095, n5822);
    let n6281: ZN = zsel_n(n1556, r_c358, n6277);
    let n6282: ZN = zsel_n(n1556, r_c368, n6278);
    let n6283: ZN = zsel_n(n1556, r_c369, n6279);
    let n6284: ZB = zb_or(n1556, n6280);
    let n6285: ZB = zb_and(n6235, n6284);
    let n6286: ZB = zb_and(n6236, n6284);
    let n6287: ZB = zb_and(n6070, n6286);
    let n6288: ZB = zb_and(n6069, n6286);
    let n6289: ZB = zb_or(n6287, n6288);
    let n6290: ZB = zb_and(n6075, n6289);
    let n6291: ZB = zb_and(n6076, n6289);
    let n6292: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6282);
    let n6293: ZB = zb_or(n6290, n6291);
    let n6294: ZN = zsel_n(n6235, n6282, n6292);
    let n6295: ZB = zb_or(n6285, n6293);
    let n6296: ZN = zsel_n(n5312, zn_splat(P8::from_raw(69510i32)), r_c356);
    let n6297: ZN = zsel_n(n5312, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n6298: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), r_c358);
    let n6299: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-98304i32)), r_c359);
    let n6300: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n5015);
    let n6301: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-327680i32)), n6033);
    let n6302: ZN = zsel_n(n1424, r_c356, n6296);
    let n6303: ZN = zsel_n(n1424, r_c357, n6297);
    let n6304: ZN = zsel_n(n1424, r_c358, n6298);
    let n6305: ZN = zsel_n(n1424, r_c359, n6299);
    let n6306: ZN = zsel_n(n1424, n6021, n6300);
    let n6307: ZN = zsel_n(n1424, n6026, n6301);
    let n6308: ZB = zb_and(n5095, n5846);
    let n6309: ZN = zsel_n(n1556, r_c356, n6302);
    let n6310: ZN = zsel_n(n1556, r_c357, n6303);
    let n6311: ZN = zsel_n(n1556, r_c358, n6304);
    let n6312: ZN = zsel_n(n1556, r_c359, n6305);
    let n6313: ZN = zsel_n(n1556, r_c368, n6306);
    let n6314: ZN = zsel_n(n1556, r_c369, n6307);
    let n6315: ZB = zb_or(n1556, n6308);
    let n6316: ZB = zb_and(n6235, n6315);
    let n6317: ZB = zb_and(n6236, n6315);
    let n6318: ZB = zb_and(n6070, n6317);
    let n6319: ZB = zb_and(n6069, n6317);
    let n6320: ZB = zb_or(n6318, n6319);
    let n6321: ZB = zb_and(n6075, n6320);
    let n6322: ZB = zb_and(n6076, n6320);
    let n6323: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6313);
    let n6324: ZB = zb_or(n6321, n6322);
    let n6325: ZN = zsel_n(n6235, n6313, n6323);
    let n6326: ZB = zb_or(n6316, n6324);
    let n6327: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n5105);
    let n6328: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n6091);
    let n6329: ZN = zsel_n(n1424, n6021, n6327);
    let n6330: ZN = zsel_n(n1424, n6026, n6328);
    let n6331: ZN = zsel_n(n1556, r_c368, n6329);
    let n6332: ZN = zsel_n(n1556, r_c369, n6330);
    let n6333: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6331);
    let n6334: ZN = zsel_n(n6235, n6331, n6333);
    let n6335: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n5156);
    let n6336: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n6118);
    let n6337: ZN = zsel_n(n1424, n6021, n6335);
    let n6338: ZN = zsel_n(n1424, n6026, n6336);
    let n6339: ZN = zsel_n(n1556, r_c368, n6337);
    let n6340: ZN = zsel_n(n1556, r_c369, n6338);
    let n6341: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6339);
    let n6342: ZN = zsel_n(n6235, n6339, n6341);
    let n6343: ZN = zsel_n(n5312, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n6344: ZN = zsel_n(n5312, zn_splat(P8::from_raw(327680i32)), n6033);
    let n6345: ZN = zsel_n(n1424, r_c359, n6343);
    let n6346: ZN = zsel_n(n1424, n6026, n6344);
    let n6347: ZN = zsel_n(n1556, r_c359, n6345);
    let n6348: ZN = zsel_n(n1556, r_c369, n6346);
    let n6349: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n6091);
    let n6350: ZN = zsel_n(n1424, n6026, n6349);
    let n6351: ZN = zsel_n(n1556, r_c369, n6350);
    let n6352: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n6118);
    let n6353: ZN = zsel_n(n1424, n6026, n6352);
    let n6354: ZN = zsel_n(n1556, r_c369, n6353);
    let n6355: ZN = zsel_n(n5312, n5041, n6143);
    let n6356: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6144);
    let n6357: ZN = zsel_n(n1424, n6021, n6355);
    let n6358: ZN = zsel_n(n1424, n6026, n6356);
    let n6359: ZB = zb_and(n5095, n5884);
    let n6360: ZN = zsel_n(n1556, r_c368, n6357);
    let n6361: ZN = zsel_n(n1556, r_c369, n6358);
    let n6362: ZB = zb_or(n1556, n6359);
    let n6363: ZB = zb_and(n6235, n6362);
    let n6364: ZB = zb_and(n6236, n6362);
    let n6365: ZB = zb_and(n6070, n6364);
    let n6366: ZB = zb_and(n6069, n6364);
    let n6367: ZB = zb_or(n6365, n6366);
    let n6368: ZB = zb_and(n6075, n6367);
    let n6369: ZB = zb_and(n6076, n6367);
    let n6370: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6360);
    let n6371: ZB = zb_or(n6368, n6369);
    let n6372: ZN = zsel_n(n6235, n6360, n6370);
    let n6373: ZB = zb_or(n6363, n6371);
    let n6374: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-327680i32)), n6165);
    let n6375: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6166);
    let n6376: ZN = zsel_n(n1424, n6021, n6374);
    let n6377: ZN = zsel_n(n1424, n6026, n6375);
    let n6378: ZB = zb_and(n5095, n5910);
    let n6379: ZN = zsel_n(n1556, r_c368, n6376);
    let n6380: ZN = zsel_n(n1556, r_c369, n6377);
    let n6381: ZB = zb_or(n1556, n6378);
    let n6382: ZB = zb_and(n6235, n6381);
    let n6383: ZB = zb_and(n6236, n6381);
    let n6384: ZB = zb_and(n6070, n6383);
    let n6385: ZB = zb_and(n6069, n6383);
    let n6386: ZB = zb_or(n6384, n6385);
    let n6387: ZB = zb_and(n6075, n6386);
    let n6388: ZB = zb_and(n6076, n6386);
    let n6389: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6379);
    let n6390: ZB = zb_or(n6387, n6388);
    let n6391: ZN = zsel_n(n6235, n6379, n6389);
    let n6392: ZB = zb_or(n6382, n6390);
    let n6393: ZN = zsel_n(n5312, zn_splat(P8::from_raw(327680i32)), n6184);
    let n6394: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6185);
    let n6395: ZN = zsel_n(n1424, n6021, n6393);
    let n6396: ZN = zsel_n(n1424, n6026, n6394);
    let n6397: ZB = zb_and(n5095, n5936);
    let n6398: ZN = zsel_n(n1556, r_c368, n6395);
    let n6399: ZN = zsel_n(n1556, r_c369, n6396);
    let n6400: ZB = zb_or(n1556, n6397);
    let n6401: ZB = zb_and(n6235, n6400);
    let n6402: ZB = zb_and(n6236, n6400);
    let n6403: ZB = zb_and(n6070, n6402);
    let n6404: ZB = zb_and(n6069, n6402);
    let n6405: ZB = zb_or(n6403, n6404);
    let n6406: ZB = zb_and(n6075, n6405);
    let n6407: ZB = zb_and(n6076, n6405);
    let n6408: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6398);
    let n6409: ZB = zb_or(n6406, n6407);
    let n6410: ZN = zsel_n(n6235, n6398, n6408);
    let n6411: ZB = zb_or(n6401, n6409);
    let n6412: ZN = zsel_n(n5312, zn_splat(P8::from_raw(0i32)), n6143);
    let n6413: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-327680i32)), n6144);
    let n6414: ZN = zsel_n(n1424, n6021, n6412);
    let n6415: ZN = zsel_n(n1424, n6026, n6413);
    let n6416: ZB = zb_and(n5095, n5960);
    let n6417: ZN = zsel_n(n1556, r_c368, n6414);
    let n6418: ZN = zsel_n(n1556, r_c369, n6415);
    let n6419: ZB = zb_or(n1556, n6416);
    let n6420: ZB = zb_and(n6235, n6419);
    let n6421: ZB = zb_and(n6236, n6419);
    let n6422: ZB = zb_and(n6070, n6421);
    let n6423: ZB = zb_and(n6069, n6421);
    let n6424: ZB = zb_or(n6422, n6423);
    let n6425: ZB = zb_and(n6075, n6424);
    let n6426: ZB = zb_and(n6076, n6424);
    let n6427: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6417);
    let n6428: ZB = zb_or(n6425, n6426);
    let n6429: ZN = zsel_n(n6235, n6417, n6427);
    let n6430: ZB = zb_or(n6420, n6428);
    let n6431: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n6165);
    let n6432: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n6166);
    let n6433: ZN = zsel_n(n1424, n6021, n6431);
    let n6434: ZN = zsel_n(n1424, n6026, n6432);
    let n6435: ZN = zsel_n(n1556, r_c368, n6433);
    let n6436: ZN = zsel_n(n1556, r_c369, n6434);
    let n6437: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6435);
    let n6438: ZN = zsel_n(n6235, n6435, n6437);
    let n6439: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n6184);
    let n6440: ZN = zsel_n(n5312, zn_splat(P8::from_raw(-231700i32)), n6185);
    let n6441: ZN = zsel_n(n1424, n6021, n6439);
    let n6442: ZN = zsel_n(n1424, n6026, n6440);
    let n6443: ZN = zsel_n(n1556, r_c368, n6441);
    let n6444: ZN = zsel_n(n1556, r_c369, n6442);
    let n6445: ZN = zsel_n(n6075, zn_splat(P8::from_raw(0i32)), n6443);
    let n6446: ZN = zsel_n(n6235, n6443, n6445);
    let n6447: ZN = zsel_n(n5312, zn_splat(P8::from_raw(327680i32)), n6144);
    let n6448: ZN = zsel_n(n1424, n6026, n6447);
    let n6449: ZN = zsel_n(n1556, r_c369, n6448);
    let n6450: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n6166);
    let n6451: ZN = zsel_n(n1424, n6026, n6450);
    let n6452: ZN = zsel_n(n1556, r_c369, n6451);
    let n6453: ZN = zsel_n(n5312, zn_splat(P8::from_raw(231700i32)), n6185);
    let n6454: ZN = zsel_n(n1424, n6026, n6453);
    let n6455: ZN = zsel_n(n1556, r_c369, n6454);
    let n6458: ZW = zw_bits_n(r_c39);
    let n6459: ZW = zw_mix1(zw_splat(11400714819323198485u64), n6458, 39u64);
    let n6460: ZW = zw_mix2(zw_splat(11562461410679940143u64), n6458, 39u64);
    let n6461: ZW = zw_bits_n(n116);
    let n6462: ZW = zw_mix1(n6459, n6461, 84u64);
    let n6463: ZW = zw_mix2(n6460, n6461, 84u64);
    let n6464: ZW = zw_bits_n(n216);
    let n6465: ZW = zw_mix1(n6462, n6464, 85u64);
    let n6466: ZW = zw_mix2(n6463, n6464, 85u64);
    let n6467: ZW = zw_bits_n(n215);
    let n6468: ZW = zw_mix1(n6465, n6467, 86u64);
    let n6469: ZW = zw_mix2(n6466, n6467, 86u64);
    let n6470: ZW = zw_bits_n(r_c87);
    let n6471: ZW = zw_mix1(n6468, n6470, 87u64);
    let n6472: ZW = zw_mix2(n6469, n6470, 87u64);
    let n6473: ZW = zw_bits_n(n752);
    let n6474: ZW = zw_mix1(n6471, n6473, 301u64);
    let n6475: ZW = zw_mix2(n6472, n6473, 301u64);
    let n6476: ZW = zw_bits_n(n475);
    let n6477: ZW = zw_mix1(n6474, n6476, 367u64);
    let n6478: ZW = zw_mix2(n6475, n6476, 367u64);
    let n6479: ZW = zw_bits_n(n753);
    let n6480: ZW = zw_mix1(n6477, n6479, 368u64);
    let n6481: ZW = zw_mix2(n6478, n6479, 368u64);
    let n6482: ZW = zw_bits_n(r_c20);
    let n6483: ZW = zw_mix1(n6480, n6482, 20u64);
    let n6484: ZW = zw_mix2(n6481, n6482, 20u64);
    let n6485: ZW = zw_bits_b(r_c41);
    let n6486: ZW = zw_mix1(n6483, n6485, 41u64);
    let n6487: ZW = zw_mix2(n6484, n6485, 41u64);
    let n6488: ZW = zw_bits_n(n1423);
    let n6489: ZW = zw_mix1(n6486, n6488, 281u64);
    let n6490: ZW = zw_mix2(n6487, n6488, 281u64);
    let n6491: ZW = zw_bits_n(n1551);
    let n6492: ZW = zw_mix1(n6489, n6491, 283u64);
    let n6493: ZW = zw_mix2(n6490, n6491, 283u64);
    let n6494: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n6495: ZW = zw_mix1(n6492, zw_splat(n6494), 284u64);
    let n6496: ZW = zw_mix2(n6493, zw_splat(n6494), 284u64);
    let n6497: ZW = zw_bits_n(n1421);
    let n6498: ZW = zw_mix1(n6495, n6497, 286u64);
    let n6499: ZW = zw_mix2(n6496, n6497, 286u64);
    let n6500: u64 = false as u64;
    let n6501: ZW = zw_mix1(n6498, zw_splat(n6500), 293u64);
    let n6502: ZW = zw_mix2(n6499, zw_splat(n6500), 293u64);
    let n6503: ZW = zw_mix1(n6501, zw_splat(n6500), 294u64);
    let n6504: ZW = zw_mix2(n6502, zw_splat(n6500), 294u64);
    let n6505: ZW = zw_bits_n(n1573);
    let n6506: ZW = zw_mix1(n6503, n6505, 300u64);
    let n6507: ZW = zw_mix2(n6504, n6505, 300u64);
    let n6508: ZW = zw_bits_n(r_c356);
    let n6509: ZW = zw_mix1(n6506, n6508, 357u64);
    let n6510: ZW = zw_mix2(n6507, n6508, 357u64);
    let n6511: ZW = zw_bits_n(r_c357);
    let n6512: ZW = zw_mix1(n6509, n6511, 358u64);
    let n6513: ZW = zw_mix2(n6510, n6511, 358u64);
    let n6514: ZW = zw_bits_n(r_c358);
    let n6515: ZW = zw_mix1(n6512, n6514, 359u64);
    let n6516: ZW = zw_mix2(n6513, n6514, 359u64);
    let n6517: ZW = zw_bits_n(r_c359);
    let n6518: ZW = zw_mix1(n6515, n6517, 360u64);
    let n6519: ZW = zw_mix2(n6516, n6517, 360u64);
    let n6520: ZW = zw_bits_b(n1552);
    let n6521: ZW = zw_mix1(n6518, n6520, 361u64);
    let n6522: ZW = zw_mix2(n6519, n6520, 361u64);
    let n6523: ZW = zw_bits_n(n1574);
    let n6524: ZW = zw_mix1(n6521, n6523, 369u64);
    let n6525: ZW = zw_mix2(n6522, n6523, 369u64);
    let n6526: ZW = zw_bits_n(n1554);
    let n6527: ZW = zw_mix1(n6524, n6526, 370u64);
    let n6528: ZW = zw_mix2(n6525, n6526, 370u64);
    let n6529: ZW = zw_bits_b(n1621);
    let n6530: ZW = zw_mix1(n6518, n6529, 361u64);
    let n6531: ZW = zw_mix2(n6519, n6529, 361u64);
    let n6532: ZW = zw_bits_n(n1633);
    let n6533: ZW = zw_mix1(n6530, n6532, 369u64);
    let n6534: ZW = zw_mix2(n6531, n6532, 369u64);
    let n6535: ZW = zw_bits_n(n1623);
    let n6536: ZW = zw_mix1(n6533, n6535, 370u64);
    let n6537: ZW = zw_mix2(n6534, n6535, 370u64);
    let n6538: ZW = zw_bits_b(n1677);
    let n6539: ZW = zw_mix1(n6518, n6538, 361u64);
    let n6540: ZW = zw_mix2(n6519, n6538, 361u64);
    let n6541: ZW = zw_bits_n(n1689);
    let n6542: ZW = zw_mix1(n6539, n6541, 369u64);
    let n6543: ZW = zw_mix2(n6540, n6541, 369u64);
    let n6544: ZW = zw_bits_n(n1679);
    let n6545: ZW = zw_mix1(n6542, n6544, 370u64);
    let n6546: ZW = zw_mix2(n6543, n6544, 370u64);
    let n6547: ZW = zw_bits_n(n1720);
    let n6548: ZW = zw_mix1(n6495, n6547, 286u64);
    let n6549: ZW = zw_mix2(n6496, n6547, 286u64);
    let n6550: ZW = zw_mix1(n6548, zw_splat(n6500), 293u64);
    let n6551: ZW = zw_mix2(n6549, zw_splat(n6500), 293u64);
    let n6552: u64 = true as u64;
    let n6553: ZW = zw_mix1(n6550, zw_splat(n6552), 294u64);
    let n6554: ZW = zw_mix2(n6551, zw_splat(n6552), 294u64);
    let n6555: ZW = zw_mix1(n6553, n6505, 300u64);
    let n6556: ZW = zw_mix2(n6554, n6505, 300u64);
    let n6557: ZW = zw_mix1(n6555, n6508, 357u64);
    let n6558: ZW = zw_mix2(n6556, n6508, 357u64);
    let n6559: ZW = zw_mix1(n6557, n6511, 358u64);
    let n6560: ZW = zw_mix2(n6558, n6511, 358u64);
    let n6561: ZW = zw_mix1(n6559, n6514, 359u64);
    let n6562: ZW = zw_mix2(n6560, n6514, 359u64);
    let n6563: ZW = zw_mix1(n6561, n6517, 360u64);
    let n6564: ZW = zw_mix2(n6562, n6517, 360u64);
    let n6565: ZW = zw_mix1(n6563, n6520, 361u64);
    let n6566: ZW = zw_mix2(n6564, n6520, 361u64);
    let n6567: ZW = zw_bits_n(n1732);
    let n6568: ZW = zw_mix1(n6565, n6567, 369u64);
    let n6569: ZW = zw_mix2(n6566, n6567, 369u64);
    let n6570: ZW = zw_bits_n(n1722);
    let n6571: ZW = zw_mix1(n6568, n6570, 370u64);
    let n6572: ZW = zw_mix2(n6569, n6570, 370u64);
    let n6573: ZW = zw_mix1(n6563, n6529, 361u64);
    let n6574: ZW = zw_mix2(n6564, n6529, 361u64);
    let n6575: ZW = zw_bits_n(n1773);
    let n6576: ZW = zw_mix1(n6573, n6575, 369u64);
    let n6577: ZW = zw_mix2(n6574, n6575, 369u64);
    let n6578: ZW = zw_bits_n(n1763);
    let n6579: ZW = zw_mix1(n6576, n6578, 370u64);
    let n6580: ZW = zw_mix2(n6577, n6578, 370u64);
    let n6581: ZW = zw_mix1(n6563, n6538, 361u64);
    let n6582: ZW = zw_mix2(n6564, n6538, 361u64);
    let n6583: ZW = zw_bits_n(n1814);
    let n6584: ZW = zw_mix1(n6581, n6583, 369u64);
    let n6585: ZW = zw_mix2(n6582, n6583, 369u64);
    let n6586: ZW = zw_bits_n(n1804);
    let n6587: ZW = zw_mix1(n6584, n6586, 370u64);
    let n6588: ZW = zw_mix2(n6585, n6586, 370u64);
    let n6589: ZW = zw_bits_n(n1841);
    let n6590: ZW = zw_mix1(n6480, n6589, 20u64);
    let n6591: ZW = zw_mix2(n6481, n6589, 20u64);
    let n6592: ZW = zw_bits_b(n1842);
    let n6593: ZW = zw_mix1(n6590, n6592, 41u64);
    let n6594: ZW = zw_mix2(n6591, n6592, 41u64);
    let n6595: ZW = zw_bits_n(n1843);
    let n6596: ZW = zw_mix1(n6593, n6595, 281u64);
    let n6597: ZW = zw_mix2(n6594, n6595, 281u64);
    let n6598: ZW = zw_bits_n(n1844);
    let n6599: ZW = zw_mix1(n6596, n6598, 283u64);
    let n6600: ZW = zw_mix2(n6597, n6598, 283u64);
    let n6601: ZW = zw_bits_n(n1845);
    let n6602: ZW = zw_mix1(n6599, n6601, 284u64);
    let n6603: ZW = zw_mix2(n6600, n6601, 284u64);
    let n6604: ZW = zw_mix1(n6602, n6497, 286u64);
    let n6605: ZW = zw_mix2(n6603, n6497, 286u64);
    let n6606: ZW = zw_mix1(n6604, zw_splat(n6552), 293u64);
    let n6607: ZW = zw_mix2(n6605, zw_splat(n6552), 293u64);
    let n6608: ZW = zw_mix1(n6606, zw_splat(n6500), 294u64);
    let n6609: ZW = zw_mix2(n6607, zw_splat(n6500), 294u64);
    let n6610: ZW = zw_bits_n(n1864);
    let n6611: ZW = zw_mix1(n6608, n6610, 300u64);
    let n6612: ZW = zw_mix2(n6609, n6610, 300u64);
    let n6613: ZW = zw_bits_n(n1846);
    let n6614: ZW = zw_mix1(n6611, n6613, 357u64);
    let n6615: ZW = zw_mix2(n6612, n6613, 357u64);
    let n6616: ZW = zw_bits_n(n1847);
    let n6617: ZW = zw_mix1(n6614, n6616, 358u64);
    let n6618: ZW = zw_mix2(n6615, n6616, 358u64);
    let n6619: ZW = zw_bits_n(n1848);
    let n6620: ZW = zw_mix1(n6617, n6619, 359u64);
    let n6621: ZW = zw_mix2(n6618, n6619, 359u64);
    let n6622: ZW = zw_bits_n(n1849);
    let n6623: ZW = zw_mix1(n6620, n6622, 360u64);
    let n6624: ZW = zw_mix2(n6621, n6622, 360u64);
    let n6625: ZW = zw_mix1(n6623, n6520, 361u64);
    let n6626: ZW = zw_mix2(n6624, n6520, 361u64);
    let n6627: ZW = zw_bits_n(n1865);
    let n6628: ZW = zw_mix1(n6625, n6627, 369u64);
    let n6629: ZW = zw_mix2(n6626, n6627, 369u64);
    let n6630: ZW = zw_bits_n(n1851);
    let n6631: ZW = zw_mix1(n6628, n6630, 370u64);
    let n6632: ZW = zw_mix2(n6629, n6630, 370u64);
    let n6633: ZW = zw_bits_n(n1874);
    let n6634: ZW = zw_mix1(n6614, n6633, 358u64);
    let n6635: ZW = zw_mix2(n6615, n6633, 358u64);
    let n6636: ZW = zw_bits_n(n1875);
    let n6637: ZW = zw_mix1(n6634, n6636, 359u64);
    let n6638: ZW = zw_mix2(n6635, n6636, 359u64);
    let n6639: ZW = zw_mix1(n6637, n6622, 360u64);
    let n6640: ZW = zw_mix2(n6638, n6622, 360u64);
    let n6641: ZW = zw_mix1(n6639, n6529, 361u64);
    let n6642: ZW = zw_mix2(n6640, n6529, 361u64);
    let n6643: ZW = zw_bits_n(n1888);
    let n6644: ZW = zw_mix1(n6641, n6643, 369u64);
    let n6645: ZW = zw_mix2(n6642, n6643, 369u64);
    let n6646: ZW = zw_bits_n(n1877);
    let n6647: ZW = zw_mix1(n6644, n6646, 370u64);
    let n6648: ZW = zw_mix2(n6645, n6646, 370u64);
    let n6649: ZW = zw_bits_n(n1896);
    let n6650: ZW = zw_mix1(n6634, n6649, 359u64);
    let n6651: ZW = zw_mix2(n6635, n6649, 359u64);
    let n6652: ZW = zw_mix1(n6650, n6622, 360u64);
    let n6653: ZW = zw_mix2(n6651, n6622, 360u64);
    let n6654: ZW = zw_mix1(n6652, n6538, 361u64);
    let n6655: ZW = zw_mix2(n6653, n6538, 361u64);
    let n6656: ZW = zw_bits_n(n1909);
    let n6657: ZW = zw_mix1(n6654, n6656, 369u64);
    let n6658: ZW = zw_mix2(n6655, n6656, 369u64);
    let n6659: ZW = zw_bits_n(n1898);
    let n6660: ZW = zw_mix1(n6657, n6659, 370u64);
    let n6661: ZW = zw_mix2(n6658, n6659, 370u64);
    let n6662: ZW = zw_bits_n(n1919);
    let n6663: ZW = zw_mix1(n6611, n6662, 357u64);
    let n6664: ZW = zw_mix2(n6612, n6662, 357u64);
    let n6665: ZW = zw_bits_n(n1920);
    let n6666: ZW = zw_mix1(n6663, n6665, 358u64);
    let n6667: ZW = zw_mix2(n6664, n6665, 358u64);
    let n6668: ZW = zw_bits_n(n1921);
    let n6669: ZW = zw_mix1(n6666, n6668, 359u64);
    let n6670: ZW = zw_mix2(n6667, n6668, 359u64);
    let n6671: ZW = zw_bits_n(n1922);
    let n6672: ZW = zw_mix1(n6669, n6671, 360u64);
    let n6673: ZW = zw_mix2(n6670, n6671, 360u64);
    let n6674: ZW = zw_mix1(n6672, n6520, 361u64);
    let n6675: ZW = zw_mix2(n6673, n6520, 361u64);
    let n6676: ZW = zw_bits_n(n1935);
    let n6677: ZW = zw_mix1(n6674, n6676, 369u64);
    let n6678: ZW = zw_mix2(n6675, n6676, 369u64);
    let n6679: ZW = zw_bits_n(n1924);
    let n6680: ZW = zw_mix1(n6677, n6679, 370u64);
    let n6681: ZW = zw_mix2(n6678, n6679, 370u64);
    let n6682: ZW = zw_mix1(n6663, n6633, 358u64);
    let n6683: ZW = zw_mix2(n6664, n6633, 358u64);
    let n6684: ZW = zw_mix1(n6682, n6636, 359u64);
    let n6685: ZW = zw_mix2(n6683, n6636, 359u64);
    let n6686: ZW = zw_mix1(n6684, n6671, 360u64);
    let n6687: ZW = zw_mix2(n6685, n6671, 360u64);
    let n6688: ZW = zw_mix1(n6686, n6529, 361u64);
    let n6689: ZW = zw_mix2(n6687, n6529, 361u64);
    let n6690: ZW = zw_bits_n(n1942);
    let n6691: ZW = zw_mix1(n6688, n6690, 369u64);
    let n6692: ZW = zw_mix2(n6689, n6690, 369u64);
    let n6693: ZW = zw_bits_n(n1940);
    let n6694: ZW = zw_mix1(n6691, n6693, 370u64);
    let n6695: ZW = zw_mix2(n6692, n6693, 370u64);
    let n6696: ZW = zw_mix1(n6682, n6649, 359u64);
    let n6697: ZW = zw_mix2(n6683, n6649, 359u64);
    let n6698: ZW = zw_mix1(n6696, n6671, 360u64);
    let n6699: ZW = zw_mix2(n6697, n6671, 360u64);
    let n6700: ZW = zw_mix1(n6698, n6538, 361u64);
    let n6701: ZW = zw_mix2(n6699, n6538, 361u64);
    let n6702: ZW = zw_bits_n(n1948);
    let n6703: ZW = zw_mix1(n6700, n6702, 369u64);
    let n6704: ZW = zw_mix2(n6701, n6702, 369u64);
    let n6705: ZW = zw_bits_n(n1946);
    let n6706: ZW = zw_mix1(n6703, n6705, 370u64);
    let n6707: ZW = zw_mix2(n6704, n6705, 370u64);
    let n6708: ZW = zw_bits_n(n1951);
    let n6709: ZW = zw_mix1(n6669, n6708, 360u64);
    let n6710: ZW = zw_mix2(n6670, n6708, 360u64);
    let n6711: ZW = zw_mix1(n6709, n6520, 361u64);
    let n6712: ZW = zw_mix2(n6710, n6520, 361u64);
    let n6713: ZW = zw_mix1(n6711, n6676, 369u64);
    let n6714: ZW = zw_mix2(n6712, n6676, 369u64);
    let n6715: ZW = zw_bits_n(n1952);
    let n6716: ZW = zw_mix1(n6713, n6715, 370u64);
    let n6717: ZW = zw_mix2(n6714, n6715, 370u64);
    let n6718: ZW = zw_mix1(n6684, n6708, 360u64);
    let n6719: ZW = zw_mix2(n6685, n6708, 360u64);
    let n6720: ZW = zw_mix1(n6718, n6529, 361u64);
    let n6721: ZW = zw_mix2(n6719, n6529, 361u64);
    let n6722: ZW = zw_mix1(n6720, n6690, 369u64);
    let n6723: ZW = zw_mix2(n6721, n6690, 369u64);
    let n6724: ZW = zw_bits_n(n1954);
    let n6725: ZW = zw_mix1(n6722, n6724, 370u64);
    let n6726: ZW = zw_mix2(n6723, n6724, 370u64);
    let n6727: ZW = zw_mix1(n6696, n6708, 360u64);
    let n6728: ZW = zw_mix2(n6697, n6708, 360u64);
    let n6729: ZW = zw_mix1(n6727, n6538, 361u64);
    let n6730: ZW = zw_mix2(n6728, n6538, 361u64);
    let n6731: ZW = zw_mix1(n6729, n6702, 369u64);
    let n6732: ZW = zw_mix2(n6730, n6702, 369u64);
    let n6733: ZW = zw_bits_n(n1956);
    let n6734: ZW = zw_mix1(n6731, n6733, 370u64);
    let n6735: ZW = zw_mix2(n6732, n6733, 370u64);
    let n6736: ZW = zw_mix1(n6602, n6547, 286u64);
    let n6737: ZW = zw_mix2(n6603, n6547, 286u64);
    let n6738: ZW = zw_mix1(n6736, zw_splat(n6552), 293u64);
    let n6739: ZW = zw_mix2(n6737, zw_splat(n6552), 293u64);
    let n6740: ZW = zw_mix1(n6738, zw_splat(n6552), 294u64);
    let n6741: ZW = zw_mix2(n6739, zw_splat(n6552), 294u64);
    let n6742: ZW = zw_mix1(n6740, n6610, 300u64);
    let n6743: ZW = zw_mix2(n6741, n6610, 300u64);
    let n6744: ZW = zw_mix1(n6742, n6613, 357u64);
    let n6745: ZW = zw_mix2(n6743, n6613, 357u64);
    let n6746: ZW = zw_mix1(n6744, n6616, 358u64);
    let n6747: ZW = zw_mix2(n6745, n6616, 358u64);
    let n6748: ZW = zw_mix1(n6746, n6619, 359u64);
    let n6749: ZW = zw_mix2(n6747, n6619, 359u64);
    let n6750: ZW = zw_mix1(n6748, n6622, 360u64);
    let n6751: ZW = zw_mix2(n6749, n6622, 360u64);
    let n6752: ZW = zw_mix1(n6750, n6520, 361u64);
    let n6753: ZW = zw_mix2(n6751, n6520, 361u64);
    let n6754: ZW = zw_bits_n(n1986);
    let n6755: ZW = zw_mix1(n6752, n6754, 369u64);
    let n6756: ZW = zw_mix2(n6753, n6754, 369u64);
    let n6757: ZW = zw_bits_n(n1975);
    let n6758: ZW = zw_mix1(n6755, n6757, 370u64);
    let n6759: ZW = zw_mix2(n6756, n6757, 370u64);
    let n6760: ZW = zw_mix1(n6744, n6633, 358u64);
    let n6761: ZW = zw_mix2(n6745, n6633, 358u64);
    let n6762: ZW = zw_mix1(n6760, n6636, 359u64);
    let n6763: ZW = zw_mix2(n6761, n6636, 359u64);
    let n6764: ZW = zw_mix1(n6762, n6622, 360u64);
    let n6765: ZW = zw_mix2(n6763, n6622, 360u64);
    let n6766: ZW = zw_mix1(n6764, n6529, 361u64);
    let n6767: ZW = zw_mix2(n6765, n6529, 361u64);
    let n6768: ZW = zw_bits_n(n2005);
    let n6769: ZW = zw_mix1(n6766, n6768, 369u64);
    let n6770: ZW = zw_mix2(n6767, n6768, 369u64);
    let n6771: ZW = zw_bits_n(n1994);
    let n6772: ZW = zw_mix1(n6769, n6771, 370u64);
    let n6773: ZW = zw_mix2(n6770, n6771, 370u64);
    let n6774: ZW = zw_mix1(n6760, n6649, 359u64);
    let n6775: ZW = zw_mix2(n6761, n6649, 359u64);
    let n6776: ZW = zw_mix1(n6774, n6622, 360u64);
    let n6777: ZW = zw_mix2(n6775, n6622, 360u64);
    let n6778: ZW = zw_mix1(n6776, n6538, 361u64);
    let n6779: ZW = zw_mix2(n6777, n6538, 361u64);
    let n6780: ZW = zw_bits_n(n2024);
    let n6781: ZW = zw_mix1(n6778, n6780, 369u64);
    let n6782: ZW = zw_mix2(n6779, n6780, 369u64);
    let n6783: ZW = zw_bits_n(n2013);
    let n6784: ZW = zw_mix1(n6781, n6783, 370u64);
    let n6785: ZW = zw_mix2(n6782, n6783, 370u64);
    let n6786: ZW = zw_mix1(n6742, n6662, 357u64);
    let n6787: ZW = zw_mix2(n6743, n6662, 357u64);
    let n6788: ZW = zw_mix1(n6786, n6665, 358u64);
    let n6789: ZW = zw_mix2(n6787, n6665, 358u64);
    let n6790: ZW = zw_mix1(n6788, n6668, 359u64);
    let n6791: ZW = zw_mix2(n6789, n6668, 359u64);
    let n6792: ZW = zw_mix1(n6790, n6671, 360u64);
    let n6793: ZW = zw_mix2(n6791, n6671, 360u64);
    let n6794: ZW = zw_mix1(n6792, n6520, 361u64);
    let n6795: ZW = zw_mix2(n6793, n6520, 361u64);
    let n6796: ZW = zw_bits_n(n2041);
    let n6797: ZW = zw_mix1(n6794, n6796, 369u64);
    let n6798: ZW = zw_mix2(n6795, n6796, 369u64);
    let n6799: ZW = zw_bits_n(n2030);
    let n6800: ZW = zw_mix1(n6797, n6799, 370u64);
    let n6801: ZW = zw_mix2(n6798, n6799, 370u64);
    let n6802: ZW = zw_mix1(n6786, n6633, 358u64);
    let n6803: ZW = zw_mix2(n6787, n6633, 358u64);
    let n6804: ZW = zw_mix1(n6802, n6636, 359u64);
    let n6805: ZW = zw_mix2(n6803, n6636, 359u64);
    let n6806: ZW = zw_mix1(n6804, n6671, 360u64);
    let n6807: ZW = zw_mix2(n6805, n6671, 360u64);
    let n6808: ZW = zw_mix1(n6806, n6529, 361u64);
    let n6809: ZW = zw_mix2(n6807, n6529, 361u64);
    let n6810: ZW = zw_bits_n(n2048);
    let n6811: ZW = zw_mix1(n6808, n6810, 369u64);
    let n6812: ZW = zw_mix2(n6809, n6810, 369u64);
    let n6813: ZW = zw_bits_n(n2046);
    let n6814: ZW = zw_mix1(n6811, n6813, 370u64);
    let n6815: ZW = zw_mix2(n6812, n6813, 370u64);
    let n6816: ZW = zw_mix1(n6802, n6649, 359u64);
    let n6817: ZW = zw_mix2(n6803, n6649, 359u64);
    let n6818: ZW = zw_mix1(n6816, n6671, 360u64);
    let n6819: ZW = zw_mix2(n6817, n6671, 360u64);
    let n6820: ZW = zw_mix1(n6818, n6538, 361u64);
    let n6821: ZW = zw_mix2(n6819, n6538, 361u64);
    let n6822: ZW = zw_bits_n(n2054);
    let n6823: ZW = zw_mix1(n6820, n6822, 369u64);
    let n6824: ZW = zw_mix2(n6821, n6822, 369u64);
    let n6825: ZW = zw_bits_n(n2052);
    let n6826: ZW = zw_mix1(n6823, n6825, 370u64);
    let n6827: ZW = zw_mix2(n6824, n6825, 370u64);
    let n6828: ZW = zw_mix1(n6790, n6708, 360u64);
    let n6829: ZW = zw_mix2(n6791, n6708, 360u64);
    let n6830: ZW = zw_mix1(n6828, n6520, 361u64);
    let n6831: ZW = zw_mix2(n6829, n6520, 361u64);
    let n6832: ZW = zw_mix1(n6830, n6796, 369u64);
    let n6833: ZW = zw_mix2(n6831, n6796, 369u64);
    let n6834: ZW = zw_bits_n(n2056);
    let n6835: ZW = zw_mix1(n6832, n6834, 370u64);
    let n6836: ZW = zw_mix2(n6833, n6834, 370u64);
    let n6837: ZW = zw_mix1(n6804, n6708, 360u64);
    let n6838: ZW = zw_mix2(n6805, n6708, 360u64);
    let n6839: ZW = zw_mix1(n6837, n6529, 361u64);
    let n6840: ZW = zw_mix2(n6838, n6529, 361u64);
    let n6841: ZW = zw_mix1(n6839, n6810, 369u64);
    let n6842: ZW = zw_mix2(n6840, n6810, 369u64);
    let n6843: ZW = zw_bits_n(n2058);
    let n6844: ZW = zw_mix1(n6841, n6843, 370u64);
    let n6845: ZW = zw_mix2(n6842, n6843, 370u64);
    let n6846: ZW = zw_mix1(n6816, n6708, 360u64);
    let n6847: ZW = zw_mix2(n6817, n6708, 360u64);
    let n6848: ZW = zw_mix1(n6846, n6538, 361u64);
    let n6849: ZW = zw_mix2(n6847, n6538, 361u64);
    let n6850: ZW = zw_mix1(n6848, n6822, 369u64);
    let n6851: ZW = zw_mix2(n6849, n6822, 369u64);
    let n6852: ZW = zw_bits_n(n2060);
    let n6853: ZW = zw_mix1(n6850, n6852, 370u64);
    let n6854: ZW = zw_mix2(n6851, n6852, 370u64);
    let n6855: ZW = zw_mix1(zw_splat(11400714819323198485u64), n6461, 84u64);
    let n6856: ZW = zw_mix2(zw_splat(11562461410679940143u64), n6461, 84u64);
    let n6857: ZW = zw_mix1(n6855, n6464, 85u64);
    let n6858: ZW = zw_mix2(n6856, n6464, 85u64);
    let n6859: ZW = zw_mix1(n6857, n6467, 86u64);
    let n6860: ZW = zw_mix2(n6858, n6467, 86u64);
    let n6861: ZW = zw_bits_n(n3027);
    let n6862: ZW = zw_mix1(n6859, n6861, 87u64);
    let n6863: ZW = zw_mix2(n6860, n6861, 87u64);
    let n6864: ZW = zw_mix1(n6862, n6482, 20u64);
    let n6865: ZW = zw_mix2(n6863, n6482, 20u64);
    let n6866: ZW = zw_mix1(n6864, n6485, 41u64);
    let n6867: ZW = zw_mix2(n6865, n6485, 41u64);
    let n6868: ZW = zw_mix1(n6862, n6589, 20u64);
    let n6869: ZW = zw_mix2(n6863, n6589, 20u64);
    let n6870: ZW = zw_mix1(n6868, n6592, 41u64);
    let n6871: ZW = zw_mix2(n6869, n6592, 41u64);
    let n6872: ZW = zw_bits_n(n4956);
    let n6873: ZW = zw_mix1(n6859, n6872, 87u64);
    let n6874: ZW = zw_mix2(n6860, n6872, 87u64);
    let n6875: ZW = zw_mix1(n6873, n6482, 20u64);
    let n6876: ZW = zw_mix2(n6874, n6482, 20u64);
    let n6877: ZW = zw_mix1(n6875, n6485, 41u64);
    let n6878: ZW = zw_mix2(n6876, n6485, 41u64);
    let n6879: ZW = zw_bits_n(n5316);
    let n6880: ZW = zw_mix1(n6873, n6879, 20u64);
    let n6881: ZW = zw_mix2(n6874, n6879, 20u64);
    let n6882: ZW = zw_bits_b(n5317);
    let n6883: ZW = zw_mix1(n6880, n6882, 41u64);
    let n6884: ZW = zw_mix2(n6881, n6882, 41u64);
    let n6885: ZW = zw_mix1(n6859, n6482, 20u64);
    let n6886: ZW = zw_mix2(n6860, n6482, 20u64);
    let n6887: ZW = zw_bits_b(n5506);
    let n6888: ZW = zw_mix1(n6885, n6887, 38u64);
    let n6889: ZW = zw_mix2(n6886, n6887, 38u64);
    let n6890: ZW = zw_bits_n(n5504);
    let n6891: ZW = zw_mix1(n6888, n6890, 39u64);
    let n6892: ZW = zw_mix2(n6889, n6890, 39u64);
    let n6893: ZW = zw_bits_n(n5503);
    let n6894: ZW = zw_mix1(n6891, n6893, 87u64);
    let n6895: ZW = zw_mix2(n6892, n6893, 87u64);
    let n6896: ZW = zw_bits_b(n5555);
    let n6897: ZW = zw_mix1(n6885, n6896, 38u64);
    let n6898: ZW = zw_mix2(n6886, n6896, 38u64);
    let n6899: ZW = zw_bits_n(n5553);
    let n6900: ZW = zw_mix1(n6897, n6899, 39u64);
    let n6901: ZW = zw_mix2(n6898, n6899, 39u64);
    let n6902: ZW = zw_bits_n(n5552);
    let n6903: ZW = zw_mix1(n6900, n6902, 87u64);
    let n6904: ZW = zw_mix2(n6901, n6902, 87u64);
    let n6905: ZW = zw_bits_b(n5604);
    let n6906: ZW = zw_mix1(n6885, n6905, 38u64);
    let n6907: ZW = zw_mix2(n6886, n6905, 38u64);
    let n6908: ZW = zw_bits_n(n5602);
    let n6909: ZW = zw_mix1(n6906, n6908, 39u64);
    let n6910: ZW = zw_mix2(n6907, n6908, 39u64);
    let n6911: ZW = zw_bits_n(n5601);
    let n6912: ZW = zw_mix1(n6909, n6911, 87u64);
    let n6913: ZW = zw_mix2(n6910, n6911, 87u64);
    let n6914: ZW = zw_bits_b(n5652);
    let n6915: ZW = zw_mix1(n6885, n6914, 38u64);
    let n6916: ZW = zw_mix2(n6886, n6914, 38u64);
    let n6917: ZW = zw_bits_n(n5650);
    let n6918: ZW = zw_mix1(n6915, n6917, 39u64);
    let n6919: ZW = zw_mix2(n6916, n6917, 39u64);
    let n6920: ZW = zw_bits_n(n5649);
    let n6921: ZW = zw_mix1(n6918, n6920, 87u64);
    let n6922: ZW = zw_mix2(n6919, n6920, 87u64);
    let n6923: ZW = zw_bits_b(n5700);
    let n6924: ZW = zw_mix1(n6885, n6923, 38u64);
    let n6925: ZW = zw_mix2(n6886, n6923, 38u64);
    let n6926: ZW = zw_bits_n(n5698);
    let n6927: ZW = zw_mix1(n6924, n6926, 39u64);
    let n6928: ZW = zw_mix2(n6925, n6926, 39u64);
    let n6929: ZW = zw_bits_n(n5697);
    let n6930: ZW = zw_mix1(n6927, n6929, 87u64);
    let n6931: ZW = zw_mix2(n6928, n6929, 87u64);
    let n6932: ZW = zw_bits_b(n5748);
    let n6933: ZW = zw_mix1(n6885, n6932, 38u64);
    let n6934: ZW = zw_mix2(n6886, n6932, 38u64);
    let n6935: ZW = zw_bits_n(n5746);
    let n6936: ZW = zw_mix1(n6933, n6935, 39u64);
    let n6937: ZW = zw_mix2(n6934, n6935, 39u64);
    let n6938: ZW = zw_bits_n(n5745);
    let n6939: ZW = zw_mix1(n6936, n6938, 87u64);
    let n6940: ZW = zw_mix2(n6937, n6938, 87u64);
    let n6941: ZW = zw_bits_n(n5780);
    let n6942: ZW = zw_mix1(n6859, n6941, 20u64);
    let n6943: ZW = zw_mix2(n6860, n6941, 20u64);
    let n6944: ZW = zw_bits_b(n5782);
    let n6945: ZW = zw_mix1(n6942, n6944, 38u64);
    let n6946: ZW = zw_mix2(n6943, n6944, 38u64);
    let n6947: ZW = zw_bits_n(n5779);
    let n6948: ZW = zw_mix1(n6945, n6947, 39u64);
    let n6949: ZW = zw_mix2(n6946, n6947, 39u64);
    let n6950: ZW = zw_bits_n(n5778);
    let n6951: ZW = zw_mix1(n6948, n6950, 87u64);
    let n6952: ZW = zw_mix2(n6949, n6950, 87u64);
    let n6953: ZW = zw_bits_n(n5806);
    let n6954: ZW = zw_mix1(n6859, n6953, 20u64);
    let n6955: ZW = zw_mix2(n6860, n6953, 20u64);
    let n6956: ZW = zw_bits_b(n5808);
    let n6957: ZW = zw_mix1(n6954, n6956, 38u64);
    let n6958: ZW = zw_mix2(n6955, n6956, 38u64);
    let n6959: ZW = zw_bits_n(n5805);
    let n6960: ZW = zw_mix1(n6957, n6959, 39u64);
    let n6961: ZW = zw_mix2(n6958, n6959, 39u64);
    let n6962: ZW = zw_bits_n(n5804);
    let n6963: ZW = zw_mix1(n6960, n6962, 87u64);
    let n6964: ZW = zw_mix2(n6961, n6962, 87u64);
    let n6965: ZW = zw_bits_n(n5832);
    let n6966: ZW = zw_mix1(n6859, n6965, 20u64);
    let n6967: ZW = zw_mix2(n6860, n6965, 20u64);
    let n6968: ZW = zw_bits_b(n5834);
    let n6969: ZW = zw_mix1(n6966, n6968, 38u64);
    let n6970: ZW = zw_mix2(n6967, n6968, 38u64);
    let n6971: ZW = zw_bits_n(n5831);
    let n6972: ZW = zw_mix1(n6969, n6971, 39u64);
    let n6973: ZW = zw_mix2(n6970, n6971, 39u64);
    let n6974: ZW = zw_bits_n(n5830);
    let n6975: ZW = zw_mix1(n6972, n6974, 87u64);
    let n6976: ZW = zw_mix2(n6973, n6974, 87u64);
    let n6977: ZW = zw_bits_n(n5856);
    let n6978: ZW = zw_mix1(n6859, n6977, 20u64);
    let n6979: ZW = zw_mix2(n6860, n6977, 20u64);
    let n6980: ZW = zw_bits_b(n5858);
    let n6981: ZW = zw_mix1(n6978, n6980, 38u64);
    let n6982: ZW = zw_mix2(n6979, n6980, 38u64);
    let n6983: ZW = zw_bits_n(n5855);
    let n6984: ZW = zw_mix1(n6981, n6983, 39u64);
    let n6985: ZW = zw_mix2(n6982, n6983, 39u64);
    let n6986: ZW = zw_bits_n(n5854);
    let n6987: ZW = zw_mix1(n6984, n6986, 87u64);
    let n6988: ZW = zw_mix2(n6985, n6986, 87u64);
    let n6989: ZW = zw_bits_n(n5894);
    let n6990: ZW = zw_mix1(n6859, n6989, 20u64);
    let n6991: ZW = zw_mix2(n6860, n6989, 20u64);
    let n6992: ZW = zw_bits_b(n5896);
    let n6993: ZW = zw_mix1(n6990, n6992, 38u64);
    let n6994: ZW = zw_mix2(n6991, n6992, 38u64);
    let n6995: ZW = zw_bits_n(n5893);
    let n6996: ZW = zw_mix1(n6993, n6995, 39u64);
    let n6997: ZW = zw_mix2(n6994, n6995, 39u64);
    let n6998: ZW = zw_bits_n(n5892);
    let n6999: ZW = zw_mix1(n6996, n6998, 87u64);
    let n7000: ZW = zw_mix2(n6997, n6998, 87u64);
    let n7001: ZW = zw_bits_n(n5920);
    let n7002: ZW = zw_mix1(n6859, n7001, 20u64);
    let n7003: ZW = zw_mix2(n6860, n7001, 20u64);
    let n7004: ZW = zw_bits_b(n5922);
    let n7005: ZW = zw_mix1(n7002, n7004, 38u64);
    let n7006: ZW = zw_mix2(n7003, n7004, 38u64);
    let n7007: ZW = zw_bits_n(n5919);
    let n7008: ZW = zw_mix1(n7005, n7007, 39u64);
    let n7009: ZW = zw_mix2(n7006, n7007, 39u64);
    let n7010: ZW = zw_bits_n(n5918);
    let n7011: ZW = zw_mix1(n7008, n7010, 87u64);
    let n7012: ZW = zw_mix2(n7009, n7010, 87u64);
    let n7013: ZW = zw_bits_n(n5946);
    let n7014: ZW = zw_mix1(n6859, n7013, 20u64);
    let n7015: ZW = zw_mix2(n6860, n7013, 20u64);
    let n7016: ZW = zw_bits_b(n5948);
    let n7017: ZW = zw_mix1(n7014, n7016, 38u64);
    let n7018: ZW = zw_mix2(n7015, n7016, 38u64);
    let n7019: ZW = zw_bits_n(n5945);
    let n7020: ZW = zw_mix1(n7017, n7019, 39u64);
    let n7021: ZW = zw_mix2(n7018, n7019, 39u64);
    let n7022: ZW = zw_bits_n(n5944);
    let n7023: ZW = zw_mix1(n7020, n7022, 87u64);
    let n7024: ZW = zw_mix2(n7021, n7022, 87u64);
    let n7025: ZW = zw_bits_n(n5970);
    let n7026: ZW = zw_mix1(n6859, n7025, 20u64);
    let n7027: ZW = zw_mix2(n6860, n7025, 20u64);
    let n7028: ZW = zw_bits_b(n5972);
    let n7029: ZW = zw_mix1(n7026, n7028, 38u64);
    let n7030: ZW = zw_mix2(n7027, n7028, 38u64);
    let n7031: ZW = zw_bits_n(n5969);
    let n7032: ZW = zw_mix1(n7029, n7031, 39u64);
    let n7033: ZW = zw_mix2(n7030, n7031, 39u64);
    let n7034: ZW = zw_bits_n(n5968);
    let n7035: ZW = zw_mix1(n7032, n7034, 87u64);
    let n7036: ZW = zw_mix2(n7033, n7034, 87u64);
    let n7037: ZW = zw_bits_n(n6057);
    let n7038: ZW = zw_mix1(n6471, n7037, 300u64);
    let n7039: ZW = zw_mix2(n6472, n7037, 300u64);
    let n7040: ZW = zw_bits_n(n6059);
    let n7041: ZW = zw_mix1(n7038, n7040, 366u64);
    let n7042: ZW = zw_mix2(n7039, n7040, 366u64);
    let n7043: ZW = zw_bits_n(n6060);
    let n7044: ZW = zw_mix1(n7041, n7043, 367u64);
    let n7045: ZW = zw_mix2(n7042, n7043, 367u64);
    let n7046: ZW = zw_bits_n(n6049);
    let n7047: ZW = zw_mix1(n7044, n7046, 20u64);
    let n7048: ZW = zw_mix2(n7045, n7046, 20u64);
    let n7049: ZW = zw_mix1(n7047, n6485, 41u64);
    let n7050: ZW = zw_mix2(n7048, n6485, 41u64);
    let n7051: ZW = zw_bits_n(n6050);
    let n7052: ZW = zw_mix1(n7049, n7051, 280u64);
    let n7053: ZW = zw_mix2(n7050, n7051, 280u64);
    let n7054: ZW = zw_bits_n(n6051);
    let n7055: ZW = zw_mix1(n7052, n7054, 282u64);
    let n7056: ZW = zw_mix2(n7053, n7054, 282u64);
    let n7057: ZW = zw_bits_n(n6052);
    let n7058: ZW = zw_mix1(n7055, n7057, 283u64);
    let n7059: ZW = zw_mix2(n7056, n7057, 283u64);
    let n7060: ZW = zw_bits_n(n6053);
    let n7061: ZW = zw_mix1(n7058, n7060, 285u64);
    let n7062: ZW = zw_mix2(n7059, n7060, 285u64);
    let n7063: ZW = zw_bits_b(n6054);
    let n7064: ZW = zw_mix1(n7061, n7063, 292u64);
    let n7065: ZW = zw_mix2(n7062, n7063, 292u64);
    let n7066: ZW = zw_bits_b(n6055);
    let n7067: ZW = zw_mix1(n7064, n7066, 293u64);
    let n7068: ZW = zw_mix2(n7065, n7066, 293u64);
    let n7069: ZW = zw_bits_n(n6084);
    let n7070: ZW = zw_mix1(n7067, n7069, 299u64);
    let n7071: ZW = zw_mix2(n7068, n7069, 299u64);
    let n7072: ZW = zw_mix1(n7070, n6508, 356u64);
    let n7073: ZW = zw_mix2(n7071, n6508, 356u64);
    let n7074: ZW = zw_mix1(n7072, n6511, 357u64);
    let n7075: ZW = zw_mix2(n7073, n6511, 357u64);
    let n7076: ZW = zw_mix1(n7074, n6514, 358u64);
    let n7077: ZW = zw_mix2(n7075, n6514, 358u64);
    let n7078: ZW = zw_mix1(n7076, n6517, 359u64);
    let n7079: ZW = zw_mix2(n7077, n6517, 359u64);
    let n7080: ZW = zw_bits_b(n6058);
    let n7081: ZW = zw_mix1(n7078, n7080, 360u64);
    let n7082: ZW = zw_mix2(n7079, n7080, 360u64);
    let n7083: ZW = zw_bits_n(n6085);
    let n7084: ZW = zw_mix1(n7081, n7083, 368u64);
    let n7085: ZW = zw_mix2(n7082, n7083, 368u64);
    let n7086: ZW = zw_bits_n(n6062);
    let n7087: ZW = zw_mix1(n7084, n7086, 369u64);
    let n7088: ZW = zw_mix2(n7085, n7086, 369u64);
    let n7089: ZW = zw_bits_b(n6100);
    let n7090: ZW = zw_mix1(n7078, n7089, 360u64);
    let n7091: ZW = zw_mix2(n7079, n7089, 360u64);
    let n7092: ZW = zw_bits_n(n6113);
    let n7093: ZW = zw_mix1(n7090, n7092, 368u64);
    let n7094: ZW = zw_mix2(n7091, n7092, 368u64);
    let n7095: ZW = zw_bits_n(n6102);
    let n7096: ZW = zw_mix1(n7093, n7095, 369u64);
    let n7097: ZW = zw_mix2(n7094, n7095, 369u64);
    let n7098: ZW = zw_bits_b(n6127);
    let n7099: ZW = zw_mix1(n7078, n7098, 360u64);
    let n7100: ZW = zw_mix2(n7079, n7098, 360u64);
    let n7101: ZW = zw_bits_n(n6140);
    let n7102: ZW = zw_mix1(n7099, n7101, 368u64);
    let n7103: ZW = zw_mix2(n7100, n7101, 368u64);
    let n7104: ZW = zw_bits_n(n6129);
    let n7105: ZW = zw_mix1(n7102, n7104, 369u64);
    let n7106: ZW = zw_mix2(n7103, n7104, 369u64);
    let n7107: ZW = zw_bits_n(n6149);
    let n7108: ZW = zw_mix1(n7058, n7107, 285u64);
    let n7109: ZW = zw_mix2(n7059, n7107, 285u64);
    let n7110: ZW = zw_mix1(n7108, n7063, 292u64);
    let n7111: ZW = zw_mix2(n7109, n7063, 292u64);
    let n7112: ZW = zw_bits_b(n6150);
    let n7113: ZW = zw_mix1(n7110, n7112, 293u64);
    let n7114: ZW = zw_mix2(n7111, n7112, 293u64);
    let n7115: ZW = zw_mix1(n7113, n7069, 299u64);
    let n7116: ZW = zw_mix2(n7114, n7069, 299u64);
    let n7117: ZW = zw_mix1(n7115, n6508, 356u64);
    let n7118: ZW = zw_mix2(n7116, n6508, 356u64);
    let n7119: ZW = zw_mix1(n7117, n6511, 357u64);
    let n7120: ZW = zw_mix2(n7118, n6511, 357u64);
    let n7121: ZW = zw_mix1(n7119, n6514, 358u64);
    let n7122: ZW = zw_mix2(n7120, n6514, 358u64);
    let n7123: ZW = zw_mix1(n7121, n6517, 359u64);
    let n7124: ZW = zw_mix2(n7122, n6517, 359u64);
    let n7125: ZW = zw_mix1(n7123, n7080, 360u64);
    let n7126: ZW = zw_mix2(n7124, n7080, 360u64);
    let n7127: ZW = zw_bits_n(n6163);
    let n7128: ZW = zw_mix1(n7125, n7127, 368u64);
    let n7129: ZW = zw_mix2(n7126, n7127, 368u64);
    let n7130: ZW = zw_bits_n(n6152);
    let n7131: ZW = zw_mix1(n7128, n7130, 369u64);
    let n7132: ZW = zw_mix2(n7129, n7130, 369u64);
    let n7133: ZW = zw_mix1(n7123, n7089, 360u64);
    let n7134: ZW = zw_mix2(n7124, n7089, 360u64);
    let n7135: ZW = zw_bits_n(n6182);
    let n7136: ZW = zw_mix1(n7133, n7135, 368u64);
    let n7137: ZW = zw_mix2(n7134, n7135, 368u64);
    let n7138: ZW = zw_bits_n(n6171);
    let n7139: ZW = zw_mix1(n7136, n7138, 369u64);
    let n7140: ZW = zw_mix2(n7137, n7138, 369u64);
    let n7141: ZW = zw_mix1(n7123, n7098, 360u64);
    let n7142: ZW = zw_mix2(n7124, n7098, 360u64);
    let n7143: ZW = zw_bits_n(n6201);
    let n7144: ZW = zw_mix1(n7141, n7143, 368u64);
    let n7145: ZW = zw_mix2(n7142, n7143, 368u64);
    let n7146: ZW = zw_bits_n(n6190);
    let n7147: ZW = zw_mix1(n7144, n7146, 369u64);
    let n7148: ZW = zw_mix2(n7145, n7146, 369u64);
    let n7149: ZW = zw_bits_n(n6222);
    let n7150: ZW = zw_mix1(n7044, n7149, 20u64);
    let n7151: ZW = zw_mix2(n7045, n7149, 20u64);
    let n7152: ZW = zw_bits_b(n6223);
    let n7153: ZW = zw_mix1(n7150, n7152, 41u64);
    let n7154: ZW = zw_mix2(n7151, n7152, 41u64);
    let n7155: ZW = zw_bits_n(n6224);
    let n7156: ZW = zw_mix1(n7153, n7155, 280u64);
    let n7157: ZW = zw_mix2(n7154, n7155, 280u64);
    let n7158: ZW = zw_bits_n(n6225);
    let n7159: ZW = zw_mix1(n7156, n7158, 282u64);
    let n7160: ZW = zw_mix2(n7157, n7158, 282u64);
    let n7161: ZW = zw_bits_n(n6226);
    let n7162: ZW = zw_mix1(n7159, n7161, 283u64);
    let n7163: ZW = zw_mix2(n7160, n7161, 283u64);
    let n7164: ZW = zw_mix1(n7162, n7060, 285u64);
    let n7165: ZW = zw_mix2(n7163, n7060, 285u64);
    let n7166: ZW = zw_bits_b(n6227);
    let n7167: ZW = zw_mix1(n7164, n7166, 292u64);
    let n7168: ZW = zw_mix2(n7165, n7166, 292u64);
    let n7169: ZW = zw_mix1(n7167, n7066, 293u64);
    let n7170: ZW = zw_mix2(n7168, n7066, 293u64);
    let n7171: ZW = zw_bits_n(n6246);
    let n7172: ZW = zw_mix1(n7169, n7171, 299u64);
    let n7173: ZW = zw_mix2(n7170, n7171, 299u64);
    let n7174: ZW = zw_bits_n(n6228);
    let n7175: ZW = zw_mix1(n7172, n7174, 356u64);
    let n7176: ZW = zw_mix2(n7173, n7174, 356u64);
    let n7177: ZW = zw_bits_n(n6229);
    let n7178: ZW = zw_mix1(n7175, n7177, 357u64);
    let n7179: ZW = zw_mix2(n7176, n7177, 357u64);
    let n7180: ZW = zw_bits_n(n6230);
    let n7181: ZW = zw_mix1(n7178, n7180, 358u64);
    let n7182: ZW = zw_mix2(n7179, n7180, 358u64);
    let n7183: ZW = zw_bits_n(n6231);
    let n7184: ZW = zw_mix1(n7181, n7183, 359u64);
    let n7185: ZW = zw_mix2(n7182, n7183, 359u64);
    let n7186: ZW = zw_mix1(n7184, n7080, 360u64);
    let n7187: ZW = zw_mix2(n7185, n7080, 360u64);
    let n7188: ZW = zw_bits_n(n6247);
    let n7189: ZW = zw_mix1(n7186, n7188, 368u64);
    let n7190: ZW = zw_mix2(n7187, n7188, 368u64);
    let n7191: ZW = zw_bits_n(n6233);
    let n7192: ZW = zw_mix1(n7189, n7191, 369u64);
    let n7193: ZW = zw_mix2(n7190, n7191, 369u64);
    let n7194: ZW = zw_bits_n(n6258);
    let n7195: ZW = zw_mix1(n7175, n7194, 357u64);
    let n7196: ZW = zw_mix2(n7176, n7194, 357u64);
    let n7197: ZW = zw_bits_n(n6259);
    let n7198: ZW = zw_mix1(n7195, n7197, 358u64);
    let n7199: ZW = zw_mix2(n7196, n7197, 358u64);
    let n7200: ZW = zw_mix1(n7198, n7183, 359u64);
    let n7201: ZW = zw_mix2(n7199, n7183, 359u64);
    let n7202: ZW = zw_mix1(n7200, n7089, 360u64);
    let n7203: ZW = zw_mix2(n7201, n7089, 360u64);
    let n7204: ZW = zw_bits_n(n6272);
    let n7205: ZW = zw_mix1(n7202, n7204, 368u64);
    let n7206: ZW = zw_mix2(n7203, n7204, 368u64);
    let n7207: ZW = zw_bits_n(n6261);
    let n7208: ZW = zw_mix1(n7205, n7207, 369u64);
    let n7209: ZW = zw_mix2(n7206, n7207, 369u64);
    let n7210: ZW = zw_bits_n(n6281);
    let n7211: ZW = zw_mix1(n7195, n7210, 358u64);
    let n7212: ZW = zw_mix2(n7196, n7210, 358u64);
    let n7213: ZW = zw_mix1(n7211, n7183, 359u64);
    let n7214: ZW = zw_mix2(n7212, n7183, 359u64);
    let n7215: ZW = zw_mix1(n7213, n7098, 360u64);
    let n7216: ZW = zw_mix2(n7214, n7098, 360u64);
    let n7217: ZW = zw_bits_n(n6294);
    let n7218: ZW = zw_mix1(n7215, n7217, 368u64);
    let n7219: ZW = zw_mix2(n7216, n7217, 368u64);
    let n7220: ZW = zw_bits_n(n6283);
    let n7221: ZW = zw_mix1(n7218, n7220, 369u64);
    let n7222: ZW = zw_mix2(n7219, n7220, 369u64);
    let n7223: ZW = zw_bits_n(n6309);
    let n7224: ZW = zw_mix1(n7172, n7223, 356u64);
    let n7225: ZW = zw_mix2(n7173, n7223, 356u64);
    let n7226: ZW = zw_bits_n(n6310);
    let n7227: ZW = zw_mix1(n7224, n7226, 357u64);
    let n7228: ZW = zw_mix2(n7225, n7226, 357u64);
    let n7229: ZW = zw_bits_n(n6311);
    let n7230: ZW = zw_mix1(n7227, n7229, 358u64);
    let n7231: ZW = zw_mix2(n7228, n7229, 358u64);
    let n7232: ZW = zw_bits_n(n6312);
    let n7233: ZW = zw_mix1(n7230, n7232, 359u64);
    let n7234: ZW = zw_mix2(n7231, n7232, 359u64);
    let n7235: ZW = zw_mix1(n7233, n7080, 360u64);
    let n7236: ZW = zw_mix2(n7234, n7080, 360u64);
    let n7237: ZW = zw_bits_n(n6325);
    let n7238: ZW = zw_mix1(n7235, n7237, 368u64);
    let n7239: ZW = zw_mix2(n7236, n7237, 368u64);
    let n7240: ZW = zw_bits_n(n6314);
    let n7241: ZW = zw_mix1(n7238, n7240, 369u64);
    let n7242: ZW = zw_mix2(n7239, n7240, 369u64);
    let n7243: ZW = zw_mix1(n7224, n7194, 357u64);
    let n7244: ZW = zw_mix2(n7225, n7194, 357u64);
    let n7245: ZW = zw_mix1(n7243, n7197, 358u64);
    let n7246: ZW = zw_mix2(n7244, n7197, 358u64);
    let n7247: ZW = zw_mix1(n7245, n7232, 359u64);
    let n7248: ZW = zw_mix2(n7246, n7232, 359u64);
    let n7249: ZW = zw_mix1(n7247, n7089, 360u64);
    let n7250: ZW = zw_mix2(n7248, n7089, 360u64);
    let n7251: ZW = zw_bits_n(n6334);
    let n7252: ZW = zw_mix1(n7249, n7251, 368u64);
    let n7253: ZW = zw_mix2(n7250, n7251, 368u64);
    let n7254: ZW = zw_bits_n(n6332);
    let n7255: ZW = zw_mix1(n7252, n7254, 369u64);
    let n7256: ZW = zw_mix2(n7253, n7254, 369u64);
    let n7257: ZW = zw_mix1(n7243, n7210, 358u64);
    let n7258: ZW = zw_mix2(n7244, n7210, 358u64);
    let n7259: ZW = zw_mix1(n7257, n7232, 359u64);
    let n7260: ZW = zw_mix2(n7258, n7232, 359u64);
    let n7261: ZW = zw_mix1(n7259, n7098, 360u64);
    let n7262: ZW = zw_mix2(n7260, n7098, 360u64);
    let n7263: ZW = zw_bits_n(n6342);
    let n7264: ZW = zw_mix1(n7261, n7263, 368u64);
    let n7265: ZW = zw_mix2(n7262, n7263, 368u64);
    let n7266: ZW = zw_bits_n(n6340);
    let n7267: ZW = zw_mix1(n7264, n7266, 369u64);
    let n7268: ZW = zw_mix2(n7265, n7266, 369u64);
    let n7269: ZW = zw_bits_n(n6347);
    let n7270: ZW = zw_mix1(n7230, n7269, 359u64);
    let n7271: ZW = zw_mix2(n7231, n7269, 359u64);
    let n7272: ZW = zw_mix1(n7270, n7080, 360u64);
    let n7273: ZW = zw_mix2(n7271, n7080, 360u64);
    let n7274: ZW = zw_mix1(n7272, n7237, 368u64);
    let n7275: ZW = zw_mix2(n7273, n7237, 368u64);
    let n7276: ZW = zw_bits_n(n6348);
    let n7277: ZW = zw_mix1(n7274, n7276, 369u64);
    let n7278: ZW = zw_mix2(n7275, n7276, 369u64);
    let n7279: ZW = zw_mix1(n7245, n7269, 359u64);
    let n7280: ZW = zw_mix2(n7246, n7269, 359u64);
    let n7281: ZW = zw_mix1(n7279, n7089, 360u64);
    let n7282: ZW = zw_mix2(n7280, n7089, 360u64);
    let n7283: ZW = zw_mix1(n7281, n7251, 368u64);
    let n7284: ZW = zw_mix2(n7282, n7251, 368u64);
    let n7285: ZW = zw_bits_n(n6351);
    let n7286: ZW = zw_mix1(n7283, n7285, 369u64);
    let n7287: ZW = zw_mix2(n7284, n7285, 369u64);
    let n7288: ZW = zw_mix1(n7257, n7269, 359u64);
    let n7289: ZW = zw_mix2(n7258, n7269, 359u64);
    let n7290: ZW = zw_mix1(n7288, n7098, 360u64);
    let n7291: ZW = zw_mix2(n7289, n7098, 360u64);
    let n7292: ZW = zw_mix1(n7290, n7263, 368u64);
    let n7293: ZW = zw_mix2(n7291, n7263, 368u64);
    let n7294: ZW = zw_bits_n(n6354);
    let n7295: ZW = zw_mix1(n7292, n7294, 369u64);
    let n7296: ZW = zw_mix2(n7293, n7294, 369u64);
    let n7297: ZW = zw_mix1(n7162, n7107, 285u64);
    let n7298: ZW = zw_mix2(n7163, n7107, 285u64);
    let n7299: ZW = zw_mix1(n7297, n7166, 292u64);
    let n7300: ZW = zw_mix2(n7298, n7166, 292u64);
    let n7301: ZW = zw_mix1(n7299, n7112, 293u64);
    let n7302: ZW = zw_mix2(n7300, n7112, 293u64);
    let n7303: ZW = zw_mix1(n7301, n7171, 299u64);
    let n7304: ZW = zw_mix2(n7302, n7171, 299u64);
    let n7305: ZW = zw_mix1(n7303, n7174, 356u64);
    let n7306: ZW = zw_mix2(n7304, n7174, 356u64);
    let n7307: ZW = zw_mix1(n7305, n7177, 357u64);
    let n7308: ZW = zw_mix2(n7306, n7177, 357u64);
    let n7309: ZW = zw_mix1(n7307, n7180, 358u64);
    let n7310: ZW = zw_mix2(n7308, n7180, 358u64);
    let n7311: ZW = zw_mix1(n7309, n7183, 359u64);
    let n7312: ZW = zw_mix2(n7310, n7183, 359u64);
    let n7313: ZW = zw_mix1(n7311, n7080, 360u64);
    let n7314: ZW = zw_mix2(n7312, n7080, 360u64);
    let n7315: ZW = zw_bits_n(n6372);
    let n7316: ZW = zw_mix1(n7313, n7315, 368u64);
    let n7317: ZW = zw_mix2(n7314, n7315, 368u64);
    let n7318: ZW = zw_bits_n(n6361);
    let n7319: ZW = zw_mix1(n7316, n7318, 369u64);
    let n7320: ZW = zw_mix2(n7317, n7318, 369u64);
    let n7321: ZW = zw_mix1(n7305, n7194, 357u64);
    let n7322: ZW = zw_mix2(n7306, n7194, 357u64);
    let n7323: ZW = zw_mix1(n7321, n7197, 358u64);
    let n7324: ZW = zw_mix2(n7322, n7197, 358u64);
    let n7325: ZW = zw_mix1(n7323, n7183, 359u64);
    let n7326: ZW = zw_mix2(n7324, n7183, 359u64);
    let n7327: ZW = zw_mix1(n7325, n7089, 360u64);
    let n7328: ZW = zw_mix2(n7326, n7089, 360u64);
    let n7329: ZW = zw_bits_n(n6391);
    let n7330: ZW = zw_mix1(n7327, n7329, 368u64);
    let n7331: ZW = zw_mix2(n7328, n7329, 368u64);
    let n7332: ZW = zw_bits_n(n6380);
    let n7333: ZW = zw_mix1(n7330, n7332, 369u64);
    let n7334: ZW = zw_mix2(n7331, n7332, 369u64);
    let n7335: ZW = zw_mix1(n7321, n7210, 358u64);
    let n7336: ZW = zw_mix2(n7322, n7210, 358u64);
    let n7337: ZW = zw_mix1(n7335, n7183, 359u64);
    let n7338: ZW = zw_mix2(n7336, n7183, 359u64);
    let n7339: ZW = zw_mix1(n7337, n7098, 360u64);
    let n7340: ZW = zw_mix2(n7338, n7098, 360u64);
    let n7341: ZW = zw_bits_n(n6410);
    let n7342: ZW = zw_mix1(n7339, n7341, 368u64);
    let n7343: ZW = zw_mix2(n7340, n7341, 368u64);
    let n7344: ZW = zw_bits_n(n6399);
    let n7345: ZW = zw_mix1(n7342, n7344, 369u64);
    let n7346: ZW = zw_mix2(n7343, n7344, 369u64);
    let n7347: ZW = zw_mix1(n7303, n7223, 356u64);
    let n7348: ZW = zw_mix2(n7304, n7223, 356u64);
    let n7349: ZW = zw_mix1(n7347, n7226, 357u64);
    let n7350: ZW = zw_mix2(n7348, n7226, 357u64);
    let n7351: ZW = zw_mix1(n7349, n7229, 358u64);
    let n7352: ZW = zw_mix2(n7350, n7229, 358u64);
    let n7353: ZW = zw_mix1(n7351, n7232, 359u64);
    let n7354: ZW = zw_mix2(n7352, n7232, 359u64);
    let n7355: ZW = zw_mix1(n7353, n7080, 360u64);
    let n7356: ZW = zw_mix2(n7354, n7080, 360u64);
    let n7357: ZW = zw_bits_n(n6429);
    let n7358: ZW = zw_mix1(n7355, n7357, 368u64);
    let n7359: ZW = zw_mix2(n7356, n7357, 368u64);
    let n7360: ZW = zw_bits_n(n6418);
    let n7361: ZW = zw_mix1(n7358, n7360, 369u64);
    let n7362: ZW = zw_mix2(n7359, n7360, 369u64);
    let n7363: ZW = zw_mix1(n7347, n7194, 357u64);
    let n7364: ZW = zw_mix2(n7348, n7194, 357u64);
    let n7365: ZW = zw_mix1(n7363, n7197, 358u64);
    let n7366: ZW = zw_mix2(n7364, n7197, 358u64);
    let n7367: ZW = zw_mix1(n7365, n7232, 359u64);
    let n7368: ZW = zw_mix2(n7366, n7232, 359u64);
    let n7369: ZW = zw_mix1(n7367, n7089, 360u64);
    let n7370: ZW = zw_mix2(n7368, n7089, 360u64);
    let n7371: ZW = zw_bits_n(n6438);
    let n7372: ZW = zw_mix1(n7369, n7371, 368u64);
    let n7373: ZW = zw_mix2(n7370, n7371, 368u64);
    let n7374: ZW = zw_bits_n(n6436);
    let n7375: ZW = zw_mix1(n7372, n7374, 369u64);
    let n7376: ZW = zw_mix2(n7373, n7374, 369u64);
    let n7377: ZW = zw_mix1(n7363, n7210, 358u64);
    let n7378: ZW = zw_mix2(n7364, n7210, 358u64);
    let n7379: ZW = zw_mix1(n7377, n7232, 359u64);
    let n7380: ZW = zw_mix2(n7378, n7232, 359u64);
    let n7381: ZW = zw_mix1(n7379, n7098, 360u64);
    let n7382: ZW = zw_mix2(n7380, n7098, 360u64);
    let n7383: ZW = zw_bits_n(n6446);
    let n7384: ZW = zw_mix1(n7381, n7383, 368u64);
    let n7385: ZW = zw_mix2(n7382, n7383, 368u64);
    let n7386: ZW = zw_bits_n(n6444);
    let n7387: ZW = zw_mix1(n7384, n7386, 369u64);
    let n7388: ZW = zw_mix2(n7385, n7386, 369u64);
    let n7389: ZW = zw_mix1(n7351, n7269, 359u64);
    let n7390: ZW = zw_mix2(n7352, n7269, 359u64);
    let n7391: ZW = zw_mix1(n7389, n7080, 360u64);
    let n7392: ZW = zw_mix2(n7390, n7080, 360u64);
    let n7393: ZW = zw_mix1(n7391, n7357, 368u64);
    let n7394: ZW = zw_mix2(n7392, n7357, 368u64);
    let n7395: ZW = zw_bits_n(n6449);
    let n7396: ZW = zw_mix1(n7393, n7395, 369u64);
    let n7397: ZW = zw_mix2(n7394, n7395, 369u64);
    let n7398: ZW = zw_mix1(n7365, n7269, 359u64);
    let n7399: ZW = zw_mix2(n7366, n7269, 359u64);
    let n7400: ZW = zw_mix1(n7398, n7089, 360u64);
    let n7401: ZW = zw_mix2(n7399, n7089, 360u64);
    let n7402: ZW = zw_mix1(n7400, n7371, 368u64);
    let n7403: ZW = zw_mix2(n7401, n7371, 368u64);
    let n7404: ZW = zw_bits_n(n6452);
    let n7405: ZW = zw_mix1(n7402, n7404, 369u64);
    let n7406: ZW = zw_mix2(n7403, n7404, 369u64);
    let n7407: ZW = zw_mix1(n7377, n7269, 359u64);
    let n7408: ZW = zw_mix2(n7378, n7269, 359u64);
    let n7409: ZW = zw_mix1(n7407, n7098, 360u64);
    let n7410: ZW = zw_mix2(n7408, n7098, 360u64);
    let n7411: ZW = zw_mix1(n7409, n7383, 368u64);
    let n7412: ZW = zw_mix2(n7410, n7383, 368u64);
    let n7413: ZW = zw_bits_n(n6455);
    let n7414: ZW = zw_mix1(n7411, n7413, 369u64);
    let n7415: ZW = zw_mix2(n7412, n7413, 369u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v0_b0: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b0: u16 = ALL & zb_holds(n1572);
    let ok_v1_b1: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v1_b1: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b1: u16 = ALL & zb_holds(n1632);
    let ok_v2_b2: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v2_b2: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b2: u16 = ALL & zb_holds(n1688);
    let ok_v16_b3: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v16_b3: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b3: u16 = ALL & zb_holds(n1731);
    let ok_v17_b4: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v17_b4: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b4: u16 = ALL & zb_holds(n1772);
    let ok_v18_b5: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v18_b5: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b5: u16 = ALL & zb_holds(n1813);
    let ok_v32_b6: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v32_b6: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b6: u16 = ALL & zb_holds(n1866);
    let ok_v33_b7: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v33_b7: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b7: u16 = ALL & zb_holds(n1889);
    let ok_v34_b8: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v34_b8: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b8: u16 = ALL & zb_holds(n1910);
    let ok_v36_b9: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v36_b9: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b9: u16 = ALL & zb_holds(n1936);
    let ok_v37_b10: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v37_b10: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b10: u16 = ALL & zb_holds(n1889);
    let ok_v38_b11: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v38_b11: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b11: u16 = ALL & zb_holds(n1910);
    let ok_v40_b12: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v40_b12: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b12: u16 = ALL & zb_holds(n1936);
    let ok_v41_b13: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v41_b13: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b13: u16 = ALL & zb_holds(n1889);
    let ok_v42_b14: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v42_b14: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b14: u16 = ALL & zb_holds(n1910);
    let ok_v48_b15: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v48_b15: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b15: u16 = ALL & zb_holds(n1987);
    let ok_v49_b16: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v49_b16: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b16: u16 = ALL & zb_holds(n2006);
    let ok_v50_b17: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v50_b17: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b17: u16 = ALL & zb_holds(n2025);
    let ok_v52_b18: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v52_b18: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b18: u16 = ALL & zb_holds(n2042);
    let ok_v53_b19: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v53_b19: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b19: u16 = ALL & zb_holds(n2006);
    let ok_v54_b20: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v54_b20: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b20: u16 = ALL & zb_holds(n2025);
    let ok_v56_b21: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v56_b21: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b21: u16 = ALL & zb_holds(n2042);
    let ok_v57_b22: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v57_b22: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b22: u16 = ALL & zb_holds(n2006);
    let ok_v58_b23: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v58_b23: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b23: u16 = ALL & zb_holds(n2025);
    let ok_v0_b24: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v0_b24: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b24: u16 = ALL & zb_holds(n2731);
    let ok_v1_b25: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v1_b25: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b25: u16 = ALL & zb_holds(n2764);
    let ok_v2_b26: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v2_b26: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b26: u16 = ALL & zb_holds(n2797);
    let ok_v16_b27: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v16_b27: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b27: u16 = ALL & zb_holds(n2832);
    let ok_v17_b28: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v17_b28: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b28: u16 = ALL & zb_holds(n2867);
    let ok_v18_b29: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v18_b29: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b29: u16 = ALL & zb_holds(n2902);
    let ok_v32_b30: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v32_b30: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b30: u16 = ALL & zb_holds(n2927);
    let ok_v33_b31: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v33_b31: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b31: u16 = ALL & zb_holds(n2940);
    let ok_v34_b32: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v34_b32: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b32: u16 = ALL & zb_holds(n2953);
    let ok_v36_b33: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v36_b33: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b33: u16 = ALL & zb_holds(n2964);
    let ok_v37_b34: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v37_b34: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b34: u16 = ALL & zb_holds(n2940);
    let ok_v38_b35: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v38_b35: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b35: u16 = ALL & zb_holds(n2953);
    let ok_v40_b36: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v40_b36: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b36: u16 = ALL & zb_holds(n2964);
    let ok_v41_b37: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v41_b37: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b37: u16 = ALL & zb_holds(n2940);
    let ok_v42_b38: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v42_b38: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b38: u16 = ALL & zb_holds(n2953);
    let ok_v48_b39: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v48_b39: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b39: u16 = ALL & zb_holds(n2989);
    let ok_v49_b40: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v49_b40: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b40: u16 = ALL & zb_holds(n3002);
    let ok_v50_b41: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v50_b41: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b41: u16 = ALL & zb_holds(n3015);
    let ok_v52_b42: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v52_b42: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b42: u16 = ALL & zb_holds(n3026);
    let ok_v53_b43: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v53_b43: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b43: u16 = ALL & zb_holds(n3002);
    let ok_v54_b44: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v54_b44: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b44: u16 = ALL & zb_holds(n3015);
    let ok_v56_b45: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v56_b45: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b45: u16 = ALL & zb_holds(n3026);
    let ok_v57_b46: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v57_b46: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b46: u16 = ALL & zb_holds(n3002);
    let ok_v58_b47: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1401);
    let bd_v58_b47: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b47: u16 = ALL & zb_holds(n3015);
    let ok_v0_b48: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v0_b48: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b48: u16 = ALL & zb_holds(n137) & zb_holds(n3154);
    let ok_v1_b49: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v1_b49: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b49: u16 = ALL & zb_holds(n137) & zb_holds(n3182);
    let ok_v2_b50: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v2_b50: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b50: u16 = ALL & zb_holds(n137) & zb_holds(n3209);
    let ok_v16_b51: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v16_b51: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b51: u16 = ALL & zb_holds(n137) & zb_holds(n3238);
    let ok_v17_b52: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v17_b52: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b52: u16 = ALL & zb_holds(n137) & zb_holds(n3267);
    let ok_v18_b53: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v18_b53: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b53: u16 = ALL & zb_holds(n137) & zb_holds(n3296);
    let ok_v32_b54: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v32_b54: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b54: u16 = ALL & zb_holds(n3316);
    let ok_v33_b55: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v33_b55: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b55: u16 = ALL & zb_holds(n3323);
    let ok_v34_b56: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v34_b56: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b56: u16 = ALL & zb_holds(n3330);
    let ok_v36_b57: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v36_b57: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b57: u16 = ALL & zb_holds(n3335);
    let ok_v48_b58: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v48_b58: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b58: u16 = ALL & zb_holds(n3354);
    let ok_v49_b59: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v49_b59: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b59: u16 = ALL & zb_holds(n3361);
    let ok_v50_b60: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v50_b60: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b60: u16 = ALL & zb_holds(n3368);
    let ok_v52_b61: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3102);
    let bd_v52_b61: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b61: u16 = ALL & zb_holds(n3373);
    let ok_v0_b62: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v0_b62: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b62: u16 = ALL & zb_holds(n137) & zb_holds(n3499);
    let ok_v1_b63: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v1_b63: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b63: u16 = ALL & zb_holds(n137) & zb_holds(n3527);
    let ok_v2_b64: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v2_b64: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b64: u16 = ALL & zb_holds(n137) & zb_holds(n3554);
    let ok_v16_b65: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v16_b65: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b65: u16 = ALL & zb_holds(n137) & zb_holds(n3583);
    let ok_v17_b66: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v17_b66: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b66: u16 = ALL & zb_holds(n137) & zb_holds(n3612);
    let ok_v18_b67: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v18_b67: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b67: u16 = ALL & zb_holds(n137) & zb_holds(n3641);
    let ok_v32_b68: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v32_b68: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b68: u16 = ALL & zb_holds(n3661);
    let ok_v33_b69: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v33_b69: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b69: u16 = ALL & zb_holds(n3668);
    let ok_v34_b70: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v34_b70: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b70: u16 = ALL & zb_holds(n3675);
    let ok_v36_b71: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v36_b71: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b71: u16 = ALL & zb_holds(n3680);
    let ok_v48_b72: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v48_b72: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b72: u16 = ALL & zb_holds(n3699);
    let ok_v49_b73: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v49_b73: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b73: u16 = ALL & zb_holds(n3706);
    let ok_v50_b74: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v50_b74: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b74: u16 = ALL & zb_holds(n3713);
    let ok_v52_b75: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n3447);
    let bd_v52_b75: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b75: u16 = ALL & zb_holds(n3718);
    let ok_v0_b76: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v0_b76: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b76: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5098);
    let ok_v1_b77: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v1_b77: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b77: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5150);
    let ok_v2_b78: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v2_b78: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b78: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5201);
    let ok_v16_b79: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v16_b79: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b79: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5237);
    let ok_v17_b80: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v17_b80: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b80: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5273);
    let ok_v18_b81: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v18_b81: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b81: u16 = ALL & zb_holds(n137) & zb_holds(n5095) & zb_holds(n5309);
    let ok_v32_b82: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v32_b82: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b82: u16 = ALL & zb_holds(n5342);
    let ok_v33_b83: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v33_b83: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b83: u16 = ALL & zb_holds(n5353);
    let ok_v34_b84: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v34_b84: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b84: u16 = ALL & zb_holds(n5364);
    let ok_v36_b85: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v36_b85: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b85: u16 = ALL & zb_holds(n5373);
    let ok_v48_b86: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v48_b86: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b86: u16 = ALL & zb_holds(n5396);
    let ok_v49_b87: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v49_b87: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b87: u16 = ALL & zb_holds(n5407);
    let ok_v50_b88: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v50_b88: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b88: u16 = ALL & zb_holds(n5418);
    let ok_v52_b89: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n4958);
    let bd_v52_b89: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b89: u16 = ALL & zb_holds(n5427);
    let ok_v0_b90: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5507);
    let bd_v0_b90: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b90: u16 = ALL & zb_holds(n137) & zb_holds(n5501);
    let ok_v1_b91: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5556);
    let bd_v1_b91: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b91: u16 = ALL & zb_holds(n137) & zb_holds(n5550);
    let ok_v2_b92: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5605);
    let bd_v2_b92: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b92: u16 = ALL & zb_holds(n137) & zb_holds(n5599);
    let ok_v16_b93: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5653);
    let bd_v16_b93: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b93: u16 = ALL & zb_holds(n137) & zb_holds(n5647);
    let ok_v17_b94: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5701);
    let bd_v17_b94: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b94: u16 = ALL & zb_holds(n137) & zb_holds(n5695);
    let ok_v18_b95: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5749);
    let bd_v18_b95: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b95: u16 = ALL & zb_holds(n137) & zb_holds(n5743);
    let ok_v32_b96: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5783);
    let bd_v32_b96: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b96: u16 = ALL & zb_holds(n5788);
    let ok_v33_b97: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5809);
    let bd_v33_b97: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b97: u16 = ALL & zb_holds(n5814);
    let ok_v34_b98: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5835);
    let bd_v34_b98: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b98: u16 = ALL & zb_holds(n5840);
    let ok_v36_b99: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5859);
    let bd_v36_b99: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b99: u16 = ALL & zb_holds(n5864);
    let ok_v48_b100: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5897);
    let bd_v48_b100: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b100: u16 = ALL & zb_holds(n5902);
    let ok_v49_b101: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5923);
    let bd_v49_b101: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b101: u16 = ALL & zb_holds(n5928);
    let ok_v50_b102: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5949);
    let bd_v50_b102: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b102: u16 = ALL & zb_holds(n5954);
    let ok_v52_b103: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n5973);
    let bd_v52_b103: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b103: u16 = ALL & zb_holds(n5978);
    let ok_v0_b104: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v0_b104: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b104: u16 = ALL & zb_holds(n6086);
    let ok_v1_b105: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v1_b105: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b105: u16 = ALL & zb_holds(n6114);
    let ok_v2_b106: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v2_b106: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b106: u16 = ALL & zb_holds(n6141);
    let ok_v16_b107: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v16_b107: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b107: u16 = ALL & zb_holds(n6164);
    let ok_v17_b108: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v17_b108: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b108: u16 = ALL & zb_holds(n6183);
    let ok_v18_b109: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v18_b109: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b109: u16 = ALL & zb_holds(n6202);
    let ok_v32_b110: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v32_b110: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b110: u16 = ALL & zb_holds(n6248);
    let ok_v33_b111: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v33_b111: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b111: u16 = ALL & zb_holds(n6273);
    let ok_v34_b112: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v34_b112: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b112: u16 = ALL & zb_holds(n6295);
    let ok_v36_b113: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v36_b113: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b113: u16 = ALL & zb_holds(n6326);
    let ok_v37_b114: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v37_b114: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b114: u16 = ALL & zb_holds(n6273);
    let ok_v38_b115: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v38_b115: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b115: u16 = ALL & zb_holds(n6295);
    let ok_v40_b116: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v40_b116: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b116: u16 = ALL & zb_holds(n6326);
    let ok_v41_b117: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v41_b117: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b117: u16 = ALL & zb_holds(n6273);
    let ok_v42_b118: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v42_b118: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b118: u16 = ALL & zb_holds(n6295);
    let ok_v48_b119: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v48_b119: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b119: u16 = ALL & zb_holds(n6373);
    let ok_v49_b120: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v49_b120: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b120: u16 = ALL & zb_holds(n6392);
    let ok_v50_b121: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v50_b121: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b121: u16 = ALL & zb_holds(n6411);
    let ok_v52_b122: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v52_b122: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b122: u16 = ALL & zb_holds(n6430);
    let ok_v53_b123: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v53_b123: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b123: u16 = ALL & zb_holds(n6392);
    let ok_v54_b124: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v54_b124: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b124: u16 = ALL & zb_holds(n6411);
    let ok_v56_b125: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v56_b125: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b125: u16 = ALL & zb_holds(n6430);
    let ok_v57_b126: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v57_b126: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b126: u16 = ALL & zb_holds(n6392);
    let ok_v58_b127: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n6064);
    let bd_v58_b127: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b127: u16 = ALL & zb_holds(n6411);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c367: n475,
        c368: n753,
        c301: n752,
        c85: n216,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c367: n475,
        c368: n753,
        c301: n752,
        c85: n216,
    };
    let sh2 = KShared2 {
        c87: n3027,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh3 = KShared3 {
        c87: n3027,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh4 = KShared4 {
        c87: n4956,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh5 = KShared5 {
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh6 = KShared6 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c366: n6059,
        c367: n6060,
        c300: n6057,
        c85: n216,
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
    let mut take_6_0: u16 = 0;
    let mut take_6_1: u16 = 0;
    let mut take_6_2: u16 = 0;
    let mut take_6_3: u16 = 0;
    let mut take_6_4: u16 = 0;
    let mut take_6_5: u16 = 0;
    let mut take_6_6: u16 = 0;
    let mut take_6_7: u16 = 0;
    let mut take_6_8: u16 = 0;
    let mut take_6_9: u16 = 0;
    let mut take_6_10: u16 = 0;
    let mut take_6_11: u16 = 0;
    let mut take_6_12: u16 = 0;
    let mut take_6_13: u16 = 0;
    let mut take_6_14: u16 = 0;
    let mut take_6_15: u16 = 0;
    let mut take_6_16: u16 = 0;
    let mut take_6_17: u16 = 0;
    let mut take_6_18: u16 = 0;
    let mut take_6_19: u16 = 0;
    let mut take_6_20: u16 = 0;
    let mut take_6_21: u16 = 0;
    let mut take_6_22: u16 = 0;
    let mut take_6_23: u16 = 0;
    // 128 distinct button assignments; per outcome they fall
    // into [24, 24, 2, 2, 2, 14, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1552,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1574,
        c370: n1554,
        c300: n1573,
        h1: n6527, h2: n6528,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1621,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1633,
        c370: n1623,
        c300: n1573,
        h1: n6536, h2: n6537,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1677,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1689,
        c370: n1679,
        c300: n1573,
        h1: n6545, h2: n6546,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1552,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1732,
        c370: n1722,
        c300: n1573,
        h1: n6571, h2: n6572,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1621,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1773,
        c370: n1763,
        c300: n1573,
        h1: n6579, h2: n6580,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1677,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1814,
        c370: n1804,
        c300: n1573,
        h1: n6587, h2: n6588,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1847,
        c281: n1843,
        c359: n1848,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1865,
        c370: n1851,
        c300: n1864,
        h1: n6631, h2: n6632,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1888,
        c370: n1877,
        c300: n1864,
        h1: n6647, h2: n6648,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1909,
        c370: n1898,
        c300: n1864,
        h1: n6660, h2: n6661,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1935,
        c370: n1924,
        c300: n1864,
        h1: n6680, h2: n6681,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1942,
        c370: n1940,
        c300: n1864,
        h1: n6694, h2: n6695,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1948,
        c370: n1946,
        c300: n1864,
        h1: n6706, h2: n6707,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1935,
        c370: n1952,
        c300: n1864,
        h1: n6716, h2: n6717,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1942,
        c370: n1954,
        c300: n1864,
        h1: n6725, h2: n6726,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1948,
        c370: n1956,
        c300: n1864,
        h1: n6734, h2: n6735,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1847,
        c281: n1843,
        c359: n1848,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1986,
        c370: n1975,
        c300: n1864,
        h1: n6758, h2: n6759,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2005,
        c370: n1994,
        c300: n1864,
        h1: n6772, h2: n6773,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2024,
        c370: n2013,
        c300: n1864,
        h1: n6784, h2: n6785,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2041,
        c370: n2030,
        c300: n1864,
        h1: n6800, h2: n6801,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2048,
        c370: n2046,
        c300: n1864,
        h1: n6814, h2: n6815,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2054,
        c370: n2052,
        c300: n1864,
        h1: n6826, h2: n6827,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2041,
        c370: n2056,
        c300: n1864,
        h1: n6835, h2: n6836,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2048,
        c370: n2058,
        c300: n1864,
        h1: n6844, h2: n6845,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2054,
        c370: n2060,
        c300: n1864,
        h1: n6853, h2: n6854,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1552,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1574,
        c370: n1554,
        c300: n1573,
        h1: n6527, h2: n6528,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_1 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1621,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1633,
        c370: n1623,
        c300: n1573,
        h1: n6536, h2: n6537,
    };
    // body 25: buttons 0x01, forks 0x0
    sink.o1(1, take_1_1, &sh1, &o1);
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_2 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1677,
        c286: n1421,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n1689,
        c370: n1679,
        c300: n1573,
        h1: n6545, h2: n6546,
    };
    // body 26: buttons 0x02, forks 0x0
    sink.o1(2, take_1_2, &sh1, &o1);
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_3 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1552,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1732,
        c370: n1722,
        c300: n1573,
        h1: n6571, h2: n6572,
    };
    // body 27: buttons 0x10, forks 0x0
    sink.o1(16, take_1_3, &sh1, &o1);
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_4 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1621,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1773,
        c370: n1763,
        c300: n1573,
        h1: n6579, h2: n6580,
    };
    // body 28: buttons 0x11, forks 0x0
    sink.o1(17, take_1_4, &sh1, &o1);
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_5 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n1423,
        c359: r_c358,
        c360: r_c359,
        c283: n1551,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n1677,
        c286: n1720,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n1814,
        c370: n1804,
        c300: n1573,
        h1: n6587, h2: n6588,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1847,
        c281: n1843,
        c359: n1848,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1865,
        c370: n1851,
        c300: n1864,
        h1: n6631, h2: n6632,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1888,
        c370: n1877,
        c300: n1864,
        h1: n6647, h2: n6648,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1909,
        c370: n1898,
        c300: n1864,
        h1: n6660, h2: n6661,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1935,
        c370: n1924,
        c300: n1864,
        h1: n6680, h2: n6681,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1942,
        c370: n1940,
        c300: n1864,
        h1: n6694, h2: n6695,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1948,
        c370: n1946,
        c300: n1864,
        h1: n6706, h2: n6707,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1935,
        c370: n1952,
        c300: n1864,
        h1: n6716, h2: n6717,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1942,
        c370: n1954,
        c300: n1864,
        h1: n6725, h2: n6726,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1421,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1948,
        c370: n1956,
        c300: n1864,
        h1: n6734, h2: n6735,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1847,
        c281: n1843,
        c359: n1848,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1986,
        c370: n1975,
        c300: n1864,
        h1: n6758, h2: n6759,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2005,
        c370: n1994,
        c300: n1864,
        h1: n6772, h2: n6773,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1846,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1849,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2024,
        c370: n2013,
        c300: n1864,
        h1: n6784, h2: n6785,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2041,
        c370: n2030,
        c300: n1864,
        h1: n6800, h2: n6801,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2048,
        c370: n2046,
        c300: n1864,
        h1: n6814, h2: n6815,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1922,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2054,
        c370: n2052,
        c300: n1864,
        h1: n6826, h2: n6827,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1920,
        c281: n1843,
        c359: n1921,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1552,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2041,
        c370: n2056,
        c300: n1864,
        h1: n6835, h2: n6836,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1875,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1621,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2048,
        c370: n2058,
        c300: n1864,
        h1: n6844, h2: n6845,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n1841,
        c41: n1842,
        c357: n1919,
        c358: n1874,
        c281: n1843,
        c359: n1896,
        c360: n1951,
        c283: n1844,
        c284: n1845,
        c361: n1677,
        c286: n1720,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n2054,
        c370: n2060,
        c300: n1864,
        h1: n6853, h2: n6854,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    declined |= live_v1_b49 & (if bd_v1_b49 { ALL } else { !ok_v1_b49 });
    take_2_0 |= live_v1_b49 & ok_v1_b49 & (if bd_v1_b49 { 0 } else { ALL });
    declined |= live_v2_b50 & (if bd_v2_b50 { ALL } else { !ok_v2_b50 });
    take_2_0 |= live_v2_b50 & ok_v2_b50 & (if bd_v2_b50 { 0 } else { ALL });
    declined |= live_v16_b51 & (if bd_v16_b51 { ALL } else { !ok_v16_b51 });
    take_2_0 |= live_v16_b51 & ok_v16_b51 & (if bd_v16_b51 { 0 } else { ALL });
    declined |= live_v17_b52 & (if bd_v17_b52 { ALL } else { !ok_v17_b52 });
    take_2_0 |= live_v17_b52 & ok_v17_b52 & (if bd_v17_b52 { 0 } else { ALL });
    declined |= live_v18_b53 & (if bd_v18_b53 { ALL } else { !ok_v18_b53 });
    take_2_0 |= live_v18_b53 & ok_v18_b53 & (if bd_v18_b53 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n6866, h2: n6867,
    };
    // body 53: buttons 0x12, forks 0x0
    sink.o2(18, take_2_0, &sh2, &o2);
    declined |= live_v32_b54 & (if bd_v32_b54 { ALL } else { !ok_v32_b54 });
    take_2_1 |= live_v32_b54 & ok_v32_b54 & (if bd_v32_b54 { 0 } else { ALL });
    declined |= live_v33_b55 & (if bd_v33_b55 { ALL } else { !ok_v33_b55 });
    take_2_1 |= live_v33_b55 & ok_v33_b55 & (if bd_v33_b55 { 0 } else { ALL });
    declined |= live_v34_b56 & (if bd_v34_b56 { ALL } else { !ok_v34_b56 });
    take_2_1 |= live_v34_b56 & ok_v34_b56 & (if bd_v34_b56 { 0 } else { ALL });
    declined |= live_v36_b57 & (if bd_v36_b57 { ALL } else { !ok_v36_b57 });
    take_2_1 |= live_v36_b57 & ok_v36_b57 & (if bd_v36_b57 { 0 } else { ALL });
    declined |= live_v48_b58 & (if bd_v48_b58 { ALL } else { !ok_v48_b58 });
    take_2_1 |= live_v48_b58 & ok_v48_b58 & (if bd_v48_b58 { 0 } else { ALL });
    declined |= live_v49_b59 & (if bd_v49_b59 { ALL } else { !ok_v49_b59 });
    take_2_1 |= live_v49_b59 & ok_v49_b59 & (if bd_v49_b59 { 0 } else { ALL });
    declined |= live_v50_b60 & (if bd_v50_b60 { ALL } else { !ok_v50_b60 });
    take_2_1 |= live_v50_b60 & ok_v50_b60 & (if bd_v50_b60 { 0 } else { ALL });
    declined |= live_v52_b61 & (if bd_v52_b61 { ALL } else { !ok_v52_b61 });
    take_2_1 |= live_v52_b61 & ok_v52_b61 & (if bd_v52_b61 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n1841,
        c41: n1842,
        h1: n6870, h2: n6871,
    };
    // body 61: buttons 0x34, forks 0x0
    sink.o2(52, take_2_1, &sh2, &o2);
    declined |= live_v0_b62 & (if bd_v0_b62 { ALL } else { !ok_v0_b62 });
    take_3_0 |= live_v0_b62 & ok_v0_b62 & (if bd_v0_b62 { 0 } else { ALL });
    declined |= live_v1_b63 & (if bd_v1_b63 { ALL } else { !ok_v1_b63 });
    take_3_0 |= live_v1_b63 & ok_v1_b63 & (if bd_v1_b63 { 0 } else { ALL });
    declined |= live_v2_b64 & (if bd_v2_b64 { ALL } else { !ok_v2_b64 });
    take_3_0 |= live_v2_b64 & ok_v2_b64 & (if bd_v2_b64 { 0 } else { ALL });
    declined |= live_v16_b65 & (if bd_v16_b65 { ALL } else { !ok_v16_b65 });
    take_3_0 |= live_v16_b65 & ok_v16_b65 & (if bd_v16_b65 { 0 } else { ALL });
    declined |= live_v17_b66 & (if bd_v17_b66 { ALL } else { !ok_v17_b66 });
    take_3_0 |= live_v17_b66 & ok_v17_b66 & (if bd_v17_b66 { 0 } else { ALL });
    declined |= live_v18_b67 & (if bd_v18_b67 { ALL } else { !ok_v18_b67 });
    take_3_0 |= live_v18_b67 & ok_v18_b67 & (if bd_v18_b67 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        h1: n6866, h2: n6867,
    };
    // body 67: buttons 0x12, forks 0x0
    sink.o3(18, take_3_0, &sh3, &o3);
    declined |= live_v32_b68 & (if bd_v32_b68 { ALL } else { !ok_v32_b68 });
    take_3_1 |= live_v32_b68 & ok_v32_b68 & (if bd_v32_b68 { 0 } else { ALL });
    declined |= live_v33_b69 & (if bd_v33_b69 { ALL } else { !ok_v33_b69 });
    take_3_1 |= live_v33_b69 & ok_v33_b69 & (if bd_v33_b69 { 0 } else { ALL });
    declined |= live_v34_b70 & (if bd_v34_b70 { ALL } else { !ok_v34_b70 });
    take_3_1 |= live_v34_b70 & ok_v34_b70 & (if bd_v34_b70 { 0 } else { ALL });
    declined |= live_v36_b71 & (if bd_v36_b71 { ALL } else { !ok_v36_b71 });
    take_3_1 |= live_v36_b71 & ok_v36_b71 & (if bd_v36_b71 { 0 } else { ALL });
    declined |= live_v48_b72 & (if bd_v48_b72 { ALL } else { !ok_v48_b72 });
    take_3_1 |= live_v48_b72 & ok_v48_b72 & (if bd_v48_b72 { 0 } else { ALL });
    declined |= live_v49_b73 & (if bd_v49_b73 { ALL } else { !ok_v49_b73 });
    take_3_1 |= live_v49_b73 & ok_v49_b73 & (if bd_v49_b73 { 0 } else { ALL });
    declined |= live_v50_b74 & (if bd_v50_b74 { ALL } else { !ok_v50_b74 });
    take_3_1 |= live_v50_b74 & ok_v50_b74 & (if bd_v50_b74 { 0 } else { ALL });
    declined |= live_v52_b75 & (if bd_v52_b75 { ALL } else { !ok_v52_b75 });
    take_3_1 |= live_v52_b75 & ok_v52_b75 & (if bd_v52_b75 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n1841,
        c41: n1842,
        h1: n6870, h2: n6871,
    };
    // body 75: buttons 0x34, forks 0x0
    sink.o3(52, take_3_1, &sh3, &o3);
    declined |= live_v0_b76 & (if bd_v0_b76 { ALL } else { !ok_v0_b76 });
    take_4_0 |= live_v0_b76 & ok_v0_b76 & (if bd_v0_b76 { 0 } else { ALL });
    declined |= live_v1_b77 & (if bd_v1_b77 { ALL } else { !ok_v1_b77 });
    take_4_0 |= live_v1_b77 & ok_v1_b77 & (if bd_v1_b77 { 0 } else { ALL });
    declined |= live_v2_b78 & (if bd_v2_b78 { ALL } else { !ok_v2_b78 });
    take_4_0 |= live_v2_b78 & ok_v2_b78 & (if bd_v2_b78 { 0 } else { ALL });
    declined |= live_v16_b79 & (if bd_v16_b79 { ALL } else { !ok_v16_b79 });
    take_4_0 |= live_v16_b79 & ok_v16_b79 & (if bd_v16_b79 { 0 } else { ALL });
    declined |= live_v17_b80 & (if bd_v17_b80 { ALL } else { !ok_v17_b80 });
    take_4_0 |= live_v17_b80 & ok_v17_b80 & (if bd_v17_b80 { 0 } else { ALL });
    declined |= live_v18_b81 & (if bd_v18_b81 { ALL } else { !ok_v18_b81 });
    take_4_0 |= live_v18_b81 & ok_v18_b81 & (if bd_v18_b81 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        h1: n6877, h2: n6878,
    };
    // body 81: buttons 0x12, forks 0x0
    sink.o4(18, take_4_0, &sh4, &o4);
    declined |= live_v32_b82 & (if bd_v32_b82 { ALL } else { !ok_v32_b82 });
    take_4_1 |= live_v32_b82 & ok_v32_b82 & (if bd_v32_b82 { 0 } else { ALL });
    declined |= live_v33_b83 & (if bd_v33_b83 { ALL } else { !ok_v33_b83 });
    take_4_1 |= live_v33_b83 & ok_v33_b83 & (if bd_v33_b83 { 0 } else { ALL });
    declined |= live_v34_b84 & (if bd_v34_b84 { ALL } else { !ok_v34_b84 });
    take_4_1 |= live_v34_b84 & ok_v34_b84 & (if bd_v34_b84 { 0 } else { ALL });
    declined |= live_v36_b85 & (if bd_v36_b85 { ALL } else { !ok_v36_b85 });
    take_4_1 |= live_v36_b85 & ok_v36_b85 & (if bd_v36_b85 { 0 } else { ALL });
    declined |= live_v48_b86 & (if bd_v48_b86 { ALL } else { !ok_v48_b86 });
    take_4_1 |= live_v48_b86 & ok_v48_b86 & (if bd_v48_b86 { 0 } else { ALL });
    declined |= live_v49_b87 & (if bd_v49_b87 { ALL } else { !ok_v49_b87 });
    take_4_1 |= live_v49_b87 & ok_v49_b87 & (if bd_v49_b87 { 0 } else { ALL });
    declined |= live_v50_b88 & (if bd_v50_b88 { ALL } else { !ok_v50_b88 });
    take_4_1 |= live_v50_b88 & ok_v50_b88 & (if bd_v50_b88 { 0 } else { ALL });
    declined |= live_v52_b89 & (if bd_v52_b89 { ALL } else { !ok_v52_b89 });
    take_4_1 |= live_v52_b89 & ok_v52_b89 & (if bd_v52_b89 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n5316,
        c41: n5317,
        h1: n6883, h2: n6884,
    };
    // body 89: buttons 0x34, forks 0x0
    sink.o4(52, take_4_1, &sh4, &o4);
    declined |= live_v0_b90 & (if bd_v0_b90 { ALL } else { !ok_v0_b90 });
    take_5_0 |= live_v0_b90 & ok_v0_b90 & (if bd_v0_b90 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5503,
        c39: n5504,
        c20: r_c20,
        c38: n5506,
        h1: n6894, h2: n6895,
    };
    // body 90: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v1_b91 & (if bd_v1_b91 { ALL } else { !ok_v1_b91 });
    take_5_1 |= live_v1_b91 & ok_v1_b91 & (if bd_v1_b91 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5552,
        c39: n5553,
        c20: r_c20,
        c38: n5555,
        h1: n6903, h2: n6904,
    };
    // body 91: buttons 0x01, forks 0x0
    sink.o5(1, take_5_1, &sh5, &o5);
    declined |= live_v2_b92 & (if bd_v2_b92 { ALL } else { !ok_v2_b92 });
    take_5_2 |= live_v2_b92 & ok_v2_b92 & (if bd_v2_b92 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5601,
        c39: n5602,
        c20: r_c20,
        c38: n5604,
        h1: n6912, h2: n6913,
    };
    // body 92: buttons 0x02, forks 0x0
    sink.o5(2, take_5_2, &sh5, &o5);
    declined |= live_v16_b93 & (if bd_v16_b93 { ALL } else { !ok_v16_b93 });
    take_5_3 |= live_v16_b93 & ok_v16_b93 & (if bd_v16_b93 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5649,
        c39: n5650,
        c20: r_c20,
        c38: n5652,
        h1: n6921, h2: n6922,
    };
    // body 93: buttons 0x10, forks 0x0
    sink.o5(16, take_5_3, &sh5, &o5);
    declined |= live_v17_b94 & (if bd_v17_b94 { ALL } else { !ok_v17_b94 });
    take_5_4 |= live_v17_b94 & ok_v17_b94 & (if bd_v17_b94 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5697,
        c39: n5698,
        c20: r_c20,
        c38: n5700,
        h1: n6930, h2: n6931,
    };
    // body 94: buttons 0x11, forks 0x0
    sink.o5(17, take_5_4, &sh5, &o5);
    declined |= live_v18_b95 & (if bd_v18_b95 { ALL } else { !ok_v18_b95 });
    take_5_5 |= live_v18_b95 & ok_v18_b95 & (if bd_v18_b95 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5745,
        c39: n5746,
        c20: r_c20,
        c38: n5748,
        h1: n6939, h2: n6940,
    };
    // body 95: buttons 0x12, forks 0x0
    sink.o5(18, take_5_5, &sh5, &o5);
    declined |= live_v32_b96 & (if bd_v32_b96 { ALL } else { !ok_v32_b96 });
    take_5_6 |= live_v32_b96 & ok_v32_b96 & (if bd_v32_b96 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5778,
        c39: n5779,
        c20: n5780,
        c38: n5782,
        h1: n6951, h2: n6952,
    };
    // body 96: buttons 0x20, forks 0x0
    sink.o5(32, take_5_6, &sh5, &o5);
    declined |= live_v33_b97 & (if bd_v33_b97 { ALL } else { !ok_v33_b97 });
    take_5_7 |= live_v33_b97 & ok_v33_b97 & (if bd_v33_b97 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5804,
        c39: n5805,
        c20: n5806,
        c38: n5808,
        h1: n6963, h2: n6964,
    };
    // body 97: buttons 0x21, forks 0x0
    sink.o5(33, take_5_7, &sh5, &o5);
    declined |= live_v34_b98 & (if bd_v34_b98 { ALL } else { !ok_v34_b98 });
    take_5_8 |= live_v34_b98 & ok_v34_b98 & (if bd_v34_b98 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5830,
        c39: n5831,
        c20: n5832,
        c38: n5834,
        h1: n6975, h2: n6976,
    };
    // body 98: buttons 0x22, forks 0x0
    sink.o5(34, take_5_8, &sh5, &o5);
    declined |= live_v36_b99 & (if bd_v36_b99 { ALL } else { !ok_v36_b99 });
    take_5_9 |= live_v36_b99 & ok_v36_b99 & (if bd_v36_b99 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5854,
        c39: n5855,
        c20: n5856,
        c38: n5858,
        h1: n6987, h2: n6988,
    };
    // body 99: buttons 0x24, forks 0x0
    sink.o5(36, take_5_9, &sh5, &o5);
    declined |= live_v48_b100 & (if bd_v48_b100 { ALL } else { !ok_v48_b100 });
    take_5_10 |= live_v48_b100 & ok_v48_b100 & (if bd_v48_b100 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5892,
        c39: n5893,
        c20: n5894,
        c38: n5896,
        h1: n6999, h2: n7000,
    };
    // body 100: buttons 0x30, forks 0x0
    sink.o5(48, take_5_10, &sh5, &o5);
    declined |= live_v49_b101 & (if bd_v49_b101 { ALL } else { !ok_v49_b101 });
    take_5_11 |= live_v49_b101 & ok_v49_b101 & (if bd_v49_b101 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5918,
        c39: n5919,
        c20: n5920,
        c38: n5922,
        h1: n7011, h2: n7012,
    };
    // body 101: buttons 0x31, forks 0x0
    sink.o5(49, take_5_11, &sh5, &o5);
    declined |= live_v50_b102 & (if bd_v50_b102 { ALL } else { !ok_v50_b102 });
    take_5_12 |= live_v50_b102 & ok_v50_b102 & (if bd_v50_b102 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5944,
        c39: n5945,
        c20: n5946,
        c38: n5948,
        h1: n7023, h2: n7024,
    };
    // body 102: buttons 0x32, forks 0x0
    sink.o5(50, take_5_12, &sh5, &o5);
    declined |= live_v52_b103 & (if bd_v52_b103 { ALL } else { !ok_v52_b103 });
    take_5_13 |= live_v52_b103 & ok_v52_b103 & (if bd_v52_b103 { 0 } else { ALL });
    let o5 = KOut5 {
        c87: n5968,
        c39: n5969,
        c20: n5970,
        c38: n5972,
        h1: n7035, h2: n7036,
    };
    // body 103: buttons 0x34, forks 0x0
    sink.o5(52, take_5_13, &sh5, &o5);
    declined |= live_v0_b104 & (if bd_v0_b104 { ALL } else { !ok_v0_b104 });
    take_6_0 |= live_v0_b104 & ok_v0_b104 & (if bd_v0_b104 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6058,
        c285: n6053,
        c292: n6054,
        c293: n6055,
        c368: n6085,
        c369: n6062,
        c299: n6084,
        h1: n7087, h2: n7088,
    };
    // body 104: buttons 0x00, forks 0x0
    sink.o6(0, take_6_0, &sh6, &o6);
    declined |= live_v1_b105 & (if bd_v1_b105 { ALL } else { !ok_v1_b105 });
    take_6_1 |= live_v1_b105 & ok_v1_b105 & (if bd_v1_b105 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6100,
        c285: n6053,
        c292: n6054,
        c293: n6055,
        c368: n6113,
        c369: n6102,
        c299: n6084,
        h1: n7096, h2: n7097,
    };
    // body 105: buttons 0x01, forks 0x0
    sink.o6(1, take_6_1, &sh6, &o6);
    declined |= live_v2_b106 & (if bd_v2_b106 { ALL } else { !ok_v2_b106 });
    take_6_2 |= live_v2_b106 & ok_v2_b106 & (if bd_v2_b106 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6127,
        c285: n6053,
        c292: n6054,
        c293: n6055,
        c368: n6140,
        c369: n6129,
        c299: n6084,
        h1: n7105, h2: n7106,
    };
    // body 106: buttons 0x02, forks 0x0
    sink.o6(2, take_6_2, &sh6, &o6);
    declined |= live_v16_b107 & (if bd_v16_b107 { ALL } else { !ok_v16_b107 });
    take_6_3 |= live_v16_b107 & ok_v16_b107 & (if bd_v16_b107 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6058,
        c285: n6149,
        c292: n6054,
        c293: n6150,
        c368: n6163,
        c369: n6152,
        c299: n6084,
        h1: n7131, h2: n7132,
    };
    // body 107: buttons 0x10, forks 0x0
    sink.o6(16, take_6_3, &sh6, &o6);
    declined |= live_v17_b108 & (if bd_v17_b108 { ALL } else { !ok_v17_b108 });
    take_6_4 |= live_v17_b108 & ok_v17_b108 & (if bd_v17_b108 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6100,
        c285: n6149,
        c292: n6054,
        c293: n6150,
        c368: n6182,
        c369: n6171,
        c299: n6084,
        h1: n7139, h2: n7140,
    };
    // body 108: buttons 0x11, forks 0x0
    sink.o6(17, take_6_4, &sh6, &o6);
    declined |= live_v18_b109 & (if bd_v18_b109 { ALL } else { !ok_v18_b109 });
    take_6_5 |= live_v18_b109 & ok_v18_b109 & (if bd_v18_b109 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6049,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n6050,
        c358: r_c358,
        c359: r_c359,
        c282: n6051,
        c283: n6052,
        c360: n6127,
        c285: n6149,
        c292: n6054,
        c293: n6150,
        c368: n6201,
        c369: n6190,
        c299: n6084,
        h1: n7147, h2: n7148,
    };
    // body 109: buttons 0x12, forks 0x0
    sink.o6(18, take_6_5, &sh6, &o6);
    declined |= live_v32_b110 & (if bd_v32_b110 { ALL } else { !ok_v32_b110 });
    take_6_6 |= live_v32_b110 & ok_v32_b110 & (if bd_v32_b110 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6229,
        c280: n6224,
        c358: n6230,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6247,
        c369: n6233,
        c299: n6246,
        h1: n7192, h2: n7193,
    };
    // body 110: buttons 0x20, forks 0x0
    sink.o6(32, take_6_6, &sh6, &o6);
    declined |= live_v33_b111 & (if bd_v33_b111 { ALL } else { !ok_v33_b111 });
    take_6_7 |= live_v33_b111 & ok_v33_b111 & (if bd_v33_b111 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6272,
        c369: n6261,
        c299: n6246,
        h1: n7208, h2: n7209,
    };
    // body 111: buttons 0x21, forks 0x0
    sink.o6(33, take_6_7, &sh6, &o6);
    declined |= live_v34_b112 & (if bd_v34_b112 { ALL } else { !ok_v34_b112 });
    take_6_8 |= live_v34_b112 & ok_v34_b112 & (if bd_v34_b112 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6294,
        c369: n6283,
        c299: n6246,
        h1: n7221, h2: n7222,
    };
    // body 112: buttons 0x22, forks 0x0
    sink.o6(34, take_6_8, &sh6, &o6);
    declined |= live_v36_b113 & (if bd_v36_b113 { ALL } else { !ok_v36_b113 });
    take_6_9 |= live_v36_b113 & ok_v36_b113 & (if bd_v36_b113 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6310,
        c280: n6224,
        c358: n6311,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6325,
        c369: n6314,
        c299: n6246,
        h1: n7241, h2: n7242,
    };
    // body 113: buttons 0x24, forks 0x0
    sink.o6(36, take_6_9, &sh6, &o6);
    declined |= live_v37_b114 & (if bd_v37_b114 { ALL } else { !ok_v37_b114 });
    take_6_10 |= live_v37_b114 & ok_v37_b114 & (if bd_v37_b114 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6334,
        c369: n6332,
        c299: n6246,
        h1: n7255, h2: n7256,
    };
    // body 114: buttons 0x25, forks 0x0
    sink.o6(37, take_6_10, &sh6, &o6);
    declined |= live_v38_b115 & (if bd_v38_b115 { ALL } else { !ok_v38_b115 });
    take_6_11 |= live_v38_b115 & ok_v38_b115 & (if bd_v38_b115 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6342,
        c369: n6340,
        c299: n6246,
        h1: n7267, h2: n7268,
    };
    // body 115: buttons 0x26, forks 0x0
    sink.o6(38, take_6_11, &sh6, &o6);
    declined |= live_v40_b116 & (if bd_v40_b116 { ALL } else { !ok_v40_b116 });
    take_6_12 |= live_v40_b116 & ok_v40_b116 & (if bd_v40_b116 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6310,
        c280: n6224,
        c358: n6311,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6325,
        c369: n6348,
        c299: n6246,
        h1: n7277, h2: n7278,
    };
    // body 116: buttons 0x28, forks 0x0
    sink.o6(40, take_6_12, &sh6, &o6);
    declined |= live_v41_b117 & (if bd_v41_b117 { ALL } else { !ok_v41_b117 });
    take_6_13 |= live_v41_b117 & ok_v41_b117 & (if bd_v41_b117 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6334,
        c369: n6351,
        c299: n6246,
        h1: n7286, h2: n7287,
    };
    // body 117: buttons 0x29, forks 0x0
    sink.o6(41, take_6_13, &sh6, &o6);
    declined |= live_v42_b118 & (if bd_v42_b118 { ALL } else { !ok_v42_b118 });
    take_6_14 |= live_v42_b118 & ok_v42_b118 & (if bd_v42_b118 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6053,
        c292: n6227,
        c293: n6055,
        c368: n6342,
        c369: n6354,
        c299: n6246,
        h1: n7295, h2: n7296,
    };
    // body 118: buttons 0x2a, forks 0x0
    sink.o6(42, take_6_14, &sh6, &o6);
    declined |= live_v48_b119 & (if bd_v48_b119 { ALL } else { !ok_v48_b119 });
    take_6_15 |= live_v48_b119 & ok_v48_b119 & (if bd_v48_b119 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6229,
        c280: n6224,
        c358: n6230,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6372,
        c369: n6361,
        c299: n6246,
        h1: n7319, h2: n7320,
    };
    // body 119: buttons 0x30, forks 0x0
    sink.o6(48, take_6_15, &sh6, &o6);
    declined |= live_v49_b120 & (if bd_v49_b120 { ALL } else { !ok_v49_b120 });
    take_6_16 |= live_v49_b120 & ok_v49_b120 & (if bd_v49_b120 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6391,
        c369: n6380,
        c299: n6246,
        h1: n7333, h2: n7334,
    };
    // body 120: buttons 0x31, forks 0x0
    sink.o6(49, take_6_16, &sh6, &o6);
    declined |= live_v50_b121 & (if bd_v50_b121 { ALL } else { !ok_v50_b121 });
    take_6_17 |= live_v50_b121 & ok_v50_b121 & (if bd_v50_b121 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6228,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6231,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6410,
        c369: n6399,
        c299: n6246,
        h1: n7345, h2: n7346,
    };
    // body 121: buttons 0x32, forks 0x0
    sink.o6(50, take_6_17, &sh6, &o6);
    declined |= live_v52_b122 & (if bd_v52_b122 { ALL } else { !ok_v52_b122 });
    take_6_18 |= live_v52_b122 & ok_v52_b122 & (if bd_v52_b122 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6310,
        c280: n6224,
        c358: n6311,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6429,
        c369: n6418,
        c299: n6246,
        h1: n7361, h2: n7362,
    };
    // body 122: buttons 0x34, forks 0x0
    sink.o6(52, take_6_18, &sh6, &o6);
    declined |= live_v53_b123 & (if bd_v53_b123 { ALL } else { !ok_v53_b123 });
    take_6_19 |= live_v53_b123 & ok_v53_b123 & (if bd_v53_b123 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6438,
        c369: n6436,
        c299: n6246,
        h1: n7375, h2: n7376,
    };
    // body 123: buttons 0x35, forks 0x0
    sink.o6(53, take_6_19, &sh6, &o6);
    declined |= live_v54_b124 & (if bd_v54_b124 { ALL } else { !ok_v54_b124 });
    take_6_20 |= live_v54_b124 & ok_v54_b124 & (if bd_v54_b124 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6312,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6446,
        c369: n6444,
        c299: n6246,
        h1: n7387, h2: n7388,
    };
    // body 124: buttons 0x36, forks 0x0
    sink.o6(54, take_6_20, &sh6, &o6);
    declined |= live_v56_b125 & (if bd_v56_b125 { ALL } else { !ok_v56_b125 });
    take_6_21 |= live_v56_b125 & ok_v56_b125 & (if bd_v56_b125 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6310,
        c280: n6224,
        c358: n6311,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6058,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6429,
        c369: n6449,
        c299: n6246,
        h1: n7396, h2: n7397,
    };
    // body 125: buttons 0x38, forks 0x0
    sink.o6(56, take_6_21, &sh6, &o6);
    declined |= live_v57_b126 & (if bd_v57_b126 { ALL } else { !ok_v57_b126 });
    take_6_22 |= live_v57_b126 & ok_v57_b126 & (if bd_v57_b126 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6259,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6100,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6438,
        c369: n6452,
        c299: n6246,
        h1: n7405, h2: n7406,
    };
    // body 126: buttons 0x39, forks 0x0
    sink.o6(57, take_6_22, &sh6, &o6);
    declined |= live_v58_b127 & (if bd_v58_b127 { ALL } else { !ok_v58_b127 });
    take_6_23 |= live_v58_b127 & ok_v58_b127 & (if bd_v58_b127 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n6222,
        c41: n6223,
        c356: n6309,
        c357: n6258,
        c280: n6224,
        c358: n6281,
        c359: n6347,
        c282: n6225,
        c283: n6226,
        c360: n6127,
        c285: n6149,
        c292: n6227,
        c293: n6150,
        c368: n6446,
        c369: n6455,
        c299: n6246,
        h1: n7414, h2: n7415,
    };
    // body 127: buttons 0x3a, forks 0x0
    sink.o6(58, take_6_23, &sh6, &o6);
    declined
}
