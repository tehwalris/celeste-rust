// GENERATED from a TRACED frame (shape 8). Do not edit.
//
// One input shape, 7 output shapes, 80 distinct button
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
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c85: ZN,
    pub c38: ZB,
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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_2: u64 = 12324093367925927381;
pub const KPART2_2: u64 = 12364506016476404550;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 10611605397908074725;
pub const KPART2_3: u64 = 10354055476980256846;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_4: u64 = 7359791922650843579;
pub const KPART2_4: u64 = 17068849822392135886;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_5: u64 = 16651243698737751054;
pub const KPART2_5: u64 = 14372757396449290073;

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

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_6: u64 = 6556206805066229762;
pub const KPART2_6: u64 = 15592723234554252880;

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
    let n220: ZB = zb_and(n137, n219);
    let n221: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), r_c300);
    let n222: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n221);
    let n223: ZB = zn_gt(n222, zn_splat(P8::from_raw(7340032i32)));
    let n224: ZB = zb_and(n219, n223);
    let n225: ZB = zn_lt(n217, zn_splat(P8::from_raw(3145728i32)));
    let n226: ZB = zb_and(n224, n225);
    let n227: ZB = zn_lt(n221, zn_splat(P8::from_raw(7864320i32)));
    let n228: ZB = zb_and(n226, n227);
    let n229: ZB = zb_and(n137, n228);
    let n230: ZB = zn_ge(r_c369, zn_splat(P8::from_raw(0i32)));
    let n231: ZN = zn_mul(r_c368, zn_splat(P8::from_raw(13107i32)));
    let n232: ZB = zn_gt(n218, zn_splat(P8::from_raw(6815744i32)));
    let n233: ZB = zb_and(n220, n223);
    let n234: ZB = zb_and(n225, n233);
    let n235: ZB = zb_and(n227, n234);
    let n236: ZB = zb_and(n230, n235);
    let n237: ZB = zn_lt(n217, zn_splat(P8::from_raw(7340032i32)));
    let n238: ZN = zn_add(r_c366, n231);
    let n239: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n238);
    let n240: ZN = zn_flr(n239);
    let n241: ZN = zn_sub(n239, zn_splat(P8::from_raw(32768i32)));
    let n242: ZN = zn_sub(n241, n240);
    let n243: ZB = zn_gt(n240, zn_splat(P8::from_raw(0i32)));
    let n244: ZB = zn_lt(n240, zn_splat(P8::from_raw(0i32)));
    let n245: ZN = zsel_n(n244, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n246: ZN = zsel_n(n243, zn_splat(P8::from_raw(65536i32)), n245);
    let n247: ZN = zn_abs(n240);
    let n248: ZN = zn_add(n217, n246);
    let n249: ZB = zn_tile_flag_at(g.cache, g.cart, n248, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n250: ZN = zn_add(r_c299, n246);
    let n251: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n247);
    let n252: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n250);
    let n253: ZN = zn_add(n246, n252);
    let n254: ZB = zn_tile_flag_at(g.cache, g.cart, n253, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n255: ZN = zn_add(n246, n250);
    let n256: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n247);
    let n257: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n255);
    let n258: ZN = zn_add(n246, n257);
    let n259: ZB = zn_tile_flag_at(g.cache, g.cart, n258, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n260: ZN = zn_add(n246, n255);
    let n261: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n247);
    let n262: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n260);
    let n263: ZN = zn_add(n246, n262);
    let n264: ZB = zn_tile_flag_at(g.cache, g.cart, n263, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n265: ZN = zn_add(n246, n260);
    let n266: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n247);
    let n267: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n265);
    let n268: ZN = zn_add(n246, n267);
    let n269: ZB = zn_tile_flag_at(g.cache, g.cart, n268, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n270: ZN = zn_add(n246, n265);
    let n271: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n247);
    let n272: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n270);
    let n273: ZN = zn_add(n246, n272);
    let n274: ZB = zn_tile_flag_at(g.cache, g.cart, n273, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n275: ZN = zn_add(n246, n270);
    let n276: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n247);
    let n277: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n275);
    let n278: ZN = zn_add(n246, n277);
    let n279: ZB = zn_tile_flag_at(g.cache, g.cart, n278, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n280: ZN = zn_add(n246, n275);
    let n281: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n247);
    let n282: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n280);
    let n283: ZN = zn_add(n246, n282);
    let n284: ZB = zn_tile_flag_at(g.cache, g.cart, n283, zn_splat(P8::from_raw(7274496i32)), P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n285: ZN = zn_add(n246, n280);
    let n286: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n247);
    let n287: ZN = zsel_n(n284, n280, n285);
    let n288: ZN = zsel_n(n284, zn_splat(P8::from_raw(0i32)), n242);
    let n289: ZN = zsel_n(n284, zn_splat(P8::from_raw(0i32)), n231);
    let n290: ZB = zb_or(n284, n286);
    let n291: ZN = zsel_n(n281, n280, n287);
    let n292: ZN = zsel_n(n281, n242, n288);
    let n293: ZN = zsel_n(n281, n231, n289);
    let n294: ZB = zb_or(n281, n290);
    let n295: ZN = zsel_n(n279, n275, n291);
    let n296: ZN = zsel_n(n279, zn_splat(P8::from_raw(0i32)), n292);
    let n297: ZN = zsel_n(n279, zn_splat(P8::from_raw(0i32)), n293);
    let n298: ZB = zb_or(n279, n294);
    let n299: ZN = zsel_n(n276, n275, n295);
    let n300: ZN = zsel_n(n276, n242, n296);
    let n301: ZN = zsel_n(n276, n231, n297);
    let n302: ZB = zb_or(n276, n298);
    let n303: ZN = zsel_n(n274, n270, n299);
    let n304: ZN = zsel_n(n274, zn_splat(P8::from_raw(0i32)), n300);
    let n305: ZN = zsel_n(n274, zn_splat(P8::from_raw(0i32)), n301);
    let n306: ZB = zb_or(n274, n302);
    let n307: ZN = zsel_n(n271, n270, n303);
    let n308: ZN = zsel_n(n271, n242, n304);
    let n309: ZN = zsel_n(n271, n231, n305);
    let n310: ZB = zb_or(n271, n306);
    let n311: ZN = zsel_n(n269, n265, n307);
    let n312: ZN = zsel_n(n269, zn_splat(P8::from_raw(0i32)), n308);
    let n313: ZN = zsel_n(n269, zn_splat(P8::from_raw(0i32)), n309);
    let n314: ZB = zb_or(n269, n310);
    let n315: ZN = zsel_n(n266, n265, n311);
    let n316: ZN = zsel_n(n266, n242, n312);
    let n317: ZN = zsel_n(n266, n231, n313);
    let n318: ZB = zb_or(n266, n314);
    let n319: ZN = zsel_n(n264, n260, n315);
    let n320: ZN = zsel_n(n264, zn_splat(P8::from_raw(0i32)), n316);
    let n321: ZN = zsel_n(n264, zn_splat(P8::from_raw(0i32)), n317);
    let n322: ZB = zb_or(n264, n318);
    let n323: ZN = zsel_n(n261, n260, n319);
    let n324: ZN = zsel_n(n261, n242, n320);
    let n325: ZN = zsel_n(n261, n231, n321);
    let n326: ZB = zb_or(n261, n322);
    let n327: ZN = zsel_n(n259, n255, n323);
    let n328: ZN = zsel_n(n259, zn_splat(P8::from_raw(0i32)), n324);
    let n329: ZN = zsel_n(n259, zn_splat(P8::from_raw(0i32)), n325);
    let n330: ZB = zb_or(n259, n326);
    let n331: ZN = zsel_n(n256, n255, n327);
    let n332: ZN = zsel_n(n256, n242, n328);
    let n333: ZN = zsel_n(n256, n231, n329);
    let n334: ZB = zb_or(n256, n330);
    let n335: ZN = zsel_n(n254, n250, n331);
    let n336: ZN = zsel_n(n254, zn_splat(P8::from_raw(0i32)), n332);
    let n337: ZN = zsel_n(n254, zn_splat(P8::from_raw(0i32)), n333);
    let n338: ZB = zb_or(n254, n334);
    let n339: ZN = zsel_n(n251, n250, n335);
    let n340: ZN = zsel_n(n251, n242, n336);
    let n341: ZN = zsel_n(n251, n231, n337);
    let n342: ZB = zb_or(n251, n338);
    let n343: ZN = zsel_n(n249, r_c299, n339);
    let n344: ZN = zsel_n(n249, zn_splat(P8::from_raw(0i32)), n340);
    let n345: ZN = zsel_n(n249, zn_splat(P8::from_raw(0i32)), n341);
    let n346: ZB = zb_or(n249, n342);
    let n347: ZN = zn_add(r_c367, zn_splat(P8::from_raw(-196608i32)));
    let n348: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n347);
    let n349: ZN = zn_flr(n348);
    let n350: ZN = zn_sub(n348, zn_splat(P8::from_raw(32768i32)));
    let n351: ZN = zn_sub(n350, n349);
    let n352: ZB = zn_gt(n349, zn_splat(P8::from_raw(0i32)));
    let n353: ZB = zn_lt(n349, zn_splat(P8::from_raw(0i32)));
    let n354: ZN = zsel_n(n353, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n355: ZN = zsel_n(n352, zn_splat(P8::from_raw(65536i32)), n354);
    let n356: ZN = zn_abs(n349);
    let n357: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n343);
    let n358: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n357);
    let n359: ZN = zn_add(zn_splat(P8::from_raw(7274496i32)), n355);
    let n360: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n359, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n361: ZN = zn_add(zn_splat(P8::from_raw(7077888i32)), n355);
    let n362: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n356);
    let n363: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n361);
    let n364: ZN = zn_add(n355, n363);
    let n365: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n364, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n366: ZN = zn_add(n355, n361);
    let n367: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n356);
    let n368: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n366);
    let n369: ZN = zn_add(n355, n368);
    let n370: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n369, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n371: ZN = zn_add(n355, n366);
    let n372: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n356);
    let n373: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n371);
    let n374: ZN = zn_add(n355, n373);
    let n375: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n374, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n376: ZN = zn_add(n355, n371);
    let n377: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n356);
    let n378: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n376);
    let n379: ZN = zn_add(n355, n378);
    let n380: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n379, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n381: ZN = zn_add(n355, n376);
    let n382: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n356);
    let n383: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n381);
    let n384: ZN = zn_add(n355, n383);
    let n385: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n384, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n386: ZN = zn_add(n355, n381);
    let n387: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n356);
    let n388: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n386);
    let n389: ZN = zn_add(n355, n388);
    let n390: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n389, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n391: ZN = zn_add(n355, n386);
    let n392: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n356);
    let n393: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n391);
    let n394: ZN = zn_add(n355, n393);
    let n395: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n394, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n396: ZN = zn_add(n355, n391);
    let n397: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n356);
    let n398: ZB = zb_and(n346, n397);
    let n399: ZN = zsel_n(n395, n391, n396);
    let n400: ZN = zsel_n(n395, zn_splat(P8::from_raw(0i32)), n351);
    let n401: ZN = zsel_n(n395, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(-196608i32)));
    let n402: ZB = zsel_b(n395, n346, n398);
    let n403: ZN = zsel_n(n392, n391, n399);
    let n404: ZN = zsel_n(n392, n351, n400);
    let n405: ZN = zsel_n(n392, zn_splat(P8::from_raw(-196608i32)), n401);
    let n406: ZB = zsel_b(n392, n346, n402);
    let n407: ZN = zsel_n(n390, n386, n403);
    let n408: ZN = zsel_n(n390, zn_splat(P8::from_raw(0i32)), n404);
    let n409: ZN = zsel_n(n390, zn_splat(P8::from_raw(0i32)), n405);
    let n410: ZB = zsel_b(n390, n346, n406);
    let n411: ZN = zsel_n(n387, n386, n407);
    let n412: ZN = zsel_n(n387, n351, n408);
    let n413: ZN = zsel_n(n387, zn_splat(P8::from_raw(-196608i32)), n409);
    let n414: ZB = zsel_b(n387, n346, n410);
    let n415: ZN = zsel_n(n385, n381, n411);
    let n416: ZN = zsel_n(n385, zn_splat(P8::from_raw(0i32)), n412);
    let n417: ZN = zsel_n(n385, zn_splat(P8::from_raw(0i32)), n413);
    let n418: ZB = zsel_b(n385, n346, n414);
    let n419: ZN = zsel_n(n382, n381, n415);
    let n420: ZN = zsel_n(n382, n351, n416);
    let n421: ZN = zsel_n(n382, zn_splat(P8::from_raw(-196608i32)), n417);
    let n422: ZB = zsel_b(n382, n346, n418);
    let n423: ZN = zsel_n(n380, n376, n419);
    let n424: ZN = zsel_n(n380, zn_splat(P8::from_raw(0i32)), n420);
    let n425: ZN = zsel_n(n380, zn_splat(P8::from_raw(0i32)), n421);
    let n426: ZB = zsel_b(n380, n346, n422);
    let n427: ZN = zsel_n(n377, n376, n423);
    let n428: ZN = zsel_n(n377, n351, n424);
    let n429: ZN = zsel_n(n377, zn_splat(P8::from_raw(-196608i32)), n425);
    let n430: ZB = zsel_b(n377, n346, n426);
    let n431: ZN = zsel_n(n375, n371, n427);
    let n432: ZN = zsel_n(n375, zn_splat(P8::from_raw(0i32)), n428);
    let n433: ZN = zsel_n(n375, zn_splat(P8::from_raw(0i32)), n429);
    let n434: ZB = zsel_b(n375, n346, n430);
    let n435: ZN = zsel_n(n372, n371, n431);
    let n436: ZN = zsel_n(n372, n351, n432);
    let n437: ZN = zsel_n(n372, zn_splat(P8::from_raw(-196608i32)), n433);
    let n438: ZB = zsel_b(n372, n346, n434);
    let n439: ZN = zsel_n(n370, n366, n435);
    let n440: ZN = zsel_n(n370, zn_splat(P8::from_raw(0i32)), n436);
    let n441: ZN = zsel_n(n370, zn_splat(P8::from_raw(0i32)), n437);
    let n442: ZB = zsel_b(n370, n346, n438);
    let n443: ZN = zsel_n(n367, n366, n439);
    let n444: ZN = zsel_n(n367, n351, n440);
    let n445: ZN = zsel_n(n367, zn_splat(P8::from_raw(-196608i32)), n441);
    let n446: ZB = zsel_b(n367, n346, n442);
    let n447: ZN = zsel_n(n365, n361, n443);
    let n448: ZN = zsel_n(n365, zn_splat(P8::from_raw(0i32)), n444);
    let n449: ZN = zsel_n(n365, zn_splat(P8::from_raw(0i32)), n445);
    let n450: ZB = zsel_b(n365, n346, n446);
    let n451: ZN = zsel_n(n362, n361, n447);
    let n452: ZN = zsel_n(n362, n351, n448);
    let n453: ZN = zsel_n(n362, zn_splat(P8::from_raw(-196608i32)), n449);
    let n454: ZB = zsel_b(n362, n346, n450);
    let n455: ZN = zsel_n(n360, zn_splat(P8::from_raw(7077888i32)), n451);
    let n456: ZN = zsel_n(n360, zn_splat(P8::from_raw(0i32)), n452);
    let n457: ZN = zsel_n(n360, zn_splat(P8::from_raw(0i32)), n453);
    let n458: ZB = zsel_b(n360, n346, n454);
    let n459: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n455);
    let n460: ZN = zn_div(n357, zn_splat(P8::from_raw(524288i32)));
    let n461: ZN = zn_flr(n460);
    let n462: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n461);
    let n463: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n357);
    let n464: ZN = zn_sub(n463, zn_splat(P8::from_raw(65536i32)));
    let n465: ZN = zn_div(n464, zn_splat(P8::from_raw(524288i32)));
    let n466: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n465);
    let n467: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n462);
    let n468: ZB = zn_le(n467, n466);
    let n469: ZB = zn_gt(n467, n466);
    let n470: ZB = zb_and(n236, n468);
    let n471: ZB = zb_and(n236, n469);
    let n472: ZN = zn_div(n459, zn_splat(P8::from_raw(524288i32)));
    let n473: ZN = zn_flr(n472);
    let n474: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n473);
    let n475: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n459);
    let n476: ZN = zn_sub(n475, zn_splat(P8::from_raw(65536i32)));
    let n477: ZN = zn_div(n476, zn_splat(P8::from_raw(524288i32)));
    let n478: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n477);
    let n479: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n474);
    let n480: ZB = zn_le(n479, n478);
    let n481: ZB = zn_gt(n479, n478);
    let n482: ZB = zb_and(n470, n480);
    let n483: ZB = zb_and(n470, n481);
    let n484: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n467);
    let n485: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n479);
    let n486: ZN = zn_mget(g.cart, n484, n485);
    let n487: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n486);
    let n488: ZN = zn_rem(n476, zn_splat(P8::from_raw(524288i32)));
    let n489: ZB = zn_ge(n488, zn_splat(P8::from_raw(393216i32)));
    let n490: ZN = zn_mul(n479, zn_splat(P8::from_raw(524288i32)));
    let n491: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n490);
    let n492: ZB = zn_eq(n475, n491);
    let n493: ZB = zb_or(n489, n492);
    let n494: ZB = zb_and(n487, n493);
    let n495: ZB = zn_ge(n457, zn_splat(P8::from_raw(0i32)));
    let n496: ZB = zb_and(n494, n495);
    let n497: ZB = zb_not(n496);
    let n498: ZB = zb_and(n482, n497);
    let n499: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n486);
    let n500: ZN = zn_rem(n459, zn_splat(P8::from_raw(524288i32)));
    let n501: ZB = zn_le(n500, zn_splat(P8::from_raw(131072i32)));
    let n502: ZB = zb_and(n499, n501);
    let n503: ZB = zb_not(n502);
    let n504: ZB = zb_and(n498, n502);
    let n505: ZB = zb_and(n498, n503);
    let n506: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n486);
    let n507: ZN = zn_rem(n357, zn_splat(P8::from_raw(524288i32)));
    let n508: ZB = zn_le(n507, zn_splat(P8::from_raw(131072i32)));
    let n509: ZB = zb_and(n506, n508);
    let n510: ZB = zn_le(n345, zn_splat(P8::from_raw(0i32)));
    let n511: ZB = zb_and(n509, n510);
    let n512: ZB = zb_not(n511);
    let n513: ZB = zb_and(n505, n512);
    let n514: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n486);
    let n515: ZN = zn_rem(n464, zn_splat(P8::from_raw(524288i32)));
    let n516: ZB = zn_ge(n515, zn_splat(P8::from_raw(393216i32)));
    let n517: ZN = zn_mul(n467, zn_splat(P8::from_raw(524288i32)));
    let n518: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n517);
    let n519: ZB = zn_eq(n463, n518);
    let n520: ZB = zb_or(n516, n519);
    let n521: ZB = zb_and(n514, n520);
    let n522: ZB = zn_ge(n345, zn_splat(P8::from_raw(0i32)));
    let n523: ZB = zb_and(n521, n522);
    let n524: ZB = zb_not(n523);
    let n525: ZB = zb_and(n513, n524);
    let n526: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n474);
    let n527: ZB = zn_le(n526, n478);
    let n528: ZB = zn_gt(n526, n478);
    let n529: ZB = zb_and(n525, n527);
    let n530: ZB = zb_and(n525, n528);
    let n531: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n526);
    let n532: ZN = zn_mget(g.cart, n484, n531);
    let n533: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n532);
    let n534: ZN = zn_mul(n526, zn_splat(P8::from_raw(524288i32)));
    let n535: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n534);
    let n536: ZB = zn_eq(n475, n535);
    let n537: ZB = zb_or(n489, n536);
    let n538: ZB = zb_and(n533, n537);
    let n539: ZB = zb_and(n495, n538);
    let n540: ZB = zb_not(n539);
    let n541: ZB = zb_and(n529, n540);
    let n542: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n532);
    let n543: ZB = zb_and(n501, n542);
    let n544: ZB = zb_not(n543);
    let n545: ZB = zb_and(n541, n543);
    let n546: ZB = zb_and(n541, n544);
    let n547: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n532);
    let n548: ZB = zb_and(n508, n547);
    let n549: ZB = zb_and(n510, n548);
    let n550: ZB = zb_not(n549);
    let n551: ZB = zb_and(n546, n550);
    let n552: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n532);
    let n553: ZB = zb_and(n520, n552);
    let n554: ZB = zb_and(n522, n553);
    let n555: ZB = zb_not(n554);
    let n556: ZB = zb_and(n551, n555);
    let n557: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n474);
    let n558: ZB = zn_le(n557, n478);
    let n559: ZB = zn_gt(n557, n478);
    let n560: ZB = zb_and(n556, n558);
    let n561: ZB = zb_and(n556, n559);
    let n562: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n557);
    let n563: ZN = zn_mget(g.cart, n484, n562);
    let n564: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n563);
    let n565: ZN = zn_mul(n557, zn_splat(P8::from_raw(524288i32)));
    let n566: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n565);
    let n567: ZB = zn_eq(n475, n566);
    let n568: ZB = zb_or(n489, n567);
    let n569: ZB = zb_and(n564, n568);
    let n570: ZB = zb_and(n495, n569);
    let n571: ZB = zb_not(n570);
    let n572: ZB = zb_and(n560, n571);
    let n573: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n563);
    let n574: ZB = zb_and(n501, n573);
    let n575: ZB = zb_not(n574);
    let n576: ZB = zb_and(n572, n574);
    let n577: ZB = zb_and(n572, n575);
    let n578: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n563);
    let n579: ZB = zb_and(n508, n578);
    let n580: ZB = zb_and(n510, n579);
    let n581: ZB = zb_not(n580);
    let n582: ZB = zb_and(n577, n581);
    let n583: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n563);
    let n584: ZB = zb_and(n520, n583);
    let n585: ZB = zb_and(n522, n584);
    let n586: ZB = zb_not(n585);
    let n587: ZB = zb_and(n582, n586);
    let n588: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n474);
    let n589: ZB = zn_gt(n588, n478);
    let n590: ZB = zb_and(n458, n589);
    let n591: ZB = zb_or(n561, n587);
    let n592: ZB = zsel_b(n559, n458, n590);
    let n593: ZB = zb_or(n530, n591);
    let n594: ZB = zsel_b(n528, n458, n592);
    let n595: ZB = zb_or(n483, n593);
    let n596: ZB = zsel_b(n481, n458, n594);
    let n597: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n462);
    let n598: ZB = zn_le(n597, n466);
    let n599: ZB = zn_gt(n597, n466);
    let n600: ZB = zb_and(n595, n598);
    let n601: ZB = zb_and(n595, n599);
    let n602: ZB = zb_and(n481, n600);
    let n603: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n597);
    let n604: ZN = zn_mget(g.cart, n603, n485);
    let n605: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n604);
    let n606: ZB = zb_and(n480, n595);
    let n607: ZB = zb_and(n598, n606);
    let n608: ZB = zb_and(n493, n605);
    let n609: ZB = zb_and(n495, n608);
    let n610: ZB = zb_not(n609);
    let n611: ZB = zb_and(n607, n610);
    let n612: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n604);
    let n613: ZB = zb_and(n501, n612);
    let n614: ZB = zb_not(n613);
    let n615: ZB = zb_and(n611, n613);
    let n616: ZB = zb_and(n611, n614);
    let n617: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n604);
    let n618: ZB = zb_and(n508, n617);
    let n619: ZB = zb_and(n510, n618);
    let n620: ZB = zb_not(n619);
    let n621: ZB = zb_and(n616, n620);
    let n622: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n604);
    let n623: ZN = zn_mul(n597, zn_splat(P8::from_raw(524288i32)));
    let n624: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n623);
    let n625: ZB = zn_eq(n463, n624);
    let n626: ZB = zb_or(n516, n625);
    let n627: ZB = zb_and(n622, n626);
    let n628: ZB = zb_and(n522, n627);
    let n629: ZB = zb_not(n628);
    let n630: ZB = zb_and(n621, n629);
    let n631: ZB = zb_and(n528, n630);
    let n632: ZN = zn_mget(g.cart, n603, n531);
    let n633: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n632);
    let n634: ZB = zb_and(n527, n621);
    let n635: ZB = zb_and(n629, n634);
    let n636: ZB = zb_and(n537, n633);
    let n637: ZB = zb_and(n495, n636);
    let n638: ZB = zb_not(n637);
    let n639: ZB = zb_and(n635, n638);
    let n640: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n632);
    let n641: ZB = zb_and(n501, n640);
    let n642: ZB = zb_not(n641);
    let n643: ZB = zb_and(n639, n641);
    let n644: ZB = zb_and(n639, n642);
    let n645: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n632);
    let n646: ZB = zb_and(n508, n645);
    let n647: ZB = zb_and(n510, n646);
    let n648: ZB = zb_not(n647);
    let n649: ZB = zb_and(n644, n648);
    let n650: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n632);
    let n651: ZB = zb_and(n626, n650);
    let n652: ZB = zb_and(n522, n651);
    let n653: ZB = zb_not(n652);
    let n654: ZB = zb_and(n649, n653);
    let n655: ZB = zb_and(n559, n654);
    let n656: ZN = zn_mget(g.cart, n603, n562);
    let n657: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n656);
    let n658: ZB = zb_and(n558, n649);
    let n659: ZB = zb_and(n653, n658);
    let n660: ZB = zb_and(n568, n657);
    let n661: ZB = zb_and(n495, n660);
    let n662: ZB = zb_not(n661);
    let n663: ZB = zb_and(n659, n662);
    let n664: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n656);
    let n665: ZB = zb_and(n501, n664);
    let n666: ZB = zb_not(n665);
    let n667: ZB = zb_and(n663, n665);
    let n668: ZB = zb_and(n663, n666);
    let n669: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n656);
    let n670: ZB = zb_and(n508, n669);
    let n671: ZB = zb_and(n510, n670);
    let n672: ZB = zb_not(n671);
    let n673: ZB = zb_and(n668, n672);
    let n674: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n656);
    let n675: ZB = zb_and(n626, n674);
    let n676: ZB = zb_and(n522, n675);
    let n677: ZB = zb_not(n676);
    let n678: ZB = zb_and(n673, n677);
    let n679: ZB = zb_and(n589, n596);
    let n680: ZB = zb_or(n655, n678);
    let n681: ZB = zsel_b(n559, n596, n679);
    let n682: ZB = zb_or(n631, n680);
    let n683: ZB = zsel_b(n528, n596, n681);
    let n684: ZB = zb_or(n602, n682);
    let n685: ZB = zsel_b(n481, n596, n683);
    let n686: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n462);
    let n687: ZB = zn_le(n686, n466);
    let n688: ZB = zn_gt(n686, n466);
    let n689: ZB = zb_and(n684, n687);
    let n690: ZB = zb_and(n684, n688);
    let n691: ZB = zb_and(n481, n689);
    let n692: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n686);
    let n693: ZN = zn_mget(g.cart, n692, n485);
    let n694: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n693);
    let n695: ZB = zb_and(n480, n684);
    let n696: ZB = zb_and(n687, n695);
    let n697: ZB = zb_and(n493, n694);
    let n698: ZB = zb_and(n495, n697);
    let n699: ZB = zb_not(n698);
    let n700: ZB = zb_and(n696, n699);
    let n701: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n693);
    let n702: ZB = zb_and(n501, n701);
    let n703: ZB = zb_not(n702);
    let n704: ZB = zb_and(n700, n702);
    let n705: ZB = zb_and(n700, n703);
    let n706: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n693);
    let n707: ZB = zb_and(n508, n706);
    let n708: ZB = zb_and(n510, n707);
    let n709: ZB = zb_not(n708);
    let n710: ZB = zb_and(n705, n709);
    let n711: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n693);
    let n712: ZN = zn_mul(n686, zn_splat(P8::from_raw(524288i32)));
    let n713: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n712);
    let n714: ZB = zn_eq(n463, n713);
    let n715: ZB = zb_or(n516, n714);
    let n716: ZB = zb_and(n711, n715);
    let n717: ZB = zb_and(n522, n716);
    let n718: ZB = zb_not(n717);
    let n719: ZB = zb_and(n710, n718);
    let n720: ZB = zb_and(n528, n719);
    let n721: ZN = zn_mget(g.cart, n692, n531);
    let n722: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n721);
    let n723: ZB = zb_and(n527, n710);
    let n724: ZB = zb_and(n718, n723);
    let n725: ZB = zb_and(n537, n722);
    let n726: ZB = zb_and(n495, n725);
    let n727: ZB = zb_not(n726);
    let n728: ZB = zb_and(n724, n727);
    let n729: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n721);
    let n730: ZB = zb_and(n501, n729);
    let n731: ZB = zb_not(n730);
    let n732: ZB = zb_and(n728, n730);
    let n733: ZB = zb_and(n728, n731);
    let n734: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n721);
    let n735: ZB = zb_and(n508, n734);
    let n736: ZB = zb_and(n510, n735);
    let n737: ZB = zb_not(n736);
    let n738: ZB = zb_and(n733, n737);
    let n739: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n721);
    let n740: ZB = zb_and(n715, n739);
    let n741: ZB = zb_and(n522, n740);
    let n742: ZB = zb_not(n741);
    let n743: ZB = zb_and(n738, n742);
    let n744: ZB = zb_and(n559, n743);
    let n745: ZN = zn_mget(g.cart, n692, n562);
    let n746: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n745);
    let n747: ZB = zb_and(n558, n738);
    let n748: ZB = zb_and(n742, n747);
    let n749: ZB = zb_and(n568, n746);
    let n750: ZB = zb_and(n495, n749);
    let n751: ZB = zb_not(n750);
    let n752: ZB = zb_and(n748, n751);
    let n753: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n745);
    let n754: ZB = zb_and(n501, n753);
    let n755: ZB = zb_not(n754);
    let n756: ZB = zb_and(n752, n754);
    let n757: ZB = zb_and(n752, n755);
    let n758: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n745);
    let n759: ZB = zb_and(n508, n758);
    let n760: ZB = zb_and(n510, n759);
    let n761: ZB = zb_not(n760);
    let n762: ZB = zb_and(n757, n761);
    let n763: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n745);
    let n764: ZB = zb_and(n715, n763);
    let n765: ZB = zb_and(n522, n764);
    let n766: ZB = zb_not(n765);
    let n767: ZB = zb_and(n762, n766);
    let n768: ZB = zb_and(n589, n685);
    let n769: ZB = zb_or(n744, n767);
    let n770: ZB = zsel_b(n559, n685, n768);
    let n771: ZB = zb_or(n720, n769);
    let n772: ZB = zsel_b(n528, n685, n770);
    let n773: ZB = zb_or(n691, n771);
    let n774: ZB = zsel_b(n481, n685, n772);
    let n775: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n462);
    let n776: ZB = zn_gt(n775, n466);
    let n777: ZB = zb_and(n774, n776);
    let n778: ZB = zb_or(n690, n773);
    let n779: ZB = zsel_b(n688, n685, n777);
    let n780: ZB = zb_or(n601, n778);
    let n781: ZB = zsel_b(n599, n596, n779);
    let n782: ZB = zb_or(n471, n780);
    let n783: ZB = zsel_b(n469, n458, n781);
    let n784: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n459);
    let n785: ZB = zn_tile_flag_at(g.cache, g.cart, n358, n784, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n786: ZB = zb_not(n785);
    let n787: ZB = zb_not(r_c292);
    let n788: ZB = zn_gt(r_c285, zn_splat(P8::from_raw(0i32)));
    let n789: ZN = zn_sub(r_c285, zn_splat(P8::from_raw(65536i32)));
    let n790: ZN = zsel_n(n788, n789, r_c285);
    let n791: ZN = zsel_n(n785, zn_splat(P8::from_raw(393216i32)), n790);
    let n792: ZN = zn_sub(r_c280, zn_splat(P8::from_raw(65536i32)));
    let n793: ZB = zn_gt(r_c282, zn_splat(P8::from_raw(0i32)));
    let n794: ZN = zn_sub(r_c282, zn_splat(P8::from_raw(65536i32)));
    let n795: ZB = zn_gt(n345, r_c358);
    let n796: ZN = zn_sub(n345, r_c356);
    let n797: ZN = zn_max(r_c358, n796);
    let n798: ZN = zn_add(r_c356, n345);
    let n799: ZN = zn_min(r_c358, n798);
    let n800: ZN = zsel_n(n795, n797, n799);
    let n801: ZB = zn_gt(n457, r_c359);
    let n802: ZN = zn_sub(n457, r_c357);
    let n803: ZN = zn_max(r_c359, n802);
    let n804: ZN = zn_add(r_c357, n457);
    let n805: ZN = zn_min(r_c359, n804);
    let n806: ZN = zsel_n(n801, n803, n805);
    let n807: ZN = zsel_n(n786, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n808: ZN = zn_abs(n345);
    let n809: ZB = zn_gt(n808, zn_splat(P8::from_raw(65536i32)));
    let n810: ZB = zn_gt(n345, zn_splat(P8::from_raw(0i32)));
    let n811: ZB = zn_lt(n345, zn_splat(P8::from_raw(0i32)));
    let n812: ZB = zn_gt(n345, zn_splat(P8::from_raw(65536i32)));
    let n813: ZN = zn_sub(n345, zn_splat(P8::from_raw(9830i32)));
    let n814: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n813);
    let n815: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n345);
    let n816: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n815);
    let n817: ZB = zn_gt(n345, zn_splat(P8::from_raw(-65536i32)));
    let n818: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n813);
    let n819: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n815);
    let n820: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n813);
    let n821: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n815);
    let n822: ZN = zsel_n(n817, n818, n819);
    let n823: ZN = zsel_n(n810, n820, n821);
    let n824: ZN = zsel_n(n812, n814, n816);
    let n825: ZN = zsel_n(n811, n822, n823);
    let n826: ZN = zsel_n(n810, n824, n825);
    let n827: ZN = zn_sub(n345, n807);
    let n828: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n827);
    let n829: ZN = zn_add(n345, n807);
    let n830: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n829);
    let n831: ZN = zsel_n(n810, n828, n830);
    let n832: ZN = zsel_n(n809, n826, n831);
    let n833: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n832);
    let n834: ZB = zb_not(n833);
    let n835: ZB = zn_lt(n832, zn_splat(P8::from_raw(0i32)));
    let n836: ZB = zsel_b(n834, n835, r_c360);
    let n837: ZN = zn_abs(n457);
    let n838: ZB = zn_le(n837, zn_splat(P8::from_raw(9830i32)));
    let n839: ZN = zsel_n(n838, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n840: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n459);
    let n841: ZN = zn_add(n457, n839);
    let n842: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n841);
    let n843: ZN = zsel_n(n786, n842, n457);
    let n844: ZB = zn_gt(n791, zn_splat(P8::from_raw(0i32)));
    let n845: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n357);
    let n846: ZB = zn_tile_flag_at(g.cache, g.cart, n845, n840, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n847: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n357);
    let n848: ZB = zn_tile_flag_at(g.cache, g.cart, n847, n840, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n849: ZN = zsel_n(n848, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n850: ZN = zsel_n(n846, zn_splat(P8::from_raw(-65536i32)), n849);
    let n851: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n850);
    let n852: ZB = zb_not(n851);
    let n853: ZN = zn_neg(n850);
    let n854: ZN = zn_mul(n853, zn_splat(P8::from_raw(131072i32)));
    let n855: ZN = zsel_n(n852, n854, n832);
    let n856: ZN = zsel_n(n852, zn_splat(P8::from_raw(-131072i32)), n843);
    let n857: ZN = zsel_n(n844, zn_splat(P8::from_raw(0i32)), n791);
    let n858: ZN = zsel_n(n844, n832, n855);
    let n859: ZN = zsel_n(n844, zn_splat(P8::from_raw(-131072i32)), n856);
    let n860: ZN = zsel_n(n836, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n861: ZB = zn_gt(n860, zn_splat(P8::from_raw(0i32)));
    let n862: ZB = zn_lt(n860, zn_splat(P8::from_raw(0i32)));
    let n863: ZN = zsel_n(n862, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n864: ZN = zsel_n(n861, zn_splat(P8::from_raw(131072i32)), n863);
    let n865: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n860);
    let n866: ZB = zb_not(n865);
    let n867: ZN = zsel_n(n866, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n868: ZN = zsel_n(n793, n794, r_c282);
    let n869: ZB = zsel_b(n793, r_c360, n836);
    let n870: ZN = zsel_n(n793, n800, n832);
    let n871: ZN = zsel_n(n793, n806, n843);
    let n872: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n874: ZB = zn_lt(n343, zn_splat(P8::from_raw(-65536i32)));
    let n875: ZB = zn_gt(n343, zn_splat(P8::from_raw(7929856i32)));
    let n876: ZB = zb_or(n874, n875);
    let n877: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n343);
    let n878: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n877);
    let n879: ZN = zsel_n(n876, n878, n343);
    let n880: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n870);
    let n881: ZN = zsel_n(n872, n343, n879);
    let n882: ZN = zsel_n(n872, n870, n880);
    let n886: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n827);
    let n887: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n829);
    let n888: ZN = zsel_n(n817, n886, n887);
    let n889: ZN = zsel_n(n809, n826, n888);
    let n890: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n889);
    let n891: ZB = zb_not(n890);
    let n892: ZB = zn_lt(n889, zn_splat(P8::from_raw(0i32)));
    let n893: ZB = zsel_b(n891, n892, r_c360);
    let n894: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n357);
    let n895: ZB = zn_tile_flag_at(g.cache, g.cart, n894, n840, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n896: ZN = zsel_n(n895, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n897: ZN = zn_min(n841, n896);
    let n898: ZN = zsel_n(n786, n897, n457);
    let n899: ZN = zsel_n(n852, n854, n889);
    let n900: ZN = zsel_n(n852, zn_splat(P8::from_raw(-131072i32)), n898);
    let n901: ZN = zsel_n(n844, n889, n899);
    let n902: ZN = zsel_n(n844, zn_splat(P8::from_raw(-131072i32)), n900);
    let n903: ZB = zsel_b(n793, r_c360, n893);
    let n904: ZN = zsel_n(n793, n800, n889);
    let n905: ZN = zsel_n(n793, n806, n898);
    let n906: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n904);
    let n907: ZN = zsel_n(n872, n904, n906);
    let n908: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n827);
    let n909: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n829);
    let n910: ZN = zsel_n(n812, n908, n909);
    let n911: ZN = zsel_n(n809, n826, n910);
    let n912: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n911);
    let n913: ZB = zb_not(n912);
    let n914: ZB = zn_lt(n911, zn_splat(P8::from_raw(0i32)));
    let n915: ZB = zsel_b(n913, n914, r_c360);
    let n916: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n357);
    let n917: ZB = zn_tile_flag_at(g.cache, g.cart, n916, n840, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n918: ZN = zsel_n(n917, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n919: ZN = zn_min(n841, n918);
    let n920: ZN = zsel_n(n786, n919, n457);
    let n921: ZN = zsel_n(n852, n854, n911);
    let n922: ZN = zsel_n(n852, zn_splat(P8::from_raw(-131072i32)), n920);
    let n923: ZN = zsel_n(n844, n911, n921);
    let n924: ZN = zsel_n(n844, zn_splat(P8::from_raw(-131072i32)), n922);
    let n925: ZB = zsel_b(n793, r_c360, n915);
    let n926: ZN = zsel_n(n793, n800, n911);
    let n927: ZN = zsel_n(n793, n806, n920);
    let n928: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n926);
    let n929: ZN = zsel_n(n872, n926, n928);
    let n930: ZN = zsel_n(n136, n857, n791);
    let n931: ZN = zsel_n(n136, n858, n832);
    let n932: ZN = zsel_n(n136, n859, n843);
    let n933: ZN = zsel_n(n793, n791, n930);
    let n934: ZN = zsel_n(n793, n800, n931);
    let n935: ZN = zsel_n(n793, n806, n932);
    let n936: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n934);
    let n937: ZN = zsel_n(n872, n934, n936);
    let n938: ZN = zsel_n(n136, n901, n889);
    let n939: ZN = zsel_n(n136, n902, n898);
    let n940: ZN = zsel_n(n793, n800, n938);
    let n941: ZN = zsel_n(n793, n806, n939);
    let n942: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n940);
    let n943: ZN = zsel_n(n872, n940, n942);
    let n944: ZN = zsel_n(n136, n923, n911);
    let n945: ZN = zsel_n(n136, n924, n920);
    let n946: ZN = zsel_n(n793, n800, n944);
    let n947: ZN = zsel_n(n793, n806, n945);
    let n948: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n946);
    let n949: ZN = zsel_n(n872, n946, n948);
    let n950: ZN = zsel_n(n787, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n951: ZB = zb_or(r_c41, n787);
    let n952: ZN = zsel_n(n787, zn_splat(P8::from_raw(655360i32)), n792);
    let n953: ZN = zsel_n(n787, zn_splat(P8::from_raw(262144i32)), r_c282);
    let n954: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), zn_splat(P8::from_raw(65536i32)));
    let n955: ZN = zsel_n(n787, zn_splat(P8::from_raw(98304i32)), r_c356);
    let n956: ZN = zsel_n(n787, n867, r_c357);
    let n957: ZN = zsel_n(n787, n864, r_c358);
    let n958: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), r_c359);
    let n959: ZN = zsel_n(n787, n860, n832);
    let n960: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n843);
    let n961: ZN = zsel_n(n793, r_c20, n950);
    let n962: ZB = zsel_b(n793, r_c41, n951);
    let n963: ZN = zsel_n(n793, n792, n952);
    let n964: ZN = zsel_n(n793, n794, n953);
    let n965: ZN = zsel_n(n793, zn_splat(P8::from_raw(65536i32)), n954);
    let n966: ZN = zsel_n(n793, r_c356, n955);
    let n967: ZN = zsel_n(n793, r_c357, n956);
    let n968: ZN = zsel_n(n793, r_c358, n957);
    let n969: ZN = zsel_n(n793, r_c359, n958);
    let n970: ZN = zsel_n(n793, n800, n959);
    let n971: ZN = zsel_n(n793, n806, n960);
    let n972: ZB = zn_gt(n961, zn_splat(P8::from_raw(0i32)));
    let n973: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n970);
    let n974: ZN = zsel_n(n972, n343, n879);
    let n975: ZN = zsel_n(n972, n970, n973);
    let n976: ZN = zsel_n(n787, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n977: ZN = zsel_n(n787, zn_splat(P8::from_raw(-131072i32)), r_c358);
    let n978: ZN = zsel_n(n787, zn_splat(P8::from_raw(-327680i32)), n889);
    let n979: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n898);
    let n980: ZN = zsel_n(n793, r_c357, n976);
    let n981: ZN = zsel_n(n793, r_c358, n977);
    let n982: ZN = zsel_n(n793, n800, n978);
    let n983: ZN = zsel_n(n793, n806, n979);
    let n984: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n982);
    let n985: ZN = zsel_n(n972, n982, n984);
    let n986: ZN = zsel_n(n787, zn_splat(P8::from_raw(131072i32)), r_c358);
    let n987: ZN = zsel_n(n787, zn_splat(P8::from_raw(327680i32)), n911);
    let n988: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n920);
    let n989: ZN = zsel_n(n793, r_c358, n986);
    let n990: ZN = zsel_n(n793, n800, n987);
    let n991: ZN = zsel_n(n793, n806, n988);
    let n992: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n990);
    let n993: ZN = zsel_n(n972, n990, n992);
    let n995: ZN = zsel_n(n787, zn_splat(P8::from_raw(69510i32)), r_c356);
    let n996: ZN = zsel_n(n787, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n997: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), r_c358);
    let n998: ZN = zsel_n(n787, zn_splat(P8::from_raw(-98304i32)), r_c359);
    let n999: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n832);
    let n1000: ZN = zsel_n(n787, zn_splat(P8::from_raw(-327680i32)), n843);
    let n1001: ZN = zsel_n(n793, r_c356, n995);
    let n1002: ZN = zsel_n(n793, r_c357, n996);
    let n1003: ZN = zsel_n(n793, r_c358, n997);
    let n1004: ZN = zsel_n(n793, r_c359, n998);
    let n1005: ZN = zsel_n(n793, n800, n999);
    let n1006: ZN = zsel_n(n793, n806, n1000);
    let n1007: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1005);
    let n1008: ZN = zsel_n(n972, n1005, n1007);
    let n1009: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n889);
    let n1010: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n898);
    let n1011: ZN = zsel_n(n793, n800, n1009);
    let n1012: ZN = zsel_n(n793, n806, n1010);
    let n1013: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1011);
    let n1014: ZN = zsel_n(n972, n1011, n1013);
    let n1015: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n911);
    let n1016: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n920);
    let n1017: ZN = zsel_n(n793, n800, n1015);
    let n1018: ZN = zsel_n(n793, n806, n1016);
    let n1019: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1017);
    let n1020: ZN = zsel_n(n972, n1017, n1019);
    let n1021: ZN = zsel_n(n787, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n1022: ZN = zsel_n(n787, zn_splat(P8::from_raw(327680i32)), n843);
    let n1023: ZN = zsel_n(n793, r_c359, n1021);
    let n1024: ZN = zsel_n(n793, n806, n1022);
    let n1025: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n898);
    let n1026: ZN = zsel_n(n793, n806, n1025);
    let n1027: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n920);
    let n1028: ZN = zsel_n(n793, n806, n1027);
    let n1029: ZN = zsel_n(n787, n860, n931);
    let n1030: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n932);
    let n1031: ZN = zsel_n(n793, n800, n1029);
    let n1032: ZN = zsel_n(n793, n806, n1030);
    let n1033: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1031);
    let n1034: ZN = zsel_n(n972, n1031, n1033);
    let n1035: ZN = zsel_n(n787, zn_splat(P8::from_raw(-327680i32)), n938);
    let n1036: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n939);
    let n1037: ZN = zsel_n(n793, n800, n1035);
    let n1038: ZN = zsel_n(n793, n806, n1036);
    let n1039: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1037);
    let n1040: ZN = zsel_n(n972, n1037, n1039);
    let n1041: ZN = zsel_n(n787, zn_splat(P8::from_raw(327680i32)), n944);
    let n1042: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n945);
    let n1043: ZN = zsel_n(n793, n800, n1041);
    let n1044: ZN = zsel_n(n793, n806, n1042);
    let n1045: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1043);
    let n1046: ZN = zsel_n(n972, n1043, n1045);
    let n1047: ZN = zsel_n(n787, zn_splat(P8::from_raw(0i32)), n931);
    let n1048: ZN = zsel_n(n787, zn_splat(P8::from_raw(-327680i32)), n932);
    let n1049: ZN = zsel_n(n793, n800, n1047);
    let n1050: ZN = zsel_n(n793, n806, n1048);
    let n1051: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1049);
    let n1052: ZN = zsel_n(n972, n1049, n1051);
    let n1053: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n938);
    let n1054: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n939);
    let n1055: ZN = zsel_n(n793, n800, n1053);
    let n1056: ZN = zsel_n(n793, n806, n1054);
    let n1057: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1055);
    let n1058: ZN = zsel_n(n972, n1055, n1057);
    let n1059: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n944);
    let n1060: ZN = zsel_n(n787, zn_splat(P8::from_raw(-231700i32)), n945);
    let n1061: ZN = zsel_n(n793, n800, n1059);
    let n1062: ZN = zsel_n(n793, n806, n1060);
    let n1063: ZN = zsel_n(n876, zn_splat(P8::from_raw(0i32)), n1061);
    let n1064: ZN = zsel_n(n972, n1061, n1063);
    let n1065: ZN = zsel_n(n787, zn_splat(P8::from_raw(327680i32)), n932);
    let n1066: ZN = zsel_n(n793, n806, n1065);
    let n1067: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n939);
    let n1068: ZN = zsel_n(n793, n806, n1067);
    let n1069: ZN = zsel_n(n787, zn_splat(P8::from_raw(231700i32)), n945);
    let n1070: ZN = zsel_n(n793, n806, n1069);
    let n1071: ZB = zb_not(n228);
    let n1072: ZB = zb_and(n137, n1071);
    let n1073: ZB = zn_lt(r_c369, zn_splat(P8::from_raw(0i32)));
    let n1074: ZB = zb_and(n229, n1073);
    let n1075: ZB = zb_or(n1072, n1074);
    let n1076: ZB = zb_and(n223, n232);
    let n1077: ZB = zb_and(n237, n1076);
    let n1078: ZB = zb_and(n227, n1077);
    let n1079: ZB = zb_and(n1075, n1078);
    let n1080: ZB = zb_and(n223, n227);
    let n1081: ZB = zb_and(n230, n1080);
    let n1082: ZB = zb_and(n232, n1081);
    let n1083: ZB = zb_and(n237, n1082);
    let n1084: ZB = zb_and(n1075, n1083);
    let n1085: ZB = zb_and(n468, n1084);
    let n1086: ZB = zb_and(n469, n1084);
    let n1087: ZB = zb_and(n481, n1085);
    let n1088: ZB = zb_and(n468, n480);
    let n1089: ZB = zb_and(n1084, n1088);
    let n1090: ZB = zb_and(n497, n1089);
    let n1091: ZB = zb_and(n502, n1090);
    let n1092: ZB = zb_and(n503, n1090);
    let n1093: ZB = zb_and(n512, n1092);
    let n1094: ZB = zb_and(n524, n1093);
    let n1095: ZB = zb_and(n528, n1094);
    let n1096: ZB = zb_and(n524, n527);
    let n1097: ZB = zb_and(n1093, n1096);
    let n1098: ZB = zb_and(n540, n1097);
    let n1099: ZB = zb_and(n543, n1098);
    let n1100: ZB = zb_and(n544, n1098);
    let n1101: ZB = zb_and(n550, n1100);
    let n1102: ZB = zb_and(n555, n1101);
    let n1103: ZB = zb_and(n559, n1102);
    let n1104: ZB = zb_and(n555, n558);
    let n1105: ZB = zb_and(n1101, n1104);
    let n1106: ZB = zb_and(n571, n1105);
    let n1107: ZB = zb_and(n574, n1106);
    let n1108: ZB = zb_and(n575, n1106);
    let n1109: ZB = zb_and(n581, n1108);
    let n1110: ZB = zb_and(n586, n1109);
    let n1111: ZB = zb_or(n1103, n1110);
    let n1112: ZB = zb_or(n1095, n1111);
    let n1113: ZB = zb_or(n1087, n1112);
    let n1114: ZB = zb_and(n598, n1113);
    let n1115: ZB = zb_and(n599, n1113);
    let n1116: ZB = zb_and(n481, n1114);
    let n1117: ZB = zb_and(n480, n598);
    let n1118: ZB = zb_and(n1113, n1117);
    let n1119: ZB = zb_and(n610, n1118);
    let n1120: ZB = zb_and(n613, n1119);
    let n1121: ZB = zb_and(n614, n1119);
    let n1122: ZB = zb_and(n620, n1121);
    let n1123: ZB = zb_and(n629, n1122);
    let n1124: ZB = zb_and(n528, n1123);
    let n1125: ZB = zb_and(n527, n629);
    let n1126: ZB = zb_and(n1122, n1125);
    let n1127: ZB = zb_and(n638, n1126);
    let n1128: ZB = zb_and(n641, n1127);
    let n1129: ZB = zb_and(n642, n1127);
    let n1130: ZB = zb_and(n648, n1129);
    let n1131: ZB = zb_and(n653, n1130);
    let n1132: ZB = zb_and(n559, n1131);
    let n1133: ZB = zb_and(n558, n653);
    let n1134: ZB = zb_and(n1130, n1133);
    let n1135: ZB = zb_and(n662, n1134);
    let n1136: ZB = zb_and(n665, n1135);
    let n1137: ZB = zb_and(n666, n1135);
    let n1138: ZB = zb_and(n672, n1137);
    let n1139: ZB = zb_and(n677, n1138);
    let n1140: ZB = zb_or(n1132, n1139);
    let n1141: ZB = zb_or(n1124, n1140);
    let n1142: ZB = zb_or(n1116, n1141);
    let n1143: ZB = zb_and(n687, n1142);
    let n1144: ZB = zb_and(n688, n1142);
    let n1145: ZB = zb_and(n481, n1143);
    let n1146: ZB = zb_and(n480, n687);
    let n1147: ZB = zb_and(n1142, n1146);
    let n1148: ZB = zb_and(n699, n1147);
    let n1149: ZB = zb_and(n702, n1148);
    let n1150: ZB = zb_and(n703, n1148);
    let n1151: ZB = zb_and(n709, n1150);
    let n1152: ZB = zb_and(n718, n1151);
    let n1153: ZB = zb_and(n528, n1152);
    let n1154: ZB = zb_and(n527, n718);
    let n1155: ZB = zb_and(n1151, n1154);
    let n1156: ZB = zb_and(n727, n1155);
    let n1157: ZB = zb_and(n730, n1156);
    let n1158: ZB = zb_and(n731, n1156);
    let n1159: ZB = zb_and(n737, n1158);
    let n1160: ZB = zb_and(n742, n1159);
    let n1161: ZB = zb_and(n559, n1160);
    let n1162: ZB = zb_and(n558, n742);
    let n1163: ZB = zb_and(n1159, n1162);
    let n1164: ZB = zb_and(n751, n1163);
    let n1165: ZB = zb_and(n754, n1164);
    let n1166: ZB = zb_and(n755, n1164);
    let n1167: ZB = zb_and(n761, n1166);
    let n1168: ZB = zb_and(n766, n1167);
    let n1169: ZB = zb_or(n1161, n1168);
    let n1170: ZB = zb_or(n1153, n1169);
    let n1171: ZB = zb_or(n1145, n1170);
    let n1172: ZB = zb_or(n1144, n1171);
    let n1173: ZB = zb_or(n1115, n1172);
    let n1174: ZB = zb_or(n1086, n1173);
    let n1176: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n1177: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1176);
    let n1178: ZB = zb_and(n482, n496);
    let n1179: ZB = zb_and(n505, n511);
    let n1180: ZB = zb_and(n513, n523);
    let n1181: ZB = zb_or(n1179, n1180);
    let n1182: ZB = zb_or(n504, n1181);
    let n1183: ZB = zb_or(n1178, n1182);
    let n1184: ZB = zb_and(n529, n539);
    let n1185: ZB = zb_and(n546, n549);
    let n1186: ZB = zb_and(n551, n554);
    let n1187: ZB = zb_or(n1185, n1186);
    let n1188: ZB = zb_or(n545, n1187);
    let n1189: ZB = zb_or(n1184, n1188);
    let n1190: ZB = zb_and(n560, n570);
    let n1191: ZB = zb_and(n577, n580);
    let n1192: ZB = zb_and(n582, n585);
    let n1193: ZB = zb_or(n1191, n1192);
    let n1194: ZB = zb_or(n576, n1193);
    let n1195: ZB = zb_or(n1190, n1194);
    let n1196: ZB = zb_or(n1189, n1195);
    let n1197: ZB = zb_or(n1183, n1196);
    let n1198: ZB = zb_and(n607, n609);
    let n1199: ZB = zb_and(n616, n619);
    let n1200: ZB = zb_and(n621, n628);
    let n1201: ZB = zb_or(n1199, n1200);
    let n1202: ZB = zb_or(n615, n1201);
    let n1203: ZB = zb_or(n1198, n1202);
    let n1204: ZB = zb_and(n635, n637);
    let n1205: ZB = zb_and(n644, n647);
    let n1206: ZB = zb_and(n649, n652);
    let n1207: ZB = zb_or(n1205, n1206);
    let n1208: ZB = zb_or(n643, n1207);
    let n1209: ZB = zb_or(n1204, n1208);
    let n1210: ZB = zb_and(n659, n661);
    let n1211: ZB = zb_and(n668, n671);
    let n1212: ZB = zb_and(n673, n676);
    let n1213: ZB = zb_or(n1211, n1212);
    let n1214: ZB = zb_or(n667, n1213);
    let n1215: ZB = zb_or(n1210, n1214);
    let n1216: ZB = zb_or(n1209, n1215);
    let n1217: ZB = zb_or(n1203, n1216);
    let n1218: ZB = zb_and(n696, n698);
    let n1219: ZB = zb_and(n705, n708);
    let n1220: ZB = zb_and(n710, n717);
    let n1221: ZB = zb_or(n1219, n1220);
    let n1222: ZB = zb_or(n704, n1221);
    let n1223: ZB = zb_or(n1218, n1222);
    let n1224: ZB = zb_and(n724, n726);
    let n1225: ZB = zb_and(n733, n736);
    let n1226: ZB = zb_and(n738, n741);
    let n1227: ZB = zb_or(n1225, n1226);
    let n1228: ZB = zb_or(n732, n1227);
    let n1229: ZB = zb_or(n1224, n1228);
    let n1230: ZB = zb_and(n748, n750);
    let n1231: ZB = zb_and(n757, n760);
    let n1232: ZB = zb_and(n762, n765);
    let n1233: ZB = zb_or(n1231, n1232);
    let n1234: ZB = zb_or(n756, n1233);
    let n1235: ZB = zb_or(n1230, n1234);
    let n1236: ZB = zb_or(n1229, n1235);
    let n1237: ZB = zb_or(n1223, n1236);
    let n1238: ZB = zb_or(n1217, n1237);
    let n1239: ZB = zsel_b(n1217, n596, n685);
    let n1240: ZB = zb_or(n1197, n1238);
    let n1241: ZB = zsel_b(n1197, n458, n1239);
    let n1242: ZB = zsel_b(n1240, n1241, n783);
    let n1244: ZB = zb_and(n496, n1089);
    let n1245: ZB = zb_and(n511, n1092);
    let n1246: ZB = zb_and(n523, n1093);
    let n1247: ZB = zb_or(n1245, n1246);
    let n1248: ZB = zb_or(n1091, n1247);
    let n1249: ZB = zb_or(n1244, n1248);
    let n1250: ZB = zb_and(n539, n1097);
    let n1251: ZB = zb_and(n549, n1100);
    let n1252: ZB = zb_and(n554, n1101);
    let n1253: ZB = zb_or(n1251, n1252);
    let n1254: ZB = zb_or(n1099, n1253);
    let n1255: ZB = zb_or(n1250, n1254);
    let n1256: ZB = zb_and(n570, n1105);
    let n1257: ZB = zb_and(n580, n1108);
    let n1258: ZB = zb_and(n585, n1109);
    let n1259: ZB = zb_or(n1257, n1258);
    let n1260: ZB = zb_or(n1107, n1259);
    let n1261: ZB = zb_or(n1256, n1260);
    let n1262: ZB = zb_or(n1255, n1261);
    let n1263: ZB = zb_or(n1249, n1262);
    let n1264: ZB = zb_and(n609, n1118);
    let n1265: ZB = zb_and(n619, n1121);
    let n1266: ZB = zb_and(n628, n1122);
    let n1267: ZB = zb_or(n1265, n1266);
    let n1268: ZB = zb_or(n1120, n1267);
    let n1269: ZB = zb_or(n1264, n1268);
    let n1270: ZB = zb_and(n637, n1126);
    let n1271: ZB = zb_and(n647, n1129);
    let n1272: ZB = zb_and(n652, n1130);
    let n1273: ZB = zb_or(n1271, n1272);
    let n1274: ZB = zb_or(n1128, n1273);
    let n1275: ZB = zb_or(n1270, n1274);
    let n1276: ZB = zb_and(n661, n1134);
    let n1277: ZB = zb_and(n671, n1137);
    let n1278: ZB = zb_and(n676, n1138);
    let n1279: ZB = zb_or(n1277, n1278);
    let n1280: ZB = zb_or(n1136, n1279);
    let n1281: ZB = zb_or(n1276, n1280);
    let n1282: ZB = zb_or(n1275, n1281);
    let n1283: ZB = zb_or(n1269, n1282);
    let n1284: ZB = zb_and(n698, n1147);
    let n1285: ZB = zb_and(n708, n1150);
    let n1286: ZB = zb_and(n717, n1151);
    let n1287: ZB = zb_or(n1285, n1286);
    let n1288: ZB = zb_or(n1149, n1287);
    let n1289: ZB = zb_or(n1284, n1288);
    let n1290: ZB = zb_and(n726, n1155);
    let n1291: ZB = zb_and(n736, n1158);
    let n1292: ZB = zb_and(n741, n1159);
    let n1293: ZB = zb_or(n1291, n1292);
    let n1294: ZB = zb_or(n1157, n1293);
    let n1295: ZB = zb_or(n1290, n1294);
    let n1296: ZB = zb_and(n750, n1163);
    let n1297: ZB = zb_and(n760, n1166);
    let n1298: ZB = zb_and(n765, n1167);
    let n1299: ZB = zb_or(n1297, n1298);
    let n1300: ZB = zb_or(n1165, n1299);
    let n1301: ZB = zb_or(n1296, n1300);
    let n1302: ZB = zb_or(n1295, n1301);
    let n1303: ZB = zb_or(n1289, n1302);
    let n1304: ZB = zb_or(n1283, n1303);
    let n1305: ZB = zsel_b(n1283, n596, n685);
    let n1306: ZB = zb_or(n1263, n1304);
    let n1307: ZB = zsel_b(n1263, n458, n1305);
    let n1308: ZB = zsel_b(n1306, n1307, n783);
    let n1311: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c369);
    let n1312: ZB = zb_not(n1078);
    let n1313: ZB = zb_and(n1075, n1312);
    let n1314: ZB = zb_and(n1073, n1079);
    let n1315: ZB = zb_or(n1313, n1314);
    let n1316: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c368);
    let n1317: ZB = zb_not(n1316);
    let n1318: ZB = zb_not(n1311);
    let n1319: ZB = zb_or(n1317, n1318);
    let n1320: ZB = zb_not(n1319);
    let n1321: ZN = zn_add(r_c366, r_c368);
    let n1322: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1321);
    let n1323: ZN = zn_flr(n1322);
    let n1324: ZB = zn_gt(n1323, zn_splat(P8::from_raw(0i32)));
    let n1325: ZB = zn_lt(n1323, zn_splat(P8::from_raw(0i32)));
    let n1326: ZN = zsel_n(n1325, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1327: ZN = zsel_n(n1324, zn_splat(P8::from_raw(65536i32)), n1326);
    let n1328: ZN = zn_abs(n1323);
    let n1329: ZN = zn_add(n217, n1327);
    let n1330: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n221);
    let n1331: ZB = zn_tile_flag_at(g.cache, g.cart, n1329, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1332: ZN = zn_add(r_c299, n1327);
    let n1333: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1328);
    let n1334: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1332);
    let n1335: ZN = zn_add(n1327, n1334);
    let n1336: ZB = zn_tile_flag_at(g.cache, g.cart, n1335, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1337: ZN = zn_add(n1327, n1332);
    let n1338: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1328);
    let n1339: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1337);
    let n1340: ZN = zn_add(n1327, n1339);
    let n1341: ZB = zn_tile_flag_at(g.cache, g.cart, n1340, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1342: ZN = zn_add(n1327, n1337);
    let n1343: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1328);
    let n1344: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1342);
    let n1345: ZN = zn_add(n1327, n1344);
    let n1346: ZB = zn_tile_flag_at(g.cache, g.cart, n1345, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1347: ZN = zn_add(n1327, n1342);
    let n1348: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1328);
    let n1349: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1347);
    let n1350: ZN = zn_add(n1327, n1349);
    let n1351: ZB = zn_tile_flag_at(g.cache, g.cart, n1350, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1352: ZN = zn_add(n1327, n1347);
    let n1353: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1328);
    let n1354: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1352);
    let n1355: ZN = zn_add(n1327, n1354);
    let n1356: ZB = zn_tile_flag_at(g.cache, g.cart, n1355, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1357: ZN = zn_add(n1327, n1352);
    let n1358: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1328);
    let n1359: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1357);
    let n1360: ZN = zn_add(n1327, n1359);
    let n1361: ZB = zn_tile_flag_at(g.cache, g.cart, n1360, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1362: ZN = zn_add(n1327, n1357);
    let n1363: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1328);
    let n1364: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1362);
    let n1365: ZN = zn_add(n1327, n1364);
    let n1366: ZB = zn_tile_flag_at(g.cache, g.cart, n1365, n1330, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1367: ZN = zn_add(n1327, n1362);
    let n1368: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1328);
    let n1369: ZN = zsel_n(n1366, n1362, n1367);
    let n1370: ZN = zsel_n(n1366, zn_splat(P8::from_raw(0i32)), r_c368);
    let n1371: ZB = zb_or(n1366, n1368);
    let n1372: ZN = zsel_n(n1363, n1362, n1369);
    let n1373: ZN = zsel_n(n1363, r_c368, n1370);
    let n1374: ZB = zb_or(n1363, n1371);
    let n1375: ZN = zsel_n(n1361, n1357, n1372);
    let n1376: ZN = zsel_n(n1361, zn_splat(P8::from_raw(0i32)), n1373);
    let n1377: ZB = zb_or(n1361, n1374);
    let n1378: ZN = zsel_n(n1358, n1357, n1375);
    let n1379: ZN = zsel_n(n1358, r_c368, n1376);
    let n1380: ZB = zb_or(n1358, n1377);
    let n1381: ZN = zsel_n(n1356, n1352, n1378);
    let n1382: ZN = zsel_n(n1356, zn_splat(P8::from_raw(0i32)), n1379);
    let n1383: ZB = zb_or(n1356, n1380);
    let n1384: ZN = zsel_n(n1353, n1352, n1381);
    let n1385: ZN = zsel_n(n1353, r_c368, n1382);
    let n1386: ZB = zb_or(n1353, n1383);
    let n1387: ZN = zsel_n(n1351, n1347, n1384);
    let n1388: ZN = zsel_n(n1351, zn_splat(P8::from_raw(0i32)), n1385);
    let n1389: ZB = zb_or(n1351, n1386);
    let n1390: ZN = zsel_n(n1348, n1347, n1387);
    let n1391: ZN = zsel_n(n1348, r_c368, n1388);
    let n1392: ZB = zb_or(n1348, n1389);
    let n1393: ZN = zsel_n(n1346, n1342, n1390);
    let n1394: ZN = zsel_n(n1346, zn_splat(P8::from_raw(0i32)), n1391);
    let n1395: ZB = zb_or(n1346, n1392);
    let n1396: ZN = zsel_n(n1343, n1342, n1393);
    let n1397: ZN = zsel_n(n1343, r_c368, n1394);
    let n1398: ZB = zb_or(n1343, n1395);
    let n1399: ZN = zsel_n(n1341, n1337, n1396);
    let n1400: ZN = zsel_n(n1341, zn_splat(P8::from_raw(0i32)), n1397);
    let n1401: ZB = zb_or(n1341, n1398);
    let n1402: ZN = zsel_n(n1338, n1337, n1399);
    let n1403: ZN = zsel_n(n1338, r_c368, n1400);
    let n1404: ZB = zb_or(n1338, n1401);
    let n1405: ZN = zsel_n(n1336, n1332, n1402);
    let n1406: ZN = zsel_n(n1336, zn_splat(P8::from_raw(0i32)), n1403);
    let n1407: ZB = zb_or(n1336, n1404);
    let n1408: ZN = zsel_n(n1333, n1332, n1405);
    let n1409: ZN = zsel_n(n1333, r_c368, n1406);
    let n1410: ZB = zb_or(n1333, n1407);
    let n1411: ZN = zsel_n(n1331, r_c299, n1408);
    let n1412: ZN = zsel_n(n1331, zn_splat(P8::from_raw(0i32)), n1409);
    let n1413: ZB = zb_or(n1331, n1410);
    let n1414: ZN = zn_add(r_c367, r_c369);
    let n1415: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n1414);
    let n1416: ZN = zn_flr(n1415);
    let n1417: ZB = zn_gt(n1416, zn_splat(P8::from_raw(0i32)));
    let n1418: ZB = zn_lt(n1416, zn_splat(P8::from_raw(0i32)));
    let n1419: ZN = zsel_n(n1418, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1420: ZN = zsel_n(n1417, zn_splat(P8::from_raw(65536i32)), n1419);
    let n1421: ZN = zn_abs(n1416);
    let n1422: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1411);
    let n1423: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1422);
    let n1424: ZN = zn_add(n221, n1420);
    let n1425: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1424, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1426: ZN = zn_add(r_c300, n1420);
    let n1427: ZB = zn_gt(zn_splat(P8::from_raw(65536i32)), n1421);
    let n1428: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1426);
    let n1429: ZN = zn_add(n1420, n1428);
    let n1430: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1429, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1431: ZN = zn_add(n1420, n1426);
    let n1432: ZB = zn_gt(zn_splat(P8::from_raw(131072i32)), n1421);
    let n1433: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1431);
    let n1434: ZN = zn_add(n1420, n1433);
    let n1435: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1434, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1436: ZN = zn_add(n1420, n1431);
    let n1437: ZB = zn_gt(zn_splat(P8::from_raw(196608i32)), n1421);
    let n1438: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1436);
    let n1439: ZN = zn_add(n1420, n1438);
    let n1440: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1439, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1441: ZN = zn_add(n1420, n1436);
    let n1442: ZB = zn_gt(zn_splat(P8::from_raw(262144i32)), n1421);
    let n1443: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1441);
    let n1444: ZN = zn_add(n1420, n1443);
    let n1445: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1444, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1446: ZN = zn_add(n1420, n1441);
    let n1447: ZB = zn_gt(zn_splat(P8::from_raw(327680i32)), n1421);
    let n1448: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1446);
    let n1449: ZN = zn_add(n1420, n1448);
    let n1450: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1449, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1451: ZN = zn_add(n1420, n1446);
    let n1452: ZB = zn_gt(zn_splat(P8::from_raw(393216i32)), n1421);
    let n1453: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1451);
    let n1454: ZN = zn_add(n1420, n1453);
    let n1455: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1454, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1456: ZN = zn_add(n1420, n1451);
    let n1457: ZB = zn_gt(zn_splat(P8::from_raw(458752i32)), n1421);
    let n1458: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1456);
    let n1459: ZN = zn_add(n1420, n1458);
    let n1460: ZB = zn_tile_flag_at(g.cache, g.cart, n1423, n1459, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1461: ZN = zn_add(n1420, n1456);
    let n1462: ZB = zn_gt(zn_splat(P8::from_raw(524288i32)), n1421);
    let n1463: ZB = zb_and(n1413, n1462);
    let n1464: ZN = zsel_n(n1460, n1456, n1461);
    let n1465: ZN = zsel_n(n1460, zn_splat(P8::from_raw(0i32)), r_c369);
    let n1466: ZB = zsel_b(n1460, n1413, n1463);
    let n1467: ZN = zsel_n(n1457, n1456, n1464);
    let n1468: ZN = zsel_n(n1457, r_c369, n1465);
    let n1469: ZB = zsel_b(n1457, n1413, n1466);
    let n1470: ZN = zsel_n(n1455, n1451, n1467);
    let n1471: ZN = zsel_n(n1455, zn_splat(P8::from_raw(0i32)), n1468);
    let n1472: ZB = zsel_b(n1455, n1413, n1469);
    let n1473: ZN = zsel_n(n1452, n1451, n1470);
    let n1474: ZN = zsel_n(n1452, r_c369, n1471);
    let n1475: ZB = zsel_b(n1452, n1413, n1472);
    let n1476: ZN = zsel_n(n1450, n1446, n1473);
    let n1477: ZN = zsel_n(n1450, zn_splat(P8::from_raw(0i32)), n1474);
    let n1478: ZB = zsel_b(n1450, n1413, n1475);
    let n1479: ZN = zsel_n(n1447, n1446, n1476);
    let n1480: ZN = zsel_n(n1447, r_c369, n1477);
    let n1481: ZB = zsel_b(n1447, n1413, n1478);
    let n1482: ZN = zsel_n(n1445, n1441, n1479);
    let n1483: ZN = zsel_n(n1445, zn_splat(P8::from_raw(0i32)), n1480);
    let n1484: ZB = zsel_b(n1445, n1413, n1481);
    let n1485: ZN = zsel_n(n1442, n1441, n1482);
    let n1486: ZN = zsel_n(n1442, r_c369, n1483);
    let n1487: ZB = zsel_b(n1442, n1413, n1484);
    let n1488: ZN = zsel_n(n1440, n1436, n1485);
    let n1489: ZN = zsel_n(n1440, zn_splat(P8::from_raw(0i32)), n1486);
    let n1490: ZB = zsel_b(n1440, n1413, n1487);
    let n1491: ZN = zsel_n(n1437, n1436, n1488);
    let n1492: ZN = zsel_n(n1437, r_c369, n1489);
    let n1493: ZB = zsel_b(n1437, n1413, n1490);
    let n1494: ZN = zsel_n(n1435, n1431, n1491);
    let n1495: ZN = zsel_n(n1435, zn_splat(P8::from_raw(0i32)), n1492);
    let n1496: ZB = zsel_b(n1435, n1413, n1493);
    let n1497: ZN = zsel_n(n1432, n1431, n1494);
    let n1498: ZN = zsel_n(n1432, r_c369, n1495);
    let n1499: ZB = zsel_b(n1432, n1413, n1496);
    let n1500: ZN = zsel_n(n1430, n1426, n1497);
    let n1501: ZN = zsel_n(n1430, zn_splat(P8::from_raw(0i32)), n1498);
    let n1502: ZB = zsel_b(n1430, n1413, n1499);
    let n1503: ZN = zsel_n(n1427, n1426, n1500);
    let n1504: ZN = zsel_n(n1427, r_c369, n1501);
    let n1505: ZB = zsel_b(n1427, n1413, n1502);
    let n1506: ZN = zsel_n(n1425, r_c300, n1503);
    let n1507: ZN = zsel_n(n1425, zn_splat(P8::from_raw(0i32)), n1504);
    let n1508: ZB = zsel_b(n1425, n1413, n1505);
    let n1509: ZN = zsel_n(n1319, n1411, r_c299);
    let n1510: ZN = zsel_n(n1319, n1506, r_c300);
    let n1511: ZN = zsel_n(n1319, n1412, r_c368);
    let n1512: ZN = zsel_n(n1319, n1507, r_c369);
    let n1513: ZB = zb_or(n1320, n1508);
    let n1514: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1509);
    let n1515: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1510);
    let n1516: ZN = zn_div(n1514, zn_splat(P8::from_raw(524288i32)));
    let n1517: ZN = zn_flr(n1516);
    let n1518: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1517);
    let n1519: ZN = zn_add(zn_splat(P8::from_raw(393216i32)), n1514);
    let n1520: ZN = zn_sub(n1519, zn_splat(P8::from_raw(65536i32)));
    let n1521: ZN = zn_div(n1520, zn_splat(P8::from_raw(524288i32)));
    let n1522: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1521);
    let n1523: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1518);
    let n1524: ZB = zn_le(n1523, n1522);
    let n1525: ZB = zn_gt(n1523, n1522);
    let n1526: ZB = zb_and(n1315, n1524);
    let n1527: ZB = zb_and(n1315, n1525);
    let n1528: ZN = zn_div(n1515, zn_splat(P8::from_raw(524288i32)));
    let n1529: ZN = zn_flr(n1528);
    let n1530: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1529);
    let n1531: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n1515);
    let n1532: ZN = zn_sub(n1531, zn_splat(P8::from_raw(65536i32)));
    let n1533: ZN = zn_div(n1532, zn_splat(P8::from_raw(524288i32)));
    let n1534: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n1533);
    let n1535: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1530);
    let n1536: ZB = zn_le(n1535, n1534);
    let n1537: ZB = zn_gt(n1535, n1534);
    let n1538: ZB = zb_and(n1526, n1536);
    let n1539: ZB = zb_and(n1526, n1537);
    let n1540: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1523);
    let n1541: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1535);
    let n1542: ZN = zn_mget(g.cart, n1540, n1541);
    let n1543: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1542);
    let n1544: ZN = zn_rem(n1532, zn_splat(P8::from_raw(524288i32)));
    let n1545: ZB = zn_ge(n1544, zn_splat(P8::from_raw(393216i32)));
    let n1546: ZN = zn_mul(n1535, zn_splat(P8::from_raw(524288i32)));
    let n1547: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1546);
    let n1548: ZB = zn_eq(n1531, n1547);
    let n1549: ZB = zb_or(n1545, n1548);
    let n1550: ZB = zb_and(n1543, n1549);
    let n1551: ZB = zn_ge(n1512, zn_splat(P8::from_raw(0i32)));
    let n1552: ZB = zb_and(n1550, n1551);
    let n1553: ZB = zb_not(n1552);
    let n1554: ZB = zb_and(n1538, n1552);
    let n1555: ZB = zb_and(n1538, n1553);
    let n1556: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1542);
    let n1557: ZN = zn_rem(n1515, zn_splat(P8::from_raw(524288i32)));
    let n1558: ZB = zn_le(n1557, zn_splat(P8::from_raw(131072i32)));
    let n1559: ZB = zb_and(n1556, n1558);
    let n1560: ZB = zn_le(n1512, zn_splat(P8::from_raw(0i32)));
    let n1561: ZB = zb_and(n1559, n1560);
    let n1562: ZB = zb_not(n1561);
    let n1563: ZB = zb_and(n1555, n1561);
    let n1564: ZB = zb_and(n1555, n1562);
    let n1565: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1542);
    let n1566: ZN = zn_rem(n1514, zn_splat(P8::from_raw(524288i32)));
    let n1567: ZB = zn_le(n1566, zn_splat(P8::from_raw(131072i32)));
    let n1568: ZB = zb_and(n1565, n1567);
    let n1569: ZB = zn_le(n1511, zn_splat(P8::from_raw(0i32)));
    let n1570: ZB = zb_and(n1568, n1569);
    let n1571: ZB = zb_not(n1570);
    let n1572: ZB = zb_and(n1564, n1570);
    let n1573: ZB = zb_and(n1564, n1571);
    let n1574: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1542);
    let n1575: ZN = zn_rem(n1520, zn_splat(P8::from_raw(524288i32)));
    let n1576: ZB = zn_ge(n1575, zn_splat(P8::from_raw(393216i32)));
    let n1577: ZN = zn_mul(n1523, zn_splat(P8::from_raw(524288i32)));
    let n1578: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1577);
    let n1579: ZB = zn_eq(n1519, n1578);
    let n1580: ZB = zb_or(n1576, n1579);
    let n1581: ZB = zb_and(n1574, n1580);
    let n1582: ZB = zn_ge(n1511, zn_splat(P8::from_raw(0i32)));
    let n1583: ZB = zb_and(n1581, n1582);
    let n1584: ZB = zb_not(n1583);
    let n1585: ZB = zb_and(n1573, n1583);
    let n1586: ZB = zb_and(n1573, n1584);
    let n1587: ZB = zb_or(n1572, n1585);
    let n1588: ZB = zb_or(n1563, n1587);
    let n1589: ZB = zb_or(n1554, n1588);
    let n1590: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1530);
    let n1591: ZB = zn_le(n1590, n1534);
    let n1592: ZB = zn_gt(n1590, n1534);
    let n1593: ZB = zb_and(n1586, n1591);
    let n1594: ZB = zb_and(n1586, n1592);
    let n1595: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1590);
    let n1596: ZN = zn_mget(g.cart, n1540, n1595);
    let n1597: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1596);
    let n1598: ZN = zn_mul(n1590, zn_splat(P8::from_raw(524288i32)));
    let n1599: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1598);
    let n1600: ZB = zn_eq(n1531, n1599);
    let n1601: ZB = zb_or(n1545, n1600);
    let n1602: ZB = zb_and(n1597, n1601);
    let n1603: ZB = zb_and(n1551, n1602);
    let n1604: ZB = zb_not(n1603);
    let n1605: ZB = zb_and(n1593, n1603);
    let n1606: ZB = zb_and(n1593, n1604);
    let n1607: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1596);
    let n1608: ZB = zb_and(n1558, n1607);
    let n1609: ZB = zb_and(n1560, n1608);
    let n1610: ZB = zb_not(n1609);
    let n1611: ZB = zb_and(n1606, n1609);
    let n1612: ZB = zb_and(n1606, n1610);
    let n1613: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1596);
    let n1614: ZB = zb_and(n1567, n1613);
    let n1615: ZB = zb_and(n1569, n1614);
    let n1616: ZB = zb_not(n1615);
    let n1617: ZB = zb_and(n1612, n1615);
    let n1618: ZB = zb_and(n1612, n1616);
    let n1619: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1596);
    let n1620: ZB = zb_and(n1580, n1619);
    let n1621: ZB = zb_and(n1582, n1620);
    let n1622: ZB = zb_not(n1621);
    let n1623: ZB = zb_and(n1618, n1621);
    let n1624: ZB = zb_and(n1618, n1622);
    let n1625: ZB = zb_or(n1617, n1623);
    let n1626: ZB = zb_or(n1611, n1625);
    let n1627: ZB = zb_or(n1605, n1626);
    let n1628: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1530);
    let n1629: ZB = zn_le(n1628, n1534);
    let n1630: ZB = zn_gt(n1628, n1534);
    let n1631: ZB = zb_and(n1624, n1629);
    let n1632: ZB = zb_and(n1624, n1630);
    let n1633: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1628);
    let n1634: ZN = zn_mget(g.cart, n1540, n1633);
    let n1635: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1634);
    let n1636: ZN = zn_mul(n1628, zn_splat(P8::from_raw(524288i32)));
    let n1637: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1636);
    let n1638: ZB = zn_eq(n1531, n1637);
    let n1639: ZB = zb_or(n1545, n1638);
    let n1640: ZB = zb_and(n1635, n1639);
    let n1641: ZB = zb_and(n1551, n1640);
    let n1642: ZB = zb_not(n1641);
    let n1643: ZB = zb_and(n1631, n1641);
    let n1644: ZB = zb_and(n1631, n1642);
    let n1645: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1634);
    let n1646: ZB = zb_and(n1558, n1645);
    let n1647: ZB = zb_and(n1560, n1646);
    let n1648: ZB = zb_not(n1647);
    let n1649: ZB = zb_and(n1644, n1647);
    let n1650: ZB = zb_and(n1644, n1648);
    let n1651: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1634);
    let n1652: ZB = zb_and(n1567, n1651);
    let n1653: ZB = zb_and(n1569, n1652);
    let n1654: ZB = zb_not(n1653);
    let n1655: ZB = zb_and(n1650, n1653);
    let n1656: ZB = zb_and(n1650, n1654);
    let n1657: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1634);
    let n1658: ZB = zb_and(n1580, n1657);
    let n1659: ZB = zb_and(n1582, n1658);
    let n1660: ZB = zb_not(n1659);
    let n1661: ZB = zb_and(n1656, n1659);
    let n1662: ZB = zb_and(n1656, n1660);
    let n1663: ZB = zb_or(n1655, n1661);
    let n1664: ZB = zb_or(n1649, n1663);
    let n1665: ZB = zb_or(n1643, n1664);
    let n1666: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1530);
    let n1667: ZB = zn_gt(n1666, n1534);
    let n1668: ZB = zb_and(n1513, n1667);
    let n1669: ZB = zb_or(n1632, n1662);
    let n1670: ZB = zsel_b(n1630, n1513, n1668);
    let n1671: ZB = zb_or(n1627, n1665);
    let n1672: ZB = zb_or(n1594, n1669);
    let n1673: ZB = zsel_b(n1592, n1513, n1670);
    let n1674: ZB = zb_or(n1589, n1671);
    let n1675: ZB = zb_or(n1539, n1672);
    let n1676: ZB = zsel_b(n1537, n1513, n1673);
    let n1677: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1518);
    let n1678: ZB = zn_le(n1677, n1522);
    let n1679: ZB = zn_gt(n1677, n1522);
    let n1680: ZB = zb_and(n1675, n1678);
    let n1681: ZB = zb_and(n1675, n1679);
    let n1682: ZB = zb_and(n1537, n1680);
    let n1683: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1677);
    let n1684: ZN = zn_mget(g.cart, n1683, n1541);
    let n1685: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1684);
    let n1686: ZB = zb_and(n1536, n1675);
    let n1687: ZB = zb_and(n1678, n1686);
    let n1688: ZB = zb_and(n1549, n1685);
    let n1689: ZB = zb_and(n1551, n1688);
    let n1690: ZB = zb_not(n1689);
    let n1691: ZB = zb_and(n1687, n1689);
    let n1692: ZB = zb_and(n1687, n1690);
    let n1693: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1684);
    let n1694: ZB = zb_and(n1558, n1693);
    let n1695: ZB = zb_and(n1560, n1694);
    let n1696: ZB = zb_not(n1695);
    let n1697: ZB = zb_and(n1692, n1695);
    let n1698: ZB = zb_and(n1692, n1696);
    let n1699: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1684);
    let n1700: ZB = zb_and(n1567, n1699);
    let n1701: ZB = zb_and(n1569, n1700);
    let n1702: ZB = zb_not(n1701);
    let n1703: ZB = zb_and(n1698, n1701);
    let n1704: ZB = zb_and(n1698, n1702);
    let n1705: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1684);
    let n1706: ZN = zn_mul(n1677, zn_splat(P8::from_raw(524288i32)));
    let n1707: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1706);
    let n1708: ZB = zn_eq(n1519, n1707);
    let n1709: ZB = zb_or(n1576, n1708);
    let n1710: ZB = zb_and(n1705, n1709);
    let n1711: ZB = zb_and(n1582, n1710);
    let n1712: ZB = zb_not(n1711);
    let n1713: ZB = zb_and(n1704, n1711);
    let n1714: ZB = zb_and(n1704, n1712);
    let n1715: ZB = zb_or(n1703, n1713);
    let n1716: ZB = zb_or(n1697, n1715);
    let n1717: ZB = zb_or(n1691, n1716);
    let n1718: ZB = zb_and(n1592, n1714);
    let n1719: ZN = zn_mget(g.cart, n1683, n1595);
    let n1720: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1719);
    let n1721: ZB = zb_and(n1591, n1704);
    let n1722: ZB = zb_and(n1712, n1721);
    let n1723: ZB = zb_and(n1601, n1720);
    let n1724: ZB = zb_and(n1551, n1723);
    let n1725: ZB = zb_not(n1724);
    let n1726: ZB = zb_and(n1722, n1724);
    let n1727: ZB = zb_and(n1722, n1725);
    let n1728: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1719);
    let n1729: ZB = zb_and(n1558, n1728);
    let n1730: ZB = zb_and(n1560, n1729);
    let n1731: ZB = zb_not(n1730);
    let n1732: ZB = zb_and(n1727, n1730);
    let n1733: ZB = zb_and(n1727, n1731);
    let n1734: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1719);
    let n1735: ZB = zb_and(n1567, n1734);
    let n1736: ZB = zb_and(n1569, n1735);
    let n1737: ZB = zb_not(n1736);
    let n1738: ZB = zb_and(n1733, n1736);
    let n1739: ZB = zb_and(n1733, n1737);
    let n1740: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1719);
    let n1741: ZB = zb_and(n1709, n1740);
    let n1742: ZB = zb_and(n1582, n1741);
    let n1743: ZB = zb_not(n1742);
    let n1744: ZB = zb_and(n1739, n1742);
    let n1745: ZB = zb_and(n1739, n1743);
    let n1746: ZB = zb_or(n1738, n1744);
    let n1747: ZB = zb_or(n1732, n1746);
    let n1748: ZB = zb_or(n1726, n1747);
    let n1749: ZB = zb_and(n1630, n1745);
    let n1750: ZN = zn_mget(g.cart, n1683, n1633);
    let n1751: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1750);
    let n1752: ZB = zb_and(n1629, n1739);
    let n1753: ZB = zb_and(n1743, n1752);
    let n1754: ZB = zb_and(n1639, n1751);
    let n1755: ZB = zb_and(n1551, n1754);
    let n1756: ZB = zb_not(n1755);
    let n1757: ZB = zb_and(n1753, n1755);
    let n1758: ZB = zb_and(n1753, n1756);
    let n1759: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1750);
    let n1760: ZB = zb_and(n1558, n1759);
    let n1761: ZB = zb_and(n1560, n1760);
    let n1762: ZB = zb_not(n1761);
    let n1763: ZB = zb_and(n1758, n1761);
    let n1764: ZB = zb_and(n1758, n1762);
    let n1765: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1750);
    let n1766: ZB = zb_and(n1567, n1765);
    let n1767: ZB = zb_and(n1569, n1766);
    let n1768: ZB = zb_not(n1767);
    let n1769: ZB = zb_and(n1764, n1767);
    let n1770: ZB = zb_and(n1764, n1768);
    let n1771: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1750);
    let n1772: ZB = zb_and(n1709, n1771);
    let n1773: ZB = zb_and(n1582, n1772);
    let n1774: ZB = zb_not(n1773);
    let n1775: ZB = zb_and(n1770, n1773);
    let n1776: ZB = zb_and(n1770, n1774);
    let n1777: ZB = zb_or(n1769, n1775);
    let n1778: ZB = zb_or(n1763, n1777);
    let n1779: ZB = zb_or(n1757, n1778);
    let n1780: ZB = zb_and(n1667, n1676);
    let n1781: ZB = zb_or(n1749, n1776);
    let n1782: ZB = zsel_b(n1630, n1676, n1780);
    let n1783: ZB = zb_or(n1748, n1779);
    let n1784: ZB = zb_or(n1718, n1781);
    let n1785: ZB = zsel_b(n1592, n1676, n1782);
    let n1786: ZB = zb_or(n1717, n1783);
    let n1787: ZB = zb_or(n1682, n1784);
    let n1788: ZB = zsel_b(n1537, n1676, n1785);
    let n1789: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n1518);
    let n1790: ZB = zn_le(n1789, n1522);
    let n1791: ZB = zn_gt(n1789, n1522);
    let n1792: ZB = zb_and(n1787, n1790);
    let n1793: ZB = zb_and(n1787, n1791);
    let n1794: ZB = zb_and(n1537, n1792);
    let n1795: ZN = zn_add(zn_splat(P8::from_raw(2097152i32)), n1789);
    let n1796: ZN = zn_mget(g.cart, n1795, n1541);
    let n1797: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1796);
    let n1798: ZB = zb_and(n1536, n1787);
    let n1799: ZB = zb_and(n1790, n1798);
    let n1800: ZB = zb_and(n1549, n1797);
    let n1801: ZB = zb_and(n1551, n1800);
    let n1802: ZB = zb_not(n1801);
    let n1803: ZB = zb_and(n1799, n1801);
    let n1804: ZB = zb_and(n1799, n1802);
    let n1805: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1796);
    let n1806: ZB = zb_and(n1558, n1805);
    let n1807: ZB = zb_and(n1560, n1806);
    let n1808: ZB = zb_not(n1807);
    let n1809: ZB = zb_and(n1804, n1807);
    let n1810: ZB = zb_and(n1804, n1808);
    let n1811: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1796);
    let n1812: ZB = zb_and(n1567, n1811);
    let n1813: ZB = zb_and(n1569, n1812);
    let n1814: ZB = zb_not(n1813);
    let n1815: ZB = zb_and(n1810, n1813);
    let n1816: ZB = zb_and(n1810, n1814);
    let n1817: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1796);
    let n1818: ZN = zn_mul(n1789, zn_splat(P8::from_raw(524288i32)));
    let n1819: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n1818);
    let n1820: ZB = zn_eq(n1519, n1819);
    let n1821: ZB = zb_or(n1576, n1820);
    let n1822: ZB = zb_and(n1817, n1821);
    let n1823: ZB = zb_and(n1582, n1822);
    let n1824: ZB = zb_not(n1823);
    let n1825: ZB = zb_and(n1816, n1823);
    let n1826: ZB = zb_and(n1816, n1824);
    let n1827: ZB = zb_or(n1815, n1825);
    let n1828: ZB = zb_or(n1809, n1827);
    let n1829: ZB = zb_or(n1803, n1828);
    let n1830: ZB = zb_and(n1592, n1826);
    let n1831: ZN = zn_mget(g.cart, n1795, n1595);
    let n1832: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1831);
    let n1833: ZB = zb_and(n1591, n1816);
    let n1834: ZB = zb_and(n1824, n1833);
    let n1835: ZB = zb_and(n1601, n1832);
    let n1836: ZB = zb_and(n1551, n1835);
    let n1837: ZB = zb_not(n1836);
    let n1838: ZB = zb_and(n1834, n1836);
    let n1839: ZB = zb_and(n1834, n1837);
    let n1840: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1831);
    let n1841: ZB = zb_and(n1558, n1840);
    let n1842: ZB = zb_and(n1560, n1841);
    let n1843: ZB = zb_not(n1842);
    let n1844: ZB = zb_and(n1839, n1842);
    let n1845: ZB = zb_and(n1839, n1843);
    let n1846: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1831);
    let n1847: ZB = zb_and(n1567, n1846);
    let n1848: ZB = zb_and(n1569, n1847);
    let n1849: ZB = zb_not(n1848);
    let n1850: ZB = zb_and(n1845, n1848);
    let n1851: ZB = zb_and(n1845, n1849);
    let n1852: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1831);
    let n1853: ZB = zb_and(n1821, n1852);
    let n1854: ZB = zb_and(n1582, n1853);
    let n1855: ZB = zb_not(n1854);
    let n1856: ZB = zb_and(n1851, n1854);
    let n1857: ZB = zb_and(n1851, n1855);
    let n1858: ZB = zb_or(n1850, n1856);
    let n1859: ZB = zb_or(n1844, n1858);
    let n1860: ZB = zb_or(n1838, n1859);
    let n1861: ZB = zb_and(n1630, n1857);
    let n1862: ZN = zn_mget(g.cart, n1795, n1633);
    let n1863: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n1862);
    let n1864: ZB = zb_and(n1629, n1851);
    let n1865: ZB = zb_and(n1855, n1864);
    let n1866: ZB = zb_and(n1639, n1863);
    let n1867: ZB = zb_and(n1551, n1866);
    let n1868: ZB = zb_not(n1867);
    let n1869: ZB = zb_and(n1865, n1867);
    let n1870: ZB = zb_and(n1865, n1868);
    let n1871: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n1862);
    let n1872: ZB = zb_and(n1558, n1871);
    let n1873: ZB = zb_and(n1560, n1872);
    let n1874: ZB = zb_not(n1873);
    let n1875: ZB = zb_and(n1870, n1873);
    let n1876: ZB = zb_and(n1870, n1874);
    let n1877: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n1862);
    let n1878: ZB = zb_and(n1567, n1877);
    let n1879: ZB = zb_and(n1569, n1878);
    let n1880: ZB = zb_not(n1879);
    let n1881: ZB = zb_and(n1876, n1879);
    let n1882: ZB = zb_and(n1876, n1880);
    let n1883: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n1862);
    let n1884: ZB = zb_and(n1821, n1883);
    let n1885: ZB = zb_and(n1582, n1884);
    let n1886: ZB = zb_not(n1885);
    let n1887: ZB = zb_and(n1882, n1885);
    let n1888: ZB = zb_and(n1882, n1886);
    let n1889: ZB = zb_or(n1881, n1887);
    let n1890: ZB = zb_or(n1875, n1889);
    let n1891: ZB = zb_or(n1869, n1890);
    let n1892: ZB = zb_and(n1667, n1788);
    let n1893: ZB = zb_or(n1861, n1888);
    let n1894: ZB = zsel_b(n1630, n1788, n1892);
    let n1895: ZB = zb_or(n1860, n1891);
    let n1896: ZB = zb_or(n1830, n1893);
    let n1897: ZB = zsel_b(n1592, n1788, n1894);
    let n1898: ZB = zb_or(n1829, n1895);
    let n1899: ZB = zb_or(n1794, n1896);
    let n1900: ZB = zsel_b(n1537, n1788, n1897);
    let n1901: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1518);
    let n1902: ZB = zn_gt(n1901, n1522);
    let n1903: ZB = zb_and(n1900, n1902);
    let n1904: ZB = zb_or(n1786, n1898);
    let n1905: ZB = zsel_b(n1786, n1676, n1788);
    let n1906: ZB = zb_or(n1793, n1899);
    let n1907: ZB = zsel_b(n1791, n1788, n1903);
    let n1908: ZB = zb_or(n1674, n1904);
    let n1909: ZB = zsel_b(n1674, n1513, n1905);
    let n1910: ZB = zb_or(n1681, n1906);
    let n1911: ZB = zsel_b(n1679, n1676, n1907);
    let n1912: ZB = zb_or(n1527, n1910);
    let n1913: ZB = zsel_b(n1525, n1513, n1911);
    let n1914: ZB = zn_gt(n1510, zn_splat(P8::from_raw(8388608i32)));
    let n1915: ZB = zn_le(n1510, zn_splat(P8::from_raw(8388608i32)));
    let n1916: ZN = zsel_n(n1914, n1177, n1176);
    let n1917: ZB = zb_and(n1912, n1914);
    let n1918: ZN = zsel_n(n1908, n1916, n1176);
    let n1919: ZB = zb_or(n1908, n1917);
    let n1920: ZB = zsel_b(n1908, n1909, n1913);
    let n1921: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1514);
    let n1922: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1515);
    let n1923: ZB = zn_tile_flag_at(g.cache, g.cart, n1921, n1922, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1924: ZB = zb_not(n1923);
    let n1925: ZB = zn_lt(r_c283, zn_splat(P8::from_raw(65536i32)));
    let n1926: ZN = zsel_n(n1925, zn_splat(P8::from_raw(65536i32)), r_c283);
    let n1927: ZN = zsel_n(n1923, n1926, r_c283);
    let n1928: ZN = zsel_n(n1923, zn_splat(P8::from_raw(393216i32)), n790);
    let n1929: ZB = zn_gt(n1511, r_c358);
    let n1930: ZB = zn_gt(n1512, r_c359);
    let n1931: ZN = zsel_n(n1924, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n1932: ZN = zn_abs(n1511);
    let n1933: ZB = zn_gt(n1932, zn_splat(P8::from_raw(65536i32)));
    let n1934: ZB = zn_gt(n1511, zn_splat(P8::from_raw(0i32)));
    let n1935: ZB = zn_lt(n1511, zn_splat(P8::from_raw(0i32)));
    let n1936: ZB = zn_gt(n1511, zn_splat(P8::from_raw(65536i32)));
    let n1937: ZN = zn_sub(n1511, zn_splat(P8::from_raw(9830i32)));
    let n1938: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1937);
    let n1939: ZN = zn_add(zn_splat(P8::from_raw(9830i32)), n1511);
    let n1940: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1939);
    let n1941: ZB = zn_gt(n1511, zn_splat(P8::from_raw(-65536i32)));
    let n1942: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1937);
    let n1943: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1939);
    let n1944: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1937);
    let n1945: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1939);
    let n1946: ZN = zsel_n(n1941, n1942, n1943);
    let n1947: ZN = zsel_n(n1934, n1944, n1945);
    let n1948: ZN = zsel_n(n1936, n1938, n1940);
    let n1949: ZN = zsel_n(n1935, n1946, n1947);
    let n1950: ZN = zsel_n(n1934, n1948, n1949);
    let n1951: ZN = zn_sub(n1511, n1931);
    let n1952: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n1951);
    let n1953: ZN = zn_add(n1511, n1931);
    let n1954: ZN = zn_min(zn_splat(P8::from_raw(0i32)), n1953);
    let n1955: ZN = zsel_n(n1934, n1952, n1954);
    let n1956: ZN = zsel_n(n1933, n1950, n1955);
    let n1957: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1956);
    let n1958: ZB = zb_not(n1957);
    let n1959: ZB = zn_lt(n1956, zn_splat(P8::from_raw(0i32)));
    let n1960: ZB = zsel_b(n1958, n1959, r_c360);
    let n1961: ZN = zn_abs(n1512);
    let n1962: ZB = zn_le(n1961, zn_splat(P8::from_raw(9830i32)));
    let n1963: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n1515);
    let n1964: ZB = zn_gt(n1512, zn_splat(P8::from_raw(131072i32)));
    let n1965: ZB = zn_gt(n1928, zn_splat(P8::from_raw(0i32)));
    let n1966: ZN = zn_add(zn_splat(P8::from_raw(-196608i32)), n1514);
    let n1967: ZB = zn_tile_flag_at(g.cache, g.cart, n1966, n1963, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1968: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n1514);
    let n1969: ZB = zn_tile_flag_at(g.cache, g.cart, n1968, n1963, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1970: ZN = zsel_n(n1969, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n1971: ZN = zsel_n(n1967, zn_splat(P8::from_raw(-65536i32)), n1970);
    let n1972: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1971);
    let n1973: ZB = zb_not(n1972);
    let n1974: ZB = zn_gt(n1927, zn_splat(P8::from_raw(0i32)));
    let n1975: ZN = zsel_n(n1960, zn_splat(P8::from_raw(-65536i32)), zn_splat(P8::from_raw(65536i32)));
    let n1976: ZB = zn_gt(n1975, zn_splat(P8::from_raw(0i32)));
    let n1977: ZB = zn_lt(n1975, zn_splat(P8::from_raw(0i32)));
    let n1978: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1975);
    let n1979: ZB = zb_not(n1978);
    let n1980: ZB = zn_lt(n1510, zn_splat(P8::from_raw(-262144i32)));
    let n1981: ZB = zn_ge(n1510, zn_splat(P8::from_raw(-262144i32)));
    let n1982: ZB = zb_and(n1919, n1980);
    let n1985: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n1951);
    let n1986: ZN = zn_min(zn_splat(P8::from_raw(-65536i32)), n1953);
    let n1987: ZN = zsel_n(n1941, n1985, n1986);
    let n1988: ZN = zsel_n(n1933, n1950, n1987);
    let n1989: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n1988);
    let n1990: ZB = zb_not(n1989);
    let n1991: ZB = zn_lt(n1988, zn_splat(P8::from_raw(0i32)));
    let n1992: ZB = zsel_b(n1990, n1991, r_c360);
    let n1993: ZN = zn_add(zn_splat(P8::from_raw(-65536i32)), n1514);
    let n1994: ZB = zn_tile_flag_at(g.cache, g.cart, n1993, n1963, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n1995: ZN = zsel_n(n1994, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n1996: ZB = zn_gt(n1512, n1995);
    let n1997: ZN = zn_max(zn_splat(P8::from_raw(65536i32)), n1951);
    let n1998: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n1953);
    let n1999: ZN = zsel_n(n1936, n1997, n1998);
    let n2000: ZN = zsel_n(n1933, n1950, n1999);
    let n2001: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n2000);
    let n2002: ZB = zb_not(n2001);
    let n2003: ZB = zn_lt(n2000, zn_splat(P8::from_raw(0i32)));
    let n2004: ZB = zsel_b(n2002, n2003, r_c360);
    let n2005: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n1514);
    let n2006: ZB = zn_tile_flag_at(g.cache, g.cart, n2005, n1963, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n2007: ZN = zsel_n(n2006, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(131072i32)));
    let n2008: ZB = zn_gt(n1512, n2007);
    let n2009: ZB = zb_and(n787, n1974);
    let n2010: ZN = zsel_n(n2009, zn_splat(P8::from_raw(131072i32)), r_c20);
    let n2011: ZB = zb_or(r_c41, n2009);
    let n2012: ZN = zsel_n(n793, r_c20, n2010);
    let n2013: ZB = zsel_b(n793, r_c41, n2011);
    let n2017: ZB = zb_and(n1912, n1915);
    let n2018: ZB = zb_and(n1980, n2017);
    let n2019: ZB = zb_and(n1981, n2017);
    let n2020: ZN = zsel_n(n2018, r_c87, n1918);
    let n2021: ZN = zsel_n(n2018, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n2022: ZB = zb_not(n2018);
    let n2023: ZB = zb_or(n1982, n2018);
    let n2024: ZB = zsel_b(n2018, n1913, n1920);
    let n2025: ZN = zsel_n(n2023, n2020, n1176);
    let n2026: ZN = zsel_n(n2023, n2021, zn_splat(P8::from_raw(983040i32)));
    let n2027: ZB = zb_not(n2023);
    let n2028: ZB = zb_or(n2022, n2027);
    let n2029: ZB = zsel_b(n2023, n2024, n1308);
    let n2031: ZN = zsel_n(n2023, n2012, n961);
    let n2032: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n2033: ZN = zn_sub(n1322, zn_splat(P8::from_raw(32768i32)));
    let n2034: ZN = zn_sub(n2033, n1323);
    let n2035: ZN = zsel_n(n1366, zn_splat(P8::from_raw(0i32)), n2034);
    let n2036: ZN = zsel_n(n1363, n2034, n2035);
    let n2037: ZN = zsel_n(n1361, zn_splat(P8::from_raw(0i32)), n2036);
    let n2038: ZN = zsel_n(n1358, n2034, n2037);
    let n2039: ZN = zsel_n(n1356, zn_splat(P8::from_raw(0i32)), n2038);
    let n2040: ZN = zsel_n(n1353, n2034, n2039);
    let n2041: ZN = zsel_n(n1351, zn_splat(P8::from_raw(0i32)), n2040);
    let n2042: ZN = zsel_n(n1348, n2034, n2041);
    let n2043: ZN = zsel_n(n1346, zn_splat(P8::from_raw(0i32)), n2042);
    let n2044: ZN = zsel_n(n1343, n2034, n2043);
    let n2045: ZN = zsel_n(n1341, zn_splat(P8::from_raw(0i32)), n2044);
    let n2046: ZN = zsel_n(n1338, n2034, n2045);
    let n2047: ZN = zsel_n(n1336, zn_splat(P8::from_raw(0i32)), n2046);
    let n2048: ZN = zsel_n(n1333, n2034, n2047);
    let n2049: ZN = zsel_n(n1331, zn_splat(P8::from_raw(0i32)), n2048);
    let n2050: ZN = zn_sub(n1415, zn_splat(P8::from_raw(32768i32)));
    let n2051: ZN = zn_sub(n2050, n1416);
    let n2052: ZN = zsel_n(n1460, zn_splat(P8::from_raw(0i32)), n2051);
    let n2053: ZN = zsel_n(n1457, n2051, n2052);
    let n2054: ZN = zsel_n(n1455, zn_splat(P8::from_raw(0i32)), n2053);
    let n2055: ZN = zsel_n(n1452, n2051, n2054);
    let n2056: ZN = zsel_n(n1450, zn_splat(P8::from_raw(0i32)), n2055);
    let n2057: ZN = zsel_n(n1447, n2051, n2056);
    let n2058: ZN = zsel_n(n1445, zn_splat(P8::from_raw(0i32)), n2057);
    let n2059: ZN = zsel_n(n1442, n2051, n2058);
    let n2060: ZN = zsel_n(n1440, zn_splat(P8::from_raw(0i32)), n2059);
    let n2061: ZN = zsel_n(n1437, n2051, n2060);
    let n2062: ZN = zsel_n(n1435, zn_splat(P8::from_raw(0i32)), n2061);
    let n2063: ZN = zsel_n(n1432, n2051, n2062);
    let n2064: ZN = zsel_n(n1430, zn_splat(P8::from_raw(0i32)), n2063);
    let n2065: ZN = zsel_n(n1427, n2051, n2064);
    let n2066: ZN = zsel_n(n1425, zn_splat(P8::from_raw(0i32)), n2065);
    let n2067: ZN = zsel_n(n1319, n2049, r_c366);
    let n2068: ZN = zsel_n(n1319, n2066, r_c367);
    let n2069: ZN = zn_sub(n1511, r_c356);
    let n2070: ZN = zn_max(r_c358, n2069);
    let n2071: ZN = zn_add(r_c356, n1511);
    let n2072: ZN = zn_min(r_c358, n2071);
    let n2073: ZN = zsel_n(n1929, n2070, n2072);
    let n2074: ZN = zn_sub(n1512, r_c357);
    let n2075: ZN = zn_max(r_c359, n2074);
    let n2076: ZN = zn_add(r_c357, n1512);
    let n2077: ZN = zn_min(r_c359, n2076);
    let n2078: ZN = zsel_n(n1930, n2075, n2077);
    let n2079: ZN = zsel_n(n1962, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(13762i32)));
    let n2080: ZN = zn_sub(n1512, n2079);
    let n2081: ZN = zn_max(zn_splat(P8::from_raw(131072i32)), n2080);
    let n2082: ZN = zn_add(n1512, n2079);
    let n2083: ZN = zn_min(zn_splat(P8::from_raw(131072i32)), n2082);
    let n2084: ZN = zsel_n(n1964, n2081, n2083);
    let n2085: ZN = zsel_n(n1924, n2084, n1512);
    let n2086: ZN = zn_neg(n1971);
    let n2087: ZN = zn_mul(n2086, zn_splat(P8::from_raw(131072i32)));
    let n2088: ZN = zsel_n(n1973, n2087, n1956);
    let n2089: ZN = zsel_n(n1973, zn_splat(P8::from_raw(-131072i32)), n2085);
    let n2090: ZN = zsel_n(n1965, zn_splat(P8::from_raw(0i32)), n1928);
    let n2091: ZN = zsel_n(n1965, n1956, n2088);
    let n2092: ZN = zsel_n(n1965, zn_splat(P8::from_raw(-131072i32)), n2089);
    let n2093: ZN = zn_sub(n1927, zn_splat(P8::from_raw(65536i32)));
    let n2094: ZN = zsel_n(n1977, zn_splat(P8::from_raw(-131072i32)), zn_splat(P8::from_raw(0i32)));
    let n2095: ZN = zsel_n(n1976, zn_splat(P8::from_raw(131072i32)), n2094);
    let n2096: ZN = zsel_n(n1979, zn_splat(P8::from_raw(69510i32)), zn_splat(P8::from_raw(98304i32)));
    let n2097: ZB = zsel_b(n793, r_c360, n1960);
    let n2098: ZN = zsel_n(n793, n2073, n1956);
    let n2099: ZN = zsel_n(n793, n2078, n2085);
    let n2100: ZN = zsel_n(n872, n2032, r_c20);
    let n2101: ZN = zsel_n(n872, r_c280, n792);
    let n2102: ZN = zsel_n(n872, r_c282, n868);
    let n2103: ZN = zsel_n(n872, r_c283, n1927);
    let n2104: ZN = zsel_n(n872, r_c285, n1928);
    let n2105: ZB = zb_and(r_c292, n872);
    let n2106: ZB = zb_and(r_c293, n872);
    let n2107: ZN = zsel_n(n872, r_c299, n1509);
    let n2108: ZN = zsel_n(n872, r_c300, n1510);
    let n2109: ZB = zsel_b(n872, r_c360, n2097);
    let n2110: ZN = zsel_n(n872, r_c366, n2067);
    let n2111: ZN = zsel_n(n872, r_c367, n2068);
    let n2112: ZN = zsel_n(n872, r_c368, n2098);
    let n2113: ZN = zsel_n(n872, r_c369, n2099);
    let n2114: ZB = zb_or(n872, n2019);
    let n2115: ZB = zb_or(n872, n1913);
    let n2116: ZB = zn_gt(n2100, zn_splat(P8::from_raw(0i32)));
    let n2117: ZB = zn_lt(n2107, zn_splat(P8::from_raw(-65536i32)));
    let n2118: ZB = zn_gt(n2107, zn_splat(P8::from_raw(7929856i32)));
    let n2119: ZB = zb_or(n2117, n2118);
    let n2120: ZN = zn_min(zn_splat(P8::from_raw(7929856i32)), n2107);
    let n2121: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n2120);
    let n2122: ZN = zsel_n(n2119, n2121, n2107);
    let n2123: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2112);
    let n2124: ZN = zsel_n(n2116, n2107, n2122);
    let n2125: ZN = zsel_n(n2116, n2112, n2123);
    let n2127: ZN = zn_max(n1995, n2080);
    let n2128: ZN = zn_min(n1995, n2082);
    let n2129: ZN = zsel_n(n1996, n2127, n2128);
    let n2130: ZN = zsel_n(n1924, n2129, n1512);
    let n2131: ZN = zsel_n(n1973, n2087, n1988);
    let n2132: ZN = zsel_n(n1973, zn_splat(P8::from_raw(-131072i32)), n2130);
    let n2133: ZN = zsel_n(n1965, n1988, n2131);
    let n2134: ZN = zsel_n(n1965, zn_splat(P8::from_raw(-131072i32)), n2132);
    let n2135: ZB = zsel_b(n793, r_c360, n1992);
    let n2136: ZN = zsel_n(n793, n2073, n1988);
    let n2137: ZN = zsel_n(n793, n2078, n2130);
    let n2138: ZB = zsel_b(n872, r_c360, n2135);
    let n2139: ZN = zsel_n(n872, r_c368, n2136);
    let n2140: ZN = zsel_n(n872, r_c369, n2137);
    let n2141: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2139);
    let n2142: ZN = zsel_n(n2116, n2139, n2141);
    let n2143: ZN = zn_max(n2007, n2080);
    let n2144: ZN = zn_min(n2007, n2082);
    let n2145: ZN = zsel_n(n2008, n2143, n2144);
    let n2146: ZN = zsel_n(n1924, n2145, n1512);
    let n2147: ZN = zsel_n(n1973, n2087, n2000);
    let n2148: ZN = zsel_n(n1973, zn_splat(P8::from_raw(-131072i32)), n2146);
    let n2149: ZN = zsel_n(n1965, n2000, n2147);
    let n2150: ZN = zsel_n(n1965, zn_splat(P8::from_raw(-131072i32)), n2148);
    let n2151: ZB = zsel_b(n793, r_c360, n2004);
    let n2152: ZN = zsel_n(n793, n2073, n2000);
    let n2153: ZN = zsel_n(n793, n2078, n2146);
    let n2154: ZB = zsel_b(n872, r_c360, n2151);
    let n2155: ZN = zsel_n(n872, r_c368, n2152);
    let n2156: ZN = zsel_n(n872, r_c369, n2153);
    let n2157: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2155);
    let n2158: ZN = zsel_n(n2116, n2155, n2157);
    let n2159: ZN = zsel_n(n136, n2090, n1928);
    let n2160: ZN = zsel_n(n136, n2091, n1956);
    let n2161: ZN = zsel_n(n136, n2092, n2085);
    let n2162: ZN = zsel_n(n793, n1928, n2159);
    let n2163: ZN = zsel_n(n793, n2073, n2160);
    let n2164: ZN = zsel_n(n793, n2078, n2161);
    let n2165: ZN = zsel_n(n872, r_c285, n2162);
    let n2166: ZB = zb_or(r_c293, n137);
    let n2167: ZN = zsel_n(n872, r_c368, n2163);
    let n2168: ZN = zsel_n(n872, r_c369, n2164);
    let n2169: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2167);
    let n2170: ZN = zsel_n(n2116, n2167, n2169);
    let n2171: ZN = zsel_n(n136, n2133, n1988);
    let n2172: ZN = zsel_n(n136, n2134, n2130);
    let n2173: ZN = zsel_n(n793, n2073, n2171);
    let n2174: ZN = zsel_n(n793, n2078, n2172);
    let n2175: ZN = zsel_n(n872, r_c368, n2173);
    let n2176: ZN = zsel_n(n872, r_c369, n2174);
    let n2177: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2175);
    let n2178: ZN = zsel_n(n2116, n2175, n2177);
    let n2179: ZN = zsel_n(n136, n2149, n2000);
    let n2180: ZN = zsel_n(n136, n2150, n2146);
    let n2181: ZN = zsel_n(n793, n2073, n2179);
    let n2182: ZN = zsel_n(n793, n2078, n2180);
    let n2183: ZN = zsel_n(n872, r_c368, n2181);
    let n2184: ZN = zsel_n(n872, r_c369, n2182);
    let n2185: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2183);
    let n2186: ZN = zsel_n(n2116, n2183, n2185);
    let n2187: ZN = zsel_n(n2009, zn_splat(P8::from_raw(655360i32)), n792);
    let n2188: ZN = zsel_n(n2009, zn_splat(P8::from_raw(262144i32)), r_c282);
    let n2189: ZN = zsel_n(n2009, n2093, n1927);
    let n2190: ZN = zsel_n(n2009, zn_splat(P8::from_raw(98304i32)), r_c356);
    let n2191: ZN = zsel_n(n2009, n2096, r_c357);
    let n2192: ZN = zsel_n(n2009, n2095, r_c358);
    let n2193: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), r_c359);
    let n2194: ZN = zsel_n(n2009, n1975, n1956);
    let n2195: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2085);
    let n2196: ZN = zsel_n(n793, n792, n2187);
    let n2197: ZN = zsel_n(n793, n794, n2188);
    let n2198: ZN = zsel_n(n793, n1927, n2189);
    let n2199: ZN = zsel_n(n793, r_c356, n2190);
    let n2200: ZN = zsel_n(n793, r_c357, n2191);
    let n2201: ZN = zsel_n(n793, r_c358, n2192);
    let n2202: ZN = zsel_n(n793, r_c359, n2193);
    let n2203: ZN = zsel_n(n793, n2073, n2194);
    let n2204: ZN = zsel_n(n793, n2078, n2195);
    let n2205: ZN = zsel_n(n872, n2032, n2012);
    let n2206: ZB = zsel_b(n872, r_c41, n2013);
    let n2207: ZN = zsel_n(n872, r_c280, n2196);
    let n2208: ZN = zsel_n(n872, r_c282, n2197);
    let n2209: ZN = zsel_n(n872, r_c283, n2198);
    let n2210: ZB = zb_or(r_c292, n137);
    let n2211: ZN = zsel_n(n872, r_c356, n2199);
    let n2212: ZN = zsel_n(n872, r_c357, n2200);
    let n2213: ZN = zsel_n(n872, r_c358, n2201);
    let n2214: ZN = zsel_n(n872, r_c359, n2202);
    let n2215: ZN = zsel_n(n872, r_c368, n2203);
    let n2216: ZN = zsel_n(n872, r_c369, n2204);
    let n2217: ZB = zn_gt(n2205, zn_splat(P8::from_raw(0i32)));
    let n2218: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2215);
    let n2219: ZN = zsel_n(n2217, n2107, n2122);
    let n2220: ZN = zsel_n(n2217, n2215, n2218);
    let n2221: ZN = zsel_n(n2009, zn_splat(P8::from_raw(69510i32)), r_c357);
    let n2222: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-131072i32)), r_c358);
    let n2223: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-327680i32)), n1988);
    let n2224: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2130);
    let n2225: ZN = zsel_n(n793, r_c357, n2221);
    let n2226: ZN = zsel_n(n793, r_c358, n2222);
    let n2227: ZN = zsel_n(n793, n2073, n2223);
    let n2228: ZN = zsel_n(n793, n2078, n2224);
    let n2229: ZN = zsel_n(n872, r_c357, n2225);
    let n2230: ZN = zsel_n(n872, r_c358, n2226);
    let n2231: ZN = zsel_n(n872, r_c368, n2227);
    let n2232: ZN = zsel_n(n872, r_c369, n2228);
    let n2233: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2231);
    let n2234: ZN = zsel_n(n2217, n2231, n2233);
    let n2235: ZN = zsel_n(n2009, zn_splat(P8::from_raw(131072i32)), r_c358);
    let n2236: ZN = zsel_n(n2009, zn_splat(P8::from_raw(327680i32)), n2000);
    let n2237: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2146);
    let n2238: ZN = zsel_n(n793, r_c358, n2235);
    let n2239: ZN = zsel_n(n793, n2073, n2236);
    let n2240: ZN = zsel_n(n793, n2078, n2237);
    let n2241: ZN = zsel_n(n872, r_c358, n2238);
    let n2242: ZN = zsel_n(n872, r_c368, n2239);
    let n2243: ZN = zsel_n(n872, r_c369, n2240);
    let n2244: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2242);
    let n2245: ZN = zsel_n(n2217, n2242, n2244);
    let n2246: ZN = zsel_n(n2009, zn_splat(P8::from_raw(69510i32)), r_c356);
    let n2247: ZN = zsel_n(n2009, zn_splat(P8::from_raw(98304i32)), r_c357);
    let n2248: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), r_c358);
    let n2249: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-98304i32)), r_c359);
    let n2250: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n1956);
    let n2251: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-327680i32)), n2085);
    let n2252: ZN = zsel_n(n793, r_c356, n2246);
    let n2253: ZN = zsel_n(n793, r_c357, n2247);
    let n2254: ZN = zsel_n(n793, r_c358, n2248);
    let n2255: ZN = zsel_n(n793, r_c359, n2249);
    let n2256: ZN = zsel_n(n793, n2073, n2250);
    let n2257: ZN = zsel_n(n793, n2078, n2251);
    let n2258: ZN = zsel_n(n872, r_c356, n2252);
    let n2259: ZN = zsel_n(n872, r_c357, n2253);
    let n2260: ZN = zsel_n(n872, r_c358, n2254);
    let n2261: ZN = zsel_n(n872, r_c359, n2255);
    let n2262: ZN = zsel_n(n872, r_c368, n2256);
    let n2263: ZN = zsel_n(n872, r_c369, n2257);
    let n2264: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2262);
    let n2265: ZN = zsel_n(n2217, n2262, n2264);
    let n2266: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n1988);
    let n2267: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n2130);
    let n2268: ZN = zsel_n(n793, n2073, n2266);
    let n2269: ZN = zsel_n(n793, n2078, n2267);
    let n2270: ZN = zsel_n(n872, r_c368, n2268);
    let n2271: ZN = zsel_n(n872, r_c369, n2269);
    let n2272: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2270);
    let n2273: ZN = zsel_n(n2217, n2270, n2272);
    let n2274: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2000);
    let n2275: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n2146);
    let n2276: ZN = zsel_n(n793, n2073, n2274);
    let n2277: ZN = zsel_n(n793, n2078, n2275);
    let n2278: ZN = zsel_n(n872, r_c368, n2276);
    let n2279: ZN = zsel_n(n872, r_c369, n2277);
    let n2280: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2278);
    let n2281: ZN = zsel_n(n2217, n2278, n2280);
    let n2282: ZN = zsel_n(n2009, zn_splat(P8::from_raw(131072i32)), r_c359);
    let n2283: ZN = zsel_n(n2009, zn_splat(P8::from_raw(327680i32)), n2085);
    let n2284: ZN = zsel_n(n793, r_c359, n2282);
    let n2285: ZN = zsel_n(n793, n2078, n2283);
    let n2286: ZN = zsel_n(n872, r_c359, n2284);
    let n2287: ZN = zsel_n(n872, r_c369, n2285);
    let n2288: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2130);
    let n2289: ZN = zsel_n(n793, n2078, n2288);
    let n2290: ZN = zsel_n(n872, r_c369, n2289);
    let n2291: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2146);
    let n2292: ZN = zsel_n(n793, n2078, n2291);
    let n2293: ZN = zsel_n(n872, r_c369, n2292);
    let n2294: ZN = zsel_n(n2009, n1975, n2160);
    let n2295: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2161);
    let n2296: ZN = zsel_n(n793, n2073, n2294);
    let n2297: ZN = zsel_n(n793, n2078, n2295);
    let n2298: ZN = zsel_n(n872, r_c368, n2296);
    let n2299: ZN = zsel_n(n872, r_c369, n2297);
    let n2300: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2298);
    let n2301: ZN = zsel_n(n2217, n2298, n2300);
    let n2302: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-327680i32)), n2171);
    let n2303: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2172);
    let n2304: ZN = zsel_n(n793, n2073, n2302);
    let n2305: ZN = zsel_n(n793, n2078, n2303);
    let n2306: ZN = zsel_n(n872, r_c368, n2304);
    let n2307: ZN = zsel_n(n872, r_c369, n2305);
    let n2308: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2306);
    let n2309: ZN = zsel_n(n2217, n2306, n2308);
    let n2310: ZN = zsel_n(n2009, zn_splat(P8::from_raw(327680i32)), n2179);
    let n2311: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2180);
    let n2312: ZN = zsel_n(n793, n2073, n2310);
    let n2313: ZN = zsel_n(n793, n2078, n2311);
    let n2314: ZN = zsel_n(n872, r_c368, n2312);
    let n2315: ZN = zsel_n(n872, r_c369, n2313);
    let n2316: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2314);
    let n2317: ZN = zsel_n(n2217, n2314, n2316);
    let n2318: ZN = zsel_n(n2009, zn_splat(P8::from_raw(0i32)), n2160);
    let n2319: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-327680i32)), n2161);
    let n2320: ZN = zsel_n(n793, n2073, n2318);
    let n2321: ZN = zsel_n(n793, n2078, n2319);
    let n2322: ZN = zsel_n(n872, r_c368, n2320);
    let n2323: ZN = zsel_n(n872, r_c369, n2321);
    let n2324: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2322);
    let n2325: ZN = zsel_n(n2217, n2322, n2324);
    let n2326: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n2171);
    let n2327: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n2172);
    let n2328: ZN = zsel_n(n793, n2073, n2326);
    let n2329: ZN = zsel_n(n793, n2078, n2327);
    let n2330: ZN = zsel_n(n872, r_c368, n2328);
    let n2331: ZN = zsel_n(n872, r_c369, n2329);
    let n2332: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2330);
    let n2333: ZN = zsel_n(n2217, n2330, n2332);
    let n2334: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2179);
    let n2335: ZN = zsel_n(n2009, zn_splat(P8::from_raw(-231700i32)), n2180);
    let n2336: ZN = zsel_n(n793, n2073, n2334);
    let n2337: ZN = zsel_n(n793, n2078, n2335);
    let n2338: ZN = zsel_n(n872, r_c368, n2336);
    let n2339: ZN = zsel_n(n872, r_c369, n2337);
    let n2340: ZN = zsel_n(n2119, zn_splat(P8::from_raw(0i32)), n2338);
    let n2341: ZN = zsel_n(n2217, n2338, n2340);
    let n2342: ZN = zsel_n(n2009, zn_splat(P8::from_raw(327680i32)), n2161);
    let n2343: ZN = zsel_n(n793, n2078, n2342);
    let n2344: ZN = zsel_n(n872, r_c369, n2343);
    let n2345: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2172);
    let n2346: ZN = zsel_n(n793, n2078, n2345);
    let n2347: ZN = zsel_n(n872, r_c369, n2346);
    let n2348: ZN = zsel_n(n2009, zn_splat(P8::from_raw(231700i32)), n2180);
    let n2349: ZN = zsel_n(n793, n2078, n2348);
    let n2350: ZN = zsel_n(n872, r_c369, n2349);
    let n2352: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n2353: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n2354: ZW = zw_add(zw_splat(0u64), n2352);
    let n2355: ZW = zw_add(zw_splat(0u64), n2353);
    let n2356: ZW = zw_cellmix_n(84u64, n116, 1542469173u64);
    let n2357: ZW = zw_cellmix_n(84u64, n116, 668265263u64);
    let n2358: ZW = zw_add(n2354, n2356);
    let n2359: ZW = zw_add(n2355, n2357);
    let n2360: ZW = zw_cellmix_n(85u64, n216, 1542469173u64);
    let n2361: ZW = zw_cellmix_n(85u64, n216, 668265263u64);
    let n2362: ZW = zw_add(n2358, n2360);
    let n2363: ZW = zw_add(n2359, n2361);
    let n2364: ZW = zw_cellmix_n(86u64, n215, 1542469173u64);
    let n2365: ZW = zw_cellmix_n(86u64, n215, 668265263u64);
    let n2366: ZW = zw_add(n2362, n2364);
    let n2367: ZW = zw_add(n2363, n2365);
    let n2368: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n2369: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n2370: ZW = zw_add(n2366, n2368);
    let n2371: ZW = zw_add(n2367, n2369);
    let n2372: ZW = zw_cellmix_n(301u64, n455, 1542469173u64);
    let n2373: ZW = zw_cellmix_n(301u64, n455, 668265263u64);
    let n2374: ZW = zw_add(n2370, n2372);
    let n2375: ZW = zw_add(n2371, n2373);
    let n2376: ZW = zw_cellmix_n(367u64, n344, 1542469173u64);
    let n2377: ZW = zw_cellmix_n(367u64, n344, 668265263u64);
    let n2378: ZW = zw_add(n2374, n2376);
    let n2379: ZW = zw_add(n2375, n2377);
    let n2380: ZW = zw_cellmix_n(368u64, n456, 1542469173u64);
    let n2381: ZW = zw_cellmix_n(368u64, n456, 668265263u64);
    let n2382: ZW = zw_add(n2378, n2380);
    let n2383: ZW = zw_add(n2379, n2381);
    let n2384: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n2385: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n2386: ZW = zw_add(n2382, n2384);
    let n2387: ZW = zw_add(n2383, n2385);
    let n2388: ZW = zw_cellmix_b(41u64, r_c41, 1542469173u64);
    let n2389: ZW = zw_cellmix_b(41u64, r_c41, 668265263u64);
    let n2390: ZW = zw_add(n2386, n2388);
    let n2391: ZW = zw_add(n2387, n2389);
    let n2392: ZW = zw_cellmix_n(281u64, n792, 1542469173u64);
    let n2393: ZW = zw_cellmix_n(281u64, n792, 668265263u64);
    let n2394: ZW = zw_add(n2390, n2392);
    let n2395: ZW = zw_add(n2391, n2393);
    let n2396: ZW = zw_cellmix_n(283u64, n868, 1542469173u64);
    let n2397: ZW = zw_cellmix_n(283u64, n868, 668265263u64);
    let n2398: ZW = zw_add(n2394, n2396);
    let n2399: ZW = zw_add(n2395, n2397);
    let n2400: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n2401: ZW = zw_cellmix_n(284u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n2402: ZW = zw_add(n2398, n2400);
    let n2403: ZW = zw_add(n2399, n2401);
    let n2404: ZW = zw_cellmix_n(286u64, n791, 1542469173u64);
    let n2405: ZW = zw_cellmix_n(286u64, n791, 668265263u64);
    let n2406: ZW = zw_add(n2402, n2404);
    let n2407: ZW = zw_add(n2403, n2405);
    let n2408: ZW = zw_cellmix_b(293u64, zb_splat(false), 1542469173u64);
    let n2409: ZW = zw_cellmix_b(293u64, zb_splat(false), 668265263u64);
    let n2410: ZW = zw_add(n2406, n2408);
    let n2411: ZW = zw_add(n2407, n2409);
    let n2412: ZW = zw_cellmix_b(294u64, zb_splat(false), 1542469173u64);
    let n2413: ZW = zw_cellmix_b(294u64, zb_splat(false), 668265263u64);
    let n2414: ZW = zw_add(n2410, n2412);
    let n2415: ZW = zw_add(n2411, n2413);
    let n2416: ZW = zw_cellmix_n(300u64, n881, 1542469173u64);
    let n2417: ZW = zw_cellmix_n(300u64, n881, 668265263u64);
    let n2418: ZW = zw_add(n2414, n2416);
    let n2419: ZW = zw_add(n2415, n2417);
    let n2420: ZW = zw_cellmix_n(357u64, r_c356, 1542469173u64);
    let n2421: ZW = zw_cellmix_n(357u64, r_c356, 668265263u64);
    let n2422: ZW = zw_add(n2418, n2420);
    let n2423: ZW = zw_add(n2419, n2421);
    let n2424: ZW = zw_cellmix_n(358u64, r_c357, 1542469173u64);
    let n2425: ZW = zw_cellmix_n(358u64, r_c357, 668265263u64);
    let n2426: ZW = zw_add(n2422, n2424);
    let n2427: ZW = zw_add(n2423, n2425);
    let n2428: ZW = zw_cellmix_n(359u64, r_c358, 1542469173u64);
    let n2429: ZW = zw_cellmix_n(359u64, r_c358, 668265263u64);
    let n2430: ZW = zw_add(n2426, n2428);
    let n2431: ZW = zw_add(n2427, n2429);
    let n2432: ZW = zw_cellmix_n(360u64, r_c359, 1542469173u64);
    let n2433: ZW = zw_cellmix_n(360u64, r_c359, 668265263u64);
    let n2434: ZW = zw_add(n2430, n2432);
    let n2435: ZW = zw_add(n2431, n2433);
    let n2436: ZW = zw_cellmix_b(361u64, n869, 1542469173u64);
    let n2437: ZW = zw_cellmix_b(361u64, n869, 668265263u64);
    let n2438: ZW = zw_add(n2434, n2436);
    let n2439: ZW = zw_add(n2435, n2437);
    let n2440: ZW = zw_cellmix_n(369u64, n882, 1542469173u64);
    let n2441: ZW = zw_cellmix_n(369u64, n882, 668265263u64);
    let n2442: ZW = zw_add(n2438, n2440);
    let n2443: ZW = zw_add(n2439, n2441);
    let n2444: ZW = zw_cellmix_n(370u64, n871, 1542469173u64);
    let n2445: ZW = zw_cellmix_n(370u64, n871, 668265263u64);
    let n2446: ZW = zw_add(n2442, n2444);
    let n2447: ZW = zw_add(n2443, n2445);
    let n2448: ZW = zw_cellmix_b(361u64, n903, 1542469173u64);
    let n2449: ZW = zw_cellmix_b(361u64, n903, 668265263u64);
    let n2450: ZW = zw_add(n2434, n2448);
    let n2451: ZW = zw_add(n2435, n2449);
    let n2452: ZW = zw_cellmix_n(369u64, n907, 1542469173u64);
    let n2453: ZW = zw_cellmix_n(369u64, n907, 668265263u64);
    let n2454: ZW = zw_add(n2450, n2452);
    let n2455: ZW = zw_add(n2451, n2453);
    let n2456: ZW = zw_cellmix_n(370u64, n905, 1542469173u64);
    let n2457: ZW = zw_cellmix_n(370u64, n905, 668265263u64);
    let n2458: ZW = zw_add(n2454, n2456);
    let n2459: ZW = zw_add(n2455, n2457);
    let n2460: ZW = zw_cellmix_b(361u64, n925, 1542469173u64);
    let n2461: ZW = zw_cellmix_b(361u64, n925, 668265263u64);
    let n2462: ZW = zw_add(n2434, n2460);
    let n2463: ZW = zw_add(n2435, n2461);
    let n2464: ZW = zw_cellmix_n(369u64, n929, 1542469173u64);
    let n2465: ZW = zw_cellmix_n(369u64, n929, 668265263u64);
    let n2466: ZW = zw_add(n2462, n2464);
    let n2467: ZW = zw_add(n2463, n2465);
    let n2468: ZW = zw_cellmix_n(370u64, n927, 1542469173u64);
    let n2469: ZW = zw_cellmix_n(370u64, n927, 668265263u64);
    let n2470: ZW = zw_add(n2466, n2468);
    let n2471: ZW = zw_add(n2467, n2469);
    let n2472: ZW = zw_cellmix_n(286u64, n933, 1542469173u64);
    let n2473: ZW = zw_cellmix_n(286u64, n933, 668265263u64);
    let n2474: ZW = zw_add(n2402, n2472);
    let n2475: ZW = zw_add(n2403, n2473);
    let n2476: ZW = zw_add(n2474, n2408);
    let n2477: ZW = zw_add(n2475, n2409);
    let n2478: ZW = zw_cellmix_b(294u64, zb_splat(true), 1542469173u64);
    let n2479: ZW = zw_cellmix_b(294u64, zb_splat(true), 668265263u64);
    let n2480: ZW = zw_add(n2476, n2478);
    let n2481: ZW = zw_add(n2477, n2479);
    let n2482: ZW = zw_add(n2480, n2416);
    let n2483: ZW = zw_add(n2481, n2417);
    let n2484: ZW = zw_add(n2482, n2420);
    let n2485: ZW = zw_add(n2483, n2421);
    let n2486: ZW = zw_add(n2484, n2424);
    let n2487: ZW = zw_add(n2485, n2425);
    let n2488: ZW = zw_add(n2486, n2428);
    let n2489: ZW = zw_add(n2487, n2429);
    let n2490: ZW = zw_add(n2488, n2432);
    let n2491: ZW = zw_add(n2489, n2433);
    let n2492: ZW = zw_add(n2490, n2436);
    let n2493: ZW = zw_add(n2491, n2437);
    let n2494: ZW = zw_cellmix_n(369u64, n937, 1542469173u64);
    let n2495: ZW = zw_cellmix_n(369u64, n937, 668265263u64);
    let n2496: ZW = zw_add(n2492, n2494);
    let n2497: ZW = zw_add(n2493, n2495);
    let n2498: ZW = zw_cellmix_n(370u64, n935, 1542469173u64);
    let n2499: ZW = zw_cellmix_n(370u64, n935, 668265263u64);
    let n2500: ZW = zw_add(n2496, n2498);
    let n2501: ZW = zw_add(n2497, n2499);
    let n2502: ZW = zw_add(n2490, n2448);
    let n2503: ZW = zw_add(n2491, n2449);
    let n2504: ZW = zw_cellmix_n(369u64, n943, 1542469173u64);
    let n2505: ZW = zw_cellmix_n(369u64, n943, 668265263u64);
    let n2506: ZW = zw_add(n2502, n2504);
    let n2507: ZW = zw_add(n2503, n2505);
    let n2508: ZW = zw_cellmix_n(370u64, n941, 1542469173u64);
    let n2509: ZW = zw_cellmix_n(370u64, n941, 668265263u64);
    let n2510: ZW = zw_add(n2506, n2508);
    let n2511: ZW = zw_add(n2507, n2509);
    let n2512: ZW = zw_add(n2490, n2460);
    let n2513: ZW = zw_add(n2491, n2461);
    let n2514: ZW = zw_cellmix_n(369u64, n949, 1542469173u64);
    let n2515: ZW = zw_cellmix_n(369u64, n949, 668265263u64);
    let n2516: ZW = zw_add(n2512, n2514);
    let n2517: ZW = zw_add(n2513, n2515);
    let n2518: ZW = zw_cellmix_n(370u64, n947, 1542469173u64);
    let n2519: ZW = zw_cellmix_n(370u64, n947, 668265263u64);
    let n2520: ZW = zw_add(n2516, n2518);
    let n2521: ZW = zw_add(n2517, n2519);
    let n2522: ZW = zw_cellmix_n(20u64, n961, 1542469173u64);
    let n2523: ZW = zw_cellmix_n(20u64, n961, 668265263u64);
    let n2524: ZW = zw_add(n2382, n2522);
    let n2525: ZW = zw_add(n2383, n2523);
    let n2526: ZW = zw_cellmix_b(41u64, n962, 1542469173u64);
    let n2527: ZW = zw_cellmix_b(41u64, n962, 668265263u64);
    let n2528: ZW = zw_add(n2524, n2526);
    let n2529: ZW = zw_add(n2525, n2527);
    let n2530: ZW = zw_cellmix_n(281u64, n963, 1542469173u64);
    let n2531: ZW = zw_cellmix_n(281u64, n963, 668265263u64);
    let n2532: ZW = zw_add(n2528, n2530);
    let n2533: ZW = zw_add(n2529, n2531);
    let n2534: ZW = zw_cellmix_n(283u64, n964, 1542469173u64);
    let n2535: ZW = zw_cellmix_n(283u64, n964, 668265263u64);
    let n2536: ZW = zw_add(n2532, n2534);
    let n2537: ZW = zw_add(n2533, n2535);
    let n2538: ZW = zw_cellmix_n(284u64, n965, 1542469173u64);
    let n2539: ZW = zw_cellmix_n(284u64, n965, 668265263u64);
    let n2540: ZW = zw_add(n2536, n2538);
    let n2541: ZW = zw_add(n2537, n2539);
    let n2542: ZW = zw_add(n2540, n2404);
    let n2543: ZW = zw_add(n2541, n2405);
    let n2544: ZW = zw_cellmix_b(293u64, zb_splat(true), 1542469173u64);
    let n2545: ZW = zw_cellmix_b(293u64, zb_splat(true), 668265263u64);
    let n2546: ZW = zw_add(n2542, n2544);
    let n2547: ZW = zw_add(n2543, n2545);
    let n2548: ZW = zw_add(n2546, n2412);
    let n2549: ZW = zw_add(n2547, n2413);
    let n2550: ZW = zw_cellmix_n(300u64, n974, 1542469173u64);
    let n2551: ZW = zw_cellmix_n(300u64, n974, 668265263u64);
    let n2552: ZW = zw_add(n2548, n2550);
    let n2553: ZW = zw_add(n2549, n2551);
    let n2554: ZW = zw_cellmix_n(357u64, n966, 1542469173u64);
    let n2555: ZW = zw_cellmix_n(357u64, n966, 668265263u64);
    let n2556: ZW = zw_add(n2552, n2554);
    let n2557: ZW = zw_add(n2553, n2555);
    let n2558: ZW = zw_cellmix_n(358u64, n967, 1542469173u64);
    let n2559: ZW = zw_cellmix_n(358u64, n967, 668265263u64);
    let n2560: ZW = zw_add(n2556, n2558);
    let n2561: ZW = zw_add(n2557, n2559);
    let n2562: ZW = zw_cellmix_n(359u64, n968, 1542469173u64);
    let n2563: ZW = zw_cellmix_n(359u64, n968, 668265263u64);
    let n2564: ZW = zw_add(n2560, n2562);
    let n2565: ZW = zw_add(n2561, n2563);
    let n2566: ZW = zw_cellmix_n(360u64, n969, 1542469173u64);
    let n2567: ZW = zw_cellmix_n(360u64, n969, 668265263u64);
    let n2568: ZW = zw_add(n2564, n2566);
    let n2569: ZW = zw_add(n2565, n2567);
    let n2570: ZW = zw_add(n2568, n2436);
    let n2571: ZW = zw_add(n2569, n2437);
    let n2572: ZW = zw_cellmix_n(369u64, n975, 1542469173u64);
    let n2573: ZW = zw_cellmix_n(369u64, n975, 668265263u64);
    let n2574: ZW = zw_add(n2570, n2572);
    let n2575: ZW = zw_add(n2571, n2573);
    let n2576: ZW = zw_cellmix_n(370u64, n971, 1542469173u64);
    let n2577: ZW = zw_cellmix_n(370u64, n971, 668265263u64);
    let n2578: ZW = zw_add(n2574, n2576);
    let n2579: ZW = zw_add(n2575, n2577);
    let n2580: ZW = zw_cellmix_n(358u64, n980, 1542469173u64);
    let n2581: ZW = zw_cellmix_n(358u64, n980, 668265263u64);
    let n2582: ZW = zw_add(n2556, n2580);
    let n2583: ZW = zw_add(n2557, n2581);
    let n2584: ZW = zw_cellmix_n(359u64, n981, 1542469173u64);
    let n2585: ZW = zw_cellmix_n(359u64, n981, 668265263u64);
    let n2586: ZW = zw_add(n2582, n2584);
    let n2587: ZW = zw_add(n2583, n2585);
    let n2588: ZW = zw_add(n2586, n2566);
    let n2589: ZW = zw_add(n2587, n2567);
    let n2590: ZW = zw_add(n2588, n2448);
    let n2591: ZW = zw_add(n2589, n2449);
    let n2592: ZW = zw_cellmix_n(369u64, n985, 1542469173u64);
    let n2593: ZW = zw_cellmix_n(369u64, n985, 668265263u64);
    let n2594: ZW = zw_add(n2590, n2592);
    let n2595: ZW = zw_add(n2591, n2593);
    let n2596: ZW = zw_cellmix_n(370u64, n983, 1542469173u64);
    let n2597: ZW = zw_cellmix_n(370u64, n983, 668265263u64);
    let n2598: ZW = zw_add(n2594, n2596);
    let n2599: ZW = zw_add(n2595, n2597);
    let n2600: ZW = zw_cellmix_n(359u64, n989, 1542469173u64);
    let n2601: ZW = zw_cellmix_n(359u64, n989, 668265263u64);
    let n2602: ZW = zw_add(n2582, n2600);
    let n2603: ZW = zw_add(n2583, n2601);
    let n2604: ZW = zw_add(n2602, n2566);
    let n2605: ZW = zw_add(n2603, n2567);
    let n2606: ZW = zw_add(n2604, n2460);
    let n2607: ZW = zw_add(n2605, n2461);
    let n2608: ZW = zw_cellmix_n(369u64, n993, 1542469173u64);
    let n2609: ZW = zw_cellmix_n(369u64, n993, 668265263u64);
    let n2610: ZW = zw_add(n2606, n2608);
    let n2611: ZW = zw_add(n2607, n2609);
    let n2612: ZW = zw_cellmix_n(370u64, n991, 1542469173u64);
    let n2613: ZW = zw_cellmix_n(370u64, n991, 668265263u64);
    let n2614: ZW = zw_add(n2610, n2612);
    let n2615: ZW = zw_add(n2611, n2613);
    let n2616: ZW = zw_cellmix_n(357u64, n1001, 1542469173u64);
    let n2617: ZW = zw_cellmix_n(357u64, n1001, 668265263u64);
    let n2618: ZW = zw_add(n2552, n2616);
    let n2619: ZW = zw_add(n2553, n2617);
    let n2620: ZW = zw_cellmix_n(358u64, n1002, 1542469173u64);
    let n2621: ZW = zw_cellmix_n(358u64, n1002, 668265263u64);
    let n2622: ZW = zw_add(n2618, n2620);
    let n2623: ZW = zw_add(n2619, n2621);
    let n2624: ZW = zw_cellmix_n(359u64, n1003, 1542469173u64);
    let n2625: ZW = zw_cellmix_n(359u64, n1003, 668265263u64);
    let n2626: ZW = zw_add(n2622, n2624);
    let n2627: ZW = zw_add(n2623, n2625);
    let n2628: ZW = zw_cellmix_n(360u64, n1004, 1542469173u64);
    let n2629: ZW = zw_cellmix_n(360u64, n1004, 668265263u64);
    let n2630: ZW = zw_add(n2626, n2628);
    let n2631: ZW = zw_add(n2627, n2629);
    let n2632: ZW = zw_add(n2630, n2436);
    let n2633: ZW = zw_add(n2631, n2437);
    let n2634: ZW = zw_cellmix_n(369u64, n1008, 1542469173u64);
    let n2635: ZW = zw_cellmix_n(369u64, n1008, 668265263u64);
    let n2636: ZW = zw_add(n2632, n2634);
    let n2637: ZW = zw_add(n2633, n2635);
    let n2638: ZW = zw_cellmix_n(370u64, n1006, 1542469173u64);
    let n2639: ZW = zw_cellmix_n(370u64, n1006, 668265263u64);
    let n2640: ZW = zw_add(n2636, n2638);
    let n2641: ZW = zw_add(n2637, n2639);
    let n2642: ZW = zw_add(n2618, n2580);
    let n2643: ZW = zw_add(n2619, n2581);
    let n2644: ZW = zw_add(n2642, n2584);
    let n2645: ZW = zw_add(n2643, n2585);
    let n2646: ZW = zw_add(n2644, n2628);
    let n2647: ZW = zw_add(n2645, n2629);
    let n2648: ZW = zw_add(n2646, n2448);
    let n2649: ZW = zw_add(n2647, n2449);
    let n2650: ZW = zw_cellmix_n(369u64, n1014, 1542469173u64);
    let n2651: ZW = zw_cellmix_n(369u64, n1014, 668265263u64);
    let n2652: ZW = zw_add(n2648, n2650);
    let n2653: ZW = zw_add(n2649, n2651);
    let n2654: ZW = zw_cellmix_n(370u64, n1012, 1542469173u64);
    let n2655: ZW = zw_cellmix_n(370u64, n1012, 668265263u64);
    let n2656: ZW = zw_add(n2652, n2654);
    let n2657: ZW = zw_add(n2653, n2655);
    let n2658: ZW = zw_add(n2642, n2600);
    let n2659: ZW = zw_add(n2643, n2601);
    let n2660: ZW = zw_add(n2658, n2628);
    let n2661: ZW = zw_add(n2659, n2629);
    let n2662: ZW = zw_add(n2660, n2460);
    let n2663: ZW = zw_add(n2661, n2461);
    let n2664: ZW = zw_cellmix_n(369u64, n1020, 1542469173u64);
    let n2665: ZW = zw_cellmix_n(369u64, n1020, 668265263u64);
    let n2666: ZW = zw_add(n2662, n2664);
    let n2667: ZW = zw_add(n2663, n2665);
    let n2668: ZW = zw_cellmix_n(370u64, n1018, 1542469173u64);
    let n2669: ZW = zw_cellmix_n(370u64, n1018, 668265263u64);
    let n2670: ZW = zw_add(n2666, n2668);
    let n2671: ZW = zw_add(n2667, n2669);
    let n2672: ZW = zw_cellmix_n(360u64, n1023, 1542469173u64);
    let n2673: ZW = zw_cellmix_n(360u64, n1023, 668265263u64);
    let n2674: ZW = zw_add(n2626, n2672);
    let n2675: ZW = zw_add(n2627, n2673);
    let n2676: ZW = zw_add(n2674, n2436);
    let n2677: ZW = zw_add(n2675, n2437);
    let n2678: ZW = zw_add(n2676, n2634);
    let n2679: ZW = zw_add(n2677, n2635);
    let n2680: ZW = zw_cellmix_n(370u64, n1024, 1542469173u64);
    let n2681: ZW = zw_cellmix_n(370u64, n1024, 668265263u64);
    let n2682: ZW = zw_add(n2678, n2680);
    let n2683: ZW = zw_add(n2679, n2681);
    let n2684: ZW = zw_add(n2644, n2672);
    let n2685: ZW = zw_add(n2645, n2673);
    let n2686: ZW = zw_add(n2684, n2448);
    let n2687: ZW = zw_add(n2685, n2449);
    let n2688: ZW = zw_add(n2686, n2650);
    let n2689: ZW = zw_add(n2687, n2651);
    let n2690: ZW = zw_cellmix_n(370u64, n1026, 1542469173u64);
    let n2691: ZW = zw_cellmix_n(370u64, n1026, 668265263u64);
    let n2692: ZW = zw_add(n2688, n2690);
    let n2693: ZW = zw_add(n2689, n2691);
    let n2694: ZW = zw_add(n2658, n2672);
    let n2695: ZW = zw_add(n2659, n2673);
    let n2696: ZW = zw_add(n2694, n2460);
    let n2697: ZW = zw_add(n2695, n2461);
    let n2698: ZW = zw_add(n2696, n2664);
    let n2699: ZW = zw_add(n2697, n2665);
    let n2700: ZW = zw_cellmix_n(370u64, n1028, 1542469173u64);
    let n2701: ZW = zw_cellmix_n(370u64, n1028, 668265263u64);
    let n2702: ZW = zw_add(n2698, n2700);
    let n2703: ZW = zw_add(n2699, n2701);
    let n2704: ZW = zw_add(n2540, n2472);
    let n2705: ZW = zw_add(n2541, n2473);
    let n2706: ZW = zw_add(n2704, n2544);
    let n2707: ZW = zw_add(n2705, n2545);
    let n2708: ZW = zw_add(n2706, n2478);
    let n2709: ZW = zw_add(n2707, n2479);
    let n2710: ZW = zw_add(n2708, n2550);
    let n2711: ZW = zw_add(n2709, n2551);
    let n2712: ZW = zw_add(n2710, n2554);
    let n2713: ZW = zw_add(n2711, n2555);
    let n2714: ZW = zw_add(n2712, n2558);
    let n2715: ZW = zw_add(n2713, n2559);
    let n2716: ZW = zw_add(n2714, n2562);
    let n2717: ZW = zw_add(n2715, n2563);
    let n2718: ZW = zw_add(n2716, n2566);
    let n2719: ZW = zw_add(n2717, n2567);
    let n2720: ZW = zw_add(n2718, n2436);
    let n2721: ZW = zw_add(n2719, n2437);
    let n2722: ZW = zw_cellmix_n(369u64, n1034, 1542469173u64);
    let n2723: ZW = zw_cellmix_n(369u64, n1034, 668265263u64);
    let n2724: ZW = zw_add(n2720, n2722);
    let n2725: ZW = zw_add(n2721, n2723);
    let n2726: ZW = zw_cellmix_n(370u64, n1032, 1542469173u64);
    let n2727: ZW = zw_cellmix_n(370u64, n1032, 668265263u64);
    let n2728: ZW = zw_add(n2724, n2726);
    let n2729: ZW = zw_add(n2725, n2727);
    let n2730: ZW = zw_add(n2712, n2580);
    let n2731: ZW = zw_add(n2713, n2581);
    let n2732: ZW = zw_add(n2730, n2584);
    let n2733: ZW = zw_add(n2731, n2585);
    let n2734: ZW = zw_add(n2732, n2566);
    let n2735: ZW = zw_add(n2733, n2567);
    let n2736: ZW = zw_add(n2734, n2448);
    let n2737: ZW = zw_add(n2735, n2449);
    let n2738: ZW = zw_cellmix_n(369u64, n1040, 1542469173u64);
    let n2739: ZW = zw_cellmix_n(369u64, n1040, 668265263u64);
    let n2740: ZW = zw_add(n2736, n2738);
    let n2741: ZW = zw_add(n2737, n2739);
    let n2742: ZW = zw_cellmix_n(370u64, n1038, 1542469173u64);
    let n2743: ZW = zw_cellmix_n(370u64, n1038, 668265263u64);
    let n2744: ZW = zw_add(n2740, n2742);
    let n2745: ZW = zw_add(n2741, n2743);
    let n2746: ZW = zw_add(n2730, n2600);
    let n2747: ZW = zw_add(n2731, n2601);
    let n2748: ZW = zw_add(n2746, n2566);
    let n2749: ZW = zw_add(n2747, n2567);
    let n2750: ZW = zw_add(n2748, n2460);
    let n2751: ZW = zw_add(n2749, n2461);
    let n2752: ZW = zw_cellmix_n(369u64, n1046, 1542469173u64);
    let n2753: ZW = zw_cellmix_n(369u64, n1046, 668265263u64);
    let n2754: ZW = zw_add(n2750, n2752);
    let n2755: ZW = zw_add(n2751, n2753);
    let n2756: ZW = zw_cellmix_n(370u64, n1044, 1542469173u64);
    let n2757: ZW = zw_cellmix_n(370u64, n1044, 668265263u64);
    let n2758: ZW = zw_add(n2754, n2756);
    let n2759: ZW = zw_add(n2755, n2757);
    let n2760: ZW = zw_add(n2710, n2616);
    let n2761: ZW = zw_add(n2711, n2617);
    let n2762: ZW = zw_add(n2760, n2620);
    let n2763: ZW = zw_add(n2761, n2621);
    let n2764: ZW = zw_add(n2762, n2624);
    let n2765: ZW = zw_add(n2763, n2625);
    let n2766: ZW = zw_add(n2764, n2628);
    let n2767: ZW = zw_add(n2765, n2629);
    let n2768: ZW = zw_add(n2766, n2436);
    let n2769: ZW = zw_add(n2767, n2437);
    let n2770: ZW = zw_cellmix_n(369u64, n1052, 1542469173u64);
    let n2771: ZW = zw_cellmix_n(369u64, n1052, 668265263u64);
    let n2772: ZW = zw_add(n2768, n2770);
    let n2773: ZW = zw_add(n2769, n2771);
    let n2774: ZW = zw_cellmix_n(370u64, n1050, 1542469173u64);
    let n2775: ZW = zw_cellmix_n(370u64, n1050, 668265263u64);
    let n2776: ZW = zw_add(n2772, n2774);
    let n2777: ZW = zw_add(n2773, n2775);
    let n2778: ZW = zw_add(n2760, n2580);
    let n2779: ZW = zw_add(n2761, n2581);
    let n2780: ZW = zw_add(n2778, n2584);
    let n2781: ZW = zw_add(n2779, n2585);
    let n2782: ZW = zw_add(n2780, n2628);
    let n2783: ZW = zw_add(n2781, n2629);
    let n2784: ZW = zw_add(n2782, n2448);
    let n2785: ZW = zw_add(n2783, n2449);
    let n2786: ZW = zw_cellmix_n(369u64, n1058, 1542469173u64);
    let n2787: ZW = zw_cellmix_n(369u64, n1058, 668265263u64);
    let n2788: ZW = zw_add(n2784, n2786);
    let n2789: ZW = zw_add(n2785, n2787);
    let n2790: ZW = zw_cellmix_n(370u64, n1056, 1542469173u64);
    let n2791: ZW = zw_cellmix_n(370u64, n1056, 668265263u64);
    let n2792: ZW = zw_add(n2788, n2790);
    let n2793: ZW = zw_add(n2789, n2791);
    let n2794: ZW = zw_add(n2778, n2600);
    let n2795: ZW = zw_add(n2779, n2601);
    let n2796: ZW = zw_add(n2794, n2628);
    let n2797: ZW = zw_add(n2795, n2629);
    let n2798: ZW = zw_add(n2796, n2460);
    let n2799: ZW = zw_add(n2797, n2461);
    let n2800: ZW = zw_cellmix_n(369u64, n1064, 1542469173u64);
    let n2801: ZW = zw_cellmix_n(369u64, n1064, 668265263u64);
    let n2802: ZW = zw_add(n2798, n2800);
    let n2803: ZW = zw_add(n2799, n2801);
    let n2804: ZW = zw_cellmix_n(370u64, n1062, 1542469173u64);
    let n2805: ZW = zw_cellmix_n(370u64, n1062, 668265263u64);
    let n2806: ZW = zw_add(n2802, n2804);
    let n2807: ZW = zw_add(n2803, n2805);
    let n2808: ZW = zw_add(n2764, n2672);
    let n2809: ZW = zw_add(n2765, n2673);
    let n2810: ZW = zw_add(n2808, n2436);
    let n2811: ZW = zw_add(n2809, n2437);
    let n2812: ZW = zw_add(n2810, n2770);
    let n2813: ZW = zw_add(n2811, n2771);
    let n2814: ZW = zw_cellmix_n(370u64, n1066, 1542469173u64);
    let n2815: ZW = zw_cellmix_n(370u64, n1066, 668265263u64);
    let n2816: ZW = zw_add(n2812, n2814);
    let n2817: ZW = zw_add(n2813, n2815);
    let n2818: ZW = zw_add(n2780, n2672);
    let n2819: ZW = zw_add(n2781, n2673);
    let n2820: ZW = zw_add(n2818, n2448);
    let n2821: ZW = zw_add(n2819, n2449);
    let n2822: ZW = zw_add(n2820, n2786);
    let n2823: ZW = zw_add(n2821, n2787);
    let n2824: ZW = zw_cellmix_n(370u64, n1068, 1542469173u64);
    let n2825: ZW = zw_cellmix_n(370u64, n1068, 668265263u64);
    let n2826: ZW = zw_add(n2822, n2824);
    let n2827: ZW = zw_add(n2823, n2825);
    let n2828: ZW = zw_add(n2794, n2672);
    let n2829: ZW = zw_add(n2795, n2673);
    let n2830: ZW = zw_add(n2828, n2460);
    let n2831: ZW = zw_add(n2829, n2461);
    let n2832: ZW = zw_add(n2830, n2800);
    let n2833: ZW = zw_add(n2831, n2801);
    let n2834: ZW = zw_cellmix_n(370u64, n1070, 1542469173u64);
    let n2835: ZW = zw_cellmix_n(370u64, n1070, 668265263u64);
    let n2836: ZW = zw_add(n2832, n2834);
    let n2837: ZW = zw_add(n2833, n2835);
    let n2838: ZW = zw_add(zw_splat(0u64), n2356);
    let n2839: ZW = zw_add(zw_splat(0u64), n2357);
    let n2840: ZW = zw_add(n2838, n2360);
    let n2841: ZW = zw_add(n2839, n2361);
    let n2842: ZW = zw_add(n2840, n2364);
    let n2843: ZW = zw_add(n2841, n2365);
    let n2844: ZW = zw_cellmix_n(87u64, n1176, 1542469173u64);
    let n2845: ZW = zw_cellmix_n(87u64, n1176, 668265263u64);
    let n2846: ZW = zw_add(n2842, n2844);
    let n2847: ZW = zw_add(n2843, n2845);
    let n2848: ZW = zw_add(n2846, n2384);
    let n2849: ZW = zw_add(n2847, n2385);
    let n2850: ZW = zw_add(n2848, n2388);
    let n2851: ZW = zw_add(n2849, n2389);
    let n2852: ZW = zw_add(n2846, n2522);
    let n2853: ZW = zw_add(n2847, n2523);
    let n2854: ZW = zw_add(n2852, n2526);
    let n2855: ZW = zw_add(n2853, n2527);
    let n2856: ZW = zw_cellmix_n(87u64, n1918, 1542469173u64);
    let n2857: ZW = zw_cellmix_n(87u64, n1918, 668265263u64);
    let n2858: ZW = zw_add(n2842, n2856);
    let n2859: ZW = zw_add(n2843, n2857);
    let n2860: ZW = zw_add(n2858, n2384);
    let n2861: ZW = zw_add(n2859, n2385);
    let n2862: ZW = zw_add(n2860, n2388);
    let n2863: ZW = zw_add(n2861, n2389);
    let n2864: ZW = zw_cellmix_n(20u64, n2012, 1542469173u64);
    let n2865: ZW = zw_cellmix_n(20u64, n2012, 668265263u64);
    let n2866: ZW = zw_add(n2858, n2864);
    let n2867: ZW = zw_add(n2859, n2865);
    let n2868: ZW = zw_cellmix_b(41u64, n2013, 1542469173u64);
    let n2869: ZW = zw_cellmix_b(41u64, n2013, 668265263u64);
    let n2870: ZW = zw_add(n2866, n2868);
    let n2871: ZW = zw_add(n2867, n2869);
    let n2872: ZW = zw_cellmix_b(38u64, n2028, 1542469173u64);
    let n2873: ZW = zw_cellmix_b(38u64, n2028, 668265263u64);
    let n2874: ZW = zw_add(zw_splat(0u64), n2872);
    let n2875: ZW = zw_add(zw_splat(0u64), n2873);
    let n2876: ZW = zw_cellmix_n(39u64, n2026, 1542469173u64);
    let n2877: ZW = zw_cellmix_n(39u64, n2026, 668265263u64);
    let n2878: ZW = zw_add(n2874, n2876);
    let n2879: ZW = zw_add(n2875, n2877);
    let n2880: ZW = zw_add(n2878, n2356);
    let n2881: ZW = zw_add(n2879, n2357);
    let n2882: ZW = zw_add(n2880, n2360);
    let n2883: ZW = zw_add(n2881, n2361);
    let n2884: ZW = zw_add(n2882, n2364);
    let n2885: ZW = zw_add(n2883, n2365);
    let n2886: ZW = zw_cellmix_n(87u64, n2025, 1542469173u64);
    let n2887: ZW = zw_cellmix_n(87u64, n2025, 668265263u64);
    let n2888: ZW = zw_add(n2884, n2886);
    let n2889: ZW = zw_add(n2885, n2887);
    let n2890: ZW = zw_add(n2888, n2384);
    let n2891: ZW = zw_add(n2889, n2385);
    let n2892: ZW = zw_cellmix_n(20u64, n2031, 1542469173u64);
    let n2893: ZW = zw_cellmix_n(20u64, n2031, 668265263u64);
    let n2894: ZW = zw_add(n2888, n2892);
    let n2895: ZW = zw_add(n2889, n2893);
    let n2896: ZW = zw_cellmix_n(300u64, n2108, 1542469173u64);
    let n2897: ZW = zw_cellmix_n(300u64, n2108, 668265263u64);
    let n2898: ZW = zw_add(n2370, n2896);
    let n2899: ZW = zw_add(n2371, n2897);
    let n2900: ZW = zw_cellmix_n(366u64, n2110, 1542469173u64);
    let n2901: ZW = zw_cellmix_n(366u64, n2110, 668265263u64);
    let n2902: ZW = zw_add(n2898, n2900);
    let n2903: ZW = zw_add(n2899, n2901);
    let n2904: ZW = zw_cellmix_n(367u64, n2111, 1542469173u64);
    let n2905: ZW = zw_cellmix_n(367u64, n2111, 668265263u64);
    let n2906: ZW = zw_add(n2902, n2904);
    let n2907: ZW = zw_add(n2903, n2905);
    let n2908: ZW = zw_cellmix_n(20u64, n2100, 1542469173u64);
    let n2909: ZW = zw_cellmix_n(20u64, n2100, 668265263u64);
    let n2910: ZW = zw_add(n2906, n2908);
    let n2911: ZW = zw_add(n2907, n2909);
    let n2912: ZW = zw_add(n2910, n2388);
    let n2913: ZW = zw_add(n2911, n2389);
    let n2914: ZW = zw_cellmix_n(280u64, n2101, 1542469173u64);
    let n2915: ZW = zw_cellmix_n(280u64, n2101, 668265263u64);
    let n2916: ZW = zw_add(n2912, n2914);
    let n2917: ZW = zw_add(n2913, n2915);
    let n2918: ZW = zw_cellmix_n(282u64, n2102, 1542469173u64);
    let n2919: ZW = zw_cellmix_n(282u64, n2102, 668265263u64);
    let n2920: ZW = zw_add(n2916, n2918);
    let n2921: ZW = zw_add(n2917, n2919);
    let n2922: ZW = zw_cellmix_n(283u64, n2103, 1542469173u64);
    let n2923: ZW = zw_cellmix_n(283u64, n2103, 668265263u64);
    let n2924: ZW = zw_add(n2920, n2922);
    let n2925: ZW = zw_add(n2921, n2923);
    let n2926: ZW = zw_cellmix_n(285u64, n2104, 1542469173u64);
    let n2927: ZW = zw_cellmix_n(285u64, n2104, 668265263u64);
    let n2928: ZW = zw_add(n2924, n2926);
    let n2929: ZW = zw_add(n2925, n2927);
    let n2930: ZW = zw_cellmix_b(292u64, n2105, 1542469173u64);
    let n2931: ZW = zw_cellmix_b(292u64, n2105, 668265263u64);
    let n2932: ZW = zw_add(n2928, n2930);
    let n2933: ZW = zw_add(n2929, n2931);
    let n2934: ZW = zw_cellmix_b(293u64, n2106, 1542469173u64);
    let n2935: ZW = zw_cellmix_b(293u64, n2106, 668265263u64);
    let n2936: ZW = zw_add(n2932, n2934);
    let n2937: ZW = zw_add(n2933, n2935);
    let n2938: ZW = zw_cellmix_n(299u64, n2124, 1542469173u64);
    let n2939: ZW = zw_cellmix_n(299u64, n2124, 668265263u64);
    let n2940: ZW = zw_add(n2936, n2938);
    let n2941: ZW = zw_add(n2937, n2939);
    let n2942: ZW = zw_cellmix_n(356u64, r_c356, 1542469173u64);
    let n2943: ZW = zw_cellmix_n(356u64, r_c356, 668265263u64);
    let n2944: ZW = zw_add(n2940, n2942);
    let n2945: ZW = zw_add(n2941, n2943);
    let n2946: ZW = zw_cellmix_n(357u64, r_c357, 1542469173u64);
    let n2947: ZW = zw_cellmix_n(357u64, r_c357, 668265263u64);
    let n2948: ZW = zw_add(n2944, n2946);
    let n2949: ZW = zw_add(n2945, n2947);
    let n2950: ZW = zw_cellmix_n(358u64, r_c358, 1542469173u64);
    let n2951: ZW = zw_cellmix_n(358u64, r_c358, 668265263u64);
    let n2952: ZW = zw_add(n2948, n2950);
    let n2953: ZW = zw_add(n2949, n2951);
    let n2954: ZW = zw_cellmix_n(359u64, r_c359, 1542469173u64);
    let n2955: ZW = zw_cellmix_n(359u64, r_c359, 668265263u64);
    let n2956: ZW = zw_add(n2952, n2954);
    let n2957: ZW = zw_add(n2953, n2955);
    let n2958: ZW = zw_cellmix_b(360u64, n2109, 1542469173u64);
    let n2959: ZW = zw_cellmix_b(360u64, n2109, 668265263u64);
    let n2960: ZW = zw_add(n2956, n2958);
    let n2961: ZW = zw_add(n2957, n2959);
    let n2962: ZW = zw_cellmix_n(368u64, n2125, 1542469173u64);
    let n2963: ZW = zw_cellmix_n(368u64, n2125, 668265263u64);
    let n2964: ZW = zw_add(n2960, n2962);
    let n2965: ZW = zw_add(n2961, n2963);
    let n2966: ZW = zw_cellmix_n(369u64, n2113, 1542469173u64);
    let n2967: ZW = zw_cellmix_n(369u64, n2113, 668265263u64);
    let n2968: ZW = zw_add(n2964, n2966);
    let n2969: ZW = zw_add(n2965, n2967);
    let n2970: ZW = zw_cellmix_b(360u64, n2138, 1542469173u64);
    let n2971: ZW = zw_cellmix_b(360u64, n2138, 668265263u64);
    let n2972: ZW = zw_add(n2956, n2970);
    let n2973: ZW = zw_add(n2957, n2971);
    let n2974: ZW = zw_cellmix_n(368u64, n2142, 1542469173u64);
    let n2975: ZW = zw_cellmix_n(368u64, n2142, 668265263u64);
    let n2976: ZW = zw_add(n2972, n2974);
    let n2977: ZW = zw_add(n2973, n2975);
    let n2978: ZW = zw_cellmix_n(369u64, n2140, 1542469173u64);
    let n2979: ZW = zw_cellmix_n(369u64, n2140, 668265263u64);
    let n2980: ZW = zw_add(n2976, n2978);
    let n2981: ZW = zw_add(n2977, n2979);
    let n2982: ZW = zw_cellmix_b(360u64, n2154, 1542469173u64);
    let n2983: ZW = zw_cellmix_b(360u64, n2154, 668265263u64);
    let n2984: ZW = zw_add(n2956, n2982);
    let n2985: ZW = zw_add(n2957, n2983);
    let n2986: ZW = zw_cellmix_n(368u64, n2158, 1542469173u64);
    let n2987: ZW = zw_cellmix_n(368u64, n2158, 668265263u64);
    let n2988: ZW = zw_add(n2984, n2986);
    let n2989: ZW = zw_add(n2985, n2987);
    let n2990: ZW = zw_cellmix_n(369u64, n2156, 1542469173u64);
    let n2991: ZW = zw_cellmix_n(369u64, n2156, 668265263u64);
    let n2992: ZW = zw_add(n2988, n2990);
    let n2993: ZW = zw_add(n2989, n2991);
    let n2994: ZW = zw_cellmix_n(285u64, n2165, 1542469173u64);
    let n2995: ZW = zw_cellmix_n(285u64, n2165, 668265263u64);
    let n2996: ZW = zw_add(n2924, n2994);
    let n2997: ZW = zw_add(n2925, n2995);
    let n2998: ZW = zw_add(n2996, n2930);
    let n2999: ZW = zw_add(n2997, n2931);
    let n3000: ZW = zw_cellmix_b(293u64, n2166, 1542469173u64);
    let n3001: ZW = zw_cellmix_b(293u64, n2166, 668265263u64);
    let n3002: ZW = zw_add(n2998, n3000);
    let n3003: ZW = zw_add(n2999, n3001);
    let n3004: ZW = zw_add(n3002, n2938);
    let n3005: ZW = zw_add(n3003, n2939);
    let n3006: ZW = zw_add(n3004, n2942);
    let n3007: ZW = zw_add(n3005, n2943);
    let n3008: ZW = zw_add(n3006, n2946);
    let n3009: ZW = zw_add(n3007, n2947);
    let n3010: ZW = zw_add(n3008, n2950);
    let n3011: ZW = zw_add(n3009, n2951);
    let n3012: ZW = zw_add(n3010, n2954);
    let n3013: ZW = zw_add(n3011, n2955);
    let n3014: ZW = zw_add(n3012, n2958);
    let n3015: ZW = zw_add(n3013, n2959);
    let n3016: ZW = zw_cellmix_n(368u64, n2170, 1542469173u64);
    let n3017: ZW = zw_cellmix_n(368u64, n2170, 668265263u64);
    let n3018: ZW = zw_add(n3014, n3016);
    let n3019: ZW = zw_add(n3015, n3017);
    let n3020: ZW = zw_cellmix_n(369u64, n2168, 1542469173u64);
    let n3021: ZW = zw_cellmix_n(369u64, n2168, 668265263u64);
    let n3022: ZW = zw_add(n3018, n3020);
    let n3023: ZW = zw_add(n3019, n3021);
    let n3024: ZW = zw_add(n3012, n2970);
    let n3025: ZW = zw_add(n3013, n2971);
    let n3026: ZW = zw_cellmix_n(368u64, n2178, 1542469173u64);
    let n3027: ZW = zw_cellmix_n(368u64, n2178, 668265263u64);
    let n3028: ZW = zw_add(n3024, n3026);
    let n3029: ZW = zw_add(n3025, n3027);
    let n3030: ZW = zw_cellmix_n(369u64, n2176, 1542469173u64);
    let n3031: ZW = zw_cellmix_n(369u64, n2176, 668265263u64);
    let n3032: ZW = zw_add(n3028, n3030);
    let n3033: ZW = zw_add(n3029, n3031);
    let n3034: ZW = zw_add(n3012, n2982);
    let n3035: ZW = zw_add(n3013, n2983);
    let n3036: ZW = zw_cellmix_n(368u64, n2186, 1542469173u64);
    let n3037: ZW = zw_cellmix_n(368u64, n2186, 668265263u64);
    let n3038: ZW = zw_add(n3034, n3036);
    let n3039: ZW = zw_add(n3035, n3037);
    let n3040: ZW = zw_cellmix_n(369u64, n2184, 1542469173u64);
    let n3041: ZW = zw_cellmix_n(369u64, n2184, 668265263u64);
    let n3042: ZW = zw_add(n3038, n3040);
    let n3043: ZW = zw_add(n3039, n3041);
    let n3044: ZW = zw_cellmix_n(20u64, n2205, 1542469173u64);
    let n3045: ZW = zw_cellmix_n(20u64, n2205, 668265263u64);
    let n3046: ZW = zw_add(n2906, n3044);
    let n3047: ZW = zw_add(n2907, n3045);
    let n3048: ZW = zw_cellmix_b(41u64, n2206, 1542469173u64);
    let n3049: ZW = zw_cellmix_b(41u64, n2206, 668265263u64);
    let n3050: ZW = zw_add(n3046, n3048);
    let n3051: ZW = zw_add(n3047, n3049);
    let n3052: ZW = zw_cellmix_n(280u64, n2207, 1542469173u64);
    let n3053: ZW = zw_cellmix_n(280u64, n2207, 668265263u64);
    let n3054: ZW = zw_add(n3050, n3052);
    let n3055: ZW = zw_add(n3051, n3053);
    let n3056: ZW = zw_cellmix_n(282u64, n2208, 1542469173u64);
    let n3057: ZW = zw_cellmix_n(282u64, n2208, 668265263u64);
    let n3058: ZW = zw_add(n3054, n3056);
    let n3059: ZW = zw_add(n3055, n3057);
    let n3060: ZW = zw_cellmix_n(283u64, n2209, 1542469173u64);
    let n3061: ZW = zw_cellmix_n(283u64, n2209, 668265263u64);
    let n3062: ZW = zw_add(n3058, n3060);
    let n3063: ZW = zw_add(n3059, n3061);
    let n3064: ZW = zw_add(n3062, n2926);
    let n3065: ZW = zw_add(n3063, n2927);
    let n3066: ZW = zw_cellmix_b(292u64, n2210, 1542469173u64);
    let n3067: ZW = zw_cellmix_b(292u64, n2210, 668265263u64);
    let n3068: ZW = zw_add(n3064, n3066);
    let n3069: ZW = zw_add(n3065, n3067);
    let n3070: ZW = zw_add(n3068, n2934);
    let n3071: ZW = zw_add(n3069, n2935);
    let n3072: ZW = zw_cellmix_n(299u64, n2219, 1542469173u64);
    let n3073: ZW = zw_cellmix_n(299u64, n2219, 668265263u64);
    let n3074: ZW = zw_add(n3070, n3072);
    let n3075: ZW = zw_add(n3071, n3073);
    let n3076: ZW = zw_cellmix_n(356u64, n2211, 1542469173u64);
    let n3077: ZW = zw_cellmix_n(356u64, n2211, 668265263u64);
    let n3078: ZW = zw_add(n3074, n3076);
    let n3079: ZW = zw_add(n3075, n3077);
    let n3080: ZW = zw_cellmix_n(357u64, n2212, 1542469173u64);
    let n3081: ZW = zw_cellmix_n(357u64, n2212, 668265263u64);
    let n3082: ZW = zw_add(n3078, n3080);
    let n3083: ZW = zw_add(n3079, n3081);
    let n3084: ZW = zw_cellmix_n(358u64, n2213, 1542469173u64);
    let n3085: ZW = zw_cellmix_n(358u64, n2213, 668265263u64);
    let n3086: ZW = zw_add(n3082, n3084);
    let n3087: ZW = zw_add(n3083, n3085);
    let n3088: ZW = zw_cellmix_n(359u64, n2214, 1542469173u64);
    let n3089: ZW = zw_cellmix_n(359u64, n2214, 668265263u64);
    let n3090: ZW = zw_add(n3086, n3088);
    let n3091: ZW = zw_add(n3087, n3089);
    let n3092: ZW = zw_add(n3090, n2958);
    let n3093: ZW = zw_add(n3091, n2959);
    let n3094: ZW = zw_cellmix_n(368u64, n2220, 1542469173u64);
    let n3095: ZW = zw_cellmix_n(368u64, n2220, 668265263u64);
    let n3096: ZW = zw_add(n3092, n3094);
    let n3097: ZW = zw_add(n3093, n3095);
    let n3098: ZW = zw_cellmix_n(369u64, n2216, 1542469173u64);
    let n3099: ZW = zw_cellmix_n(369u64, n2216, 668265263u64);
    let n3100: ZW = zw_add(n3096, n3098);
    let n3101: ZW = zw_add(n3097, n3099);
    let n3102: ZW = zw_cellmix_n(357u64, n2229, 1542469173u64);
    let n3103: ZW = zw_cellmix_n(357u64, n2229, 668265263u64);
    let n3104: ZW = zw_add(n3078, n3102);
    let n3105: ZW = zw_add(n3079, n3103);
    let n3106: ZW = zw_cellmix_n(358u64, n2230, 1542469173u64);
    let n3107: ZW = zw_cellmix_n(358u64, n2230, 668265263u64);
    let n3108: ZW = zw_add(n3104, n3106);
    let n3109: ZW = zw_add(n3105, n3107);
    let n3110: ZW = zw_add(n3108, n3088);
    let n3111: ZW = zw_add(n3109, n3089);
    let n3112: ZW = zw_add(n3110, n2970);
    let n3113: ZW = zw_add(n3111, n2971);
    let n3114: ZW = zw_cellmix_n(368u64, n2234, 1542469173u64);
    let n3115: ZW = zw_cellmix_n(368u64, n2234, 668265263u64);
    let n3116: ZW = zw_add(n3112, n3114);
    let n3117: ZW = zw_add(n3113, n3115);
    let n3118: ZW = zw_cellmix_n(369u64, n2232, 1542469173u64);
    let n3119: ZW = zw_cellmix_n(369u64, n2232, 668265263u64);
    let n3120: ZW = zw_add(n3116, n3118);
    let n3121: ZW = zw_add(n3117, n3119);
    let n3122: ZW = zw_cellmix_n(358u64, n2241, 1542469173u64);
    let n3123: ZW = zw_cellmix_n(358u64, n2241, 668265263u64);
    let n3124: ZW = zw_add(n3104, n3122);
    let n3125: ZW = zw_add(n3105, n3123);
    let n3126: ZW = zw_add(n3124, n3088);
    let n3127: ZW = zw_add(n3125, n3089);
    let n3128: ZW = zw_add(n3126, n2982);
    let n3129: ZW = zw_add(n3127, n2983);
    let n3130: ZW = zw_cellmix_n(368u64, n2245, 1542469173u64);
    let n3131: ZW = zw_cellmix_n(368u64, n2245, 668265263u64);
    let n3132: ZW = zw_add(n3128, n3130);
    let n3133: ZW = zw_add(n3129, n3131);
    let n3134: ZW = zw_cellmix_n(369u64, n2243, 1542469173u64);
    let n3135: ZW = zw_cellmix_n(369u64, n2243, 668265263u64);
    let n3136: ZW = zw_add(n3132, n3134);
    let n3137: ZW = zw_add(n3133, n3135);
    let n3138: ZW = zw_cellmix_n(356u64, n2258, 1542469173u64);
    let n3139: ZW = zw_cellmix_n(356u64, n2258, 668265263u64);
    let n3140: ZW = zw_add(n3074, n3138);
    let n3141: ZW = zw_add(n3075, n3139);
    let n3142: ZW = zw_cellmix_n(357u64, n2259, 1542469173u64);
    let n3143: ZW = zw_cellmix_n(357u64, n2259, 668265263u64);
    let n3144: ZW = zw_add(n3140, n3142);
    let n3145: ZW = zw_add(n3141, n3143);
    let n3146: ZW = zw_cellmix_n(358u64, n2260, 1542469173u64);
    let n3147: ZW = zw_cellmix_n(358u64, n2260, 668265263u64);
    let n3148: ZW = zw_add(n3144, n3146);
    let n3149: ZW = zw_add(n3145, n3147);
    let n3150: ZW = zw_cellmix_n(359u64, n2261, 1542469173u64);
    let n3151: ZW = zw_cellmix_n(359u64, n2261, 668265263u64);
    let n3152: ZW = zw_add(n3148, n3150);
    let n3153: ZW = zw_add(n3149, n3151);
    let n3154: ZW = zw_add(n3152, n2958);
    let n3155: ZW = zw_add(n3153, n2959);
    let n3156: ZW = zw_cellmix_n(368u64, n2265, 1542469173u64);
    let n3157: ZW = zw_cellmix_n(368u64, n2265, 668265263u64);
    let n3158: ZW = zw_add(n3154, n3156);
    let n3159: ZW = zw_add(n3155, n3157);
    let n3160: ZW = zw_cellmix_n(369u64, n2263, 1542469173u64);
    let n3161: ZW = zw_cellmix_n(369u64, n2263, 668265263u64);
    let n3162: ZW = zw_add(n3158, n3160);
    let n3163: ZW = zw_add(n3159, n3161);
    let n3164: ZW = zw_add(n3140, n3102);
    let n3165: ZW = zw_add(n3141, n3103);
    let n3166: ZW = zw_add(n3164, n3106);
    let n3167: ZW = zw_add(n3165, n3107);
    let n3168: ZW = zw_add(n3166, n3150);
    let n3169: ZW = zw_add(n3167, n3151);
    let n3170: ZW = zw_add(n3168, n2970);
    let n3171: ZW = zw_add(n3169, n2971);
    let n3172: ZW = zw_cellmix_n(368u64, n2273, 1542469173u64);
    let n3173: ZW = zw_cellmix_n(368u64, n2273, 668265263u64);
    let n3174: ZW = zw_add(n3170, n3172);
    let n3175: ZW = zw_add(n3171, n3173);
    let n3176: ZW = zw_cellmix_n(369u64, n2271, 1542469173u64);
    let n3177: ZW = zw_cellmix_n(369u64, n2271, 668265263u64);
    let n3178: ZW = zw_add(n3174, n3176);
    let n3179: ZW = zw_add(n3175, n3177);
    let n3180: ZW = zw_add(n3164, n3122);
    let n3181: ZW = zw_add(n3165, n3123);
    let n3182: ZW = zw_add(n3180, n3150);
    let n3183: ZW = zw_add(n3181, n3151);
    let n3184: ZW = zw_add(n3182, n2982);
    let n3185: ZW = zw_add(n3183, n2983);
    let n3186: ZW = zw_cellmix_n(368u64, n2281, 1542469173u64);
    let n3187: ZW = zw_cellmix_n(368u64, n2281, 668265263u64);
    let n3188: ZW = zw_add(n3184, n3186);
    let n3189: ZW = zw_add(n3185, n3187);
    let n3190: ZW = zw_cellmix_n(369u64, n2279, 1542469173u64);
    let n3191: ZW = zw_cellmix_n(369u64, n2279, 668265263u64);
    let n3192: ZW = zw_add(n3188, n3190);
    let n3193: ZW = zw_add(n3189, n3191);
    let n3194: ZW = zw_cellmix_n(359u64, n2286, 1542469173u64);
    let n3195: ZW = zw_cellmix_n(359u64, n2286, 668265263u64);
    let n3196: ZW = zw_add(n3148, n3194);
    let n3197: ZW = zw_add(n3149, n3195);
    let n3198: ZW = zw_add(n3196, n2958);
    let n3199: ZW = zw_add(n3197, n2959);
    let n3200: ZW = zw_add(n3198, n3156);
    let n3201: ZW = zw_add(n3199, n3157);
    let n3202: ZW = zw_cellmix_n(369u64, n2287, 1542469173u64);
    let n3203: ZW = zw_cellmix_n(369u64, n2287, 668265263u64);
    let n3204: ZW = zw_add(n3200, n3202);
    let n3205: ZW = zw_add(n3201, n3203);
    let n3206: ZW = zw_add(n3166, n3194);
    let n3207: ZW = zw_add(n3167, n3195);
    let n3208: ZW = zw_add(n3206, n2970);
    let n3209: ZW = zw_add(n3207, n2971);
    let n3210: ZW = zw_add(n3208, n3172);
    let n3211: ZW = zw_add(n3209, n3173);
    let n3212: ZW = zw_cellmix_n(369u64, n2290, 1542469173u64);
    let n3213: ZW = zw_cellmix_n(369u64, n2290, 668265263u64);
    let n3214: ZW = zw_add(n3210, n3212);
    let n3215: ZW = zw_add(n3211, n3213);
    let n3216: ZW = zw_add(n3180, n3194);
    let n3217: ZW = zw_add(n3181, n3195);
    let n3218: ZW = zw_add(n3216, n2982);
    let n3219: ZW = zw_add(n3217, n2983);
    let n3220: ZW = zw_add(n3218, n3186);
    let n3221: ZW = zw_add(n3219, n3187);
    let n3222: ZW = zw_cellmix_n(369u64, n2293, 1542469173u64);
    let n3223: ZW = zw_cellmix_n(369u64, n2293, 668265263u64);
    let n3224: ZW = zw_add(n3220, n3222);
    let n3225: ZW = zw_add(n3221, n3223);
    let n3226: ZW = zw_add(n3062, n2994);
    let n3227: ZW = zw_add(n3063, n2995);
    let n3228: ZW = zw_add(n3226, n3066);
    let n3229: ZW = zw_add(n3227, n3067);
    let n3230: ZW = zw_add(n3228, n3000);
    let n3231: ZW = zw_add(n3229, n3001);
    let n3232: ZW = zw_add(n3230, n3072);
    let n3233: ZW = zw_add(n3231, n3073);
    let n3234: ZW = zw_add(n3232, n3076);
    let n3235: ZW = zw_add(n3233, n3077);
    let n3236: ZW = zw_add(n3234, n3080);
    let n3237: ZW = zw_add(n3235, n3081);
    let n3238: ZW = zw_add(n3236, n3084);
    let n3239: ZW = zw_add(n3237, n3085);
    let n3240: ZW = zw_add(n3238, n3088);
    let n3241: ZW = zw_add(n3239, n3089);
    let n3242: ZW = zw_add(n3240, n2958);
    let n3243: ZW = zw_add(n3241, n2959);
    let n3244: ZW = zw_cellmix_n(368u64, n2301, 1542469173u64);
    let n3245: ZW = zw_cellmix_n(368u64, n2301, 668265263u64);
    let n3246: ZW = zw_add(n3242, n3244);
    let n3247: ZW = zw_add(n3243, n3245);
    let n3248: ZW = zw_cellmix_n(369u64, n2299, 1542469173u64);
    let n3249: ZW = zw_cellmix_n(369u64, n2299, 668265263u64);
    let n3250: ZW = zw_add(n3246, n3248);
    let n3251: ZW = zw_add(n3247, n3249);
    let n3252: ZW = zw_add(n3234, n3102);
    let n3253: ZW = zw_add(n3235, n3103);
    let n3254: ZW = zw_add(n3252, n3106);
    let n3255: ZW = zw_add(n3253, n3107);
    let n3256: ZW = zw_add(n3254, n3088);
    let n3257: ZW = zw_add(n3255, n3089);
    let n3258: ZW = zw_add(n3256, n2970);
    let n3259: ZW = zw_add(n3257, n2971);
    let n3260: ZW = zw_cellmix_n(368u64, n2309, 1542469173u64);
    let n3261: ZW = zw_cellmix_n(368u64, n2309, 668265263u64);
    let n3262: ZW = zw_add(n3258, n3260);
    let n3263: ZW = zw_add(n3259, n3261);
    let n3264: ZW = zw_cellmix_n(369u64, n2307, 1542469173u64);
    let n3265: ZW = zw_cellmix_n(369u64, n2307, 668265263u64);
    let n3266: ZW = zw_add(n3262, n3264);
    let n3267: ZW = zw_add(n3263, n3265);
    let n3268: ZW = zw_add(n3252, n3122);
    let n3269: ZW = zw_add(n3253, n3123);
    let n3270: ZW = zw_add(n3268, n3088);
    let n3271: ZW = zw_add(n3269, n3089);
    let n3272: ZW = zw_add(n3270, n2982);
    let n3273: ZW = zw_add(n3271, n2983);
    let n3274: ZW = zw_cellmix_n(368u64, n2317, 1542469173u64);
    let n3275: ZW = zw_cellmix_n(368u64, n2317, 668265263u64);
    let n3276: ZW = zw_add(n3272, n3274);
    let n3277: ZW = zw_add(n3273, n3275);
    let n3278: ZW = zw_cellmix_n(369u64, n2315, 1542469173u64);
    let n3279: ZW = zw_cellmix_n(369u64, n2315, 668265263u64);
    let n3280: ZW = zw_add(n3276, n3278);
    let n3281: ZW = zw_add(n3277, n3279);
    let n3282: ZW = zw_add(n3232, n3138);
    let n3283: ZW = zw_add(n3233, n3139);
    let n3284: ZW = zw_add(n3282, n3142);
    let n3285: ZW = zw_add(n3283, n3143);
    let n3286: ZW = zw_add(n3284, n3146);
    let n3287: ZW = zw_add(n3285, n3147);
    let n3288: ZW = zw_add(n3286, n3150);
    let n3289: ZW = zw_add(n3287, n3151);
    let n3290: ZW = zw_add(n3288, n2958);
    let n3291: ZW = zw_add(n3289, n2959);
    let n3292: ZW = zw_cellmix_n(368u64, n2325, 1542469173u64);
    let n3293: ZW = zw_cellmix_n(368u64, n2325, 668265263u64);
    let n3294: ZW = zw_add(n3290, n3292);
    let n3295: ZW = zw_add(n3291, n3293);
    let n3296: ZW = zw_cellmix_n(369u64, n2323, 1542469173u64);
    let n3297: ZW = zw_cellmix_n(369u64, n2323, 668265263u64);
    let n3298: ZW = zw_add(n3294, n3296);
    let n3299: ZW = zw_add(n3295, n3297);
    let n3300: ZW = zw_add(n3282, n3102);
    let n3301: ZW = zw_add(n3283, n3103);
    let n3302: ZW = zw_add(n3300, n3106);
    let n3303: ZW = zw_add(n3301, n3107);
    let n3304: ZW = zw_add(n3302, n3150);
    let n3305: ZW = zw_add(n3303, n3151);
    let n3306: ZW = zw_add(n3304, n2970);
    let n3307: ZW = zw_add(n3305, n2971);
    let n3308: ZW = zw_cellmix_n(368u64, n2333, 1542469173u64);
    let n3309: ZW = zw_cellmix_n(368u64, n2333, 668265263u64);
    let n3310: ZW = zw_add(n3306, n3308);
    let n3311: ZW = zw_add(n3307, n3309);
    let n3312: ZW = zw_cellmix_n(369u64, n2331, 1542469173u64);
    let n3313: ZW = zw_cellmix_n(369u64, n2331, 668265263u64);
    let n3314: ZW = zw_add(n3310, n3312);
    let n3315: ZW = zw_add(n3311, n3313);
    let n3316: ZW = zw_add(n3300, n3122);
    let n3317: ZW = zw_add(n3301, n3123);
    let n3318: ZW = zw_add(n3316, n3150);
    let n3319: ZW = zw_add(n3317, n3151);
    let n3320: ZW = zw_add(n3318, n2982);
    let n3321: ZW = zw_add(n3319, n2983);
    let n3322: ZW = zw_cellmix_n(368u64, n2341, 1542469173u64);
    let n3323: ZW = zw_cellmix_n(368u64, n2341, 668265263u64);
    let n3324: ZW = zw_add(n3320, n3322);
    let n3325: ZW = zw_add(n3321, n3323);
    let n3326: ZW = zw_cellmix_n(369u64, n2339, 1542469173u64);
    let n3327: ZW = zw_cellmix_n(369u64, n2339, 668265263u64);
    let n3328: ZW = zw_add(n3324, n3326);
    let n3329: ZW = zw_add(n3325, n3327);
    let n3330: ZW = zw_add(n3286, n3194);
    let n3331: ZW = zw_add(n3287, n3195);
    let n3332: ZW = zw_add(n3330, n2958);
    let n3333: ZW = zw_add(n3331, n2959);
    let n3334: ZW = zw_add(n3332, n3292);
    let n3335: ZW = zw_add(n3333, n3293);
    let n3336: ZW = zw_cellmix_n(369u64, n2344, 1542469173u64);
    let n3337: ZW = zw_cellmix_n(369u64, n2344, 668265263u64);
    let n3338: ZW = zw_add(n3334, n3336);
    let n3339: ZW = zw_add(n3335, n3337);
    let n3340: ZW = zw_add(n3302, n3194);
    let n3341: ZW = zw_add(n3303, n3195);
    let n3342: ZW = zw_add(n3340, n2970);
    let n3343: ZW = zw_add(n3341, n2971);
    let n3344: ZW = zw_add(n3342, n3308);
    let n3345: ZW = zw_add(n3343, n3309);
    let n3346: ZW = zw_cellmix_n(369u64, n2347, 1542469173u64);
    let n3347: ZW = zw_cellmix_n(369u64, n2347, 668265263u64);
    let n3348: ZW = zw_add(n3344, n3346);
    let n3349: ZW = zw_add(n3345, n3347);
    let n3350: ZW = zw_add(n3316, n3194);
    let n3351: ZW = zw_add(n3317, n3195);
    let n3352: ZW = zw_add(n3350, n2982);
    let n3353: ZW = zw_add(n3351, n2983);
    let n3354: ZW = zw_add(n3352, n3322);
    let n3355: ZW = zw_add(n3353, n3323);
    let n3356: ZW = zw_cellmix_n(369u64, n2350, 1542469173u64);
    let n3357: ZW = zw_cellmix_n(369u64, n2350, 668265263u64);
    let n3358: ZW = zw_add(n3354, n3356);
    let n3359: ZW = zw_add(n3355, n3357);
    let ok_v0_b0: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v0_b0: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b0: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v1_b1: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v1_b1: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b1: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v2_b2: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v2_b2: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b2: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v16_b3: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v16_b3: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b3: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v17_b4: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v17_b4: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b4: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v18_b5: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v18_b5: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b5: u16 = ALL & zb_holds(n137) & zb_holds(n782);
    let ok_v32_b6: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v32_b6: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b6: u16 = ALL & zb_holds(n782);
    let ok_v33_b7: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v33_b7: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b7: u16 = ALL & zb_holds(n782);
    let ok_v34_b8: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v34_b8: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b8: u16 = ALL & zb_holds(n782);
    let ok_v36_b9: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v36_b9: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b9: u16 = ALL & zb_holds(n782);
    let ok_v37_b10: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v37_b10: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b10: u16 = ALL & zb_holds(n782);
    let ok_v38_b11: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v38_b11: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b11: u16 = ALL & zb_holds(n782);
    let ok_v40_b12: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v40_b12: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b12: u16 = ALL & zb_holds(n782);
    let ok_v41_b13: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v41_b13: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b13: u16 = ALL & zb_holds(n782);
    let ok_v42_b14: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v42_b14: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b14: u16 = ALL & zb_holds(n782);
    let ok_v48_b15: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v48_b15: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b15: u16 = ALL & zb_holds(n782);
    let ok_v49_b16: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v49_b16: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b16: u16 = ALL & zb_holds(n782);
    let ok_v50_b17: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v50_b17: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b17: u16 = ALL & zb_holds(n782);
    let ok_v52_b18: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v52_b18: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b18: u16 = ALL & zb_holds(n782);
    let ok_v53_b19: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v53_b19: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b19: u16 = ALL & zb_holds(n782);
    let ok_v54_b20: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v54_b20: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b20: u16 = ALL & zb_holds(n782);
    let ok_v56_b21: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v56_b21: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b21: u16 = ALL & zb_holds(n782);
    let ok_v57_b22: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v57_b22: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b22: u16 = ALL & zb_holds(n782);
    let ok_v58_b23: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v58_b23: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b23: u16 = ALL & zb_holds(n782);
    let ok_v0_b24: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v0_b24: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b24: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v1_b25: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v1_b25: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b25: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v2_b26: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v2_b26: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b26: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v16_b27: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v16_b27: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b27: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v17_b28: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v17_b28: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b28: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v18_b29: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v18_b29: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b29: u16 = ALL & zb_holds(n137) & zb_holds(n1174);
    let ok_v32_b30: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v32_b30: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b30: u16 = ALL & zb_holds(n1174);
    let ok_v33_b31: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v33_b31: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b31: u16 = ALL & zb_holds(n1174);
    let ok_v34_b32: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v34_b32: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b32: u16 = ALL & zb_holds(n1174);
    let ok_v36_b33: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v36_b33: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b33: u16 = ALL & zb_holds(n1174);
    let ok_v37_b34: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v37_b34: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b34: u16 = ALL & zb_holds(n1174);
    let ok_v38_b35: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v38_b35: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b35: u16 = ALL & zb_holds(n1174);
    let ok_v40_b36: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v40_b36: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b36: u16 = ALL & zb_holds(n1174);
    let ok_v41_b37: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v41_b37: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b37: u16 = ALL & zb_holds(n1174);
    let ok_v42_b38: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v42_b38: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b38: u16 = ALL & zb_holds(n1174);
    let ok_v48_b39: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v48_b39: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b39: u16 = ALL & zb_holds(n1174);
    let ok_v49_b40: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v49_b40: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b40: u16 = ALL & zb_holds(n1174);
    let ok_v50_b41: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v50_b41: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b41: u16 = ALL & zb_holds(n1174);
    let ok_v52_b42: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v52_b42: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b42: u16 = ALL & zb_holds(n1174);
    let ok_v53_b43: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v53_b43: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b43: u16 = ALL & zb_holds(n1174);
    let ok_v54_b44: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v54_b44: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b44: u16 = ALL & zb_holds(n1174);
    let ok_v56_b45: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v56_b45: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b45: u16 = ALL & zb_holds(n1174);
    let ok_v57_b46: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v57_b46: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b46: u16 = ALL & zb_holds(n1174);
    let ok_v58_b47: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n783);
    let bd_v58_b47: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b47: u16 = ALL & zb_holds(n1174);
    let ok_v0_b48: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1242);
    let bd_v0_b48: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b48: u16 = ALL & zb_holds(n1240);
    let ok_v32_b49: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1242);
    let bd_v32_b49: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b49: u16 = ALL & zb_holds(n1240);
    let ok_v0_b50: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1308);
    let bd_v0_b50: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b50: u16 = ALL & zb_holds(n1306);
    let ok_v32_b51: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1308);
    let bd_v32_b51: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b51: u16 = ALL & zb_holds(n1306);
    let ok_v0_b52: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1920);
    let bd_v0_b52: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b52: u16 = ALL & zb_holds(n1919) & zb_holds(n1981);
    let ok_v32_b53: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n1920);
    let bd_v32_b53: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b53: u16 = ALL & zb_holds(n1919) & zb_holds(n1981);
    let ok_v0_b54: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2029);
    let bd_v0_b54: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b54: u16 = ALL & zb_holds(n2023);
    let ok_v32_b55: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2029);
    let bd_v32_b55: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b55: u16 = ALL & zb_holds(n2023);
    let ok_v0_b56: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v0_b56: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v0_b56: u16 = ALL & zb_holds(n2114);
    let ok_v1_b57: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v1_b57: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v1_b57: u16 = ALL & zb_holds(n2114);
    let ok_v2_b58: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v2_b58: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v2_b58: u16 = ALL & zb_holds(n2114);
    let ok_v16_b59: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v16_b59: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v16_b59: u16 = ALL & zb_holds(n2114);
    let ok_v17_b60: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v17_b60: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v17_b60: u16 = ALL & zb_holds(n2114);
    let ok_v18_b61: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v18_b61: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v18_b61: u16 = ALL & zb_holds(n2114);
    let ok_v32_b62: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v32_b62: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v32_b62: u16 = ALL & zb_holds(n2114);
    let ok_v33_b63: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v33_b63: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v33_b63: u16 = ALL & zb_holds(n2114);
    let ok_v34_b64: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v34_b64: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v34_b64: u16 = ALL & zb_holds(n2114);
    let ok_v36_b65: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v36_b65: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v36_b65: u16 = ALL & zb_holds(n2114);
    let ok_v37_b66: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v37_b66: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v37_b66: u16 = ALL & zb_holds(n2114);
    let ok_v38_b67: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v38_b67: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v38_b67: u16 = ALL & zb_holds(n2114);
    let ok_v40_b68: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v40_b68: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v40_b68: u16 = ALL & zb_holds(n2114);
    let ok_v41_b69: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v41_b69: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v41_b69: u16 = ALL & zb_holds(n2114);
    let ok_v42_b70: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v42_b70: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v42_b70: u16 = ALL & zb_holds(n2114);
    let ok_v48_b71: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v48_b71: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v48_b71: u16 = ALL & zb_holds(n2114);
    let ok_v49_b72: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v49_b72: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v49_b72: u16 = ALL & zb_holds(n2114);
    let ok_v50_b73: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v50_b73: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v50_b73: u16 = ALL & zb_holds(n2114);
    let ok_v52_b74: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v52_b74: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v52_b74: u16 = ALL & zb_holds(n2114);
    let ok_v53_b75: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v53_b75: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v53_b75: u16 = ALL & zb_holds(n2114);
    let ok_v54_b76: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v54_b76: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v54_b76: u16 = ALL & zb_holds(n2114);
    let ok_v56_b77: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v56_b77: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v56_b77: u16 = ALL & zb_holds(n2114);
    let ok_v57_b78: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v57_b78: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v57_b78: u16 = ALL & zb_holds(n2114);
    let ok_v58_b79: u16 = ALL & zb_holds(n179) & zb_holds(r_c295) & zb_holds(r_c278) & zb_holds(n144) & zb_holds(n111) & zb_holds(n145) & zb_holds(n110) & zb_holds(n135) & zb_holds(r_c270) & zb_holds(n109) & zb_holds(n134) & zb_holds(n142) & zb_holds(n132) & zb_holds(n106) & zb_holds(n105) & zb_holds(r_c259) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n153) & zb_holds(r_c251) & zb_holds(n104) & zb_holds(n152) & zb_holds(n101) & zb_holds(n149) & zb_holds(n148) & zb_holds(n100) & zb_holds(r_c240) & zb_holds(n147) & zb_holds(r_c175) & zb_holds(n146) & zb_holds(n197) & zb_holds(n208) & zb_holds(n2115);
    let bd_v58_b79: bool = !n108 || !n107 || !n143 || !n133 || !n103 || !n102 || !n151 || !n150 || !n199 || !n201 || !n203 || !n205;
    let live_v58_b79: u16 = ALL & zb_holds(n2114);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c367: n344,
        c368: n456,
        c301: n455,
        c85: n216,
    };
    let sh1 = KShared1 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c367: n344,
        c368: n456,
        c301: n455,
        c85: n216,
    };
    let sh2 = KShared2 {
        c87: n1176,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh3 = KShared3 {
        c87: n1176,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh4 = KShared4 {
        c87: n1918,
        c84: n116,
        c86: n215,
        c85: n216,
    };
    let sh5 = KShared5 {
        c87: n2025,
        c39: n2026,
        c84: n116,
        c86: n215,
        c85: n216,
        c38: n2028,
    };
    let sh6 = KShared6 {
        c87: r_c87,
        c39: r_c39,
        c84: n116,
        c86: n215,
        c366: n2110,
        c367: n2111,
        c300: n2108,
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
    // 80 distinct button assignments; per outcome they fall
    // into [24, 24, 2, 2, 2, 2, 24] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: r_c41,
        c357: r_c356,
        c358: r_c357,
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n869,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n882,
        c370: n871,
        c300: n881,
        h1: n2446, h2: n2447,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n903,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n907,
        c370: n905,
        c300: n881,
        h1: n2458, h2: n2459,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n925,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n929,
        c370: n927,
        c300: n881,
        h1: n2470, h2: n2471,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n869,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n937,
        c370: n935,
        c300: n881,
        h1: n2500, h2: n2501,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n903,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n943,
        c370: n941,
        c300: n881,
        h1: n2510, h2: n2511,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n925,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n949,
        c370: n947,
        c300: n881,
        h1: n2520, h2: n2521,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n967,
        c281: n963,
        c359: n968,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n975,
        c370: n971,
        c300: n974,
        h1: n2578, h2: n2579,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n985,
        c370: n983,
        c300: n974,
        h1: n2598, h2: n2599,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n993,
        c370: n991,
        c300: n974,
        h1: n2614, h2: n2615,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1008,
        c370: n1006,
        c300: n974,
        h1: n2640, h2: n2641,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1014,
        c370: n1012,
        c300: n974,
        h1: n2656, h2: n2657,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1020,
        c370: n1018,
        c300: n974,
        h1: n2670, h2: n2671,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1008,
        c370: n1024,
        c300: n974,
        h1: n2682, h2: n2683,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1014,
        c370: n1026,
        c300: n974,
        h1: n2692, h2: n2693,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1020,
        c370: n1028,
        c300: n974,
        h1: n2702, h2: n2703,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n967,
        c281: n963,
        c359: n968,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1034,
        c370: n1032,
        c300: n974,
        h1: n2728, h2: n2729,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1040,
        c370: n1038,
        c300: n974,
        h1: n2744, h2: n2745,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1046,
        c370: n1044,
        c300: n974,
        h1: n2758, h2: n2759,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1052,
        c370: n1050,
        c300: n974,
        h1: n2776, h2: n2777,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1058,
        c370: n1056,
        c300: n974,
        h1: n2792, h2: n2793,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1064,
        c370: n1062,
        c300: n974,
        h1: n2806, h2: n2807,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1052,
        c370: n1066,
        c300: n974,
        h1: n2816, h2: n2817,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1058,
        c370: n1068,
        c300: n974,
        h1: n2826, h2: n2827,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1064,
        c370: n1070,
        c300: n974,
        h1: n2836, h2: n2837,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n869,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n882,
        c370: n871,
        c300: n881,
        h1: n2446, h2: n2447,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n903,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n907,
        c370: n905,
        c300: n881,
        h1: n2458, h2: n2459,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n925,
        c286: n791,
        c293: zb_splat(false),
        c294: zb_splat(false),
        c369: n929,
        c370: n927,
        c300: n881,
        h1: n2470, h2: n2471,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n869,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n937,
        c370: n935,
        c300: n881,
        h1: n2500, h2: n2501,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n903,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n943,
        c370: n941,
        c300: n881,
        h1: n2510, h2: n2511,
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
        c281: n792,
        c359: r_c358,
        c360: r_c359,
        c283: n868,
        c284: zn_splat(P8::from_raw(65536i32)),
        c361: n925,
        c286: n933,
        c293: zb_splat(false),
        c294: zb_splat(true),
        c369: n949,
        c370: n947,
        c300: n881,
        h1: n2520, h2: n2521,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_5, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_6 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n967,
        c281: n963,
        c359: n968,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n975,
        c370: n971,
        c300: n974,
        h1: n2578, h2: n2579,
    };
    // body 30: buttons 0x20, forks 0x0
    sink.o1(32, take_1_6, &sh1, &o1);
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_7 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n985,
        c370: n983,
        c300: n974,
        h1: n2598, h2: n2599,
    };
    // body 31: buttons 0x21, forks 0x0
    sink.o1(33, take_1_7, &sh1, &o1);
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_8 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n993,
        c370: n991,
        c300: n974,
        h1: n2614, h2: n2615,
    };
    // body 32: buttons 0x22, forks 0x0
    sink.o1(34, take_1_8, &sh1, &o1);
    declined |= live_v36_b33 & (if bd_v36_b33 { ALL } else { !ok_v36_b33 });
    take_1_9 |= live_v36_b33 & ok_v36_b33 & (if bd_v36_b33 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1008,
        c370: n1006,
        c300: n974,
        h1: n2640, h2: n2641,
    };
    // body 33: buttons 0x24, forks 0x0
    sink.o1(36, take_1_9, &sh1, &o1);
    declined |= live_v37_b34 & (if bd_v37_b34 { ALL } else { !ok_v37_b34 });
    take_1_10 |= live_v37_b34 & ok_v37_b34 & (if bd_v37_b34 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1014,
        c370: n1012,
        c300: n974,
        h1: n2656, h2: n2657,
    };
    // body 34: buttons 0x25, forks 0x0
    sink.o1(37, take_1_10, &sh1, &o1);
    declined |= live_v38_b35 & (if bd_v38_b35 { ALL } else { !ok_v38_b35 });
    take_1_11 |= live_v38_b35 & ok_v38_b35 & (if bd_v38_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1020,
        c370: n1018,
        c300: n974,
        h1: n2670, h2: n2671,
    };
    // body 35: buttons 0x26, forks 0x0
    sink.o1(38, take_1_11, &sh1, &o1);
    declined |= live_v40_b36 & (if bd_v40_b36 { ALL } else { !ok_v40_b36 });
    take_1_12 |= live_v40_b36 & ok_v40_b36 & (if bd_v40_b36 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1008,
        c370: n1024,
        c300: n974,
        h1: n2682, h2: n2683,
    };
    // body 36: buttons 0x28, forks 0x0
    sink.o1(40, take_1_12, &sh1, &o1);
    declined |= live_v41_b37 & (if bd_v41_b37 { ALL } else { !ok_v41_b37 });
    take_1_13 |= live_v41_b37 & ok_v41_b37 & (if bd_v41_b37 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1014,
        c370: n1026,
        c300: n974,
        h1: n2692, h2: n2693,
    };
    // body 37: buttons 0x29, forks 0x0
    sink.o1(41, take_1_13, &sh1, &o1);
    declined |= live_v42_b38 & (if bd_v42_b38 { ALL } else { !ok_v42_b38 });
    take_1_14 |= live_v42_b38 & ok_v42_b38 & (if bd_v42_b38 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n791,
        c293: zb_splat(true),
        c294: zb_splat(false),
        c369: n1020,
        c370: n1028,
        c300: n974,
        h1: n2702, h2: n2703,
    };
    // body 38: buttons 0x2a, forks 0x0
    sink.o1(42, take_1_14, &sh1, &o1);
    declined |= live_v48_b39 & (if bd_v48_b39 { ALL } else { !ok_v48_b39 });
    take_1_15 |= live_v48_b39 & ok_v48_b39 & (if bd_v48_b39 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n967,
        c281: n963,
        c359: n968,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1034,
        c370: n1032,
        c300: n974,
        h1: n2728, h2: n2729,
    };
    // body 39: buttons 0x30, forks 0x0
    sink.o1(48, take_1_15, &sh1, &o1);
    declined |= live_v49_b40 & (if bd_v49_b40 { ALL } else { !ok_v49_b40 });
    take_1_16 |= live_v49_b40 & ok_v49_b40 & (if bd_v49_b40 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1040,
        c370: n1038,
        c300: n974,
        h1: n2744, h2: n2745,
    };
    // body 40: buttons 0x31, forks 0x0
    sink.o1(49, take_1_16, &sh1, &o1);
    declined |= live_v50_b41 & (if bd_v50_b41 { ALL } else { !ok_v50_b41 });
    take_1_17 |= live_v50_b41 & ok_v50_b41 & (if bd_v50_b41 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n966,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n969,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1046,
        c370: n1044,
        c300: n974,
        h1: n2758, h2: n2759,
    };
    // body 41: buttons 0x32, forks 0x0
    sink.o1(50, take_1_17, &sh1, &o1);
    declined |= live_v52_b42 & (if bd_v52_b42 { ALL } else { !ok_v52_b42 });
    take_1_18 |= live_v52_b42 & ok_v52_b42 & (if bd_v52_b42 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1052,
        c370: n1050,
        c300: n974,
        h1: n2776, h2: n2777,
    };
    // body 42: buttons 0x34, forks 0x0
    sink.o1(52, take_1_18, &sh1, &o1);
    declined |= live_v53_b43 & (if bd_v53_b43 { ALL } else { !ok_v53_b43 });
    take_1_19 |= live_v53_b43 & ok_v53_b43 & (if bd_v53_b43 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1058,
        c370: n1056,
        c300: n974,
        h1: n2792, h2: n2793,
    };
    // body 43: buttons 0x35, forks 0x0
    sink.o1(53, take_1_19, &sh1, &o1);
    declined |= live_v54_b44 & (if bd_v54_b44 { ALL } else { !ok_v54_b44 });
    take_1_20 |= live_v54_b44 & ok_v54_b44 & (if bd_v54_b44 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1004,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1064,
        c370: n1062,
        c300: n974,
        h1: n2806, h2: n2807,
    };
    // body 44: buttons 0x36, forks 0x0
    sink.o1(54, take_1_20, &sh1, &o1);
    declined |= live_v56_b45 & (if bd_v56_b45 { ALL } else { !ok_v56_b45 });
    take_1_21 |= live_v56_b45 & ok_v56_b45 & (if bd_v56_b45 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n1002,
        c281: n963,
        c359: n1003,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n869,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1052,
        c370: n1066,
        c300: n974,
        h1: n2816, h2: n2817,
    };
    // body 45: buttons 0x38, forks 0x0
    sink.o1(56, take_1_21, &sh1, &o1);
    declined |= live_v57_b46 & (if bd_v57_b46 { ALL } else { !ok_v57_b46 });
    take_1_22 |= live_v57_b46 & ok_v57_b46 & (if bd_v57_b46 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n981,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n903,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1058,
        c370: n1068,
        c300: n974,
        h1: n2826, h2: n2827,
    };
    // body 46: buttons 0x39, forks 0x0
    sink.o1(57, take_1_22, &sh1, &o1);
    declined |= live_v58_b47 & (if bd_v58_b47 { ALL } else { !ok_v58_b47 });
    take_1_23 |= live_v58_b47 & ok_v58_b47 & (if bd_v58_b47 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: n961,
        c41: n962,
        c357: n1001,
        c358: n980,
        c281: n963,
        c359: n989,
        c360: n1023,
        c283: n964,
        c284: n965,
        c361: n925,
        c286: n933,
        c293: zb_splat(true),
        c294: zb_splat(true),
        c369: n1064,
        c370: n1070,
        c300: n974,
        h1: n2836, h2: n2837,
    };
    // body 47: buttons 0x3a, forks 0x0
    sink.o1(58, take_1_23, &sh1, &o1);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_2_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        c41: r_c41,
        h1: n2850, h2: n2851,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b49 & (if bd_v32_b49 { ALL } else { !ok_v32_b49 });
    take_2_1 |= live_v32_b49 & ok_v32_b49 & (if bd_v32_b49 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: n961,
        c41: n962,
        h1: n2854, h2: n2855,
    };
    // body 49: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b50 & (if bd_v0_b50 { ALL } else { !ok_v0_b50 });
    take_3_0 |= live_v0_b50 & ok_v0_b50 & (if bd_v0_b50 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: r_c20,
        c41: r_c41,
        h1: n2850, h2: n2851,
    };
    // body 50: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined |= live_v32_b51 & (if bd_v32_b51 { ALL } else { !ok_v32_b51 });
    take_3_1 |= live_v32_b51 & ok_v32_b51 & (if bd_v32_b51 { 0 } else { ALL });
    let o3 = KOut3 {
        c20: n961,
        c41: n962,
        h1: n2854, h2: n2855,
    };
    // body 51: buttons 0x20, forks 0x0
    sink.o3(32, take_3_1, &sh3, &o3);
    declined |= live_v0_b52 & (if bd_v0_b52 { ALL } else { !ok_v0_b52 });
    take_4_0 |= live_v0_b52 & ok_v0_b52 & (if bd_v0_b52 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: r_c20,
        c41: r_c41,
        h1: n2862, h2: n2863,
    };
    // body 52: buttons 0x00, forks 0x0
    sink.o4(0, take_4_0, &sh4, &o4);
    declined |= live_v32_b53 & (if bd_v32_b53 { ALL } else { !ok_v32_b53 });
    take_4_1 |= live_v32_b53 & ok_v32_b53 & (if bd_v32_b53 { 0 } else { ALL });
    let o4 = KOut4 {
        c20: n2012,
        c41: n2013,
        h1: n2870, h2: n2871,
    };
    // body 53: buttons 0x20, forks 0x0
    sink.o4(32, take_4_1, &sh4, &o4);
    declined |= live_v0_b54 & (if bd_v0_b54 { ALL } else { !ok_v0_b54 });
    take_5_0 |= live_v0_b54 & ok_v0_b54 & (if bd_v0_b54 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: r_c20,
        h1: n2890, h2: n2891,
    };
    // body 54: buttons 0x00, forks 0x0
    sink.o5(0, take_5_0, &sh5, &o5);
    declined |= live_v32_b55 & (if bd_v32_b55 { ALL } else { !ok_v32_b55 });
    take_5_1 |= live_v32_b55 & ok_v32_b55 & (if bd_v32_b55 { 0 } else { ALL });
    let o5 = KOut5 {
        c20: n2031,
        h1: n2894, h2: n2895,
    };
    // body 55: buttons 0x20, forks 0x0
    sink.o5(32, take_5_1, &sh5, &o5);
    declined |= live_v0_b56 & (if bd_v0_b56 { ALL } else { !ok_v0_b56 });
    take_6_0 |= live_v0_b56 & ok_v0_b56 & (if bd_v0_b56 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2109,
        c285: n2104,
        c292: n2105,
        c293: n2106,
        c368: n2125,
        c369: n2113,
        c299: n2124,
        h1: n2968, h2: n2969,
    };
    // body 56: buttons 0x00, forks 0x0
    sink.o6(0, take_6_0, &sh6, &o6);
    declined |= live_v1_b57 & (if bd_v1_b57 { ALL } else { !ok_v1_b57 });
    take_6_1 |= live_v1_b57 & ok_v1_b57 & (if bd_v1_b57 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2138,
        c285: n2104,
        c292: n2105,
        c293: n2106,
        c368: n2142,
        c369: n2140,
        c299: n2124,
        h1: n2980, h2: n2981,
    };
    // body 57: buttons 0x01, forks 0x0
    sink.o6(1, take_6_1, &sh6, &o6);
    declined |= live_v2_b58 & (if bd_v2_b58 { ALL } else { !ok_v2_b58 });
    take_6_2 |= live_v2_b58 & ok_v2_b58 & (if bd_v2_b58 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2154,
        c285: n2104,
        c292: n2105,
        c293: n2106,
        c368: n2158,
        c369: n2156,
        c299: n2124,
        h1: n2992, h2: n2993,
    };
    // body 58: buttons 0x02, forks 0x0
    sink.o6(2, take_6_2, &sh6, &o6);
    declined |= live_v16_b59 & (if bd_v16_b59 { ALL } else { !ok_v16_b59 });
    take_6_3 |= live_v16_b59 & ok_v16_b59 & (if bd_v16_b59 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2109,
        c285: n2165,
        c292: n2105,
        c293: n2166,
        c368: n2170,
        c369: n2168,
        c299: n2124,
        h1: n3022, h2: n3023,
    };
    // body 59: buttons 0x10, forks 0x0
    sink.o6(16, take_6_3, &sh6, &o6);
    declined |= live_v17_b60 & (if bd_v17_b60 { ALL } else { !ok_v17_b60 });
    take_6_4 |= live_v17_b60 & ok_v17_b60 & (if bd_v17_b60 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2138,
        c285: n2165,
        c292: n2105,
        c293: n2166,
        c368: n2178,
        c369: n2176,
        c299: n2124,
        h1: n3032, h2: n3033,
    };
    // body 60: buttons 0x11, forks 0x0
    sink.o6(17, take_6_4, &sh6, &o6);
    declined |= live_v18_b61 & (if bd_v18_b61 { ALL } else { !ok_v18_b61 });
    take_6_5 |= live_v18_b61 & ok_v18_b61 & (if bd_v18_b61 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2100,
        c41: r_c41,
        c356: r_c356,
        c357: r_c357,
        c280: n2101,
        c358: r_c358,
        c359: r_c359,
        c282: n2102,
        c283: n2103,
        c360: n2154,
        c285: n2165,
        c292: n2105,
        c293: n2166,
        c368: n2186,
        c369: n2184,
        c299: n2124,
        h1: n3042, h2: n3043,
    };
    // body 61: buttons 0x12, forks 0x0
    sink.o6(18, take_6_5, &sh6, &o6);
    declined |= live_v32_b62 & (if bd_v32_b62 { ALL } else { !ok_v32_b62 });
    take_6_6 |= live_v32_b62 & ok_v32_b62 & (if bd_v32_b62 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2212,
        c280: n2207,
        c358: n2213,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2220,
        c369: n2216,
        c299: n2219,
        h1: n3100, h2: n3101,
    };
    // body 62: buttons 0x20, forks 0x0
    sink.o6(32, take_6_6, &sh6, &o6);
    declined |= live_v33_b63 & (if bd_v33_b63 { ALL } else { !ok_v33_b63 });
    take_6_7 |= live_v33_b63 & ok_v33_b63 & (if bd_v33_b63 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2234,
        c369: n2232,
        c299: n2219,
        h1: n3120, h2: n3121,
    };
    // body 63: buttons 0x21, forks 0x0
    sink.o6(33, take_6_7, &sh6, &o6);
    declined |= live_v34_b64 & (if bd_v34_b64 { ALL } else { !ok_v34_b64 });
    take_6_8 |= live_v34_b64 & ok_v34_b64 & (if bd_v34_b64 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2245,
        c369: n2243,
        c299: n2219,
        h1: n3136, h2: n3137,
    };
    // body 64: buttons 0x22, forks 0x0
    sink.o6(34, take_6_8, &sh6, &o6);
    declined |= live_v36_b65 & (if bd_v36_b65 { ALL } else { !ok_v36_b65 });
    take_6_9 |= live_v36_b65 & ok_v36_b65 & (if bd_v36_b65 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2259,
        c280: n2207,
        c358: n2260,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2265,
        c369: n2263,
        c299: n2219,
        h1: n3162, h2: n3163,
    };
    // body 65: buttons 0x24, forks 0x0
    sink.o6(36, take_6_9, &sh6, &o6);
    declined |= live_v37_b66 & (if bd_v37_b66 { ALL } else { !ok_v37_b66 });
    take_6_10 |= live_v37_b66 & ok_v37_b66 & (if bd_v37_b66 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2273,
        c369: n2271,
        c299: n2219,
        h1: n3178, h2: n3179,
    };
    // body 66: buttons 0x25, forks 0x0
    sink.o6(37, take_6_10, &sh6, &o6);
    declined |= live_v38_b67 & (if bd_v38_b67 { ALL } else { !ok_v38_b67 });
    take_6_11 |= live_v38_b67 & ok_v38_b67 & (if bd_v38_b67 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2281,
        c369: n2279,
        c299: n2219,
        h1: n3192, h2: n3193,
    };
    // body 67: buttons 0x26, forks 0x0
    sink.o6(38, take_6_11, &sh6, &o6);
    declined |= live_v40_b68 & (if bd_v40_b68 { ALL } else { !ok_v40_b68 });
    take_6_12 |= live_v40_b68 & ok_v40_b68 & (if bd_v40_b68 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2259,
        c280: n2207,
        c358: n2260,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2265,
        c369: n2287,
        c299: n2219,
        h1: n3204, h2: n3205,
    };
    // body 68: buttons 0x28, forks 0x0
    sink.o6(40, take_6_12, &sh6, &o6);
    declined |= live_v41_b69 & (if bd_v41_b69 { ALL } else { !ok_v41_b69 });
    take_6_13 |= live_v41_b69 & ok_v41_b69 & (if bd_v41_b69 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2273,
        c369: n2290,
        c299: n2219,
        h1: n3214, h2: n3215,
    };
    // body 69: buttons 0x29, forks 0x0
    sink.o6(41, take_6_13, &sh6, &o6);
    declined |= live_v42_b70 & (if bd_v42_b70 { ALL } else { !ok_v42_b70 });
    take_6_14 |= live_v42_b70 & ok_v42_b70 & (if bd_v42_b70 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2104,
        c292: n2210,
        c293: n2106,
        c368: n2281,
        c369: n2293,
        c299: n2219,
        h1: n3224, h2: n3225,
    };
    // body 70: buttons 0x2a, forks 0x0
    sink.o6(42, take_6_14, &sh6, &o6);
    declined |= live_v48_b71 & (if bd_v48_b71 { ALL } else { !ok_v48_b71 });
    take_6_15 |= live_v48_b71 & ok_v48_b71 & (if bd_v48_b71 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2212,
        c280: n2207,
        c358: n2213,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2301,
        c369: n2299,
        c299: n2219,
        h1: n3250, h2: n3251,
    };
    // body 71: buttons 0x30, forks 0x0
    sink.o6(48, take_6_15, &sh6, &o6);
    declined |= live_v49_b72 & (if bd_v49_b72 { ALL } else { !ok_v49_b72 });
    take_6_16 |= live_v49_b72 & ok_v49_b72 & (if bd_v49_b72 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2309,
        c369: n2307,
        c299: n2219,
        h1: n3266, h2: n3267,
    };
    // body 72: buttons 0x31, forks 0x0
    sink.o6(49, take_6_16, &sh6, &o6);
    declined |= live_v50_b73 & (if bd_v50_b73 { ALL } else { !ok_v50_b73 });
    take_6_17 |= live_v50_b73 & ok_v50_b73 & (if bd_v50_b73 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2211,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2214,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2317,
        c369: n2315,
        c299: n2219,
        h1: n3280, h2: n3281,
    };
    // body 73: buttons 0x32, forks 0x0
    sink.o6(50, take_6_17, &sh6, &o6);
    declined |= live_v52_b74 & (if bd_v52_b74 { ALL } else { !ok_v52_b74 });
    take_6_18 |= live_v52_b74 & ok_v52_b74 & (if bd_v52_b74 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2259,
        c280: n2207,
        c358: n2260,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2325,
        c369: n2323,
        c299: n2219,
        h1: n3298, h2: n3299,
    };
    // body 74: buttons 0x34, forks 0x0
    sink.o6(52, take_6_18, &sh6, &o6);
    declined |= live_v53_b75 & (if bd_v53_b75 { ALL } else { !ok_v53_b75 });
    take_6_19 |= live_v53_b75 & ok_v53_b75 & (if bd_v53_b75 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2333,
        c369: n2331,
        c299: n2219,
        h1: n3314, h2: n3315,
    };
    // body 75: buttons 0x35, forks 0x0
    sink.o6(53, take_6_19, &sh6, &o6);
    declined |= live_v54_b76 & (if bd_v54_b76 { ALL } else { !ok_v54_b76 });
    take_6_20 |= live_v54_b76 & ok_v54_b76 & (if bd_v54_b76 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2261,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2341,
        c369: n2339,
        c299: n2219,
        h1: n3328, h2: n3329,
    };
    // body 76: buttons 0x36, forks 0x0
    sink.o6(54, take_6_20, &sh6, &o6);
    declined |= live_v56_b77 & (if bd_v56_b77 { ALL } else { !ok_v56_b77 });
    take_6_21 |= live_v56_b77 & ok_v56_b77 & (if bd_v56_b77 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2259,
        c280: n2207,
        c358: n2260,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2109,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2325,
        c369: n2344,
        c299: n2219,
        h1: n3338, h2: n3339,
    };
    // body 77: buttons 0x38, forks 0x0
    sink.o6(56, take_6_21, &sh6, &o6);
    declined |= live_v57_b78 & (if bd_v57_b78 { ALL } else { !ok_v57_b78 });
    take_6_22 |= live_v57_b78 & ok_v57_b78 & (if bd_v57_b78 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2230,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2138,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2333,
        c369: n2347,
        c299: n2219,
        h1: n3348, h2: n3349,
    };
    // body 78: buttons 0x39, forks 0x0
    sink.o6(57, take_6_22, &sh6, &o6);
    declined |= live_v58_b79 & (if bd_v58_b79 { ALL } else { !ok_v58_b79 });
    take_6_23 |= live_v58_b79 & ok_v58_b79 & (if bd_v58_b79 { 0 } else { ALL });
    let o6 = KOut6 {
        c20: n2205,
        c41: n2206,
        c356: n2258,
        c357: n2229,
        c280: n2207,
        c358: n2241,
        c359: n2286,
        c282: n2208,
        c283: n2209,
        c360: n2154,
        c285: n2165,
        c292: n2210,
        c293: n2166,
        c368: n2341,
        c369: n2350,
        c299: n2219,
        h1: n3358, h2: n3359,
    };
    // body 79: buttons 0x3a, forks 0x0
    sink.o6(58, take_6_23, &sh6, &o6);
    declined
}
